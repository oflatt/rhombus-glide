#lang racket/base
;; display list -> .pptx
;;
;; Every item becomes a real PowerPoint object: a rectangle is a `prstGeom`
;; rectangle, a path is a `custGeom`, a string is a text box you can retype, a
;; bitmap is a picture. Nothing is rasterized that does not have to be.
;;
;; The package is written from scratch with one master, one blank layout and one
;; theme, and every property stated explicitly on the slide. That needs no
;; placeholder or inheritance machinery, and it means our own importer can read
;; back what we write, which is what makes the export self-testing.
(require racket/list racket/string racket/math racket/file racket/path
         racket/format racket/class racket/draw
         (only-in racket/draw/unsafe/png
                  create-png-writer write-png destroy-png-writer)
         file/zip
         "draw-ir.rkt" "source-tag.rkt" (prefix-in ir: "ir.rkt"))
(provide display-pages->pptx display-page-sequence->pptx current-write-warnings)

(define current-write-warnings (make-parameter #f))
(define (warn! fmt . args)
  (define b (current-write-warnings))
  (when b (set-box! b (cons (apply format fmt args) (unbox b)))))

;; ------------------------------------------------------------------- units

(define (emu v) (inexact->exact (round (* 12700.0 (exact->inexact v)))))
(define (angle-60k deg)
  (inexact->exact (round (* 60000.0 (let loop ([d (exact->inexact deg)])
                                      (cond [(< d 0) (loop (+ d 360.0))]
                                            [(>= d 360.0) (loop (- d 360.0))]
                                            [else d]))))))
;; A percentage, in DrawingML's thousandths of a percent. `pct` is for the ones
;; that are a fraction of one -- an alpha, a gradient stop, a crop -- and holds
;; them there. `pct*` is for the ones that are not: a line spacing of 1.5, a
;; bullet drawn at 120% of the text, a subscript's -25%. Clamping those to one
;; was quietly flattening them on the way out, and since the merge compares
;; what it wrote against what the program says, the next save would have
;; written the flattened value back into the program.
(define (pct v) (inexact->exact (round (* 100000.0 (max 0.0 (min 1.0 v))))))
(define (pct* v) (inexact->exact (round (* 100000.0 v))))

(define (hex-of c)
  (define (two n)
    (define s (number->string (max 0 (min 255 (inexact->exact (round n)))) 16))
    (string-upcase (if (= 1 (string-length s)) (string-append "0" s) s)))
  (string-append (two (rgba*-r c)) (two (rgba*-g c)) (two (rgba*-b c))))

;; A color, with an alpha child only when it is not opaque.
(define (clr c)
  (if (>= (rgba*-a c) 0.999)
      (format "<a:srgbClr val=\"~a\"/>" (hex-of c))
      (format "<a:srgbClr val=\"~a\"><a:alpha val=\"~a\"/></a:srgbClr>"
              (hex-of c) (pct (rgba*-a c)))))

(define (xml-escape s)
  (for/fold ([s s]) ([p (in-list '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
                                   ("\"" . "&quot;")))])
    (string-replace s (car p) (cdr p))))

;; ------------------------------------------------------------------- paint

;; Set while one slide is being written, so a fill that is a picture can add its
;; file to the package the way a picture item does. Threading it through every
;; item writer would mean a new argument on each of them for one kind of fill.
(define current-image-register (make-parameter #f))

(define (fill-xml f)
  (cond
    [(not f) "<a:noFill/>"]
    [(fill:solid? f) (format "<a:solidFill>~a</a:solidFill>" (clr (fill:solid-color f)))]
    ;; A picture as a fill: the file goes in the package and the shape points
    ;; at it. Without a registrar there is nowhere to put the file, so the
    ;; shape is left unfilled rather than pointing at nothing.
    [(fill:image? f)
     (define reg (current-image-register))
     (cond
       [(not reg) "<a:noFill/>"]
       [else
        (define rid (reg f "fill" (src-extension (fill:image-src f))))
        (format (string-append "<a:blipFill rotWithShape=\"0\"><a:blip r:embed=\"~a\">~a</a:blip>"
                               "<a:stretch><a:fillRect/></a:stretch></a:blipFill>")
                rid
                (let ([o (fill:image-opacity f)])
                  (if (< o 0.999) (format "<a:alphaModFix amt=\"~a\"/>" (pct o)) "")))])]
    [(fill:linear? f)
     (define dx (- (fill:linear-x1 f) (fill:linear-x0 f)))
     (define dy (- (fill:linear-y1 f) (fill:linear-y0 f)))
     ;; `ang` is clockwise from the positive x axis, and our y already points
     ;; down, so atan2 gives it directly.
     (format "<a:gradFill rotWithShape=\"0\"><a:gsLst>~a</a:gsLst><a:lin ang=\"~a\" scaled=\"0\"/></a:gradFill>"
             (stops-xml (fill:linear-stops f))
             (angle-60k (radians->degrees (atan dy dx))))]
    [(fill:radial? f)
     (format (string-append "<a:gradFill rotWithShape=\"0\"><a:gsLst>~a</a:gsLst>"
                            "<a:path path=\"circle\"><a:fillToRect l=\"50000\" t=\"50000\""
                            " r=\"50000\" b=\"50000\"/></a:path></a:gradFill>")
             (stops-xml (fill:radial-stops f)))]
    [else "<a:noFill/>"]))

;; DrawingML needs at least two stops and them in order.
(define (stops-xml stops)
  (define ss (sort (if (< (length stops) 2)
                       (list (list 0.0 (second (first stops)))
                             (list 1.0 (second (first stops))))
                       stops)
                   < #:key first))
  (apply string-append
         (for/list ([s (in-list ss)])
           (format "<a:gs pos=\"~a\">~a</a:gs>" (pct (first s)) (clr (second s))))))

(define dash-xml
  (hash 'solid "" 'dash "<a:prstDash val=\"dash\"/>"
        'short-dash "<a:prstDash val=\"sysDash\"/>"
        'dot "<a:prstDash val=\"sysDot\"/>" 'dash-dot "<a:prstDash val=\"dashDot\"/>"))

;; A spelled-out dash goes back as it came, so a line that was drawn with one
;; keeps its exact pattern rather than the nearest preset.
(define (dash-part p)
  (define custom (pen*-dash-pattern p))
  (if custom
      (format "<a:custDash>~a</a:custDash>"
              (apply string-append
                     (for/list ([ds (in-list custom)])
                       (format "<a:ds d=\"~a\" sp=\"~a\"/>"
                               (inexact->exact (round (* 1000 (car ds))))
                               (inexact->exact (round (* 1000 (cdr ds))))))))
      (hash-ref dash-xml (pen*-dash p) "")))

(define (line-xml p) (line-xml/name "ln" p))

;; A cell's borders are the same line under four other names.
(define (line-xml/name name p)
  (cond
    [(not p) (format "<a:~a><a:noFill/></a:~a>" name name)]
    [else
     (define w (if (zero? (pen*-width p)) HAIRLINE-WIDTH (pen*-width p)))
     (format "<a:~a w=\"~a\" cap=\"~a\"><a:solidFill>~a</a:solidFill>~a<a:~a/>~a~a</a:~a>"
             name
             (emu w)
             (case (pen*-cap p) [(round) "rnd"] [(square) "sq"] [else "flat"])
             (clr (pen*-color p))
             (dash-part p)
             (case (pen*-join p) [(round) "round"] [(bevel) "bevel"] [else "miter"])
             (end-xml "headEnd" (pen*-head p))
             (end-xml "tailEnd" (pen*-tail p))
             name)]))

;; The decoration at one end of a line. The order matters: DrawingML wants the
;; head before the tail, both after the dash and join.
(define (end-xml tag e)
  (if (not e)
      ""
      (format "<a:~a type=\"~a\" w=\"~a\" len=\"~a\"/>"
              tag
              (case (ir:line-end-kind e)
                [(triangle) "triangle"] [(stealth) "stealth"]
                [(diamond) "diamond"] [(oval) "oval"] [else "arrow"])
              (ir:line-end-width e) (ir:line-end-length e))))

;; ------------------------------------------------------------------ shapes

;; The name is the one piece of non-visual shape metadata LibreOffice preserves
;; reliably. In particular, it drops `descr` from groups (and from some shapes
;; that it reconstructs), so an automatic source identity has to travel here as
;; well as in alt text. It is still generated only in the exported file -- it
;; never appears in the user's program -- and its optional suffix keeps the
;; original, readable object name visible at the end of the Selection Pane
;; entry.
(define (editor-name tag fallback)
  (or tag fallback))

(define (nv-xml id name #:tag [tag #f])
  (format (string-append "<p:nvSpPr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
                         "<p:cNvSpPr/><p:nvPr/></p:nvSpPr>")
          id (xml-escape (editor-name tag name))
          (if tag (format " descr=\"glide-pptx:~a\"" (xml-escape tag)) "")))

(define (xfrm-xml x y w h rot [fh #f] [fv #f])
  (format "<a:xfrm~a~a~a><a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/></a:xfrm>"
          (if (zero? rot) "" (format " rot=\"~a\"" (angle-60k rot)))
          (if fh " flipH=\"1\"" "") (if fv " flipV=\"1\"" "")
          (emu x) (emu y) (max 1 (emu w)) (max 1 (emu h))))

(define EMPTY-TX "<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody>")

(define (shape-xml id name geom x y w h rot fill pen)
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id name) (xfrm-xml x y w h rot) geom
          (fill-xml fill) (line-xml pen) EMPTY-TX))

(define (rect-item-xml id i)
  (define r (it:rect-radius i))
  (define w (it:rect-w i)) (define h (it:rect-h i))
  (define geom
    (if (and (positive? r) (positive? (min w h)))
        ;; roundRect's adjustment is a fraction of the smaller side.
        (format "<a:prstGeom prst=\"roundRect\"><a:avLst><a:gd name=\"adj\" fmla=\"val ~a\"/></a:avLst></a:prstGeom>"
                (pct (min 0.5 (/ r (min w h)))))
        "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>"))
  (shape-xml id (format "Rectangle ~a" id) geom
             (it:rect-x i) (it:rect-y i) w h (it:rect-rot i)
             (it:rect-fill i) (it:rect-pen i)))

(define (ellipse-item-xml id i)
  (shape-xml id (format "Oval ~a" id) "<a:prstGeom prst=\"ellipse\"><a:avLst/></a:prstGeom>"
             (it:ellipse-x i) (it:ellipse-y i) (it:ellipse-w i) (it:ellipse-h i)
             (it:ellipse-rot i) (it:ellipse-fill i) (it:ellipse-pen i)))

;; ------------------------------------------------------------------- paths

;; A custGeom's coordinates live in the path's own space, which the shape's
;; extent then maps, so they are written relative to the item's bounding box.
(define (path-item-xml id i)
  (define segs (it:path-segs i))
  (define-values (bx by bw bh) (apply values (segs-bounds segs)))
  (define w (max bw 0.001)) (define h (max bh 0.001))
  (shape-xml id (format "Freeform ~a" id) (path-geom-xml segs bx by w h)
             bx by w h 0.0 (it:path-fill i) (it:path-pen i)))

;; Path coordinates live in the path's own space, which the shape's extent maps,
;; so they are written relative to the bounding box.
(define (path-geom-xml segs bx by w h)
  (define (px v) (emu (- v bx)))
  (define (py v) (emu (- v by)))
  (define body
    (apply string-append
           (for/list ([s (in-list segs)])
             (cond
               [(seg:move? s) (format "<a:moveTo><a:pt x=\"~a\" y=\"~a\"/></a:moveTo>"
                                      (px (seg:move-x s)) (py (seg:move-y s)))]
               [(seg:line? s) (format "<a:lnTo><a:pt x=\"~a\" y=\"~a\"/></a:lnTo>"
                                      (px (seg:line-x s)) (py (seg:line-y s)))]
               [(seg:curve? s)
                (format (string-append "<a:cubicBezTo><a:pt x=\"~a\" y=\"~a\"/>"
                                       "<a:pt x=\"~a\" y=\"~a\"/><a:pt x=\"~a\" y=\"~a\"/>"
                                       "</a:cubicBezTo>")
                        (px (seg:curve-x1 s)) (py (seg:curve-y1 s))
                        (px (seg:curve-x2 s)) (py (seg:curve-y2 s))
                        (px (seg:curve-x s)) (py (seg:curve-y s)))]
               [else "<a:close/>"]))))
  ;; The text rectangle is named rather than measured: `r` and `b` are the
  ;; shape's own right and bottom, which is what PowerPoint writes and what
  ;; every file here has. Written as numbers instead -- the same numbers, in
  ;; the space the path is drawn in -- LibreOffice puts the text somewhere it
  ;; cannot be seen, and a callout comes back as an empty bubble.
  (format (string-append "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
                         "<a:rect l=\"0\" t=\"0\" r=\"r\" b=\"b\"/>"
                         "<a:pathLst><a:path w=\"~a\" h=\"~a\">~a</a:path></a:pathLst>"
                         "</a:custGeom>")
          (emu w) (emu h) body))

;; -------------------------------------------------------------------- text

;; A one-line text box with zero insets and no wrapping, sized to the measured
;; string, so PowerPoint puts the first baseline where the pict had it.
(define (text-item-xml id i)
  (define sz (max 100 (inexact->exact (round (* 100 (it:text-size i))))))
  (define rpr
    (format (string-append "<a:rPr lang=\"en-US\" sz=\"~a\"~a~a~a dirty=\"0\">"
                           "<a:solidFill>~a</a:solidFill>"
                           "<a:latin typeface=\"~a\"/><a:cs typeface=\"~a\"/></a:rPr>")
            sz
            (if (it:text-bold? i) " b=\"1\"" "")
            (if (it:text-italic? i) " i=\"1\"" "")
            (if (it:text-underline? i) " u=\"sng\"" "")
            (clr (it:text-color i))
            (xml-escape (it:text-face i)) (xml-escape (it:text-face i))))
  (define body
    (format (string-append "<p:txBody><a:bodyPr wrap=\"none\" lIns=\"0\" tIns=\"0\""
                           " rIns=\"0\" bIns=\"0\" anchor=\"t\" anchorCtr=\"0\">"
                           "<a:noAutofit/></a:bodyPr><a:lstStyle/>"
                           "<a:p><a:pPr algn=\"l\" marL=\"0\" indent=\"0\"><a:lnSpc>"
                           "<a:spcPct val=\"100000\"/></a:lnSpc><a:buNone/></a:pPr>"
                           "<a:r>~a<a:t>~a</a:t></a:r></a:p></p:txBody>")
            rpr (xml-escape (it:text-str i))))
  (format "<p:sp>~a<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/><a:ln><a:noFill/></a:ln></p:spPr>~a</p:sp>"
          (nv-xml id (format "Text ~a" id))
          (xfrm-xml (it:text-x i) (it:text-y i)
                    (+ TEXT-BOX-SLACK (it:text-w i)) (it:text-h i) (it:text-rot i))
          body))

;; ------------------------------------------------------- rich text bodies

;; Writes an `ir:text-body` as paragraphs and runs, so PowerPoint lays the text
;; out and can reflow it. This is the inverse of the import side's text parsing,
;; and it is what a text box exported semantically gets instead of one
;; unwrappable box per drawn word.
(define (txbody-xml body)
  (define ins (ir:text-body-insets body))
  (format (string-append "<p:txBody><a:bodyPr wrap=\"~a\" lIns=\"~a\" tIns=\"~a\""
                         " rIns=\"~a\" bIns=\"~a\" anchor=\"~a\" anchorCtr=\"~a\">"
                         "~a</a:bodyPr><a:lstStyle/>~a</p:txBody>")
          (if (ir:text-body-wrap? body) "square" "none")
          (emu (ir:insets-l ins)) (emu (ir:insets-t ins))
          (emu (ir:insets-r ins)) (emu (ir:insets-b ins))
          (case (ir:text-body-anchor body)
            [(center) "ctr"] [(bottom) "b"] [else "t"])
          (if (ir:text-body-anchor-center? body) "1" "0")
          (case (ir:text-body-autofit body)
            ;; The scale is stated as spent. PowerPoint caches how far it
            ;; shrank the text in this attribute, and that shrink is folded
            ;; into the sizes when the deck is read -- so a bare normAutofit,
            ;; which means "no cache", invites the next renderer to work the
            ;; shrink out again and apply it a second time.
            [(shrink) "<a:normAutofit fontScale=\"100000\" lnSpcReduction=\"0\"/>"] [(grow) "<a:spAutoFit/>"]
            [else "<a:noAutofit/>"])
          (apply string-append
                 (for/list ([p (in-list (ir:text-body-paras body))]) (para-xml p)))))

(define (para-xml p)
  (define b (ir:para-bullet p))
  (define spacing (ir:para-line-spacing p))
  (format "<a:p><a:pPr algn=\"~a\" lvl=\"~a\" marL=\"~a\" indent=\"~a\">~a~a~a~a</a:pPr>~a</a:p>"
          (case (ir:para-align p)
            [(center) "ctr"] [(right) "r"] [(justify) "just"] [else "l"])
          (ir:para-level p)
          (emu (ir:para-margin-left p)) (emu (ir:para-indent p))
          (if (eq? 'points (car spacing))
              (format "<a:lnSpc><a:spcPts val=\"~a\"/></a:lnSpc>"
                      (inexact->exact (round (* 100 (cdr spacing)))))
              (format "<a:lnSpc><a:spcPct val=\"~a\"/></a:lnSpc>" (pct* (cdr spacing))))
          ;; Written even when it is nothing, like the alignment and the
          ;; margins beside it. A paragraph that says nothing about the space
          ;; above it is given whatever the master says, and the deck this came
          ;; from said nothing because it had already said zero.
          (format "<a:spcBef><a:spcPts val=\"~a\"/></a:spcBef>"
                  (inexact->exact (round (* 100 (ir:para-space-before p)))))
          (format "<a:spcAft><a:spcPts val=\"~a\"/></a:spcAft>"
                  (inexact->exact (round (* 100 (ir:para-space-after p)))))
          (bullet-xml b)
          ;; A paragraph whose runs are all empty writes no run at all, and a
          ;; paragraph with no run takes its line height from `endParaRPr`. Say
          ;; there what the run would have said, or the blank line between two
          ;; 60pt ones comes back the height of the default text.
          (let ([runs (apply string-append
                             (for/list ([r (in-list (ir:para-runs p))]) (run-xml r)))])
            (if (and (string=? "" runs) (pair? (ir:para-runs p)))
                (rpr-xml (car (ir:para-runs p)) "a:endParaRPr")
                runs))))

;; The order is the format's: colour, then size, then font, then the bullet
;; itself.
(define (bullet-size-xml b)
  (define frac (ir:bullet-size-frac b))
  ;; Read, drawn, and until now never written: a bullet set to two fifths of
  ;; its line came back the full size of the text.
  (if (and frac (not (= frac 1.0)))
      (format "<a:buSzPct val=\"~a\"/>" (inexact->exact (round (* 100000 frac))))
      ""))

(define (bullet-xml b)
  (case (ir:bullet-kind b)
    [(char) (format "~a~a~a<a:buChar char=\"~a\"/>"
                    (if (ir:bullet-color b)
                        (format "<a:buClr>~a</a:buClr>"
                                (clr (ir-rgba (ir:bullet-color b)))) "")
                    (bullet-size-xml b)
                    (if (ir:bullet-font b)
                        (format "<a:buFont typeface=\"~a\"/>" (xml-escape (ir:bullet-font b)))
                        "")
                    (xml-escape (or (ir:bullet-char b) "\u2022")))]
    ;; A numbered bullet has a typeface like any other. It was written only for
    ;; the `char` kind, so a numbered list came back saying nothing about its
    ;; own font -- and a program that had stated one disagreed with its own deck
    ;; about every paragraph in the list.
    [(number) (format "~a~a~a<a:buAutoNum type=\"~a\"/>"
                      (if (ir:bullet-color b)
                          (format "<a:buClr>~a</a:buClr>"
                                  (clr (ir-rgba (ir:bullet-color b)))) "")
                      (bullet-size-xml b)
                      (if (ir:bullet-font b)
                          (format "<a:buFont typeface=\"~a\"/>"
                                  (xml-escape (ir:bullet-font b)))
                          "")
                      (xml-escape (or (ir:bullet-char b) "arabicPeriod")))]
    [else "<a:buNone/>"]))

;; A run holding a line break is written as one: the parser reads `<a:br/>` as a
;; newline in the run's text, and a newline inside `<a:t>` is not a line break
;; but whitespace, so a paragraph that had been broken by hand came back as one
;; long line with a stray space in it.
(define (run-xml r)
  (define pieces (string-split (ir:trun-text r) "\n" #:trim? #f))
  (string-join (for/list ([piece (in-list pieces)])
                 (if (string=? "" piece) "" (run-piece-xml r piece)))
               "<a:br/>"))

(define (run-piece-xml r text)
  (format "<a:r>~a<a:t>~a</a:t></a:r>" (rpr-xml r "a:rPr") (xml-escape text)))

;; The run properties, under whichever tag asks for them: `a:rPr` for a run,
;; `a:endParaRPr` for the empty paragraph that has no run to carry them.
(define (rpr-xml r tag)
  (format (string-append "<~a lang=\"en-US\" sz=\"~a\"~a~a~a~a~a~a~a dirty=\"0\">"
                         "<a:solidFill>~a</a:solidFill>"
                         "<a:latin typeface=\"~a\"/><a:cs typeface=\"~a\"/></~a>")
          tag
          (max 100 (inexact->exact (round (* 100 (ir:trun-size r)))))
          (if (ir:trun-bold? r) " b=\"1\"" "")
          (if (ir:trun-italic? r) " i=\"1\"" "")
          (if (ir:trun-underline? r) " u=\"sng\"" "")
          (if (ir:trun-strike? r) " strike=\"sngStrike\"" "")
          ;; Read, drawn, and until now never written: a title that a layout
          ;; had set in capitals came back in the case it was typed in.
          (case (ir:trun-caps r)
            [(all) " cap=\"all\""]
            [(small) " cap=\"small\""]
            [else ""])
          (if (zero? (ir:trun-spacing r)) ""
              (format " spc=\"~a\"" (inexact->exact (round (* 100 (ir:trun-spacing r))))))
          (if (zero? (ir:trun-baseline r)) ""
              (format " baseline=\"~a\"" (pct* (ir:trun-baseline r))))
          (clr (ir-rgba (ir:trun-color r)))
          (xml-escape (ir:trun-family r)) (xml-escape (ir:trun-family r))
          tag))

(define (ir-rgba c)
  (rgba* (ir:rgba-r c) (ir:rgba-g c) (ir:rgba-b c) (ir:rgba-a c)))

(define (body-or-empty body)
  (if (and body (not (ir:text-body-empty? body))) (txbody-xml body) EMPTY-TX))

;; ---------------------------------------------------- items with structure

;; A named preset, so PowerPoint draws its own version of the shape and the
;; text inside stays reflowable.
(define (preset-item-xml id i)
  (define adjust (it:preset-adjust i))
  (define geom
    (format "<a:prstGeom prst=\"~a\"><a:avLst>~a</a:avLst></a:prstGeom>"
            (xml-escape (it:preset-name i))
            (apply string-append
                   (for/list ([a (in-list adjust)])
                     (format "<a:gd name=\"~a\" fmla=\"~a\"/>"
                             (xml-escape (car a)) (xml-escape (cdr a)))))))
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id (format "Shape ~a" id) #:tag (it:preset-tag i))
          (xfrm-xml (it:preset-x i) (it:preset-y i) (it:preset-w i) (it:preset-h i)
                    (it:preset-rot i) (it:preset-flip-h? i) (it:preset-flip-v? i))
          geom (fill-xml (it:preset-fill i)) (line-xml (it:preset-pen i))
          (body-or-empty (it:preset-body i))))

;; A table, written as one. Flattened into rectangles and text it reads
;; correctly and edits terribly, and the borders a table style draws -- which
;; live in no cell -- are not in the flattening at all, so a table came back
;; without them. Written as `<a:tbl>` the style applies again on the far side
;; and the borders come back on their own.
(define (table-item-xml id i)
  (define cols (it:table-col-widths i))
  (define rows (it:table-row-heights i))
  (define cells (it:table-cells i))
  (format (string-append
           "<p:graphicFrame>"
           "<p:nvGraphicFramePr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
           "<p:cNvGraphicFramePr><a:graphicFrameLocks noGrp=\"1\"/></p:cNvGraphicFramePr>"
           "<p:nvPr/></p:nvGraphicFramePr>"
           "<p:xfrm><a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/></p:xfrm>"
           "<a:graphic><a:graphicData uri=\"~a\">"
                      ;; No style and no banding flags. The style a table named was
           ;; resolved when it was read and each cell carries its own paint
           ;; now, so asking for a style here paints it a second time.
           "<a:tbl><a:tblPr/><a:tblGrid>~a</a:tblGrid>~a</a:tbl>"
           "</a:graphicData></a:graphic></p:graphicFrame>")
          id (xml-escape (editor-name (it:table-tag i) (format "Table ~a" id)))
          (if (it:table-tag i)
              (format " descr=\"glide-pptx:~a\"" (xml-escape (it:table-tag i)))
              "")
          (emu (it:table-x i)) (emu (it:table-y i))
          (max 1 (emu (it:table-w i))) (max 1 (emu (it:table-h i)))
          "http://schemas.openxmlformats.org/drawingml/2006/table"
          (apply string-append
                 (for/list ([w (in-list cols)]) (format "<a:gridCol w=\"~a\"/>" (max 1 (emu w)))))
          (apply string-append
                 (for/list ([row (in-list cells)] [h (in-list rows)])
                   (format "<a:tr h=\"~a\">~a</a:tr>" (max 1 (emu h))
                           (apply string-append (map cell-xml row)))))))

(define (cell-body-xml body)
  (regexp-replace* #rx"(</?)p:txBody" (body-or-empty body) "\\1a:txBody"))

(define (cell-xml c)
  (define span (it:cell-col-span c))
  (define rspan (it:cell-row-span c))
  (define merged (it:cell-merged? c))
  (format "<a:tc~a~a~a>~a<a:tcPr>~a~a</a:tcPr></a:tc>"
          (if (and span (> span 1)) (format " gridSpan=\"~a\"" span) "")
          (if (and rspan (> rspan 1)) (format " rowSpan=\"~a\"" rspan) "")
          ;; A cell covered by the one that spans it says so and holds nothing.
          (case merged
            [("1" #t) " hMerge=\"1\""]
            [else ""])
          ;; A cell's text body is `a:txBody`, not the `p:txBody` a shape
          ;; carries. Written with a shape's name it is not read at all, and
          ;; the table comes back with its borders and none of its words.
          (cell-body-xml (it:cell-body c))
          ;; The one line a cell carries is the line on all four of its sides:
          ;; that is what it was read from, and a table whose cells state their
          ;; own borders states the same one on each.
          (let ([pen (it:cell-pen c)])
            (if pen
                (apply string-append
                       (for/list ([side (in-list '("lnL" "lnR" "lnT" "lnB"))])
                         (line-xml/name side pen)))
                ""))
          (fill-xml (it:cell-fill c))))

(define (textbox-item-xml id i)
  (format (string-append "<p:sp>~a<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/>"
                         "</a:prstGeom><a:noFill/><a:ln><a:noFill/></a:ln></p:spPr>~a</p:sp>")
          (nv-xml id (format "TextBox ~a" id) #:tag (it:textbox-tag i))
          (xfrm-xml (it:textbox-x i) (it:textbox-y i) (it:textbox-w i) (it:textbox-h i)
                    (it:textbox-rot i))
          (body-or-empty (it:textbox-body i))))

;; Custom geometry keeps its drawn path but not its name, and still carries text.
(define EMU-PT (/ 1.0 12700.0))

(define (shape-path-item-xml id i)
  (define segs (it:shape-path-segs i))
  ;; The shape's own box, not the path's extent. A curve's control points can
  ;; sit outside the box, and OOXML allows a path coordinate outside 0..w -- the
  ;; decks that draw connectors this way rely on it -- so taking the extent
  ;; instead moved and grew the shape a little on every round trip. The rotation
  ;; was dropped outright, which turned a rotated freeform upright.
  (define-values (bx by bw0 bh0) (apply values (it:shape-path-box i)))
  ;; A zero extent is still worth avoiding; one EMU is what PowerPoint writes.
  (define bw (if (< bw0 EMU-PT) EMU-PT bw0))
  (define bh (if (< bh0 EMU-PT) EMU-PT bh0))
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id (format "Freeform ~a" id) #:tag (it:shape-path-tag i))
          (xfrm-xml bx by bw bh (it:shape-path-rot i)
                    (it:shape-path-flip-h? i) (it:shape-path-flip-v? i))
          (path-geom-xml segs bx by bw bh)
          (fill-xml (it:shape-path-fill i)) (line-xml (it:shape-path-pen i))
          (body-or-empty (it:shape-path-body i))))

;; A picture from a file on disk keeps its original encoding rather than being
;; round-tripped through pixels.
(define (picture-item-xml id i rid)
  (format (string-append "<p:pic><p:nvPicPr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
                         "<p:cNvPicPr/><p:nvPr/></p:nvPicPr>"
                         "<p:blipFill><a:blip r:embed=\"~a\">~a</a:blip>~a"
                         "<a:stretch><a:fillRect/></a:stretch></p:blipFill>"
                         "<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>~a</p:spPr>"
                         "</p:pic>")
          id (xml-escape (editor-name (it:picture-tag i) (format "Picture ~a" id)))
          (if (it:picture-tag i)
              (format " descr=\"glide-pptx:~a\"" (xml-escape (it:picture-tag i))) "")
          rid
          ;; A washed-out picture is an alphaModFix on the blip, not a fill.
          (let ([o (it:picture-opacity i)])
            (if (< o 0.999) (format "<a:alphaModFix amt=\"~a\"/>" (pct o)) ""))
          (let ([c (it:picture-crop i)])
            ;; Signed, because a crop edge is: a negative one extends the source
            ;; rectangle rather than eating into it, and clamping it to zero
            ;; wrote the picture back uncropped on that edge.
            (if (and c (ormap (lambda (v) (> (abs v) 1e-9)) c))
                (format "<a:srcRect l=\"~a\" t=\"~a\" r=\"~a\" b=\"~a\"/>"
                        (pct* (first c)) (pct* (second c))
                        (pct* (third c)) (pct* (fourth c)))
                ""))
          (xfrm-xml (it:picture-x i) (it:picture-y i) (it:picture-w i) (it:picture-h i)
                    (it:picture-rot i) (it:picture-flip-h? i) (it:picture-flip-v? i))
          (if (it:picture-pen i) (line-xml (it:picture-pen i)) "")))

;; ------------------------------------------------------------------ images

(define (image-item-xml id i rid)
  (format (string-append "<p:pic><p:nvPicPr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
                         "<p:cNvPicPr/><p:nvPr/></p:nvPicPr>"
                         "<p:blipFill><a:blip r:embed=\"~a\"/>"
                         "<a:stretch><a:fillRect/></a:stretch></p:blipFill>"
                         "<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></p:spPr>"
                         "</p:pic>")
          id
          (xml-escape (editor-name (it:image-tag i) (format "Picture ~a" id)))
          (if (it:image-tag i)
              (format " descr=\"glide-pptx:~a\"" (xml-escape (it:image-tag i))) "")
          rid
          (xfrm-xml (it:image-x i) (it:image-y i) (it:image-w i) (it:image-h i)
                    (it:image-rot i))))

;; ------------------------------------------------------------------- slide

;; Returns (values slide-xml image-parts) where each image part is
;; (list part-name relationship-id bytes-writer).
(define (slide-xml page index)
  ;; Ids are unique within a slide and start after 1, which the group frame
  ;; takes. Images are collected as they are met so the relationship part can be
  ;; written alongside.
  (define images '())
  (define next-id (box 1))
  (define (fresh-id!) (set-box! next-id (add1 (unbox next-id))) (unbox next-id))

  (define (register-image! i stem ext)
    (define k (add1 (length images)))
    (define rid (format "rId~a" (+ 1 k)))
    (define name (format "ppt/media/~a~a-~a~a" stem index k ext))
    (set! images (cons (list name rid i) images))
    rid)
  ;; A fill that is a picture registers its file the same way, and the same
  ;; registrar counts both so the ids do not collide.
  (define (register-fill! f stem ext) (register-image! f stem ext))

  ;; Recursive, so a group's children are written the same way its siblings are.
  (define (item->xml i)
    (define n (fresh-id!))
    (cond
      [(it:rect? i) (rect-item-xml n i)]
      [(it:ellipse? i) (ellipse-item-xml n i)]
      [(it:path? i) (path-item-xml n i)]
      [(it:text? i) (text-item-xml n i)]
      [(it:image? i) (image-item-xml n i (register-image! i "image" ".png"))]
      [(it:group? i) (group-item-xml n i item->xml)]
      [(it:table? i) (table-item-xml n i)]
      [(it:preset? i) (preset-item-xml n i)]
      [(it:textbox? i) (textbox-item-xml n i)]
      [(it:shape-path? i) (shape-path-item-xml n i)]
      [(it:picture? i)
       (picture-item-xml n i (register-image! i "pic" (picture-extension i)))]
      [else ""]))

  (define shapes
    (parameterize ([current-image-register register-fill!])
      (for/list ([i (in-list (display-page-items page))]) (item->xml i))))
  (parameterize ([current-image-register register-fill!])
  (values
   (format (string-append xml-decl
                          ;; `show="0"` is a slide the editor was told to skip.
                          "<p:sld xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\"~a>"
                          "<p:cSld>~a<p:spTree>"
                          "<p:nvGrpSpPr><p:cNvPr id=\"1\" name=\"\"/><p:cNvGrpSpPr/>"
                          "<p:nvPr/></p:nvGrpSpPr>"
                          "<p:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/>"
                          "<a:ext cx=\"0\" cy=\"0\"/><a:chOff x=\"0\" y=\"0\"/>"
                          "<a:chExt cx=\"0\" cy=\"0\"/></a:xfrm></p:grpSpPr>"
                          "~a</p:spTree></p:cSld>"
                          "<p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>")
           NS-A NS-R NS-P
           (if (display-page-hidden? page) " show=\"0\"" "")
           ;; A page-wide fill is the slide's background, which is where
           ;; PowerPoint keeps it -- not an extra shape on the slide.
           (let ([bg (display-page-background page)])
             (if bg (format "<p:bg><p:bgPr>~a<a:effectLst/></p:bgPr></p:bg>" (fill-xml bg)) ""))
           (apply string-append shapes))
   (reverse images))))

;; A picture whose relationship resolved to nothing has no source and so no
;; extension to read; a blank png stands in for it.
(define (picture-extension i) (src-extension (it:picture-src i)))

(define (src-extension src)
  (define p (and src (if (path? src) src (string->path src))))
  (define e (and p (path-get-extension p)))
  (define e* (if e (string-downcase (bytes->string/utf-8 e)) ".png"))
  (if (member e* '(".png" ".jpg" ".jpeg" ".gif")) e* ".png"))

;; A group, written as one. Its children are already in the slide's
;; coordinates, so the child space is the group's own box.
(define (group-item-xml id i emit-child)
  (format (string-append "<p:grpSp><p:nvGrpSpPr>"
                         "<p:cNvPr id=\"~a\" name=\"~a\"~a/><p:cNvGrpSpPr/><p:nvPr/>"
                         "</p:nvGrpSpPr>"
                         "<p:grpSpPr><a:xfrm~a>"
                         "<a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/>"
                         "<a:chOff x=\"~a\" y=\"~a\"/><a:chExt cx=\"~a\" cy=\"~a\"/>"
                         "</a:xfrm></p:grpSpPr>~a</p:grpSp>")
          id
          (xml-escape (editor-name (it:group-tag i) (format "Group ~a" id)))
          (if (it:group-tag i)
              (format " descr=\"glide-pptx:~a\"" (xml-escape (it:group-tag i))) "")
          (string-append
           (if (zero? (it:group-rot i)) ""
               (format " rot=\"~a\"" (angle-60k (it:group-rot i))))
           (if (it:group-flip-h? i) " flipH=\"1\"" "")
           (if (it:group-flip-v? i) " flipV=\"1\"" ""))
          (emu (it:group-x i)) (emu (it:group-y i))
          (max 1 (emu (it:group-w i))) (max 1 (emu (it:group-h i)))
          (emu (it:group-x i)) (emu (it:group-y i))
          (max 1 (emu (it:group-w i))) (max 1 (emu (it:group-h i)))
          (apply string-append (map emit-child (it:group-items i)))))

;; ---------------------------------------------------------------- package

(define xml-decl "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
(define NS-A "http://schemas.openxmlformats.org/drawingml/2006/main")
(define NS-R "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(define NS-P "http://schemas.openxmlformats.org/presentationml/2006/main")
(define REL "http://schemas.openxmlformats.org/officeDocument/2006/relationships")

(define (content-types slide-count image-names)
  (format (string-append xml-decl
                         "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
                         "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
                         "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
                         "<Default Extension=\"png\" ContentType=\"image/png\"/>"
                         "<Default Extension=\"jpg\" ContentType=\"image/jpeg\"/>"
                         "<Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/>"
                         "<Default Extension=\"gif\" ContentType=\"image/gif\"/>"
                         "<Override PartName=\"/ppt/presentation.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml\"/>"
                         "<Override PartName=\"/ppt/slideMasters/slideMaster1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml\"/>"
                         "<Override PartName=\"/ppt/slideLayouts/slideLayout1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml\"/>"
                         "<Override PartName=\"/ppt/theme/theme1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.theme+xml\"/>"
                         "~a</Types>")
          (apply string-append
                 (for/list ([i (in-range 1 (add1 slide-count))])
                   (format "<Override PartName=\"/ppt/slides/slide~a.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slide+xml\"/>" i)))))

(define root-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/officeDocument\" Target=\"ppt/presentation.xml\"/>"
                         "</Relationships>")
          REL))

(define (presentation-xml n width height)
  (format (string-append xml-decl
                         "<p:presentation xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\">"
                         "<p:sldMasterIdLst><p:sldMasterId id=\"2147483648\" r:id=\"rId1\"/></p:sldMasterIdLst>"
                         "<p:sldIdLst>~a</p:sldIdLst>"
                         "<p:sldSz cx=\"~a\" cy=\"~a\"/><p:notesSz cx=\"~a\" cy=\"~a\"/>"
                         "</p:presentation>")
          NS-A NS-R NS-P
          (apply string-append
                 (for/list ([i (in-range 1 (add1 n))])
                   (format "<p:sldId id=\"~a\" r:id=\"rId~a\"/>" (+ 255 i) (+ 1 i))))
          (emu width) (emu height) (emu height) (emu width)))

(define (presentation-rels n)
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideMaster\" Target=\"slideMasters/slideMaster1.xml\"/>"
                         "~a"
                         "<Relationship Id=\"rId~a\" Type=\"~a/theme\" Target=\"theme/theme1.xml\"/>"
                         "</Relationships>")
          REL
          (apply string-append
                 (for/list ([i (in-range 1 (add1 n))])
                   (format "<Relationship Id=\"rId~a\" Type=\"~a/slide\" Target=\"slides/slide~a.xml\"/>"
                           (+ 1 i) REL i)))
          (+ n 2) REL))

(define (blank-sp-tree [bg ""])
  (string-append "<p:cSld>" bg "<p:spTree>"
                 "<p:nvGrpSpPr><p:cNvPr id=\"1\" name=\"\"/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>"
                 "<p:grpSpPr/></p:spTree></p:cSld>"))

;; The master and the layout both say white. A slide writes its own background,
;; so this is only what an unstated one inherits -- but "unstated" is not the
;; same as "white", and a consumer that does not assume white paints the slide
;; black. Saying it is one line and it removes the guess.
(define white-background
  (string-append "<p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"FFFFFF\"/></a:solidFill>"
                 "<a:effectLst/></p:bgPr></p:bg>"))

(define master-xml
  (format (string-append xml-decl
                         "<p:sldMaster xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\">~a"
                         "<p:clrMap bg1=\"lt1\" tx1=\"dk1\" bg2=\"lt2\" tx2=\"dk2\""
                         " accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\""
                         " accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\""
                         " hlink=\"hlink\" folHlink=\"folHlink\"/>"
                         "<p:sldLayoutIdLst><p:sldLayoutId id=\"2147483649\" r:id=\"rId1\"/>"
                         "</p:sldLayoutIdLst></p:sldMaster>")
          NS-A NS-R NS-P (blank-sp-tree white-background)))

(define master-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
                         "<Relationship Id=\"rId2\" Type=\"~a/theme\" Target=\"../theme/theme1.xml\"/>"
                         "</Relationships>")
          REL REL))

(define layout-xml
  (format (string-append xml-decl
                         "<p:sldLayout xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\""
                         " type=\"blank\" preserve=\"1\">~a"
                         "<p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sldLayout>")
          NS-A NS-R NS-P (blank-sp-tree white-background)))

(define layout-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/>"
                         "</Relationships>")
          REL))

(define (slide-rels image-parts)
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
                         "~a</Relationships>")
          REL
          (apply string-append
                 (for/list ([p (in-list image-parts)])
                   (format "<Relationship Id=\"~a\" Type=\"~a/image\" Target=\"../media/~a\"/>"
                           (second p) REL (file-name-from-path (first p)))))))

;; A theme is mandatory. This one is minimal but complete enough to satisfy
;; PowerPoint's schema, and nothing on our slides refers to it.
(define (font-entry tag face) (format "<a:~a typeface=\"~a\"/>" tag face))
(define theme-xml
  (let* ([scheme-colors
          (string-append
           "<a:dk1><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:dk1>"
           "<a:lt1><a:sysClr val=\"window\" lastClr=\"FFFFFF\"/></a:lt1>"
           "<a:dk2><a:srgbClr val=\"44546A\"/></a:dk2><a:lt2><a:srgbClr val=\"E7E6E6\"/></a:lt2>"
           "<a:accent1><a:srgbClr val=\"4472C4\"/></a:accent1>"
           "<a:accent2><a:srgbClr val=\"ED7D31\"/></a:accent2>"
           "<a:accent3><a:srgbClr val=\"A5A5A5\"/></a:accent3>"
           "<a:accent4><a:srgbClr val=\"FFC000\"/></a:accent4>"
           "<a:accent5><a:srgbClr val=\"5B9BD5\"/></a:accent5>"
           "<a:accent6><a:srgbClr val=\"70AD47\"/></a:accent6>"
           "<a:hlink><a:srgbClr val=\"0563C1\"/></a:hlink>"
           "<a:folHlink><a:srgbClr val=\"954F72\"/></a:folHlink>")]
         [fonts (string-append
                 "<a:majorFont>" (font-entry 'latin "Calibri Light")
                 (font-entry 'ea "") (font-entry 'cs "") "</a:majorFont>"
                 "<a:minorFont>" (font-entry 'latin "Calibri")
                 (font-entry 'ea "") (font-entry 'cs "") "</a:minorFont>")]
         [fill "<a:solidFill><a:schemeClr val=\"phClr\"/></a:solidFill>"]
         [ln (string-append "<a:ln w=\"6350\" cap=\"flat\" cmpd=\"sng\" algn=\"ctr\">"
                            fill "<a:prstDash val=\"solid\"/></a:ln>")])
    (format (string-append xml-decl
                           "<a:theme xmlns:a=\"~a\" name=\"glide-pptx\"><a:themeElements>"
                           "<a:clrScheme name=\"glide-pptx\">~a</a:clrScheme>"
                           "<a:fontScheme name=\"glide-pptx\">~a</a:fontScheme>"
                           "<a:fmtScheme name=\"glide-pptx\">"
                           "<a:fillStyleLst>~a~a~a</a:fillStyleLst>"
                           "<a:lnStyleLst>~a~a~a</a:lnStyleLst>"
                           "<a:effectStyleLst>~a~a~a</a:effectStyleLst>"
                           "<a:bgFillStyleLst>~a~a~a</a:bgFillStyleLst>"
                           "</a:fmtScheme></a:themeElements></a:theme>")
            NS-A scheme-colors fonts
            fill fill fill ln ln ln
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            fill fill fill)))

;; ------------------------------------------------------------------ writing

;; Writes `pages` as a .pptx at `path`. All pages take the size of the first.
(define (display-pages->pptx pages path)
  (when (null? pages) (error 'display-pages->pptx "no pages"))
  (display-page-sequence->pptx
   (in-list pages) (length pages)
   (display-page-width (first pages)) (display-page-height (first pages)) path))

;; The streaming form used by `picts->pptx`: one page is adapted, encoded and
;; released before the next is made. A long animated talk can otherwise retain
;; the raw ARGB of every flattened fallback until the final zip -- several
;; gigabytes for 149 full-HD epochs, enough for racket/draw's next PNG encoder
;; to fail with an invalid native-memory reference.
(define (display-page-sequence->pptx pages page-count width height path)
  (when (zero? page-count) (error 'display-page-sequence->pptx "no pages"))
  (define dir (make-temporary-file "pptx-out~a" 'directory))
  (define (put! name content)
    (define full (build-path dir (string->path name)))
    (make-directory* (path-only full))
    (if (bytes? content)
        (call-with-output-file full #:exists 'replace (lambda (o) (write-bytes content o)))
        (call-with-output-file full #:exists 'replace (lambda (o) (write-string content o)))))
  (define all-images '())
  (define names '())
  (define (add! name content) (put! name content) (set! names (cons name names)))
  (for ([page pages] [i (in-naturals 1)])
    (define-values (xml images) (slide-xml page i))
    (add! (format "ppt/slides/slide~a.xml" i) xml)
    (add! (format "ppt/slides/_rels/slide~a.xml.rels" i) (slide-rels images))
    (for ([im (in-list images)])
      (add! (first im) (item-bytes (third im)))
      (set! all-images (cons (first im) all-images))))
  (add! "[Content_Types].xml" (content-types page-count all-images))
  (add! "_rels/.rels" root-rels)
  (add! "ppt/presentation.xml" (presentation-xml page-count width height))
  (add! "ppt/_rels/presentation.xml.rels" (presentation-rels page-count))
  (add! "ppt/slideMasters/slideMaster1.xml" master-xml)
  (add! "ppt/slideMasters/_rels/slideMaster1.xml.rels" master-rels)
  (add! "ppt/slideLayouts/slideLayout1.xml" layout-xml)
  (add! "ppt/slideLayouts/_rels/slideLayout1.xml.rels" layout-rels)
  (add! "ppt/theme/theme1.xml" theme-xml)
  (define target (path->complete-path path))
  ;; The folder a deck is asked for in may not be there: `raco glide` keeps the
  ;; deck in `.glide`, and a session that finds one left by a session that did
  ;; not finish clears it -- folder and all -- before writing the deck again.
  ;; `zip` opens the file and does not make the way to it, so this did.
  (let ([into (path-only target)]) (when into (make-directory* into)))
  (when (file-exists? target) (delete-file target))
  (parameterize ([current-directory dir])
    (apply zip target (map string->path (reverse names))))
  (delete-directory/files dir #:must-exist? #f)
  target)

;; A picture item names a file, so its original encoding is kept; a bitmap item
;; only has pixels, and has to be encoded.
(define (item-bytes i)
  (cond
    [(it:picture? i) (src-bytes (it:picture-src i))]
    [(fill:image? i) (src-bytes (fill:image-src i))]
    [else (png-bytes i)]))

(define (src-bytes src)
  (if (and src (file-exists? src))
      (file->bytes src)
      (begin (warn! "image ~a is missing; a blank stands in" src)
             (blank-png))))

(define (blank-png)
  (define bm (make-bitmap 1 1))
  (define o (open-output-bytes))
  (send bm save-file o 'png)
  (get-output-bytes o))

;; Small images take the native bitmap path, which converts channels much faster
;; than a Racket loop. Very large flattened fallbacks do not: creating a second
;; full-size Cairo surface for a 3840x2160 page intermittently failed in
;; `save-file` on a 149-epoch talk. If a smaller native encode ever fails for
;; the same reason, retry it through the bounded direct path too.
(define DIRECT-PNG-PIXELS 4000000)

(define (png-bytes i)
  (cond
    [(> (* (it:image-src-w i) (it:image-src-h i)) DIRECT-PNG-PIXELS)
     (png-bytes/direct i)]
    [else
     (with-handlers ([exn:fail? (lambda (_exn) (png-bytes/direct i))])
       (png-bytes/bitmap i))]))

(define (png-bytes/bitmap i)
  (define w (it:image-src-w i)) (define h (it:image-src-h i))
  (define bm (make-bitmap w h))
  (send bm set-argb-pixels 0 0 w h (it:image-argb i))
  (define o (open-output-bytes))
  (send bm save-file o 'png)
  (get-output-bytes o))

;; Direct libpng encoding still needs RGBA rows, but no second Cairo surface.
(define (png-bytes/direct i)
  (define w (it:image-src-w i)) (define h (it:image-src-h i))
  (define argb (it:image-argb i))
  (define stride (* 4 w))
  (define rows
    (for/vector #:length h ([j (in-range h)])
      (define row (make-bytes stride))
      (define src-row (* j stride))
      (for ([x (in-range w)])
        (define src (+ src-row (* 4 x)))
        (define dst (* 4 x))
        (bytes-set! row dst (bytes-ref argb (+ src 1)))
        (bytes-set! row (+ dst 1) (bytes-ref argb (+ src 2)))
        (bytes-set! row (+ dst 2) (bytes-ref argb (+ src 3)))
        (bytes-set! row (+ dst 3) (bytes-ref argb src)))
      row))
  (define o (open-output-bytes))
  (define writer (create-png-writer o w h #f #t))
  (dynamic-wind
    void
    (lambda () (write-png writer rows))
    (lambda () (destroy-png-writer writer)))
  (get-output-bytes o))
