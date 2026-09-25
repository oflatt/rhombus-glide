#lang racket/base
;; Unit tests for the pieces that are pure functions of the file format.
(require rackunit/log)
(require rackunit racket/list racket/class racket/draw
         racket/file racket/string racket/runtime-path
         glide-pptx/units glide-pptx/xml-util glide-pptx/ir
         glide-pptx/theme glide-pptx/geometry glide-pptx/opc
         glide-pptx/parse "deck-edit.rkt")

(define-runtime-path decks-dir "decks")

;; ------------------------------------------------------------------- units

(check-= (emu->pt 914400) 72.0 1e-9 "an inch is 72 points")
(check-= (emu->pt 12700) 1.0 1e-9 "12700 EMU is one point")
(check-= (hundredths->pt 4400) 44.0 1e-9 "font sizes are hundredths of a point")
(check-= (angle->degrees 1200000) 20.0 1e-9 "rotations are 60000ths of a degree")
(check-= (percent->fraction 20000) 0.2 1e-9 "percentages are 1000ths of a percent")
(check-= (string->percent "50%") 0.5 1e-9 "a literal percent sign is accepted too")
(check-equal? (string->bool "1") #t)
(check-equal? (string->bool "false") #f)
(check-equal? (string->bool #f 'fallback) 'fallback "an absent attribute takes the default")

;; --------------------------------------------------------------- xml-util

(define slide-xml
  '(p:sld ((xmlns:a "urn:a"))
          (p:cSld ()
                  (p:spTree ()
                            (p:sp () (p:spPr () (a:xfrm ((rot "1200000"))
                                                        (a:off ((x "914400") (y "0")))
                                                        (a:ext ((cx "914400") (cy "457200"))))))
                            (p:sp () (p:txBody () (a:p () (a:r () (a:t () "hello"))
                                                       (a:br ())
                                                       (a:r () (a:t () "world")))))))))

(check-equal? (local-name 'a:off) 'off "the prefix is ignored")
(check-equal? (local-name 'off) 'off "an unprefixed name is its own local name")
(define tree (xpath slide-xml 'cSld 'spTree))
(check-equal? (length (children tree 'sp)) 2)
(check-equal? (attr (xpath (first (children tree 'sp)) 'spPr 'xfrm 'off) 'x) "914400")
(check-equal? (all-text (xpath (second (children tree 'sp)) 'txBody))
              "hello\nworld"
              "<a:br/> reads as a newline")

;; An element with both an unprefixed and a namespaced attribute of the same
;; local name: the relationship is the namespaced one.
(define sld-id '(p:sldId ((id "256") (r:id "rId7"))))
(check-equal? (attr sld-id 'id) "256")
(check-equal? (attr-ns sld-id 'id) "rId7" "attr-ns only matches a prefixed name")

;; ---------------------------------------------------- relationship targets

(check-equal? (resolve-part-name "ppt/slides/slide1.xml" "../media/image1.png")
              "ppt/media/image1.png")
(check-equal? (resolve-part-name "ppt/slides/slide1.xml" "../slideLayouts/slideLayout2.xml")
              "ppt/slideLayouts/slideLayout2.xml")
(check-equal? (resolve-part-name "ppt/presentation.xml" "slides/slide1.xml")
              "ppt/slides/slide1.xml")
(check-equal? (resolve-part-name "" "ppt/presentation.xml") "ppt/presentation.xml"
              "the package root has no directory to resolve against")
(check-equal? (resolve-part-name "ppt/presentation.xml" "/ppt/theme/theme1.xml")
              "ppt/theme/theme1.xml"
              "a leading slash means package-absolute")

;; -------------------------------------------------------------- theme colors

(define theme-xml
  '(a:theme ()
            (a:themeElements ()
                             (a:clrScheme ()
                                          (a:dk1 () (a:sysClr ((val "windowText")
                                                               (lastClr "000000"))))
                                          (a:lt1 () (a:sysClr ((val "window")
                                                               (lastClr "FFFFFF"))))
                                          (a:accent1 () (a:srgbClr ((val "4472C4")))))
                             (a:fontScheme ()
                                           (a:majorFont () (a:latin ((typeface "Calibri Light"))))
                                           (a:minorFont () (a:latin ((typeface "Calibri"))))))))

(define th (parse-theme theme-xml))
(define ctx (clr-ctx th (hash 'tx1 'dk1 'bg1 'lt1 'accent1 'accent1) #f))

(check-equal? (resolve-color ctx '(a:srgbClr ((val "FF8000")))) (rgb 255 128 0))
(check-equal? (resolve-color ctx '(a:schemeClr ((val "accent1")))) (rgb 68 114 196)
              "a scheme color resolves through the color map into the theme")
(check-equal? (resolve-color ctx '(a:schemeClr ((val "tx1")))) black
              "tx1 maps to dk1, whose sysClr carries its last known value")
(check-equal? (resolve-color ctx '(a:prstClr ((val "red")))) (rgb 255 0 0))
(check-= (rgba-a (resolve-color ctx '(a:srgbClr ((val "000000")) (a:alpha ((val "40000"))))))
         0.4 1e-6 "alpha is a percentage")

;; A tint lightens toward white and a shade darkens toward black.
(let ([tinted (resolve-color ctx '(a:srgbClr ((val "000000")) (a:tint ((val "50000")))))]
      [shaded (resolve-color ctx '(a:srgbClr ((val "FFFFFF")) (a:shade ((val "50000")))))])
  (check-= (rgba-r tinted) 127.5 0.6)
  (check-= (rgba-r shaded) 127.5 0.6))

(check-equal? (theme-latin-font th 'minor) "Calibri")
(check-equal? (resolve-typeface th "+mj-lt") "Calibri Light"
              "+mj-lt is the theme's major latin font")
(check-equal? (resolve-typeface th "Arial") "Arial")

;; A color transform we do not model must not change the color, rather than error.
(check-equal? (resolve-color ctx '(a:srgbClr ((val "112233")) (a:gamma ())))
              (rgb 17 34 51))

;; ------------------------------------------------------------------ geometry

(check-true (preset-known? "roundRect"))
(check-true (preset-known? "star5"))
(check-true (preset-known? "rightArrow"))
(check-false (preset-known? "notAShapeName"))
(check-false (geometry-closed? "line") "a line cannot be filled")
(check-true (geometry-closed? "rect"))

;; Every registered preset must produce a path inside its own box.
(for ([name (in-list (preset-names))])
  (define p (preset-path name 100.0 60.0 '()))
  (define-values (x y w h) (send p get-bounding-box))
  (check-true (and (>= x -0.51) (>= y -0.51) (<= (+ x w) 100.51) (<= (+ y h) 60.51))
              (format "~a stays within its box: ~a ~a ~a ~a" name x y w h)))

;; An unknown preset falls back to the full rectangle rather than failing.
(let ()
  (define p (preset-path "someShapeWeHaveNotMet" 40.0 20.0 '()))
  (define-values (x y w h) (send p get-bounding-box))
  (check-= w 40.0 1e-6)
  (check-= h 20.0 1e-6))

;; Flipping mirrors the geometry within the same box.
(let ()
  (define plain (preset-path "rightArrow" 100.0 50.0 '()))
  (define flipped (preset-path "rightArrow" 100.0 50.0 '() #:flip-h? #t))
  (define-values (x0 y0 w0 h0) (send plain get-bounding-box))
  (define-values (x1 y1 w1 h1) (send flipped get-bounding-box))
  (check-= w1 w0 1e-6 "a flip does not change the width")
  (check-= h1 h0 1e-6 "a flip does not change the height"))

;; --------------------------------------------------------------------- ir

(check-equal? (rgba-hex (rgb 68 114 196)) "4472C4")
(check-equal? (rgba-hex (rgb 0 0 0)) "000000" "single digits are padded")
(let-values ([(cx cy) (bbox-center (make-bbox 10.0 20.0 100.0 50.0))])
  (check-= cx 60.0 1e-9)
  (check-= cy 45.0 1e-9))
(check-true (text-body-empty? #f))
(check-true (text-body-empty?
             (text-body (list (para (list (trun "" "Calibri" 18.0 #f #f #f #f black 0.0 'none 0.0
                                                'all))
                                    'left 0 0.0 0.0 '(percent . 1.0) 0.0 0.0 no-bullet 'all))
                        'top #f #t 'none default-insets 0.0 'all)))

;; A deck round-trips through `write`/`read`, which is what golden IR relies on.
(let* ([d (deck 960.0 540.0
                (list (slide 1 "One" 960.0 540.0 #f '()
                             (list (shape 2 "Box" (make-bbox 0.0 0.0 10.0 10.0)
                                          (preset-geom "rect" '())
                                          (solid-fill (rgb 1 2 3)) #f #f #f))
                             #f #f))
                "/tmp" "x.pptx")]
       [again (read (open-input-string (format "~s" d)))])
  (check-equal? again d "prefab structs survive a write/read round trip"))

;; ------------------------------------------------------- dc state hygiene

;; pict's `dc` contract rejects a draw procedure that leaves the dc changed, so
;; simply constructing and drawing these is the test. Both cases were real bugs
;; found on a deck with outlined pictures: a strikethrough run set a pen, and an
;; outlined picture set a brush, and neither put it back.
(let ()
  (local-require glide-pptx/runtime pict)
  (define (render p)
    (define bm (make-bitmap (max 1 (inexact->exact (ceiling (pict-width p))))
                            (max 1 (inexact->exact (ceiling (pict-height p))))))
    (draw-pict p (new bitmap-dc% [bitmap bm]) 0 0)
    #t)
  (define struck
    (textbox #:width 120.0 #:height 30.0
             (para* (run* "struck" #:size 14.0 #:strike? #t))))
  (check-true (render struck) "a strikethrough run leaves the dc as it found it")
  (define outlined
    (shape-pict #:width 60.0 #:height 30.0 #:fill (hex "4472C4")
                #:line (make-stroke (hex "202020") #:width 1.0)
                #:body (body* (para* (run* "x" #:size 10.0 #:strike? #t)))))
  (check-true (render (vc-append 4 struck outlined)) "and so does a composition of them")
  (check-true (render (slide-canvas #:width 200.0 #:height 100.0
                                    (at 10.0 10.0 struck)
                                    (at 10.0 50.0 outlined)))
              "and a whole slide"))

;; ------------------------------------------------- charts and SmartArt refuse

;; A chart or a SmartArt diagram has no representation here, so on a round trip
;; it would come back as an empty box -- the content silently gone. Refusing is
;; the only honest answer; `--allow-unsupported` is for looking, not syncing.
(let ()
  (define src (build-path decks-dir "03-shapes.pptx"))
  (define deck (build-path (find-system-path 'temp-dir) "glide-pptx-chart.pptx"))
  (copy-file src deck #t)
  ;; A graphicFrame holding a diagram, which is what SmartArt is on disk.
  (define frame
    (string-append
     "<p:graphicFrame><p:nvGraphicFramePr>"
     "<p:cNvPr id=\"99\" name=\"Diagram 99\"/><p:cNvGraphicFramePr/><p:nvPr/>"
     "</p:nvGraphicFramePr>"
     "<p:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"1000000\" cy=\"1000000\"/></p:xfrm>"
     "<a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/diagram\">"
     "</a:graphicData></a:graphic></p:graphicFrame>"))
  (with-unpacked-deck
   deck
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" "slide1.xml"))
     (define d (file->string part))
     (display-to-file (string-replace d "</p:spTree>" (string-append frame "</p:spTree>"))
                      part #:exists 'replace)))

  (define work (make-temporary-file "chartwork~a" 'directory))
  (check-exn #rx"chart or diagram"
             (lambda () (pptx->deck deck #:workdir work))
             "a diagram is refused by default")
  (check-true (parameterize ([current-allow-unsupported? #t])
                (deck? (pptx->deck deck #:workdir work)))
              "and read as an empty box only when that is asked for")
  (delete-directory/files work #:must-exist? #f)
  (delete-file deck))

(printf "unit tests done\n")

;; ------------------------------------------- generated code has to reparse

;; A sync reads the program's source to find the literals it may rewrite, so
;; every program we emit has to be readable by the shrubbery reader. That is not
;; implied by the program *running*: Rhombus compiled these files fine while the
;; reader rejected them, and the sync failed with `wrong indentation` on a file
;; the emitter had just written.
;;
;; The shape that broke it: an argument column deeper than 48 makes the printer
;; outdent the continuation lines, and it used to leave the first argument up at
;; the aligned column -- so the rest of the arguments sat at a shallower column
;; than the one that set the standard. In Racket that was only ugly. Shrubbery
;; reads indentation, so it was a syntax error.
(let ()
  (local-require glide-pptx/emit-common glide-pptx/emit-rhombus
                 (only-in glide-pptx/sync find-at-sites at-site-tag)
                 (only-in glide-pptx/source-tag source-position-tag automatic-tag?
                          automatic-tag-key automatic-tag-name)
                 (only-in shrubbery/parse parse-all))
  (define (reparses? text)
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (void (parse-all (open-input-string text)))
      #t))

  (define named-tag (source-position-tag "/tmp/talk.rhm" 42 "Readable"))
  (check-true (automatic-tag? named-tag))
  (check-equal? (automatic-tag-name named-tag) "Readable")
  (check-equal? (automatic-tag-key named-tag)
                (source-position-tag "/tmp/talk.rhm" 42)
                "the readable suffix is metadata rather than identity")
  (check-equal? (source-position-tag (string->path "/tmp/talk.rhm") 42)
                (source-position-tag "/tmp/talk.rhm" 42)
                "string and path srcloc sources have the same identity")
  (check-false (automatic-tag? "source:foo")
               "the prefix alone does not reserve an explicit user tag")

  ;; Nested calls, indented far enough in to cross the threshold.
  (define deep
    (v:call "at" (list (v:num 0.0) (v:num 96.35)
                       (kwv "tag" (v:str "Line 5"))
                       (v:call "shape_pict"
                               (list (kwv "width" (v:num 196.75))
                                     (kwv "shape" (v:str "line"))
                                     (kwv "line"
                                          (v:call "make_stroke"
                                                  (list (v:call "hex" (list (v:str "000000")))
                                                        (kwv "width" (v:num 0.5))
                                                        (kwv "dash" (v:sym "dash-dot"))))))))))
  (for ([ind (in-list '(0 2 8 16 17 18 19 20 24 32 40))])
    (define text (string-join (render-lines deep rhombus-flavor ind 88) "\n"))
    (check-true (reparses? text)
                (format "an `at` printed at column ~a reparses:\n~a" ind text)))

  ;; And the whole of every fixture, read the way a sync reads it -- which also
  ;; says the reader understood the structure, not merely that it did not choke:
  ;; every `at` in the file has to come back as a site with an automatic tag,
  ;; while none of that editor bookkeeping appears in the emitted program.
  (for ([name (in-list '("01-placeholders" "02-text" "03-shapes"
                         "04-pictures-groups" "05-realistic"))])
    (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                          #:workdir (make-temporary-file "rp~a" 'directory)))
    (define out (make-temporary-file "rp~a.rhm"))
    (write-rhombus-deck d out #:source-name name)
    (define sites
      (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
        (find-at-sites out)))
    (check-true (list? sites)
                (format "~a emits a program the reader accepts: ~a" name sites))
    (when (list? sites)
      (define source (file->string out))
      (define written (length (regexp-match* #px"(?m:^[ ]*at[(])" source)))
      (check-equal? (length sites) written
                    (format "~a: every `at` written was found again" name))
      (check-true (andmap automatic-tag? (map at-site-tag sites))
                  (format "~a: source locations supply the tags" name))
      (check-false (regexp-match? #rx"~tag:" source)
                   (format "~a: tags are absent from the program" name)))
    (delete-file out)))

;; `spAutoFit` has two representations at once. The file retains the rectangle
;; the author drew plus the auto-fit instruction, while its displayed shape is
;; resized to the natural text extent. Previewing the stored rectangle was the
;; large visible mismatch; writing the displayed rectangle instead made the IR
;; fail to round-trip. Check both halves here without needing LibreOffice.
(let ()
  (local-require glide-pptx/runtime glide-pptx/semantic glide-pptx/draw-ir pict)
  (define stored-w 144.0)
  (define stored-h 187.2)
  (define x 12.0)
  (define y 20.0)
  (define body
    (body* #:anchor 'center #:wrap? #f #:autofit 'grow
           (para* (run* "middle" #:size 16.0 #:font "Carlito"))))
  (define shape-box
    (shape-pict #:width stored-w #:height stored-h #:fill (rgb 220 80 80)
                #:body body))
  (define shape-page
    (pict->page (slide-canvas #:width 400.0 #:height 300.0
                              (at x y shape-box #:tag "shape middle"))
                400.0 300.0))
  (define shape-item (first (display-page-items shape-page)))
  (check-true (it:preset? shape-item) "an inserted text box remains a preset shape")
  (when (it:preset? shape-item)
    (check-= (it:preset-x shape-item) x 0.001 "the stored x is exported")
    (check-= (it:preset-y shape-item) y 0.001 "the stored y is exported")
    (check-= (it:preset-w shape-item) stored-w 0.001 "the stored width is exported")
    (check-= (it:preset-h shape-item) stored-h 0.001 "the stored height is exported"))

  ;; Bounding box of anything not white in a rendered pict.
  (define (ink-bounds p)
    (define tmp (make-temporary-file "autofit~a.png"))
    (pict->png p tmp)
    (define bm (read-bitmap tmp))
    (delete-file tmp)
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define px (make-bytes (* 4 w h)))
    (send bm get-argb-pixels 0 0 w h px)
    (define xs '())
    (define ys '())
    (for* ([j (in-range h)] [i (in-range w)])
      (define o (* 4 (+ i (* j w))))
      (unless (and (> (bytes-ref px (+ o 1)) 245)
                   (> (bytes-ref px (+ o 2)) 245)
                   (> (bytes-ref px (+ o 3)) 245))
        (set! xs (cons i xs))
        (set! ys (cons j ys))))
    (values (apply min xs) (apply min ys)
            (add1 (- (apply max xs) (apply min xs)))
            (add1 (- (apply max ys) (apply min ys)))))
  (define-values (sx sy sw sh) (ink-bounds shape-box))
  (check-true (< sw (* 0.75 stored-w)) "the preview draws the fitted shape width")
  (check-true (< sh (* 0.40 stored-h)) "the preview draws the fitted shape height")
  (check-= (+ sx (/ sw 2.0)) (/ stored-w 2.0) 2.0
           "horizontal auto-fit is centred in the stored rectangle")
  (check-= (+ sy (/ sh 2.0)) (/ stored-h 2.0) 2.0
           "centre-anchored auto-fit is vertically centred")

  ;; A bare textbox has no fill to expose its fitted box, but left-aligned text
  ;; moves from the stored left inset to the centred natural-width box.
  (define text-box
    (textbox #:width stored-w #:height stored-h #:wrap? #f #:autofit 'grow
             (para* (run* "middle" #:size 16.0 #:font "Carlito"))))
  (define-values (tx ty tw th) (ink-bounds text-box))
  (check-true (> tx (* 0.20 stored-w)) "bare auto-fit text is centred horizontally")
  (check-true (< ty (* 0.20 stored-h)) "a top-anchored auto-fit box keeps its top edge"))

;; ------------------------------------------- a leading space is not a wrap

;; Text that starts with spaces is indented by them. They used to be dropped
;; along with the space a line breaks at -- one rule for both -- so a line the
;; deck indents by two spaces was drawn flush left, and every line under it in
;; the same box lined up with the wrong thing.
(let ()
  (local-require glide-pptx/runtime glide-pptx/draw-ir glide-pptx/record-adapt
                 pict racket/list racket/string)
  (define (left-of text #:width [w 2000.0])
    (define box (textbox #:width w #:height 200.0
                         (para* (run* text #:size 40.0 #:font "Liberation Sans"))))
    (define page (pict->display-page (lambda (dc) (draw-pict box dc 0 0)) w 200.0))
    ;; The first thing with ink on it: the spaces are drawn too, as pieces of
    ;; the line, and where they sit is not what is being asked.
    (define ts (sort (for/list ([i (in-list (display-page-items page))]
                                #:when (and (it:text? i)
                                            (not (string=? "" (string-trim (it:text-str i))))))
                       i)
                     < #:key it:text-x))
    (and (pair? ts) (it:text-x (first ts))))
  (define flush (left-of "(a + 1) - 1"))
  (define indented (left-of "  (a + 1) - 1"))
  (check-true (> indented flush)
              (format "two spaces indent the line: ~a against ~a" indented flush))

  ;; And the space a line actually breaks at is still dropped, so wrapped text
  ;; stays flush with the line above it.
  (define (line-lefts text w)
    (define box (textbox #:width w #:height 400.0
                         (para* (run* text #:size 40.0 #:font "Liberation Sans"))))
    (define page (pict->display-page (lambda (dc) (draw-pict box dc 0 0)) w 400.0))
    (define ts (for/list ([i (in-list (display-page-items page))]
                          #:when (and (it:text? i)
                                      (not (string=? "" (string-trim (it:text-str i))))))
                 i))
    (define rows (group-by it:text-y ts))
    (for/list ([r (in-list (sort rows < #:key (lambda (r) (it:text-y (first r)))))])
      (apply min (map it:text-x r))))
  (define lefts (line-lefts "aaaaaaa bbbbbbb ccccccc ddddddd" 260.0))
  (check-true (> (length lefts) 1) "the text wrapped")
  (when (> (length lefts) 1)
    (check-= (second lefts) (first lefts) 0.6
             "and the wrapped line starts where the first one does")))

;; ----------------------------------------- character spacing goes *between*

;; A run's character spacing is drawn between its characters, so a segment's
;; width has to be measured the same way. It used to be added to the advance
;; from one word to the next while each word was drawn at its natural width, so
;; every word landed `spacing` times its length to the left of where it should
;; -- and at -3pt on a 116pt title, nine characters of "Efficient" swallowed the
;; space after it. The words in a talk's title ran together and nothing here
;; noticed, because the only spaced fixture spells `l e t t e r s`, whose words
;; are one character long and for whom the two rules agree.
(let ()
  (local-require glide-pptx/runtime glide-pptx/draw-ir glide-pptx/record-adapt
                 pict racket/list)
  ;; The talk's own title, at the size it is set in: ten characters before the
  ;; space is what makes the difference visible rather than arithmetic.
  (define TEXT "Efficient Extraction")
  (define SPACING -3.0)
  (define (drawn spacing)
    (define box
      (textbox #:width 4000.0 #:height 400.0
               (para* (run* TEXT #:size 116.0 #:font "Liberation Sans" #:spacing spacing))))
    (define page (pict->display-page (lambda (dc) (draw-pict box dc 0 0)) 4000.0 400.0))
    (sort (filter it:text? (display-page-items page)) < #:key it:text-x))
  (define plain (drawn 0.0))
  (define tight (drawn SPACING))
  (check-equal? (apply string-append (map it:text-str plain)) TEXT
                "unspaced, the line is drawn as it reads")
  (check-equal? (apply string-append (map it:text-str tight)) TEXT
                "and spaced, every character is still drawn, in order")
  ;; The line from the left of its first character to the right of its last.
  ;; Measured this way because the two are drawn differently -- unspaced a word
  ;; is one call, spaced it is one call per character -- and what has to agree
  ;; is where the line ends, not how many calls it took.
  (define (extent items)
    (and (pair? items)
         (- (apply max (for/list ([i (in-list items)]) (+ (it:text-x i) (it:text-w i))))
            (apply min (map it:text-x items)))))
  ;; One gap per pair of characters, each of them `SPACING` tighter.
  ;; Within a point or so: a spaced line is drawn character by character and so
  ;; loses the kerning a whole word is drawn with, which is worth about that
  ;; much across twenty characters at this size.
  (check-= (extent tight)
           (+ (extent plain) (* SPACING (sub1 (string-length TEXT)))) 2.5
           "the characters are drawn closer by the spacing, not the words")
  ;; And the symptom: the gap between the B and the C. Measured from the right
  ;; of whatever was drawn ending in B to the left of whatever begins with C, so
  ;; that it reads the same whether the line was drawn word by word or character
  ;; by character. With the spacing applied to the advance and not to the
  ;; drawing this went *negative*: the words overlapped, and the space with them.
  ;; The space is drawn either way -- it is a piece of the line like any other --
  ;; so the gap is measured from what is drawn on each side of it.
  (define (gap items)
    (define i (for/first ([(it k) (in-indexed items)]
                          #:when (string=? " " (it:text-str it)))
                k))
    (and i (positive? i) (< (add1 i) (length items))
         (let ([before (list-ref items (sub1 i))]
               [after (list-ref items (add1 i))])
           (- (it:text-x after) (+ (it:text-x before) (it:text-w before))))))
  (check-true (and (gap tight) (> (gap tight) (* 0.5 (gap plain))))
              (format "the space is still there: ~a points of it against ~a unspaced"
                      (gap tight) (gap plain)))
  ;; The gap crosses two character boundaries -- after the t and after the space
  ;; -- so it narrows by twice the spacing and no more. It used to narrow by the
  ;; whole word's worth, which at ten characters left nothing of it.
  (check-= (gap tight) (+ (gap plain) (* 2 SPACING)) 0.6
           "and it is the spacing narrower, not gone"))

;; --------------------------------------------- line spacing is 1.2 per cent

;; A percentage line spacing is a percentage of *single* spacing, and single
;; spacing is 1.2 times the largest font size on the line. It is not a
;; percentage of the font's own bounding box, which is what this used to
;; multiply -- and which is font-dependent: Liberation Sans measures 1.12 times
;; its size, Carlito 1.22.
;;
;; Measured against LibreOffice both ways round: a two-line title at 116pt with
;; 80% spacing advances 111.75pt there and here (it was 8pt tight), and
;; 32pt bullets at 100% advance 38.6pt there against 38.4 here (they were 39.1).
;;
;; Every fixture is one line per paragraph, so nothing here noticed until a real
;; deck's title wrapped.
(let ()
  (local-require glide-pptx/runtime glide-pptx/draw-ir glide-pptx/record-adapt
                 pict racket/list)
  (define (advance size pct)
    ;; Two paragraphs, so there are two lines whose tops can be compared.
    (define box
      (textbox #:width 2000.0 #:height 400.0
               (para* #:line-spacing (cons 'percent pct)
                      (run* "First" #:size size #:font "Liberation Sans"))
               (para* #:line-spacing (cons 'percent pct)
                      (run* "Second" #:size size #:font "Liberation Sans"))))
    ;; The recorded draw calls, so the y of each drawn line is the real one.
    (define page (pict->display-page (lambda (dc) (draw-pict box dc 0 0)) 2000.0 400.0))
    (define texts
      (sort (filter it:text? (display-page-items page)) < #:key it:text-y))
    (and (= 2 (length texts))
         (- (it:text-y (second texts)) (it:text-y (first texts)))))

  (for ([size (in-list '(116.0 32.0 18.0))])
    (for ([pct (in-list '(1.0 0.8 1.5))])
      (define got (advance size pct))
      (define want (* pct 1.2 size))
      (check-true (and got (< (abs (- got want)) 0.01))
                  (format "~apt at ~a%: advance ~a, want ~a (1.2 x size x the percentage)"
                          size (* 100 pct) got want)))))

;; --------------------------------------------------- the ends of a line

;; Reported from a real deck: "the lines on slide 15 seem pretty messed up". Ten
;; connectors there carry `<a:tailEnd type="arrow"/>`, and arrowheads were not
;; parsed, drawn, emitted or written -- the diagram's arrows were plain lines.
;;
;; Nothing caught it, and nothing could have: a round trip compares what we
;; parsed against what we wrote, and a feature that is not parsed is absent on
;; both sides. The pixel diff against LibreOffice did draw them, but ten
;; arrowheads are two hundredths of a percent of a slide, which is well inside
;; the residual that text metrics already account for.
(let ()
  (local-require glide-pptx/geometry racket/math)
  ;; Which way the two ends of a connector face, since that is what orients an
  ;; arrowhead and what `flipH`/`flipV` change.
  (define plain (path-ends "straightConnector1" 100.0 60.0))
  (check-true (and plain #t) "a connector has ends")
  (check-equal? (list (first plain) (second plain)) '(0.0 0.0) "starting at the corner")
  (check-equal? (list (fourth plain) (fifth plain)) '(100.0 60.0) "and ending at the other")
  (define flipped (path-ends "straightConnector1" 100.0 60.0 #:flip-h? #t))
  (check-equal? (list (first flipped) (second flipped)) '(100.0 0.0)
                "a flip swaps which corner is the start")
  ;; The outward angles are opposite each other on a straight line.
  (check-= (abs (- (third plain) (sixth plain))) pi 0.001
           "the two ends face opposite ways")

  ;; A closed shape has no ends to decorate.
  (check-false (path-ends "rect" 100.0 60.0) "a rectangle has no ends")

  ;; And the decoration survives being written and read again.
  (local-require glide-pptx/export glide-pptx/runtime racket/file)
  (define ln (make-stroke (hex "000000") #:width 4.0
                          #:head (line-end 'triangle "sm" "lg")
                          #:tail (line-end 'arrow "med" "med")))
  (define out (make-temporary-file "ends~a.pptx"))
  (picts->pptx (list (slide-canvas #:width 400.0 #:height 300.0 #:background (hex "FFFFFF")
                                   (at 80.0 60.0 #:tag "Arrow"
                                       (shape-pict #:width 200.0 #:height 120.0
                                                   #:shape "straightConnector1" #:line ln))))
               out #:width 400.0 #:height 300.0)
  (define back (pptx->deck out #:workdir (make-temporary-file "ends~a" 'directory)))
  (define shape
    (for*/first ([s (in-list (deck-slides back))]
                 [e (in-list (slide-elements s))]
                 #:when (and (shape? e) (shape-line e)))
      e))
  (check-true (and shape #t) "the connector came back")
  (when shape
    (define l (shape-line shape))
    (check-equal? (line-end-kind (stroke-head l)) 'triangle "with its head")
    (check-equal? (line-end-length (stroke-head l)) "lg" "at the size it was given")
    (check-equal? (line-end-kind (stroke-tail l)) 'arrow "and its tail"))
  (delete-file out))

;; ------------------------------------------------ where the baseline sits

;; Single spacing is 1.2 times the font size and the baseline sits 1.0 of it
;; down -- so the descent that positions a line is a fifth of the size, not the
;; font's own, which varies (Liberation Sans reports 0.212, Carlito 0.269).
;;
;; Measured against LibreOffice at 96pt: with the font's descent our text sat
;; 1.08pt high and its glyph box was otherwise identical -- same left edge, same
;; height, ink within 0.02%. With a fifth of the size the two agree exactly, and
;; the worst element in the per-element comparison went from 31% to under 8%.
(let ()
  (local-require glide-pptx/runtime glide-pptx/draw-ir glide-pptx/record-adapt
                 pict racket/list)
  ;; Where the first line's baseline lands in a top-anchored box with no insets.
  (define (baseline size)
    (define box
      (textbox #:width 900.0 #:height 400.0 #:insets (insets 0.0 0.0 0.0 0.0)
               (para* (run* "Hxg" #:size size #:font "Liberation Sans"))))
    (define page (pict->display-page (lambda (dc) (draw-pict box dc 0 0)) 900.0 400.0))
    (define t (findf it:text? (display-page-items page)))
    (and t (+ (it:text-y t) (it:text-ascent t))))

  (for ([size (in-list '(12.0 32.0 48.0 96.0 116.0))])
    (define got (baseline size))
    (check-true (and got #t) (format "~apt drew a line" size))
    (when got
      ;; The line box is 1.2 times the size and the baseline a fifth up from its
      ;; foot, which is one size down from its head.
      (check-= got size 0.05
               (format "~apt: the baseline sits one em down, not ~a" size got)))))

;; ------------------------------------------------- a part may open with a BOM

;; Two decks in the corpus begin their XML with a byte-order mark. It is legal,
;; and Racket's reader treats it as content and refuses the document -- so both
;; failed to import until it was taken off.
(let ()
  (local-require racket/file)
  (define f (make-temporary-file "bom~a.xml"))
  (display-to-file (bytes-append (bytes #xEF #xBB #xBF)
                                 #"<?xml version=\"1.0\"?><a:root xmlns:a=\"x\"/>")
                   f #:exists 'replace #:mode 'binary)
  (check-true (xml-element? (read-xexpr-file f)) "a part opening with a BOM reads")
  (delete-file f))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
