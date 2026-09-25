#lang racket/base
;; The drawing runtime shared by the direct renderer and by emitted programs.
;;
;; Everything here works in points with a top-left origin, matching the IR, and
;; produces ordinary picts -- so emitted code can be edited, composed with the
;; rest of `pict`, and animated, without knowing anything about pptx.
(require racket/treelist racket/class racket/list racket/math racket/string racket/promise
         racket/draw pict
         "ir.rkt" "geometry.rkt" "tagged.rkt" "source-tag.rkt")
(provide shown-picts canvas-transition canvas-hidden?
         set-slide-numbers! slide-numbers? slide-numbers-on? number-on
         forget-bitmaps!
         (struct-out slide-spec) make-slide-spec
         register-slide-manifest slide-manifest-ref
         ;; composition
         (struct-out placed) at tag-pict slide-canvas from-stage pin-placed
         placed-position
         source-location-tag automatic-tag? automatic-tag-key automatic-tag-name
         ;; structure carried on the pict, for export
         (all-from-out "tagged.rkt")
         ;; leaves
         shape-pict text-pict textbox image-pict group-pict table-pict
         current-default-font
         ;; text description, as emitted code writes it
         run* para* body*
         ;; paint helpers
         hex color->rgba fill->brush
         ;; output
         deck->pdf picts->pdf pict->png media-lookup current-media-base
         ;; diagnostics
         runtime-warnings
         ;; Re-exported so a generated program needs only this one module.
         ;; Deliberately not `all-from-out`: the IR's accessors include names
         ;; like `slide-width` that generated code defines itself.
         solid-fill gradient-fill image-fill pattern-fill
         stroke make-stroke line-end
         shadow make-shadow
         rgba rgb black white
         insets default-insets body-natural-size
         bullet no-bullet
         preset-geom custom-geom
         tbl-cell)

(define runtime-warnings (make-parameter #f))
(define (warn! msg)
  (define b (runtime-warnings))
  (when b (set-box! b (cons msg (unbox b)))))

;; A `glide_slides` declaration keeps the ordinary list that user code expects,
;; and additionally registers a manifest for synchronization. `source` is the
;; undecorated slide that owns editor changes; `shown` is what the slideshow,
;; PDF, and direct exporter use. A generated page such as a section divider has
;; neither an owner nor a separate source value; the loader uses its shown value
;; so page numbering stays aligned.
(struct slide-spec (owner source shown) #:transparent)

(define (make-slide-spec owner source shown)
  (slide-spec owner source shown))

(define slide-manifests (make-weak-hasheq))

(define (register-slide-manifest specs slides)
  (hash-set! slide-manifests slides specs)
  slides)

(define (slide-manifest-ref slides [failure-result #f])
  (hash-ref slide-manifests slides failure-result))

;; ------------------------------------------------------------------- paint

(define (rgba->color c)
  (make-object color%
               (inexact->exact (round (max 0 (min 255 (rgba-r c)))))
               (inexact->exact (round (max 0 (min 255 (rgba-g c)))))
               (inexact->exact (round (max 0 (min 255 (rgba-b c)))))
               (max 0.0 (min 1.0 (rgba-a c)))))

;; "#4472C4" or "4472C4" to an rgba, for readable emitted code.
(define (hex s #:alpha [a 1.0])
  (define t (if (and (> (string-length s) 0) (char=? #\# (string-ref s 0)))
                (substring s 1) s))
  (define n (string->number t 16))
  (if n
      (rgba (arithmetic-shift (bitwise-and n #xFF0000) -16)
            (arithmetic-shift (bitwise-and n #x00FF00) -8)
            (bitwise-and n #xFF) a)
      black))

(define (color->rgba c) (if (rgba? c) c (hex (format "~a" c))))

;; Density of the `pct*` pattern presets, used to blend fg over bg since
;; racket/draw has no equivalent hatch set.
(define (pattern-density name)
  (define m (regexp-match #rx"^pct([0-9]+)$" name))
  (cond [m (/ (string->number (cadr m)) 100.0)]
        [(regexp-match #rx"Horz|Vert|Grid|Diag|Trellis|Cross" name) 0.5]
        [else 0.5]))

(define (blend a b t)
  (define (mix x y) (+ (* x (- 1.0 t)) (* y t)))
  (rgba (mix (rgba-r b) (rgba-r a)) (mix (rgba-g b) (rgba-g a))
        (mix (rgba-b b) (rgba-b a)) (mix (rgba-a b) (rgba-a a))))

;; The gradient's endpoints across a w x h box for an angle in degrees
;; clockwise from the positive x axis.
(define (gradient-line w h deg)
  (define a (degrees->radians deg))
  (define cx (/ w 2.0)) (define cy (/ h 2.0))
  (define len (+ (abs (* w (cos a))) (abs (* h (sin a)))))
  (values (- cx (* (/ len 2.0) (cos a))) (- cy (* (/ len 2.0) (sin a)))
          (+ cx (* (/ len 2.0) (cos a))) (+ cy (* (/ len 2.0) (sin a)))))

;; A bare color counts as a solid fill, which keeps generated code short.
(define (fill->brush f0 w h)
  (define f (if (rgba? f0) (solid-fill f0) f0))
  (cond
    [(solid-fill? f) (new brush% [color (rgba->color (solid-fill-color f))])]
    [(gradient-fill? f)
     (define-values (x0 y0 x1 y1) (gradient-line w h (gradient-fill-angle f)))
     (define stops (for/list ([s (in-list (gradient-fill-stops f))])
                     (list (max 0.0 (min 1.0 (car s))) (rgba->color (cdr s)))))
     ;; A single stop is not a gradient; racket/draw needs at least two.
     (cond
       [(< (length stops) 2) (new brush% [color (cadr (first stops))])]
       [else (new brush%
                  [gradient (new linear-gradient%
                                 [x0 x0] [y0 y0] [x1 x1] [y1 y1] [stops stops])])])]
    [(pattern-fill? f)
     (new brush% [color (rgba->color (blend (pattern-fill-fg f) (pattern-fill-bg f)
                                            (pattern-density (pattern-fill-name f))))])]
    [else (new brush% [style 'transparent])]))

(define dash-styles (hash 'solid 'solid 'dash 'long-dash 'short-dash 'short-dash
                          'dot 'dot 'dash-dot 'dot-dash))

(define (stroke->pen s)
  (cond
    [(stroke? s)
     (new pen% [color (rgba->color (stroke-color s))]
          ;; pen% caps width at 255; a wider line means the file said something
          ;; we misread, and clamping keeps that visible instead of fatal.
          [width (max 0.0 (min 255.0 (stroke-width s)))]
          [style (hash-ref dash-styles (stroke-dash s) 'solid)]
          [cap (case (stroke-cap s) [(round) 'round] [(projecting) 'projecting]
                 [else 'butt])]
          [join 'miter])]
    [else (new pen% [style 'transparent])]))

;; ------------------------------------------------------------------ bitmaps

;; Every image a program draws, read once. Kept for the watch loop, which reads
;; the same program on every save and would otherwise read its pictures again
;; each time.
;;
;; Emptied when the program changes, because then none of them will be asked for
;; again: a sweep over five hundred decks kept every image from every one of
;; them, which is eighty megabytes a deck and the reason the sweep looked like a
;; hang.
(define bitmap-cache (make-hash))
(define (forget-bitmaps!) (hash-clear! bitmap-cache))
(define (load-bitmap path)
  (cond
    [(not path) (warn! "a picture has no image to draw") #f]
    [else (load-bitmap* path)]))

(define (load-bitmap* path)
  (hash-ref! bitmap-cache (if (path? path) (path->string path) path)
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e)
                                            (warn! (format "cannot read image ~a" path))
                                            #f)])
                 (read-bitmap path)))))

;; --------------------------------------------------------------- text setup

;; `draw-bitmap-section-smooth` exists only on bitmap-dc%, so scaling a source
;; region onto a destination box goes through the dc's own transformation.
(define (draw-bitmap-stretched dc bm dest-x dest-y dest-w dest-h
                               src-x src-y src-w src-h
                               #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define saved (send dc get-transformation))
  (send dc translate (+ dest-x (if fh dest-w 0)) (+ dest-y (if fv dest-h 0)))
  (send dc scale (if fh -1.0 1.0) (if fv -1.0 1.0))
  (send dc scale (/ dest-w (max 1.0 src-w)) (/ dest-h (max 1.0 src-h)))
  (send dc draw-bitmap-section bm 0 0 src-x src-y src-w src-h)
  (send dc set-transformation saved))

(define measure-dc
  (delay (let ([d (new bitmap-dc% [bitmap (make-bitmap 8 8)])])
           (send d set-smoothing 'smoothed)
           d)))

(define font-cache (make-hash))

;; `get-text-extent` rounds to whole device units, which at slide font sizes is
;; a 1-2pt error in both advance width and line height. Measuring at
;; MEASURE-SCALE times the size and dividing recovers the real metrics: for
;; 20pt Carlito that takes the measured advance from 253.0 to 246.1, against
;; 246.4 from the same font in LibreOffice.
(define MEASURE-SCALE 16.0)

;; `make-font` refuses a size above 1024, so a big heading cannot be measured at
;; the full multiple. Backing off keeps well over a thousand units of precision,
;; which is far more than the tenth of a point anything downstream can use.
(define (measure-scale-for size)
  (max 1.0 (min MEASURE-SCALE (/ 1000.0 (max 0.01 size)))))

;; A run that sits off the baseline is drawn smaller, which is what a
;; superscript or a subscript is. The format says only how far to shift it --
;; the size is the renderer's to choose, and every one of them chooses a
;; smaller one. 58% is what LibreOffice draws, measured against it rather than
;; guessed, and it is the same whatever the shift.
;;
;; It is the drawn size that matters for where a line breaks, which is why this
;; is here and not at the drawing: full-size subscripts made every line wider
;; than it should have been, and a line that had fitted before wrapped its last
;; character onto the next one.
(define SHIFTED-SIZE 0.58)

(define (run-size r)
  (define b (trun-baseline r))
  (if (or (not b) (zero? b)) (trun-size r) (* SHIFTED-SIZE (trun-size r))))

(define (run-font r [scale 1.0])
  (hash-ref! font-cache
             (list (trun-family r) (* scale (run-size r)) (trun-bold? r)
                   (trun-italic? r) (trun-underline? r))
             (lambda ()
               ;; The device unit is the point, so the font size has to be a
               ;; device measurement: racket/draw's "points" are 96ths of an
               ;; inch, which would come out a third too large.
               (make-font #:face (trun-family r)
                          #:family 'default
                          #:size (max 1.0 (* scale (run-size r)))
                          #:size-in-pixels? #t
                          #:weight (if (trun-bold? r) 'bold 'normal)
                          #:style (if (trun-italic? r) 'italic 'normal)
                          ;; Drawn beside the text instead, where its weight
                          ;; and its distance below the baseline are ours to
                          ;; match against LibreOffice.
                          #:underlined? #f))))

;; (values width height descent) of `str` in `font`, where `font` was built at
;; `scale` times its nominal size.
(define (measure-scaled str font [scale MEASURE-SCALE])
  (define-values (w h d _a) (send (force measure-dc) get-text-extent str font #t))
  (values (/ w scale) (/ h scale) (/ d scale)))

;; Metrics for one run's text, at the run's real size.
;;
;; A run with character spacing is measured the way it is drawn: character by
;; character, with the spacing added after each one. Measuring the string whole
;; and adding the spacing to the total would be the same number, but only if
;; the string were also drawn whole -- and it cannot be, because the spacing
;; has to go *between* the characters. Widths and drawing have to come from the
;; same rule or the two disagree, which is how the spaces went missing from a
;; 116pt title: every word was drawn at its natural width and the next one
;; started three points per character to the left of it.
(define (measure-run str r)
  (define k (measure-scale-for (run-size r)))
  (define spc (trun-spacing r))
  (cond
    [(or (not spc) (zero? spc) (string=? str ""))
     (measure-scaled str (run-font r k) k)]
    [else
     (define font (run-font r k))
     (for/fold ([w 0.0] [h 0.0] [d 0.0]) ([ch (in-string str)])
       (define-values (cw ch* cd) (measure-scaled (string ch) font k))
       (values (+ w cw spc) (max h ch*) (max d cd)))]))

(define (run-display-text r)
  (case (trun-caps r)
    [(all) (string-upcase (trun-text r))]
    [else (trun-text r)]))

;; ---------------------------------------------------------------- tokenizing

;; A measured piece of a line. `space?` pieces are dropped from the width used
;; for alignment when they fall at the end of a line.
(struct seg (run text w h desc space?) #:transparent)

(define (tokenize-run r)
  (define txt (run-display-text r))
  (define out '())
  ;; Hard breaks arrive as newlines inside a run's text.
  (for ([piece (in-list (string-split txt "\n" #:trim? #f))] [i (in-naturals)])
    (unless (zero? i) (set! out (cons 'break out)))
    (for ([tok (in-list (regexp-match* #px"[ \t]+|[^ \t]+" piece))])
      (define space? (and (regexp-match? #px"^[ \t]+$" tok) #t))
      (define-values (w h d) (measure-run tok r))
      (set! out (cons (seg r tok w h d space?) out))))
  (reverse out))

;; Splits a segment at the last character that still fits in `avail`; returns
;; (values #f #f) when the text is a single character and cannot be split.
(define (split-seg t avail)
  (define txt (seg-text t))
  (define r (seg-run t))
  (cond
    [(<= (string-length txt) 1) (values #f #f)]
    [else
     (define k
       (let loop ([i 1] [best 0])
         (cond
           [(>= i (string-length txt)) (max best 1)]
           [else
            (define-values (w _h _d) (measure-run (substring txt 0 i) r))
            (if (<= w avail) (loop (add1 i) i) (max best 1))])))
     (define (piece str)
       (define-values (w h d) (measure-run str r))
       (seg r str w h d #f))
     (values (piece (substring txt 0 k))
             (and (< k (string-length txt)) (piece (substring txt k))))]))

;; ------------------------------------------------------------------ layout

;; One laid-out line: its segments, its height, and its baseline offset from the
;; line's top.
(struct line-box (segs height baseline width first?) #:transparent)

(define (empty-line-metrics p)
  (define r (if (pair? (para-runs p)) (first (para-runs p))
                (trun "" "sans-serif" 18.0 #f #f #f #f black 0.0 'none 0.0 'all)))
  (define-values (w h d) (measure-run "Ag" r))
  (values h d))

(define (line-width segs)
  ;; Trailing spaces do not participate in alignment.
  (define trimmed (let loop ([rs (reverse segs)])
                    (cond [(and (pair? rs) (seg-space? (car rs))) (loop (cdr rs))]
                          [else (reverse rs)])))
  (for/sum ([s (in-list trimmed)]) (seg-w s)))

(define (wrap-paragraph p avail wrap?)
  (define toks (append* (for/list ([r (in-list (para-runs p))]) (tokenize-run r))))
  (define lines '())
  (define cur '())
  (define curw 0.0)
  ;; Whether the line being filled was started by running out of room. A space
  ;; that starts a *wrapped* line is the one the text broke at, and drawing it
  ;; would leave the wrapped text hanging; a space that starts a paragraph, or a
  ;; line after a hard break, is what someone typed and belongs on the page.
  (define wrapped? #f)
  (define (flush! [w? #f])
    (set! lines (cons (reverse cur) lines))
    (set! cur '()) (set! curw 0.0) (set! wrapped? w?))
  (define (push! t) (set! cur (cons t cur)) (set! curw (+ curw (seg-w t))))
  ;; Places one non-space token, breaking inside it when it cannot fit a line
  ;; even on its own -- which is what PowerPoint does with a long unbroken run
  ;; of characters, and the only way a narrow box stays readable.
  (define (place-word! t)
    (cond
      [(or (not wrap?) (<= (+ curw (seg-w t)) avail)) (push! t)]
      [(pair? cur) (flush! #t) (place-word! t)]
      [else
       (define-values (head tail) (split-seg t avail))
       (cond
         [(not head) (push! t)]
         [else (push! head) (flush! #t) (when tail (place-word! tail))])]))
  (for ([t (in-list toks)])
    (cond
      [(eq? t 'break) (flush!)]
      [(seg-space? t)
       ;; Dropped only where the line broke, not where someone indented.
       (unless (and (null? cur) wrapped?) (push! t))]
      [else (place-word! t)]))
  (flush!)
  (define raw (reverse lines))
  (for/list ([segs (in-list raw)] [i (in-naturals)])
    (define-values (eh ed) (empty-line-metrics p))
    (define nat-h (if (null? segs) eh (apply max (map seg-h segs))))
    (define desc (if (null? segs) ed (apply max (map seg-desc segs))))
    (define spacing (para-line-spacing p))
    ;; A percentage is a percentage of *single* spacing, and single spacing is
    ;; 1.2 times the largest font size on the line -- not the font's own
    ;; bounding box, which measures about 1.12 times the size. The two are 7%
    ;; apart, and that only shows once a paragraph has a second line for the
    ;; first to push down: every fixture here is one line per paragraph, which
    ;; is why the fixtures agreed with LibreOffice while a real deck's wrapped
    ;; title sat 8pt tight per line.
    (define single (* 1.2 (line-font-size p segs)))
    (define height (if (eq? 'points (car spacing))
                       (cdr spacing)
                       (* (cdr spacing) single)))
    ;; The extra room goes above the line, as in PowerPoint, so the baseline sits
    ;; a descent up from the bottom of the line box -- and the descent that
    ;; positions it is a fifth of the font size, not the font's own. Single
    ;; spacing is 1.2 times the size and the baseline sits 1.0 of it down, which
    ;; is what LibreOffice draws: measured, the two agree to a fifth of a point
    ;; at 96pt where the font's own descent put us a point high.
    (line-box segs height (- height (* 0.2 (line-font-size p segs))) (line-width segs)
              (zero? i))))

;; The size single spacing is reckoned from: the largest run on the line, or the
;; paragraph's own first run when the line is empty.
(define (line-font-size p segs)
  (cond
    [(pair? segs) (apply max (map (lambda (s) (trun-size (seg-run s))) segs))]
    [(pair? (para-runs p)) (trun-size (first (para-runs p)))]
    [else 18.0]))

;; The full vertical layout of a text body inside w x h.
;; Returns (listof (list para line-box y)) plus the total height.
;; The size a text body needs, laid out in a box `w` wide.
;;
;; A box that does not wrap is as wide as its longest line, whatever its own
;; width says, and one set to grow is as tall as its lines come to. PowerPoint
;; stores the box the author drew and grows it as the text is edited; a renderer
;; that does not honour `wrap="none"` wraps at the stored width instead, and the
;; label reads "Floatin g poin t". So the export writes the size the text
;; actually takes, and then every renderer agrees.
(define (body-natural-size tb w h)
  (define-values (lines total) (layout-body tb w h))
  (define ins (text-body-insets tb))
  (define widest
    (for/fold ([m 0.0]) ([entry (in-list lines)])
      (max m (line-box-width (second entry)))))
  (values (+ widest (insets-l ins) (insets-r ins))
          (+ total (insets-t ins) (insets-b ins))))

;; The rectangle an auto-fitting body actually draws within its stored shape.
;; Horizontal growth is centred; a wrapping box keeps its width. Vertically,
;; the text anchor is the fixed edge or point. Returning the stored rectangle
;; for every other body lets shape and bare-textbox rendering share the rule.
(define (body-drawn-box tb w h)
  (define grow? (and tb (eq? 'grow (text-body-autofit tb))))
  (define-values (natural-w natural-h)
    (if grow? (body-natural-size tb w h) (values w h)))
  (define draw-w (if (and grow? (not (text-body-wrap? tb))) natural-w w))
  (define draw-h natural-h)
  (values draw-w draw-h
          (/ (- w draw-w) 2.0)
          (if grow?
              (case (text-body-anchor tb)
                [(center) (/ (- h draw-h) 2.0)]
                [(bottom) (- h draw-h)]
                [else 0.0])
              0.0)))

(define (layout-body tb w h)
  (define ins (text-body-insets tb))
  (define avail-w (max 1.0 (- w (insets-l ins) (insets-r ins))))
  (define per-para
    (for/list ([p (in-list (text-body-paras tb))])
      (define indent-room (+ (para-margin-left p) (min 0.0 (para-indent p))))
      (define lines (wrap-paragraph p (max 1.0 (- avail-w (para-margin-left p)))
                                    (text-body-wrap? tb)))
      (list p lines)))
  ;; Space-before is dropped for the first paragraph, which is what PowerPoint
  ;; and LibreOffice both do -- keeping it puts every line 20% of a font size
  ;; too low.
  (define (space-before-of pl i) (if (zero? i) 0.0 (para-space-before (first pl))))
  (define total
    (for/sum ([pl (in-list per-para)] [i (in-naturals)])
      (+ (space-before-of pl i)
         (for/sum ([l (in-list (second pl))]) (line-box-height l))
         (para-space-after (first pl)))))
  (define y0
    (case (text-body-anchor tb)
      [(center) (+ (insets-t ins) (/ (- h (insets-t ins) (insets-b ins) total) 2.0))]
      [(bottom) (- h (insets-b ins) total)]
      [else (insets-t ins)]))
  (define placed-lines
    (let loop ([ps per-para] [y y0] [acc '()] [i 0])
      (cond
        [(null? ps) (reverse acc)]
        [else
         (define p (first (car ps)))
         (define lines (second (car ps)))
         (define y* (+ y (if (zero? i) 0.0 (para-space-before p))))
         (define-values (y** acc*)
           (for/fold ([yy y*] [a acc]) ([l (in-list lines)])
             (values (+ yy (line-box-height l)) (cons (list p l yy) a))))
         (loop (cdr ps) (+ y** (para-space-after p)) acc* (add1 i))])))
  (values placed-lines total))

;; Bullet numbering has to be assigned across paragraphs, since a level's
;; counter restarts whenever an outer level intervenes.
(define (bullet-numbers paras)
  (define counters (make-hash))
  (for/list ([p (in-list paras)])
    (define lvl (para-level p))
    (cond
      [(eq? 'number (bullet-kind (para-bullet p)))
       (for ([(k _v) (in-hash counters)]) (when (> k lvl) (hash-remove! counters k)))
       (define n (add1 (hash-ref counters lvl 0)))
       (hash-set! counters lvl n)
       n]
      [else
       (for ([(k _v) (in-hash counters)]) (when (>= k lvl) (hash-remove! counters k)))
       #f])))

(define (number-label fmt n)
  (define (alpha i lower?)
    (define s (string (integer->char (+ (if lower? 97 65) (modulo (sub1 i) 26)))))
    s)
  (define (roman i)
    (define pairs '((1000 . "m") (900 . "cm") (500 . "d") (400 . "cd") (100 . "c")
                    (90 . "xc") (50 . "l") (40 . "xl") (10 . "x") (9 . "ix")
                    (5 . "v") (4 . "iv") (1 . "i")))
    (let loop ([i i] [ps pairs] [acc ""])
      (cond [(or (zero? i) (null? ps)) acc]
            [(>= i (caar ps)) (loop (- i (caar ps)) ps (string-append acc (cdar ps)))]
            [else (loop i (cdr ps) acc)])))
  (cond
    [(regexp-match? #rx"^alphaLcPeriod" fmt) (format "~a." (alpha n #t))]
    [(regexp-match? #rx"^alphaUcPeriod" fmt) (format "~a." (alpha n #f))]
    [(regexp-match? #rx"^alphaLcParenR" fmt) (format "~a)" (alpha n #t))]
    [(regexp-match? #rx"^alphaUcParenR" fmt) (format "~a)" (alpha n #f))]
    [(regexp-match? #rx"^romanLcPeriod" fmt) (format "~a." (roman n))]
    [(regexp-match? #rx"^romanUcPeriod" fmt) (format "~a." (string-upcase (roman n)))]
    [(regexp-match? #rx"^arabicParenR" fmt) (format "~a)" n)]
    [(regexp-match? #rx"^arabicPlain" fmt) (format "~a" n)]
    [else (format "~a." n)]))

;; ------------------------------------------------------------- text drawing

(define (draw-body dc dx dy tb w h)
  (define ins (text-body-insets tb))
  (define avail-w (max 1.0 (- w (insets-l ins) (insets-r ins))))
  (define-values (lines _total) (layout-body tb w h))
  (define numbers (bullet-numbers (text-body-paras tb)))
  (define number-of (for/hash ([p (in-list (text-body-paras tb))] [n (in-list numbers)])
                      (values p n)))
  ;; pict's `dc` requires a draw procedure to leave the dc as it found it, so
  ;; everything this touches is saved -- including the pen, which only a
  ;; strikethrough run sets.
  (define old-font (send dc get-font))
  (define old-fg (send dc get-text-foreground))
  (define old-pen (send dc get-pen))
  (define old-brush (send dc get-brush))
  (for ([entry (in-list lines)])
    (define p (first entry))
    (define l (second entry))
    (define y (third entry))
    (define text-left (+ (insets-l ins) (para-margin-left p)))
    ;; A first-line indent moves the text only when there is no bullet: with a
    ;; bullet the indent is the hanging space the bullet itself sits in.
    (define bulleted? (not (eq? 'none (bullet-kind (para-bullet p)))))
    (define first-x (if (and (line-box-first? l) (not bulleted?))
                        (+ text-left (para-indent p))
                        text-left))
    (define box-w (- avail-w (para-margin-left p)))
    (define x0
      (case (para-align p)
        [(center) (+ text-left (/ (- box-w (line-box-width l)) 2.0))]
        [(right) (+ text-left (- box-w (line-box-width l)))]
        [else first-x]))
    (define baseline (+ y (line-box-baseline l)))
    ;; The bullet sits at the first line's indent position, left of the text.
    (when (and (line-box-first? l) (not (eq? 'none (bullet-kind (para-bullet p))))
               (pair? (line-box-segs l)))
      (define b (para-bullet p))
      (define model (seg-run (first (line-box-segs l))))
      (define size (* (or (bullet-size-frac b) 1.0) (trun-size model)))
      (define glyph
        (if (eq? 'number (bullet-kind b))
            (number-label (bullet-char b) (or (hash-ref number-of p #f) 1))
            (bullet-char b)))
      (define bfont (make-font #:face (or (bullet-font b) (trun-family model))
                               #:family 'default #:size (max 1.0 size)
                               #:size-in-pixels? #t))
      (define bk (measure-scale-for size))
      (define-values (bw bh bd)
        (measure-scaled glyph
                        (make-font #:face (or (bullet-font b) (trun-family model))
                                   #:family 'default
                                   #:size (max 1.0 (* bk size))
                                   #:size-in-pixels? #t)
                        bk))
      (send dc set-font bfont)
      (send dc set-text-foreground
            (rgba->color (or (bullet-color b) (trun-color model))))
      (send dc draw-text glyph (+ dx text-left (para-indent p))
            (+ dy (- baseline (- bh bd))) #t))
    ;; Trailing spaces are not drawn, so a centered line is not pushed left.
    (define segs (let loop ([rs (reverse (line-box-segs l))])
                   (cond [(and (pair? rs) (seg-space? (car rs))) (loop (cdr rs))]
                         [else (reverse rs)])))
    (for/fold ([x x0]) ([s (in-list segs)])
      (define r (seg-run s))
      (define font (run-font r))
      (send dc set-font font)
      (send dc set-text-foreground (rgba->color (trun-color r)))
      (define top (- baseline (- (seg-h s) (seg-desc s))
                     ;; Superscript and subscript shift the baseline by a
                     ;; fraction of the font size.
                     (* (trun-baseline r) (trun-size r))))
      ;; Character by character when the run is spaced, because the spacing
      ;; goes between the characters and `draw-text` has nowhere to put it.
      (define spc (trun-spacing r))
      (cond
        [(or (not spc) (zero? spc)) (send dc draw-text (seg-text s) (+ dx x) (+ dy top) #t)]
        [else
         (define k (measure-scale-for (run-size r)))
         (define mfont (run-font r k))
         (for/fold ([cx (+ dx x)]) ([ch (in-string (seg-text s))])
           (send dc draw-text (string ch) cx (+ dy top) #t)
           (define-values (cw _h _d) (measure-scaled (string ch) mfont k))
           (+ cx cw spc))
         (void)])
      ;; Both rules are drawn here rather than left to the font. The one the
      ;; font draws is a hairline whatever the size, which at 40pt is a third
      ;; of the weight LibreOffice gives it; and both of these sat higher than
      ;; LibreOffice puts them. The fractions are of the ascent, measured
      ;; against LibreOffice across three faces rather than guessed, and they
      ;; land within a pixel of it at 40pt.
      (define (rule! frac weight)
        (define y (+ dy top (* frac (- (seg-h s) (seg-desc s)))))
        (send dc set-pen (new pen% [color (rgba->color (trun-color r))]
                              [width (max 0.5 (/ (trun-size r) weight))]))
        (send dc draw-line (+ dx x) y (+ dx x (seg-w s)) y))
      (when (trun-strike? r) (rule! 0.78 14.0))
      (when (trun-underline? r) (rule! 1.07 15.0))
      (+ x (seg-w s))))
  (send dc set-font old-font)
  (send dc set-text-foreground old-fg)
  (send dc set-pen old-pen)
  (send dc set-brush old-brush))

;; ------------------------------------------------------------------- leaves

;; A text body drawn inside a w x h box, with no fill or outline of its own.
(define (text-pict tb w h)
  (cond
    [(or (not tb) (text-body-empty? tb)) (blank w h)]
    [(zero? (text-body-rot tb))
     (dc (lambda (dc dx dy) (draw-body dc dx dy tb w h)) w h)]
    [else
     ;; A rotated body rotates within the same box, so re-center after rotating.
     (define inner (dc (lambda (dc dx dy) (draw-body dc dx dy tb w h)) w h))
     (define r (rotate inner (- (degrees->radians (text-body-rot tb)))))
     (cc-superimpose (blank w h) r)]))

;; ------------------------------------------------------ ends of a drawn line

;; PowerPoint sizes an arrowhead from the line's width: "med" is about three
;; times it long and as wide again. The two size words scale that.
(define (end-scale word)
  (case word [("sm") 0.75] [("lg") 1.5] [else 1.0]))

(define (draw-line-end! dc e x y angle pen-w)
  (define len (* 3.0 pen-w (end-scale (line-end-length e))))
  (define half (* 1.5 pen-w (end-scale (line-end-width e))))
  (define ca (cos angle))
  (define sa (sin angle))
  ;; The tip is at the end of the line, and the head runs back down it.
  (define (at d across)
    (cons (+ x (* d ca) (* across (- sa)))
          (+ y (* d sa) (* across ca))))
  (define pts
    (case (line-end-kind e)
      [(diamond) (list (at 0.0 0.0) (at (- (/ len 2.0)) half)
                       (at (- len) 0.0) (at (- (/ len 2.0)) (- half)))]
      [(stealth) (list (at 0.0 0.0) (at (- len) half)
                       (at (* -0.6 len) 0.0) (at (- len) (- half)))]
      [(oval) #f]
      [else (list (at 0.0 0.0) (at (- len) half) (at (- len) (- half)))]))
  (define old-pen (send dc get-pen))
  (cond
    [(eq? 'oval (line-end-kind e))
     (send dc set-pen (new pen% [style 'transparent]))
     (send dc draw-ellipse (- x half) (- y half) (* 2 half) (* 2 half))]
    [else
     (send dc set-pen (new pen% [style 'transparent]))
     (send dc draw-polygon (map (lambda (p) (cons (car p) (cdr p))) pts))])
  (send dc set-pen old-pen))

;; Both ends of a shape's outline, if it has ends and the stroke asks for them.
(define (draw-line-ends! dc line geom w h fh fv dx dy)
  (when (and (stroke? line) (or (stroke-head line) (stroke-tail line)))
    (define ends
      (if (custom-geom? geom)
          (custom-path-ends geom w h #:flip-h? fh #:flip-v? fv)
          (path-ends (preset-geom-name geom) w h #:flip-h? fh #:flip-v? fv)))
    (when ends
      (define pen-w (max 0.5 (let ([v (stroke-width line)]) (if (real? v) v 1.0))))
      (define old-brush (send dc get-brush))
      (send dc set-brush (new brush% [color (rgba->color (stroke-color line))]))
      (define-values (x0 y0 a0 x1 y1 a1) (apply values ends))
      (when (stroke-head line)
        (draw-line-end! dc (stroke-head line) (+ dx x0) (+ dy y0) a0 pen-w))
      (when (stroke-tail line)
        (draw-line-end! dc (stroke-tail line) (+ dx x1) (+ dy y1) a1 pen-w))
      (send dc set-brush old-brush))))

;; An auto-shape: geometry filled and outlined, with its text on top.
;; `#:shape` names a preset directly, which is the common case; `#:geom` takes a
;; full geometry when adjustment values or a custom path are involved.
;; A shadow as a program writes one: the colour first, since its alpha is what
;; makes it a shadow, then how far it falls and in which direction.
(define (make-shadow color #:blur [blur 0.0] #:distance [dist 0.0] #:direction [dir 0.0])
  (shadow blur dist dir (if (rgba? color) color black)))

;; Where a shadow falls: `direction` is degrees clockwise from east, which is
;; how DrawingML says it and how the page's y runs.
(define (shadow-offset sh)
  (define r (* (shadow-dir sh) (/ pi 180.0)))
  (values (* (shadow-dist sh) (cos r)) (* (shadow-dist sh) (sin r))))

;; Drawn as a stack of copies, growing and fading outwards, because there is no
;; blur here to ask for: a soft edge is several soft edges at once. Six steps is
;; enough that the bands do not read as bands at the sizes decks use.
(define SHADOW-STEPS 6)

(define (draw-shadow! dc sh path-for w h dx dy)
  (define-values (ox oy) (shadow-offset sh))
  (define c (shadow-color sh))
  (define blur (shadow-blur sh))
  (define old-pen (send dc get-pen))
  (define old-brush (send dc get-brush))
  (define old-alpha (send dc get-alpha))
  (define steps (if (> blur 0.01) SHADOW-STEPS 1))
  (for ([i (in-range steps)])
    ;; The innermost copy is the shape itself; the rest grow by a share of the
    ;; blur, each fainter than the last.
    (define grow (if (= steps 1) 0.0 (* blur (/ (exact->inexact i) (sub1 steps)))))
    (define a (* (rgba-a c) (/ 1.0 steps)))
    (send dc set-alpha (max 0.0 (min 1.0 a)))
    (send dc set-pen (new pen% [style 'transparent]))
    (send dc set-brush (new brush% [color (rgba->color c)]))
    (define sx (if (> w 0.01) (/ (+ w (* 2 grow)) w) 1.0))
    (define sy (if (> h 0.01) (/ (+ h (* 2 grow)) h) 1.0))
    (define p (path-for))
    (define t (send dc get-transformation))
    (send dc translate (+ dx ox (- grow)) (+ dy oy (- grow)))
    (send dc scale sx sy)
    (send dc draw-path p 0 0)
    (send dc set-transformation t))
  (send dc set-alpha old-alpha)
  (send dc set-pen old-pen)
  (send dc set-brush old-brush))

(define (shape-pict #:width w #:height h
                    #:shape [shape-name #f]
                    #:geom [geom0 #f]
                    #:fill [fill0 #f] #:line [line #f] #:body [body #f]
                    #:shadow [sh #f]
                    #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define geom (or geom0 (preset-geom (or shape-name "rect") '())))
  (define fill (if (rgba? fill0) (solid-fill fill0) fill0))
  ;; DrawingML keeps the extent that was originally drawn even when
  ;; <a:spAutoFit/> tells the editor to resize the shape to its contents.
  ;; LibreOffice and PowerPoint preserve the old centre while doing that. Keep
  ;; the pict's stored extent (so its source placement does not move), but draw
  ;; the resized shape and text at the corresponding inset. The semantic
  ;; descriptor retains that stored extent and `spAutoFit`, so the editor
  ;; performs the same displayed resize itself.
  (define-values (draw-w draw-h draw-x draw-y) (body-drawn-box body w h))
  (define closed?
    (or (custom-geom? geom) (geometry-closed? (preset-geom-name geom))))
  (define base
    (dc (lambda (dc dx dy)
          (define dx* (+ dx draw-x))
          (define dy* (+ dy draw-y))
          (define path
            (if (custom-geom? geom)
                (custom-path geom draw-w draw-h #:flip-h? fh #:flip-v? fv)
                (preset-path (preset-geom-name geom) draw-w draw-h (preset-geom-adjust geom)
                             #:flip-h? fh #:flip-v? fv)))
          (define old-pen (send dc get-pen))
          (define old-brush (send dc get-brush))
          ;; Behind the shape, and only where the shape is filled: an outline
          ;; with nothing inside it casts no shadow from its middle.
          (when (and (shadow? sh) closed?)
            (draw-shadow! dc sh
                          (lambda ()
                            (if (custom-geom? geom)
                                (custom-path geom draw-w draw-h #:flip-h? fh #:flip-v? fv)
                                (preset-path (preset-geom-name geom) draw-w draw-h
                                             (preset-geom-adjust geom)
                                             #:flip-h? fh #:flip-v? fv)))
                          draw-w draw-h dx* dy*))
          (cond
            [(image-fill? fill)
             ;; An image fill is the bitmap clipped to the shape's outline.
             (define bm (load-bitmap (image-fill-src fill)))
             (define fill-alpha (image-fill-opacity fill))
             (when bm
               (define old-rgn (send dc get-clipping-region))
               (define rgn (new region% [dc dc]))
               (define moved (new dc-path%))
               (send moved append path)
               (send moved translate dx* dy*)
               (send rgn set-path moved)
               (send dc set-clipping-region rgn)
               (define old-alpha (send dc get-alpha))
               (when (< fill-alpha 0.999) (send dc set-alpha (* old-alpha fill-alpha)))
               (draw-bitmap-stretched dc bm dx* dy* draw-w draw-h
                                      0 0 (send bm get-width) (send bm get-height))
               (send dc set-alpha old-alpha)
               (send dc set-clipping-region old-rgn))]
            [else
             (send dc set-brush (if closed? (fill->brush fill draw-w draw-h)
                                    (new brush% [style 'transparent])))])
          (send dc set-pen (stroke->pen line))
          (unless (and (image-fill? fill) (not (stroke? line)))
            (send dc draw-path path dx* dy*))
          (draw-line-ends! dc line geom draw-w draw-h fh fv dx* dy*)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush))
        w h))
  (define text-layer
    (and body
         (not (text-body-empty? body))
         ;; `dc` deliberately retains the stored extent while allowing a shape
         ;; that grows beyond it to paint outside that bookkeeping rectangle.
         (let ([tp (text-pict body draw-w draw-h)])
           (dc (lambda (dc dx dy)
                 (draw-pict tp dc (+ dx draw-x) (+ dy draw-y)))
               w h))))
  (with-desc (if text-layer (lt-superimpose base text-layer) base)
             (shape-desc w h geom fill line body fh fv sh)))

;; A bitmap stretched into w x h, optionally cropped by fractions of its source.
(define (image-pict src w h #:crop [crop #f] #:flip-h? [fh #f] #:flip-v? [fv #f]
                    #:line [line #f] #:opacity [opacity 1.0] #:shadow [sh #f])
  (with-desc (let ([base (image-pict* src w h crop fh fv line opacity)])
               (if (shadow? sh)
                   (dc (lambda (dc dx dy)
                         (draw-shadow! dc sh
                                       (lambda ()
                                         (let ([p (new dc-path%)])
                                           (send p rectangle 0 0 w h)
                                           p))
                                       w h dx dy)
                         (draw-pict base dc dx dy))
                       w h)
                   base))
             (image-desc w h src crop line fh fv opacity sh)))

(define (image-pict* src w h crop fh fv line opacity)
  (define bm (load-bitmap src))
  (cond
    [(not bm) (shape-pict #:width w #:height h #:fill #f
                          #:line (or line (make-stroke (rgb 200 60 60) #:width 1.0)))]
    [else
     (dc (lambda (dc dx dy)
           (define bw (send bm get-width)) (define bh (send bm get-height))
           (define l (if crop (first crop) 0.0)) (define t (if crop (second crop) 0.0))
           (define r (if crop (third crop) 0.0)) (define b (if crop (fourth crop) 0.0))
           (define sx (* bw l)) (define sy (* bh t))
           (define sw (max 1.0 (* bw (- 1.0 l r)))) (define sh (max 1.0 (* bh (- 1.0 t b))))
           (define old-alpha (send dc get-alpha))
           (when (< opacity 0.999) (send dc set-alpha (* old-alpha opacity)))
           (draw-bitmap-stretched dc bm dx dy w h sx sy sw sh
                                  #:flip-h? fh #:flip-v? fv)
           (send dc set-alpha old-alpha)
           (when (stroke? line)
             (define old-pen (send dc get-pen))
             (define old-brush (send dc get-brush))
             (send dc set-pen (stroke->pen line))
             (send dc set-brush (new brush% [style 'transparent]))
             (send dc draw-rectangle dx dy w h)
             (send dc set-pen old-pen)
             (send dc set-brush old-brush)))
         w h)]))

;; Children are drawn in their own coordinate space and then scaled onto the
;; group's box, which is how PowerPoint scales grouped text along with shapes.
(define (group-pict #:width w #:height h
                    #:child-x [cx 0.0] #:child-y [cy 0.0]
                    #:child-width [cw w] #:child-height [ch h]
                    #:flip-h? [fh #f] #:flip-v? [fv #f]
                    . placeds)
  (define inner
    (for/fold ([base (blank (max cw 1.0) (max ch 1.0))]) ([pl (in-list placeds)])
      (pin-placed base pl (- cx) (- cy))))
  (define sx (if (zero? cw) 1.0 (/ w cw)))
  (define sy (if (zero? ch) 1.0 (/ h ch)))
  (define scaled (scale inner sx sy))
  ;; A flipped group mirrors everything inside it. `pict` has no negative
  ;; scale, so the drawing goes through a dc transform.
  (define shown
    (if (or fh fv)
        (dc (lambda (dc* dx dy)
              (define t (send dc* get-transformation))
              (send dc* translate (+ dx (if fh w 0.0)) (+ dy (if fv h 0.0)))
              (send dc* scale (if fh -1.0 1.0) (if fv -1.0 1.0))
              (draw-pict scaled dc* 0 0)
              (send dc* set-transformation t))
            w h)
        scaled))
  (with-desc shown (group-desc w h cx cy cw ch placeds fh fv)))

(define (table-pict #:width w #:height h #:col-widths cols #:row-heights rows
                    #:cells cells)
  (with-desc (table-pict* w h cols rows cells)
             (table-desc w h cols rows cells)))

(define (table-pict* w h cols rows cells)
  (dc (lambda (dc dx dy)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (for/fold ([y 0.0]) ([row (in-list cells)] [rh (in-list rows)])
          (for/fold ([x 0.0]) ([c (in-list row)] [cwid (in-list cols)])
            (when (tbl-cell? c)
              (send dc set-brush (fill->brush (tbl-cell-fill c) cwid rh))
              (send dc set-pen (stroke->pen (tbl-cell-line c)))
              (send dc draw-rectangle (+ dx x) (+ dy y) cwid rh)
              (when (and (tbl-cell-body c) (not (text-body-empty? (tbl-cell-body c))))
                (draw-body dc (+ dx x) (+ dy y) (tbl-cell-body c) cwid rh)))
            (+ x cwid))
          (+ y rh))
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      w h))

;; -------------------------------------------------------------- composition

;; An element positioned on a slide. `rot` is degrees clockwise about the
;; element's center, matching PowerPoint.
;; `tag` names the element for export and for merging edits back. Rhombus's
;; `at` macro supplies an opaque tag derived from the call's source location;
;; an explicit tag overrides it for a site that deliberately names a family.
;;
;; `nudge` is a `(list dx dy)` correction added to the position. It exists for
;; the case where x and y are *computed* -- `(at margin (+ top 20) ...)` -- and
;; so there is no number a merge could rewrite. Dragging such an element in
;; PowerPoint records the correction here instead, which keeps the program's own
;; layout logic intact and says plainly that a hand adjustment was made.
;;
;; Being one argument rather than a wrapper is deliberate: a second drag updates
;; these two numbers, so corrections cannot stack up the way nested pads do.
(struct placed (x y rot pict tag nudge) #:transparent)

(define (at x y p #:rotate [rot 0.0] #:tag [tag #f] #:name [name #f] #:nudge [nudge #f]
            #:source-location [source-location #f])
  ;; A name already carried by the pict is an explicit user identity too. It
  ;; predates automatic source tags and is how a pict placed by a helper keeps
  ;; its name; the fallback must not hide it merely because `at` also has a
  ;; source location.
  (define desc (and (pict? p) (pict-desc p)))
  (define pict-tag (and (name-desc? desc) (name-desc-name desc)))
  (placed x y rot p (or tag pict-tag (source-location-tag source-location name)) nudge))

;; The same name, put on the pict rather than on the placement, for a pict
;; something other than a canvas will place: see `name-desc`.
(define (tag-pict p name)
  (if (and (pict? p) (string? name)) (with-desc p (name-desc name)) p))

;; The position an element actually draws at.
(define (placed-position pl)
  (define n (placed-nudge pl))
  (if (and (list? n) (= 2 (length n)))
      (values (+ (placed-x pl) (first n)) (+ (placed-y pl) (second n)))
      (values (placed-x pl) (placed-y pl))))

(define (pin-placed base pl [ox 0.0] [oy 0.0])
  (define p (placed-pict pl))
  (define-values (px py) (placed-position pl))
  (define x (+ px ox))
  (define y (+ py oy))
  (cond
    [(zero? (placed-rot pl)) (pin-over base x y p)]
    [else
     ;; pict rotates counterclockwise and grows the bounding box, so re-center
     ;; the result on the element's original center.
     (define r (rotate p (- (degrees->radians (placed-rot pl)))))
     (define cx (+ x (/ (pict-width p) 2.0)))
     (define cy (+ y (/ (pict-height p) 2.0)))
     (pin-over base (- cx (/ (pict-width r) 2.0)) (- cy (/ (pict-height r) 2.0)) r)]))

;; A whole slide: a background of the given size with elements pinned on top.
;; `hidden?` is PowerPoint's Hide Slide and Keynote's Skip Slide: the slide
;; stays in the deck and is passed over in the show. It is written to the file
;; and read back from it, so hiding a slide in the editor is an edit like any
;; other rather than something the next regeneration undoes.
;; `build` names the slide this one is a frame of, when a built slide was split
;; into one slide per click. Nothing here draws differently for it -- it is
;; there so that an edit to a shape on one frame can be written to the same
;; shape on the others.
;; `transition` is how the show moves to this slide -- `'up`, `'down`, `'left`,
;; `'right`, `'swirl` or `'none`. It belongs to the show rather than to the
;; deck: nothing is drawn differently for it and it is not written back to the
;; .pptx, because the transitions a deck can hold and the ones `staged.rhm`
;; performs are not the same set.
;; A slide with something laid over it from one stage on.
;;
;; `elems` are `at` forms like any others, so the shape keeps its tag and can be
;; dragged again afterwards -- which is the whole point of writing an edit as
;; source. The staging itself is `settle.rhm`'s: rhombus/pict is loaded on
;; demand, because a deck that does not animate should not pay for it.
(define (from-stage stage slide elems)
  (cond
    ;; A slide written as a function of no arguments is built when it is shown
    ;; rather than when the program loads -- which is how a talk starts part way
    ;; through without paying for the slides it skipped. Wrapped, it stays a
    ;; function, or the wrapping would build every slide in the deck.
    [(and (procedure? slide) (procedure-arity-includes? slide 0))
     (lambda () (from-stage stage (slide) elems))]
    [else
     (define over (dynamic-require '(lib "glide-pptx/settle.rhm") 'over_from))
     (over stage slide
           (lambda (w h)
             (apply slide-canvas #:width w #:height h #:background #f elems)))]))

(define (slide-canvas #:width w #:height h #:background [bg (solid-fill white)]
                      #:hidden? [hidden? #f]
                      #:build [build #f]
                      #:transition [transition #f]
                      . args)
  ;; A list argument is spliced, so a slide can be built with `for List` or
  ;; `for/list` without an `apply`. Generated code never does this; hand-written
  ;; code, which is what the generated code is there to become, wants to. A
  ;; Rhombus `List` is a treelist rather than a list.
  (define placeds
    (let flatten ([xs args])
      (append* (for/list ([x (in-list xs)])
                 (cond
                   [(list? x) (flatten x)]
                   [(treelist? x) (flatten (treelist->list x))]
                   [else (list x)])))))
  (define base
    (cond
      [(not bg) (blank w h)]
      [else (dc (lambda (dc dx dy)
                  (define old (send dc get-brush))
                  (define oldp (send dc get-pen))
                  (send dc set-brush (fill->brush bg w h))
                  (send dc set-pen (new pen% [style 'transparent]))
                  (send dc draw-rectangle dx dy w h)
                  (send dc set-brush old) (send dc set-pen oldp))
                w h)]))
  (with-desc (for/fold ([acc base]) ([pl (in-list placeds)]) (pin-placed acc pl))
             (slide-desc w h bg placeds hidden? transition)))

;; ------------------------------------- text description used by emitted code

;; The typeface a run uses when it does not name one. Generated programs set
;; this once from the deck's theme font, so restyling a whole deck is one edit.
(define current-default-font (make-parameter "Calibri"))

(define (run* text #:font [family (current-default-font)] #:size [size 18.0]
              #:bold? [bold? #f] #:italic? [italic? #f] #:underline? [u? #f]
              #:strike? [s? #f] #:color [color black] #:spacing [spc 0.0]
              #:caps [caps 'none] #:baseline [base 0.0])
  (trun text family size bold? italic? u? s? (color->rgba color) spc caps base 'all))

(define (para* #:align [align 'left] #:level [level 0]
               #:margin-left [marl 0.0] #:indent [indent 0.0]
               #:line-spacing [ls '(percent . 1.0)]
               #:space-before [sb 0.0] #:space-after [sa 0.0]
               #:bullet [b no-bullet]
               . runs)
  (para (if (null? runs)
            (list (run* ""))
            runs)
        align level marl indent ls sb sa b 'all))

;; A text box: the same options as `body*`, plus the box it lays out in.
(define (textbox #:width w #:height h
                 #:anchor [anchor 'top] #:anchor-center? [ac? #f] #:wrap? [wrap? #t]
                 #:autofit [autofit 'none] #:insets [ins default-insets]
                 #:rotate [rot 0.0]
                 . paras)
  (define body (text-body paras anchor ac? wrap? autofit ins rot 'all))
  (define-values (draw-w draw-h draw-x draw-y) (body-drawn-box body w h))
  (define tp (text-pict body draw-w draw-h))
  ;; As for a shape, keep the stored pict extent so the source placement stays
  ;; put while the auto-fit text paints at its displayed offset within it.
  (with-desc (dc (lambda (dc dx dy)
                   (draw-pict tp dc (+ dx draw-x) (+ dy draw-y)))
                 w h)
             (text-desc w h body)))

(define (body* #:anchor [anchor 'top] #:anchor-center? [ac? #f] #:wrap? [wrap? #t]
               #:autofit [autofit 'none] #:insets [ins default-insets] #:rotate [rot 0.0]
               . paras)
  (text-body paras anchor ac? wrap? autofit ins rot 'all))

;; ------------------------------------------------------------------ output

;; racket/draw's PostScript/PDF defaults scale drawing by 0.8 and inset it by a
;; 16pt margin. A slide has to land at exactly its stated size, so both are
;; cleared for the life of the dc.
(define (make-slide-ps-setup)
  (define ps (make-object ps-setup%))
  (send ps set-scaling 1.0 1.0)
  (send ps set-margin 0 0)
  (send ps set-editor-margin 0 0)
  ps)

;; Where a generated program's `media` subdirectory is rooted. A program run
;; directly gets this from `current-load-relative-directory`, but a program
;; loaded with `dynamic-require` -- which is how `raco glide export` reads
;; one -- does not reliably have that set, so a caller can say where the module
;; lives instead.
(define current-media-base (make-parameter #f))

;; Resolves image names against a directory beside the module that calls this,
;; so a generated program can be moved as a folder and still find its images.
;; The directory has to be captured at load time, hence the closure.
(define (media-lookup subdir)
  (define root (build-path (or (current-media-base)
                               (current-load-relative-directory)
                               (current-directory))
                           subdir))
  (lambda (name) (build-path root name)))

;; A slide the editor was told to skip is skipped here too: the PDF and the
;; slideshow are the show, and `~hidden:` is what a program says to keep a
;; slide in the deck and out of it. The deck itself keeps them, marked.
;; The transition a canvas asked for, or #f. A slide that has been wrapped --
;; animated, faded, combined with another -- is no longer the canvas, and this
;; says nothing about it: the show falls back to its own default.
(define (canvas-transition p)
  (define d (and (pict? p) (pict-desc p)))
  (and (slide-desc? d) (slide-desc-transition d)))

;; Whether a canvas said `~hidden:`. Answers #f for anything that is not a
;; canvas, which includes a slide that has been given stages.
;; Anything that is not a pict is not a canvas, so it is not a hidden one: a
;; slide may reach here as a Rhombus `Pict` or as a thunk, and asking either of
;; those what it says about itself is asking the wrong question rather than an
;; error.
(define (canvas-hidden? p)
  (and (pict? p)
       (let ([d (pict-desc p)])
         (and (slide-desc? d) (slide-desc-hidden? d) #t))))

;; Whether a slide is numbered in its own corner. Off by default: it is ink the
;; deck did not ask for. A talk turns it on at the top level rather than inside
;; `module main`, so that the deck the exporter writes carries the same number
;; the show draws -- the number used to be drawn by the slide assembler, which
;; is downstream of everything but the show, so the deck came out with the
;; corner empty.
(define slide-numbers? (box #f))
(define (set-slide-numbers! [on? #t]) (set-box! slide-numbers? (and on? #t)))
(define (slide-numbers-on?) (unbox slide-numbers?))

;; The number in the corner, at the slide's own scale. Sized and inset as a
;; fraction of the slide, so it lands in the same place whether the slide is
;; 720 points wide or 1920, and so the show and the deck agree once the show has
;; scaled the slide to fit its window.
(define (number-on p n)
  (define h (pict-height p))
  (define size (max 8.0 (* 0.032 h)))
  (define body
    (text-body (list (para (list (trun (number->string n) (current-default-font) size
                                       #t #f #f #f (rgba 0 0 0 1.0) 0.0 'none 0.0 'all))
                           'right 0 0.0 0.0 '(percent . 1.0) 0.0 0.0 no-bullet 'all))
               'bottom #f #f 'none (insets 0.0 0.0 0.0 0.0) 0.0 'all))
  (define-values (tw th) (body-natural-size body (* 0.12 (pict-width p)) size))
  (define tag (text-pict body (max 1.0 tw) (max 1.0 th)))
  (pin-over p
            (- (pict-width p) (pict-width tag) (* 0.006 (pict-width p)))
            (- h (pict-height tag) (* 0.006 h))
            tag))

;; A slide written as a function of no arguments is not built yet, and asking
;; whether it is hidden would build it -- which is the one thing being lazy is
;; for. It is kept; a lazy deck leaves out what it does not want shown.
;;
;; The number is put on here, where every reader of a slide list passes: the
;; show, the exporter and the backup PDF. A slide that is still a thunk stays
;; one -- it is wrapped, not built -- because building it early is the one thing
;; being lazy is for.
(define (shown-picts picts)
  (for/list ([p (in-list picts)]
             #:unless (and (not (procedure? p))
                           (let ([d (pict-desc p)])
                             (and (slide-desc? d) (slide-desc-hidden? d)))))
    p))

(define (picts->pdf picts0 path #:width w #:height h)
  (define picts (shown-picts picts0))
  (define dc (parameterize ([current-ps-setup (make-slide-ps-setup)])
               (new pdf-dc% [interactive #f] [use-paper-bbox #f] [as-eps #f]
                    [output path] [width w] [height h])))
  (send dc start-doc "glide-pptx")
  (for ([p (in-list picts)])
    (send dc start-page)
    (send dc set-smoothing 'smoothed)
    (draw-pict p dc 0 0)
    (send dc end-page))
  (send dc end-doc)
  path)

;; Convenience for emitted programs: one call from a list of slide picts.
(define (deck->pdf picts path #:width [w #f] #:height [h #f])
  (define w* (or w (if (pair? picts) (pict-width (first picts)) 720.0)))
  (define h* (or h (if (pair? picts) (pict-height (first picts)) 540.0)))
  (picts->pdf picts path #:width w* #:height h*))

(define (pict->png p path #:scale [s 1.0])
  (define bm (make-bitmap (max 1 (inexact->exact (ceiling (* s (pict-width p)))))
                          (max 1 (inexact->exact (ceiling (* s (pict-height p)))))))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-smoothing 'smoothed)
  (send dc set-brush (new brush% [color (make-object color% 255 255 255)]))
  (send dc set-pen (new pen% [style 'transparent]))
  (send dc draw-rectangle 0 0 (send bm get-width) (send bm get-height))
  (draw-pict (scale p s) dc 0 0)
  (send bm save-file path 'png)
  path)
