#lang racket/base
;; Preset shape geometry: DrawingML `prstGeom` names to drawing paths.
;;
;; Each builder receives the shape's width and height in points plus its
;; adjustment values, and returns a `dc-path%` in shape-local coordinates. Only
;; the presets that actually show up in decks are spelled out; anything else
;; falls back to a rectangle and is reported, so a missing preset shows up as a
;; diff rather than as a crash.
(require racket/class racket/math racket/list racket/string
         racket/draw
         "ir.rkt")
(provide preset-path preset-known? preset-names path-ends custom-path-ends
         custom-path
         geometry-closed?)

;; Adjustment values are 100000ths of the shape's smaller side, or of a
;; preset-specific reference; `adj` looks one up with a default.
(define (adj-value adjust name default)
  (define hit (assoc name adjust))
  (cond
    [(not hit) default]
    [else
     ;; Formulas are "val 16667" for a literal; anything else we leave alone.
     (define m (regexp-match #rx"^ *val +(-?[0-9]+) *$" (cdr hit)))
     (if m (/ (string->number (cadr m)) 100000.0) default)]))

(define (path-of pts #:close? [close? #t])
  (define p (new dc-path%))
  (when (pair? pts)
    (send p move-to (car (first pts)) (cdr (first pts)))
    (for ([q (in-list (rest pts))]) (send p line-to (car q) (cdr q)))
    (when close? (send p close)))
  p)


;; Scales and shifts points so they exactly fill a w x h box. A star with an odd
;; number of points has no vertex at the bottom, so its raw outline is shorter
;; than its box; PowerPoint's presets carry hand-tuned factors to compensate,
;; and normalizing computes the same thing without the magic numbers.
(define (normalize-points pts w h)
  (define xs (map car pts)) (define ys (map cdr pts))
  (define x0 (apply min xs)) (define x1 (apply max xs))
  (define y0 (apply min ys)) (define y1 (apply max ys))
  (define sx (if (<= (- x1 x0) 1e-9) 1.0 (/ w (- x1 x0))))
  (define sy (if (<= (- y1 y0) 1e-9) 1.0 (/ h (- y1 y0))))
  (for/list ([p (in-list pts)])
    (cons (* sx (- (car p) x0)) (* sy (- (cdr p) y0)))))

(define (star-path w h points inner-ratio)
  (define pts
    (for/list ([i (in-range (* 2 points))])
      ;; Start at the top so a 5-point star sits the way PowerPoint draws it.
      (define a (- (* i (/ pi points)) (/ pi 2.0)))
      (define r (if (even? i) 1.0 inner-ratio))
      (cons (* r (cos a)) (* r (sin a)))))
  (path-of (normalize-points pts w h)))

(define (regular-polygon-path w h n [rotate-by (- (/ pi 2.0))])
  (path-of (normalize-points
            (for/list ([i (in-range n)])
              (define a (+ rotate-by (* i (/ (* 2 pi) n))))
              (cons (cos a) (sin a)))
            w h)))

(define (rounded-rect-path w h r)
  (define rr (max 0.0 (min r (/ w 2.0) (/ h 2.0))))
  (define p (new dc-path%))
  (cond
    [(zero? rr) (send p rectangle 0 0 w h)]
    [else
     (send p move-to rr 0)
     (send p line-to (- w rr) 0)
     (send p arc (- w (* 2 rr)) 0 (* 2 rr) (* 2 rr) (/ pi 2.0) 0 #f)
     (send p line-to w (- h rr))
     (send p arc (- w (* 2 rr)) (- h (* 2 rr)) (* 2 rr) (* 2 rr) 0 (- (/ pi 2.0)) #f)
     (send p line-to rr h)
     (send p arc 0 (- h (* 2 rr)) (* 2 rr) (* 2 rr) (- (/ pi 2.0)) pi #f)
     (send p line-to 0 rr)
     (send p arc 0 0 (* 2 rr) (* 2 rr) pi (/ pi 2.0) #f)
     (send p close)])
  p)

;; A horizontal arrow pointing right: `head` is the head length as a fraction of
;; the width, `tail` the shaft thickness as a fraction of the height.
(define (right-arrow-path w h head tail)
  (define hy (* h (/ (- 1.0 tail) 2.0)))
  ;; The head's length is a fraction of the shape's *shorter* side, not of its
  ;; width -- `dx2 = ss * adj2` in the spec, where ss is min(w, h). Taking it
  ;; from the width made the head half of a wide arrow instead of a third, which
  ;; is what LibreOffice and PowerPoint both draw.
  (define hx (max 0.0 (- w (* (min w h) head))))
  (path-of (list (cons 0 hy) (cons hx hy) (cons hx 0)
                 (cons w (/ h 2.0)) (cons hx h) (cons hx (- h hy)) (cons 0 (- h hy)))))

(define (mirror-x path w)
  ;; dc-path% has no reflect, so re-walk the points through a transform.
  (define p (new dc-path%))
  (send p append path)
  (send p transform (vector -1.0 0.0 0.0 1.0 w 0.0))
  p)

(define (transpose-path path w h)
  (define p (new dc-path%))
  (send p append path)
  (send p transform (vector 0.0 1.0 1.0 0.0 0.0 0.0))
  p)

(define builders (make-hash))
(define-syntax-rule (define-preset (name ...) (w h adjust) body ...)
  (for ([n (in-list (list name ...))])
    (hash-set! builders n (lambda (w h adjust) body ...))))

;; ------------------------------------------------------------- rectangles

(define-preset ("rect" "flowChartProcess" "flowChartPredefinedProcess"
                "flowChartInternalStorage" "actionButtonBlank")
  (w h adjust)
  (let ([p (new dc-path%)]) (send p rectangle 0 0 w h) p))

(define-preset ("roundRect" "flowChartAlternateProcess" "round1Rect" "round2SameRect")
  (w h adjust)
  (rounded-rect-path w h (* (adj-value adjust "adj" 0.16667) (min w h))))

(define-preset ("flowChartTerminator") (w h adjust)
  (rounded-rect-path w h (/ h 2.0)))

(define-preset ("snip1Rect" "snip2SameRect") (w h adjust)
  (let ([c (* (adj-value adjust "adj" 0.16667) (min w h))])
    (path-of (list (cons c 0) (cons (- w c) 0) (cons w c)
                   (cons w h) (cons 0 h) (cons 0 c)))))

;; ---------------------------------------------------------------- ellipses

(define-preset ("ellipse" "flowChartConnector" "actionButtonSound") (w h adjust)
  (let ([p (new dc-path%)]) (send p ellipse 0 0 w h) p))

(define-preset ("donut") (w h adjust)
  ;; The ring cannot be thicker than half the shape -- the spec pins this
  ;; adjustment to 50% -- and a file that says otherwise gave a negative inner
  ;; radius, which is not a shape a dc will draw.
  (let* ([t (max 0.0 (min 0.5 (adj-value adjust "adj" 0.25)))]
         [p (new dc-path%)]
         [ix (* w t)] [iy (* h t)])
    (send p ellipse 0 0 w h)
    (send p ellipse ix iy (- w (* 2 ix)) (- h (* 2 iy)))
    p))

(define-preset ("teardrop") (w h adjust)
  (let ([p (new dc-path%)])
    (send p move-to (/ w 2.0) 0)
    (send p arc 0 0 w h (/ pi 2.0) (* 2 pi) #f)
    (send p line-to w 0)
    (send p close)
    p))

;; --------------------------------------------------------------- triangles

(define-preset ("triangle" "isoscelesTriangle") (w h adjust)
  (path-of (list (cons (* w (adj-value adjust "adj" 0.5)) 0) (cons w h) (cons 0 h))))

(define-preset ("rtTriangle") (w h adjust)
  (path-of (list (cons 0 0) (cons 0 h) (cons w h))))

(define-preset ("flowChartDecision" "diamond") (w h adjust)
  (path-of (list (cons (/ w 2.0) 0) (cons w (/ h 2.0)) (cons (/ w 2.0) h) (cons 0 (/ h 2.0)))))

;; --------------------------------------------------------------- polygons

(define-preset ("pentagon" "flowChartPreparation") (w h adjust)
  (regular-polygon-path w h 5))
(define-preset ("hexagon") (w h adjust)
  ;; PowerPoint's hexagon is flat-topped, and its adjustment is a fraction of
  ;; the smaller side rather than of the width.
  (let ([c (* (min w h) (adj-value adjust "adj" 0.25))])
    (path-of (list (cons c 0) (cons (- w c) 0) (cons w (/ h 2.0))
                   (cons (- w c) h) (cons c h) (cons 0 (/ h 2.0))))))
(define-preset ("heptagon") (w h adjust) (regular-polygon-path w h 7))
(define-preset ("octagon") (w h adjust) (regular-polygon-path w h 8 (/ pi 8.0)))
(define-preset ("decagon") (w h adjust) (regular-polygon-path w h 10))
(define-preset ("dodecagon") (w h adjust) (regular-polygon-path w h 12))

(define-preset ("parallelogram") (w h adjust)
  (let ([o (* w (adj-value adjust "adj" 0.25))])
    (path-of (list (cons o 0) (cons w 0) (cons (- w o) h) (cons 0 h)))))

(define-preset ("trapezoid") (w h adjust)
  (let ([o (* w (adj-value adjust "adj" 0.25))])
    (path-of (list (cons o 0) (cons (- w o) 0) (cons w h) (cons 0 h)))))

;; The point is set in from the right by a fraction of the *shortest* side, not
;; of the width, and the fraction is half rather than a quarter: both of those
;; are what the preset says, and both were wrong here, so a wide pentagon or
;; chevron came out with too shallow a point and too shallow a notch.
(define (point-inset w h adjust)
  (* (min w h) (adj-value adjust "adj" 0.5)))

(define-preset ("homePlate") (w h adjust)
  (let ([o (point-inset w h adjust)])
    (path-of (list (cons 0 0) (cons (- w o) 0) (cons w (/ h 2.0))
                   (cons (- w o) h) (cons 0 h)))))

(define-preset ("chevron") (w h adjust)
  (let ([o (point-inset w h adjust)])
    (path-of (list (cons 0 0) (cons (- w o) 0) (cons w (/ h 2.0)) (cons (- w o) h)
                   (cons 0 h) (cons o (/ h 2.0))))))

(define-preset ("plus" "mathPlus") (w h adjust)
  (let* ([t (adj-value adjust "adj" 0.25)]
         [ax (* w t)] [ay (* h t)])
    (path-of (list (cons ax 0) (cons (- w ax) 0) (cons (- w ax) ay) (cons w ay)
                   (cons w (- h ay)) (cons (- w ax) (- h ay)) (cons (- w ax) h)
                   (cons ax h) (cons ax (- h ay)) (cons 0 (- h ay))
                   (cons 0 ay) (cons ax ay)))))

;; ------------------------------------------------------------------ stars

(define-preset ("star4") (w h adjust) (star-path w h 4 (adj-value adjust "adj" 0.375)))
(define-preset ("star5") (w h adjust) (star-path w h 5 (adj-value adjust "adj" 0.382)))
(define-preset ("star6") (w h adjust) (star-path w h 6 (adj-value adjust "adj" 0.577)))
(define-preset ("star7") (w h adjust) (star-path w h 7 (adj-value adjust "adj" 0.6)))
(define-preset ("star8") (w h adjust) (star-path w h 8 (adj-value adjust "adj" 0.625)))
(define-preset ("star10") (w h adjust) (star-path w h 10 (adj-value adjust "adj" 0.7)))
(define-preset ("star12") (w h adjust) (star-path w h 12 (adj-value adjust "adj" 0.75)))

;; ----------------------------------------------------------------- arrows

(define-preset ("rightArrow") (w h adjust)
  (right-arrow-path w h (adj-value adjust "adj2" 0.5) (adj-value adjust "adj1" 0.5)))

(define-preset ("leftArrow") (w h adjust)
  (mirror-x (right-arrow-path w h (adj-value adjust "adj2" 0.5)
                              (adj-value adjust "adj1" 0.5))
            w))

(define-preset ("downArrow") (w h adjust)
  (transpose-path (right-arrow-path h w (adj-value adjust "adj2" 0.5)
                                    (adj-value adjust "adj1" 0.5))
                  w h))

(define-preset ("upArrow") (w h adjust)
  (transpose-path (mirror-x (right-arrow-path h w (adj-value adjust "adj2" 0.5)
                                              (adj-value adjust "adj1" 0.5))
                            h)
                  w h))

(define-preset ("leftRightArrow") (w h adjust)
  (let* ([tail (adj-value adjust "adj1" 0.5)]
         [head (adj-value adjust "adj2" 0.25)]
         [hy (* h (/ (- 1.0 tail) 2.0))]
         [hx (* w head)])
    (path-of (list (cons 0 (/ h 2.0)) (cons hx 0) (cons hx hy) (cons (- w hx) hy)
                   (cons (- w hx) 0) (cons w (/ h 2.0)) (cons (- w hx) h)
                   (cons (- w hx) (- h hy)) (cons hx (- h hy)) (cons hx h)))))

(define-preset ("upDownArrow") (w h adjust)
  (let* ([tail (adj-value adjust "adj1" 0.5)]
         [head (adj-value adjust "adj2" 0.25)]
         [hx (* w (/ (- 1.0 tail) 2.0))]
         [hy (* h head)])
    (path-of (list (cons (/ w 2.0) 0) (cons w hy) (cons (- w hx) hy)
                   (cons (- w hx) (- h hy)) (cons w (- h hy)) (cons (/ w 2.0) h)
                   (cons 0 (- h hy)) (cons hx (- h hy)) (cons hx hy) (cons 0 hy)))))

;; ------------------------------------------------------------------- lines

(define-preset ("line" "straightConnector1") (w h adjust)
  (path-of (list (cons 0 0) (cons w h)) #:close? #f))

(define-preset ("bentConnector2") (w h adjust)
  (path-of (list (cons 0 0) (cons w 0) (cons w h)) #:close? #f))

(define-preset ("bentConnector3") (w h adjust)
  (let ([m (* w (adj-value adjust "adj1" 0.5))])
    (path-of (list (cons 0 0) (cons m 0) (cons m h) (cons w h)) #:close? #f)))

(define-preset ("curvedConnector3") (w h adjust)
  (let ([p (new dc-path%)] [m (* w (adj-value adjust "adj1" 0.5))])
    (send p move-to 0 0)
    (send p curve-to m 0 m h w h)
    p))

;; ----------------------------------------------------------------- lookups

(define (preset-known? name) (hash-has-key? builders name))
(define (preset-names) (sort (hash-keys builders) string<?))

;; The path for `name` at this size, with flips applied as coordinate
;; reflections so a shape's text is left unmirrored the way PowerPoint does it.
(define (preset-path name w h adjust #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define build (hash-ref builders name #f))
  (define base
    (cond
      [build (build (max w 0.0) (max h 0.0) adjust)]
      [else (let ([p (new dc-path%)]) (send p rectangle 0 0 w h) p)]))
  (cond
    [(or fh fv)
     (define p (new dc-path%))
     (send p append base)
     (send p transform (vector (if fh -1.0 1.0) 0.0 0.0 (if fv -1.0 1.0)
                              (if fh w 0.0) (if fv h 0.0)))
     p]
    [else base]))

;; Whether a preset's outline is a closed region, which decides whether a fill
;; can apply at all.
(define (geometry-closed? name)
  (not (member name '("line" "straightConnector1" "bentConnector2" "bentConnector3"
                      "curvedConnector3" "bentConnector4" "bentConnector5"
                      "curvedConnector2" "curvedConnector4" "curvedConnector5"))))

;; ------------------------------------------------------------ custom paths

;; Custom geometry arrives in its own coordinate space; scale it onto w x h.
;; ------------------------------------------------------ ends of an open path

;; Where a line starts and finishes, and which way it points there, so a
;; decoration can be drawn at each end. #f for a shape that has no ends to speak
;; of -- a closed one, or a preset whose path this does not know.
;;
;; Returns (list start-x start-y start-angle end-x end-y end-angle), the angles
;; in radians and pointing *outward*, which is the way an arrowhead faces.
(define (path-ends name w h #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define pts (open-path-points name w h))
  (and pts (>= (length pts) 2)
       (let* ([flip (lambda (p)
                      (cons (if fh (- w (car p)) (car p))
                            (if fv (- h (cdr p)) (cdr p))))]
              [ps (map flip pts)]
              [a (first ps)] [a2 (second ps)]
              [z (last ps)] [z2 (list-ref ps (- (length ps) 2))])
         (list (car a) (cdr a) (atan (- (cdr a) (cdr a2)) (- (car a) (car a2)))
               (car z) (cdr z) (atan (- (cdr z) (cdr z2)) (- (car z) (car z2)))))))

;; The corner points of the open presets, in order. Only the straight ones: a
;; curve's end direction is its tangent, which these are not.
(define (open-path-points name w h)
  (case name
    [("line" "straightConnector1") (list (cons 0.0 0.0) (cons w h))]
    [("bentConnector2") (list (cons 0.0 0.0) (cons w 0.0) (cons w h))]
    [else #f]))

;; The same, for a custom path: its first and last points, with the ones next to
;; them for direction.
(define (custom-path-ends geom w h #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define sx (path-scale (custom-geom-w geom) w))
  (define sy (path-scale (custom-geom-h geom) h))
  (define paths (custom-geom-paths geom))
  (and (pair? paths)
       (let* ([pts (command-points (first paths) sx sy)]
              [flip (lambda (p) (cons (if fh (- w (car p)) (car p))
                                      (if fv (- h (cdr p)) (cdr p))))]
              [ps (map flip pts)])
         (and (>= (length ps) 2)
              (let ([a (first ps)] [a2 (second ps)]
                    [z (last ps)] [z2 (list-ref ps (- (length ps) 2))])
                (and (not (and (= (car a) (car z)) (= (cdr a) (cdr z))))
                     (list (car a) (cdr a) (atan (- (cdr a) (cdr a2)) (- (car a) (car a2)))
                           (car z) (cdr z)
                           (atan (- (cdr z) (cdr z2)) (- (car z) (car z2))))))))))

;; The points a subpath visits, scaled onto the box. A curve contributes its
;; control points, which is enough for the direction at an end.
(define (command-points subpath sx sy)
  (append*
   (for/list ([cmd (in-list subpath)])
     (case (car cmd)
       [(move line) (list (cons (* sx (car (second cmd))) (* sy (cdr (second cmd)))))]
       [(curve quad) (for/list ([p (in-list (rest cmd))])
                       (cons (* sx (car p)) (* sy (cdr p))))]
       [else '()]))))

;; What one unit of a path's own space is worth in points. A space of 0 means
;; the coordinates are EMU inside the shape, which is a twelve-thousandth of a
;; point each and not a stretch onto the box.
(define (path-scale space extent)
  (if (zero? space) (/ 1.0 12700.0) (/ extent (exact->inexact space))))

(define (custom-path geom w h #:flip-h? [fh #f] #:flip-v? [fv #f])
  (define sx (path-scale (custom-geom-w geom) w))
  (define sy (path-scale (custom-geom-h geom) h))
  (define p (new dc-path%))
  (for ([subpath (in-list (custom-geom-paths geom))])
    (define (X v) (* sx v)) (define (Y v) (* sy v))
    ;; A path in the wild does not always open with a move, and dc-path%
    ;; refuses a line before one. The first point opens the path whatever it
    ;; claims to be.
    (define open? (box #f))
    (define (goto! x y)
      (cond [(unbox open?) (send p line-to x y)]
            [else (send p move-to x y) (set-box! open? #t)]))
    (for ([cmd (in-list subpath)])
      (case (car cmd)
        [(move) (send p move-to (X (car (second cmd))) (Y (cdr (second cmd))))
                (set-box! open? #t)]
        [(line) (goto! (X (car (second cmd))) (Y (cdr (second cmd))))]
        [(curve) (let ([ps (rest cmd)])
                   (when (= 3 (length ps))
                     (unless (unbox open?)
                       (goto! (X (car (first ps))) (Y (cdr (first ps)))))
                     (send p curve-to
                           (X (car (first ps))) (Y (cdr (first ps)))
                           (X (car (second ps))) (Y (cdr (second ps)))
                           (X (car (third ps))) (Y (cdr (third ps))))))]
        ;; A quadratic segment promoted to a cubic with the standard 2/3 rule.
        [(quad) (let ([ps (rest cmd)])
                  (when (= 2 (length ps))
                    (unless (unbox open?)
                      (goto! (X (car (first ps))) (Y (cdr (first ps)))))
                    (send p curve-to
                          (X (car (first ps))) (Y (cdr (first ps)))
                          (X (car (first ps))) (Y (cdr (first ps)))
                          (X (car (second ps))) (Y (cdr (second ps))))))]
        [(close) (when (unbox open?) (send p close)) (set-box! open? #f)]
        [else (void)])))
  (cond
    [(or fh fv)
     (define q (new dc-path%))
     (send q append p)
     (send q transform (vector (if fh -1.0 1.0) 0.0 0.0 (if fv -1.0 1.0)
                              (if fh w 0.0) (if fv h 0.0)))
     q]
    [else p]))
