#lang racket/base
;; record-dc% datum -> display list.
;;
;; This is the only module that knows the recorded datum's shape, which is not a
;; documented stability promise. Everything downstream sees the display list, so
;; if the datum ever changes -- or turns out to lose something -- this module is
;; replaced by a `dc<%>` and no backend notices.
;;
;; The datum is a command stream over dc state, so interpreting it means keeping
;; the same state a dc would: a current matrix, a pen, a brush, a font, an alpha
;; and a clip.
(require racket/list racket/math racket/class racket/draw racket/promise
         "draw-ir.rkt")
(provide pict->display-page datum->items current-adapt-warnings)

(define current-adapt-warnings (make-parameter #f))
(define (warn! fmt . args)
  (define b (current-adapt-warnings))
  (when b (set-box! b (cons (apply format fmt args) (unbox b)))))

;; --------------------------------------------------------------- measuring

(define measure-dc
  (delay (let ([d (new bitmap-dc% [bitmap (make-bitmap 8 8)])])
           (send d set-smoothing 'smoothed)
           d)))

;; `get-text-extent` rounds to whole device units, so measure large and divide.
;; `make-font` refuses a size above 1024, so the multiple backs off for a large
;; font rather than failing on it.
(define MEASURE-SCALE 16.0)
(define (measure-scale-for size)
  (max 1.0 (min MEASURE-SCALE (/ 1000.0 (max 0.01 size)))))

(define (font-of spec [scale 1.0])
  (define size (max 0.01 (* scale (first spec))))
  (define face (second spec))
  (make-font #:size size
             #:face (and (string? face) face)
             #:family (third spec)
             #:style (fourth spec)
             #:weight (fifth spec)
             #:underlined? (and (sixth spec) #t)
             #:smoothing (seventh spec)
             #:size-in-pixels? (and (eighth spec) #t)
             #:hinting (ninth spec)))

;; (values width height ascent) in device units for text drawn with `spec`.
(define (measure-text str spec combine?)
  (define k (measure-scale-for (first spec)))
  (define f (font-of spec k))
  (define-values (w h d _a) (send (force measure-dc) get-text-extent str f (and combine? #t)))
  (values (/ w k) (/ h k) (/ (- h d) k)))

;; The typeface to name in the output. A font often gives only a family, and
;; the generic names a family resolves to here -- "Sans", "Serif", "Monospace" --
;; mean nothing to PowerPoint, so they become the conventional faces every
;; system has. That is a real substitution, and it is why text set by family
;; rather than by face does not measure identically after a round trip.
(define family-faces
  (hash "Sans" "Arial" "Helvetica" "Arial"
        "Serif" "Times New Roman" "Monospace" "Courier New"))

(define (spec-face spec)
  (define face (second spec))
  (cond
    [(string? face) face]
    [else
     (define builtin (get-family-builtin-face (third spec)))
     (hash-ref family-faces builtin (lambda () (or builtin "Arial")))]))

;; A font's size as a device measurement. racket/draw treats a "point" as a 96th
;; of an inch unless told otherwise, so a size given in points has to be scaled
;; before it means anything on a dc whose unit is the point.
(define (font-device-size spec)
  (define size (first spec))
  (if (eighth spec) size (* size (/ 96.0 72.0))))

;; ------------------------------------------------------------------- paint

(define (color-of c alpha)
  (rgba* (first c) (second c) (third c)
         (* (max 0.0 (min 1.0 (fourth c))) alpha)))

(define dash-of
  (hash 'solid 'solid 'dot 'dot 'long-dash 'dash 'short-dash 'dash
        'dot-dash 'dash-dot 'xor-dot 'dot 'xor-long-dash 'dash
        'xor-short-dash 'dash 'xor-dot-dash 'dash-dot 'xor 'solid))

(define (pen-of spec alpha ctm)
  (define style (third spec))
  (cond
    [(eq? style 'transparent) #f]
    [else
     ;; A width of 0 means a hairline; anything else scales with the drawing.
     (define w (second spec))
     (pen* (color-of (first spec) alpha)
           (if (zero? w) 0.0 (* w (mat-scale-factor ctm)))
           (hash-ref dash-of style 'solid)
           (case (fourth spec) [(round) 'round] [(projecting) 'square] [else 'flat])
           (case (fifth spec) [(round) 'round] [(bevel) 'bevel] [else 'miter])
           ;; A recorded pen is what a dc drew with; the ends were already drawn
           ;; as filled shapes by then.
           #f #f #f)]))

(define (brush-of spec alpha ctm)
  (define style (second spec))
  (define grad (fourth spec))
  (cond
    [(eq? style 'transparent) #f]
    [(list? grad) (gradient-of grad alpha ctm)]
    ;; A stipple is an image brush; approximate with the brush color and say so.
    [(third spec) (warn! "stipple brush drawn as a solid color")
                  (fill:solid (color-of (first spec) alpha))]
    [else (fill:solid (color-of (first spec) alpha))]))

;; A linear gradient records five fields and a radial one seven, which is how
;; they are told apart.
(define (gradient-of g alpha ctm)
  (define (stops-of raw)
    (for/list ([s (in-list raw)])
      (list (exact->inexact (first s))
            (color-of (rest s) alpha))))
  (cond
    [(= 5 (length g))
     (define-values (x0 y0) (mat-apply ctm (first g) (second g)))
     (define-values (x1 y1) (mat-apply ctm (third g) (fourth g)))
     (fill:linear x0 y0 x1 y1 (stops-of (fifth g)))]
    [(= 7 (length g))
     (define-values (x0 y0) (mat-apply ctm (first g) (second g)))
     (define-values (x1 y1) (mat-apply ctm (fourth g) (fifth g)))
     (define s (mat-scale-factor ctm))
     (fill:radial x0 y0 (* s (third g)) x1 y1 (* s (sixth g)) (stops-of (seventh g)))]
    [else (warn! "unrecognized gradient with ~a fields" (length g)) #f]))

;; ------------------------------------------------------------------ shapes

;; Places an axis-aligned box through the matrix. Returns #f when the matrix
;; shears, in which case the caller emits a path instead.
(define (place-box ctm x y w h)
  (define-values (kind p q) (mat-decompose ctm))
  (case kind
    [(axis)
     (define-values (x0 y0) (mat-apply ctm x y))
     (define-values (x1 y1) (mat-apply ctm (+ x w) (+ y h)))
     (list (min x0 x1) (min y0 y1) (abs (- x1 x0)) (abs (- y1 y0)) 0.0)]
    [(rotate)
     (define-values (cx cy) (mat-apply ctm (+ x (/ w 2.0)) (+ y (/ h 2.0))))
     (define nw (* p w)) (define nh (* p h))
     (list (- cx (/ nw 2.0)) (- cy (/ nh 2.0)) nw nh q)]
    [else #f]))

(define (rect-segs x y w h)
  (list (seg:move x y) (seg:line (+ x w) y) (seg:line (+ x w) (+ y h))
        (seg:line x (+ y h)) (seg:close)))

;; A quarter-circle Bezier constant, for turning ellipses and arcs into paths.
(define KAPPA 0.5522847498307936)

(define (ellipse-segs x y w h)
  (define rx (/ w 2.0)) (define ry (/ h 2.0))
  (define cx (+ x rx)) (define cy (+ y ry))
  (define kx (* KAPPA rx)) (define ky (* KAPPA ry))
  (list (seg:move (+ cx rx) cy)
        (seg:curve (+ cx rx) (+ cy ky) (+ cx kx) (+ cy ry) cx (+ cy ry))
        (seg:curve (- cx kx) (+ cy ry) (- cx rx) (+ cy ky) (- cx rx) cy)
        (seg:curve (- cx rx) (- cy ky) (- cx kx) (- cy ry) cx (- cy ry))
        (seg:curve (+ cx kx) (- cy ry) (+ cx rx) (- cy ky) (+ cx rx) cy)
        (seg:close)))

(define (transform-segs ctm segs)
  (for/list ([s (in-list segs)])
    (cond
      [(seg:move? s) (let-values ([(x y) (mat-apply ctm (seg:move-x s) (seg:move-y s))])
                       (seg:move x y))]
      [(seg:line? s) (let-values ([(x y) (mat-apply ctm (seg:line-x s) (seg:line-y s))])
                       (seg:line x y))]
      [(seg:curve? s)
       (let*-values ([(x1 y1) (mat-apply ctm (seg:curve-x1 s) (seg:curve-y1 s))]
                     [(x2 y2) (mat-apply ctm (seg:curve-x2 s) (seg:curve-y2 s))]
                     [(x y) (mat-apply ctm (seg:curve-x s) (seg:curve-y s))])
         (seg:curve x1 y1 x2 y2 x y))]
      [else s])))

;; The recorded path is (cons closed-subpaths open-points): each closed subpath
;; is a point list that closes implicitly, and the trailing points form one open
;; subpath. A cons is a vertex; a vector holds the two control points that
;; precede the next vertex.
(define (path-datum->segs data dx dy)
  (define (points->segs pts close?)
    (let loop ([pts pts] [first? #t] [acc '()])
      (cond
        [(null? pts) (reverse (if close? (cons (seg:close) acc) acc))]
        [(vector? (car pts))
         (define v (car pts))
         (define end (and (pair? (cdr pts)) (cadr pts)))
         (cond
           [(pair? end)
            (loop (cddr pts) #f
                  (cons (seg:curve (+ dx (vector-ref v 0)) (+ dy (vector-ref v 1))
                                   (+ dx (vector-ref v 2)) (+ dy (vector-ref v 3))
                                   (+ dx (car end)) (+ dy (cdr end)))
                        acc))]
           [else (loop (cdr pts) #f acc)])]
        [else
         (define p (car pts))
         (loop (cdr pts) #f
               (cons (if first?
                         (seg:move (+ dx (car p)) (+ dy (cdr p)))
                         (seg:line (+ dx (car p)) (+ dy (cdr p))))
                     acc))])))
  (define closed (if (pair? data) (car data) '()))
  (define open (if (pair? data) (cdr data) '()))
  (append (append* (for/list ([sp (in-list closed)]) (points->segs sp #t)))
          (if (pair? open) (points->segs open #f) '())))

(define (points->path-segs pts dx dy close?)
  (define ps (for/list ([p (in-list pts)])
               (cons (+ dx (car p)) (+ dy (cdr p)))))
  (cond
    [(null? ps) '()]
    [else
     (append (list (seg:move (car (first ps)) (cdr (first ps))))
             (for/list ([p (in-list (rest ps))]) (seg:line (car p) (cdr p)))
             (if close? (list (seg:close)) '()))]))

;; An arc as Bezier quarters, which is close enough that no renderer can tell.
(define (arc-segs x y w h start end)
  (define rx (/ w 2.0)) (define ry (/ h 2.0))
  (define cx (+ x rx)) (define cy (+ y ry))
  ;; racket/draw measures counterclockwise in a y-down space.
  (define (at a) (values (+ cx (* rx (cos a))) (- cy (* ry (sin a)))))
  (define sweep (- end start))
  (define steps (max 1 (exact-ceiling (/ (abs sweep) (/ pi 2.0)))))
  (define step (/ sweep steps))
  (define k (* (/ 4.0 3.0) (tan (/ step 4.0))))
  (define-values (x0 y0) (at start))
  (cons (seg:move x0 y0)
        (for/list ([i (in-range steps)])
          (define a0 (+ start (* i step)))
          (define a1 (+ a0 step))
          (define-values (px py) (at a0))
          (define-values (qx qy) (at a1))
          (seg:curve (- px (* k rx (sin a0) -1)) (- py (* k ry (cos a0)))
                     (+ qx (* k rx (sin a1) -1)) (+ qy (* k ry (cos a1)))
                     qx qy))))

;; Only the drawn section of a bitmap is worth putting in a package, so the
;; source rectangle is cut out of the recorded ARGB rows here.
(define (exact-round* v) (inexact->exact (round v)))

(define (crop-argb data w h sx sy sw sh)
  (define cx (max 0 (min sx (sub1 (max 1 w)))))
  (define cy (max 0 (min sy (sub1 (max 1 h)))))
  (define cw (max 1 (min sw (- w cx))))
  (define ch (max 1 (min sh (- h cy))))
  (cond
    [(and (= cw w) (= ch h) (zero? cx) (zero? cy)) (values data w h)]
    [else
     (define out (make-bytes (* 4 cw ch)))
     (for ([row (in-range ch)])
       (define from (* 4 (+ (* (+ cy row) w) cx)))
       (bytes-copy! out (* 4 row cw) data from (+ from (* 4 cw))))
     (values out cw ch)]))

;; ------------------------------------------------------------- interpreter

(struct st (ctm im ox oy sx sy rot pen brush font alpha text-fg clip) #:mutable)

(define (fresh-state)
  (st identity-matrix identity-matrix 0.0 0.0 1.0 1.0 0.0
      #f #f (list 12 #f 'default 'normal 'normal #f 'default #f 'aligned (hash))
      1.0 (list 0 0 0 1.0) #f))

(define (recompute! s)
  (set-st-ctm! s (mat* (mat* (mat* (st-im s) (mat-translate (st-ox s) (st-oy s)))
                             (mat-scale (st-sx s) (st-sy s)))
                       (mat-rotate (st-rot s)))))

;; Interprets a recorded datum into display-list items.
(define (datum->items ops)
  (define s (fresh-state))
  (define alpha-stack '())
  (define items '())
  (define (emit! i) (set! items (cons i items)))
  (define (fill) (and (st-brush s) (brush-of (st-brush s) (st-alpha s) (st-ctm s))))
  (define (pen) (and (st-pen s) (pen-of (st-pen s) (st-alpha s) (st-ctm s))))
  ;; Emits a box-shaped item, falling back to a path when the matrix shears.
  (define (emit-box! kind x y w h radius segs-thunk)
    (define placed (place-box (st-ctm s) x y w h))
    (cond
      [placed
       (define-values (px py pw ph prot) (apply values placed))
       (case kind
         [(rect) (emit! (it:rect px py pw ph prot
                                 (* radius (mat-scale-factor (st-ctm s))) (fill) (pen)))]
         [(ellipse) (emit! (it:ellipse px py pw ph prot (fill) (pen)))])]
      [else (emit! (it:path (transform-segs (st-ctm s) (segs-thunk)) (fill) (pen)))]))
  (for ([op (in-list ops)])
    (define args (cdr op))
    (case (car op)
      ;; ------------------------------------------------------------ state
      [(do-set-pen!) (set-st-pen! s (first args))]
      [(do-set-brush!) (set-st-brush! s (first args))]
      [(set-font) (set-st-font! s (first args))]
      [(set-alpha) (set-st-alpha! s (first args))]
      ;; A recording dc does not replay `cellophane` as a `set-alpha`: it brackets
      ;; the faded drawing in `start-alpha`/`end-alpha`, and the value there is a
      ;; factor on whatever alpha is already in force, so they nest.
      [(start-alpha)
       (set! alpha-stack (cons (st-alpha s) alpha-stack))
       (set-st-alpha! s (* (st-alpha s) (first args)))]
      [(end-alpha)
       (unless (null? alpha-stack)
         (set-st-alpha! s (car alpha-stack))
         (set! alpha-stack (cdr alpha-stack)))]
      [(set-text-foreground) (set-st-text-fg! s (first args))]
      ;; racket/draw folds a transform into the initial matrix and resets the
      ;; origin, scale and rotation -- so a later `set-scale` multiplies on top
      ;; of it rather than replacing it. Treating `transform` as a plain
      ;; accumulation loses every transform that precedes a `set-scale`, which
      ;; is exactly what pict emits when a scaled pict contains a scaled one.
      [(transform)
       (set-st-im! s (mat* (st-ctm s) (first args)))
       (set-st-ox! s 0.0) (set-st-oy! s 0.0)
       (set-st-sx! s 1.0) (set-st-sy! s 1.0) (set-st-rot! s 0.0)
       (recompute! s)]
      [(set-initial-matrix)
       (set-st-im! s (first args))
       (set-st-ox! s 0.0) (set-st-oy! s 0.0)
       (set-st-sx! s 1.0) (set-st-sy! s 1.0) (set-st-rot! s 0.0)
       (recompute! s)]
      [(set-origin) (set-st-ox! s (first args)) (set-st-oy! s (second args)) (recompute! s)]
      [(set-scale) (set-st-sx! s (first args)) (set-st-sy! s (second args)) (recompute! s)]
      [(set-rotation) (set-st-rot! s (first args)) (recompute! s)]
      [(set-clipping-region)
       (define r (first args))
       (set-st-clip! s (and r #t))
       (when r (warn! "a clipping region is in effect; DrawingML cannot clip"))]
      [(set-smoothing set-background set-text-background set-text-mode
        set-clipping-rect erase clear set-brush set-pen)
       (void)]
      ;; ------------------------------------------------------------ shapes
      [(draw-rectangle)
       (define-values (x y w h) (apply values (take args 4)))
       (emit-box! 'rect x y w h 0.0 (lambda () (rect-segs x y w h)))]
      [(draw-rounded-rectangle)
       (define-values (x y w h r) (apply values (take args 5)))
       ;; A negative radius is a fraction of the smaller side.
       (define rr (if (negative? r) (* (- r) (min w h) -1.0) r))
       (emit-box! 'rect x y w h (abs rr) (lambda () (rect-segs x y w h)))]
      [(draw-ellipse)
       (define-values (x y w h) (apply values (take args 4)))
       (emit-box! 'ellipse x y w h 0.0 (lambda () (ellipse-segs x y w h)))]
      [(draw-line)
       (define-values (x1 y1 x2 y2) (apply values (take args 4)))
       (emit! (it:path (transform-segs (st-ctm s)
                                       (list (seg:move x1 y1) (seg:line x2 y2)))
                       #f (pen)))]
      [(draw-lines)
       (emit! (it:path (transform-segs (st-ctm s)
                                       (points->path-segs (first args)
                                                          (or (and (> (length args) 1) (second args)) 0)
                                                          (or (and (> (length args) 2) (third args)) 0)
                                                          #f))
                       #f (pen)))]
      [(draw-polygon)
       (emit! (it:path (transform-segs (st-ctm s)
                                       (points->path-segs (first args)
                                                          (or (and (> (length args) 1) (second args)) 0)
                                                          (or (and (> (length args) 2) (third args)) 0)
                                                          #t))
                       (fill) (pen)))]
      [(draw-path)
       (emit! (it:path (transform-segs (st-ctm s)
                                       (path-datum->segs (first args) (second args) (third args)))
                       (fill) (pen)))]
      [(draw-arc)
       (define-values (x y w h a0 a1) (apply values (take args 6)))
       (emit! (it:path (transform-segs (st-ctm s) (arc-segs x y w h a0 a1)) (fill) (pen)))]
      [(draw-spline)
       (define-values (x1 y1 x2 y2 x3 y3) (apply values (take args 6)))
       (emit! (it:path (transform-segs (st-ctm s)
                                       (list (seg:move x1 y1)
                                             (seg:curve x2 y2 x2 y2 x3 y3)))
                       #f (pen)))]
      [(draw-point)
       (define-values (x y) (apply values (take args 2)))
       (emit! (it:path (transform-segs (st-ctm s) (list (seg:move x y) (seg:line x y)))
                       #f (pen)))]
      ;; -------------------------------------------------------------- text
      [(draw-text)
       (define str (first args))
       (define x (second args)) (define y (third args))
       (define combine? (and (> (length args) 3) (fourth args)))
       (define angle (if (> (length args) 5) (sixth args) 0.0))
       (unless (string=? "" str)
         (define spec (st-font s))
         (define-values (tw th ascent) (measure-text str spec combine?))
         (define m (if (zero? angle)
                       (st-ctm s)
                       (mat* (mat* (mat* (st-ctm s) (mat-translate x y))
                                   (mat-rotate (- angle)))
                             (mat-translate (- x) (- y)))))
         (define placed (place-box m x y tw th))
         (cond
           [placed
            (define-values (px py pw ph prot) (apply values placed))
            (define scale (mat-scale-factor m))
            (emit! (it:text px py pw ph prot str
                            (spec-face spec)
                            (* scale (font-device-size spec))
                            (memq (fifth spec) '(bold semibold))
                            (eq? (fourth spec) 'italic)
                            (and (sixth spec) #t)
                            (* scale ascent)
                            (color-of (st-text-fg s) (st-alpha s))))]
           [else (warn! "sheared text ~s cannot be expressed as a text box" str)]))]
      ;; ------------------------------------------------------------ images
      ;; draw-bitmap is (bm dest-x dest-y ...), while draw-bitmap-section is
      ;; (bm dest-x dest-y src-x src-y src-w src-h ...) -- its fourth and fifth
      ;; arguments are the source origin, not a destination size. A section is
      ;; drawn at its own size and any scaling comes from the matrix.
      [(draw-bitmap draw-bitmap-section)
       (define bm (first args))
       (define bm-w (first bm)) (define bm-h (second bm))
       (define all-bytes (last bm))
       (define section? (eq? 'draw-bitmap-section (car op)))
       (define x (second args)) (define y (third args))
       (define sx (if section? (exact-round* (fourth args)) 0))
       (define sy (if section? (exact-round* (fifth args)) 0))
       (define sw (if section? (exact-round* (sixth args)) bm-w))
       (define sh (if section? (exact-round* (seventh args)) bm-h))
       (define placed (place-box (st-ctm s) x y sw sh))
       (cond
         [placed
          (define-values (px py pw ph prot) (apply values placed))
          (define-values (bytes cw ch) (crop-argb all-bytes bm-w bm-h sx sy sw sh))
          (emit! (it:image px py pw ph prot bytes cw ch #f))]
         [else (warn! "a sheared bitmap cannot be placed")])]
      [else (warn! "ignored drawing operation ~a" (car op))]))
  (reverse items))

;; Draws `p` through a recording dc and returns its display list.
(define (pict->display-page draw-proc width height)
  (define dc (new record-dc% [width width] [height height]))
  (send dc set-smoothing 'smoothed)
  (draw-proc dc)
  (display-page width height #f (datum->items (send dc get-recorded-datum)) #f))
