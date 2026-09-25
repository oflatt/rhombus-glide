#lang racket/base
;; Every way one element can differ from another, named.
;;
;; Shared by the round-trip tests, which is the point: the fuzzer generated a
;; line spacing of 1.5, the writer clamped it to 1.0, and both oracles compared
;; the element's box and its text and said nothing, because neither looked at
;; the properties. Every property the merge knows about is compared here, so a
;; value that does not survive being written out fails where it happens rather
;; than turning up in someone's talk.
(require racket/list racket/format racket/string racket/path racket/file
         glide-pptx/ir glide-pptx/parse glide-pptx/sync glide-pptx/sync-state)
(provide element-diffs ir-diffs slide-diffs disagreements deck-states-by-name)

;; The deck as the file describes it, the names it gives its shapes included.
;;
;; `deck-slide-states` asks the stricter question a merge has to ask -- is this
;; element one the program placed? -- and only our own alt text answers it. So a
;; box the editor drew, or a slide pasted out of somebody else's deck, comes
;; back from that with no tag at all, which is right for a merge and wrong for a
;; comparison: here the name is the identity, and "the same elements, in order"
;; is a statement about a file.
(define (deck-states-by-name deck [workdir #f])
  (deck->slide-states
   (pptx->deck deck #:workdir (or workdir (make-temporary-file "cmp~a" 'directory)))))

;; A slide's own properties, which belong to no element on it.
(define (slide-diffs a b)
  (filter values
          (list (and (not (eq? (and (slide-hidden? a) #t) (and (slide-hidden? b) #t)))
                     (format "hidden ~s -> ~s" (slide-hidden? a) (slide-hidden? b))))))

;; ------------------------------------------------------- field for field

;; The element itself, rather than the summary a merge works from: its
;; geometry, the detail of its fill, the pattern its dashes spell out, what a
;; group holds, what a table is shaped like. None of that reaches `el-state`,
;; so none of it was compared by a round trip -- 53 preset geometries and
;; every gradient stop went unchecked.
;;
;; What is allowed to differ: our own element ids, and the name a file gives a
;; picture inside its package.
(define (ir-facts e)
  (append
   (list (cons 'name (element-name e))
         (cons 'box (box-facts (element-bbox e))))
   (cond
     [(shape? e)
      (list (cons 'geom (geom-facts (shape-geom e) (element-bbox e)))
            (cons 'fill (fill-facts (shape-fill e)))
            (cons 'line (line-facts (shape-line e)))
            (cons 'body (body-facts (shape-body e))))]
     [(picture? e)
      (list (cons 'crop (and (picture-crop e) (map round3 (picture-crop e))))
            (cons 'opacity (round3 (picture-opacity e)))
            (cons 'fill (fill-facts (picture-fill e)))
            (cons 'line (line-facts (picture-line e))))]
     ;; A group maps what it holds onto its own box, and a round trip resolves
     ;; that mapping into the children: their boxes, line widths, font sizes
     ;; and insets all come back scaled, correctly. What must not change is
     ;; what it holds and what each of those is.
     [(group? e)
      (list (cons 'child-box (box-facts (group-child-bbox e)))
            (cons 'children
                  (for/list ([c (in-list (group-children e))])
                    (list (element-name c)
                          ;; A child's box is scaled by the group, so where a
                          ;; point sits across it is scaled too; the path's
                          ;; shape is what has to survive.
                          (cond [(shape? c) (list 'shape
                                                  (geom-facts (shape-geom c) (element-bbox c)
                                                              #:placed? #f)
                                                  (fill-facts (shape-fill c)))]
                                [(picture? c) 'picture]
                                [(group? c) (list 'group (length (group-children c)))]
                                [(tbl? c) 'table]
                                [else 'other])))))]
     [(tbl? e)
      (list (cons 'columns (map round3 (tbl-col-widths e)))
            (cons 'rows (map round3 (tbl-row-heights e)))
            (cons 'cells (for/list ([row (in-list (tbl-cells e))])
                           (for/list ([c (in-list row)])
                             (list (body-facts (tbl-cell-body c))
                                   (fill-facts (tbl-cell-fill c))
                                   (line-facts (tbl-cell-line c)))))))]
     [else '()])))

;; Rounded to what the format keeps: EMU is a 12700th of a point, so a position
;; is exact to far more than three places, while a font size, a letter spacing
;; and the space around a paragraph are hundredths of a point and nothing
;; finer. Comparing more precisely than the file can hold is comparing noise.
;; `-0.0` and `0.0` are not `equal?`, and rounding a small negative gives the
;; first while the value that went out gives the second.
(define (unzero v) (if (and (real? v) (zero? v)) 0.0 v))
(define (round3 v) (if (real? v) (unzero (/ (round (* 1000.0 v)) 1000.0)) v))
(define (round2 v) (if (real? v) (unzero (/ (round (* 100.0 v)) 100.0)) v))

(define (box-facts b)
  ;; A rotation is kept in 60000ths of a degree, and a hundredth of one is
  ;; already finer than anything anyone can see.
  ;; Hundredths of a point: a round trip goes through the renderer's own
  ;; arithmetic, which lands within a thousandth, and a hundredth of a point is
  ;; a seventh of a screen pixel.
  (and b (list (round2 (bbox-x b)) (round2 (bbox-y b))
               (round2 (bbox-w b)) (round2 (bbox-h b))
               (round2 (bbox-rot b)) (bbox-flip-h? b) (bbox-flip-v? b))))

;; `box` is the shape the path is drawn inside, which is the space a path
;; declaring none is written in.
(define (geom-facts g box #:placed? [want-placed? #t])
  (cond
    [(preset-geom? g) (list 'preset (preset-geom-name g) (preset-geom-adjust g))]
    ;; Where each point sits across the shape, which is the one thing about a
    ;; path that has to survive. The numbers themselves cannot be compared: a
    ;; path is written in a coordinate space of its own, and the writer picks
    ;; the shape's own box in EMU rather than whatever came in. A space
    ;; declared as 0 or 1 means the numbers are already EMU inside the shape,
    ;; which is what several real decks write and what the reader floors to 1.
    [(custom-geom? g)
     ;; Zero, and only zero, means the numbers are EMU inside the shape; any
     ;; other space is a space to stretch onto the box, even a small one.
     (define (across declared extent)
       (if (zero? declared) (* 12700.0 extent) (exact->inexact declared)))
     (define w (across (custom-geom-w g) (bbox-w box)))
     (define h (across (custom-geom-h g) (bbox-h box)))
     ;; Whether a point's place is worth comparing depends on the shape, not on
     ;; the space it happens to be written in -- the two sides use different
     ;; spaces, and a rule about the space fires on one side and not the other.
     ;; Under a point across, the space is a handful of EMU and where a point
     ;; sits in it says nothing; the path's shape still has to survive.
     (define placed? (and want-placed? (> (bbox-w box) 1.0) (> (bbox-h box) 1.0)))
     (define (place pt)
       (if (and placed? (pair? pt))
           (cons (round3 (/ (exact->inexact (car pt)) w))
                 (round3 (/ (exact->inexact (cdr pt)) h)))
           (if (pair? pt) 'point pt)))
     (list 'custom
           (for/list ([path (in-list (custom-geom-paths g))])
             (for/list ([cmd (in-list path)])
               (cons (car cmd) (map place (cdr cmd))))))]
    [else g]))

(define (colour-facts c)
  (and c (list (round3 (rgba-r c)) (round3 (rgba-g c)) (round3 (rgba-b c)) (round3 (rgba-a c)))))

(define (fill-facts f)
  (cond
    [(solid-fill? f) (list 'solid (colour-facts (solid-fill-color f)))]
    [(gradient-fill? f)
     ;; The angle goes out as a direction and comes back through `atan`, which
     ;; lands a thousandth of a degree away.
     (list 'gradient (round2 (gradient-fill-angle f))
           (for/list ([st (in-list (gradient-fill-stops f))])
             (cons (round3 (car st)) (colour-facts (cdr st)))))]
    [(pattern-fill? f) (list 'pattern (pattern-fill-name f)
                             (colour-facts (pattern-fill-fg f))
                             (colour-facts (pattern-fill-bg f)))]
    ;; A package names the file whatever it likes; what matters is that there
    ;; is one and how see-through it is.
    [(image-fill? f) (list 'image (round3 (image-fill-opacity f)))]
    [else f]))

(define (end-facts e)
  (and (line-end? e) (list (line-end-kind e) (line-end-width e) (line-end-length e))))

(define (line-facts l)
  (and (stroke? l)
       (list (colour-facts (stroke-color l))
             ;; Hundredths, as the merge compares them: a line's width goes
             ;; out in EMU and comes back a rounding away from where it began.
             (and (real? (stroke-width l)) (round2 (stroke-width l)))
             (stroke-dash l) (stroke-cap l)
             (end-facts (stroke-head l)) (end-facts (stroke-tail l))
             (and (stroke-dash-pattern l)
                  (for/list ([d (in-list (stroke-dash-pattern l))])
                    (cons (round3 (car d)) (round3 (cdr d))))))))

(define (body-facts b)
  (and (text-body? b)
       (list (text-body-anchor b) (text-body-anchor-center? b) (text-body-wrap? b)
             (text-body-autofit b) (round3 (text-body-rot b))
             (let ([i (text-body-insets b)])
               (map round3 (list (insets-l i) (insets-t i) (insets-r i) (insets-b i))))
             (for/list ([p (in-list (text-body-paras b))])
               (list (para-align p) (para-level p)
                     (round3 (para-margin-left p)) (round3 (para-indent p))
                     ;; A percentage to a hundredth: the value goes out through
                     ;; the renderer's arithmetic and lands a thousandth away.
                     (cons (car (para-line-spacing p)) (round2 (cdr (para-line-spacing p))))
                     (round2 (para-space-before p)) (round2 (para-space-after p))
                     (bullet-facts (para-bullet p))
                     (for/list ([r (in-list (para-runs p))])
                       (list (trun-text r) (trun-family r) (round2 (trun-size r))
                             (trun-bold? r) (trun-italic? r) (trun-underline? r)
                             (trun-strike? r) (colour-facts (trun-color r))
                             (round2 (trun-spacing r)) (trun-caps r)
                             (round3 (trun-baseline r)))))))))

(define (bullet-facts b)
  (and (bullet? b)
       (list (bullet-kind b) (bullet-char b) (bullet-font b)
             (and (bullet-size-frac b) (round3 (bullet-size-frac b)))
             (colour-facts (bullet-color b)))))

;; Named differences between two elements, field by field.
(define (ir-diffs a b)
  (for/list ([kv (in-list (ir-facts a))]
             #:unless (close-enough? (cdr kv) (cdr (assq (car kv) (ir-facts b)))))
    (format "~a ~s -> ~s" (car kv) (cdr kv) (cdr (assq (car kv) (ir-facts b))))))

;; A round trip here goes through the renderer, whose arithmetic lands a
;; fraction of a percent from where it started -- a gradient's angle recomputed
;; from a direction, a line spacing carried through a layout. What this is
;; watching for is a value that does not survive at all: a spacing clamped to
;; 1.0, a dash renamed, a stop dropped.
(define (close-enough? x y)
  (cond
    ;; A hundredth, or a thousandth of the value for the larger ones: a point
    ;; three thousand times outside its own path space still lands within a
    ;; tenth of a percent of where it started, and what this is watching for is
    ;; a value that does not survive at all.
    [(and (real? x) (real? y))
     (< (abs (- x y)) (max 0.02 (* 0.001 (max (abs x) (abs y)))))]
    [(and (pair? x) (pair? y) (not (list? x)) (not (list? y)))
     (and (close-enough? (car x) (car y)) (close-enough? (cdr x) (cdr y)))]
    [(and (list? x) (list? y)) (and (= (length x) (length y)) (andmap close-enough? x y))]
    [else (equal? x y)]))

(define (element-diffs a b)
  (append
   (filter
    values
    (list
     (and (not (eq? (el-state-kind a) (el-state-kind b)))
          (format "kind ~a -> ~a" (el-state-kind a) (el-state-kind b)))
     (and (not (el-geometry-same? a b))
          (format "geometry ~a -> ~a" (shown (el-geometry a)) (shown (el-geometry b))))
     (and (not (string=? (el-state-text a) (el-state-text b)))
          (format "text ~s -> ~s" (el-state-text a) (el-state-text b)))
     (and (not (string=? (el-state-paint a) (el-state-paint b)))
          (format "paint ~s -> ~s" (el-state-paint a) (el-state-paint b)))))
   (style-diffs a b)))

;; A property one side does not carry is not a difference: a deck states some
;; of its appearance and inherits the rest, and what we write out states more
;; than what came in. What matters is the two of them naming one property and
;; giving it different values.
;; A geometry is five numbers and two flips.
(define (shown g)
  (for/list ([v (in-list g)]) (if (real? v) (~r v #:precision 2) (format "~a" v))))

(define (style-diffs a b)
  (for/list ([kv (in-list (el-state-style a))]
             #:when (assoc (car kv) (el-state-style b))
             #:unless (same-value? (cdr kv) (cdr (assoc (car kv) (el-state-style b)))))
    (format "~a ~s -> ~s" (car kv) (cdr kv)
            (cdr (assoc (car kv) (el-state-style b))))))

;; Numbers are compared to the precision the writer keeps: EMU is a 12700th of
;; a point, and a percentage is a thousandth.
(define (same-value? x y)
  (cond
    [(and (real? x) (real? y)) (< (abs (- x y)) 0.01)]
    [(and (pair? x) (pair? y) (not (list? x)) (not (list? y)))
     (and (equal? (car x) (car y)) (same-value? (cdr x) (cdr y)))]
    [(and (list? x) (list? y) (= (length x) (length y)))
     (andmap same-value? x y)]
    [else (equal? x y)]))

;; ------------------------------- a program against the deck beside it

;; What the program renders to, against what the editor holds. Syncing again
;; and hearing nothing says much the same thing -- the base a merge writes is
;; the program as it then reads, so a refused change is reported again next
;; pass rather than forgotten. What this adds is *which* property differs, and
;; a view the merge does not have: an element the program carries under a tag
;; the deck does not know is a disagreement here and no action there.
;; A property is named either by a symbol or, for a run or paragraph after the
;; first, by a list of the two -- so these are looked up with `assoc`.
;;
;; A property one side does not carry at all is not a disagreement: a deck
;; states only what the shape says for itself, and our own writer leaves out
;; what is already the default. What matters is the two of them naming the same
;; property and giving it different values.
(define (style-disagreements tag a b)
  (for/list ([kv (in-list (el-state-style a))]
             #:when (assoc (car kv) (el-state-style b))
             #:unless (equal? (cdr kv) (cdr (assoc (car kv) (el-state-style b)))))
    (format "~s ~a: ~s vs ~s" tag (car kv) (cdr kv)
            (cdr (assoc (car kv) (el-state-style b))))))

(define (element-disagreements a b)
  (define tag (el-state-tag a))
  (append
   (if (el-geometry-same? a b)
       '()
       (list (format "~s box: ~s vs ~s" tag (el-geometry a) (el-geometry b))))
   (if (equal? (el-state-text a) (el-state-text b))
       '()
       (list (format "~s text: ~s vs ~s" tag (el-state-text a) (el-state-text b))))
   (style-disagreements tag a b)))

(define (slide-disagreements p d)
  (define by-tag
    (for/hash ([e (in-list (slide-state-elements d))] #:when (el-state-tag e))
      (values (el-state-tag e) e)))
  (append
   (if (equal? (slide-state-background p) (slide-state-background d))
       '()
       (list (format "slide ~a background: ~s vs ~s" (slide-state-index p)
                     (slide-state-background p) (slide-state-background d))))
   (append*
    (for/list ([e (in-list (slide-state-elements p))] #:when (el-state-tag e))
      (define o (hash-ref by-tag (el-state-tag e) #f))
      (if o
          (element-disagreements e o)
          (list (format "~s is in the program and not the deck" (el-state-tag e))))))))

(define (disagreements program deck)
  (define ps (program-slide-states program))
  (define ds (deck-states-by-name deck))
  (if (= (length ps) (length ds))
      (append* (for/list ([p (in-list ps)] [d (in-list ds)]) (slide-disagreements p d)))
      (list (format "~a slides in the program, ~a in the deck" (length ps) (length ds)))))

