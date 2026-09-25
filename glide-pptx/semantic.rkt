#lang racket/base
;; Structure-aware export: turn the descriptors a pict carries into items that
;; keep their meaning, and fall back to the flattened display list for anything
;; that carries none.
;;
;; The walk is over the runtime's own composition -- `slide-canvas` collecting
;; `placed` elements -- rather than over the pict tree, which makes it exact
;; rather than a reconstruction. Elements are emitted in paint order, and an
;; element with no descriptor is rendered on its own and spliced in at the right
;; point, so z-order is preserved either way.
(require "units.rkt" racket/list racket/math racket/class racket/draw pict
         "ir.rkt" "tagged.rkt" "draw-ir.rkt"
         (only-in "record-adapt.rkt" pict->display-page current-adapt-warnings)
         (only-in "runtime.rkt" placed placed? placed-x placed-y placed-rot
                  placed-pict placed-tag placed-position pin-placed
                  body-natural-size))
(provide pict->page semantic-page? canvas-tags addressable-count current-flatten-opaque?)

;; A pict is exported semantically when it says how it was built.
(define (semantic-page? p) (slide-desc? (pict-desc p)))

(define (warn! fmt . args)
  (define b (current-adapt-warnings))
  (when b (set-box! b (cons (apply format fmt args) (unbox b)))))

(define (pict->page p width height)
  (define d (pict-desc p))
  (define inner (and (not (slide-desc? d)) (pict? p) (pieces-within p)))
  (cond
    [(slide-desc? d)
     (display-page width height
                   (ir-fill->fill (slide-desc-background d))
                   (slide-items d p width height)
                   (slide-desc-hidden? d))]
    ;; A slide built from canvases rather than being one: a talk that gives a
    ;; slide stages holds a base canvas and one per reveal, and what reaches
    ;; here is the picture they compose into. The canvases are still inside it
    ;; -- an element's tag rides on the pict as a field, so every combinator on
    ;; the way has kept it -- so the page is what they draw between them, in the
    ;; order they draw it, and every element on it keeps the tag its `at` gave
    ;; it. Without this the drawing is read back as anonymous shapes, which is
    ;; a slide nobody can edit.
    [(pair? inner)
     (display-page width height
                   ;; The background is the first canvas's; a stage that is a
                   ;; group has none of its own.
                   (let ([canvas (for/first ([e (in-list inner)]
                                             #:when (and (eq? 'layer (first e))
                                                         (slide-desc? (second e))))
                                   (second e))])
                     (and canvas (ir-fill->fill (slide-desc-background canvas))))
                   (composed-items inner width height)
                   #f)]
    [else (pict->display-page (lambda (dc) (draw-pict p dc 0 0)) width height)]))

;; The tags a pict would give a page, whether it is a canvas or is built from
;; some. Used to choose between the frames an animated slide settles on: the
;; one that shows the most of the slide is the one worth reading.
(define (canvas-tags p)
  (define d (pict-desc p))
  (define layers
    (cond
      [(slide-desc? d) (list d)]
      [(pict? p) (for/list ([e (in-list (pieces-within p))] #:when (eq? 'layer (first e)))
                   (second e))]
      [else '()]))
  (remove-duplicates
   (filter values
           (append* (for/list ([ld (in-list layers)])
                      (for/list ([pl (in-list (if (slide-desc? ld)
                                                  (slide-desc-placeds ld)
                                                  (group-desc-placeds ld)))])
                        (placed-tag pl)))))))

;; How much of a slide a frame shows: the elements its canvases place and the
;; picts it names on its own, which are the things an edit can be written back
;; to. What `settle` picks a frame by.
(define (addressable-count p)
  (+ (length (canvas-tags p))
     (if (pict? p)
         (for/sum ([e (in-list (pieces-within p))] #:when (eq? 'named (first e))) 1)
         0)))

;; How a slide that is not a canvas is taken apart, in the order it is drawn.
;;
;; A talk composes a slide out of pieces: a base canvas, a group per stage it
;; reveals, and whatever else it draws around them -- a section header, an
;; outline, a divider built from bare picts. The pieces that are canvases or
;; groups say what they hold, and their elements keep the tags their `at` forms
;; gave them, which is what makes them editable. The rest says nothing, and is
;; kept as what it draws.
;;
;; Keeping the rest is the point. Composing only the layers dropped everything
;; else on the floor: four divider slides came out blank, and the section header
;; would have gone missing from every slide that has one.
;;
;; `pict-children` lists the last-drawn child first, so consing as the walk goes
;; gives the pieces back in the order they are drawn.
;; The children of a pict, last-drawn first -- the order `pieces-within` needs,
;; and not the order `pict-children` gives: a `picture` (superimpose, place, pin)
;; lists its children last-drawn first, an append lists them first-drawn first.
;; Only the drawing settles it, so the order is read off `pict-draw`, whose
;; commands each carry the child's own drawing and so can be matched by `eq?`.
;; A drawing we cannot account for child-for-child is left in the order it came,
;; which is the order this always assumed.
;; A pict that paints nothing: `ghost` -- which is also what `cellophane` and so
;; Rhombus's `alpha` give at zero -- keeps its subject as a child but drops it
;; from the drawing, leaving a `picture` with no commands. Its canvases are still
;; found by a walk over children, and reading them puts a stage that is not on
;; screen into the page, in front of the one that is.
(define (draws-nothing? p)
  (define draw (pict-draw p))
  (and (list? draw) (pair? draw) (eq? 'picture (car draw)) (null? (cdddr draw))))

(define (children-last-drawn-first p)
  (define kids (pict-children p))
  (define draw (pict-draw p))
  (cond
    [(or (null? kids) (null? (cdr kids))
         (not (and (list? draw) (pair? draw) (eq? 'picture (car draw)))))
     kids]
    [else
     (define waiting (make-hasheq))
     (for ([c (in-list kids)])
       (hash-update! waiting (pict-draw (child-pict c))
                     (lambda (l) (append l (list c))) '()))
     (define ordered
       (for*/list ([cmd (in-list (cdddr draw))]
                   [part (in-list (if (list? cmd) cmd '()))]
                   #:when (pair? (hash-ref waiting part '())))
         (define l (hash-ref waiting part))
         (hash-set! waiting part (cdr l))
         (car l)))
     (if (= (length ordered) (length kids)) (reverse ordered) kids)]))

(define (pieces-within p)
  (define (has-layer? p)
    (and (pict? p)
         (let ([d (pict-desc p)])
           (or (slide-desc? d) (group-desc? d) (name-desc? d)
               (for/or ([c (in-list (pict-children p))]) (has-layer? (child-pict c)))))))
  (let walk ([p p] [t (xf 0.0 0.0 1.0 1.0)] [acc '()])
    (define d (and (pict? p) (pict-desc p)))
    (cond
      [(not (pict? p)) acc]
      [(or (slide-desc? d) (group-desc? d)) (cons (list 'layer d t) acc)]
      ;; A pict with a name of its own: read as one piece, and what it draws
      ;; answers to that name. Left inside a larger drawing it would be read
      ;; back with everything else around it and answer to nothing.
      [(name-desc? d) (cons (list 'named p t) acc)]
      [(draws-nothing? p) acc]
      ;; Nothing addressable in here, so what it draws is what it is.
      [(not (has-layer? p)) (cons (list 'drawn p t) acc)]
      [else
       (for/fold ([acc acc]) ([c (in-list (children-last-drawn-first p))])
         ;; `child-dy` is measured from the *bottom* of the parent, and a page
         ;; is measured from the top. Reading it as a top-down offset puts a
         ;; header at the foot of the slide and sends an arrow on a detour --
         ;; and it does so only for the pieces around a canvas, since a canvas
         ;; places its own elements itself.
         (define kid (child-pict c))
         (define sy (* (xf-sy t) (child-sy c)))
         (define top (- (pict-height p) (child-dy c)
                        (* (child-sy c) (pict-height kid))))
         (walk kid
               (xf (+ (xf-ox t) (* (xf-sx t) (child-dx c)))
                   (+ (xf-oy t) (* (xf-sy t) top))
                   (* (xf-sx t) (child-sx c))
                   sy)
               acc))])))

;; What one piece draws, in the page's own coordinates. A canvas draws its
;; elements; a stage laid over the slide is a group, and its children are the
;; elements -- the group itself is not one, since nothing placed it and it has
;; no tag of its own to be edited by. Anything else is read back from its
;; drawing, the way an arbitrary pict has always been.
(define (piece-items entry width height)
  (define kind (first entry))
  (define t (third entry))
  (cond
    [(eq? 'layer kind)
     (define d (second entry))
     (if (slide-desc? d)
         (append* (for/list ([pl (in-list (slide-desc-placeds d))])
                    (placed-items pl t width height)))
         (group-items d (xf-ox t) (xf-oy t) t width height))]
    [(eq? 'named kind)
     (define p (second entry))
     (define name (name-desc-name (pict-desc p)))
     ;; What it holds, read the way anything else is -- a shape it describes
     ;; stays a shape, and a picture built from nothing but drawing is read back
     ;; from that drawing -- and then given the name.
     (define inner
       (let ([kid (let ([cs (pict-children p)]) (and (pair? cs) (child-pict (car cs))))])
         (cond
           [(and kid (shape-desc? (pict-desc kid)))
            (shape-items (pict-desc kid) (xf-x t 0.0) (xf-y t 0.0) 0.0 t (xf-factor t) name)]
           [else
            (define q (if (and (= 1.0 (xf-sx t)) (= 1.0 (xf-sy t)))
                          p
                          (scale p (xf-sx t) (xf-sy t))))
            (display-page-items
             (pict->display-page (lambda (dc) (draw-pict q dc (xf-ox t) (xf-oy t)))
                                 width height))])))
     (for/list ([i (in-list inner)]) (item-with-tag i name))]
    [else
     (define p (second entry))
     (define q (if (and (= 1.0 (xf-sx t)) (= 1.0 (xf-sy t)))
                   p
                   (scale p (xf-sx t) (xf-sy t))))
     (display-page-items
      (pict->display-page (lambda (dc) (draw-pict q dc (xf-ox t) (xf-oy t)))
                          width height))]))


(define (composed-items pieces width height)
  (define items
    (append* (for/list ([entry (in-list pieces)]) (piece-items entry width height))))
  (define seen (make-hash))
  (for ([i (in-list (reverse items))])
    (define tag (item-tag i))
    (when (and tag (not (hash-ref seen tag #f))) (hash-set! seen tag i)))
  (for/list ([i (in-list items)]
             #:when (let ([tag (item-tag i)])
                      (or (not tag) (eq? i (hash-ref seen tag #f)))))
    i))

;; ------------------------------------------------------------------ colors

(define (ir-color->rgba c)
  (if (rgba? c) (rgba* (rgba-r c) (rgba-g c) (rgba-b c) (rgba-a c)) (rgba* 0 0 0 1.0)))

(define (ir-fill->fill f)
  (cond
    ;; A bare color is a solid fill. The drawing path has always taken one --
    ;; `slide_canvas(~background: hex("FFFFFF"))` is what generated code
    ;; writes -- so refusing it here silently dropped every generated deck's
    ;; background on export, and a consumer that does not default to white then
    ;; painted the slide black.
    [(rgba? f) (fill:solid (ir-color->rgba f))]
    [(solid-fill? f) (fill:solid (ir-color->rgba (solid-fill-color f)))]
    [(gradient-fill? f)
     ;; Endpoints are resolved by the writer from the angle, so a unit span is
     ;; enough here; the angle is what carries the direction.
     (define a (degrees->radians (gradient-fill-angle f)))
     (fill:linear 0.0 0.0 (cos a) (sin a)
                  (for/list ([s (in-list (gradient-fill-stops f))])
                    (list (exact->inexact (car s)) (ir-color->rgba (cdr s)))))]
    ;; A picture as a fill keeps its file, so the export can write the bytes
    ;; back out rather than dropping the picture on the floor.
    [(image-fill? f) (fill:image (image-fill-src f) (image-fill-opacity f))]
    [(pattern-fill? f) (fill:solid (ir-color->rgba (pattern-fill-fg f)))]
    [else #f]))

(define (ir-line->pen l scale)
  (and (stroke? l)
       (pen* (ir-color->rgba (stroke-color l))
             (* scale (stroke-width l))
             (stroke-dash l)
             (case (stroke-cap l) [(round) 'round] [(projecting) 'square] [else 'flat])
             'miter
             (stroke-head l) (stroke-tail l) (stroke-dash-pattern l))))

;; A group with a scale changes text size along with everything else, matching
;; what the renderer draws.
(define (scale-body body factor)
  (cond
    [(or (not body) (= 1.0 factor)) body]
    [else
     (struct-copy
      text-body body
      [paras (for/list ([p (in-list (text-body-paras body))])
               (struct-copy para p
                            [runs (for/list ([r (in-list (para-runs p))])
                                    (struct-copy trun r [size (* factor (trun-size r))]))]
                            [margin-left (* factor (para-margin-left p))]
                            [indent (* factor (para-indent p))]
                            [space-before (* factor (para-space-before p))]
                            [space-after (* factor (para-space-after p))]))]
      [insets (let ([i (text-body-insets body)])
                (insets (* factor (insets-l i)) (* factor (insets-t i))
                        (* factor (insets-r i)) (* factor (insets-b i))))])]))

;; ------------------------------------------------------------------- walking

;; A transform from an element's own coordinate space to the slide's:
;; absolute = offset + scale * local.
(struct xf (ox oy sx sy) #:transparent)
(define (xf-x t v) (+ (xf-ox t) (* (xf-sx t) v)))
(define (xf-y t v) (+ (xf-oy t) (* (xf-sy t) v)))
(define (xf-factor t) (sqrt (max 0.0 (* (abs (xf-sx t)) (abs (xf-sy t))))))

(define (slide-items d whole width height)
  (append* (for/list ([pl (in-list (slide-desc-placeds d))])
             (placed-items pl (xf 0.0 0.0 1.0 1.0) width height))))

(define (placed-items pl t page-w page-h)
  ;; A pict named where it was built rather than where it was placed: the name
  ;; is the placement's if it has one, and otherwise the pict's own, and what is
  ;; inside the wrapper is read exactly as it would have been without it.
  (define p0 (placed-pict pl))
  (define named
    (and (name-desc? (pict-desc p0))
         (let ([cs (pict-children p0)]) (and (pair? cs) (child-pict (car cs))))))
  (define p (or named p0))
  (define d (pict-desc p))
  (define-values (local-x local-y) (placed-position pl))
  (define x (xf-x t local-x))
  (define y (xf-y t local-y))
  (define rot (placed-rot pl))
  (define f (xf-factor t))
  (define tag (or (placed-tag pl)
                  (and (name-desc? (pict-desc p0)) (name-desc-name (pict-desc p0)))))
  (cond
    [(shape-desc? d) (shape-items d x y rot t f tag)]
    [(text-desc? d)
     ;; The box the text needs, not only the box it was given. A box that does
     ;; not wrap is as wide as its longest line and one set to grow is as tall
     ;; as its lines come to -- and a renderer that ignores `wrap="none"` wraps
     ;; at whatever width it was handed, which is how "Floating point" came out
     ;; as "Floatin g poin t" in one editor. Never smaller than the box: a box
     ;; someone drew larger than its text stays that size.
     (define body (text-desc-body d))
     (define-values (nat-w nat-h)
       (body-natural-size body (text-desc-width d) (text-desc-height d)))
     ;; A little more than the text needs. Two renderers do not measure a
     ;; string to the same width -- across four strings in one deck they
     ;; differed by up to 1.3% -- so a box sized exactly to our own measurement
     ;; is one the other may still wrap. "CAD" at 48pt needs 98 of its 100
     ;; points here and rather more there. A box that does not wrap has nothing
     ;; to lose by being wider, and its text is anchored, not stretched.
     (define w (if (text-body-wrap? body)
                   (text-desc-width d)
                   (max (text-desc-width d) (+ nat-w 4.0))))
     (define h (if (eq? 'grow (text-body-autofit body))
                   (max (text-desc-height d) nat-h)
                   (text-desc-height d)))
     (list (it:textbox x y (* (xf-sx t) w) (* (xf-sy t) h)
                       rot (scale-body body f) tag))]
    [(image-desc? d)
     (list (it:picture x y (* (xf-sx t) (image-desc-width d))
                       (* (xf-sy t) (image-desc-height d)) rot
                       (image-desc-src d) (image-desc-crop d)
                       (image-desc-flip-h? d) (image-desc-flip-v? d)
                       (ir-line->pen (image-desc-line d) f)
                       (image-desc-opacity d) tag))]
    [(group-desc? d) (list (group-item d x y rot t tag page-w page-h))]
    [(table-desc? d)
     (list (it:table x y (* (xf-sx t) (table-desc-width d)) (* (xf-sy t) (table-desc-height d))
                     rot
                     (for/list ([w (in-list (table-desc-col-widths d))]) (* (xf-sx t) w))
                     (for/list ([h (in-list (table-desc-row-heights d))]) (* (xf-sy t) h))
                     (for/list ([row (in-list (table-desc-cells d))])
                       (for/list ([c (in-list row)])
                         (it:cell (and (tbl-cell-body c) (scale-body (tbl-cell-body c) f))
                                  (ir-fill->fill (tbl-cell-fill c))
                                  (ir-line->pen (tbl-cell-line c) f)
                                  (tbl-cell-row-span c) (tbl-cell-col-span c)
                                  (tbl-cell-merged? c))))
                     tag))]
    [else (opaque-items pl t x y rot tag page-w page-h)]))

;; How much of an element's structure the editor is allowed to see.
;;
;; A pict with no descriptor -- `(vc-append 5 a b c)` inside an `at`, say -- has
;; no structure we can sync. Flattening its *drawing* gives a pile of separate
;; shapes, every one of which can be dragged in Keynote and none of which can be
;; moved back, because the only thing the source names is the enclosing `at`.
;; Emitting one picture instead makes the file's affordances match what the tool
;; can honor: one object per `at`, and dragging it lands on numbers that exist.
;;
;; The cost is that its text becomes pixels. That is why this is off for a
;; one-way export, where nothing will be synced and separate shapes are strictly
;; better.
(define current-flatten-opaque? (make-parameter #t))

;; Rendered at twice its final size, so it still looks sharp on a projector.
(define FLATTEN-OVERSAMPLE 2.0)

(define (opaque-items pl t x y rot tag page-w page-h)
  (cond
    [(not (current-flatten-opaque?)) (raw-items pl page-w page-h)]
    [else
     (define p (placed-pict pl))
     (define w (* (abs (xf-sx t)) (pict-width p)))
     (define h (* (abs (xf-sy t)) (pict-height p)))
     (cond
       [(or (< w 0.01) (< h 0.01)) '()]
       [else
        (define-values (argb iw ih)
          (pict->argb p (* FLATTEN-OVERSAMPLE (max 1.0 (xf-factor t)))))
        (cond
          ;; A pict that draws nothing is nothing to export. A talk pins its
          ;; arrows between `blank` markers -- `at(x, y, ~tag: "data flow from",
          ;; blank(1.0, 1.0))` -- and a picture of a blank is an empty image
          ;; part, and a note about it on every export, for something nobody can
          ;; see. Both sides of the sync read the page the same way, so leaving
          ;; it out leaves it out of both.
          [(no-ink? argb) '()]
          [else
           ;; Saying why, because the why is the fix: the child of an `at` has to
           ;; be a bare `shape_pict`, `textbox`, `image_pict`, `group_pict` or
           ;; `table_pict`. Anything wrapped around one -- a `pad`, a `scale`, a
           ;; `colorize`, an `overlay` -- is a different pict, and the descriptor
           ;; that says what it is does not come with it.
           (warn! (string-append
                   "~a is not a bare shape, text box or picture -- something is"
                   " wrapped around it -- so it is exported as one picture, and"
                   " cannot be edited in the editor")
                  (or tag "an unnamed element"))
           (list (it:image x y w h rot argb iw ih tag))])])]))

;; Whether every pixel is fully transparent.
(define (no-ink? argb)
  (for/and ([i (in-range 0 (bytes-length argb) 4)])
    (zero? (bytes-ref argb i))))

;; Draws `p` on its own and returns its pixels.
(define (pict->argb p oversample)
  (define iw (max 1 (exact-ceiling (* oversample (pict-width p)))))
  (define ih (max 1 (exact-ceiling (* oversample (pict-height p)))))
  (define bm (make-bitmap iw ih))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-smoothing (quote smoothed))
  (draw-pict (scale p (/ iw (pict-width p)) (/ ih (pict-height p))) dc 0 0)
  (define bs (make-bytes (* 4 iw ih)))
  (send bm get-argb-pixels 0 0 iw ih bs)
  (values bs iw ih))

(define (shape-items d x y rot t f tag)
  (define w (* (xf-sx t) (shape-desc-width d)))
  (define h (* (xf-sy t) (shape-desc-height d)))
  (define geom (shape-desc-geom d))
  (define fill (ir-fill->fill (shape-desc-fill d)))
  (define pen (ir-line->pen (shape-desc-line d) f))
  (define body (scale-body (shape-desc-body d) f))
  (cond
    [(preset-geom? geom)
     (list (it:preset x y w h rot (preset-geom-name geom) (preset-geom-adjust geom)
                      (shape-desc-flip-h? d) (shape-desc-flip-v? d)
                      fill pen body tag))]
    ;; Custom geometry has no preset to name, so it stays a drawn path; the text
    ;; still travels with it.
    [else
     (define segs (custom-geom->segs geom x y w h))
     (list (it:shape-path segs fill pen (list x y w h) rot
                          (shape-desc-flip-h? d) (shape-desc-flip-v? d)
                          body tag))]))

;; Custom geometry arrives in its own coordinate space; scale it onto the box.
(define (custom-geom->segs g x y w h)
  (define gw (custom-geom-w g))
  (define gh (custom-geom-h g))
  ;; A path space of 0 -- which is what an omitted `w` or `h` means -- says the
  ;; coordinates are EMU within the shape, not a space to stretch onto the box.
  ;; Clamping the divisor to 1 instead multiplied them by the box's size, which
  ;; is a 21600-fold blow-up on the decks that write paths this way.
  (define (X v) (if (zero? gw) (+ x (emu->pt v)) (+ x (* w (/ (exact->inexact v) gw)))))
  (define (Y v) (if (zero? gh) (+ y (emu->pt v)) (+ y (* h (/ (exact->inexact v) gh)))))
  (append*
   (for/list ([path (in-list (custom-geom-paths g))])
     (for/list ([cmd (in-list path)])
       (case (car cmd)
         [(move) (seg:move (X (car (second cmd))) (Y (cdr (second cmd))))]
         [(line) (seg:line (X (car (second cmd))) (Y (cdr (second cmd))))]
         [(curve) (let ([ps (rest cmd)])
                    (if (= 3 (length ps))
                        (seg:curve (X (car (first ps))) (Y (cdr (first ps)))
                                   (X (car (second ps))) (Y (cdr (second ps)))
                                   (X (car (third ps))) (Y (cdr (third ps))))
                        (seg:close)))]
         [(quad) (let ([ps (rest cmd)])
                   (if (= 2 (length ps))
                       (seg:curve (X (car (first ps))) (Y (cdr (first ps)))
                                  (X (car (first ps))) (Y (cdr (first ps)))
                                  (X (car (second ps))) (Y (cdr (second ps))))
                       (seg:close)))]
         [else (seg:close)])))))

;; A group stays a group: its children are laid out in the slide's coordinates,
;; as they already were, and wrapped in one item.
(define (group-item d gx gy rot t tag page-w page-h)
  (define w (* (abs (xf-sx t)) (group-desc-width d)))
  (define h (* (abs (xf-sy t)) (group-desc-height d)))
  (it:group gx gy w h rot
            (group-desc-flip-h? d) (group-desc-flip-v? d)
            (group-items d gx gy t page-w page-h) tag))

(define (group-items d gx gy t page-w page-h)
  (define cw (max 1e-9 (group-desc-child-width d)))
  (define ch (max 1e-9 (group-desc-child-height d)))
  (define inner-sx (/ (group-desc-width d) cw))
  (define inner-sy (/ (group-desc-height d) ch))
  (define sx (* (xf-sx t) inner-sx))
  (define sy (* (xf-sy t) inner-sy))
  (define inner
    (xf (- gx (* sx (group-desc-child-x d)))
        (- gy (* sy (group-desc-child-y d)))
        sx sy))
  (append* (for/list ([pl (in-list (group-desc-placeds d))])
             (placed-items pl inner page-w page-h))))

;; Renders one element by itself, through the runtime's own placement, so the
;; ink lands exactly where the slide put it.
(define (raw-items pl page-w page-h)
  (define composed (pin-placed (blank page-w page-h) pl))
  (display-page-items
   (pict->display-page (lambda (dc) (draw-pict composed dc 0 0)) page-w page-h)))
