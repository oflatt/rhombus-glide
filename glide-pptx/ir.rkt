#lang racket/base
;; The intermediate representation shared by the parser, the renderer and the
;; code emitters.
;;
;; Every length is in PostScript points (1/72 inch) and every position is
;; absolute within its slide, with the origin at the top-left corner. Angles
;; are degrees, clockwise, matching OOXML's sign convention.
;;
;; Structs are prefab so a deck round-trips through `write`/`read`, which is
;; what the golden-IR tests compare and what `--dump-ir` prints.
(require racket/list)
(provide (all-defined-out))

;; ---------------------------------------------------------------- geometry

;; rot is degrees clockwise about the center of the bbox.
(struct bbox (x y w h rot flip-h? flip-v?) #:prefab)

(define (make-bbox x y w h #:rot [rot 0.0] #:flip-h? [fh #f] #:flip-v? [fv #f])
  (bbox x y w h rot fh fv))

(define (bbox-center b)
  (values (+ (bbox-x b) (/ (bbox-w b) 2.0))
          (+ (bbox-y b) (/ (bbox-h b) 2.0))))

(define (bbox-rotated? b) (not (zero? (bbox-rot b))))

;; ------------------------------------------------------------------- paint

;; Components are 0..255; alpha is 0.0..1.0.
(struct rgba (r g b a) #:prefab)

(define (rgb r g b) (rgba r g b 1.0))
(define black (rgb 0 0 0))
(define white (rgb 255 255 255))

(define (rgba-hex c)
  (string-upcase
   (apply string-append
          (for/list ([v (in-list (list (rgba-r c) (rgba-g c) (rgba-b c)))])
            (define s (number->string (max 0 (min 255 (inexact->exact (round v)))) 16))
            (if (= 1 (string-length s)) (string-append "0" s) s)))))

;; A fill is #f (none), or one of:
(struct solid-fill (color) #:prefab)
;; stops: (listof (cons fraction rgba)), angle in degrees clockwise from +x.
(struct gradient-fill (stops angle) #:prefab)
;; src is a path string relative to the deck's media directory. `opacity` is
;; DrawingML's alphaModFix: a washed-out background picture is an ordinary thing
;; for a title slide to have, and ignoring it draws the image at full strength.
(struct image-fill (src opacity) #:prefab)
;; Producers emit many pattern names; the two colors alone already read right.
(struct pattern-fill (name fg bg) #:prefab)

;; A line is #f (none) or a stroke. dash is 'solid, 'dash, 'dot or 'dash-dot.
;; `head` and `tail` are the decorations at the two ends of a line -- an arrow on
;; a connector, most often -- as (line-end kind width length) or #f for none.
;; PowerPoint calls the start of the path the head.
;; `dash` is the style to draw with; `dash-pattern` is the exact
;; `<a:custDash>` when the file gave one, as a list of (dash . space) in
;; hundredths of the line's width, so the writer can hand it back unchanged.
(struct stroke (color width dash cap head tail dash-pattern) #:prefab)
(struct line-end (kind width length) #:prefab)

(define (make-stroke color #:width [w 1.0] #:dash [d 'solid] #:cap [c 'flat]
                     #:head [head #f] #:tail [tail #f] #:dash-pattern [dp #f])
  (stroke color w d c head tail dp))

;; -------------------------------------------------------------------- text

(struct insets (l t r b) #:prefab)
(define default-insets (insets 7.2 3.6 7.2 3.6)) ; PowerPoint's bodyPr defaults

;; kind: 'none, 'char (char is the glyph) or 'number (char is the format id).
(struct bullet (kind char font size-frac color) #:prefab)
(define no-bullet (bullet 'none #f #f #f #f))

;; size is in points; color is an rgba. `spacing` is letter spacing in points.
;; `stated` is which of these the shape said for itself, as against inherited
;; from its placeholder, the layout, the master or the theme. It matters to a
;; merge and to nothing else: an editor that states nothing leaves whatever it
;; inherits standing, and reading that as an edit rewrites the program's
;; typography to values nobody chose. `'all` means every field is the shape's
;; own, which is what a program's own runs are.
(struct trun (text family size bold? italic? underline? strike? color spacing caps
              baseline stated) #:prefab)

;; align: 'left 'center 'right 'justify. line-spacing is either
;; (cons 'percent f) or (cons 'points p). space-before/after are points.
(struct para (runs align level margin-left indent line-spacing
              space-before space-after bullet stated) #:prefab)

;; anchor: 'top 'center 'bottom. autofit: 'none, 'shrink or 'grow.
(struct text-body (paras anchor anchor-center? wrap? autofit insets rot stated) #:prefab)

(define (text-body-empty? tb)
  (or (not tb)
      (for/and ([p (in-list (text-body-paras tb))])
        (for/and ([r (in-list (para-runs p))])
          (string=? "" (trun-text r))))))

;; ---------------------------------------------------------------- elements

;; `geom` is (preset-geom name adjust-alist) or (custom-geom paths w h).
(struct preset-geom (name adjust) #:prefab)
(struct custom-geom (paths w h) #:prefab)

;; Every element carries the shape id and author-visible name from the file so
;; emitted code can be traced back to what the user clicked on in PowerPoint,
;; which is what the eventual round-trip needs.
;; The shadow a shape casts: `dist` points away from it at `dir` degrees
;; clockwise from east, blurred by `blur` points, in `color`. DrawingML has six
;; kinds of effect and this is the one decks use -- 547 of the corpus's 586.
(struct shadow (blur dist dir color) #:prefab)

(struct element (id name bbox) #:prefab)
(struct shape element (geom fill line body effect) #:prefab)
(struct picture element (src fill line crop opacity effect) #:prefab)
;; child-bbox is the coordinate space the children's boxes were written in;
;; the renderer maps it onto `bbox`.
(struct group element (children child-bbox) #:prefab)
;; Cells hold text bodies; `col-widths`/`row-heights` are points.
(struct tbl element (col-widths row-heights cells) #:prefab)
(struct tbl-cell (body fill line row-span col-span merged?) #:prefab)

;; `inherited` holds the shapes the slide layout and master paint behind the
;; slide. They are kept separate from `elements` because they are not the
;; slide's to edit -- and a write-back must not turn them into slide shapes.
;; `hidden?` is PowerPoint's Hide Slide and Keynote's Skip Slide: the slide is
;; in the deck and not in the show. It is the slide's own property, not a
;; property of anything on it, so nothing else can carry it.
;; `build` is the slide this one is a frame of, when a slide that is built in
;; clicks was split into one slide per click: the name they share, so an edit to
;; a shape on one frame can be written to the same shape on the others. Without
;; it a build is ten slides that merely look alike, and moving something means
;; moving it ten times.
(struct slide (index name width height background inherited elements hidden? build) #:prefab)
(struct deck (width height slides media-dir source) #:prefab)

;; ------------------------------------------------------------------- fonts

;; Every typeface the deck's own text names, in the order they first appear.
;; A generated program lists these so that it can refuse to run on substitutes:
;; racket/draw picks a stand-in without saying so, and a stand-in with different
;; metrics lays the deck out differently -- line spacing is a percentage of the
;; font size, so the leading stays where it was while the ink moves.
;;
;; Inherited shapes are not walked: they belong to the layout and the master,
;; and a program that does not draw them does not need their fonts.
(define (deck-font-families d)
  (define seen (make-hash))
  (define out '())
  (define (note! family)
    (when (and family (string? family) (not (string=? "" family))
               (not (hash-ref seen family #f)))
      (hash-set! seen family #t)
      (set! out (cons family out))))
  (define (walk-body! b)
    (when (text-body? b)
      (for* ([p (in-list (text-body-paras b))] [r (in-list (para-runs p))])
        (note! (trun-family r)))))
  (define (walk-element! e)
    (cond
      [(shape? e) (walk-body! (shape-body e))]
      [(group? e) (for-each walk-element! (group-children e))]
      [(tbl? e) (for* ([row (in-list (tbl-cells e))] [c (in-list row)])
                  (when (tbl-cell? c) (walk-body! (tbl-cell-body c))))]
      [else (void)]))
  (for* ([s (in-list (deck-slides d))] [e (in-list (slide-elements s))])
    (walk-element! e))
  (reverse out))

;; ------------------------------------------------------------------ walking

;; Rebuilds `e` with a different bounding box, preserving everything else.
(define (element-with-bbox e b)
  (cond
    [(shape? e) (shape (element-id e) (element-name e) b
                       (shape-geom e) (shape-fill e) (shape-line e) (shape-body e)
                       (shape-effect e))]
    [(picture? e) (picture (element-id e) (element-name e) b
                           (picture-src e) (picture-fill e) (picture-line e)
                           (picture-crop e) (picture-opacity e) (picture-effect e))]
    [(group? e) (group (element-id e) (element-name e) b
                       (group-children e) (group-child-bbox e))]
    [(tbl? e) (tbl (element-id e) (element-name e) b
                   (tbl-col-widths e) (tbl-row-heights e) (tbl-cells e))]
    [else e]))

;; Rebuilds `e` under a different name, preserving everything else.
(define (element-with-name e name)
  (cond
    [(shape? e) (shape (element-id e) name (element-bbox e)
                       (shape-geom e) (shape-fill e) (shape-line e) (shape-body e)
                       (shape-effect e))]
    [(picture? e) (picture (element-id e) name (element-bbox e)
                           (picture-src e) (picture-fill e) (picture-line e)
                           (picture-crop e) (picture-opacity e) (picture-effect e))]
    [(group? e) (group (element-id e) name (element-bbox e)
                       (group-children e) (group-child-bbox e))]
    [(tbl? e) (tbl (element-id e) name (element-bbox e)
                   (tbl-col-widths e) (tbl-row-heights e) (tbl-cells e))]
    [else e]))

;; Maps `f` over the bounding box of `e` and of every element inside it.
(define (element-map-bbox e f)
  (define moved (element-with-bbox e (f (element-bbox e))))
  (cond
    [(group? moved)
     (group (element-id moved) (element-name moved) (element-bbox moved)
            (for/list ([c (in-list (group-children moved))]) (element-map-bbox c f))
            (f (group-child-bbox moved)))]
    [(tbl? moved)
     ;; Column and row sizes are geometry too, so they scale with the box.
     (define b0 (element-bbox e))
     (define b1 (element-bbox moved))
     (define sx (if (zero? (bbox-w b0)) 1.0 (/ (bbox-w b1) (bbox-w b0))))
     (define sy (if (zero? (bbox-h b0)) 1.0 (/ (bbox-h b1) (bbox-h b0))))
     (tbl (element-id moved) (element-name moved) b1
          (for/list ([w (in-list (tbl-col-widths moved))]) (* sx w))
          (for/list ([h (in-list (tbl-row-heights moved))]) (* sy h))
          (tbl-cells moved))]
    [else moved]))

;; Applies `f` to every element, descending into groups.
(define (element-walk e f)
  (f e)
  (when (group? e)
    (for ([c (in-list (group-children e))]) (element-walk c f))))

(define (slide-all-elements s)
  (append (slide-inherited s) (slide-elements s)))

(define (deck-elements d)
  (for*/list ([s (in-list (deck-slides d))] [e (in-list (slide-all-elements s))]) e))

(define (deck-count-elements d)
  (define n 0)
  (for ([e (in-list (deck-elements d))]) (element-walk e (lambda (_) (set! n (add1 n)))))
  n)
