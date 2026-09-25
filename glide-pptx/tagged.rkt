#lang racket/base
;; Attaching structure to a pict, so export can do better than flattening it.
;;
;; A pict knows how to draw itself and nothing else, so exporting an arbitrary
;; one means reading back drawing operations: a rounded rectangle arrives as a
;; path and a paragraph as one box per drawn word. When the pict came from this
;; runtime we know more than that, and this is where that knowledge rides along.
;;
;; The carrier is a `pict` subtype rather than a side table: the tag becomes a
;; field of the value, so there is no lifetime question and nothing to keep in
;; sync. `pict`'s constructor is exported, the subtype satisfies `pict?`, and it
;; survives every pict combinator except `launder`, whose job is to erase it.
(require pict)
(provide (struct-out desc-pict) with-desc pict-desc
         (struct-out slide-desc) (struct-out shape-desc) (struct-out text-desc)
         (struct-out image-desc) (struct-out group-desc) (struct-out table-desc)
         (struct-out name-desc))

(struct desc-pict pict (desc) #:transparent)

;; Wraps `p` so it draws identically but carries `desc`. Reusing `pict-draw` is
;; what renders; listing `p` as a child is bookkeeping for finding and
;; transforms, so this does not draw twice.
(define (with-desc p desc)
  (desc-pict (pict-draw p) (pict-width p) (pict-height p)
             (pict-ascent p) (pict-descent p)
             (list (make-child p 0 0 1 1 0 0))
             (pict-panbox p) (pict-last p)
             desc))

(define (pict-desc p) (and (desc-pict? p) (desc-pict-desc p)))

;; ------------------------------------------------------------- descriptors

;; `placeds` are the runtime's `placed` structs, in paint order.
(struct slide-desc (width height background placeds hidden? transition) #:transparent)
(struct shape-desc (width height geom fill line body flip-h? flip-v? effect) #:transparent)
(struct text-desc (width height body) #:transparent)
(struct image-desc (width height src crop line flip-h? flip-v? opacity effect) #:transparent)
(struct group-desc (width height child-x child-y child-width child-height placeds
                    flip-h? flip-v?)
  #:transparent)
(struct table-desc (width height col-widths row-heights cells) #:transparent)

;; A name for what one pict draws, wherever that pict ends up.
;;
;; A tag reaches an element through the `at` that placed it, which is fine until
;; something else does the placing: a helper composing a picture of its own, a
;; blob laid over a slide with `put`. Then the element is on the slide with no
;; name, and nothing an editor does to it can be traced back. This is the name
;; on the pict itself, so it travels with what it names.
(struct name-desc (name) #:transparent)
