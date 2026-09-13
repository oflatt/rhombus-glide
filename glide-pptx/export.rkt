#lang racket/base
;; pict -> .pptx, the mirror of `picts->pdf`.
;;
;; Works for any pict, from any program: the pict is drawn through a recording
;; dc, the recording becomes a display list, and the display list becomes real
;; PowerPoint objects. Nothing is required of the program being exported.
(require racket/list racket/class racket/treelist racket/generator pict
         "draw-ir.rkt" "record-adapt.rkt" "pptx-write.rkt" "semantic.rkt")
(provide picts->pptx pict->pptx current-export-warnings semantic-page?)

(define current-export-warnings (make-parameter #f))

;; A Rhombus `List` is a treelist rather than a Racket list, and a Rhombus
;; program's slides arrive as one.
(define (picts->pptx picts* path #:width [width #f] #:height [height #f])
  (define picts (if (treelist? picts*) (treelist->list picts*) picts*))
  (when (null? picts) (error 'picts->pptx "no picts to export"))
  (define w (or width (pict-width (first picts))))
  (define h (or height (pict-height (first picts))))
  (define warnings (current-export-warnings))
  ;; A pict that says how it was built exports as real shapes with reflowable
  ;; text; anything else falls back to the flattened display list.
  (parameterize ([current-write-warnings warnings])
    (display-page-sequence->pptx
     ;; A generator matters here, rather than a `for/list`: fallback pictures
     ;; carry raw pixels until the writer has encoded them. Yielding one page at
     ;; a time keeps a 100+ epoch talk bounded by one page's fallbacks.
     (in-generator
      (parameterize ([current-adapt-warnings warnings])
        (for ([p (in-list picts)])
          (collect-garbage 'incremental)
          (yield (pict->page p w h)))))
     (length picts) w h path)))

(define (pict->pptx p path #:width [width #f] #:height [height #f])
  (picts->pptx (list p) path #:width width #:height height))
