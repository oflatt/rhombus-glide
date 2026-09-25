#lang racket/base
;; deck IR -> picts, by way of the same runtime that emitted programs call.
;;
;; Keeping one runtime means the direct render and a generated program cannot
;; drift apart: a fidelity fix lands in both at once.
(require racket/list racket/path
         pict
         "ir.rkt" "runtime.rkt")
(provide deck->picts slide->pict element->placed
         current-media-root)

;; Where a picture's package-relative part name resolves against.
(define current-media-root (make-parameter #f))

;; A picture whose relationship could not be resolved has no source at all,
;; which the parser already reported.
(define (media-path src)
  (and src
       (let ([root (current-media-root)])
         (if root (build-path root src) src))))

(define (deck->picts d)
  (parameterize ([current-media-root (or (current-media-root) (deck-media-dir d))])
    (for/list ([s (in-list (deck-slides d))]) (slide->pict s))))

(define (slide->pict s)
  (apply slide-canvas
         #:width (slide-width s) #:height (slide-height s)
         #:background (or (slide-background s) (solid-fill white))
         #:hidden? (slide-hidden? s)
         (for/list ([e (in-list (slide-all-elements s))]) (element->placed e))))

(define (element->placed e)
  (define b (element-bbox e))
  (at (bbox-x b) (bbox-y b) (element->pict e)
      #:rotate (bbox-rot b)
      ;; The element's name is its tag, so a deck exported straight from the IR
      ;; carries the same keys generated code would.
      #:tag (let ([n (element-name e)]) (and (not (string=? "" n)) n))))

(define (element->pict e)
  (define b (element-bbox e))
  (define w (max 0.0 (bbox-w b)))
  (define h (max 0.0 (bbox-h b)))
  (cond
    [(shape? e)
     (shape-pict #:width w #:height h
                 #:geom (shape-geom e)
                 #:fill (resolve-fill (shape-fill e))
                 #:line (shape-line e)
                 #:body (shape-body e)
                 ;; The shadow the deck says it casts. A program carries this as
                 ;; `~shadow:` and draws it; rendering a deck straight to a page
                 ;; did not, which made a shadowed box the one thing the
                 ;; comparisons against LibreOffice could not see.
                 #:shadow (shape-effect e)
                 #:flip-h? (bbox-flip-h? b) #:flip-v? (bbox-flip-v? b))]
    [(picture? e)
     (image-pict (media-path (picture-src e)) w h
                 #:crop (picture-crop e)
                 #:line (picture-line e)
                 #:opacity (picture-opacity e)
                 #:shadow (picture-effect e)
                 #:flip-h? (bbox-flip-h? b) #:flip-v? (bbox-flip-v? b))]
    [(group? e)
     (define cb (group-child-bbox e))
     (apply group-pict
            #:width w #:height h
            #:child-x (bbox-x cb) #:child-y (bbox-y cb)
            #:child-width (max 1.0 (bbox-w cb)) #:child-height (max 1.0 (bbox-h cb))
            #:flip-h? (bbox-flip-h? b) #:flip-v? (bbox-flip-v? b)
            (for/list ([c (in-list (group-children e))]) (element->placed c)))]
    [(tbl? e)
     (table-pict #:width w #:height h
                 #:col-widths (tbl-col-widths e)
                 #:row-heights (tbl-row-heights e)
                 #:cells (tbl-cells e))]
    [else (blank w h)]))

;; An image fill's src is a part name too, so it needs the same resolution.
(define (resolve-fill f)
  (if (image-fill? f)
      (image-fill (media-path (image-fill-src f)) (image-fill-opacity f))
      f))
