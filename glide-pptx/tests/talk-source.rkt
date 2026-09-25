#lang racket/base
;; A real Rhombus talk at every epoch: the pictures the source draws against
;; the editable deck that LibreOffice draws.
;;
;; The fixture tests start from a pptx. That is useful for importer fidelity,
;; but it cannot catch an exporter losing something that only exists in the
;; source program. Name a talk with
;;
;;   GLIDE_TALK=/path/to/talk.rhm xvfb-run -a raco test tests/talk-source.rkt
;;
;; and this expands each animated slide to the end of every epoch, just as
;; `raco glide export --stages --numbers` does. Without a named talk it says so
;; and passes, since the EGGCC sources do not belong to this repository.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format
         pict
         glide-pptx/sync glide-pptx/export
         (only-in glide-pptx/runtime picts->pdf)
         glide-pptx/verify
         "ink.rkt")

(define named (getenv "GLIDE_TALK"))
(define talk (and named (path->complete-path named)))
(define INK-LIMIT (string->number (or (getenv "GLIDE_TALK_INK") "0.08")))
(define MAE-LIMIT (string->number (or (getenv "GLIDE_TALK_MAE") "0.06")))
(define BAD-LIMIT (string->number (or (getenv "GLIDE_TALK_BAD") "0.08")))
;; One more grid cell than the isolated-element test: a 1920-pixel page makes
;; this about eleven pixels, enough for LibreOffice's baseline/ink-box shift on
;; an 85-point title while still much smaller than a displaced object.
(define TALK-INK-SLACK 4)
(define work (build-path (find-system-path 'temp-dir) "glide-pptx-talk-source"))

(cond
  [(not named)
   (printf "no talk source to compare (set GLIDE_TALK); skipped\n")]
  [(not (file-exists? talk))
   (error 'talk-source "GLIDE_TALK names a file that is not there: ~a" named)]
  [else
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (set-stage-slides! #t)
   (ask-slide-numbers! #t)
   (define picts (load-program-picts talk))
   (check-true (pair? picts) "the talk produced slides")
   (when (pair? picts)
     (define w (pict-width (first picts)))
     (define h (pict-height (first picts)))
     (define direct (build-path work "source-epochs.pdf"))
     (define deck (build-path work "editable-epochs.pptx"))
     (define warnings (box '()))
     ;; Export first. A long PDF draw leaves Cairo surfaces pending collection;
     ;; asking racket/draw to encode the exporter's flattened PNGs afterwards
     ;; can exhaust its native bitmap resources on a 100+ epoch talk.
     (parameterize ([current-export-warnings warnings])
       (picts->pptx picts deck #:width w #:height h))
     (picts->pdf picts direct #:width w #:height h)
     (define lo (libreoffice-pdf deck (build-path work "libreoffice")))
     (define source-pages
       (rasterize-pdf direct (build-path work "source-page") #:dpi 96))
     (define deck-pages
       (rasterize-pdf lo (build-path work "deck-page") #:dpi 96))
     (check-equal? (length deck-pages) (length source-pages)
                   "the editable deck has every shown epoch")
     (define scored
       (for/list ([source (in-list source-pages)]
                  [shown (in-list deck-pages)]
                  [i (in-naturals 1)])
         (define diff (build-path work (format "diff-~a.png" i)))
         (define-values (mae bad _w _h)
           (compare-images source shown #:diff-path diff))
         (list i (ink-error/slack source shown TALK-INK-SLACK) mae bad)))
     (printf "~a: ~a shown epochs; source vs LibreOffice editable deck\n"
             (file-name-from-path talk) (length scored))
     (for ([r (in-list (take (sort scored > #:key second)
                             (min 15 (length scored))))])
       (printf "  page ~a  ink ~a%  mean ~a%  pixels off ~a%\n"
               (~a (first r) #:min-width 3)
               (~r (* 100 (second r)) #:precision 2)
               (~r (* 100 (third r)) #:precision 2)
               (~r (* 100 (fourth r)) #:precision 2)))
     (for ([r (in-list scored)])
       (check-true (<= (second r) INK-LIMIT)
                   (format "epoch page ~a: ~a% of its drawing differs, over ~a%"
                           (first r) (~r (* 100 (second r)) #:precision 2)
                           (~r (* 100 INK-LIMIT) #:precision 1)))
       (check-true (<= (third r) MAE-LIMIT)
                   (format "epoch page ~a: mean error ~a%, over ~a%"
                           (first r) (~r (* 100 (third r)) #:precision 2)
                           (~r (* 100 MAE-LIMIT) #:precision 1)))
       (check-true (<= (fourth r) BAD-LIMIT)
                   (format "epoch page ~a: ~a% of pixels off, over ~a%"
                           (first r) (~r (* 100 (fourth r)) #:precision 2)
                           (~r (* 100 BAD-LIMIT) #:precision 1))))
     (for ([warning (in-list (remove-duplicates (reverse (unbox warnings))))])
       (printf "  ! ~a\n" warning))
     (printf "talk-source artifacts under ~a\n" work))])

(module+ main (void (test-log #:display? #t #:exit? #t)))
