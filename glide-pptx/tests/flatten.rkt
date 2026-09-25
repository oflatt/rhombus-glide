#lang racket/base
;; What the exported file offers should be what the tool can honor.
;;
;; A pict with no descriptor -- `vc-append` of a few things inside an `at` --
;; has no structure to sync. Flattening its drawing would give a pile of
;; separate shapes, each draggable in the editor and none of them movable back,
;; because the only thing the source names is the enclosing `at`. One picture
;; instead means one object per `at`, and dragging it lands on numbers that
;; exist.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/runtime-path pict
         glide-pptx/ir glide-pptx/export glide-pptx/semantic glide-pptx/runtime
         glide-pptx/sync "deck-edit.rkt")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-flatten"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define (slide-part pptx n) (deck-part pptx (format "ppt/slides/slide~a.xml" n)))

(define (count rx s) (length (regexp-match* rx s)))

;; A described shape, and an undescribed composition, side by side.
(define (make-slide)
  (slide-canvas
   #:width 480.0 #:height 270.0 #:background (hex "FFFFFF")
   (at 20.0 20.0 #:tag "Real Shape"
       (shape-pict #:width 120.0 #:height 50.0 #:shape "roundRect"
                   #:fill (hex "4472C4")))
   ;; No descriptor: pict composed the parts, so only the `at` is nameable.
   (at 200.0 20.0 #:tag "Diagram"
       (vc-append 6
                  (shape-pict #:width 100.0 #:height 40.0 #:fill (hex "ED7D31"))
                  (shape-pict #:width 100.0 #:height 40.0 #:fill (hex "70AD47"))
                  (textbox #:width 100.0 #:height 20.0
                           (para* (run* "inside" #:size 10.0)))))))

(define warns (box '()))
(define flat (build-path work "flat.pptx"))
(parameterize ([current-export-warnings warns] [current-flatten-opaque? #t])
  (picts->pptx (list (make-slide)) flat #:width 480.0 #:height 270.0))
(define flat-xml (slide-part flat 1))

;; One picture for the composition, and the described shape untouched.
(check-equal? (count #rx"<p:pic>" flat-xml) 1
              "the composition became exactly one picture")
(check-true (regexp-match? #rx"glide-pptx:Diagram" flat-xml)
            "and it carries the tag, so a drag has somewhere to land")
(check-true (regexp-match? #rx"prst=\"roundRect\"" flat-xml)
            "the described shape is still a real shape")
(check-equal? (count #rx"<a:t>" flat-xml) 0
              "the composition's text is inside the picture, not a separate box")
(check-true (for/or ([w (in-list (unbox warns))])
              (regexp-match? #rx"Diagram.*one picture" w))
            (format "and the flattening was reported: ~a" (unbox warns)))

;; Without flattening, the same composition scatters into separate shapes --
;; which is what a one-way export wants and a round trip does not.
(define loose (build-path work "loose.pptx"))
(parameterize ([current-flatten-opaque? #f])
  (picts->pptx (list (make-slide)) loose #:width 480.0 #:height 270.0))
(define loose-xml (slide-part loose 1))
(check-equal? (count #rx"<p:pic>" loose-xml) 0 "nothing was rasterized")
(check-true (> (count #rx"<p:sp>" loose-xml) (count #rx"<p:sp>" flat-xml))
            "and the composition is several shapes instead of one picture")
(check-true (> (count #rx"<a:t>" loose-xml) 0) "with its text as text")

;; The payoff: the flattened object is draggable *and* syncable.
(let ()
  (define dir (build-path work "sync"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "export:"
              "  all_slides"
              "def slide_1 = slide_canvas("
              "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
              "  at(200.0, 20.0, ~tag: \"Diagram\","
              "     vstack(~sep: 6,"
              "            shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"ED7D31\")),"
              "            shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"70AD47\"))))"
              ")"
              "def all_slides = [slide_1]"
              "")
        "\n") o)))
  (define deck (build-path dir "deck.pptx"))
  (define picts (load-program-picts program))
  (picts->pptx picts deck #:width 480.0 #:height 270.0)
  (check-equal? (count #rx"<p:pic>" (slide-part deck 1)) 1
              "the whole element is one object in the deck")
  (sync-once program deck #:workdir (build-path dir "w"))

  ;; Drag that one object.
  (check-true (drag-in-deck! deck 1 "Diagram" 300.0 90.0)
              "the flattened object was found in the deck to drag")
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r)) 1
                "dragging the flattened object merged back")
  (check-true (regexp-match? #rx"at[(]300[.]0, 90[.]0" (file->string program))
              (format "onto the `at` numbers:\n~a" (file->string program))))

(printf "flatten tests done\n")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
