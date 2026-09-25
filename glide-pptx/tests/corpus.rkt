#lang racket/base
;; The whole pipeline over a corpus of real decks.
;;
;; Five fixtures written by us test what we thought to test. LibreOffice's pptx
;; regression suite -- several hundred decks, each aimed at one awkward corner of
;; the format -- tests what we did not, and it found real bugs on the first run:
;; a custom path that opens with a line instead of a move, and a picture whose
;; relationship resolves to nothing.
;;
;; What is checked here is *crash-freedom* across import, render, draw, export
;; and re-import, not fidelity. These files are deliberately strange, and a
;; per-deck pixel budget for them would mean nothing. The approximation notes are
;; printed as an inventory, which is how the unsupported-feature list stays
;; honest.
;;
;; The decks are not committed -- they belong to LibreOffice. Run
;; `tools/fetch-corpus.sh` to get them; with no corpus present this test says so
;; and passes.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/class racket/draw racket/runtime-path pict
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/emit-rhombus glide-pptx/sync-state
         (only-in glide-pptx/sync find-at-sites))

(define-runtime-path corpus-dir "corpus")

(define decks
  (if (directory-exists? corpus-dir)
      (sort (filter (lambda (p) (regexp-match? #rx"[.]pptx$" (path->string p)))
                    (directory-list corpus-dir))
            string<? #:key path->string)
      '()))

(cond
  [(null? decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   ;; These decks include 62 with charts or SmartArt. Elsewhere that is an error,
  ;; because an empty box would quietly replace the content on a round trip.
  ;; Here nothing is round-tripped -- the question is only whether we crash.
  (current-allow-unsupported? #t)
  (define work (build-path (find-system-path 'temp-dir) "glide-pptx-corpus"))
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)

;; What the round-trip comparison still finds, named so it stays visible. Each
;; is a bug rather than a tolerance: they are here so the sweep can guard the
;; other 398 decks in the meantime.
(define known-round-trip-gaps
  (hash "tdf149865.pptx"
        (string-append
         "a `userDrawn` layout shape comes in at the wrong box and loses its "
         "scheme-colour fill")))

   (define failures '())
   (define notes (make-hash))

   (for ([d (in-list decks)] [i (in-naturals 1)])
     (define name (path->string d))
     (define warns (box '()))
     ;; A file this refuses on purpose is not a failure. Some of these decks are
     ;; deliberately corrupt -- POI keeps its fuzzer's findings here -- and the
     ;; right behaviour is to say so in our own words. What is a failure is an
     ;; internal error: a contract violation or a parse error from underneath,
     ;; which means the malformed input reached code that assumed good input.
     (define (refusal? e)
       (and (exn? e) (regexp-match? #rx"^glide[-a-z]*:" (exn-message e))))
     (define (phase label thunk)
       (with-handlers ([(lambda (_e) #t)
                        (lambda (e)
                          (cond
                            [(refusal? e)
                             (hash-update! notes
                                           ;; The path differs per deck; the
                                           ;; reason does not.
                                           (format "refused: ~a"
                                                   (regexp-replace
                                                    #px"^glide[-a-z]*: [^ ]+ "
                                                    (first (string-split (exn-message e) "\n"))
                                                    ""))
                                           add1 0)]
                            [else
                             (set! failures
                                   (cons (list name label
                                               (if (exn? e)
                                                   (first (string-split (exn-message e) "\n"))
                                                   (format "~a" e)))
                                         failures))])
                          #f)])
         (thunk)))
     (define deck
       (parameterize ([current-warnings warns])
         (phase 'import (lambda () (pptx->deck (build-path corpus-dir d)
                                               #:workdir (build-path work (format "u~a" i)))))))
     (define picts
       (and deck (parameterize ([runtime-warnings warns])
                   (phase 'render (lambda () (deck->picts deck))))))
     (when (and picts (pair? picts))
       ;; Actually draw a page: dc-level bugs only show up when something draws.
       (parameterize ([runtime-warnings warns])
         (phase 'draw
                (lambda ()
                  (define p (first picts))
                  (define (dim v) (max 1 (min 400 (inexact->exact (ceiling v)))))
                  (define bm (make-bitmap (dim (pict-width p)) (dim (pict-height p))))
                  (draw-pict p (new bitmap-dc% [bitmap bm]) 0 0)
                  #t)))
       (define out (build-path work (format "out~a.pptx" i)))
       (define wrote
         (parameterize ([current-export-warnings warns])
           (phase 'export (lambda () (picts->pptx picts out
                                                  #:width (deck-width deck)
                                                  #:height (deck-height deck))))))
       (when wrote
         (phase 'reimport
                (lambda () (pptx->deck out #:workdir (build-path work (format "r~a" i))))))
       ;; The round trip has to preserve the elements. Both sides go through the
       ;; same renderer, so unlike a pixel diff there is nothing here that a
       ;; difference could legitimately be: anything that moves, grows, changes
       ;; colour or loses its rotation is a bug. This is where a dropped rotation
       ;; on a rotated freeform was found, and a box taken from a curve's control
       ;; points instead of the shape.
       (when wrote
         (phase 'structure
                (lambda ()
                  (define again (pptx->deck out #:workdir (build-path work (format "s~a" i))))
                  (define bs (deck->slide-states deck #:include-inherited? #t #:descend-groups? #t))
                  (define as (deck->slide-states again #:include-inherited? #t #:descend-groups? #t))
                  (define ds
                    (append*
                     (for/list ([b (in-list bs)] [a (in-list as)])
                       ;; Matched by tag, so only an element that has one and
                       ;; whose name is its alone can take part. An inherited
                       ;; shape's name is not made unique -- uniquifying runs
                       ;; over a slide's own elements -- so a repeated name is
                       ;; this comparison's blind spot, not a difference.
                       (define counts
                         (for/fold ([h (hash)]) ([e (in-list (slide-state-elements b))])
                           (hash-update h (el-state-tag e) add1 0)))
                       (define by-tag
                         (for/hash ([e (in-list (slide-state-elements a))]
                                    #:when (el-state-tag e))
                           (values (el-state-tag e) e)))
                       (append*
                        ;; A table is drawn on export but not written back as a
                        ;; table -- it becomes the shapes and text it is made of,
                        ;; so it keeps its appearance and loses its identity.
                        ;; That is the one difference this sweep still finds, on
                        ;; 33 of these decks, and it is a gap rather than a
                        ;; mistake: `<a:tbl>` is not written yet.
                        ;; A slide holding a table is not comparable: the table
                        ;; exports as the shapes and text it is made of, so the
                        ;; slide gains elements with no counterpart and matching
                        ;; the rest by name pairs the wrong two. The gap itself
                        ;; is recorded below.
                        (for/list ([e (in-list (slide-state-elements b))]
                                   #:unless (for/or ([x (in-list (slide-state-elements b))])
                                              (eq? 'table (el-state-kind x)))
                                   #:when (and (el-state-tag e)
                                               (= 1 (hash-ref counts (el-state-tag e) 0))))
                          (define hit (hash-ref by-tag (el-state-tag e) #f))
                          (cond
                            [(not hit) (list (format "~s is gone" (el-state-tag e)))]
                            [(not (el-geometry-same? e hit))
                             (list (format "~s geometry ~a -> ~a" (el-state-tag e)
                                           (map (lambda (v) (~r v #:precision 2))
                                                (el-geometry e))
                                           (map (lambda (v) (~r v #:precision 2))
                                                (el-geometry hit))))]
                            [else '()]))))))
                  (unless (null? ds)
                    (cond
                      [(hash-ref known-round-trip-gaps name #f)
                       => (lambda (why)
                            (hash-update! notes
                                          (format "round trip: ~a (~a)" why name)
                                          add1 0))]
                      [else
                       (error 'structure "~a"
                              (string-join (take ds (min 3 (length ds))) "; "))]))
                  #t))))
     ;; The emitted program has to be readable by the reader a sync uses. This
     ;; is where the printer's outdenting was caught writing indentation
     ;; shrubbery rejects -- on 8 of these decks, in files Rhombus itself
     ;; compiled without complaint.
     (when deck
       (define prog (build-path work (format "p~a.rhm" i)))
       (when (phase 'emit (lambda () (write-rhombus-deck deck prog
                                                         #:source-name name)
                                     #t))
         (phase 'reparse (lambda () (find-at-sites prog) #t))))
     ;; Note kinds, with names and numbers folded out so they group.
     (for ([w (in-list (remove-duplicates (unbox warns)))])
       (hash-update! notes (regexp-replace* #px"[0-9]+|\"[^\"]*\"" w "_") add1 0)))

   (define broken (remove-duplicates (map first failures)))
   (printf "~a decks, ~a with a failure\n" (length decks) (length broken))
   (for ([f (in-list (reverse failures))])
     (printf "  ~a  [~a]  ~a\n" (~a (first f) #:min-width 32) (second f) (third f)))
   (printf "\napproximation notes, by kind:\n")
   (for ([p (in-list (sort (hash->list notes) > #:key cdr))])
     (printf "  ~a  ~a\n" (~a (cdr p) #:min-width 5) (car p)))

   (check-equal? failures '()
                 (format "~a of ~a decks failed somewhere in the pipeline"
                         (length broken) (length decks)))
   (delete-directory/files work #:must-exist? #f)])

(printf "corpus tests done\n")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
