#lang racket/base
;; Edits to a real talk, made in the deck and merged back.
;;
;; The other suites build the programs they test, which keeps them small and
;; keeps them honest about one thing at a time. A talk is the other kind of
;; evidence: sixteen slides somebody wrote for a conference, half of them drawn
;; by helpers, with an e-graph laid out by code and animations on top -- and
;; every refusal this found was a refusal on a slide no built-up example had.
;;
;; The talk is not in this repository, so this needs to be told where it is:
;;
;;   GLIDE_TALK=~/src/talks/2026-eggcc/talk.rhm raco test tests/talk-edits.rkt
;;
;; Without that it says so and passes. `docs/talk-backlog.md` is the list this
;; comes from, and says what each of these stands for.
(require rackunit rackunit/log racket/file racket/list racket/string racket/path
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "deck-edit.rkt")

(define talk-src (getenv "GLIDE_TALK"))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-talk-edits"))

;; What an edit is expected to come to: written into the source, or said and
;; not written. `where` is what the source has to hold afterwards.
(struct expectation (kinds where) #:transparent)
(define (applied #:kinds [kinds '(moved)] #:source [where #f]) (expectation kinds where))
(define (reported) (expectation '() #f))

;; One scenario: a name, what it does to the deck, and what should come of it.
(struct scenario (name do expect) #:transparent)

(define scenarios
  (list
   ;; Text
   (scenario "title-retype"
             (lambda (d) (retext-in-deck! d 1 "Efficient Extraction from Effectful E-Graphs"
                                          "Efficient Extraction from Effectful E-Graphs!"))
             (applied #:kinds '(retext)
                      #:source #rx"Efficient Extraction from Effectful E-Graphs!"))
   (scenario "icon-retype" (lambda (d) (retext-in-deck! d 3 "SQL" "Datalog"))
             (applied #:kinds '(retext) #:source #rx"run[(]\"Datalog\""))
   (scenario "axis-retype" (lambda (d) (retext-in-deck! d 13 "LLVM-O0-O0" "LLVM -O0"))
             (applied #:kinds '(retext)))
   ;; A caption the program draws rather than places: said, not written.
   (scenario "caption-retype" (lambda (d) (retext-in-deck! d 4 "Text 19" "Data"))
             (reported))
   ;; Position
   (scenario "title-move"
             (lambda (d) (drag-in-deck! d 1 "Efficient Extraction from Effectful E-Graphs"
                                        200.0 120.0))
             (applied #:source #rx"at[(]200[.]0, 120[.]0"))
   (scenario "group-move" (lambda (d) (nudge-family-in-deck! d 3 "Group (3)" 40.0 20.0))
             (applied))
   (scenario "node-move" (lambda (d) (drag-in-deck! d 4 "a" 400.0 300.0))
             (applied #:source #rx"~nudge:"))
   (scenario "plot-move" (lambda (d) (drag-in-deck! d 11 "extraction-time-cdf.pdf" 200.0 200.0))
             (applied))
   ;; Size
   (scenario "node-resize" (lambda (d) (resize-in-deck! d 4 "a" 140.0 140.0))
             (applied #:kinds '(resized)))
   (scenario "chart-resize" (lambda (d) (resize-in-deck! d 13 "g0.pdf" 900.0 500.0))
             (applied #:kinds '(resized)))
   ;; Structure
   (scenario "add-shape" (lambda (d) (add-shape-to-deck! d 4 "New Box" #:x 300.0 #:y 300.0))
             (applied #:kinds '(added) #:source #rx"~tag: \"New Box\""))
   (scenario "duplicate" (lambda (d) (duplicate-in-deck! d 3 "…and more!"))
             (applied #:kinds '(added) #:source #rx"~tag: \"…and more! [(]2[)]\""))
   (scenario "copy-across" (lambda (d) (move-element-to-slide! d "…and more!" 3 4))
             (applied #:kinds '(removed added)))
   (scenario "ungroup" (lambda (d) (ungroup-in-deck! d 3 "Group (3)"))
             (applied #:kinds '(removed added added)))
   (scenario "delete-element" (lambda (d) (delete-from-deck! d 3 "SQL"))
             (applied #:kinds '(removed moved)))
   ;; The list of slides: entries a talk writes as calls, not as bare names.
   (scenario "delete-slide" (lambda (d) (delete-slide! d 16))
             (applied #:kinds '(removed-slide)))
   (scenario "reorder" (lambda (d) (move-slide! d 15 16))
             (applied #:kinds '(reordered)))))

;; A base to copy for each scenario: the talk beside its own modules, the deck
;; exported from it, and the agreed base beside that.
(define (prepare!)
  (define src-dir (path-only (path->complete-path talk-src)))
  (define base (build-path work "base"))
  (make-directory* work)
  (delete-directory/files base #:must-exist? #f)
  (make-directory* base)
  (for ([f (in-list (directory-list src-dir #:build? #t))])
    (define name (file-name-from-path f))
    (when (or (regexp-match? #rx"[.]rhm$" (path->string f)) (directory-exists? f))
      (if (directory-exists? f)
          (when (equal? "media" (path->string name))
            (copy-directory/files f (build-path base name)))
          (copy-file f (build-path base name) #t))))
  (define talk (build-path base (file-name-from-path talk-src)))
  (picts->pptx (load-program-picts talk) (build-path base "deck.pptx"))
  (void (sync-once talk (build-path base "deck.pptx") #:workdir (build-path base "w")))
  base)

;; The base file names the program it was agreed with, so a copy is told it is
;; now that copy's base.
(define (retarget! base dir)
  (define g (build-path dir ".glide"))
  (when (directory-exists? g)
    (for ([f (in-list (directory-list g #:build? #t))]
          #:when (regexp-match? #rx"[.]rktd$" (path->string f)))
      (display-to-file (string-replace (file->string f)
                                       (path->string (path->complete-path base))
                                       (path->string (path->complete-path dir)))
                       f #:exists 'replace))))

(define (run-scenario! base s)
  (define dir (build-path work (string-append "run-" (scenario-name s))))
  (delete-directory/files dir #:must-exist? #f)
  (copy-directory/files base dir)
  (retarget! base dir)
  (define talk (build-path dir (file-name-from-path talk-src)))
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (define did ((scenario-do s) deck))
  (check-not-false did (format "~a: the deck takes the edit" (scenario-name s)))
  (when did
    (define r (sync-once talk deck #:workdir w #:atomic? #t))
    (define want (scenario-expect s))
    (check-equal? (sort (map (lambda (a) (format "~a" (sync-action-kind a)))
                             (sync-report-applied r))
                        string<?)
                  (sort (map (lambda (k) (format "~a" k)) (expectation-kinds want)) string<?)
                  (format "~a: ~a" (scenario-name s)
                          (if (null? (expectation-kinds want))
                              "is reported rather than written"
                              "is written")))
    (when (expectation-where want)
      (check-regexp-match (expectation-where want) (file->string talk)
                          (format "~a: and this is what the source says" (scenario-name s))))
    ;; Whatever it did, the program still reads and the deck written from it has
    ;; nothing left to merge -- which is the property every one of these shares.
    (when (pair? (sync-report-applied r))
      (picts->pptx (load-program-picts talk) deck)
      (check-equal? (sync-report-actions (sync-once talk deck #:workdir w #:atomic? #t)) '()
                    (format "~a: and it settled" (scenario-name s))))))

(cond
  [(not talk-src)
   (printf "talk-edits: skipped -- set GLIDE_TALK to a talk's .rhm to run these\n")]
  [(not (file-exists? talk-src))
   (printf "talk-edits: skipped -- ~a is not there\n" talk-src)]
  [else
   (define base (prepare!))
   (for ([s (in-list scenarios)]) (run-scenario! base s))
   (printf "talk-edits: ~a scenarios; artifacts under ~a\n" (length scenarios) work)])

(module+ main (void (test-log #:display? #t #:exit? #t)))
