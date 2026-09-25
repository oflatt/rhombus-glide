#lang racket/base
;; The rest: everything that renders through LibreOffice to compare against, and
;; everything that sweeps the corpus. It needs both of those present.
;;
;; Most of an hour, nearly all of it `agreement.rkt`: that one reads a whole
;; Rhombus program for each of five hundred decks, and compiling one of those
;; costs more than everything else here put together. `GLIDE_AGREE_N` cuts it
;; down to a slice while working on one deck.
;;
;; `tools/fetch-corpus.sh` downloads the decks; without them the corpus and
;; coverage modules say so and pass.
(require rackunit/log)
(require "export.rkt" "roundtrip.rkt" "fidelity.rkt" "elements.rkt" "render.rkt" "roundtrip-look.rkt"
         "coverage.rkt" "corpus.rkt"
         ;; The same corpus, asked whether each deck agrees with the deck it
         ;; writes rather than only whether it survives being read, and asked
         ;; what it makes of random combinations of edits.
         "agreement.rkt" "action-fuzz-corpus.rkt"
         ;; Here rather than in the fast suite because it runs whole Rhombus
         ;; programs, and compiling one of those costs more than everything the
         ;; fast suite does.
         "staged.rkt"
         ;; A whole talk, when there is one to compare against; it says so and
         ;; passes when there is not.
         "talk.rkt" "talk-source.rkt")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
