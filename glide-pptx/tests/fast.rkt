#lang racket/base
;; The suite that needs no corpus. It also exercises actual LibreOffice saves,
;; edits and live reloads when LibreOffice is installed; those checks announce
;; that they were skipped on a Racket-only machine. A few minutes, which is the
;; one to run while working.
;;
;; What it does cover is everything with an exact answer -- the round trip
;; through the IR, the sync, the fuzzer, the parser's own units -- so a
;; regression in any of that shows up here rather than in the long sweep.
(require rackunit/log
         "unit.rkt" "fuzz.rkt" "structural.rkt" "flatten.rkt"
         "sync.rkt" "actions.rkt" "sessions.rkt" "scenarios.rkt" "watch.rkt"
         ;; Random combinations of edits, over decks it discovers targets in
         ;; rather than a catalogue written for one fixture.
         "action-fuzz.rkt"
         ;; And what could be edited at all, which needs no deck to answer.
         "editable.rkt"
         ;; Every string in a program retyped, one run at a time.
         "retype-all.rkt"
         ;; These skip themselves where there is no LibreOffice to drive.
         ;; `lo-roundtrip` needs only `--convert-to`; `lo-edit` drives a real
         ;; editor through a real edit -- move, retype, recolour, delete, add --
         ;; which is the workflow the rest of this only approximates.
         "libreoffice.rkt" "lo-roundtrip.rkt" "lo-edit.rkt")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
