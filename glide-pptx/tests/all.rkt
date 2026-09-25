#lang racket/base
;; Everything. `raco test tests/fast.rkt` is the one to run while working -- it
;; needs nothing but Racket and takes about a minute and a half; this adds the
;; comparisons against LibreOffice and the sweep over the corpus.
(require rackunit/log)
(require "fast.rkt" "slow.rkt")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
