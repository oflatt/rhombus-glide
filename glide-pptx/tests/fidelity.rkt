#lang racket/base
;; How close our rendering is to LibreOffice's rendering of the same deck.
;;
;; This is a regression guard, not a proof of correctness: LibreOffice is itself
;; an approximation of PowerPoint, and two text engines will never agree to the
;; pixel. The thresholds are set just above where the pipeline currently sits, so
;; a real layout regression trips them while antialiasing noise does not.
;;
;; Each deck records the score it achieved, so a change that makes fidelity
;; *better* is visible in the diff rather than silently absorbed.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/verify)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-fidelity"))

;; deck -> (mean-error-ceiling . differing-pixel-ceiling), as fractions.
;; Text-heavy pages score worse than shape-heavy ones because glyph rasterization
;; is where the two engines differ most.
(define budgets
  (hash "01-placeholders" '(0.010 . 0.015)
        "02-text"         '(0.010 . 0.020)
        "03-shapes"       '(0.018 . 0.032)
        "04-pictures-groups" '(0.008 . 0.012)
        "05-realistic"    '(0.018 . 0.025)
        ;; What only exists in the drawing: scripts, a strike, letter spacing,
        ;; line spacing. Text-heavy and small, so the page-level numbers are
        ;; modest -- `elements.rkt` is where this deck is really asked about.
        "06-drawn"        '(0.020 . 0.030)))

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

(define (sorted l) (sort l string<?))

(check-equal? (sorted decks) (sorted (hash-keys budgets))
              "every fixture deck has a fidelity budget")

(for ([name (in-list decks)])
  (define budget (hash-ref budgets name '(0.02 . 0.06)))
  (define dd (verify-deck (build-path decks-dir (string-append name ".pptx")) work
                          #:dpi 96
                          #:mae-threshold (car budget)
                          #:bad-threshold (cdr budget)
                          #:keep-images? #t))
  (printf "~a\n" (format-report dd #:mae-threshold (car budget) #:bad-threshold (cdr budget)))
  (for ([p (in-list (deck-diff-pages dd))])
    (check-true (<= (page-diff-mae p) (car budget))
                (format "~a page ~a mean error ~a% is within ~a%"
                        name (page-diff-index p)
                        (~r (* 100 (page-diff-mae p)) #:precision 2)
                        (* 100 (car budget))))
    (check-true (<= (page-diff-bad-fraction p) (cdr budget))
                (format "~a page ~a has ~a% of pixels off, budget ~a%"
                        name (page-diff-index p)
                        (~r (* 100 (page-diff-bad-fraction p)) #:precision 2)
                        (* 100 (cdr budget))))))

(printf "fidelity tests done; diff images under ~a\n" work)

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
