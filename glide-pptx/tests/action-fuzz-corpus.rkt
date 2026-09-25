#lang racket/base
;; The action fuzzer over the corpus rather than the fixtures.
;;
;; `action-fuzz.rkt` deals random combinations of edits at whatever it is
;; handed. Handed the six fixtures it finds what six deliberate decks can show;
;; handed five hundred real ones it finds what a real one does, which is the
;; point of having them.
;;
;; In slices, each in its own process, for the reason `agreement.rkt` gives: a
;; program read costs about ten megabytes that is never given back, so five
;; hundred in one process thrashes.
;;
;; `GLIDE_FUZZ_CORPUS_ALL` sweeps the whole corpus; without it a rotating slice
;; is taken, so a run costs minutes and a week of runs covers everything.
(require rackunit/log)
(require rackunit racket/list racket/string racket/system racket/port racket/file
         racket/runtime-path)

(define-runtime-path corpus-dir "corpus")
(define-runtime-path fuzz "action-fuzz.rkt")

(define decks
  (if (directory-exists? corpus-dir)
      (sort (for/list ([f (in-list (directory-list corpus-dir))]
                       #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
              (path->string f))
            string<?)
      '()))

;; How many decks one process takes. Twenty is well under the leak's reach and
;; about three minutes.
(define SLICE 20)
(define ALL? (and (getenv "GLIDE_FUZZ_CORPUS_ALL") #t))
;; Where a rotating slice starts: the day, so consecutive runs cover new ground
;; without anyone choosing a number.
(define START
  (if ALL?
      0
      (modulo (* SLICE (quotient (current-seconds) 86400)) (max 1 (length decks)))))

(define racket-exe
  (let ([e (find-system-path 'exec-file)])
    (if (absolute-path? e) e (or (find-executable-path e) e))))

(define (run-slice! skip n)
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port out]
                   [current-environment-variables
                    (environment-variables-copy (current-environment-variables))])
      (putenv "GLIDE_FUZZ_CORPUS" (number->string n))
      (putenv "GLIDE_FUZZ_CORPUS_SKIP" (number->string skip))
      (putenv "GLIDE_FUZZ_ROUNDS" "3")
      (system* racket-exe (path->string fuzz))))
  (define text (get-output-string out))
  (printf "~a" text)
  (flush-output)
  ;; A slice that failed a check says so in its own output and exits non-zero.
  (check-true ok? (format "the slice from ~a came through" skip)))

(cond
  [(null? decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   (define wanted (if ALL? (length decks) SLICE))
   (printf "fuzzing actions over ~a corpus decks from ~a\n" wanted START)
   (for ([start (in-range 0 wanted SLICE)])
     (run-slice! (modulo (+ START start) (length decks))
                 (min SLICE (- wanted start))))])

(module+ main (void (test-log #:display? #t #:exit? #t)))
