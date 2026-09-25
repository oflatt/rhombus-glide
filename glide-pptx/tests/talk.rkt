#lang racket/base
;; A whole talk, page by page: what this draws against what LibreOffice draws
;; from the same deck.
;;
;; The fixtures in `decks/` are small and deliberate, and small deliberate decks
;; are exactly what a real one is not. A talk someone is actually giving has
;; forty pages of callouts, code, subscripts, arrows and photographs, and the
;; differences that matter show up there first -- a title whose spaces had
;; vanished sat in one for a week while every fixture passed.
;;
;; The deck is not in the repository, because it is not ours: name one with
;;
;;   GLIDE_TALK_DECK=/path/to/talk.pptx raco test tests/talk.rkt
;;
;; or drop it in `tests/decks/local/`, which is not tracked. Without one this
;; says so and passes.
;;
;; The measure is `ink.rkt`'s: the share of the drawing that differs, which does
;; not count a drawing that sits half a pixel from where the other renderer put
;; it. `GLIDE_TALK_INK` sets the per-page ceiling.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format racket/string
         racket/runtime-path
         glide-pptx/verify "ink.rkt")

(define-runtime-path decks-dir "decks")

(define (local-decks)
  (define dir (build-path decks-dir "local"))
  (if (directory-exists? dir)
      (sort (for/list ([f (in-list (directory-list dir #:build? #t))]
                       #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
              f)
            string<? #:key path->string)
      '()))

(define decks
  (let ([named (getenv "GLIDE_TALK_DECK")])
    (cond
      [(and named (file-exists? named)) (list (string->path named))]
      [named (error 'talk "GLIDE_TALK_DECK names a file that is not there: ~a" named)]
      [else (local-decks)])))

(define INK-LIMIT (string->number (or (getenv "GLIDE_TALK_INK") "0.03")))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-talk"))

(cond
  [(null? decks)
   (printf "no talk deck to compare (set GLIDE_TALK_DECK or fill tests/decks/local); skipped\n")]
  [else
   (for ([deck (in-list decks)])
     (define name (path->string (path-replace-extension (file-name-from-path deck) "")))
     (define dd (verify-deck deck work #:dpi 96 #:keep-images? #t))
     (define dir (build-path work name))
     (define pages (deck-diff-pages dd))
     (printf "~a: ~a pages\n" name (length pages))
     (define scored
       (for/list ([p (in-list pages)])
         (define i (page-diff-index p))
         (define ours (build-path dir (format "our-page-~a.png" i)))
         (define ref (build-path dir (format "ref-page-~a.png" i)))
         (list i
               (and (file-exists? ours) (file-exists? ref) (ink-error ours ref))
               (page-diff-mae p))))
     (for ([r (in-list (sort scored > #:key (lambda (r) (or (second r) 0))))])
       (printf "  page ~a  ink ~a%  mean ~a%\n" (~a (first r) #:min-width 3)
               (~r (* 100 (or (second r) 0)) #:precision 2)
               (~r (* 100 (third r)) #:precision 2)))
     (printf "  ~a of ~a pages are the same picture\n"
             (for/sum ([r (in-list scored)]) (if (and (second r) (zero? (second r))) 1 0))
             (length scored))
     (for ([r (in-list scored)])
       (check-true (real? (second r))
                   (format "~a page ~a could be compared" name (first r)))
       (when (real? (second r))
         (check-true (<= (second r) INK-LIMIT)
                     (format "~a page ~a: ~a% of the drawing differs, over ~a%"
                             name (first r) (~r (* 100 (second r)) #:precision 2)
                             (~r (* 100 INK-LIMIT) #:precision 1))))))])

(printf "talk tests done\n")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
