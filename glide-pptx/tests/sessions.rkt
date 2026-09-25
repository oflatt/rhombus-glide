#lang racket/base
;; A session, rather than an action: a handful of edits, saved, then another
;; handful, for as long as anyone keeps working.
;;
;; `actions.rkt` asks what the merge makes of each thing a person can do.
;; Nobody does one thing. They bold a word, drag a box, recolour a shape and
;; hit save; then delete a slide and reorder what is left. The interesting
;; failures live in the combinations -- an edit that reads a form another edit
;; has already moved, a refusal that has to take the whole save with it, a
;; program that drifts a little on every round until nothing matches.
;;
;; So: deal the catalogue out at random, apply what still applies, save, and
;; insist on what must hold however the cards fall.
;;
;;   * A save lands whole or not at all. If anything was refused, nothing was
;;     written and the program is byte for byte what it was.
;;   * A save that lands settles: another one straight after has nothing to say.
;;   * And then the program and the deck agree, property by property.
;;   * A comment written in the source between saves is still there. The
;;     program is theirs as well, and the merge writes into it, not over it.
;;
;; Seeded, so a failure names the round that produced it: re-run one with
;; `GLIDE_SESSION_SEED=<n> GLIDE_SESSION_COUNT=1`. `GLIDE_SESSION_ROUNDS` and
;; `GLIDE_SESSION_EDITS` make the sessions longer and the handfuls bigger,
;; which is what a sweep looking for a combination wants.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "editor-actions.rkt" "ir-diff.rkt")

(define SESSIONS (string->number (or (getenv "GLIDE_SESSION_COUNT") "6")))
(define ROUNDS (string->number (or (getenv "GLIDE_SESSION_ROUNDS") "5")))
(define BASE-SEED (string->number (or (getenv "GLIDE_SESSION_SEED") "20260905")))
;; How many at once. Three is a save; more is a sweep looking for the pair that
;; does not get on.
(define EDITS (string->number (or (getenv "GLIDE_SESSION_EDITS") "3")))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-sessions"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; A tag the deck does not know is what a copy looks like between the merge
;; naming it and the deck being written again -- the editor gave the copy the
;; name it was copied from, and the program has just renamed it. The values are
;; what matter here.
(define (value-disagreements program deck)
  (filter (lambda (d) (not (regexp-match? #rx"is in the program and not the deck" d)))
          (disagreements program deck)))

;; The person has the program open too -- it is the source, and they are working
;; in it while the deck is open beside them. A comment they wrote is not
;; something a deck can say, so nothing the merge does may eat it: it is written
;; above an `at` form, which is where a comment is most in the way, and it has
;; to be there afterwards.
(define (comment-the-program! program n)
  (define text (file->string program))
  (define m (regexp-match-positions #px"(?m:^  at[(])" text))
  (and m
       (let ([at (caar m)])
         (display-to-file (string-append (substring text 0 at)
                                         (format "  // theirs ~a\n" n)
                                         (substring text at))
                          program #:exists 'replace)
         #t)))

(define all (catalogue))
(define applied-total 0)
(define refused-total 0)
(define rounds-total 0)

(for ([s (in-range SESSIONS)])
  (define seed (+ BASE-SEED s))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
  ;; `shuffle` deals from whichever generator is current, so the seed has to be
  ;; the current one for a failure to name a session anyone can re-run.
  (current-pseudo-random-generator rng)
  (define dir (build-path work (format "s~a" seed)))
  (make-directory* dir)
  (define-values (program deck) (lay-down! dir))
  (define workdir (build-path dir "w"))

  ;; Dealt without replacement: doing the same thing twice is not an
  ;; interesting combination, and the edits here are XML rewrites rather than
  ;; an editor -- applying one twice writes an attribute twice, which is a deck
  ;; nobody could make.
  (define deck-of-cards (shuffle all))
  ;; A refusal that cannot be got past -- a group holding two shapes under one
  ;; name is one -- would refuse every save after it as well, and the rest of
  ;; the session would go by with nothing tried. The person would give up on
  ;; that edit and let the program win, so after the second refusal in a row
  ;; that is what happens here: the deck is written again from the program.
  (define refused-in-a-row 0)
  (let round-loop ([n 0] [cards deck-of-cards])
    (when (and (< n ROUNDS) (pair? cards))
      ;; A handful, in the order they were dealt.
      (define k (min (length cards) (add1 (random EDITS rng))))
      (define picked (take cards k))
      (define rest-cards (drop cards k))
      (define landed
        (for/list ([a (in-list picked)]
                   #:when (with-handlers ([exn:fail? (lambda (_e) #f)])
                            (and ((act-spec-edit a) deck) #t)))
          a))
      (define names (map act-spec-name landed))
      (set! rounds-total (add1 rounds-total))
      ;; Now and then they leave a note in the source as well.
      (define commented? (and (zero? (random 3 rng)) (comment-the-program! program n)))
      (with-check-info (['session seed] ['round n] ['edits names])
        (unless (null? landed)
          (define before (file->string program))
          (define r
            (with-handlers ([exn:fail? (lambda (e)
                                         (fail (format "seed ~a round ~a ~s: ~a" seed n names
                                                       (first (string-split (exn-message e) "\n"))))
                                         #f)])
              (sync-once program deck #:workdir workdir #:atomic? #t)))
          (when r
            (cond
              ;; Refused: nothing written, and the program untouched.
              [(pair? (sync-report-skipped r))
               (set! refused-total (add1 refused-total))
               (set! refused-in-a-row (add1 refused-in-a-row))
               (check-equal? (sync-report-applied r) '()
                             (format "seed ~a round ~a: a refused save wrote something" seed n))
               (check-equal? (file->string program) before
                             (format "seed ~a round ~a: a refused save changed the program" seed n))
               (when (>= refused-in-a-row 2)
                 (picts->pptx (load-program-picts program) deck)
                 (void (sync-once program deck #:workdir workdir))
                 (set! refused-in-a-row 0))]
              [else
               (set! refused-in-a-row 0)
               (set! applied-total (+ applied-total (length (sync-report-applied r))))
               ;; Settles: a pass can leave the drawing order for the next.
               ;; A note is not an edit and does not go away: it says the deck
               ;; describes something differently, and it says so every time.
               (define (settled? report)
                 (null? (filter (lambda (x) (not (eq? 'noted (sync-action-kind x))))
                                (sync-report-actions report))))
               (define passes
                 (let loop ([k 1])
                   (define again (sync-once program deck #:workdir workdir #:atomic? #t))
                   (cond
                     [(settled? again) k]
                     [(>= k 3)
                      (fail (format "seed ~a round ~a ~s: still reporting ~s after ~a passes"
                                    seed n names
                                    (map sync-action-kind (sync-report-actions again)) k))
                      k]
                     [(and (null? (sync-report-applied again))
                           (pair? (sync-report-skipped again)))
                      ;; A refusal that only shows up on the second pass is
                      ;; still a refusal, and still must write nothing.
                      k]
                     [else (loop (add1 k))])))
               (void passes)
               ;; And the two of them agree -- unless a note stands, which is
               ;; the merge saying they do not and why.
               (when (null? (sync-report-notes r))
                 (check-equal? (value-disagreements program deck) '()
                               (format "seed ~a round ~a ~s: the program and the deck disagree"
                                       seed n names)))
               ;; What they wrote is still theirs -- unless the form it sat
               ;; above is gone, which takes the comment about it along.
               (when (and commented?
                          (not (ormap (lambda (x) (memq (sync-action-kind x)
                                                        '(removed removed-slide)))
                                      (sync-report-applied r))))
                 (check-regexp-match (regexp (format "// theirs ~a" n))
                                     (file->string program)
                                     (format "seed ~a round ~a ~s: the merge ate a comment"
                                             seed n names)))
               ;; Then the loop writes the deck again, which is what it does
               ;; after a merge -- and what gives a copy made in the editor the
               ;; name the program gave it.
               (picts->pptx (load-program-picts program) deck)
               (void (sync-once program deck #:workdir workdir))]))))
      (round-loop (add1 n) rest-cards)))

  ;; Whatever the session did to it, the program is still one glide can read:
  ;; it loads, its slides are pictures, and a fresh look at it says nothing new.
  (with-check-info (['session seed])
    (define picts
      (with-handlers ([exn:fail? (lambda (e)
                                   (fail (format "seed ~a: the program no longer loads: ~a" seed
                                                 (first (string-split (exn-message e) "\n"))))
                                   '())])
        (load-program-picts program)))
    (check-true (pair? picts) (format "seed ~a: it still has slides" seed))))

(printf "session tests done; ~a rounds over ~a sessions from seed ~a -- ~a edits written, ~a saves refused\n"
        rounds-total SESSIONS BASE-SEED applied-total refused-total)

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
