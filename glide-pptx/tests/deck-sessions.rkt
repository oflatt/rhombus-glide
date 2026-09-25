#lang racket/base
;; A session on a deck nobody here wrote.
;;
;; `sessions.rkt` works on a fixture with eight elements in it, which is to say
;; on what I thought to put there. These are real decks -- LibreOffice's and
;; Apache POI's regression suites, several hundred of them, each one aimed at
;; whatever corner of the format someone had a bug in -- translated to a program
;; and then worked on the way anyone works: a handful of edits, saved, and
;; again, for as long as the session lasts.
;;
;; The edits cannot be a catalogue here, because the elements are not ours to
;; name. Each round reads what the program actually holds -- which slides, which
;; tags, which of those carry text and which are groups -- and does something to
;; one of them. What is asked of the result is what `sessions.rkt` asks: a save
;; lands whole or not at all, a save that lands settles, the program and the
;; deck agree afterwards, and the program still reads.
;;
;; A deck whose import already disagrees with its export is skipped rather than
;; failed. That is a fidelity question and `corpus.rkt` and `fidelity.rkt` are
;; where it is asked; here the question is what a session does to a deck the two
;; sides already agree on.
;;
;; The decks are not committed. Run `tools/fetch-corpus.sh` to get them; with no
;; corpus present this says so and passes.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-rhombus glide-pptx/export
         glide-pptx/sync
         "deck-edit.rkt" "ir-diff.rkt")

(define-runtime-path corpus-dir "corpus")

(define DECKS (string->number (or (getenv "GLIDE_DECK_COUNT") "8")))
(define ROUNDS (string->number (or (getenv "GLIDE_DECK_ROUNDS") "6")))
(define SEED (string->number (or (getenv "GLIDE_DECK_SEED") "20260905")))
;; One deck by name, which is how a failure here is looked at again.
(define ONLY (getenv "GLIDE_DECK_ONLY"))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-deck-sessions"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; A tag the deck does not know is what a copy looks like between the merge
;; naming it and the deck being written again.
(define (value-disagreements program deck)
  (filter (lambda (d) (not (regexp-match? #rx"is in the program and not the deck" d)))
          (disagreements program deck)))

;; What the program holds right now, as the editor would see it: one entry per
;; `at` form the canvas itself draws, with the slide it is on and enough about
;; it to know what can be done to it.
(struct spot (tag slide group? text?) #:transparent)

(define (program-spots path)
  (define text (file->string path))
  (define-values (sites scopes _slides _layout) (find-program-sites path))
  (define (slide-of st)
    (and scopes (let ([i (index-of scopes (at-site-scope st))]) (and i (add1 i)))))
  (for*/list ([st (in-list sites)]
              [whole (in-value (at-site-whole st))]
              #:when (and whole (slide-of st)
                          ;; Not a shape inside a group: the deck's editor
                          ;; cannot pick one of those out on its own.
                          (not (for/or ([o (in-list sites)])
                                 (and (not (eq? o st)) (at-site-whole o)
                                      (< (rng-start (at-site-whole o)) (rng-start whole))
                                      (< (rng-end whole) (rng-end (at-site-whole o))))))))
    (define src (substring text (rng-start whole) (rng-end whole)))
    (spot (at-site-tag st) (slide-of st)
          (regexp-match? #rx"group_pict[(]" src)
          (regexp-match? #rx"textbox[(]" src))))

;; One thing a person does, chosen for the deck in front of them. Returns #f
;; when there is nothing of that kind here to do it to, which is most of them
;; most of the time.
(define (do-something! deck spots slides n rng)
  (define (pick xs) (and (pair? xs) (list-ref xs (random (length xs) rng))))
  (define movable (filter (lambda (s) (not (spot-group? s))) spots))
  (define texts (filter spot-text? spots))
  (case (random 12 rng)
    [(0) (let ([s (pick spots)])
           (and s (drag-in-deck! deck (spot-slide s) (spot-tag s)
                                 (* 12700.0 (+ 20 (random 300 rng)))
                                 (* 12700.0 (+ 20 (random 200 rng))))))]
    [(1) (let ([s (pick movable)])
           (and s (resize-in-deck! deck (spot-slide s) (spot-tag s)
                                   (+ 40.0 (random 200 rng)) (+ 30.0 (random 150 rng)))))]
    [(2) (let ([s (pick movable)])
           (and s (rotate-in-deck! deck (spot-slide s) (spot-tag s) (random 360 rng))))]
    [(3) (let ([s (pick spots)]) (and s (bring-to-front! deck (spot-slide s) (spot-tag s))))]
    ;; With more than ASCII in it: positions are counted in characters, and a
    ;; splice counting bytes would land short for everything written after.
    [(4) (let ([s (pick texts)])
           (and s (retext-in-deck! deck (spot-slide s) (spot-tag s)
                                   (format "rewritten ~a — naïve 日本語" n))))]
    [(5) (let ([s (pick movable)])
           (and s (edit-after-tag! deck (spot-slide s) (spot-tag s)
                                   #px"<a:srgbClr val=\"[0-9A-Fa-f]{6}\"/>"
                                   "<a:srgbClr val=\"00B050\"/>")))]
    [(6) (let ([s (pick spots)]) (and s (delete-from-deck! deck (spot-slide s) (spot-tag s))))]
    [(7) (let ([s (pick movable)]) (and s (duplicate-in-deck! deck (spot-slide s) (spot-tag s))))]
    [(8) (let ([s (pick spots)])
           (and s (add-shape-to-deck! deck (spot-slide s) (format "Drawn ~a" n)) #t))]
    ;; Two on one slide, neither of them a group and neither of them named the
    ;; same as the other.
    [(9) (let* ([here (and (pair? movable)
                           (let ([s (pick movable)])
                             (filter (lambda (o) (equal? (spot-slide o) (spot-slide s)))
                                     movable)))])
           (and here (> (length here) 1)
                (not (equal? (spot-tag (first here)) (spot-tag (second here))))
                (group-in-deck! deck (spot-slide (first here))
                                (spot-tag (first here)) (spot-tag (second here))
                                #:name (format "Grouped ~a" n))))]
    [(10) (and (> slides 1)
               (let ([a (add1 (random slides rng))] [b (add1 (random slides rng))])
                 (and (not (= a b)) (move-slide! deck a b))))]
    ;; Not one that is hidden already: the attribute would be written twice,
    ;; which is a deck no editor would save.
    [(11) (let ([s (pick spots)])
            (and s (edit-slide-part! deck (spot-slide s)
                                     #px"<p:sld (?![^>]*show=)([^>]*)>"
                                     "<p:sld \\1 show=\"0\">")))]
    [else #f]))

(define all-decks
  (if (directory-exists? corpus-dir)
      (sort (filter (lambda (p) (regexp-match? #rx"[.]pptx$" (path->string p)))
                    (directory-list corpus-dir))
            string<? #:key path->string)
      '()))

;; Sets one up the way `raco glide` does: translate the deck to a program, write
;; the deck back out from the program, and record what the two agree on. From
;; then on the program is the source and the deck is what the editor holds.
(define (lay-out! d dir)
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (define imported (pptx->deck (build-path corpus-dir d) #:workdir (build-path dir "u")))
  (write-rhombus-deck imported program #:source-name (path->string d))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (values program deck))

(cond
  [(null? all-decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   (define rng (make-pseudo-random-generator))
   (parameterize ([current-pseudo-random-generator rng]) (random-seed SEED))
   (current-pseudo-random-generator rng)
   (define order
     (if ONLY
         (filter (lambda (d) (equal? ONLY (path->string d))) all-decks)
         (shuffle all-decks)))
   (define used 0)
   (define skipped 0)
   (define rounds-total 0)
   (define applied-total 0)
   (define refused-total 0)

   (for ([d (in-list order)] #:break (>= used DECKS))
     (define name (path->string d))
     (define dir (build-path work (regexp-replace #rx"[.]pptx$" name "")))
     ;; A deck this cannot translate, or one whose two sides do not start out
     ;; agreeing, is not what is being asked about here.
     (define ready
       (with-handlers ([exn:fail? (lambda (_e) #f)])
         (define-values (program deck) (lay-out! d dir))
         (and (pair? (program-spots program))
              (null? (value-disagreements program deck))
              (list program deck))))
     (cond
       [(not ready) (set! skipped (add1 skipped))]
       [else
        (set! used (add1 used))
        (define program (first ready))
        (define deck (second ready))
        (define workdir (build-path dir "w"))
        ;; Its own stream, from its own name, so which decks came before it
        ;; makes no difference to what is done to it: a failure here is looked
        ;; at again with GLIDE_DECK_ONLY=<name> and nothing else.
        (define drng (make-pseudo-random-generator))
        (parameterize ([current-pseudo-random-generator drng])
          (random-seed (modulo (+ SEED (abs (equal-hash-code name))) (expt 2 31))))
        (for ([n (in-range ROUNDS)])
          (define spots (program-spots program))
          (define slides (length (remove-duplicates (map spot-slide spots))))
          (define did
            (for/list ([_ (in-range (add1 (random 3 drng)))]
                       #:when (with-handlers ([exn:fail? (lambda (_e) #f)])
                                (and (do-something! deck spots slides n drng) #t)))
              #t))
          (set! rounds-total (add1 rounds-total))
          (with-check-info (['deck name] ['round n])
            (unless (null? did)
              (define before (file->string program))
              ;; Anything raised anywhere in the round is this round's failure
              ;; and not the end of the run: there are two dozen more decks
              ;; behind this one, and each of them has something to say.
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (fail (format "~a round ~a: ~a" name n
                                               (first (string-split (exn-message e) "\n")))))])
              (define r (sync-once program deck #:workdir workdir #:atomic? #t))
              (when r
                (cond
                  [(pair? (sync-report-skipped r))
                   (set! refused-total (add1 refused-total))
                   (check-equal? (sync-report-applied r) '()
                                 (format "~a round ~a: a refused save wrote something" name n))
                   (check-equal? (file->string program) before
                                 (format "~a round ~a: a refused save changed the program" name n))
                   ;; Nothing here can get past that one, so the program wins and
                   ;; the session carries on.
                   (picts->pptx (load-program-picts program) deck)
                   (void (sync-once program deck #:workdir workdir))]
                  [else
                   (set! applied-total (+ applied-total (length (sync-report-applied r))))
                   (define (settled? report)
                     (null? (filter (lambda (x) (not (eq? 'noted (sync-action-kind x))))
                                    (sync-report-actions report))))
                   (let loop ([k 1])
                     (define again (sync-once program deck #:workdir workdir #:atomic? #t))
                     (cond
                       [(settled? again) (void)]
                       [(>= k 3)
                        (fail (format "~a round ~a: still reporting ~s after ~a passes" name n
                                      (map sync-action-kind (sync-report-actions again)) k))]
                       [(and (null? (sync-report-applied again))
                             (pair? (sync-report-skipped again)))
                        (void)]
                       [else (loop (add1 k))]))
                   (when (null? (sync-report-notes r))
                     (check-equal? (value-disagreements program deck) '()
                                   (format "~a round ~a: the program and the deck disagree"
                                           name n)))
                   (picts->pptx (load-program-picts program) deck)
                   (void (sync-once program deck #:workdir workdir))]))))))
        ;; Whatever the session did to it, it is still a program glide can read.
        (with-check-info (['deck name])
          (define picts
            (with-handlers ([exn:fail? (lambda (e)
                                         (fail (format "~a: the program no longer loads: ~a" name
                                                       (first (string-split (exn-message e) "\n"))))
                                         '())])
              (load-program-picts program)))
          (check-true (pair? picts) (format "~a: it still has slides" name)))]))

   (printf "deck session tests done; ~a rounds over ~a real decks (~a passed over) -- ~a edits written, ~a saves refused\n"
           rounds-total used skipped applied-total refused-total)])

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
