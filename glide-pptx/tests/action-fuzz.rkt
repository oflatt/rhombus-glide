#lang racket/base
;; Random combinations of edits, against whatever program it is handed.
;;
;; `sessions.rkt` deals a fixed catalogue over one hand-written two-slide
;; program: every edit names a tag that program is known to hold. That found a
;; great deal, and it cannot find anything about a deck it has never seen. The
;; shapes that break the merge live in real files -- a picture that states its
;; size positionally, a group whose child hangs outside it, a slide built from
;; stages -- and in a talk that has been rewritten by hand for months.
;;
;; So the edits here discover their own targets: the program says which of its
;; elements have an `at` form and what that form holds, an edit is chosen that
;; the element could plausibly receive, and it is applied to the deck by the
;; same XML surgery an editor would end up performing.
;;
;; What must hold, however the cards fall:
;;
;;   * A save lands whole or not at all. Refused, the program is byte for byte
;;     what it was.
;;   * A save that lands settles: exporting again and looking has nothing to say.
;;   * The program still loads afterwards, and still holds every slide.
;;   * An edit the program has a literal for is not refused. This is the one
;;     that says "if it seems reasonable to edit, it works": a refusal here is
;;     a reason to go and make it writable.
;;
;; Seeded. `GLIDE_FUZZ_SEED`, `GLIDE_FUZZ_ROUNDS` and `GLIDE_FUZZ_EDITS` move
;; the sweep; `GLIDE_FUZZ_PROGRAM` points it at one program -- a talk, say --
;; instead of the fixtures, and `GLIDE_FUZZ_CORPUS` at a slice of the corpus.
;;
;; A talk is not in the repository, because it is not ours. Drop one in
;; `tests/programs/local/`, which is not tracked, and it is fuzzed along with
;; the fixtures: a talk that has been rewritten by hand for months is where the
;; shapes the merge has never seen actually live, and it is the one program
;; whose editability anybody is going to complain about.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         glide-pptx/sync glide-pptx/export glide-pptx/parse
         glide-pptx/emit-rhombus racket/runtime-path
         "deck-edit.rkt")

(define-runtime-path decks-dir "decks")
(define-runtime-path corpus-dir "corpus")
(define-runtime-path local-dir "programs/local")

(define ROUNDS (string->number (or (getenv "GLIDE_FUZZ_ROUNDS") "4")))
(define EDITS (string->number (or (getenv "GLIDE_FUZZ_EDITS") "3")))
(define BASE-SEED (string->number (or (getenv "GLIDE_FUZZ_SEED") "20260907")))
(define ONE-PROGRAM (getenv "GLIDE_FUZZ_PROGRAM"))

(define reasons (make-hash))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-action-fuzz"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; ------------------------------------------------------------------- targets

;; What an edit needs to know about one element: where it is, and which of its
;; properties the program holds as a literal. `writable?` is what makes a
;; refusal a finding rather than a fact of life.
(struct target (slide tag site) #:transparent)

;; The elements the merge could act on, with the slide each is on. A site's
;; scope names the slide's definition; the deck's slide numbers are the order
;; those definitions are shown in, which `all_slides` states.
(define (targets-of program)
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (define order (or scopes '()))
  (define index-of
    (for/hash ([s (in-list order)] [i (in-naturals 1)] #:when s) (values s i)))
  ;; A tag naming two forms on one slide cannot be acted on at all, and that is
  ;; the program's own doing rather than something to report here.
  (define shared
    (for/fold ([h (hash)]) ([s (in-list sites)])
      (hash-update h (cons (at-site-scope s) (at-site-tag s)) add1 0)))
  (for/list ([s (in-list sites)]
             #:when (and (hash-ref index-of (at-site-scope s) #f)
                         (= 1 (hash-ref shared (cons (at-site-scope s) (at-site-tag s)) 0))))
    (target (hash-ref index-of (at-site-scope s)) (at-site-tag s) s)))

;; ------------------------------------------------------------------- edits

;; An edit is a name, a predicate saying which elements can receive it, and a
;; procedure that performs it on the deck. `writable?` says whether the program
;; holds what the edit would have to rewrite.
;; `action` is the kind of action the merge should read this edit as, so a
;; refusal can be traced back to the edit that asked for it rather than to
;; whatever else happened to name the same element.
(struct edit-kind (name action fits? writable? do!) #:transparent)

(define (jitter rng lo hi) (+ lo (* (- hi lo) (random rng))))

(define KINDS
  (list
   (edit-kind "drag" 'moved
              (lambda (t) #t)
              (lambda (t) (and (at-site-x (target-site t)) (at-site-y (target-site t)) #t))
              (lambda (deck t rng)
                (drag-in-deck! deck (target-slide t) (target-tag t)
                               (jitter rng 20.0 400.0) (jitter rng 20.0 300.0))))
   (edit-kind "resize" 'resized
              (lambda (t) #t)
              (lambda (t) (and (at-site-width (target-site t))
                               (at-site-height (target-site t)) #t))
              (lambda (deck t rng)
                (resize-in-deck! deck (target-slide t) (target-tag t)
                                 (jitter rng 40.0 300.0) (jitter rng 30.0 200.0))))
   (edit-kind "rotate" 'moved
              (lambda (t) #t)
              (lambda (t) (or (at-site-rot (target-site t))
                              (and (at-site-insert-at (target-site t)) #t)))
              (lambda (deck t rng)
                (rotate-in-deck! deck (target-slide t) (target-tag t)
                                 (round (jitter rng 5.0 350.0)))))
   (edit-kind "retype" 'retext
              (lambda (t) (pair? (at-site-texts (target-site t))))
              ;; Only where the text is one run. Replacing the whole of a body
              ;; written as several is a retyping that crosses them, and which
              ;; run it belonged to is a guess -- the merge says so and refuses,
              ;; and it is right to.
              (lambda (t) (= 1 (length (at-site-texts (target-site t)))))
              (lambda (deck t rng)
                (retext-in-deck! deck (target-slide t) (target-tag t)
                                 (format "fuzzed ~a" (random 1000 rng)))))
   (edit-kind "recolour" 'restyle
              (lambda (t) #t)
              (lambda (t) #f)   ; a fill may or may not be a literal; not a finding
              (lambda (deck t rng)
                (edit-after-tag! deck (target-slide t) (target-tag t)
                                 #px"<a:srgbClr val=\"[0-9A-Fa-f]{6}\"/>"
                                 (format "<a:srgbClr val=\"~a\"/>"
                                         (string-upcase
                                          (~r (random 16777215 rng) #:base 16 #:min-width 6
                                              #:pad-string "0"))))))
   (edit-kind "bring to front" 'restacked
              (lambda (t) #t)
              (lambda (t) #f)
              (lambda (deck t rng) (bring-to-front! deck (target-slide t) (target-tag t))))
   (edit-kind "delete" 'removed
              (lambda (t) #t)
              (lambda (t) #f)
              (lambda (deck t rng) (delete-from-deck! deck (target-slide t) (target-tag t))))
   (edit-kind "duplicate" 'added
              (lambda (t) #t)
              (lambda (t) #f)
              (lambda (deck t rng) (duplicate-in-deck! deck (target-slide t) (target-tag t))))))

;; ------------------------------------------------------------------ the run

;; One program, dealt `ROUNDS` handfuls of edits.
(define (fuzz-program! label program dir seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
  (current-pseudo-random-generator rng)
  (define deck (build-path dir "deck.pptx"))
  (define workdir (build-path dir "w"))
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (define picts (load-program-picts program))
  (picts->pptx picts deck)
  (void (sync-once program deck #:workdir workdir))
  (define slides (length picts))
  (define all-targets (targets-of program))
  (define applied-total 0)
  (define refused-total 0)
  (define findings '())
  (when (pair? all-targets)
    (for ([n (in-range ROUNDS)])
      (define picked
        (for/list ([_ (in-range (add1 (random EDITS rng)))])
          (define t (list-ref all-targets (random (length all-targets) rng)))
          (define fits (filter (lambda (k) ((edit-kind-fits? k) t)) KINDS))
          (cons t (list-ref fits (random (length fits) rng)))))
      (define landed
        (for/list ([p (in-list picked)]
                   #:when (with-handlers ([exn:fail? (lambda (_e) #f)])
                            (and ((edit-kind-do! (cdr p)) deck (car p) rng) #t)))
          p))
      (unless (null? landed)
        (define names (for/list ([p (in-list landed)])
                        (format "~a ~s" (edit-kind-name (cdr p)) (target-tag (car p)))))
        (with-check-info (['program label] ['seed seed] ['round n] ['edits names])
          (define before (file->string program))
          (define r
            (with-handlers ([exn:fail? (lambda (e)
                                         (fail (format "~a seed ~a round ~a: ~a" label seed n
                                                       (first (string-split (exn-message e) "\n"))))
                                         #f)])
              (sync-once program deck #:workdir workdir #:atomic? #t)))
          (when r
            (define skipped (sync-report-skipped r))
            ;; Refused means refused: a base is recorded whenever the save
            ;; landed, and a handful whose every difference was one the source
            ;; has no place for lands with nothing written rather than refusing.
            (define refused? (not (sync-report-base-written? r)))
            (cond
              [refused?
               (set! refused-total (add1 refused-total))
               ;; Refused, so nothing was written.
               (check-equal? (file->string program) before
                             (format "~a: a refused save leaves the program alone" label))]
              [else
               (set! applied-total (+ applied-total (length (sync-report-applied r))))
               ;; It still loads, and still has every slide.
               (define after-picts
                 (with-handlers ([exn:fail? (lambda (e)
                                              (fail (format "~a: the program no longer loads: ~a"
                                                            label
                                                            (first (string-split (exn-message e) "\n"))))
                                              #f)])
                   (load-program-picts program)))
               (when after-picts
                 (check-equal? (length after-picts) slides
                               (format "~a: every slide is still there" label))
                 ;; And it settles: written again, there is nothing more to say.
                 (picts->pptx after-picts deck)
                 (define again (sync-once program deck #:workdir workdir #:dry-run? #t))
                 (check-equal? (for/list ([a (in-list (sync-report-actions again))])
                                 (format "~a ~s" (sync-action-kind a) (sync-action-tag a)))
                               '()
                               (format "~a: the save settles" label)))])
            ;; Every reason anything was refused, tallied. Not a failure --
            ;; some of these are facts about the program rather than about the
            ;; merge -- but the list is what says which of them to go and fix.
            (for ([sk (in-list skipped)])
              (hash-update! reasons (cdr sk) add1 0))
            ;; An edit the program holds a literal for should not be refused.
            ;; Collected rather than failed: this is the list to work through.
            (for ([sk (in-list skipped)] #:when refused?)
              (define a (car sk))
              (define t (for/first ([p (in-list landed)]
                                    #:when (and (equal? (target-tag (car p)) (sync-action-tag a))
                                                (equal? (edit-kind-action (cdr p))
                                                        (sync-action-kind a))))
                          p))
              (when (and t ((edit-kind-writable? (cdr t)) (car t)))
                (set! findings
                      (cons (format "~a: ~a ~s refused -- ~a" label
                                    (edit-kind-name (cdr t)) (sync-action-tag a) (cdr sk))
                            findings)))))))))
  ;; Outside the `when`: a deck with nothing the merge can act on has nothing
  ;; to report, and still owes its caller three answers.
  (values applied-total refused-total (reverse findings)))

;; --------------------------------------------------------------- what to run

;; `GLIDE_FUZZ_CORPUS` runs it over the corpus instead -- five hundred real
;; decks rather than six fixtures, which is where the shapes the merge has never
;; seen live. `GLIDE_FUZZ_CORPUS_SKIP` moves the slice along.
(define CORPUS (string->number (or (getenv "GLIDE_FUZZ_CORPUS") "0")))
(define CORPUS-SKIP (string->number (or (getenv "GLIDE_FUZZ_CORPUS_SKIP") "0")))

;; The fixture decks, translated: real files, and the only ones committed here.
(define fixtures
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

(define applied 0)
(define refused 0)
(define all-findings '())

;; A talk of one's own, if there is one beside the tests. Its own folder is
;; copied into the scratch first: a test does not edit somebody's talk.
(define (local-programs)
  (if (directory-exists? local-dir)
      (sort (for/list ([f (in-list (directory-list local-dir #:build? #t))]
                       #:when (regexp-match? #rx"[.]rhm$" (path->string f)))
              f)
            string<? #:key path->string)
      '()))

;; Everything beside the program comes with it: helper modules it imports,
;; images it names, fonts it checks for.
(define (copy-beside! program dir)
  (define from (path-only (path->complete-path program)))
  (for ([f (in-list (directory-list from))])
    (define p (build-path from f))
    (cond [(directory-exists? p)
           (copy-directory/files p (build-path dir f) #:keep-modify-seconds? #t)]
          [else (copy-file p (build-path dir f) #t)]))
  (build-path dir (file-name-from-path program)))

(define (run! label program dir seed)
  (define-values (a r fs) (fuzz-program! label program dir seed))
  (set! applied (+ applied a))
  (set! refused (+ refused r))
  (set! all-findings (append all-findings fs)))

(cond
  [ONE-PROGRAM
   (define dir (build-path work "one"))
   (make-directory* dir)
   (run! (path->string (file-name-from-path ONE-PROGRAM))
         (copy-beside! ONE-PROGRAM dir) dir BASE-SEED)]
  [(positive? CORPUS)
   (define all
     (if (directory-exists? corpus-dir)
         (sort (for/list ([f (in-list (directory-list corpus-dir))]
                          #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
                 (path->string f))
               string<?)
         '()))
   (cond
     [(null? all) (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
     [else
      (current-allow-unsupported? #t)
      (for ([name (in-list (take (drop all (min CORPUS-SKIP (length all)))
                                 (min CORPUS (max 0 (- (length all) CORPUS-SKIP)))))]
            [i (in-naturals)])
        (define dir (build-path work (format "c~a" i)))
        (with-handlers
            ([(lambda (_e) #t)
              (lambda (e)
                (define msg (first (string-split (exn-message e) "\n")))
                ;; A deck we refuse in our own words, or one whose fonts are not
                ;; here, is not a finding about the merge.
                (unless (regexp-match? #rx"^glide[-a-z]*:" msg)
                  (fail (format "~a: ~a" name msg))))])
          (make-directory* dir)
          (define program (build-path dir "p.rhm"))
          (define d (pptx->deck (build-path corpus-dir name) #:workdir (build-path dir "u")))
          (write-rhombus-deck d program #:source-name name)
          (run! name program dir (+ BASE-SEED i))))])]
  [else
   (for ([name (in-list fixtures)] [i (in-naturals)])
     (define dir (build-path work name))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                           #:workdir (build-path dir "u")))
     (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
     (run! name program dir (+ BASE-SEED i)))
   ;; And a talk of one's own, when there is one.
   (define mine (local-programs))
   (if (null? mine)
       (printf "no talk in tests/programs/local; only the fixtures were fuzzed\n")
       (for ([p (in-list mine)] [i (in-naturals)])
         (define dir (build-path work (format "local~a" i)))
         (make-directory* dir)
         (run! (path->string (file-name-from-path p))
               (copy-beside! p dir) dir (+ BASE-SEED 100 i))))])

(unless (zero? (hash-count reasons))
  (printf "\nwhy saves were refused:\n")
  (for ([p (in-list (sort (hash->list reasons) > #:key cdr))])
    (printf "  ~a x  ~a\n" (cdr p) (car p))))
(unless (null? all-findings)
  (printf "\nedits the program has a literal for and the merge refused:\n")
  (for ([f (in-list (remove-duplicates all-findings))]) (printf "  ~a\n" f)))
(printf "action fuzz done; ~a edits written, ~a saves refused, ~a findings\n"
        applied refused (length (remove-duplicates all-findings)))

(module+ main (void (test-log #:display? #t #:exit? #t)))
