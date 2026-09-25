#lang racket/base
;; Every piece of text in a program, retyped.
;;
;; "If it seems reasonable to edit, it should work" is most obviously true of
;; text: somebody clicks into a box in the editor, changes a word, and expects
;; the string in the source to change. There is no reason for that to fail on
;; one text box and not another, so this asks it of all of them at once and
;; names the ones that did not.
;;
;; One run at a time, because that is what changing a word is. Putting the whole
;; body in the first run is a retyping that crosses runs, and which run it
;; belonged to is a guess the merge refuses to make -- rightly, and separately
;; tested.
;;
;; All of them in one save rather than one save each: a talk has a couple of
;; hundred strings, and reading the program back for each would be an hour. What
;; is checked is that every new string is in the source afterwards, which is the
;; same question.
;;
;; `GLIDE_RETYPE_PROGRAM` points it at one program -- a talk -- and a talk
;; dropped in `tests/programs/local` is swept along with the fixtures.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/sync glide-pptx/export glide-pptx/parse glide-pptx/emit-rhombus
         "deck-edit.rkt")

(define-runtime-path decks-dir "decks")
(define-runtime-path local-dir "programs/local")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-retype-all"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Which slide each of a program's tagged `at` forms is on, and how many runs it
;; holds. The runs come from the source rather than the deck, because the source
;; is what an edit has to land in: a body written as two `run`s has two places a
;; word could be changed.
(struct spot (slide tag runs) #:transparent)

(define (spots-of program)
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (define index-of
    (for/hash ([s (in-list (or scopes '()))] [i (in-naturals 1)] #:when s) (values s i)))
  ;; A tag naming two forms on one slide cannot be acted on at all, which is the
  ;; program's own doing and tested elsewhere.
  (define shared
    (for/fold ([h (hash)]) ([s (in-list sites)])
      (hash-update h (cons (at-site-scope s) (at-site-tag s)) add1 0)))
  (for/list ([s (in-list sites)]
             #:when (and (pair? (at-site-texts s))
                         (hash-ref index-of (at-site-scope s) #f)
                         (= 1 (hash-ref shared (cons (at-site-scope s) (at-site-tag s)) 0))))
    (spot (hash-ref index-of (at-site-scope s)) (at-site-tag s)
          (length (at-site-texts s)))))

;; The word put in each place, unique so that finding it in the source proves it
;; was that edit and not another.
(define (retyped-to n) (format "retyped~a" n))

(define (retype-everything! label program dir)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (define spots (spots-of program))
  (cond
    [(null? spots) (printf "  ~a: no text to retype\n" label)]
    [else
     ;; Dealt out with a number each, and only the ones the editor could find.
     (define wanted
       (for/list ([sp (in-list spots)] [n (in-naturals 1)]
                  #:when (with-handlers ([exn:fail? (lambda (_e) #f)])
                           (retype-run-in-deck! deck (spot-slide sp) (spot-tag sp)
                                                1 (retyped-to n))))
         (cons sp (retyped-to n))))
     (define r (sync-once program deck #:workdir w #:atomic? #t))
     (define text (file->string program))
     (define missing
       (for/list ([p (in-list wanted)]
                  #:unless (regexp-match? (regexp (regexp-quote (cdr p))) text))
         (format "~s on slide ~a" (spot-tag (car p)) (spot-slide (car p)))))
     (printf "  ~a: ~a of ~a strings retyped~a\n" label
             (- (length wanted) (length missing)) (length wanted)
             (if (null? missing) "" (format ", ~a not written" (length missing))))
     (for ([why (in-list (remove-duplicates
                          (for/list ([sk (in-list (sync-report-skipped r))]) (cdr sk))))])
       (printf "     refused: ~a\n" why))
     (check-equal? missing '()
                   (format "~a: every string that was retyped is in the program" label))]))

(define fixtures
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

(define (copy-beside! program dir)
  (define from (path-only (path->complete-path program)))
  (for ([f (in-list (directory-list from))])
    (define p (build-path from f))
    (cond [(directory-exists? p)
           (copy-directory/files p (build-path dir f) #:keep-modify-seconds? #t)]
          [else (copy-file p (build-path dir f) #t)]))
  (build-path dir (file-name-from-path program)))

(printf "retyping every string:\n")
(for ([name (in-list fixtures)])
  (define dir (build-path work name))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                        #:workdir (build-path dir "u")))
  (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
  (retype-everything! name program dir))

;; And a talk of one's own, named or dropped beside the tests.
(define mine
  (append (let ([named (getenv "GLIDE_RETYPE_PROGRAM")])
            (if named (list (string->path named)) '()))
          (if (directory-exists? local-dir)
              (sort (for/list ([f (in-list (directory-list local-dir #:build? #t))]
                               #:when (regexp-match? #rx"[.]rhm$" (path->string f)))
                      f)
                    string<? #:key path->string)
              '())))

(if (null? mine)
    (printf "  no talk of your own; drop one in tests/programs/local to sweep it\n")
    (for ([p (in-list mine)] [i (in-naturals)])
      (define dir (build-path work (format "local~a" i)))
      (make-directory* dir)
      (retype-everything! (path->string (file-name-from-path p))
                          (copy-beside! p dir) dir)))

(module+ main (void (test-log #:display? #t #:exit? #t)))
