#lang racket/base
;; Does a translated deck agree with the deck it writes? Over the corpus.
;;
;; `structural.rkt` asks this of six fixtures. Five hundred real files ask it of
;; everything the fixtures never thought of, and on the first run it found four
;; things: a table had no state at all, so a deck with a table could not be
;; synced; a picture cropped by nothing disagreed with a picture not cropped, on
;; fifteen decks; a numbered bullet lost its typeface; and a picture whose blip
;; resolved to nothing was written as `media("missing.png")` into a program with
;; no `media` defined, which then did not load.
;;
;; The property is exact. Anything the merge says is a difference between what
;; the program means and what the deck holds, and the next real edit is merged
;; on top of it.
;;
;; The base is recorded by a pass that has none. That pass writes only the base
;; and never touches the program -- and without it every later pass reports
;; nothing whatever the deck holds, which is a test that cannot fail.
;;
;; Swept in slices, each in its own process. Reading a Rhombus program costs
;; about ten megabytes that is never given back -- any module outside the four
;; the loader attaches is instantiated afresh and something global keeps it --
;; so five hundred of them in one process reaches ten gigabytes and thrashes.
;; A slice per process bounds that, and the slices are what the driver below
;; runs and adds up.
;;
;; The decks are not committed -- they belong to LibreOffice and POI. Run
;; `tools/fetch-corpus.sh` to get them; with no corpus present this says so and
;; passes. `GLIDE_AGREE_N` and `GLIDE_AGREE_SKIP` cut it down to a slice while
;; working on one, and `GLIDE_AGREE_SLICE` is how the driver asks for one.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path racket/system racket/port
         glide-pptx/ir glide-pptx/parse glide-pptx/export glide-pptx/sync
         glide-pptx/emit-rhombus)

(define-runtime-path corpus-dir "corpus")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-agreement"))

(define all
  (if (directory-exists? corpus-dir)
      (sort (for/list ([f (in-list (directory-list corpus-dir))]
                       #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
              (path->string f))
            string<?)
      '()))

(define N (string->number (or (getenv "GLIDE_AGREE_N") "10000")))
(define SKIP (string->number (or (getenv "GLIDE_AGREE_SKIP") "0")))
(define decks (take (drop all (min SKIP (length all)))
                    (min N (max 0 (- (length all) SKIP)))))

;; How many decks one process takes before it is asked to stop. Forty is under
;; a gigabyte of the leak above and about four minutes.
(define SLICE 40)
(define slice? (and (getenv "GLIDE_AGREE_SLICE") #t))

;; `exec-file` is however racket was invoked, which is a bare name when it came
;; off the PATH -- and `system*` cannot exec one of those.
(define racket-exe
  (let ([e (find-system-path 'exec-file)])
    (if (absolute-path? e) e (or (find-executable-path e) e))))

(define-runtime-path here "agreement.rkt")

;; One slice, in its own process. Its own output is passed through, so a deck it
;; disagrees about is named where it happened; its last line is the tally, which
;; is read back and added up.
(define (run-slice! skip n)
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port out]
                   [current-environment-variables
                    (environment-variables-copy (current-environment-variables))])
      (putenv "GLIDE_AGREE_SLICE" "1")
      (putenv "GLIDE_AGREE_SKIP" (number->string skip))
      (putenv "GLIDE_AGREE_N" (number->string n))
      (system* racket-exe (path->string here))))
  (define text (get-output-string out))
  (for ([l (in-list (string-split text "\n"))]
        #:unless (regexp-match? #px"^agreement over" l))
    (printf "~a\n" l))
  (flush-output)
  (define m (regexp-match #px"agreement over ([0-9]+) decks: ([0-9]+) agreed, ([0-9]+) disagreed, ([0-9]+) refused, ([0-9]+) needed a font" text))
  (cond
    [m (map string->number (cdr m))]
    [else (check-true #f (format "a slice from ~a said nothing it could be added up from" skip))
          (list n 0 0 0 0)]))

;; What still disagrees, named so it stays visible rather than tolerated. Each
;; is a bug; they are listed so the sweep can guard the other five hundred in
;; the meantime.
;; Nothing, at present: every deck in the corpus that can be measured agrees
;; with the deck it writes. A deck listed here would be a bug being guarded
;; against rather than tolerated, so the sweep can keep watch over the other
;; four hundred in the meantime.
(define known (hash))

(cond
  [(null? decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  ;; The driver: slices, each in its own process, added up.
  [(and (not slice?) (> (length decks) SLICE))
   (define totals
     (for/fold ([acc (list 0 0 0 0 0)])
               ([start (in-range 0 (length decks) SLICE)])
       (define got (run-slice! (+ SKIP start) (min SLICE (- (length decks) start))))
       (map + acc got)))
   (printf "agreement over ~a decks: ~a agreed, ~a disagreed, ~a refused, ~a needed a font\n"
           (first totals) (second totals) (third totals) (fourth totals) (fifth totals))
   (check-equal? (third totals) 0 "every deck that can be measured agrees with its own export")]
  [else
   (current-allow-unsupported? #t)
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (define agreed 0)
   (define fontless 0)
   (define refused 0)
   (define surprises '())
   (for ([name (in-list decks)] [i (in-naturals 1)])
     (define dir (build-path work (format "d~a" i)))
     (define (note! what) (set! surprises (cons (cons name what) surprises)))
     (with-handlers
         ([(lambda (_e) #t)
           (lambda (e)
             (define msg (first (string-split (exn-message e) "\n")))
             (cond
               ;; The generated program checks its fonts and will not run on
               ;; substitutes, which is its own doing and not a disagreement.
               [(regexp-match? #rx"required font is not installed" msg)
                (set! fontless (add1 fontless))]
               ;; A file we refuse in our own words is a file we refuse. Some of
               ;; these are deliberately corrupt -- POI keeps its fuzzer's
               ;; findings here.
               [(regexp-match? #rx"^glide[-a-z]*:" msg) (set! refused (add1 refused))]
               [else (note! (format "raised: ~a" msg))]))])
       (make-directory* dir)
       (define program (build-path dir "p.rhm"))
       (define pptx (build-path dir "out.pptx"))
       (define d (pptx->deck (build-path corpus-dir name) #:workdir (build-path dir "u")))
       (write-rhombus-deck d program #:source-name name)
       (picts->pptx (load-program-picts program) pptx)
       (define first-pass (sync-once program pptx #:workdir (build-path dir "w")))
       (unless (sync-report-base-written? first-pass)
         (note! "the first pass recorded no base"))
       (define r (sync-once program pptx #:workdir (build-path dir "w") #:dry-run? #t))
       (define as (sync-report-actions r))
       (cond
         [(null? as) (set! agreed (add1 agreed))]
         [else
          (note! (string-join
                  (remove-duplicates
                   (for/list ([a (in-list as)])
                     (format "~a ~s" (sync-action-kind a) (sync-action-tag a))))
                  ", "))])))
   ;; Reported deck by deck, so a new one is named rather than buried in a count.
   (for ([s (in-list (reverse surprises))])
     (cond
       [(hash-ref known (car s) #f)
        => (lambda (why) (printf "  known: ~a -- ~a\n" (car s) why))]
       [else (check-equal? (cdr s) '() (format "~a: nothing to merge" (car s)))]))
   (printf "agreement over ~a decks: ~a agreed, ~a disagreed, ~a refused, ~a needed a font\n"
           (length decks) agreed
           (length (filter (lambda (s) (not (hash-ref known (car s) #f))) surprises))
           refused fontless)])

(module+ main (void (test-log #:display? #t #:exit? #t)))
