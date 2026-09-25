#lang racket/base
;; Random decks, checked against an oracle that needs no reference renderer.
;;
;; The schema is completely specified, so a generator can stay inside it: every
;; preset geometry with random adjustments, fills, strokes with dashes and ends,
;; text with runs and paragraphs, groups, rotations and flips. What is *not*
;; specified is how any of it should be laid out -- text especially -- so the
;; property checked here is not "does it look right" but "does it survive":
;;
;;   * writing a deck and reading it back gives the same deck, and
;;   * every stage of the pipeline runs without raising.
;;
;; That oracle is exact and cheap, which is what makes fuzzing worth doing: a
;; whole-slide pixel diff has a tolerance, and this does not.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/math
         racket/format
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/geometry glide-pptx/sync-state
         (only-in glide-pptx/drawing dash-like)
         "ir-diff.rkt")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-fuzz"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Seeded, so a failure names the case that produced it: re-run one with
;; `GLIDE_FUZZ_SEED=<n> GLIDE_FUZZ_ROUNDS=1`. The default is a number of rounds
;; that keeps the suite quick; widen it when hunting.
(define ROUNDS (string->number (or (getenv "GLIDE_FUZZ_ROUNDS") "150")))
(define BASE-SEED (string->number (or (getenv "GLIDE_FUZZ_SEED") "20260904")))

(define (pick rng . xs) (list-ref xs (random (length xs) rng)))
(define (chance rng p) (< (random rng) p))
(define (real-in rng lo hi) (+ lo (* (- hi lo) (random rng))))

(define (a-color rng)
  (rgba (random 256 rng) (random 256 rng) (random 256 rng) 1.0))

(define (a-fill rng)
  (cond
    [(chance rng 0.15) #f]
    [(chance rng 0.2)
     (gradient-fill (list (cons 0.0 (a-color rng)) (cons 1.0 (a-color rng)))
                    (real-in rng 0.0 360.0))]
    [else (solid-fill (a-color rng))]))

(define (an-end rng)
  (and (chance rng 0.4)
       (line-end (pick rng 'arrow 'triangle 'stealth 'diamond 'oval)
                 (pick rng "sm" "med" "lg") (pick rng "sm" "med" "lg"))))

;; A line whose dash and spelled-out pattern agree, which is the only kind a
;; file produces: `custDash` and `prstDash` are alternatives in the schema, so
;; a deck states one of them and the reader names the other from it. Generating
;; the two independently made a stroke no file could hold, and the round trip
;; normalized it -- which is a difference in the generator, not in the code.
(define (a-line rng)
  (cond
    [(chance rng 0.25) #f]
    [else
     (define pattern
       (and (chance rng 0.2)
            (list (cons (real-in rng 50.0 600.0) (real-in rng 50.0 600.0)))))
     (stroke (a-color rng) (real-in rng 0.25 8.0)
             (if pattern
                 (dash-like pattern)
                 (pick rng 'solid 'dash 'dot 'dash-dot 'short-dash))
             (pick rng 'flat 'round 'projecting)
             (an-end rng) (an-end rng)
             pattern)]))

(define (a-run rng)
  (trun (pick rng "Ag" "hello world" "x" "a longer piece of text to lay out" "σʹ")
        (pick rng "Arial" "Liberation Sans" "PT Mono")
        (real-in rng 8.0 60.0)
        (chance rng 0.3) (chance rng 0.2) (chance rng 0.1) (chance rng 0.1)
        (a-color rng) (real-in rng -2.0 2.0) 'none 0.0
        ;; A generated run states everything, the way a program's own does.
        'all))

(define (a-body rng)
  (and (chance rng 0.5)
       (text-body (for/list ([_ (in-range (add1 (random 2 rng)))])
                    ;; runs align level margin-left indent line-spacing
                    ;; space-before space-after bullet
                    (para (for/list ([_ (in-range (add1 (random 2 rng)))]) (a-run rng))
                          (pick rng 'left 'center 'right)
                          0 0.0 0.0
                          (cons 'percent (real-in rng 0.7 1.6))
                          (real-in rng 0.0 12.0) 0.0
                          (if (chance rng 0.3)
                              (bullet 'char "\u2022" "Arial" 1.0 #f)
                              no-bullet)
                          'all))
                  (pick rng 'top 'center 'bottom)
                  (chance rng 0.3)
                  (chance rng 0.7)
                  (pick rng 'none 'shrink 'grow)
                  default-insets
                  0.0
                  'all)))

(define (a-geom rng)
  (define names (preset-names))
  (preset-geom (list-ref names (random (length names) rng))
               ;; An adjustment's value is a formula, and "val N" is the
               ;; literal form the presets read.
               (if (chance rng 0.4)
                   (list (cons "adj" (format "val ~a" (random 5000 90000 rng)))
                         (cons "adj1" (format "val ~a" (random 5000 90000 rng)))
                         (cons "adj2" (format "val ~a" (random 5000 90000 rng))))
                   '())))

(define (a-bbox rng w h)
  ;; Sometimes degenerate on purpose: a zero-width connector and a hairline box
  ;; are both things real decks contain, and both have crashed something here.
  (define bw (if (chance rng 0.08) (real-in rng 0.0 0.5) (real-in rng 8.0 (/ w 2.0))))
  (define bh (if (chance rng 0.08) (real-in rng 0.0 0.5) (real-in rng 8.0 (/ h 2.0))))
  (make-bbox (real-in rng 0.0 (- w bw)) (real-in rng 0.0 (- h bh)) bw bh
             #:rot (if (chance rng 0.3) (real-in rng 0.0 360.0) 0.0)
             #:flip-h? (chance rng 0.2) #:flip-v? (chance rng 0.2)))

(define (a-custom-geom rng)
  ;; A path in its own space, with the space sometimes declared 0 -- which means
  ;; the coordinates are EMU, and is what several real decks write.
  (define span (pick rng 0 1 21600 100000))
  (define (coord) (random 21600 rng))
  (custom-geom
   (list (append (list (list 'move (cons (coord) (coord))))
                 (for/list ([_ (in-range (add1 (random 4 rng)))])
                   (if (chance rng 0.4)
                       (list 'curve (cons (coord) (coord)) (cons (coord) (coord))
                             (cons (coord) (coord)))
                       (list 'line (cons (coord) (coord)))))
                 (if (chance rng 0.5) (list (list 'close)) '())))
   span span))

(define (a-shape rng id w h)
  (shape id (format "Shape ~a" id) (a-bbox rng w h)
         (if (chance rng 0.25) (a-custom-geom rng) (a-geom rng))
         (a-fill rng) (a-line rng) (a-body rng) (a-shadow rng)))

;; A shadow now and then, so one goes round the trip.
(define (a-shadow rng)
  (and (chance rng 0.2)
       (shadow (real-in rng 0.0 20.0) (real-in rng 0.0 12.0)
               (real-in rng 0.0 360.0) (a-color rng))))

(define (an-element rng id w h [depth 0])
  (cond
    ;; A group, whose children sit inside its own box.
    [(and (< depth 2) (chance rng 0.15))
     (define b (a-bbox rng w h))
     (group id (format "Group ~a" id) b
            (for/list ([k (in-range (add1 (random 3 rng)))])
              (a-shape rng (+ (* 1000 id) k) (max 8.0 (bbox-w b)) (max 8.0 (bbox-h b))))
            b)]
    [else (a-shape rng id w h)]))

(define (a-deck rng)
  (define w (pick rng 720.0 960.0 1920.0))
  (define h (* w (pick rng 0.5625 0.75)))
  (define n (add1 (random 5 rng)))
  (deck w h
        (for/list ([i (in-range (add1 (random 2 rng)))])
          (define k (add1 (random 5 rng)))
          (slide (add1 i) (format "Slide ~a" (add1 i)) w h
                 (solid-fill (a-color rng)) '()
                 (for/list ([j (in-range k)]) (an-element rng (+ 100 (* 10 i) j) w h))
                 ;; A slide the show skips, now and then.
                 (chance rng 0.15)
                 #f))
        #f "fuzz"))

;; What a round trip may not change: everything the merge can see, compared the
;; way the round-trip tests compare it. It used to be the box and the flips
;; alone, which is how a line spacing of 1.5 came back as 1.0 for a year -- the
;; generator was producing them, and nothing was looking.
(define (elements-by-tag d)
  (append*
   ;; A group is one element, as it is to a merge. Its children are compared
   ;; through the group's own box: a group scales what it holds, so a child's
   ;; width coming back different is the group doing its job.
   (for/list ([s (in-list (deck->slide-states d))])
     (for/list ([e (in-list (slide-state-elements s))] #:when (el-state-tag e))
       (cons (list (slide-state-index s) (el-state-tag e)) e)))))

;; And the elements themselves, which is where the geometry, the gradient
;; stops, the dash patterns and what a group holds actually live.
(define (ir-by-name d)
  (append*
   (for/list ([s (in-list (deck-slides d))])
     (for/list ([e (in-list (slide-elements s))]
                #:unless (string=? "" (element-name e)))
       (cons (list (slide-index s) (element-name e)) e)))))

(define failures 0)

(for ([round-n (in-range ROUNDS)])
  (define seed (+ BASE-SEED round-n))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed))
  (define d (a-deck rng))
  (define dir (build-path work (format "r~a" seed)))
  (make-directory* dir)
  (define out (build-path dir "out.pptx"))
  (define (stage what thunk)
    (with-handlers ([exn:fail? (lambda (e)
                                 (set! failures (add1 failures))
                                 (fail (format "seed ~a: ~a raised: ~a" seed what
                                               (first (string-split (exn-message e) "\n"))))
                                 #f)])
      (thunk)))
  (define picts (stage 'render (lambda () (deck->picts d))))
  (when picts
    (when (stage 'export (lambda () (picts->pptx picts out
                                                 #:width (deck-width d)
                                                 #:height (deck-height d))))
      (define back (stage 'reimport (lambda () (pptx->deck out #:workdir (build-path dir "u")))))
      (when back
        ;; Only what differs, so a failure names the field rather than printing
        ;; two decks to compare by eye.
        (define was (elements-by-tag d))
        (define now (for/hash ([p (in-list (elements-by-tag back))]) (values (car p) (cdr p))))
        (define diffs
          (append
           (if (= (length was) (hash-count now))
               '()
               (list (format "~a elements became ~a" (length was) (hash-count now))))
           (append*
            (for/list ([p (in-list was)])
              (define other (hash-ref now (car p) #f))
              (cond
                [(not other) (list (format "~s vanished" (car p)))]
                [else (for/list ([d (in-list (element-diffs (cdr p) other))])
                        (format "~s: ~a" (car p) d))])))))
        ;; The same again, field for field on the elements themselves.
        (define was-ir (ir-by-name d))
        (define now-ir (for/hash ([p (in-list (ir-by-name back))]) (values (car p) (cdr p))))
        (define ir-differences
          (append*
           (for/list ([p (in-list was-ir)])
             (define other (hash-ref now-ir (car p) #f))
             (cond
               [(not other) (list (format "~s vanished" (car p)))]
               [else (for/list ([d (in-list (ir-diffs (cdr p) other))])
                       (format "~s: ~a" (car p) d))]))))
        ;; And what a slide says about itself.
        (define slide-differences
          (append*
           (for/list ([x (in-list (deck-slides d))] [y (in-list (deck-slides back))])
             (for/list ([w (in-list (slide-diffs x y))])
               (format "slide ~a: ~a" (slide-index x) w)))))
        (check-equal? (append diffs ir-differences slide-differences) '()
                      (format "seed ~a" seed))))))

(printf "fuzz done; ~a rounds from seed ~a\n" ROUNDS BASE-SEED)

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
