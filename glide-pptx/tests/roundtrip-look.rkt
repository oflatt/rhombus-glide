#lang racket/base
;; Does a deck still look like itself after a round trip?
;;
;; Import it, write the program, load the program, export a deck: that is the
;; whole of what `raco glide` does to a file before anyone edits anything. Then
;; LibreOffice draws the deck that went in and the deck that came out, and the
;; two pictures are compared.
;;
;; Our renderer is not in this. `elements.rkt` asks whether we draw what
;; LibreOffice draws; this asks whether what we *write* is what we were given,
;; in the only currency that settles it. A property can survive the IR
;; perfectly, be drawn perfectly, and still be dropped on the way out -- three
;; were, and each of them looked like nothing at all until a picture of the
;; before and the after were put side by side.
;;
;; Each deck carries the score it got. Zero is not a fantasy: most of these are
;; zero, so a deck that starts differing is a regression with a number on it
;; rather than a vague sense that something moved.
;;
;; Needs LibreOffice and a corpus. Without either it says so and passes.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format racket/string
         racket/runtime-path
         glide-pptx/parse glide-pptx/emit-rhombus glide-pptx/export
         glide-pptx/sync glide-pptx/verify
         "ink.rkt")

(define-runtime-path corpus-dir "corpus")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-roundtrip-look"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Deck -> the worst page it is allowed to differ by, as a fraction of the ink.
(define recorded
  (hash "poi-layouts.pptx" 0.01
        "font-scale.pptx" 0.03
        ;; A hanging indent still comes back a little wide.
        "bullet-indent.pptx" 0.055
        "paraMarginAndIndentation.pptx" 0.002
        "bulletMarginAndIndent.pptx" 0.002
        "tdf105150.pptx" 0.002
        "master-bg-color.pptx" 0.002
        "tdf111863.pptx" 0.002
        "n778859.pptx" 0.005
        ;; Tables that name a style. What the style paints is resolved when the
        ;; deck is read and written into the cells, so the far side needs no
        ;; style to draw them the same way.
        "n90190.pptx" 0.05
        "bnc887225.pptx" 0.20
        ;; A table is written back as a table now, so the style that draws its
        ;; borders applies again on the far side and it comes back exactly.
        "poi-table-with-no-theme.pptx" 0.0
        ;; A title slide whose whole design is its layout's background picture.
        ;; Its first page is exact; the rest carry tables and are recorded as
        ;; they stand.
        "poi-aptia.pptx" 0.41))

;; The round trip, as the command line does it.
(define (round-trip src dir)
  (define d (pptx->deck src #:workdir (build-path dir "u")))
  (define program (build-path dir "p.rhm"))
  (write-rhombus-deck d program #:source-name (path->string (file-name-from-path src)))
  (define out (build-path dir "out.pptx"))
  (picts->pptx (load-program-picts program) out)
  out)

(define (worst-page src out dir)
  (define a (rasterize-pdf (libreoffice-pdf src (build-path dir "a"))
                           (build-path dir "ap") #:dpi 72))
  (define b (rasterize-pdf (libreoffice-pdf out (build-path dir "b"))
                           (build-path dir "bp") #:dpi 72))
  (cond
    [(not (= (length a) (length b))) (values 'pages (length a) (length b))]
    [else (values 'ok (apply max 0.0 (for/list ([x (in-list a)] [y (in-list b)]) (ink-error x y)))
                  #f)]))

(cond
  [(not (or (find-executable-path "soffice") (find-executable-path "libreoffice")))
   (printf "no LibreOffice to render against; skipped\n")]
  [(not (directory-exists? corpus-dir))
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   (current-allow-unsupported? #t)
   (for ([(name ceiling) (in-hash recorded)])
     (define src (build-path corpus-dir name))
     (when (file-exists? src)
       (define dir (build-path work name))
       (make-directory* dir)
       (with-check-info (['deck name])
         (define out
           (with-handlers ([exn:fail? (lambda (e)
                                        (fail (format "~a: the round trip failed: ~a" name
                                                      (first (string-split (exn-message e) "\n"))))
                                        #f)])
             (round-trip src dir)))
         (when out
           (define-values (how bad other) (worst-page src out dir))
           (case how
             [(pages) (fail (format "~a: ~a pages went in and ~a came out" name bad other))]
             [else
              (check-true (<= bad ceiling)
                          (format "~a: worst page differs by ~a, over its recorded ~a"
                                  name (real->decimal-string bad 3) ceiling))
              (when (< bad (* 0.5 ceiling))
                (printf "~a is better than recorded: ~a against ~a -- lower it\n"
                        name (real->decimal-string bad 3) ceiling))])))))
   (printf "round-trip look tests done\n")])

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
