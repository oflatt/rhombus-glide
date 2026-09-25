#lang racket/base
;; What we draw, against what LibreOffice draws, on decks nobody here wrote.
;;
;; Everything else compares one description of a deck with another: the IR that
;; came out of a file against the IR that goes back in, or a program against the
;; deck beside it. None of that can see a drawing bug. A run with
;; `baseline: -0.06` at 50pt survived every round trip exactly -- and was drawn
;; at 50pt, where a subscript should be smaller, so every line came out wider
;; than it should have been and a line that had fitted wrapped its last
;; character onto the next one. Nothing disagreed, because nothing looked.
;;
;; Two things did look, and neither of them at this. `fidelity.rkt` and
;; `elements.rkt` compare against LibreOffice, but only over the five decks
;; written here -- and not one of them had a superscript in it. That gap is
;; closed by `06-drawn.pptx`, which holds what only exists in the drawing, and
;; by this: the corpus, where the features are whatever real decks happen to
;; have rather than whatever anyone thought to put in a fixture.
;;
;; A page-level number is blunt -- a wrapped line is a fraction of a percent of
;; its page, which is why the fixture goes through `elements.rkt` instead, where
;; each element is measured against its own ink. What this catches is the other
;; kind: a background that is not drawn at all, a fill that comes out white.
;;
;; Each deck has the score it got recorded beside it, so a change that makes one
;; worse trips, and one that makes it better shows up in the diff.
;;
;; Needs LibreOffice and a corpus. Without either it says so and passes.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/verify glide-pptx/parse)

(define-runtime-path corpus-dir "corpus")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-render"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define (worst-of pptx #:dpi [dpi 60])
  (define r (verify-deck pptx work #:dpi dpi
                         #:mae-threshold 1.0 #:bad-threshold 1.0 #:keep-images? #t))
  (values (apply max 0.0 (map page-diff-bad-fraction (deck-diff-pages r)))
          (deck-diff-pages r)))

;; Each with the score it got. A ceiling rather than a number to hit: two text
;; engines never agree to the pixel, and what this is watching for is a line
;; that broke somewhere else.
(define recorded
  (hash "font-scale.pptx" 0.15
        "bullet-indent.pptx" 0.02
        "paraMarginAndIndentation.pptx" 0.01
        "bulletMarginAndIndent.pptx" 0.004
        "poi-layouts.pptx" 0.03
        "tdf105150.pptx" 0.02
        "activex_picture.pptx" 0.06
        "n778859.pptx" 0.03
        ;; The slide's texture background, the arrow's picture fill and the
        ;; ellipse's are all drawn as nothing, so most of the page differs.
        ;; Recorded as it stands so the rest of these are guarded meanwhile.
        "poi-backgrounds.pptx" 0.73
        "master-bg-color.pptx" 0.01
        "tdf111863.pptx" 0.002
        "poi-table-with-no-theme.pptx" 0.02))

(cond
  [(not (or (find-executable-path "soffice") (find-executable-path "libreoffice")))
   (printf "no LibreOffice to render against; skipped\n")]
  [else
   (current-allow-unsupported? #t)
   (cond
     [(not (directory-exists? corpus-dir))
      (printf "no corpus present; run tools/fetch-corpus.sh for the rest of this\n")]
     [else
      (for ([(name ceiling) (in-hash recorded)])
        (define pptx (build-path corpus-dir name))
        (when (file-exists? pptx)
          (define-values (bad pages) (worst-of pptx))
          (with-check-info (['deck name] ['pages (length pages)])
            (check-true (<= bad ceiling)
                        (format "~a: worst page differs by ~a, over its recorded ~a"
                                name (real->decimal-string bad 3) ceiling))
            ;; Better than recorded is worth saying: the number beside it is
            ;; then out of date, and out-of-date ceilings guard nothing.
            (when (< bad (* 0.6 ceiling))
              (printf "~a is better than recorded: ~a against ~a -- lower it\n"
                      name (real->decimal-string bad 3) ceiling)))))])
   (printf "render tests done\n")])

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
