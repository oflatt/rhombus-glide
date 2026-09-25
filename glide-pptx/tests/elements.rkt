#lang racket/base
;; Every element on a page of its own, compared against LibreOffice.
;;
;; A whole-slide pixel diff cannot see a small thing that is wrong. Ten missing
;; arrowheads on a busy slide moved its mean error from 2.15% to 2.16%, which is
;; noise -- so the arrows were drawn as plain lines for as long as this existed.
;;
;; Two changes fix that. Each element gets a page the size of its own box, so it
;; is the whole picture rather than a detail of one. And the error is measured
;; against the *ink* rather than the page, because a thin line on its own page
;; is still mostly white: the same missing arrowheads score 9.6% against 17.6%
;; that way, where against the page they scored 2.02% against 2.46%.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format racket/string
         racket/class racket/draw racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/verify "ink.rkt")

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-elements"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define MARGIN 12.0)
;; With the metric no longer inventing differences, what is left is real. It
;; reads 0.05% for the same drawing shifted half a pixel, and the worst element
;; here is 0.2%, so one percent is twenty times the floor and five times the
;; worst thing we draw -- room for a rasterizer disagreeing about a stem, and
;; not much else.
;;
;; It was fifteen percent an hour ago, and under it sat a strikethrough drawn
;; twice as thick as it should be and in the wrong place, a chevron with the
;; wrong notch, and a pentagon with the wrong point. A loose bound is not a
;; lenient test; it is an absent one.
(define INK-LIMIT 0.01)

;; A line of text gets a limit of its own, because the two text stacks do not
;; agree about advances. The glyphs themselves are the same picture -- measured
;; on the bubble in `06-drawn`, every glyph's ink is the same width to the pixel
;; at 300 dpi -- but the later ones sit further right here than in LibreOffice,
;; by up to 7 pixels across a line. Measured over four strings in that deck, the
;; drift is +0.16%, +0.38%, +1.01% and +1.30% of the line's own width, so it is
;; the string that decides it and not the size: kerning accounts for at most
;; 0.31% of any of them, and turning it off does not close the gap.
;;
;; So this is not something to fix here. A shape is still held to one percent;
;; a line of text on a page the size of its own box is held to two, which is
;; above that measured drift and well under a glyph.
(define TEXT-INK-LIMIT 0.02)

;; Whether what this element draws includes text.
(define (text-element? e)
  (and (shape? e) (not (text-body-empty? (shape-body e)))))

;; The share of the drawing that differs lives in `ink.rkt`, which explains what
;; it does and does not count. What it does not count any more is a drawing that
;; sits half a pixel away from where the other renderer put it: measured against
;; itself, that used to read as up to 45% wrong, which is why the limit here had
;; to be 15% and why 14.2% of a strikethrough drawn in the wrong place, twice as
;; thick as it should be, sat under it and said nothing.

;; One element, alone, on a page the size of its own box.
(define (element-deck src-deck e)
  (define b (element-bbox e))
  (define w (+ (* 2 MARGIN) (max 8.0 (bbox-w b))))
  (define h (+ (* 2 MARGIN) (max 8.0 (bbox-h b))))
  (define moved
    (element-map-bbox e (lambda (bb)
                          (make-bbox (+ MARGIN (- (bbox-x bb) (bbox-x b)))
                                     (+ MARGIN (- (bbox-y bb) (bbox-y b)))
                                     (bbox-w bb) (bbox-h bb)
                                     #:rot (bbox-rot bb)
                                     #:flip-h? (bbox-flip-h? bb)
                                     #:flip-v? (bbox-flip-v? bb)))))
  (values (deck w h (list (slide 1 "one" w h #f '() (list moved) #f #f))
                (deck-media-dir src-deck) (deck-source src-deck))
          w h))

(define (elements-of name)
  (define src (build-path decks-dir (string-append name ".pptx")))
  (define dir (build-path work name))
  (make-directory* dir)
  (define d (pptx->deck src #:workdir (build-path dir "u")))
  (define out '())
  (for* ([s (in-list (deck-slides d))]
         [e (in-list (slide-elements s))])
    (define tag (format "s~a-~a" (slide-index s)
                        (regexp-replace* #px"[^A-Za-z0-9]+" (element-name e) "_")))
    (with-handlers ([exn:fail? (lambda (ex)
                                 (set! out (cons (list tag 'failed (exn-message ex) INK-LIMIT)
                                                 out)))])
      (define-values (d1 w h) (element-deck d e))
      (define pptx (build-path dir (format "~a.pptx" tag)))
      (picts->pptx (deck->picts d1) pptx #:width w #:height h)
      ;; Rendered so the element is about this many pixels across, whatever its
      ;; size on the slide. A 40pt shape at a fixed 200 dpi is a hundred pixels
      ;; of which the edge is most, and antialiasing then swamps the measure --
      ;; the same element scored 65% small and 10% blown up.
      (define dpi (max 96.0 (min 600.0 (/ (* 1200.0 72.0) (max w h)))))
      (define r (verify-deck pptx (build-path dir tag) #:dpi dpi))
      (define pages (and r (deck-diff-pages r)))
      (when (and pages (pair? pages))
        (define sub (build-path dir tag (format "~a" tag)))
        (define ours (build-path sub "our-page-1.png"))
        (define ref (build-path sub "ref-page-1.png"))
        (when (and (file-exists? ours) (file-exists? ref))
          (set! out (cons (list tag (ink-error ours ref) #f
                                (if (text-element? e) TEXT-INK-LIMIT INK-LIMIT))
                          out))))))
  (reverse out))

(define all
  (append* (for/list ([name (in-list '("01-placeholders" "02-text" "03-shapes"
                                       "04-pictures-groups" "05-realistic"
                                       ;; What only exists in the drawing.
                                       "06-drawn"))])
             (elements-of name))))

(define compared (filter (lambda (r) (real? (second r))) all))
(define ranked (sort compared > #:key second))
(printf "~a elements compared, worst first:\n" (length ranked))
(for ([r (in-list (take ranked (min 8 (length ranked))))])
  (printf "  ~a%  ~a\n" (~r (* 100 (second r)) #:precision 1) (first r)))

(for ([r (in-list all)])
  (check-true (real? (second r))
              (format "~a could not be compared: ~a" (first r) (third r))))
(for ([r (in-list ranked)])
  (define limit (fourth r))
  (check-true (< (second r) limit)
              (format "~a: ~a% of its drawing differs from LibreOffice's, over ~a%"
                      (first r) (~r (* 100 (second r)) #:precision 1)
                      (~r (* 100 limit) #:precision 0))))

(printf "element tests done; artifacts under ~a\n" work)

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
