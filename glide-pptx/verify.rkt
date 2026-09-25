#lang racket/base
;; Fidelity checking: render a deck twice and compare the results as images.
;;
;; The reference is LibreOffice's own conversion of the .pptx, which is the
;; closest thing to PowerPoint available headlessly. Both sides are rasterized
;; by the same rasterizer at the same resolution, so the numbers reflect layout
;; differences rather than two rasterizers disagreeing.
;;
;; Exact equality is not the goal and is not reachable: LibreOffice and cairo
;; hint and antialias text differently. What the numbers are for is catching
;; regressions and pointing at the slide that broke.
(require racket/class racket/list racket/string racket/file racket/path
         racket/system racket/format
         racket/draw
         "ir.rkt" "parse.rkt" "render.rkt" "runtime.rkt")
(provide (struct-out page-diff) (struct-out deck-diff)
         verify-deck
         libreoffice-pdf rasterize-pdf compare-images
         format-report)

;; `mae` is the mean absolute channel error over the page, 0..1.
;; `bad-fraction` is the share of pixels off by more than `bad-threshold`, which
;; is the number that actually tracks "something moved" rather than "text is
;; antialiased differently".
(struct page-diff (index width height mae bad-fraction diff-path montage-path) #:transparent)
(struct deck-diff (name pages ref-pdf our-pdf ok?) #:transparent)

(define bad-threshold 0.25)

(define (find-exe name)
  (or (find-executable-path name)
      (for/or ([d (in-list '("/usr/bin" "/usr/local/bin" "/opt/libreoffice/program"))])
        (define p (build-path d name))
        (and (file-exists? p) p))))

(define (run-quiet exe . args)
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out] [current-error-port out])
      (apply system*/exit-code exe args)))
  (values code (get-output-string out)))

;; ---------------------------------------------------------------- reference

;; Converts `pptx` to PDF with LibreOffice, returning the path. Each call gets
;; its own user profile so concurrent runs do not fight over one.
(define (libreoffice-pdf pptx out-dir #:profile [profile #f])
  (define soffice (or (find-exe "soffice") (find-exe "libreoffice")))
  (unless soffice
    (error 'libreoffice-pdf "LibreOffice not found; install libreoffice-impress"))
  (make-directory* out-dir)
  (define prof (or profile (build-path out-dir ".loprofile")))
  (make-directory* prof)
  (define-values (code output)
    (run-quiet soffice "--headless" "--norestore" "--nolockcheck"
               (format "-env:UserInstallation=file://~a" (path->string (path->complete-path prof)))
               "--convert-to" "pdf" "--outdir" (path->string out-dir)
               (path->string (path->complete-path pptx))))
  (define expected
    (build-path out-dir (path-replace-extension (file-name-from-path pptx) ".pdf")))
  (cond
    [(file-exists? expected) expected]
    [else (error 'libreoffice-pdf "conversion produced nothing (exit ~a)\n~a" code output)]))

;; ---------------------------------------------------------------- rasterizing

;; One PNG per page, named "<prefix>-N.png", returned in page order.
(define (rasterize-pdf pdf prefix #:dpi [dpi 96])
  (define pdftoppm (find-exe "pdftoppm"))
  (unless pdftoppm (error 'rasterize-pdf "pdftoppm not found; install poppler-utils"))
  (make-directory* (path-only (path->complete-path prefix)))
  (define-values (code output)
    (run-quiet pdftoppm "-r" (number->string dpi) "-png"
               (path->string (path->complete-path pdf))
               (path->string (path->complete-path prefix))))
  (unless (zero? code) (error 'rasterize-pdf "pdftoppm failed: ~a" output))
  (define dir (path-only (path->complete-path prefix)))
  (define stem (path->string (file-name-from-path prefix)))
  (sort (for/list ([f (in-list (directory-list dir))]
                   #:when (regexp-match? (format "^~a-[0-9]+[.]png$" (regexp-quote stem))
                                         (path->string f)))
          (build-path dir f))
        < #:key page-number-of))

(define (page-number-of p)
  (define m (regexp-match #rx"-([0-9]+)[.]png$" (path->string p)))
  (if m (string->number (cadr m)) 0))

;; ----------------------------------------------------------------- comparing

;; Compares two page images, writing a diff visualization. Pages of unequal
;; size are compared over their overlap and the excess counted as different, so
;; a size mismatch cannot masquerade as a good score.
(define (compare-images a-path b-path #:diff-path [diff-path #f])
  (define a (read-bitmap a-path))
  (define b (read-bitmap b-path))
  (define w (min (send a get-width) (send b get-width)))
  (define h (min (send a get-height) (send b get-height)))
  (define full-w (max (send a get-width) (send b get-width)))
  (define full-h (max (send a get-height) (send b get-height)))
  (define ap (make-bytes (* 4 w h)))
  (define bp (make-bytes (* 4 w h)))
  (send a get-argb-pixels 0 0 w h ap)
  (send b get-argb-pixels 0 0 w h bp)
  (define diff (and diff-path (make-bytes (* 4 w h) 255)))
  (define total 0.0)
  (define bad 0)
  (for ([i (in-range (* w h))])
    (define o (* 4 i))
    (define dr (abs (- (bytes-ref ap (+ o 1)) (bytes-ref bp (+ o 1)))))
    (define dg (abs (- (bytes-ref ap (+ o 2)) (bytes-ref bp (+ o 2)))))
    (define db (abs (- (bytes-ref ap (+ o 3)) (bytes-ref bp (+ o 3)))))
    (define mx (max dr dg db))
    (set! total (+ total (/ (+ dr dg db) 3.0)))
    (when (> mx (* 255 bad-threshold)) (set! bad (add1 bad)))
    (when diff
      ;; Grey where the pages agree, red where they do not, so a diff image
      ;; reads at a glance.
      (define grey (- 255 (quotient (* 3 (- 255 (bytes-ref ap (+ o 1)))) 4)))
      (bytes-set! diff (+ o 0) 255)
      (bytes-set! diff (+ o 1) (if (> mx 8) 255 grey))
      (bytes-set! diff (+ o 2) (if (> mx 8) (max 0 (- 255 (* 4 mx))) grey))
      (bytes-set! diff (+ o 3) (if (> mx 8) (max 0 (- 255 (* 4 mx))) grey))))
  (define pixels (* w h))
  ;; Area present in only one of the two pages counts as fully different.
  (define extra (- (* full-w full-h) pixels))
  (define denom (max 1 (+ pixels extra)))
  (when diff
    (define bm (make-bitmap w h))
    (send bm set-argb-pixels 0 0 w h diff)
    (send bm save-file diff-path 'png))
  (values (/ (+ total (* 255.0 extra)) (* 255.0 denom))
          (/ (+ bad extra) (exact->inexact denom))
          full-w full-h))

;; Stacks the reference above ours above the diff, for eyeballing one page.
(define (montage paths out-path)
  (define bms (map read-bitmap paths))
  (define w (apply max (map (lambda (b) (send b get-width)) bms)))
  (define gap 6)
  (define h (+ (* gap (sub1 (length bms))) (apply + (map (lambda (b) (send b get-height)) bms))))
  (define bm (make-bitmap w h))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-brush (new brush% [color (make-object color% 128 128 128)]))
  (send dc set-pen (new pen% [style 'transparent]))
  (send dc draw-rectangle 0 0 w h)
  (for/fold ([y 0]) ([b (in-list bms)])
    (send dc draw-bitmap b 0 y)
    (+ y (send b get-height) gap))
  (send bm save-file out-path 'png)
  out-path)

;; ------------------------------------------------------------------- driver

;; Renders `pptx` both ways and reports per-page differences.
(define (verify-deck pptx work-dir
                     #:dpi [dpi 96]
                     #:mae-threshold [mae-threshold 0.02]
                     #:bad-threshold [bad-frac-threshold 0.06]
                     #:keep-images? [keep-images? #t]
                     #:warnings [warnings #f])
  (define name (path->string (path-replace-extension (file-name-from-path pptx) "")))
  (define dir (build-path work-dir name))
  (make-directory* dir)
  (define ref-pdf (libreoffice-pdf pptx (build-path dir "ref")))
  (define d (parameterize ([current-warnings warnings])
              (pptx->deck pptx #:workdir (build-path dir "unpacked"))))
  (define our-pdf (build-path dir "ours.pdf"))
  (define picts (parameterize ([runtime-warnings warnings]) (deck->picts d)))
  (picts->pdf picts our-pdf #:width (deck-width d) #:height (deck-height d))
  (define ref-pages (rasterize-pdf ref-pdf (build-path dir "ref-page") #:dpi dpi))
  (define our-pages (rasterize-pdf our-pdf (build-path dir "our-page") #:dpi dpi))
  (define n (max (length ref-pages) (length our-pages)))
  (define pages
    (for/list ([i (in-range n)])
      (define r (and (< i (length ref-pages)) (list-ref ref-pages i)))
      (define o (and (< i (length our-pages)) (list-ref our-pages i)))
      (cond
        [(and r o)
         (define diff-path (and keep-images? (build-path dir (format "diff-~a.png" (add1 i)))))
         (define-values (mae bad w h) (compare-images r o #:diff-path diff-path))
         (page-diff (add1 i) w h mae bad diff-path
                    (and keep-images?
                         (montage (list r o diff-path)
                                  (build-path dir (format "montage-~a.png" (add1 i))))))]
        ;; A page one side does not have at all is a total mismatch.
        [else (page-diff (add1 i) 0 0 1.0 1.0 #f #f)])))
  (deck-diff name pages ref-pdf our-pdf
             (and (= (length ref-pages) (length our-pages))
                  (for/and ([p (in-list pages)])
                    (and (<= (page-diff-mae p) mae-threshold)
                         (<= (page-diff-bad-fraction p) bad-frac-threshold))))))

(define (format-report dd #:mae-threshold [mae 0.02] #:bad-threshold [bad 0.06])
  (define out (open-output-string))
  (fprintf out "~a\n" (deck-diff-name dd))
  (fprintf out "  ~a  ~a  ~a  ~a\n"
           (~a "page" #:min-width 6) (~a "mean err" #:min-width 10)
           (~a "pixels off" #:min-width 11) "")
  (for ([p (in-list (deck-diff-pages dd))])
    (define ok? (and (<= (page-diff-mae p) mae) (<= (page-diff-bad-fraction p) bad)))
    (fprintf out "  ~a  ~a  ~a  ~a\n"
             (~a (page-diff-index p) #:min-width 6)
             (~a (~r (* 100.0 (page-diff-mae p)) #:precision '(= 2)) "%" #:min-width 10)
             (~a (~r (* 100.0 (page-diff-bad-fraction p)) #:precision '(= 2)) "%" #:min-width 11)
             (if ok? "ok" "DIFFERS"))) 
  (fprintf out "  ~a\n" (if (deck-diff-ok? dd) "PASS" "FAIL"))
  (get-output-string out))
