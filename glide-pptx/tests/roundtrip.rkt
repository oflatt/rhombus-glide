#lang racket/base
;; Emitted programs must draw exactly what the direct renderer draws.
;;
;; Both go through the same runtime, so any difference means the emitter dropped
;; or garbled something -- a missing keyword, a default assumed wrongly, a
;; coordinate rounded away. Comparing rendered pages catches that where reading
;; the generated source would not.
(require rackunit/log)
(require rackunit racket/list racket/string racket/class racket/draw racket/file racket/path racket/system racket/port
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/emit-rhombus glide-pptx/verify)

(define-runtime-path decks-dir "decks")
(define-runtime-path here ".")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-roundtrip"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; `exec-file` is however racket was invoked, which is a bare name when it came
;; off the PATH -- and `system*` cannot exec one of those.
(define racket-exe
  (let ([e (find-system-path 'exec-file)])
    (if (absolute-path? e) e (or (find-executable-path e) e))))

;; Running the program itself opens a slideshow, which is what running a talk
;; should do -- so the PDF comes from its `pdf` submodule.
(define (run-program path)
  (define dir (path-only path))
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory dir]
                   [current-output-port out] [current-error-port out])
      (system*/exit-code racket-exe "-l" "racket/base" "-e"
                         (format "(require (submod (file ~s) pdf))"
                                 (path->string path)))))
  (unless (zero? code)
    (error 'run-program "~a exited with ~a\n~a" path code (get-output-string out)))
  (build-path dir (path-replace-extension (file-name-from-path path) ".pdf")))

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          f)
        string<? #:key path->string))

(check-true (>= (length decks) 5) "the fixture decks are present")

(for ([deck (in-list decks)])
  (define name (path->string (path-replace-extension deck "")))
  (define pptx (build-path decks-dir deck))
  (define dir (build-path work name))
  (make-directory* dir)

  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))

  ;; The reference for this test is the direct render, not LibreOffice.
  (define direct-pdf (build-path dir "direct.pdf"))
  (picts->pdf (deck->picts d) direct-pdf
              #:width (deck-width d) #:height (deck-height d))
  (define direct-pages (rasterize-pdf direct-pdf (build-path dir "direct") #:dpi 96))

  (let ([lang 'rhombus])
    (define program (build-path dir (string-append name ".rhm")))
    (write-rhombus-deck d program #:source-name (path->string pptx))
    (check-true (file-exists? program) (format "~a ~a was emitted" name lang))

    (define pdf (run-program program))
    (define pages (rasterize-pdf pdf (build-path dir (format "~a-page" lang)) #:dpi 96))

    (check-equal? (length pages) (length direct-pages)
                  (format "~a ~a: page count" name lang))
    (for ([a (in-list direct-pages)] [b (in-list pages)] [i (in-naturals 1)])
      (define-values (mae bad w h) (compare-images a b))
      ;; The two paths differ only in how the same runtime calls were spelled,
      ;; so the only allowed difference is antialiasing around coordinates the
      ;; emitter rounded to a thousandth of a point.
      (check-= bad 0.0 0.0005
               (format "~a ~a page ~a: emitted output matches the direct render" name lang i))
      (check-= mae 0.0 0.0005
               (format "~a ~a page ~a: no visible residual difference" name lang i)))))

(printf "roundtrip tests done (~a decks)\n" (length decks))

;; ------------------------------------------- the program is a slideshow

;; Running a talk should show it. The generated `module main` is a slideshow, so
;; `racket talk.rhm` opens the slides -- it was reported as surprising when it
;; wrote a PDF instead, which is what the PDF's own submodule is now for.
(let ()
  (define dir (build-path work "slideshow"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "show.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define src (file->string program))

  ;; What `racket show.rhm` runs.
  (check-regexp-match #rx"module main:\n  import: lib[(]\"glide-pptx/show.rhm\"[)] open" src)
  (check-regexp-match #rx"show_slides[(]all_slides, ~width: slide_width" src
                      "each slide is handed to slideshow, which fills the page")
  (check-regexp-match #rx"module pdf:" src "and the PDF has its own submodule")

  ;; Slideshow needs a display, so the real thing is only checked where there is
  ;; one to borrow. It is the check that matters: the pages come out.
  (define xvfb (find-executable-path "xvfb-run"))
  (cond
    [(not xvfb) (printf "no xvfb-run; skipping the slideshow render\n")]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-directory dir]
                      [current-output-port out] [current-error-port out])
         (system*/exit-code xvfb "-a" racket-exe (path->string program)
                            "--widescreen" "--pdf" "-c" "-e" "-o" "show.pdf")))
     (define pdf (build-path dir "show.pdf"))
     (check-equal? code 0 (format "the slideshow ran:\n~a" (get-output-string out)))
     (check-true (file-exists? pdf) "and wrote its pages")
     (when (file-exists? pdf)
       (check-equal? (length (rasterize-pdf pdf (build-path dir "pg") #:dpi 24))
                     (length (deck-slides d))
                     "one page per slide"))]))

;; ------------------------------------- the slide fills slideshow's page

;; Reported from a real talk: the first slide came out small, with white margins
;; around it. slideshow lays content out inside the client area, which is the
;; page less its margins, so a slide scaled to the client leaves those margins
;; showing -- and a converted slide is a finished page, not content.
;;
;; The check that missed it counted pages. Three pages came out, so it passed.
;; What matters is where the ink lands, so that is what this measures.
(let ()
  (define xvfb (find-executable-path "xvfb-run"))
  (define dir (build-path work "fills"))
  (make-directory* dir)
  ;; A dark background, so the slide's own edges can be found in the render.
  (define program (build-path dir "fills.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "  slide_width"
          "  slide_height"
          "def slide_width = 959.976"
          "def slide_height = 540.0"
          "def slide_1 = slide_canvas("
          "  ~width: slide_width, ~height: slide_height, ~background: hex(\"1F3B63\"),"
          "  at(40.0, 40.0, ~tag: \"Box\","
          "     shape_pict(~width: 200.0, ~height: 100.0, ~fill: hex(\"ED7D31\")))"
          ")"
          "glide_slides all_slides:"
          "  [slide_1]"
          "module main:"
          "  import: lib(\"glide-pptx/show.rhm\") open"
          "  show_slides(all_slides, ~width: slide_width, ~height: slide_height)"
          "")
    "\n")
   program #:exists 'replace)

  (cond
    [(not xvfb) (printf "no xvfb-run; skipping the slideshow geometry check\n")]
    [else
     (define pdf (build-path dir "fills.pdf"))
     (define out (open-output-string))
     (define code
       (parameterize ([current-directory dir]
                      [current-output-port out] [current-error-port out])
         (system*/exit-code xvfb "-a" racket-exe (path->string program)
                            "--widescreen" "--pdf" "-c" "-e" "-o" "fills.pdf")))
     (check-equal? code 0 (format "the slideshow ran:\n~a" (get-output-string out)))
     (when (file-exists? pdf)
       (define pages (rasterize-pdf pdf (build-path dir "pg") #:dpi 50))
       (check-equal? (length pages) 1 "one page")
       (define bm (read-bitmap (first pages)))
       (define w (send bm get-width))
       (define h (send bm get-height))
       (define px (make-bytes (* w h 4)))
       (send bm get-argb-pixels 0 0 w h px)
       ;; The navy background: a low blue channel is the slide, not the page.
       (define-values (minx miny maxx maxy)
         (for*/fold ([minx w] [miny h] [maxx 0] [maxy 0])
                    ([y (in-range h)] [x (in-range w)])
           (define i (* 4 (+ x (* y w))))
           (if (< (bytes-ref px (+ i 3)) 200)
               (values (min minx x) (min miny y) (max maxx x) (max maxy y))
               (values minx miny maxx maxy))))
       (define fill-w (/ (add1 (- maxx minx)) (exact->inexact w)))
       (define fill-h (/ (add1 (- maxy miny)) (exact->inexact h)))
       (printf "  slide covers ~a% of the page width, ~a% of its height\n"
               (round (* 100 fill-w)) (round (* 100 fill-h)))
       ;; It used to be 96% by 95%, with the margins showing on every side.
       (check-true (> fill-w 0.99) (format "the slide fills the page width (~a)" fill-w))
       (check-true (> fill-h 0.99) (format "and its height (~a)" fill-h))
       (check-true (<= minx 1) (format "flush to the left edge (~a)" minx))
       (check-true (<= miny 1) (format "and the top (~a)" miny)))]))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
