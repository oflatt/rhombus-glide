#lang racket/base
;; Does an exported .pptx look like the picts it came from?
;;
;; The measurement is the same one used on the import side: render our picts to
;; PDF, have LibreOffice render the exported .pptx, rasterize both with the same
;; rasterizer and compare. Anything the writer gets wrong shows up here.
;;
;; The fixture decks are used as a convenient source of realistic picts; this
;; test is about the *export* path, so the reference is our own render rather
;; than LibreOffice's reading of the original deck.
(require rackunit/log)
(require rackunit racket/list racket/file racket/path racket/format racket/string
         racket/runtime-path pict
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/draw-ir glide-pptx/record-adapt glide-pptx/verify
         glide-pptx/emit-rhombus (only-in glide-pptx/sync load-program-picts)
         (only-in "deck-edit.rkt" deck-part)
         (only-in file/unzip read-zip-directory zip-directory-entries unzip
                  make-filesystem-entry-reader))

(define-runtime-path decks-dir "decks")

;; `zip-directory-entries` yields entry names as bytes.
(define (slide-part pptx n)
  (define dir (make-temporary-file "slidepart~a" 'directory))
  (call-with-input-file pptx (lambda (in) (parameterize ([current-directory dir])
                                            (unzip in (make-filesystem-entry-reader)))))
  (define xml (file->string (build-path dir "ppt" "slides" (format "slide~a.xml" n))))
  (delete-directory/files dir #:must-exist? #f)
  xml)

(define (zip-part pptx name)
  (define dir (make-temporary-file "zippart~a" 'directory))
  (call-with-input-file pptx (lambda (in) (parameterize ([current-directory dir])
                                            (unzip in (make-filesystem-entry-reader)))))
  (define xml (file->string (build-path dir (string->path name))))
  (delete-directory/files dir #:must-exist? #f)
  xml)

(define (zip-entry-names path)
  (for/list ([e (in-list (zip-directory-entries (read-zip-directory path)))])
    (bytes->string/utf-8 e)))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-export"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Compared against *our* render, so these budgets are looser than the semantic
;; ones further down, and deliberately so: an exported preset is drawn by
;; PowerPoint's own geometry, which is not the approximation `geometry.rkt`
;; draws. That gap is our renderer being approximate, not the writer being
;; wrong, which is why the meaningful check is against the original deck.
(define budgets
  (hash "01-placeholders"    '(0.015 . 0.020)
        "02-text"            '(0.012 . 0.020)
        "03-shapes"          '(0.020 . 0.035)
        "04-pictures-groups" '(0.008 . 0.015)
        "05-realistic"       '(0.020 . 0.030)))

(define (export-fidelity name)
  (define dir (build-path work name))
  (make-directory* dir)
  (define pptx (build-path decks-dir (string-append name ".pptx")))
  (define warnings (box '()))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define picts (deck->picts d))
  ;; Our own render is the reference.
  (define ours (build-path dir "ours.pdf"))
  (picts->pdf picts ours #:width (deck-width d) #:height (deck-height d))
  ;; The exported package, as LibreOffice reads it.
  (define exported (build-path dir "exported.pptx"))
  (parameterize ([current-export-warnings warnings])
    (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d)))
  (check-true (file-exists? exported) (format "~a: a package was written" name))
  (define ref (libreoffice-pdf exported (build-path dir "ref")))
  (define a (rasterize-pdf ours (build-path dir "ours") #:dpi 96))
  (define b (rasterize-pdf ref (build-path dir "theirs") #:dpi 96))
  (check-equal? (length b) (length a) (format "~a: page count survives the round trip" name))
  (values a b (remove-duplicates (reverse (unbox warnings)))))

(for ([name (in-list (sort (hash-keys budgets) string<?))])
  (define budget (hash-ref budgets name))
  (define-values (ours theirs warnings) (export-fidelity name))
  (printf "~a\n" name)
  (for ([a (in-list ours)] [b (in-list theirs)] [i (in-naturals 1)])
    (define-values (mae bad w h) (compare-images a b))
    (printf "  page ~a  mean err ~a%  pixels off ~a%\n" i
            (~r (* 100 mae) #:precision '(= 2)) (~r (* 100 bad) #:precision '(= 2)))
    (check-true (<= mae (car budget))
                (format "~a page ~a: mean error ~a% within ~a%" name i
                        (~r (* 100 mae) #:precision 2) (* 100 (car budget))))
    (check-true (<= bad (cdr budget))
                (format "~a page ~a: ~a% of pixels off, budget ~a%" name i
                        (~r (* 100 bad) #:precision 2) (* 100 (cdr budget)))))
  (for ([w (in-list warnings)]) (printf "  ! ~a\n" w)))

;; The writer should be producing real PowerPoint objects, not rasterizing. If a
;; simple slide ever comes out as pictures, something regressed badly.
(let ()
  (define p (vc-append 10
                       (filled-rectangle 100 40 #:color "SteelBlue")
                       (filled-ellipse 80 40 #:color "orange")))
  (define page (pict->display-page (lambda (dc) (draw-pict p dc 0 0)) 200.0 120.0))
  (define items (display-page-items page))
  (check-equal? (length items) 2 "two shapes, two items")
  (check-true (it:rect? (first items)) "a filled-rectangle stays a rectangle")
  (check-true (it:ellipse? (second items)) "a filled-ellipse stays an ellipse")
  (check-false (ormap it:image? items) "nothing was rasterized"))

;; A slide asked to carry its number carries it into the deck.
;;
;; The number used to be drawn by the slide assembler, which nothing but the
;; show goes through -- so a talk that rehearsed with numbers on exported a deck
;; with the corner empty. It is composed onto the slide now, which is a place
;; the exporter, the backup PDF and the show all read.
(let ()
  (define dir (build-path work "numbered"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import: lib(\"glide-pptx/runtime.rhm\") open"
          "export: all_slides"
          "set_slide_numbers(#true)"
          "def one = slide_canvas(~width: 480.0, ~height: 270.0)"
          "def two = slide_canvas(~width: 480.0, ~height: 270.0)"
          "def all_slides = [one, two]")
    "\n")
   program #:exists 'replace)
  (define out (build-path dir "out.pptx"))
  (picts->pptx (load-program-picts program) out)
  (define (digits-on n)
    (define xml (deck-part out (format "ppt/slides/slide~a.xml" n)))
    (list (regexp-match* #px"<a:t>([0-9]+)</a:t>" xml #:match-select cadr)
          (and (regexp-match? #rx"b=\"1\"" xml) #t)))
  (check-equal? (digits-on 1) (list '("1") #t)
                "the first slide is numbered, in bold")
  (check-equal? (digits-on 2) (list '("2") #t)
                "and so is the second, with its own number"))

;; A faded pict is faded in the deck too.
;;
;; A recording dc does not replay `cellophane` -- which is what Rhombus's
;; `.alpha()` becomes -- as a `set-alpha`. It brackets the drawing in
;; `start-alpha`/`end-alpha`, and reading only `set-alpha` left every faded
;; thing at full strength: a talk's dimmed outline came out in the accent
;; colours of the section it was not on.
(let ()
  (define page (pict->display-page
                (lambda (dc)
                  (draw-pict (cellophane (filled-ellipse 50 50 #:color "red") 0.25)
                             dc 0 0))
                60.0 60.0))
  (define e (first (display-page-items page)))
  (check-true (it:ellipse? e) "a faded ellipse is still an ellipse")
  (check-= (rgba*-a (fill:solid-color (it:ellipse-fill e))) 0.25 1e-6
           "and it carries the fade")
  ;; They nest, because each `start-alpha` is a factor on the alpha in force.
  (define nested (pict->display-page
                  (lambda (dc)
                    (draw-pict (cellophane (cellophane (filled-ellipse 50 50 #:color "red")
                                                       0.5)
                                           0.4)
                               dc 0 0))
                  60.0 60.0))
  (check-= (rgba*-a (fill:solid-color (it:ellipse-fill (first (display-page-items nested)))))
           0.2 1e-6 "two fades multiply"))

;; An empty paragraph keeps its height.
;;
;; A run with no text writes no `<a:r>`, so the paragraph holding it writes
;; none -- and a paragraph with no run takes its line height from
;; `endParaRPr`. Without one, the blank line between two 60pt lines came back
;; the height of the default text and pulled everything under it up.
(let ()
  (define dir (build-path work "empty-para"))
  (make-directory* dir)
  (define pptx (build-path dir "out.pptx"))
  (define (line s) (para* #:line-spacing '(percent . 0.8) (run* s #:size 60.0)))
  (define box (textbox #:width 400.0 #:height 300.0 #:wrap? #f
                       (line "first") (line "") (line "third")))
  (picts->pptx (list (slide-canvas #:width 640.0 #:height 480.0 (at 20.0 20.0 box)))
               pptx)
  (define read-back (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define paras
    (let* ([s (first (deck-slides read-back))]
           [e (first (slide-elements s))])
      (text-body-paras (shape-body e))))
  (check-equal? (length paras) 3 "three paragraphs came back")
  (check-equal? (map (lambda (p) (trun-text (first (para-runs p)))) paras)
                '("first" "" "third") "the middle one is still empty")
  (check-equal? (map (lambda (p) (trun-size (first (para-runs p)))) paras)
                '(60.0 60.0 60.0) "and still 60pt, so it still holds a line"))

;; A deck with images has to come out with media parts in the package. This
;; guards a real bug: a generated program resolves its images relative to
;; itself, which it cannot work out when loaded with `dynamic-require`, so the
;; pictures silently vanished from the export.
(let ()
  (define dir (build-path work "media-check"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "04-pictures-groups.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define picts
    (load-program-picts program))
  (define out (build-path dir "out.pptx"))
  (picts->pptx picts out #:width (deck-width d) #:height (deck-height d))
  (define media
    (filter (lambda (n) (regexp-match? #rx"^ppt/media/" n)) (zip-entry-names out)))
  (check-true (>= (length media) 2)
              (format "the exported package carries its images, found ~a" media)))

;; The strong check on the semantic path: a deck exported from a generated
;; program should reproduce the *original* deck, because it hands PowerPoint back
;; the same presets and text bodies it had. This scores better than our own
;; renderer does against the same original, which is the whole point of knowing
;; an element's structure rather than only its ink.
(define semantic-budgets
  ;; LibreOffice 24.2 with Carlito measures the second placeholder page at
  ;; 0.64%; 0.7% keeps the guard above that raster/text drift without relaxing
  ;; its differing-pixel bound.
  (hash "01-placeholders" '(0.007 . 0.012)
        "02-text"         '(0.010 . 0.020)
        "03-shapes"       '(0.010 . 0.020)
        "04-pictures-groups" '(0.006 . 0.010)
        "05-realistic"    '(0.006 . 0.012)))

(for ([name (in-list (sort (hash-keys semantic-budgets) string<?))])
  (define budget (hash-ref semantic-budgets name))
  (define dir (build-path work (string-append name "-semantic")))
  (make-directory* dir)
  (define pptx (build-path decks-dir (string-append name ".pptx")))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  ;; Go through the emitted program, so this exercises what a user would run.
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define picts
    (load-program-picts program))
  (for ([p (in-list picts)])
    (check-true (semantic-page? p)
                (format "~a: a generated slide carries its structure" name)))
  (define exported (build-path dir "exported.pptx"))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (define theirs (rasterize-pdf (libreoffice-pdf exported (build-path dir "ref"))
                                (build-path dir "theirs") #:dpi 96))
  (define original (rasterize-pdf (libreoffice-pdf pptx (build-path dir "orig"))
                                  (build-path dir "orig-page") #:dpi 96))
  (printf "~a (semantic, vs the original deck)\n" name)
  (for ([a (in-list original)] [b (in-list theirs)] [i (in-naturals 1)])
    (define-values (mae bad w h) (compare-images a b))
    (printf "  page ~a  mean err ~a%  pixels off ~a%\n" i
            (~r (* 100 mae) #:precision '(= 2)) (~r (* 100 bad) #:precision '(= 2)))
    (check-true (<= mae (car budget))
                (format "~a page ~a: ~a% within ~a%" name i
                        (~r (* 100 mae) #:precision 2) (* 100 (car budget))))
    (check-true (<= bad (cdr budget))
                (format "~a page ~a: ~a% of pixels off, budget ~a%" name i
                        (~r (* 100 bad) #:precision 2) (* 100 (cdr budget))))))

;; Structure has to survive into the package: a generated deck of preset shapes
;; and paragraphs must not come out as freeform paths and one box per word.
(let ()
  (define dir (build-path work "structure"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define out (build-path dir "out.pptx"))
  (picts->pptx (deck->picts d) out #:width (deck-width d) #:height (deck-height d))
  (define xml (slide-part out 3))
  (check-true (regexp-match? #rx"prst=\"roundRect\"" xml) "a roundRect stays a roundRect")
  (check-true (regexp-match? #rx"prst=\"rightArrow\"" xml) "an arrow stays an arrow")
  (check-false (regexp-match? #rx"<a:custGeom" xml) "no shape fell back to a path")
  (check-true (regexp-match? #rx"descr=\"glide-pptx:" xml) "elements carry their tags")
  (check-true (>= (length (regexp-match* #rx"<a:t>" xml)) 5)
              "text is text, in runs"))

(printf "export tests done; artifacts under ~a\n" work)

;; ------------------------------------------------- the background is written

;; Reported from a real deck opened in Keynote after a round trip: black
;; backgrounds, nothing readable.
;;
;; Two causes, both about a background that was never stated. `hex("FFFFFF")` is
;; a color, not a `solid-fill`, and that is exactly what generated code passes
;; to `slide_canvas` -- so the fill conversion returned #f and the slide went out
;; with no `<p:bg>` at all. The master had none either, so the whole inheritance
;; chain said nothing, and a consumer that does not assume white is free to paint
;; black. LibreOffice assumes white, which is why the pixel diffs never noticed.
(let ()
  (local-require glide-pptx/semantic glide-pptx/draw-ir glide-pptx/runtime
                 (only-in glide-pptx/sync load-program-picts))
  (define dir (build-path work "background"))
  (make-directory* dir)

  ;; The conversion itself: a bare color is a fill.
  (define canvas
    (slide-canvas #:width 480.0 #:height 270.0 #:background (hex "1F3B63")
                  (at 10.0 10.0 (shape-pict #:width 40.0 #:height 20.0
                                            #:fill (hex "ED7D31")))))
  (define page (pict->page canvas 480.0 270.0))
  (check-true (and (display-page-background page) #t)
              "a slide built with a bare color has a background to write")

  ;; And it reaches the file, at every level of the inheritance chain.
  (define out (build-path dir "bg.pptx"))
  (picts->pptx (list canvas) out #:width 480.0 #:height 270.0)
  (define slide-xml (slide-part out 1))
  (check-regexp-match #rx"<p:cSld><p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"1F3B63\"/>"
                      slide-xml
                      "the slide states its own background, first in cSld as the schema wants")
  (for ([part (in-list '("ppt/slideMasters/slideMaster1.xml"
                         "ppt/slideLayouts/slideLayout1.xml"))])
    (check-regexp-match #rx"<p:bg><p:bgPr><a:solidFill>" (zip-part out part)
                        (format "~a states a background too, so nothing has to guess" part)))

  ;; White is a background like any other: it used to be indistinguishable from
  ;; "unstated", which is the whole bug.
  (define white-canvas
    (slide-canvas #:width 480.0 #:height 270.0 #:background (hex "FFFFFF")
                  (at 10.0 10.0 (shape-pict #:width 40.0 #:height 20.0
                                            #:fill (hex "4472C4")))))
  (define out2 (build-path dir "white.pptx"))
  (picts->pptx (list white-canvas) out2 #:width 480.0 #:height 270.0)
  (check-regexp-match #rx"<p:bg><p:bgPr><a:solidFill><a:srgbClr val=\"FFFFFF\"/>"
                      (slide-part out2 1)
                      "a white background is written, not left to a default"))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.;; A deck can be written into a folder that is not there yet.
;;
;; `raco glide` keeps the deck in `.glide`, and a session that finds one left by
;; a session that did not finish clears it -- the folder with it -- before
;; writing the deck again. `zip` opens the file and does not make the way to it,
;; so the deck could not be written, and the loop then read a deck that was
;; never written and died on the way up. What it said was
;; "open-output-file: error opening file", which names neither the folder nor
;; the deck.
(let ()
  (define dir (build-path work "missing-folder"))
  (delete-directory/files dir #:must-exist? #f)
  (define out (build-path dir "not" "there" "deck.pptx"))
  (check-false (directory-exists? dir) "the folder is not there to begin with")
  (picts->pptx (list (slide-canvas #:width 480.0 #:height 270.0
                                   #:background (solid-fill (rgba 255 255 255 1.0))))
               out)
  (check-true (file-exists? out) "and the deck is written into it anyway")
  (check-equal? (length (deck-slides (pptx->deck out #:workdir (build-path dir "u")))) 1
                "and reads back as the slide it was"))


(module+ main (void (test-log #:display? #t #:exit? #t)))
