#lang racket/base
;; Import, export, import again, and compare the two intermediate
;; representations directly.
;;
;; Fidelity is otherwise measured in pixels, where a dropped attribute hides
;; easily: a colour that reverts to black on a small shape, a rotation lost on
;; something square, a run's boldness. This comparison is exact and says which
;; field of which element changed, which is the difference between "0.3%, fine"
;; and knowing what was lost.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/sync-state glide-pptx/sync glide-pptx/emit-rhombus
         glide-pptx/fonts
         "ir-diff.rkt" "deck-edit.rkt")

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-structural"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; EMU is a 12700th of a point, so that is the floor on agreement.
(define EPS 0.01)

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

(define total-diffs 0)

(for ([name (in-list decks)])
  (define dir (build-path work name))
  (make-directory* dir)
  (define original (build-path decks-dir (string-append name ".pptx")))

  (define before (pptx->deck original #:workdir (build-path dir "a")))
  (define exported (build-path dir "round.pptx"))
  (picts->pptx (deck->picts before) exported
               #:width (deck-width before) #:height (deck-height before))
  (define after (pptx->deck exported #:workdir (build-path dir "b")))

  (check-= (deck-width after) (deck-width before) 0.01 (format "~a: slide width" name))
  (check-= (deck-height after) (deck-height before) 0.01 (format "~a: slide height" name))
  (check-equal? (length (deck-slides after)) (length (deck-slides before))
                (format "~a: slide count" name))

  (define bs (deck->slide-states before #:include-inherited? #t #:descend-groups? #t))
  (define as (deck->slide-states after #:include-inherited? #t #:descend-groups? #t))

  (define reported '())
  (for ([b (in-list bs)] [a (in-list as)])
    (define by-tag (for/hash ([e (in-list (slide-state-elements a))]
                              #:when (el-state-tag e))
                     (values (el-state-tag e) e)))
    (for ([e (in-list (slide-state-elements b))])
      (define tag (el-state-tag e))
      (define hit (and tag (hash-ref by-tag tag #f)))
      (cond
        [(not tag) (void)]   ; nothing to match on; the pixel tests cover it
        [(not hit)
         (set! reported (cons (format "slide ~a: ~s vanished"
                                      (slide-state-index b) tag)
                              reported))]
        [else
         (for ([d (in-list (element-diffs e hit))])
           (set! reported (cons (format "slide ~a: ~s ~a" (slide-state-index b) tag d)
                                reported)))])))
  (set! total-diffs (+ total-diffs (length reported)))
  (printf "~a: ~a element~a, ~a difference~a\n"
          (~a name #:min-width 20)
          (for/sum ([s (in-list bs)]) (length (slide-state-elements s)))
          (if (= 1 (for/sum ([s (in-list bs)]) (length (slide-state-elements s)))) "" "s")
          (length reported) (if (= 1 (length reported)) "" "s"))
  (for ([r (in-list (reverse reported))]) (printf "    ~a\n" r))
  (check-equal? reported '()
                (format "~a: the representation survived the round trip" name)))

;; --------------------------------------------------- a slide the show skips

;; Hide Slide in PowerPoint, Skip Slide in Keynote: the slide stays in the deck
;; and is passed over in the show. It is `show="0"` in the file, and a program
;; says `~hidden: #true` -- which the PDF and the slideshow honour, since those
;; are the show.
(let ()
  (define dir (build-path work "hidden-slide"))
  (make-directory* dir)
  (define (a-slide i hidden?)
    (slide i "" 720.0 540.0 (solid-fill (rgba 255 255 255 1.0)) '()
           (list (shape (+ 1 i) (format "Box~a" i) (bbox 60.0 60.0 120.0 80.0 0.0 #f #f)
                        (preset-geom "rect" '()) (solid-fill (rgba 200 100 50 1.0)) #f #f #f))
           hidden? #f))
  (define d (deck 720.0 540.0 (list (a-slide 1 #f) (a-slide 2 #t)) #f "test"))
  (define out (build-path dir "d.pptx"))
  (picts->pptx (deck->picts d) out #:width 720.0 #:height 540.0)
  (check-regexp-match #rx"show=\"0\"" (deck-part out "ppt/slides/slide2.xml"))

  (check-false (regexp-match? #rx"show=\"0\"" (deck-part out "ppt/slides/slide1.xml"))
               "and says nothing about the one that is not")
  (define back (pptx->deck out #:workdir (build-path dir "u")))
  (check-equal? (map slide-hidden? (deck-slides back)) '(#f #t)
                "and it comes back that way")
  ;; The show skips it; the deck keeps it.
  (check-equal? (length (shown-picts (deck->picts back))) 1 "one slide to show")
  (check-equal? (length (deck-slides back)) 2 "two in the deck"))

;; ------------------------------------------- a path that declares no space

;; `<a:path>` may leave out `w` and `h`, and several real decks do: the
;; coordinates are then EMU inside the shape rather than a space to stretch
;; onto it. The reader used to floor that to a space of 1, which is a stretch
;; by the shape's own size -- a twelve-thousand-fold blow-up, drawn far off the
;; slide. Nothing caught it: our own writer always states a space, so a round
;; trip never produced one to read.
(let ()
  (define dir (build-path work "pathless-space"))
  (make-directory* dir)
  (define d
    (deck 720.0 540.0
          (list (slide 1 "" 720.0 540.0 (solid-fill (rgba 255 255 255 1.0)) '()
                       (list (shape 2 "Path" (bbox 100.0 100.0 200.0 100.0 0.0 #f #f)
                                    (custom-geom
                                     (list (list (list 'move (cons 0 0))
                                                 (list 'line (cons 21600 21600))
                                                 (list 'line (cons 10800 5400))))
                                     21600 21600)
                                    (solid-fill (rgba 200 100 50 1.0)) #f #f #f))
                       #f #f))
          #f "test"))
  (define stated (build-path dir "stated.pptx"))
  (picts->pptx (deck->picts d) stated #:width 720.0 #:height 540.0)
  ;; The same deck with the space left out, which is what those decks write.
  (define bare (build-path dir "bare.pptx"))
  (copy-file stated bare #t)
  (check-true (edit-slide-part! bare 1 #px"<a:path w=\"[0-9]+\" h=\"[0-9]+\">" "<a:path>")
              "the path space was left out")

  (define (path-facts pptx into)
    (define back (pptx->deck pptx #:workdir (build-path dir into)))
    (define e (first (slide-elements (first (deck-slides back)))))
    (define g (shape-geom e))
    (define b (element-bbox e))
    (list (custom-geom-w g) (custom-geom-h g) (bbox-w b) (bbox-h b)
          (for/list ([cmd (in-list (first (custom-geom-paths g)))])
            (map (lambda (pt) (if (pair? pt) (cons (car pt) (cdr pt)) pt)) (cdr cmd)))))

  (define bare-facts (path-facts bare "a"))
  (check-equal? (first bare-facts) 0 "which the reader keeps as no space")
  ;; Written out again, the path has to land inside the shape it belongs to
  ;; rather than thousands of times outside it.
  (define again (build-path dir "again.pptx"))
  (define back (pptx->deck bare #:workdir (build-path dir "b")))
  (picts->pptx (deck->picts back) again #:width 720.0 #:height 540.0)
  (define facts (path-facts again "c"))
  (define space-w (first facts))
  (define widest (for*/fold ([m 0]) ([cmd (in-list (fifth facts))] [pt (in-list cmd)])
                   (max m (car pt))))
  (check-true (<= widest (* 1.01 space-w))
              (format "the path stays inside its shape: ~a of ~a" widest space-w)))

;; ------------------------------------------------- a picture used as a fill

;; A shape whose fill is a picture exported as no fill at all: nothing in the
;; drawing description held the file, so the picture was dropped on the way
;; out -- and since the deck is rewritten from the program on every save, the
;; first one lost it. The program renders it, so this only ever showed up in
;; the file.
(define-runtime-path media-dir "media")

(let ()
  (define dir (build-path work "image-fill"))
  (make-directory* (build-path dir "media"))
  (copy-file (build-path media-dir "checker.png")
             (build-path dir "media" "checker.png") #t)
  (define program (build-path dir "f.rhm"))
  (define deck (build-path dir "f.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def media = media_lookup(\"media\")"
          "def slide_1 = slide_canvas("
          "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
          "  at(60.0, 60.0, ~tag: \"Filled\","
          "     shape_pict(~width: 200.0, ~height: 150.0,"
          "                ~fill: image_fill(media(\"checker.png\"), 0.5)))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (define d (pptx->deck deck #:workdir (build-path dir "w")))
  (define e (first (slide-elements (first (deck-slides d)))))
  (check-true (image-fill? (shape-fill e)) "the fill came back as a picture")
  (check-= (image-fill-opacity (shape-fill e)) 0.5 0.01 "with its opacity")
  ;; The importer names a part rather than a path, so the file it points at is
  ;; the one the package holds.
  (check-regexp-match #rx"^ppt/media/" (format "~a" (image-fill-src (shape-fill e))))
  (check-true (file-exists? (build-path dir "w" (image-fill-src (shape-fill e))))
              "and the file it names is in the package"))

(printf "structural tests done; ~a difference~a in total\n"
        total-diffs (if (= 1 total-diffs) "" "s"))


;; ---------------------------------------------------------------- builds

;; A slide the presenter advances through is several slides here. PowerPoint
;; and Keynote both record a build as a timeline of clicks, each naming the
;; shapes it brings in; a program holds still slides and nothing else, so the
;; only faithful thing a translation can do is hand back one slide per click.
;;
;; The timing is written into a fixture rather than kept as one, because what
;; is being tested is the reading of it and a hand-written block says exactly
;; what is being read.
(let ()
  (define dir (build-path work "builds"))
  (make-directory* dir)
  (define deck (build-path dir "built.pptx"))
  (copy-file (build-path decks-dir "03-shapes.pptx") deck #t)
  ;; Two clicks: the rounded rectangle appears on the first, the oval on the
  ;; second. Everything else is there from the start.
  (define timing
    (string-append
     "<p:timing><p:tnLst><p:par><p:cTn id=\"1\" dur=\"indefinite\" restart=\"never\""
     " nodeType=\"tmRoot\"><p:childTnLst><p:seq concurrent=\"1\" nextAc=\"seek\">"
     "<p:cTn id=\"2\" dur=\"indefinite\" nodeType=\"mainSeq\"><p:childTnLst>"
     (apply string-append
            (for/list ([spid (in-list '("3" "4"))])
              (string-append
               "<p:par><p:cTn nodeType=\"clickEffect\"><p:childTnLst>"
               "<p:set><p:cBhvr><p:tgtEl><p:spTgt spid=\"" spid "\"/></p:tgtEl>"
               "<p:attrNameLst><p:attrName>style.visibility</p:attrName></p:attrNameLst>"
               "</p:cBhvr><p:to><p:strVal val=\"visible\"/></p:to></p:set>"
               "</p:childTnLst></p:cTn></p:par>")))
     "</p:childTnLst></p:cTn></p:seq></p:childTnLst></p:cTn></p:par></p:tnLst></p:timing>"))
  (check-true (and (edit-slide-part! deck 1 #px"</p:cSld>" (string-append "</p:cSld>" timing)) #t)
              "a build was written into the fixture")

  (define plain
    (parameterize ([current-build-frames? #f])
      (pptx->deck deck #:workdir (build-path dir "u1"))))
  (define built
    (parameterize ([current-build-frames? #t])
      (pptx->deck deck #:workdir (build-path dir "u2"))))
  ;; The fixture has a second slide with no build, which stays one slide.
  (check-equal? (length (deck-slides plain)) 2 "read as it is, the deck has two slides")
  (check-equal? (length (deck-slides built)) 4
                "and four with the build split out: one per click, and one before")

  (define counts (for/list ([s (in-list (take (deck-slides built) 3))])
                   (length (slide-elements s))))
  (check-equal? (length (remove-duplicates counts)) 3 "each frame holds one more than the last")
  (check-equal? counts (sort counts <) "and they grow in click order")
  (check-equal? (- (third counts) (first counts)) 2 "by exactly the two shapes that appear")
  ;; The frames say which of the slide they are, so the program reads as a
  ;; build rather than as three slides that happen to look alike.
  (check-regexp-match #rx"1 of 3" (slide-name (first (deck-slides built))))
  (check-regexp-match #rx"3 of 3" (slide-name (third (deck-slides built)))))

;; An edit to one frame of a build is an edit to the build.
;;
;; The frames are the same shapes drawn again -- a still slide is all a program
;; can hold -- so moving a shape on one of them and not the others is not
;; something anyone means: the build would jump as it played. And a shape added
;; part way through a build stays for the rest of it, which is what a build is.
(let ()
  (define dir (build-path work "build-edits"))
  (make-directory* dir)
  (define deck (build-path dir "built.pptx"))
  (copy-file (build-path decks-dir "03-shapes.pptx") deck #t)
  (define timing
    (string-append
     "<p:timing><p:tnLst><p:par><p:cTn id=\"1\" dur=\"indefinite\" restart=\"never\""
     " nodeType=\"tmRoot\"><p:childTnLst><p:seq concurrent=\"1\" nextAc=\"seek\">"
     "<p:cTn id=\"2\" dur=\"indefinite\" nodeType=\"mainSeq\"><p:childTnLst>"
     (apply string-append
            (for/list ([spid (in-list '("3" "4"))])
              (string-append
               "<p:par><p:cTn nodeType=\"clickEffect\"><p:childTnLst>"
               "<p:set><p:cBhvr><p:tgtEl><p:spTgt spid=\"" spid "\"/></p:tgtEl>"
               "<p:attrNameLst><p:attrName>style.visibility</p:attrName></p:attrNameLst>"
               "</p:cBhvr><p:to><p:strVal val=\"visible\"/></p:to></p:set>"
               "</p:childTnLst></p:cTn></p:par>")))
     "</p:childTnLst></p:cTn></p:seq></p:childTnLst></p:cTn></p:par></p:tnLst></p:timing>"))
  (void (edit-slide-part! deck 1 #px"</p:cSld>" (string-append "</p:cSld>" timing)))
  (define program (build-path dir "p.rhm"))
  (define out (build-path dir "out.pptx"))
  (define built
    (parameterize ([current-build-frames? #t])
      (pptx->deck deck #:workdir (build-path dir "u"))))
  (write-rhombus-deck built program #:source-name "built.pptx")
  (check-regexp-match #rx"~build:" (file->string program)
                      "each frame says which build it belongs to")
  (picts->pptx (load-program-picts program) out)
  (void (sync-once program out #:workdir (build-path dir "w")))

  ;; "Rectangle 1" is on every frame, since nothing brings it in.
  (check-true (and (drag-in-deck! out 3 "Rectangle 1" 500.0 400.0) #t)
              "a shape was dragged on the last frame")
  (define moved (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied moved)) '(moved) "the drag was written")
  (check-true (sync-report-deck-behind? moved)
              "and the merge says the deck's other frames are now behind the program")
  ;; The deck is written again from the program after a merge, which is what
  ;; puts the frames back in step. Without it the next pass reads the frames
  ;; the merge just moved as a drag undoing itself.
  (picts->pptx (load-program-picts program) out)
  (define settled (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-actions settled) '() "and then it settles")
  (check-equal? (length (regexp-match* #rx"at[(]500[.]0, 400[.]0" (file->string program))) 3
                "on all three frames, not just the one that was dragged")

  ;; Added part way through: there for the rest of the build and not before.
  (check-true (and (add-shape-to-deck! out 2 "Added") #t) "something was drawn on the second frame")
  (define added (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied added)) '(added) "it was added")
  (check-equal? (length (regexp-match* #rx"~tag: \"Added\"" (file->string program))) 2
                "to that frame and the one after it, and not to the one before")
  (check-true (pair? (load-program-picts program)) "and the program still reads")

  ;; A canvas may say more than the emitter wrote -- `~transition:` is written
  ;; by hand, since a deck's transitions and the ones the show performs are not
  ;; the same set. The merge reads a canvas by keyword, so another keyword is
  ;; nothing to it; this is here because that is easy to say and easy to get
  ;; wrong, and what breaks is every edit to the slide underneath it.
  (let ()
    (define before (file->string program))
    (define with-transition
      (regexp-replace #rx"slide_canvas[(]\n  ~width:" before
                      "slide_canvas(\n  ~transition: #'left, ~width:"))
    (check-not-equal? with-transition before "a canvas was given a transition")
    (call-with-output-file program #:exists 'replace
      (lambda (o) (write-string with-transition o)))
    (picts->pptx (load-program-picts program) out)
    (void (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
    (check-true (and (drag-in-deck! out 1 "Isosceles Triangle 4" 111.0 222.0) #t)
                "and a shape on it was dragged")
    (define r (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
    (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                  "the drag was written")
    (check-regexp-match #rx"~transition: #'left" (file->string program)
                        "and the transition is still there")))

;; A slide may be written as a function, and everything still finds it.
;;
;; `def slide_1 = slide_canvas(...)` builds every slide when the program loads;
;; `fun slide_1(): slide_canvas(...)` builds one when it is shown, which is what
;; lets a talk start at slide twenty without paying for the nineteen before it.
;; The merge has to read both, or taking the faster shape would cost the editor.
(let ()
  (define dir (build-path work "lazy"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define out (build-path dir "out.pptx"))
  (define d (pptx->deck (build-path decks-dir "03-shapes.pptx")
                        #:workdir (build-path dir "u")))
  (write-rhombus-deck d program #:source-name "03-shapes.pptx")
  ;; The same program, with its slides written as functions.
  (define eager (file->string program))
  (define lazy (regexp-replace* #px"def (slide_\\d+) = slide_canvas[(]" eager
                                "fun \\1(): slide_canvas("))
  (check-not-equal? lazy eager "the slides were rewritten as functions")
  (call-with-output-file program #:exists 'replace
    (lambda (o) (write-string lazy o)))

  (check-equal? (length (load-program-picts program)) (length (deck-slides d))
                "every slide is still found, and called")
  (picts->pptx (load-program-picts program) out)
  (void (sync-once program out #:workdir (build-path dir "w")))
  (check-true (and (drag-in-deck! out 1 "Rectangle 1" 321.0 123.0) #t) "a shape was dragged")
  (define r (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "and the drag was written into the function's canvas")
  (check-regexp-match #rx"at[(]321[.]0, 123[.]0" (file->string program)
                      "where the shape now is")
  (check-regexp-match #rx"fun slide_1[(][)]: slide_canvas" (file->string program)
                      "and the program still reads as it was written"))

;; A program and the deck it writes agree, with nothing to merge.
;;
;; This is the round trip's strongest statement: translate a deck, export the
;; program that came out of it, and the merge should have nothing to say -- not
;; a move, not a restyle, not an addition. Anything it does say is a difference
;; between what the program means and what the deck holds, which is either a
;; wrong export or a wrong reading, and the next real edit will be merged on top
;; of that difference.
;;
;; It is checked twice: once against the deck the base was recorded from, and
;; once against a deck exported again afterwards, which is what the watch loop
;; does on every save. A talk with stages is checked the same way, because that
;; is where this last went wrong: a staged slide reported every shape on it as
;; newly drawn, nine hundred of them, and no fixture noticed.
(define (agrees-with-its-own-deck name program out workdir)
  (picts->pptx (load-program-picts program) out)
  (define first-pass (sync-once program out #:workdir workdir))
  (check-true (sync-report-base-written? first-pass)
              (format "~a: the first pass records a base" name))
  (define at-rest (sync-once program out #:workdir workdir))
  (check-equal? (sync-report-actions at-rest) '()
                (format "~a: nothing to merge at rest" name))
  ;; Exported again, as the loop does after every save.
  (picts->pptx (load-program-picts program) out)
  (define again (sync-once program out #:workdir workdir))
  (check-equal? (sync-report-actions again) '()
                (format "~a: nothing to merge after a fresh export" name)))

(for ([name (in-list decks)])
  (define dir (build-path work (string-append name "-agrees")))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define out (build-path dir "out.pptx"))
  (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                        #:workdir (build-path dir "u")))
  (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
  (agrees-with-its-own-deck name program out (build-path dir "w")))

;; The same, for a talk that gives a slide stages and draws a divider from bare
;; picts -- the two shapes a hand-written deck takes that a translated one does
;; not.
(let ()
  (define dir (build-path work "staged-agrees"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "  pict as pc"
              ""
              "export: slide_width slide_height all_slides"
              ""
              "def slide_width = 480.0"
              "def slide_height = 270.0"
              ""
              "fun staged_slide():"
              "  def base_canvas:"
              "    slide_canvas(~width: slide_width, ~height: slide_height,"
              "                 at(40.0, 60.0, ~tag: \"Base\","
              "                    shape_pict(~width: 80.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
              "  def stage_1:"
              "    group_pict(~width: slide_width, ~height: slide_height,"
              "               at(240.0, 60.0, ~tag: \"Revealed\","
              "                  shape_pict(~width: 80.0, ~height: 40.0, ~fill: hex(\"ED7D31\"))))"
              "  def b = pc.Pict.from_handle(base_canvas)"
              "  def s = pc.Pict.from_handle(stage_1)"
              "  def settled = b.snapshot(math.max(0, b.duration - 1), 1.0)"
              "  pc.switch(b, pc.animate(fun (t):"
              "                            pc.overlay(~horiz: #'left, ~vert: #'top,"
              "                                       settled, s.alpha(t))))"
              ""
              "// A divider: what it draws is not placed by any `at`, so there is"
              "// nothing on it to edit -- and it still has to survive the trip."
              "fun divider():"
              "  def page = slide_canvas(~width: slide_width, ~height: slide_height)"
              "  def bar = shape_pict(~width: 200.0, ~height: 60.0, ~fill: hex(\"70AD47\"))"
              "  pc.overlay(~horiz: #'left, ~vert: #'top,"
              "             pc.Pict.from_handle(page), pc.Pict.from_handle(bar))"
              ""
              "def all_slides = [staged_slide, divider]")
        "\n")
       o)))
  (agrees-with-its-own-deck "a staged talk" program
                            (build-path dir "out.pptx") (build-path dir "w")))

;; A slide built from stages is edited like any other.
;;
;; A talk gives a slide stages: a base canvas, a group per reveal, and an
;; animation that fades them in. What the program then holds is an animated
;; `Pict`, not a canvas -- but the tag an element carries rides on the pict as a
;; field, so every canvas and every group it was built from is still in there
;; with its tags. Settling the slide to the frame that shows the most of it and
;; composing those layers gives back the slide as a page: every element it
;; draws, tagged, in the order it draws them. Without that a staged slide
;; reached the deck as anonymous shapes, and nothing on it could be edited.
(let ()
  (define dir (build-path work "staged-edit"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define out (build-path dir "out.pptx"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "  pict as pc"
              ""
              "export: slide_width slide_height all_slides"
              ""
              "def slide_width = 480.0"
              "def slide_height = 270.0"
              ""
              "fun slide_1():"
              "  def base_canvas:"
              "    slide_canvas("
              "      ~width: slide_width, ~height: slide_height,"
              "      at(40.0, 60.0, ~tag: \"Base\","
              "         shape_pict(~width: 80.0, ~height: 40.0, ~fill: hex(\"4472C4\"))),"
              "      at(40.0, 160.0, ~tag: \"Twice\","
              "         shape_pict(~width: 60.0, ~height: 30.0, ~fill: hex(\"70AD47\"))))"
              "  // A stage: a group laid over the base, and one shape the base"
              "  // already drew, drawn again -- which is what a reveal does."
              "  def stage_1:"
              "    group_pict(~width: slide_width, ~height: slide_height,"
              "               at(240.0, 60.0, ~tag: \"Revealed\","
              "                  shape_pict(~width: 80.0, ~height: 40.0, ~fill: hex(\"ED7D31\"))),"
              "               at(40.0, 160.0, ~tag: \"Twice\","
              "                  shape_pict(~width: 60.0, ~height: 30.0, ~fill: hex(\"70AD47\"))))"
              "  def b = pc.Pict.from_handle(base_canvas)"
              "  def s = pc.Pict.from_handle(stage_1)"
              "  def settled = b.snapshot(math.max(0, b.duration - 1), 1.0)"
              "  pc.switch(b, pc.animate(fun (t):"
              "                            pc.overlay(~horiz: #'left, ~vert: #'top,"
              "                                       settled, s.alpha(t))"
              "                              .refocus(settled)))"
              ""
              "def all_slides = [slide_1]")
        "\n")
       o)))
  (define states (program-slide-states program))
  (check-equal? (length states) 1 "one slide")
  (define tags (map el-state-tag (slide-state-elements (first states))))
  (check-equal? (sort tags string<?) '("Base" "Revealed" "Twice")
                (format "the base's elements and the stage's, all of them tagged: ~s" tags))

  (picts->pptx (load-program-picts program) out)
  (void (sync-once program out #:workdir (build-path dir "w")))

  ;; An element the stage introduced: dragging it lands in the stage.
  (check-true (and (drag-in-deck! out 1 "Revealed" 300.0 200.0) #t) "the stage's own shape was dragged")
  (define r (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                (format "and the drag was written: ~s" (map (lambda (s) (cdr s)) (sync-report-skipped r))))
  (check-regexp-match #rx"at[(]300[.]0, 200[.]0, ~tag: \"Revealed\"" (file->string program)
                      "into the `at` form the stage holds")

  ;; And a shape the base and the stage both draw moves as one: they are the
  ;; same shape, and moving one and not the other is not something anyone means.
  (check-true (and (drag-in-deck! out 1 "Twice" 111.0 222.0) #t) "the twinned shape was dragged")
  (define r2 (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(moved) "the drag was written")
  (check-equal? (length (regexp-match* #rx"at[(]111[.]0, 222[.]0" (file->string program))) 2
                "to both the base's copy and the stage's")
  (check-true (pair? (load-program-picts program)) "and the program still reads"))

;; A slide the program does not lay out is left alone, and does not stop the
;; rest of the save.
;;
;; A talk gives a slide stages, and what the name then holds is an animated pict
;; built from canvases rather than a canvas. Its elements cannot be read back --
;; the drawing is flattened -- and there would be no one `at` form to write them
;; to. Compared as an empty slide, every shape the deck draws on it looked newly
;; added: on one real talk, nine hundred refusals, and since a save lands whole
;; or not at all, nothing else could be merged either. Not even dragging the
;; slides about in the navigator, which touches no element at all.
(let ()
  (define dir (build-path work "staged-slide"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define out (build-path dir "out.pptx"))
  (define (canvas x tag)
    (format (string-append "slide_canvas(\n  ~~width: slide_width, ~~height: slide_height,\n"
                           "  at(~a, 100.0, ~~tag: ~s,\n"
                           "     shape_pict(~~width: 80.0, ~~height: 40.0, ~~fill: hex(\"4472C4\"))))")
            x tag))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "  pict as pc"
              ""
              "export: slide_width slide_height all_slides slide_1 slide_2 slide_3"
              ""
              "def slide_width = 720.0"
              "def slide_height = 540.0"
              ""
              (format "def slide_1 = ~a" (canvas "50.0" "One"))
              "// The slide with stages: an animated pict, not a canvas."
              "def slide_2:"
              (format "  def base = pc.Pict.from_handle(~a)" (canvas "150.0" "Two"))
              "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
              (format "def slide_3 = ~a" (canvas "250.0" "Three"))
              ""
              "def all_slides = [slide_1, slide_2, slide_3]")
        "\n")
       o)))
  (check-equal? (length (load-program-picts program)) 3 "three slides, one of them staged")
  (picts->pptx (load-program-picts program) out)
  (void (sync-once program out #:workdir (build-path dir "w")))

  ;; Drag the last slide to the front, as one does in the navigator.
  (check-true (and (move-slide! out 3 1) #t) "the slides were reordered in the deck")
  (define r (sync-once program out #:workdir (build-path dir "w") #:atomic? #t))
  (define kinds (map sync-action-kind (sync-report-actions r)))
  (check-false (memq 'added kinds)
               (format "nothing on the staged slide is reported as newly drawn: ~a" kinds))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(reordered)
                "and the reorder is written")
  (check-equal? (sync-report-skipped r) '() "with nothing refused")
  (check-regexp-match #rx"all_slides = \\[slide_3, slide_1, slide_2\\]" (file->string program)
                      "the program's slide list says the deck's order")
  (check-true (pair? (load-program-picts program)) "and the program still reads"))

;; A program says which typefaces it needs, and stops if they are not there.
;;
;; racket/draw substitutes silently, and a substitute is not cosmetic: the
;; leading here is a percentage of the font size, so it stays where it was while
;; the ink moves. A deck drawn in the wrong face is worse than one that refuses
;; to draw, because only one of the two tells you.
(let ()
  (define dir (build-path work "fonts"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define d (pptx->deck (build-path decks-dir "06-drawn.pptx")
                        #:workdir (build-path dir "u")))
  (check-equal? (deck-font-families d) '("DejaVu Sans Mono" "DejaVu Sans")
                "the families the deck's own text names, in the order they appear")
  (write-rhombus-deck d program #:source-name "06-drawn.pptx")
  (define src (file->string program))
  (check-regexp-match #rx"register_fonts[(]\"fonts\"[)]" src
                      "fonts beside the program are loaded before anything is drawn")
  (check-regexp-match #rx"check_fonts[(]" src "and the program says which it needs")
  (for ([f (in-list (deck-font-families d))])
    (check-regexp-match (regexp (format "\\[~s\\]" f)) src
                        (format "~a is named" f)))
  (check-true (pair? (load-program-picts program))
              "and it runs, because these fonts are here")

  ;; The refusal, which is the point of writing the check at all.
  (define missing (build-path dir "missing.rhm"))
  (call-with-output-file missing #:exists 'replace
    (lambda (o) (write-string (string-replace src "[\"DejaVu Sans\"]" "[\"Nonexistent Face 91\"]") o)))
  (define why
    (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
      (load-program-picts missing)
      #f))
  (check-true (and why (regexp-match? #rx"Nonexistent Face 91" why))
              (format "a font that is not there stops the program: ~a" why))

  ;; And a font file beside the program is loaded from there. Any installed file
  ;; will do to show that the loading works -- an uninstalled one would show
  ;; more, and there is not one of those to be sure of on every machine.
  (define sample (font-file-for "DejaVu Sans"))
  (cond
    [(not sample) (printf "no fc-match; bundled fonts are not exercised\n")]
    [else
     (define bundle (build-path dir "fonts"))
     (make-directory* bundle)
     (copy-file sample (build-path bundle (file-name-from-path sample)) #t)
     (check-equal? (register-fonts! bundle) 1 "the font file beside the program was loaded")]))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
