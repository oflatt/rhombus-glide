#lang racket/base
;; Showing a converted deck: `staged.rhm`, and the slide that has been given
;; stages by hand.
;;
;; A converted deck is meant to be rewritten, and the first thing anyone does
;; by hand is give a slide stages -- which turns that slide from a canvas into
;; an animated `Pict`. Everything downstream of the canvas has to take both, and
;; the two places that did not were the show, which called `Pict.from_handle` on
;; something that was already a `Pict`, and the backup PDF, which drew the first
;; frame of an animation and called that the slide.
;;
;; The show needs a display; the PDF does not, and is checked either way.
(require rackunit/log)
(require rackunit racket/file racket/list racket/path racket/system racket/port
         racket/string racket/runtime-path racket/class racket/draw pict
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "deck-edit.rkt")

(define-runtime-path here ".")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-staged"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; `exec-file` is however racket was invoked, which is a bare name when it came
;; off the PATH -- and `system*` cannot exec one of those.
(define racket-exe
  (let ([e (find-system-path 'exec-file)])
    (if (absolute-path? e) e (or (find-executable-path e) e))))
;; `xvfb-run` first even where DISPLAY is set: a DISPLAY that names a server
;; nobody is running does not fail, it hangs, and a test that hangs is worse
;; than one that says it could not run.
(define xvfb (find-executable-path "xvfb-run"))
(define display? (or xvfb (getenv "DISPLAY")))

;; Runs a Rhombus program and hands back what it printed.
(define (run-rhm src #:display? [needs-display? #f])
  (define path (build-path work (format "p~a.rhm" (equal-hash-code src))))
  (call-with-output-file path #:exists 'replace (lambda (o) (write-string src o)))
  (define out (open-output-string))
  (define err (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port err])
      (cond
        [(and needs-display? xvfb) (system* xvfb "-a" racket-exe path)]
        [else (system* racket-exe path)])))
  (values ok? (get-output-string out) (get-output-string err)))

(define PROLOGUE
  (string-join
   '("#lang rhombus/and_meta"
     "import:"
     "  pict open"
     "  lib(\"racket/base.rkt\") as rkt"
     "  lib(\"glide-pptx/runtime.rhm\") as glide"
     ""
     "def w = 320.0"
     "def h = 240.0"
     "def canvas = glide.slide_canvas(~width: w, ~height: h)"
     "def panned = glide.slide_canvas(~width: w, ~height: h, ~transition: #'left)"
     "def hidden = glide.slide_canvas(~width: w, ~height: h, ~hidden: #true)"
     "// A slide given stages by hand: three advances, and not a canvas any more."
     "def with_stages:"
     "  def base = Pict.from_handle(canvas)"
     "  switch(base, animate(fun (t): base.alpha(t)), animate(fun (t): base.alpha(t)))"
     "// Three epochs that all animate, so that dropping one off either end"
     "// leaves the same two: `~before: -1` moves the first off the front of the"
     "// timeline, which is how a pict says it has an arrival to play, and"
     "// `~after: -1` drops the last, which is those two epochs and no arrival."
     "def all_moving:"
     "  def base = Pict.from_handle(canvas)"
     "  switch(animate(fun (t): base.alpha(t)),"
     "         animate(fun (t): base.alpha(t)),"
     "         animate(fun (t): base.alpha(t)))"
     "def with_lead = all_moving.time_pad(~before: -1)"
     "def no_lead = all_moving.time_pad(~after: -1)"
     "")
   "\n"))

;; One program, one run: a Rhombus module costs more to compile than everything
;; asked of it here, so all of it is asked at once.
(define PROGRAM
  (string-append
   PROLOGUE
   (string-join
    (list
     "import:"
     "  lib(\"slideshow/main.rkt\") as ss"
     "  // For `sliderec_title`, which is what `a` and `s` navigate by and is"
     "  // not something `slideshow/main` hands out."
     "  lib(\"slideshow/core.rkt\") as core"
     "  lib(\"glide-pptx/show.rhm\") open"
     ""
     "println(\"plain \" +& glide.transition_of(canvas))"
     "println(\"panned \" +& glide.transition_of(panned))"
     "println(\"staged \" +& glide.transition_of(with_stages))"
     "println(\"advances \" +& with_stages.duration)"
     ""
     (format "glide.deck_to_pdf([canvas, hidden, with_stages, with_lead], ~s, ~~width: w, ~~height: h)"
             (path->string (build-path work "deck.pdf")))
     ""
     "// Counted as the defaults leave them -- a cut between slides and no fade"
     "// up from blank -- so that what is counted is the slides themselves and"
     "// not the frames they would be played through."
     "fun emitted():"
     "  recur count(n = 0):"
     "    if ss.#{most-recent-slide}()"
     "    | block:"
     "        ss.#{retract-most-recent-slide}()"
     "        count(n + 1)"
     "    | n"
     "show_slides([canvas, canvas, hidden], ~width: w, ~height: h)"
     "println(\"stills \" +& emitted())"
     "// A slide that names a transition still gets one: the default is what a"
     "// slide gets when it says nothing, not a veto."
     "show_slides([canvas, panned], ~width: w, ~height: h)"
     "println(\"panned \" +& emitted())"
     "show_slides([with_stages], ~width: w, ~height: h)"
     "println(\"stages \" +& emitted())"
     "// A pict that starts before epoch 0 has an arrival, and `slide` plays it on"
     "// the press that lands on the slide rather than spending a waiting frame on"
     "// it -- so the same two epochs are worth more frames with one than without."
     "show_slides([with_lead], ~width: w, ~height: h)"
     "println(\"leadin \" +& emitted())"
     "show_slides([no_lead], ~width: w, ~height: h)"
     "println(\"nolead \" +& emitted())"
     "// And a transition arrives in the same place, so the two have to be played"
     "// in turn rather than one padded over the other. Asked of `arrive` itself:"
     "// the lead-in play is a fixed number of frames however long the epoch is, so"
     "// counting slides cannot see the difference, but the epoch's extent can."
     "def a_lead = animate(~extent: 0.5, fun (n): Pict.from_handle(canvas).alpha(n))"
     "def (arrived, _l1) = arrive(a_lead, with_lead)"
     "def (arrived_plain, _l2) = arrive(a_lead, no_lead)"
     "println(\"arrive own \" +& arrived.epoch_extent(-1)"
     "          +& \" plain \" +& arrived_plain.epoch_extent(-1)"
     "          +& \" duration \" +& arrived.duration +& \"/\" +& no_lead.duration)"
     "// The title every page carries, which `a` and `s` group by. Collected"
     "// while retracting, so it also clears the slides it counted."
     "fun titles():"
     "  recur go(acc = []):"
     "    def s = ss.#{most-recent-slide}()"
     "    if s"
     "    | block:"
     "        ss.#{retract-most-recent-slide}()"
     "        go([core.#{sliderec-title}(s), & acc])"
     "    | acc"
     "show_slides([with_stages, canvas], ~width: w, ~height: h)"
     "def ts = titles()"
     "def groups = for Map (t in ts): values(t, #true)"
     "println(\"titles \" +& ts.length() +& \" pages \" +& groups.length() +& \" groups\")"
     "// Whether the number a slide is known by is drawn is asked of the pixels,"
     "// in `numbers-are-not-drawn` below: two pages the same height is what this"
     "// used to ask, and a page with a title over it is the same height as one"
     "// without -- so it passed while the number was being drawn on every slide."
     "// And with the reveal turned on, a still slide is faded up rather than cut"
     "// to, which costs it an advance."
     "set_reveal(#true)"
     "show_slides([canvas, panned], ~width: w, ~height: h)"
     "println(\"revealed \" +& emitted())"
     ""
     "// A slide written as a function is built when it is shown and not before,"
     "// which is what makes starting part way through worth anything."
     "def built = Array(0)"
     "fun lazy_slide(n):"
     "  fun ():"
     "    built[0] := built[0] + n"
     "    canvas"
     "set_reveal(#false)"
     "// The slide number counts up across the whole program, so `start_from` is"
     "// set past every slide there could be rather than to a number this test"
     "// would have to know."
     "set_start_from(100000)"
     "show_slides([lazy_slide(1), lazy_slide(10)], ~width: w, ~height: h)"
     "println(\"skipped: built \" +& built[0] +& \", shown \" +& emitted())"
     "set_start_from(0)"
     "show_slides([lazy_slide(100)], ~width: w, ~height: h)"
     "println(\"kept: built \" +& built[0] +& \", shown \" +& emitted())"
     "// A program that has registered slides ends by showing them, and a show"
     "// waits for a keypress that is not coming. Everything asked of it has been"
     "// answered by here, so leave rather than open a window nobody is at."
     "Port.Output.flush()"
     "rkt.#{exit}(0)")
    "\n")
   "\n"))

(cond
  [(not display?)
   (printf "no display and no xvfb-run; the show is not checked\n")]
  [else
   (define-values (ok? out err) (run-rhm PROGRAM #:display? #t))
   (check-true ok? (format "the program ran: ~a" err))

   ;; ------------------------------------------ what the canvas remembers
   (check-regexp-match #rx"plain #false" out "a canvas that named no transition says so")
   (check-regexp-match #rx"panned left" out "and one that named a transition remembers it")
   ;; A slide with stages is no longer the canvas, and nothing pretends otherwise:
   ;; the show falls back to its own default rather than reading through a wrapper.
   (check-regexp-match #rx"staged #false" out "a slide with stages is not a canvas")
   (check-regexp-match #rx"advances 3" out "and it is three advances long")

   ;; ------------------------------------------------------------ the show
   ;; Three slides in, one of them hidden. A slide with stages reaches `slide`
   ;; as a `Pict`, which is what used to raise here.
   ;; Two slides, one advance each: by default a slide is cut to, not faded up,
   ;; and not panned to either. A deck behaves the way it did in PowerPoint
   ;; unless the talk asks for something else.
   (check-regexp-match #rx"stills 2" out "a hidden slide is not shown, and a still slide is one slide")
   (check-regexp-match #px"panned ([3-9]|[0-9][0-9]+)" out
                       "and a slide that asks to be panned to is played into")
   ;; An animated epoch is played as several pages, so this counts pages and not
   ;; presses. The number is exact because the hold is what is being checked:
   ;; the last epoch of an animated slide plays as the transition off it, so the
   ;; show sustains the final frame rather than letting the move-on eat it, and
   ;; that is one page more than the same slide shown without it -- 24 here,
   ;; where it would be 23. A still slide is not held, which is `stills 2`.
   (check-regexp-match #rx"stages 24" out
                       "a slide with stages is played out, with its last frame held")
   (check-regexp-match #px"revealed ([3-9]|[0-9][0-9]+)" out
                       "and with the reveal on, a still slide is faded up rather than cut to")

   ;; ----------------------------------------------------------- the arrival
   ;; `time_pad(~before: -1)` moves a pict's first epoch off the front of the
   ;; timeline, which is what `pan_transition` hands back and what a slide that
   ;; animates itself in can hand back too. Slideshow plays that epoch on the
   ;; press that lands on the slide, skipping both ends, so it costs no waiting
   ;; frame of its own -- and the two picts compared here have the same two
   ;; epochs, so every frame of the difference is the arrival being played.
   ;;
   ;; Nothing here asks for that: a pict that starts before epoch 0 says so, and
   ;; `slide` reads it. This is the check that it does.
   (let ([l (regexp-match #px"leadin ([0-9]+)" out)]
         [n (regexp-match #px"nolead ([0-9]+)" out)])
     (check-true (and l n #t) "both were shown")
     (when (and l n)
       (check-true (> (string->number (cadr l)) (string->number (cadr n)))
                   "a pict that starts before epoch 0 has its arrival played")))

;; A transition arrives in the same place, and `switch` carries only epochs 0
   ;; and up -- so a lead padded in front of a slide that already had an arrival
   ;; used to drop that arrival on the floor, silently. `arrive` plays the two in
   ;; turn within the one epoch instead: its extent is both of them, and the
   ;; slide is no longer than it was.
   (let ([m (regexp-match #px"arrive own ([0-9.]+) plain ([0-9.]+) duration ([0-9]+)/([0-9]+)" out)])
     (check-true (and m #t) "`arrive` was asked both ways")
     (when m
       (define own (string->number (cadr m)))
       (define plain (string->number (caddr m)))
       (check-equal? plain 0.5 "a slide with no arrival of its own is just the lead")
       (check-true (> own plain)
                   "and one that brought an arrival plays that too, after the lead")
       (check-equal? (string->number (cadddr m)) (string->number (list-ref m 4))
                     "neither costs the slide an epoch")))

   ;; ------------------------------------------------------- a and s navigate
   ;; `s` skips to the next slide with a different title and `a` back to the
   ;; start of the previous group, so the title is what decides how far a press
   ;; of either goes. Every slide had the same one -- `#false` -- and both keys
   ;; ran to the end of the talk. Now an animated slide's pages share one title
   ;; and the next slide has its own: two groups over more than two pages, which
   ;; is `s` stepping a slide at a time rather than a frame at a time.
   (let ([m (regexp-match #px"titles ([0-9]+) pages ([0-9]+) groups" out)])
     (check-true (and m #t) "the pages say what they are titled")
     (when m
       (check-equal? (string->number (caddr m)) 2
                     "an animated slide and a still one are two groups")
       (check-true (> (string->number (cadr m)) 2)
                   "over more pages than that, which is what the grouping is for")))

   ;; Nothing is built for the slides that are skipped, and the one that is
   ;; shown is built when it is shown. Starting part way through a real talk is
   ;; six of the nine seconds it takes to start.
   (check-regexp-match #rx"skipped: built 0, shown 0" out
                       "a slide the show skips is never built")
   (check-regexp-match #rx"kept: built 100, shown 1" out
                       "and the one it shows is")

   ;; --------------------------------------------------------- the backup PDF
   (define pdf (build-path work "deck.pdf"))
   (check-true (file-exists? pdf) "there is a PDF")
   ;; Counted with `pdfinfo` rather than by reading the file: the page objects
   ;; are in a compressed stream, so there is nothing in the bytes to count.
   (define pdfinfo (find-executable-path "pdfinfo"))
   (cond
     [(not (and pdfinfo (file-exists? pdf)))
      (printf "no pdfinfo; the pages are not counted\n")]
     [else
      (define out (open-output-string))
      (parameterize ([current-output-port out]) (system* pdfinfo (path->string pdf)))
      (define m (regexp-match #px"Pages:\\s*(\\d+)" (get-output-string out)))
      ;; One for the canvas, three for the three advances, none for the hidden
      ;; one -- and three for the lead-in slide, whose two epochs are two pages
      ;; and whose arrival lands on a third. Without that last one the backup
      ;; would be missing the picture the slide comes to rest on.
      (check-equal? (and m (string->number (cadr m))) 7
                    "an advance is a page, an arrival lands on one, and a hidden slide is not")])])

;; A program can be loaded twice in one process.
;;
;; `load-program-picts` reads a program in a namespace of its own, because a
;; second `dynamic-require` in the same one hands back the instance it already
;; has and the merge would never see its own patch. But a talk whose helpers
;; import slideshow pulls in racket/gui, which cannot be instantiated twice in a
;; process -- and the watch loop loads the program again on every save. So the
;; second save died on a program that drew through slideshow, which is to say on
;; a talk.
(cond
  [(not display?)
   (printf "no display and no xvfb-run; loading twice is not checked\n")]
  [else
   (define dir (build-path work "twice"))
   (make-directory* dir)
   (define program (build-path dir "p.rhm"))
   (call-with-output-file program #:exists 'replace
     (lambda (o)
       (write-string
        (string-join
         (list "#lang rhombus/and_meta"
               "import:"
               "  lib(\"glide-pptx/runtime.rhm\") open"
               "  // The import that pulls in the GUI, as a talk's helpers do."
               "  slideshow as ss"
               ""
               "export: all_slides"
               "def slide_1 = slide_canvas(~width: 320.0, ~height: 240.0)"
               "def all_slides = [slide_1]")
         "\n")
        o)))
   (define driver (build-path dir "twice.rkt"))
   (call-with-output-file driver #:exists 'replace
     (lambda (o)
       (write-string
        (format "#lang racket/base\n(require glide-pptx/sync)\n~a\n~a\n~a\n"
                (format "(define p ~s)" (path->string program))
                "(printf \"first ~a\\n\" (length (load-program-picts p)))"
                "(printf \"second ~a\\n\" (length (load-program-picts p)))")
        o)))
   (define out (open-output-string))
   (define err (open-output-string))
   ;; With arguments on the command line, because `raco glide talk.rhm --app none`
   ;; has some -- and a program that imports slideshow reads them when it loads,
   ;; and refuses anything that is not a single module file. They are this
   ;; command's arguments, not the program's, and the program must not see them.
   (define ok?
     (parameterize ([current-output-port out] [current-error-port err])
       (if xvfb
           (system* xvfb "-a" racket-exe (path->string driver) "--app" "none")
           (system* racket-exe (path->string driver) "--app" "none"))))
   (check-true ok? (format "the program loaded twice: ~a" (get-output-string err)))
   (check-regexp-match #rx"first 1" (get-output-string out) "the first load found the slide")
   (check-regexp-match #rx"second 1" (get-output-string out)
                       "and so did the second, in the same process")])

;; --------------------------------------------- one slide per stage, on request
;;
;; A slide given stages is one page per stage in the show -- slideshow's own
;; doing -- and a deck that holds one flattened picture of it cannot be stepped
;; through at all. `--stages` asks for the pages instead, and what arrives has
;; to be slides with shapes on them: `export --slideshow` already produced one
;; picture per advance, which is a deck nobody can edit or print sharply.
;;
;; Off by default, and it has to stay off for the deck a session keeps: an edit
;; has to have one place to go, and a slide that appears four times is a shape
;; dragged on one of them and three that disagree.
(let ()
  (define program (build-path work "stages.rhm"))
  (display-to-file
   (string-append
    PROLOGUE
    (string-join
     (list "export: all_slides"
           "def one = glide.slide_canvas("
           "  ~width: w, ~height: h,"
           "  glide.at(20.0, 20.0, ~tag: \"Box\","
           "           glide.shape_pict(~width: 60.0, ~height: 40.0,"
           "                            ~fill: glide.hex(\"4472C4\"))))"
           "def two:"
           "  def base = Pict.from_handle(one)"
           "  switch(base, animate(fun (t): base.alpha(t)))"
           "def all_slides = [one, two]")
     "
"))
   program #:exists 'replace)
  (define tags
    (for/list ([st (in-list (program-slide-states program))])
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (check-equal? tags '(("Box") ("Box"))
                "settled, a staged slide is one slide")
  (set-stage-slides! #t)
  (define staged
    (for/list ([st (in-list (program-slide-states program))])
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (set-stage-slides! #f)
  (check-equal? staged '(("Box") ("Box") ("Box"))
                "with stages, it is one slide per stage -- with its shapes, not a picture of them")
  ;; And the number every page of one slide carries is that slide's own, which
  ;; is what the show draws: the number is composed before slideshow makes a
  ;; page of each stage.
  (set-stage-slides! #t)
  (ask-slide-numbers! #t)
  (define numbered
    (for/list ([st (in-list (program-slide-states program))])
      (length (slide-state-elements st))))
  (ask-slide-numbers! #f)
  (set-stage-slides! #f)
  (check-equal? numbered '(2 2 2) "each page gains the number in its corner"))

;; ------------------------------------------- editing a slide stage by stage
;;
;; With one slide of the deck per stage, a shape can be put where it belongs on
;; the stage it appears on. Three things have to hold for that to be worth
;; anything:
;;
;;   * the deck has a slide per stage, and each of them can be read;
;;   * an edit made on the third stage is written to the one `at` form that
;;     draws the shape -- `all_slides` names the animation, not the canvas it
;;     was built from, so the scope is found one definition further in;
;;   * the deck is written again from the program afterwards. One form draws the
;;     shape on every stage, so the stages that were not edited still show what
;;     they showed, and without that they report the same difference back for
;;     ever.
(let ()
  (define program (build-path work "stage-edit.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "def canvas = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))),"
          "  at(120.0, 120.0, ~tag: \"Later\","
          "     shape_pict(~width: 50.0, ~height: 30.0, ~fill: hex(\"ED7D31\"))))"
          "def staged:"
          "  def base = pc.Pict.from_handle(canvas)"
          "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
          "def all_slides = [staged]")
    "\n")
   program #:exists 'replace)
  (set-stage-slides! #t)
  (define picts (load-program-picts program))
  (check-equal? (length picts) 2 "a slide of two stages is two slides of the deck")
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (check-equal? scopes '(canvas)
                "and `all_slides` is followed through the animation to the canvas")
  (define deck (build-path work "stage-edit.pptx"))
  (define w (build-path work "stage-edit-work"))
  (picts->pptx picts deck)
  (void (sync-once program deck #:workdir w))
  (check-true (drag-in-deck! deck 2 "Later" 200.0 60.0)
              "something is dragged on the second stage")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "the drag is written")
  (check-regexp-match #rx"at[(]200[.]0, 60[.]0, ~tag: \"Later\""
                      (file->string program)
                      "to the one `at` form that draws it")
  (check-true (sync-report-deck-behind? r)
              "and the deck is behind, so the other stages are written again")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "after which there is nothing left to merge")
  (set-stage-slides! #f))

;; ------------------------------------------ a shape drawn on a later stage
;;
;; Drawing a box on the third stage of a slide means a box that appears there
;; and not before. The slide's canvas cannot say that -- a canvas is one still
;; picture, and the staging is applied to it from outside -- so what is written
;; is a layer over the slide, in a `from_stage` wrapped around its entry in
;; `all_slides`. The `at` form inside it keeps its tag, which is what makes the
;; next drag land: a shape this wrote is a shape like any other.
(let ()
  (define program (build-path work "stage-add.rhm"))
  (define (fresh!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "  pict as pc"
            "export: all_slides"
            "def canvas = slide_canvas("
            "  ~width: 320.0, ~height: 240.0,"
            "  at(20.0, 20.0, ~tag: \"Box\","
            "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
            "def staged:"
            "  def base = pc.Pict.from_handle(canvas)"
            "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
            "glide_slides all_slides:"
            "  [staged]")
      "\n")
     program #:exists 'replace))
  (fresh!)
  (set-stage-slides! #t)
  (define deck (build-path work "stage-add.pptx"))
  (define w (build-path work "stage-add-work"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-equal? (add-shape-to-deck! deck 2 "Drawn late" #:x 40.0 #:y 200.0
                                    #:width 60.0 #:height 20.0)
                "Drawn late"
                "a shape is drawn on the second stage")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(added)
                "and written")
  (check-regexp-match #rx"show_as[(]staged, from_stage[(]2, staged,"
                      (file->string program)
                      "as a checked layer over the slide, appearing from that stage")
  (define (tags-on i)
    (for/first ([st (in-list (program-slide-states program))]
                #:when (= i (slide-state-index st)))
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (check-equal? (tags-on 1) '("Box") "the first stage does not hold it")
  (check-equal? (tags-on 2) '("Box" "Drawn late") "and the second does")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "the deck written from the program has nothing to merge")
  ;; And it can be dragged, like anything else with a tag and an `at`.
  (check-true (drag-in-deck! deck 2 "Drawn late" 90.0 150.0) "it is dragged")
  (define r2 (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(moved)
                "and the drag is written")
  (check-regexp-match #rx"at[(]90[.]0, 150[.]0, ~tag: \"Drawn late\""
                      (file->string program)
                      "to the `at` form the layer holds")
  (set-stage-slides! #f))

;; The same two insertion paths across source modules. A canvas nested in an
;; animated definition takes its indentation from that canvas's direct
;; arguments, while a later-stage layer rewrites the running-order entry in the
;; root module. Neither may slice offsets from the other file.
(let ()
  (define dir (build-path work "split-stage-add"))
  (make-directory* dir)
  (define slides (build-path dir "slides.rhm"))
  (define program (build-path dir "talk.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export:"
          "  first"
          "  second"
          "def first:"
          "  def canvas = slide_canvas("
          "    ~width: 320.0, ~height: 240.0,"
          "    at(20.0, 20.0, ~tag: \"First\","
          "       shape_pict(~width: 60.0, ~height: 40.0)))"
          "  def base = pc.Pict.from_handle(canvas)"
          "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
          "def second:"
          "  def canvas = slide_canvas("
          "    ~width: 320.0, ~height: 240.0,"
          "    at(120.0, 20.0, ~tag: \"Second\","
          "       shape_pict(~width: 60.0, ~height: 40.0)))"
          "  def base = pc.Pict.from_handle(canvas)"
          "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))")
    "\n")
   slides #:exists 'replace)
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  \"slides.rhm\" open"
          "export: all_slides"
          "glide_slides all_slides:"
          "  [first, second]")
    "\n")
   program #:exists 'replace)
  (set-stage-slides! #t)
  (define deck (build-path dir "talk.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-equal? (add-shape-to-deck! deck 1 "Base add" #:x 40.0 #:y 180.0)
                "Base add")
  (check-equal? (add-shape-to-deck! deck 4 "Late add" #:x 140.0 #:y 180.0)
                "Late add")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(added added)
                "both split-source insertion paths are written")
  (check-regexp-match #rx"Base add" (file->string slides)
                      "the first-stage shape went into the imported canvas")
  (check-regexp-match #rx"show_as[(]second, from_stage[(]2, second,"
                      (file->string program)
                      "the later-stage shape wrapped the root entry")
  (check-true (pair? (load-program-picts program))
              "the two files still form a valid epoch deck")
  (set-stage-slides! #f))

;; ------------------------------------------ which frame of a build settles
;;
;; Without stages a deck holds one slide per slide of the program, and the frame
;; it holds is the one that shows the most of that slide. "The most" counts
;; everything an edit could be written back to, the picts the program names
;; among them -- and a talk that draws named things over its canvas as it goes
;; has frames whose canvases are identical. Counting only the canvas tags made
;; every frame a tie, the earliest won, and the eggcc talk's e-class regions --
;; named picts, laid over the slide from the second frame on -- were absent from
;; the deck altogether, so none of them could be edited.
(let ()
  (define program (build-path work "settle-named.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "def canvas = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
          "def blob:"
          "  tag(shape_pict(~width: 40.0, ~height: 30.0,"
          "                 ~fill: hex(\"929292\", ~alpha: 0.3)), \"late\")"
          "def slide_1:"
          "  def base = pc.Pict.from_handle(canvas)"
          "  def with_blob:"
          "    pc.overlay(~horiz: #'left, ~vert: #'top, base,"
          "               pc.Pict.from_handle(blob).pad(~left: 150.0, ~top: 100.0))"
          "  pc.switch(base, with_blob)"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (check-equal? (for/list ([st (in-list (program-slide-states program))])
                  (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e)))
                '(("Box" "late"))
                "the frame that shows the named pict is the one that settles"))

;; -------------------------------------- a slide a helper builds, added to
;;
;; Half the slides of a real talk are built by helpers: `divider(0)` composes
;; over a canvas of its own, `in_section(1, s)` wraps a slide in a header. The
;; canvas is inside the helper, so there is no `slide_canvas(...)` in the
;; program to put another `at` form in -- and a box drawn on such a slide used
;; to be refused, which under the rule that a save lands whole or not at all
;; took every other edit in the save with it.
;;
;; A canvas over the slide is what a talk's own helpers do to a slide when they
;; add a header to it, and it is what gets written. Not the slide inside a
;; canvas: `at` holds a still picture, so a slide that animates could not go in
;; one, and a slide that does not would become a single flattened element with
;; everything it holds buried inside.
(let ()
  (define program (build-path work "helper-slide.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "def canvas = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
          "fun divider():"
          "  pc.Pict.from_handle(canvas)"
          "def all_slides = [divider()]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path work "helper-slide.pptx"))
  (define w (build-path work "helper-slide-work"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (check-equal? scopes '(#f) "the slide names no canvas of its own")
  (check-equal? (add-shape-to-deck! deck 1 "Drawn over" #:x 40.0 #:y 200.0
                                    #:width 60.0 #:height 20.0)
                "Drawn over"
                "a shape is drawn on it")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(added)
                "and written")
  (check-regexp-match #rx"over[(]divider[(][)]," (file->string program)
                      "as a canvas laid over the slide")
  (check-equal? (for/first ([st (in-list (program-slide-states program))]) 
                  (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e)))
                '("Box" "Drawn over")
                "the slide now holds it, and still holds what it held")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "the deck written from the program has nothing to merge")
  ;; And it can be dragged, like anything else with a tag and an `at`.
  (check-true (drag-in-deck! deck 1 "Drawn over" 100.0 120.0) "it is dragged")
  (check-equal? (map sync-action-kind
                     (sync-report-applied (sync-once program deck #:workdir w #:atomic? #t)))
                '(moved)
                "and the drag is written")
  (check-regexp-match #rx"at[(]100[.]0, 120[.]0, ~tag: \"Drawn over\""
                      (file->string program)
                      "to the `at` form the layer holds"))

;; ------------------------------------ the number a slide is known by is not drawn
;;
;; Every slide is given a name so that `a` and `s` step a whole slide rather
;; than an animation frame -- with every slide sharing one name, both keys ran
;; to the end of the talk. The name must not be drawn: a converted deck carries
;; its own title inside the page, and a second one over the top is not what the
;; deck looked like.
;;
;; Asked of the pixels, because the question is about pixels. It was asked of
;; two pages' heights before, and a page with a title over it is exactly as tall
;; as one without -- so it passed while the name was being drawn on every slide
;; of a real talk, fading in and out through the animation because a title takes
;; part in the timeline. Passing the name as slideshow's `~name:` rather than as
;; a `~title:` is what stopped it: `~title:` is composed into the page by the
;; Rhombus slide assembler, which is not the assembler this used to install.
;;
;; The titled case is here so that this cannot pass by seeing nothing: a title
;; somebody asks for is drawn, and these pixels say so.
(cond
  [(not display?) (printf "no display; what a page draws is not checked\n")]
  [else
   (define (program-of name body)
     (define path (build-path work (format "draws-~a.rhm" name)))
     (display-to-file
      (string-join
       (list "#lang rhombus/and_meta"
             "import:"
             "  slideshow open"
             "  lib(\"glide-pptx/runtime.rhm\") as glide"
             "  lib(\"glide-pptx/staged.rhm\") open"
             "def canvas = glide.slide_canvas("
             "  ~width: 320.0, ~height: 240.0, ~background: glide.hex(\"FFFFFF\"),"
             "  glide.at(20.0, 20.0, ~tag: \"Box\","
             "           glide.shape_pict(~width: 60.0, ~height: 40.0,"
             "                            ~fill: glide.hex(\"4472C4\"))))"
             (format "for (n in 1..3): ~a" body))
       "\n")
      path #:exists 'replace)
     path)
   (define raw (program-of "raw" "slide(Pict.from_handle(canvas), ~layout: #'center)"))
   (define named (program-of "named"
                             "do_staged_slide(Pict.from_handle(canvas), ~layout: #'center)"))
   (define titled (program-of "titled"
                              (string-append "do_staged_slide(Pict.from_handle(canvas),"
                                             " ~title: \"Zebra\", ~layout: #'center)")))
   (define W 512) (define H 384)
   (define (page-bytes path)
     (define get (dynamic-require 'slideshow/slides-to-picts 'get-slides-as-picts))
     (define p (first (get (path->string path) W H #t)))
     (define bm (make-bitmap W H))
     (define dc (new bitmap-dc% [bitmap bm]))
     (send dc set-brush "white" 'solid)
     (send dc set-pen "white" 1 'solid)
     (send dc draw-rectangle 0 0 W H)
     (draw-pict p dc 0 0)
     (define bs (make-bytes (* 4 W H)))
     (send bm get-argb-pixels 0 0 W H bs)
     bs)
   (define (differing a b)
     (for/sum ([i (in-range (* 4 W H))])
       (if (> (abs (- (bytes-ref a i) (bytes-ref b i))) 8) 1 0)))
   (define raw-px (page-bytes raw))
   (check-equal? (differing raw-px (page-bytes named)) 0
                 "the name a slide is known by draws nothing")
   (check-true (> (differing raw-px (page-bytes titled)) 100)
               "and a title somebody asks for is drawn, so these pixels can see one")])

(printf "staged tests done\n")

(module+ main (void (test-log #:display? #t #:exit? #t)))
