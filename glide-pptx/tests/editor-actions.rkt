#lang racket/base
;; What a person can do in a slideshow editor, as data.
;;
;; The list is taken from the editors' own surfaces -- PowerPoint's ribbon
;; (Home, Insert, Design), its Format pane (Fill, Line, Size, Position, Text
;; Box), and Keynote's inspector. `actions.rkt` walks it one at a time;
;; `sessions.rkt` deals them out in handfuls, which is how anyone actually
;; edits a deck.
;;
;; Each says what it does to a deck and what the merge should make of it:
;;
;;   applied   written into the program
;;   reported  refused, with a reason, and nothing written
;;   noted     seen, and not an edit -- what a deck says when it says nothing
;;   ignored   deliberately invisible, like a transition
(require racket/list racket/string racket/file racket/path racket/runtime-path
         glide-pptx/sync glide-pptx/export
         "deck-edit.rkt")

(provide (struct-out act-spec) catalogue lay-down! fixture-media)

(define-runtime-path media-dir "media")
(define fixture-media media-dir)

;; `edit` takes the deck and returns whether it landed: an edit whose shape has
;; already been deleted by an earlier one simply does not apply.
(struct act-spec (name outcome edit writes says settles-in) #:transparent)

(define entries '())
(define (act name outcome edit #:writes [writes #f] #:says [says #f]
             #:settles-in [settles-in 1])
  (set! entries (cons (act-spec name outcome edit writes says settles-in) entries)))
(define (catalogue) (reverse entries))

;; Enough to act on, laid down where the caller asks: the program, the deck it
;; exports to, and a first sync so the two agree.
(define (lay-down! dir)
  (make-directory* (build-path dir "media"))
  (for ([f (in-list '("checker.png" "gradient.png"))])
    (copy-file (build-path media-dir f) (build-path dir "media" f) #t))
  (define program (build-path dir "deck.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (display-to-file program-text program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (values program deck))

;; Enough to act on: a shape with an outline and one with neither fill nor
;; outline, a colour two shapes share, a line of two runs, a list of two
;; paragraphs, a picture, and a second slide.
(define program-text
  (string-join
   (list "#lang rhombus/and_meta"
         "import:"
         "  lib(\"glide-pptx/runtime.rhm\") open"
         "export:"
         "  all_slides"
         "def media = media_lookup(\"media\")"
         "def brand = hex(\"4472C4\")"
         "def slide_1 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(40.0, 40.0, ~tag: \"Outlined\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
         "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
         "  at(200.0, 40.0, ~tag: \"Bare\","
         "     shape_pict(~width: 90.0, ~height: 60.0)),"
         "  at(340.0, 40.0, ~tag: \"Shared\","
         "     shape_pict(~width: 90.0, ~height: 60.0, ~fill: brand)),"
         "  at(480.0, 40.0, ~tag: \"Twin\","
         "     shape_pict(~width: 90.0, ~height: 60.0, ~fill: brand)),"
         "  at(40.0, 180.0, ~tag: \"Line\","
         "     textbox(~width: 400.0, ~height: 60.0, ~anchor: #'top,"
         "             para(~align: #'left, ~line_spacing: pair(#'percent, 1.0),"
         "                  run(\"hello \", ~font: \"Arial\", ~size: 24.0),"
         "                  run(\"world\", ~font: \"Arial\", ~size: 18.0)))),"
         "  at(40.0, 280.0, ~tag: \"List\","
         "     textbox(~width: 400.0, ~height: 120.0,"
         "             para(~align: #'left, run(\"first\", ~size: 18.0)),"
         "             para(~align: #'left, run(\"second\", ~size: 18.0)))),"
         "  at(40.0, 420.0, ~tag: \"Photo\","
         "     image_pict(media(\"checker.png\"), 160.0, 120.0))"
         ")"
         "def slide_2 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(60.0, 60.0, ~tag: \"Second\","
         "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"70AD47\")))"
         ")"
         "def all_slides = [slide_1, slide_2]"
         "")
   "\n"))

(define ((after tag rx to) deck) (edit-after-tag! deck 1 tag rx to))

;; ------------------------------------------------------------ Home: the font

(act "bold" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" b=\"1\"")
        #:writes #rx"~bold: #true")
(act "italic" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" i=\"1\"")
        #:writes #rx"~italic: #true")
(act "underline" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" u=\"sng\"")
        #:writes #rx"~underline: #true")
(act "strikethrough" 'applied
        (after "Line" #px"sz=\"2400\"" "sz=\"2400\" strike=\"sngStrike\"")
        #:writes #rx"~strike: #true")
(act "font size" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"3600\"")
        #:writes #rx"~size: 36[.]0")
(act "typeface" 'applied (after "Line" #px"typeface=\"Arial\"" "typeface=\"Georgia\"")
        #:writes #rx"~font: \"Georgia\"")
(act "font colour" 'applied
        (after "Line" #px"<a:srgbClr val=\"000000\"/>" "<a:srgbClr val=\"CC0000\"/>")
        #:writes #rx"~color: hex[(]\"CC0000\"[)]")
(act "character spacing" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" spc=\"300\"")
        #:writes #rx"~spacing: 3[.]0")
(act "all caps" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" cap=\"all\"")
        #:writes #rx"~caps: #'all")
(act "superscript" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" baseline=\"30000\"")
        #:writes #rx"~baseline: 0[.]3")
;; The second run of a line, which is one word of it. Its size is what names
;; it -- and not one that is bold already: once the line above has been set to
;; the default size there are two runs at 18pt, and bolding one of them twice
;; writes the attribute twice, which is a deck no editor would save.
(act "bold one word" 'applied
        (after "Line" #px"sz=\"1800\"(?![^>]*b=\")" "sz=\"1800\" b=\"1\"")
        #:writes #rx"run[(]\"world\", ~font: \"Arial\", ~size: 18[.]0, ~bold: #true[)]")

;; ------------------------------------------------------- Home: the paragraph

(act "centre a paragraph" 'applied (after "List" #px"algn=\"l\"" "algn=\"ctr\"")
        #:writes #rx"~align: #'center")
(act "line spacing" 'applied
        (after "List" #px"<a:spcPct val=\"100000\"/>" "<a:spcPct val=\"150000\"/>")
        #:writes #rx"~line_spacing: pair[(]#'percent, 1[.]5[)]")
(act "space before a paragraph" 'applied
        (after "List" #px"</a:lnSpc>" "</a:lnSpc><a:spcBef><a:spcPts val=\"1200\"/></a:spcBef>")
        #:writes #rx"~space_before: 12[.]0")
(act "indent level" 'applied (after "List" #px"lvl=\"0\"" "lvl=\"1\"")
        #:writes #rx"~level: 1")
(act "hanging indent" 'applied
        (after "List" #px"marL=\"0\" indent=\"0\"" "marL=\"457200\" indent=\"-228600\"")
        #:writes #rx"~margin_left: 36[.]0")
(act "a bulleted list" 'applied
        (after "List" #px"<a:buNone/>" "<a:buChar char=\"•\"/>")
        #:writes #rx"~bullet: bullet[(]#'char")
(act "retype text" 'applied (after "List" #px"<a:t>first</a:t>" "<a:t>primary</a:t>")
        #:writes #rx"run[(]\"primary\"")
;; Characters, not bytes. A program's positions are counted in characters, and
;; a splice that counted bytes would land three places to the left of where it
;; meant to for every edit written after this line.
(act "retype text with more than ASCII in it" 'applied
        (after "List" #px"<a:t>second</a:t>" "<a:t>naïve — 日本語 ✓</a:t>")
        #:writes #rx"run[(]\"naïve")

;; ----------------------------------------------------- Format pane: the text box

(act "vertical anchor" 'applied (after "Line" #px"anchor=\"t\"" "anchor=\"ctr\"")
        #:writes #rx"~anchor: #'center")
(act "word wrap off" 'applied (after "Line" #px"wrap=\"square\"" "wrap=\"none\"")
        #:writes #rx"~wrap: #false")
(act "shrink text on overflow" 'applied (after "Line" #px"<a:noAutofit/>" "<a:normAutofit/>")
        #:writes #rx"~autofit: #'shrink")
(act "text insets" 'applied (after "Line" #px"lIns=\"91440\"" "lIns=\"228600\"")
        #:writes #rx"~insets: insets[(]18[.]0")

;; --------------------------------------------------- Format pane: fill and line

(act "fill colour" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"ED7D31\"/>" "<a:srgbClr val=\"C00000\"/>")
        #:writes #rx"~fill: hex[(]\"C00000\"[)]")
(act "fill opacity" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"ED7D31\"/>"
               "<a:srgbClr val=\"ED7D31\"><a:alpha val=\"50000\"/></a:srgbClr>")
        #:writes #rx"~alpha: 0[.]5")
(act "no fill" 'applied
        (after "Outlined" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>" "<a:noFill/>")
        #:writes #rx"~fill: #false")
(act "a fill where there was none" 'applied
        (after "Bare" #px"<a:noFill/><a:ln>"
               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill><a:ln>")
        #:writes #rx"~fill: hex[(]\"00B050\"[)]")
(act "a gradient fill" 'reported
        (after "Outlined" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>"
               (string-append "<a:gradFill><a:gsLst><a:gs pos=\"0\"><a:srgbClr val=\"FF0000\"/></a:gs>"
                              "<a:gs pos=\"100000\"><a:srgbClr val=\"0000FF\"/></a:gs></a:gsLst>"
                              "<a:lin ang=\"0\"/></a:gradFill>"))
        #:says #rx"gradient")
(act "outline colour" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"203040\"/>" "<a:srgbClr val=\"FF0000\"/>")
        #:writes #rx"make_stroke[(]hex[(]\"FF0000\"[)]")
(act "outline width" 'applied (after "Outlined" #px"<a:ln w=\"25400\"" "<a:ln w=\"76200\"")
        #:writes #rx"~width: 6[.]0")
(act "a dashed outline" 'applied
        (after "Outlined" #px"</a:ln>" "<a:prstDash val=\"dash\"/></a:ln>")
        #:writes #rx"~dash: #'dash")
(act "a round line cap" 'applied (after "Outlined" #px"cap=\"flat\"" "cap=\"rnd\"")
        #:writes #rx"~cap: #'round")
(act "an arrowhead" 'applied
        (after "Outlined" #px"</a:ln>" "<a:tailEnd type=\"triangle\" w=\"med\" len=\"med\"/></a:ln>")
        #:writes #rx"~tail: line_end[(]#'triangle")
(act "no outline" 'applied
        (after "Outlined" #px"<a:ln w=\"[0-9]+\"[^>]*>.*?</a:ln>" "<a:ln><a:noFill/></a:ln>")
        #:writes #rx"~line: #false")
(act "an outline where there was none" 'applied
        (after "Bare" #px"<a:ln><a:noFill/></a:ln>"
               "<a:ln w=\"19050\"><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></a:ln>")
        #:writes #rx"~line: make_stroke")
(act "recolour one of two that share a colour" 'reported
        (after "Shared" #px"<a:srgbClr val=\"4472C4\"/>" "<a:srgbClr val=\"7030A0\"/>")
        #:says #rx"shared with")

;; --------------------------------------------------- Format pane: size and place

(act "move" 'applied (lambda (deck) (drag-in-deck! deck 1 "Outlined" 300.0 320.0))
        #:writes #rx"at[(]300[.]0, 320[.]0")
(act "resize" 'applied (lambda (deck) (resize-in-deck! deck 1 "Outlined" 200.0 150.0))
        #:writes #rx"~width: 200[.]0")
(act "rotate" 'applied (lambda (deck) (rotate-in-deck! deck 1 "Outlined" 30.0))
        #:writes #rx"~rotate: 30[.]0")
(act "flip" 'applied (after "Outlined" #px"<a:xfrm" "<a:xfrm flipH=\"1\"")
        #:writes #rx"~flip_h: #true")

;; ------------------------------------------------------------------ a picture

(act "crop a picture" 'applied
        (after "Photo" #px"</a:blip><a:stretch>"
               "</a:blip><a:srcRect l=\"10000\" t=\"5000\"/><a:stretch>")
        #:writes #rx"~crop: [[]0[.]1")
(act "fade a picture" 'applied
        (after "Photo" #px"></a:blip>" "><a:alphaModFix amt=\"40000\"/></a:blip>")
        #:writes #rx"~opacity: 0[.]4")
(act "swap a picture's image" 'applied
        (lambda (deck)
          (with-unpacked-deck deck
            (lambda (d)
              (for ([f (in-list (directory-list (build-path d "ppt" "media")))])
                (copy-file (build-path media-dir "gradient.png")
                           (build-path d "ppt" "media" f) #t))
              #t)))
        #:writes #rx"media[(]\"pic")

;; ------------------------------------------------------------- Home: arranging

(act "bring to front" 'applied (lambda (deck) (bring-to-front! deck 1 "Outlined"))
        #:writes #rx"~tag: \"Outlined\"")
(act "group" 'applied
        (lambda (deck) (group-in-deck! deck 1 "Outlined" "Bare" #:name "Pair"))
        ;; The group is drawn where its shapes were; the deck draws it last, and
        ;; that order is merged once the group exists to be moved.
        #:settles-in 2
        #:writes #rx"group_pict[(]")
(act "duplicate" 'applied (lambda (deck) (duplicate-in-deck! deck 1 "Bare"))
        #:writes #rx"~tag: \"Bare [(]2[)]\"")
(act "delete a shape" 'applied (lambda (deck) (delete-from-deck! deck 1 "Bare"))
        #:writes #rx"^(?!.*\"Bare\").*$")
(act "draw a new shape" 'applied
        (lambda (deck) (and (add-shape-to-deck! deck 1 "Drawn") #t))
        #:writes #rx"~tag: \"Drawn\"")
(act "move a shape to another slide" 'applied
        (lambda (deck) (move-element-to-slide! deck "Second" 2 1))
        #:writes #rx"~tag: \"Second\"")

;; ------------------------------------------------------------------- the slides

(act "background colour" 'applied
        (lambda (deck) (edit-slide-part! deck 1 #px"<a:srgbClr val=\"FFFFFF\"/>"
                                    "<a:srgbClr val=\"102040\"/>"))
        #:writes #rx"~background: hex[(]\"102040\"[)]")
(act "reorder the slides" 'applied (lambda (deck) (move-slide! deck 2 1))
        #:writes #rx"all_slides = [[]slide_2, slide_1[]]")
(act "delete a slide" 'applied (lambda (deck) (delete-slide! deck 2))
        #:writes #rx"^(?!.*slide_2).*$")
;; Hide Slide, which PowerPoint and Keynote both offer and which used to be
;; undone by the next regeneration.
(act "hide a slide" 'applied
        (lambda (deck) (edit-part! deck "ppt/slides/slide2.xml" #px"<p:sld " "<p:sld show=\"0\" "))
        #:writes #rx"~hidden: #true")
;; Design -> Slide Size. A deck has one size however many slides it has, so
;; this is one edit on every canvas that states it.
(act "resize the deck" 'applied
        (lambda (deck) (edit-part! deck "ppt/presentation.xml"
                               #px"<p:sldSz cx=\"[0-9]+\" cy=\"[0-9]+\""
                               "<p:sldSz cx=\"12192000\" cy=\"6858000\""))
        #:writes #rx"~width: 960[.]0")

;; ------------------------------------------------- what an editor does not mean

;; A transition belongs to the deck, not to what is on it, and animations are
;; the code's business. Neither is an edit to merge.
(act "a slide transition" 'ignored
        (lambda (deck) (edit-slide-part! deck 1 #px"</p:cSld>"
                                    "</p:cSld><p:transition><p:fade/></p:transition>")))
;; A theme's colours are not what our shapes are painted with -- they state
;; their own -- so changing one changes nothing here.
(act "the theme's colours" 'ignored
        (lambda (deck) (edit-part! deck "ppt/theme/theme1.xml" #px"<a:srgbClr val=\"4472C4\"/>"
                               "<a:srgbClr val=\"FF00FF\"/>")))
;; A run that states the defaults says what a deck says when it says nothing.
(act "text set to the defaults" 'noted
        (after "Line" #px"sz=\"2400\"" "sz=\"1800\""))

