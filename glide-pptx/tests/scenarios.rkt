#lang racket/base
;; What a session in the editor actually looks like: several edits at once, on
;; several slides, then one merge. The single-action tests in `sync.rkt` say
;; each edit can be written; these say a run of them lands together and leaves
;; a program that still says what the deck says.
;;
;; The check every scenario ends with is the same one: sync again and nothing
;; is reported. That means the program, rendered, is the deck the editor holds
;; -- which is the whole point of the merge.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/format racket/path
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "deck-edit.rkt" "ir-diff.rkt")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-scenarios"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Three slides, and between them a shape with an outline, one with neither
;; fill nor outline, a styled line of text, a bullet list, and a shape whose
;; colour is shared with another.
(define (program-text)
  (string-join
   (list "#lang rhombus/and_meta"
         "import:"
         "  lib(\"glide-pptx/runtime.rhm\") open"
         "export:"
         "  all_slides"
         "  slide_1"
         "  slide_2"
         "  slide_3"
         "def brand = hex(\"4472C4\")"
         "def slide_1 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(60.0, 60.0, ~tag: \"Box\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
         "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
         "  at(240.0, 60.0, ~tag: \"Bare\","
         "     shape_pict(~width: 90.0, ~height: 60.0)),"
         "  at(60.0, 200.0, ~tag: \"Mixed\","
         "     textbox(~width: 400.0, ~height: 60.0,"
         "             para(run(\"hello \", ~size: 24.0),"
         "                  run(\"world\", ~size: 24.0, ~bold: #true))))"
         ")"
         "def slide_2 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(60.0, 60.0, ~tag: \"Blue\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
         "  at(240.0, 60.0, ~tag: \"Also blue\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
         "  at(60.0, 200.0, ~tag: \"Bullets\","
         "     textbox(~width: 400.0, ~height: 120.0,"
         "             para(run(\"first line\", ~size: 18.0)),"
         "             para(run(\"second line\", ~size: 18.0))))"
         ")"
         "def slide_3 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(100.0, 100.0, ~tag: \"Alone\","
         "     shape_pict(~width: 200.0, ~height: 100.0, ~fill: hex(\"70AD47\")))"
         ")"
         "def all_slides = [slide_1, slide_2, slide_3]"
         "")
   "\n"))

;; Runs one scenario: a fresh program and deck, the editor's edits, one merge.
;; `expect` is how many actions should be applied; `refused` how many should
;; not be. Both are checked, because a scenario that quietly applies nothing
;; passes every other check.
(define (scenario name edits! #:applied [applied #f] #:refused [refused 0]
                  #:agree? [agree? #t] #:then [then void])
  (define dir (build-path work (string-replace name " " "-")))
  (make-directory* dir)
  (define program (build-path dir "s.rhm"))
  (define deck (build-path dir "s.pptx"))
  (display-to-file (program-text) program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))
  (with-check-info (['scenario name])
    (edits! deck)
    (define r (sync!))
    (when applied
      (check-equal? (length (sync-report-applied r)) applied
                    (format "~a: applied" name)))
    (check-equal? (length (sync-report-skipped r)) refused
                  (format "~a: refused ~s" name (map cdr (sync-report-skipped r))))
    ;; The program still loads, and says what the deck says. A pass can leave
    ;; one thing for the next -- a shape added last has its drawing order fixed
    ;; after it exists -- so a couple of passes are allowed, and no more.
    ;;
    ;; A scenario that is meant to be refused never settles, by definition.
    ;; What it must do instead is stay refused and leave the program alone.
    (cond
      [(zero? refused)
       (define passes
         (let loop ([n 1])
           (define after (sync!))
           (cond
             [(null? (sync-report-actions after)) n]
             [(>= n 3)
              (fail (format "~a: still reporting ~s after ~a passes" name
                            (map sync-action-kind (sync-report-actions after)) n))
              n]
             [(null? (sync-report-applied after))
              (fail (format "~a: stuck reporting ~s, applying none" name
                            (map sync-action-kind (sync-report-actions after))))
              n]
             [else (loop (add1 n))])))
       (check-true (<= passes 2) (format "~a: settled in ~a passes" name passes))
       ;; And the program really does render to the deck the editor holds.
       (when agree?
         (check-equal? (disagreements program deck) '()
                       (format "~a: the program and the deck agree" name)))]
      [else
       (define before (file->string program))
       (define again (sync!))
       (check-equal? (length (sync-report-skipped again)) refused
                     (format "~a: still refused" name))
       (check-equal? (file->string program) before
                     (format "~a: and the program is left alone" name))])
    (then program deck)))

;; ------------------------------------------------------------- the scenarios

;; A tidy-up pass: drag two things on different slides, resize a third.
(scenario "a tidy-up pass"
          (lambda (deck)
            (check-true (drag-in-deck! deck 1 "Box" 100.0 120.0))
            (check-true (drag-in-deck! deck 2 "Blue" 300.0 200.0))
            (check-true (resize-in-deck! deck 3 "Alone" 260.0 130.0)))
          #:applied 3)

;; A restyling pass: a fill, a font, a word, an alignment and a background.
(scenario "a restyling pass"
          (lambda (deck)
            (check-true (edit-after-tag! deck 1 "Box" #px"<a:srgbClr val=\"ED7D31\"/>"
                                         "<a:srgbClr val=\"C00000\"/>"))
            (check-true (edit-after-tag! deck 1 "Mixed" #px"typeface=\"[^\"]*\""
                                         "typeface=\"Georgia\""))
            (check-true (edit-after-tag! deck 1 "Mixed" #px"<a:t>world</a:t>"
                                         "<a:t>planet</a:t>"))
            (check-true (edit-after-tag! deck 2 "Bullets" #px"<a:pPr algn=\"l\""
                                         "<a:pPr algn=\"ctr\"")))
          #:applied 4
          #:then (lambda (program deck)
                   (define src (file->string program))
                   (check-regexp-match #rx"hex[(]\"C00000\"[)]" src)
                   (check-regexp-match #rx"run[(]\"planet\"" src)
                   ;; A typeface read from the first run is written back to it,
                   ;; and an alignment read from the first paragraph to that.
                   (check-regexp-match #rx"~font: \"Georgia\"" src)
                   (check-regexp-match #rx"~align: #'center" src)))

;; Drawing a shape, then coming back and styling it in a second pass -- which
;; is how anyone actually works.
(scenario "draw a shape, then style it"
          (lambda (deck)
            (check-true (and (add-shape-to-deck! deck 3 "Callout") #t)))
          #:applied 1
          #:then
          (lambda (program deck)
            (check-regexp-match #rx"~tag: \"Callout\"" (file->string program))
            (check-true (edit-after-tag!
                         deck 3 "Callout" #px"</a:solidFill></p:spPr>"
                         (string-append "</a:solidFill><a:ln w=\"19050\"><a:solidFill>"
                                        "<a:srgbClr val=\"203040\"/></a:solidFill>"
                                        "<a:prstDash val=\"dash\"/></a:ln></p:spPr>"))
                        "the new shape was given a dashed outline")
            (define r (sync-once program deck
                                 #:workdir (build-path (path-only program) "w")))
            (check-equal? (length (sync-report-applied r)) 1 "the outline was written too")
            (define src (file->string program))
            (check-regexp-match #rx"~line: make_stroke[(]hex[(]\"203040\"[)]" src)
            (check-regexp-match #rx"~dash: #'dash" src)))

;; Deleting a shape, and deleting a whole slide, in one pass.
(scenario "delete a shape and a slide"
          (lambda (deck)
            (check-true (delete-from-deck! deck 1 "Bare"))
            (check-true (delete-slide! deck 3)))
          #:applied 2
          #:then (lambda (program deck)
                   (define src (file->string program))
                   (check-false (regexp-match? #rx"\"Bare\"" src))
                   (check-false (regexp-match? #rx"slide_3" src))))

;; Restacking and repainting, which are the two edits that belong to a slide
;; rather than to anything on it.
(scenario "restack and repaint"
          (lambda (deck)
            (check-true (bring-to-front! deck 1 "Box"))
            (check-true (edit-slide-part! deck 2 #px"<a:srgbClr val=\"FFFFFF\"/>"
                                          "<a:srgbClr val=\"102040\"/>")))
          #:applied 2)

;; A colour two shapes share, changed on both of them: the definition moves,
;; once, rather than either shape being given a colour of its own.
(scenario "recolour a shared colour"
          (lambda (deck)
            (check-true (edit-after-tag! deck 2 "Blue" #px"<a:srgbClr val=\"4472C4\"/>"
                                         "<a:srgbClr val=\"7030A0\"/>"))
            (check-true (edit-after-tag! deck 2 "Also blue" #px"<a:srgbClr val=\"4472C4\"/>"
                                         "<a:srgbClr val=\"7030A0\"/>")))
          #:applied 2
          #:then (lambda (program deck)
                   (define src (file->string program))
                   (check-regexp-match #rx"def brand = hex[(]\"7030A0\"[)]" src)
                   (check-equal? (length (regexp-match* #rx"7030A0" src)) 1
                                 "written once, in the definition")))

;; Copying a shape and dragging the copy somewhere: two actions, and the copy
;; needs a tag of its own or nothing later can read the program.
;; Copying a shape: the copy carries the tag it was copied from, and the merge
;; gives it one of its own -- otherwise nothing could read the program again.
;; So the two sides do not agree by tag until the deck is written next, which
;; is why this one does not ask them to.
(scenario "duplicate and move the copy"
          (lambda (deck)
            (check-true (duplicate-in-deck! deck 1 "Bare")))
          #:applied 1 #:agree? #f
          #:then (lambda (program deck)
                   (define src (file->string program))
                   (check-regexp-match #rx"~tag: \"Bare [(]2[)]\"" src)
                   (check-equal? (length (regexp-match* #rx"~tag: \"Bare" src)) 2
                                 "the original and the copy")))

;; Grouping two shapes, which the editor does with one command and which the
;; program has a form for.
;; Grouping, which moves two `at` forms inside a `group_pict`. The drawing
;; order follows in the pass after, since the group only exists once it is
;; written.
(scenario "group two shapes"
          (lambda (deck)
            (check-true (group-in-deck! deck 1 "Box" "Bare" #:name "Pair")))
          #:applied 1
          #:then (lambda (program deck)
                   (define src (file->string program))
                   (check-regexp-match #rx"~tag: \"Pair\"" src)
                   (check-regexp-match #rx"group_pict[(]" src)
                   (check-regexp-match #rx"~tag: \"Box\"" src "with its shapes inside it")
                   (check-regexp-match #rx"~tag: \"Bare\"" src)))

;; And the other way: the forms come back out of the group rather than being
;; written afresh from the deck.
(scenario "ungroup two shapes"
          (lambda (deck)
            (check-true (group-in-deck! deck 1 "Box" "Bare" #:name "Pair"))
            (check-true (ungroup-in-deck! deck 1 "Pair")))
          ;; Grouping and ungrouping leaves the shapes where they were but at
          ;; the end of the deck's drawing order, so what is left to merge is
          ;; that order.
          #:applied 1
          #:then (lambda (program deck)
                   (check-false (regexp-match? #rx"group_pict" (file->string program)))))

;; Moving a shape to another slide.
(scenario "move a shape to another slide"
          (lambda (deck)
            (check-true (move-element-to-slide! deck "Alone" 3 1)))
          #:applied #f #:refused 0)

;; An element whose `at` is written in a helper the slide calls.
;;
;; A talk that draws a badge with `with_icon(...)` puts the `at` inside the
;; helper, and the slide never mentions the tag at all. A tag was looked up
;; under the slide's own definition, missed, and the drag came back as having
;; "no tagged `at` form in the source" while the file held exactly one -- which
;; is a real talk's own shape, and not any fixture's.
;;
;; Overlaid rather than placed, because that is what a stage is: a group placed
;; by an `at` is one element to drag, and a group laid over the slide gives up
;; its `at` forms as elements of their own.
(let ()
  (define dir (build-path work "at-in-a-helper"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          ""
          "fun with_icon(label):"
          "  group_pict(~width: 480.0, ~height: 270.0,"
          "             at(300.0, 40.0, ~tag: \"Badge\","
          "                textbox(~width: 120.0, ~height: 30.0, ~wrap: #false,"
          "                        para(run(label, ~size: 12.0)))))"
          ""
          "fun a_slide():"
          "  def base:"
          "    slide_canvas(~width: 480.0, ~height: 270.0,"
          "                 at(20.0, 20.0, ~tag: \"Title\","
          "                    textbox(~width: 200.0, ~height: 30.0, ~wrap: #false,"
          "                            para(run(\"hello\", ~size: 14.0)))))"
          "  pc.overlay(~horiz: #'left, ~vert: #'top,"
          "             pc.Pict.from_handle(base),"
          "             pc.Pict.from_handle(with_icon(\"new\")))"
          ""
          "def all_slides = [a_slide]")
    "\n")
   program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (check-true (and (drag-in-deck! deck 1 "Badge" 111.0 222.0) #t)
              "the badge was dragged in the deck")
  (define r (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (for/list ([sk (in-list (sync-report-skipped r))]) (cdr sk)) '()
                "nothing was refused")
  (check-equal? (length (sync-report-applied r)) 1 "and the drag was written")
  ;; Into the helper's own form, which is the only one there is.
  (check-true (regexp-match? #px"at\\(111[.]0, 222[.]0, ~tag: \"Badge\""
                             (file->string program))
              "the `at` inside the helper is where it landed")
  ;; And it settles: written again, there is nothing more to say.
  (picts->pptx (load-program-picts program) deck)
  (define again (sync-once program deck #:workdir (build-path dir "w") #:dry-run? #t))
  (check-equal? (for/list ([a (in-list (sync-report-actions again))])
                  (format "~a ~s" (sync-action-kind a) (sync-action-tag a)))
                '()
                "and the program and the deck agree afterwards"))

;; One `at` drawing an element on two slides, and a drag on one of them.
;;
;; Writing the shared form would move both, so the deck's other slide would be
;; left holding the old value. Read as a fresh edit, that old occurrence could
;; then move the first one back and make the watcher oscillate forever.
;;
;; A page-local edit to a source site shared by distinct logical slides is not
;; representable. Refuse it as one operation and leave both the program and its
;; base alone; independently editable calls need distinct outer proxy tags.
(let ()
  (define dir (build-path work "shared-across-slides"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          ""
          "fun with_icon():"
          "  group_pict(~width: 480.0, ~height: 270.0,"
          "             at(300.0, 40.0, ~tag: \"Badge\","
          "                textbox(~width: 120.0, ~height: 30.0, ~wrap: #false,"
          "                        para(run(\"new\", ~size: 12.0)))))"
          ""
          "fun a_slide(label):"
          "  def base:"
          "    slide_canvas(~width: 480.0, ~height: 270.0,"
          "                 at(20.0, 20.0, ~tag: \"Title\","
          "                    textbox(~width: 200.0, ~height: 30.0, ~wrap: #false,"
          "                            para(run(label, ~size: 14.0)))))"
          "  pc.overlay(~horiz: #'left, ~vert: #'top,"
          "             pc.Pict.from_handle(base),"
          "             pc.Pict.from_handle(with_icon()))"
          ""
          "fun one(): a_slide(\"one\")"
          "fun two(): a_slide(\"two\")"
          "def all_slides = [one, two]")
    "\n")
   program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (check-true (and (drag-in-deck! deck 1 "Badge" 111.0 222.0) #t)
              "the badge was dragged on the first slide")
  (define first-pass (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-applied first-pass)) 0
                "the page-local drag was not written into a shared source call")
  (check-equal? (length (sync-report-skipped first-pass)) 1
                "the shared occurrence was refused as one operation")
  (check-false (sync-report-deck-behind? first-pass)
               "the refused edit did not advance the base")
  ;; Regenerating from the unchanged program discards the unrepresentable drag.
  (picts->pptx (load-program-picts program) deck)
  ;; Twice, because the old bug took two passes to show its oscillation: the
  ;; first reported the other slide and the second reported this one again.
  (for ([pass (in-list '(1 2))])
    (define r (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
    (check-equal? (for/list ([a (in-list (sync-report-actions r))])
                    (format "~a ~s on slide ~a" (sync-action-kind a)
                            (sync-action-tag a) (sync-action-slide a)))
                  '()
                  (format "and it has settled (pass ~a)" pass))))

(printf "scenario tests done; artifacts under ~a\n" work)

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
