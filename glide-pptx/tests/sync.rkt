#lang racket/base
;; Two-way sync: a deck's edits merged back into the program that made it.
;;
;; The properties that matter are as much about restraint as about function. A
;; merge must change exactly the numbers that moved and nothing else in the
;; file, and it must refuse rather than guess when the thing it would patch is
;; not a literal.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/port racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-rhombus
         glide-pptx/export
         glide-pptx/sync glide-pptx/sync-state glide-pptx/runtime "deck-edit.rkt"
         (only-in "ir-diff.rkt" deck-states-by-name)
         (only-in glide-pptx/watch program-picts)
         ;; What `raco glide --new` writes.
         (only-in glide-pptx/main starter-deck))

;; Loads a program's slides the way the watcher does, in a fresh namespace, so
;; a re-export after a patch sees the patched source.
(define (program-picts-fresh path) (program-picts path))

(define-runtime-path decks-dir "decks")
(define-runtime-path media-dir "media")

;; Source-derived ids are deliberately bookkeeping. A source edit can move a
;; later call site and therefore change its opaque id without changing which
;; editor object the user sees.
(define (visible-tag t) (or (and t (automatic-tag-name t)) t))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-sync"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Sets up a program and a deck exported from it, with a base recorded.
(define (fixture name deck-name)
  (define dir (build-path work name))
  (make-directory* dir)
  (define pptx (build-path decks-dir deck-name))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define exported (build-path dir "deck.pptx"))
  (define picts (load-program-picts program))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-true (sync-report-base-written? r) "the first pass records a base")
  (check-equal? (sync-report-actions r) '() "and has nothing to merge")
  (values dir program exported))

;; ------------------------------------------- a drag comes back as a literal

(let ()
  (define-values (dir program exported) (fixture "drag" "05-realistic.pptx"))
  (define before (file->string program))
  (check-true (drag-in-deck! exported 3 "Rounded Rectangle 2" 100.0 200.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define moves (filter (lambda (a) (eq? 'moved (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length moves) 1 "exactly one element moved")
  (check-equal? (automatic-tag-name (sync-action-tag (first moves)))
                "Rounded Rectangle 2")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  (define after (file->string program))
  ;; The restraint property: one line differs, and it is the right one.
  (define changed
    (for/list ([a (in-list (string-split before "\n"))]
               [b (in-list (string-split after "\n"))]
               #:unless (string=? a b))
      (cons a b)))
  (check-equal? (length changed) 1 "exactly one line of source changed")
  (check-true (regexp-match? #rx"57[.]6, 158[.]4" (car (first changed))) "was the old position")
  (check-true (regexp-match? #rx"100[.]0, 200[.]0" (cdr (first changed))) "is the new one")
  (check-equal? (length (string-split before "\n")) (length (string-split after "\n"))
                "no lines were added or removed")

  ;; And the result is still a program. Loading it is the check: running it
  ;; opens a slideshow, which needs a display.
  (check-true (pair? (load-program-picts program)) "the patched program still loads")

  ;; A second pass has nothing left to do, because the base moved with it.
  (define again (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-equal? (filter (lambda (a) (memq (sync-action-kind a) '(moved resized)))
                        (sync-report-actions again))
                '()
                "the sync converges: a second pass finds nothing"))

;; ------------------------------- a computed position becomes a correction

;; `left` is a variable, so there is no literal to replace. The drag is recorded
;; as a `#:nudge` on `at` instead, which keeps the program's own layout logic
;; and -- because it is one argument rather than a wrapper -- cannot stack up
;; when the element is dragged again.
(let ()
  (define dir (build-path work "computed"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string (string-join
                     '("#lang rhombus/and_meta"
                       "import:"
                       "  lib(\"glide-pptx/runtime.rhm\") open"
                       "export:"
                       "  all_slides"
                       "def left = 40.0"
                       "def slide_1 = slide_canvas("
                       "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                       "  at(left, 60.0, ~tag: \"Box\","
                       "     shape_pict(~width: 100.0, ~height: 40.0,"
                       "                ~fill: hex(\"4472C4\")))"
                       ")"
                       "def all_slides = [slide_1]"
                       "")
                     "\n") o)))
  (define exported (build-path dir "deck.pptx"))
  (define (re-export!)
    (define picts (parameterize ([current-media-base dir])
                    (program-picts-fresh program)))
    (picts->pptx picts exported #:width 480.0 #:height 270.0))
  (re-export!)
  (sync-once program exported #:workdir (build-path dir "w"))

  ;; First drag: a correction appears.
  (check-true (drag-in-deck! exported 1 "Box" 200.0 100.0) "the shape to drag was found")
  (define before (file->string program))
  (define r1 (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r1)) 1 "the drag was applied")
  (define after1 (file->string program))
  (check-true (regexp-match? #rx"~nudge: [[]160[.]0, 40[.]0[]]" after1)
              (format "a correction of the right size was recorded:\n~a" after1))
  (check-equal? (length (regexp-match* #rx"~nudge" after1)) 1 "exactly one correction")
  (check-true (regexp-match? #rx"at[(]left, 60[.]0" after1)
              "and the program's own computed position is untouched")
  (check-equal? (length (string-split before "\n")) (length (string-split after1 "\n"))
                "no lines were added or removed")

  ;; The element now really draws where it was dragged to.
  (define states (program-slide-states program))
  (define box (findf (lambda (e) (equal? "Box" (el-state-tag e)))
                     (slide-state-elements (first states))))
  (check-true (and box #t) "the element is still found")
  (when box
    (check-= (el-state-x box) 200.0 0.01 "at the dragged x")
    (check-= (el-state-y box) 100.0 0.01 "and the dragged y"))

  ;; Second drag: the correction is updated in place, not nested.
  (re-export!)
  (sync-once program exported #:workdir (build-path dir "w"))
  (check-true (drag-in-deck! exported 1 "Box" 260.0 70.0) "the shape to drag was found")
  (define r2 (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r2)) 1 "the second drag was applied too")
  (define after2 (file->string program))
  (check-equal? (length (regexp-match* #rx"~nudge" after2)) 1
                (format "still exactly one correction, not a stack of them:\n~a" after2))
  (check-true (regexp-match? #rx"~nudge: [[]220[.]0, 10[.]0[]]" after2)
              (format "and it accumulated to the new offset:\n~a" after2))
  (define states2 (program-slide-states program))
  (define box2 (findf (lambda (e) (equal? "Box" (el-state-tag e)))
                      (slide-state-elements (first states2))))
  (when box2
    (check-= (el-state-x box2) 260.0 0.01 "drawing at the second dragged x")
    (check-= (el-state-y box2) 70.0 0.01 "and y")))

;; ------------------------------------------- the same thing, in Rhombus

;; A Rhombus program is patched the same way, which matters because that is the
;; language glide is written for. Shrubbery syntax objects carry source
;; locations, so the literals can be located and overwritten without
;; regenerating any of the surrounding text -- commas and line breaks included.
(let ()
  (define dir (build-path work "rhombus"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define exported (build-path dir "deck.pptx"))
  (define picts (load-program-picts program))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (sync-once program exported #:workdir (build-path dir "w"))
  (define before (file->string program))
  (check-true (drag-in-deck! exported 3 "Rounded Rectangle 2" 111.0 222.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r)) 1 "the drag was applied to the .rhm")
  (define after (file->string program))
  (define changed
    (for/list ([a (in-list (string-split before "\n"))]
               [b (in-list (string-split after "\n"))]
               #:unless (string=? a b))
      (cons a b)))
  (check-equal? (length changed) 1 "exactly one line of Rhombus source changed")
  (check-true (regexp-match? #rx"at[(]111[.]0, 222[.]0," (cdr (first changed)))
              "with its commas intact")
  (check-true (regexp-match? #rx"~name: \"Rounded Rectangle 2\"" (cdr (first changed)))
              "and the rest of the line untouched")
  ;; And it is still a Rhombus program. Loading it is the check: running it
  ;; opens a slideshow, which needs a display.
  (check-true (pair? (load-program-picts program)) "the patched program still loads"))

(printf "sync tests done; artifacts under ~a\n" work)

;; --------------------------------------- slides owned by imported source files

;; The root owns the order; each imported module owns its slide definition and
;; element literals. A deck save can touch both modules in one transaction, and
;; a slide reorder still belongs to the root list.
(let ()
  (define dir (build-path work "multiple-files"))
  (make-directory* dir)
  (define program (build-path dir "talk.rhm"))
  (define first-source (build-path dir "first.rhm"))
  (define common-source (build-path dir "common.rhm"))
  (define second-source (build-path dir "second.rhm"))
  (define computed-program (build-path dir "computed.rhm"))
  (define exported (build-path dir "talk.pptx"))
  (define (write-source! path lines)
    (display-to-file (string-join (append lines '("")) "\n") path #:exists 'replace))
  (write-source!
   common-source
   '("#lang rhombus/and_meta"
     "import: lib(\"glide-pptx/runtime.rhm\") open"
     "export: first_fill"
     "def first_fill = hex(\"4472C4\")"))
  (write-source!
   first-source
   '("#lang rhombus/and_meta"
     "import:"
     "  lib(\"glide-pptx/runtime.rhm\") open"
     "  \"common.rhm\" open"
     "export:"
     "  slide_1"
     "  first_marker"
     "def first_marker = 1"
     "fun section_wrap(mk): mk()"
     "fun slide_1():"
     "  fun content():"
     "    slide_canvas("
     "      ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
     "      at(40.0, 60.0,"
     "         shape_pict(~width: 100.0, ~height: 40.0, ~fill: first_fill))"
     "    )"
     "  section_wrap(content)"))
  (write-source!
   second-source
   '("#lang rhombus/and_meta"
     "import: lib(\"glide-pptx/runtime.rhm\") open"
     "export:"
     "  slide_2"
     "  second_marker"
     "def second_marker = 2"
     "def first_fill = hex(\"112233\")" ; private and unrelated to common.rhm's name
     "def slide_2 = slide_canvas("
     "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
     "  at(200.0, 80.0,"
     "     shape_pict(~width: 120.0, ~height: 50.0, ~fill: hex(\"ED7D31\")))"
     ")"))
  (write-source!
   program
   '("#lang rhombus/and_meta"
     "import:"
     "  lib(\"glide-pptx/runtime.rhm\") open"
     "  \"first.rhm\" open"
     "  \"second.rhm\" open"
     "export: all_slides"
     "// The show-time wrapper preserves the source slide it decorates."
     "// The manifest separately records slide_1 as the writable owner."
     "fun in_section(i, mk): mk"
     "glide_slides all_slides:"
     "  [in_section(0, slide_1), slide_2]"))
  (write-source!
   computed-program
   '("#lang rhombus/and_meta"
     "import:"
     "  lib(\"glide-pptx/runtime.rhm\") open"
     "  \"first.rhm\" open"
     "export: all_slides"
     "glide_slides all_slides:"
     "  [section_wrap(slide_1)]"))

  (check-exn #rx"traceable `all_slides`"
             (lambda () (find-program-sites computed-program))
             "wrappers in `all_slides` are rejected before they make source mapping ambiguous")
  (check-exn #rx"glide_slides: expected a slide name"
             (lambda () (load-program-picts computed-program))
             "the macro rejects an ambiguous wrapper when the program compiles")

  (check-equal? (program-source-files program)
                (map path->complete-path
                     (list program first-source common-source second-source))
                "transitive local Rhombus imports are the editable source set")
  (define-values (sites _scopes _slide-sites layout) (find-program-sites program))
  (check-true
   (and (hash-has-key? (program-layout-globals layout)
                       (cons (path->complete-path common-source) 'first_fill))
        (hash-has-key? (program-layout-globals layout)
                       (cons (path->complete-path second-source) 'first_fill)))
   "private shared-value names stay qualified by their source module")
  (define first-tag (at-site-tag (first sites)))
  (define second-tag (at-site-tag (second sites)))
  (check-equal? (map (lambda (s) (rng-source (at-site-whole s))) sites)
                (map path->complete-path (list first-source second-source))
                "each recovered site remembers the module that owns it")

  (picts->pptx (load-program-picts program) exported #:width 480.0 #:height 270.0)
  (check-exn #rx"traceable `all_slides`"
             (lambda () (sync-once computed-program exported
                                   #:workdir (build-path dir "computed-w")))
             "a sync refuses the untraceable list before recording a base")
  (sync-once program exported #:workdir (build-path dir "w"))
  (define root-before (file->string program))
  (define first-before (file->string first-source))
  (check-true (drag-in-deck! exported 2 second-tag 222.0 111.0))
  (define r1 (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-applied r1)) 1)
  (check-equal? (file->string program) root-before "the root was not regenerated")
  (check-equal? (file->string first-source) first-before "the other module was untouched")
  (check-regexp-match #rx"at[(]222[.]0, 111[.]0" (file->string second-source)
                      "the edit landed in the imported module")

  ;; One editor save can patch source ranges in two modules.
  (check-true (drag-in-deck! exported 1 first-tag 75.0 95.0))
  (check-true (drag-in-deck! exported 2 second-tag 260.0 120.0))
  (define r2 (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-applied r2)) 2)
  (check-regexp-match #rx"at[(]75[.]0, 95[.]0" (file->string first-source))
  (check-regexp-match #rx"at[(]260[.]0, 120[.]0" (file->string second-source))
  (check-true (pair? (load-program-picts program)) "the split program still loads")

  ;; The bare colour in first.rhm resolves through its direct open import and
  ;; export declaration. The private same-named definition in the sibling
  ;; module is never a candidate merely because it has the same spelling.
  (check-true
   (edit-after-tag! exported 1 first-tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                    "<a:srgbClr val=\"70AD47\"/>"))
  (define r-style
    (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r-style)) '(restyle))
  (check-regexp-match #rx"def first_fill = hex[(]\"70AD47\"[)]"
                      (file->string common-source)
                      "the exported imported binding was rewritten")
  (check-regexp-match #rx"def first_fill = hex[(]\"112233\"[)]"
                      (file->string second-source)
                      "the sibling module's private homonym was untouched")

  ;; Navigation order remains a property of the root module.
  (check-true (move-slide! exported 2 1))
  (define r3 (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-applied r3)) 1)
  (check-regexp-match #rx"[[]slide_2, in_section[(]0, slide_1[)][]]"
                      (file->string program)
                      "reordering preserves the complete wrapper entry")

  ;; Deleting that first deck slide removes its definition and export from the
  ;; helper, and its ordering entry from the root.
  (check-true (delete-slide! exported 1))
  (define r4 (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-applied r4)) 1)
  (check-false (regexp-match? #rx"slide_2" (file->string second-source)))
  (check-regexp-match #rx"second_marker" (file->string second-source))
  (check-regexp-match #rx"[[]in_section[(]0, slide_1[)][]]" (file->string program))
  (check-true (pair? (load-program-picts program)) "the deletion left the split program valid"))

;; A named function may delegate to raw slide content through a section
;; wrapper. An explicit tag on a helper call is the escape hatch for a helper
;; whose inner `at` is invoked as independently editable objects; when that tag
;; is unique, it remains traceable through the wrapper's extra scope.
(let ()
  (define dir (build-path work "named-wrapper-proxy"))
  (make-directory* dir)
  (define program (build-path dir "talk.rhm"))
  (define exported (build-path dir "talk.pptx"))
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun section_wrap(mk): mk()"
      "fun place(x, y, ~tag: tag, ~nudge: nudge = #false):"
      "  at(x, y, ~tag: tag, ~nudge: nudge,"
      "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"4472C4\")))"
      "def start_x = 40.0"
      "def start_y = 60.0"
      "fun raw_slide():"
      "  slide_canvas(~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
      "               place(start_x, start_y, ~tag: \"Box\"))"
      "fun slide_1(): section_wrap(raw_slide)"
      "def all_slides = [slide_1]"
      "")
    "\n")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) exported #:width 480.0 #:height 270.0)
  (sync-once program exported #:workdir (build-path dir "w"))
  (check-true (drag-in-deck! exported 1 "Box" 100.0 90.0))
  (define r (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved))
  (check-regexp-match #rx"place[(]start_x, start_y, ~tag: \"Box\", ~nudge: [[]60[.]0, 30[.]0[]]"
                      (file->string program)
                      "the correction landed on the unique helper call behind the wrapper"))

;; A proxy need not expose coordinates when it states a literal correction.
;; This is useful for a title helper whose position is an implementation detail:
;; the call site still owns any editor adjustment.
(let ()
  (define dir (build-path work "nudge-only-proxy"))
  (make-directory* dir)
  (define program (build-path dir "talk.rhm"))
  (define exported (build-path dir "talk.pptx"))
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun titled(title, ~tag: tag, ~nudge: nudge):"
      "  slide_canvas(~width: 480.0, ~height: 270.0,"
      "               at(40.0, 60.0, ~tag: tag, ~name: title, ~nudge: nudge,"
      "                  shape_pict(~width: 100.0, ~height: 40.0)))"
      "fun slide_1(): titled(\"One\", ~tag: \"Title\", ~nudge: [0.0, 0.0])"
      "def all_slides = [slide_1]"
      "")
    "\n")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) exported #:width 480.0 #:height 270.0)
  (void (sync-once program exported #:workdir (build-path dir "w")))
  (check-true (drag-in-deck! exported 1 "Title" 75.0 85.0))
  (define r (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved))
  (check-regexp-match #rx"~nudge: [[]35[.]0, 25[.]0[]]" (file->string program)
                      "the helper call's stated correction was updated"))

;; A computed display name is available when the program runs but not while its
;; source is parsed. It decorates the automatic id without becoming identity,
;; so the runtime object and the static call site still meet.
(let ()
  (define dir (build-path work "dynamic-name"))
  (make-directory* dir)
  (define program (build-path dir "talk.rhm"))
  (define exported (build-path dir "talk.pptx"))
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun slide_1():"
      "  def title = \"Computed Name\""
      "  slide_canvas(~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
      "               at(40.0, 60.0, ~name: title,"
      "                  shape_pict(~width: 100.0, ~height: 40.0)))"
      "def all_slides = [slide_1]"
      "")
    "\n")
   program #:exists 'replace)
  (define static-tag (at-site-tag (first (find-at-sites program))))
  (define runtime-tag
    (el-state-tag (first (slide-state-elements (first (program-slide-states program))))))
  (check-equal? (automatic-tag-key runtime-tag) (automatic-tag-key static-tag)
                "a dynamic name does not change source identity")
  (check-equal? (automatic-tag-name runtime-tag) "Computed Name")
  (picts->pptx (load-program-picts program) exported #:width 480.0 #:height 270.0)
  (sync-once program exported #:workdir (build-path dir "w"))
  (check-true (drag-in-deck! exported 1 runtime-tag 90.0 80.0))
  (define r (sync-once program exported #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved))
  (check-regexp-match #rx"at[(]90[.]0, 80[.]0" (file->string program)))

;; Reusing one source site on separate slides is renderable, but a page-local
;; edit is not independently writable: changing the source would change every
;; occurrence. The merge refuses that save. An entirely dynamic tag with no
;; static proxy is rejected before a base is recorded at all.
(let ()
  (define dir (build-path work "untraceable-runtime-sites"))
  (make-directory* dir)
  (define repeated (build-path dir "repeated.rhm"))
  (define repeated-deck (build-path dir "repeated.pptx"))
  (define orphan (build-path dir "orphan.rhm"))
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun common(title):"
      "  slide_canvas(~width: 480.0, ~height: 270.0,"
      "               at(40.0, 60.0, ~name: title,"
      "                  shape_pict(~width: 100.0, ~height: 40.0)))"
      "fun slide_1(): common(\"One\")"
      "fun slide_2(): common(\"Two\")"
      "def all_slides = [slide_1, slide_2]"
      "")
    "\n")
   repeated #:exists 'replace)
  (define repeated-picts (load-program-picts repeated))
  (check-not-exn (lambda () (validate-program-picts! repeated repeated-picts))
                 "a shared source object can be rendered on several pages")
  (picts->pptx repeated-picts repeated-deck #:width 480.0 #:height 270.0)
  (void (sync-once repeated repeated-deck #:workdir (build-path dir "repeated-w")))
  (define shared-tag
    (el-state-tag (first (slide-state-elements
                          (first (program-slide-states repeated))))))
  (define repeated-before (file->string repeated))
  (check-true (drag-in-deck! repeated-deck 1 shared-tag 90.0 80.0))
  (define repeated-report
    (sync-once repeated repeated-deck #:workdir (build-path dir "repeated-w")
               #:atomic? #t))
  (check-equal? (sync-report-applied repeated-report) '())
  (check-regexp-match #rx"also appears on slides"
                      (cdr (first (sync-report-skipped repeated-report)))
                      "a page-local edit to a shared source object is refused")
  (check-equal? (file->string repeated) repeated-before)

  ;; The same hazard exists for an explicit tag written on the inner helper.
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun common():"
      "  slide_canvas(~width: 480.0, ~height: 270.0,"
      "               at(40.0, 60.0, ~tag: \"Shared\","
      "                  shape_pict(~width: 100.0, ~height: 40.0)))"
      "fun slide_1(): common()"
      "fun slide_2(): common()"
      "def all_slides = [slide_1, slide_2]"
      "")
    "\n")
   repeated #:exists 'replace)
  (define old-base (base-path-for repeated))
  (when (file-exists? old-base) (delete-file old-base))
  (picts->pptx (load-program-picts repeated) repeated-deck #:width 480.0 #:height 270.0)
  (void (sync-once repeated repeated-deck #:workdir (build-path dir "explicit-w")))
  (check-true (drag-in-deck! repeated-deck 1 "Shared" 100.0 90.0))
  (define explicit-report
    (sync-once repeated repeated-deck #:workdir (build-path dir "explicit-w")
               #:atomic? #t))
  (check-equal? (sync-report-applied explicit-report) '())
  (check-regexp-match #rx"also appears on slides"
                      (cdr (first (sync-report-skipped explicit-report)))
                      "an explicit inner tag cannot bypass the family check")

  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "fun common(tag):"
      "  slide_canvas(~width: 480.0, ~height: 270.0,"
      "               at(40.0, 60.0, ~tag: tag,"
      "                  shape_pict(~width: 100.0, ~height: 40.0)))"
      "fun slide_1(): common(\"orphan\")"
      "def all_slides = [slide_1]"
      "")
    "\n")
   orphan #:exists 'replace)
  (check-exn #rx"no writable source site"
             (lambda () (validate-program-picts! orphan (load-program-picts orphan)))
             "a dynamic runtime tag cannot slip through initial validation"))

;; ------------------------------------- source ids distinguish repeated names

;; A real deck names shapes per slide, so "Title 1" exists on every slide. Each
;; source call nevertheless gets its own hidden id, and the merge has to patch
;; the call that drew slide 3 rather than either of the other titles.
(let ()
  (define-values (dir program exported) (fixture "perslide" "01-placeholders.pptx"))
  (define before (file->string program))
  (define tags (for/list ([s (in-list (find-at-sites program))]) (at-site-tag s)))
  (check-equal? (length (filter (lambda (t) (equal? (automatic-tag-name t) "Title 1"))
                                tags))
                3 "all three source ids retain the editor-facing name")
  (check-equal? (length (remove-duplicates tags)) (length tags)
                "while each source call has a distinct identity")

  (check-true (drag-in-deck! exported 3 "Title 1" 111.0 222.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define moves (filter (lambda (a) (eq? 'moved (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length moves) 1 "one element moved")
  (check-equal? (sync-action-slide (first moves)) 3 "on slide 3")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  ;; The patched `at` is the one under `slide-3`, which is the last of the three.
  (define (title-lines text)
    (for/list ([l (in-list (string-split text "\n"))]
               #:when (regexp-match? #rx"~name: \"Title 1\"" l))
      l))
  (define b (title-lines before))
  (define a (title-lines (file->string program)))
  (check-equal? (length a) 3)
  (check-equal? (first a) (first b) "slide 1's title is untouched")
  (check-equal? (second a) (second b) "slide 2's title is untouched")
  (check-not-equal? (third a) (third b) "slide 3's title moved")
  (check-true (and (regexp-match? #rx"111" (third a)) (regexp-match? #rx"222" (third a)))
              "to where it was dragged"))

;; ------------------------------------------ one tag, several elements

;; A tag names a *code site*, and one `at` in a loop draws several elements. So a
;; shared tag is not an error -- dragging all of them should move all of them,
;; because that is one correction on the one `at`. What cannot be expressed is an
;; edit to part of a family, and that is refused rather than guessed at.
(let ()
  (define dir (build-path work "family"))
  (make-directory* dir)
  (define program (build-path dir "loop.rhm"))
  (define deck (build-path dir "loop.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  for List (i: 0..3):"
            "    at(20.0 + i * 120.0, 60.0, ~tag: \"Box\","
            "       shape_pict(~width: 100.0, ~height: 60.0, ~shape: \"rect\","
            "                  ~fill: hex(\"4472C4\")))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))

  (define (xs) (for/list ([s (in-list (program-slide-states program))])
                 (map el-state-x (slide-state-elements s))))
  (define (at-line) (findf (lambda (l) (regexp-match? #rx"at[(]" l))
                           (string-split (file->string program) "\n")))

  ;; One `at`, three elements, one tag -- and the tag survives the round trip as
  ;; one tag rather than three invented ones.
  (reset!)
  (check-equal? (xs) '((20.0 140.0 260.0)) "the loop draws three")
  (define deck-tags
    (for/list ([s (in-list (deck-slide-states deck #:workdir (build-path dir "u")))])
      (map el-state-tag (slide-state-elements s))))
  (check-equal? deck-tags '(("Box" "Box" "Box"))
                "and they come back under the one tag they were drawn with")

  ;; Drag all of them the same way: one correction, all three move, spacing kept.
  (check-equal? (nudge-family-in-deck! deck 1 "Box" 50.0 30.0) 3 "all three were moved")
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(moved))
  (check-equal? (length (sync-report-applied r)) 1 "one edit, not three")
  (check-regexp-match #rx"~nudge: [[]50[.]0, 30[.]0[]]" (at-line))
  (check-equal? (xs) '((70.0 190.0 310.0)) "every element moved, and only by that")

  ;; Drag one of them: there is no single correction that does this.
  (reset!)
  (check-true (drag-in-deck! deck 1 "Box" 400.0 400.0) "one was dragged")
  (define r2 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(ambiguous))
  (check-equal? (sync-report-applied r2) '() "nothing was applied")
  (check-regexp-match #rx"did not all move the same way"
                      (cdr (first (sync-report-skipped r2))))
  (check-equal? (xs) '((20.0 140.0 260.0)) "and the program is untouched")

  ;; Delete one of them: a loop cannot say "all but that one".
  (reset!)
  (check-true (delete-from-deck! deck 1 "Box") "one was deleted")
  (define r3 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r3)) '(ambiguous))
  (check-equal? (sync-report-applied r3) '() "nothing was applied")
  (check-regexp-match #rx"deleting one of them" (cdr (first (sync-report-skipped r3))))
  (check-equal? (xs) '((20.0 140.0 260.0)) "and the program is untouched"))

;; Two `at` forms under one tag is a different thing: there is no way to tell
;; which of them an edit belongs to, so it is refused before anything is read.
(let ()
  (define dir (build-path work "twosites"))
  (make-directory* dir)
  (define program (build-path dir "two.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
          "  at(20.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"4472C4\"))),"
          "  at(200.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"ED7D31\")))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "two.pptx"))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (check-true (drag-in-deck! deck 1 "Box" 300.0 300.0))
  ;; The drag could land on either `at`, so it is not written. It is this
  ;; slide's edit that is refused, while unambiguous edits elsewhere can still
  ;; merge.
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '()
                "an edit that could land on either `at` form is not written")
  (check-equal? (length (sync-report-skipped r)) 1 "it is refused, and said so")
  (check-regexp-match #rx"appears 2 times" (cdr (first (sync-report-skipped r)))
                      "with what is wrong with the program")
  (check-regexp-match #rx"distinct literal" (cdr (first (sync-report-skipped r)))
                      "and what to do about it")
  (check-regexp-match #rx"at[(]200[.]0, 60[.]0" (file->string program)
                      "the program is untouched"))

;; ------------------------------------------ shapes added and deleted in the editor

;; Drawing a new shape in PowerPoint or deleting one there has to reach the
;; program, or the next export would put it back -- the round trip would not be
;; one. An addition is written as source the same way the translator writes it;
;; a deletion takes the `at` form and its comment and nothing else.
(let ()
  (define-values (dir program exported) (fixture "addremove" "05-realistic.pptx"))
  (define before (file->string program))

  (check-equal? (add-shape-to-deck! exported 2 "New Rect 42" #:x 200.0 #:y 300.0
                                    #:width 120.0 #:height 80.0)
                "New Rect 42")
  (check-true (delete-from-deck! exported 3 "TextBox 10") "the shape to delete was found")

  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define kinds (map sync-action-kind (sync-report-actions r)))
  (check-equal? (sort (map symbol->string kinds) string<?) '("added" "removed")
                "one addition and one deletion are seen")
  (check-equal? (length (sync-report-applied r)) 2 "and both are applied")

  ;; The addition reads like the rest of the file: a comment, then an `at`.
  (define after (file->string program))
  (check-regexp-match #rx"// New Rect 42 [(]id 9001[)]\n  at[(]200[.]0, 300[.]0, ~tag: \"New Rect 42\","
                      after)
  ;; The deletion leaves nothing behind -- no orphaned comment, no stray comma.
  (check-false (regexp-match? #rx"TextBox 10" after) "the deleted element is gone")
  (check-false (regexp-match? #rx",[ \t]*\n[ \t]*[)]" after) "and no dangling comma is left")

  ;; What matters is the invariant: the program now draws what the deck holds.
  (define prog-tags
    (for/list ([s (in-list (program-slide-states program))])
      (map (lambda (e) (visible-tag (el-state-tag e))) (slide-state-elements s))))
  (define deck-tags
    (for/list ([s (in-list (deck-states-by-name exported (build-path dir "cmp")))])
      (map (lambda (e) (visible-tag (el-state-tag e))) (slide-state-elements s))))
  (check-equal? prog-tags deck-tags "program and deck hold the same elements, in order")

  ;; And a second pass has nothing to do, so the edit converged.
  (define again (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-equal? (sync-report-actions again) '() "the merge converged")

  ;; The file still parses and still exports.
  (check-equal? (length (load-program-picts program)) 3 "the patched program still runs")
  (check-true (> (length (string-split after "\n")) 0))
  (check-not-equal? before after))

;; --------------------------------------- the merge does not pair slides by index

;; It used to, and taking the difference between two unrelated slides for edits
;; was actively destructive: one slide pasted at the front of a three-slide deck
;; produced 28 "edits" and deleted eight real elements from the program. Slides
;; are matched on their contents now, so an inserted slide does not shift the
;; ones after it.
(let ()
  (define-values (dir program exported) (fixture "slideset" "05-realistic.pptx"))
  (define other (build-path decks-dir "03-shapes.pptx"))
  (check-equal? (paste-slide! exported other 1) 4 "a slide was pasted in")
  (check-true (move-slide! exported 4 1) "at the front, so every later index shifts")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  ;; The one thing that changed, and nothing else.
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(added-slide)
                "only the new slide is reported")
  (define removed (filter (lambda (a) (eq? 'removed (sync-action-kind a)))
                          (sync-report-actions r)))
  (check-equal? removed '() "no element is thought to have been deleted"))

;; A conflict and a text edit both used to raise an arity error rather than being
;; reported: `sync-action` takes five fields and those two calls passed four.
;; Nothing exercised them, so editing text in PowerPoint always crashed.
(let ()
  (define-values (dir program exported) (fixture "retext" "05-realistic.pptx"))
  (check-true (retext-in-deck! exported 3 "TextBox 1" "Rewritten") "the text was changed")
  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (define texts (filter (lambda (a) (eq? 'retext (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length texts) 1 "a text edit is reported, not raised")
  (check-equal? (automatic-tag-name (sync-action-tag (first texts))) "TextBox 1")
  (check-equal? (length (sync-report-applied r)) 1 "and applied")
  (check-regexp-match #rx"\"Rewritten\"" (file->string program)
                      "the string literal in the source was replaced")
  (define again (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (sync-report-actions again) '() "and it converged"))

;; ------------------------------------------------ slides added in the editor

;; Pasting slides in from another deck. The program is where slides live, so a
;; pasted slide has to become a `def slide_N` and an entry in `all_slides` --
;; and the deck it came from has to survive a re-export unchanged, which is the
;; whole point of the round trip.
(define (deck-shape pptx dir tag)
  (for/list ([s (in-list (deck-states-by-name pptx (build-path dir tag)))])
    (map (lambda (e) (visible-tag (el-state-tag e))) (slide-state-elements s))))

(let ()
  (define-values (dir program exported) (fixture "addslide" "05-realistic.pptx"))
  (define other (build-path decks-dir "03-shapes.pptx"))

  (check-equal? (paste-slide! exported other 1) 4 "a slide was pasted in at the end")
  (define shape-after-paste (deck-shape exported dir "s1"))
  (check-equal? (length shape-after-paste) 4 "the deck has four slides")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(added-slide)
                "the new slide is the only thing to merge")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  ;; It reads like the rest of the file: a rule, a name, a canvas.
  (define src (file->string program))
  (check-regexp-match #rx"def slide_4 = slide_canvas[(]" src)
  (check-regexp-match #rx"[[]slide_1, slide_2, slide_3, slide_4[]]" src)
  (check-regexp-match #rx"\n  slide_4\n" src "and it is exported like the others")

  ;; The program now draws four slides, and re-exporting reproduces the deck
  ;; the editor holds -- same elements, same order.
  (check-equal? (length (load-program-picts program)) 4 "the program builds four slides")
  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape-after-paste
                "the re-exported deck holds what the editor held")

  ;; And a second pass has nothing to do.
  (check-equal? (sync-report-actions
                 (sync-once program exported #:workdir (build-path dir "w2")))
                '()
                "the merge converged"))

;; Where it lands matters: a slide pasted at the front belongs at the front of
;; `all_slides`. The definition itself goes after the last one -- `all_slides`
;; carries the order, so nothing has to be renumbered or reflowed.
(let ()
  (define-values (dir program exported) (fixture "addfront" "05-realistic.pptx"))
  (check-equal? (paste-slide! exported (build-path decks-dir "03-shapes.pptx") 1) 4)
  (check-true (move-slide! exported 4 1) "and moved to the front")
  (define shape (deck-shape exported dir "s1"))

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (length (sync-report-applied r)) 1 "the paste was applied")
  (check-regexp-match #rx"[[]slide_4, slide_1, slide_2, slide_3[]]"
                      (file->string program)
                      "the new slide is first in the order")

  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape
                "and the re-export has the slides in that order"))

;; Two at once, and one of them carrying an image, which has to be copied next
;; to the program or the program cannot draw it.
(let ()
  (define-values (dir program exported) (fixture "addtwo" "05-realistic.pptx"))
  (check-equal? (paste-slide! exported (build-path decks-dir "03-shapes.pptx") 1) 4)
  (check-equal? (paste-slide! exported (build-path decks-dir "04-pictures-groups.pptx") 1) 5)
  (define shape (deck-shape exported dir "s1"))
  (check-equal? (length shape) 5 "five slides now")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (length (sync-report-applied r)) 2 "both were applied")
  (define src (file->string program))
  (check-regexp-match #rx"[[]slide_1, slide_2, slide_3, slide_4, slide_5[]]" src)

  (check-equal? (length (load-program-picts program)) 5 "the program builds five slides")
  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape "and the deck round-trips"))

;; Deleting a slide in the editor deletes its definition and its entry in
;; `all_slides`. Nothing else: a program that names the slide somewhere else is
;; one the merge would be rewriting rather than following, and it says so.
(let ()
  (define-values (dir program exported) (fixture "delslide" "05-realistic.pptx"))
  (define before (file->string program))
  (define slides-before (length (regexp-match* #rx"def slide_[0-9]+ =" before)))
  (check-true (delete-slide! exported 2) "a slide was deleted in the editor")
  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(removed-slide))
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")
  (define after (file->string program))
  (check-equal? (length (regexp-match* #rx"def slide_[0-9]+ =" after)) (sub1 slides-before)
                "one definition fewer")
  (check-false (regexp-match? #rx"slide_2" after) "and nothing names it any more")
  (check-equal? (sync-report-actions (sync-once program exported #:workdir (build-path dir "w2")))
                '() "and it settled")

  ;; The same deletion, on a program that mentions the slide elsewhere.
  (define-values (dir2 program2 exported2) (fixture "delslide-used" "05-realistic.pptx"))
  (display-to-file (string-append (file->string program2) "\ndef also = slide_2\n")
                   program2 #:exists 'replace)
  (define kept (file->string program2))
  (check-true (delete-slide! exported2 2))
  (define r2 (sync-once program2 exported2 #:workdir (build-path dir2 "w2")))
  (check-equal? (sync-report-applied r2) '() "nothing is written")
  (check-regexp-match #rx"named 1 more time" (cdr (first (sync-report-skipped r2)))
                      "and the report says why")
  (check-equal? (file->string program2) kept "the program is untouched"))

;; ------------------------------------------------- a program holding a group

;; Reported from a live session as a crash: `it:shape-path-box` given an
;; `it:group`. The state builder's last branch assumed anything left was a
;; shape-path, and a group became a semantic item when groups started exporting
;; as groups -- so it was read as one. Nothing here had ever put a program with a
;; group through the sync.
(let ()
  (define-values (dir program exported) (fixture "grouped" "04-pictures-groups.pptx"))
  (define states (program-slide-states program))
  (check-true (pair? states) "a program with a group has slide states")
  (define kinds
    (remove-duplicates
     (append* (for/list ([s (in-list states)])
                (map el-state-kind (slide-state-elements s))))))
  (check-true (and (memq 'group kinds) #t)
              (format "and a group among them: ~s" kinds))

  ;; And it syncs: the group is one element to drag.
  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (sync-report-actions r) '() "nothing to merge at rest")
  (define found
    (for*/first ([s (in-list states)]
                 [e (in-list (slide-state-elements s))]
                 #:when (eq? 'group (el-state-kind e)))
      (cons (slide-state-index s) (el-state-tag e))))
  (check-true (and found #t) "the group has a tag")
  (when found
    (check-true (drag-in-deck! exported (car found) (cdr found) 120.0 140.0)
                "the group was dragged")
    (define r2 (sync-once program exported #:workdir (build-path dir "w2")))
    (check-equal? (map sync-action-kind (sync-report-actions r2)) '(moved)
                  "and the drag comes back as a move, on the group itself")
    (check-equal? (sync-action-tag (first (sync-report-actions r2))) (cdr found))))

;; LibreOffice 24.2 removes the alternative-description field from a group and
;; all of its children when it saves. Their ordinary names survive. Those names
;; are presentation metadata, not source identity, but a unique readable suffix
;; is enough to recover the child that an automatic source tag named.
(let ()
  (define-values (dir program exported)
    (fixture "grouped-stripped-descriptions" "04-pictures-groups.pptx"))
  (check-true
   (edit-slide-part! exported 2 #px" descr=\"glide-pptx:[^\"]*\"" "" #:all? #t)
   "the group descriptions were stripped")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork") #:dry-run? #t))
  (check-equal? (sync-report-actions r) '()
                "unique preserved group-child names recover their source identities"))

;; LibreOffice may reconstruct an ordinary shape when its text is edited. The
;; hidden description can disappear in that rewrite, but its unique readable
;; name remains and must still lead back to the source call.
(let ()
  (define-values (dir program exported)
    (fixture "edited-stripped-description" "03-shapes.pptx"))
  (check-true
   (edit-slide-part! exported 1 #px" descr=\"glide-pptx:[^\"]*\"" "" #:all? #t)
   "the editor stripped the hidden descriptions")
  (check-true (retext-in-deck! exported 1 "Rectangle 1" "still the rectangle")
              "and retyped the named shape")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork") #:dry-run? #t))
  (define acted (filter (lambda (a) (not (eq? 'noted (sync-action-kind a))))
                        (sync-report-actions r)))
  (check-equal? (map sync-action-kind acted) '(retext)
                "the unique readable name recovers the source identity")
  (check-equal? (automatic-tag-name (sync-action-tag (first acted))) "Rectangle 1"))

;; ============================================ the editing workflow, end to end

;; The single edits each have their own test above. What those do not cover is
;; the workflow: several edits in one pass, the same element edited twice, an
;; element added and then moved, and rounds of this in a row. Each round asserts
;; the same two things -- the program draws what the editor holds, and a second
;; pass has nothing left to do.

;; What the program and the deck each say is on a slide, as tag -> geometry, so
;; the two can be compared without a renderer.
;; The five numbers rounded, and the two flips as they are -- `el-geometry` ends
;; in booleans, which do not round.
(define (shape-of e)
  (cons (visible-tag (el-state-tag e))
        (append (for/list ([v (in-list (take (el-geometry e) 5))])
                  (/ (round (* 10.0 v)) 10.0))
                (list (and (el-state-flip-h? e) #t) (and (el-state-flip-v? e) #t)))))

(define (program-shape program)
  (for/list ([s (in-list (program-slide-states program))])
    (map shape-of (slide-state-elements s))))

(define (deck-shape-of pptx dir tag)
  (for/list ([s (in-list (deck-states-by-name pptx (build-path dir tag)))])
    (map shape-of (slide-state-elements s))))

;; One round: apply `edit!` to the deck, merge, and check that the program now
;; agrees with the deck and that the merge has settled.
(define (round! name dir program exported edit! [expect #f])
  (check-true (edit!) (format "~a: the edit went into the deck" name))
  (define r (sync-once program exported #:workdir (build-path dir "w")))
  (when expect
    (check-equal? (sort (map symbol->string (map sync-action-kind (sync-report-actions r)))
                        string<?)
                  (sort (map symbol->string expect) string<?)
                  (format "~a: what the merge saw" name)))
  (check-equal? (sync-report-skipped r) '()
                (format "~a: every action was applied" name))
  (check-equal? (program-shape program) (deck-shape-of exported dir (format "~a-cmp" name))
                (format "~a: the program now draws what the deck holds" name))
  (define again (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (sync-report-actions again) '()
                (format "~a: and the merge settled" name))
  r)

(let ()
  (define-values (dir program exported) (fixture "workflow" "05-realistic.pptx"))

  ;; Move.
  (round! "move" dir program exported
          (lambda () (drag-in-deck! exported 3 "Rounded Rectangle 2" 90.0 210.0))
          '(moved))

  ;; Resize -- the size is a literal on the leaf, not on `at`.
  (round! "resize" dir program exported
          (lambda () (resize-in-deck! exported 3 "Rounded Rectangle 2" 200.0 100.0))
          '(resized))

  ;; Retype the text.
  (round! "retext" dir program exported
          (lambda () (retext-in-deck! exported 3 "TextBox 1" "Rewritten"))
          '(retext))
  (check-regexp-match #rx"\"Rewritten\"" (file->string program))

  ;; Move and retype in one pass, on different elements.
  (round! "move+retext" dir program exported
          (lambda () (and (drag-in-deck! exported 3 "Right Arrow 3" 300.0 320.0)
                          (retext-in-deck! exported 3 "TextBox 1" "Twice")))
          '(moved retext))

  ;; Add a shape, then move the shape that was added.
  (round! "add" dir program exported
          (lambda () (and (add-shape-to-deck! exported 3 "New Box" #:x 40.0 #:y 400.0) #t))
          '(added))
  (round! "move the added one" dir program exported
          (lambda () (drag-in-deck! exported 3 "New Box" 120.0 430.0))
          '(moved))

  ;; Delete it again.
  (round! "delete" dir program exported
          (lambda () (delete-from-deck! exported 3 "New Box"))
          '(removed))
  (check-false (regexp-match? #rx"New Box" (file->string program))
               "the deleted element left no trace in the source")

  ;; Several elements moved at once.
  (round! "three at once" dir program exported
          (lambda () (and (drag-in-deck! exported 3 "Rounded Rectangle 2" 60.0 180.0)
                          (drag-in-deck! exported 3 "Rounded Rectangle 4" 260.0 180.0)
                          (drag-in-deck! exported 3 "Rounded Rectangle 6" 460.0 180.0)))
          '(moved moved moved))

  ;; And rounds of it, to see that nothing accumulates.
  (for ([i (in-range 3)])
    (round! (format "round ~a" i) dir program exported
            (lambda () (drag-in-deck! exported 3 "Right Arrow 5"
                                      (+ 200.0 (* i 30.0)) (+ 250.0 (* i 20.0))))
            '(moved)))

  ;; The file is still a program, and still the same one.
  (check-equal? (length (load-program-picts program)) 3 "three slides throughout"))

;; ------------------------------------------ rotating and mirroring an element

;; Dragging a line's endpoint past the other end mirrors the shape rather than
;; moving it, and rotating one turns it. Both were dropped: a rotation was
;; counted as applied while nothing was written -- there was no `~rotate:` to
;; write to and none was added -- and a mirror was not in the state a merge
;; compares, so it was never noticed at all.
(let ()
  (define dir (build-path work "turned"))
  (make-directory* dir)
  (define program (build-path dir "t.rhm"))
  (define deck (build-path dir "t.pptx"))
  (define (write-program! extra-at extra-leaf)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            (format "  at(100.0, 100.0, ~a~~tag: \"Line\"," extra-at)
            "     shape_pict(~width: 200.0, ~height: 120.0,"
            (format "                ~a~~shape: \"straightConnector1\"," extra-leaf)
            "                ~line: make_stroke(hex(\"000000\"), ~width: 3.0)))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))

  ;; Neither stated: both have to be added to the source.
  (write-program! "" "")
  (check-true (rotate-in-deck! deck 1 "Line" 30.0) "the line was rotated")
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r)) 1 "the rotation was applied")
  (check-regexp-match #rx"~rotate: 30[.]0" (file->string program)
                      "and written into the source, which had none")
  (check-equal? (sync-report-actions
                 (sync-once program deck #:workdir (build-path dir "w")))
                '() "and it settled")

  (write-program! "" "")
  (check-true (edit-after-tag! deck 1 "Line" #px"<a:xfrm" "<a:xfrm flipH=\"1\"")
              "the line was mirrored")
  (define r2 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r2)) 1 "the mirror was applied")
  (check-regexp-match #rx"~flip_h: #true" (file->string program)
                      "and written into the leaf, which had none")
  (check-equal? (sync-report-actions
                 (sync-once program deck #:workdir (build-path dir "w")))
                '() "and it settled")

  ;; Both stated: the values have to change, not be added again.
  (write-program! "~rotate: 20.0, " "~flip_h: #true, ")
  (check-true (rotate-in-deck! deck 1 "Line" 50.0))
  (check-true (edit-after-tag! deck 1 "Line" #px" flipH=\"1\"" "") "and un-mirrored")
  (define r3 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r3)) 1 "one action for both")
  (define src (file->string program))
  (check-regexp-match #rx"~rotate: 50[.]0" src "the rotation was changed in place")
  (check-regexp-match #rx"~flip_h: #false" src "and so was the mirror")
  (check-equal? (length (regexp-match* #rx"~rotate:" src)) 1 "no second rotation")
  (check-equal? (length (regexp-match* #rx"~flip_h:" src)) 1 "no second mirror"))

;; ------------------------------------------------ recolouring and restyling

;; Appearance is the code's, and it used to be simply dropped: recolouring a
;; shape in the editor vanished without a word. It is reported now, and written
;; where the source states it as a literal.
;;
;; A colour with a name is different. It belongs to everything that uses it, so
;; it is rewritten only when everything that uses it changed the same way --
;; which is the rule a repeated tag already follows.
(let ()
  (define dir (build-path work "restyle"))
  (make-directory* dir)
  (define program (build-path dir "r.rhm"))
  (define deck (build-path dir "r.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def brand = hex(\"4472C4\")"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Plain\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"))),"
            "  at(240.0, 60.0, ~tag: \"One\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
            "  at(400.0, 60.0, ~tag: \"Two\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
            "  at(60.0, 200.0, ~tag: \"Words\","
            "     textbox(~width: 300.0, ~height: 60.0,"
            "             para(run(\"hello\", ~font: \"Arial\", ~size: 24.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (recolour! tag hex)
    (edit-after-tag! deck 1 tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                     (format "<a:srgbClr val=\"~a\"/>" hex)))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))

  ;; A literal colour is rewritten where it stands.
  (reset!)
  (check-true (recolour! "Plain" "70AD47") "the shape was recoloured")
  (define r (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(restyle)
                "and that is reported as a restyle, not passed over")
  (check-equal? (length (sync-report-applied r)) 1 "and applied")
  (check-regexp-match #rx"~fill: hex[(]\"70AD47\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A named colour, changed on one of the two that use it: refused, and the
  ;; report says which name and how many did not change with it.
  (reset!)
  (check-true (recolour! "One" "70AD47"))
  (define r2 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(restyle))
  (check-equal? (sync-report-applied r2) '() "nothing was written")
  (check-regexp-match #rx"brand" (cdr (first (sync-report-skipped r2)))
                      "and the report names the colour")
  (check-regexp-match #rx"1 other element that did not change"
                      (cdr (first (sync-report-skipped r2))))
  (check-regexp-match #rx"def brand = hex[(]\"4472C4\"[)]" (file->string program)
                      "the definition is untouched")

  ;; Both of them: the definition is rewritten, once.
  (reset!)
  (check-true (recolour! "One" "70AD47"))
  (check-true (recolour! "Two" "70AD47"))
  (define r3 (sync!))
  (check-equal? (length (sync-report-applied r3)) 2 "both are applied")
  (define src (file->string program))
  (check-regexp-match #rx"def brand = hex[(]\"70AD47\"[)]" src "through the definition")
  (check-equal? (length (regexp-match* #rx"70AD47" src)) 1 "which is written once")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A save is one thing. Two edits, one of which cannot be written: neither is
  ;; written, and the program is left exactly as it was. Writing the one that
  ;; could would leave the program and the deck each holding part of what was
  ;; done in the editor, with nothing to say which part.
  (reset!)
  (define before (file->string program))
  (check-true (drag-in-deck! deck 1 "Plain" 300.0 300.0) "one edit that can be written")
  (check-true (recolour! "One" "70AD47") "and one that cannot")
  (define ra (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (length (sync-report-actions ra)) 2 "both are reported")
  (check-equal? (sync-report-applied ra) '() "and neither is written")
  (check-equal? (file->string program) before "the program is untouched")
  (check-false (sync-report-base-written? ra)
               "and the base is left alone, so the next save tries again")
  ;; The same merge, without the rule: the one that can be written is.
  (define rp (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied rp)) 1 "the drag lands on its own")
  (check-equal? (length (sync-report-skipped rp)) 1 "and the colour is still refused")

  ;; A font and a size on a single-run body.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"typeface=\"[^\"]*\"" "typeface=\"Courier New\""))
  (check-true (edit-after-tag! deck 1 "Words" #px"sz=\"[0-9]+\"" "sz=\"4000\""))
  (define r4 (sync!))
  (check-equal? (length (sync-report-applied r4)) 1 "font and size in one action")
  (define src4 (file->string program))
  (check-regexp-match #rx"~font: \"Courier New\"" src4)
  (check-regexp-match #rx"~size: 40[.]0" src4)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; A bare style name can be a lexical parameter. Even if a top-level or
;; imported definition happens to have the same spelling, its identity is not
;; established by that coincidence and it must not be rewritten.
(let ()
  (define dir (build-path work "lexical-style"))
  (make-directory* dir)
  (define program (build-path dir "lexical.rhm"))
  (define deck (build-path dir "lexical.pptx"))
  (display-to-file
   (string-join
    '("#lang rhombus/and_meta"
      "import: lib(\"glide-pptx/runtime.rhm\") open"
      "export: all_slides"
      "def fill = hex(\"112233\")"
      "fun box(fill):"
      "  at(60.0, 60.0,"
      "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: fill))"
      "def slide_1 = slide_canvas("
      "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
      "  box(hex(\"4472C4\"))"
      ")"
      "def all_slides = [slide_1]"
      "")
    "\n")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 480.0 #:height 270.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (define tag (at-site-tag (first (find-at-sites program))))
  (check-true
   (edit-after-tag! deck 1 tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                    "<a:srgbClr val=\"70AD47\"/>"))
  (define r (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-applied r) '())
  (check-regexp-match #rx"def fill = hex[(]\"112233\"[)]" (file->string program)
                      "a same-named top-level definition was untouched"))

;; ------------------------------------------------------- a slideshow to start from

;; `raco glide --new` writes one, through the same emitter that writes an
;; imported deck -- so what it writes is what glide reads back, rather than a
;; second idiom to keep in step. The check is that: write it, open it, and the
;; two agree with nothing to merge.
(let ()
  (define dir (build-path work "new"))
  (make-directory* dir)
  (define program (build-path dir "new.rhm"))
  (define deck (build-path dir "new.pptx"))
  (write-rhombus-deck (starter-deck) program #:source-name #f)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (define picts (load-program-picts program))
  (check-equal? (length picts) 2 "two slides to start from")
  ;; No size given, which is what the watcher does: the deck takes the
  ;; program's own.
  (picts->pptx picts deck)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (sync-report-actions (sync-once program deck #:workdir (build-path dir "w")
                                                #:atomic? #t))
                '()
                "and the two of them agree from the start")
  (check-regexp-match #rx"~name: \"Title\"" (file->string program)
                      "with a readable editor name but no identity bookkeeping"))

;; --------------------------------------------------------- the size of the deck

;; Changing the slide size is one edit for the whole deck, and a generated
;; program states it once: `def slide_width = 959.976`, which every canvas
;; names. So the edit lands on the definition rather than on each slide.
(let ()
  (define dir (build-path work "slide-size"))
  (make-directory* dir)
  (define program (build-path dir "s.rhm"))
  (define deck (build-path dir "s.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_width = 720.0"
          "def slide_height = 540.0"
          "def slide_1 = slide_canvas("
          "  ~width: slide_width, ~height: slide_height, ~background: hex(\"FFFFFF\"),"
          "  at(60.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\")))"
          ")"
          "def slide_2 = slide_canvas("
          "  ~width: slide_width, ~height: slide_height, ~background: hex(\"FFFFFF\"),"
          "  at(60.0, 60.0, ~tag: \"Other\","
          "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"70AD47\")))"
          ")"
          "def all_slides = [slide_1, slide_2]"
          "")
    "\n")
   program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  ;; 12192000 EMU is 960pt, which is the 16:9 the editors offer.
  (check-true (edit-part! deck "ppt/presentation.xml"
                          #px"<p:sldSz cx=\"[0-9]+\" cy=\"[0-9]+\""
                          "<p:sldSz cx=\"12192000\" cy=\"6858000\"")
              "the deck was made 16:9")
  (define r (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(resized-deck)
                "one edit for the deck, not one per slide")
  (check-equal? (length (sync-report-applied r)) 1 "and it was written")
  (define src (file->string program))
  (check-regexp-match #rx"def slide_width = 960[.]0" src "into the definition")
  (check-regexp-match #rx"def slide_height = 540[.]0" src)
  (check-equal? (length (regexp-match* #rx"960[.]0" src)) 1 "which is written once")
  (check-equal? (sync-report-actions (sync-once program deck #:workdir (build-path dir "w")
                                                #:atomic? #t))
                '() "and it settled"))

;; ------------------------------------------------- a scratch from another program

;; The scratch is picked up rather than cleared when a session starts, so edits
;; made while nothing was watching are merged rather than thrown away. A folder
;; that came from somewhere else -- copied with a project, left by another
;; program -- holds a deck and a base that have nothing to do with this one, and
;; merging those would be merging someone else's edits.
(let ()
  (define dir (build-path work "foreign-base"))
  (make-directory* dir)
  (define program (build-path dir "a.rhm"))
  (define other (build-path dir "b.rhm"))
  (define deck (build-path dir "a.pptx"))
  (define text
    (string-join
     (list "#lang rhombus/and_meta"
           "import:"
           "  lib(\"glide-pptx/runtime.rhm\") open"
           "export:"
           "  all_slides"
           "def slide_1 = slide_canvas("
           "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
           "  at(60.0, 60.0, ~tag: \"Box\","
           "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\")))"
           ")"
           "def all_slides = [slide_1]"
           "")
     "\n"))
  (display-to-file text program #:exists 'replace)
  (display-to-file text other #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  ;; A base recorded for `b.rhm`, sitting where `a.rhm`'s belongs.
  (void (sync-once other deck #:workdir (build-path dir "w")))
  (copy-file (base-path-for other) (base-path-for program) #t)
  (define msg (with-handlers ([exn:fail? exn-message])
                (sync-once program deck #:workdir (build-path dir "w"))))
  (check-regexp-match #rx"written for a different program" msg)
  (check-regexp-match #rx"b[.]rhm" msg "and says which one")
  (check-regexp-match #rx"Delete" msg "and what to do about it"))

;; -------------------------------------------- an editor that states less than we do

;; Reported from live use: a Keynote round trip produced 298 refusals and
;; merged nothing. Keynote's pptx export leaves out a great deal that ours
;; writes -- a body's anchor and insets, a paragraph's alignment and spacing, a
;; run's typeface and size -- and every one of those absences was being read as
;; the user having removed something. Under the rule that a save lands whole or
;; not at all, one of them blocked the lot.
;;
;; Nothing in any editor removes a typeface or an anchor; they only change. So
;; a side that does not state one did not say, and only the properties whose
;; absence the program can state -- a fill, an outline, an arrowhead, a crop, a
;; bullet -- are read as removals.
(let ()
  (define dir (build-path work "terse-editor"))
  (make-directory* dir)
  (define program (build-path dir "t.rhm"))
  (define deck (build-path dir "t.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
          "  at(60.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"))),"
          "  at(60.0, 200.0, ~tag: \"Words\","
          "     textbox(~width: 400.0, ~height: 90.0, ~anchor: #'center,"
          "             para(~align: #'right, ~line_spacing: pair(#'percent, 1.5),"
          "                  run(\"hello \", ~font: \"Georgia\", ~size: 24.0),"
          "                  run(\"world\", ~size: 18.0, ~bold: #true))))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define base (base-path-for program))
  (when (file-exists? base) (delete-file base))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (define before (file->string program))

  ;; An editor that writes the text but not what we said about it.
  (check-true (edit-slide-part! deck 1 #px"<a:bodyPr [^>/]*" "<a:bodyPr" #:all? #t)
              "the body properties were dropped")
  (check-true (edit-slide-part! deck 1 #px"(?s:<a:pPr[^>]*>(?:(?!</a:pPr>).)*</a:pPr>)" ""
                                #:all? #t)
              "and the paragraph properties")
  (check-true (edit-slide-part! deck 1 #px"(?s:<a:rPr[^>]*>(?:(?!</a:rPr>).)*</a:rPr>)"
                                "<a:rPr/>" #:all? #t)
              "and the run properties")
  (define r (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-actions r) '()
                "an unstated property is not a statement, so there is nothing to report")
  (check-equal? (sync-report-skipped r) '() "and nothing stops the save")
  (check-equal? (file->string program) before "and the program is untouched")

  ;; And a real edit in the same terse deck still lands.
  (check-true (drag-in-deck! deck 1 "Box" 200.0 240.0) "a shape was dragged")
  (define r2 (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(moved))
  (check-equal? (length (sync-report-applied r2)) 1 "the drag is written")
  (check-equal? (sync-report-skipped r2) '() "and nothing stopped it")
  (check-regexp-match #rx"~font: \"Georgia\"" (file->string program)
                      "the typeface the program states is still what it states")

  ;; An editor that does state the defaults is a different matter: it says the
  ;; text is Calibri 18 plain, which is what a deck says when it says nothing.
  ;; That is noted rather than written, since the two cannot be told apart --
  ;; and a note does not stop a save.
  (check-true (edit-slide-part! deck 1 #px"<a:rPr/>"
                                "<a:rPr sz=\"1800\" b=\"0\"><a:latin typeface=\"Calibri\"/></a:rPr>"
                                #:all? #t)
              "the editor stated the defaults")
  (define r3 (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-true (and (memq 'noted (map sync-action-kind (sync-report-actions r3))) #t)
              "which is noted")
  (check-equal? (sync-report-skipped r3) '() "and does not stop a save")
  (check-regexp-match #rx"when it says nothing" (cdr (first (sync-report-notes r3))))
  (check-regexp-match #rx"~font: \"Georgia\"" (file->string program)
                      "the program keeps what it says")
  (check-regexp-match #rx"at[(]200[.]0, 240[.]0, ~tag: \"Box\"" (file->string program)
                      "and the drag that did land is still there"))

;; ------------------------------------------------ where a new shape is written

;; A shape drawn in the editor is somewhere in the drawing order, and that
;; order is the order of the `at` forms. Writing every new form last put every
;; new shape on top -- and since the deck is rewritten from the program as soon
;; as the merge has written it, there was no second pass in which to fix it.
(let ()
  (define dir (build-path work "adding"))
  (make-directory* dir)
  (define program (build-path dir "a.rhm"))
  (define deck (build-path dir "a.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Bottom\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"))),"
            "  at(240.0, 60.0, ~tag: \"Middle\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"4472C4\"))),"
            "  at(420.0, 60.0, ~tag: \"Top\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"70AD47\")))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (define (order)
    (map second (regexp-match* #px"~tag: \"([^\"]*)\"" (file->string program)
                               #:match-select values)))

  ;; Drawn, then sent one step back: the form goes between the two it is drawn
  ;; between.
  (reset!)
  (check-true (and (add-shape-to-deck! deck 1 "Inserted" #:after "Bottom") #t)
              "a shape was added over the bottom one")
  (define r (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(added))
  (check-equal? (length (sync-report-applied r)) 1 "and it was written")
  (check-equal? (order) '("Bottom" "Inserted" "Middle" "Top")
                "in the order the deck draws them")
  (check-equal? (sync-report-actions (sync!)) '()
                "and it settled -- no second pass to fix the order")

  ;; And one sent all the way back goes first.
  (reset!)
  (check-true (and (add-shape-to-deck! deck 1 "Backdrop" #:under? #t) #t)
              "a shape was added under everything")
  (check-equal? (length (sync-report-applied (sync!))) 1 "and written")
  (check-equal? (order) '("Backdrop" "Bottom" "Middle" "Top"))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Drawn on top, which is where a shape starts: last, as before.
  (reset!)
  (check-true (and (add-shape-to-deck! deck 1 "Above") #t))
  (check-equal? (length (sync-report-applied (sync!))) 1)
  (check-equal? (order) '("Bottom" "Middle" "Top" "Above"))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; ------------------------------------------------------- grouping and ungrouping

;; Grouping two shapes in the editor is one edit, not an addition and two
;; deletions -- read as three it looked like most of a slide being thrown away,
;; and the merge refused the whole slide. The `at` forms move inside a
;; `group_pict`, so what the code says about them survives: a colour shared
;; with something else stays shared, and a comment stays where it was.
(let ()
  (define dir (build-path work "grouping"))
  (make-directory* dir)
  (define program (build-path dir "g.rhm"))
  (define deck (build-path dir "g.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def brand = hex(\"4472C4\")"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  // the orange one"
            "  at(60.0, 60.0, ~tag: \"Box\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"))),"
            "  at(240.0, 60.0, ~tag: \"Bare\","
            "     shape_pict(~width: 90.0, ~height: 60.0, ~fill: brand)),"
            "  at(60.0, 200.0, ~tag: \"Words\","
            "     textbox(~width: 300.0, ~height: 60.0, para(run(\"hi\", ~size: 24.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  ;; A pass can leave the drawing order for the next one, so this settles.
  (define (settle!)
    (let loop ([n 0])
      (define r (sync!))
      (cond
        [(null? (sync-report-actions r)) n]
        [(or (> n 3) (null? (sync-report-applied r)))
         (fail (format "stuck on ~s, refusing ~s"
                       (map sync-action-kind (sync-report-actions r))
                       (map cdr (sync-report-skipped r))))
         n]
        [else (loop (add1 n))])))

  (reset!)
  (check-true (group-in-deck! deck 1 "Box" "Bare" #:name "Pair") "two shapes were grouped")
  (define r (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(grouped)
                "one edit, not an addition and two deletions")
  (check-equal? (length (sync-report-applied r)) 1 "and it was written")
  (define src (file->string program))
  (check-regexp-match #rx"~tag: \"Pair\",\n *group_pict[(]~width: 270[.]0" src
                      "a group where the first of them was")
  (check-regexp-match #rx"at[(]0[.]0, 0[.]0, ~tag: \"Box\"" src
                      "its children positioned inside it")
  (check-regexp-match #rx"at[(]180[.]0, 0[.]0, ~tag: \"Bare\"" src)
  (check-regexp-match #rx"~fill: brand" src "the shared colour is still shared")
  (check-regexp-match #rx"// the orange one" src "and the comment came with its shape")
  (check-equal? (length (regexp-match* #rx"~tag: \"Box\"" src)) 1 "written once")
  (settle!)

  ;; And ungrouped again: the forms come back out rather than being written
  ;; afresh from the deck, which would turn `brand` into a literal colour.
  (check-true (ungroup-in-deck! deck 1 "Pair") "the group was dissolved")
  (define r2 (sync!))
  (check-true (positive? (length (sync-report-applied r2))) "which was written")
  (check-equal? (sync-report-skipped r2) '() "and nothing was refused")
  (define src2 (file->string program))
  (check-false (regexp-match? #rx"group_pict" src2) "the group is gone")
  (check-regexp-match #rx"at[(]60[.]0, 60[.]0, ~tag: \"Box\"" src2
                      "and its children are back where they were")
  (check-regexp-match #rx"at[(]240[.]0, 60[.]0, ~tag: \"Bare\"" src2)
  (check-regexp-match #rx"~fill: brand" src2 "with the shared colour intact")
  (check-regexp-match #rx"// the orange one" src2 "and the comment too")
  (void (settle!)))

;; ------------------------------------------------------------------ pictures

;; A picture has arguments of its own: how much of it is cropped away and how
;; see-through it is. Both are things the editor's inspector changes, and
;; neither had anywhere to land -- the crop was not even compared.
(let ()
  (define dir (build-path work "pictures"))
  (make-directory* (build-path dir "media"))
  (copy-file (build-path media-dir "checker.png")
             (build-path dir "media" "checker.png") #t)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "p.pptx"))
  (define (reset!)
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
            "  at(60.0, 60.0, ~tag: \"Photo\","
            "     image_pict(media(\"checker.png\"), 200.0, 150.0))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))
  (define (one! r why) (check-equal? (length (sync-report-applied r)) 1 why))

  ;; Cropped with the crop tool.
  (reset!)
  ;; The replacement is written as it stands -- no backreferences -- so the
  ;; edits here keep what they match out of it.
  (check-true (edit-after-tag! deck 1 "Photo" #px"</a:blip><a:stretch>"
                               "</a:blip><a:srcRect l=\"10000\" t=\"5000\"/><a:stretch>")
              "the picture was cropped")
  (one! (sync!) "and the crop was written")
  (check-regexp-match #rx"~crop: [[]0[.]1, 0[.]05, 0[.]0, 0[.]0[]]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And uncropped again: the argument says there is none rather than going.
  (check-true (edit-after-tag! deck 1 "Photo" #px"<a:srcRect[^>]*/>" "")
              "the crop was taken off")
  (one! (sync!) "and that was written")
  (check-regexp-match #rx"~crop: #false" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Swapped for another image. The two sides name the same bytes differently
  ;; -- the program a path, the deck a part inside itself -- so the file is
  ;; what they have in common: it comes to sit beside the program, under a name
  ;; that does not clobber what is already there, and the source is pointed at
  ;; it. Rewriting the name alone would leave it naming a file that is absent.
  (reset!)
  (define before-media (sort (map path->string (directory-list (build-path dir "media")))
                             string<?))
  (check-equal? before-media '("checker.png") "one picture to begin with")
  (check-true (with-unpacked-deck deck
                (lambda (d)
                  (for ([f (in-list (directory-list (build-path d "ppt" "media")))])
                    (copy-file (build-path media-dir "gradient.png")
                               (build-path d "ppt" "media" f) #t))
                  #t))
              "the picture in the deck was swapped for another")
  (define rs (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions rs)) '(restyle))
  (one! rs "and it was written")
  (define srcs (file->string program))
  (check-false (regexp-match? #rx"media[(]\"checker[.]png\"[)]" srcs)
               "the source no longer names the old file")
  (define named (second (regexp-match #px"media[(]\"([^\"]*)\"[)]" srcs)))
  (check-true (file-exists? (build-path dir "media" named))
              "and the file it does name is there")
  (check-true (file-exists? (build-path dir "media" "checker.png"))
              "with the one that was there left alone")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Faded with the opacity slider.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Photo" #px"></a:blip>"
                               "><a:alphaModFix amt=\"40000\"/></a:blip>")
              "the picture was faded")
  (one! (sync!) "and the opacity was written")
  (check-regexp-match #rx"~opacity: 0[.]4" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; ------------------------------------------------------ retyping styled text

;; A line with a bold word in it is two runs, and there is no one literal
;; holding the line. Retyping a word of it used to be refused for that. The run
;; the change fell inside is the one that gets it -- and when the change falls
;; across two of them, that is a guess, so it is still refused.
(let ()
  (define dir (build-path work "runs"))
  (make-directory* dir)
  (define program (build-path dir "t.rhm"))
  (define deck (build-path dir "t.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Mixed\","
            "     textbox(~width: 400.0, ~height: 60.0,"
            "             para(run(\"hello \", ~size: 24.0),"
            "                  run(\"world\", ~size: 18.0, ~bold: #true)))),"
            "  at(60.0, 200.0, ~tag: \"Bullets\","
            "     textbox(~width: 400.0, ~height: 120.0,"
            "             para(~align: #'left, run(\"first line\", ~size: 18.0)),"
            "             para(~align: #'right, run(\"second line\", ~size: 30.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))
  (define (one! r why) (check-equal? (length (sync-report-applied r)) 1 why))
  (define (retype! tag from to)
    (edit-after-tag! deck 1 tag (pregexp (format "<a:t>~a</a:t>" (regexp-quote from)))
                     (format "<a:t>~a</a:t>" to)))

  ;; The bold word, retyped.
  (reset!)
  (check-true (retype! "Mixed" "world" "planet") "the bold word was retyped")
  (define r (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(retext))
  (one! r "and it was written")
  (define src (file->string program))
  (check-regexp-match #rx"run[(]\"planet\", ~size: 18[.]0, ~bold: #true[)]" src
                      "into the run it belongs to")
  (check-regexp-match #rx"run[(]\"hello \"" src "the other run is untouched")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The plain half, retyped.
  (reset!)
  (check-true (retype! "Mixed" "hello " "goodbye "))
  (one! (sync!) "the first run was written")
  (check-regexp-match #rx"run[(]\"goodbye \", ~size: 24[.]0[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; One line of two paragraphs.
  (reset!)
  (check-true (retype! "Bullets" "second line" "second thoughts"))
  (one! (sync!) "the second paragraph was written")
  (define srcb (file->string program))
  (check-regexp-match #rx"run[(]\"second thoughts\"" srcb)
  (check-regexp-match #rx"run[(]\"first line\"" srcb "the first is untouched")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Styling one run of several. Bolding a word is as ordinary as retyping it,
  ;; and it used to be dropped without a word: the state read a body's weight
  ;; and size off the first run, so a change to any other was invisible.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Mixed" #px"sz=\"1800\"" "sz=\"1800\" i=\"1\"")
              "the second run was italicised")
  (define ri (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions ri)) '(restyle))
  (one! ri "and it was written")
  (define srci (file->string program))
  (check-regexp-match #rx"run[(]\"world\", ~size: 18[.]0, ~bold: #true, ~italic: #true[)]" srci
                      "into the run it belongs to")
  (check-regexp-match #rx"run[(]\"hello \", ~size: 24[.]0[)]" srci "the first is untouched")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And the report names which run it was.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Mixed" #px"sz=\"1800\"" "sz=\"3600\""))
  (one! (sync!) "the size of the second run was written")
  (check-regexp-match #rx"run[(]\"world\", ~size: 36[.]0" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; One paragraph of several, the same way.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Bullets" #px"algn=\"r\"" "algn=\"ctr\"")
              "the second paragraph was centred")
  (one! (sync!) "and it was written")
  (define srcp2 (file->string program))
  (check-regexp-match #rx"para[(]~align: #'center, run[(]\"second line\"" srcp2)
  (check-regexp-match #rx"para[(]~align: #'left, run[(]\"first line\"" srcp2
                      "the first is untouched")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; What cannot be written names the run in the report, so "font" and "font of
  ;; run 2" are told apart.
  (reset!)
  (display-to-file
   (regexp-replace
    #rx"~size: 18[.]0, ~bold: #true"
    (regexp-replace #rx"def slide_1 = " (file->string program)
                    "def heading_font = \"Georgia\"\ndef slide_1 = ")
    "~size: 18.0, ~bold: #true, ~font: heading_font")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync!))
  (check-true (edit-after-tag! deck 1 "Mixed" #px"typeface=\"Georgia\""
                               "typeface=\"Courier New\"")
              "the second run's typeface was changed")
  (define rn (sync!))
  (check-equal? (sync-report-applied rn) '() "which is not a literal to write")
  (check-regexp-match #rx"font of run 2" (format "~a" (map cdr (sync-report-skipped rn)))
                      "and the report says which run")

  ;; A retyping that swallows the whole line: it spans both runs, and which of
  ;; them it belongs to is not something to guess at.
  (reset!)
  (check-true (retype! "Mixed" "hello " "one "))
  (check-true (retype! "Mixed" "world" "two"))
  (define rc (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions rc)) '(retext))
  (check-equal? (sync-report-applied rc) '() "nothing is written")
  (check-regexp-match #rx"crosses runs" (cdr (first (sync-report-skipped rc)))
                      "and the report says why")
  (check-regexp-match #rx"run[(]\"hello \"" (file->string program)
                      "the program is left as it was"))

;; ------------------------------------------- appearance the source never states

;; Most of what an editor does to a shape's appearance is not a value the
;; program already has: a solid line has no `~dash:` to rewrite, and a shape
;; with no fill has no `~fill:` at all. Reporting those is worse than useless --
;; the deck and the program disagree and the user is told to fix it by hand --
;; so the argument is added, and one that the editor took away is removed.
(let ()
  (define dir (build-path work "appearance"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "p.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Box\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
            "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
            "  at(240.0, 60.0, ~tag: \"Bare\","
            "     shape_pict(~width: 90.0, ~height: 60.0)),"
            "  at(60.0, 200.0, ~tag: \"Words\","
            "     textbox(~width: 300.0, ~height: 60.0,"
            "             para(run(\"hello\", ~font: \"Arial\", ~size: 24.0,"
            "                      ~color: hex(\"101010\"))))),"
            "  at(60.0, 300.0, ~tag: \"Plain\","
            "     textbox(~width: 300.0, ~height: 60.0, ~anchor: #'top,"
            "             para(~align: #'left, run(\"plain\", ~size: 18.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))
  (define (applied! r why) (check-equal? (length (sync-report-applied r)) 1 why))
  ;; The slide part itself, for what no element carries.
  (define (edit-slide! pptx slide rx to)
    (with-unpacked-deck pptx
      (lambda (d)
        (define part (build-path d "ppt" "slides" (format "slide~a.xml" slide)))
        (define t (file->string part))
        (and (regexp-match? rx t)
             (begin (display-to-file (regexp-replace rx t to) part #:exists 'replace) #t)))))
  ;; Keynote writes a colour into the run's properties, whether or not the run
  ;; had one.
  (define (recolour-run! tag hex)
    (or (edit-after-tag! deck 1 tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/></a:solidFill>"
                         (format "<a:srgbClr val=\"~a\"/></a:solidFill>" hex))
        (edit-after-tag! deck 1 tag #px"(<a:rPr[^>]*)/>"
                         (format "\\1><a:solidFill><a:srgbClr val=\"~a\"/></a:solidFill></a:rPr>" hex))))

  ;; A dash on a line that is solid: the argument is added inside the stroke.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"</a:ln>"
                               "<a:prstDash val=\"dash\"/></a:ln>")
              "the line was dashed")
  (applied! (sync!) "and the dash was written")
  (check-regexp-match #rx"~dash: #'dash" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A fill made translucent: the alpha is added inside its own `hex`.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box" #px"<a:srgbClr val=\"ED7D31\"/>"
               "<a:srgbClr val=\"ED7D31\"><a:alpha val=\"50000\"/></a:srgbClr>")
              "the fill was made translucent")
  (applied! (sync!) "and the opacity was written")
  (check-regexp-match #rx"hex[(]\"ED7D31\", ~alpha: 0[.]5[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; An outline on a shape that has none: a whole stroke, with the width the
  ;; editor gave it, since neither has anywhere to be written on its own.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:ln><a:noFill/></a:ln>"
               "<a:ln w=\"19050\"><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></a:ln>")
              "the shape was given an outline")
  (applied! (sync!) "and the stroke was written")
  (check-regexp-match #rx"~line: make_stroke[(]hex[(]\"FF0000\"[)], ~width: 1[.]5[)]"
                      (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A fill on a shape that has none.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:noFill/><a:ln>"
               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill><a:ln>")
              "the shape was given a fill")
  (applied! (sync!) "and the fill was written")
  (check-regexp-match #rx"~fill: hex[(]\"00B050\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And taken away again: `#false` is how the program says a shape has none,
  ;; and the whole argument goes, not the colour inside it.
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill>"
               "<a:noFill/>")
              "the fill was removed")
  (applied! (sync!) "and the removal was written")
  (check-regexp-match #rx"~fill: #false" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The outline of the shape that has one.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:ln w=\"[0-9]+\"[^>]*>.*?</a:ln>"
                               "<a:ln><a:noFill/></a:ln>")
              "the outline was removed")
  (applied! (sync!) "and the removal was written")
  (define src (file->string program))
  (check-regexp-match #rx"~line: #false" src)
  (check-false (regexp-match? #rx"make_stroke" src) "the stroke call is gone")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Bold and italic are flags: absent means false, so they are added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:rPr lang=\"en-US\" sz=\"2400\""
                               "<a:rPr lang=\"en-US\" sz=\"2400\" b=\"1\" i=\"1\"")
              "the text was bolded and italicised")
  (applied! (sync!) "and both were written")
  (define src2 (file->string program))
  (check-regexp-match #rx"~bold: #true" src2)
  (check-regexp-match #rx"~italic: #true" src2)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A colour the source never states for its text.
  (reset!)
  (check-true (recolour-run! "Plain" "CC0000") "the text was recoloured")
  (applied! (sync!) "and the colour was written")
  (check-regexp-match #rx"~color: hex[(]\"CC0000\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Centring text is the first thing anyone does in an editor. The paragraph
  ;; does not state an alignment, so one is added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:pPr algn=\"l\"" "<a:pPr algn=\"ctr\"")
              "the text was centred")
  (applied! (sync!) "and the alignment was written")
  (check-regexp-match #rx"~align: #'center" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And where it does state one, that one changes -- rather than a second
  ;; `~align:` appearing beside it, which would not compile.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Plain" #px"<a:pPr algn=\"l\"" "<a:pPr algn=\"r\"")
              "the text was right-aligned")
  (applied! (sync!) "and the alignment was written")
  (define srca (file->string program))
  (check-regexp-match #rx"~align: #'right" srca)
  (check-equal? (length (regexp-match* #rx"~align:" srca)) 1
                "the one the paragraph already had, and no second one beside it")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Line spacing is a pair, so the whole value is written.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:spcPct val=\"100000\"/>"
                               "<a:spcPct val=\"150000\"/>")
              "the lines were spaced out")
  (applied! (sync!) "and the spacing was written")
  (check-regexp-match #rx"~line_spacing: pair[(]#'percent, 1[.]5[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Space before and after a paragraph, in points.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"</a:lnSpc>"
                               (string-append "</a:lnSpc><a:spcBef><a:spcPts val=\"1200\"/></a:spcBef>"
                                              "<a:spcAft><a:spcPts val=\"600\"/></a:spcAft>"))
              "the paragraph was given room")
  (applied! (sync!) "and both were written")
  (define srcs (file->string program))
  (check-regexp-match #rx"~space_before: 12[.]0" srcs)
  (check-regexp-match #rx"~space_after: 6[.]0" srcs)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A dash the source already states: `#'dash` is a quote and a name, and the
  ;; two of them together are what gets rewritten.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"</a:ln>"
                               "<a:prstDash val=\"dash\"/></a:ln>"))
  (applied! (sync!) "the dash was added")
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:prstDash val=\"dash\"/>"
                               "<a:prstDash val=\"sysDot\"/>")
              "and then changed")
  (applied! (sync!) "and the change was written")
  (define srcd (file->string program))
  (check-equal? (length (regexp-match* #rx"~dash:" srcd)) 1 "in place, not beside")
  (check-false (regexp-match? #rx"~dash: #'dash" srcd) "and it is not the old one")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Where the text sits in its box, which the editor's inspector changes
  ;; without touching a word of the text. None of it is stated, so all of it is
  ;; added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"anchor=\"t\"" "anchor=\"ctr\"")
              "the text was anchored to the middle")
  (check-true (edit-after-tag! deck 1 "Words" #px"wrap=\"square\"" "wrap=\"none\"")
              "and wrapping turned off")
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:noAutofit/>" "<a:normAutofit/>")
              "and set to shrink on overflow")
  (check-true (edit-after-tag! deck 1 "Words" #px"lIns=\"91440\"" "lIns=\"228600\"")
              "and given a wider inset")
  (applied! (sync!) "all four in one action")
  (define srcb (file->string program))
  (check-regexp-match #rx"~anchor: #'center" srcb)
  (check-regexp-match #rx"~wrap: #false" srcb)
  (check-regexp-match #rx"~autofit: #'shrink" srcb)
  (check-regexp-match #rx"~insets: insets[(]18[.]0, 3[.]6, 7[.]2, 3[.]6[)]" srcb)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The box that states its anchor has that one changed.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Plain" #px"anchor=\"t\"" "anchor=\"b\"")
              "the text was anchored to the bottom")
  (applied! (sync!) "and written")
  (define srcp (file->string program))
  (check-regexp-match #rx"~anchor: #'bottom" srcp)
  (check-equal? (length (regexp-match* #rx"~anchor:" srcp)) 1 "in place")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; An arrowhead on a line that has none. `line_end(...)` is a call, so the
  ;; whole of it is what is written -- and `#false` is what says there is none.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"</a:ln>"
                               "<a:tailEnd type=\"triangle\" w=\"med\" len=\"med\"/></a:ln>")
              "the line was given an arrowhead")
  (applied! (sync!) "and it was written")
  (check-regexp-match #rx"~tail: line_end[(]#'triangle, \"med\", \"med\"[)]"
                      (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Changed to another kind, in place.
  (check-true (edit-after-tag! deck 1 "Box" #px"type=\"triangle\"" "type=\"oval\"")
              "the arrowhead was changed")
  (applied! (sync!) "and the change was written")
  (define srce (file->string program))
  (check-regexp-match #rx"~tail: line_end[(]#'oval" srce)
  (check-equal? (length (regexp-match* #rx"~tail:" srce)) 1 "in place, not beside")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And taken off again.
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:tailEnd[^>]*/>" "")
              "the arrowhead was removed")
  (applied! (sync!) "and the removal was written")
  (check-regexp-match #rx"~tail: #false" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A shape given an outline that has an arrowhead on it gets both at once,
  ;; since the stroke it belongs to is what is being written.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:ln><a:noFill/></a:ln>"
               (string-append "<a:ln w=\"19050\"><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill>"
                              "<a:headEnd type=\"stealth\" w=\"med\" len=\"med\"/></a:ln>"))
              "the shape was given an arrow")
  (applied! (sync!) "and the stroke was written")
  (check-regexp-match #rx"~line: make_stroke[(]hex[(]\"FF0000\"[)], ~width: 1[.]5, ~head: line_end[(]#'stealth"
                      (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The slide's own paint, which no element carries: repainting a background
  ;; used to be as invisible to the merge as it is obvious on the screen.
  (reset!)
  (check-true (edit-slide! deck 1 #px"<a:srgbClr val=\"FFFFFF\"/>"
                           "<a:srgbClr val=\"102040\"/>")
              "the slide was repainted")
  (define rb (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions rb)) '(repainted))
  (check-equal? (length (sync-report-applied rb)) 1 "and it was written")
  (check-regexp-match #rx"~background: hex[(]\"102040\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Renaming a shape in the editor's object list is not renaming its tag: the
  ;; tag lives in the description, and the merge still knows which `at` it is.
  (reset!)
  (check-true (edit-slide! deck 1 #px"name=\"Box\" descr="
                           "name=\"Blue rectangle\" descr=")
              "the shape was renamed")
  (check-true (drag-in-deck! deck 1 "Box" 120.0 140.0) "and dragged")
  (define rn (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions rn)) '(moved)
                "the rename is not a change, the drag is")
  (check-equal? (length (sync-report-applied rn)) 1 "and it was written")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A gradient the source does stand behind: it cannot be told which stops the
  ;; editor picked, but a plain colour replaces the whole of it.
  (reset!)
  (display-to-file
   (regexp-replace #rx"~fill: hex[(]\"ED7D31\"[)]" (file->string program)
                   "~fill: gradient_fill([pair(0.0, hex(\"FF0000\")), pair(1.0, hex(\"0000FF\"))], 0.0)")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync!))
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:gradFill.*?</a:gradFill>"
                               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill>")
              "the gradient was made a plain colour")
  (applied! (sync!) "and the whole fill was written")
  (define srcg (file->string program))
  (check-regexp-match #rx"~fill: hex[(]\"00B050\"[)]" srcg)
  (check-false (regexp-match? #rx"gradient_fill" srcg) "the gradient call is gone")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A gradient is not a colour the source can be told to be, and saying so is
  ;; the point: the alternative is writing one stop of it and calling it done.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>"
               (string-append "<a:gradFill><a:gsLst>"
                              "<a:gs pos=\"0\"><a:srgbClr val=\"FF0000\"/></a:gs>"
                              "<a:gs pos=\"100000\"><a:srgbClr val=\"0000FF\"/></a:gs>"
                              "</a:gsLst><a:lin ang=\"0\"/></a:gradFill>"))
              "the fill was made a gradient")
  (define rg (sync!))
  (check-equal? (sync-report-applied rg) '() "which is not written")
  (check-regexp-match #rx"~fill: hex[(]\"ED7D31\"[)]" (file->string program)
                      "and the fill it could not write is left as it was")
  (check-regexp-match #rx"gradient" (cdr (first (sync-report-skipped rg)))
                      "and the report says it was made a gradient"))

;; --------------------------------- the rest of what an editor can do to a deck

;; Surveyed by simulating each action and seeing what the merge made of it. Four
;; were silent -- a dashed line, a translucent fill, bringing a shape to the
;; front, reordering the slides -- and one was worse than silent: duplicating a
;; shape wrote a second `at` under the same tag, which left the program in a
;; state no later sync could read.
(let ()
  (define dir (build-path work "actions"))
  (make-directory* dir)
  (define program (build-path dir "a.rhm"))
  (define deck (build-path dir "a.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Box\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
            "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
            "  at(240.0, 60.0, ~tag: \"Other\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"4472C4\")))"
            ")"
            "def slide_2 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Second\","
            "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"70AD47\")))"
            ")"
            "def all_slides = [slide_1, slide_2]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))

  ;; A line's colour and width are literals inside `make_stroke`.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box"
               #px"<a:ln[^>]*><a:solidFill><a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
               "<a:ln w=\"76200\"><a:solidFill><a:srgbClr val=\"FF0000\"/>"))
  (define r (sync!))
  (check-equal? (length (sync-report-applied r)) 1 "the line was restyled")
  (define src (file->string program))
  (check-regexp-match #rx"make_stroke[(]hex[(]\"FF0000\"[)]" src "its colour written")
  (check-regexp-match #rx"~width: 6[.]0" src "and its width")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Bringing a shape to the front is reported, since rewriting it would mean
  ;; moving the `at` form rather than a literal in it.
  (reset!)
  (check-true (bring-to-front! deck 1 "Box") "the shape was brought to the front")
  (define r2 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(restacked)
                "one action for the slide, not one per shape")
  (check-equal? (length (sync-report-applied r2)) 1 "and it was applied")
  ;; The order of the `at` forms is the order they are drawn in, so the form
  ;; itself moves -- and the commas and indentation stay where they were.
  (define src2 (file->string program))
  (check-true (> (caar (regexp-match-positions #rx"\"Box\"" src2))
                 (caar (regexp-match-positions #rx"\"Other\"" src2)))
              "`Box` is written after `Other` now")
  (check-equal? (length (regexp-match* #rx"  at[(]" src2)) 3
                "every `at` in the file is still there")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Two edits in one save, one of them inside the run of forms the other
  ;; moves. Bringing a shape to the front rewrites that whole run, so a restyle
  ;; written into one of the forms at the same time was laid over the top of it
  ;; -- and what came out did not parse.
  (reset!)
  (check-true (bring-to-front! deck 1 "Box") "brought to the front")
  ;; Something that changes the length of the form it sits in, so an edit
  ;; written at offsets the move has already shifted lands in the wrong place.
  (check-true (edit-after-tag! deck 1 "Other"
                               #px"<a:solidFill><a:srgbClr val=\"4472C4\"/></a:solidFill>"
                               "<a:noFill/>")
              "and the other one's fill taken away, in the same save")
  (define r-both (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-skipped r-both) '() "both were written")
  (check-equal? (sort (map (lambda (x) (format "~a" (sync-action-kind x)))
                           (sync-report-applied r-both))
                      string<?)
                '("restacked" "restyle"))
  (define src-both (file->string program))
  (check-false (regexp-match? #rx"4472C4" src-both) "the fill was taken away")
  (check-true (> (caar (regexp-match-positions #rx"\"Box\"" src-both))
                 (caar (regexp-match-positions #rx"\"Other\"" src-both)))
              "and the form moved past it")
  (check-equal? (length (regexp-match* #rx"  at[(]" src-both)) 3
                "with every `at` still there, once")
  (check-true (pair? (program-picts program)) "and the program still reads")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A slide whose last element is a group. A new one is written at the
  ;; canvas's own indentation: the forms inside a `group_pict` sit far to the
  ;; right, and taking the indentation from one of them wrote the new element
  ;; into the middle of the group, where it did not belong and did not parse.
  (reset!)
  (check-true (group-in-deck! deck 1 "Box" "Other" #:name "Pair") "the two were grouped")
  (void (sync!))
  (void (sync!))
  (check-true (and (add-shape-to-deck! deck 1 "Drawn") #t) "and something new was drawn")
  (define r-added (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r-added)) '(added) "it was added")
  (define src-added (file->string program))
  (check-regexp-match #px"(?m:^  at[(][^\n]*~tag: \"Drawn\")" src-added
                      "at the canvas's own indentation")
  (check-true (pair? (program-picts program)) "and the program still reads")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Two new shapes in one save. They are written at the same place, and one
  ;; insertion must not be read as sitting inside the other.
  (reset!)
  (check-true (and (add-shape-to-deck! deck 1 "First New") #t) "one was drawn")
  (check-true (and (add-shape-to-deck! deck 1 "Second New") #t) "and another")
  (define r-two (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-skipped r-two) '() "both were written")
  (define src-two (file->string program))
  (check-regexp-match #rx"~tag: \"First New\"" src-two "the first is there")
  (check-regexp-match #rx"~tag: \"Second New\"" src-two "and the second")
  (check-true (pair? (program-picts program)) "and the program still reads")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A new shape and a reordering in one save: the insertion sits inside the
  ;; run of forms the reordering moves.
  (reset!)
  (check-true (and (add-shape-to-deck! deck 1 "Late") #t) "something was drawn")
  (check-true (bring-to-front! deck 1 "Box") "and a shape brought to the front")
  (define r-mix (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-skipped r-mix) '() "both were written")
  (define src-mix (file->string program))
  (check-regexp-match #rx"~tag: \"Late\"" src-mix "the new one is there")
  (check-equal? (length (regexp-match* #rx"~tag: \"Late\"" src-mix)) 1 "once")
  (check-true (pair? (program-picts program)) "and the program still reads")
  ;; The drawing order is the order of the forms, and the new form has to be
  ;; written before there is an order to put it in -- so it takes the pass
  ;; after, which is why a save settles before the deck is written again.
  (define r-order (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r-order)) '(restacked)
                "the order followed on the next pass")
  (define src-order (file->string program))
  (define (written-at tag)
    (caar (regexp-match-positions (regexp (format "~s" tag)) src-order)))
  (check-true (< (written-at "Other") (written-at "Late") (written-at "Box"))
              "written in the order they are drawn in")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Duplicating a shape inside a group gives the group two shapes under one
  ;; name, since the copy is given the name it was copied from. There is one
  ;; `at` form here for the two of them, and writing it twice is a program no
  ;; later sync can read -- so the grouping is refused and said.
  (reset!)
  (check-true (group-in-deck! deck 1 "Box" "Other" #:name "Pair") "two were grouped")
  (check-true (and (duplicate-in-deck! deck 1 "Box") #t) "and one of them duplicated")
  (define was (file->string program))
  (define r-clash (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-applied r-clash) '() "nothing was written")
  (check-equal? (length (sync-report-skipped r-clash)) 1 "and the grouping was refused")
  (check-regexp-match #rx"one name" (cdr (car (sync-report-skipped r-clash)))
                      "for the reason it was")
  (check-equal? (file->string program) was "the program was left as it was")
  (check-true (pair? (program-picts program)) "and it still reads")

  ;; A shape the editor drew and did not name. LibreOffice writes `name=""` for
  ;; one, and nameless it has no key: the merge could not name it, could not
  ;; find it, and could not write it, so it refused -- and with a save landing
  ;; whole or not at all, that refusal took everything else in the save with it.
  (reset!)
  (check-true (and (edit-slide-part!
                    deck 1 #px"</p:spTree>"
                    (string-append
                     "<p:sp><p:nvSpPr><p:cNvPr id=\"99\" name=\"\"/><p:cNvSpPr/><p:nvPr/>"
                     "</p:nvSpPr><p:spPr><a:xfrm><a:off x=\"3000000\" y=\"2000000\"/>"
                     "<a:ext cx=\"1000000\" cy=\"800000\"/></a:xfrm>"
                     "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>"
                     "<a:solidFill><a:srgbClr val=\"70AD47\"/></a:solidFill></p:spPr>"
                     "<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody></p:sp>"
                     "</p:spTree>"))
                   #t)
              "a nameless shape was drawn in the editor")
  (define r-nameless (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (sync-report-skipped r-nameless) '() "the save went through")
  (check-equal? (map sync-action-kind (sync-report-applied r-nameless)) '(added)
                "and it was added")
  (check-regexp-match #rx"~tag: \"Shape 99\"" (file->string program)
                      "under a name made from the id the file gave it")
  (check-true (pair? (program-picts program)) "and the program still reads")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The program is the source and there is one copy of it, so a save is a
  ;; rename rather than a write into the file it is replacing -- and the mode
  ;; comes across with it, since a temporary file is made with whatever the
  ;; umask says.
  (reset!)
  (file-or-directory-permissions program #o600)
  (check-true (drag-in-deck! deck 1 "Box" (* 12700.0 300.0) (* 12700.0 200.0)) "dragged")
  (define r-mode (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r-mode)) '(moved) "and written")
  (check-equal? (file-or-directory-permissions program 'bits) #o600
                "the program is still the user's alone")

  ;; Two copies made in one save. The editor gives each the name it was copied
  ;; from, and the program's own names are read before any of the save is
  ;; written -- so both were told the name was free and both took it, which is
  ;; two `at` forms under one tag and a program no later sync can read.
  (reset!)
  (check-true (and (duplicate-in-deck! deck 1 "Box") #t) "it was duplicated")
  (check-true (and (duplicate-in-deck! deck 1 "Box") #t) "and duplicated again")
  (define r-copies (sync-once program deck #:workdir (build-path dir "w") #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r-copies)) '(added added)
                "both were written")
  (define src-copies (file->string program))
  (define tags
    (map (lambda (m) (cadr m))
         (regexp-match* #rx"~tag: \"([^\"]*)\"" src-copies #:match-select values)))
  (check-equal? (length (remove-duplicates tags)) (length tags)
                (format "every tag is its own: ~s" tags))
  (check-true (pair? (program-picts program)) "and the program still reads")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A comment above an `at` describes that `at`, so it moves with it. One
  ;; standing on its own between two of them describes neither, and moving the
  ;; forms around it would leave it describing whatever ended up beneath it --
  ;; so that is a reason not to.
  (reset!)
  (display-to-file
   (regexp-replace #rx"  at[(]240[.]0" (file->string program)
                   "  // the ones below are the blue ones\n\n  at(240.0")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync!))
  (check-true (bring-to-front! deck 1 "Box"))
  (define rc (sync!))
  (check-equal? (sync-report-applied rc) '() "nothing is moved")
  (check-regexp-match #rx"move them yourself" (cdr (first (sync-report-skipped rc)))
                      "and the report says so")

  ;; Reordering the slides is `all_slides`, which is a literal list.
  (reset!)
  (check-true (move-slide! deck 2 1) "the slides were reordered")
  (define r3 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r3)) '(reordered))
  (check-equal? (length (sync-report-applied r3)) 1 "and applied")
  (check-regexp-match #rx"all_slides = [[]slide_2, slide_1[]]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Duplicating a shape gives the copy a name of its own, so the program stays
  ;; readable.
  (reset!)
  (check-true (duplicate-in-deck! deck 1 "Other") "the shape was duplicated")
  (define r4 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r4)) '(added))
  (check-equal? (length (sync-report-applied r4)) 1 "the copy was added")
  (check-regexp-match #rx"~tag: \"Other [(]2[)]\"" (file->string program)
                      "under a name of its own")
  (check-equal? (sync-report-actions (sync!)) '()
                "and the program is still one a sync can read"))

;; ------------------------------------ a helper that says what can be edited
;;
;; A talk draws things with helpers of its own -- a bubble pinned to a token, a
;; callout with a spike -- and what they draw has nothing in the source an edit
;; could be written into: the position is measured from the thing it points at,
;; and the words are arguments the helper lays out itself. So the call says what
;; it offers. `~tag:` makes what it draws one element with a name; `~nudge:` is
;; where a drag is recorded, leaving the measured position alone; and the
;; strings it holds are its runs.
;;
;; The helper's own `at` is not a site -- its arguments are variables -- which
;; is exactly why the call has to be one.
(let ()
  (define dir (build-path work "helper"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export: all_slides"
          "fun bubble(words, ~x: x, ~y: y, ~tag: tag = #false, ~nudge: nudge = #false):"
          "  at(x, y, ~tag: tag, ~nudge: nudge,"
          "     textbox(~width: 120.0, ~height: 30.0, ~wrap: #false, para(run(words))))"
          "def anchor = 30.0"
          "def slide_1 = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))),"
          "  bubble(\"hello there\", ~x: anchor * 4, ~y: anchor * 5,"
          "         ~tag: \"why\", ~nudge: [0.0, 0.0]))"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (define why (findf (lambda (s) (equal? "why" (at-site-tag s))) sites))
  (check-true (and why #t) "the call is a site")
  (check-false (at-site-x why) "with no position of its own to write")
  (check-true (and (at-site-nudge why) #t) "a correction that can be written")
  (check-equal? (length (first (at-site-texts why))) 1 "and one run, which is its words")

  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  ;; Dragged: the measured position stays, and the drag is the correction.
  (check-true (drag-in-deck! deck 1 "why" 200.0 150.0) "the bubble is dragged")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "which is written")
  (check-regexp-match #rx"~x: anchor [*] 4" (file->string program)
                      "the helper keeps working out where it goes")
  (check-regexp-match #rx"~nudge: [[]80[.]0, 0[.]0[]]" (file->string program)
                      "and the drag is recorded as how far off that was")
  ;; Retyped: the words are the call's own string.
  (check-true (retext-in-deck! deck 1 "why" "hello world") "the bubble is retyped")
  (define r2 (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(retext)
                "which is written too")
  (check-regexp-match #rx"bubble[(]\"hello world\"" (file->string program)
                      "into the string the helper was handed")
  ;; And it settles: the program draws what the deck holds.
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "and there is nothing left to merge"))

;; ---------------------------------------- the shape a shape is drawn as
;;
;; Making a rounded box an ellipse in the editor changes nothing else about it:
;; same place, same size, same fill, same words. Compared on everything but the
;; geometry, the two sides agreed and the change was reported nowhere, written
;; nowhere, and thrown away by the next deck the program wrote.
;;
;; Written where the source says it. A shape with adjustments states its
;; geometry as `~geom: preset_geom("roundRect", ...)` and a plain one as
;; `~shape: "roundRect"`, and the name inside whichever it is gets rewritten.
(let ()
  (define dir (build-path work "shape"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export: all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Plain\","
          "     shape_pict(~width: 80.0, ~height: 40.0, ~shape: \"roundRect\","
          "                ~fill: hex(\"4472C4\"))),"
          "  at(140.0, 20.0, ~tag: \"Adjusted\","
          "     shape_pict(~width: 80.0, ~height: 40.0,"
          "                ~geom: preset_geom(\"roundRect\", [pair(\"adj\", \"val 33878\")]),"
          "                ~fill: hex(\"ED7D31\"))))"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (edit-after-tag! deck 1 "Plain" #px"prst=\"roundRect\"" "prst=\"ellipse\"")
              "the plain one is made an ellipse in the editor")
  (check-true (edit-after-tag! deck 1 "Adjusted" #px"prst=\"roundRect\"" "prst=\"ellipse\"")
              "and so is the one with adjustments")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(restyle restyle)
                "both are seen and written")
  (define src (file->string program))
  (check-regexp-match #rx"~shape: \"ellipse\"" src "the plain one says so")
  (check-regexp-match #rx"preset_geom[(]\"ellipse\"" src
                      "and the adjusted one says so inside its own geometry")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "and there is nothing left to merge"))

;; ------------------------------------------- the shape's own handles
;;
;; A rounded rectangle's roundness is not its name: dragging the yellow handle
;; leaves the preset called "roundRect" and changes the adjustment inside it. The
;; compared state held the name alone, so a shape reshaped in the editor was a
;; save with nothing in it -- no edit written and nothing reported either, which
;; is the one failure a person cannot see.
(let ()
  (define dir (build-path work "shape-handles"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define (fresh!)
    (delete-directory/files (build-path dir ".glide") #:must-exist? #f)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export: all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 320.0, ~height: 240.0,"
            "  at(20.0, 20.0, ~tag: \"Blob\","
            "     shape_pict(~width: 80.0, ~height: 40.0,"
            "                ~geom: preset_geom(\"roundRect\", [pair(\"adj\", \"val 17500\")]),"
            "                ~fill: hex(\"ED7D31\"))),"
            "  at(140.0, 20.0, ~tag: \"Plain\","
            "     shape_pict(~width: 80.0, ~height: 40.0, ~shape: \"roundRect\","
            "                ~fill: hex(\"4472C4\"))))"
            "def all_slides = [slide_1]")
      "\n")
     program #:exists 'replace))
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (fresh!)
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (edit-after-tag! deck 1 "Blob" #px"val 17500" "val 30000")
              "the handle is dragged in the editor")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(restyle)
                "which is seen and written")
  (check-regexp-match #rx"preset_geom[(]\"roundRect\", [[]pair[(]\"adj\", \"val 30000\"[)][]][)]"
                      (file->string program)
                      "into the list the source states them in")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "and there is nothing left to merge")

  ;; A shape named with `~shape:` states no adjustments at all, and stating none
  ;; means the preset's own defaults -- which a deck writes out in full. So an
  ;; adjustment the source does not state cannot be told from the defaults
  ;; written out, and reshaping that one is not a difference either side can
  ;; see. Pinned here because it is the edge of what the comparison can say.
  (fresh!)
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (edit-after-tag! deck 1 "Plain" #px"<a:avLst></a:avLst>"
                               "<a:avLst><a:gd name=\"adj\" fmla=\"val 30000\"/></a:avLst>")
              "the plain one is reshaped in the editor")
  (define r2 (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (sync-report-actions r2) '()
                "which the merge does not see, the source stating no adjustment"))

;; ------------------------------- words the program works out, and a drag beside
;;
;; A helper's words can be computed, or shared between everything it draws with
;; them: an e-graph's `update` nodes are three elements with one word between
;; them. Retyping one of those is not something the source can be made to say --
;; there is no literal to rewrite and nothing a person could go and fix -- so it
;; is said and the rest of the save still lands. It used to take every other
;; edit in the save with it, which is a drag lost for asking a question.
(let ()
  (define dir (build-path work "computed-words"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export: all_slides"
          "def shared = \"update\""
          "fun word(x, y, ~tag: tag :: String, ~nudge: nudge = #false):"
          "  at(x, y, ~tag: tag, ~nudge: nudge,"
          "     textbox(~width: 90.0, ~height: 20.0, ~wrap: #false, para(run(shared))))"
          "def slide_1 = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))),"
          "  word(40.0, 120.0, ~tag: \"One\"), word(40.0, 160.0, ~tag: \"Two\"))"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (retext-in-deck! deck 1 "One" "combine") "one of them is retyped")
  (check-true (drag-in-deck! deck 1 "Box" 200.0 150.0) "and something else is dragged")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "the drag lands")
  (check-regexp-match #rx"at[(]200[.]0, 150[.]0, ~tag: \"Box\"" (file->string program)
                      "in the program")
  (check-equal? (length (sync-report-skipped r)) 1 "and the retyping is reported")
  (check-regexp-match #rx"not literals here" (cdr (first (sync-report-skipped r)))
                      "as words the program works out")
  (check-true (sync-report-base-written? r)
              "the save landed, rather than being refused whole"))

;; --------------------------------- a name on the pict, wherever it is placed
;;
;; `at`'s `~tag:` names what a canvas places, which leaves out everything placed
;; some other way: a helper composing a picture of its own, a blob laid over a
;; slide with a `put` of the program's own. Those arrived on the slide with no
;; name, and nothing an editor did to one could be traced back -- a talk's
;; e-class regions, drawn exactly that way, were 14 elements nobody could move.
;;
;; `tag(p, "name")` puts the name on the pict, so it travels with what it names.
;; The call that places it is where an edit to it is written, and the position
;; is that call's own business, so a drag is recorded as a correction.
(let ()
  (define dir (build-path work "named-pict"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "fun put(base, p, x, y, ~nudge: nudge = #false):"
          "  def [dx, dy] = if nudge | nudge | [0.0, 0.0]"
          "  pc.overlay(~horiz: #'left, ~vert: #'top, pc.Pict.from_handle(base),"
          "             pc.Pict.from_handle(p).pad(~left: x + dx, ~top: y + dy))"
          "def blob:"
          "  shape_pict(~width: 90.0, ~height: 50.0,"
          "             ~geom: preset_geom(\"roundRect\", [pair(\"adj\", \"val 33878\")]),"
          "             ~fill: hex(\"929292\", ~alpha: 0.3))"
          "def slide_1:"
          "  def canvas = slide_canvas("
          "    ~width: 320.0, ~height: 240.0,"
          "    at(20.0, 20.0, tag(shape_pict(~width: 60.0, ~height: 40.0,"
          "                                  ~fill: hex(\"4472C4\")), \"Box\")))"
          "  put(canvas, tag(blob, \"root class\"), 150.0, 120.0)"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (for*/list ([st (in-list (deck-states-by-name deck (build-path dir "cmp")))]
                            [e (in-list (slide-state-elements st))])
                  (list (el-state-tag e) (el-state-kind e)))
                '(("Box" shape) ("root class" shape))
                "both are named elements, and both are still shapes")
  (void (sync-once program deck #:workdir w))
  (check-true (drag-in-deck! deck 1 "root class" 40.0 60.0) "the blob is dragged")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "which is written")
  (check-regexp-match #rx"put[(]canvas, tag[(]blob, \"root class\"[)], 150[.]0, 120[.]0,"
                      (file->string program)
                      "leaving the program's own placement alone")
  (check-regexp-match #rx"~nudge: [[]-110[.]0, -60[.]0[]]" (file->string program)
                      "with the drag recorded as a correction on the call that placed it")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "and there is nothing left to merge"))

;; A call is a site wherever it sits, not only at the head of its group. A
;; program that draws a row of things binds each step -- `let p = region(p, ...,
;; ~tag: "class 1")` -- and the call that placed the thing is still where a drag
;; on it belongs.
(let ()
  (define dir (build-path work "bound-call"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "fun as_p(v): if v is_a pc.Pict | v | pc.Pict.from_handle(v)"
          "fun put(base, p, x, y, ~nudge: nudge = #false):"
          "  def [dx, dy] = if nudge | nudge | [0.0, 0.0]"
          "  pc.overlay(~horiz: #'left, ~vert: #'top, as_p(base),"
          "             as_p(p).pad(~left: x + dx, ~top: y + dy))"
          "fun slide_1():"
          "  fun region(p, x, y, ~tag: name, ~nudge: nudge = #false):"
          "    put(p, tag(shape_pict(~width: 60.0, ~height: 30.0,"
          "                          ~fill: hex(\"929292\", ~alpha: 0.3)), name),"
          "        x, y, ~nudge: nudge)"
          "  def canvas = slide_canvas("
          "    ~width: 320.0, ~height: 240.0,"
          "    at(20.0, 20.0, tag(shape_pict(~width: 60.0, ~height: 40.0,"
          "                                  ~fill: hex(\"4472C4\")), \"Box\")))"
          "  let p = region(canvas, 100.0, 60.0, ~tag: \"class 1\")"
          "  region(p, 100.0, 140.0, ~tag: \"class 2\")"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (check-equal? (sort (for/list ([s (in-list (find-at-sites program))]) (or (at-site-tag s) "?"))
                      string<?)
                '("Box" "class 1" "class 2")
                "both bound calls are found, and the `at` too")
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (drag-in-deck! deck 1 "class 1" 40.0 60.0) "the first is dragged")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "which is written")
  (check-regexp-match #rx"~tag: \"class 1\", ~nudge: [[]-60[.]0, 0[.]0[]]"
                      (file->string program)
                      "into the row that placed it")
  (check-equal? (length (regexp-match* #rx"~nudge: [[]" (file->string program))) 1
                "and only that row")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "with nothing left to merge"))

;; A size the call works out for itself has nowhere to be written, and saying so
;; is the point: written as a correction to the position alone, the new size was
;; lost and the save still counted the resize as applied -- and a resize that
;; did not move the corner wrote `~nudge: [0.0, 0.0]` and called that the edit.
(let ()
  (define dir (build-path work "computed-size"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "fun as_p(v): if v is_a pc.Pict | v | pc.Pict.from_handle(v)"
          "fun put(base, p, x, y, ~nudge: nudge = #false):"
          "  def [dx, dy] = if nudge | nudge | [0.0, 0.0]"
          "  pc.overlay(~horiz: #'left, ~vert: #'top, as_p(base),"
          "             as_p(p).pad(~left: x + dx, ~top: y + dy))"
          "fun slide_1():"
          "  fun blob(p, x, y, ~tag: name, ~nudge: nudge = #false):"
          "    put(p, tag(shape_pict(~width: 60.0, ~height: 30.0,"
          "                          ~fill: hex(\"929292\", ~alpha: 0.3)), name),"
          "        x, y, ~nudge: nudge)"
          "  def canvas = slide_canvas("
          "    ~width: 320.0, ~height: 240.0,"
          "    at(20.0, 20.0, ~tag: \"Box\","
          "       shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
          "  blob(canvas, 100.0, 60.0, ~tag: \"class 1\")"
          "def all_slides = [slide_1]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-true (resize-in-deck! deck 1 "class 1" 100.0 50.0) "it is resized in the deck")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (sync-report-applied r) '()
                "a resize with nowhere to write the size applies nothing")
  (check-regexp-match #rx"its size is computed" (format-sync-report r)
                      "and the report says which part could not be written")
  ;; The helper's own `~nudge:` argument is in the source either way; what must
  ;; not be there is a correction written by the merge.
  (check-equal? (regexp-match* #rx"~nudge: [[]" (file->string program)) '()
                "and a correction of nothing is not written either")
  ;; A drag on the same shape still lands. The deck keeps the size the source
  ;; had nowhere to take, so this is still a resize -- and the position part of
  ;; it is written, with the size said again.
  (check-true (drag-in-deck! deck 1 "class 1" 40.0 60.0) "it is dragged")
  (define r2 (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(resized)
                "which is applied")
  (check-regexp-match #rx"~nudge: [[]-60[.]0, 0[.]0[]]" (file->string program)
                      "as a correction on the call that placed it")
  (check-regexp-match #rx"its size is computed" (format-sync-report r2)
                      "and the size is still said to be unwritable"))

;; ------------------------------- slides listed as calls rather than names
;;
;; `all_slides` need not be a list of bare names. A talk wraps its slides --
;; `in_section(0, slide_2)`, `divider(1)` -- and every structural edit refused
;; on that account: reordering the slides said the list was not a literal one,
;; and deleting a slide said the merge could not see its entry, so a whole talk
;; could not be reordered in the editor at all.
(let ()
  (define dir (build-path work "call-entries"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (define (fresh!)
    (delete-directory/files (build-path dir ".glide") #:must-exist? #f)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "  pict as pc"
            "export: all_slides"
            "def slide_width = 320.0"
            "def slide_height = 240.0"
            "fun framed(mk):"
            "  fun (): pc.Pict.from_handle(mk())"
            "fun slide_1(): slide_canvas("
            "  ~width: 320.0, ~height: 240.0,"
            "  at(20.0, 20.0, ~tag: \"Box\","
            "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
            "fun slide_2(): slide_canvas("
            "  ~width: 320.0, ~height: 240.0,"
            "  at(30.0, 30.0, ~tag: \"Other\","
            "     shape_pict(~width: 50.0, ~height: 50.0, ~fill: hex(\"ED7D31\"))))"
            "fun plain(): pc.Pict.from_handle(slide_canvas(~width: 320.0, ~height: 240.0))"
            "def all_slides:"
            "  [framed(slide_1),"
            "   // the second one"
            "   framed(slide_2),"
            "   plain()]")
      "\n")
     program #:exists 'replace)
    (picts->pptx (load-program-picts program) deck)
    (void (sync-once program deck #:workdir w)))
  (define (sync!) (sync-once program deck #:workdir w #:atomic? #t))

  ;; Reordered in the navigator.
  (fresh!)
  (check-true (move-slide! deck 3 1) "the last slide is dragged to the front")
  (define r1 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r1)) '(reordered)
                "which is written")
  (check-regexp-match #px"(?s:\\[plain[(][)],\\s*// the second one\\s*framed[(]slide_1[)],\\s*framed[(]slide_2[)]\\])"
                      (file->string program)
                      "the entries keep their shape, and the comment stays where it was")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A slide whose entry is a call and which nothing else draws: the entry goes,
  ;; and the definition with it.
  (fresh!)
  (check-true (delete-slide! deck 2) "the second slide is deleted")
  (define r2 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(removed-slide)
                "which is written")
  (check-false (regexp-match? #rx"framed[(]slide_2[)]" (file->string program))
               "its entry is gone")
  (check-false (regexp-match? #rx"fun slide_2" (file->string program))
               "and so is what it drew")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A slide pasted in the editor needs the list found before its name can go
  ;; in it, and a talk writes a long list as a block rather than after an `=`.
  (fresh!)
  (check-not-false (paste-slide! deck deck 1) "the first slide is pasted")
  (define r4 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r4)) '(added-slide)
                "which is written")
  (check-regexp-match #rx"def slide_3" (file->string program)
                      "as a definition of its own")
  (check-regexp-match #rx"plain[(][)], slide_3" (file->string program)
                      "and an entry in the list, after the slide it was pasted behind")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A slide drawn by something with no `at` form of its own has no definition
  ;; the merge can point at. The entry still goes -- the slide is what was
  ;; deleted -- and the report says what was left behind.
  (fresh!)
  (check-true (delete-slide! deck 3) "the third slide is deleted")
  (define r3 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r3)) '(removed-slide)
                "which is written")
  (check-false (regexp-match? #rx"plain[(][)]," (file->string program))
               "its entry is gone")
  (check-regexp-match #rx"fun plain" (file->string program) "what drew it is not")
  (check-regexp-match #rx"nothing was deleted with it" (format-sync-report r3)
                      "and the report says so")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; ------------------------------ a copied element keeps the face it was in
;;
;; A run written into a program names its typeface unless the program's own
;; default already is that face -- and the default is what the program passed to
;; `current_default_font`, not the face most of the deck happens to be in. A
;; talk whose default is its code face had a copied line of prose written with
;; no face at all, which is the code face: the copy came out in the wrong one.
(let ()
  (define dir (build-path work "copy-font"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (define (words tag y face)
    (list (format "  at(20.0, ~a, ~a: ~s," y "~tag" tag)
          (format "     textbox(~a: 200.0, ~a: 40.0," "~width" "~height")
          (format "             para(run(~s~a~a, ~a: 20.0))))," tag
                  (if face ", ~font: " "") (if face (format "~s" face) "")
                  "~size")))
  (define (fresh!)
    (delete-directory/files (build-path dir ".glide") #:must-exist? #f)
    (display-to-file
     (string-join
      (append
       (list "#lang rhombus/and_meta"
             "import:"
             "  lib(\"glide-pptx/runtime.rhm\") open"
             "  pict as pc"
             "export: all_slides"
             "current_default_font(\"PT Mono\")"
             "def slide_1 = slide_canvas("
             "  ~width: 320.0, ~height: 240.0,")
       (words "prose" 20.0 "Liberation Sans")
       (words "more prose" 70.0 "Liberation Sans")
       (words "code" 120.0 #f)
       (list "  )"
             "def all_slides = [slide_1]"))
      "\n")
     program #:exists 'replace)
    (picts->pptx (load-program-picts program) deck)
    (void (sync-once program deck #:workdir w)))
  (define (sync!) (sync-once program deck #:workdir w #:atomic? #t))

  (fresh!)
  (check-true (duplicate-in-deck! deck 1 "prose") "the prose box is copied")
  (define r1 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r1)) '(added) "and written")
  (check-regexp-match #rx"~tag: \"prose [(]2[)]\"" (file->string program) "under its own name")
  (check-regexp-match #px"(?s:prose [(]2[)].*~font: \"Liberation Sans\")"
                      (file->string program)
                      "naming the face it was copied from, which the program's default is not")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The other way: a run already in the program's default face names nothing,
  ;; because naming it would be noise.
  (fresh!)
  (check-true (duplicate-in-deck! deck 1 "code") "the code box is copied")
  (define r2 (sync!))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(added) "and written")
  (check-false (regexp-match? #px"(?s:code [(]2[)].*~font:)" (file->string program))
               "with no face to name")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; --------------------------------- a tag two `at` forms in the program share
;;
;; A helper that draws a badge puts the `at` inside itself, and a slide that
;; calls it never mentions the tag. That is followed -- writing the one form
;; moves everything it draws, which is what sharing code means. What cannot be
;; followed is two `at` forms under one tag, and the report has to say which of
;; the two it is: "no tagged `at` form in the source" of a name the file writes
;; twice sends a person looking for the wrong thing.
(let ()
  (define dir (build-path work "shared-tag"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "fun badge():"
          "  at(200.0, 40.0, ~tag: \"Box\","
          "     shape_pict(~width: 40.0, ~height: 40.0, ~fill: hex(\"70AD47\")))"
          "def slide_1 = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
          "def slide_2 = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  badge())"
          "def all_slides = [slide_1, slide_2]")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) deck)
  ;; The helper's site is outside slide_2's scope, while another site has the
  ;; same explicit name. Waiting until the first drag would put editor work at
  ;; risk, so this is rejected before a base is recorded.
  (check-exn #rx"no writable source site"
             (lambda () (sync-once program deck #:workdir w))
             "an ambiguous cross-scope helper identity is rejected at startup"))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
