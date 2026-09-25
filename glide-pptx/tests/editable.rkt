#lang racket/base
;; What can be edited, element by element.
;;
;; The merge can only write an edit where the program holds something to write:
;; an `at` form with numbers in it, a `run` with a string in it. "If it seems
;; reasonable to edit, it should work" is therefore a property of the source the
;; translator emits and of the source people write by hand, and it can be
;; measured without a deck, a merge or an editor -- which is what makes it worth
;; asserting on every run rather than discovering from a session that refused.
;;
;; Measured on a real talk it found three gaps at once. A picture states its
;; size positionally -- `image_pict(media("logo.png"), 115.0, 115.0)` -- and the
;; finder looked only for `~width:`, so not one picture could be resized. A slide
;; with stages writes its canvas as a `def` inside the function, so two thirds of
;; the slides had nowhere to add a shape. And a talk that draws nine nodes writes
;; `def n_update = shape_pict(~width: 135.0, ...)` and places it by name, so the
;; size the corner would drag was stated somewhere the finder never looked.
;;
;; `GLIDE_EDITABLE_PROGRAM` points it at one program -- a talk -- and prints the
;; table for it.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/parse glide-pptx/emit-rhombus glide-pptx/sync glide-pptx/ir)

(define-runtime-path decks-dir "decks")
(define-runtime-path local-dir "programs/local")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-editable"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; The table, and the checks that go with it. Everything here is "all of them":
;; a floor would let a regression hide behind the elements that still work.
(define (editable! label program #:all-slides-addable? [all-addable? #t])
  (define text (file->string program))
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (define (source-of s)
    (define w (at-site-whole s))
    (substring text (rng-start w) (min (string-length text) (rng-end w))))
  ;; An element that writes a `run` carries text somebody could retype. One that
  ;; does not -- a picture, a plain shape -- has nothing to retype, and counting
  ;; it as a gap would make the number meaningless.
  (define with-text
    (filter (lambda (s) (regexp-match? #rx"run[(]|run[*][(]" (source-of s))) sites))
  (define (all label* p [of sites])
    (define missed (filter (lambda (s) (not (p s))) of))
    (check-equal? (map at-site-tag missed) '()
                  (format "~a: every element is ~a" label label*)))
  (printf "  ~a: ~a elements, ~a with text, ~a slides, ~a of them addable\n"
          label (length sites) (length with-text)
          (length (remove-duplicates (map at-site-scope sites))) (length slide-sites))
  (all "movable" (lambda (s) (and (at-site-x s) (at-site-y s))))
  (all "resizable" (lambda (s) (and (at-site-width s) (at-site-height s))))
  (all "rotatable" (lambda (s) (or (at-site-rot s) (at-site-insert-at s))))
  (all "retypable" (lambda (s) (pair? (at-site-texts s))) with-text)
  ;; A slide is somewhere a shape can be drawn. A slide with no canvas of its own
  ;; -- one built by laying a group over another slide -- has nowhere to put one,
  ;; and says so rather than being counted.
  (when all-addable?
    (check-equal? (sort (map (lambda (s) (format "~a" s))
                             (remove* (map slide-site-scope slide-sites)
                                      (remove-duplicates (map at-site-scope sites))))
                        string<?)
                  '()
                  (format "~a: every slide with elements on it can have one added" label))))

(define fixtures
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

;; A talk in the three shapes a translated deck never takes: a canvas written as
;; a `def` inside a staged function, a leaf defined once and placed by name, and
;; a picture stating its size positionally.
(define STAGED-TALK
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
         "def media = media_lookup(\"media\")"
         ""
         "fun staged():"
         "  // The size a corner would drag is stated here, not at the `at`."
         "  def node = shape_pict(~width: 60.0, ~height: 30.0, ~fill: hex(\"4472C4\"))"
         "  def base_canvas:"
         "    slide_canvas(~width: slide_width, ~height: slide_height,"
         "                 at(40.0, 60.0, ~tag: \"Node\", node),"
         "                 at(40.0, 140.0, ~tag: \"Words\","
         "                    textbox(~width: 200.0, ~height: 40.0, ~wrap: #false,"
         "                            para(run(\"hello\", ~size: 14.0)))),"
         "                 at(240.0, 60.0, ~tag: \"Logo\","
         "                    image_pict(media(\"checker.png\"), 40.0, 40.0)))"
         "  def stage_1:"
         "    group_pict(~width: slide_width, ~height: slide_height,"
         "               at(300.0, 140.0, ~tag: \"Later\","
         "                  shape_pict(~width: 50.0, ~height: 25.0, ~fill: hex(\"ED7D31\"))))"
         "  def b = pc.Pict.from_handle(base_canvas)"
         "  def s = pc.Pict.from_handle(stage_1)"
         "  def settled = b.snapshot(math.max(0, b.duration - 1), 1.0)"
         "  pc.switch(b, pc.animate(fun (t):"
         "                            pc.overlay(~horiz: #'left, ~vert: #'top,"
         "                                       settled, s.alpha(t))))"
         ""
         "def all_slides = [staged]")
   "\n"))

(printf "what can be edited:\n")
(for ([name (in-list fixtures)])
  (define dir (build-path work name))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                        #:workdir (build-path dir "u")))
  (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
  (editable! name program))

(let ()
  (define dir (build-path work "staged"))
  (make-directory* dir)
  (make-directory* (build-path dir "media"))
  (copy-file (build-path decks-dir 'up "media" "checker.png")
             (build-path dir "media" "checker.png") #t)
  (define program (build-path dir "p.rhm"))
  (display-to-file STAGED-TALK program #:exists 'replace)
  (editable! "a staged talk" program))

;; A talk of one's own, named or dropped in `programs/local`. A slide laid over
;; another slide has no canvas of its own, so not every slide of a hand-written
;; talk can take a new shape -- which is why that one check is relaxed here and
;; nowhere else.
(define mine
  (append (let ([named (getenv "GLIDE_EDITABLE_PROGRAM")])
            (if named (list (string->path named)) '()))
          (if (directory-exists? local-dir)
              (sort (for/list ([f (in-list (directory-list local-dir #:build? #t))]
                               #:when (regexp-match? #rx"[.]rhm$" (path->string f)))
                      f)
                    string<? #:key path->string)
              '())))

(if (null? mine)
    (printf "  no talk of your own; drop one in tests/programs/local to have it measured\n")
    (for ([p (in-list mine)])
      (editable! (path->string (file-name-from-path p)) p
                 #:all-slides-addable? #f)))

(module+ main (void (test-log #:display? #t #:exit? #t)))
