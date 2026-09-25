#lang racket/base
;; The round trip with a real editor in the middle of it.
;;
;; `structural.rkt` checks that a program and the deck it writes have nothing to
;; merge. That is the round trip through *our* code, and it passed while a real
;; save was reporting nineteen edits nobody had made -- because an editor does
;; not keep the file it was given. LibreOffice rewrites what it reads, and the
;; deck the merge is handed is the deck LibreOffice wrote.
;;
;; What it rewrote was groups. A pict may place a child outside its group's own
;; bounds -- a label hanging above the box it labels -- and it may declare more
;; room than it uses; LibreOffice states a group as the box around its contents
;; either way. Nine groups came back "resized", eight of them drawn by a helper
;; with no `at` form to write to, so the save was refused whole and a real drag
;; made in the same session was lost with it.
;;
;; So: export, let LibreOffice load and save the deck, and require the merge to
;; say nothing. `--convert-to` is the whole dependency -- no UNO, no display --
;; which is why this can run wherever LibreOffice is installed.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/port racket/runtime-path
         glide-pptx/parse glide-pptx/export glide-pptx/sync glide-pptx/emit-rhombus)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-lo-roundtrip"))

(define soffice (or (find-executable-path "soffice") (find-executable-path "libreoffice")))

;; LibreOffice reads the deck and writes it again. Its own profile, in the
;; scratch: a shared one is locked by whatever else is running, and a fresh one
;; asks a first-run question that nothing headless can answer.
(define (libreoffice-resave! pptx profile out-dir)
  (make-directory* out-dir)
  (make-directory* profile)
  (define ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* soffice
               (format "-env:UserInstallation=file://~a" (path->string profile))
               "--headless" "--norestore" "--nolockcheck" "--convert-to" "pptx"
               "--outdir" (path->string out-dir) (path->string pptx))))
  (define written (build-path out-dir (file-name-from-path pptx)))
  (and ok? (file-exists? written)
       (begin (copy-file written pptx #t) #t)))

;; A program, the deck it writes, and a real editor's rewrite of that deck: the
;; merge has nothing to say about any of it.
;;
;; The base is recorded first, by a pass that has none. That pass writes only
;; the base and never touches the program -- and without it every later pass
;; reports nothing whatever the deck holds, which is a test that cannot fail.
(define (agrees-after-a-real-save name program dir)
  (define pptx (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) pptx)
  (define first-pass (sync-once program pptx #:workdir w))
  (check-true (sync-report-base-written? first-pass)
              (format "~a: the first pass records a base" name))
  (cond
    [(not (libreoffice-resave! pptx (build-path dir "profile") (build-path dir "lo")))
     (check-true #f (format "~a: LibreOffice would not re-save the deck" name))]
    [else
     (define r (sync-once program pptx #:workdir w #:dry-run? #t))
     (define said
       (for/list ([a (in-list (sync-report-actions r))])
         (format "~a ~s on slide ~a" (sync-action-kind a) (sync-action-tag a)
                 (sync-action-slide a))))
     (check-equal? said '()
                   (format "~a: nothing to merge after LibreOffice saved it" name))]))

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

;; A talk that gives a slide stages, places a child outside its group and
;; declares a group bigger than it fills -- which is the shape the phantom
;; resizes came from, and which no translated deck has.
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
         ""
         "fun labelled():"
         "  slide_canvas("
         "    ~width: slide_width, ~height: slide_height,"
         "    // The label hangs above the group, and the group declares more"
         "    // room than its children fill. Both are what an editor rewrites."
         "    at(60.0, 120.0, ~tag: \"Bubble\","
         "       group_pict("
         "         ~width: 160.0, ~height: 120.0,"
         "         at(0.0, -18.0, ~tag: \"Label\","
         "            textbox(~width: 120.0, ~height: 20.0, ~wrap: #false,"
         "                    para(run(\"SQL\", ~size: 14.0)))),"
         "         at(10.0, 10.0, ~tag: \"Body\","
         "            shape_pict(~width: 100.0, ~height: 60.0,"
         "                       ~fill: hex(\"4472C4\"))))))"
         ""
         "fun staged():"
         "  def base_canvas:"
         "    slide_canvas(~width: slide_width, ~height: slide_height,"
         "                 at(40.0, 60.0, ~tag: \"Base\","
         "                    shape_pict(~width: 80.0, ~height: 40.0,"
         "                               ~fill: hex(\"ED7D31\"))))"
         "  def stage_1:"
         "    group_pict(~width: slide_width, ~height: slide_height,"
         "               at(240.0, 60.0, ~tag: \"Revealed\","
         "                  shape_pict(~width: 80.0, ~height: 40.0,"
         "                             ~fill: hex(\"70AD47\"))))"
         "  def b = pc.Pict.from_handle(base_canvas)"
         "  def s = pc.Pict.from_handle(stage_1)"
         "  def settled = b.snapshot(math.max(0, b.duration - 1), 1.0)"
         "  pc.switch(b, pc.animate(fun (t):"
         "                            pc.overlay(~horiz: #'left, ~vert: #'top,"
         "                                       settled, s.alpha(t))))"
         ""
         "def all_slides = [labelled, staged]")
   "\n"))

(cond
  [(not soffice)
   (printf "no LibreOffice; the round trip through a real save is not checked\n")]
  [else
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (for ([name (in-list decks)])
     (define dir (build-path work name))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                           #:workdir (build-path dir "u")))
     (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
     (agrees-after-a-real-save name program dir))
   (let ()
     (define dir (build-path work "staged"))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (display-to-file STAGED-TALK program #:exists 'replace)
     (agrees-after-a-real-save "a staged talk" program dir))
   (printf "round trip through a real LibreOffice save done; artifacts under ~a\n"
           work)])

(module+ main (void (test-log #:display? #t #:exit? #t)))
