#lang racket/base
;; Command line entry point: `raco glide <subcommand> ...`.
(require racket/cmdline racket/list racket/string racket/file racket/path racket/treelist pict
         racket/pretty racket/format
         "ir.rkt" "parse.rkt" "render.rkt" "runtime.rkt"
         "emit-rhombus.rkt" "verify.rkt" "geometry.rkt"
         "export.rkt" "sync.rkt" "watch.rkt" "fonts.rkt"
         (only-in "semantic.rkt" current-flatten-opaque?)
         (only-in "parse.rkt" current-allow-unsupported?))
(provide main starter-deck default-app)

;; A deck has to be unzipped to be read, and the images are read from there
;; while the slides draw -- so there is a scratch directory for the length of a
;; command. It goes next to the output rather than in the system temp dir so that
;; `--keep-work` can leave it somewhere findable.
(define (default-workdir out) (build-path out ".glide"))

(define keep-work? (make-parameter #f))

(define (clean-work! dir)
  (if (keep-work?)
      (printf "scratch kept in ~a\n" dir)
      (delete-directory/files dir #:must-exist? #f)))

;; A Keynote document is not a .pptx, but Keynote will write one -- and the
;; watcher already knows how to ask it to. So a `.key` is accepted anywhere a
;; deck is, by exporting it first.
(define (as-pptx path out)
  (cond
    [(not (regexp-match? #rx"[.](key|keynote)$"
                         (string-downcase (if (path? path) (path->string path) path))))
     path]
    [else
     (define full (path->complete-path path))
     (define exported (build-path (default-workdir out)
                                  (string-append (deck-stem full) ".pptx")))
     (make-directory* (default-workdir out))
     (printf "exporting ~a from Keynote\n" (file-name-from-path full))
     ((app-adapter-harvest! (adapter-named 'keynote)) full exported)
     (unless (file-exists? exported)
       (error 'glide
              (string-append "Keynote did not write ~a.\n"
                             "  Export it by hand -- File > Export To > PowerPoint -- and"
                             " pass the .pptx.")
              exported))
     exported]))

;; A deck read from the command line, with its builds split out: a slide the
;; presenter advances through is several slides here, because a still slide is
;; all a program can hold and all a deck of stills can show. Nothing else is
;; lost by it -- a slide with no build comes back as itself.
(define (with-deck given out proc)
  (define pptx (as-pptx given out))
  (define warnings (box '()))
  (define d (parameterize ([current-warnings warnings]
                           [current-build-frames? #t])
              (pptx->deck pptx #:workdir (build-path (default-workdir out)
                                                     (deck-stem pptx)))))
  (begin0 (parameterize ([runtime-warnings warnings]) (proc d warnings))
          (report-warnings warnings)
          ;; Everything the output needs has been copied out by now.
          (clean-work! (default-workdir out))))

(define (deck-stem pptx)
  (path->string (path-replace-extension (file-name-from-path pptx) "")))

(define (report-warnings warnings)
  (define ws (remove-duplicates (reverse (unbox warnings))))
  (unless (null? ws)
    (eprintf "~a note~a about approximate rendering:\n" (length ws)
             (if (= 1 (length ws)) "" "s"))
    (for ([w (in-list ws)]) (eprintf "  - ~a\n" w))))

;; ------------------------------------------------------------- subcommands

(define (cmd-translate args)
  (define out-dir (box "out"))
  (define files
    (parse-command-line
     "glide translate" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Output directory" "dir")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  ;; `-o` names a directory, but `-o talk.rhm` clearly means the file, so both
  ;; work: the images go beside whichever it is.
  (define named-file?
    (regexp-match? #rx"[.]rhm$" (unbox out-dir)))
  (define dir (if named-file?
                  (let ([p (path-only (path->complete-path (unbox out-dir)))])
                    (if p (path->string p) "."))
                  (unbox out-dir)))
  (for ([f (in-list files)])
    (with-deck f dir
      (lambda (d _w)
        (define path
          (if named-file?
              (path->complete-path (unbox out-dir))
              (build-path dir (string-append (deck-stem f) ".rhm"))))
        (write-rhombus-deck d path #:source-name f)
        (printf "~a  (~a slides, ~a elements)\n"
                path (length (deck-slides d)) (deck-count-elements d))))))

(define (cmd-render args)
  (define out-dir (box "out"))
  (define files
    (parse-command-line
     "glide render" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Output directory" "dir")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  (for ([f (in-list files)])
    (with-deck f (unbox out-dir)
      (lambda (d _w)
        (make-directory* (unbox out-dir))
        (define path (build-path (unbox out-dir) (string-append (deck-stem f) ".pdf")))
        (picts->pdf (deck->picts d) path #:width (deck-width d) #:height (deck-height d))
        (printf "~a  (~a pages)\n" path (length (deck-slides d)))))))

(define (cmd-ir args)
  (define files
    (parse-command-line "glide ir" args '() (lambda (_ . fs) fs) '("deck.pptx")))
  (for ([f (in-list files)])
    (with-deck f "out"
      (lambda (d _w)
        (pretty-write d)))))

(define (cmd-verify args)
  (define out-dir (box "out/verify"))
  (define dpi (box 96))
  (define mae (box 0.02))
  (define bad (box 0.06))
  (define files
    (parse-command-line
     "glide verify" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Where to put images" "dir")]
        [("--dpi") ,(lambda (_ v) (set-box! dpi (string->number v))) ("Raster resolution" "n")]
        [("--mae") ,(lambda (_ v) (set-box! mae (string->number v)))
                   ("Max mean error, 0..1" "x")]
        [("--bad") ,(lambda (_ v) (set-box! bad (string->number v)))
                   ("Max fraction of differing pixels, 0..1" "x")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  (define results
    (for/list ([f (in-list files)])
      (define warnings (box '()))
      (define dd (verify-deck f (unbox out-dir) #:dpi (unbox dpi)
                              #:mae-threshold (unbox mae)
                              #:bad-threshold (unbox bad)
                              #:warnings warnings))
      (display (format-report dd #:mae-threshold (unbox mae) #:bad-threshold (unbox bad)))
      (report-warnings warnings)
      dd))
  (define failed (filter (lambda (dd) (not (deck-diff-ok? dd))) results))
  (printf "\n~a of ~a deck~a within tolerance (mean err <= ~a%, pixels off <= ~a%)\n"
          (- (length results) (length failed)) (length results)
          (if (= 1 (length results)) "" "s")
          (* 100 (unbox mae)) (* 100 (unbox bad)))
  (when (pair? failed) (exit 1)))

;; Any module that provides slide picts can be exported, whether or not it was
;; generated by this tool: `all-slides` is the convention the emitters follow,
;; and `all_slides` is its Rhombus spelling.
(define (cmd-export args)
  (define out (box #f))
  (define width (box #f))
  (define height (box #f))
  (define binding (box #f))
  (define slideshow? (box #f))
  (define flatten? (box #t))
  (define files
    (parse-command-line
     "glide export" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out v)) ("Output .pptx" "file")]
        [("--width") ,(lambda (_ v) (set-box! width (string->number v)))
                     ("Slide width in points" "pt")]
        [("--height") ,(lambda (_ v) (set-box! height (string->number v)))
                      ("Slide height in points" "pt")]
        [("--slides") ,(lambda (_ v) (set-box! binding v))
                      ("Name of the provided list of picts" "id")]
        [("--slideshow") ,(lambda (_) (set-box! slideshow? #t))
                         ("Run as a slideshow program: one slide per advance")]
        [("--stages") ,(lambda (_) (set-stage-slides! #t))
                      ("One slide per stage of a slide that animates")]
        [("--numbers") ,(lambda (_) (ask-slide-numbers! #t))
                       ("Draw the slide number in the corner, as the show does")]
        [("--no-flatten") ,(lambda (_) (set-box! flatten? #f))
                          ("Keep unsyncable elements as separate shapes")]))
     (lambda (_ . fs) fs)
     '("program.rkt")))
  (for ([f (in-list files)])
    (define full (path->complete-path f))
    (define picts
      (if (unbox slideshow?)
          (slideshow-slides full (unbox width) (unbox height))
          (load-program-picts full #:named (unbox binding))))
    (define target
      (or (unbox out)
          (path->string (path-replace-extension (file-name-from-path f) ".pptx"))))
    (define warnings (box '()))
    (parameterize ([current-export-warnings warnings]
                   [current-flatten-opaque? (unbox flatten?)])
      (picts->pptx picts target #:width (unbox width) #:height (unbox height)))
    (printf "~a  (~a slide~a)\n" target (length picts) (if (= 1 (length picts)) "" "s"))
    (report-warnings warnings)))

;; A `slideshow` program does not provide a list of slides; it *calls* `slide`,
;; and each animation step is a separate frame. `get-slides-as-picts` is the
;; same entry point `raco slideshow --pdf` uses, so one pptx slide comes out per
;; step -- which for an animated Rhombus talk is one per epoch.
;;
;; It is required lazily because it pulls in the GUI toolkit, and nothing else
;; here needs that.
(define SLIDESHOW-W 1360.0)
(define SLIDESHOW-H 766.0)

(define (slideshow-slides path width height)
  (define get
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (error 'export
                              (string-append
                               "--slideshow needs slideshow and its GUI support:\n"
                               "  ~a")
                              (first (string-split (exn-message e) "\n"))))])
      (dynamic-require 'slideshow/slides-to-picts 'get-slides-as-picts)))
  (define w (or width 960.0))
  (define h (or height 540.0))
  ;; Always condensed. An animated pict is many frames between advances, so
  ;; without this a 291-slide talk comes out as 4869 slides and 135 MB.
  (define frames
    (parameterize ([current-directory (path-only path)])
      (get (path->string path) SLIDESHOW-W SLIDESHOW-H #t)))
  ;; Slideshow renders at its own size, so the frames are scaled onto the page.
  (for/list ([p (in-list frames)])
    (scale p (/ w SLIDESHOW-W) (/ h SLIDESHOW-H))))



;; One merge pass between a program and a deck. With no base recorded yet this
;; only writes the base, since there is nothing to merge against.
(define (cmd-sync args)
  (define dry (box #f))
  (define files
    (parse-command-line
     "glide sync" args
     `((once-each
        [("-n" "--dry-run") ,(lambda (_) (set-box! dry #t))
                            ("Report what would change without touching anything")]))
     (lambda (_ program . deck) (cons program deck))
     '("program.rhm" "[deck.pptx]")))
  (define program (first files))
  ;; With no deck named, the one `glide <program>` keeps in scratch.
  ;; A `.key` is accepted here as well: Keynote writes the .pptx to merge from.
  (define deck
    (cond
      [(pair? (rest files))
       (as-pptx (second files)
                (let ([p (path-only (path->complete-path program))])
                  (if p (path->string p) ".")))]
      [else
       (define d (scratch-deck program))
       (unless (file-exists? d)
         (error 'glide
                (string-append
                 "there is no deck to merge from yet.\n"
                 "  `raco glide ~a` makes one and keeps the two in step;\n"
                 "  or name a deck: `raco glide sync ~a deck.pptx`.")
                (file-name-from-path program) (file-name-from-path program)))
       d]))
  (define r
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "~a <-> ~a\n" program deck)
                                 (eprintf "~a\n" (exn-message e))
                                 (exit 1))])
      (sync-once program deck #:dry-run? (unbox dry))))
  (printf "~a <-> ~a\n" program deck)
  (display (format-sync-report r))
  (when (sync-report-base-written? r)
    (printf "  base: ~a\n" (base-path-for program))))

;; The parent process: regenerate the deck when the program is saved, and merge
;; the deck's geometry back when it is saved.
;; The deck and, for Keynote, the document made from it are scratch: the program
;; is the artifact. So they go in a hidden directory beside it rather than in the
;; way, and what the folder holds is the program and its images.
(define (scratch-deck program)
  (build-path (scratch-dir-of program)
              (path->string (path-replace-extension
                             (file-name-from-path (path->complete-path program))
                             ".pptx"))))

;; Keynote on a Mac, since that is what there is to drive there.
;; LibreOffice, and nothing else unless it is asked for. It reads and writes
;; .pptx itself, it is on every platform, and it costs nothing -- and one
;; editor that is known to work is worth more than a choice between three that
;; might. On a Mac it lives inside the application bundle rather than on the
;; PATH, which is where a search would otherwise give up.
;;
;; Without one, this says so and stops. Opening nothing and carrying on is how
;; a session ends up reporting an AppleScript syntax error about a property,
;; which says nothing about what is wrong.
;;
;; `--app` still names another: PowerPoint and Keynote are driven the same way,
;; and `--app none` leaves the deck to be opened by hand.
(define (default-app)
  (unless (soffice-exe)
    (error 'glide
           (string-append
            "no LibreOffice to open the deck with.\n"
            "  Install it from libreoffice.org -- it is looked for on the PATH,"
            " and on a Mac\n"
            "  inside /Applications/LibreOffice.app as well.\n"
            "  Or pass --app none and open the deck yourself: the two are kept"
            " in step either way.")))
  'libreoffice)

(define (cmd-edit args)
  (define out (box #f))
  (define app (box #f))
  (define width (box #f))
  (define height (box #f))
  (define once (box #f))
  (define files
    (parse-command-line
     "glide edit" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out v)) ("Deck to generate" "file")]
        [("--app") ,(lambda (_ v) (set-box! app (string->symbol v)))
                   ("keynote, powerpoint, libreoffice or none" "name")]
        [("--width") ,(lambda (_ v) (set-box! width (string->number v)))
                     ("Slide width in points" "pt")]
        [("--height") ,(lambda (_ v) (set-box! height (string->number v)))
                      ("Slide height in points" "pt")]
        [("--once") ,(lambda (_) (set-box! once #t))
                         ("Regenerate once and exit, without watching")]
        ;; A talk that numbers its own slides usually calls `set_slide_numbers`
        ;; inside `module main`, where the show runs it and nothing else does --
        ;; so the deck in the editor carries no numbers and the show does. This
        ;; asks for them on both sides of the session at once, which is what
        ;; keeps them from reading as an element somebody added.
        [("--numbers") ,(lambda (_) (ask-slide-numbers! #t))
                       ("Draw the slide number in the corner, as the show does")]
        ;; One slide per stage in the deck being edited, so that a shape can be
        ;; put where it belongs on the stage it appears on. An edit still goes
        ;; to the one `at` form that draws it, and the deck is written again
        ;; from the program afterwards so the other stages follow.
        [("--stages") ,(lambda (_) (set-stage-slides! #t))
                      ("One slide per stage of a slide that animates")]))
     (lambda (_ program) (list program))
     '("program.rhm")))
  (define program (first files))
  (define pptx (or (unbox out) (scratch-deck program)))
  (make-directory* (path-only (path->complete-path pptx)))
  (define adapter (adapter-named (or (unbox app) (default-app))))
  ;; Asked for by name, and not there. An editor that cannot be driven opens
  ;; nothing and answers every reload with a syntax error from AppleScript
  ;; about a property, which is a poor way to learn you have no PowerPoint.
  (when (and (eq? 'powerpoint (app-adapter-name adapter)) (not (powerpoint-installed?)))
    (error 'glide
           (string-append "no Microsoft PowerPoint installed to drive.\n"
                          "  Install LibreOffice, which glide will find and use,"
                          " or pass --app none\n"
                          "  and open the deck yourself: it is kept in step either way.")))
  (void
   (if (unbox once)
       (watch-once program pptx #:adapter adapter
                   #:width (unbox width) #:height (unbox height))
       (watch-loop program pptx #:adapter adapter
                   #:width (unbox width) #:height (unbox height)))))

;; A slideshow to start from. It is built as a deck and written by the same
;; emitter that writes an imported one, so a new program is in the dialect
;; glide reads back -- source-derived identities, the `all_slides` list, the
;; slideshow and PDF submodules -- rather than a second idiom that has to be
;; kept in step.
(define (starter-deck)
  (define (box x y w h) (bbox x y w h 0.0 #f #f))
  (define (words text size bold?)
    (text-body (list (para (list (trun text "Calibri" size bold? #f #f #f
                                       (rgba 32 32 32 1.0) 0.0 'none 0.0 'all))
                           'left 0 0.0 0.0 '(percent . 1.0) 0.0 0.0 no-bullet 'all))
               'top #f #t 'none default-insets 0.0 'all))
  (define (rect id name x y w h fill)
    (shape id name (box x y w h) (preset-geom "rect" '()) (solid-fill fill) #f #f #f))
  (define (label id name x y w h text size bold?)
    (shape id name (box x y w h) (preset-geom "rect" '()) #f #f (words text size bold?) #f))
  (deck
   960.0 540.0
   (list (slide 1 "" 960.0 540.0 (solid-fill (rgba 255 255 255 1.0)) '()
                (list (label 2 "Title" 80.0 180.0 800.0 90.0 "A new slideshow" 54.0 #t)
                      (label 3 "Subtitle" 80.0 290.0 800.0 50.0
                             "Drag things about in the editor; the code follows" 24.0 #f))
                #f #f)
         (slide 2 "" 960.0 540.0 (solid-fill (rgba 255 255 255 1.0)) '()
                (list (label 4 "Heading" 80.0 80.0 800.0 60.0 "A slide" 40.0 #t)
                      (rect 5 "Box" 80.0 200.0 240.0 160.0 (rgba 68 114 196 1.0))
                      (label 6 "Note" 360.0 200.0 520.0 160.0
                             "Every shape here has a tag, and a tag is what an edit is written back to."
                             24.0 #f))
                #f #f))
   #f 'new))

(define (cmd-new args)
  (define files
    (parse-command-line
     "glide new" args '() (lambda (_ . fs) fs) '("program.rhm")))
  (when (null? files) (error 'glide "give the program a name: raco glide --new talk.rhm"))
  (for ([f (in-list files)])
    (define path (path->complete-path (if (regexp-match? #rx"[.]rhm$" f)
                                          f
                                          (string-append f ".rhm"))))
    (when (file-exists? path)
      (error 'glide "~a is already there; delete it or pick another name"
             (file-name-from-path path)))
    (write-rhombus-deck (starter-deck) path #:source-name #f)
    (printf "~a  (2 slides)\n" (path->string path))
    (printf "  raco glide ~a   opens it in an editor and keeps both in step\n"
            (path->string (file-name-from-path path)))))

;; The fonts a program or a deck names, copied in beside it.
;;
;; A program that carries its fonts runs the same on a machine that has never
;; heard of them: `register_fonts` loads what is in the folder before anything
;; is drawn. It is a copy, so the licence on each font is the licence on the
;; copy -- most typefaces may not be redistributed, and this says which ones it
;; took so that is a decision and not an accident.
(define (families-named path)
  (cond
    [(regexp-match? #rx"[.]pptx$" (path->string path))
     (deck-font-families (with-deck path))]
    [else
     ;; Read rather than run: a program that names a font it has not got is
     ;; exactly the program this is for, and running it would refuse.
     (define src (file->string path))
     (remove-duplicates
      (append (for/list ([m (in-list (regexp-match* #px"~font:\\s*\"([^\"]+)\"" src
                                                    #:match-select cadr))])
                m)
              (for/list ([m (in-list (regexp-match* #px"current_default_font\\(\"([^\"]+)\"\\)" src
                                                    #:match-select cadr))])
                m)))]))

(define (cmd-fonts args)
  (define files
    (parse-command-line
     "glide fonts" args '() (lambda (_ . fs) fs) '("program.rhm-or-deck.pptx")))
  (when (null? files)
    (error 'glide "name a program or a deck: raco glide fonts talk.rhm"))
  (for ([f (in-list files)])
    (define path (path->complete-path f))
    (unless (file-exists? path) (error 'glide "~a is not there" f))
    (define families (families-named path))
    (define dir (build-path (or (path-only path) (current-directory)) "fonts"))
    (make-directory* dir)
    (printf "~a names ~a ~a\n" (file-name-from-path path) (length families)
            (if (= 1 (length families)) "typeface" "typefaces"))
    (define taken
      (for/list ([fam (in-list families)])
        (define src (font-file-for fam))
        (cond
          [(not src)
           (printf "  ~a -- not installed here, so there is nothing to copy\n" fam)
           #f]
          [else
           (define dest (build-path dir (file-name-from-path src)))
           (copy-file src dest #t)
           (printf "  ~a -> fonts/~a\n" fam (file-name-from-path src))
           dest])))
    (define n (length (filter values taken)))
    (cond
      [(zero? n) (printf "  nothing was copied\n")]
      [else
       (printf "  ~a copied into ~a\n" n (path->string dir))
       (printf "  They travel with the program now. Check you may redistribute\n")
       (printf "  them: most typefaces say, and a talk in a public repository is\n")
       (printf "  a redistribution.\n")])))

(define (cmd-presets args)
  (printf "~a preset geometries drawn exactly:\n" (length (preset-names)))
  (for ([g (in-list (preset-names))]) (printf "  ~a\n" g))
  (printf "Anything else is drawn as a rectangle and reported as a note.\n"))

(define subcommands
  (list (list "new" "write a slideshow to start from" cmd-new)
        (list "translate" "emit a Pict program from a deck" cmd-translate)
        (list "export" "write a .pptx from a program's slide picts" cmd-export)
        (list "render" "render a deck straight to PDF" cmd-render)
        (list "sync" "merge a deck's edits back into a program, once" cmd-sync)
        (list "edit" "open a program in an editor and keep both in step" cmd-edit)
        (list "watch" "the same thing, under its older name" cmd-edit)
        (list "verify" "compare our PDF against LibreOffice's" cmd-verify)
        (list "ir" "print the intermediate representation" cmd-ir)
        (list "fonts" "copy the fonts a program names in beside it" cmd-fonts)
        (list "presets" "list the shape geometries we draw exactly" cmd-presets)))

(define (usage)
  (printf "usage: raco glide program.rhm         open it in an editor, keep both in step\n")
  (printf "       raco glide <command> [options] ...\n\n")
  (for ([s (in-list subcommands)])
    (printf "  ~a~a\n" (~a (first s) #:min-width 12) (second s)))
  (printf "\nRun a command with --help for its options.\n")
  (printf "\n  --allow-unsupported  draw charts and diagrams as empty boxes\n")
  (printf "                       instead of refusing the deck\n")
  (printf "  --keep-work          leave the unzipped deck behind for inspection\n"))

(define (main . argv)
  (define args (if (and (= 1 (length argv)) (vector? (car argv)))
                   (vector->list (car argv))
                   argv))
  ;; This one is not per-command: every command that reads a deck honours it,
  ;; and it says "I am only looking", not "this deck is safe to round-trip".
  (define allow? (and (member "--allow-unsupported" args) #t))
  (set! args (remove "--allow-unsupported" args))
  (define keep? (and (member "--keep-work" args) #t))
  (set! args (remove "--keep-work" args))
  (define (run! cmd rest)
    (parameterize ([current-allow-unsupported? allow?]
                   [keep-work? keep?]
                   [current-keep-work? keep?])
      (cmd (list->vector rest))))
  (cond
    [(or (null? args) (member (car args) '("-h" "--help" "help"))) (usage)]
    [(assoc (car args) subcommands) => (lambda (s) (run! (third s) (cdr args)))]
    ;; `glide talk.rhm` is the whole workflow, so it needs no verb: a
    ;; program named where a command would go means "edit this". The name moves
    ;; to the end, since flag parsing stops at the first argument that is not a
    ;; flag and `talk.rhm --once` is the natural way to write it.
    ;; `--new talk.rhm`, since that is the shape it is reached for in.
    [(member (car args) '("--new" "-n")) (run! cmd-new (cdr args))]
    [(regexp-match? #rx"[.]rhm$" (car args))
     (run! cmd-edit (append (cdr args) (list (car args))))]
    [else
     (eprintf "~a is neither a command nor a .rhm program\n\n" (car args))
     (usage)
     (exit 2)]))

;; `raco` runs a registered command by requiring its module, so the dispatch
;; happens at module level rather than in a `main` submodule.
(apply main (vector->list (current-command-line-arguments)))
