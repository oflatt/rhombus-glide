#lang racket/base
;; The parent process: keep a Pict program and a presentation in step.
;;
;;   program saved  ->  regenerate the deck, tell the app to show it
;;   deck saved     ->  merge the geometry back into the program
;;
;; Change detection is by content hash rather than by filesystem event. That
;; costs a poll and buys three things: it works for a Keynote `.key`, which is a
;; directory rather than a file; it collapses the burst of writes an editor makes
;; when saving; and it is the loop guard, since a file we just wrote ourselves
;; hashes to what we expect and so does not look like someone else's edit.
(require racket/list racket/string racket/format racket/file racket/path
         racket/system racket/port file/sha1 racket/runtime-path
         "export.rkt" "sync.rkt")
(provide (struct-out app-adapter) adapters adapter-named scratch-dir-of
         libreoffice-macro-reload! libreoffice-macro-open?
         libreoffice-user-dir glide-macro-files
         watch-loop watch-once program-picts
         soffice-exe powerpoint-installed?
         current-watch-log program-content-hash)

(define current-watch-log (make-parameter (lambda (fmt . args)
                                            (apply printf fmt args)
                                            (flush-output))))
(define (log! fmt . args) (apply (current-watch-log) fmt args))

;; -------------------------------------------------------------- adapters

;; `document` is the path the user actually edits, given the deck we generate:
;; the same file for an app whose native format is .pptx, and a .key bundle for
;; Keynote. `harvest!` turns that document back into a .pptx we can read, and
;; `reload!` makes the app show a deck we just regenerated.
;; `open?` says whether the editor still has the deck open, so closing it can end
;; the session and clear the scratch away. An adapter that cannot tell says #t
;; and the session runs until it is interrupted.
(struct app-adapter (name document harvest! reload! open?) #:transparent)

;; An adapter that cannot tell whether its editor is still open says #t, and the
;; session then runs until it is interrupted.
(define (always-open) #t)

;; Asked through System Events, which reports on a running process without
;; starting one -- `application "Keynote" is running` can launch it.
(define (process-running? name)
  (define exe (find-executable-path "osascript"))
  (and exe
       (let ([out (open-output-string)])
         (define code
           (parameterize ([current-output-port out] [current-error-port out])
             (system*/exit-code
              exe "-e"
              (format "tell application \"System Events\" to (name of processes) contains \"~a\""
                      name))))
         (and (zero? code)
              (regexp-match? #rx"true" (get-output-string out))))))

(define (osascript . lines)
  (define exe (find-executable-path "osascript"))
  (cond
    [(not exe) (log! "  ! osascript not found; this needs macOS\n") #f]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-output-port out] [current-error-port out])
         (apply system*/exit-code exe (append* (for/list ([l (in-list lines)])
                                                 (list "-e" l))))))
     (unless (zero? code) (log! "  ! osascript: ~a\n" (string-trim (get-output-string out))))
     (zero? code)]))

;; Nothing outside the files. Used by the tests, and by anyone who would rather
;; drive their editor themselves.
(define none-adapter
  (app-adapter 'none (lambda (pptx) pptx) (lambda (doc pptx) #t) (lambda (pptx) #t)
               always-open))

;; PowerPoint edits .pptx natively, so the document *is* the deck and there is
;; nothing to harvest -- the easiest case, and the one to prefer if the choice
;; is open. Untested here: this box has no macOS.
(define powerpoint-adapter
  (app-adapter
   'powerpoint
   (lambda (pptx) pptx)
   (lambda (doc pptx) #t)
   ;; Reloading means closing and reopening, which puts the editor back on the
   ;; first slide -- and a regeneration happens every time the program is
   ;; saved, so that is once a keystroke. The slide being looked at is read
   ;; first and set again after, so the view stays where it was. Every step of
   ;; that is wrapped: an editor that will not answer should cost the reload,
   ;; not the session.
   (lambda (pptx)
     (define p (path->string (path->complete-path pptx)))
     (osascript "tell application \"Microsoft PowerPoint\""
                (format "  set target to POSIX file \"~a\"" p)
                "  set showing to 1"
                "  try"
                "    repeat with w in document windows"
                (format "      if (full name of (presentation of w)) is \"~a\" then" p)
                "        set showing to slide index of view of w"
                "      end if"
                "    end repeat"
                "  end try"
                "  repeat with d in presentations"
                (format "    if (full name of d) is \"~a\" then close d saving no" p)
                "  end repeat"
                "  open target"
                "  activate"
                "  try"
                "    set slide index of view of document window 1 to showing"
                "  end try"
                "end tell"))
   (lambda () (process-running? "Microsoft PowerPoint"))))

;; Keynote's own format is a .key bundle and it never saves .pptx, so the deck
;; has to be exported out of it before a merge can read it, and a regenerated
;; deck has to be re-imported. Keynote has no reload, so the document is closed
;; and reopened: the slide being looked at is read first and set again after,
;; and the selection is lost either way. Untested here.
(define keynote-adapter
  (app-adapter
   'keynote
   ;; The user edits a .key beside the deck we generate.
   (lambda (pptx) (path-replace-extension pptx ".key"))
   (lambda (doc pptx)
     (define d (path->string (path->complete-path doc)))
     (define out (path->string (path->complete-path pptx)))
     (osascript "tell application \"Keynote\""
                (format "  set d to (open POSIX file \"~a\")" d)
                (format "  export d to POSIX file \"~a\" as Microsoft PowerPoint" out)
                "end tell"))
   (lambda (pptx)
     (define p (path->string (path->complete-path pptx)))
     (define k (path->string (path-replace-extension (path->complete-path pptx) ".key")))
     ;; Opening a .pptx gives Keynote an unsaved import, so the first Cmd-S would
     ;; ask where to put it. Saving it here means Cmd-S just saves, and it saves
     ;; where the watcher is looking. Wrapped, because a Keynote that refuses is
     ;; not worth failing over -- the deck is still open and editable.
     (osascript "tell application \"Keynote\""
                ;; Which slide is being looked at, so reopening puts it back
                ;; there rather than at the beginning.
                "  set showing to 1"
                "  try"
                "    if (count of documents) > 0 then"
                "      set showing to slide number of current slide of document 1"
                "    end if"
                "  end try"
                "  repeat with d in documents"
                "    close d saving no"
                "  end repeat"
                (format "  set d to (open POSIX file \"~a\")" p)
                "  activate"
                "  try"
                (format "    save d in POSIX file \"~a\"" k)
                "  end try"
                "  try"
                "    if showing > 0 and showing <= (count of slides of d) then"
                "      set current slide of d to slide showing of d"
                "    end if"
                "  end try"
                "end tell"))
   (lambda () (process-running? "Keynote"))))

;; LibreOffice edits .pptx natively, so the document is the deck, as with
;; PowerPoint. What it does not have is a reload: opening a file it already has
;; open raises the window it is already showing, whatever is on disk now. A
;; deck is regenerated every time the program is saved, so left at that the
;; editor shows a deck from several edits ago -- and a save from there writes
;; that back over the program's work, which the merge then reads as a pile of
;; edits undoing everything. So it is asked over UNO instead, which is the
;; interface it does have, and the slide in view is kept across the reload.
;;
;; It runs on a profile of its own, under the scratch beside the program. That
;; is what makes the socket certain -- LibreOffice is one process per profile,
;; and a copy the user already had running would otherwise swallow the launch
;; and have no socket -- and it is somewhere to say, once, that saving a .pptx
;; as a .pptx needs no warning.
(define-runtime-path libreoffice-driver "libreoffice.py")

(define LIBREOFFICE-PORT 2143)

;; On a Mac it is inside the application bundle rather than on the PATH, which
;; is where a `raco glide` run would otherwise conclude there is no LibreOffice
;; and reach for PowerPoint instead.
(define MAC-SOFFICE "/Applications/LibreOffice.app/Contents/MacOS/soffice")

;; Whether there is a PowerPoint to drive. Without one, `tell application
;; "Microsoft PowerPoint"` cannot even be compiled -- AppleScript has no
;; dictionary to read `full name` out of -- and every reload fails with a
;; syntax error about a property, which says nothing about what is wrong.
(define (powerpoint-installed?)
  (and (eq? 'macosx (system-type 'os))
       (directory-exists? "/Applications/Microsoft PowerPoint.app")))

(define (soffice-exe)
  (or (find-executable-path "soffice")
      (find-executable-path "libreoffice")
      (and (file-exists? MAC-SOFFICE) (string->path MAC-SOFFICE))))

;; UNO comes from LibreOffice's own Python. A Mac's `python3` is the system's
;; and knows nothing about it, and even on Linux the packaged one is surer than
;; whatever `python3` happens to mean today.
(define (libreoffice-driver! what pptx)
  (define py (find-executable-path "python3"))
  (and py
       (file-exists? libreoffice-driver)
       (parameterize ([current-output-port (open-output-nowhere)]
                      [current-error-port (open-output-nowhere)])
         (system*/exit-code py (path->string libreoffice-driver) what
                            (number->string LIBREOFFICE-PORT)
                            (path->string (path->complete-path pptx))))))

;; --------------------------- reloading LibreOffice without a Python at all

;; LibreOffice has no reload on its command line, and the UNO bridge that would
;; ask for one needs its own Python -- which a recent macOS will not let anyone
;; but LibreOffice run. But it will run a *Basic* macro named on the command
;; line, and a second `soffice` hands that to the copy already running. So the
;; reload is a macro, installed once into the profile LibreOffice is using.
;;
;; Its own library, `Glide`, rather than `Standard`: somebody's own macros live
;; in Standard and are not ours to write over.

;; Where LibreOffice keeps the profile it is using. Only somewhere that already
;; exists -- if LibreOffice has never run there is nothing to install into, and
;; making one would be making decisions about somebody's configuration.
(define (libreoffice-user-dir)
  (define home (find-system-path 'home-dir))
  (define candidates
    (case (system-type 'os)
      [(macosx) (list (build-path home "Library" "Application Support"
                                  "LibreOffice" "4" "user"))]
      [(windows) (let ([app (getenv "APPDATA")])
                   (if app (list (build-path app "LibreOffice" "4" "user")) '()))]
      [else (list (build-path home ".config" "libreoffice" "4" "user"))]))
  (for/or ([d (in-list candidates)])
    (and (directory-exists? (build-path d "basic")) d)))

;; `&`, `<` and `>` are Basic's string concatenation and comparisons, and the
;; module is XML: unescaped, the file does not parse and the library silently
;; does not load, which looks exactly like a macro that ran and did nothing.
(define (xml-text s)
  (regexp-replaces s '((#rx"&" "\\&amp;") (#rx"<" "\\&lt;") (#rx">" "\\&gt;"))))

(define (glide-basic target-file proof-file)
  (format #<<BASIC
REM  *****  BASIC  *****

' Reload the deck glide named, and say whether it was open to be reloaded.
'
' The URL is read from a file rather than passed as an argument to the macro,
' so nothing has to survive being quoted through a URL and a shell.
'
' `StarDesktop.Components` holds more than documents -- the Basic IDE is in
' there -- and asking one of those for its URL is an error that takes the whole
' macro with it, so each one is checked for `XModel` first.
Sub Run
  Dim sUrl As String
  Dim sFound As String
  Dim iFile As Integer
  Dim iIn As Integer
  Dim oEnum
  Dim oComp
  Dim oFrame
  Dim oDisp

  sFound = "not open"

  iIn = FreeFile
  Open "~a" For Input As #iIn
  Line Input #iIn, sUrl
  Close #iIn

  oEnum = StarDesktop.Components.createEnumeration()
  Do While oEnum.hasMoreElements()
    oComp = oEnum.nextElement()
    If HasUnoInterfaces(oComp, "com.sun.star.frame.XModel") Then
      If InStr(oComp.getURL(), sUrl) > 0 Then
        oFrame = oComp.CurrentController.Frame
        oDisp = createUnoService("com.sun.star.frame.DispatchHelper")
        oDisp.executeDispatch(oFrame, ".uno:Reload", "", 0, Array())
        sFound = "reloaded"
      End If
    End If
  Loop

  iFile = FreeFile
  Open "~a" For Output As #iFile
  Print #iFile, sFound
  Close #iFile
End Sub

' Whether the deck glide named is still open. Closing the editor is how a
' session ends, and this is how that is noticed without a Python either.
Sub Ask
  Dim sUrl As String
  Dim sFound As String
  Dim iFile As Integer
  Dim iIn As Integer
  Dim oEnum
  Dim oComp

  sFound = "not open"

  iIn = FreeFile
  Open "~a" For Input As #iIn
  Line Input #iIn, sUrl
  Close #iIn

  oEnum = StarDesktop.Components.createEnumeration()
  Do While oEnum.hasMoreElements()
    oComp = oEnum.nextElement()
    If HasUnoInterfaces(oComp, "com.sun.star.frame.XModel") Then
      If InStr(oComp.getURL(), sUrl) > 0 Then
        sFound = "open"
      End If
    End If
  Loop

  iFile = FreeFile
  Open "~a" For Output As #iFile
  Print #iFile, sFound
  Close #iFile
End Sub
BASIC
          target-file proof-file target-file proof-file))

;; Written every time, because they are ours; the index of libraries is only
;; added to, because it is not.
(define (install-glide-macro! user)
  (define lib (build-path user "basic" "Glide"))
  (make-directory* lib)
  (define target (path->string (build-path user "glide-target.txt")))
  (define proof (path->string (build-path user "glide-reloaded.txt")))
  (display-to-file
   (string-append
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<!DOCTYPE script:module PUBLIC \"-//OpenOffice.org//DTD OfficeDocument 1.0//EN\""
    " \"module.dtd\">\n"
    "<script:module xmlns:script=\"http://openoffice.org/2000/script\""
    " script:name=\"Reload\" script:language=\"StarBasic\">"
    (xml-text (glide-basic target proof))
    "</script:module>")
   (build-path lib "Reload.xba") #:exists 'replace)
  (display-to-file
   (string-append
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<!DOCTYPE library:library PUBLIC \"-//OpenOffice.org//DTD OfficeDocument 1.0//EN\""
    " \"library.dtd\">\n"
    "<library:library xmlns:library=\"http://openoffice.org/2000/library\""
    " library:name=\"Glide\" library:readonly=\"false\" library:passwordprotected=\"false\">\n"
    " <library:element library:name=\"Reload\"/>\n</library:library>")
   (build-path lib "script.xlb") #:exists 'replace)
  (define xlc (build-path user "basic" "script.xlc"))
  (when (file-exists? xlc)
    (define text (file->string xlc))
    (unless (regexp-match? #rx"library:name=\"Glide\"" text)
      (display-to-file
       (regexp-replace #rx"</library:libraries>" text
                       (string-append
                        " <library:library library:name=\"Glide\" library:link=\"false\"/>\n"
                        "</library:libraries>"))
       xlc #:exists 'replace)))
  (values target proof))

;; Whether the macro has been put there this session, and whether it can be.
(define glide-macro (box 'unknown))

(define (glide-macro-files)
  (when (eq? 'unknown (unbox glide-macro))
    (define user (libreoffice-user-dir))
    (set-box!
     glide-macro
     (and user
          (with-handlers ([exn:fail? (lambda (_e) #f)])
            (define-values (target proof) (install-glide-macro! user))
            (cons target proof)))))
  (unbox glide-macro))

;; Runs `soffice` and gives up on it after `seconds`. A macro handed to a
;; LibreOffice that is not running starts one, which then sits there being a
;; running application: without a bound, the loop would stop there.
(define (soffice-bounded exe args seconds)
  ;; Pipes rather than a sink: `subprocess` takes file-stream ports or nothing,
  ;; and handing it anything else raises rather than running the process.
  (define-values (sp out in err) (apply subprocess #f #f #f exe args))
  (define ok? (and (sync/timeout seconds sp) (eqv? 0 (subprocess-status sp))))
  (unless (memq (subprocess-status sp) '(0))
    (with-handlers ([exn:fail? void]) (subprocess-kill sp #t)))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  ok?)

;; The `soffice` that carries the request exits as soon as the copy already
;; running has been handed it, which is before that copy has run the macro. So
;; the answer is waited for rather than looked for once.
(define (wait-for-file p seconds)
  (let loop ([left (* 20 seconds)])
    (cond
      [(file-exists? p) #t]
      [(<= left 0) #f]
      [else (sleep 0.05) (loop (sub1 left))])))

;; Whether LibreOffice is up at all. A macro handed to a LibreOffice that is not
;; running starts one with nothing in it, which then sits there being a running
;; application -- so the macro is only worth handing over when there is a copy
;; to hand it to. `pgrep` is on both a Mac and a Linux; where there is none this
;; says yes and the bound on the dispatch is the backstop.
(define (soffice-running?)
  (define pgrep (find-executable-path "pgrep"))
  ;; A headless LibreOffice whose display went away can remain as a zombie
  ;; until its parent reaps it. `pgrep` reports that process even though there
  ;; is nobody there to receive a macro; treating it as live makes the first
  ;; reload request start an empty office instead of opening the deck.
  (define (zombie? pid)
    (define stat (build-path "/proc" pid "stat"))
    (and (file-exists? stat)
         (with-handlers ([exn:fail? (lambda (_e) #f)])
           (define m (regexp-match #rx"^[0-9]+ [(].*[)] ([A-Z]) " (file->string stat)))
           (and m (string=? "Z" (second m))))))
  (cond
    [(not pgrep) #t]
    [else
     (for/or ([name (in-list '("soffice.bin" "soffice"))])
       (define out (open-output-string))
       (define code
         (parameterize ([current-output-port out]
                        [current-error-port (open-output-nowhere)])
           (system*/exit-code pgrep "-x" name)))
       (and (eqv? 0 code)
            (for/or ([pid (in-list (string-split (get-output-string out)))])
              (not (zombie? pid)))))]))

;; Runs one of the macro's subs about one deck and hands back what it said, or
;; #f when it could not be asked at all.
(define (libreoffice-macro! sub pptx)
  (define exe (soffice-exe))
  (define files (glide-macro-files))
  (cond
    [(not (and exe files (soffice-running?))) #f]
    [else
     (define target (car files))
     (define proof (cdr files))
     (with-handlers ([exn:fail? (lambda (_e) #f)])
       (display-to-file (string-append (path->url-string (path->complete-path pptx)) "\n")
                        target #:exists 'replace)
       (when (file-exists? proof) (delete-file proof))
       (soffice-bounded exe (list (format "macro:///Glide.Reload.~a" sub)) 12)
       (wait-for-file proof 10)
       (and (file-exists? proof)
            (let ([said (file->string proof)])
              (cond
                [(regexp-match? #rx"reloaded" said) 'reloaded]
                [(regexp-match? #rx"not open" said) 'not-open]
                [(regexp-match? #rx"open" said) 'open]
                [else #f]))))]))

(define (libreoffice-macro-reload! pptx) (libreoffice-macro! "Run" pptx))
(define (libreoffice-macro-open? pptx) (libreoffice-macro! "Ask" pptx))

;; LibreOffice matches documents by URL, and its own are `file://` with the
;; awkward characters escaped.
(define (path->url-string p)
  (string-append "file://"
                 (regexp-replace* #rx"[ ?#]"
                                  (path->string p)
                                  (lambda (m)
                                    (case (string-ref m 0)
                                      [(#\space) "%20"] [(#\?) "%3F"] [else "%23"])))))

;; How this deck is going to be reloaded, said once and only when it is not the
;; usual way. The macro has to be installed before LibreOffice starts, because
;; LibreOffice reads its Basic libraries once, when it starts -- so this is
;; called on the way to the launch rather than on the way to a reload.
(define reload-way-said (box #f))

(define (note-reload-way!)
  (unless (unbox reload-way-said)
    (set-box! reload-way-said #t)
    ;; Nothing to say when it works: the deck reloading is the whole report.
    (unless (glide-macro-files)
      (log! (string-append
             "  no LibreOffice profile here to install the reload macro in, so\n"
             "  the deck may keep showing an older version than the program.\n"
             "  File > Reload refreshes it by hand.\n")))))

(define (libreoffice-launch! pptx)
  (define exe (soffice-exe))
  (note-reload-way!)
  ;; Started on the person's own LibreOffice, with their settings and none of
  ;; the dialogs a fresh profile puts up -- and a fresh profile puts up a
  ;; welcome wizard, which is modal, which means nothing can be asked of the
  ;; document underneath it and a reload goes nowhere.
  ;;
  ;; The socket is what the reload is asked over. LibreOffice is one process
  ;; per profile, so a copy already running without one swallows this launch
  ;; and there is nothing to ask: then the deck is still written, still opens,
  ;; and only the reload is lost -- which the loop says out loud rather than
  ;; leaving a stale deck on screen looking current.
  ;; Argument by argument rather than as a command line: a talk kept in a
  ;; folder with a space in its name is not an unusual thing to have.
  (and exe
       (begin
         (process*/ports
          (open-output-nowhere) #f (open-output-nowhere)
          exe "--norestore"
          (format "--accept=socket,host=localhost,port=~a;urp;" LIBREOFFICE-PORT)
          (path->string (path->complete-path pptx)))
         #t)))

(define libreoffice-adapter
  (app-adapter
   'libreoffice
   (lambda (pptx) pptx)
   (lambda (doc pptx) #t)
   (lambda (pptx)
     ;; UNO waits for the asynchronous reload and restores the slide in view.
     ;; The Basic macro is the fallback on systems whose Python cannot import
     ;; LibreOffice's UNO module (notably current macOS packages).
     (case (libreoffice-driver! "reload" pptx)
       [(0) #t]
       [else
        (case (libreoffice-macro-reload! pptx)
          [(reloaded) #t]
          [else (libreoffice-launch! pptx)])]))
   ;; Closing the editor ends the session. A macro that could not be asked says
   ;; nothing rather than "closed": ending a session because an answer did not
   ;; arrive would throw away the work it was in the middle of.
   (lambda ()
     (define deck (current-libreoffice-deck))
     (case (and deck (libreoffice-driver! "open" deck))
       [(0) #t]
       [(4) #f]
       [else
        (case (and deck (libreoffice-macro-open? deck))
          [(not-open) #f]
          [else #t])]))))

;; Which deck `open?` should ask about. The adapter's own `open?` takes no
;; argument -- it is asked about the session, and there is one deck in it.
(define current-libreoffice-deck (make-parameter #f))

(define adapters
  (hash 'none none-adapter 'powerpoint powerpoint-adapter
        'keynote keynote-adapter 'libreoffice libreoffice-adapter))

(define (adapter-named name)
  (hash-ref adapters name
            (lambda () (error 'watch "no adapter named ~a; try one of ~a"
                              name (sort (map symbol->string (hash-keys adapters))
                                         string<?)))))

;; ----------------------------------------------------------------- hashing

;; A .key is a directory, so a bundle is summarized by what it contains rather
;; than read whole.
(define (content-hash path)
  (cond
    [(not (or (file-exists? path) (directory-exists? path))) #f]
    [(directory-exists? path)
     (sha1 (open-input-string
            (string-join
             (for/list ([p (in-list (sort (map path->string (all-files path)) string<?))])
               (define f (string->path p))
               (format "~a:~a:~a" p (file-size f) (file-or-directory-modify-seconds f)))
             "\n")))]
    [else (call-with-input-file path sha1)]))

;; A Rhombus program is its root module and the local modules it imports. The
;; path list is part of the hash, so adding or removing an import is a change
;; even when the remaining files happen to have the same bytes.
(define (program-sources program [fallback #f])
  (define root (simplify-path (path->complete-path program) #f))
  (with-handlers ([exn:fail? (lambda (_e) (or fallback (list root)))])
    (program-source-files root)))

(define (sources-content-hash sources)
  (sha1
   (open-input-string
    (string-join
     (for/list ([source (in-list sources)])
       (format "~a:~a" (path->string source) (content-hash source)))
     "\n"))))

(define (program-content-hash program)
  (sources-content-hash (program-sources program)))

(define (all-files dir)
  (append*
   (for/list ([p (in-list (directory-list dir #:build? #t))])
     (cond [(directory-exists? p) (all-files p)]
           [else (list p)]))))

;; Waits for a path to stop changing, so one save is one event.
(define (settle path #:quiet [quiet 0.25] #:limit [limit 5.0])
  (let loop ([h (content-hash path)] [waited 0.0])
    (sleep quiet)
    (define h2 (content-hash path))
    (cond
      [(equal? h h2) h2]
      [(>= waited limit) h2]
      [else (loop h2 (+ waited quiet))])))

(define (settle-program program #:sources [sources #f]
                        #:quiet [quiet 0.25] #:limit [limit 5.0])
  (define watched (or sources (program-sources program)))
  (let loop ([h (sources-content-hash watched)] [waited 0.0])
    (sleep quiet)
    (define h2 (sources-content-hash watched))
    (cond
      [(equal? h h2) h2]
      [(>= waited limit) h2]
      [else (loop h2 (+ waited quiet))])))

;; ------------------------------------------------------------------- steps

;; program -> deck, then show it.
(define (regenerate! program pptx adapter #:width [w #f] #:height [h #f])
  (log! "program changed -> regenerating ~a\n" (file-name-from-path pptx))
  (define warnings (box '()))
  (with-handlers ([exn:fail? (lambda (e)
                               (log! "  ! regeneration failed: ~a\n"
                                     (first (string-split (exn-message e) "\n")))
                               #f)])
    (define picts (program-picts program))
    ;; Refuse an untraceable source shape before replacing the editor's deck.
    ;; The returned states become the new agreed base only after export works.
    (define states (validate-program-picts! program picts))
    (parameterize ([current-export-warnings warnings])
      (picts->pptx picts pptx #:width w #:height h))
    ;; The editor and the base advance together. If reload fails, it is still
    ;; showing the old deck, so recording these new source identities would
    ;; pair a stale editor document with a future base.
    (unless ((app-adapter-reload! adapter) pptx)
      (error 'glide "~a did not reload the regenerated deck"
             (app-adapter-name adapter)))
    (record-program-base! program pptx #:states states)
    (for ([m (in-list (remove-duplicates (reverse (unbox warnings))))])
      (log! "  note: ~a\n" m))
    (log! "  ~a slides written\n" (length picts))
    #t))

(define (program-picts program) (load-program-picts program))

;; deck -> program.
;; Returns #t when the merge went through, which means *all* of it did. A save
;; is one thing: if any edit in it cannot be written, none of them is, and this
;; says so and fails. Writing some and reporting the rest would leave the
;; program and the deck each holding part of what was done.
;;
;; On a refusal the whole message is logged, not its first line: what to do
;; about it is usually on the lines after the first, and the editor is still
;; open on the slide that caused it.
(define (merge-back! program pptx document adapter workdir)
  (log! "deck changed -> merging into ~a\n" (file-name-from-path program))
  (with-handlers ([exn:fail? (lambda (e)
                               (log! "  ! merge refused\n")
                               (for ([l (in-list (string-split (exn-message e) "\n"))])
                                 (log! "    ~a\n" (string-trim l #:right? #f)))
                               #f)])
    ;; For an app that does not save .pptx, get one out of it first.
    (unless (equal? (path->string document) (path->string pptx))
      ((app-adapter-harvest! adapter) document pptx))
    ;; Until it settles. One pass can leave work for the next -- an edit
    ;; inside a form another edit rewrote wholesale is left for a second look,
    ;; and grouping changes the drawing order -- and the deck is written again
    ;; from the program as soon as this returns, so anything still outstanding
    ;; would be written over rather than kept.
    (let loop ([pass 1])
      (define r (sync-once program pptx #:workdir workdir #:atomic? #t))
      ;; A later pass that found nothing is the usual case and says nothing.
      (when (or (= pass 1) (pair? (sync-report-actions r)))
        (for ([l (in-list (string-split (format-sync-report r) "\n"))]) (log! "~a\n" l)))
      (define left (sync-report-skipped r))
      ;; Whether the save landed. Not whether anything was refused: a difference
      ;; the source has no place for -- an element it does not draw with an `at`,
      ;; a property it does not hold as a literal -- is reported and the save
      ;; still lands, because there is nothing a person could go and fix. Read
      ;; as a refusal it stopped every save on the slide, for ever.
      (define landed? (sync-report-base-written? r))
      (cond
        [(not landed?)
         (log! "  ! nothing was merged: ~a of these could not be written\n" (length left))
         #f]
        [(pair? left)
         (log! "  ~a of these could not be written; the rest was merged\n" (length left))
         #t]
        ;; A pass that carried an edit onto the other frames of a build has
        ;; left the deck behind on purpose: those frames still hold the old
        ;; value, and looking again would read that as an edit undoing the one
        ;; just made. The deck is written from the program as soon as this
        ;; returns, which is what puts them back in step.
        [(sync-report-deck-behind? r) #t]
        [(or (null? (sync-report-applied r)) (>= pass 3)) #t]
        [else (loop (add1 pass))]))))

;; Closing the editor ends the session, and so does Ctrl-C. Either way the last
;; edits are merged first and the scratch is cleared -- but only if that merge
;; went through. A refusal means the deck still holds something the program does
;; not, and deleting it would throw that away.
(define (finish! program pptx document adapter workdir why)
  (log! "~a\n" why)
  (define merged?
    (cond
      [(not (file-exists? document))
       (log! "  nothing left to merge\n")
       #t]
      [else (merge-back! program pptx document adapter workdir)]))
  (define scratch (scratch-dir-of program))
  (cond
    [(not merged?)
     (log! "  keeping ~a: the deck has edits this could not merge, and the
"
           (file-name-from-path scratch))
     (log! "  program was left as it was rather than take some of them\n")]
    [(not (directory-exists? scratch)) (void)]
    [else
     (delete-directory/files scratch #:must-exist? #f)
     (log! "  cleared ~a\n" (file-name-from-path scratch))])
  merged?)

;; The scratch beside a program, which is where the deck, the editor's document
;; and the agreed base live.
(define (scratch-dir-of program)
  (define full (path->complete-path program))
  (build-path (or (path-only full) (current-directory)) ".glide"))

;; ------------------------------------------------------------------- loop

;; One pass, for tests and for a single shot from the command line.
(define (watch-once program pptx #:adapter [adapter none-adapter]
                    #:width [w #f] #:height [h #f] #:workdir [workdir #f])
  (regenerate! program pptx adapter #:width w #:height h))

;; Polls until interrupted. Only one side is acted on per tick, and the program
;; wins, so a burst of edits on both cannot interleave into a half-merge.
(define (watch-loop program pptx
                    #:adapter [adapter none-adapter]
                    #:width [w #f] #:height [h #f]
                    #:workdir [workdir #f]
                    #:interval [interval 0.4]
                    #:open-check [open-check 10.0]
                    #:ticks [ticks #f])
  (define document ((app-adapter-document adapter) pptx))
  ;; Which deck this session is about, for an adapter that has to ask the
  ;; editor whether it is still open.
  (current-libreoffice-deck pptx)
  (log! "watching\n  program ~a\n  deck    ~a\n  app     ~a\n"
        program document (app-adapter-name adapter))
  ;; The program is the truth, so a session starts from it. Anything left in
  ;; the scratch is from a session that ended without merging -- a crash, or a
  ;; deck opened behind glide's back -- and merging it would be merging edits
  ;; against a program that has moved on since. It goes, and the deck is
  ;; written again from the program.
  (define scratch (scratch-dir-of program))
  (when (and (directory-exists? scratch) (file-exists? (base-path-for program)))
    (log! "  clearing ~a, left from a session that did not finish\n"
          (file-name-from-path scratch))
    (delete-directory/files scratch #:must-exist? #f))
  ;; The deck lives in the scratch, and clearing it took the folder with it.
  ;; Put it back before writing the deck into it -- and put back whatever else
  ;; the caller pointed the deck at, since `--out` can name anywhere.
  (make-directory* scratch)
  (let ([deck-dir (path-only (path->complete-path pptx))])
    (when deck-dir (make-directory* deck-dir)))
  (define wrote-deck? (regenerate! program pptx adapter #:width w #:height h))
  ;; A deck that was never written is not one to merge from: reading it raises,
  ;; and the session dies on the way up rather than waiting for the program to
  ;; be fixed. Which is what watching is for.
  (cond
    [(and wrote-deck? (file-exists? pptx)) (sync-once program pptx #:workdir workdir)]
    [else
     (log! "  no deck to merge from yet; fix the program and save it\n")])
  ;; `stuck?` means the editor holds edits that were refused. Until they are
  ;; resolved the deck is not regenerated -- otherwise the obvious next move,
  ;; fixing the program as the message asks, would overwrite the very slides the
  ;; refusal was protecting. While stuck, a change on either side retries the
  ;; merge: the fix can be in the editor or in the program.
  ;; Asking the editor whether it is still open costs a subprocess, so it is
  ;; asked on a timer rather than every tick.
  (define open-every (max 1 (inexact->exact (round (/ open-check (max interval 0.01))))))
  (define (merge-and-refresh!)
    (define merged? (merge-back! program pptx document adapter workdir))
    (cond
      [(not merged?) #f]
      [(regenerate! program pptx adapter #:width w #:height h) #t]
      [else
       ;; `sync-once` has advanced the source and base, but the editor is still
       ;; showing the saved document with the identities it had before source
       ;; splices shifted later call sites. Do not accept another editor save
       ;; against that pairing. Restarting rebuilds it from the program.
       (error 'glide
              "watch stopped because the merged deck could not be regenerated and reloaded")]))
  ;; Said once the first deck is written and before the first hash is taken, so
  ;; that "it is watching now" is something anyone can wait for rather than
  ;; guess at. A cold start compiles the runtime and takes seconds.
  (log! "watching for changes\n")
  ;; Discover imports once per program change, not once per polling tick. If an
  ;; import is added or removed, the module containing that import is already
  ;; in this set and changes first; the graph is then refreshed below. Parsing
  ;; a large talk every 0.4 seconds would otherwise keep a core busy while idle.
  (define initial-sources (program-sources program))
  (with-handlers ([exn:break? (lambda (_e)
                                (newline)
                                (finish! program pptx document adapter workdir
                                         "interrupted"))])
   (let loop ([prog-sources initial-sources]
             [prog-hash (sources-content-hash initial-sources)]
             [doc-hash (content-hash document)]
             [stuck? #f]
             [n 0])
    (cond
      [(and ticks (>= n ticks)) (log! "done\n")]
      ;; The editor was closed, so the session is over.
      [(and (zero? (modulo n open-every)) (positive? n)
            (not ((app-adapter-open? adapter))))
       (finish! program pptx document adapter workdir
                (format "~a closed" (app-adapter-name adapter)))]
      [else
       (sleep interval)
       (define ph (sources-content-hash prog-sources))
       (define dh (content-hash document))
       (define changed?
         (or (and ph (not (equal? ph prog-hash)))
             (and dh (not (equal? dh doc-hash)))))
       (cond
         [(and stuck? changed?)
          (settle-program program #:sources prog-sources)
          (settle document)
          (define ok? (merge-and-refresh!))
          (unless ok?
            (log! "    the program is untouched and the deck is not being rewritten,
")
            (log! "    so nothing you have done in ~a is lost. To drop those edits
"
                  (app-adapter-name adapter))
            (log! "    and take the program as it is, delete ~a and save the program.
"
                  (file-name-from-path (base-path-for program))))
          (define next-sources (program-sources program prog-sources))
          (loop next-sources (sources-content-hash next-sources)
                (content-hash document) (not ok?) (add1 n))]
         [stuck? (loop prog-sources prog-hash doc-hash #t (add1 n))]
         [(and ph (not (equal? ph prog-hash)))
          (settle-program program #:sources prog-sources)
          (unless (regenerate! program pptx adapter #:width w #:height h)
            ;; Continuing would let a later save from a stale editor document
            ;; be compared with identities it has never seen. Stop with the old
            ;; base intact; restarting retries the reload from the program.
            (error 'glide
                   "watch stopped because the regenerated deck could not be reloaded"))
          ;; Re-read both, since we just wrote the deck ourselves.
          (define next-sources (program-sources program prog-sources))
          (loop next-sources (sources-content-hash next-sources)
                (content-hash document) #f (add1 n))]
         [(and dh (not (equal? dh doc-hash)))
          (settle document)
          (define ok? (merge-and-refresh!))
          (unless ok?
            (log! "    the program is untouched and the deck will not be rewritten,
")
            (log! "    so nothing you have done in ~a is lost -- fix what it names,
"
                  (app-adapter-name adapter))
            (log! "    there or in the program, and save again.
"))
          (define next-sources (program-sources program prog-sources))
          (loop next-sources (sources-content-hash next-sources)
                (content-hash document) (not ok?) (add1 n))]
         [else (loop prog-sources prog-hash doc-hash #f (add1 n))])]))))
