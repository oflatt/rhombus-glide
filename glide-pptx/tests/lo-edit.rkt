#lang racket/base
;; The workflow this whole thing is for: drag it in LibreOffice, save, and the
;; program changes.
;;
;; Every other test edits the deck the way we would -- by rewriting its XML.
;; That is not the workflow. LibreOffice loads the deck, applies the edit
;; through its own model, and writes the file again from scratch; what the merge
;; is handed is LibreOffice's idea of the deck, edit and rewrite together. A
;; drag that works when we forge it can still be lost when a real editor makes
;; it, and a shape added by a real editor is not the shape we would have added.
;;
;; So each fixture is exported, handed to a headless LibreOffice which moves,
;; retypes, deletes and adds through the drawing API, saved back as .pptx, and
;; merged. Two things are asked of the merge, and the second is what keeps the
;; first honest:
;;
;;   * every edit made is applied -- an edit reported and refused is a failure;
;;   * a second pass has nothing left to say -- the program now draws the deck
;;     LibreOffice wrote.
;;
;; Without the first, refusing everything would pass. Without the second,
;; applying an edit into the wrong place would.
;;
;; LibreOffice is driven by a Basic macro in a scratch profile, which needs no
;; UNO bridge and no display. Where there is no `soffice` this says so and
;; passes.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/port racket/runtime-path
         glide-pptx/parse glide-pptx/export glide-pptx/sync glide-pptx/sync-state
         glide-pptx/emit-rhombus)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-lo-edit"))
(define profile (build-path work "profile"))

(define soffice (or (find-executable-path "soffice") (find-executable-path "libreoffice")))

;; LibreOffice measures in hundredths of a millimetre, and a point is 25.4/72 of
;; one. Rounded here rather than in Basic: a decimal written into the job file
;; is read back through the profile's locale, where a comma may be the point.
(define (mm100 pt) (inexact->exact (round (* pt 2540/72))))

;; The macro. It reads a job file -- one tab-separated command a line -- and
;; does to the document what the file says. Glide writes the tag into the
;; description and a readable name into the object list. LibreOffice discards
;; the description of groups, so the test driver accepts the readable suffix
;; of an automatic tag too; exported names are unique within each slide.
(define glide-edit-basic #<<BASIC
Dim gDoc As Object
Dim gLog As String

Sub Note(msg As String)
  Dim h As Integer
  If gLog = "" Then Exit Sub
  h = Freefile
  Open gLog For Append As #h
  Print #h, msg
  Close #h
End Sub

Sub Run
  Dim job As String, sLine As String, n As Integer
  job = Environ("GLIDE_JOB")
  gLog = Environ("GLIDE_LOG")
  If job = "" Then Exit Sub
  n = Freefile
  Open job For Input As #n
  Do While Not EOF(n)
    Line Input #n, sLine
    Do1(sLine)
  Loop
  Close #n
End Sub

Function ShapeOf(slide As Integer, name As String) As Object
  ShapeOf = ShapeIn(gDoc.DrawPages.getByIndex(slide - 1), name)
End Function

Function HasName(sh As Object, name As String) As Boolean
  HasName = (sh.Name = name Or sh.Description = "glide-pptx:" & name)
  ' source: plus forty hexadecimal digits is the stable key. What follows its
  ' next colon is only the readable object-list name.
  If Not HasName And Left(name, 7) = "source:" And Len(name) > 48 Then
    If Mid(name, 48, 1) = ":" Then HasName = (sh.Name = Mid(name, 49))
  End If
End Function

' What holds the shape, which is the page or the group it is in: a child is
' removed from its group, and asking the page to remove it does nothing at all.
Function HolderOf(where As Object, name As String) As Object
  Dim i As Integer, sh As Object, inner As Object
  For i = 0 To where.Count - 1
    sh = where.getByIndex(i)
    If HasName(sh, name) Then
      HolderOf = where
      Exit Function
    End If
    If sh.supportsService("com.sun.star.drawing.GroupShape") Then
      inner = HolderOf(sh, name)
      If Not IsNull(inner) Then
        HolderOf = inner
        Exit Function
      End If
    End If
  Next i
End Function

Function ShapeIn(where As Object, name As String) As Object
  Dim i As Integer, sh As Object, inner As Object
  For i = 0 To where.Count - 1
    sh = where.getByIndex(i)
    If HasName(sh, name) Then
      ShapeIn = sh
      Exit Function
    End If
    If sh.supportsService("com.sun.star.drawing.GroupShape") Then
      inner = ShapeIn(sh, name)
      If Not IsNull(inner) Then
        ShapeIn = inner
        Exit Function
      End If
    End If
  Next i
End Function

Sub Do1(sLine As String)
  On Error Goto Failed
  Dim f() As String
  Dim cmd As String
  Dim sh As Object, pg As Object, ns As Object
  Dim pos As New com.sun.star.awt.Point
  Dim sz As New com.sun.star.awt.Size
  Dim op(0) As New com.sun.star.beans.PropertyValue
  Dim sp(0) As New com.sun.star.beans.PropertyValue
  Dim i As Integer, h As Integer
  If Len(Trim(sLine)) = 0 Then Exit Sub
  f = Split(sLine, Chr(9))
  cmd = f(0)
  Select Case cmd
    Case "open"
      ' Not hidden: copy and paste go through the document's own controller,
      ' and a hidden document has none. Headless, there is no window either way.
      op(0).Name = "Hidden"
      op(0).Value = False
      gDoc = StarDesktop.loadComponentFromURL(ConvertToURL(f(1)), "_blank", 0, op())
    Case "list"
      h = Freefile
      Open f(2) For Output As #h
      pg = gDoc.DrawPages.getByIndex(CInt(f(1)) - 1)
      For i = 0 To pg.Count - 1
        sh = pg.getByIndex(i)
        Print #h, sh.Name & Chr(9) & sh.Description & Chr(9) & sh.Position.X & Chr(9) & sh.Position.Y & Chr(9) & sh.Size.Width & Chr(9) & sh.Size.Height
      Next i
      Close #h
    Case "move"
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      pos.X = sh.Position.X + CLng(f(3))
      pos.Y = sh.Position.Y + CLng(f(4))
      sh.setPosition(pos)
    Case "resize"
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      sz.Width = CLng(f(3))
      sz.Height = CLng(f(4))
      sh.setSize(sz)
    Case "delete"
      pg = HolderOf(gDoc.DrawPages.getByIndex(CInt(f(1)) - 1), f(2))
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Or IsNull(pg) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      pg.remove(sh)
    Case "addtext"
      pg = gDoc.DrawPages.getByIndex(CInt(f(1)) - 1)
      ns = gDoc.createInstance("com.sun.star.drawing.TextShape")
      pg.add(ns)
      sz.Width = CLng(f(4))
      sz.Height = CLng(f(5))
      ns.setSize(sz)
      pos.X = CLng(f(2))
      pos.Y = CLng(f(3))
      ns.setPosition(pos)
      ns.setString(f(6))
    Case "fill"
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      sh.FillStyle = com.sun.star.drawing.FillStyle.SOLID
      sh.FillColor = CLng(f(3))
    Case "copy"
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      Dim ctrl As Object, disp As Object
      ctrl = gDoc.CurrentController
      If IsNull(ctrl) Then
        Note("no controller to copy through")
        Exit Sub
      End If
      ctrl.select(sh)
      disp = createUnoService("com.sun.star.frame.DispatchHelper")
      disp.executeDispatch(ctrl.Frame, ".uno:Copy", "", 0, Array())
      disp.executeDispatch(ctrl.Frame, ".uno:Paste", "", 0, Array())
    Case "retext"
      sh = ShapeOf(CInt(f(1)), f(2))
      If IsNull(sh) Then
        Note("no shape <" & f(2) & "> on slide " & f(1))
        Exit Sub
      End If
      Dim paras As Object, runs As Object
      paras = sh.Text.createEnumeration()
      If Not paras.hasMoreElements() Then
        Note("nothing to retype in <" & f(2) & ">")
        Exit Sub
      End If
      runs = paras.nextElement().createEnumeration()
      If Not runs.hasMoreElements() Then
        Note("nothing to retype in <" & f(2) & ">")
        Exit Sub
      End If
      runs.nextElement().setString(f(3))
    Case "save"
      sp(0).Name = "FilterName"
      sp(0).Value = "Impress MS PowerPoint 2007 XML"
      gDoc.storeToURL(ConvertToURL(f(1)), sp())
    Case "close"
      gDoc.close(True)
    Case "done"
      h = Freefile
      Open f(1) For Output As #h
      Print #h, "ok"
      Close #h
    Case "quit"
      StarDesktop.terminate()
  End Select
  Exit Sub
Failed:
  Note("could not do <" & sLine & ">: " & Error$)
End Sub
BASIC
  )

(define (xml-text s)
  (for/fold ([s s]) ([p (in-list '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))])
    (string-replace s (car p) (cdr p))))

;; A profile of its own, made by a run that does nothing: a shared one is locked
;; by whatever else is running, and the macro has to be put where LibreOffice
;; looks for it, which is inside a profile that already exists.
(define (install-macro!)
  (make-directory* profile)
  (define user (build-path profile "user"))
  (unless (file-exists? (build-path user "basic" "script.xlc"))
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* soffice (format "-env:UserInstallation=file://~a" (path->string profile))
               "--headless" "--norestore" "--nolockcheck" "--terminate_after_init")))
  (define lib (build-path user "basic" "Glide"))
  (make-directory* lib)
  (display-to-file
   (string-append
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<!DOCTYPE script:module PUBLIC \"-//OpenOffice.org//DTD OfficeDocument 1.0//EN\""
    " \"module.dtd\">\n"
    "<script:module xmlns:script=\"http://openoffice.org/2000/script\""
    " script:name=\"Edit\" script:language=\"StarBasic\">"
    (xml-text glide-edit-basic)
    "</script:module>")
   (build-path lib "Edit.xba") #:exists 'replace)
  (display-to-file
   (string-append
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<!DOCTYPE library:library PUBLIC \"-//OpenOffice.org//DTD OfficeDocument 1.0//EN\""
    " \"library.dtd\">\n"
    "<library:library xmlns:library=\"http://openoffice.org/2000/library\""
    " library:name=\"Glide\" library:readonly=\"false\" library:passwordprotected=\"false\">\n"
    " <library:element library:name=\"Edit\"/>\n</library:library>")
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
       xlc #:exists 'replace))))

;; The edits, made by LibreOffice, to the deck it is given.
(define (libreoffice-edit! pptx cmds)
  (define job (build-path work "job.txt"))
  (define out (build-path work "out.pptx"))
  (define done (build-path work "done.txt"))
  (define log (build-path work "log.txt"))
  (for ([p (in-list (list out done log))]) (when (file-exists? p) (delete-file p)))
  (display-to-file
   (string-join
    (for/list ([c (in-list (append (list (list "open" (path->string pptx)))
                                   cmds
                                   (list (list "save" (path->string out))
                                         (list "close")
                                         (list "done" (path->string done))
                                         (list "quit"))))])
      (string-join (map (lambda (v) (if (string? v) v (format "~a" v))) c) "\t"))
    "\n" #:after-last "\n")
   job #:exists 'replace)
  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (putenv "GLIDE_JOB" (path->string job))
    (putenv "GLIDE_LOG" (path->string log))
    (system* soffice (format "-env:UserInstallation=file://~a" (path->string profile))
             "--headless" "--norestore" "--nolockcheck" "macro:///Glide.Edit.Run"))
  ;; What it could not do, said out loud. Basic keeps going after an error in a
  ;; sub, so a command that failed is a command that silently did nothing --
  ;; which reads as a merge that lost the edit rather than as a test that never
  ;; made it.
  (define trouble (if (file-exists? log) (string-trim (file->string log)) ""))
  (unless (string=? "" trouble) (printf "     LibreOffice: ~a\n" trouble))
  (and (string=? "" trouble) (file-exists? out)
       (begin (copy-file out pptx #t) #t)))

;; The tags on a slide, in the program's own order, with the ones it cannot tell
;; apart left out: a tag naming two forms is refused by the merge, which is its
;; own test.
(define (slide-tags program)
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (define index-of
    (for/hash ([s (in-list (or scopes '()))] [i (in-naturals 1)] #:when s) (values s i)))
  (define shared
    (for/fold ([h (hash)]) ([s (in-list sites)])
      (hash-update h (cons (at-site-scope s) (at-site-tag s)) add1 0)))
  (for/fold ([h (hash)]) ([s (in-list sites)])
    (define i (hash-ref index-of (at-site-scope s) #f))
    (cond
      [(or (not i) (> (hash-ref shared (cons (at-site-scope s) (at-site-tag s)) 0) 1)) h]
      [else (hash-update h i (lambda (l) (append l (list s))) '())])))

;; A tag the job file can carry. LibreOffice Basic reads the file it is given in
;; the system encoding, so a name with an ellipsis or an accent in it arrives as
;; something else and names no shape at all. That is this test's limit, not the
;; merge's -- `retype-all` retypes those through the XML.
(define (nameable? st)
  (for/and ([c (in-string (at-site-tag st))]) (char<=? c #\~)))

(define (applied-kinds r)
  (for/list ([a (in-list (sync-report-applied r))]) (sync-action-kind a)))

(define (actionable-actions r)
  (filter (lambda (a) (not (eq? 'noted (sync-action-kind a))))
          (sync-report-actions r)))

(define (print-settle-actions r)
  (for ([a (in-list (sync-report-actions r))])
    (printf "     ~a: ~a ~s on slide ~a\n"
            (if (eq? 'noted (sync-action-kind a)) "informational" "unsettled")
            (sync-action-kind a) (sync-action-tag a) (sync-action-slide a))))

(define (edited-in-libreoffice name program dir)
  (define pptx (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) pptx)
  ;; The base first, by a pass that writes only the base: without it the merge
  ;; has nothing to compare the edited deck against and reports nothing at all.
  (void (sync-once program pptx #:workdir w))
  ;; A slide with enough on it to move one thing, retype another and delete a
  ;; third.
  (define by-slide (slide-tags program))
  ;; What the program draws, so a target is picked for what it is: a box with
  ;; words in it can be retyped, and a group cannot.
  (define states (program-slide-states program))
  (define (words-on i)
    (for/list ([st (in-list states)] #:when (= i (slide-state-index st))
               #:when #t
               [e (in-list (slide-state-elements st))]
               #:when (and (memq (el-state-kind e) '(text shape))
                           (not (string=? "" (el-state-text e)))
                           (el-state-tag e)))
      (el-state-tag e)))
  ;; Including the words inside a group. A group is one element to drag and its
  ;; children are still text somebody can click into and retype, so the tags
  ;; the group's own state carries are targets too -- and `04-pictures-groups`,
  ;; whose only text is in a group, was the fixture this test skipped.
  (define (group-words-on i)
    (for/list ([st (in-list states)] #:when (= i (slide-state-index st))
               #:when #t
               [e (in-list (slide-state-elements st))]
               #:when (eq? 'group (el-state-kind e))
               #:when #t
               [entry (in-list (group-text-entries (el-state-text e)))]
               ;; The ones with words in them: a group's digest names everything
               ;; it holds, and a plain oval has nothing to retype.
               #:when (not (string=? "" (cdr entry))))
      (car entry)))
  (define (has-words? i tag)
    (and (or (member tag (words-on i)) (member tag (group-words-on i))) #t))
  (define (fills-on i)
    (for/list ([st (in-list states)] #:when (= i (slide-state-index st))
               #:when #t
               [e (in-list (slide-state-elements st))]
               #:when (and (el-state-tag e) (assoc 'fill (el-state-style e))
                           (cdr (assoc 'fill (el-state-style e)))))
      (el-state-tag e)))
  (define slide
    (for/or ([i (in-list (sort (hash-keys by-slide) <))])
      (define ss (hash-ref by-slide i))
      (and (>= (length (filter nameable? ss)) 2)
           (ormap (lambda (st) (and (nameable? st) (has-words? i (at-site-tag st)))) ss)
           i)))
  (cond
    [(not slide) (printf "  ~a: no slide with a tagged element holding words\n" name)]
    [else
     (define ss (hash-ref by-slide slide))
     (define texty (findf (lambda (st) (and (nameable? st)
                                            (has-words? slide (at-site-tag st))))
                          ss))
     (define others (filter nameable? (remq texty ss)))
     (define to-move (if (pair? others) (first others) texty))
     (define to-delete (and (> (length others) 1) (last others)))
     ;; Recoloured: the editor's colour picker is one of the first things
     ;; anybody reaches for, and a fill is only an edit where the source states
     ;; one -- so it goes to an element whose own state has a fill in it.
     (define filled (fills-on slide))
     (define to-fill
       (findf (lambda (st) (and (member (at-site-tag st) filled)
                                (not (eq? st to-delete))))
              ss))
     (define poured "008080")
     (define typed "Typed in LibreOffice")
     (define added "Added in LibreOffice")
     (printf "  ~a: slide ~a -- moving ~s, retyping ~s, ~a, ~a, adding a text box\n"
             name slide (at-site-tag to-move) (at-site-tag texty)
             (if to-delete (format "deleting ~s" (at-site-tag to-delete))
                 "nothing to delete")
             (if to-fill (format "recolouring ~s" (at-site-tag to-fill))
                 "nothing to recolour"))
     (check-true
      (libreoffice-edit!
       pptx
       (append
        (list (list "move" slide (at-site-tag to-move) (mm100 40) (mm100 20))
              (list "retext" slide (at-site-tag texty) typed))
        (if to-delete (list (list "delete" slide (at-site-tag to-delete))) '())
        (if to-fill
            (list (list "fill" slide (at-site-tag to-fill)
                        (string->number poured 16)))
            '())
        (list (list "addtext" slide (mm100 100) (mm100 100)
                    (mm100 200) (mm100 50) added))))
      (format "~a: LibreOffice saved the deck it was given" name))
     (copy-file pptx (build-path dir "libreoffice-edited.pptx") #t)
     (define r (sync-once program pptx #:workdir w #:atomic? #t))
     (define kinds (applied-kinds r))
     (for ([sk (in-list (sync-report-skipped r))])
       (printf "     refused ~a ~s on slide ~a: ~a\n" (sync-action-kind (car sk))
               (sync-action-tag (car sk)) (sync-action-slide (car sk)) (cdr sk)))
     (printf "     applied: ~a\n" (string-join (map symbol->string kinds) ", "))
     (define text (file->string program))
     (for ([kind (in-list (append '(moved retext added)
                                  (if to-delete '(removed) '())
                                  (if to-fill '(restyle) '())))])
       (check-true (and (memq kind kinds) #t)
                   (format "~a: the ~a made in LibreOffice was applied" name kind)))
     (check-true (string-contains? text typed)
                 (format "~a: the text typed in LibreOffice is in the program" name))
     (check-true (string-contains? text added)
                 (format "~a: the text box added in LibreOffice is in the program" name))
     (when to-fill
       (check-true (string-contains? (string-downcase text) (string-downcase poured))
                   (format "~a: the colour poured in LibreOffice is in the program" name)))
     (when to-delete
       (check-false (string-contains? text (format "~s" (at-site-tag to-delete)))
                    (format "~a: the element deleted in LibreOffice is gone from the program"
                            name)))
     ;; And the program now draws what LibreOffice wrote. A merge that put an
     ;; edit in the wrong place still reports it as applied, so this is the
     ;; check that says it landed: read the same deck again and nothing about
     ;; the program's own elements may be left.
     ;;
     ;; The shape LibreOffice made itself is the exception, and has to be. It
     ;; carries no alt text of ours, so the deck cannot say it is the element
     ;; the merge has just written an `at` form for; it says so once the deck is
     ;; written again from the program, which is the next thing the watch loop
     ;; does.
     (define known (for*/list ([(i ss) (in-hash by-slide)] [st (in-list ss)])
                     (at-site-tag st)))
     (define again (sync-once program pptx #:workdir w #:atomic? #t))
     (define stale
       (for/list ([a (in-list (sync-report-actions again))]
                  #:when (member (sync-action-tag a) known))
         (format "~a ~s on slide ~a" (sync-action-kind a) (sync-action-tag a)
                 (sync-action-slide a))))
     (for ([a (in-list (sync-report-actions again))])
       (printf "     left over: ~a ~s on slide ~a\n" (sync-action-kind a)
               (sync-action-tag a) (sync-action-slide a)))
     (check-equal? stale '()
                   (format "~a: every edit landed -- nothing is reported twice" name))
     ;; Then the deck as the loop writes it, and a merge with nothing at all to
     ;; say: the program draws the deck it was given, the new shape included.
     (picts->pptx (load-program-picts program) pptx)
     (define settled (sync-once program pptx #:workdir w #:atomic? #t))
     (print-settle-actions settled)
     (check-equal? (length (actionable-actions settled)) 0
                   (format "~a: the deck written from the program has nothing to merge"
                           name))]))

;; Copying an element, which is the first thing anybody does with a shape they
;; like. The copy has to become an element of its own -- its own `at` form, its
;; own tag -- and the shape it was copied from has to be left alone. Two `at`
;; forms under one tag is a program no later sync can read, and a talk that
;; cannot copy a shape is a talk written the hard way.
;;
;; LibreOffice names the copy after the shape it numbered last, which in a deck
;; translated from PowerPoint is very often the name of another element already
;; on the slide. So this is also the test that says a copy is not whatever it
;; happens to be called.
(define (copied-in-libreoffice name program dir)
  (define pptx (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (picts->pptx (load-program-picts program) pptx)
  (void (sync-once program pptx #:workdir w))
  (define by-slide (slide-tags program))
  (define slide (and (pair? (hash-keys by-slide)) (first (sort (hash-keys by-slide) <))))
  (define to-copy (and slide (first (hash-ref by-slide slide))))
  (cond
    [(not to-copy) (printf "  ~a: nothing tagged to copy\n" name)]
    [else
     (printf "  ~a: slide ~a -- copying ~s\n" name slide (at-site-tag to-copy))
     (check-true
      (libreoffice-edit! pptx (list (list "copy" slide (at-site-tag to-copy))))
      (format "~a: LibreOffice saved the deck it was given" name))
     (define r (sync-once program pptx #:workdir w #:atomic? #t))
     (for ([sk (in-list (sync-report-skipped r))])
       (printf "     refused ~a ~s: ~a\n" (sync-action-kind (car sk))
               (sync-action-tag (car sk)) (cdr sk)))
     (printf "     applied: ~a\n"
             (string-join (map symbol->string (applied-kinds r)) ", "))
     (check-equal? (applied-kinds r) '(added)
                   (format "~a: the copy is one element added, and nothing else" name))
     ;; The tags stay distinct, which is what makes the next edit possible.
     (define after (slide-tags program))
     (define tags (for/list ([st (in-list (hash-ref after slide '()))]) (at-site-tag st)))
     (check-equal? (length tags) (length (remove-duplicates tags))
                   (format "~a: and every `at` form on that slide still has its own tag"
                           name))
     (check-equal? (length tags) (add1 (length (hash-ref by-slide slide)))
                   (format "~a: the slide has one more `at` form than it had" name))
     ;; And it settles: the deck written from the program holds both of them.
     (picts->pptx (load-program-picts program) pptx)
     (define settled (sync-once program pptx #:workdir w #:atomic? #t))
     (print-settle-actions settled)
     (check-equal? (length (actionable-actions settled)) 0
                   (format "~a: the deck written from the program has nothing to merge"
                           name))]))

;; A text box drawn on every slide, in one save. "Does it work on every slide?"
;; is a different question from "does it work": a slide the program builds in a
;; helper, a slide whose canvas is not a literal `slide_canvas` call, a slide
;; that is hidden -- each of those is a place a new box may have nowhere to go,
;; and one save that lands on fifteen slides and refuses the sixteenth says so
;; where fifteen separate saves would not.
(define (added-everywhere name program dir)
  (define pptx (build-path dir "deck.pptx"))
  (define w (build-path dir "w"))
  (define picts (load-program-picts program))
  (define slides (length picts))
  ;; In an epoch deck, exercise the last editable page of every logical slide.
  ;; That reaches every source owner, including the `from_stage` write-back
  ;; path, without asking for mutually inconsistent boxes on every cumulative
  ;; frame of the same build.
  (define origins (current-slide-origins))
  (define target-pages
    (if (getenv "GLIDE_LO_STAGES")
        (for/list ([origin (in-list origins)] [i (in-naturals 1)]
                   [next (in-list (append (cdr origins) '(#f)))]
                   #:when (not (equal? origin next)))
          i)
        (for/list ([i (in-range 1 (add1 slides))]) i)))
  (picts->pptx picts pptx)
  (void (sync-once program pptx #:workdir w))
  (define (typed i) (format "Drawn on slide ~a" i))
  (printf "  ~a: a text box on ~a source slides across ~a editor pages\n"
          name (length target-pages) slides)
  (check-true
   (libreoffice-edit!
    pptx
    (for/list ([i (in-list target-pages)])
      (list "addtext" i (mm100 60) (mm100 60) (mm100 220) (mm100 40) (typed i))))
   (format "~a: LibreOffice saved the deck it was given" name))
  ;; Keep the editor's actual rewrite: if matching fails, the generated deck
  ;; below intentionally replaces `pptx`, and losing the evidence makes a
  ;; LibreOffice-only failure need another several-minute run to inspect.
  (copy-file pptx (build-path dir "libreoffice-added.pptx") #t)
  (define r (sync-once program pptx #:workdir w #:atomic? #t))
  (define adds
    (for/list ([a (in-list (sync-report-applied r))]
               #:when (eq? 'added (sync-action-kind a)))
      (sync-action-slide a)))
  (for ([sk (in-list (sync-report-skipped r))])
    (printf "     refused ~a ~s on slide ~a: ~a\n" (sync-action-kind (car sk))
            (sync-action-tag (car sk)) (sync-action-slide (car sk)) (cdr sk)))
  ;; A hand-written talk can keep every canvas in an imported module. Check
  ;; the whole editable source tree, not just its running-order file.
  (define-values (_sites _scopes _slide-sites layout) (find-program-sites program))
  (define text
    (string-join
     (for/list ([source (in-list (program-layout-files layout))]
                #:when (file-exists? source))
       (file->string source))
     "\n"))
  (define missing
    (for/list ([i (in-list target-pages)]
               #:unless (string-contains? text (typed i)))
      i))
  ;; A slide a helper builds has no canvas of its own to add a form to, and says
  ;; so. That is the program's shape, not a failure -- so what is asked is that
  ;; every slide either takes the box or says why it cannot, and that the ones
  ;; that cannot do not take the rest of the save down with them.
  (define no-canvas
    (for/list ([sk (in-list (sync-report-skipped r))]
               #:when (regexp-match? #rx"slide-canvas" (cdr sk)))
      (sync-action-slide (car sk))))
  (printf "     ~a of ~a landed~a\n" (- (length target-pages) (length missing))
          (length target-pages)
          (if (null? missing)
              ""
              (format ", not on slide~a ~a~a" (if (= 1 (length missing)) "" "s")
                      (string-join (map number->string missing) ", ")
                      (if (null? no-canvas) ""
                          " -- no `slide_canvas` of their own"))))
  (check-equal? (sort missing <) (sort (remove-duplicates no-canvas) <)
                (format "~a: a box lands on every slide that has a canvas to hold it" name))
  (check-equal? (sort (remove-duplicates (append adds no-canvas)) <)
                target-pages
                (format "~a: and every slide either takes it or says why not" name))
  ;; And the deck written back from the program holds all of them.
  (picts->pptx (load-program-picts program) pptx)
  (define settled (sync-once program pptx #:workdir w #:atomic? #t))
  (print-settle-actions settled)
  (check-equal? (length (actionable-actions settled)) 0
                (format "~a: the deck written from the program has nothing to merge" name)))

(define fixtures
  (let ([only (getenv "GLIDE_LO_DECKS")])
    (for/list ([f (in-list (sort (map path->string (directory-list decks-dir)) string<?))]
               #:when (and (regexp-match? #rx"[.]pptx$" f)
                           (or (not only) (string-contains? f only))))
      (path->string (path-replace-extension f "")))))

(cond
  [(not soffice)
   (printf "no LibreOffice here; the editor workflow is not tested\n")]
  [else
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (install-macro!)
   (printf "editing in LibreOffice:\n")
   (for ([name (in-list fixtures)])
     (define dir (build-path work name))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                           #:workdir (build-path dir "u")))
     (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
     (edited-in-libreoffice name program dir))
   (printf "copying in LibreOffice:\n")
   (for ([name (in-list fixtures)])
     (define dir (build-path work (string-append name "-copy")))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                           #:workdir (build-path dir "u")))
     (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
     (copied-in-libreoffice name program dir))
   (printf "drawing a text box on every slide:\n")
   (for ([name (in-list fixtures)])
     (define dir (build-path work (string-append name "-every")))
     (make-directory* dir)
     (define program (build-path dir "p.rhm"))
     (define d (pptx->deck (build-path decks-dir (string-append name ".pptx"))
                           #:workdir (build-path dir "u")))
     (write-rhombus-deck d program #:source-name (string-append name ".pptx"))
     (added-everywhere name program dir))
   ;; And a talk of one's own, which is where the slides are built by hand and
   ;; the answer is not obvious. `GLIDE_LO_PROGRAM` names it.
   (let ([mine (getenv "GLIDE_LO_PROGRAM")])
     (when mine
       (when (getenv "GLIDE_LO_STAGES") (set-stage-slides! #t))
       (define from (path-only (path->complete-path mine)))
       (define dir (build-path work "local"))
       (make-directory* dir)
       ;; Everything beside it, except the scratch: `.glide` holds a base
       ;; written for the program where it came from, and the merge refuses one
       ;; of those rather than merging somebody else's agreed state -- rightly.
       (for ([f (in-list (directory-list from))]
             #:unless (equal? (path->string f) ".glide"))
         (define src (build-path from f))
         (if (directory-exists? src)
             (copy-directory/files src (build-path dir f) #:keep-modify-seconds? #t)
             (copy-file src (build-path dir f) #t)))
       (define program (build-path dir (file-name-from-path mine)))
       (printf "your own program:\n")
       (added-everywhere (path->string (file-name-from-path mine)) program dir)
       ;; Useful when validating a large real talk: the all-slides addition is
       ;; independently valuable and need not pay for another export/edit pass.
       (unless (equal? (getenv "GLIDE_LO_PROGRAM_MODE") "add")
         (edited-in-libreoffice (path->string (file-name-from-path mine)) program dir))))])

(module+ main (void (test-log #:display? #t #:exit? #t)))
