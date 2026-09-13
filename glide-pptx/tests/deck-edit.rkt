#lang racket/base
;; Editing a .pptx the way an editor would, for tests.
;;
;; Racket's own zip and unzip are used rather than the command-line tools, so a
;; test that simulates a drag needs nothing installed beyond Racket.
(require racket/list racket/string racket/file racket/path racket/format
         file/unzip file/zip)
(provide with-unpacked-deck drag-in-deck! deck-part
         add-shape-to-deck! delete-from-deck! nudge-family-in-deck! paste-slide!
         retext-in-deck! retype-run-in-deck! delete-slide! move-slide!
         resize-in-deck! rotate-in-deck! edit-after-tag! shape-rx find-tag
         bring-to-front! duplicate-in-deck!
         group-in-deck! ungroup-in-deck! move-element-to-slide! edit-slide-part!
         edit-part!)

;; Unpacks `pptx`, calls `proc` with the directory, and repacks whatever is
;; there back over the original.
(define (with-unpacked-deck pptx proc)
  (define dir (make-temporary-file "deck~a" 'directory))
  (call-with-input-file pptx
    (lambda (in) (parameterize ([current-directory dir])
                   (unzip in (make-filesystem-entry-reader #:exists 'replace)))))
  (define result (proc dir))
  (when (file-exists? pptx) (delete-file pptx))
  (parameterize ([current-directory dir])
    (apply zip (path->complete-path pptx) (directory-list)))
  (delete-directory/files dir #:must-exist? #f)
  result)

(define (deck-part pptx name)
  (define dir (make-temporary-file "part~a" 'directory))
  (call-with-input-file pptx
    (lambda (in) (parameterize ([current-directory dir])
                   (unzip in (make-filesystem-entry-reader)))))
  (define text (file->string (build-path dir (string->path name))))
  (delete-directory/files dir #:must-exist? #f)
  text)

;; Moves the shape tagged `tag` on slide `slide` to (x, y) in points, which is
;; what dragging it in PowerPoint or Keynote amounts to. Returns #t when the
;; shape was found.
(define (drag-in-deck! pptx slide tag x y)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     ;; The tag is in the alt text, and the offset that follows it is the
     ;; element's own -- which holds for a group too. Matching the whole element
     ;; instead does not: a group contains shapes, so a closing tag inside it
     ;; ends the match early.
     (define at (find-tag d tag))
     (cond
       [(not at) #f]
       [else
        (define m (regexp-match-positions #px"<a:off x=\"-?\\d+\" y=\"-?\\d+\"/>" d at))
        (cond
          [(not m) #f]
          [else
           (define-values (s e) (values (caar m) (cdar m)))
           (call-with-output-file part #:exists 'replace
             (lambda (o)
               (write-string
                (string-append (substring d 0 s)
                               (format "<a:off x=\"~a\" y=\"~a\"/>"
                                       (inexact->exact (round (* 12700 x)))
                                       (inexact->exact (round (* 12700 y))))
                               (substring d e))
                o)))
           #t])]))))

;; Where the element named `tag` is named, so what follows is that element's. The
;; alt text is how a deck we wrote names things, and the shape name is how an
;; editor's own new shape does -- the importer reads either, so this does too.
(define (find-tag d tag)
  (define m (or (regexp-match-positions
                 (pregexp (format "descr=\"glide-pptx:~a\"" (regexp-quote tag))) d)
                (regexp-match-positions
                 (pregexp (format "name=\"~a\"" (regexp-quote tag))) d)
                ;; Tests often address an automatically tagged object by the
                ;; readable suffix a user recognizes. LibreOffice-preserving
                ;; exports carry the full generated identity in `name` too.
                (regexp-match-positions
                 (pregexp
                  (format "(?:name=\"|descr=\"glide-pptx:)source:[0-9a-f]{40}:~a\""
                          (regexp-quote tag)))
                 d)))
  (and m (cdar m)))

;; The regexp that finds one tagged shape, which is how an editor's edits are
;; located in a part: the tag is in the alt text and the shape is the element
;; that contains it.
;; A group is draggable too, so `grpSp` belongs here with `sp` and `pic`.
;; The element itself, and not the group it is in: a shape's name sits in its
;; header, before any child, so a match that has crossed another opening tag on
;; the way to the name started too far out. Without that the first member of a
;; group matches from `<p:grpSp>` to `</p:sp>`, and moving that chunk cuts the
;; group in half. A group that holds shapes still does not match -- the closing
;; side cannot count either -- and callers that need one say so themselves.
(define (shape-rx tag)
  (pregexp (format (string-append
                    "(?s:<p:(?:sp|pic|grpSp)>"
                    "(?:(?!<p:(?:sp|pic|grpSp)>)(?!</p:(?:sp|pic|grpSp)>).)*?"
                    "(?:descr=\"glide-pptx:~a\"|name=\"~a\"|"
                    "(?:descr=\"glide-pptx:|name=\")source:[0-9a-f]{40}:~a\")"
                    "(?:(?!</p:(?:sp|pic|grpSp)>).)*?</p:(?:sp|pic|grpSp)>)")
                   (regexp-quote tag) (regexp-quote tag) (regexp-quote tag))))

;; The element that holds a tag, counting its own openers and closers, so a
;; group comes back whole rather than stopping at its first child's closing
;; tag. `shape-rx` cannot do that -- a regexp cannot count -- and a group is
;; something an editor deletes and restacks like anything else.
;; Returns (cons start end) over `d`, or #f.
(define (shape-span d tag)
  (define at (find-tag d tag))
  (and at
       (let loop ([i 0] [open '()] [found #f])
         (define m (regexp-match-positions
                    #px"<p:(?:sp|pic|grpSp)(?:\\s[^>]*)?>|</p:(?:sp|pic|grpSp)>" d i))
         (cond
           [(or (not m) found) found]
           [else
            (define-values (s e) (values (caar m) (cdar m)))
            (cond
              [(char=? #\/ (string-ref d (add1 s))) ; a closer: the innermost open one ends
               (define start (if (pair? open) (car open) #f))
               (if (and start (<= start at) (<= at e))
                   (loop e (if (pair? open) (cdr open) '()) (cons start e))
                   (loop e (if (pair? open) (cdr open) '()) #f))]
              [else (loop e (cons s open) #f)])]))))

;; A raw edit to any part of the package, for what no element carries.
(define (edit-part! pptx name rx to)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir (string->path name)))
     (and (file-exists? part)
          (let ([t (file->string part)])
            (and (regexp-match? rx t)
                 (begin (display-to-file (regexp-replace rx t to) part #:exists 'replace)
                        #t)))))))

;; A raw edit to one slide part, for what no element carries. `all?` replaces
;; every match rather than the first, which is how an editor that states less
;; than we do is imitated.
(define (edit-slide-part! pptx slide rx to #:all? [all? #f])
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define t (file->string part))
     (and (regexp-match? rx t)
          (begin (display-to-file ((if all? regexp-replace* regexp-replace) rx t to)
                                  part #:exists 'replace)
                 #t)))))

;; Groups two shapes, which is what the editor's Group command does: the two
;; forms move inside a `<p:grpSp>` whose box covers them both, and whose child
;; offsets are the same as its own, so nothing is scaled.
(define (group-in-deck! pptx slide tag-a tag-b #:name [name "Group 1"])
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define ma (regexp-match (shape-rx tag-a) d))
     (define mb (regexp-match (shape-rx tag-b) d))
     (define (box sp)
       (define m (regexp-match #px"<a:off x=\"(-?[0-9]+)\" y=\"(-?[0-9]+)\"/><a:ext cx=\"(-?[0-9]+)\" cy=\"(-?[0-9]+)\"/>" sp))
       (and m (map string->number (cdr m))))
     (define ba (and ma (box (first ma))))
     (define bb (and mb (box (first mb))))
     (cond
       [(not (and ba bb)) #f]
       [else
        (define x (min (first ba) (first bb)))
        (define y (min (second ba) (second bb)))
        (define cx (- (max (+ (first ba) (third ba)) (+ (first bb) (third bb))) x))
        (define cy (- (max (+ (second ba) (fourth ba)) (+ (second bb) (fourth bb))) y))
        (define grp
          (format (string-append
                   "<p:grpSp><p:nvGrpSpPr><p:cNvPr id=\"900\" name=\"~a\" descr=\"glide-pptx:~a\"/>"
                   "<p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr><p:grpSpPr><a:xfrm>"
                   "<a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/>"
                   "<a:chOff x=\"~a\" y=\"~a\"/><a:chExt cx=\"~a\" cy=\"~a\"/>"
                   "</a:xfrm></p:grpSpPr>~a~a</p:grpSp>")
                  name name x y cx cy x y cx cy (first ma) (first mb)))
        (define stripped (string-replace (string-replace d (first ma) "") (first mb) ""))
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-replace stripped "</p:spTree>"
                                                    (string-append grp "</p:spTree>"))
                                    o)))
        #t]))))

;; Dissolves a group, which is what the editor's Ungroup does: the children
;; come back out at the level the group was at. The child coordinate space is
;; the same as the group's own, so nothing needs rescaling.
(define (ungroup-in-deck! pptx slide tag)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     ;; `shape-rx` cannot match a group that holds shapes -- it stops at the
     ;; first closing tag -- so the group is found by counting its own.
     (define at (find-tag d tag))
     (define open (and at (let loop ([j at])
                            (cond [(< j 0) #f]
                                  [(regexp-match? #px"^<p:grpSp>" d j) j]
                                  [else (loop (sub1 j))]))))
     (define shut
       (and open
            (let loop ([j (+ open (string-length "<p:grpSp>"))] [depth 1])
              (cond
                [(>= j (string-length d)) #f]
                [(zero? depth) j]
                [(regexp-match? #px"^<p:grpSp>" d j) (loop (add1 j) (add1 depth))]
                [(regexp-match? #px"^</p:grpSp>" d j)
                 (if (= 1 depth)
                     (+ j (string-length "</p:grpSp>"))
                     (loop (add1 j) (sub1 depth)))]
                [else (loop (add1 j) depth)]))))
     (define whole (and shut (substring d open shut)))
     (define inner
       (and whole (regexp-match #px"(?s:</p:grpSpPr>(.*)</p:grpSp>$)" whole)))
     (cond
       [(not inner) #f]
       [else
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-append (substring d 0 open) (second inner)
                                                   (substring d shut))
                                    o)))
        #t]))))

;; Cuts a shape from one slide and pastes it on another, last in the tree.
(define (move-element-to-slide! pptx tag from to)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define src (build-path dir "ppt" "slides" (format "slide~a.xml" from)))
     (define dst (build-path dir "ppt" "slides" (format "slide~a.xml" to)))
     (define d (file->string src))
     (define m (regexp-match (shape-rx tag) d))
     (cond
       [(not m) #f]
       [else
        (call-with-output-file src #:exists 'replace
          (lambda (o) (write-string (string-replace d (first m) "") o)))
        (define t (file->string dst))
        (call-with-output-file dst #:exists 'replace
          (lambda (o) (write-string (string-replace t "</p:spTree>"
                                                    (string-append (first m) "</p:spTree>"))
                                    o)))
        #t]))))

;; Adds a rectangle to slide `slide`, last in the tree, which is what drawing
;; one in PowerPoint or Keynote amounts to. Returns the name it was given.
;; `after` puts it just over the shape with that tag rather than over
;; everything, which is what drawing a shape and sending it backward amounts
;; to; `under?` puts it under the lot.
(define (add-shape-to-deck! pptx slide name
                            #:x [x 100.0] #:y [y 100.0]
                            #:width [w 120.0] #:height [h 80.0]
                            #:fill [fill "FF0000"]
                            #:after [after #f]
                            #:under? [under? #f])
  (define (emu v) (inexact->exact (round (* 12700 v))))
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define sp
       (format (string-append
                "<p:sp><p:nvSpPr><p:cNvPr id=\"9001\" name=\"~a\"/>"
                "<p:cNvSpPr/><p:nvPr/></p:nvSpPr>"
                "<p:spPr><a:xfrm><a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/></a:xfrm>"
                "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>"
                "<a:solidFill><a:srgbClr val=\"~a\"/></a:solidFill></p:spPr>"
                "<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody></p:sp>")
               name (emu x) (emu y) (emu w) (emu h) fill))
     (define placed
       (cond
         [after
          (define m (regexp-match (shape-rx after) d))
          (and m (string-replace d (first m) (string-append (first m) sp)))]
         [under?
          (define m (regexp-match #px"(?s:</p:grpSpPr>)" d))
          (and m (string-replace d (first m) (string-append (first m) sp)))]
         [else (string-replace d "</p:spTree>" (string-append sp "</p:spTree>"))]))
     (cond
       [(not placed) #f]
       [else
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string placed o)))
        name]))))

;; Deletes the shape tagged `tag` from slide `slide`. Returns #t when it was
;; there to delete.
(define (delete-from-deck! pptx slide tag)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define sp (shape-span d tag))
     (cond
       [(not sp) #f]
       [else
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-append (substring d 0 (car sp))
                                                   (substring d (cdr sp)))
                                    o)))
        #t]))))

;; Shifts every shape tagged `tag` on slide `slide` by (dx, dy) points, which is
;; what selecting a row of them and dragging amounts to. Returns how many moved.
(define (nudge-family-in-deck! pptx slide tag dx dy)
  (define (emu v) (inexact->exact (round (* 12700 v))))
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define moved 0)
     (define out
       (regexp-replace*
        (shape-rx tag) d
        (lambda (whole . _)
          (set! moved (add1 moved))
          (regexp-replace
           #px"<a:off x=\"(-?\\d+)\" y=\"(-?\\d+)\"/>" whole
           (lambda (_m x y)
             (format "<a:off x=\"~a\" y=\"~a\"/>"
                     (+ (string->number x) (emu dx))
                     (+ (string->number y) (emu dy))))))))
     (call-with-output-file part #:exists 'replace
       (lambda (o) (write-string out o)))
     moved)))

;; Copies slide `from` of `src` into `pptx` at the end, the way pasting a slide
;; in from another deck does: a new slide part, its relationships, a content-type
;; override, and an entry in the slide id list.
(define (paste-slide! pptx src from)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define n
       (add1 (apply max 0 (for/list ([p (in-list (directory-list (build-path dir "ppt" "slides")))]
                                     #:when (regexp-match #rx"^slide([0-9]+)[.]xml$"
                                                          (path->string p)))
                            (string->number
                             (cadr (regexp-match #rx"^slide([0-9]+)[.]xml$"
                                                 (path->string p))))))))
     (define name (format "slide~a.xml" n))
     ;; The slide's own XML and its relationships, taken from the other deck.
     (define tmp (make-temporary-file "paste~a" 'directory))
     (call-with-input-file src
       (lambda (in) (parameterize ([current-directory tmp])
                      (unzip in (make-filesystem-entry-reader)))))
     (copy-file (build-path tmp "ppt" "slides" (format "slide~a.xml" from))
                (build-path dir "ppt" "slides" name) #t)
     ;; The pasted slide names a layout from the deck it came from, which the
     ;; target does not have. PowerPoint either brings the layout along or
     ;; remaps to one already there; remapping is enough here.
     (make-directory* (build-path dir "ppt" "slides" "_rels"))
     (define layout
       (let ([ls (sort (for/list ([p (in-list (directory-list
                                               (build-path dir "ppt" "slideLayouts")))]
                                  #:when (regexp-match? #rx"[.]xml$" (path->string p)))
                         (path->string p))
                       string<?)])
         (if (pair? ls) (first ls) "slideLayout1.xml")))
     (display-to-file
      (format (string-append
               "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
               "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/"
               "relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas."
               "openxmlformats.org/officeDocument/2006/relationships/slideLayout\""
               " Target=\"../slideLayouts/~a\"/></Relationships>")
              layout)
      (build-path dir "ppt" "slides" "_rels" (format "~a.rels" name))
      #:exists 'replace)
     (delete-directory/files tmp #:must-exist? #f)

     ;; A relationship from the presentation, and an id list entry that uses it.
     (define prels (build-path dir "ppt" "_rels" "presentation.xml.rels"))
     (define prels-text (file->string prels))
     (define rid (format "rId~a" (+ 900 n)))
     (display-to-file
      (string-replace prels-text "</Relationships>"
                      (format (string-append
                               "<Relationship Id=\"~a\" Type=\"http://schemas.openxmlformats.org"
                               "/officeDocument/2006/relationships/slide\" Target=\"slides/~a\"/>"
                               "</Relationships>")
                              rid name))
      prels #:exists 'replace)

     (define pres (build-path dir "ppt" "presentation.xml"))
     (define pres-text (file->string pres))
     (display-to-file
      (string-replace pres-text "</p:sldIdLst>"
                      (format "<p:sldId id=\"~a\" r:id=\"~a\"/></p:sldIdLst>" (+ 500 n) rid))
      pres #:exists 'replace)

     (define ct (build-path dir "[Content_Types].xml"))
     (define ct-text (file->string ct))
     (display-to-file
      (string-replace ct-text "</Types>"
                      (format (string-append
                               "<Override PartName=\"/ppt/slides/~a\" ContentType="
                               "\"application/vnd.openxmlformats-officedocument"
                               ".presentationml.slide+xml\"/></Types>")
                              name))
      ct #:exists 'replace)
     n)))

;; Replaces the text of the shape tagged `tag` on slide `slide`, which is what
;; retyping it in the editor amounts to. Returns #t when it was found.
(define (retext-in-deck! pptx slide tag text)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define m (regexp-match (shape-rx tag) d))
     (cond
       [(not m) #f]
       [else
        (define once (box #f))
        (define retexted
          (regexp-replace* #px"<a:t>[^<]*</a:t>" (first m)
                           (lambda (_w)
                             (cond [(unbox once) ""]
                                   [else (set-box! once #t)
                                         (format "<a:t>~a</a:t>" text)]))))
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-replace d (first m) retexted) o)))
        #t]))))

;; Retypes one run of the shape tagged `tag`, leaving its other runs as they
;; were -- which is what changing a word in the editor amounts to. `retext-in-deck!`
;; puts the whole body in the first run and empties the rest, which is a
;; retyping that crosses runs and cannot be traced back to one of them.
;; `which` counts from 1. Returns #t when there was a run that far in.
(define (retype-run-in-deck! pptx slide tag which text)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define m (regexp-match (shape-rx tag) d))
     (cond
       [(not m) #f]
       [else
        (define seen (box 0))
        (define hit (box #f))
        (define retyped
          (regexp-replace* #px"<a:t>[^<]*</a:t>" (first m)
                           (lambda (whole)
                             (set-box! seen (add1 (unbox seen)))
                             (cond
                               [(= which (unbox seen))
                                (set-box! hit #t)
                                (format "<a:t>~a</a:t>" text)]
                               [else whole]))))
        (cond
          [(not (unbox hit)) #f]
          [else
           (call-with-output-file part #:exists 'replace
             (lambda (o) (write-string (string-replace d (first m) retyped) o)))
           #t])]))))

;; Removes slide `n` from the deck's slide list, which is what deleting it in the
;; editor amounts to. The part is left in the package, unreferenced, the way an
;; editor often leaves it. Returns #t when there was one to remove.
(define (delete-slide! pptx n)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define pres (build-path dir "ppt" "presentation.xml"))
     (define t (file->string pres))
     (define ids (regexp-match* #px"<p:sldId [^>]*/>" t))
     (cond
       [(or (< n 1) (> n (length ids))) #f]
       [else
        (display-to-file (string-replace t (list-ref ids (sub1 n)) "" #:all? #f)
                         pres #:exists 'replace)
        #t]))))

;; Moves slide `from` to position `to` in the slide list, one-based, which is
;; what dragging it in the slide navigator amounts to.
(define (move-slide! pptx from to)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define pres (build-path dir "ppt" "presentation.xml"))
     (define t (file->string pres))
     (define ids (regexp-match* #px"<p:sldId [^>]*/>" t))
     (cond
       [(or (< from 1) (> from (length ids)) (< to 1) (> to (length ids))) #f]
       [else
        (define moved (list-ref ids (sub1 from)))
        (define without (remove moved ids))
        (define reordered
          (append (take without (sub1 to)) (list moved) (drop without (sub1 to))))
        (define lst (regexp-match #px"(?s:<p:sldIdLst>.*?</p:sldIdLst>)" t))
        (display-to-file
         (string-replace t (first lst)
                         (string-append "<p:sldIdLst>" (apply string-append reordered)
                                        "</p:sldIdLst>"))
         pres #:exists 'replace)
        #t]))))

;; Resizes the element tagged `tag` on slide `slide`, which is what dragging a
;; handle amounts to. Returns #t when it was there.
(define (resize-in-deck! pptx slide tag w h)
  (edit-after-tag! pptx slide tag #px"<a:ext cx=\"-?\\d+\" cy=\"-?\\d+\"/>"
                   (format "<a:ext cx=\"~a\" cy=\"~a\"/>"
                           (inexact->exact (round (* 12700 w)))
                           (inexact->exact (round (* 12700 h))))))

;; Rotates it, the way the rotation handle does. `deg` is degrees clockwise.
(define (rotate-in-deck! pptx slide tag deg)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define at (find-tag d tag))
     (cond
       [(not at) #f]
       [else
        ;; The rotation is an attribute of the transform, so it is added to the
        ;; <a:xfrm> that follows the tag rather than replacing anything.
        (define m (regexp-match-positions #px"<a:xfrm( rot=\"-?\\d+\")?" d at))
        (cond
          [(not m) #f]
          [else
           (call-with-output-file part #:exists 'replace
             (lambda (o)
               (write-string
                (string-append (substring d 0 (caar m))
                               (format "<a:xfrm rot=\"~a\""
                                       (inexact->exact (round (* 60000 deg))))
                               (substring d (cdar m)))
                o)))
           #t])]))))

;; Replaces the first thing matching `rx` after the element's alt text, which is
;; where that element's own properties begin.
(define (edit-after-tag! pptx slide tag rx replacement)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define at (find-tag d tag))
     (cond
       [(not at) #f]
       [else
        (define m (regexp-match-positions rx d at))
        (cond
          [(not m) #f]
          [else
           (call-with-output-file part #:exists 'replace
             (lambda (o)
               (write-string (string-append (substring d 0 (caar m))
                                            replacement
                                            (substring d (cdar m)))
                             o)))
           #t])]))))

;; Moves the shape tagged `tag` to the end of the tree, which is what bringing
;; it to the front does.
(define (bring-to-front! pptx slide tag)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define sp (shape-span d tag))
     (cond
       [(not sp) #f]
       [else
        (define moved (substring d (car sp) (cdr sp)))
        (define without (string-append (substring d 0 (car sp)) (substring d (cdr sp))))
        (display-to-file
         (string-replace without "</p:spTree>" (string-append moved "</p:spTree>"))
         part #:exists 'replace)
        #t]))))

;; Copies it, which is what duplicating does -- and the copy carries the same
;; name, which is the interesting part.
(define (duplicate-in-deck! pptx slide tag)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define sp (shape-span d tag))
     (cond
       [(not sp) #f]
       [else
        (define one (substring d (car sp) (cdr sp)))
        (display-to-file (string-append (substring d 0 (car sp)) one one (substring d (cdr sp)))
                         part #:exists 'replace)
        #t]))))
