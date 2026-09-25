#lang racket/base
;; What the decks use that we never look at.
;;
;; This exists because of arrowheads. A connector's `<a:tailEnd>` was never
;; parsed, so it was absent from the IR -- and a round trip compares what we
;; parsed against what we wrote, which makes an unparsed feature invisible on
;; both sides. The pixel diff against LibreOffice did draw them, but ten
;; arrowheads are two hundredths of a percent of a slide, well inside the
;; residual that text metrics already explain. Nothing said a word.
;;
;; So the gap is named here instead: every element the corpus uses is either one
;; the parser asks for, or one listed below as knowingly skipped. A new one is a
;; failure, which is a prompt to decide -- implement it, or write down that it is
;; not worth implementing and why.
(require rackunit/log)
(require rackunit racket/list racket/string racket/set racket/file racket/path
         racket/format racket/runtime-path file/unzip)

(define-runtime-path corpus-dir "corpus")
(define-runtime-path decks-dir "decks")
(define-runtime-path parser-dir "..")

;; Every `(child ... 'name)`, `(xpath ... 'name)` and `(attr ... 'name)` the
;; parser mentions: what it knows how to look for.
(define (names-the-parser-reads)
  (define acc (mutable-set))
  (for ([f (in-list '("shapes.rkt" "text.rkt" "drawing.rkt" "theme.rkt"
                      "parse.rkt" "geometry.rkt" "opc.rkt" "xml-util.rkt"))])
    (for ([line (in-list (string-split (file->string (build-path parser-dir f)) "\n"))])
      ;; A line that looks something up in the XML: every name quoted on it is a
      ;; name we ask for. `(xpath s 'txBody 'bodyPr)` names two.
      (when (regexp-match? #px"child|xpath|attr|elem-name|find-descendant" line)
        (for ([m (in-list (regexp-match* #px"'([A-Za-z0-9_]+)" line #:match-select cadr))])
          (set-add! acc (string->symbol m))))
      ;; `(case (elem-name node) [(sp pic graphicFrame) ...])` names shapes.
      (for ([m (in-list (regexp-match* #px"\\[\\(([a-zA-Z0-9_ ]+)\\)" line #:match-select cadr))])
        (for ([w (in-list (string-split m))]) (set-add! acc (string->symbol w))))))
  acc)

;; Elements we have looked at and decided to leave. Say why, so the list is a
;; record of decisions rather than a place things go to be forgotten.
(define knowingly-skipped
  (hash
   'effectLst      "shadows and glows are not drawn"
   'outerShdw      "shadows are not drawn"
   'innerShdw      "shadows are not drawn"
   'glow           "glows are not drawn"
   'softEdge       "soft edges are not drawn"
   'reflection     "reflections are not drawn"
   'scene3d        "3-D is not drawn"
   'sp3d           "3-D is not drawn"
   'bevel          "3-D bevels are not drawn"
   'camera         "3-D is not drawn"
   'lightRig       "3-D is not drawn"
   'extLst         "vendor extensions are not read"
   'ext            "vendor extensions are not read"
   'timing         "animations are not imported yet"
   'transition     "slide transitions are not imported"
   'zoom                   "a slide transition's effect, and transitions are not imported"
   'wipe                   "a slide transition's effect, and transitions are not imported"
   'wheel                  "a slide transition's effect, and transitions are not imported"
   'strips                 "a slide transition's effect, and transitions are not imported"
   'randomBar              "a slide transition's effect, and transitions are not imported"
   'blinds                 "a slide transition's effect, and transitions are not imported"
   'push                   "a slide transition's effect, and transitions are not imported"
   'plus                   "a slide transition's effect, and transitions are not imported"
   'dissolve               "a slide transition's effect, and transitions are not imported"
   'diamond                "a slide transition's effect, and transitions are not imported"
   'newsflash              "a slide transition's effect, and transitions are not imported"
   'comb                   "a slide transition's effect, and transitions are not imported"
   'cover                  "a slide transition's effect, and transitions are not imported"
   'cut                    "a slide transition's effect, and transitions are not imported"
   'fade                   "a slide transition's effect, and transitions are not imported"
   'pull                   "a slide transition's effect, and transitions are not imported"
   'split                  "a slide transition's effect, and transitions are not imported"
   'circle                 "a slide transition's effect, and transitions are not imported"
   'checker                "a slide transition's effect, and transitions are not imported"
   'barn                   "a slide transition's effect, and transitions are not imported"
   'zoomIn                 "a slide transition's effect, and transitions are not imported"
   'hlinkMouseOver         "a hyperlink, which is not followed"
   'custDataLst    "custom data is not ours to read"
   'hlinkClick     "hyperlinks are not followed"
   'hlinkHover     "hyperlinks are not followed"
   'cxnLst         "connection sites do not affect drawing"
   'ahLst          "adjust handles do not affect drawing"
   'gdLst          "shape guides are read through the presets instead"
   'cNvCxnSpPr             "a container this walks past on the way to what is inside it"
   'cNvGraphicFramePr      "a container this walks past on the way to what is inside it"
   'cNvGrpSpPr             "a container this walks past on the way to what is inside it"
   'cNvPicPr               "a container this walks past on the way to what is inside it"
   'cNvSpPr                "a container this walks past on the way to what is inside it"
   'control                "a container this walks past on the way to what is inside it"
   'controls               "a container this walks past on the way to what is inside it"
   'dgm                    "a container this walks past on the way to what is inside it"
   'graphic                "a container this walks past on the way to what is inside it"
   'graphicData            "a container this walks past on the way to what is inside it"
   'graphicEl              "a container this walks past on the way to what is inside it"
   'nvPr                   "a container this walks past on the way to what is inside it"
   'oleObj                 "a container this walks past on the way to what is inside it"
   'sld                    "a container this walks past on the way to what is inside it"
   'tags                   "a container this walks past on the way to what is inside it"
   'txEl                   "a container this walks past on the way to what is inside it"
   'cxnSpLocks             "an editing lock, which says nothing about how the slide looks"
   'graphicFrameLocks      "an editing lock, which says nothing about how the slide looks"
   'grpSpLocks             "an editing lock, which says nothing about how the slide looks"
   'picLocks               "an editing lock, which says nothing about how the slide looks"
   'spLocks                "an editing lock, which says nothing about how the slide looks"
   'anim                   "animation and timing, not imported yet"
   'animClr                "animation and timing, not imported yet"
   'animEffect             "animation and timing, not imported yet"
   'animMotion             "animation and timing, not imported yet"
   'animRot                "animation and timing, not imported yet"
   'animScale              "animation and timing, not imported yet"
   'attrName               "animation and timing, not imported yet"
   'attrNameLst            "animation and timing, not imported yet"
   'bldAsOne               "animation and timing, not imported yet"
   'bldDgm                 "animation and timing, not imported yet"
   'bldGraphic             "animation and timing, not imported yet"
   'bldLst                 "animation and timing, not imported yet"
   'bldP                   "animation and timing, not imported yet"
   'bldSub                 "animation and timing, not imported yet"
   'by                     "animation and timing, not imported yet"
   'cBhvr                  "animation and timing, not imported yet"
   'cMediaNode             "animation and timing, not imported yet"
   'cTn                    "animation and timing, not imported yet"
   'childTnLst             "animation and timing, not imported yet"
   'cmd                    "animation and timing, not imported yet"
   'cond                   "animation and timing, not imported yet"
   'endCondLst             "animation and timing, not imported yet"
   'endSync                "animation and timing, not imported yet"
   'fltVal                 "animation and timing, not imported yet"
   'from                   "animation and timing, not imported yet"
   'iterate                "animation and timing, not imported yet"
   'nextCondLst            "animation and timing, not imported yet"
   'par                    "animation and timing, not imported yet"
   'prevCondLst            "animation and timing, not imported yet"
   'rtn                    "animation and timing, not imported yet"
   'seq                    "animation and timing, not imported yet"
   'set                    "animation and timing, not imported yet"
   'sldTgt                 "animation and timing, not imported yet"
   'spTgt                  "animation and timing, not imported yet"
   'split                  "animation and timing, not imported yet"
   'stCondLst              "animation and timing, not imported yet"
   'strVal                 "animation and timing, not imported yet"
   'subTnLst               "animation and timing, not imported yet"
   'tav                    "animation and timing, not imported yet"
   'tavLst                 "animation and timing, not imported yet"
   'tgtEl                  "animation and timing, not imported yet"
   'tmAbs                  "animation and timing, not imported yet"
   'tmPct                  "animation and timing, not imported yet"
   'tn                     "animation and timing, not imported yet"
   'tnLst                  "animation and timing, not imported yet"
   'to                     "animation and timing, not imported yet"
   'audio                  "sound and video, not imported"
   'audioFile              "sound and video, not imported"
   'snd                    "sound and video, not imported"
   'sndAc                  "sound and video, not imported"
   'sndTgt                 "sound and video, not imported"
   'stSnd                  "sound and video, not imported"
   'video                  "sound and video, not imported"
   'videoFile              "sound and video, not imported"
   'ahPolar                "a shape's connection points and adjust handles, which do not affect drawing"
   'ahXY                   "a shape's connection points and adjust handles, which do not affect drawing"
   'cxn                    "a shape's connection points and adjust handles, which do not affect drawing"
   'endCxn                 "a shape's connection points and adjust handles, which do not affect drawing"
   'rCtr                   "a shape's connection points and adjust handles, which do not affect drawing"
   'rect                   "a shape's connection points and adjust handles, which do not affect drawing"
   'stCxn                  "a shape's connection points and adjust handles, which do not affect drawing"
   'bevelB                 "a line join, which is always drawn mitred"
   'bevelT                 "a line join, which is always drawn mitred"
   'miter                  "a line join, which is always drawn mitred"
   'round                  "a line join, which is always drawn mitred"
   'buBlip                 "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'buClrTx                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'buFontTx               "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'buSzPts                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'buSzTx                 "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'cs                     "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'defPPr                 "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'ea                     "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'fld                    "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'highlight              "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl1pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl2pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl3pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl4pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl5pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl6pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl7pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl8pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'lvl9pPr                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'noAutofit              "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'pRg                    "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'prstTxWarp             "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'sym                    "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'tabLst                 "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'uFill                  "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'uFillTx                "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'uLnTx                  "a text feature not implemented: the symbol font, list levels past the first, underline and highlight styling, fields and tabs"
   'biLevel                "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'clrChange              "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'clrFrom                "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'clrTo                  "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'contourClr             "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'duotone                "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'extrusionClr           "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'fade                   "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'fillRect               "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'fillToRect             "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'grayscl                "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'hueOff                 "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'prstShdw               "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'stretch                "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'tile                   "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'tileRect               "a fill or effect variant not implemented: image tiling and recolouring, gradient rectangles, preset shadows"
   'lnB                    "a table property read through the cells instead"
   'lnBlToTr               "a table property read through the cells instead"
   'lnL                    "a table property read through the cells instead"
   'lnR                    "a table property read through the cells instead"
   'lnT                    "a table property read through the cells instead"
   'lnTlToBr               "a table property read through the cells instead"
   'tableStyleId           "a table property read through the cells instead"
   'tblPr                  "a table property read through the cells instead"
   'effectRef              "a theme reference resolved another way: effects are not drawn, and the colour map is read from the master"
   'masterClrMapping       "a theme reference resolved another way: effects are not drawn, and the colour map is read from the master"
   'link                   "a hyperlink, which is not followed"))

(define (elements-used-in dir)
  (define acc (make-hash))
  (define files
    (if (directory-exists? dir)
        (filter (lambda (p)
                  (define n (path->string p))
                  (and (regexp-match? #rx"[.]pptx$" n)
                       ;; A fuzzer's findings, kept as regression tests. Their
                       ;; tag names are corrupted by construction -- `eSld` for
                       ;; `cSld`, one flipped byte -- and inventing an entry for
                       ;; each would make this list nonsense.
                       (not (regexp-match? #rx"clusterfuzz" n))))
                (directory-list dir #:build? #t))
        '()))
  (for ([f (in-list files)])
    (with-handlers ([exn:fail? (lambda (_e) (void))])
      (define tmp (make-temporary-file "cov~a" 'directory))
      (call-with-input-file f
        (lambda (in) (parameterize ([current-directory tmp])
                       (unzip in (make-filesystem-entry-reader)))))
      (for ([part (in-list (find-files (lambda (p) (regexp-match? #rx"slide[0-9]*[.]xml$"
                                                                 (path->string p)))
                                       tmp))])
        (define d (file->string part))
        (for ([m (in-list (regexp-match* #px"<[ap]:([A-Za-z0-9_]+)" d #:match-select cadr))])
          (hash-update! acc (string->symbol m) add1 0)))
      (delete-directory/files tmp #:must-exist? #f)))
  acc)

(define read-names (names-the-parser-reads))
(define used (elements-used-in (if (directory-exists? corpus-dir) corpus-dir decks-dir)))

(define unexamined
  (sort (for/list ([(n c) (in-hash used)]
                   #:unless (or (set-member? read-names n)
                                (hash-has-key? knowingly-skipped n)))
          (cons n c))
        > #:key cdr))

(printf "~a element names used; ~a the parser reads, ~a knowingly skipped\n"
        (hash-count used)
        (for/sum ([(n _) (in-hash used)] #:when (set-member? read-names n)) 1)
        (for/sum ([(n _) (in-hash used)] #:when (hash-has-key? knowingly-skipped n)) 1))
(unless (null? unexamined)
  (printf "not looked at:\n")
  (for ([p (in-list unexamined)])
    (printf "  ~a  ~a\n" (~a (cdr p) #:min-width 6) (car p))))

(check-equal? (map car unexamined) '()
              (string-append
               "these appear in the decks and the parser never asks for them. "
               "Implement one, or add it to `knowingly-skipped` with the reason -- "
               "an unread element is invisible to every other test here."))

(printf "coverage census done\n")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
