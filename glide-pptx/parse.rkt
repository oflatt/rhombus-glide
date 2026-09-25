#lang racket/base
;; pptx -> deck IR.
;;
;; The package is expanded into `workdir` and stays there, because the IR refers
;; to media by package-relative part name and the renderer has to be able to
;; open those files.
(require racket/list racket/string racket/file racket/path
         "xml-util.rkt" "units.rkt" "ir.rkt" "opc.rkt" "theme.rkt"
         "drawing.rkt" "text.rkt" "shapes.rkt")
(provide pptx->deck current-warnings current-allow-unsupported? build-steps
         current-build-frames? current-slide-tag-names)

;; Collected diagnostics for things we render approximately.
(define current-warnings (make-parameter #f))

(define (warn! msg)
  (define bbox* (current-warnings))
  (when bbox* (set-box! bbox* (cons msg (unbox bbox*)))))

(define TYPE/SLIDE "/slide")
(define TYPE/LAYOUT "/slideLayout")
(define TYPE/MASTER "/slideMaster")
(define TYPE/THEME "/theme")
(define TYPE/TABLE-STYLES "/tableStyles")

(define (pptx->deck pptx-path #:workdir workdir)
  (make-directory* workdir)
  (define pkg (open-package pptx-path #:into workdir))
  (define pres-name (main-document-name pkg))
  (define pres (part-xexpr pkg pres-name))
  (define sz (child pres 'sldSz))
  (define width (or (and sz (string->emu-pt (attr sz 'cx))) 720.0))
  (define height (or (and sz (string->emu-pt (attr sz 'cy))) 540.0))
  (define default-text-style (child pres 'defaultTextStyle))
  ;; <p:sldId> carries both an unprefixed id and the r:id that names the
  ;; relationship, so the relationship has to be read as the namespaced one.
  (define slide-names
    (for/list ([sid (in-list (xpath* pres 'sldIdLst 'sldId))])
      (define rid (attr-ns sid 'id))
      (and rid (rel-target pkg pres-name rid))))
  ;; How tables are painted when they say only which style they use, which is
  ;; what a table made in an editor says. Read once for the package.
  (define table-styles
    (let ([n (single-rel pkg pres-name TYPE/TABLE-STYLES)])
      (and n (part-xexpr pkg n))))
  ;; A slide that is built in clicks comes back as several, so the numbering is
  ;; done after the fact.
  (define slides
    (for/list ([s (in-list (append*
                            (for/list ([name (in-list (filter values slide-names))]
                                       [i (in-naturals 1)])
                              (parse-slide pkg name i width height default-text-style
                                           table-styles))))]
               [i (in-naturals 1)])
      (struct-copy slide s [index i])))
  (deck width height slides workdir (path->string (simplify-path pptx-path))))

;; The package root's relationships name the main document part; for a
;; presentation that is ppt/presentation.xml, but the name is not guaranteed.
(define (main-document-name pkg)
  (define targets (rel-targets-by-type pkg "" "/officeDocument"))
  (if (pair? targets) (first targets) "ppt/presentation.xml"))

;; ------------------------------------------------------------------- slides

(define (single-rel pkg from suffix)
  (define ts (rel-targets-by-type pkg from suffix))
  (and (pair? ts) (first ts)))

;; The clicks a slide is built in.
;;
;; PowerPoint and Keynote both record a build as a timeline: a main sequence of
;; nodes, each with a `nodeType` saying what starts it, and each naming the
;; shapes it acts on. A `clickEffect` starts a new step -- the presenter
;; advances -- and `withEffect` and `afterEffect` belong to the step before
;; them. What comes back is one list of shape ids per click, in click order.
;;
;; Only entrance effects matter for splitting a slide into the frames a reader
;; sees: something that appears was not there before. An emphasis or an exit is
;; ignored, which leaves the shape on every frame -- less wrong than dropping it.
(define ENTRANCE-KINDS '("entr"))

(define (build-steps slide-x)
  (define timing (and slide-x (child slide-x 'timing)))
  (cond
    [(not timing) '()]
    [else
     (define steps
       (for/fold ([steps '()]) ([node (in-list (find-descendants timing 'cTn))])
         (define kind (attr node 'nodeType))
         (define targets
           (remove-duplicates
            (filter values
                    (for/list ([tgt (in-list (find-descendants node 'spTgt))])
                      (attr-num tgt 'spid)))))
         (cond
           ;; A step of its own, whether or not it turns out to name anything.
           [(equal? "clickEffect" kind) (cons targets steps)]
           [(and (member kind '("withEffect" "afterEffect")) (pair? steps))
            (cons (append (first steps) targets) (rest steps))]
           [else steps])))
     ;; Only the shapes that were not there before: a `set` to visible, or an
     ;; entrance animation.
     (define entering (entering-ids timing))
     (define kept
       (for/list ([step (in-list (reverse steps))])
         (filter (lambda (id) (memv id entering)) step)))
     ;; Trailing clicks that reveal nothing add no frame.
     (let loop ([ss (reverse kept)])
       (cond [(and (pair? ss) (null? (first ss))) (loop (rest ss))]
             [else (reverse ss)]))]))

;; The shapes some effect brings in. An entrance animation says `presetClass`
;; "entr"; a plain appear is a `set` of `style.visibility` to `visible`.
(define (entering-ids timing)
  (remove-duplicates
   (filter
    values
    (append
     (for*/list ([node (in-list (find-descendants timing 'animEffect))]
                 [tgt (in-list (find-descendants node 'spTgt))])
       (attr-num tgt 'spid))
     (for*/list ([node (in-list (find-descendants timing 'par))]
                 #:when (member (attr (or (child node 'cTn) node) 'presetClass)
                                ENTRANCE-KINDS)
                 [tgt (in-list (find-descendants node 'spTgt))])
       (attr-num tgt 'spid))
     (for*/list ([node (in-list (find-descendants timing 'set))]
                 #:when (let ([a (find-descendant node 'attrName)])
                          (and a (equal? "style.visibility" (all-text a))))
                 [tgt (in-list (find-descendants node 'spTgt))])
       (attr-num tgt 'spid))))))

(define (parse-slide pkg slide-name index width height default-text-style table-styles)
  (define slide-x (part-xexpr pkg slide-name))
  (define layout-name (single-rel pkg slide-name TYPE/LAYOUT))
  (define layout-x (and layout-name (part-xexpr pkg layout-name)))
  (define master-name (and layout-name (single-rel pkg layout-name TYPE/MASTER)))
  (define master-x (and master-name (part-xexpr pkg master-name)))
  (define theme-name (and master-name (single-rel pkg master-name TYPE/THEME)))
  (define th (if theme-name (parse-theme (part-xexpr pkg theme-name)) (empty-theme)))

  ;; The master's clrMap names which scheme slot "tx1" and friends mean; a slide
  ;; or layout may override it.
  (define clr-map (merge-clr-map master-x layout-x slide-x))
  (define cctx (clr-ctx th clr-map #f))

  (define (sp-tree-of x) (and x (xpath x 'cSld 'spTree)))
  (define layout-tree (sp-tree-of layout-x))
  (define master-tree (sp-tree-of master-x))
  (define layout-phs (if layout-tree (collect-phs layout-tree) '()))
  (define master-phs (if master-tree (collect-phs master-tree) '()))

  (define (media-for part) (lambda (rid) (rel-target pkg part rid)))

  (define (ctx-for part layout-phs master-phs tx-styles)
    (make-shape-ctx #:clr-ctx cctx #:theme th
                    #:layout-phs layout-phs #:master-phs master-phs
                    #:tx-styles tx-styles #:default-text-style default-text-style
                    #:media (media-for part) #:warn warn!
                    #:table-styles table-styles))

  (define tx-styles (and master-x (child master-x 'txStyles)))

  ;; PowerPoint paints the master's and layout's own graphics behind the slide,
  ;; unless the slide turns that off. Their placeholders are not painted: those
  ;; only appear where the slide fills them in.
  (define show-master? (string->bool (attr slide-x 'showMasterSp) #t))
  (define background-elements
    (if show-master?
        (append
         (if master-tree
             (parse-sp-tree (ctx-for master-name '() master-phs tx-styles) master-tree
                            #:skip-placeholders? #t)
             '())
         (if layout-tree
             (parse-sp-tree (ctx-for layout-name layout-phs master-phs tx-styles) layout-tree
                            #:skip-placeholders? #t)
             '()))
        '()))

  (define slide-tree (sp-tree-of slide-x))
  ;; Filled in as the tree is read, with the names that came from our alt text.
  (define tag-names (make-hash))
  (define elements
    (parameterize ([current-tag-names tag-names])
      (parse-sp-tree (ctx-for slide-name layout-phs master-phs tx-styles) slide-tree)))
  (let ([collect (current-slide-tag-names)])
    (when collect (hash-set! collect index tag-names)))

  (define bg (resolve-background
              cctx
              (list (cons slide-x (media-for slide-name))
                    (cons layout-x (and layout-name (media-for layout-name)))
                    (cons master-x (and master-name (media-for master-name))))))
  (build-frames
   (slide index
         (or (slide-display-name slide-x) (format "Slide ~a" index))
         width height bg
         background-elements
         ;; A name is the element's key for export and for merging edits back,
         ;; so it has to be unique within its slide -- which PowerPoint does not
         ;; guarantee. Doing it here means every path downstream gets a key,
         ;; whether it goes through generated code or renders directly.
         ;;
         ;; A name that came from our own alt text is exempt: several shapes
         ;; carrying one tag were drawn by one `at`, and renaming them apart
         ;; would turn one code site into three.
         (uniquify-names elements (hash-keys tag-names))
         ;; `show="0"` is a slide the editor was told to skip. Absent means
         ;; shown, which is all but a handful of slides.
         (equal? "0" (attr slide-x 'show))
         #f)
   (if (current-build-frames?) (build-steps slide-x) '())))

;; A slide built in clicks is several slides: one for what is there before the
;; first click, and one after each of them. That is what a deck of still slides
;; can say about a build, and it is what the room sees.
;;
;; Only the shapes some effect brings in are held back. Everything else is on
;; every frame, which is what a slide with no build looks like.
(define (build-frames s steps)
  (cond
    [(null? steps) (list s)]
    [else
     (define entering (append* steps))
     (define frames (add1 (length steps)))
     (for/list ([k (in-range frames)])
       (define revealed (append* (take steps k)))
       (struct-copy slide s
                    [name (format "~a (~a of ~a)" (slide-name s) (add1 k) frames)]
                    [build (slide-name s)]
                    [elements
                     (for/list ([e (in-list (slide-elements s))]
                                #:unless (and (memv (element-id e) entering)
                                              (not (memv (element-id e) revealed))))
                       e)]))]))

;; Whether a slide built in clicks becomes one slide per click. The command line
;; turns it on: a deck of still slides has no other way to say what a build
;; says. Off by default so that everything measuring one slide against one
;; slide -- every fidelity test here -- keeps doing that.
(define current-build-frames? (make-parameter #f))

(define (uniquify-names elements [exempt '()])
  (define seen (make-hash))
  (define (rename e)
    ;; A shape with no name at all gets one. Editors write them: draw a
    ;; rectangle in LibreOffice and it is saved as `name=""`. Nameless, it has
    ;; no key -- the merge sees an element it cannot name, cannot find, and
    ;; cannot write, and a save that would otherwise have gone through is
    ;; refused whole. The id is the file's own and unique within the slide.
    (define name
      (let ([n (element-name e)])
        (if (string=? "" n) (format "Shape ~a" (element-id e)) n)))
    (define e* (if (equal? name (element-name e)) e (element-with-name e name)))
    (define renamed
      (cond
        [(member name exempt) e*]
        [else
         (define n (hash-ref seen name 0))
         (hash-set! seen name (add1 n))
         (if (zero? n) e* (element-with-name e* (format "~a (~a)" name (add1 n))))]))
    (if (group? renamed)
        (group (element-id renamed) (element-name renamed) (element-bbox renamed)
               (map rename (group-children renamed)) (group-child-bbox renamed))
        renamed))
  (map rename elements))

(define (slide-display-name slide-x)
  (define cs (child slide-x 'cSld))
  (define n (and cs (attr cs 'name)))
  (and n (not (string=? n "")) n))

(define (collect-phs sp-tree)
  (for/list ([sp (in-list (elem-children sp-tree))]
             #:when (memq (local-name (car sp)) '(sp pic graphicFrame)))
    (define-values (type idx) (placeholder-info sp))
    (and type (list type idx sp))))

(define clr-map-keys
  '(bg1 tx1 bg2 tx2 accent1 accent2 accent3 accent4 accent5 accent6 hlink folHlink))

(define (merge-clr-map master-x layout-x slide-x)
  (define base
    (let ([m (and master-x (child master-x 'clrMap))])
      (if m
          (for/hash ([k (in-list clr-map-keys)])
            (values k (string->symbol (or (attr m k) (symbol->string k)))))
          (hash))))
  ;; An override replaces the whole map, so take the innermost one that has it.
  (define (override-of x)
    (define o (and x (child x 'clrMapOvr)))
    (define m (and o (child o 'overrideClrMapping)))
    (and m (for/hash ([k (in-list clr-map-keys)])
             (values k (string->symbol (or (attr m k) (symbol->string k)))))))
  (or (override-of slide-x) (override-of layout-x) base))

;; The first of slide/layout/master that states a background.
;; `candidates` are (part-xexpr . media-for-that-part) pairs, slide first, then
;; layout, then master. Each part's own relationships resolve its own image
;; ids: a background picture named in the layout is r:embed "rId2" *there*, and
;; looking that up in the slide's relationships finds whatever the slide
;; happens to call rId2 -- a notes page, in the deck that turned this up.
(define (resolve-background cctx candidates)
  (for/or ([c (in-list candidates)])
    (define x (car c))
    (define media (cdr c))
    (define bg (and x (xpath x 'cSld 'bg)))
    (cond
      [(not bg) #f]
      [(child bg 'bgPr) => (lambda (p) (let ([f (parse-fill cctx p #:media media)])
                                         (and (not (eq? 'inherit f)) f)))]
      ;; <p:bgRef> selects an entry of the theme's background fill list.
      [(child bg 'bgRef)
       => (lambda (r)
            (define idx (or (attr-num r 'idx) 0))
            (define styles (theme-bg-fill-styles (clr-ctx-theme cctx)))
            (and (>= idx 1) (<= idx (length styles))
                 (parse-fill-element
                  (struct-copy clr-ctx cctx [ph-color (resolve-color-child cctx r)])
                  (list-ref styles (sub1 idx)) #:media media)))]
      [else #f])))
