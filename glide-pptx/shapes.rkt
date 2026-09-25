#lang racket/base
;; Turning a <p:spTree> into IR elements.
;;
;; The slide is only half the story: a shape that is a placeholder takes its
;; position, size and text defaults from the matching placeholder in the slide
;; layout, and from there in the slide master. `shape-ctx` holds those two
;; lookups plus everything else the walk needs.
(require racket/list racket/string
         "xml-util.rkt" "units.rkt" "ir.rkt" "theme.rkt" "drawing.rkt" "text.rkt"
         "source-tag.rkt")
(provide (struct-out shape-ctx) make-shape-ctx
         parse-sp-tree
         placeholder-info
         current-allow-unsupported?
         current-tag-names current-slide-tag-names)

;; layout-phs / master-phs map a placeholder key to its <p:sp>.
;; tx-styles is the master's <p:txStyles>; default-text-style is the
;; presentation's <p:defaultTextStyle>.
(struct shape-ctx (clr-ctx theme layout-phs master-phs tx-styles default-text-style
                   media warn table-styles))

(define (make-shape-ctx #:clr-ctx clr-ctx #:theme theme
                        #:layout-phs [layout-phs '()] #:master-phs [master-phs '()]
                        #:tx-styles [tx-styles #f] #:default-text-style [dts #f]
                        #:media [media (lambda (rid) #f)]
                        #:warn [warn void]
                        #:table-styles [table-styles #f])
  (shape-ctx clr-ctx theme layout-phs master-phs tx-styles dts media warn table-styles))

;; ------------------------------------------------------------- placeholders

;; (values type idx) for a shape's placeholder, or (values #f #f).
(define (placeholder-info sp)
  (define ph (or (xpath sp 'nvSpPr 'nvPr 'ph)
                 (xpath sp 'nvPicPr 'nvPr 'ph)
                 (xpath sp 'nvGraphicFramePr 'nvPr 'ph)))
  (if ph
      (values (string->symbol (or (attr ph 'type) "body"))
              (or (attr-num ph 'idx) 0))
      (values #f #f)))

;; Whether content we cannot represent is a warning instead of an error. Off,
;; because silently emptying a chart on a round trip loses the author's work.
(define current-allow-unsupported? (make-parameter #f))

(define title-types '(title ctrTitle))
(define body-types '(body subTitle obj tbl chart dgm clipArt media pic))

(define (normalize-ph-type t)
  (cond [(memq t title-types) 'title]
        [(memq t body-types) 'body]
        [else t]))

;; Which of the master's txStyles governs a placeholder of this type.
(define (tx-style-name t)
  (cond [(not t) #f]
        [(memq t title-types) 'titleStyle]
        [(memq t body-types) 'bodyStyle]
        [else 'otherStyle]))

;; Placeholders are collected by the caller; see `collect-phs` in parse.rkt.

;; PowerPoint matches a slide placeholder to a layout placeholder by index,
;; except that the title is matched by kind -- a slide's ctrTitle finds the
;; layout's title even though both carry index 0 among other index-0 shapes.
;;
;; Kind before index, though, once the index has failed to find something of
;; the same kind. Indexes are only unique within a part: a slide's second
;; content placeholder is index 2, and so is the master's date placeholder,
;; which is vertically centred and right-aligned because that is what a date is.
;; Matching those two put the body of a comparison slide in the middle of its
;; box and its first line against the right edge -- and the master is matched by
;; kind anyway, since it holds one placeholder of each.
(define (find-placeholder phs type idx)
  (define entries (filter values phs))
  (define (same-kind? e) (eq? (normalize-ph-type type) (normalize-ph-type (first e))))
  (or (and (memq type title-types)
           (for/first ([e (in-list entries)] #:when (memq (first e) title-types)) (third e)))
      (for/first ([e (in-list entries)]
                  #:when (and (equal? idx (second e)) (same-kind? e)))
        (third e))
      (for/first ([e (in-list entries)] #:when (same-kind? e)) (third e))
      (for/first ([e (in-list entries)] #:when (equal? idx (second e))) (third e))))

;; ------------------------------------------------------------ style sources

;; The <a:lstStyle>-shaped nodes to consult for a shape's text, most specific
;; first, plus the <a:bodyPr> chain.
(define (text-sources ctx sp layout-sp master-sp type)
  (define (lst s) (and s (xpath s 'txBody 'lstStyle)))
  (define (bpr s) (and s (xpath s 'txBody 'bodyPr)))
  (define style-node (child sp 'style))
  (define tx-styles (shape-ctx-tx-styles ctx))
  (define master-style
    (let ([n (tx-style-name type)])
      (and n tx-styles (child tx-styles n))))
  (values
   (filter values
           (list (lst sp)
                 (lst layout-sp)
                 (lst master-sp)
                 master-style
                 ;; A non-placeholder shape's default text color and typeface
                 ;; come from its own <p:style><a:fontRef>.
                 (and (not type) (font-ref->lst-style ctx style-node))
                 (shape-ctx-default-text-style ctx)
                 ;; Autoshapes without a placeholder still get the master's
                 ;; "other" sizes as a final fallback.
                 (and (not type) tx-styles (child tx-styles 'otherStyle))))
   (filter values (list (bpr sp) (bpr layout-sp) (bpr master-sp)))))

;; Synthesizes an lstStyle carrying the color and typeface a <a:fontRef> names,
;; so it can join the ordinary inheritance chain.
(define (font-ref->lst-style ctx style-node)
  (define ref (and style-node (child style-node 'fontRef)))
  (cond
    [(not ref) #f]
    [else
     (define color (color-child ref))
     (define face (case (attr ref 'idx) [("major") "+mj-lt"] [else "+mn-lt"]))
     (define defRPr
       `(a:defRPr () (a:latin ((typeface ,face)))
                  ,@(if color (list `(a:solidFill () ,color)) '())))
     `(a:lstStyle ()
                  ,@(for/list ([i (in-range 1 10)])
                      `(,(string->symbol (format "a:lvl~apPr" i)) () ,defRPr)))]))

;; ------------------------------------------------------------------- shapes

;; Elements of one shape tree, in document order (which is z-order).
(define (parse-sp-tree ctx sp-tree
                       #:placeholders-only? [ph-only? #f]
                       #:skip-placeholders? [skip-ph? #f])
  (filter values
          (for/list ([node (in-list (elem-children sp-tree))])
            (define-values (type _idx) (if (memq (local-name (car node))
                                                 '(sp pic graphicFrame grpSp cxnSp))
                                           (placeholder-info node)
                                           (values #f #f)))
            (cond
              [(and ph-only? (not type)) #f]
              [(and skip-ph? type) #f]
              [else (parse-node ctx node)]))))

(define (parse-node ctx node)
  (case (local-name (car node))
    [(sp) (parse-shape ctx node)]
    [(pic) (parse-picture ctx node)]
    [(cxnSp) (parse-connector ctx node)]
    [(grpSp) (parse-group ctx node)]
    [(graphicFrame) (parse-graphic-frame ctx node)]
    [(AlternateContent) (let ([fb (find-descendant node 'Fallback)])
                          (and fb (let ([kids (elem-children fb)])
                                    (and (pair? kids) (parse-node ctx (first kids))))))]
    [else #f]))

;; A shape we wrote carries its tag in the alt text as well as the name, and the
;; alt text is what survives being renamed in the editor -- so it wins. It also
;; says something the name cannot: several shapes with one tag came from one
;; place in the code, and are not several things that happen to share a name.
(define TAG-PREFIX "glide-pptx:")

;; The names that came from alt text on this slide. `uniquify-names` leaves these
;; alone, and there is nowhere on `element` to record it -- the IR is the format,
;; not the provenance.
(define current-tag-names (make-parameter #f))

;; Where the parser puts those names, slide by slide, for a caller that asked.
;; A name is not evidence that a shape is the program's: an editor invents one
;; for every shape somebody draws, and the one it invents for a new text box is
;; the name the box deleted in the same sitting had. The alt text is evidence.
;; So the merge, which has to tell the program's elements from the editor's own,
;; hands a hash to `pptx->deck` and is told which names the deck states as ours.
;; The translator asks for none: reading a real deck, a name is the only tag
;; there is.
(define current-slide-tag-names (make-parameter #f))

(define (shape-id-name node)
  (define nv (or (xpath node 'nvSpPr 'cNvPr) (xpath node 'nvPicPr 'cNvPr)
                 (xpath node 'nvCxnSpPr 'cNvPr) (xpath node 'nvGrpSpPr 'cNvPr)
                 (xpath node 'nvGraphicFramePr 'cNvPr)))
  (define descr (and nv (attr nv 'descr)))
  (define name (and nv (attr nv 'name)))
  (define tag
    (or (and descr (string-prefix? descr TAG-PREFIX)
             (substring descr (string-length TAG-PREFIX)))
        ;; LibreOffice drops the description of groups and can drop it from a
        ;; shape that it rebuilds, but it preserves the object name. Glide's
        ;; own automatic protocol is recognizable without treating an ordinary
        ;; user name such as `source:foo` as an identity.
        (and (automatic-tag? name) name)))
  (when (and tag (current-tag-names))
    (hash-set! (current-tag-names) tag #t))
  (values (or (and nv (attr-num nv 'id)) 0)
          (or tag name "")))

;; Geometry, walking up to the layout and master placeholder when the slide
;; shape states none.
(define (resolve-box ctx sp layout-sp master-sp)
  (define (xfrm-of s) (and s (xpath s 'spPr 'xfrm)))
  (define-values (b _cb)
    (parse-xfrm (or (xfrm-of sp) (xfrm-of layout-sp) (xfrm-of master-sp))
                #:default-bbox (make-bbox 0.0 0.0 0.0 0.0)))
  b)

;; The shadow a shape casts, from the first `outerShdw` in the properties given.
;; A shape says its effects itself, or inherits them from the placeholder it
;; fills in; the theme's effect styles are another source and not read here.
;;
;; `dir` is in 60000ths of a degree clockwise from east, `dist` and `blurRad`
;; in EMU. The colour carries its own alpha, which is what makes a shadow a
;; shadow rather than a black copy of the shape.
(define (parse-effect cctx . props-list)
  (for/or ([props (in-list props-list)])
    (define lst (and props (child props 'effectLst)))
    (define sh (and lst (child lst 'outerShdw)))
    (and sh
         (shadow (or (string->emu-pt (attr sh 'blurRad)) 0.0)
                 (or (string->emu-pt (attr sh 'dist)) 0.0)
                 (or (string->angle (attr sh 'dir)) 0.0)
                 (or (resolve-color-child cctx sh) black)))))

(define (parse-shape ctx sp)
  (define-values (id name) (shape-id-name sp))
  (define-values (type idx) (placeholder-info sp))
  (define layout-sp (and type (find-placeholder (shape-ctx-layout-phs ctx) type idx)))
  (define master-sp (and type (find-placeholder (shape-ctx-master-phs ctx) type idx)))
  (define spPr (child sp 'spPr))
  (define style-node (child sp 'style))
  (define cctx (shape-ctx-clr-ctx ctx))
  (define media (shape-ctx-media ctx))
  (define fill
    (effective-fill (parse-fill cctx spPr #:media media)
                    (parse-fill cctx (and layout-sp (child layout-sp 'spPr)) #:media media)
                    (parse-fill cctx (and master-sp (child master-sp 'spPr)) #:media media)
                    (or (fill-from-style cctx style-node #:media media) 'inherit)
                    ;; A text bbox with nothing said about its fill has none;
                    ;; other shapes fall back to the theme's first fill style.
                    (if (text-box? sp) #f 'inherit)))
  (define line
    (effective-line (parse-line cctx spPr)
                    (parse-line cctx (and layout-sp (child layout-sp 'spPr)))
                    (parse-line cctx (and master-sp (child master-sp 'spPr)))
                    (or (line-from-style cctx style-node) 'inherit)
                    (if (text-box? sp) #f 'inherit)))
  (define geom (parse-geometry ctx spPr layout-sp master-sp))
  (define-values (lvl-sources body-prs) (text-sources ctx sp layout-sp master-sp type))
  (define tctx (text-ctx cctx (shape-ctx-theme ctx) lvl-sources body-prs))
  ;; Top, which is what the format says when a shape says nothing: PowerPoint
  ;; writes `anchor="ctr"` on the autoshapes it centres, so a shape with no
  ;; anchor at all is one nobody centred. Assuming otherwise put a box of
  ;; overflowing text half a block higher than every other renderer draws it.
  (define body
    (parse-text-body tctx (child sp 'txBody) #:default-anchor 'top))
  (shape id name (resolve-box ctx sp layout-sp master-sp)
         geom fill line (and (not (text-body-empty? body)) body)
         (parse-effect cctx spPr
                       (and layout-sp (child layout-sp 'spPr))
                       (and master-sp (child master-sp 'spPr)))))

(define (text-box? sp)
  (define c (xpath sp 'nvSpPr 'cNvSpPr))
  (and c (string->bool (attr c 'txBox) #f)))

(define (parse-geometry ctx spPr layout-sp master-sp)
  (define (geom-of props)
    (and props
         (or (let ([p (child props 'prstGeom)])
               (and p (preset-geom (or (attr p 'prst) "rect")
                                   (for/list ([g (in-list (xpath* p 'avLst 'gd))])
                                     (cons (attr g 'name)
                                           (or (attr g 'fmla) ""))))))
             (let ([c (child props 'custGeom)])
               (and c (parse-custom-geometry c))))))
  (or (geom-of spPr)
      (geom-of (and layout-sp (child layout-sp 'spPr)))
      (geom-of (and master-sp (child master-sp 'spPr)))
      (preset-geom "rect" '())))

;; Custom geometry paths, normalized to the path's own coordinate space so the
;; renderer can scale them onto the shape bbox.
(define (parse-custom-geometry c)
  (define paths (xpath* c 'pathLst 'path))
  (define (pt node) (cons (or (attr-num node 'x) 0) (or (attr-num node 'y) 0)))
  ;; A path that declares no space, or declares 0, is written in EMU inside the
  ;; shape rather than in a space to be stretched onto it. Flooring that to 1
  ;; turned every such path into one scaled by the shape's own size -- a
  ;; twelve-thousand-fold blow-up on the decks that write paths this way -- so
  ;; the 0 is kept and everything downstream reads it as EMU.
  (define w (for/fold ([m 0]) ([p (in-list paths)]) (max m (or (attr-num p 'w) 0))))
  (define h (for/fold ([m 0]) ([p (in-list paths)]) (max m (or (attr-num p 'h) 0))))
  (custom-geom
   (for/list ([p (in-list paths)])
     (for/list ([cmd (in-list (elem-children p))])
       (case (local-name (car cmd))
         [(moveTo) (list 'move (pt (child cmd 'pt)))]
         [(lnTo) (list 'line (pt (child cmd 'pt)))]
         [(cubicBezTo) (cons 'curve (map pt (children cmd 'pt)))]
         [(quadBezTo) (cons 'quad (map pt (children cmd 'pt)))]
         [(close) (list 'close)]
         [(arcTo) (list 'arc
                        (or (attr-num cmd 'wR) 0) (or (attr-num cmd 'hR) 0)
                        (or (string->angle (attr cmd 'stAng)) 0.0)
                        (or (string->angle (attr cmd 'swAng)) 0.0))]
         [else (list 'nop)])))
   w h))

;; ---------------------------------------------------------------- pictures

(define (parse-picture ctx pic)
  (define-values (id name) (shape-id-name pic))
  (define-values (type idx) (placeholder-info pic))
  (define layout-sp (and type (find-placeholder (shape-ctx-layout-phs ctx) type idx)))
  (define master-sp (and type (find-placeholder (shape-ctx-master-phs ctx) type idx)))
  (define blip (xpath pic 'blipFill 'blip))
  (define rid (and blip (attr blip 'embed)))
  (define src (and rid ((shape-ctx-media ctx) rid)))
  (define cctx (shape-ctx-clr-ctx ctx))
  (define spPr (child pic 'spPr))
  ;; srcRect trims the source image, as fractions of its extent.
  (define sr (xpath pic 'blipFill 'srcRect))
  (define crop (and sr (list (or (string->percent (attr sr 'l)) 0.0)
                             (or (string->percent (attr sr 't)) 0.0)
                             (or (string->percent (attr sr 'r)) 0.0)
                             (or (string->percent (attr sr 'b)) 0.0))))
  (unless src ((shape-ctx-warn ctx) (format "picture ~a: unresolved image ~a" name rid)))
  (picture id name (resolve-box ctx pic layout-sp master-sp)
           src
           (effective-fill (parse-fill cctx spPr #:media (shape-ctx-media ctx)) #f)
           (effective-line (parse-line cctx spPr) #f)
           crop
           (blip-opacity blip)
           (parse-effect cctx spPr)))

;; -------------------------------------------------------------- connectors

;; A connector is a shape whose geometry is a line and which never has a fill.
(define (parse-connector ctx sp)
  (define-values (id name) (shape-id-name sp))
  (define spPr (child sp 'spPr))
  (define cctx (shape-ctx-clr-ctx ctx))
  (define-values (b _cb) (parse-xfrm (child spPr 'xfrm)
                                     #:default-bbox (make-bbox 0.0 0.0 0.0 0.0)))
  (shape id name b
         (parse-geometry ctx spPr #f #f)
         #f
         (effective-line (parse-line cctx spPr)
                         (or (line-from-style cctx (child sp 'style)) 'inherit)
                         (make-stroke black #:width 1.0))
         #f
         (parse-effect cctx spPr)))

;; ------------------------------------------------------------------ groups

(define (parse-group ctx grp)
  (define-values (id name) (shape-id-name grp))
  (define grpSpPr (child grp 'grpSpPr))
  (define-values (b cb) (parse-xfrm (child grpSpPr 'xfrm)
                                    #:default-bbox (make-bbox 0.0 0.0 0.0 0.0)))
  (define children
    (filter values (for/list ([n (in-list (elem-children grp))]
                              #:unless (memq (local-name (car n)) '(nvGrpSpPr grpSpPr)))
                     (parse-node ctx n))))
  ;; A group's chOff/chExt map its children's coordinates onto its own box. That
  ;; mapping is applied here, once, rather than carried into the IR: it scales
  ;; positions and extents but *not* font sizes, which is what PowerPoint does
  ;; -- resizing a group moves and resizes shapes and leaves their text alone.
  ;; Flattening it also means every coordinate in the IR, and so in generated
  ;; code, is absolute within its slide.
  (group id name b (map (child-space-transform b (or cb b)) children) b))

;; A function from a child's box in the group's child coordinate space to the
;; slide's own coordinates, applied through every level of nesting.
(define (child-space-transform b cb)
  (define sx (if (zero? (bbox-w cb)) 1.0 (/ (bbox-w b) (bbox-w cb))))
  (define sy (if (zero? (bbox-h cb)) 1.0 (/ (bbox-h b) (bbox-h cb))))
  (define (move bb)
    (make-bbox (+ (bbox-x b) (* sx (- (bbox-x bb) (bbox-x cb))))
               (+ (bbox-y b) (* sy (- (bbox-y bb) (bbox-y cb))))
               (* sx (bbox-w bb))
               (* sy (bbox-h bb))
               #:rot (bbox-rot bb)
               #:flip-h? (bbox-flip-h? bb) #:flip-v? (bbox-flip-v? bb)))
  (lambda (e) (element-map-bbox e move)))

;; ---------------------------------------------------------- graphic frames

(define (parse-graphic-frame ctx gf)
  (define-values (id name) (shape-id-name gf))
  (define-values (b _cb) (parse-xfrm (child gf 'xfrm)
                                     #:default-bbox (make-bbox 0.0 0.0 0.0 0.0)))
  (define tbl-node (find-descendant gf 'tbl))
  (cond
    [tbl-node (parse-table ctx id name b tbl-node)]
    [else
     ;; A chart or a diagram would round-trip to an empty box, quietly losing
     ;; the content. That is worse than refusing, so it refuses -- unless the
     ;; caller has said it only wants to look at the deck, not to trust it.
     (unless (current-allow-unsupported?)
       (error 'glide
              (string-append
               "~s is a chart or diagram, which is not supported.\n"
               "  It would come back as an empty box, losing the content.\n"
               "  To read the deck anyway, allow unsupported content.")
              name))
     ((shape-ctx-warn ctx)
      (format "graphicFrame ~a: unsupported content (chart or diagram), drawn as an empty bbox"
              name))
     (shape id name b (preset-geom "rect" '()) #f #f #f #f)]))

;; How a table style paints one cell.
;;
;; A table made in an editor says which style it uses and nothing else: the
;; fills and the borders live in `ppt/tableStyles.xml`, under names like
;; `wholeTbl` and `band1H`. Read as the cells alone, such a table has no paint
;; at all -- which is how a banded blue table came back as an empty grid.
;;
;; The parts apply in the order the format gives them, each overriding the last:
;; the whole table, then the banding, then the first and last column, then the
;; first and last row.
;; Only the style the table actually names. A table that names none is painted
;; by whatever draws it -- a plain grid -- and not by the file's `def` style:
;; falling back to that painted a deliberately plain table in banded blue.
(define (table-style-for styles-x id)
  (and styles-x id
       (for/first ([st (in-list (children styles-x 'tblStyle))]
                   #:when (equal? id (attr st 'styleId)))
         st)))

;; The names of the style parts that paint the cell at (row, col), in the order
;; they are applied.
(define (style-parts-for flags rows cols r c)
  (define (on? k) (hash-ref flags k #f))
  (define first-row? (and (on? 'firstRow) (= r 0)))
  (define last-row? (and (on? 'lastRow) (= r (sub1 rows))))
  (define first-col? (and (on? 'firstCol) (= c 0)))
  (define last-col? (and (on? 'lastCol) (= c (sub1 cols))))
  ;; Banding counts from after the header, and the first band is band1.
  (define band-row (- r (if (on? 'firstRow) 1 0)))
  (define band-col (- c (if (on? 'firstCol) 1 0)))
  (append
   (list 'wholeTbl)
   (if (and (on? 'bandCol) (not first-col?) (not last-col?) (>= band-col 0))
       (list (if (even? band-col) 'band1V 'band2V)) '())
   (if (and (on? 'bandRow) (not first-row?) (not last-row?) (>= band-row 0))
       (list (if (even? band-row) 'band1H 'band2H)) '())
   (if first-col? (list 'firstCol) '())
   (if last-col? (list 'lastCol) '())
   (if first-row? (list 'firstRow) '())
   (if last-row? (list 'lastRow) '())))

;; (values fill line) that a style paints a cell with, or (values #f #f).
(define (style-paint cctx style parts media)
  (for/fold ([fill #f] [line #f]) ([part-name (in-list parts)])
    (define part (and style (child style part-name)))
    (define tc (and part (child part 'tcStyle)))
    (cond
      [(not tc) (values fill line)]
      [else
       (define f (let ([node (child tc 'fill)])
                   (and node
                        (let ([inner (let ([ks (children* node)])
                                       (and (pair? ks) (first ks)))])
                          (and inner (let ([v (parse-fill-element cctx inner #:media media)])
                                       (and (not (eq? 'inherit v)) v)))))))
       ;; One line for the cell, since that is what a cell here carries. The
       ;; sides of a style are almost always the same line under six names.
       (define bdr (child tc 'tcBdr))
       (define l (and bdr
                      (for/or ([side (in-list '(insideH insideV left top right bottom))])
                        (let ([node (child bdr side)])
                          (and node (let ([v (parse-line cctx node)])
                                      (and (not (eq? 'inherit v)) v)))))))
       (values (or f fill) (or l line))])))

;; The border a cell states for itself: one line for the four sides, taken from
;; whichever of them it names first, since that is all a cell carries here.
(define (cell-own-line cctx tcPr)
  (or (and tcPr
           (for/or ([side (in-list '(lnL lnT lnR lnB))])
             (let ([node (child tcPr side)])
               (and node (parse-line-element cctx node)))))
      'inherit))

(define (parse-table ctx id name b tbl-node)
  (define cctx (shape-ctx-clr-ctx ctx))
  (define col-widths (for/list ([g (in-list (xpath* tbl-node 'tblGrid 'gridCol))])
                       (or (string->emu-pt (attr g 'w)) 0.0)))
  (define rows (children tbl-node 'tr))
  (define row-heights (for/list ([r (in-list rows)])
                        (or (string->emu-pt (attr r 'h)) 0.0)))
  (define tblPr (child tbl-node 'tblPr))
  (define flags
    (for/hash ([k (in-list '(firstRow lastRow firstCol lastCol bandRow bandCol))])
      (values k (and tblPr (string->bool (attr tblPr k) #f)))))
  (define style
    (table-style-for (shape-ctx-table-styles ctx)
                     (let ([n (and tblPr (child tblPr 'tableStyleId))])
                       (and n (all-text n)))))
  (define n-rows (length rows))
  (define cells
    (for/list ([r (in-list rows)] [ri (in-naturals)])
      (define n-cols (length (children r 'tc)))
      (for/list ([tc (in-list (children r 'tc))] [ci (in-naturals)])
        (define-values (style-fill style-line)
          (style-paint cctx style (style-parts-for flags n-rows n-cols ri ci)
                       (shape-ctx-media ctx)))
        (define tcPr (child tc 'tcPr))
        (define tctx (text-ctx cctx (shape-ctx-theme ctx)
                               (filter values (list (xpath tc 'txBody 'lstStyle)
                                                    (shape-ctx-default-text-style ctx)))
                               (filter values (list (xpath tc 'txBody 'bodyPr) tcPr))))
        (tbl-cell (parse-text-body tctx (child tc 'txBody) #:default-anchor 'top)
              (effective-fill (parse-fill cctx tcPr #:media (shape-ctx-media ctx))
                              style-fill #f)
              ;; A cell states its borders one side at a time -- `lnL`, `lnT`,
              ;; `lnR`, `lnB` -- and never as the plain `ln` a shape uses, so
              ;; asking for that found nothing and every stated border was
              ;; thrown away in favour of the style's.
              (effective-line (cell-own-line cctx tcPr) style-line #f)
              (or (attr-num tc 'rowSpan) 1)
              (or (attr-num tc 'gridSpan) 1)
              (or (attr tc 'hMerge) (attr tc 'vMerge) #f)))))
  (tbl id name b col-widths row-heights cells))
