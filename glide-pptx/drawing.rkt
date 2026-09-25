#lang racket/base
;; Fill and outline parsing for DrawingML shape properties.
;;
;; A shape's own <p:spPr> may say nothing about its fill, in which case the
;; answer comes from the <p:style> format-scheme reference. The parsers here
;; return 'inherit for "not stated" so callers can tell that apart from an
;; explicit <a:noFill>.
(require racket/list racket/string
         "xml-util.rkt" "units.rkt" "ir.rkt" "theme.rkt")
(provide parse-fill parse-fill-element parse-line parse-line-element blip-opacity
         fill-from-style line-from-style
         effective-fill effective-line
         ;; The fuzzer names a dash the way the reader does, so that it
         ;; generates strokes a file could hold.
         dash-like
         parse-xfrm)

;; ------------------------------------------------------------------- fills

(define fill-tags '(noFill solidFill gradFill blipFill pattFill grpFill))

;; props is a <p:spPr>, <a:tcPr>, <a:rPr> or similar. `media` maps a
;; relationship id to a media path.
;; Returns a fill struct, #f for an explicit no-fill, or 'inherit.
(define (parse-fill ctx props #:media [media (lambda (rid) #f)])
  (define f (and props
                 (for/first ([c (in-list (elem-children props))]
                             #:when (memq (local-name (car c)) fill-tags))
                   c)))
  (if f (parse-fill-element ctx f #:media media) 'inherit))

(define (parse-fill-element ctx f #:media [media (lambda (rid) #f)])
  (case (local-name (car f))
    [(noFill) #f]
    [(solidFill) (let ([c (resolve-color-child ctx f)]) (and c (solid-fill c)))]
    [(gradFill) (parse-gradient ctx f)]
    [(blipFill) (let* ([blip (child f 'blip)]
                       [rid (and blip (attr blip 'embed))]
                       [src (and rid (media rid))])
                  (and src (image-fill src (blip-opacity blip))))]
    [(pattFill) (pattern-fill (or (attr f 'prst) "pct50")
                              (or (resolve-color-child ctx (child f 'fgClr)) black)
                              (or (resolve-color-child ctx (child f 'bgClr)) white))]
    ;; A group fill defers to the enclosing group, which we treat as no fill.
    [(grpFill) #f]
    [else 'inherit]))

;; <a:blip><a:alphaModFix amt="35116"/> means the picture is drawn at 35%.
(define (blip-opacity blip)
  (define fix (and blip (child blip 'alphaModFix)))
  (or (and fix (string->percent (attr fix 'amt))) 1.0))

(define (parse-gradient ctx f)
  (define stops
    (sort (for/list ([gs (in-list (xpath* f 'gsLst 'gs))])
            (cons (or (string->percent (attr gs 'pos)) 0.0)
                  (or (resolve-color-child ctx gs) black)))
          < #:key car))
  (define lin (child f 'lin))
  ;; ang is clockwise from the positive x axis; without <a:lin> we use the
  ;; top-to-bottom default that <a:path> gradients also read as.
  (define angle (if lin (or (string->angle (attr lin 'ang)) 0.0) 90.0))
  (and (pair? stops) (gradient-fill stops angle)))

;; ------------------------------------------------------------------- lines

;; The four we draw, and which of DrawingML's presets each stands for. It has
;; to agree with what the writer puts back: `sysDash` was read as a plain dash
;; while the writer used it for a short one, so a short dash came back a long
;; one every time a deck went out and in again.
(define dash-map
  (hash "solid" 'solid
        "dot" 'dot "sysDot" 'dot
        "sysDash" 'short-dash
        "dash" 'dash "lgDash" 'dash
        "dashDot" 'dash-dot "sysDashDot" 'dash-dot
        "lgDashDot" 'dash-dot "lgDashDotDot" 'dash-dot))

;; Returns a stroke, #f for an explicit no-line, or 'inherit. A stroke's
;; width/dash/cap may themselves be 'inherit; `effective-line` resolves those.
(define (parse-line ctx props)
  (define ln (and props (child props 'ln)))
  (if ln (parse-line-element ctx ln) 'inherit))

;; The thinnest width that means something on both sides of a round trip.
(define HAIRLINE 0.75)

(define (parse-line-element ctx ln)
  (define fill (parse-fill ctx ln))
  (define color (cond [(solid-fill? fill) (solid-fill-color fill)]
                      [(gradient-fill? fill) (cdr (first (gradient-fill-stops fill)))]
                      [(eq? fill 'inherit) 'inherit]
                      [else #f]))
  (cond
    [(not color) #f]                      ; <a:ln><a:noFill/></a:ln>
    [else
     ;; A line width is in EMU, unlike font sizes and spacing, which are
     ;; hundredths of a point.
     ;;
     ;; A width of zero is a hairline: the thinnest the renderer can draw. The
     ;; writer has always put the thinnest real width in its place, since a
     ;; zero can be taken as "no line at all" -- so reading it as zero left the
     ;; program saying one thing and the deck the next, and a merge reporting a
     ;; restyle of a line nobody had touched, over and over.
     (define w
       (let ([v (string->emu-pt (attr ln 'w))])
         (cond [(not v) 'inherit]
               [(zero? v) HAIRLINE]
               [else v])))
     ;; A line can name a preset dash or spell one out. Spelled out is the
     ;; common case in decks written by Keynote, and ignoring it drew every one
     ;; of those lines solid.
     (define custom (parse-cust-dash (child ln 'custDash)))
     (define dash (cond
                    [custom (dash-like custom)]
                    [(child ln 'prstDash)
                     => (lambda (d) (hash-ref dash-map (or (attr d 'val) "") 'solid))]
                    [else #f]))
     (define cap (case (attr ln 'cap)
                   [("rnd") 'round] [("sq") 'projecting] [("flat") 'flat] [else #f]))
     (stroke color w (or dash 'inherit) (or cap 'inherit)
             (parse-line-end (child ln 'headEnd))
             (parse-line-end (child ln 'tailEnd))
             custom)]))

;; `<a:custDash><a:ds d="200000" sp="200000"/>...</a:custDash>`: each pair is a
;; dash and the gap after it, in thousandths of a percent of the line's width.
;; Kept as percentages, which is how they are written back.
(define (parse-cust-dash e)
  (define pairs
    (for/list ([ds (in-list (if e (children e 'ds) '()))])
      (cons (/ (or (string->number (or (attr ds 'd) "")) 100000) 1000.0)
            (/ (or (string->number (or (attr ds 'sp) "")) 100000) 1000.0))))
  (and (pair? pairs) pairs))

;; The nearest style there is to draw with. A dc pen has a fixed set of dashes,
;; so a spelled-out pattern is matched to the closest of them by how long its
;; dashes are relative to the line's width.
(define (dash-like pattern)
  (define d (car (first pattern)))
  (define varied? (> (length (remove-duplicates (map car pattern))) 1))
  (cond
    [varied? 'dash-dot]
    [(<= d 150.0) 'dot]
    [(>= d 500.0) 'dash]
    [else 'short-dash]))

;; `<a:headEnd type="arrow" w="med" len="med"/>`. Only the shape matters here;
;; the two size words are kept so the writer can hand them back.
(define (parse-line-end e)
  (define kind (case (or (and e (attr e 'type)) "none")
                 [("triangle") 'triangle]
                 [("stealth") 'stealth]
                 [("diamond") 'diamond]
                 [("oval") 'oval]
                 [("arrow") 'arrow]
                 [else #f]))
  (and kind (line-end kind
                      (or (and e (attr e 'w)) "med")
                      (or (and e (attr e 'len)) "med"))))

;; ------------------------------------------------------- format-scheme refs

;; The fill a <p:style><a:fillRef> selects, or #f.
(define (fill-from-style ctx style #:media [media (lambda (rid) #f)])
  (define ref (and style (child style 'fillRef)))
  (define hit (and ref (fill-style-ref ctx ref)))
  (and hit (parse-fill-element (cdr hit) (car hit) #:media media)))

(define (line-from-style ctx style)
  (define ref (and style (child style 'lnRef)))
  (define hit (and ref (line-style-ref ctx ref)))
  (and hit (parse-line-element (cdr hit) (car hit))))

;; Picks the first candidate that is not 'inherit; #f when all are.
(define (effective-fill . candidates)
  (let loop ([cs candidates])
    (cond [(null? cs) #f]
          [(eq? 'inherit (car cs)) (loop (cdr cs))]
          [else (car cs)])))

;; Like `effective-fill`, but also fills in a stroke's inherited width, dash and
;; cap from later candidates, since <a:ln> commonly states only a color.
(define (effective-line . candidates)
  (define resolved (filter (lambda (c) (not (eq? 'inherit c))) candidates))
  (define winner (if (null? resolved) #f (first resolved)))
  (cond
    [(not (stroke? winner)) winner]
    [else
     (define (pick get default)
       (let loop ([rs resolved])
         (cond [(null? rs) default]
               [(not (stroke? (car rs))) (loop (cdr rs))]
               [(eq? 'inherit (get (car rs))) (loop (cdr rs))]
               [else (get (car rs))])))
     (define color (pick stroke-color 'inherit))
     (cond
       ;; An <a:ln> can state a width, a dash and a miter limit and never say
       ;; what color to draw in -- PowerPoint for Mac writes exactly that on a
       ;; plain text box. With nothing in the style chain supplying one either,
       ;; the shape has no outline at all. Defaulting to black instead puts a
       ;; visible box around every such shape.
       [(eq? 'inherit color) #f]
       ;; A line end is not inherited through the chain the way a width is: the
       ;; shape that draws an arrow is the one that says so.
       [else (stroke color (pick stroke-width 1.0)
                     (pick stroke-dash 'solid) (pick stroke-cap 'flat)
                     (stroke-head winner) (stroke-tail winner)
                     (stroke-dash-pattern winner))])]))

;; ---------------------------------------------------------------- transform

;; Reads <a:xfrm>; returns (values bbox child-bbox) where child-bbox is the group
;; child coordinate space from chOff/chExt, or #f when absent.
(define (parse-xfrm xfrm #:default-bbox [default #f])
  (cond
    [(not xfrm) (values default #f)]
    [else
     (define off (child xfrm 'off))
     (define ext (child xfrm 'ext))
     (define (num n el fallback) (or (string->emu-pt (attr el n)) fallback))
     (define b (make-bbox (if off (num 'x off 0.0) (if default (bbox-x default) 0.0))
                         (if off (num 'y off 0.0) (if default (bbox-y default) 0.0))
                         (if ext (num 'cx ext 0.0) (if default (bbox-w default) 0.0))
                         (if ext (num 'cy ext 0.0) (if default (bbox-h default) 0.0))
                         #:rot (or (string->angle (attr xfrm 'rot)) 0.0)
                         #:flip-h? (string->bool (attr xfrm 'flipH) #f)
                         #:flip-v? (string->bool (attr xfrm 'flipV) #f)))
     (define ch-off (child xfrm 'chOff))
     (define ch-ext (child xfrm 'chExt))
     (define cb (and ch-off ch-ext
                     (make-bbox (num 'x ch-off 0.0) (num 'y ch-off 0.0)
                               (num 'cx ch-ext 0.0) (num 'cy ch-ext 0.0))))
     (values b cb)]))
