#lang racket/base
;; Theme resolution: the color scheme, the font scheme, and the format scheme
;; that shape styles refer to by index.
;;
;; DrawingML colors are rarely literal. A shape says "accent1, luminance
;; modulated to 60%", the slide master says which scheme slot "accent1" means,
;; and the theme says what that slot's RGB is. `resolve-color` performs that
;; whole walk, including the `phClr` indirection used inside the format scheme.
(require racket/list racket/string racket/math
         "xml-util.rkt" "units.rkt" "ir.rkt")
(provide (struct-out theme) (struct-out clr-ctx)
         parse-theme empty-theme
         resolve-color color-child resolve-color-child
         theme-latin-font resolve-typeface
         fill-style-ref line-style-ref)

;; clr-scheme maps a scheme slot symbol (dk1, lt1, accent1, ...) to its color
;; xexpr. fill-styles/line-styles are the format scheme's indexed lists.
(struct theme (clr-scheme font-scheme fill-styles line-styles bg-fill-styles) #:prefab)

;; clr-map maps the names shapes use (tx1, bg1, ...) onto scheme slots; it comes
;; from the slide master. ph-color is the color bound to `phClr` while resolving
;; a format-scheme entry.
(struct clr-ctx (theme clr-map ph-color))

(define default-clr-map
  (hash 'bg1 'lt1 'tx1 'dk1 'bg2 'lt2 'tx2 'dk2
        'accent1 'accent1 'accent2 'accent2 'accent3 'accent3
        'accent4 'accent4 'accent5 'accent5 'accent6 'accent6
        'hlink 'hlink 'folHlink 'folHlink))

(define (empty-theme) (theme (hash) (hash) '() '() '()))

(define (parse-theme x)
  (define elts (child x 'themeElements))
  (define (scheme-hash node)
    (if node
        (for/hash ([c (in-list (elem-children node))])
          (values (local-name (car c)) c))
        (hash)))
  (define fmt (and elts (child elts 'fmtScheme)))
  (theme (scheme-hash (and elts (child elts 'clrScheme)))
         (scheme-hash (and elts (child elts 'fontScheme)))
         (if fmt (elem-children (or (child fmt 'fillStyleLst) '(none ()))) '())
         (if fmt (elem-children (or (child fmt 'lnStyleLst) '(none ()))) '())
         (if fmt (elem-children (or (child fmt 'bgFillStyleLst) '(none ()))) '())))

;; -------------------------------------------------------------- color names

;; The element names DrawingML uses for a color, in the order a container may
;; hold them (only ever one).
(define color-tags '(srgbClr schemeClr sysClr prstClr hslClr scrgbClr))

;; First color-valued child of `x`, or #f. Used for <a:solidFill>, <a:fgClr>,
;; style refs, and text fills alike.
(define (color-child x)
  (and x (for/first ([c (in-list (elem-children x))]
                     #:when (memq (local-name (car c)) color-tags))
           c)))

(define (resolve-color-child ctx x)
  (define c (color-child x))
  (and c (resolve-color ctx c)))

(define preset-colors
  (hash "black" '(0 0 0) "white" '(255 255 255) "red" '(255 0 0)
        "green" '(0 128 0) "blue" '(0 0 255) "yellow" '(255 255 0)
        "cyan" '(0 255 255) "magenta" '(255 0 255) "gray" '(128 128 128)
        "grey" '(128 128 128) "darkGray" '(169 169 169) "lightGray" '(211 211 211)
        "orange" '(255 165 0) "purple" '(128 0 128) "brown" '(165 42 42)
        "pink" '(255 192 203) "lime" '(0 255 0) "navy" '(0 0 128)
        "teal" '(0 128 128) "olive" '(128 128 0) "maroon" '(128 0 0)
        "silver" '(192 192 192) "aqua" '(0 255 255) "fuchsia" '(255 0 255)))

(define (hex->rgba s)
  (define n (string->number (string-downcase (or s "")) 16))
  (and n (rgba (arithmetic-shift (bitwise-and n #xFF0000) -16)
               (arithmetic-shift (bitwise-and n #x00FF00) -8)
               (bitwise-and n #x0000FF)
               1.0)))

;; Resolves a color element to an rgba, applying its transform children.
(define (resolve-color ctx c)
  (define base (resolve-color-base ctx c))
  (and base (apply-color-transforms base (elem-children c))))

(define (resolve-color-base ctx c)
  (case (local-name (car c))
    [(srgbClr) (hex->rgba (attr c 'val))]
    [(sysClr) (or (hex->rgba (attr c 'lastClr))
                  (if (equal? "window" (attr c 'val)) white black))]
    [(prstClr) (let ([v (hash-ref preset-colors (or (attr c 'val) "") #f)])
                 (and v (rgb (first v) (second v) (third v))))]
    [(scrgbClr) (rgba (* 255.0 (or (string->percent (attr c 'r)) 0.0))
                      (* 255.0 (or (string->percent (attr c 'g)) 0.0))
                      (* 255.0 (or (string->percent (attr c 'b)) 0.0))
                      1.0)]
    [(hslClr) (hsl->rgba (/ (or (attr-num c 'hue) 0) 60000.0)
                         (or (string->percent (attr c 'sat)) 0.0)
                         (or (string->percent (attr c 'lum)) 0.0))]
    [(schemeClr) (resolve-scheme-color ctx (attr c 'val))]
    [else #f]))

(define (resolve-scheme-color ctx name)
  (cond
    [(not name) #f]
    ;; Inside the format scheme, phClr stands for the color the style ref gave.
    [(string=? name "phClr") (clr-ctx-ph-color ctx)]
    [else
     (define asked (string->symbol name))
     (define slot (hash-ref (clr-ctx-clr-map ctx) asked
                            (lambda () (hash-ref default-clr-map asked asked))))
     (define node (hash-ref (theme-clr-scheme (clr-ctx-theme ctx)) slot #f))
     ;; The scheme entry wraps the actual color element.
     (define inner (and node (color-child node)))
     (and inner (resolve-color ctx inner))]))

;; ---------------------------------------------------------- color transforms

(define (clamp01 v) (max 0.0 (min 1.0 v)))
(define (clamp255 v) (max 0.0 (min 255.0 v)))

(define (apply-color-transforms c xs)
  (for/fold ([c c]) ([x (in-list xs)])
    (define v (string->percent (attr x 'val)))
    (case (local-name (car x))
      [(alpha) (if v (struct-copy rgba c [a (clamp01 v)]) c)]
      [(alphaMod) (if v (struct-copy rgba c [a (clamp01 (* (rgba-a c) v))]) c)]
      [(alphaOff) (if v (struct-copy rgba c [a (clamp01 (+ (rgba-a c) v))]) c)]
      ;; tint lightens toward white, shade darkens toward black.
      [(tint) (if v (map-rgb c (lambda (ch) (+ (* ch v) (* 255.0 (- 1.0 v))))) c)]
      [(shade) (if v (map-rgb c (lambda (ch) (* ch v))) c)]
      [(lumMod) (if v (adjust-hsl c #:lum-mod v) c)]
      [(lumOff) (if v (adjust-hsl c #:lum-off v) c)]
      [(satMod) (if v (adjust-hsl c #:sat-mod v) c)]
      [(satOff) (if v (adjust-hsl c #:sat-off v) c)]
      [(hueMod) (if v (adjust-hsl c #:hue-mod v) c)]
      [(gray) (let ([y (luminance c)]) (rgba y y y (rgba-a c)))]
      [(inv) (map-rgb c (lambda (ch) (- 255.0 ch)))]
      [(comp complement) c]
      [else c])))

(define (map-rgb c f)
  (rgba (clamp255 (f (rgba-r c))) (clamp255 (f (rgba-g c))) (clamp255 (f (rgba-b c)))
        (rgba-a c)))

(define (luminance c)
  (+ (* 0.299 (rgba-r c)) (* 0.587 (rgba-g c)) (* 0.114 (rgba-b c))))

(define (adjust-hsl c #:lum-mod [lm #f] #:lum-off [lo #f]
                    #:sat-mod [sm #f] #:sat-off [so #f] #:hue-mod [hm #f])
  (define-values (h s l) (rgba->hsl c))
  (define h* (if hm (* h hm) h))
  (define s* (clamp01 (+ (if sm (* s sm) s) (or so 0.0))))
  (define l* (clamp01 (+ (if lm (* l lm) l) (or lo 0.0))))
  (struct-copy rgba (hsl->rgba h* s* l*) [a (rgba-a c)]))

;; h in degrees, s and l in 0..1.
(define (rgba->hsl c)
  (define r (/ (rgba-r c) 255.0)) (define g (/ (rgba-g c) 255.0))
  (define b (/ (rgba-b c) 255.0))
  (define mx (max r g b)) (define mn (min r g b))
  (define l (/ (+ mx mn) 2.0))
  (define d (- mx mn))
  (cond
    [(zero? d) (values 0.0 0.0 l)]
    [else
     (define s (/ d (if (> l 0.5) (- 2.0 mx mn) (+ mx mn))))
     (define h
       (cond [(= mx r) (* 60.0 (let ([v (/ (- g b) d)]) (if (< g b) (+ v 6.0) v)))]
             [(= mx g) (* 60.0 (+ 2.0 (/ (- b r) d)))]
             [else (* 60.0 (+ 4.0 (/ (- r g) d)))]))
     (values h s l)]))

(define (hsl->rgba h s l)
  (define (f n)
    (define k (modulo* (+ (/ h 30.0) n) 12.0))
    (define a (* s (min l (- 1.0 l))))
    (- l (* a (max -1.0 (min (- k 3.0) (- 9.0 k) 1.0)))))
  (rgba (clamp255 (* 255.0 (f 0.0))) (clamp255 (* 255.0 (f 8.0)))
        (clamp255 (* 255.0 (f 4.0))) 1.0))

(define (modulo* x m) (let ([v (- x (* m (floor (/ x m))))]) (if (< v 0) (+ v m) v)))

;; ------------------------------------------------------------------- fonts

;; which is 'major or 'minor; returns the latin typeface name or #f.
(define (theme-latin-font th which)
  (define node (hash-ref (theme-font-scheme th) (if (eq? which 'major) 'majorFont 'minorFont) #f))
  (define latin (and node (child node 'latin)))
  (define face (and latin (attr latin 'typeface)))
  (and face (not (string=? face "")) face))

;; Expands the "+mj-lt"/"+mn-lt" indirection a typeface attribute may hold.
(define (resolve-typeface th face)
  (cond
    [(not face) #f]
    [(string-prefix? face "+mj") (theme-latin-font th 'major)]
    [(string-prefix? face "+mn") (theme-latin-font th 'minor)]
    [(string=? face "") #f]
    [else face]))

;; --------------------------------------------------------------- style refs

;; A <a:fillRef idx="n"><a:schemeClr .../></a:fillRef> selects entry n of the
;; format scheme's fill list, with that color bound to phClr. idx 0 means none.
;; Returns (cons style-element context-with-phClr), or #f.
(define (fill-style-ref ctx ref)
  (define idx (or (attr-num ref 'idx) 0))
  (define styles (theme-fill-styles (clr-ctx-theme ctx)))
  (cond
    [(or (zero? idx) (> idx (length styles))) #f]
    [else
     (define ph (resolve-color-child ctx ref))
     (cons (list-ref styles (sub1 idx)) (struct-copy clr-ctx ctx [ph-color ph]))]))

(define (line-style-ref ctx ref)
  (define idx (or (attr-num ref 'idx) 0))
  (define styles (theme-line-styles (clr-ctx-theme ctx)))
  (cond
    [(or (zero? idx) (> idx (length styles))) #f]
    [else
     (define ph (resolve-color-child ctx ref))
     (cons (list-ref styles (sub1 idx)) (struct-copy clr-ctx ctx [ph-color ph]))]))
