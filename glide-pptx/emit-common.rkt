#lang racket/base
;; The language-neutral half of code generation.
;;
;; Deciding what a slide's elements should turn into -- which options differ from
;; their defaults, when a shape is really a text box, how a bullet is described
;; -- is the same work whether the output is Racket or Rhombus. That work happens
;; here and produces a tree of `v:` values; a flavor then renders the tree with
;; its own syntax and its own printer.
(require racket/list racket/string racket/format racket/file racket/path
         "ir.rkt")
(provide (struct-out v:num) (struct-out v:str) (struct-out v:sym) (struct-out v:bool)
         (struct-out v:raw) (struct-out v:list) (struct-out v:pair) (struct-out v:call)
         (struct-out kwv)
         (struct-out flavor)
         current-media-names media-name current-identity-tags?
         current-deck-font dominant-font
         num-string
         element->value slide-background-value
         render render-lines
         copy-media! media-names-for default-pdf-name element-tag)

;; ---------------------------------------------------------------- value trees

(struct v:num (x) #:transparent)
(struct v:str (s) #:transparent)
(struct v:sym (s) #:transparent)
(struct v:bool (b) #:transparent)
;; Text the flavor produced itself, spliced in as-is.
(struct v:raw (s) #:transparent)
(struct v:list (items) #:transparent)
(struct v:pair (a b) #:transparent)
;; `name` is canonical and kebab-cased; the flavor maps it to its own spelling.
;; `args` holds v: values and kwv entries, in the order they should appear.
(struct v:call (name args) #:transparent)
;; A keyword argument. `name` keeps the trailing `?` where Racket has one; a
;; flavor that cannot spell that drops it.
(struct kwv (name value) #:transparent)

;; A flavor renders one value to a string and knows how to lay out a call.
;; `call-open` receives the mapped function name and returns the text before the
;; first argument, e.g. "(shape-pict" or "shape_pict(".
(struct flavor (name                ; symbol, for messages
                map-fn              ; canonical name -> printed name
                render-value        ; (v flavor) -> string, for leaves
                call-open           ; printed-name -> string
                call-close          ; string appended after the last argument
                first-arg-prefix    ; between the head and the first argument
                arg-separator       ; between arguments on one line
                arg-continue        ; separator when an argument starts a new line
                keyword-prefix      ; canonical keyword name -> text before its value
                line-comment)       ; string prefix for a comment line
  #:transparent)

;; ------------------------------------------------------------------- literals

;; Measurements are rounded to a thousandth of a point. EMU divide evenly into
;; points for almost every real value, so this prints "54.0" and "172.8" rather
;; than the full float, while staying far below anything a renderer can show.
(define (num-string v)
  (cond
    ;; Counts stay counts; only measurements get a decimal point.
    [(exact-integer? v) (~a v)]
    [else
     (define r (/ (round (* 1000.0 (exact->inexact v))) 1000.0))
     (if (integer? r) (~a (inexact->exact r) ".0") (~a r))]))

;; Images are copied out under their base name; the part name would be noise.
(define current-media-names (make-parameter (hash)))
(define (media-name src) (hash-ref (current-media-names) src src))

(define current-deck-font (make-parameter "Calibri"))

;; A fresh translation lets the Rhombus `at` macro derive identity from the
;; source location. Structural edits are different: the editor already has an
;; object under its old id, so the first source form for that object keeps the
;; id explicitly. This parameter is only enabled by the syncer's insertion
;; paths, including nested children of a newly pasted group.
(define current-identity-tags? (make-parameter #f))

;; Names are already unique within a slide, made so when the deck was parsed.
(define (element-tag e)
  (define name (element-name e))
  (and (not (string=? "" name)) name))

;; The typeface most runs use, so the rest can leave it out.
(define (dominant-font d)
  (define counts (make-hash))
  (for ([e (in-list (deck-elements d))])
    (element-walk
     e (lambda (x)
         (when (and (shape? x) (shape-body x))
           (for* ([p (in-list (text-body-paras (shape-body x)))]
                  [r (in-list (para-runs p))])
             (hash-update! counts (trun-family r) add1 0))))))
  (if (zero? (hash-count counts))
      "Calibri"
      (car (argmax cdr (hash->list counts)))))

;; ---------------------------------------------------------------------- paint

(define (color-value c)
  (if (= 1.0 (rgba-a c))
      (v:call "hex" (list (v:str (rgba-hex c))))
      (v:call "hex" (list (v:str (rgba-hex c)) (kwv "alpha" (v:num (rgba-a c)))))))

;; A solid fill is written as the bare color: the runtime accepts one, and the
;; wrapper adds nothing to read.
(define (fill-value f)
  (cond
    [(not f) (v:bool #f)]
    [(solid-fill? f) (color-value (solid-fill-color f))]
    [(gradient-fill? f)
     (v:call "gradient-fill"
             (list (v:list (for/list ([s (in-list (gradient-fill-stops f))])
                             (v:pair (v:num (car s)) (color-value (cdr s)))))
                   (v:num (gradient-fill-angle f))))]
    [(image-fill? f)
     (v:call "image-fill" (list (v:call "media" (list (v:str (media-name (image-fill-src f)))))
                                (v:num (image-fill-opacity f))))]
    [(pattern-fill? f)
     (v:call "pattern-fill" (list (v:str (pattern-fill-name f))
                                  (color-value (pattern-fill-fg f))
                                  (color-value (pattern-fill-bg f))))]
    [else (v:bool #f)]))

;; The shadow a shape casts, as an argument. Its colour carries the alpha that
;; makes it a shadow rather than a black copy of the shape, so the colour comes
;; first the way a stroke's does.
(define (shadow-args e)
  (define sh (element-effect e))
  (if (shadow? sh)
      (list (kwv "shadow"
                 (v:call "make-shadow"
                         (append (list (color-value (shadow-color sh)))
                                 (if (zero? (shadow-blur sh)) '()
                                     (list (kwv "blur" (v:num (shadow-blur sh)))))
                                 (if (zero? (shadow-dist sh)) '()
                                     (list (kwv "distance" (v:num (shadow-dist sh)))))
                                 (if (zero? (shadow-dir sh)) '()
                                     (list (kwv "direction" (v:num (shadow-dir sh)))))))))
      '()))

;; Whichever of the two kinds of element carries one.
(define (element-effect e)
  (cond [(shape? e) (shape-effect e)]
        [(picture? e) (picture-effect e)]
        [else #f]))

(define (line-value l)
  (cond
    [(not (stroke? l)) (v:bool #f)]
    [else
     (v:call "make-stroke"
             (append (list (color-value (stroke-color l))
                           (kwv "width" (v:num (stroke-width l))))
                     (if (eq? 'solid (stroke-dash l)) '()
                         (list (kwv "dash" (v:sym (stroke-dash l)))))
                     (if (eq? 'flat (stroke-cap l)) '()
                         (list (kwv "cap" (v:sym (stroke-cap l)))))
                     (end-args "head" (stroke-head l))
                     (end-args "tail" (stroke-tail l))
                     (if (stroke-dash-pattern l)
                         (list (kwv "dash-pattern"
                                    (v:list (for/list ([d (in-list (stroke-dash-pattern l))])
                                              (v:pair (v:num (car d)) (v:num (cdr d)))))))
                         '())))]))

(define (end-args name e)
  (if (not e)
      '()
      (list (kwv name (v:call "line-end"
                              (list (v:sym (line-end-kind e))
                                    (v:str (line-end-width e))
                                    (v:str (line-end-length e))))))))

(define (geom-args g)
  (cond
    [(custom-geom? g)
     (list (kwv "geom"
                (v:call "custom-geom"
                        (list (v:list (for/list ([path (in-list (custom-geom-paths g))])
                                        (v:list (for/list ([cmd (in-list path)])
                                                  (v:list (map command-part cmd))))))
                              (v:num (custom-geom-w g))
                              (v:num (custom-geom-h g))))))]
    ;; A plain preset is named with #:shape; adjustments need the full geometry.
    [(null? (preset-geom-adjust g)) (list (kwv "shape" (v:str (preset-geom-name g))))]
    [else
     (list (kwv "geom"
                (v:call "preset-geom"
                        (list (v:str (preset-geom-name g))
                              (v:list (for/list ([a (in-list (preset-geom-adjust g))])
                                        (v:pair (v:str (car a)) (v:str (cdr a)))))))))]))

(define (command-part p)
  (cond
    [(symbol? p) (v:sym p)]
    [(pair? p) (v:pair (v:num (car p)) (v:num (cdr p)))]
    [(real? p) (v:num p)]
    [else (v:str (format "~a" p))]))

;; ----------------------------------------------------------------------- text

(define DEFAULT-RUN-SIZE 18.0)

(define (run-value r)
  (v:call "run"
          (append
           (list (v:str (trun-text r)))
           (filter
            values
            (list (and (not (equal? (trun-family r) (current-deck-font)))
                       (kwv "font" (v:str (trun-family r))))
                  (and (not (= DEFAULT-RUN-SIZE (trun-size r)))
                       (kwv "size" (v:num (trun-size r))))
                  (and (trun-bold? r) (kwv "bold?" (v:bool #t)))
                  (and (trun-italic? r) (kwv "italic?" (v:bool #t)))
                  (and (trun-underline? r) (kwv "underline?" (v:bool #t)))
                  (and (trun-strike? r) (kwv "strike?" (v:bool #t)))
                  (and (not (equal? black (trun-color r)))
                       (kwv "color" (color-value (trun-color r))))
                  (and (not (zero? (trun-spacing r)))
                       (kwv "spacing" (v:num (trun-spacing r))))
                  (and (not (eq? 'none (trun-caps r))) (kwv "caps" (v:sym (trun-caps r))))
                  (and (not (zero? (trun-baseline r)))
                       (kwv "baseline" (v:num (trun-baseline r)))))))))

(define (bullet-value b)
  (v:call "bullet"
          (list (v:sym (bullet-kind b))
                (if (bullet-char b) (v:str (bullet-char b)) (v:bool #f))
                (if (bullet-font b) (v:str (bullet-font b)) (v:bool #f))
                (if (bullet-size-frac b) (v:num (bullet-size-frac b)) (v:bool #f))
                (if (bullet-color b) (color-value (bullet-color b)) (v:bool #f)))))

(define (para-value p)
  (v:call "para"
          (append
           (filter
            values
            (list (and (not (eq? 'left (para-align p))) (kwv "align" (v:sym (para-align p))))
                  (and (not (zero? (para-level p))) (kwv "level" (v:num (para-level p))))
                  (and (not (zero? (para-margin-left p)))
                       (kwv "margin-left" (v:num (para-margin-left p))))
                  (and (not (zero? (para-indent p))) (kwv "indent" (v:num (para-indent p))))
                  (and (not (equal? '(percent . 1.0) (para-line-spacing p)))
                       (kwv "line-spacing" (v:pair (v:sym (car (para-line-spacing p)))
                                                   (v:num (cdr (para-line-spacing p))))))
                  (and (not (zero? (para-space-before p)))
                       (kwv "space-before" (v:num (para-space-before p))))
                  (and (not (zero? (para-space-after p)))
                       (kwv "space-after" (v:num (para-space-after p))))
                  (and (not (eq? 'none (bullet-kind (para-bullet p))))
                       (kwv "bullet" (bullet-value (para-bullet p))))))
           (map run-value (para-runs p)))))

(define (body-option-args tb)
  (filter values
          (list (and (not (eq? 'top (text-body-anchor tb)))
                     (kwv "anchor" (v:sym (text-body-anchor tb))))
                (and (text-body-anchor-center? tb) (kwv "anchor-center?" (v:bool #t)))
                (and (not (text-body-wrap? tb)) (kwv "wrap?" (v:bool #f)))
                (and (not (eq? 'none (text-body-autofit tb)))
                     (kwv "autofit" (v:sym (text-body-autofit tb))))
                (and (not (equal? default-insets (text-body-insets tb)))
                     (let ([i (text-body-insets tb)])
                       (kwv "insets" (v:call "insets" (list (v:num (insets-l i))
                                                            (v:num (insets-t i))
                                                            (v:num (insets-r i))
                                                            (v:num (insets-b i)))))))
                (and (not (zero? (text-body-rot tb)))
                     (kwv "rotate" (v:num (text-body-rot tb)))))))

(define (body-value tb)
  (v:call "body" (append (body-option-args tb) (map para-value (text-body-paras tb)))))

;; ---------------------------------------------------------------------- picts

(define (plain-rect? g)
  (and (preset-geom? g) (equal? "rect" (preset-geom-name g))
       (null? (preset-geom-adjust g))))

(define (size-args b)
  (list (kwv "width" (v:num (max 0.0 (bbox-w b)))) (kwv "height" (v:num (max 0.0 (bbox-h b))))))

(define (flip-args b)
  (filter values (list (and (bbox-flip-h? b) (kwv "flip-h?" (v:bool #t)))
                       (and (bbox-flip-v? b) (kwv "flip-v?" (v:bool #t))))))

(define (pict-value e)
  (define b (element-bbox e))
  (cond
    [(shape? e)
     (define has-text? (and (shape-body e) (not (text-body-empty? (shape-body e)))))
     (cond
       ;; A shape that carries nothing but text is a text box; saying so is
       ;; shorter and closer to how it will be edited.
       [(and has-text? (not (shape-fill e)) (not (shape-line e))
             (plain-rect? (shape-geom e)) (null? (flip-args b)))
        (define tb (shape-body e))
        (v:call "textbox" (append (size-args b) (body-option-args tb)
                                  (map para-value (text-body-paras tb))))]
       [else
        (v:call "shape-pict"
                (append (size-args b) (flip-args b)
                        (if (plain-rect? (shape-geom e)) '() (geom-args (shape-geom e)))
                        (if (shape-fill e) (list (kwv "fill" (fill-value (shape-fill e)))) '())
                        (if (shape-line e) (list (kwv "line" (line-value (shape-line e)))) '())
                        (if has-text? (list (kwv "body" (body-value (shape-body e)))) '())
                        (shadow-args e)))])]
    [(picture? e)
     (v:call "image-pict"
             (append (list (v:call "media" (list (v:str (media-name (or (picture-src e)
                                                                       MISSING-MEDIA)))))
                           (v:num (bbox-w b)) (v:num (bbox-h b)))
                     (if (picture-crop e)
                         (list (kwv "crop" (v:list (map v:num (picture-crop e)))))
                         '())
                     (if (stroke? (picture-line e))
                         (list (kwv "line" (line-value (picture-line e)))) '())
                     (if (< (picture-opacity e) 0.999)
                         (list (kwv "opacity" (v:num (picture-opacity e)))) '())
                     (shadow-args e)
                     (flip-args b)))]
    [(group? e)
     (define cb (group-child-bbox e))
     ;; Children are emitted relative to the group, which reads better and lets
     ;; the child-space keywords fall away: the parser has already flattened the
     ;; group's coordinate mapping, so the only thing left to say is the size.
     (v:call "group-pict"
             (append (size-args b)
                     (if (= (bbox-w cb) (bbox-w b)) '()
                         (list (kwv "child-width" (v:num (max 1.0 (bbox-w cb))))))
                     (if (= (bbox-h cb) (bbox-h b)) '()
                         (list (kwv "child-height" (v:num (max 1.0 (bbox-h cb))))))
                     (for/list ([c (in-list (group-children e))])
                       (element->value c (bbox-x cb) (bbox-y cb)))))]
    [(tbl? e)
     (v:call "table-pict"
             (append (size-args b)
                     (list (kwv "col-widths" (v:list (map v:num (tbl-col-widths e))))
                           (kwv "row-heights" (v:list (map v:num (tbl-row-heights e))))
                           (kwv "cells"
                                (v:list (for/list ([row (in-list (tbl-cells e))])
                                          (v:list (map cell-value row))))))))]
    [else (v:call "blank" (list (v:num (bbox-w b)) (v:num (bbox-h b))))]))

(define (cell-value cl)
  (v:call "tbl-cell"
          (list (if (and (tbl-cell-body cl) (not (text-body-empty? (tbl-cell-body cl))))
                    (body-value (tbl-cell-body cl))
                    (v:bool #f))
                (fill-value (tbl-cell-fill cl))
                (line-value (tbl-cell-line cl))
                (v:num (tbl-cell-row-span cl))
                (v:num (tbl-cell-col-span cl))
                (if (tbl-cell-merged? cl) (v:str (format "~a" (tbl-cell-merged? cl)))
                    (v:bool #f)))))

;; `ox`/`oy` are the absolute origin of the enclosing container, so a group's
;; children come out positioned relative to their group.
(define (element->value e [ox 0.0] [oy 0.0])
  (define b (element-bbox e))
  (define name (element-tag e))
  (v:call "at" (append (list (v:num (- (bbox-x b) ox)) (v:num (- (bbox-y b) oy)))
                       (if (zero? (bbox-rot b)) '()
                           (list (kwv "rotate" (v:num (bbox-rot b)))))
                       ;; Ordinary generated source uses a display name and lets
                       ;; Rhombus derive identity from this call's location. An
                       ;; editor-created object keeps its existing id for the
                       ;; one structural handoff into source.
                       (if name
                           (list (kwv (if (current-identity-tags?) "tag" "name")
                                      (v:str name)))
                           '())
                       (list (pict-value e)))))

(define (slide-background-value s)
  (fill-value (or (slide-background s) (solid-fill white))))

;; ------------------------------------------------------------------ rendering

;; One-line rendering of a value, used to decide whether it fits.
(define (render v fl)
  (cond
    [(v:call? v)
     (define name ((flavor-map-fn fl) (v:call-name v)))
     (define args (v:call-args v))
     (string-append ((flavor-call-open fl) name)
                    (if (null? args) "" (flavor-first-arg-prefix fl))
                    (string-join (for/list ([a (in-list args)]) (render a fl))
                                 (flavor-arg-separator fl))
                    (flavor-call-close fl))]
    [(kwv? v)
     (string-append ((flavor-keyword-prefix fl) (kwv-name v)) (render (kwv-value v) fl))]
    [else ((flavor-render-value fl) v fl)]))

;; Lines for `v` starting at column `ind`, packing arguments greedily so that
;; keyword pairs share a line and only the arguments that need a block get one.
(define (render-lines v fl ind width)
  (define one (render v fl))
  (define (spaces n) (make-string (max 0 n) #\space))
  (cond
    [(<= (+ ind (string-length one)) width)
     (list (string-append (spaces ind) one))]
    ;; A keyword stays attached to the form it introduces rather than stranding
    ;; itself on a line of its own.
    [(kwv? v)
     (define prefix ((flavor-keyword-prefix fl) (kwv-name v)))
     (define sub (render-lines (kwv-value v) fl (+ ind (string-length prefix)) width))
     (cons (string-append (spaces ind) prefix (string-trim (car sub) #:right? #f))
           (cdr sub))]
    [(not (v:call? v)) (list (string-append (spaces ind) one))]
    [else
     (define name ((flavor-map-fn fl) (v:call-name v)))
     (define open ((flavor-call-open fl) name))
     (define args (v:call-args v))
     ;; Continuation lines align under the first argument, unless that column is
     ;; deep enough that a plain indent reads better.
     (define aligned (+ ind (string-length open)
                        (string-length (flavor-first-arg-prefix fl))))
     (define outdent? (> aligned 48))
     (define col (if outdent? (+ ind 2) aligned))
     (define sep (flavor-arg-continue fl))
     ;; Outdenting has to take the *first* argument down with it. Shrubbery reads
     ;; indentation, and the arguments of one call all have to start at the same
     ;; column: leaving the first at the aligned column and putting the rest at a
     ;; shallower one is not deep code, it is a syntax error.
     (define acc
       (if outdent?
           (list (spaces col) (string-append (spaces ind) open))
           (list (string-append (spaces ind) open))))
     (for ([a (in-list args)] [i (in-naturals)])
       (define sub (render-lines a fl col width))
       (define head (string-trim (car sub) #:right? #f))
       (define prefix (if (zero? i) (flavor-first-arg-prefix fl) sep))
       (cond
         [(and (= 1 (length sub))
               (<= (+ (string-length (car acc)) (string-length prefix) (string-length head))
                   width))
          (set! acc (cons (string-append (car acc) prefix head) (cdr acc)))]
         [else
          (define opened (if (zero? i) acc (cons (string-append (car acc) (string-trim sep #:left? #f))
                                                 (cdr acc))))
          (set! acc (append (reverse sub) opened))]))
     (define ordered (reverse acc))
     (append (drop-right ordered 1)
             (list (string-append (last ordered) (flavor-call-close fl))))]))

;; ------------------------------------------------------------- media on disk

;; Copies every image a deck references next to `program-path`, returning the
;; part-name -> file-name map the emitters use.
(define (copy-media! d program-path media-subdir)
  (define dir (path-only (path->complete-path program-path)))
  (make-directory* dir)
  (define names (media-names-for d))
  (unless (zero? (hash-count names))
    (define out (build-path dir media-subdir))
    (make-directory* out)
    (for ([(src name) (in-hash names)])
      (define from (build-path (deck-media-dir d) src))
      (when (file-exists? from) (copy-file from (build-path out name) #t))))
  names)

(define (default-pdf-name program-path)
  (path->string (path-replace-extension (file-name-from-path program-path) ".pdf")))

;; Part names nest images under ppt/media; the emitted tree flattens them to
;; base names, disambiguating the rare case where two parts share one.
(define (media-names-for d)
  (for/fold ([taken (hash)] [out (hash)] #:result out) ([src (in-list (collect-media d))])
    (define base (path->string (file-name-from-path src)))
    (define n (hash-ref taken base 0))
    (define name
      (if (zero? n)
          base
          (let ([m (regexp-match #rx"^(.*)([.][^.]*)$" base)])
            (if m (format "~a-~a~a" (cadr m) n (caddr m)) (format "~a-~a" base n)))))
    (values (hash-set taken base (add1 n)) (hash-set out src name))))

;; The sources a program will name. A picture whose blip resolves to nothing is
;; still written -- as `media("missing.png")`, so the slide keeps its shape and
;; the file that is missing is named -- and it has to be counted here, or the
;; program says `media(...)` with no `media` defined and does not load at all.
(define MISSING-MEDIA "missing.png")

(define (collect-media d)
  (define acc '())
  (define (note! v) (set! acc (cons (or v MISSING-MEDIA) acc)))
  (for ([s (in-list (deck-slides d))])
    (when (image-fill? (slide-background s)) (note! (image-fill-src (slide-background s))))
    (for ([e (in-list (slide-all-elements s))])
      (element-walk
       e (lambda (x)
           (cond [(picture? x) (note! (picture-src x))]
                 [(and (shape? x) (image-fill? (shape-fill x)))
                  (note! (image-fill-src (shape-fill x)))]
                 [else (void)])))))
  (remove-duplicates (filter values acc)))
