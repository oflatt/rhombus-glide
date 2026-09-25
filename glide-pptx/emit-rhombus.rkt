#lang racket/base
;; deck IR -> a Rhombus program that draws it.
;;
;; The generated program imports glide-pptx/runtime.rhm, which is Rhombus naming
;; over the same Racket runtime the Racket back end uses, so both outputs draw
;; identically and a fidelity fix lands in both at once.
;;
;; Rhombus is the more interesting target for where this is headed: its pict
;; library has animation built in, which is the part of a deck that PowerPoint
;; cannot express and code can.
(require racket/list racket/string racket/format racket/path racket/file
         "ir.rkt" "emit-common.rkt")
(provide rhombus-flavor emit-rhombus-deck write-rhombus-deck
         rhombus-element-source rhombus-slide-source)

(define LINE-WIDTH 88)

;; Canonical names are kebab-cased; Rhombus spells them with underscores.
(define (map-name n)
  (regexp-replace* #rx"-" n "_"))

;; Rhombus cannot spell a keyword ending in `?`, so the shim drops it.
(define (keyword-name kw)
  (regexp-replace* #rx"-" (regexp-replace #rx"[?]$" kw "") "_"))

(define (render-value v fl)
  (cond
    [(v:num? v) (num-string (v:num-x v))]
    [(v:str? v) (format "~s" (v:str-s v))]
    ;; A hyphen is subtraction in Rhombus, so a name that is not an identifier
    ;; there has to be written the long way: `#'short-dash` reads as `short`
    ;; minus `dash`, and the program does not compile.
    [(v:sym? v)
     (define n (format "~a" (v:sym-s v)))
     (if (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" n)
         (format "#'~a" n)
         (format "#'#{~a}" n))]
    [(v:bool? v) (if (v:bool-b v) "#true" "#false")]
    [(v:raw? v) (v:raw-s v)]
    [(v:list? v)
     (format "[~a]" (string-join (for/list ([i (in-list (v:list-items v))]) (render i fl)) ", "))]
    [(v:pair? v) (format "pair(~a, ~a)" (render (v:pair-a v) fl) (render (v:pair-b v) fl))]
    [else (format "~a" v)]))

(define rhombus-flavor
  (flavor 'rhombus map-name render-value
          (lambda (name) (string-append name "("))
          ")" "" ", " ", "
          (lambda (kw) (format "~~~a: " (keyword-name kw)))
          "// "))

;; ------------------------------------------------------------------- program

(define (line out ind fmt . args)
  (write-string (make-string (max 0 ind) #\space) out)
  (write-string (apply format fmt args) out)
  (newline out))

(define (write-element out ind e last?)
  (unless (string=? "" (element-name e))
    (line out ind "// ~a (id ~a)" (element-name e) (element-id e)))
  (define ls (render-lines (element->value e) rhombus-flavor ind LINE-WIDTH))
  (for ([l (in-list ls)] [i (in-naturals)])
    (write-string l out)
    ;; Arguments of the enclosing slide_canvas call are comma-separated.
    (when (and (= i (sub1 (length ls))) (not last?)) (write-string "," out))
    (newline out)))

;; The source for one element, as an editor's addition needs it: exactly the
;; text a fresh emit would have written for it, at the indentation it will sit
;; at. `font` is the deck font, which decides whether a run has to name its
;; typeface at all.
(define (rhombus-element-source e ind
                               #:media-names [names (hash)] #:font [font #f]
                               ;; One line, when the caller has nowhere to put a
                               ;; second: a form written around an entry that
                               ;; shares its line with the next one cannot
                               ;; continue below it, because the indentation
                               ;; that would mean is the next entry's.
                               #:width [width LINE-WIDTH]
                               #:comment? [comment? #t]
                               #:identity-tags? [identity-tags? #f])
  (parameterize ([current-media-names names]
                 [current-deck-font (or font (current-deck-font))]
                 [current-identity-tags? identity-tags?])
    (define head
      (if (or (not comment?) (string=? "" (element-name e)))
          '()
          (list (string-append (make-string (max 0 ind) #\space)
                               (format "// ~a (id ~a)" (element-name e) (element-id e))))))
    (string-join (append head (render-lines (element->value e) rhombus-flavor ind width))
                 "\n")))

;; One slide definition, as `emit-rhombus-deck` writes it -- factored out because
;; a slide pasted into the deck has to be written into the program the same way a
;; fresh translate would have written it.
(define (rhombus-slide-source s name
                              #:media-names [names (hash)]
                              #:font [font #f]
                              #:identity-tags? [identity-tags? #f]
                              #:width-expr [width-expr "slide_width"]
                              #:height-expr [height-expr "slide_height"])
  (parameterize ([current-media-names names]
                 [current-deck-font (or font (current-deck-font))]
                 [current-identity-tags? identity-tags?])
    (define out (open-output-string))
    (write-slide out s name width-expr height-expr)
    ;; The writer ends every line, including the last.
    (string-trim (get-output-string out) "\n" #:left? #f)))

(define (write-slide out s name width-expr height-expr)
  (line out 0 "// ~a" (make-string (- LINE-WIDTH 3) #\-))
  (line out 0 "// ~a~a" name
        (if (or (not (slide-name s))
                (equal? (slide-name s) (format "Slide ~a" (slide-index s))))
            "" (format ": ~a" (slide-name s))))
  (line out 0 "def ~a = slide_canvas(" name)
  (define elements (append (slide-inherited s) (slide-elements s)))
  (line out 2 "~~width: ~a, ~~height: ~a," width-expr height-expr)
  (line out 2 "~~background: ~a~a~a~a"
        (render (slide-background-value s) rhombus-flavor)
        ;; A slide the editor was told to skip says so, and keeps saying it.
        (if (slide-hidden? s) ", ~hidden: #true" "")
        ;; And a frame of a build says which build, so that moving a shape on
        ;; one frame moves it on the others: they are the same shape, drawn
        ;; again because a still slide is all a program can hold.
        (if (slide-build s) (format ", ~~build: ~s" (slide-build s)) "")
        (if (null? elements) "" ","))
  (define n (length (slide-inherited s)))
  (for ([e (in-list elements)] [i (in-naturals)])
    (when (and (positive? n) (= i 0))
      (line out 2 "// Drawn by the slide layout and master, not by this slide."))
    (when (and (positive? n) (= i n))
      (line out 2 "// The slide's own shapes."))
    (write-element out 2 e (= i (sub1 (length elements)))))
  (line out 0 ")"))

(define (emit-rhombus-deck d out
                           #:source-name [source-name #f]
                           #:media-subdir [media-subdir "media"]
                           #:media-names [media-names (hash)]
                           #:pdf-name [pdf-name "slides.pdf"]
                           #:program-name [program-name "slides.rhm"])
  (parameterize ([current-media-names media-names]
                 [current-deck-font (dominant-font d)])
    (define from (or source-name (deck-source d)))
    (line out 0 "#lang rhombus/and_meta")
    (if (equal? from 'new)
        (line out 0 "// A slideshow, written by `raco glide --new`.")
        (line out 0 "// Generated by glide-pptx from ~a." from))
    (line out 0 "// ~a slides at ~a x ~a pt."
          (length (deck-slides d)) (num-string (deck-width d)) (num-string (deck-height d)))
    (line out 0 "//")
    (line out 0 "// Every slide_N is a pict. Open this with `raco glide` and what you drag")
    (line out 0 "// in the editor is written back here; Glide supplies its editor tags from")
    (line out 0 "// source locations, so they do not have to appear in the program.")
    (newline out)
    (line out 0 "import:")
    (line out 2 "lib(\"glide-pptx/runtime.rhm\") open")
    (newline out)
    ;; Exported so the deck can be composed from another module, and so
    ;; `raco glide export` can find the slides.
    (line out 0 "export:")
    (line out 2 "slide_width")
    (line out 2 "slide_height")
    (line out 2 "all_slides")
    (for ([s (in-list (deck-slides d))])
      (line out 2 "slide_~a" (slide-index s)))
    (newline out)
    (line out 0 "def slide_width = ~a" (num-string (deck-width d)))
    (line out 0 "def slide_height = ~a" (num-string (deck-height d)))
    (newline out)
    (line out 0 "// The theme font. Runs that name no typeface use this one, so restyling")
    (line out 0 "// the whole deck is one edit.")
    (line out 0 "current_default_font(~s)" (current-deck-font))
    (newline out)
    (write-font-check out d)
    (unless (zero? (hash-count media-names))
      (newline out)
      (line out 0 "// Images sit next to this file, so the program travels as a folder.")
      (line out 0 "def media = media_lookup(~s)" media-subdir))
    (for ([s (in-list (deck-slides d))])
      (newline out)
      (write-slide out s (format "slide_~a" (slide-index s))
                   "slide_width" "slide_height"))
    (newline out)
    (line out 0 "glide_slides all_slides:")
    (line out 2 "[~a]"
          (string-join (for/list ([s (in-list (deck-slides d))])
                         (format "slide_~a" (slide-index s)))
                       ", "))
    (newline out)
    ;; Running the program shows the slides, which is what running a talk should
    ;; do. Each slide is a pict the size of the original deck, so it is scaled to
    ;; fit slideshow's page: the two aspect ratios are close but not equal, and
    ;; fitting letterboxes rather than cropping.
    (line out 0 "module main:")
    (line out 2 "import: lib(\"glide-pptx/show.rhm\") open")
    (line out 2 "show_slides(all_slides, ~~width: slide_width, ~~height: slide_height)")
    (newline out)
    (line out 0 "// The backup PDF, without opening a window:")
    (line out 0 "//   racket -l racket/base -e '(require (submod (file \"~a\") pdf))'"
          program-name)
    (line out 0 "module pdf:")
    (line out 2 "deck_to_pdf(all_slides, ~s, ~~width: slide_width, ~~height: slide_height)"
          pdf-name)
    (line out 2 "println(\"wrote \" +& ~s)" pdf-name)))

;; The typefaces this deck is set in, and what to do about them.
;;
;; racket/draw substitutes silently for a font that is not installed, and a
;; substitute is not a cosmetic difference: this runtime's line spacing is a
;; percentage of the font size, so the leading does not move when the face does
;; -- a 116pt title's two lines overlapped by 5.6pt when Helvetica Neue was
;; drawn as Noto Sans. So the program says which faces it needs and stops if
;; they are not there, rather than draw a deck nobody would recognise.
;;
;; Fonts in a `fonts` folder beside the program are loaded first, so a deck can
;; carry the faces it is set in. That has to happen before anything is drawn:
;; the drawing library builds its font map once.
(define (write-font-check out d)
  (define families (deck-font-families d))
  (define named (remove-duplicates (cons (current-deck-font) families)))
  (line out 0 "// The typefaces this deck is set in. A font file in `fonts` beside this")
  (line out 0 "// program is loaded from there; anything else has to be installed.")
  (line out 0 "//")
  (line out 0 "// Add a stand-in to any of these to accept a metric-compatible clone --")
  (line out 0 "//   [\"Helvetica Neue\", \"Nimbus Sans\"]")
  (line out 0 "// -- which is checked by measuring, since a font reports the face that was")
  (line out 0 "// asked for and not the one that was drawn.")
  (line out 0 "register_fonts(\"fonts\")")
  (cond
    [(null? named) (void)]
    [else
     (line out 0 "check_fonts(")
     (for ([f (in-list named)] [i (in-naturals)])
       (line out 2 "[~s]~a" f (if (= i (sub1 (length named))) "" ",")))
     (line out 0 ")")]))

(define (write-rhombus-deck d path
                            #:source-name [source-name #f]
                            #:media-subdir [media-subdir "media"]
                            #:pdf-name [pdf-name #f])
  (define names (copy-media! d path media-subdir))
  (call-with-output-file path #:exists 'replace
    (lambda (out)
      (emit-rhombus-deck d out
                         #:source-name source-name
                         #:media-subdir media-subdir
                         #:media-names names
                         #:pdf-name (or pdf-name (default-pdf-name path))
                         #:program-name (path->string (file-name-from-path path)))))
  path)
