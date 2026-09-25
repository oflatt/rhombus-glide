#lang racket/base
;; The fonts a deck asks for, and whether this machine can draw them.
;;
;; A deck names typefaces; a program that draws it needs them. racket/draw
;; substitutes silently when one is missing, and a substitute with different
;; metrics is not a cosmetic difference here: line spacing is a percentage of
;; the font size, so the leading does not move when the face does -- only the
;; ink height does, and a 116pt title's two lines overlap.
;;
;; So two things live here. A program can carry its fonts beside it, in which
;; case they are registered before anything is drawn; and it can say which
;; families it needs, in which case it refuses to run on substitutes rather
;; than draw a deck nobody would recognise.
(require racket/list racket/string racket/class racket/draw racket/file
         racket/path racket/promise racket/system racket/port ffi/unsafe)

(provide register-font-file! register-fonts! font-resolves? check-fonts!
         font-file-for family-substituted?)

;; ---------------------------------------------------------------- registering

;; Adding a font to the process rather than to the machine: the file is used for
;; drawing from here on, and nothing outside this process sees it.
;;
;; fontconfig is how that is done wherever pango is drawing through it, which is
;; every platform we can test. `FcConfigAppFontAddFile` against the current
;; configuration (#f) is the whole of it. Where there is no fontconfig this
;; says so by answering #f, and the caller's own check is what then reports the
;; missing font -- silently drawing with a substitute is the one thing this
;; module exists to prevent.
(define fontconfig
  (delay/sync
   (with-handlers ([exn:fail? (lambda (_) #f)])
     (ffi-lib "libfontconfig" '("1" "")))))

(define add-file
  (delay/sync
   (let ([lib (force fontconfig)])
     (and lib
          (get-ffi-obj "FcConfigAppFontAddFile" lib (_fun _pointer _string -> _bool)
                       (lambda () #f))))))

;; #t when the file was added, #f when it could not be.
(define (register-font-file! path)
  (define f (force add-file))
  (and f (file-exists? path)
       (f #f (path->string (path->complete-path path)))))

;; Every font file in `dir`, if there is one. Answers the number registered,
;; or #f where fonts cannot be registered at all.
;;
;; A relative folder is beside the program that asked for it, not beside
;; whatever directory it was run from -- the same rule the images follow, and
;; for the same reason: a program travels as a folder.
(define (register-fonts! dir0)
  (define dir
    (and dir0
         (let ([p (if (string? dir0) (string->path dir0) dir0)])
           (if (absolute-path? p)
               p
               (build-path (or (current-load-relative-directory) (current-directory)) p)))))
  (cond
    [(not (force add-file)) #f]
    [(not (and dir (directory-exists? dir))) 0]
    [else
     (for/sum ([f (in-list (directory-list dir #:build? #t))]
               #:when (regexp-match? #rx"(?i:[.](ttf|otf|ttc|otc|pfb))$" (path->string f)))
       (if (register-font-file! f) 1 0))]))

;; ------------------------------------------------------------------ resolving

;; A name no font has, whose metrics are therefore the machine's default
;; substitute. Any family that measures the same as this one is not installed:
;; it is being substituted, which is the case this module exists to catch.
(define MISSING-NAME "glide-pptx no such family 8f3a")

;; Long enough that two faces of different widths cannot agree by accident.
(define PROBE "Efficient Extraction from Effectful E-Graphs 0123456789")

(define probe-dc
  (delay/sync (new bitmap-dc% [bitmap (make-bitmap 1 1)])))

(define (probe-width family)
  (define dc (force probe-dc))
  (define f (make-font #:face family #:family 'default #:size 64.0 #:size-in-pixels? #t))
  (define-values (w h d a) (send dc get-text-extent PROBE f #t))
  w)

;; Whether asking for `family` gets a face of its own rather than the substitute
;; every missing name gets. Measured rather than looked up in `get-face-list`,
;; because a font registered from a file draws but does not appear in that list.
(define (font-resolves? family)
  (or (and (member family (get-face-list)) #t)
      (not (= (probe-width family) (probe-width MISSING-NAME)))))

;; Whether `family` resolves to something other than `stand-in` -- which is what
;; a declared stand-in has to be checked for: a `font%` reports the face that was
;; asked for, not the one that was drawn, so the only way to know is to measure.
(define (family-substituted? family stand-in)
  (not (< (abs (- (probe-width family) (probe-width stand-in))) 0.5)))

;; -------------------------------------------------------------------- the check

;; `spec` is a list of (family) or (family stand-in): a family with no stand-in
;; must itself be installed, and one with a stand-in may resolve to that instead
;; -- which is how a deck set in a typeface nobody can license is drawn at all.
;; Raises with what is missing and what to do about it.
(define (check-fonts! spec #:who [who 'glide])
  (for ([entry (in-list spec)])
    (define family (if (pair? entry) (first entry) entry))
    (define stand-in (and (pair? entry) (> (length entry) 1) (second entry)))
    (unless (font-resolves? family)
      (cond
        [(not stand-in)
         (error who (string-append
                     "required font is not installed\n"
                     "  font: ~a\n"
                     "  Install it, or put the file in the `fonts` folder beside\n"
                     "  this program, where it is loaded without being installed.")
                family)]
        [(not (font-resolves? stand-in))
         (error who (string-append
                     "required font is not installed, and neither is its stand-in\n"
                     "  font: ~a\n"
                     "  stand-in: ~a")
                family stand-in)]
        [(family-substituted? family stand-in)
         (error who (string-append
                     "font resolves to something other than the stand-in it declares\n"
                     "  font: ~a\n"
                     "  stand-in: ~a\n"
                     "  Alias it, e.g. in ~~/.config/fontconfig/conf.d/60-~a.conf,\n"
                     "  or put the real font in the `fonts` folder beside this program.")
                family stand-in
                (string-downcase (string-replace family " " "-")))]
        [else (void)]))))

;; ------------------------------------------------------------------- bundling

;; The file fontconfig would draw `family` from, so a program can be given a
;; copy of the fonts it names. #f when there is no fontconfig to ask, or when
;; the answer is a substitute rather than the family itself.
(define (font-file-for family)
  (define fc-match (find-executable-path "fc-match"))
  (and fc-match
       (let ([out (open-output-string)])
         (parameterize ([current-output-port out]
                        [current-error-port (open-output-nowhere)])
           (system*/exit-code fc-match "--format=%{file}\t%{family}" family))
         (define line (string-trim (get-output-string out)))
         (define parts (string-split line "\t"))
         (and (= 2 (length parts))
              ;; fc-match always answers something, so the answer only counts
              ;; when the family it names is the one that was asked for.
              (member (string-downcase family)
                      (map string-downcase (string-split (second parts) ",")))
              (file-exists? (first parts))
              (first parts)))))
