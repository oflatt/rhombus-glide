#lang racket/base
;; Opaque editor identifiers derived from Rhombus source locations.
;;
;; They live in exported deck metadata, not in the user's program. Keeping the
;; spelling here gives the runtime (which creates them), the source reader
;; (which recovers the same id), and the pptx writer one definition of the
;; protocol. The writer uses both alt text and the object name because
;; LibreOffice does not reliably preserve the former.
(require file/sha1 racket/path racket/string)
(provide source-location-tag source-position-tag automatic-tag? automatic-tag-key
         automatic-tag-name)

(define PREFIX "source:")
(define DIGEST-LENGTH 40)
(define KEY-LENGTH (+ (string-length PREFIX) DIGEST-LENGTH))

(define (source-name source)
  (cond
    ;; A reader is allowed to put either a path or a string in an srcloc.
    ;; Treat a string the same way here: the static reader uses a normalized
    ;; path, and an equivalent string from the runtime must hash to that same
    ;; source identity.
    [(or (path? source) (string? source))
     (with-handlers ([exn:fail? (lambda (_e) (format "~a" source))])
       (path->string
        (simplify-path
         (path->complete-path (if (path? source) source (string->path source)))
         #f)))]
    [else (format "~a" source)]))

(define (source-position-tag source position [name #f])
  (and source position
       (string-append
        PREFIX
        (sha1 (open-input-string
               (format "~a\u0000~a" (source-name source) position)))
        (if (and (string? name) (not (string=? name "")))
            (string-append ":" name)
            ""))))

(define (source-location-tag loc [name #f])
  (and (srcloc? loc)
       (source-position-tag (srcloc-source loc) (srcloc-position loc) name)))

(define (automatic-tag? v)
  (and (string? v)
       (<= KEY-LENGTH (string-length v))
       (string-prefix? v PREFIX)
       (for/and ([c (in-string v (string-length PREFIX) KEY-LENGTH)])
         (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))
       (or (= KEY-LENGTH (string-length v))
           (char=? #\: (string-ref v KEY-LENGTH)))))

;; The source digest is identity. Anything after it is display metadata, so a
;; computed `~name:` can change without changing what source call an editor
;; object belongs to.
(define (automatic-tag-key tag)
  (and (automatic-tag? tag) (substring tag 0 KEY-LENGTH)))

(define (automatic-tag-name tag)
  (and (automatic-tag? tag)
       (let ([i KEY-LENGTH])
         (and (< i (string-length tag))
              (char=? #\: (string-ref tag i))
              (substring tag (add1 i))))))
