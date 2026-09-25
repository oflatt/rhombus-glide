#lang racket/base
;; Reading an Open Packaging Convention container: a .pptx is a zip whose parts
;; reference each other through side-car relationship files.
;;
;; The package is expanded to a directory once and then read from disk, because
;; images have to be real files for `racket/draw` to load them anyway.
(require racket/file racket/path racket/string racket/list
         file/unzip
         "xml-util.rkt")
(provide (struct-out package)
         open-package close-package call-with-package
         part-exists? part-path part-xexpr part-bytes
         rels-of rel-target rel-targets-by-type
         resolve-part-name)

;; `dir` is where the container was expanded; `own?` records whether we created
;; it and therefore should delete it.
(struct package (dir own? [xml-cache #:mutable] [rels-cache #:mutable]))

(define (open-package pptx-path #:into [into #f])
  (define dir (or into (make-temporary-file "pptx~a" 'directory)))
  (make-directory* dir)
  (unless (file-exists? pptx-path)
    (error 'glide "~a does not exist" pptx-path))
  (with-handlers ([exn:fail? (lambda (e) (not-a-pptx pptx-path "it is not a zip archive"))])
    (call-with-input-file pptx-path
      (lambda (in)
        (parameterize ([current-directory dir])
          (unzip in (make-filesystem-entry-reader #:exists 'replace))))))
  (define pkg (package dir (not into) (make-hash) (make-hash)))
  ;; Every OPC package has this, and a file that does not is not one -- most
  ;; often a Keynote document, which is a zip of something else entirely. Saying
  ;; so here beats a missing-part error from four frames down.
  (unless (part-exists? pkg "[Content_Types].xml")
    (not-a-pptx pptx-path "it has no [Content_Types].xml, so it is not an Office package"))
  (unless (part-exists? pkg "ppt/presentation.xml")
    (not-a-pptx pptx-path "it is an Office package but not a presentation"))
  pkg)

(define (not-a-pptx path why)
  (define keynote?
    (regexp-match? #rx"[.](key|keynote)$" (string-downcase (if (path? path)
                                                               (path->string path)
                                                               path))))
  (error 'glide
         (string-append
          "~a cannot be read as a .pptx: ~a.~a")
         path why
         (if keynote?
             (string-append
              "\n  Keynote's own format is not PowerPoint's. Export it first:"
              "\n    File > Export To > PowerPoint...,"
              "\n  or from a script:"
              "\n    osascript -e 'tell application \"Keynote\" to export document 1"
              " to POSIX file \"/path/out.pptx\" as Microsoft PowerPoint'")
             "")))

(define (close-package pkg)
  (when (package-own? pkg)
    (delete-directory/files (package-dir pkg) #:must-exist? #f)))

(define (call-with-package pptx-path proc #:into [into #f])
  (define pkg (open-package pptx-path #:into into))
  (dynamic-wind void (lambda () (proc pkg)) (lambda () (close-package pkg))))

;; Part names are absolute within the package, without a leading slash:
;; "ppt/slides/slide1.xml".
(define (part-path pkg name)
  (build-path (package-dir pkg) (string->path name)))

(define (part-exists? pkg name) (file-exists? (part-path pkg name)))

(define (part-xexpr pkg name)
  (hash-ref! (package-xml-cache pkg) name
             (lambda ()
               (unless (part-exists? pkg name)
                 (error 'part-xexpr "package has no part ~a" name))
               (read-xexpr-file (part-path pkg name)))))

(define (part-bytes pkg name) (file->bytes (part-path pkg name)))

;; ------------------------------------------------------------ relationships

;; The rels part for "ppt/slides/slide1.xml" is
;; "ppt/slides/_rels/slide1.xml.rels"; the package root's own is "_rels/.rels".
(define (rels-part-name name)
  (cond
    [(string=? name "") "_rels/.rels"]
    [else (rels-part-name* name)]))

(define (rels-part-name* name)
  (define-values (dir file _) (split-path (string->path name)))
  (define prefix (if (eq? dir 'relative) "" (path->string dir)))
  (string-append prefix "_rels/" (path->string file) ".rels"))

;; Hash of relationship id -> (cons type resolved-part-name). External targets
;; keep their URL and are marked by a #f part name.
(define (rels-of pkg name)
  (hash-ref!
   (package-rels-cache pkg) name
   (lambda ()
     (define rp (rels-part-name name))
     (cond
       [(not (part-exists? pkg rp)) (hash)]
       [else
        (for/hash ([r (in-list (children (part-xexpr pkg rp) 'Relationship))])
          (define target (attr r 'Target))
          (define external? (equal? "External" (attr r 'TargetMode)))
          (values (attr r 'Id)
                  (cons (attr r 'Type)
                        (if external? #f (resolve-part-name name target)))))]))))

;; Resolves a relationship target, which is relative to the referring part's
;; directory and may climb out of it with "../".
(define (resolve-part-name from target)
  (cond
    [(string-prefix? target "/") (substring target 1)]
    [else
     ;; `from` is "" for the package root, whose directory is the package itself.
     (define base
       (if (string=? from "")
           '()
           (let-values ([(dir _f _d) (split-path (string->path from))])
             (if (eq? dir 'relative) '() (explode-path dir)))))
     (define steps (string-split target "/"))
     (define parts
       (for/fold ([acc (reverse (map path->string base))]) ([s (in-list steps)])
         (cond [(string=? s ".") acc]
               [(string=? s "..") (if (null? acc) acc (cdr acc))]
               [else (cons s acc)])))
     (string-join (reverse parts) "/")]))

;; Part name a relationship id points at, or #f.
(define (rel-target pkg from rid)
  (define r (hash-ref (rels-of pkg from) rid #f))
  (and r (cdr r)))

;; Every target of a given relationship type, in id order so slide order from
;; presentation.xml can be applied on top.
(define (rel-targets-by-type pkg from type-suffix)
  (for/list ([(_id r) (in-hash (rels-of pkg from))]
             #:when (and (car r) (string-suffix? (car r) type-suffix)))
    (cdr r)))
