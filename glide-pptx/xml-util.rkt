#lang racket/base
;; Namespace-tolerant queries over the xexprs produced by `xml->xexpr`.
;;
;; Racket's XML reader keeps qualified names verbatim, so a slide's offset
;; element arrives as the symbol `a:off`. Every accessor here matches on the
;; local part of the name, which keeps the parser working across producers
;; that bind the DrawingML namespace to a different prefix.
(require racket/list racket/string xml)
(provide read-xexpr-file
         xml-element? local-name tag-is?
         elem-children child child* children children*
         xpath xpath*
         attr attr-ns attr-num
         all-text
         find-descendant find-descendants)

;; A part may begin with a byte-order mark. It is legal, Word and PowerPoint
;; both write one sometimes, and Racket's reader treats it as content and
;; refuses the document -- so it comes off first.
(define (read-xexpr-file path)
  (call-with-input-file path
    (lambda (in)
      (skip-bom! in)
      (xml->xexpr (document-element (read-xml in))))))

(define (skip-bom! in)
  (when (equal? (peek-bytes 3 0 in) (bytes #xEF #xBB #xBF))
    (read-bytes 3 in)))

(define (xml-element? x) (and (pair? x) (symbol? (car x))))

(define (local-name tag)
  (define s (symbol->string tag))
  (define i (for/last ([j (in-range (string-length s))]
                       #:when (char=? #\: (string-ref s j)))
              j))
  (if i (string->symbol (substring s (add1 i))) tag))

(define (tag-is? x name)
  (and (xml-element? x) (eq? (local-name (car x)) name)))

(define (elem-children x)
  (if (xml-element? x) (filter xml-element? (cddr x)) '()))

;; First child element named `name`, or #f.
(define (child x name)
  (for/first ([c (in-list (elem-children x))] #:when (tag-is? c name)) c))

;; Like `child`, but errors when the element is absent; for parts of the format
;; the schema makes mandatory.
(define (child* x name)
  (or (child x name)
      (error 'child* "no <~a> child of <~a>" name (and (xml-element? x) (car x)))))

(define (children x name)
  (for/list ([c (in-list (elem-children x))] #:when (tag-is? c name)) c))

;; All children in document order, which matters because it is z-order.
(define (children* x) (elem-children x))

;; (xpath sp 'spPr 'xfrm 'off) walks one named child per step.
(define (xpath x . names)
  (for/fold ([cur x]) ([n (in-list names)])
    (and cur (child cur n))))

;; Like `xpath`, but the last step collects every match.
(define (xpath* x . names)
  (cond
    [(null? names) '()]
    [else
     (define parent (apply xpath x (drop-right names 1)))
     (if parent (children parent (last names)) '())]))

(define (attr x name)
  (and (xml-element? x)
       (let ([as (cadr x)])
         (and (list? as)
              (for/first ([a (in-list as)]
                          #:when (and (pair? a) (eq? (local-name (car a)) name)))
                (cadr a))))))

;; Like `attr`, but only matches an attribute that carries a prefix. Needed
;; where an element has both an unprefixed and a namespaced attribute of the
;; same local name, as <p:sldId id=".." r:id=".."/> does.
(define (attr-ns x name)
  (and (xml-element? x)
       (let ([as (cadr x)])
         (and (list? as)
              (for/first ([a (in-list as)]
                          #:when (and (pair? a)
                                      (eq? (local-name (car a)) name)
                                      (not (eq? (local-name (car a)) (car a)))))
                (cadr a))))))

(define (attr-num x name [default #f])
  (define v (attr x name))
  (or (and v (string->number (string-trim v))) default))

;; Concatenated character data of every descendant, with <a:br/> as a newline
;; and <a:tab/> as a tab -- the two structural runs that carry no text node.
(define (all-text x)
  (define out (open-output-string))
  (let walk ([x x])
    (cond
      [(string? x) (write-string x out)]
      [(xml-element? x)
       (case (local-name (car x))
         [(br) (write-string "\n" out)]
         [(tab) (write-string "\t" out)]
         [else (for-each walk (cddr x))])]
      [else (void)]))
  (get-output-string out))

(define (find-descendant x name)
  (let/ec return
    (let walk ([x x])
      (when (xml-element? x)
        (when (tag-is? x name) (return x))
        (for-each walk (elem-children x))))
    #f))

(define (find-descendants x name)
  (define acc '())
  (let walk ([x x])
    (when (xml-element? x)
      (when (tag-is? x name) (set! acc (cons x acc)))
      (for-each walk (elem-children x))))
  (reverse acc))
