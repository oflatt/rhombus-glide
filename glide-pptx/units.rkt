#lang racket/base
;; Conversions between the units used in OOXML and the points used by the IR.
(provide EMU-PER-POINT EMU-PER-INCH
         emu->pt pt->emu
         hundredths->pt pt->hundredths
         angle->degrees degrees->angle
         percent->fraction fraction->percent
         string->emu-pt string->angle string->percent string->bool)

(define EMU-PER-INCH 914400)
(define EMU-PER-POINT 12700)

(define (emu->pt e) (/ (exact->inexact e) EMU-PER-POINT))
(define (pt->emu p) (inexact->exact (round (* p EMU-PER-POINT))))

;; Font sizes and line widths in `sz`/`w` attributes are hundredths of a point.
(define (hundredths->pt v) (/ (exact->inexact v) 100.0))
(define (pt->hundredths p) (inexact->exact (round (* p 100))))

;; Rotations are 60000ths of a degree, measured clockwise.
(define (angle->degrees v) (/ (exact->inexact v) 60000.0))
(define (degrees->angle d) (inexact->exact (round (* d 60000))))

;; Percentages are 1000ths of a percent.
(define (percent->fraction v) (/ (exact->inexact v) 100000.0))
(define (fraction->percent f) (inexact->exact (round (* f 100000))))

(define (parse-num s) (and s (let ([n (string->number (string-trim* s))]) (and (real? n) n))))

(define (string-trim* s)
  (let loop ([a 0] [b (string-length s)])
    (cond [(and (< a b) (char-whitespace? (string-ref s a))) (loop (add1 a) b)]
          [(and (< a b) (char-whitespace? (string-ref s (sub1 b)))) (loop a (sub1 b))]
          [else (substring s a b)])))

(define (string->emu-pt s [default #f])
  (define n (parse-num s))
  (if n (emu->pt n) default))

(define (string->angle s [default #f])
  (define n (parse-num s))
  (if n (angle->degrees n) default))

;; Percent attributes appear both as "50000" and, in some producers, as "50%".
(define (string->percent s [default #f])
  (cond
    [(not s) default]
    [(regexp-match #rx"^ *(-?[0-9.]+)% *$" s)
     => (lambda (m) (/ (string->number (cadr m)) 100.0))]
    [else (let ([n (parse-num s)]) (if n (percent->fraction n) default))]))

(define (string->bool s [default #f])
  (case s
    [("1" "true" "on") #t]
    [("0" "false" "off") #f]
    [else default]))
