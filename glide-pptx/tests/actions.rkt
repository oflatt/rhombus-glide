#lang racket/base
;; Every action a slideshow editor offers, walked through one at a time.
;;
;; The list itself is `editor-actions.rkt`, taken from the editors' own
;; surfaces. What this file does is insist that each one lands somewhere:
;;
;;   applied   written into the program
;;   reported  refused, with a reason, and nothing written
;;   noted     seen, and not an edit -- what a deck says when it says nothing
;;   ignored   deliberately invisible, like a transition
;;
;; and never in the fifth place, which is *silence*: the deck and the program
;; disagreeing with nobody told. A property that stops being compared fails
;; here rather than going quiet.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         glide-pptx/sync glide-pptx/sync-state
         "editor-actions.rkt")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-actions"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)
(define-values (program deck) (lay-down! work))
(define workdir (build-path work "w"))

;; Laid down once. Each action restores these three files rather than exporting
;; again, which is the difference between a minute and five.
(define pristine (build-path work "pristine"))
(make-directory* pristine)
(define (files) (list program deck (base-path-for program)))
(for ([f (in-list (files))]) (copy-file f (build-path pristine (file-name-from-path f)) #t))
(define (restore!)
  (for ([f (in-list (files))])
    (copy-file (build-path pristine (file-name-from-path f)) f #t)))

(define counts (make-hash))

(for ([a (in-list (catalogue))])
  (define name (act-spec-name a))
  (restore!)
  (with-check-info (['action name])
    (define landed ((act-spec-edit a) deck))
    (check-true (and landed #t) (format "~a: the edit was made" name))
    (define r (sync-once program deck #:workdir workdir #:atomic? #t))
    (define acted (filter (lambda (x) (not (eq? 'noted (sync-action-kind x))))
                          (sync-report-actions r)))
    (hash-update! counts (act-spec-outcome a) add1 0)
    (case (act-spec-outcome a)
      [(applied)
       (check-true (pair? acted) (format "~a: it was seen" name))
       (check-equal? (sync-report-skipped r) '()
                     (format "~a: refused ~s" name (map cdr (sync-report-skipped r))))
       (check-true (pair? (sync-report-applied r)) (format "~a: and written" name))
       (when (act-spec-writes a)
         (check-regexp-match (act-spec-writes a) (file->string program)
                             (format "~a: what it wrote" name)))
       (let loop ([n 1])
         (define again (sync-once program deck #:workdir workdir #:atomic? #t))
         (cond
           [(null? (sync-report-actions again)) (void)]
           [(>= n (act-spec-settles-in a))
            (fail (format "~a: still reporting ~s after ~a pass~a" name
                          (map sync-action-kind (sync-report-actions again))
                          n (if (= n 1) "" "es")))]
           [else (loop (add1 n))]))]
      [(reported)
       (check-true (pair? acted) (format "~a: it was seen" name))
       (check-equal? (sync-report-applied r) '() (format "~a: nothing written" name))
       (check-true (pair? (sync-report-skipped r)) (format "~a: and refused" name))
       (when (act-spec-says a)
         (check-regexp-match (act-spec-says a) (format "~a" (map cdr (sync-report-skipped r)))
                             (format "~a: with a reason" name)))]
      [(noted)
       (check-true (pair? (sync-report-notes r)) (format "~a: it was noted" name))
       (check-equal? (sync-report-skipped r) '() (format "~a: and did not stop a save" name))
       (check-equal? (sync-report-applied r) '() (format "~a: nor was it written" name))]
      [(ignored)
       (check-equal? (sync-report-actions r) '()
                     (format "~a: deliberately invisible" name))])))

(printf "action tests done; ~a\n"
        (string-join (for/list ([k (in-list '(applied reported noted ignored))])
                       (format "~a ~a" (hash-ref counts k 0) k))
                     ", "))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
