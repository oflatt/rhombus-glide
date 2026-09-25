#lang racket/base
;; The parent process, driven from both sides.
;;
;; Timing-sensitive by nature, so the assertions are about the state the loop
;; settles into rather than about how quickly it gets there.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/port
         racket/system racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-rhombus glide-pptx/export
         glide-pptx/sync glide-pptx/watch glide-pptx/runtime "deck-edit.rkt"
         (only-in glide-pptx/main default-app))

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-watch"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; --------------------------------------------------------- adapter surface

(check-true (hash-has-key? adapters 'keynote) "there is a Keynote adapter")
(check-true (hash-has-key? adapters 'powerpoint) "and a PowerPoint one")
(check-true (hash-has-key? adapters 'libreoffice) "and a LibreOffice one")
(check-equal? (app-adapter-name (adapter-named 'none)) 'none)
;; Keynote edits a .key beside the deck, and never the deck itself.
(check-equal? (path->string
               ((app-adapter-document (adapter-named 'keynote)) (string->path "/tmp/a.pptx")))
              "/tmp/a.key")
;; An app whose own format is .pptx edits the deck directly, with nothing to
;; harvest -- which is why it is the easier target.
(check-equal? ((app-adapter-document (adapter-named 'powerpoint)) (string->path "/tmp/a.pptx"))
              (string->path "/tmp/a.pptx"))
(check-exn exn:fail? (lambda () (adapter-named 'inkscape)) "an unknown app is refused")

;; The watched program is the whole local import graph, not only the root file.
(let ()
  (define dir (build-path work "source-hash"))
  (make-directory* dir)
  (define root (build-path dir "talk.rhm"))
  (define slides (build-path dir "slides.rhm"))
  (display-to-file
   "#lang rhombus\nimport: \"slides.rhm\" open\nexport: all_slides\ndef all_slides = [slide_1]\n"
   root #:exists 'replace)
  (display-to-file
   "#lang rhombus\nexport: slide_1\ndef slide_1 = 1\n"
   slides #:exists 'replace)
  (define before (program-content-hash root))
  (display-to-file
   "#lang rhombus\nexport: slide_1\ndef slide_1 = 2\n"
   slides #:exists 'replace)
  (check-not-equal? (program-content-hash root) before
                    "saving an imported source module changes the program hash"))

;; ------------------------------------------------------------ a hand-written
;; A tiny program, so the test is about the loop rather than about a deck.

(define dir (build-path work "loop"))
(make-directory* dir)
(define program (build-path dir "deck.rhm"))
(define slide-program (build-path dir "slide.rhm"))
(define pptx (build-path dir "deck.pptx"))

(display-to-file
 (string-join
  (list "#lang rhombus/and_meta"
        "import: \"slide.rhm\" open"
        "export: all_slides"
        "def all_slides = [slide_1]"
        "")
  "\n")
 program #:exists 'replace)

(define (write-program! color [before-at ""])
  (call-with-output-file slide-program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "export:"
              "  slide_1"
              "  slide_width"
              "  slide_height"
              "def slide_width = 480.0"
              "def slide_height = 270.0"
              "def slide_1 = slide_canvas("
              "  ~width: slide_width, ~height: slide_height, ~background: hex(\"FFFFFF\"),"
              before-at
              "  at(40.0, 60.0,"
              "     shape_pict(~width: 100.0, ~height: 40.0, ~shape: \"roundRect\","
              (format "                ~~fill: hex(~s)))," color)
              "  at(200.0, 60.0,"
              "     textbox(~width: 200.0, ~height: 30.0,"
              "             para(run(\"hello\", ~size: 18.0))))"
              ")"
              "")
        "\n") o))))

(void (write-program! "4472C4"))
(define box-tag (at-site-tag (first (find-at-sites program))))

;; --------------------------------------------------- one pass, deterministic

(check-true (and (parameterize ([current-watch-log void])
                   (watch-once program pptx #:adapter (adapter-named 'none)))
                 #t)
            "a single pass regenerates the deck")
(check-true (file-exists? pptx) "and the deck exists")

;; A regenerated file is not the agreed base until the editor confirms that it
;; is showing it. Otherwise the next save could come from the old document but
;; be compared with new source-derived identities.
(define refusing-reload-adapter
  (app-adapter 'refusing-reload (lambda (p) p) (lambda (_doc _pptx) #t)
               (lambda (_pptx) #f) (lambda () #t)))
(define first-base (base-path-for program))
(when (file-exists? first-base) (delete-file first-base))
(check-false (parameterize ([current-watch-log void])
               (watch-once program pptx #:adapter refusing-reload-adapter))
             "a failed editor reload fails regeneration")
(check-false (file-exists? first-base)
             "and does not advance the merge base past the editor")
(check-true (parameterize ([current-watch-log void])
              (watch-once program pptx #:adapter (adapter-named 'none)))
            "a successful reload records the base normally")

;; ------------------------------------------------------------ the whole loop

(define log-lines '())
(define (note fmt . args) (set! log-lines (cons (apply format fmt args) log-lines)))

(define watcher
  (thread (lambda ()
            (parameterize ([current-watch-log note])
              (watch-loop program pptx
                          #:adapter (adapter-named 'none)
                          #:workdir (build-path dir "w")
                          #:interval 0.15
                          #:ticks 120)))))

(define (wait-for! what pred [limit 15.0])
  (let loop ([waited 0.0])
    (cond
      [(pred) #t]
      [(>= waited limit)
       (eprintf "LOOP LOG:\n~a\n" (apply string-append (reverse log-lines)))
       (fail (format "timed out waiting for ~a" what)) #f]
      [else (sleep 0.2) (loop (+ waited 0.2))])))

;; 1. The program is saved: the deck should be rewritten.
;;
;; Waited for rather than slept through: the loop writes the deck once at
;; startup, and on a cold compile that takes longer than any sleep worth
;; writing. A program saved while it was still starting up was already in the
;; hash it started watching from, so the change it was waiting for had already
;; happened.
(void (wait-for! "the loop to say it is watching"
                 (lambda () (ormap (lambda (l) (regexp-match? #rx"watching for changes" l))
                                   log-lines))))
(define deck-before (file->bytes pptx))
(void (write-program! "ED7D31" "  // Moving this comment shifts the source identity below."))
(void (wait-for! "the deck to be regenerated"
                 (lambda () (not (equal? deck-before (file->bytes pptx))))))
(check-false (equal? deck-before (file->bytes pptx))
             "saving the program regenerated the deck")

;; 2. The deck is saved with a shape moved: the program should be patched.
(sleep 0.8)
(define source-before (file->string slide-program))
(define shifted-box-tag (at-site-tag (first (find-at-sites program))))
(check-not-equal? shifted-box-tag box-tag "the save really shifted the automatic identity")
(void (check-true (drag-in-deck! pptx 1 shifted-box-tag 150.0 90.0)
                  "the shape to drag was found in the generated deck"))

(void (wait-for! "the program to be patched"
                 (lambda () (regexp-match? #rx"at[(]150[.]0, 90[.]0"
                                            (file->string slide-program)))))
(define source-after (file->string slide-program))
(check-true (regexp-match? #rx"at[(]150[.]0, 90[.]0" source-after)
            "dragging in the deck moved the literal in the program")
(check-equal? (length (string-split source-before "\n"))
              (length (string-split source-after "\n"))
              "and changed no other line")
(check-true (regexp-match? #rx"ED7D31" source-after)
            "the program's own edit was left alone")

(void (kill-thread watcher))

(check-true (for/or ([l (in-list log-lines)]) (regexp-match? #rx"regenerating" l))
            "the loop reported regenerating the deck")
(check-true (for/or ([l (in-list log-lines)]) (regexp-match? #rx"merging" l))
            "and reported merging back")

(printf "watch tests done; artifacts under ~a\n" work)

;; The pieces the latch test needs, kept out of the test body.
(define (deck-slide-count pptx)
  (length (deck-slides (pptx->deck pptx #:workdir (make-temporary-file "wc~a" 'directory)))))

;; A merge goes through when nothing in it was refused -- which is what the
;; watch loop asks, and is not the same as not raising: a program the merge
;; cannot read on one slide refuses that slide's edits and carries on with the
;; rest, so the refusal is in the report rather than in an exception.
(define (merge-back-once program pptx workdir)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define r (sync-once program pptx #:workdir workdir #:atomic? #t))
    (null? (sync-report-skipped r))))

(define (merge-back-message program pptx workdir)
  (with-handlers ([exn:fail? exn-message])
    (define r (sync-once program pptx #:workdir workdir #:atomic? #t))
    (string-join (map cdr (sync-report-skipped r)) "\n")))

;; A session that starts on a scratch left by one that did not finish.
;;
;; The scratch holds the deck, and clearing it took the folder with it -- so the
;; deck could not be written into the folder it lives in, and the loop then read
;; a deck that was never written and died on the way up. Twice in a row was the
;; giveaway: the second run had a scratch to clear.
(let ()
  (define dir (build-path work "stale-scratch"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
          "  at(40.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"4472C4\")))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define scratch (scratch-dir-of program))
  (define pptx (build-path scratch "deck.pptx"))
  (make-directory* scratch)

  (define lines '())
  (define (note fmt . args) (set! lines (cons (apply format fmt args) lines)))
  (define (said? rx) (ormap (lambda (l) (regexp-match? rx l)) lines))
  (define (run!)
    (set! lines '())
    (parameterize ([current-watch-log note])
      (watch-loop program pptx #:width 480.0 #:height 270.0
                  #:adapter (adapter-named 'none)
                  #:workdir (build-path dir "w")
                  #:interval 0.05 #:ticks 2)))

  ;; The first session lays the deck and the base down.
  (run!)
  (check-true (file-exists? pptx) "the first session wrote the deck")
  (check-true (file-exists? (base-path-for program)) "and recorded a base")

  ;; The second finds them and clears them, which is what a session that did not
  ;; finish leaves behind.
  (run!)
  (check-true (said? #rx"clearing") "the second session cleared the scratch")
  (check-true (file-exists? pptx) "and wrote the deck again, into the folder it put back")
  (check-false (said? #rx"did not run|does not exist")
               (format "with nothing to complain about: ~a" (reverse lines))))

;; ------------------------------------- a refused merge protects the deck

;; The trap this closes: a refused merge left the editor's edits sitting there,
;; and then the obvious next move -- fixing the program as the message asks --
;; regenerated the deck and threw those edits away. So a refusal latches: while
;; it stands the deck is not rewritten, and a change on either side retries.
;;
;; A save is one thing: if any edit in it cannot be written, none of them is.
;; So the refusals used here are ones that stand -- a tag on two `at` forms,
;; which an edit could land on either of, and a colour two shapes share
;; recoloured on one of them.
(let ()
  (define dir (build-path work "stuck"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (define pptx (build-path dir "deck.pptx"))
  (define (write-program! color #:duplicate-tag? [duplicate-tag? #f])
    (call-with-output-file program #:exists 'replace
      (lambda (o)
        (write-string
         (string-join
          (append
           (list "#lang rhombus/and_meta"
                 "import:"
                 "  lib(\"glide-pptx/runtime.rhm\") open"
                 "export:"
                 "  all_slides"
                 (format "def brand = hex(~s)" color)
                 "def slide_1 = slide_canvas("
                 "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                 "  at(40.0, 60.0, ~tag: \"Box\","
                 "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: brand)),"
                 "  at(200.0, 60.0, ~tag: \"Twin\","
                 (format "     shape_pict(~~width: 100.0, ~~height: 40.0, ~~fill: brand))~a"
                         (if duplicate-tag? "," "")))
           ;; A second `at` under the same tag: an edit under it could land on
           ;; either, so the merge refuses the program rather than guessing.
           (if duplicate-tag?
               (list "  at(200.0, 60.0, ~tag: \"Box\","
                     "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"ED7D31\")))")
               '())
           (list ")"
                 "def slide_2 = slide_canvas("
                 "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                 "  at(60.0, 90.0, ~tag: \"Other\","
                 "     shape_pict(~width: 80.0, ~height: 30.0, ~fill: hex(\"70AD47\")))"
                 ")"
                 "def all_slides = [slide_1, slide_2]"
                 ""))
          "\n") o))))
  (write-program! "4472C4")

  ;; One pass to lay the deck down and record a base.
  (watch-once program pptx #:width 480.0 #:height 270.0)
  (sync-once program pptx #:workdir (build-path dir "w"))
  (check-equal? (deck-slide-count pptx) 2 "the deck starts with the program's two slides")

  ;; A slide the merge cannot read: two `at` forms answering to one tag, so an
  ;; edit could land on either. It is that slide's edits that are refused --
  ;; the rest of the deck still merges -- and the whole message comes through,
  ;; because what to do about it is not on the first line.
  (write-program! "4472C4" #:duplicate-tag? #t)
  (check-true (drag-in-deck! pptx 1 "Box" 300.0 100.0) "a shape on that slide was dragged")
  (check-false (merge-back-once program pptx (build-path dir "w"))
               "and merging that drag is refused")
  (define msg (merge-back-message program pptx (build-path dir "w")))
  (check-regexp-match #rx"Box" msg)
  (check-regexp-match #rx"distinct identities" msg "the message keeps its later lines")
  (write-program! "4472C4")

  ;; Now the trap, with the loop actually running: the refusal has to happen
  ;; inside a run and a program save has to follow it in the same run.
  (define lines '())
  (define (note fmt . args) (set! lines (cons (apply format fmt args) lines)))
  (define (said? rx) (ormap (lambda (l) (regexp-match? rx l)) lines))
  (define (wait! what pred [limit 20.0])
    (let loop ([waited 0.0])
      (cond [(pred) #t]
            [(>= waited limit) (fail (format "timed out waiting for ~a" what)) #f]
            [else (sleep 0.1) (loop (+ waited 0.1))])))

  ;; Back to agreement first: the loop refuses to start on a deck it cannot
  ;; merge, which is right but is not what this is testing.
  (watch-once program pptx #:width 480.0 #:height 270.0)
  (void (sync-once program pptx #:workdir (build-path dir "w")))
  (check-equal? (deck-slide-count pptx) 2 "the deck is whole again")

  (define runner
    (thread (lambda ()
              (parameterize ([current-watch-log note])
                (watch-loop program pptx #:width 480.0 #:height 270.0
                            #:adapter (adapter-named 'none)
                            #:workdir (build-path dir "w")
                            #:interval 0.05 #:ticks 400)))))
  ;; Startup unzips, merges and regenerates, which is slower than any sleep
  ;; worth writing -- so wait for it to say it is watching. Not for the deck to
  ;; be written: that is said before the program is read, which takes seconds,
  ;; and a deck edited inside that window is one the loop never sees change.
  ;; "watching for changes" is said after the reading and before the first hash
  ;; is taken, which is what makes it the thing to wait for.
  (wait! "the loop to finish starting" (lambda () (said? #rx"watching for changes")))
  (sleep 0.3)
  ;; Two edits in one save, one of which cannot be written: the resize can be,
  ;; and recolouring one of the two shapes that share `brand` cannot. So the
  ;; save fails whole -- and the resize is still in the deck, because the deck
  ;; was never rewritten from a program that never took it.
  (check-true (resize-in-deck! pptx 1 "Box" 200.0 80.0) "a shape was resized")
  (check-true (edit-after-tag! pptx 1 "Twin" #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                               "<a:srgbClr val=\"70AD47\"/>")
              "and one of the two shapes sharing a colour was recoloured")
  (wait! "the save to fail whole" (lambda () (said? #rx"nothing was merged")))
  ;; Then save the program, which is what the message asks for.
  (write-program! "70AD47")
  (wait! "the loop to say the program is untouched" (lambda () (said? #rx"untouched")))
  (sleep 0.4)
  (check-regexp-match #rx"cx=\"2540000\""
                      (deck-part pptx "ppt/slides/slide1.xml")
                      "the resize survived a program save -- the deck was not regenerated")
  (check-false (regexp-match? #rx"~width: 200[.]0" (file->string program))
               "and the program never took it, since the save failed whole")
  (kill-thread runner))

;; ------------------------------------------- closing the editor ends the session

;; Quitting the editor should end the session and take the scratch with it --
;; the deck, the editor's own document and the agreed base are all derived, and
;; leaving them behind is what the `.glide` folder was complained about for. The
;; last edits are merged first, though, and a merge that is refused keeps the
;; scratch: the deck then holds something the program does not.
(define (closing-adapter open-box)
  (app-adapter 'test (lambda (pptx) pptx) (lambda (doc pptx) #t) (lambda (pptx) #t)
               (lambda () (unbox open-box))))

(let ()
  (define dir (build-path work "closing"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
          "  at(40.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"4472C4\")))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define scratch (scratch-dir-of program))
  (define pptx (build-path scratch "deck.pptx"))
  (make-directory* scratch)

  (define open? (box #t))
  (define lines '())
  (define (note fmt . args) (set! lines (cons (apply format fmt args) lines)))
  (define (said? rx) (ormap (lambda (l) (regexp-match? rx l)) lines))

  ;; A session that is running, then closed.
  (define runner
    (thread (lambda ()
              (parameterize ([current-watch-log note])
                (watch-loop program pptx #:width 480.0 #:height 270.0
                            #:adapter (closing-adapter open?)
                            #:workdir (build-path dir "w")
                            #:interval 0.05 #:open-check 0.1 #:ticks 400)))))
  (let wait ([n 0])
    (cond [(said? #rx"slides written") (void)]
          [(> n 200) (fail "the session never started")]
          [else (sleep 0.1) (wait (add1 n))]))
  (check-true (file-exists? pptx) "the deck is in scratch while the session runs")

  (set-box! open? #f)
  (let wait ([n 0])
    (cond [(said? #rx"cleared") (void)]
          [(> n 200) (fail "the session never finished")]
          [else (sleep 0.1) (wait (add1 n))]))
  (check-true (said? #rx"test closed") "closing the editor is what ended it")
  (check-false (directory-exists? scratch) "and the scratch went with it")
  (check-true (file-exists? program) "the program is what is left")
  (kill-thread runner))

;; Which editor a bare `raco glide program.rhm` opens. One that is known to
;; work, or a refusal to start: opening nothing and carrying on is how a
;; session ends up reporting an AppleScript syntax error about a property,
;; which says nothing about what is wrong.
(let ()
  (cond
    [(soffice-exe)
     (check-equal? (default-app) 'libreoffice "LibreOffice, where there is one")]
    [else
     (check-exn #rx"no LibreOffice" (lambda () (default-app))
                "and otherwise it says so and stops")]))

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
