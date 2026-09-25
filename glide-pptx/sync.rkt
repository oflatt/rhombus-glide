#lang racket/base
;; Two-way sync between a Pict program and a .pptx.
;;
;; Editing the program and editing the deck alternate, so both sides drift
;; between syncs. That is a three-way merge against a base -- the state both
;; sides agreed on last time -- not two one-way converters.
;;
;; What makes it stable is an ownership rule that falls out of the intent:
;; PowerPoint owns geometry, because that is what you opened it to change, and
;; code owns everything else. With the two sides owning disjoint properties the
;; ordinary cycle has no conflicts at all.
;;
;; Nothing here ever restructures the program. Every edit is one of two things:
;; replace a numeric or string literal, or wrap an expression in a known form.
(require racket/list racket/set racket/string racket/format racket/file racket/path
         racket/math racket/port racket/treelist racket/promise pict
         (only-in racket/draw get-face-list)
         (only-in shrubbery/parse parse-all)
         "ir.rkt" "draw-ir.rkt" "parse.rkt" "semantic.rkt" "sync-state.rkt"
         (only-in "runtime.rkt" current-media-base current-default-font
                  number-on slide-numbers? set-slide-numbers! forget-bitmaps!
                  slide-manifest-ref slide-spec-source slide-spec-shown)
         (only-in "source-tag.rkt" source-position-tag automatic-tag? automatic-tag-key
                  automatic-tag-name)
         (only-in "emit-common.rkt" media-names-for dominant-font)
         (only-in "emit-rhombus.rkt" rhombus-element-source rhombus-slide-source))
(provide (struct-out sync-action) (struct-out sync-report)
         program-slide-states deck-slide-states load-program-picts
         validate-program-picts! record-program-base!
         set-stage-slides! current-slide-origins ask-slide-numbers!
         match-elements merge-states
         apply-actions! sync-once
         find-at-sites find-program-sites program-source-files
         (struct-out at-site) (struct-out slide-site) (struct-out rng)
         (struct-out program-layout) (struct-out name-list) (struct-out name-entry)
         base-path-for format-sync-report current-keep-work?)

;; Whether scratch directories are left behind for inspection.
(define current-keep-work? (make-parameter #f))

;; kind: 'moved, 'resized, 'retext, 'restyle, 'restacked, 'reordered,
;; 'repainted, 'conflict, 'added, 'removed, 'grouped, 'added-slide,
;; 'removed-slide, 'ambiguous
;; `prior` is where the program currently draws the element, which is what a
;; correction for a computed position is measured against. It is #f when the
;; program has no element by that name.
(struct sync-action (kind tag slide detail prior) #:transparent)
;; `skipped` pairs each action the merge could not make with the reason. Knowing
;; an edit was refused, and why, is the whole difference between "nothing moved"
;; and "your drag was thrown away".
;; `notes` are differences the merge saw and did not act on, because they are
;; not assertions: an editor that states nothing leaves the format's defaults
;; standing, and those are not edits. They belong in the report and must not
;; stop a save, which is why they are neither applied nor skipped.
(struct sync-report (actions applied skipped notes base-written? deck-behind?) #:transparent)

;; ------------------------------------------------------- reading both sides

;; Loading a program's slides, which three callers need and each got slightly
;; wrong on its own: the binding is `all_slides` in Rhombus and `all-slides` in
;; Racket, and a Rhombus `List` is a treelist rather than a list.
;;
;; The load happens in a fresh namespace. A sync patches the source and then
;; reads it again to record the new agreed state, and `dynamic-require` hands
;; back the module instance it already has -- so without this the second read
;; returns the state from before the patch and the sync never converges. The
;; runtime and `pict` are attached rather than re-instantiated, so the picts that
;; come back are the same struct types this module knows, and the media base is
;; the same parameter.
;; Which program was read last, so a read of a different one can let go of the
;; pictures the last one drew.
(define last-program-read (box #f))

;; The one namespace every read takes its modules from: the first program's,
;; kept only if that program started a GUI. See `load-program-picts`.
(define gui-program-namespace (box #f))

;; The frames a slide settles on, asked for in the program's own namespace.
;;
;; It has to be that namespace: a `Pict` is a struct type, and a rhombus/pict
;; instantiated separately here would have a different one -- so the test for
;; "is this an animated slide" would answer no about every slide, silently, and
;; nothing would be settled at all. Loaded on demand, because a deck that does
;; not animate should not pay for rhombus/pict.
;; The pages an animated slide becomes, rather than the one it comes to rest on.
;; See `stage_frames`.
(define (stage-frames v)
  (define f (dynamic-require '(lib "glide-pptx/settle.rhm") 'stage_frames))
  (define l (f v))
  (filter pict? (if (list? l) l (treelist->list l))))

(define (settle-frames v)
  (define f (dynamic-require '(lib "glide-pptx/settle.rhm") 'settle_frames))
  (define l (f v))
  (if (list? l) l (treelist->list l)))

;; The frame of an animated slide that shows the most of it. A slide's stages
;; each add something, so that is usually the last; a slide whose last beat
;; fades something out settles on an earlier one, and this finds it rather than
;; assuming. Ties go to the earliest, which is where a build starts from.
;;
;; "The most of it" counts everything an edit could be written back to -- the
;; elements a canvas places and the picts the program names -- so a slide that
;; draws named things over its canvas as it goes settles on a frame that has
;; them. See `addressable-count`.
(define (settle v)
  (define frames (filter pict? (settle-frames v)))
  (cond
    [(null? frames) v]
    [(null? (cdr frames)) (car frames)]
    [else (argmax addressable-count frames)]))

;; A slide may be written as a function of no arguments, so that a talk which
;; starts part way through never builds the slides it skipped. Everything but
;; the show wants the picture, so this is where they are called.
;;
;; And a slide that has been given stages is an animated `Pict` rather than a
;; pict: settling it gives back a pict, with the canvases it was built from
;; still inside. See `settle.rhm`.
(define (force-slide s)
  (define v (slide-value s))
  (if (pict? v) v (settle v)))

;; A slide as the program hands it over: called if it is a function, and not
;; settled -- a stage expansion needs the animation itself.
(define (slide-value s)
  (if (and (procedure? s) (procedure-arity-includes? s 0)) (s) s))

;; Whether a deck holds one slide per stage.
(define stage-slides? (box #f))
(define (set-stage-slides! [on? #t]) (set-box! stage-slides? (and on? #t)))

;; Which slide of the program each slide of the deck came from, 1-based, as the
;; last read left it. Without stages that is 1, 2, 3 ...; with them, a slide
;; given three stages is three slides of the deck and all three of them are that
;; one slide of the program -- which is what says where an edit made on the
;; third of them is to be written.
(define slide-origins (box '()))
(define (current-slide-origins) (unbox slide-origins))

;; The scope for each slide of the deck. One `all_slides` entry is one scope,
;; and with stages one entry is several slides; everything that writes an edit
;; finds its scope by the slide's position, so stretching this one list is what
;; carries stages through all of it.
(define (scopes-by-deck-slide scopes)
  (define origins (unbox slide-origins))
  (cond
    [(or (not scopes) (null? origins)) scopes]
    [else (for/list ([j (in-list origins)])
            (and (<= 1 j (length scopes)) (list-ref scopes (sub1 j))))]))

;; And whether to draw the number in the corner whatever the program said. The
;; program's own `set_slide_numbers` is a call in its body, which a talk may
;; make inside `module main` -- where the show runs it and nothing else does --
;; so the command line can ask for the same thing.
(define numbers-asked? (box #f))
(define (ask-slide-numbers! [on? #t]) (set-box! numbers-asked? (and on? #t)))

;; The number in each slide's corner, when the program asked for one. Put on
;; after forcing and settling, because it is drawn as an ordinary pict and
;; composing one onto a slide that still animates would flatten it -- the show
;; composes its own with Rhombus's `overlay`, which does not.
;;
;; By position in the whole list, including the slides a canvas says to skip:
;; those are exported, with `show="0"`, and the show counts them too, so a
;; number that skipped them would differ on the two sides.
;; Numbered by slide and not by page: every page of one animated slide carries
;; the same number in the show, because the number is composed onto the slide
;; before slideshow makes a page of each epoch.
(define (numbered vs)
  (define want? (or (unbox slide-numbers?) (unbox numbers-asked?)))
  (define origins '())
  (define out
    (append*
     (for/list ([v (in-list vs)] [n (in-naturals 1)])
       (define pages
         (cond
           [(not (unbox stage-slides?)) (list (force-slide v))]
           [else (let ([frames (stage-frames (slide-value v))])
                   ;; A slide that animates nothing has no epochs to expand, and
                   ;; a value `stage_frames` cannot read is settled as before.
                   (if (null? frames) (list (force-slide v)) frames))]))
       (set! origins (append origins (for/list ([_ (in-list pages)]) n)))
       (if want? (for/list ([p (in-list pages)]) (number-on p n)) pages))))
  (set-box! slide-origins origins)
  out)

;; Can this namespace hand its racket/gui/base to another one? Declared is not
;; enough: expansion loads the module without running it, and only an
;; instantiated module has an instance to share.
(define (gui-instantiated? ns)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (namespace-attach-module ns 'racket/gui/base (make-base-empty-namespace))
    #t))

;; Source identities come from the `at` macro itself. Reading the real module
;; keeps imports, source locations, and the runtime manifest aligned; no source
;; rewrite or synthetic module is involved.
(define (require-program full name)
  (dynamic-require `(file ,(path->string full)) name (lambda () #f)))

(define (load-program-picts program-path #:named [named #f])
  (define full (path->complete-path program-path))
  (define ns (make-base-empty-namespace))
  ;; Everything comes from one registry, and it is the same one every time.
  ;;
  ;; A program whose helpers import slideshow pulls in racket/gui, and that
  ;; cannot be instantiated twice in one process -- while the watch loop reads
  ;; the program again on every save. So once a program has started a GUI, that
  ;; is the namespace every later read takes its modules from. Taking the picts
  ;; from here and the GUI from there does not work: the two registries
  ;; disagree about the modules underneath them, and the attach fails rather
  ;; than mixing them -- which left the GUI to be started a second time.
  ;;
  ;; One fixed namespace rather than the last one read into. Attaching chains
  ;; the registries together, so reading from the last one kept every namespace
  ;; before it alive: a talk with two helper modules of its own leaked ten
  ;; megabytes of compiled code per save, which a long afternoon of dragging
  ;; things around turns into gigabytes.
  (define from (or (unbox gui-program-namespace) (current-namespace)))
  (for ([m (in-list '(pict glide-pptx/runtime glide-pptx/tagged glide-pptx/ir))])
    (namespace-attach-module from m ns))
  ;; And the GUI itself, from the namespace of a program that started one.
  ;; Nothing is attached when none has, which is the usual case and has to stay
  ;; that way: translating a deck should never start a GUI.
  ;;
  ;; `slideshow` is in the list for a reason worth stating. It cannot be
  ;; attached from the tool's own namespace, because instantiating it reads the
  ;; command line and brings up the GUI machinery, which translating a deck
  ;; must never do. But once a program has instantiated it, it must be attached
  ;; from there: it is pinned by the `racket/gui` it registers with, and
  ;; `racket/gui` is shared, so a fresh copy per read is a copy that is never
  ;; released. That was ten megabytes a read -- a save, in the watch loop --
  ;; and a long afternoon of dragging things around turns that into gigabytes.
  (define gui-shared?
    (and (unbox gui-program-namespace)
         (with-handlers ([exn:fail? (lambda (_e) #f)])
           (namespace-attach-module from 'racket/gui/base ns)
           ;; A program can start a GUI without slideshow, and then has no
           ;; slideshow to give.
           (for ([m (in-list '(racket/draw slideshow slideshow/base))])
             (with-handlers ([exn:fail? void])
               (namespace-attach-module from m ns)))
           #t)))
  (define names (if named (list (string->symbol named)) '(all_slides all-slides)))
  ;; Whatever the last program asked for is not what this one asks for. The
  ;; numbering is a switch the program throws as it loads, and the box it throws
  ;; is shared with everything that has already read a program in this process
  ;; -- so it starts off, and a program that wants numbers says so again.
  (set-slide-numbers! #f)
  ;; A different program draws different pictures, and the ones the last program
  ;; drew will not be asked for again. The same program keeps its own: that is
  ;; the watch loop, reading on every save.
  (unless (equal? (unbox last-program-read) (path->string full))
    (forget-bitmaps!)
    (set-box! last-program-read (path->string full)))
  ;; The namespace kept is the one a later read takes its GUI from, and that
  ;; asks for an instance rather than a declaration: expanding a program loads
  ;; racket/gui/base without running it, so a program that drew no slide of its
  ;; own leaves the module declared with no instance behind it. Keeping that
  ;; one left the next read to start a GUI of its own -- and the read after
  ;; that to start a second, which is the one that raises.
  ;;
  ;; Kept whether the program got to the end or not: one that fails part way
  ;; through may already have started a GUI, and the next read has to take that
  ;; one rather than start a second. A program that started none keeps nothing,
  ;; which is the usual case -- translating, exporting, syncing.
  (define (keep-gui-namespace!)
    (when (and (not gui-shared?) (gui-instantiated? ns))
      (set-box! gui-program-namespace ns)))
  (define found
    (dynamic-wind
     void
     (lambda ()
      (parameterize ([current-media-base (path-only full)] [current-namespace ns]
                   ;; A program that imports slideshow reads the command line
                   ;; when it loads, and refuses anything that is not a single
                   ;; module file -- so `raco glide talk.rhm --app none` failed
                   ;; to load the program it was given. The arguments are this
                   ;; command's, not the program's.
                   [current-command-line-arguments (vector)])
      (define v (for/or ([name (in-list names)])
                  (require-program full name)))
      (define manifest (and v (slide-manifest-ref v)))
      (define manifest-slides
        (and manifest
             (for/list ([spec (in-list (cond [(list? manifest) manifest]
                                             [(treelist? manifest) (treelist->list manifest)]
                                             [else '()]))])
               (slide-spec-shown spec))))
      ;; Settled here, inside the program's namespace, for the reason
      ;; `settle-frames` gives.
      (define shown (or manifest-slides v))
      (cond
        ;; Numbered here, so the deck carries the number the show draws. After
        ;; forcing and settling, because the number is drawn as an ordinary pict
        ;; and composing one onto a slide that still animates would flatten it.
        ;; By position in the whole list, including the slides a canvas says to
        ;; skip: those are exported, with `show="0"`, and the show counts them
        ;; too, so a number that skipped them would differ on the two sides.
        [(list? shown) (numbered shown)]
        ;; A Rhombus `[...]` is a treelist, which is what a talk's `all_slides`
        ;; is -- so this branch is the one a hand-written talk takes.
        [(treelist? shown) (numbered (treelist->list shown))]
        [else (set-box! slide-origins '(1)) (force-slide shown)])))
     keep-gui-namespace!))
  (cond
    [(list? found) found]
    [(pict? found) (list found)]
    [else
     (error 'glide
            (string-append "~a provides no list of slide picts.\n"
                           "  Expected a provided `all_slides` (or `all-slides`),"
                           " or a name passed with --slides.")
            program-path)]))

(define (picts->slide-states picts)
  (for/list ([p (in-list picts)] [i (in-naturals 1)])
    (define w (pict-width p)) (define h (pict-height p))
    (define pg (pict->page p w h))
    (items->slide-state i w h (display-page-items pg)
                        #:background (display-page-background pg)
                        #:hidden? (display-page-hidden? pg))))

(define (program-slide-states program-path)
  (picts->slide-states (load-program-picts program-path)))

(define (deck-slide-states pptx-path #:workdir [workdir #f])
  (define dir (or workdir (make-temporary-file "syncdeck~a" 'directory)))
  ;; Which names the deck states as ours, collected as it is read and consulted
  ;; as the states are built. The merge is the one caller that has to tell the
  ;; program's elements from the ones the editor made itself: see
  ;; `current-slide-tag-names`.
  (define stated (make-hash))
  (parameterize ([current-slide-tag-names stated])
    (deck->slide-states (pptx->deck pptx-path #:workdir dir))))

;; ------------------------------------------------------------------ matching

;; Matches `now` against `base` within one slide. Tags come first, because they
;; are exact when the editor preserved them; a signature match picks up the rest
;; and is what makes this work when they were stripped.
;;
;; Returns (values pairs unmatched-now unmatched-base), pairs as (now . base).
(define MATCH-LIMIT 8.0)
;; And how much for a pair the tags disagree about: under the 2.0 a different
;; colour costs and the 6.0 different words cost, so a dragged or resized shape
;; that lost its alt text is still itself and a new shape is not.
(define LOST-TAG-LIMIT 1.0)

;; Automatic tags may carry a readable name after their source digest. That
;; suffix is editor metadata, not identity: in particular a computed `~name:`
;; is known only when the program runs, while source recovery knows the same
;; call by its digest alone.
(define (tag-key tag) (or (and tag (automatic-tag-key tag)) tag))
(define (same-tag? a b) (equal? (tag-key a) (tag-key b)))

;; Generated frames of one PowerPoint build contain separate source `at` calls,
;; so their automatic digests differ, but the original shape name is retained
;; as metadata on every frame. Within a build only, that name identifies the
;; repeated shape whose edit is deliberately carried across the frames.
(define (same-build-tag? a b)
  (or (same-tag? a b)
      (let ([an (and a (automatic-tag-name a))]
            [bn (and b (automatic-tag-name b))])
        (and an bn (string=? an bn)))))

(define (match-elements now base #:slide-size [size 1000.0])
  ;; By tag, several deep: one `at` in a loop draws several elements under one
  ;; tag, so a tag can name a family on both sides. Families are paired in
  ;; z-order, which is the order the code drew them in.
  (define (by-tag l)
    (for/fold ([h (hash)] #:result (for/hash ([(k v) (in-hash h)])
                                    (values k (sort (reverse v) < #:key el-state-z))))
              ([e (in-list l)] #:when (el-state-tag e))
      (hash-update h (tag-key (el-state-tag e)) (lambda (v) (cons e v)) '())))
  (define base-by-tag (by-tag base))
  (define used (make-hasheq))
  (define matched-now (make-hasheq))
  (define tag-pairs
    (append*
     (for/list ([(tag ns) (in-hash (by-tag now))])
       (define bs (hash-ref base-by-tag tag '()))
       ;; Only when the family is the same size on both sides. If one member was
       ;; deleted, pairing by position would slide every survivor onto the wrong
       ;; base element and read as a move -- so the whole family goes to the
       ;; signature matcher, which pairs each survivor with itself and leaves the
       ;; missing one to be reported as missing.
         (cond
           [(= (length ns) (length bs))
          (for/list ([n (in-list ns)] [b (in-list bs)])
            (hash-set! used b #t)
            (hash-set! matched-now n #t)
            (cons n b))]
         [else '()]))))
  ;; LibreOffice normally keeps our identity in `descr`, but drops it from a
  ;; group and can drop it from a shape it reconstructs while editing. It does
  ;; preserve the object-list name. Recover by that name, only from an untagged
  ;; editor object of the same kind, and only when the name is unique and the
  ;; size is still plausible. This also matters for objects a program draws
  ;; without an `at`: a LibreOffice no-op can change their text metrics enough
  ;; to defeat the signature matcher, but it leaves their names alone.
  (define (recoverable-base-name b)
    (or (and (el-state-tag b)
             (or (automatic-tag-name (el-state-tag b))
                 (and (not (automatic-tag? (el-state-tag b)))
                      (el-state-tag b))))
        (el-state-name b)))
  (define (name-geometry-plausible? n b)
    (or (eq? 'group (el-state-kind b))
        (and (or (text-driven-size? b 'w)
                 (< (abs (- (el-state-w n) (el-state-w b))) (* 0.05 size)))
             (or (text-driven-size? b 'h)
                 (< (abs (- (el-state-h n) (el-state-h b))) (* 0.05 size))))))
  (define name-pairs
    (filter
     values
     (for/list ([n (in-list now)]
                #:when (and (not (hash-ref matched-now n #f))
                            (not (el-state-tag n))
                            (el-state-name n)))
       (define name (el-state-name n))
       (define candidates
         (filter (lambda (b)
                   (and (not (hash-ref used b #f))
                        (eq? (el-state-kind n) (el-state-kind b))
                        (equal? name (recoverable-base-name b))
                        ;; LibreOffice can reuse a deleted object's name for a
                        ;; newly drawn shape. Equal names alone are therefore
                        ;; not enough; its size must still plausibly be the
                        ;; same object. Position is deliberately free, because
                        ;; detecting a drag is why matching exists.
                        (name-geometry-plausible? n b)))
                 base))
       (define now-count
         (count (lambda (other)
                  (and (not (el-state-tag other))
                       (eq? (el-state-kind n) (el-state-kind other))
                       (equal? name (el-state-name other))))
                now))
       (cond
         [(and (= 1 now-count) (= 1 (length candidates)))
          (define b (first candidates))
          (hash-set! used b #t)
          (hash-set! matched-now n #t)
          (cons n b)]
         [else #f]))))
  (define pairs (append tag-pairs name-pairs))
  (define rest-now (filter (lambda (n) (not (hash-ref matched-now n #f))) now))
  (define rest-base (filter (lambda (b) (not (hash-ref used b #f))) base))
  ;; Whether the editor kept our alt text -- asked of each kind on its own,
  ;; because an editor treats a kind consistently and treats the kinds
  ;; differently. LibreOffice usually keeps it on shapes and pictures, drops it
  ;; on groups and connectors, and can drop every tag in a shape tree it has
  ;; reconstructed. Asked once for the whole slide, a group that came back
  ;; without its tag looked like a group that had been deleted, with a new one
  ;; in its place: retyping a word inside a group reported the group deleted and
  ;; a flattened copy added, and the retyping itself was lost.
  ;;
  ;; Where a kind's tags did come back, a tagged element must not be paired with
  ;; a shape the editor made itself. A new text box is not the box the same
  ;; sitting deleted, however alike the two look, and LibreOffice numbers a new
  ;; one after the last box it numbered -- so the new one arrives wearing the
  ;; deleted one's name. Paired, the two report as one element resized and
  ;; restyled and retyped beyond recognition, and the deletion and the addition
  ;; are both lost. Apart, they report as what they are.
  (define kept-tags-of
    (for/hash ([kind (in-list (remove-duplicates (map el-state-kind base)))])
      (values kind
              (for/or ([n (in-list now)])
                (and (eq? kind (el-state-kind n))
                     (el-state-tag n)
                     (hash-ref base-by-tag (el-state-tag n) #f)
                     #t)))))
  ;; A tagged element that came back untagged is held to a much closer likeness
  ;; than the rest. Not refused outright: LibreOffice keeps our alt text on a
  ;; shape and loses it on a connector, so an element can lose its tag and still
  ;; be the same element -- and it will be, if it is otherwise the same shape.
  ;; What it may not be is a shape whose words or whose colour are different as
  ;; well. That is the editor's own new shape wearing a name it invented, and
  ;; the limit that lets it through reads a deletion and an addition as one
  ;; element changed past recognition, losing both.
  ;;
  ;; Only that way round. An untagged element that came back tagged is one the
  ;; program has since learned to place: the deck is written again from the
  ;; program after every merge, and the shape added last time carries the tag of
  ;; the `at` form written for it.
  (define (limit-for n b)
    (if (or (not (el-state-tag b))
            (el-state-tag n)
            ;; With no readable name there is no metadata left to consult, so
            ;; a kind for which this editor strips all tags must still fall
            ;; back to appearance. A named source object is different: the
            ;; unique-name pass above already recovered it. Pairing another,
            ;; nameless object to it by a loose signature would turn a delete
            ;; plus an addition into a many-property edit of the deleted box.
            (and (not (automatic-tag-name (el-state-tag b)))
                 (not (hash-ref kept-tags-of (el-state-kind b) #f))))
        MATCH-LIMIT
        LOST-TAG-LIMIT))
  ;; Everything left is matched by how much it looks alike, best pair first.
  (define costs
    (sort (for*/list ([n (in-list rest-now)] [b (in-list rest-base)]
                      #:when (< (signature-distance n b #:slide-size size)
                                (limit-for n b)))
            (list (signature-distance n b #:slide-size size) n b))
          < #:key first))
  (define extra
    (filter values
            (for/list ([c (in-list costs)])
              (define n (second c)) (define b (third c))
              (cond
                [(or (hash-ref matched-now n #f) (hash-ref used b #f)) #f]
                [else (hash-set! matched-now n #t) (hash-set! used b #t) (cons n b)]))))
  (values (append pairs extra)
          (filter (lambda (n) (not (hash-ref matched-now n #f))) now)
          (filter (lambda (b) (not (hash-ref used b #f))) base)))

;; ------------------------------------------------------------------- merging

;; Which named properties differ, as (name was now). A property missing on one
;; side is not a difference: a deck states a font where a program leaves it to
;; the theme, and neither is an edit.
;; Stands for a property one side does not have at all, so that it is told apart
;; from a `bold` that is really false.
(define ABSENT 'absent)

;; The properties an editor can take away, which are exactly the ones the
;; program can state the absence of: a shape with no fill says `~fill: #false`,
;; a line with no arrowhead `~head: #false`, a paragraph with no bullet
;; `no_bullet`.
;;
;; Nothing else has an "absent" for anyone to choose. There is no command in
;; any editor that removes a typeface, an anchor or a line spacing -- they only
;; ever change. So a side that does not state one is a side that did not say,
;; not a removal, and the two sides differ in what they bother to state:
;; Keynote's export leaves out a great deal that our own writes. Reading that
;; as edits reported hundreds of removals nobody had made, and under the rule
;; that a save lands whole or not at all, one of them blocked the lot.
;; The list is `STYLE-GROUPS`' own members -- everything that travels with a
;; fill or an outline, since an outline being added is its width and its dash
;; arriving with it -- plus the two that state their own absence.
;; A bullet is not on the list: a paragraph with none says `<a:buNone/>`, which
;; is a statement and compares as one, while a paragraph that says nothing at
;; all about bullets inherits whatever its list style gives it.
(define REMOVABLE '(fill fill-opacity line line-width dash cap head tail crop))

;; What the format means when it says nothing. A shape whose `<a:rPr>` states no
;; weight is not bold; one whose `<a:pPr>` states no alignment is left-aligned.
;; Our own export states all of it; other editors stay quiet and let the
;; defaults stand, and reading that as an edit rewrote a program's typography
;; to the defaults -- Georgia 24 bold became Calibri 18 plain, silently, on the
;; first save after a round trip through Keynote.
;;
;; So a deck value that is the default is not taken as an assertion about the
;; shape. It is noted instead: the program keeps what it says, and the report
;; says the two do not agree. The cost is that setting a property *to* its
;; default in the editor is not merged -- which is the one case that cannot be
;; told from the editor saying nothing at all.
;; The typeface a deck falls back on, which is the one the program itself names
;; -- a generated program says `current_default_font(...)` at the top, and the
;; deck we wrote from it says the same in its master. Read off the runtime once
;; the program has been loaded.
(define inherited-font (make-parameter #f))

(define INHERITED-DEFAULTS
  (hash 'size 18.0 'bold #f 'italic #f 'underline #f 'strike #f
        'spacing 0.0 'caps 'none 'baseline 0.0
        'align 'left 'line-spacing '(percent . 1.0)
        'space-before 0.0 'space-after 0.0
        'level 0 'margin-left 0.0 'indent 0.0
        'anchor 'top 'wrap #t 'autofit 'none
        'insets '(7.2 3.6 7.2 3.6)))

;; (values edits notes): the changes to act on, and the ones only worth saying.
;; A typeface the machine does not have is one the editor could not keep. Open a
;; deck on a machine without Helvetica Neue and LibreOffice draws it in
;; something else -- and writes that something else back into the file, on every
;; run that named it. Taken at face value that is the deck saying "the font is
;; Arial now", and the program would be rewritten to say so: the author's font,
;; gone, because it was opened on the wrong machine.
;;
;; So a font change is only believed when the font it moved away from is one
;; this machine could have drawn. Otherwise it is noted, which says what the
;; deck says and changes nothing.
;; Both halves matter: the font it moved *away* from is one this machine could
;; not have drawn, and the one it moved *to* is one it could. That is what a
;; substitution looks like. A typeface someone actually changed usually moves
;; between two fonts the machine has, and where it does not -- a machine with
;; hardly any fonts, which is what a build box is -- nothing is assumed and the
;; change is taken at its word.
(define (font-substitution? ch)
  (and (eq? 'font (property-head (first ch)))
       (string? (second ch)) (string? (third ch))
       (not (installed-face? (second ch)))
       (installed-face? (third ch))))

(define installed-faces (delay (get-face-list)))

(define (installed-face? name)
  (and (member name (force installed-faces)) #t))

;; Whether the box shrinks its text to fit. In one of those, the size a file
;; states is not the size anyone chose: it is the size the last renderer worked
;; out, and every renderer works it out differently -- open a deck without the
;; fonts it names and LibreOffice re-fits the text and writes a smaller size and
;; a tighter line spacing back. Taken at face value that is the deck saying the
;; author shrank the text, and the program would be rewritten to say so.
(define (shrinks-to-fit? style)
  (define p (assoc 'autofit style))
  (and p (eq? 'shrink (cdr p))))

;; A text box that does not wrap is as wide as its words; one set to grow is as
;; tall as them. Those numbers belong to the text, and the two sides do not
;; measure text with the same machinery -- so they differ for ever, and there is
;; nothing the program could be made to say that would settle it: it draws its
;; own text at its own width whatever the source states.
;;
;; Retyping a word in LibreOffice grows the box it is in, which reported the
;; retyping and a resize together. The resize was applied, wrote a width the
;; program does not use, and came back on the next save, and on the one after
;; that. So the numbers the text decides are set aside before the two boxes are
;; compared, and only a size somebody really set is a resize.
(define (text-driven-size? st which)
  (case (el-state-kind st)
    ;; DrawingML represents an ordinary inserted text box as a preset rectangle
    ;; with a text body, so it is a `shape` state here. LibreOffice normalizes
    ;; that rectangle on save exactly as it does a bare text state.
    [(text shape)
     (let ([style (el-state-style st)])
       (case which
         ;; `wrap` stated false: the box does not hold the text to a width.
         [(w) (let ([p (assoc 'wrap style)]) (and p (not (cdr p)) #t))]
         ;; `grow` is spAutoFit: the box takes the height the text needs.
         ;; `shrink` is the other way round -- the box is set and the text is
         ;; made to fit it -- so its height is a number somebody chose.
         [(h) (let ([p (assoc 'autofit style)]) (and p (eq? 'grow (cdr p))))]
         [else #f]))]
    ;; A group is the box around what it holds -- both sides work it out that
    ;; way -- so its width and its height are its children's. Where one of those
    ;; children holds text, the two sides measure that text with different
    ;; machinery, and the box comes out a little different for ever: retyping a
    ;; word inside a group reported the retyping and a resize of the group with
    ;; it, and the resize came back on every save afterwards. There is nothing
    ;; to write it to either -- a group's size in the source is the size of what
    ;; it holds, not a number of its own.
    [(group) (pair? (group-text-entries (el-state-text st)))]
    [else #f]))

;; One element with those numbers taken out of the picture, `like` saying which
;; they are: the program's side says how the box is drawn, on both sides.
(define (aside-text-driven st like)
  (define w? (text-driven-size? like 'w))
  (define h? (text-driven-size? like 'h))
  (cond
    [(not (or w? h?)) st]
    [else
     (define style (el-state-style like))
     (define anchor (let ([p (assoc 'anchor style)]) (if p (cdr p) 'top)))
     (define group? (eq? 'group (el-state-kind like)))
     (struct-copy el-state st
                  ;; A non-wrapping auto-fit box changes x and width together,
                  ;; preserving its horizontal centre. Compare that centre, so
                  ;; LibreOffice's normalization is ignored but a real drag is
                  ;; still a move.
                  ;; A group is different: the editor derives its extent from
                  ;; its children while keeping the top-left corner fixed.
                  [x (if (and w? (not group?))
                         (+ (el-state-x st) (/ (el-state-w st) 2.0))
                         (el-state-x st))]
                  ;; Vertically the text anchor is the point LibreOffice keeps
                  ;; fixed while deriving the height.
                  [y (if (and h? (not group?))
                         (case anchor
                           [(center) (+ (el-state-y st) (/ (el-state-h st) 2.0))]
                           [(bottom) (+ (el-state-y st) (el-state-h st))]
                           [else (el-state-y st)])
                         (el-state-y st))]
                  [w (if w? 0.0 (el-state-w st))]
                  [h (if h? 0.0 (el-state-h st))])]))

(define (derived-by-fitting? ch)
  (memq (property-head (first ch)) '(size line-spacing)))

(define (split-trusted changes #:shrinks? [shrinks? #f])
  (for/fold ([edits '()] [notes '()] #:result (values (reverse edits) (reverse notes)))
            ([ch (in-list changes)])
    (define d (case (property-head (first ch))
                [(font) (or (inherited-font) 'no-default)]
                [else (hash-ref INHERITED-DEFAULTS (property-head (first ch)) 'no-default)]))
    (if (or (and (not (eq? 'no-default d)) (equal? d (third ch)))
            (font-substitution? ch)
            (and shrinks? (derived-by-fitting? ch)))
        (values edits (cons ch notes))
        (values (cons ch edits) notes))))

;; What the two states disagree about, as (property was now). An outline the
;; editor added, or took away, is as much of a change as one it recoloured, so
;; a removable property missing from either side is reported with `ABSENT`.
;; How close two values of a property have to be to count as the same.
;;
;; An editor writes numbers back in whatever it stores them as. LibreOffice
;; keeps hundredths of a millimetre, so a deck it has merely opened and saved
;; comes back with every length a little different: 45pt of space above a
;; paragraph as 45.01, two points of letter spacing as 2.01, a 37% crop as
;; 36.99%. Asked for equality, the merge reported an edit for each of them --
;; 119 of them on one deck nobody had touched.
;;
;; A tenth of a point is finer than any editor's smallest step and coarser than
;; any of this. The fractions -- a crop, a line spacing, an opacity -- get a
;; five-hundredth, which is the same argument at their scale.
;;
;; Compared rather than rounded: rounding first turns a value sitting on a
;; boundary into a difference of its own, which is a bug the fuzzer found in the
;; first version of this.
(define POINT-PROPERTIES '(size spacing space-before space-after margin-left indent insets))

(define (property-epsilon k)
  (if (memq k POINT-PROPERTIES) 0.15 0.005))

(define (same-value? a b eps)
  (cond
    [(and (real? a) (real? b)) (< (abs (- a b)) eps)]
    [(and (pair? a) (pair? b) (not (list? a)) (not (list? b)))
     (and (same-value? (car a) (car b) eps) (same-value? (cdr a) (cdr b) eps))]
    [(and (list? a) (list? b))
     (and (= (length a) (length b))
          (for/and ([x (in-list a)] [y (in-list b)]) (same-value? x y eps)))]
    [else (equal? a b)]))

(define (style-changes was now)
  (define (value l k) (let ([p (assoc k l)]) (if p (cdr p) ABSENT)))
  (define keys (remove-duplicates (append (map car was) (map car now))))
  (filter values
          (for/list ([k (in-list keys)])
            (define a (value was k))
            (define b (value now k))
            (and (not (same-value? a b (property-epsilon (property-head k))))
                 (or (not (or (eq? ABSENT a) (eq? ABSENT b)))
                     (memq (property-head k) REMOVABLE))
                 (list k a b)))))

;; A tag names one *code site*, and one `at` inside a loop draws several
;; elements. So a tag can stand for a family, and an edit to a family only
;; makes sense when it is an edit to all of it:
;;
;;   dragged all of them the same way  ->  one edit, on the one `at`
;;   dragged one of them               ->  refused; the loop computes the rest
;;   deleted one of them               ->  refused; a loop cannot say "but not
;;                                          that one"
;;
;; The refusal is the point. Guessing would silently move the other two.
(define FAMILY-EPSILON 0.05)

(define (same-delta? a b)
  (and (< (abs (- (first a) (first b))) FAMILY-EPSILON)
       (< (abs (- (second a) (second b))) FAMILY-EPSILON)))

(define (geometry-delta d b)
  (list (- (el-state-x d) (el-state-x b)) (- (el-state-y d) (el-state-y b))))

;; Three-way merge for one slide. `base` is the agreed state, `prog` the program
;; as it is now, `deck` the .pptx as it is now.
;; The order the deck draws a slide's shapes in, when that is not the order the
;; program does. One action for the slide, listing the tags in the new order.
(define (restacking index pairs added removed prog-by-tag)
  ;; The tags the program knows them by, which is the base side of each pair:
  ;; a copy made in the editor carries the tag it was copied from, and the
  ;; program has already given it one of its own.
  (define tags (map (lambda (p) (el-state-tag (cdr p))) pairs))
  ;; Read in the order the deck draws them, the places the program draws them.
  (define in-deck-order
    (sort pairs < #:key (lambda (p) (el-state-z (car p)))))
  (define places (map (lambda (p) (el-state-z (cdr p))) in-deck-order))
  (cond
    ;; A shape that came or went is a different slide to reason about, and a tag
    ;; naming several elements has no order of its own to give.
    [(or (pair? added) (pair? removed)) '()]
    [(not (andmap values tags)) '()]
    [(check-duplicates (map tag-key tags)) '()]
    [(for/or ([t (in-list tags)])
       (> (length (hash-ref prog-by-tag (tag-key t) '())) 1))
     '()]
    ;; Already in that order, so nothing was restacked.
    [(equal? places (sort places <)) '()]
    [else
     (list (sync-action 'restacked (format "slide ~a" index) index
                        (map (lambda (p) (el-state-tag (cdr p))) in-deck-order)
                        #f))]))

;; What each group on one deck slide holds, as group tag -> child tags. The
;; merge's own view of a slide has a group as one element, which is what the
;; editor drags; grouping and ungrouping are the two edits that need to see
;; inside one.
(define (deck-group-children d index)
  (define s (and d (for/first ([s (in-list (deck-slides d))]
                               #:when (= index (slide-index s)))
                     s)))
  (for/fold ([h (hash)]) ([e (in-list (if s (slide-elements s) '()))])
    (cond
      [(and (group? e) (not (string=? "" (element-name e))))
       (hash-set h (tag-key (element-name e))
                 (let leaves ([es (group-children e)])
                   (append*
                    (for/list ([c (in-list es)])
                      (cond
                        [(group? c) (leaves (group-children c))]
                        [(string=? "" (element-name c)) '()]
                        [else (list (tag-key (element-name c)))])))))]
      [else h])))

;; The tag of the element the deck draws just under this one, out of those the
;; program already has: what a new `at` form should follow.
(define (drawn-under d pairs)
  (define under (for/list ([p (in-list pairs)]
                           #:when (and (el-state-tag (cdr p))
                                       (< (el-state-z (car p)) (el-state-z d))))
                  p))
  (and (pair? under)
       (el-state-tag (cdr (argmax (lambda (p) (el-state-z (car p))) under)))))

(define (merge-slide index base prog deck size #:group-children [group-children (hash)])
  (define-values (deck-pairs deck-added deck-removed)
    (match-elements deck base #:slide-size size))
  ;; By tag, not one per tag: a tag can name several elements from one `at`.
  ;; In z-order, which is the order the code drew them -- a family's members are
  ;; compared position by position, so both sides have to agree on which is
  ;; which.
  (define (by-tag l)
    (for/fold ([h (hash)] #:result (for/hash ([(k v) (in-hash h)])
                                     (values k (sort (reverse v) < #:key el-state-z))))
              ([e (in-list l)] #:when (el-state-tag e))
      (hash-update h (tag-key (el-state-tag e)) (lambda (v) (cons e v)) '())))
  (define prog-by-tag (by-tag prog))
  (define (prog-count tag) (length (hash-ref prog-by-tag (tag-key tag) '())))
  ;; The elements the deck no longer has, by tag, and the groups that turn out
  ;; to hold exactly them.
  (define removed-by-tag
    (for/hash ([b (in-list deck-removed)] #:when (el-state-tag b))
      (values (tag-key (el-state-tag b)) b)))
  (define groupings
    (for/list ([d (in-list deck-added)]
               #:when (let ([kids (and (el-state-tag d)
                                       (hash-ref group-children (tag-key (el-state-tag d)) #f))])
                        (and (pair? kids)
                             (for/and ([k (in-list kids)]) (hash-ref removed-by-tag k #f)))))
      d))
  (define into-a-group
    (for*/hash ([d (in-list groupings)]
                [k (in-list (hash-ref group-children (tag-key (el-state-tag d))))])
      (values (tag-key k) #t)))
  ;; What the program drew without an `at` form of its own: the shapes a helper
  ;; draws, the pieces of a diagram built in code. They have no tag on either
  ;; side, so they are compared one to one -- bucketed under #f instead, a slide
  ;; with thirty of them read as one family of thirty that had not all moved the
  ;; same way, and the report said `ambiguous` once and named nothing.
  (define-values (tagged-pairs drawn-pairs)
    (partition (lambda (p) (or (el-state-tag (cdr p)) (el-state-tag (car p))))
               deck-pairs))
  ;; Pairs grouped by tag, so a family is decided once rather than per element.
  ;; In the order the tags first appear, so the report reads down the slide.
  (define groups
    (let loop ([ps tagged-pairs] [order '()] [h (hash)])
      (cond
        [(null? ps) (for/list ([tag (in-list (reverse order))])
                      (list tag (reverse (hash-ref h tag))))]
        [else
         (define pair (car ps))
         (define tag (tag-key (or (el-state-tag (cdr pair)) (el-state-tag (car pair)))))
         (loop (cdr ps)
               (if (hash-has-key? h tag) order (cons tag order))
               (hash-update h tag (lambda (v) (cons pair v)) '()))])))
  (append
   ;; Said rather than written, and said with the name the deck gives it. There
   ;; is no `at` form holding this shape's position -- the code decides where it
   ;; goes -- so a drag on it is something to know about and not something the
   ;; source could be made to say. A note, so that it neither refuses the save
   ;; nor disappears without a word.
   (for/list ([p (in-list drawn-pairs)]
              #:when (or (not (el-geometry-same? (car p) (cdr p)))
                         (not (string=? (el-state-text (car p)) (el-state-text (cdr p))))))
     (sync-action 'noted (or (el-state-name (car p)) "(unnamed)") index DRAWN-BY-CODE #f))
   (append*
    (for/list ([g (in-list groups)])
      (define key (first g))
      (define pairs (second g))
      ;; Keep the readable suffix in reports and editor helpers, while every
      ;; lookup uses the source digest above.
      (define tag (or (el-state-tag (cdr (first pairs)))
                      (el-state-tag (car (first pairs)))
                      key))
      (if (or (> (length pairs) 1) (> (prog-count tag) 1))
          (family-actions tag index pairs (hash-ref prog-by-tag key '()))
          (single-actions tag index (first pairs)
                          (let ([ps (hash-ref prog-by-tag key '())])
                            (and (pair? ps) (first ps)))))))
   ;; Bringing a shape to the front changes the order everything is drawn in
   ;; and nothing else. That is the order of the `at` forms, so it is one edit
   ;; on the slide rather than one per shape -- and it only makes sense when
   ;; every shape on the slide is accounted for and answers to its own tag.
   (restacking index deck-pairs deck-added deck-removed prog-by-tag)
   ;; Grouping is one edit, not an addition and two deletions: a group the deck
   ;; has that the base does not, holding exactly elements the base has that
   ;; the deck does not. Read as three separate edits it looks like most of the
   ;; slide being thrown away, which is a slide the merge refuses to touch.
   (for/list ([d (in-list groupings)])
     (sync-action 'grouped (tag-key (el-state-tag d)) index
                  (list (el-geometry d)
                        (for/list ([t (in-list (hash-ref group-children
                                                        (tag-key (el-state-tag d))))])
                          (define key (tag-key t))
                          (cons key (el-geometry (hash-ref removed-by-tag key)))))
                  #f))
   ;; A shape drawn in the editor sits somewhere in the drawing order, and
   ;; that order is the order of the `at` forms -- so the new form says which
   ;; form it goes after. Writing it last instead put every new shape on top,
   ;; and the deck is rewritten from the program before the order could be
   ;; merged separately.
   (for/list ([d (in-list deck-added)] #:unless (memq d groupings))
     ;; With where it sits in the drawing order, which is the only handle on a
     ;; shape the editor made itself: LibreOffice writes a new text box with no
     ;; name, and a name is how every other action finds its element.
     (sync-action 'added (or (tag-key (el-state-tag d)) (el-state-name d) "(unnamed)") index
                  (list (el-geometry d) (drawn-under d deck-pairs) (el-state-z d)) #f))
   (for/list ([b (in-list deck-removed)]
              #:unless (hash-ref into-a-group (tag-key (el-state-tag b)) #f))
     (define tag (el-state-tag b))
     (cond
       ;; One of a family deleted: the others are still drawn by the same `at`.
       [(and tag (> (prog-count tag) 1))
        (sync-action 'ambiguous tag index
                     (format "~a elements share this tag, and deleting one of them is not something the code can say"
                             (prog-count tag))
                     #f)]
       ;; Nothing placed it, so nothing can stop placing it: the code draws it,
       ;; and what the code draws is the code's to decide. Said with the name
       ;; the deck gave it, and not as a deletion the merge failed to write.
       [(not tag)
        (sync-action 'noted (or (el-state-name b) "(unnamed)") index DRAWN-BY-CODE #f)]
       [else (sync-action 'removed tag index (el-geometry b) #f)]))))

;; Every element under one tag, which one `at` drew.
(define (family-actions tag index pairs prog-elements)
  (define moved
    (for/list ([pair (in-list pairs)]
               #:when (not (el-geometry-same? (car pair) (cdr pair))))
      pair))
  (cond
    [(null? moved) '()]
    ;; Some moved and some did not, or they moved differently: there is no one
    ;; correction that produces this.
    [(not (and (= (length moved) (length pairs))
               (let ([d0 (geometry-delta (car (first moved)) (cdr (first moved)))])
                 (for/and ([pair (in-list (cdr moved))])
                   (same-delta? d0 (geometry-delta (car pair) (cdr pair)))))))
     (list (sync-action 'ambiguous tag index
                        (format "~a elements share this tag and they did not all move the same way"
                                (length pairs))
                        #f))]
    [else
     ;; All of them, by the same amount. That is one correction on the one `at`.
     (define d (car (first moved)))
     (define b (cdr (first moved)))
     (define p (and (pair? prog-elements) (first prog-elements)))
     (list (sync-action 'moved tag index (el-geometry d) (and p (el-geometry p))))]))

;; The ordinary case: one element, one tag, one `at`.
;; The children of a group whose text differs, each named in its own right: the
;; `at` that draws a child carries the child's tag, so that is what an edit has
;; to be written to.
;;
;; Only the ones the deck still holds. A child that has gone from the digest
;; went with the group or was deleted, and neither is a retyping.
(define (group-retexts tag index d b)
  (cond
    [(not (eq? 'group (el-state-kind d))) '()]
    [(string=? (el-state-text d) (el-state-text b)) '()]
    [else
     (define was (group-text-entries (el-state-text b)))
     (define now (group-text-entries (el-state-text d)))
     ;; LibreOffice drops `descr`, including our source identity, from every
     ;; child of a group. It does keep the child's display name. Pair an exact
     ;; identity first, then use that readable suffix only when it is unique on
     ;; both sides. The base tag remains the action's tag, since that is the
     ;; source site that can actually be patched.
     (define paired-was (make-hasheq))
     (define paired-now (make-hasheq))
     (define (pair! n w)
       (hash-set! paired-now n w)
       (hash-set! paired-was w n))
     (for ([n (in-list now)])
       (define exact
         (filter (lambda (w)
                   (and (not (hash-ref paired-was w #f))
                        (equal? (tag-key (car n)) (tag-key (car w)))))
                 was))
       (when (= 1 (length exact)) (pair! n (first exact))))
     (for ([n (in-list now)] #:unless (hash-ref paired-now n #f))
       (define candidates
         (filter (lambda (w)
                   (and (not (hash-ref paired-was w #f))
                        (let ([readable (automatic-tag-name (car w))])
                          (and readable (string=? readable (car n))))))
                 was))
       (define same-name-now
         (count (lambda (other) (string=? (car other) (car n))) now))
       (when (and (= 1 (length candidates)) (= 1 same-name-now))
         (pair! n (first candidates))))
     (append
      (for/list ([e (in-list now)]
                 #:when (let ([old (hash-ref paired-now e #f)])
                          (and old (not (string=? (cdr old) (cdr e))))))
        (sync-action 'retext (car (hash-ref paired-now e)) index (cdr e) #f))
      ;; And one the group no longer holds. Deleting a shape inside a group is
      ;; an ordinary thing to do in an editor, and the `at` form that drew it is
      ;; there in the source to be taken out -- the group itself is not deleted,
      ;; so nothing else about the slide changes.
      (for/list ([e (in-list was)] #:unless (hash-ref paired-was e #f))
        (sync-action 'removed (car e) index (el-geometry b) #f))
      ;; One it holds that the program does not draw. Writing that means adding
      ;; a form inside the group's own form, which is a restructuring rather
      ;; than a literal edit -- so it is said and not done.
      (for/list ([e (in-list now)] #:unless (hash-ref paired-now e #f))
        (sync-action 'noted (car e) index
                     (string-append "it was drawn inside a group, and a shape cannot be"
                                    " added to a group from here")
                     #f)))]))

(define DRAWN-BY-CODE
  (string-append "the program draws this rather than placing it with an `at` form,"
                 " so there is no position in the source to write"))

(define (single-actions tag index pair p)
  (define d (car pair)) (define b (cdr pair))
  ;; Compared with the numbers the text decides set aside: see
  ;; `text-driven-size?`.
  (define d* (aside-text-driven d b))
  (define b* (aside-text-driven b b))
  (define p* (and p (aside-text-driven p b)))
  (define deck-moved? (not (el-geometry-same? d* b*)))
  (define prog-moved? (and p* (not (el-geometry-same? p* b*))))
  (define deck-retext? (not (string=? (el-state-text d) (el-state-text b))))
  (define prog-retext? (and p (not (string=? (el-state-text p) (el-state-text b)))))
  (append
   (group-retexts tag index d b)
   (filter
   values
   (list
    (cond
      ;; Both sides moved it. PowerPoint wins, because dragging is why it was
      ;; opened -- but say so rather than doing it quietly.
      [(and deck-moved? prog-moved?)
       (sync-action 'conflict tag index
                    (list 'geometry (el-geometry b) (el-geometry p) (el-geometry d))
                    (and p (el-geometry p)))]
      [deck-moved?
       (sync-action (if (and (< (abs (- (el-state-w d*) (el-state-w b*))) 0.05)
                             (< (abs (- (el-state-h d*) (el-state-h b*))) 0.05))
                        'moved 'resized)
                    tag index (el-geometry d)
                    (and p (el-geometry p)))]
      [else #f])
    ;; What the editor changed about the look of it. Appearance is the code's, so
    ;; this is reported and written only where the source holds a literal -- but
    ;; reported it must be: recolouring a shape in the editor used to disappear
    ;; without a word.
    (let-values ([(changes _noted)
                  (split-trusted (style-changes (el-state-style b) (el-state-style d))
                                 #:shrinks? (shrinks-to-fit? (el-state-style b)))])
      (and (pair? changes) (sync-action 'restyle tag index changes #f)))
    ;; And what it says the shape looks like without ever having said so: the
    ;; defaults it left standing where it stated nothing. Those are noted, not
    ;; acted on, and they do not stop a save.
    (let-values ([(_changes noted)
                  (split-trusted (style-changes (el-state-style b) (el-state-style d)))])
      (and (pair? noted) (sync-action 'noted tag index noted #f)))
    (cond
      ;; Text is the code's, so a program edit wins and a deck-only edit is
      ;; taken.
      [(and deck-retext? prog-retext?)
       (sync-action 'conflict tag index
                    (list 'text (el-state-text b) (el-state-text p) (el-state-text d))
                    (and p (el-geometry p)))]
      ;; A group's own text is the text of what it holds, and a change to that is
      ;; a change to one of its children -- reported below as the children, not
      ;; as the group, since a group has no text of its own to write to.
      [(and deck-retext? (eq? 'group (el-state-kind d))) #f]
      [deck-retext? (sync-action 'retext tag index (el-state-text d) #f)]
      [else #f])))))

;; Which deck slide is which of the base's. The merge pairs slides so that
;; adding one in the editor does not shift every later one: it used to pair by
;; index, and a slide pasted at the front made every following slide compare
;; against its neighbour -- 28 "edits" and eight elements deleted.
;;
;; Slides are matched on their elements' tags, which is a strong fingerprint: an
;; untouched slide keeps all of them, and a slide pasted in from another deck
;; shares none. Best pair first, one base slide to one deck slide, so a
;; duplicated slide claims its original once and the copy is left over as new.
(define SLIDE-MATCH 0.5)

(define (slide-affinity a b)
  ;; A generated object name is a weaker identity than an `at` tag, but it is
  ;; still useful on a slide made wholly from raw drawing: LibreOffice keeps the
  ;; names when it rewrites text metrics. Prefix the two namespaces so a user
  ;; name can never collide with a source digest.
  (define (token e)
    (or (and (el-state-tag e) (string-append "tag:" (format "~a" (tag-key (el-state-tag e)))))
        (and (el-state-name e) (string-append "name:" (el-state-name e)))))
  (define ta (filter values (map token (slide-state-elements a))))
  (define tb (filter values (map token (slide-state-elements b))))
  (define score
    (cond
    [(and (null? ta) (null? tb)) 1.0]
    ;; Everything on a slide can be deleted, or a slide can be filled from
    ;; empty. There are no tags to go on then, and the one thing left is where
    ;; the slide sits -- which is enough, since the alternative reads as a
    ;; slide deleted and another one added.
    [(or (null? ta) (null? tb))
     (if (= (slide-state-index a) (slide-state-index b)) 0.75 0.0)]
    [else
     (define-values (shared _left)
       (for/fold ([n 0] [h (for/fold ([h (hash)]) ([t (in-list tb)])
                             (hash-update h t add1 0))])
                 ([t (in-list ta)])
         (if (positive? (hash-ref h t 0))
             (values (add1 n) (hash-update h t sub1))
             (values n h))))
     (/ (* 2.0 shared) (+ (length ta) (length tb)))]))
  ;; Identical divider slides can deliberately share every source site. When
  ;; they have not been reordered, prefer the page already in this position;
  ;; a genuine reorder still wins by its distinct fingerprint.
  (+ score (if (= (slide-state-index a) (slide-state-index b)) 0.001 0.0)))

;; (values pairs added removed), pairs as (deck . base), in deck order.
(define (match-slides base deck)
  (define costs
    (sort (for*/list ([d (in-list deck)] [b (in-list base)])
            (list (slide-affinity d b) d b))
          > #:key first))
  (define used-d (make-hasheq))
  (define used-b (make-hasheq))
  (for ([c (in-list costs)])
    (when (and (>= (first c) SLIDE-MATCH)
               (not (hash-ref used-d (second c) #f))
               (not (hash-ref used-b (third c) #f)))
      (hash-set! used-d (second c) (third c))
      (hash-set! used-b (third c) #t)))
  ;; Whatever is left over pairs up by where it sits, when a slide of the same
  ;; number is left over on the other side too. Tag overlap cannot follow every
  ;; edit -- grouping two shapes of three leaves a slide looking like a
  ;; different one -- and reading that as a slide deleted and another added is
  ;; the worse of the two guesses. `wholesale-change?` is what catches a slide
  ;; that really was swapped.
  (for* ([d (in-list deck)]
         #:unless (hash-ref used-d d #f)
         [b (in-list base)]
         #:unless (hash-ref used-b b #f)
         #:when (= (slide-state-index d) (slide-state-index b)))
    (hash-set! used-d d b)
    (hash-set! used-b b #t))
  (values (for/list ([d (in-list deck)] #:when (hash-ref used-d d #f))
            (cons d (hash-ref used-d d)))
          (for/list ([d (in-list deck)] #:unless (hash-ref used-d d #f)) d)
          (for/list ([b (in-list base)] #:unless (hash-ref used-b b #f)) b)))

;; Even with every slide matched, a slide can have been swapped for a different
;; one that happens to look alike. A slide whose elements mostly do not match is
;; not the slide the base recorded, and calling the difference an edit would
;; delete the program's real elements.
(define WHOLESALE 0.5)

(define (wholesale-change? base-elements actions)
  (define n (length base-elements))
  ;; A shape that went into a group is still on the slide -- the group is what
  ;; the editor drags now. Counting it as gone read grouping as a slide swapped
  ;; for a different one.
  (define gone (for/sum ([a (in-list actions)]
                         #:when (memq (sync-action-kind a) '(removed))) 1))
  (and (> n 1) (> gone (* WHOLESALE n))))

;; A slide deleted in the editor is not merged back yet, and taking the
;; difference for edits would delete the program's real elements.
;; Deleting a slide in the editor means deleting its definition and its entry in
;; `all_slides`. The definition can be anything, so it is only removed when the
;; program does not otherwise mention its name: anything else is a program the
;; merge would be rewriting rather than following.
(define (slides-removed removed)
  (for/list ([s (in-list removed)])
    (sync-action 'removed-slide (format "slide ~a" (slide-state-index s))
                 (slide-state-index s) (list (slide-state-index s)) #f)))

;; The slides the program holds as a canvas, by their place in `all_slides`.
;;
;; A slide can be something else: a talk gives one stages, and what the name
;; then holds is an animated pict built from several canvases rather than a
;; canvas. Its elements cannot be read back and could not be written to if they
;; were -- there is no one `at` form to write. The merge leaves those slides
;; alone rather than reporting every shape on them as newly drawn, which is
;; what it used to do: nine hundred refusals on a talk with ten staged slides,
;; and, because a save lands whole or not at all, nothing else could be merged
;; either.
(define (canvas-slide-indexes program-path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define-values (all-sites scopes slide-sites layout) (find-program-sites program-path))
    (and scopes
         (let ([held (for/list ([ss (in-list slide-sites)]) (slide-site-scope ss))])
           (for/set ([scope (in-list (scopes-by-deck-slide scopes))] [i (in-naturals 1)]
                     #:when (memq scope held))
             i)))))

;; A slide the program does not hold as a canvas, described by the deck instead.
;;
;; A talk that gives a slide stages holds an animated pict there, built from
;; several canvases. Nothing can read its elements back, and nothing could be
;; written to them if it could -- there is no one `at` form to write to. Left as
;; the empty state that reading it produces, every shape the deck draws on that
;; slide looks newly added, and a slide that matches nothing makes the matcher
;; give up on the deck's order as well: on one real talk that was nine hundred
;; refusals, and, because a save lands whole or not at all, no way to merge
;; anything else either -- not even dragging the slides about in the navigator,
;; which touches no element at all.
;;
;; So for those slides the deck is taken as read. What is on them is the talk's
;; own business; what is around them -- their order, their number -- is still
;; the merge's.
(define (as-deck-says prog deck canvases)
  (cond
    [(not canvases) prog]
    [else
     (define by-index (for/hash ([d (in-list deck)]) (values (slide-state-index d) d)))
     (for/list ([p (in-list prog)])
       (define i (slide-state-index p))
       (define d (hash-ref by-index i #f))
       (if (or (set-member? canvases i) (not d))
           p
           (struct-copy slide-state d [index i])))]))

(define (merge-states base prog deck [program-path "the program"] #:deck-ir [deck-ir #f])
  (define-values (pairs added removed) (match-slides base deck))
  ;; #f when the program could not be read for them, in which case every slide
  ;; is compared as before: this must not be the thing that stops a merge.
  (define canvases (and (or (path? program-path) (path-string? program-path))
                        (file-exists? program-path)
                        (canvas-slide-indexes program-path)))
  ;; A slide the base has that the deck does not, *and* a slide the deck has
  ;; that the base does not, in the same merge: that is a slide the matching
  ;; could not follow, not a deletion and a new slide. Telling those apart is a
  ;; guess, and guessing wrong deletes a definition the program still wants --
  ;; grouping two shapes of three is enough to make a slide stop matching.
  (define muddled? (and (pair? added) (pair? removed)))
  ;; A deck has one size, so resizing it in the editor is one edit however many
  ;; slides there are -- and the program usually says it once, as a name every
  ;; canvas shares.
  (define resized
    (let* ([p (and (pair? pairs) (first pairs))]
           [d (and p (car p))] [b (and p (cdr p))])
      (if (and p (or (> (abs (- (slide-state-width d) (slide-state-width b))) 0.05)
                     (> (abs (- (slide-state-height d) (slide-state-height b))) 0.05)))
          (list (sync-action 'resized-deck "the deck" (slide-state-index b)
                             (list (slide-state-width d) (slide-state-height d)) #f))
          '())))
  (append
   resized
   (if muddled?
       (for/list ([b (in-list removed)])
         (sync-action 'ambiguous (format "slide ~a" (slide-state-index b))
                      (slide-state-index b)
                      (format (string-append "this slide and ~a in the deck no longer look like"
                                             " each other, so which is which is a guess;"
                                             " nothing here is rewritten")
                              (if (= 1 (length added)) "one" "some"))
                      #f))
       '())
   (if muddled? '() (slides-removed removed))
   (append*
    (for/list ([pair (in-list pairs)])
      (define ds (car pair))
      (define bs (cdr pair))
      (define index (slide-state-index bs))
      (define ps (for/first ([s (in-list prog)]
                             #:when (= index (slide-state-index s))) s))
      (cond
        [ps
         (define as (merge-slide index (slide-state-elements bs) (slide-state-elements ps)
                                 (slide-state-elements ds)
                                 (max 1.0 (slide-state-width bs))
                                 #:group-children
                                 (deck-group-children deck-ir (slide-state-index ds))))
         ;; Hidden in the editor, or shown again: the slide's own property, and
         ;; the only edit that leaves everything on it untouched.
         (define hiding
           (let ([was (slide-state-hidden? bs)] [now (slide-state-hidden? ds)])
             (if (eq? (and was #t) (and now #t))
                 '()
                 (list (sync-action
                        (if (eq? (and was #t) (and (slide-state-hidden? ps) #t))
                            'hidden-slide 'conflict)
                        (format "slide ~a" index) index (list (and now #t)) #f)))))
         (define bg
           (let ([was (slide-state-background bs)] [now (slide-state-background ds)])
             (if (equal? was now)
                 '()
                 ;; The program's own paint decides a conflict the same way an
                 ;; element's does: if the code changed it too, the code wins
                 ;; and the editor's is reported.
                 (list (sync-action
                        (if (equal? was (slide-state-background ps)) 'repainted 'conflict)
                        (format "slide ~a" index) index
                        (list (list 'background (or was ABSENT) (or now ABSENT))) #f)))))
         (if (wholesale-change? (slide-state-elements bs) as)
             (list (sync-action
                    'ambiguous (format "slide ~a" index) index
                    (format (string-append
                             "most of this slide's ~a elements are not in the deck any more,"
                             " so it is a different slide rather than an edited one")
                            (length (slide-state-elements bs)))
                    #f))
             (append as bg hiding))]
        [else (list (sync-action 'conflict (format "slide ~a" index) index
                                 '(slide-missing) #f))])))
   ;; Dragging slides about in the navigator changes their order and nothing
   ;; else, so no element differs and the merge used to see nothing at all.
   ;; The checked `all_slides` manifest is exactly what says the order.
   (let ([order (for/list ([d (in-list (in-deck-order deck))])
                  (let ([b (for/first ([p (in-list pairs)] #:when (eq? d (car p))) (cdr p))])
                    (and b (slide-state-index b))))])
     (if (and (andmap values order)
              (not (equal? order (sort order <))))
         (list (sync-action 'reordered "the slides" 0 order #f))
         '()))
   ;; A slide the base does not have was added in the editor. Where it goes in
   ;; the program's order is where it sits in the deck: after whichever program
   ;; slide the nearest earlier deck slide belongs to.
   (if muddled? '()
   (let loop ([ds (in-deck-order deck)] [after 0] [seq 0] [acc '()])
     (cond
       [(null? ds) (reverse acc)]
       [else
        (define d (car ds))
        (define b (for/first ([p (in-list pairs)] #:when (eq? d (car p))) (cdr p)))
        (if b
            (loop (cdr ds) (slide-state-index b) 0 acc)
            (loop (cdr ds) after (add1 seq)
                  (cons (sync-action 'added-slide
                                     (format "deck slide ~a" (slide-state-index d))
                                     (slide-state-index d)
                                     (list after seq)
                                     #f)
                        acc)))])))))

(define (in-deck-order deck)
  (sort deck < #:key slide-state-index))

;; -------------------------------------------------------- patching the source

;; One `at(x, y, ... child)` form, or an explicitly tagged helper proxy, with
;; the source ranges of the
;; literals a merge is allowed to replace. A range is #f when the value is not a
;; literal -- `(at margin (+ top 20) ...)` is a decision the code is making, and
;; is reported rather than overwritten.
;; `nudge` is (list range dx dy) for an existing `#:nudge` argument, and
;; `insert-at` is the position a new one would go, after the last identity
;; metadata or picture argument. Each site remembers the top-level definition
;; and source module it sits in; `all_slides` says which definition is which
;; slide. Source-derived ids are globally unique, while explicit proxy tags may
;; still repeat in different named slide scopes.
(struct at-site (tag x y rot width height texts nudge insert-at scope whole
                 flip-h flip-v leaf-at styles) #:transparent)

;; Where a new element goes in one slide's definition: just after the last
;; argument of its `slide-canvas` call, at that argument's indentation. Adding a
;; shape in the editor puts it on top, which is where the last argument draws.
(struct slide-site (scope insert-at indent def-start def-end background width height hidden
                   build source)
  #:transparent)
(struct rng (source start end) #:transparent)

;; Shifts every recovered range, for a reader that was handed only part of the
;; file. The shrubbery parser cannot see a `#lang` line, so a Rhombus program is
;; parsed from just after it and the ranges are moved back into the whole file.
(define current-range-offset (make-parameter 0))
(define current-source-path (make-parameter #f))

(define (make-range start end [source (current-source-path)])
  (rng source start end))

(define (range-of stx)
  (and (syntax-position stx) (syntax-span stx)
       (let ([start (+ (current-range-offset) (sub1 (syntax-position stx)))])
         (make-range start (+ start (syntax-span stx))))))

;; Two elements answering to one tag would mean an edit lands on an arbitrary one
;; of them. That is refused rather than guessed at. The usual cause is an `at`
;; that runs more than once -- inside a `for`, or in a helper called twice --
;; which is fine code, just not code an editor's edit can be traced back to.
(define (duplicate-tags tags)
  (define counts
    (for/fold ([h (hash)]) ([t (in-list tags)] #:when t)
      (hash-update h (tag-key t) add1 0)))
  (sort (for/list ([(t n) (in-hash counts)] #:when (> n 1)) (cons t n))
        string<? #:key car))

(define (check-unique-tags tags where hint)
  (define dups (duplicate-tags tags))
  (unless (null? dups)
    (error 'glide
           "~a uses one tag for more than one element, so a sync cannot tell them apart:\n~a  ~a"
           where
           (apply string-append
                  (for/list ([d (in-list dups)])
                    (format "    ~s appears ~a times\n" (car d) (cdr d))))
           hint)))

(define TAG-HINT
  (string-append
   "Ordinary `at` forms get distinct identities from their source locations. If these\n"
   "  are helper proxy calls, give each call a distinct literal `~tag:` instead."))

;; -------------------------------------------------- delimiters, on the text

;; Shrubbery's group wrappers carry no source location -- only the leaf terms do
;; -- so the extent of a call has to be found in the text. Doing it this way for
;; both languages also means the result follows the file as the user reformats
;; it, rather than as it was generated.
;;
;; `line?` and `block?` are the two comment syntaxes: ";;" for Racket, "//" and
;; "/* */" for Rhombus.
(define (match-close text open line? block?)
  (define n (string-length text))
  (define (closer c) (case c [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [else #f]))
  (let loop ([i open] [stack '()])
    (cond
      [(>= i n) #f]
      [else
       (define c (string-ref text i))
       (define (peek k) (and (< (+ i k) n) (string-ref text (+ i k))))
       (cond
         ;; A string literal can hold anything, including a lone paren.
         [(char=? c #\")
          (let skip ([j (add1 i)])
            (cond
              [(>= j n) #f]
              [(char=? (string-ref text j) #\\) (skip (+ j 2))]
              [(char=? (string-ref text j) #\") (loop (add1 j) stack)]
              [else (skip (add1 j))]))]
         [(and line? (line? c (peek 1)))
          (let skip ([j i])
            (cond [(>= j n) #f]
                  [(char=? (string-ref text j) #\newline) (loop (add1 j) stack)]
                  [else (skip (add1 j))]))]
         [(and block? (block? c (peek 1)))
          (let skip ([j (+ i 2)])
            (cond [(>= (add1 j) n) #f]
                  [(and (char=? (string-ref text j) #\*)
                        (char=? (string-ref text (add1 j)) #\/))
                   (loop (+ j 2) stack)]
                  [else (skip (add1 j))]))]
         [(closer c) (loop (add1 i) (cons (closer c) stack))]
         [(and (pair? stack) (char=? c (car stack)))
          (if (null? (cdr stack)) i (loop (add1 i) (cdr stack)))]
         [else (loop (add1 i) stack)])])))

(define (rhombus-close text open)
  (match-close text open
               (lambda (c d) (and (char=? c #\/) (eqv? d #\/)))
               (lambda (c d) (and (char=? c #\/) (eqv? d #\*)))))

;; The first opening paren at or after `i`.
(define (next-open text i)
  (define n (string-length text))
  (let loop ([j i])
    (cond [(>= j n) #f]
          [(char=? (string-ref text j) #\() j]
          [else (loop (add1 j))])))

;; Walks back over whitespace, which is where a new last argument belongs: after
;; the previous one, not after the newline that was indenting the closing paren.
(define (back-over-space text i)
  (let loop ([j i])
    (if (and (> j 0) (char-whitespace? (string-ref text (sub1 j)))) (loop (sub1 j)) j)))

;; The indentation of the line `i` sits on, which is the indentation a new
;; sibling should get.
(define (indent-at text i)
  (define start
    (let loop ([j (min i (sub1 (string-length text)))])
      (cond [(<= j 0) 0]
            [(char=? (string-ref text (sub1 j)) #\newline) j]
            [else (loop (sub1 j))])))
  (let loop ([j start] [k 0])
    (if (and (< j (string-length text)) (char=? (string-ref text j) #\space))
        (loop (add1 j) (add1 k))
        k)))

;; Where a new last argument goes in `call-name(...)`, given the position just
;; after the call's name.
;; A new element is indented like a direct argument of the canvas. Looking at
;; every `at` in the surrounding definition is not enough: an animated slide
;; commonly defines its base canvas and several deeply indented stage groups in
;; one function, and the last `at` in that scope is not a child of the canvas.
(define (canvas-argument-indent parens text [fallback 2])
  (define (first-position s)
    (cond
      [(and (syntax? s) (range-of s)) => rng-start]
      [(syntax? s) (first-position (syntax-e s))]
      [(pair? s) (for/fold ([best #f]) ([part (in-list s)])
                   (define p (first-position part))
                   (cond [(not p) best] [(not best) p] [else (min best p)]))]
      [else #f]))
  (define starts
    (filter values
            (for/list ([arg (in-list (if parens (cdr (syntax-e parens)) '()))])
              (first-position arg))))
  (cond
    [(null? starts) fallback]
    [else
     ;; Prefer an argument written on its own line. Its leading whitespace is
     ;; precisely the indentation a newly appended sibling needs.
     (or (for/first ([start (in-list (reverse starts))]
                     #:when (regexp-match? #px"^[ \t]*$"
                                           (substring text (line-start text start) start)))
           (indent-at text start))
         fallback)]))

;; The `at` forms one slide's canvas holds itself: those with its scope, less
;; the ones another of them encloses, which belong to a group and are drawn by
;; it. Drawing order, indentation and where a new element goes are all the
;; canvas's own list, not what is nested inside it.
(define (canvas-forms scope sites [source #f])
  (define here (for/list ([s (in-list sites)]
                          #:when (and (equal? scope (at-site-scope s))
                                      (at-site-whole s)
                                      (or (not source)
                                          (equal? source (rng-source (at-site-whole s))))))
                 s))
  (for/list ([s (in-list here)]
             #:unless (for/or ([o (in-list here)])
                        (and (not (eq? o s)) (encloses? (at-site-whole o) (at-site-whole s)))))
    s))

(define (canvas-insertion text after-name close)
  (define open (next-open text after-name))
  (define shut (and open (close text open)))
  (and shut
       (let ([at (back-over-space text shut)])
         (list at (indent-at text (sub1 at))))))

(define (literal-range stx pred)
  (and stx (pred (syntax-e stx)) (range-of stx)))

;; Reads the program fresh, so the ranges reflect the file as it is now rather
;; than as it was generated. That is what survives the user reformatting it: the
;; syntax tree is read to *find* things, and only the literals themselves are
;; overwritten, so no formatting is ever regenerated.
;; (values sites scopes slide-sites layout). `scopes` is the checked list of
;; source owners that builds the editor pages, in order. It comes either from a
;; `glide_slides` declaration or, for compatibility, a literal list of names.
;; An arbitrary computed slide list is rejected because it cannot trace a page
;; back to a named source scope.
;; Only Rhombus source is patched. Any program that provides slide picts can be
;; rendered and exported -- that goes through `dynamic-require` and does not care
;; what language wrote it -- but tracing an edit back to a literal means reading
;; the source, and there is one reader.
(define (find-program-sites path)
  (define s (if (path? path) (path->string path) path))
  (unless (regexp-match? #rx"[.]rhm$" s)
    (error 'glide
           (string-append "~a is not Rhombus source, so edits cannot be merged into it.\n"
                          "  Rendering and export work on any program; syncing needs a .rhm.")
           s))
  (define files (program-source-files path))
  (define parsed
    (for/list ([source (in-list files)] #:when (file-exists? source))
      (call-with-values (lambda () (rhombus-at-sites source)) list)))
  (when (null? parsed)
    (error 'glide "cannot read Rhombus source ~a" s))
  (define root (first parsed))
  (define root-layout (fourth root))
  (unless (second root)
    (error 'glide
           (string-append
            "~a does not declare a traceable `all_slides`.\n"
            "  Synchronization needs one source owner per editor page. Use, for example:\n"
            "    glide_slides all_slides:\n"
            "      [slide_1, in_section(0, slide_2)]\n"
            "  A plain literal `[slide_1, slide_2]` remains supported for compatibility.")
           s))
  ;; Keep the defining module in the key. Two imported modules may each have a
  ;; private `brand` or `slide_width`; flattening those to one bare symbol lets
  ;; an edit in one module rewrite the other's unrelated binding.
  (define globals
    (for*/fold ([h (hash)]) ([p (in-list parsed)]
                             [(key range) (in-hash (program-layout-globals (fourth p)))])
      (hash-set h key range)))
  (define exports
    (for*/fold ([h (hash)]) ([p (in-list parsed)]
                             [(source insertion)
                              (in-hash (program-layout-exports (fourth p)))])
      (hash-set h source insertion)))
  (define open-imports
    (for*/fold ([h (hash)]) ([p (in-list parsed)]
                             [(source imports)
                              (in-hash (program-layout-open-imports (fourth p)))])
      (hash-set h source imports)))
  (define public-names
    (for*/fold ([h (hash)]) ([p (in-list parsed)]
                             [(source names)
                              (in-hash (program-layout-public-names (fourth p)))])
      (hash-set h source names)))
  (values (append* (map first parsed))
          (second root)
          (append* (map third parsed))
          (struct-copy program-layout root-layout
                       [globals globals]
                       [exports exports]
                       [open-imports open-imports]
                       [public-names public-names]
                       [files files])))

(define (find-at-sites path)
  (define-values (sites _scopes _slides _layout) (find-program-sites path))
  sites)

;; ---------------------------------------------------------------- rhombus

;; Reads one Rhombus module as shrubbery. Keeping this separate from site
;; recovery lets the same parse discover local imports for both syncing and the
;; watcher. A local import is a string at the head of an import entry; strings
;; nested in `lib(...)` are collection paths and are deliberately not followed.
(define (rhombus-source-groups path)
  (define text (file->string path))
  (define after-lang
    (cond
      [(regexp-match-positions #rx"^#lang[^\n]*\n" text) => (lambda (m) (cdar m))]
      [else 0]))
  (define stx
    (let ([in (open-input-string (substring text after-lang))])
      (port-count-lines! in)
      (parse-all in #:source path)))
  (define e (syntax-e stx))
  (values text after-lang
          (if (and (list? e) (eq? 'multi (syntax-e* (car e)))) (cdr e) (list stx))))

(define (syntax-list s)
  (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))

(define (rhombus-local-imports groups source #:open-only? [open-only? #f])
  (define (block-imports s)
    (define l (syntax-list s))
    (cond
      [(not l) '()]
      ;; A direct string is a relative module path, with optional modifiers
      ;; such as `open`, `as`, or `.name` after it.
      [(and (eq? 'group (syntax-e* (first l)))
            (>= (length l) 2)
            (string? (syntax-e* (second l)))
            (or (not open-only?)
                (for/or ([part (in-list (cddr l))])
                  (eq? 'open (syntax-e* part)))))
       (list (syntax-e* (second l)))]
      ;; An import block holds one group per entry.
      [(eq? 'block (syntax-e* (first l)))
       (append* (map block-imports (cdr l)))]
      ;; Only descend through blocks. In particular, do not descend through
      ;; the parens of `lib("...")` and mistake its collection name for a file.
      [else
       (append*
        (for/list ([part (in-list l)]
                   #:when (let ([pl (syntax-list part)])
                            (and pl (eq? 'block (syntax-e* (first pl))))))
          (block-imports part)))]))
  (define names
    (append*
     (for/list ([g (in-list groups)])
       (define l (syntax-list g))
       (if (and l (>= (length l) 3)
                (eq? 'group (syntax-e* (first l)))
                (eq? 'import (syntax-e* (second l))))
           (block-imports (third l))
           '()))))
  (define base (or (path-only source) (current-directory)))
  (for/list ([name (in-list names)]
             #:when (regexp-match? #rx"[.]rhm$" name))
    (simplify-path (path->complete-path (build-path base name)) #f)))

;; Only an `open` import can make an exported definition available as a bare
;; name in the importing module. This is deliberately narrower than the source
;; graph above: global write-back must establish a binding path, not merely find
;; a coincidentally unique spelling somewhere in the project.
(define (rhombus-open-local-imports groups source)
  (rhombus-local-imports groups source #:open-only? #t))

;; The root module plus all transitively imported local `.rhm` modules, once
;; each and in import order. Collection imports are dependencies too, but they
;; are libraries rather than source owned by this program and must never be
;; patched or watched as part of it.
(define (program-source-files path)
  (define seen (make-hash))
  (define (visit source)
    (define full (simplify-path (path->complete-path source) #f))
    (cond
      [(hash-ref seen full #f) '()]
      [else
       (hash-set! seen full #t)
       (cons full
             (if (file-exists? full)
                 (let-values ([(text after-lang groups) (rhombus-source-groups full)])
                   (void text after-lang)
                   (append* (map visit (rhombus-local-imports groups full))))
                 '()))]))
  (visit path))

;; Shrubbery parses `at(57.6, 158.4, ~tag: "Box", shape_pict(...))` as
;;
;;   (group at (parens (group 57.6) (group 158.4)
;;                     (group #:tag (block (group "Box")))
;;                     (group shape_pict (parens ...))))
;;
;; so a call is an identifier followed by a parens group, a positional argument
;; is a one-element group, and a keyword argument is a group whose head is the
;; keyword and whose value sits in a block.
(define (rhombus-at-sites path)
  (define source (simplify-path (path->complete-path path) #f))
  (define-values (text after-lang groups) (rhombus-source-groups source))
  (define sites '())
  ;; The offset has to be in effect while the ranges are computed, which is
  ;; during the walk.
  (parameterize ([current-range-offset after-lang]
                 [current-source-text text]
                 [current-source-path source])
    (for ([g (in-list groups)])
      (define scope (rhombus-def-name g))
      (define local-names (rhombus-local-names g))
      (let walk ([s g])
        (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
        (when l
          (for ([call (in-list (rhombus-calls l))])
            (define direct? (eq? 'at (first call)))
            (define automatic
              (and direct?
                   (let* ([r (range-of (second call))]
                          [name-stx
                           (for/or ([arg (in-list (third call))])
                             (and (eq? '#:name (rhombus-kw-name arg))
                                  (rhombus-kw-value arg)))]
                          [name (and name-stx
                                     (string? (syntax-e* name-stx))
                                     (syntax-e* name-stx))])
                     (and r (source-position-tag
                             (rng-source r) (add1 (rng-start r)) name)))))
            (define site
              (if direct?
                  (parse-rhombus-at (third call) (local-leaf-defs g) automatic)
                  ;; A helper the program calls to draw something, which says by
                  ;; carrying a `~tag:` that what it draws is one element. See
                  ;; `parse-rhombus-tagged-call`.
                  (parse-rhombus-tagged-call
                   (third call)
                   ;; The call on its own, so that what it states about how the
                   ;; thing looks can be found the way an `at` form's leaf is.
                   (datum->syntax #f (list (second call) (fourth call))))))
            (when site
              (define extent (rhombus-call-extent text (second call)))
              (set! sites
                    (cons (mask-at-site-shared
                           (struct-copy at-site site
                                        [scope scope]
                                        [whole (and direct? extent)]
                                        [insert-at
                                         (or (at-site-insert-at site)
                                             (and extent (sub1 (rng-end extent))))])
                           local-names)
                          sites))))
          (for-each walk l))
        (void))))
  (parameterize ([current-range-offset after-lang] [current-source-path source])
    (define ordered (reverse sites))
    (define nl (rhombus-name-list groups text source))
    (define scopes (rhombus-slide-scopes groups))
    (define traced-nl
      (cond
        [(not nl) #f]
        [(not scopes) nl]
        [else
         (struct-copy name-list nl
                      [items
                       (for/list ([item (in-list (name-list-items nl))]
                                  [scope (in-list scopes)])
                         (if (name-entry-name item)
                             item
                             (struct-copy name-entry item [name scope])))])]))
    (values ordered scopes
            (rhombus-slide-sites groups text source)
            (program-layout traced-nl
                            (let ([ex (rhombus-export-block groups text)])
                              (if ex (hash source ex) (hash)))
                            (rhombus-global-colours groups)
                            (and traced-nl
                                 (map name-entry-range (name-list-items traced-nl)))
                            (hash source (rhombus-open-local-imports groups source))
                            (hash source (rhombus-exported-names groups))
                            source
                            (list source)))))

;; Where a slide can be added, and where its name has to be entered for the
;; addition to mean anything. A `def slide_N` nobody lists in `all_slides` is
;; dead code, so the two edits go together.
(struct program-layout (slide-list exports globals entries open-imports public-names source files)
  #:transparent)

;; The brackets in a `glide_slides` declaration or compatible literal list,
;; and each source owner in them.
(struct name-list (open close items source checked?) #:transparent)
(struct name-entry (name range shown-range) #:transparent)

;; The checked declaration, plus both ordinary Rhombus definition spellings:
;;
;;   glide_slides all_slides:
;;     [a, in_section(0, b), show_as(c, decorate(c)), divider(1)]
;;
;;   def all_slides = [a, b]
;;   def all_slides:
;;     [a, b]
;;
;; The latter's value is a one-expression block. Answer the brackets syntax in
;; either case, leaving any computed expression as #f.
(define (all-slides-brackets g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (and l (>= (length l) 4) (eq? 'group (syntax-e* (first l)))
       (let ([value
              (cond
                [(and (= 4 (length l))
                      (eq? 'glide_slides (syntax-e* (second l)))
                      (memq (syntax-e* (third l)) '(all_slides all-slides)))
                 (define block (syntax-list (fourth l)))
                 (define body (and block (eq? 'block (syntax-e* (first block)))
                                   (= 2 (length block)) (syntax-list (second block))))
                 (and body (eq? 'group (syntax-e* (first body)))
                      (= 2 (length body)) (second body))]
                ;; The shrubbery spelling of `=` is `(op =)`. A five-part
                ;; definition has exactly the value shape we want here; the
                ;; Rhombus parser has already established the operator slot.
                [(and (= 5 (length l))
                      (eq? 'def (syntax-e* (second l)))
                      (memq (syntax-e* (third l)) '(all_slides all-slides)))
                 (fifth l)]
                [(and (= 4 (length l))
                      (eq? 'def (syntax-e* (second l)))
                      (memq (syntax-e* (third l)) '(all_slides all-slides)))
                 (define block (syntax-list (fourth l)))
                 (define body (and block (eq? 'block (syntax-e* (first block)))
                                   (= 2 (length block)) (syntax-list (second block))))
                 (and body (eq? 'group (syntax-e* (first body)))
                      (= 2 (length body)) (second body))]
                [else #f])])
         (and (rhombus-head? value 'brackets) value))))

;; The source owner and full editable extent of one deck entry. The macro has
;; already rejected every other form at expansion time; recognizing the same
;; small language here keeps the source rewriter independent of macro output.
(define (rhombus-deck-entry grp index)
  (define v (rhombus-group-value grp))
  (cond
    [(and v (symbol? (syntax-e* v)))
     (define r (range-of v))
     (name-entry (syntax-e* v) r r)]
    [else
     (define call (rhombus-call (syntax-e grp)))
     (define args (and call (cdr call)))
     (define first-v (and args (pair? args) (rhombus-group-value (first args))))
     (define owner
       (cond
         [(and call (eq? 'show_as (car call)) (= 2 (length args))
               first-v (symbol? (syntax-e* first-v)))
          (syntax-e* first-v)]
         [(and call (eq? 'in_section (car call)) (= 2 (length args))
               (let ([last-v (rhombus-group-value (second args))])
                 (and last-v (symbol? (syntax-e* last-v)) (syntax-e* last-v))))]
         ;; Compatibility with the structural wrappers Glide itself writes
         ;; around a legacy literal slide list.
         [(and call (eq? 'over (car call)) (= 2 (length args))
               first-v (symbol? (syntax-e* first-v)))
          (syntax-e* first-v)]
         [(and call (eq? 'from_stage (car call)) (= 3 (length args))
               (let ([slide-v (rhombus-group-value (second args))])
                 (and slide-v (symbol? (syntax-e* slide-v)) (syntax-e* slide-v))))]
         [(and call (eq? 'show_only (car call)) (= 1 (length args)))
          (string->symbol (format "glide_generated_~a" index))]
         [(and call (eq? 'divider (car call)) (pair? args))
          (string->symbol (format "glide_generated_~a" index))]
         [else #f]))
     (let* ([head (call-name-range grp)]
            [start (and head (rng-start head))]
            [end (group-end grp)]
            [whole (and start end (make-range start end (rng-source head)))]
            ;; `show_as` and `show_only` are syntax understood by
            ;; `glide_slides`, not runtime functions. When Glide later wraps
            ;; an entry to add an editor-drawn layer, only their shown
            ;; expression can be nested inside the new marker.
            [shown-arg (cond
                         [(and call (eq? 'show_as (car call)) (= 2 (length args)))
                          (second args)]
                         [(and call (eq? 'show_only (car call)) (= 1 (length args)))
                          (first args)]
                         [else #f])]
            [shown-start (and shown-arg (group-start shown-arg))]
            [shown-end (and shown-arg (group-end shown-arg))]
            [shown (if (and shown-start shown-end)
                       (make-range shown-start shown-end (rng-source head))
                       whole)])
       (and start end
            (name-entry owner whole shown)))]))

;; The `[...]` of the `all_slides` definition.
(define (rhombus-name-list groups text source)
  (for/or ([g (in-list groups)])
    (define gl (syntax-list g))
    (define checked?
      (and gl (>= (length gl) 2)
           (eq? 'group (syntax-e* (first gl)))
           (eq? 'glide_slides (syntax-e* (second gl)))))
    (define brackets (all-slides-brackets g))
    (define b (and brackets (syntax-list brackets)))
    (and b
         (let* ([items
                 (for/list ([grp (in-list (cdr b))]
                            [i (in-naturals 1)])
                   (rhombus-deck-entry grp i))]
                ;; The brackets carry no useful position of their own, so find
                ;; the opening bracket from the first name. Empty slide lists
                ;; are not traceable and are rejected with the other non-name
                ;; lists below.
                [anchor (and (pair? items) (rng-start (name-entry-range (first items))))]
                [open (and anchor (prev-char text anchor #\[))]
                [close (and open (match-close text open
                                              (lambda (c d)
                                                (and (char=? c #\/) (eqv? d #\/)))
                                              (lambda (c d)
                                                (and (char=? c #\/) (eqv? d #\*)))))])
           (and (andmap values items) open close
                (name-list open close items source checked?))))))

;; The first `ch` at or before `i`.
(define (prev-char text i ch)
  (let loop ([j (min i (sub1 (string-length text)))])
    (cond [(< j 0) #f]
          [(char=? (string-ref text j) ch) j]
          [else (loop (sub1 j))])))

;; The `export:` block, as the position a new name goes after and the
;; indentation it takes. A program need not have one -- `all_slides` is what an
;; export reads -- so this is #f when there is none.
(define (rhombus-export-block groups text)
  (for/or ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (and l (= 3 (length l)) (eq? 'group (syntax-e* (first l)))
         (eq? 'export (syntax-e* (second l)))
         (let ([b (let ([e (syntax-e (third l))]) (and (list? e) e))])
           (and b (eq? 'block (syntax-e* (car b)))
                (let ([rs (filter values
                                  (for/list ([grp (in-list (cdr b))])
                                    (define v (rhombus-group-value grp))
                                    (and v (range-of v))))]
                      [kw (range-of (second l))])
                  (and (pair? rs) kw
                       (let ([last-r (argmax rng-end rs)])
                         ;; Only a block written under `export:` has a line for
                         ;; another name. `export: all_slides` on one line has
                         ;; none -- a line beneath it is a statement of its own,
                         ;; not an export -- and a name nobody exports works
                         ;; anyway, so there is nothing to write.
                         (and (> (line-start text (rng-start last-r)) (rng-end kw))
                              (cons (rng-end last-r)
                                    (indent-at text (rng-start last-r))))))))))))

;; The simple names an editable module exports. A shared style in another file
;; is writable only through a direct `open` import of one of these names. More
;; elaborate export forms are left unresolved instead of guessed at.
(define (rhombus-exported-names groups)
  (remove-duplicates
   (append*
    (for/list ([g (in-list groups)])
      (define l (syntax-list g))
      (cond
        [(and l (= 3 (length l))
              (eq? 'group (syntax-e* (first l)))
              (eq? 'export (syntax-e* (second l))))
         (define b (syntax-list (third l)))
         (if (and b (eq? 'block (syntax-e* (first b))))
             (filter values
                     (for/list ([entry (in-list (cdr b))])
                       (define v (rhombus-group-value entry))
                       (and v (symbol? (syntax-e* v)) (syntax-e* v))))
             '())]
        [else '()])))
   eq?))

;; The `slide_canvas(...)` call in each `def slide_N = slide_canvas(...)`.
;; From the head of a call to just past its closing paren.
(define (rhombus-call-extent text name)
  (define r (range-of name))
  (define open (and r (next-open text (rng-end r))))
  (define shut (and open (rhombus-close text open)))
  (and shut (make-range (rng-start r) (add1 shut) (rng-source r))))

;; The one `slide_canvas` nested anywhere in a named definition. A wrapper such
;; as `in_section` belongs inside that function so `all_slides` can remain a
;; literal name list while the canvas and its `at` forms keep the same scope.
(define (slide-canvas-parts g)
  (define found '())
  (let walk ([s g])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      ;; In `def slide_1 = slide_canvas(...)`, the call's two terms are
      ;; siblings in the definition group; they are not wrapped in a group of
      ;; their own. `call-name` deliberately recognizes both that form and a
      ;; call nested as an argument.
      (define name (call-name s))
      (when (and name (eq? 'slide_canvas (syntax-e* name)))
        (set! found (cons (cons name (call-parens s)) found)))
      (for-each walk l)))
  (reverse found))

(define (rhombus-slide-sites groups text source)
  (filter values
          (for/list ([g (in-list groups)])
            (define scope (rhombus-def-name g))
            (define local-names (rhombus-local-names g))
            (define l (let ([e (syntax-e g)]) (and (list? e) e)))
            (define canvases (and l (slide-canvas-parts g)))
            (define one (and (= 1 (length canvases)) (first canvases)))
            (define name (and one (car one)))
            (define canvas-parens (and one (cdr one)))
            (and scope name
                 (let* ([r (range-of name)]
                        [open (and r (next-open text (rng-end r)))]
                        [shut (and open (rhombus-close text open))]
                        [ins (and r (canvas-insertion text (rng-end r) rhombus-close))])
                   (and ins shut
                        (mask-slide-site-shared
                         (slide-site scope (first ins) (canvas-argument-indent canvas-parens text)
                                     (let ([d (range-of (second l))]) (and d (rng-start d)))
                                     (or (group-end g) (add1 shut))
                                     (canvas-paint-site canvas-parens)
                                     (canvas-size-site canvas-parens '#:width 'slide-width)
                                     (canvas-size-site canvas-parens '#:height 'slide-height)
                                     (canvas-flag-site canvas-parens '#:hidden 'hidden)
                                     ;; Which build this slide is a frame of,
                                     ;; if it is one.
                                     (canvas-string canvas-parens '#:build)
                                     source)
                         local-names)))))))

;; Where the canvas states its background, which is always somewhere: the
;; emitter writes one whether the slide had a background of its own or not.
;; What the canvas says its size is: a number to rewrite, or a name that every
;; slide shares -- a generated program says `~width: slide_width`, so the deck
;; being resized is one edit on one `def`.
(define (canvas-size-site parens kw property)
  (define stx (parens-kw-group parens kw))
  (define literal (and stx (literal-range (single-term stx) real?)))
  (define name (and stx (not literal) (single-symbol stx)))
  (and stx (style-site property literal name #f kw #f)))

;; The one term a group holds, when it holds one.
(define (single-term g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (and l (= 2 (length l)) (second l)))

(define (single-symbol g)
  (define t (single-term g))
  (and t (symbol? (syntax-e* t)) (syntax-e* t)))

;; Whether the canvas says the slide is skipped. Stated, it is rewritten; not
;; stated, it is added -- the same rule as any other argument.
;; A string a canvas states, or #f.
(define (canvas-string parens kw)
  (define stx (parens-kw-group parens kw))
  (define t (and stx (single-term stx)))
  (define v (and t (syntax-e t)))
  (and (string? v) v))

(define (canvas-flag-site parens kw property)
  (define stx (parens-kw-group parens kw))
  (if stx
      (style-site property (literal-range (single-term stx) boolean?) #f #f kw #f)
      (style-site property #f #f (kw-append-at parens) kw #f)))

;; Where a new argument goes in an argument list already in hand: after the
;; last keyword argument there is.
(define (kw-append-at parens)
  (and parens
       (for/fold ([best #f]) ([g (in-list (cdr (syntax-e parens)))])
         (if (rhombus-kw-name g)
             (let ([e (group-end g)]) (if (and e (> e (or best 0))) e best))
             best))))

(define (canvas-paint-site parens)
  (define stx (parens-kw-group parens '#:background))
  (define hit (and stx (not (compound-fill? stx)) (hex-site stx)))
  (and stx
       (style-site 'background
                   (and hit (eq? 'literal (car hit)) (cdr hit))
                   (and hit (eq? 'shared (car hit)) (cdr hit))
                   #f '#:background
                   (value-extent-of stx))))

;; `(group def slide_1 (op =) ...)` -> slide_1, and the same for `fun`.
(define (rhombus-def-name g)
  (define l (let ([e (syntax-e g)]) (and (list? e) e)))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (memq (syntax-e* (second l)) '(def fun))
       (symbol? (syntax-e* (third l)))
       (syntax-e* (third l))))

;; Names bound inside one top-level definition. A bare style expression with
;; one of these spellings is lexical, not a reference to a top-level colour or
;; size that Glide may rewrite. This deliberately over-approximates nested
;; bindings: declining an uncertain style edit is safe; choosing an unrelated
;; top-level definition is not.
(define (rhombus-local-names g)
  (define names '())
  (define (note! v) (when (symbol? v) (set! names (cons v names))))
  (define (note-pattern! s)
    (define l (syntax-list s))
    (cond
      [(not l) (note! (syntax-e* s))]
      [else
       (for ([part (in-list (if (memq (syntax-e* (first l)) '(group brackets parens))
                                (cdr l)
                                l))])
         (note-pattern! part))]))
  (define (parameter-name arg)
    (define l (syntax-list arg))
    (cond
      [(not l) #f]
      [(and (eq? 'group (syntax-e* (first l)))
            (>= (length l) 2)
            (symbol? (syntax-e* (second l))))
       (syntax-e* (second l))]
      [(and (eq? 'group (syntax-e* (first l)))
            (>= (length l) 3)
            (keyword? (syntax-e* (second l))))
       (define b (syntax-list (third l)))
       (define body (and b (eq? 'block (syntax-e* (first b)))
                         (pair? (cdr b)) (syntax-list (second b))))
       (and body (>= (length body) 2) (symbol? (syntax-e* (second body)))
            (syntax-e* (second body)))]
      [else #f]))
  (let walk ([s g])
    (define l (syntax-list s))
    (when l
      (when (and (eq? 'group (syntax-e* (first l)))
                 (>= (length l) 3)
                 (memq (syntax-e* (second l)) '(def fun let)))
        (note-pattern! (third l))
        (when (and (eq? 'fun (syntax-e* (second l)))
                   (>= (length l) 4)
                   (rhombus-head? (fourth l) 'parens))
          (for ([arg (in-list (cdr (syntax-e (fourth l))))])
            (note! (parameter-name arg)))))
      (when (and (eq? 'group (syntax-e* (first l)))
                 (>= (length l) 3)
                 (eq? 'for (syntax-e* (second l))))
        (for ([part (in-list (cddr l))]
              #:when (rhombus-head? part 'parens))
          (for ([clause (in-list (cdr (syntax-e part)))])
            (define c (syntax-list clause))
            (note! (parameter-name clause))
            (define before-in
              (and c (member 'in (map syntax-e* c))))
            (when before-in
              (for ([binding (in-list
                              (take c (- (length c) (length before-in))))]
                    #:unless (eq? 'group (syntax-e* binding)))
                (note-pattern! binding))))))
      (for-each walk l)))
  (remove-duplicates names eq?))

;; `def brand = hex("4472C4")` -> brand -> the range of that string, and
;; `def slide_width = 720.0` -> slide_width -> the range of that number. A value
;; given a name is shared, so changing one shape that uses it is not the same
;; edit as changing the name.
(define (rhombus-global-colours groups)
  (for/fold ([h (hash)]) ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (define nm (and l (>= (length l) 5) (eq? 'def (syntax-e* (second l)))
                    (symbol? (syntax-e* (third l)))
                    (syntax-e* (third l))))
    (define hit (and nm (hex-site g)))
    (define number (and nm (not hit) (= 5 (length l)) (literal-range (fifth l) real?)))
    (define r (cond [(and hit (eq? 'literal (car hit))) (cdr hit)] [number number] [else #f]))
    (cond
      [r (hash-set h (cons (rng-source r) nm) r)]
      [else h])))

;; `glide_slides all_slides: [slide_1, show_as(slide_2, ...)]`
;; -> '(slide_1 slide_2).
(define (rhombus-slide-scopes groups)
  (define (contains-at? g)
    (let walk ([v g])
      (define l (and (syntax? v) (syntax-list v)))
      (and l
           (or (for/or ([call (in-list (rhombus-calls l))])
                 (eq? 'at (first call)))
               (for/or ([part (in-list l)]) (walk part))))))
  (define canvases
    (for/hash ([g (in-list groups)]
               #:when (let ([name (rhombus-def-name g)])
                        (and name (= 1 (length (slide-canvas-parts g)))
                             (contains-at? g))))
      (values (rhombus-def-name g) #t)))
  (define bodies
    (for/hash ([g (in-list groups)] #:when (rhombus-def-name g))
      (values (rhombus-def-name g) g)))
  (define (scope-through name)
    (let loop ([name name] [seen (set)])
      (cond
        [(hash-ref canvases name #f) name]
        [(set-member? seen name) #f]
        [else
         (define body (hash-ref bodies name #f))
         (and body
              (let walk ([v body])
                (cond
                  [(syntax? v) (walk (syntax-e v))]
                  [(and (symbol? v) (hash-ref bodies v #f))
                   (loop v (set-add seen name))]
                  [(pair? v) (or (walk (car v)) (walk (cdr v)))]
                  [else #f])))])))
  (define (scope-in v)
    (cond
      [(syntax? v) (scope-in (syntax-e v))]
      [(and (symbol? v) (hash-ref canvases v #f)) v]
      [(pair? v) (or (scope-in (car v)) (scope-in (cdr v)))]
      [else #f]))
  (for/or ([g (in-list groups)])
    (define brackets (all-slides-brackets g))
    (define b (and brackets (syntax-list brackets)))
    (and b
         (let* ([entry-groups (cdr b)]
                [entries (for/list ([grp (in-list entry-groups)]
                                    [i (in-naturals 1)])
                           (rhombus-deck-entry grp i))])
           (and (pair? entries) (andmap values entries)
                (let* ([l (syntax-list g)]
                       [macro? (and l (>= (length l) 2)
                                    (eq? 'glide_slides (syntax-e* (second l))))]
                       [owners
                        (for/list ([entry (in-list entries)]
                                   [entry-group (in-list entry-groups)])
                          (define owner (name-entry-name entry))
                          (if macro?
                              owner
                              (or (scope-in entry-group)
                                  (and owner (scope-through owner)))))])
                  (and (or (not macro?) (andmap values owners)) owners)))))))

;; (cons name-syntax parens-syntax) for the first call-shaped pair in a group.
(define (rhombus-call-parts l)
  (and (pair? l)
       (eq? 'group (syntax-e* (first l)))
       ;; A call used as a definition's value has the same sibling shape as a
       ;; standalone call, but later in the group:
       ;; `(group def canvas (op =) titled (parens ...))`.
       (for/or ([name (in-list (cdr l))]
                [p (in-list (cddr l))])
         (and (symbol? (syntax-e* name))
              (rhombus-head? p 'parens)
              (cons name p)))))

;; (cons name arg-groups) when `l` is a call, else #f.
(define (rhombus-call l)
  (define parts (rhombus-call-parts l))
  (and parts
       (cons (syntax-e* (car parts)) (cdr (syntax-e (cdr parts))))))

;; Every call a group makes at its own term level, as
;; `(name name-stx args parens)`.
;; Usually that is the group's head, since an argument like `at(...)` is a
;; group of its own -- but a binding puts the call after the name it binds, as
;; in `let p = region(p, ..., ~tag: "class 1")`, and a helper that draws one
;; element is just as much a site there.
(define (rhombus-calls l)
  (cond
    [(and (pair? l) (eq? 'group (syntax-e* (car l))))
     (let loop ([terms (cdr l)] [found '()])
       (cond
         [(or (null? terms) (null? (cdr terms))) (reverse found)]
         [(and (symbol? (syntax-e* (car terms)))
               (rhombus-head? (cadr terms) 'parens))
          (loop (cddr terms)
                (cons (list (syntax-e* (car terms))
                            (car terms)
                            (cdr (syntax-e (cadr terms)))
                            (cadr terms))
                      found))]
         [else (loop (cdr terms) found)]))]
    [else '()]))

(define (syntax-e* s) (if (syntax? s) (syntax-e s) s))

(define (rhombus-head? s tag)
  (and (syntax? s)
       (let ([e (syntax-e s)])
         (and (list? e) (pair? e) (eq? tag (syntax-e* (car e)))))))

;; The single expression a group holds, or #f when it holds more than one.
(define (rhombus-group-value g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (= 2 (length e)) (eq? 'group (syntax-e* (first e))) (second e)))

;; A keyword argument's value, which shrubbery wraps in a block.
(define (rhombus-kw-value g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (= 3 (length e)) (eq? 'group (syntax-e* (first e)))
       (keyword? (syntax-e* (second e)))
       (let ([b (third e)])
         (and (rhombus-head? b 'block)
              (let ([inner (cdr (syntax-e b))])
                (and (= 1 (length inner)) (rhombus-group-value (car inner))))))))

(define (rhombus-kw-name g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (>= (length e) 2) (eq? 'group (syntax-e* (first e)))
       (keyword? (syntax-e* (second e)))
       (syntax-e* (second e))))

;; The leaves a scope defines and places by name. A talk that draws nine nodes
;; writes `def n_update = shape_pict(~width: 135.0, ...)` once and places it
;; with `at(338.516, 704.223, ~tag: "update", n_update)`, so the size the editor
;; would drag is stated at the `def` and not at the `at`. Sixty-three of one
;; talk's four hundred elements, and no corner of any of them could be dragged.
;;
;; Only a name exactly one `at` places: two would mean an edit to one of them
;; silently resizing the other, which is not what the corner was dragged for.
;; Memoized per scope, since every `at` in it asks the same question.
(define local-leaf-cache (make-weak-hasheq))

(define (local-leaf-defs g)
  (hash-ref! local-leaf-cache g (lambda () (compute-local-leaf-defs g))))

(define (compute-local-leaf-defs g)
  (define defs (make-hash))
  (define placed (make-hash))
  (let walk ([s g])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      ;; `def name = <call>` or `def name: <block holding one call>`.
      (when (and (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
                 (eq? 'def (syntax-e* (second l))) (symbol? (syntax-e* (third l))))
        (define name (syntax-e* (third l)))
        (define value
          (cond
            [(and (= 6 (length l)) (rhombus-head? (sixth l) 'parens))
             ;; `def name = call(...)`: the group from the call onward.
             (datum->syntax #f (list (first l) (fifth l) (sixth l)) (fifth l))]
            [(and (= 4 (length l)) (rhombus-head? (fourth l) 'block))
             (let* ([b (let ([e (syntax-e (fourth l))]) (and (list? e) e))]
                    [inner (and b (= 2 (length b)) (second b))])
               inner)]
            [else #f]))
        (when value (hash-set! defs name value)))
      ;; And which names an `at` places, so a name used twice is left alone.
      (define call (rhombus-call l))
      (when (and call (eq? 'at (car call)))
        (define positional (filter (lambda (x) (not (rhombus-kw-name x))) (cdr call)))
        (when (pair? positional)
          (define child (rhombus-group-value (last positional)))
          (when (and child (symbol? (syntax-e* child)))
            (hash-update! placed (syntax-e* child) add1 0))))
      (for-each walk l)))
  (for/hash ([(name value) (in-hash defs)]
             #:when (= 1 (hash-ref placed name 0)))
    (values name value)))

(define (call-has-literal-tag? args)
  (for/or ([g (in-list args)])
    (and (eq? '#:tag (rhombus-kw-name g))
         (let ([v (rhombus-kw-value g)]) (and v (string? (syntax-e* v)))))))

(define (parse-rhombus-at args [leaf-defs (hash)] [automatic-tag #f])
  ;; A positional argument is any group that is not a keyword argument. Note it
  ;; can hold more than one term: the last one is a call, so its group is
  ;; `(group shape_pict (parens ...))`.
  (define positional (filter (lambda (g) (not (rhombus-kw-name g))) args))
  (define kws (for/hash ([g (in-list args)] #:when (rhombus-kw-name g))
                (values (rhombus-kw-name g) (rhombus-kw-value g))))
  ;; `~tag: "Box"` on the `at`, or `tag(p, "Box")` around what it places: the
  ;; same name either way, and the second is the one that also works where a
  ;; canvas is not doing the placing.
  (define kw-tag-stx (hash-ref kws '#:tag #f))
  (define tag-stx (or kw-tag-stx (tag-call-name-stx args)))
  (define tag (or (and tag-stx (string? (syntax-e* tag-stx)) (syntax-e* tag-stx))
                  (and (not tag-stx) automatic-tag)))
  ;; A tag the program states but does not spell out -- `~tag: tag` inside a
  ;; helper -- is neither a name an edit can be written to nor an `at` glide may
  ;; name itself: a second `~tag:` in one call is a program that does not
  ;; compile. Those are left alone, as they were before glide named anything.
  (and tag
       (>= (length positional) 3)
       (let* ([written (last positional)]
              [named (let ([v (rhombus-group-value written)])
                       (and v (symbol? (syntax-e* v))
                            (hash-ref leaf-defs (syntax-e* v) #f)))]
              [child (or named written)])
         (at-site tag
                  (and (>= (length positional) 2)
                       (literal-range (rhombus-group-value (first positional)) real?))
                  (and (>= (length positional) 2)
                       (literal-range (rhombus-group-value (second positional)) real?))
                  (literal-range (hash-ref kws '#:rotate #f) real?)
                  (rhombus-child-size child 'width)
                  (rhombus-child-size child 'height)
                  (rhombus-child-paragraph-texts child)
                  (rhombus-nudge (hash-ref kws '#:nudge #f))
                  ;; Where an argument can be added: after `~tag:`, where the
                  ;; program wrote one. A name from `tag(p, "name")` sits in a
                  ;; call of its own, and so does no name at all -- both take
                  ;; the place the walk works out, just inside the parenthesis
                  ;; that closes this call.
                  (cond
                    [kw-tag-stx
                     (let ([r (range-of kw-tag-stx)]) (and r (rng-end r)))]
                    [else (group-end written)])
                  #f #f
                  (rhombus-child-flag child '#:flip_h)
                  (rhombus-child-flag child '#:flip_v)
                  (rhombus-child-insert child)
                  (style-sites child)))))

;; Any call that carries a literal `~tag:` is a site, not only `at`.
;;
;; A talk draws things with helpers of its own -- a bubble pinned to a token, a
;; callout with a spike -- and what those draw is an element on the slide with
;; nothing in the source an edit could be written into: the position is
;; measured from the thing it points at, and the words are arguments the helper
;; lays out itself. Writing `~tag:` on the call says "what this draws is one
;; element, and it is called this", which is what makes an edit to it something
;; that can be found and written. The helper passes the tag on to the `at` it
;; builds, so the element on the slide answers to the same name.
;;
;; What can be written is what the call states as a literal: the words, one run
;; per string, and a `~nudge:` for the position -- the helper keeps whatever it
;; computes and the correction says how far off it was, which is what a hand
;; adjustment to a pinned bubble is. A helper that offers this has to take
;; `~nudge:` and pass it on, or the correction would be written and never
;; drawn.
;;
;; `whole` is #f: the call is not a form this can delete on its own, because the
;; program may name it and draw it somewhere else -- and that also keeps it out
;; of the anchors a newly drawn shape is written next to.
(define (parse-rhombus-tagged-call args [call-stx #f])
  (define kws (for/hash ([g (in-list args)] #:when (rhombus-kw-name g))
                (values (rhombus-kw-name g) (rhombus-kw-value g))))
  ;; `~tag: "name"` on the call, or a `tag(p, "name")` among its arguments: the
  ;; second is how a pict something other than a canvas places gets a name, and
  ;; the call that places it is where an edit to it can be written.
  (define kw-tag-stx (hash-ref kws '#:tag #f))
  (define tag-stx (or kw-tag-stx (tag-call-name-stx args)))
  (define tag (and tag-stx (string? (syntax-e* tag-stx)) (syntax-e* tag-stx)))
  (and tag
       (let ([after-tag (and kw-tag-stx
                             (let ([r (range-of kw-tag-stx)]) (and r (rng-end r))))]
             [words (call-string-ranges args (range-of tag-stx))])
         (at-site tag #f #f
                  (literal-range (hash-ref kws '#:rotate #f) real?)
                  (literal-range (hash-ref kws '#:width #f) real?)
                  (literal-range (hash-ref kws '#:height #f) real?)
                  (if (null? words) '() (list words))
                  (rhombus-nudge (hash-ref kws '#:nudge #f))
                  after-tag
                  #f #f #f #f #f
                  ;; What the call itself says about how the thing looks: a
                  ;; helper that takes `~fill:` offers a colour the way
                  ;; `~nudge:` offers a position. What it keeps to itself is
                  ;; reported rather than written.
                  (if call-stx (style-sites call-stx) '())))))

;; The name a `tag(p, "name")` among these arguments gives, as the syntax of the
;; string itself so that its extent is known. The first one only: a call that
;; names two picts places two elements, and which of them an edit belongs to is
;; not something one call can say.
;;
;; The search stops at another call's parentheses, so the name belongs to the
;; innermost call that holds it: `slide_canvas(at(0, 0, tag(p, "Box")))` names
;; nothing itself, and the `at` inside it names "Box".
(define (tag-call-name-stx args)
  (define found (box #f))
  (for ([g (in-list args)])
    (let walk ([s g])
      (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
      (when (and l (not (unbox found)))
        (let loop ([terms l])
          (cond
            [(unbox found) (void)]
            [(or (null? terms) (null? (cdr terms)))
             (for-each walk terms)]
            [(and (eq? 'tag (syntax-e* (car terms)))
                  (rhombus-head? (second terms) 'parens))
             (define inner (cdr (syntax-e (second terms))))
             ;; `tag(p, "name")`: the name is the second argument.
             (when (>= (length inner) 2)
               (define v (rhombus-group-value (second inner)))
               (when (and v (string? (syntax-e* v))) (set-box! found v)))
             (unless (unbox found) (loop (cddr terms)))]
            [(and (symbol? (syntax-e* (car terms)))
                  (rhombus-head? (second terms) 'parens))
             ;; Another call: what it places, it names.
             (loop (cddr terms))]
            [else (walk (car terms)) (loop (cdr terms))])))))
  (unbox found))

;; Every string the call holds, in the order they are written, less the tag
;; itself: the words a helper draws are its arguments, and one string is one run
;; -- so retyping a word rewrites the string it came from, and a retyping that
;; runs across two of them is refused the way it is anywhere else.
(define (call-string-ranges args skip)
  (define acc '())
  (for ([g (in-list args)])
    (let walk ([s g])
      (cond
        [(syntax? s)
         (when (string? (syntax-e s))
           (define r (range-of s))
           (when (and r (not (and skip (= (rng-start r) (rng-start skip)))))
             (set! acc (cons r acc))))
         (walk (syntax-e s))]
        [(pair? s) (walk (car s)) (walk (cdr s))]
        [else (void)])))
  (sort acc < #:key rng-start))

;; `~nudge: [12.0, -4.0]` -> (list range dx dy).
;; The text being read, so an extent that syntax cannot give can be found in it.
(define current-source-text (make-parameter ""))

(define (rhombus-bracket-extent stx)
  (define text (current-source-text))
  (define kw (range-of stx))
  ;; `stx` is the brackets wrapper and its elements are groups, neither of which
  ;; has a position -- only the leaf inside the first group does, and the `[` is
  ;; the first one before it.
  (define inner
    (let ([e (and (syntax? stx) (syntax-e stx))])
      (and (list? e) (pair? (cdr e))
           (let ([v (rhombus-group-value (second e))])
             (and v (range-of v))))))
  (cond
    [(and (not kw) inner)
     (define open
       (let loop ([j (sub1 (rng-start inner))])
         (cond [(< j 0) #f]
               [(char=? (string-ref text j) #\[) j]
               [else (loop (sub1 j))])))
     (define shut (and open (match-close text open
                                         (lambda (c d) (and (char=? c #\/) (eqv? d #\/)))
                                         (lambda (c d) (and (char=? c #\/) (eqv? d #\*))))))
     (and shut (make-range open (add1 shut)))]
    [else kw]))

(define (rhombus-nudge stx)
  (define e (and stx (syntax? stx) (syntax-e stx)))
  (and (list? e) (eq? 'brackets (syntax-e* (car e)))
       (let ([gs (map rhombus-group-value (cdr e))])
         (and (= 2 (length gs)) (andmap values gs)
              (real? (syntax-e* (first gs))) (real? (syntax-e* (second gs)))
              (list (rhombus-bracket-extent stx)
                    (syntax-e* (first gs)) (syntax-e* (second gs)))))))

;; The child of an `at` is a group holding one call, so its keyword arguments
;; are found the same way.
;; Where a leaf states its size. Most of them say `~width:` and `~height:`; a
;; picture says both positionally -- `image_pict(media("logo.png"), 115.0,
;; 115.0)` -- and looking only for the keywords is why not one picture in a talk
;; could be resized by dragging its corner. Ninety-one of that talk's four
;; hundred elements, and every one of them a picture.
(define (rhombus-child-size child which)
  (or (rhombus-child-kw child (if (eq? 'width which) '#:width '#:height))
      (let* ([l (and (syntax? child) (let ([e (syntax-e child)]) (and (list? e) e)))]
             [call (and l (rhombus-call l))])
        (and call (eq? 'image_pict (car call))
             (let ([pos (filter (lambda (g) (not (rhombus-kw-name g))) (cdr call))])
               (and (>= (length pos) 3)
                    (literal-range (rhombus-group-value
                                    (if (eq? 'width which) (second pos) (third pos)))
                                   real?)))))))

(define (rhombus-child-kw child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (literal-range (rhombus-kw-value g) real?)))))

;; The whole group a keyword introduces, rather than the one term inside it: a
;; `~fill: hex("4472C4")` is a call, not a value.
(define (rhombus-kw-group g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (rhombus-head? (third l) 'block)
       (let ([b (syntax-e (third l))])
         (and (pair? (cdr b)) (second b)))))

;; Every `run(...)` call in a leaf, so a body of one run can have its typeface
;; and size rewritten and a body of several can be told apart from it.
(define (rhombus-para-calls child) (rhombus-calls-named child '(para para*)))

(define (rhombus-run-calls child) (rhombus-calls-named child '(run run*)))

(define (rhombus-calls-named child names)
  (define acc '())
  (let walk ([s child])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      (define call (rhombus-call l))
      (when (and call (memq (car call) names))
        (set! acc (cons s acc)))
      (for-each walk l)))
  (reverse acc))

;; A keyword's literal value inside a particular call.
(define (rhombus-child-kw-in call kw pred)
  (define l (and (syntax? call) (let ([e (syntax-e call)]) (and (list? e) e))))
  (parens-kw-in (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)) kw pred))

;; The same, for an argument list already in hand.
(define (parens-kw-in parens kw pred)
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (literal-range (rhombus-kw-value g) pred)))))

(define (rhombus-child-flag-in call kw)
  (define l (and (syntax? call) (let ([e (syntax-e call)]) (and (list? e) e))))
  (define parens (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g))
              (let ([v (rhombus-kw-value g)]) (and v (range-of v)))))))

;; Where a style property is written in the source, if it is written as a
;; literal at all. A `~fill: hex("4472C4")` can be rewritten; a
;; `~fill: brand_blue` names something shared, and changing that would recolour
;; every shape using it -- so it is reported with the name instead.
;; `range` is where the value is written, when it is written at all. `insert-at`
;; and `keyword` are how to add it when it is not: a line that is solid has no
;; `~dash:` to rewrite, and adding one is a better answer than reporting that it
;; is missing.
;; `whole` is the whole argument's extent, for a property that can be written as
;; absent: a shape whose fill the editor removed says `~fill: #false`, and there
;; is no literal inside the old `hex(...)` that means that.
(struct style-site (property range shared insert-at keyword whole) #:transparent)

(define (mask-style-shared site local-names)
  (if (and site (memq (style-site-shared site) local-names))
      (struct-copy style-site site [shared #f])
      site))

(define (mask-at-site-shared site local-names)
  (struct-copy at-site site
               [styles (for/list ([style (in-list (at-site-styles site))])
                         (mask-style-shared style local-names))]))

(define (mask-slide-site-shared site local-names)
  (struct-copy slide-site site
               [background (mask-style-shared (slide-site-background site) local-names)]
               [width (mask-style-shared (slide-site-width site) local-names)]
               [height (mask-style-shared (slide-site-height site) local-names)]))

;; The literal inside the first `hex("...")` under `stx`, or the name it is given
;; instead.
(define (hex-site stx)
  (define found (box #f))
  ;; A call is a name followed by parentheses, and the two are siblings wherever
  ;; they appear: `~fill: hex("4472C4")` puts them in a group of their own, and
  ;; `def brand = hex("4472C4")` puts them at the end of a longer one.
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found))
                   (eq? 'hex (syntax-e* a))
                   (rhombus-head? b 'parens))
          (define args (cdr (syntax-e b)))
          (define v (and (pair? args) (rhombus-group-value (first args))))
          (when (and v (string? (syntax-e* v)))
            (set-box! found (cons 'literal (range-of v))))))
      (unless (unbox found) (for-each walk l))))
  (cond
    [(unbox found) => values]
    [else
     ;; Not a `hex(...)`: a bare name is something shared.
     (define nm (let ([l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e)))])
                  (and l (= 2 (length l)) (symbol? (syntax-e* (second l)))
                       (syntax-e* (second l)))))
     (and nm (cons 'shared nm))]))


;; The `hex(...)` call itself, when the colour is written as one: an `~alpha:`
;; it does not state is added just inside these parentheses.
(define (hex-call stx)
  (define found (box #f))
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found)) (eq? 'hex (syntax-e* a)) (rhombus-head? b 'parens))
          (set-box! found (cons a b))))
      (unless (unbox found) (for-each walk l))))
  (unbox found))

;; A fill the colour cannot be read out of: a gradient's first stop is a
;; `hex(...)` too, and rewriting it would leave the fill a gradient.
(define (compound-fill? stx) (and (fill-call-named stx '(gradient_fill image_fill pattern_fill)) #t))

;; A gradient can at least be replaced whole -- by a colour, when that is what
;; the editor set. An image or a pattern fill cannot: the comparison does not
;; see one, so a change to a shape wearing one is reported rather than guessed.
(define (gradient-fill-source? stx) (and (fill-call-named stx '(gradient_fill)) #t))

(define (fill-call-named stx names)
  (let walk ([s stx])
    (define e (and (syntax? s) (syntax-e s)))
    (cond [(memq e names) #t]
          [(list? e) (for/or ([x (in-list e)]) (walk x))]
          [else #f])))

;; The same, as the single term a literal is written as: `kw-value-stx` answers
;; with the whole group, which is what a colour has to be walked for.
;; A quoted name, `#'center`, which is two terms: the quote and the name. The
;; group around them carries no position of its own, so the extent comes from
;; the two ends.
;; A `[...]` value's extent. The bracket wrapper carries no position, but its
;; head term spans the brackets and everything between them.
;; The first bracketed list anywhere under `g`: a shape's adjustments sit
;; inside `preset_geom`'s own parentheses, which the search below does not
;; reach.
(define (bracket-extent-within g)
  (let walk ([s g])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (and l
         (or (and (eq? 'brackets (syntax-e* (car l))) (range-of (car l)))
             (for/or ([x (in-list (cdr l))]) (walk x))))))

(define (bracket-extent g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (define br (and l (findf (lambda (x) (rhombus-head? x 'brackets)) l)))
  (and br (range-of (first (syntax-e br)))))

(define (quoted-range g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (define terms (if (and (pair? l) (eq? 'group (syntax-e* (first l)))) (cdr l) (or l '())))
  (and (= 2 (length terms))
       (let ([a (range-of (first terms))] [b (range-of (second terms))])
         (and a b (make-range (rng-start a) (rng-end b) (rng-source a))))))

;; A keyword's value inside an argument list already in hand.
(define (parens-kw-group parens kw)
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-group g)))))

;; The extent of a whole value, when it is a call.
(define (value-extent-of stx)
  (define n (call-name stx))
  (and n (rhombus-call-extent (current-source-text) n)))

;; The first string a named call is given, wherever it appears: `media("x.png")`
;; is a call like `hex("...")`, and its argument is what says which file.
(define (call-string-range stx name)
  (define found (box #f))
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found)) (eq? name (syntax-e* a)) (rhombus-head? b 'parens))
          (define args (cdr (syntax-e b)))
          (define v (and (pair? args) (rhombus-group-value (first args))))
          (when (and v (string? (syntax-e* v))) (set-box! found (range-of v)))))
      (unless (unbox found) (for-each walk l))))
  (unbox found))

(define (kw-single-stx child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-value g)))))

(define (kw-value-stx child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-group g)))))

;; The properties this can find in one `at` form.
(define (style-sites child)
  (define fill-stx (kw-value-stx child '#:fill))
  ;; The whole value of a keyword argument, which is what has to be rewritten
  ;; when there is no single literal inside it that says the same thing: an
  ;; outline being taken away, or a line spacing that is a `pair(...)`.
  (define (value-extent stx)
    (define name (call-name stx))
    (and name (rhombus-call-extent (current-source-text) name)))
  ;; Stated, or not stated. What the source states is rewritten where it
  ;; stands; what it does not is added. Never both: a second `~width:` in one
  ;; call would not compile, so an argument that is there but not a literal is
  ;; reported instead.
  ;;
  ;; `how` says what the value looks like: a predicate for a literal, `'call`
  ;; for a value that is a call of its own, `'flag` for a boolean.
  (define (kw-site property call kw how)
    (and call
         (let ([g (kw-value-stx call kw)])
           (if g
               (style-site property
                           (case how
                             [(call) (value-extent g)]
                             [(quoted) (quoted-range g)]
                             [(list) (bracket-extent g)]
                             [(flag) (literal-range (kw-single-stx call kw) boolean?)]
                             [else (literal-range (kw-single-stx call kw) how)])
                           #f #f kw #f)
               (style-site property #f #f (call-append-at call) kw #f)))))
  ;; Which file a picture draws, which the source names inside `media(...)`.
  (define (media-site call)
    (and call
         (let ([r (call-string-range call 'media)])
           (and r (style-site 'image r #f #f #f #f)))))
  ;; How much of a picture is cropped away: a list when there is a crop and
  ;; nothing at all when there is not, so it is added and removed like a fill.
  (define (crop-site call)
    (define g (and call (kw-value-stx call '#:crop)))
    (define r (and g (or (bracket-extent g)
                         (literal-range (kw-single-stx call '#:crop) boolean?))))
    (cond
      [(and g r) (style-site 'crop r #f #f '#:crop r)]
      [g #f]
      [call (style-site 'crop #f #f (call-append-at call) '#:crop #f)]
      [else #f]))
  ;; An arrowhead is a `line_end(...)` call when there is one and `#false` when
  ;; there is not, so both the value and its absence are written in place.
  (define (end-site property call kw)
    (and call
         (let* ([g (kw-value-stx call kw)]
                [r (and g (or (value-extent g) (literal-range (kw-single-stx call kw) boolean?)))])
           (cond
             [(and g r) (style-site property r #f #f kw r)]
             [g #f]
             [else (style-site property #f #f (call-append-at call) kw #f)]))))
  ;; A leaf can be any call at all -- a program refactored into `vstack` and
  ;; `beside` still draws the shape the editor is dragging. So an argument is
  ;; only ever added to a call known to take it: adding `~crop:` to a `vstack`
  ;; is a program that no longer runs.
  ;; The shape it is drawn as. Written where the source says it: the name inside
  ;; `~geom: preset_geom("roundRect", ...)` when the shape carries adjustments,
  ;; and `~shape: "roundRect"` when it does not -- `~geom:` wins in the runtime,
  ;; so that is the one to rewrite when both could be there. A shape drawn from
  ;; a path has a name on neither side that the other could be rewritten in, so
  ;; changing one of those into a preset is reported and not written.
  (define (shape-site child)
    (define geom-stx (kw-value-stx child '#:geom))
    (cond
      [geom-stx
       (let ([r (call-string-range geom-stx 'preset_geom)])
         (and r (style-site 'shape r #f #f #f #f)))]
      [else (kw-site 'shape (leaf-taking 'shape_pict) '#:shape string?)]))
  ;; What the shape's own handles say -- the roundness of a rounded rectangle.
  ;; The list `preset_geom` takes is what is rewritten; a shape named with
  ;; `~shape:` states none, so reshaping that one is reported.
  (define (shape-adjust-site child)
    (define geom-stx (kw-value-stx child '#:geom))
    (and geom-stx
         (let ([r (bracket-extent-within geom-stx)])
           (and r (style-site 'shape-adjust r #f #f #f #f)))))
  (define leaf-name (let ([n (call-name child)]) (and n (syntax-e* n))))
  (define (leaf-taking . names) (and leaf-name (memq leaf-name names) child))
  ;; The colour argument, however the source states it: a `hex(...)` to rewrite,
  ;; a shared name, `#false` for a shape that has none, or nothing at all -- in
  ;; which case the whole argument is added, where there is a call to add it to.
  (define (paint-site property kw stx [addable #f])
    (define hit (and stx (not (compound-fill? stx)) (hex-site stx)))
    (cond
      [(and stx (gradient-fill-source? stx))
       (style-site property #f #f #f kw (value-extent stx))]
      [(and stx (compound-fill? stx)) #f]
      [hit (style-site property
                       (and (eq? 'literal (car hit)) (cdr hit))
                       (and (eq? 'shared (car hit)) (cdr hit))
                       #f #f (value-extent stx))]
      [(and stx (literal-range stx boolean?))
       => (lambda (r) (style-site property #f #f #f kw r))]
      [(not stx) (and addable
                      (style-site property #f #f (call-append-at addable) kw #f))]
      [else #f]))
  ;; A colour's alpha lives inside its own `hex(...)`, so a shape made
  ;; translucent in the editor is an `~alpha:` written or added there.
  (define opacity
    (and fill-stx (not (compound-fill? fill-stx)) (hex-call fill-stx)
         (kw-site 'fill-opacity fill-stx '#:alpha real?)))
  ;; The stroke is a call of its own, so its colour, width and dash sit inside
  ;; it.
  (define stroke-stx (kw-value-stx child '#:line))
  (define stroke (and stroke-stx (call-name stroke-stx) stroke-stx))
  ;; The call that carries the body's own properties: a `textbox(...)` is one,
  ;; and a shape's `~body: body(...)` is the other.
  (define body-call
    (let ([nm (call-name child)])
      (if (and nm (memq (syntax-e* nm) '(textbox text_box)))
          child
          (kw-value-stx child '#:body))))
  ;; Every run and every paragraph, numbered as the state numbers them: the
  ;; k-th `run(...)` call in the source is the k-th run of the body, because
  ;; both read them paragraph by paragraph. Bolding one word of a line is a
  ;; change to the run that word is in.
  (define runs (rhombus-run-calls child))
  (define paras (rhombus-para-calls child))
  (define run (and (pair? runs) (first runs)))
  (define one-para (and (pair? paras) (first paras)))
  ;; A run's colour is a call too.
  (define (run-colour-site property r)
    (and r
         (let* ([v (kw-value-stx r '#:color)]
                [hit (and v (hex-site v))])
           (cond
             [hit (style-site property
                              (and (eq? 'literal (car hit)) (cdr hit))
                              (and (eq? 'shared (car hit)) (cdr hit))
                              #f #f #f)]
             ;; Text whose colour the source never states, recoloured in the
             ;; editor: the argument is added to the run.
             [(not v) (style-site property #f #f (call-append-at r) '#:color #f)]
             [else #f]))))
  ;; The runs and the paragraphs, each under its own number.
  (define run-sites
    (append*
     (for/list ([r (in-list runs)] [i (in-naturals 1)])
       (list (kw-site (nth-property 'size i) r '#:size real?)
             (kw-site (nth-property 'font i) r '#:font string?)
             (kw-site (nth-property 'bold i) r '#:bold 'flag)
             (kw-site (nth-property 'italic i) r '#:italic 'flag)
             (kw-site (nth-property 'underline i) r '#:underline 'flag)
             (kw-site (nth-property 'strike i) r '#:strike 'flag)
             (kw-site (nth-property 'spacing i) r '#:spacing real?)
             (kw-site (nth-property 'caps i) r '#:caps 'quoted)
             (kw-site (nth-property 'baseline i) r '#:baseline real?)
             (run-colour-site (nth-property 'text-color i) r)))))
  (define para-sites
    (append*
     (for/list ([p (in-list paras)] [i (in-naturals 1)])
       (list (kw-site (nth-property 'align i) p '#:align 'quoted)
             (kw-site (nth-property 'line-spacing i) p '#:line_spacing 'call)
             (kw-site (nth-property 'space-before i) p '#:space_before real?)
             (kw-site (nth-property 'space-after i) p '#:space_after real?)
             (kw-site (nth-property 'level i) p '#:level real?)
             (kw-site (nth-property 'margin-left i) p '#:margin_left real?)
             (kw-site (nth-property 'indent i) p '#:indent real?)
             (kw-site (nth-property 'bullet i) p '#:bullet 'call)))))
  (append
   (filter values run-sites)
   (filter values para-sites)
   (filter values
          (list (shape-site child)
                (shape-adjust-site child)
                (paint-site 'fill '#:fill fill-stx (leaf-taking 'shape_pict))
                opacity
                (paint-site 'line '#:line stroke-stx
                            (leaf-taking 'shape_pict 'image_pict))
                (kw-site 'line-width stroke '#:width real?)
                (kw-site 'dash stroke '#:dash 'quoted)
                (kw-site 'cap stroke '#:cap 'quoted)
                (end-site 'head stroke '#:head)
                (end-site 'tail stroke '#:tail)

                ;; A picture's own arguments.
                (media-site (leaf-taking 'image_pict))
                (kw-site 'opacity (leaf-taking 'image_pict) '#:opacity real?)
                (crop-site (leaf-taking 'image_pict))
                (kw-site 'anchor body-call '#:anchor 'quoted)
                (kw-site 'wrap body-call '#:wrap 'flag)
                (kw-site 'autofit body-call '#:autofit 'quoted)
                (kw-site 'insets body-call '#:insets 'call)))))

;; A boolean keyword on the leaf, as (range . value) -- so a flip that is
;; already there can be set either way. `#:width` and friends carry a number and
;; go through `rhombus-child-kw`; a flip carries `#true` or `#false`.
(define (rhombus-child-flag child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g))
              (let ([v (rhombus-kw-value g)])
                (and v (range-of v)))))))

;; Just inside the leaf call's parentheses, which is where a keyword it does not
;; have yet can be added.
(define (rhombus-child-insert child) (call-insert-at child))

;; Just inside a call's parentheses, which is where an argument it does not have
;; yet can be added.
(define (call-insert-at stx)
  (define r (call-name-range stx))
  (define open (and r (next-open (current-source-text) (rng-end r))))
  (and open (add1 open)))

;; A call's head: the symbol whose next sibling is the argument list.
(define (call-name stx)
  (define l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e))))
  (for/or ([a (in-list (or l '()))] [b (in-list (cdr (or l '(1))))])
    (and (symbol? (syntax-e* a)) (rhombus-head? b 'parens) a)))

(define (call-name-range stx)
  (let ([n (call-name stx)]) (and n (range-of n))))

;; Where a new argument belongs: after the last keyword argument there is, which
;; is how a call is laid out to begin with -- options first, content after. So
;; `textbox(~width: 300.0, ~anchor: #'center, para(...))` rather than an anchor
;; trailing the paragraphs. A call stating no options takes it at the end, which
;; is where `hex("ED7D31", ~alpha: 0.5)` wants it anyway.
(define (call-append-at stx)
  (define parens (call-parens stx))
  (define last-kw
    (and parens
         (for/fold ([best #f]) ([g (in-list (cdr (syntax-e parens)))])
           (if (rhombus-kw-name g)
               (let ([e (group-end g)]) (if (and e (> e (or best 0))) e best))
               best))))
  (define name (call-name stx))
  (define r (and (not last-kw) name (rhombus-call-extent (current-source-text) name)))
  (cond
    [last-kw last-kw]
    [r (back-over-space (current-source-text) (sub1 (rng-end r)))]
    [else #f]))

;; A call's argument list.
(define (call-parens stx)
  (define l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e))))
  (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)))

;; How far a group reaches. The group itself carries no position, but every term
;; in it does -- including the head of a nested `(...)`, which spans the lot.
(define (group-start stx)
  (let walk ([s stx] [best #f])
    (define r (and (syntax? s) (range-of s)))
    (define here (if (and r (< (rng-start r) (or best +inf.0))) (rng-start r) best))
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (if l (for/fold ([b here]) ([x (in-list l)]) (walk x b)) here)))

(define (group-end stx)
  (let walk ([s stx] [best #f])
    (define r (and (syntax? s) (range-of s)))
    (define here (if (and r (> (rng-end r) (or best 0))) (rng-end r) best))
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (if l (for/fold ([b here]) ([x (in-list l)]) (walk x b)) here)))

;; The comma a new last argument needs, which is none when the call had none.
(define (argument-comma text at)
  (if (and (> at 0) (char=? #\( (string-ref text (sub1 at)))) "" ", "))

;; The runs' string literals, grouped by the paragraph they are in: the text of
;; a body is its paragraphs joined by newlines, so where the paragraphs are is
;; what says which run a retyped word landed in.
(define (rhombus-child-paragraph-texts child)
  (define paras (rhombus-para-calls child))
  (if (pair? paras)
      (filter pair? (for/list ([p (in-list paras)]) (rhombus-child-texts p)))
      (let ([rs (rhombus-child-texts child)]) (if (null? rs) '() (list rs)))))

(define (rhombus-child-texts child)
  (define acc '())
  (let walk ([s child])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      (define call (rhombus-call l))
      (when (and call (memq (car call) '(run run_star)))
        (define first-arg (and (pair? (cdr call)) (rhombus-group-value (second call))))
        (when (and first-arg (string? (syntax-e* first-arg)))
          (set! acc (cons (range-of first-arg) acc))))
      (for-each walk l)))
  (reverse (filter values acc)))

;; --------------------------------------------------------------- applying

;; Applies the actions a merge produced, editing only literals. Returns
;; (values applied skipped).
;; Deleting an element takes with it the comment line that introduces it and the
;; whitespace that separated it from its siblings -- and, in Rhombus, the comma
;; that joined it to them, so the argument list stays well formed.
(define (deletion-range text r)
  (define comment "//")
  (define (line-start i)
    (let loop ([j i])
      (cond [(<= j 0) 0]
            [(char=? (string-ref text (sub1 j)) #\newline) j]
            [else (loop (sub1 j))])))
  ;; Absorb whole lines above while they are blank or comments.
  (define start
    (let loop ([s (rng-start r)])
      (define ls (line-start s))
      (cond
        [(not (string=? "" (string-trim (substring text ls s)))) s]
        [(zero? ls) ls]
        [else
         (define prev-start (line-start (sub1 ls)))
         (define prev (string-trim (substring text prev-start (sub1 ls))))
         (if (or (string=? "" prev) (string-prefix? prev comment))
             (loop prev-start)
             ;; Take the newline that ended the line above, so no blank is left.
             (sub1 ls))])))
  (define before (back-over-space text start))
  (cond
    [(and (> before 0) (char=? (string-ref text (sub1 before)) #\,))
     (make-range (sub1 before) (rng-end r) (rng-source r))]
    [else
     (define after
       (let loop ([j (rng-end r)])
         (if (and (< j (string-length text)) (char-whitespace? (string-ref text j)))
             (loop (add1 j))
             j)))
     (if (and (< after (string-length text)) (char=? (string-ref text after) #\,))
         (make-range start (add1 after) (rng-source r))
         (make-range start (rng-end r) (rng-source r)))]))

;; Within one slide definition a tag has to be unique, because that is the only
;; thing an edit is matched on. Across slides it need not be: "Title 1" on every
;; slide is the normal case. When the slide list is computed there is no per-slide
;; scope to speak of, so uniqueness has to hold across the local source tree.
(define (check-site-tags sites scopes path)
  (define where (format "~a" (if (path? path) (path->string path) path)))
  (void scopes)
  (for ([scope (in-list (remove-duplicates (map at-site-scope sites)))])
    (check-unique-tags (for/list ([s (in-list sites)]
                                 #:when (equal? scope (at-site-scope s)))
                         (at-site-tag s))
                       (if scope (format "~a: ~a" where scope) where)
                       TAG-HINT)))

;; Every identity produced by running the program must have one static place an
;; edit can land. Check that before a base is recorded, while no editor changes
;; are at risk. One source call may legitimately appear on several pages (a
;; shared slide helper, for example); the merge treats those occurrences as a
;; family and refuses a page-local edit rather than silently changing them all.
(define (runtime-source-site sites scopes slide-index tag)
  (define key (tag-key tag))
  (define scope (and scopes (<= 1 slide-index (length scopes))
                     (list-ref scopes (sub1 slide-index))))
  (define scoped
    (for/list ([site (in-list sites)]
               #:when (and (equal? scope (at-site-scope site))
                           (equal? key (tag-key (at-site-tag site)))))
      site))
  (define global
    (for/list ([site (in-list sites)]
               #:when (equal? key (tag-key (at-site-tag site))))
      site))
  (cond
    ;; Several sites under one tag in one slide are one editable family: moving
    ;; them together writes every site, while moving only one is reported as
    ;; ambiguous by the merge. What cannot be recovered is a tag that has no
    ;; site in this page's scope and several unrelated sites elsewhere.
    [(pair? scoped) (first scoped)]
    [(= 1 (length global)) (first global)]
    [else #f]))

(define (validate-runtime-sites sites scopes slides path)
  ;; Stage expansion can turn one declared slide into several editor pages.
  ;; `slide-origins` records which declaration owns each page.
  (define page-scopes (scopes-by-deck-slide scopes))
  (unless (and page-scopes (= (length page-scopes) (length slides)))
    (error 'glide
           (string-append
            "~a declares ~a source owner~a, but the running pages cannot all be traced to them.\n"
            "  Keep the running order in one `glide_slides` declaration; do not compute or append pages outside it.")
           path (length scopes) (if (= 1 (length scopes)) "" "s")
           ))
  (define missing
    (remove-duplicates
     (for*/list ([slide (in-list slides)]
                 [e (in-list (slide-state-elements slide))]
                 #:when (and (el-state-tag e)
                             (not (runtime-source-site
                                   sites page-scopes (slide-state-index slide)
                                   (el-state-tag e)))))
       (el-state-tag e))))
  (when (pair? missing)
    (error 'glide
           (string-append
            "~a produces editor identities that have no writable source site:\n~a"
            "  Put an ordinary object in `at(...)`. For a helper whose inner `at` is\n"
            "  called more than once, put a distinct literal `~~tag:` on each outer\n"
            "  helper call and pass its tag and `~~nudge:` through.")
           path
           (apply string-append
                  (for/list ([tag (in-list missing)])
                    (format "    ~s\n" (or (automatic-tag-name tag) tag))))))
  (void))

;; A single static call can draw on more than one editor page. That is useful
;; for shared slide helpers, but a drag on only one occurrence cannot be
;; represented by changing that one call: it would move every occurrence.
;; Identify such keys so a save is refused before any source edit is made.
(define (shared-runtime-site-keys sites scopes slides)
  (define page-scopes (scopes-by-deck-slide scopes))
  (define origins (unbox slide-origins))
  (define (logical-slide page)
    (if (<= 1 page (length origins))
        (list-ref origins (sub1 page))
        page))
  (define occurrences (make-hasheq))
  (for* ([slide (in-list slides)]
         [e (in-list (slide-state-elements slide))]
         #:when (el-state-tag e))
    (define site
      (runtime-source-site sites page-scopes (slide-state-index slide) (el-state-tag e)))
    (when site
      (hash-update! occurrences site
                    (lambda (ps) (cons (logical-slide (slide-state-index slide)) ps)) '())))
  (define by-key
    (for*/fold ([h (hash)]) ([(site ps) (in-hash occurrences)]
                              #:when (> (length (remove-duplicates ps)) 1))
      (hash-update h (tag-key (at-site-tag site))
                   (lambda (old) (append ps old)) '())))
  (for/hash ([(key ps) (in-hash by-key)])
    (values key (sort (remove-duplicates ps) <))))

(define (protect-shared-runtime-sites actions shared)
  (define (keys a)
    (case (sync-action-kind a)
      [(moved resized retext restyle removed)
       (list (tag-key (sync-action-tag a)))]
      [(grouped)
       (for/list ([child (in-list (second (sync-action-detail a)))])
         (tag-key (car child)))]
      [(restacked) (map tag-key (sync-action-detail a))]
      [else '()]))
  (define hit
    (for*/or ([a (in-list actions)] [key (in-list (keys a))])
      (and (hash-ref shared key #f) key)))
  (if (not hit)
      actions
      ;; Treat the whole editor save as one refused operation even for callers
      ;; that did not request atomic application. Grouping and ungrouping are
      ;; several low-level actions, and applying the unrelated-looking half is
      ;; another way to corrupt the shared family.
      (for/list ([a (in-list actions)] #:unless (eq? 'noted (sync-action-kind a)))
        (struct-copy sync-action a
                     [kind 'ambiguous]
                     [detail
                      (format (string-append
                               "this source object also appears on slides ~a; changing only"
                               " one occurrence would change them all -- give independently"
                               " editable helper calls distinct literal `~~tag:` arguments")
                              (hash-ref shared hit))]))))

;; Direct callers of `apply-actions!` still need a per-scope account of an
;; ambiguous source. Normal sync validates earlier, before recording a base.
(define (ambiguous-scopes sites scopes path)
  (define where (format "~a" (if (path? path) (path->string path) path)))
  (cond
    [(not scopes)
     (check-unique-tags (map at-site-tag sites)
                        (format "~a (its slide list is not traceable)" where)
                        TAG-HINT)
     (hash)]
    [else
     (for/fold ([h (hash)]) ([scope (in-list (remove-duplicates
                                              (map at-site-scope sites)))])
       (define dups
         (duplicate-tags
          (for/list ([s (in-list sites)]
                     #:when (equal? scope (at-site-scope s)))
            (at-site-tag s))))
       (if (null? dups)
           h
           (hash-set h scope
                     (cons
                      (format "~a uses one tag for more than one element: ~a"
                              (or scope "the slide")
                              (string-join
                               (for/list ([d (in-list dups)])
                                 (format "~s appears ~a times" (car d) (cdr d)))
                               ", "))
                      (map car dups)))))]))

;; An element added in the editor has to be written as source, which is the same
;; job the translator does -- so it is the same code, for one element, at the
;; indentation its new siblings sit at.
(define (added-element d index tag [z #f])
  (and d
       (let ([s (for/first ([s (in-list (deck-slides d))]
                            #:when (= index (slide-index s)))
                  s)])
         (and s
              ;; Where the state said it was drawn, which is exact -- and a
              ;; shape the editor made itself may have no name at all, or a name
              ;; another element on the slide already has.
              (or (and z (< z (length (slide-elements s)))
                       (list-ref (slide-elements s) z))
                  (let loop ([es (slide-elements s)])
                    (for/or ([e (in-list es)])
                      (cond
                        [(group? e) (loop (group-children e))]
                        [(equal? tag (element-name e)) e]
                        [else #f]))))))))

;; Where an `added` action's element sits in the deck's drawing order.
(define (added-z a)
  (let ([d (sync-action-detail a)])
    (and (list? d) (>= (length d) 3) (third d))))

;; The srcs an element needs, so they can be copied next to the program.
(define (element-media e)
  (define acc '())
  (define (note! v) (when (string? v) (set! acc (cons v acc))))
  (let walk ([e e])
    (cond
      [(picture? e) (note! (picture-src e))
                    (when (image-fill? (picture-fill e))
                      (note! (image-fill-src (picture-fill e))))]
      [(shape? e) (when (image-fill? (shape-fill e))
                    (note! (image-fill-src (shape-fill e))))]
      [(group? e) (for-each walk (group-children e))]
      [else (void)]))
  (remove-duplicates acc))

;; The face a run in this program gets when it names none: whatever the program
;; passed to `current_default_font`. An element written in names its own face
;; against that, not against the deck's most common one. #f when the program
;; works its default out rather than stating it, and then every run names its
;; face.
(define (program-default-font text)
  (define m (regexp-match #px"current_default_font[(] *\"([^\"]*)\"" text))
  (and m (cadr m)))

;; Slides added in the editor, written into the program as `def slide_N`
;; definitions and entered in `all_slides`.
;;
;; The definitions go after the last existing one, whatever order the slides
;; belong in: `all_slides` is what decides the deck's order, so appending keeps
;; the edit to two splices instead of renumbering and reflowing the file.
(define (apply-added-slides! program-path actions slide-sites layout d
                            media-names media-subdir)
  (define text (file->string (program-layout-source layout)))
  (define taken
    (for/list ([ss (in-list slide-sites)]) (symbol->string (slide-site-scope ss))))
  (define next
    (add1 (apply max 0
                 (for/list ([n (in-list taken)])
                   (cond
                     [(regexp-match #rx"([0-9]+)$" n) => (lambda (m) (string->number (cadr m)))]
                     [else 0])))))
  ;; In deck order, so the names read in the order the slides appear.
  (define (position a)
    ;; (after . seq): where in the program's order, then the order among slides
    ;; added at that same place.
    (define d (sync-action-detail a))
    (+ (* 1000 (first d)) (second d)))
  (define sorted (sort actions < #:key position))
  (define planned
    (for/list ([a (in-list sorted)] [i (in-naturals)])
      (define s (for/first ([s (in-list (deck-slides d))]
                            #:when (= (sync-action-slide a) (slide-index s)))
                  s))
      (list a s (format "slide_~a" (+ next i)))))
  (define missing (filter (lambda (p) (not (second p))) planned))
  (define usable (filter second planned))
  (define def-source (and (pair? slide-sites) (slide-site-source (last slide-sites))))
  (define export-at
    (and def-source (hash-ref (program-layout-exports layout) def-source #f)))
  (cond
    [(null? slide-sites)
     (for/list ([p (in-list planned)])
       (cons (first p) "there is no `def slide_N = slide_canvas(...)` to add one beside"))]
    [(not (program-layout-slide-list layout))
     (for/list ([p (in-list planned)])
       (cons (first p) "`all_slides` has no traceable manifest, so a slide cannot be added to it"))]
    [(and def-source
          (not (same-file-path? def-source (program-layout-source layout)))
          (not export-at))
     (for/list ([p (in-list planned)])
       (cons (first p)
             (format "~a has no `export:` block, so a new slide defined there would not be visible to `all_slides`"
                     (file-name-from-path def-source))))]
    [else
     ;; The images a pasted slide brings with it.
     (for* ([p (in-list usable)]
            [src (in-list (append* (map element-media
                                        (append (slide-inherited (second p))
                                                (slide-elements (second p))))))])
       (copy-media-file! d src program-path media-names media-subdir))
     (define defs
       (string-join
        (for/list ([p (in-list usable)])
          (rhombus-slide-source (second p) (third p)
                                #:media-names media-names
                                #:font (or (program-default-font text)
                                           (and d (dominant-font d)))
                                #:identity-tags? #t))
        "\n\n"))
     (define at (slide-site-def-end (last slide-sites)))
     (list (list (make-range at at (slide-site-source (last slide-sites)))
                 (string-append "\n\n" defs))
           ;; Each name goes where its slide sits in the deck's order.
           (name-list-edits (program-layout-slide-list layout) usable text)
           (export-edits export-at usable def-source)
           missing)]))

;; One edit per position in `all_slides`, since several slides can be added at
;; the same place and two insertions at one offset would fight.
(define (name-list-edits nl planned text)
  (define items (name-list-items nl))
  (define groups
    (let loop ([ps planned] [acc '()])
      (cond
        [(null? ps) (reverse acc)]
        [else
         (define after (first (sync-action-detail (first (car ps)))))
         (define-values (same rest)
           (splitf-at ps (lambda (p) (= after (first (sync-action-detail (first p)))))))
         (loop rest (cons (cons after (map third same)) acc))])))
  (for/list ([g (in-list groups)])
    (define after (car g))
    (define names (cdr g))
    (cond
      ;; After nothing: at the head of the list.
      [(or (zero? after) (null? items))
       (list (make-range (add1 (name-list-open nl)) (add1 (name-list-open nl))
                         (name-list-source nl))
             (string-append (string-join names ", ")
                            (if (null? items) "" ", ")))]
      [else
       (define i (min (sub1 after) (sub1 (length items))))
       (define r (name-entry-range (list-ref items i)))
       (list (make-range (rng-end r) (rng-end r) (rng-source r))
             (string-append ", " (string-join names ", ")))])))

;; A name nobody exports still works -- `all_slides` is what an export reads --
;; but the generated file lists them all, so a new one is listed too.
(define (export-edits ex planned source)
  (cond
    [(not ex) '()]
    [else
     (define at (car ex))
     (define ind (cdr ex))
     (list (list (make-range at at source)
                 (apply string-append
                        (for/list ([p (in-list planned)])
                          (format "\n~a~a" (make-string ind #\space) (third p))))))]))

(define (copy-media-file! d src program-path media-names media-subdir)
  (define from (build-path (deck-media-dir d) src))
  (define to (build-path (or (path-only (path->complete-path program-path))
                             (current-directory))
                         media-subdir (hash-ref media-names src src)))
  (when (file-exists? from)
    (make-directory* (path-only to))
    (copy-file from to #t)))

(define (apply-actions! program-path actions #:deck [d #f] #:atomic? [atomic? #f])
  (define-values (all-sites scopes* slide-sites layout) (find-program-sites program-path))
  ;; One entry of `all_slides` may be several slides of the deck: see
  ;; `scopes-by-deck-slide`.
  (define scopes (scopes-by-deck-slide scopes*))
  ;; The source text, for the sites that describe a value rather than carry it:
  ;; whether a `~rotate:` says zero, whether a `~flip_h:` says true.
  (define source-texts
    (for/hash ([source (in-list (program-layout-files layout))]
               #:when (file-exists? source))
      (values source (file->string source))))
  (define (source-text-for source)
    (hash-ref source-texts source
              (lambda () (file->string source))))
  (define (site-source site)
    (and site (at-site-whole site) (rng-source (at-site-whole site))))
  (define source-text (source-text-for (program-layout-source layout)))
  ;; Set when an edit was carried to frames of a build other than the one it
  ;; names. The deck still holds the old value on those frames, and only the
  ;; caller can put that right, by writing the deck again from the program.
  ;;
  ;; A deck holding one slide per stage is that situation by construction: one
  ;; `at` form draws the shape on every stage of its slide, so an edit made on
  ;; the third stage is written once and the other stages still show what they
  ;; showed. Without the deck being written again they would report the same
  ;; difference back on the next save, over and over.
  ;; Whether this slide of the deck is one stage of a slide that has more than
  ;; one.
  (define (staged-slide? i)
    (define origins (unbox slide-origins))
    (and (<= 1 i (length origins))
         (let ([j (list-ref origins (sub1 i))])
           (> (length (filter (lambda (k) (= k j)) origins)) 1))))
  ;; Which stage of its slide this slide of the deck is, counting from one.
  (define (stage-of i)
    (define origins (unbox slide-origins))
    (and (<= 1 i (length origins))
         (let ([j (list-ref origins (sub1 i))])
           (add1 (length (for/list ([k (in-list (take origins (sub1 i)))]
                                    #:when (= k j))
                           k))))))
  ;; What the slide's `at` forms are written in, whether or not the slide has a
  ;; canvas call of its own.
  (define (scope-for-slide i)
    (and scopes (<= 1 i (length scopes)) (list-ref scopes (sub1 i))))
  (define slide-list (program-layout-slide-list layout))
  (define checked-slide-list? (and slide-list (name-list-checked? slide-list)))
  ;; Which declared entry owns an expanded editor page. The range and the
  ;; source owner travel together here because a checked declaration may have
  ;; to wrap the entry in `show_as` while preserving its manifest owner.
  (define (entry-item-for i)
    (define origins (unbox slide-origins))
    (define items (and slide-list (name-list-items slide-list)))
    (and items (<= 1 i (length origins))
         (let ([j (list-ref origins (sub1 i))])
           (and (<= 1 j (length items)) (list-ref items (sub1 j))))))
  ;; Where this slide's entry in `all_slides` is, which is what an edit written
  ;; around a slide is written around.
  (define (entry-range-for i)
    (define origins (unbox slide-origins))
    (define entries (and layout (program-layout-entries layout)))
    (and entries (<= 1 i (length origins))
         (let ([j (list-ref origins (sub1 i))])
           (and (<= 1 j (length entries)) (list-ref entries (sub1 j))))))
  ;; Whether the program can see the runtime's own names without a prefix. What
  ;; is written has to be a name the program has: a generated program opens the
  ;; runtime, and so does a talk grown from one.
  (define opens-runtime?
    (regexp-match? #px"lib\\(\"glide-pptx/(runtime|show)\\.rhm\"\\)\\s+open"
                   (file->string program-path)))
  (define spread? (box (let ([origins (unbox slide-origins)])
                         (and (pair? origins)
                              (not (= (length origins)
                                      (length (remove-duplicates origins))))))))
  ;; The actions the source has no place for at all: an element it does not
  ;; draw with an `at` form, a property it does not hold as a literal. They are
  ;; reported like any other refusal, but they do not refuse the save.
  ;;
  ;; A save lands whole or not at all so that an edit which cannot be written
  ;; is not thrown away by the rewrite that follows one that could. That holds
  ;; only while the refusal is something a person can clear. These cannot be
  ;; cleared: the report says "fix what it names", and there is nothing to fix
  ;; -- the shape is drawn by a helper and has no `at` form to give a tag to.
  ;; Refusing on them meant a real drag made in the same session was refused
  ;; too, on every pass, for as long as the slide stayed as it was.
  (define unwritable (make-hasheq))
  (define (mark-unwritable! a) (hash-set! unwritable a #t))
  (define ambiguous (ambiguous-scopes all-sites scopes program-path))
  ;; Said once per slide, whether or not anything was edited on it: a program
  ;; whose tags do not tell its elements apart cannot be edited there, and the
  ;; time to learn that is before trying.
  (define ambiguity-notes
    (for/list ([(scope why) (in-hash ambiguous)])
      (cons (sync-action 'noted (format "~a" scope) 0 '() #f)
            (string-append (car why) ". " TAG-HINT))))
  ;; Keyed by the slide's definition, so "Title 1" on slide 3 finds slide 3's
  ;; `at`. When the slide list is computed there is no definition to key on, and
  ;; `check-site-tags` has already established that tags are unique file-wide.
  (define by-scope (for/hash ([s (in-list all-sites)])
                     (values (cons (at-site-scope s) (tag-key (at-site-tag s))) s)))
  (define shared-tags
    (for/hash ([d (in-list (duplicate-tags (map at-site-tag all-sites)))])
      (values (car d) (cdr d))))
  (define by-tag
    (for/hash ([s (in-list all-sites)]
               #:unless (hash-ref shared-tags (tag-key (at-site-tag s)) #f))
      (values (tag-key (at-site-tag s)) s)))
  ;; Why an edit found no form to write to. Two `at` forms under one tag is not
  ;; the same as none, and a talk that draws its badges with a shared helper hits
  ;; the first: saying "no tagged `at` form" of a name written twice sent a person
  ;; looking for the wrong thing.
  (define (no-site-reason tag)
    (define n (hash-ref shared-tags (tag-key tag) #f))
    (if n
        (format "~a `at` forms in the program are tagged ~s, so an edit to it cannot be traced back to one of them"
                n tag)
        NO-AT-FORM))
  ;; How many elements in the whole deck answer to a tag. A tag that names one
  ;; thing can be followed wherever its `at` is written; one that names several
  ;; cannot, because they would all move together.
  (define tag-count
    (if d
        (for*/fold ([h (hash)]) ([s (in-list (deck-slides d))]
                                 [e (in-list (slide-elements s))])
          (hash-update h (tag-key (element-name e)) add1 0))
        (hash)))
  (define (site-for a) (site-for-tag a (sync-action-tag a)))
  ;; The same, for a tag other than the action's own: a grouping names the
  ;; group, and what it moves are the elements inside it.
  (define (site-for-tag a tag)
    (define scope (and scopes
                       (<= 1 (sync-action-slide a) (length scopes))
                       (list-ref scopes (sub1 (sync-action-slide a)))))
    (cond
      ;; No definition to key on: tags are unique file-wide, which
      ;; `check-site-tags` has already insisted on.
      [(not scope) (hash-ref by-tag (tag-key tag) #f)]
      [(hash-ref by-scope (cons scope (tag-key tag)) #f)]
      ;; Not in the slide's own definition, so it is written somewhere the slide
      ;; calls: a talk that draws a badge with `with_icon(...)` puts the `at`
      ;; inside the helper, and the slide never mentions the tag at all.
      ;;
      ;; One `at` drawing several elements is not the trouble -- that is what a
      ;; helper is, and writing that one form moves everything it draws, which
      ;; is what sharing code means. The trouble is two `at` forms under one
      ;; tag, and `by-tag` leaves those out.
      ;;
      ;; A deletion is the exception. Removing one badge should not delete the
      ;; code that draws every badge, so it is only followed when nothing else
      ;; in the deck answers to the tag -- which is to say that form drew this
      ;; and nothing more.
      [(eq? 'removed (sync-action-kind a))
       (and (zero? (hash-ref tag-count (tag-key tag) 0))
            (hash-ref by-tag (tag-key tag) #f))]
      [else (hash-ref by-tag (tag-key tag) #f)]))
  ;; Where a new element goes, for the slide an action names.
  (define (slide-site-for a)
    (define scope (and scopes
                       (<= 1 (sync-action-slide a) (length scopes))
                       (list-ref scopes (sub1 (sync-action-slide a)))))
    (and scope (for/first ([ss (in-list slide-sites)]
                           #:when (equal? scope (slide-site-scope ss)))
                 ss)))
  ;; An added image needs its file beside the program, under the same names a
  ;; fresh emit would have used.
  (define media-names (if d (media-names-for d) (hash)))
  (define media-subdir "media")
  (define media?
    (for/or ([text (in-hash-values source-texts)])
      (regexp-match? #rx"media[-_]lookup" text)))
  (define edits '())
  (define applied '())
  (define skipped '())
  (define notes '())
  ;; Ungrouping is several actions at once -- the group gone, and each shape it
  ;; held arriving on its own -- so it is worked out before anything is
  ;; written. Lifting the `at` forms out of the `group_pict` keeps what the
  ;; code says about them; deleting the group and writing the shapes from the
  ;; deck would replace a shared colour with a literal and drop every comment.
  (define lifts (ungroupings actions all-sites site-for-tag))
  (define consumed
    (for*/hasheq ([l (in-list lifts)] [a (in-list (cons (first l) (second l)))])
      (values a #t)))
  ;; An edit with no range writes nothing, so it must not be counted as applied:
  ;; a merge that reports success and changes nothing is the one failure the user
  ;; cannot see. Every caller checks the result.
  ;; Every place an edit has to land: the `at` form the action names, and the
  ;; same shape on the other frames of the same build.
  ;;
  ;; A build was one slide the presenter advanced through, split into a slide
  ;; per click, so a shape that appears on frame three appears on every frame
  ;; after it -- as a separate `at` form, because a still slide is all a program
  ;; can hold. Moving it on one frame and not the others is not something anyone
  ;; means: the build would jump as it played.
  ;; Where each layer begins and ends: a `slide_canvas(...)` or a
  ;; `group_pict(...)`. A slide that reveals in stages holds a canvas for the
  ;; base and usually a group per stage, and which layer an `at` form sits in
  ;; is what says whether two forms under one tag are one shape drawn again or
  ;; two shapes.
  (define canvas-extents
    (filter values
            (for/list ([m (in-list (regexp-match-positions*
                                    #rx"slide_canvas|group_pict" source-text))])
              (define open (next-open source-text (cdr m)))
              (define shut (and open (rhombus-close source-text open)))
              (and shut (cons (car m) (add1 shut))))))

  (define (layer-holding st)
    (define pos (rng-start (at-site-whole st)))
    (for/fold ([best #f]) ([r (in-list canvas-extents)])
      (if (and (<= (car r) pos) (< pos (cdr r))
               (or (not best) (> (car r) (car best))))
          r
          best)))

  ;; The same tag, on the same slide, on an `at` form in another canvas: a stage
  ;; that redraws what the base already put there names it again, and they are
  ;; the same shape -- moving one and not the other is not something anyone
  ;; means, and it is the rule a build already follows.
  ;;
  ;; Two forms under one tag in the *same* canvas are two shapes, and an edit
  ;; naming that tag could be either. Those are refused, as they were.
  (define (twins-of a primary)
    (define scope (scope-of a))
    (define tag (sync-action-tag a))
    (define home (and primary (layer-holding primary)))
    (if (not scope)
        '()
        (for/list ([st (in-list all-sites)]
                   #:when (and (equal? scope (at-site-scope st))
                               (equal? tag (at-site-tag st))
                               (not (eq? st primary))
                               (not (equal? home (layer-holding st)))))
          st)))

  (define (frames-for a primary)
    (define home (slide-site-for a))
    (define build (and home (slide-site-build home)))
    (define tag (sync-action-tag a))
    (define others
          (if (not build)
              '()
              (for*/list ([ss (in-list slide-sites)]
                          #:when (and (not (equal? (slide-site-scope ss)
                                                   (slide-site-scope home)))
                                      (equal? build (slide-site-build ss)))
                          [st (in-list all-sites)]
                          #:when (and (equal? (slide-site-scope ss) (at-site-scope st))
                                      (same-build-tag? tag (at-site-tag st))))
                st)))
    (define kin (append (twins-of a primary) others))
    (when (pair? others) (set-box! spread? #t))
    ;; And one form drawing an element on several slides is the same situation
    ;; as one drawing several frames of a build: the others in the deck still
    ;; hold the old value, and only a deck written again from the program puts
    ;; them in step. Without this the next pass reads them as fresh edits and
    ;; writes the shared form again with another slide's delta, which never
    ;; settles -- it reported the same drag on slide 3, then slide 5, then slide
    ;; 3 again, for as long as anyone let it.
    (when (> (hash-ref tag-count tag 0) 1) (set-box! spread? #t))
    (cons primary kin))

  ;; The frames of this build that come after the one an action names. A shape
  ;; added to a build appears from the click it was added on and stays for the
  ;; rest of them, so a new one goes on this frame and every later frame.
  (define (later-frames a)
    (define home (slide-site-for a))
    (define build (and home (slide-site-build home)))
    (cond
      [(not build) '()]
      [else
       (define after? (box #f))
       (define later
         (for/list ([ss (in-list slide-sites)]
                    #:when (cond
                             [(equal? (slide-site-scope ss) (slide-site-scope home))
                              (set-box! after? #t)
                              #f]
                             [else (and (unbox after?) (equal? build (slide-site-build ss)))]))
           ss))
       (when (pair? later) (set-box! spread? #t))
       later]))

  (define (edit! r text)
    (and r (begin (set! edits (cons (list r text) edits)) #t)))
  ;; Names this save has already given out, per slide. The sites were read
  ;; before any of it was written, so without this two shapes copied in one
  ;; save are both told the name is free and both take it -- and two `at` forms
  ;; under one tag is a program no later sync can read.
  (define claimed (make-hash))
  ;; Added slides are done together: several can land at one position, and two
  ;; insertions at one offset would fight.
  (define added-slides
    (filter (lambda (a) (eq? 'added-slide (sync-action-kind a))) actions))
  (unless (null? added-slides)
    (define r (apply-added-slides! program-path added-slides slide-sites layout d
                                   media-names media-subdir))
    (cond
      ;; A list of (action . reason) means none of them could be written.
      [(andmap pair? r)
       (set! skipped (append (reverse r) skipped))]
      [else
       (define-values (edit-lists refused) (values (take r 3) (fourth r)))
       (for ([e (in-list (append* (map (lambda (x) (if (and (pair? x) (rng? (car x)))
                                                       (list x) x))
                                       edit-lists)))])
         (set! edits (cons e edits)))
       (for ([p (in-list refused)])
         (set! skipped (cons (cons (first p) "the deck has no such slide") skipped)))
       (for ([a (in-list added-slides)]
             #:unless (memq a (map first refused)))
         (set! applied (cons a applied)))]))
  ;; The lifts, before the loop: each replaces one `at` form with the forms it
  ;; held, and the actions it answers are done with.
  (for ([l (in-list lifts)])
    (set! source-text (source-text-for (site-source (site-for (first l)))))
    (current-source-text source-text)
    (current-source-path (site-source (site-for (first l))))
    (define gone (first l))
    (define inner (third l))
    (define g (sync-action-detail gone))
    (define ind (indent-at source-text (rng-start (at-site-whole (site-for gone)))))
    (define pieces
      (for/list ([st (in-list inner)])
        (define whole (at-site-whole st))
        (reindent
         (splice-string
          (substring source-text (rng-start whole) (rng-end whole))
          (list (cons (shift-range (at-site-x st) (rng-start whole))
                      (num->source (+ (first g) (at-site-number st at-site-x source-text))))
                (cons (shift-range (at-site-y st) (rng-start whole))
                      (num->source (+ (second g) (at-site-number st at-site-y source-text))))))
         (- ind (indent-at source-text (rng-start whole))))))
    (edit! (at-site-whole (site-for gone))
           (string-join pieces (string-append ",\n" (spaces ind))))
    (for ([a (in-list (cons gone (second l)))])
      (set! applied (cons a applied))))
  ;; The slide an action is about, when it is about one.
  (define (scope-of a)
    (and scopes (<= 1 (sync-action-slide a) (length scopes))
         (list-ref scopes (sub1 (sync-action-slide a)))))
  (for ([a (in-list actions)] #:unless (or (eq? 'added-slide (sync-action-kind a))
                                           (hash-ref consumed a #f)))
    (define site (site-for a))
    (define ss-for-action (slide-site-for a))
    (define action-source
      (or (site-source site)
          (and ss-for-action (slide-site-source ss-for-action))
          (program-layout-source layout)))
    (set! source-text (source-text-for action-source))
    (current-source-text source-text)
    (current-source-path action-source)
    ;; An action either lands or it does not. One that writes part of itself and
    ;; then finds it cannot write the rest is reported as refused, and what it
    ;; had written is dropped: a half-applied edit is how a program stops
    ;; compiling.
    (define before-edits edits)
    (define ambiguity (let ([sc (scope-of a)]) (and sc (hash-ref ambiguous sc #f))))
    (cond
      ;; This slide's own tags do not tell its elements apart, so an edit to one
      ;; of them cannot be placed. Only this slide's edits: the rest of the save
      ;; is not its business, and neither is a reorder, which names no element.
      ;; A drag or a resize is written to every `at` under that tag elsewhere on
      ;; the slide, so a tag repeated across a slide's canvases is no longer in
      ;; the way of those -- but one repeated *within* a canvas still is, since
      ;; then the deck has two elements and the edit names one of them. The
      ;; other kinds are refused either way: rewriting one shape's text and not
      ;; its twin's would leave the two saying different things.
      [(and ambiguity
            ;; This action's own tag is one of the doubled-up ones. Anything
            ;; else on the slide is named exactly and is not this slide's
            ;; problem to answer for.
            (member (sync-action-tag a) (cdr ambiguity))
            (or (not (memq (sync-action-kind a) '(noted reordered moved resized)))
                (let ([primary (site-for a)])
                  (and primary
                       (for/or ([st (in-list all-sites)])
                         (and (not (eq? st primary))
                              (equal? (at-site-scope st) (at-site-scope primary))
                              (equal? (at-site-tag st) (at-site-tag primary))
                              (equal? (layer-holding st) (layer-holding primary))))))))
       (set! skipped (cons (cons a (string-append (car ambiguity) ". " TAG-HINT)) skipped))]
      [else
    (case (sync-action-kind a)
      [(moved resized)
       (define g (sync-action-detail a))
       (define-values (x y w h rot fh fv) (apply values g))
       (cond
         [(not site) (begin (mark-unwritable! a)
                 (set! skipped (cons (cons a (no-site-reason (sync-action-tag a))) skipped)))]
         ;; A computed position has no number to rewrite, so the drag is
         ;; recorded as a correction on `at` instead. Because it is one
         ;; argument rather than a wrapper, a second drag updates these two
         ;; numbers -- corrections cannot stack up the way nested pads do.
         [(not (and (at-site-x site) (at-site-y site)))
          (define prior (sync-action-prior a))
          (cond
            [(not prior)
             (set! skipped (cons (cons a "its position is computed and the program has no such element")
                                 skipped))]
            [(not (at-site-insert-at site))
             (set! skipped (cons (cons a "its position is computed and its tag is not a literal")
                                 skipped))]
            [else
             (define existing (at-site-nudge site))
             (define dx (+ (if existing (second existing) 0.0) (- x (first prior))))
             (define dy (+ (if existing (third existing) 0.0) (- y (second prior))))
             (define resize? (eq? 'resized (sync-action-kind a)))
             ;; The size may still be a literal even when the position is not.
             (define size-written?
               (and resize? (at-site-width site) (at-site-height site)
                    (begin (edit! (at-site-width site) (num->source w))
                           (edit! (at-site-height site) (num->source h))
                           #t)))
             ;; A correction of nothing is not written: dragging a corner
             ;; often leaves the other one where it was, and `~nudge: [0.0,
             ;; 0.0]` would be an edit reported and not made.
             (define shifted? (or (and existing #t)
                                  (not (and (zero? dx) (zero? dy)))))
             (define wrote?
               (cond
                 [(not shifted?) #f]
                 [existing (edit! (first existing) (nudge->source dx dy))]
                 [else (edit! (make-range (at-site-insert-at site) (at-site-insert-at site)
                                          (site-source site))
                              (nudge-argument->source dx dy))]))
             ;; A size the call works out for itself has nowhere to be
             ;; written, and that is said rather than swallowed. If the same
             ;; edit moved the shape too, the move is still written.
             (when (and resize? (not size-written?))
               (set! notes (cons (cons a "its size is computed, not a literal") notes)))
             (cond
               [(or wrote? size-written?) (set! applied (cons a applied))]
               ;; Noted just above, so not also skipped: one line about it is
               ;; enough, and it says which part could not be written.
               [(and resize? (not size-written?)) (void)]
               [else (set! skipped (cons (cons a "its existing correction has no source extent")
                                         skipped))])])]
         [else
          ;; By how much it moved, not where it ended up.
          ;;
          ;; A literal `at` and the box the deck holds are not always the same
          ;; number. A group is written with the box that contains what it
          ;; holds, which is above where its `at` put it when a child hangs out
          ;; the top -- so writing the deck's own number into the literal moved
          ;; the group by the overflow. A delta also leaves the frames of a
          ;; build whatever differences they had, where one absolute written to
          ;; every frame flattened them onto each other.
          ;;
          ;; The program's own geometry is the reference, so this says exactly
          ;; what the drag did. Without one there is nothing to take a delta
          ;; from and the deck's number is the best there is.
          (define prior (sync-action-prior a))
          ;; On every frame of the build, not only the one that was dragged.
          (for ([st (in-list (frames-for a site))])
            (cond
              [prior
               (edit! (at-site-x st)
                      (num->source (+ (- x (first prior))
                                      (at-site-number st at-site-x source-text))))
               (edit! (at-site-y st)
                      (num->source (+ (- y (second prior))
                                      (at-site-number st at-site-y source-text))))]
              [else
               (edit! (at-site-x st) (num->source x))
               (edit! (at-site-y st) (num->source y))]))
          ;; A rotation and a mirror are edits like any other, and both used to
          ;; be dropped in silence: a rotate was counted as applied while
          ;; nothing was written, and a flip -- what dragging a line's endpoint
          ;; past the other end does -- was not even noticed.
          (define turned? (turn-changed? site rot))
          (define mirrored? (mirror-changed? site fh fv))
          (cond
            [(eq? 'resized (sync-action-kind a))
             (cond
               [(and (at-site-width site) (at-site-height site))
                ;; A delta, for the same reason the position is one: a group's
                ;; written box is as big as what it holds, which is not the size
                ;; its `group_pict` declares.
                (for ([st (in-list (frames-for a site))])
                  (cond
                    [prior
                     (edit! (at-site-width st)
                            (num->source (+ (- w (third prior))
                                            (at-site-number st at-site-width source-text))))
                     (edit! (at-site-height st)
                            (num->source (+ (- h (fourth prior))
                                            (at-site-number st at-site-height source-text))))]
                    [else
                     (edit! (at-site-width st) (num->source w))
                     (edit! (at-site-height st) (num->source h))]))
                (set! applied (cons a applied))]
               [else
                (set! skipped (cons (cons a "its size is computed, not a literal") skipped))])]
            [(or turned? mirrored?)
             ;; Written when there is something to write to, and said plainly
             ;; when there is not.
             (define wrote-turn? (or (not turned?) (write-turn! site rot edit!)))
             (define wrote-mirror? (or (not mirrored?) (write-mirror! site fh fv edit!)))
             (if (and wrote-turn? wrote-mirror?)
                 (set! applied (cons a applied))
                 (set! skipped
                       (cons (cons a (cond
                                       [(not wrote-turn?) "its rotation cannot be written here"]
                                       [else "its mirroring cannot be written here"]))
                             skipped)))]
            [else (set! applied (cons a applied))])])]
      [(retext)
       (define paras (and site (at-site-texts site)))
       (define want (sync-action-detail a))
       (define hit (and paras (retyped-run paras want source-text)))
       (cond
         [(not site) (begin (mark-unwritable! a)
                 (set! skipped (cons (cons a (no-site-reason (sync-action-tag a))) skipped)))]
         ;; The words are the program's own: a helper works them out, or shares
         ;; one string between the several elements it draws with it. There is
         ;; no literal here to rewrite and nothing a person could go and fix, so
         ;; this is said and the rest of the save still lands -- a retyping of
         ;; an e-graph node's operator, which one `chain_nodes` decides for the
         ;; three `update`s, used to take every other edit in the save with it.
         [(or (not paras) (null? paras))
          (mark-unwritable! a)
          (set! skipped (cons (cons a NOT-LITERAL-TEXT) skipped))]
         ;; Which run a retyping that runs across two of them belongs to is a
         ;; guess, and this one does hold up the save: retyping the one word
         ;; instead is something a person can do, and the edits already in the
         ;; deck are worth keeping until they do.
         [(eq? 'crosses hit)
          (set! skipped
                (cons (cons a (string-append "the retyping crosses runs or paragraphs,"
                                             " so which of them it belongs to is a guess"))
                      skipped))]
         [(not hit)
          (mark-unwritable! a)
          (set! skipped (cons (cons a NOT-LITERAL-TEXT) skipped))]
         [else (edit! (car hit) (format "~s" (cdr hit)))
               (set! applied (cons a applied))])]
      ;; A shape added in the editor is written into the slide it was added to,
      ;; last, which is where the editor put it in the z-order.
      [(added)
       (define ss (slide-site-for a))
       (define e (added-element d (sync-action-slide a) (sync-action-tag a) (added-z a)))
       (define srcs (if e (element-media e) '()))
       (define stage (stage-of (sync-action-slide a)))
       (define entry (entry-range-for (sync-action-slide a)))
       ;; Laid over the slide rather than put inside its canvas. Two reasons to
       ;; do that, and they are the same reason twice: the canvas is not where
       ;; this can be said.
       ;;
       ;;   * The slide has no canvas call of its own. A helper built it --
       ;;     `divider(0)` composes over `section_canvas()`, `in_section(1, s)`
       ;;     wraps a slide in a header -- so the canvas is inside the helper
       ;;     and there is no `slide_canvas(...)` in this program to add a form
       ;;     to. Wrapping the slide *in* a canvas instead would be wrong twice
       ;;     over: `at` holds a still picture, so a slide that animates cannot
       ;;     go in one at all, and a slide that does not would become a single
       ;;     flattened element with everything it holds buried inside it.
       ;;
       ;;   * It belongs to a stage after the first, which a canvas cannot say.
       ;;
       ;; A canvas over the slide is what the talk's own helpers do -- `staged`
       ;; builds its reveals with `overlay` and `fade_in` -- and it leaves the
       ;; slide exactly as it was.
       (define over? (and entry opens-runtime?
                          (or (not ss) (and stage (> stage 1)))))
       (cond
         [(and (not ss) (not over?))
          ;; No canvas, and no name in this program to lay one over the slide
          ;; with either. A fact about the program and not a failure to be
          ;; retried, so it does not hold up the rest of the save: a box drawn
          ;; on each of sixteen slides landed on none of them, because eight of
          ;; those slides are built by helpers and one refusal took the whole
          ;; save with it.
          (mark-unwritable! a)
          (set! skipped (cons (cons a "no `slide-canvas` call to add it to") skipped))]
         [(not e)
          ;; A kind the translator has no source for -- a group, a chart. There
          ;; is nothing the program could be made to say, so it is reported and
          ;; the rest of the save still lands.
          (mark-unwritable! a)
          (set! skipped (cons (cons a "it is not a shape this can write as source") skipped))]
         [(and (pair? srcs) (not media?))
          ;; The image would need a `media` lookup the program does not have,
          ;; and adding one is a restructuring, not a literal edit.
          (set! skipped (cons (cons a "it is an image and the program has no media directory")
                              skipped))]
         [else
          (for ([src (in-list srcs)])
            (define from (build-path (deck-media-dir d) src))
            (define to (build-path (or (path-only (path->complete-path program-path))
                                       (current-directory))
                                   media-subdir (hash-ref media-names src src)))
            (when (file-exists? from)
              (make-directory* (path-only to))
              (copy-file from to #t)))
          ;; Duplicating a shape in the editor gives two of them one name, and
          ;; two `at` forms under one tag is a program a sync cannot read -- so
          ;; a name already spoken for in this slide gets a fresh one.
          (define home-scope (or (and ss (slide-site-scope ss))
                                 (scope-for-slide (sync-action-slide a))))
          (define taken
            (append (for/list ([st (in-list all-sites)]
                               ;; With no scope to go by -- a slide built by a
                               ;; helper -- every tag in the file is taken: the
                               ;; one that is written has to be found again, and
                               ;; a tag found file-wide is what finds it.
                               #:when (or (not home-scope)
                                          (and (equal? home-scope (at-site-scope st))
                                               (or (not ss)
                                                   (let ([whole (at-site-whole st)])
                                                     (and whole
                                                          (equal? (slide-site-source ss)
                                                                  (rng-source whole))))))))
                      (or (automatic-tag-name (at-site-tag st)) (at-site-tag st)))
                    (hash-ref claimed home-scope '())))
          ;; A copy can retain the original's source tag in its description.
          ;; Seed the new name from its readable suffix, not from the opaque
          ;; source identity, so the copy cannot acquire that identity twice.
          (define original-name
            (or (automatic-tag-name (element-name e)) (element-name e)))
          (define named
            (let loop ([n 2] [name original-name])
              (cond
                [(not (member name taken)) (element-with-name e name)]
                [(> n 99) (element-with-name e name)]
                [else (loop (add1 n) (format "~a (~a)" original-name n))])))
          (hash-update! claimed home-scope
                        (lambda (ns) (cons (element-name named) ns)) '())
          (define src-text
            (rhombus-element-source
             named (if ss (slide-site-indent ss) 2)
             #:media-names media-names
             #:font (or (program-default-font source-text)
                        (and d (dominant-font d)))
             #:identity-tags? #t))
          ;; Drawn on a stage after the first: written as a layer over that
          ;; slide, which appears from that stage on.
          ;;
          ;; The slide's canvas cannot say when something appears -- a canvas is
          ;; one still picture, and the staging is applied to it from outside --
          ;; so the `at` form goes inside a `from_stage` wrapped around the
          ;; slide's own entry in `all_slides`. It keeps its tag there, so the
          ;; next drag finds it like any other.
          ;; After the form it is drawn over, or before the first of them when
          ;; it is drawn under everything. A slide whose forms cannot be found
          ;; takes it last, which is where the editor usually put it anyway.
          (define under
            (let ([d (sync-action-detail a)])
              (and (list? d) (>= (length d) 2) (second d))))
          (define canvas-here
            (if ss
                (canvas-forms (slide-site-scope ss) all-sites (slide-site-source ss))
                '()))
          ;; A form this same save deletes is no anchor. Its comma goes with it,
          ;; and an insertion written against that comma left the canvas holding
          ;; two arguments with no separator between them -- a program that does
          ;; not parse, which the rollback then threw away along with every
          ;; other edit in the save. With none left to lean on, the insertion
          ;; goes where the canvas ends, which brings its own comma.
          (define doomed
            (for/list ([b (in-list actions)]
                       #:when (and (eq? 'removed (sync-action-kind b))
                                   (= (sync-action-slide b) (sync-action-slide a))))
              (sync-action-tag b)))
          (define surviving
            (filter (lambda (st)
                      (not (member (tag-key (at-site-tag st))
                                   (map tag-key doomed))))
                    canvas-here))
          ;; Drawn over something the canvas does not hold itself -- a shape
          ;; inside a group -- it goes at the end of the slide instead. The
          ;; program cannot say "above one member of a group" without joining
          ;; the group, which is not what the editor was asked for.
          (define after
            (let ([st (and under (site-for-tag a under))])
              (and st (memq st surviving) st)))
          (define first-form
            (and (pair? surviving)
                 (argmin (lambda (st) (rng-start (at-site-whole st))) surviving)))
          (cond
            [over?
             ;; `over` for a slide that has no canvas of its own, `from_stage`
             ;; for a stage after the first: the same layer, and the second one
             ;; waits.
             ;; The entry lives in the running-order module even when the
             ;; canvas is in an imported module. Its ranges must be sliced from
             ;; that file, not from the last action's source file.
             (define entry-text (source-text-for (rng-source entry)))
             (define inner-call
               (if (and stage (> stage 1)) (format "from_stage(~a, " stage) "over("))
             (define entry-item (entry-item-for (sync-action-slide a)))
             (define owner (and entry-item (name-entry-name entry-item)))
             (define source-owner
               (and (symbol? owner)
                    (not (regexp-match? #rx"^glide_generated_" (symbol->string owner)))
                    owner))
             (define call
               (string-append
                (cond
                  [(and checked-slide-list? source-owner)
                   (format "show_as(~a, " source-owner)]
                  [checked-slide-list? "show_only("]
                  [else ""])
                inner-call))
             (define close (if checked-slide-list? "))" ")"))
             ;; Written below the call where the entry has its line to itself,
             ;; and on the one line where it does not: an entry can share a line
             ;; with the next one -- `in_section(0, slide_2), in_section(0,
             ;; slide_12),` is how a talk writes its list -- and then there is
             ;; no indentation a continuation could take that is not the next
             ;; entry's, which is a program that does not parse.
             (define col (- (rng-start entry)
                            (line-start entry-text (rng-start entry))))
             (define alone?
               (and (regexp-match? #px"^[ \t]*$"
                                   (substring entry-text
                                              (line-start entry-text (rng-start entry))
                                              (rng-start entry)))
                    (regexp-match? #px"^[ \t]*[,\\]]?[ \t]*(\n|$)"
                                   (substring entry-text (rng-end entry)
                                              (min (string-length entry-text)
                                                   (+ (rng-end entry) 40))))))
             (define layer
               (if alone?
                   ;; Rhombus continuations align with the call's first
                   ;; argument, not with the preceding argument. This element
                   ;; is an argument of the innermost `over`/`from_stage`, past
                   ;; any outer `show_as`/`show_only` prefix.
                   (rhombus-element-source
                    named (+ col
                             (- (string-length call) (string-length inner-call))
                             (if (and stage (> stage 1))
                                 (string-length "from_stage(")
                                 (string-length "over(")))
                                           #:media-names media-names
                                           #:font (or (program-default-font entry-text)
                                                      (and d (dominant-font d)))
                                           #:identity-tags? #t)
                   (rhombus-element-source named 0
                                           #:media-names media-names
                                           #:font (or (program-default-font entry-text)
                                                      (and d (dominant-font d)))
                                           #:width +inf.0
                                           #:comment? #f
                                           #:identity-tags? #t)))
             (define shown-entry
               (if (and checked-slide-list? entry-item
                        (name-entry-shown-range entry-item))
                   (name-entry-shown-range entry-item)
                   entry))
             (edit! entry
                    (format (if alone? "~a~a,\n~a~a" "~a~a, ~a~a")
                            call
                            (substring entry-text (rng-start shown-entry) (rng-end shown-entry))
                            layer
                            close))]
            [(and after (at-site-whole after))
             (define at (rng-end (at-site-whole after)))
             (edit! (make-range at at (rng-source (at-site-whole after)))
                    (string-append ",\n" src-text))]
            [(and (not under) first-form)
             (define at (line-start source-text (rng-start (at-site-whole first-form))))
             (edit! (make-range at at (rng-source (at-site-whole first-form)))
                    (string-append src-text ",\n"))]
            [else
             (edit! (make-range (slide-site-insert-at ss) (slide-site-insert-at ss)
                                (slide-site-source ss))
                    (string-append ",\n" src-text))])
          ;; And on every frame after this one, since a build only ever adds.
          (unless over?
            (for ([ss2 (in-list (later-frames a))])
              (edit! (make-range (slide-site-insert-at ss2) (slide-site-insert-at ss2)
                                 (slide-site-source ss2))
                     (string-append ",\n" src-text))))
          ;; A slide with stages is built from one canvas, and this went into
          ;; that canvas -- so it is there from the first stage, not from the
          ;; stage it was drawn on. Which stage a shape appears on is the code's
          ;; to say, and there is no literal here to write it in.
          (when (and (staged-slide? (sync-action-slide a))
                     (not (and over? stage (> stage 1))))
            (set! notes (cons (cons a STAGE-WIDE) notes)))
          (set! applied (cons a applied))])]
      ;; Deleted in the editor: the `at` form goes, and nothing else.
      [(removed)
       (define whole (and site (at-site-whole site)))
       (cond
         [(not site) (begin (mark-unwritable! a)
                 (set! skipped (cons (cons a (no-site-reason (sync-action-tag a))) skipped)))]
         [(not whole)
          ;; A helper's call is a site to adjust, not a form to delete: the
          ;; program may name it and draw it elsewhere, so taking it out is a
          ;; change to what the program does rather than to a literal in it.
          (mark-unwritable! a)
          (set! skipped (cons (cons a "no form here can be deleted on its own") skipped))]
         [else (edit! (deletion-range (source-text-for (rng-source whole)) whole) "")
               ;; One form draws the shape on every stage of its slide, so
               ;; taking the form out takes it off all of them -- not only the
               ;; stage it was deleted on. Which stage a shape stops appearing
               ;; on is the code's to say.
               (when (staged-slide? (sync-action-slide a))
                 (set! notes (cons (cons a STAGES-ALL) notes)))
               (set! applied (cons a applied))])]
      ;; Appearance: written where the source states it as a literal, and
      ;; reported by name where it does not.
      [(restyle)
       (define sites (if site (at-site-styles site) '()))
       (define (site-for property)
         (findf (lambda (st) (equal? property (style-site-property st))) sites))
       (define detail (sync-action-detail a))
       ;; A fill or an outline the shape did not have, or one the editor took
       ;; away, is a whole argument: everything inside it is written or removed
       ;; at once, because none of those properties has anywhere of its own to
       ;; sit.
       (define (whole-argument head keyword)
         (define ch (assoc head detail))
         (define hit (and ch (site-for head)))
         (define (took? ok) (and ok (hash-ref STYLE-GROUPS head)))
         ;; Only a plain colour has a literal inside the argument that means the
         ;; whole of it. Anything else -- nothing at all, a gradient -- changes
         ;; the argument itself.
         (define (colour? v) (and (string? v) (not (equal? "gradient" v))))
         (and hit ch
              (not (and (colour? (second ch)) (colour? (third ch))))
              (cond
                [(eq? ABSENT (third ch))
                 (and (style-site-whole hit)
                      (took? (edit! (style-site-whole hit) "#false")))]
                [(argument->source head detail)
                 => (lambda (src)
                      (took?
                       (cond
                         [(and (style-site-whole hit) (not (style-site-range hit)))
                          (edit! (style-site-whole hit) src)]
                         [(and (style-site-insert-at hit)
                               (eq? keyword (style-site-keyword hit)))
                          (define at (style-site-insert-at hit))
                          (edit! (make-range at at action-source)
                                 (format "~a~~~a: ~a"
                                         (argument-comma (current-source-text) at)
                                         (keyword->string keyword) src))]
                         [else #f])))]
                [else #f])))
       (define wholes (append (or (whole-argument 'fill '#:fill) '())
                              (or (whole-argument 'line '#:line) '())))
       (define changes
         (filter (lambda (ch) (not (member (first ch) wholes))) detail))
       (define-values (done left)
         (for/fold ([done (filter (lambda (p) (memq p '(fill line))) wholes)]
                    [left '()])
                   ([ch (in-list changes)])
           (define property (first ch))
           (define want (third ch))
           (define hit (site-for property))
           (cond
             ;; Gone. Only a property with a whole argument of its own can be
             ;; written as absent -- a fill can, a typeface cannot.
             [(eq? ABSENT want)
              (cond
                [(and hit (style-site-whole hit) (edit! (style-site-whole hit) "#false"))
                 (values (cons property done) left)]
                [else (values done (cons (format "~a was removed, and the program has no way to say that"
                                                 (property-name property))
                                         left))])]
             ;; A picture swapped for another: the file comes to sit beside the
             ;; program and the source is pointed at it. Rewriting the name
             ;; alone would leave it naming a file that is not there.
             [(equal? 'image property)
              (define file (and d (picture-file-for d a)))
              (define hit (site-for 'image))
              (cond
                [(not media?)
                 (values done (cons "the picture was replaced and the program has no media directory"
                                    left))]
                [(or (not file) (not hit) (not (style-site-range hit)))
                 (values done (cons "the picture was replaced and the program does not name its file"
                                    left))]
                [else
                 (define name (copy-media-in! file program-path media-subdir))
                 (cond
                   [(and name (edit! (style-site-range hit) (format "~s" name)))
                    (values (cons 'image done) left)]
                   [else (values done (cons "the picture was replaced and its file could not be copied"
                                            left))])])]
             ;; "gradient" is all the comparison knows of one, and it is not
             ;; something to write anywhere: not over the colour that was
             ;; there, and certainly not into a shared definition.
             [(not (statable? property want))
              (values done (cons (format "the ~a was made a gradient, which the merge does not write back"
                                         (property-name property))
                                 left))]
             [(and hit (style-site-range hit)
                   (edit! (style-site-range hit) (style->source property want)))
              (values (cons property done) left)]
             ;; Not stated, so it is added -- which is the difference between
             ;; "make this line dashed" working and being reported.
             [(and hit (not (style-site-range hit)) (not (style-site-shared hit))
                   (style-site-insert-at hit) (style-site-keyword hit)
                   (let ([at (style-site-insert-at hit)])
                     (edit! (make-range at at action-source)
                            (format "~a~~~a: ~a"
                                    (argument-comma (current-source-text) at)
                                    (keyword->string (style-site-keyword hit))
                                    (added->source property want)))))
              (values (cons property done) left)]
             [(and hit (style-site-shared hit))
              ;; A named colour belongs to everything that uses it, so it is
              ;; rewritten only when everything that uses it changed the same
              ;; way -- the same rule as a tag that names several elements.
              (define name (style-site-shared hit))
              (define target (global-range layout name action-source))
              (define users (if target
                                (sites-using all-sites property name layout target)
                                '()))
              (define agreed?
                (and target
                     (for/and ([st (in-list users)])
                       (for/or ([b (in-list actions)])
                         (and (eq? 'restyle (sync-action-kind b))
                              (same-tag? (at-site-tag st) (sync-action-tag b))
                              (for/or ([c (in-list (sync-action-detail b))])
                                (and (equal? (first c) property)
                                     (equal? (third c) want))))))))
              (cond
                [(not target)
                 (values done
                         (cons (format "~a is ~a, which has more than one definition in the imported source files"
                                       (property-name property) name)
                               left))]
                [agreed?
                 (edit! target (style->source property want))
                 (values (cons (format "~a via ~a" (property-name property) name) done) left)]
                [else
                 (values done (cons (format "~a is ~a, shared with ~a other element~a that did not change with it"
                                            (property-name property) name (max 0 (sub1 (length users)))
                                            (if (= 2 (length users)) "" "s"))
                                    left))])]
             [else (values done (cons (format "~a is not a literal here" (property-name property))
                                      left))])))
       (cond
         [(null? left) (set! applied (cons a applied))]
         [else
          ;; Nothing written, and every property it names is one the source
          ;; does not hold as a literal: there is nowhere for this to go.
          (when (and (null? done)
                     (for/and ([why (in-list left)])
                       (regexp-match? #rx"is not a literal here$" why)))
            (mark-unwritable! a))
          (set! skipped
                (cons (cons a (string-append
                               (if (null? done) "" (format "~a written; " (reverse done)))
                               (string-join (reverse left) "; ")))
                      skipped))])]
      ;; The order the slides are in, which `all_slides` states.
      [(reordered)
       (define nl (program-layout-slide-list layout))
       (define items (and nl (name-list-items nl)))
       (define entries (program-layout-entries layout))
       (define order (sync-action-detail a))
       (cond
         [(or (not entries) (not items) (not (= (length items) (length order))))
          (set! skipped (cons (cons a "`all_slides` has no traceable entry for every slide")
                              skipped))]
         [(not (= (length entries) (length order)))
          ;; With one deck slide per stage the deck's order is not the list's
          ;; order at all, and there is nothing sensible to write from it.
          (set! skipped
                (cons (cons a (if (staged-deck?)
                                  (string-append "the deck holds one slide per stage,"
                                                 " so its order is not the order of"
                                                 " `all_slides`")
                                  "`all_slides` does not list every slide"))
                      skipped))]
         [else
          ;; Each entry is rewritten where it stands, with the one that now
          ;; belongs there, so an entry keeps whatever shape it has -- a name,
          ;; or a call like `in_section(0, slide_2)` -- and the list keeps its
          ;; layout. Comments in it stay where they are.
          (define texts (for/list ([r (in-list entries)])
                          (substring source-text (rng-start r) (rng-end r))))
          (define moved
            (for/list ([r (in-list entries)] [want (in-list order)] [have (in-naturals 1)]
                       #:unless (= want have))
              (edit! r (list-ref texts (sub1 want)))))
          (cond
            [(andmap values moved)
             (when (regexp-match? #rx"//" (substring source-text
                                                     (rng-start (first entries))
                                                     (rng-end (last entries))))
               (set! notes (cons (cons a COMMENTS-STAYED) notes)))
             (set! applied (cons a applied))]
            [else
             (set! skipped (cons (cons a "one of the entries has no extent to rewrite")
                                 skipped))])])]
      ;; A slide deleted in the editor: its definition goes, and its entry in
      ;; `all_slides` with it. Anything else in the program that names it is a
      ;; reason to stop -- the merge follows the program, it does not rewrite it.
      [(removed-slide)
       (define i (sync-action-slide a))
       (define scope (and scopes (<= 1 i (length scopes)) (list-ref scopes (sub1 i))))
       (define ss (slide-site-for a))
       (define nl (program-layout-slide-list layout))
       (define items (and nl (name-list-items nl)))
       (define entries (program-layout-entries layout))
       (define entry (and items scope
                          (findf (lambda (e) (eq? scope (name-entry-name e))) items)))
       (define def-text (and ss (source-text-for (slide-site-source ss))))
       (define def-r (and ss (definition-extent ss def-text)))
       ;; Which entry of `all_slides` showed this slide. A bare name is found by
       ;; name; an entry that is a call -- `divider(0)`, `in_section(3,
       ;; slide_39)` -- is found by where it sits in the list, which is where
       ;; the slide sits in the deck. Deleting the slide deletes the whole
       ;; entry, whichever shape it has.
       (define entry-r
         (or (and entry (entry-extent entry items))
             (and entries (entry-index-for i (length entries))
                  (list-entry-extent entries (entry-index-for i (length entries))))))
       (define export-r
         (and scope
              (for/or ([source (in-list (program-layout-files layout))])
                (export-entry-extent (source-text-for source) (symbol->string scope) source))))
       (define elsewhere
         (and scope
              (for/sum ([source (in-list (program-layout-files layout))])
                (mentions-outside
                 (source-text-for source) (symbol->string scope)
                 (filter (lambda (r) (and r (equal? source (rng-source r))))
                         (list def-r entry-r export-r))))))
       (cond
         ;; One deck slide per stage: the slide the editor deleted is one beat of
         ;; a slide the program shows, and deleting the program's slide would
         ;; take the other beats with it.
         [(and (staged-deck?) (not (lone-deck-slide? i)))
          (set! skipped
                (cons (cons a (string-append "this is one stage of a slide the program"
                                             " shows, so the slide itself is left alone"))
                      skipped))]
         [(not entry-r)
          (set! skipped (cons (cons a "the merge cannot see its entry in `all_slides`")
                              skipped))]
         [(and scope (positive? elsewhere))
          (set! skipped
                (cons (cons a (format (string-append "`~a` is named ~a more time~a in the program,"
                                                     " so deleting the slide is left to you")
                                      scope elsewhere (if (= 1 elsewhere) "" "s")))
                      skipped))]
         ;; The entry goes either way. Its definition goes with it when there is
         ;; one to remove and nothing else names it -- a slide built by a helper
         ;; the other slides use as well has none of its own, and saying that is
         ;; better than refusing to delete the slide at all.
         [(and (edit! entry-r "")
               (or (not def-r) (edit! def-r ""))
               (or (not def-r) (not export-r) (edit! export-r "")))
          (unless def-r
            (set! notes (cons (cons a DEF-STAYED) notes)))
          (set! applied (cons a applied))]
         [else
          (set! skipped (cons (cons a "its definition is not one the merge can remove") skipped))])]
      ;; Two shapes grouped in the editor: their `at` forms move inside a
      ;; `group_pict`, so everything the code says about them survives -- the
      ;; comment above one, a colour shared with something else, a size that is
      ;; computed. Writing the group from the deck instead would throw all of
      ;; that away and rewrite the shapes as literals.
      [(grouped)
       (define g (first (sync-action-detail a)))
       (define kids (second (sync-action-detail a)))
       (define sites (for/list ([k (in-list kids)]) (site-for-tag a (car k))))
       (define clash (site-for-tag a (sync-action-tag a)))
       (cond
         [clash
          (set! skipped (cons (cons a (format "the program already has an `at` tagged ~s"
                                              (sync-action-tag a)))
                              skipped))]
         [(not (andmap values sites))
          (set! skipped (cons (cons a "not all of the shapes it holds are tagged `at` forms here")
                              skipped))]
         ;; Two of them under one name are one `at` form here, and writing it
         ;; twice is a program no later sync can read. Duplicating a shape
         ;; inside a group is how an editor makes that: the copy is given the
         ;; name it was copied from.
         [(not (= (length (remove-duplicates sites eq?)) (length sites)))
          (set! skipped
                (cons (cons a (string-append "two of the shapes it holds have one name,"
                                             " so the program cannot tell them apart"
                                             " -- rename one of them in the editor"))
                      skipped))]
         [(not (= 1 (length (remove-duplicates
                             (map (lambda (st) (rng-source (at-site-whole st))) sites)))))
          (set! skipped
                (cons (cons a "the shapes are defined in different source files, so their forms cannot be grouped together")
                      skipped))]
         [(not (for/and ([st (in-list sites)])
                 (and (at-site-whole st) (at-site-x st) (at-site-y st))))
          (set! skipped (cons (cons a "one of the shapes it holds has a computed position")
                              skipped))]
         [else
          (define text source-text)
          (define pieces
            (for/list ([st (in-list sites)] [k (in-list kids)])
              (define whole (at-site-whole st))
              (define kid (cdr k))
              (splice-string
               (substring text (rng-start whole) (rng-end whole))
               (list (cons (shift-range (at-site-x st) (rng-start whole))
                           (num->source (- (first kid) (first g))))
                     (cons (shift-range (at-site-y st) (rng-start whole))
                           (num->source (- (second kid) (second g))))))))
          ;; The group goes where the first of them was, so it is drawn where
          ;; they were drawn.
          (define anchor (argmin (lambda (st) (rng-start (at-site-whole st))) sites))
          (define ind (indent-at text (rng-start (at-site-whole anchor))))
          ;; This group is a new editor object. Keep its current id through the
          ;; structural handoff; its children retain their existing source ids.
          (define head (format "at(~a, ~a, ~~tag: ~s,"
                               (num->source (first g)) (num->source (second g))
                               (sync-action-tag a)))
          (define open (format "group_pict(~~width: ~a, ~~height: ~a,"
                               (num->source (third g)) (num->source (fourth g))))
          (define kid-col (+ ind 3 (string-length "group_pict(")))
          (define body
            (for/list ([piece (in-list pieces)] [st (in-list sites)])
              (reindent piece (- kid-col (indent-at text (rng-start (at-site-whole st)))))))
          (edit! (at-site-whole anchor)
                 (string-append
                  head "\n" (spaces (+ ind 3)) open "\n" (spaces kid-col)
                  (string-join body (string-append ",\n" (spaces kid-col)))
                  "))"))
          (for ([st (in-list sites)] #:unless (eq? st anchor))
            (edit! (deletion-range text (at-site-whole st)) ""))
          (set! applied (cons a applied))])]
      ;; The deck's size, which every canvas states -- as a number each, or as
      ;; one name they share.
      [(resized-deck)
       (define want (sync-action-detail a))
       (define targets
         (for*/list ([ss (in-list slide-sites)]
                     [pair (in-list (list (cons (slide-site-width ss) (first want))
                                          (cons (slide-site-height ss) (second want))))]
                     #:when (car pair))
           (list (car pair) (cdr pair) (slide-site-source ss))))
       (define resolved
         (for/list ([t (in-list targets)])
           (define site (first t))
           (define value (second t))
           (define source (third t))
           (cond
             [(style-site-range site) (cons (style-site-range site) value)]
             [(and (style-site-shared site)
                   (global-range layout (style-site-shared site) source))
              => (lambda (r) (cons r value))]
             [else #f])))
       (cond
         [(null? targets)
          (set! skipped (cons (cons a "no `slide_canvas` states its size here") skipped))]
         [(not (andmap values resolved))
          (set! skipped
                (cons (cons a "a slide states its size as something other than a number or a name")
                      skipped))]
         [else
          ;; One `def` serves every slide, so the same range comes up once per
          ;; slide and is written once.
          (for ([e (in-list (remove-duplicates resolved))])
            (edit! (car e) (num->source (cdr e))))
          (set! applied (cons a applied))])]
      ;; Hidden, or shown again. The canvas says so where it says everything
      ;; else about the slide.
      [(hidden-slide)
       (define ss (slide-site-for a))
       (define hit (and ss (slide-site-hidden ss)))
       (define want (first (sync-action-detail a)))
       (cond
         [(not hit)
          (set! skipped (cons (cons a "no `slide_canvas` to say it on") skipped))]
         [(and (style-site-range hit)
               (edit! (style-site-range hit) (if want "#true" "#false")))
          (set! applied (cons a applied))]
         [(and (style-site-insert-at hit) want)
          (define at (style-site-insert-at hit))
          (edit! (make-range at at (slide-site-source ss))
                 (format "~a~~hidden: #true" (argument-comma (current-source-text) at)))
          (set! applied (cons a applied))]
         ;; Nothing there and nothing wanted is nothing to do.
         [(not want) (set! applied (cons a applied))]
         [else
          (set! skipped (cons (cons a "the canvas does not state it as a literal") skipped))])]
      ;; The slide's own paint, which the canvas states.
      [(repainted)
       (define ss (slide-site-for a))
       (define hit (and ss (slide-site-background ss)))
       (define want (third (first (sync-action-detail a))))
       (cond
         [(not hit)
          (set! skipped (cons (cons a "the canvas does not state a background") skipped))]
         [(not (statable? 'background want))
          (set! skipped (cons (cons a (string-append "the background was made a gradient,"
                                                     " which the merge does not write back"))
                              skipped))]
         [(and (style-site-range hit) (string? want)
               (edit! (style-site-range hit) (format "~s" want)))
          (set! applied (cons a applied))]
         [(and (style-site-whole hit) (background->source want)
               (edit! (style-site-whole hit) (background->source want)))
          (set! applied (cons a applied))]
         [else
          (set! skipped (cons (cons a "its background is not a literal here") skipped))])]
      ;; The drawing order, which is the order of the `at` forms.
      [(restacked)
       (define scope (let ([i (sync-action-slide a)])
                       (and scopes (<= 1 i (length scopes)) (list-ref scopes (sub1 i)))))
       (define ss (slide-site-for a))
       (define here (sort (canvas-forms scope all-sites (and ss (slide-site-source ss)))
                          < #:key (lambda (st) (rng-start (at-site-whole st)))))
       (define want (sync-action-detail a))
       (define pieces (reorder-pieces here want source-text))
       (cond
         [(not scope)
          (set! skipped (cons (cons a "the slide it is on is not one the program names") skipped))]
         [(not pieces)
          (set! skipped
                (cons (cons a (string-append "the `at` forms are not all here to reorder"
                                             " -- move them yourself to change the drawing order"))
                      skipped))]
         [(edit! (car pieces) (cdr pieces)) (set! applied (cons a applied))]
         [else (set! skipped (cons (cons a "its `at` forms cannot be moved") skipped))])]
      ;; A difference that is not an assertion: said, and nothing more.
      [(noted)
       (set! notes
             (cons (cons a
                         (if (string? (sync-action-detail a))
                             ;; Already in words: a shape the code draws, which
                             ;; there is no `at` form to write to.
                             (sync-action-detail a)
                             (string-join
                              (for/list ([ch (in-list (sync-action-detail a))])
                                (format "~a is ~s here and ~s in the deck, which is what a deck says when it says nothing"
                                        (property-name (first ch)) (second ch) (third ch)))
                              "; ")))
                   notes))]
      [(ambiguous)
       (set! skipped (cons (cons a (sync-action-detail a)) skipped))]
      [else (set! skipped (cons (cons a "reported only") skipped))])
      ])
    (unless (or (memq a applied) (eq? 'noted (sync-action-kind a)))
      (set! edits before-edits)))
  ;; What is left to refuse on: the refusals a person can clear.
  (define blocking
    (for/list ([sk (in-list skipped)] #:unless (hash-ref unwritable (car sk) #f)) sk))
  (cond
    ;; All of it, or none of it.
    [(and atomic? (pair? blocking))
     (values '() (reverse skipped) (append ambiguity-notes (reverse notes)) #f #t)]
    [else
     (when (pair? edits) (splice-files! program-path edits))
     (values (reverse applied) (reverse skipped)
             (append ambiguity-notes (reverse notes))
             (and (pair? applied) (unbox spread?))
             #f)]))

;; Said in one place because it is also read back: a skip under this reason is
;; one the source has no place for.
(define NO-AT-FORM "no tagged `at` form in the source")

(define STAGE-WIDE
  (string-append "the slide is built from one canvas, so this is on it from the first"
                 " stage rather than the one it was drawn on"))

(define NOT-LITERAL-TEXT
  (string-append "its words are not literals here -- the program works them out,"
                 " or shares them with everything else it draws with them"))

(define COMMENTS-STAYED
  (string-append "the slides were put in the deck's order, and the comments in"
                 " `all_slides` were left where they are"))

(define DEF-STAYED
  (string-append "its entry in `all_slides` is gone; what drew it is a helper the"
                 " program uses elsewhere, so nothing was deleted with it"))

(define STAGES-ALL
  (string-append "one `at` form draws it on every stage of the slide, so it is gone"
                 " from all of them and not only the stage it was deleted on"))

;; Whether the deck's rotation or mirroring differs from what the source says.
;; The source's own value is what it was exported with, so the base is not
;; needed: a `~rotate:` that is not there means zero, and a flip that is not
;; there means false.
(define (turn-changed? site rot)
  (define r (at-site-rot site))
  (define was (if r (string->number (substring (current-source-text)
                                               (rng-start r) (rng-end r)))
                  0.0))
  (> (abs (- (or was 0.0) rot)) 0.01))

(define (flag-value site get)
  (define r (get site))
  (and r (regexp-match? #rx"true" (substring (current-source-text)
                                             (rng-start r) (rng-end r)))))

(define (mirror-changed? site fh fv)
  (or (not (eq? (flag-value site at-site-flip-h) (and fh #t)))
      (not (eq? (flag-value site at-site-flip-v) (and fv #t)))))

(define (write-turn! site rot edit!)
  (cond
    [(at-site-rot site) (edit! (at-site-rot site) (num->source rot))]
    [(at-site-insert-at site)
     (edit! (make-range (at-site-insert-at site) (at-site-insert-at site)
                        (rng-source (at-site-whole site)))
            (format ", ~~rotate: ~a" (num->source rot)))]
    [else #f]))

(define (write-mirror! site fh fv edit!)
  (define (one range want name)
    (cond
      [range (edit! range (if want "#true" "#false"))]
      [(not want) #t]                       ; nothing there and none wanted
      [(at-site-leaf-at site)
       (edit! (make-range (at-site-leaf-at site) (at-site-leaf-at site)
                          (rng-source (at-site-whole site)))
              (format "~~~a: #true, " name))]
      [else #f]))
  (and (one (at-site-flip-h site) (and fh #t) "flip_h")
       (one (at-site-flip-v site) (and fv #t) "flip_v")))

;; Resolve a shared name from the module containing its use. A same-module
;; definition wins. Otherwise it must be exported by a module that this source
;; directly imports `open`. Mere uniqueness in the project is not binding
;; evidence: a private definition in a sibling module, or a same-named function
;; parameter, must never become the target of an editor write.
(define (global-range layout name source)
  (define globals (program-layout-globals layout))
  (or (hash-ref globals (cons source name) #f)
      (let ([ranges
             (for/list ([imported (in-list
                                   (hash-ref (program-layout-open-imports layout)
                                             source '()))]
                        #:when (memq name
                                     (hash-ref (program-layout-public-names layout)
                                               imported '()))
                        #:when (hash-ref globals (cons imported name) #f))
               (hash-ref globals (cons imported name)))])
        (and (= 1 (length ranges)) (first ranges)))))

;; Every `at` in the program whose `property` resolves to the same shared
;; definition as this one.
(define (sites-using sites property name layout target)
  (for/list ([st (in-list sites)]
             #:when (for/or ([sy (in-list (at-site-styles st))])
                      (and (equal? property (style-site-property sy))
                           (eq? name (style-site-shared sy))
                           (equal? target
                                   (global-range layout name
                                                 (rng-source (at-site-whole st)))))))
    st))

;; A style value as it reads in source: a colour is the string inside `hex`, a
;; size is a number, a typeface a string, boldness a boolean.
;; Properties that live inside one argument, so they are written and removed
;; together: a fill's opacity is part of its colour, and a line's width is part
;; of its stroke. The first of each is the one that says whether the argument is
;; there at all.
(define STYLE-GROUPS
  (hash 'fill '(fill fill-opacity)
        'line '(line line-width dash cap head tail)))

;; A whole argument, for a group the source does not state at all: an outline a
;; shape was given in the editor is a stroke call, and a fill is a colour.
(define (argument->source head changes)
  (define (val p)
    (let ([ch (assoc p changes)]) (and ch (not (eq? ABSENT (third ch))) (third ch))))
  (case head
    [(fill)
     (define c (val 'fill))
     (define o (val 'fill-opacity))
     ;; "gradient" is all the comparison knows of one: which stops and which
     ;; angle is not something either side can say, so it is reported instead.
     (cond
       [(not (string? c)) #f]
       [(equal? "gradient" c) #f]
       [(and o (< o 0.999)) (format "hex(~s, ~~alpha: ~a)" c (num->source o))]
       [else (format "hex(~s)" c)])]
    [else
     (format "make_stroke(hex(~s)~a~a~a~a~a)"
             (or (val 'line) "000000")
             (let ([w (val 'line-width)]) (if w (format ", ~~width: ~a" (num->source w)) ""))
             (let ([d (val 'dash)])
               (if (and d (not (equal? "solid" (format "~a" d))))
                   (format ", ~~dash: ~a" (style->source 'dash d))
                   ""))
             (let ([c (val 'cap)])
               (if (and c (not (eq? 'flat c)))
                   (format ", ~~cap: ~a" (style->source 'cap c))
                   ""))
             (end-argument "head" (val 'head))
             (end-argument "tail" (val 'tail)))]))

;; An arrowhead, where there is one to write.
(define (end-argument name e)
  (if (list? e) (format ", ~~~a: ~a" name (style->source 'head e)) ""))

;; An argument being added rather than rewritten: a colour is a call of its own,
;; where the literal inside an existing one is just the string.
(define (added->source property value)
  (case (property-head property)
    [(text-color) (format "hex(~s)" value)]
    [else (style->source property value)]))

;; Whether the source can be given this value at all. A fill the editor made a
;; gradient cannot: which stops and which angle is not something either side of
;; the comparison can say.
(define (statable? property value)
  (case (property-head property)
    [(fill line text-color background) (not (equal? "gradient" value))]
    [else #t]))

;; A background as one argument: a colour, a colour and its opacity, or none.
;; A definition, the comment heading it, and the newline it ends on. The heading
;; is taken with it because a comment left behind would name the definition
;; after it instead.
(define (definition-extent ss text)
  (define start (slide-site-def-start ss))
  (define end (slide-site-def-end ss))
  (and start end
       (make-range (comment-block-start text start)
            (let loop ([j end])
              (cond
                [(>= j (string-length text)) j]
                [(char=? #\newline (string-ref text j)) (add1 j)]
                [(char-whitespace? (string-ref text j)) (loop (add1 j))]
                [else end]))
            (slide-site-source ss))))

;; Back over the comment lines immediately above a position, to the start of the
;; first of them.
(define (comment-block-start text at)
  (let loop ([start (line-start text at)])
    (define prev (and (> start 0) (line-start text (sub1 start))))
    (cond
      [(not prev) start]
      [(regexp-match? #px"^[ \t]*//" (substring text prev start)) (loop prev)]
      [else start])))

(define (line-start text at)
  (let loop ([j (min at (string-length text))])
    (cond [(<= j 0) 0]
          [(char=? #\newline (string-ref text (sub1 j))) j]
          [else (loop (sub1 j))])))

(define (line-end-after text at)
  (let loop ([j (min at (string-length text))])
    (cond [(>= j (string-length text)) j]
          [(char=? #\newline (string-ref text j)) (add1 j)]
          [else (loop (add1 j))])))

;; The line naming a slide in the program's `export:` block, which a generated
;; program has one of per slide. Only inside that block: a bare name on a line
;; of its own means something else anywhere but there.
(define (export-entry-extent text name [source (current-source-path)])
  (define m (regexp-match-positions #px"(?m:^export:[ \t]*$)" text))
  (and m
       (let* ([from (cdar m)]
              [block-end (let loop ([j (line-end-after text (add1 from))])
                           (cond
                             [(>= j (string-length text)) j]
                             [(regexp-match? #px"^[ \t]" (substring text j (min (string-length text) (add1 j))))
                              (loop (line-end-after text j))]
                             [(char=? #\newline (string-ref text j)) (loop (add1 j))]
                             [else j]))]
              [hit (regexp-match-positions (pregexp (format "(?m:^[ \t]+~a[ \t]*\r?\n)"
                                                            (regexp-quote name)))
                                           text from block-end)])
         (and hit (make-range (caar hit) (cdar hit) source)))))

;; How many times a name is written outside the places being removed. A program
;; that names a slide anywhere else is one the merge would be rewriting.
(define (mentions-outside text name ranges)
  (for/sum ([m (in-list (regexp-match-positions*
                         (pregexp (format "(?<![A-Za-z0-9_])~a(?![A-Za-z0-9_])"
                                          (regexp-quote name)))
                         text))]
            #:unless (for/or ([r (in-list ranges)])
                       (and r (<= (rng-start r) (car m)) (< (car m) (rng-end r)))))
    1))

;; One name out of `[a, b, c]`, with the comma that separated it.
;; Whether this deck holds one slide per stage, and whether the deck slide `i`
;; is the only one that came from its slide of the program. A structural edit to
;; one beat of an animated slide is not a structural edit to the slide.
(define (staged-deck?)
  (let ([o (unbox slide-origins)])
    (and (pair? o) (not (= (length o) (length (remove-duplicates o)))))))

(define (lone-deck-slide? i)
  (let* ([o (unbox slide-origins)]
         [mine (and (<= 1 i (length o)) (list-ref o (sub1 i)))])
    (and mine (= 1 (for/sum ([x (in-list o)] #:when (equal? x mine)) 1)))))

;; Which entry of `all_slides` the deck's slide `i` came from, counting from 0.
(define (entry-index-for i n)
  (let* ([o (unbox slide-origins)]
         [mine (and (<= 1 i (length o)) (list-ref o (sub1 i)))]
         [k (sub1 (or mine i))])
    (and (<= 0 k) (< k n) k)))

;; The extent of one entry of a literal list, the separator that follows it
;; included -- so that removing it leaves a list rather than a stray comma.
(define (list-entry-extent ranges i)
  (define r (list-ref ranges i))
  (define n (length ranges))
  (cond
    [(< (add1 i) n)
     (make-range (rng-start r) (rng-start (list-ref ranges (add1 i))) (rng-source r))]
    [(> i 0)
     (make-range (rng-end (list-ref ranges (sub1 i))) (rng-end r) (rng-source r))]
    [else r]))

(define (entry-extent entry items)
  (define r (name-entry-range entry))
  (define i (index-of items entry))
  (define n (length items))
  (cond
    [(and i (< (add1 i) n))
     (make-range (rng-start r) (rng-start (name-entry-range (list-ref items (add1 i))))
                 (rng-source r))]
    [(and i (> i 0))
     (make-range (rng-end (name-entry-range (list-ref items (sub1 i)))) (rng-end r)
                 (rng-source r))]
    [else r]))

;; The `at` forms of one slide, written out in a new order. What separates them
;; stays where it is, so the file keeps its indentation and its commas; only the
;; forms themselves move. A comment between two of them is a reason not to: it
;; would end up describing something else.
(define (reorder-pieces sites want text)
  (define by-tag (for/hash ([st (in-list sites)])
                   (values (tag-key (at-site-tag st)) st)))
  (define ordered (for/list ([t (in-list want)]) (hash-ref by-tag (tag-key t) #f)))
  ;; A comment sitting above an `at` describes that `at`, so it is part of the
  ;; piece that moves. The emitter writes one over every element it adds.
  (define (extent st)
    (make-range (comment-block-start text (rng-start (at-site-whole st)))
                (rng-end (at-site-whole st))
                (rng-source (at-site-whole st))))
  (define gaps
    (for/list ([a (in-list sites)] [b (in-list (cdr (append sites (list #f))))]
               #:when b)
      (substring text (rng-end (extent a)) (rng-start (extent b)))))
  (and (= (length sites) (length want))
       (andmap values ordered)
       ;; A comment left in a gap belongs to neither side of it.
       (not (for/or ([g (in-list gaps)]) (regexp-match? #rx"//" g)))
       ;; The forms are copied rather than spelled out, so an edit to one of
       ;; them made in the same save moves with it.
       (let ([span (make-range (rng-start (extent (first sites)))
                               (rng-end (extent (last sites)))
                               (rng-source (extent (first sites))))])
         (cons span
               (append*
                (for/list ([st (in-list ordered)] [i (in-naturals)])
                  (cons (extent st)
                        (if (< i (length gaps)) (list (list-ref gaps i)) '()))))))))

;; Which run a retyping landed in, as (range . new-value). A body's text is its
;; runs joined, with a newline between paragraphs, so the run to rewrite is the
;; one the changed stretch falls inside. When it falls across two of them --
;; or across a paragraph break -- there is no answer, only a guess, and
;; 'crosses says so.
;;
;; This is what makes retyping a word of a styled line work: `run("hello ")`
;; followed by a bold `run("world")` has no single literal for the whole line,
;; and one of the two is where the edit belongs.
(define (retyped-run paras want text)
  (define runs
    (append*
     (for/list ([p (in-list paras)] [i (in-naturals)])
       (append (if (zero? i) '() (list (cons #f "\n")))
               (for/list ([r (in-list p)]) (cons r (literal-string r text)))))))
  (define (value-of c) (cdr c))
  (cond
    [(for/or ([c (in-list runs)]) (not (value-of c))) #f]
    [else
     (define was (apply string-append (map value-of runs)))
     ;; The stretch that changed: what is left after the shared ends.
     (define keep-front
       (let loop ([i 0])
         (if (and (< i (string-length was)) (< i (string-length want))
                  (char=? (string-ref was i) (string-ref want i)))
             (loop (add1 i))
             i)))
     (define keep-back
       (let loop ([j 0])
         (if (and (< j (- (string-length was) keep-front))
                  (< j (- (string-length want) keep-front))
                  (char=? (string-ref was (- (string-length was) 1 j))
                          (string-ref want (- (string-length want) 1 j))))
             (loop (add1 j))
             j)))
     (define from keep-front)
     (define to (- (string-length was) keep-back))
     ;; Which run holds [from, to), and where it starts.
     (define-values (found start)
       (for/fold ([found #f] [at 0]) ([c (in-list runs)])
         (define end (+ at (string-length (value-of c))))
         (values (if (and (not found) (<= at from) (<= to end) (car c)) (cons c at) found)
                 end)))
     (void start)
     (cond
       [(not found) 'crosses]
       [else
        (define c (car found))
        (define at (cdr found))
        (define old (value-of c))
        (cons (car c)
              (string-append (substring old 0 (- from at))
                             (substring want from (- (string-length want) keep-back))
                             (substring old (- to at))))])]))

;; A string literal's value, read out of the source it is written in.
(define (literal-string r text)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define v (read (open-input-string (substring text (rng-start r) (rng-end r)))))
    (and (string? v) v)))

;; A group the deck no longer has, whose `at` form holds exactly the `at` forms
;; the deck now has at top level: that is the editor's Ungroup, and the shapes
;; can be lifted out rather than deleted and written again.
;;
;; Returns (list removed-action added-actions inner-sites) for each.
(define (ungroupings actions all-sites site-of)
  (define (inner-of site)
    (for/list ([st (in-list all-sites)]
               #:when (and (not (eq? st site))
                           (at-site-whole st) (at-site-whole site)
                           (encloses? (at-site-whole site) (at-site-whole st))))
      st))
  (filter
   values
   (for/list ([a (in-list actions)] #:when (eq? 'removed (sync-action-kind a)))
     (define site (site-of a (sync-action-tag a)))
     (define inner (and site (inner-of site)))
     (define tags (and inner (map at-site-tag inner)))
     (define arrived
       (and (pair? (or tags '()))
            (for/list ([b (in-list actions)]
                       #:when (and (eq? 'added (sync-action-kind b))
                                   (= (sync-action-slide a) (sync-action-slide b))
                                   (for/or ([t (in-list tags)])
                                     (same-tag? (sync-action-tag b) t))))
              b)))
     (and (pair? (or arrived '()))
          ;; Every shape it held, and nothing else: a group half of which was
          ;; ungrouped is not something to guess at.
          (= (length arrived) (length tags))
          (for/and ([st (in-list inner)])
            (and (at-site-x st) (at-site-y st)))
          (list a arrived inner)))))

;; A number the source states, read back out of it.
(define (at-site-number st get text)
  (define r (get st))
  (or (and r (string->number (string-trim (substring text (rng-start r) (rng-end r))))) 0.0))

;; The file behind a deck's picture, for the element an action names.
(define (picture-file-for d a)
  (define e (added-element d (sync-action-slide a) (sync-action-tag a) (added-z a)))
  (define src (and (picture? e) (picture-src e)))
  (define p (and src (build-path (deck-media-dir d) src)))
  (and p (file-exists? p) p))

;; A file copied to sit beside the program, under a name that is not already
;; taken by different bytes. Returns the name the program should use.
(define (copy-media-in! from program-path subdir)
  (define dir (build-path (or (path-only (path->complete-path program-path))
                              (current-directory))
                          subdir))
  (define base (path->string (file-name-from-path from)))
  (define-values (stem ext)
    (let ([m (regexp-match #px"^(.*?)([.][^.]*)?$" base)])
      (values (second m) (or (third m) ""))))
  (define name
    (let loop ([n 1])
      (define try (if (= n 1) base (format "~a-~a~a" stem n ext)))
      (define to (build-path dir try))
      (cond
        [(not (file-exists? to)) try]
        [(equal? (file-size to) (file-size from)) try]
        [(> n 99) try]
        [else (loop (add1 n))])))
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (make-directory* dir)
    (copy-file from (build-path dir name) #t)
    name))

(define (spaces n) (make-string (max 0 n) #\space))

;; Whether one range holds another, which is how a nested `at` is told from one
;; the canvas holds itself.
(define (encloses? a b)
  (and (equal? (rng-source a) (rng-source b))
       (<= (rng-start a) (rng-start b))
       (<= (rng-end b) (rng-end a))))

;; A range read out of one place, used inside a copy of it.
(define (shift-range r start)
  (make-range (- (rng-start r) start) (- (rng-end r) start) (rng-source r)))

;; Edits inside a string, back to front so the offsets hold.
(define (splice-string str edits)
  (for/fold ([s str])
            ([e (in-list (sort edits > #:key (lambda (e) (rng-start (car e)))))])
    (string-append (substring s 0 (rng-start (car e))) (cdr e)
                   (substring s (rng-end (car e))))))

;; A piece of source moved to another column. Its first line goes wherever it
;; is put; the lines under it keep their shape relative to that one.
(define (reindent str delta)
  (define lines (string-split str "\n" #:trim? #f))
  (cond
    [(or (zero? delta) (null? (cdr lines))) str]
    [(positive? delta)
     (string-join lines (string-append "\n" (spaces delta)))]
    [else
     (string-append
      (first lines)
      (apply string-append
             (for/list ([l (in-list (cdr lines))])
               (define drop (min (- delta) (- (string-length l) (string-length (string-trim l #:right? #f)))))
               (string-append "\n" (substring l drop)))))]))

(define (background->source want)
  (cond
    [(eq? ABSENT want) "#false"]
    [(string? want) (format "hex(~s)" want)]
    [(and (list? want) (= 2 (length want)))
     (format "hex(~s, ~~alpha: ~a)" (first want) (num->source (second want)))]
    [else #f]))

;; `font` and `(font 2)` are written the same way; which run they belong to is
;; the site's business, not the value's.
(define (property-head property) (if (pair? property) (first property) property))

;; How a report names one: "font" for the first run, "font of run 2" for the
;; rest, since a body of one run should read the way it always did.
(define (property-name property)
  (if (eq? 'shape-adjust property)
      "the shape's adjustment"
      (property-name* property)))

(define (property-name* property)
  (if (pair? property)
      (format "~a of ~a ~a" (first property)
              (if (memq (first property)
                        '(align line-spacing space-before space-after
                          level margin-left indent bullet))
                  "paragraph" "run")
              (second property))
      (format "~a" property)))

(define (style->source property0 value)
  (define property (property-head property0))
  (case property
    [(fill line text-color) (format "~s" value)]
    [(size line-width fill-opacity) (num->source value)]
    ;; The shape it is drawn as, and the typeface it is set in: both names, both
    ;; written as the strings the source states them as.
    [(shape font) (format "~s" value)]
    ;; The handles: `adj=val 30000;adj2=val 5000` becomes the list
    ;; `preset_geom` reads, in the order the shape states them.
    [(shape-adjust)
     (format "[~a]"
             (string-join
              (for/list ([part (in-list (string-split (format "~a" value) ";"))]
                         #:when (regexp-match? #rx"=" part))
                (let ([m (regexp-match #rx"^([^=]*)=(.*)$" part)])
                  (format "pair(~s, ~s)" (second m) (third m))))
              ", "))]
    [(bold italic) (if value "#true" "#false")]
    ;; A level is a whole number of steps, and `1.0` is not the number the
    ;; parser reads back out of `lvl="1"`.
    [(level) (format "~a" (inexact->exact (round value)))]
    [(space-before space-after margin-left indent spacing baseline)
     (num->source value)]
    [(underline strike) (if value "#true" "#false")]
    ;; A bullet is a call of five things, and no bullet at all is `no_bullet`.
    [(bullet)
     (if (list? value)
         (format "bullet(~a, ~a, ~a, ~a, ~a)"
                 (style->source 'dash (first value))
                 (if (second value) (format "~s" (second value)) "#false")
                 (if (third value) (format "~s" (third value)) "#false")
                 (if (fourth value) (num->source (fourth value)) "#false")
                 (if (fifth value) (format "hex(~s)" (fifth value)) "#false"))
         "no_bullet")]
    ;; `line_end(#'triangle, "med", "med")`, or nothing on that end at all.
    [(head tail)
     (if (list? value)
         (format "line_end(~a, ~s, ~s)"
                 (style->source 'dash (first value)) (second value) (third value))
         "#false")]
    [(wrap) (if value "#true" "#false")]
    [(insets) (format "insets(~a)" (string-join (map num->source value) ", "))]
    [(opacity) (num->source value)]
    [(crop) (if (list? value)
                (format "[~a]" (string-join (map num->source value) ", "))
                "#false")]
    ;; `(percent . 1.5)` and `(points . 18.0)` -- the runtime takes either.
    [(line-spacing) (format "pair(#'~a, ~a)" (car value) (num->source (cdr value)))]
    ;; A hyphen is subtraction in Rhombus, so a name that is not an identifier
    ;; there has to be written the long way.
    [(dash align anchor autofit cap caps) (let ([n (format "~a" value)])
              (if (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" n)
                  (format "#'~a" n)
                  (format "#'#{~a}" n)))]
    [else (format "~a" value)]))

;; The correction's value, as a Rhombus list.
(define (nudge->source dx dy)
  (format "[~a, ~a]" (num->source dx) (num->source dy)))

;; A whole `~nudge:` argument, inserted just after the tag.
(define (nudge-argument->source dx dy)
  (format ", ~~nudge: ~a" (nudge->source dx dy)))

;; A number as it should read in source: the same rounding the emitter uses.
(define (num->source v)
  (define r (/ (round (* 1000.0 (exact->inexact v))) 1000.0))
  (if (integer? r) (format "~a.0" (inexact->exact r)) (format "~a" r)))

;; Replaces ranges from the end backwards, so earlier offsets stay valid.
;; Comments, formatting and every untouched line survive exactly.
;;
;; The work happens in characters, not bytes: `syntax-position` counts
;; characters, and a generated program has multi-byte ones in it -- a bullet
;; glyph is three bytes and one character, so splicing by byte offset lands in
;; the wrong place further down the file.
(define (spliced-file-text path edits)
  (define text (file->string path))
  (splice-nested text edits))

;; Apply a save to every source module that owns one of its ranges. Ranges are
;; grouped before any file is written, so offsets from two modules can never be
;; interpreted against the root program by accident. Every result is rendered
;; and staged before the first rename. Backups make the sequence transactional
;; across modules too: if a later rename fails, earlier modules are restored.
(define (splice-files! root edits)
  (define by-source
    (for/fold ([h (hash)]) ([e (in-list edits)])
      (define source (or (rng-source (first e)) (path->complete-path root)))
      (hash-update h source (lambda (es) (cons e es)) '())))
  (define planned
    (for/list ([(source source-edits) (in-hash by-source)])
      ;; The grouping fold conses, so restore the order `apply-actions!` chose.
      ;; Equal-position insertions rely on it: two shapes added in one save must
      ;; be emitted in the same drawing order as the deck.
      (cons source (spliced-file-text source (reverse source-edits)))))
  (define staged '()) ; (list source replacement backup)
  (define committed '())
  (define (discard path)
    (when (and path (file-exists? path))
      (with-handlers ([exn:fail? void]) (delete-file path))))
  (define (cleanup!)
    (for ([s (in-list staged)])
      (discard (second s))
      (discard (third s))))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          ;; A replacement already renamed into place gets its original back.
          (define unrecovered (make-hash))
          (for ([s (in-list committed)])
            (with-handlers ([exn:fail? (lambda (_restore-error)
                                         ;; Leave this backup beside the source
                                         ;; rather than discard the only intact
                                         ;; copy when restoration itself fails.
                                         (hash-set! unrecovered (third s) #t))])
              (rename-file-or-directory (third s) (first s) #t)))
          (for ([s (in-list staged)])
            (discard (second s))
            (unless (hash-ref unrecovered (third s) #f)
              (discard (third s))))
          (raise e))])
    (for ([p (in-list planned)])
      (define source (car p))
      (define dir (or (path-only source) (current-directory)))
      (define replacement
        (make-temporary-file (path->string (build-path dir ".glide-stage~a"))))
      (define backup
        (make-temporary-file (path->string (build-path dir ".glide-backup~a"))))
      (copy-file source backup #t)
      (call-with-output-file replacement #:exists 'truncate/replace
        (lambda (o) (write-string (cdr p) o)))
      (define mode (with-handlers ([exn:fail? (lambda (_e) #f)])
                     (file-or-directory-permissions source 'bits)))
      (when mode (file-or-directory-permissions replacement mode))
      (set! staged (cons (list source replacement backup) staged)))
    (for ([s (in-list staged)])
      (rename-file-or-directory (second s) (first s) #t)
      (set! committed (cons s committed)))
    (cleanup!)))

;; What an edit replaces its range with: either a string, or a plan -- strings
;; and ranges of the file to copy, in the order they should read.
;;
;; A plan is how a rewrite that moves text says so. Reordering a slide's `at`
;; forms moves the forms and leaves the commas and indentation between them, so
;; it is a plan of eight pieces rather than one string; and because the pieces
;; say where they came from, an edit inside a form that moves travels with it.
;; Written flat, the two would be laid over each other and the program would
;; stop parsing.
(define (edit-tree-range t) (first t))

;; Which edits are inside which. Sorted so a range comes before what it holds,
;; each one then takes the edits that fall within it as its own.
(define (nest-edits edits)
  (define sorted
    (sort edits (lambda (a b)
                  (define ra (first a))
                  (define rb (first b))
                  (if (= (rng-start ra) (rng-start rb))
                      (> (rng-end ra) (rng-end rb))
                      (< (rng-start ra) (rng-start rb))))))
  (define (gather es limit)
    (cond
      [(null? es) (values '() '())]
      [(and limit (>= (rng-start (first (car es))) limit)) (values '() es)]
      ;; Half in and half out: nothing here writes such a pair, and there is no
      ;; order that makes both come true, so it is dropped rather than written
      ;; over its neighbour.
      [(and limit (> (rng-end (first (car es))) limit)) (gather (cdr es) limit)]
      [else
       (define e (car es))
       (define-values (kids rest) (gather (cdr es) (rng-end (first e))))
       (define-values (sibs left) (gather rest limit))
       (values (cons (list (first e) (second e) kids) sibs) left)]))
  (define-values (trees rest) (gather sorted #f))
  trees)

;; The text one edit produces, with everything inside it already applied.
(define (render-edit text t)
  (define body (second t))
  (define kids (third t))
  (cond
    ;; A rewrite of its own: it says what its range now reads, so an edit that
    ;; was inside it has nothing left to change. Dropping it costs a pass and
    ;; not the edit -- the base is the program as it now reads, so the next
    ;; look reports the difference again.
    ;;
    ;; An insertion is not one of those. It does not change the text being
    ;; rewritten; it puts new text at a point inside it, and there is nothing
    ;; left of that point -- so it goes after what the rewrite says. Dropped, it
    ;; costs the edit and not a pass: deleting a shape in the editor and drawing
    ;; a new one writes the new form after the deleted one, and the deletion
    ;; rewrites that form to nothing. The form was reported as written, the loop
    ;; wrote the deck again from a program that did not hold it, and the shape
    ;; the editor had just drawn was gone.
    [(string? body)
     (apply string-append body
            (for/list ([k (in-list (sort kids < #:key (lambda (k) (rng-start (first k)))))]
                       #:when (let ([r (first k)]) (= (rng-start r) (rng-end r))))
              (render-edit text k)))]
    [else
     (apply string-append
            (for/list ([item (in-list body)])
              (if (string? item)
                  item
                  (copy-with-edits text item
                                   (for/list ([k (in-list kids)]
                                              #:when (encloses? item (first k)))
                                     k)))))]))

;; A copied stretch of the file, with the edits that fall inside it applied.
(define (copy-with-edits text r kids)
  (for/fold ([s (substring text (rng-start r) (rng-end r))])
            ([k (in-list (sort kids > #:key (lambda (k) (rng-start (first k)))))])
    (string-append (substring s 0 (- (rng-start (first k)) (rng-start r)))
                   (render-edit text k)
                   (substring s (- (rng-end (first k)) (rng-start r))))))

;; Outermost edits, from the end backwards so earlier offsets stay valid.
(define (splice-nested text edits)
  (for/fold ([s text])
            ([t (in-list (sort (nest-edits edits) >
                               #:key (lambda (t) (rng-start (edit-tree-range t)))))])
    (define r (edit-tree-range t))
    (string-append (substring s 0 (rng-start r))
                   (render-edit text t)
                   (substring s (rng-end r)))))

;; ------------------------------------------------------------------- driver

;; The state both sides last agreed on. It is derived -- deleting it means the
;; next pass just records where things stand -- so it lives in scratch with the
;; deck rather than beside the program, which holds the program and its images
;; and nothing else.
;; Two paths naming one file, as far as anything here can tell.
(define (same-file-path? a b)
  (define (norm p)
    (with-handlers ([exn:fail? (lambda (_e) (format "~a" p))])
      (path->string (simplify-path (path->complete-path (if (path? p) p (string->path p))) #f))))
  (equal? (norm a) (norm b)))

(define (base-path-for program-path)
  (define full (path->complete-path program-path))
  (define dir (or (path-only full) (current-directory)))
  (build-path dir ".glide"
              (path->string (path-replace-extension (file-name-from-path full)
                                                    ".sync.rktd"))))

;; The watcher already has the picts in hand before it overwrites the editor's
;; deck. Validate their source identities first, then use the same states as the
;; new merge base after the export succeeds. This keeps position-derived ids in
;; step when an ordinary source edit shifts later call sites.
(define (validate-program-picts! program-path picts
                                 #:sites [given-sites #f]
                                 #:scopes [given-scopes #f])
  (define-values (sites scopes)
    (if (and given-sites given-scopes)
        (values given-sites given-scopes)
        (let-values ([(sites scopes _slide-sites _layout)
                      (find-program-sites program-path)])
          (values sites scopes))))
  (define states (picts->slide-states picts))
  (validate-runtime-sites sites scopes states program-path)
  states)

(define (record-program-base! program-path pptx-path #:states [states #f])
  (define current (or states (validate-program-picts!
                             program-path (load-program-picts program-path))))
  (write-sync-base (base-path-for program-path) current
                   #:program (path->string (path->complete-path program-path))
                   #:deck (path->string (path->complete-path pptx-path))))

;; One merge pass: read both sides, merge against the base, patch the source,
;; and record the new agreed state.
;; `atomic?` is what a save means: either every edit the editor made is written
;; or none of them is. A merge that writes four of five edits and reports the
;; fifth leaves the program and the deck each holding part of the truth, and
;; nobody can say which part -- so the fifth failing takes the other four with
;; it, the program is left exactly as it was, and the deck keeps everything
;; until whatever caused it is resolved.
(define (sync-once program-path pptx-path
                   #:workdir [workdir #f]
                   #:dry-run? [dry-run? #f]
                   #:atomic? [atomic? #f])
  ;; Validate the source map before recording a base or opening an editing
  ;; session. Waiting until the first deck change would let an untraceable
  ;; `all_slides` arrangement appear to work and refuse only after the user had
  ;; edits at stake.
  (define-values (sites scopes _slide-sites layout)
    (find-program-sites program-path))
  (define base-file (base-path-for program-path))
  (define-values (base recorded-program _d) (read-sync-base base-file))
  ;; The scratch is picked up rather than cleared when a session starts, so
  ;; that edits made while nothing was watching are merged rather than thrown
  ;; away. That only holds while it is *this* program's scratch: a folder
  ;; copied along with a project, or left by another program, holds a deck and
  ;; a base that have nothing to do with this one.
  (when (and base recorded-program
             (not (same-file-path? recorded-program program-path)))
    (error 'glide
           (string-append
            "~a was written for a different program.\n"
            "  it says:  ~a\n"
            "  this is:  ~a\n"
            "  Delete ~a and start again -- it holds a deck and an agreed base\n"
            "  from another session, and merging those into this program would\n"
            "  be merging someone else's edits.")
           (file-name-from-path base-file) recorded-program
           (path->string (path->complete-path program-path))
           (let ([d (path-only base-file)]) (if d (path->string d) base-file))))
  (define picts (load-program-picts program-path))
  (define prog (validate-program-picts! program-path picts
                                        #:sites sites #:scopes scopes))
  ;; The program has been loaded now, so the typeface it falls back on is known.
  (define fallback-font (current-default-font))
  ;; The deck is unzipped to be read. When the caller did not say where, this
  ;; owns the scratch and clears it before returning.
  (define given-dir workdir)
  (define dir (or workdir (make-temporary-file "syncdeck~a" 'directory)))
  ;; Which of the deck's names it states as ours, collected as it is read and
  ;; consulted as the states are built: the merge is the one reader that has to
  ;; tell the program's elements from the ones the editor made itself. See
  ;; `current-slide-tag-names`.
  (define stated (make-hash))
  (define-values (deck-ir deck)
    (parameterize ([current-slide-tag-names stated])
      (let ([d (pptx->deck pptx-path #:workdir dir)])
        (values d (deck->slide-states d)))))
  (define (done! v)
    (when (and (not given-dir) (not (current-keep-work?)))
      (delete-directory/files dir #:must-exist? #f))
    v)
  (cond
    ;; With no base there is nothing to merge against: the program is the truth
    ;; and this pass just records where both sides stand.
    [(not base)
     (unless dry-run?
       (write-sync-base base-file prog
                        #:program (path->string (path->complete-path program-path))
                        #:deck (path->string (path->complete-path pptx-path))))
     (done! (sync-report '() '() '() '() (not dry-run?) #f))]
    [else
     (define actions
       (protect-shared-runtime-sites
        (parameterize ([inherited-font fallback-font])
          (merge-states base prog deck program-path #:deck-ir deck-ir))
        (shared-runtime-site-keys sites scopes prog)))
     (cond
       [dry-run? (done! (sync-report actions '() '() '() #f #f))]
       [else
        (define before-texts
          (for/hash ([source (in-list (program-layout-files layout))]
                     #:when (file-exists? source))
            (values source (file->string source))))
        (define-values (applied skipped notes behind? refused?)
          (apply-actions! program-path actions #:deck deck-ir #:atomic? atomic?))
        ;; The new base is the program as it now reads, so the next pass
        ;; compares against something both sides agree on. Reading it is also
        ;; the only way to find out whether what was written still runs.
        ;;
        ;; It may not. An element another part of the program refers to -- an
        ;; arrow drawn between two nodes it finds by name -- can be deleted in
        ;; the editor, and the deletion is a perfectly good edit until the
        ;; program is next loaded and the arrow cannot find what it points at.
        ;; Nothing can know that in advance, so it is found out afterwards and
        ;; put back: the file is the source, there is one copy of it, and a
        ;; save that leaves it unloadable is worse than a save refused.
        (define failed (box #f))
        (define after
          (and (not refused?)
               (with-handlers ([exn:fail? (lambda (e)
                                            (set-box! failed (exn-message e))
                                            #f)])
                 (validate-program-picts! program-path
                                          (load-program-picts program-path)))))
        (cond
          ;; Nothing was written, so there is nothing new for the base to
          ;; record: leaving it alone is what makes the next save try again.
          [refused? (done! (sync-report actions '() skipped notes #f #f))]
          [(unbox failed)
           (for ([(source text) (in-hash before-texts)])
             (write-atomically source (lambda (o) (write-string text o))))
           (done! (sync-report
                   actions '()
                   (append (for/list ([a (in-list applied)])
                             (cons a (format "the program does not run with this written: ~a"
                                             (first (string-split (unbox failed) "\n")))))
                           skipped)
                   notes #f #f))]
          [else
           (write-sync-base base-file after
                            #:program (path->string (path->complete-path program-path))
                            #:deck (path->string (path->complete-path pptx-path)))
           (done! (sync-report actions applied skipped notes #t behind?))])])]))

;; (action . why) pairs by why, in the order the reasons first appear, each
;; carrying the names of the elements it was said about. A deck can have thirty
;; shapes refused for one structural reason, and thirty identical lines bury the
;; edits that did apply.
(define (by-reason pairs)
  (let loop ([ps pairs] [order '()] [h (hash)])
    (cond
      [(null? ps) (for/list ([why (in-list (reverse order))])
                    (cons why (reverse (hash-ref h why))))]
      [else
       (define why (cdr (car ps)))
       (loop (cdr ps)
             (if (hash-has-key? h why) order (cons why order))
             (hash-update h why (lambda (v) (cons (sync-action-tag (car (car ps))) v)) '()))])))

;; Four of them and a count of the rest, which is as much as a line holds.
(define (named tags)
  (format "~a~a"
          (string-join (map (lambda (t) (format "~s" t)) (take tags (min 4 (length tags))))
                       ", ")
          (if (> (length tags) 4) (format " and ~a more" (- (length tags) 4)) "")))

(define (format-sync-report r)
  (define (shown-tag t) (or (and (string? t) (automatic-tag-name t)) t))
  (define o (open-output-string))
  (define as (sync-report-actions r))
  (cond
    [(null? as) (fprintf o "  nothing to merge\n")]
    [else
     (for ([a (in-list as)] #:unless (eq? 'noted (sync-action-kind a)))
       (fprintf o "  slide ~a  ~a  ~s~a\n" (sync-action-slide a)
                (~a (sync-action-kind a) #:min-width 12)
                (shown-tag (sync-action-tag a))
                (if (memq (sync-action-kind a) '(moved resized))
                    (let ([g (sync-action-detail a)])
                      (format "  -> ~a,~a ~ax~a"
                              (~r (first g) #:precision 1) (~r (second g) #:precision 1)
                              (~r (third g) #:precision 1) (~r (fourth g) #:precision 1)))
                    "")))
     ;; Grouped by reason. A deck can have thirty shapes the merge refuses for
     ;; one structural reason, and thirty identical lines bury the edits that
     ;; did apply.
     (define skipped-by-reason
       (let loop ([sks (sync-report-skipped r)] [order '()] [h (hash)])
         (cond
           [(null? sks) (for/list ([why (in-list (reverse order))])
                          (cons why (reverse (hash-ref h why))))]
           [else
            (define why (cdr (car sks)))
            (loop (cdr sks)
                  (if (hash-has-key? h why) order (cons why order))
                  (hash-update h why (lambda (v) (cons (shown-tag (sync-action-tag (car (car sks)))) v))
                               '()))])))
     (for ([g (in-list skipped-by-reason)])
       (define tags (cdr g))
       (cond
         [(= 1 (length tags))
          (fprintf o "    not applied: ~s -- ~a\n" (first tags) (car g))]
         [else
          (fprintf o "    not applied, ~a of them -- ~a\n" (length tags) (car g))
          (fprintf o "      ~a\n" (named tags))]))
     ;; Notes, grouped by reason and named. A terse editor can leave the same
     ;; note on every text box on a slide, and a slide can have thirty shapes
     ;; the code draws rather than places -- so one line a reason, and the names
     ;; under it, because the one thing to do about a note is know which shape
     ;; it is about.
     (define-values (with-caveat not-merged)
       (partition (lambda (n) (and (memq (car n) (sync-report-applied r)) #t))
                  (sync-report-notes r)))
     (define (say-notes notes heading)
       (unless (null? notes)
         (define groups (by-reason notes))
         (fprintf o "  ~a element~a ~a:\n"
                  (length notes) (if (= 1 (length notes)) "" "s") heading)
         (for ([g (in-list (take groups (min 3 (length groups))))])
           (define tags (cdr g))
           (cond
             [(= 1 (length tags)) (fprintf o "    ~s -- ~a\n" (first tags) (car g))]
             [else (fprintf o "    ~a of them -- ~a\n" (length tags) (car g))
                   (fprintf o "      ~a\n" (named tags))]))
         (when (> (length groups) 3)
           (fprintf o "    and ~a more like it\n" (- (length groups) 3)))))
     (say-notes not-merged "not merged")
     ;; Written, and not quite as the editor showed it.
     (say-notes with-caveat "written, and worth knowing")
     ;; The notes are their own actions rather than a subset of these, so they
     ;; are counted and not subtracted -- doing both reported "-1 reported" on a
     ;; pass that applied one edit and noted one thing.
     (define acted (filter (lambda (a) (not (eq? 'noted (sync-action-kind a)))) as))
     (fprintf o "  ~a applied, ~a reported~a\n"
              (length (sync-report-applied r))
              (max 0 (- (length acted) (length (sync-report-applied r))))
              (let ([notes (sync-report-notes r)])
                (if (null? notes) "" (format ", ~a noted" (length notes)))))])
  (get-output-string o))
