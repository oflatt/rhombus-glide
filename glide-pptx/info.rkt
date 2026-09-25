#lang info

(define collection "glide-pptx")
(define version "0.0")
(define license 'MIT)

(define deps '("base" "pict-lib" "draw-lib" "rhombus-lib" "shrubbery-lib"
               ;; `show.rhm` and `staged.rhm` present a deck as a talk.
               "slideshow-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "Direct manipulation of Rhombus and Racket pict slideshows through PowerPoint or Keynote.")

;; `raco glide <command>`. The collection stays `glide-pptx`, because every
;; program this has ever written imports `lib("glide-pptx/runtime.rhm")`.
(define raco-commands
  '(("glide" glide-pptx/main "translate, export and sync PowerPoint decks" 60)))
