# Rhombus Glide

Rhombus Glide is a library that provides a GUI for *direct manipulation*
of rhombus slideshow programs.
It works with little modification
to existing rhombus programs, only requiring a list of slides and a
list of other tabs to be defined.
A slide is a function the produces an animated picture.
A *tab* is a list of slides, useful for providing tools to the GUI
  that can be dragged onto existing slides.

```
#lang rhombus

import:
  pict open
  glide open

glide {}:
  fun slide1():
    beside(~sep: 100, bubble(~width: 100), bubble())
  
  fun slide2():
    bubble()
  
  let tabs:
    [[bubble()]]
  
  let slides:
    [slide1()]

2

```

Select sub-elements of your slides and drag them to add padding.
Drag and drop new elements to add them to your slides.