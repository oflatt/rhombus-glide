# glide-pptx

Direct manipulation of pict slideshows through PowerPoint or Keynote, instead of
through a GUI of our own.

`glide` gives you a window to drag slide elements around in. `glide-pptx` gives
you PowerPoint or Keynote for the same job: it translates a `.pptx` into a
[Pict](https://docs.racket-lang.org/pict/) program in Rhombus, exports
any pict program back to `.pptx`, and keeps the two in step while you edit
either one. Both directions are checked by rendering to PDF and diffing the
pages.

The point is the same as glide's: use a direct-manipulation editor for what it is
good at — dragging things until they look right — and code for what it is good at
— animation, abstraction, reuse, version control. The two share the hard part,
which is turning a drag into an edit of the source that leaves the rest of the
file alone.

```console
$ raco glide translate -o out talk.pptx
out/talk.rkt  (14 slides, 212 elements)

$ racket out/talk.rkt
wrote talk.pdf

$ raco glide export -o talk-edit.pptx out/talk.rkt
talk-edit.pptx  (14 slides)

$ raco glide verify talk.pptx
talk
  page    mean err    pixels off
  1       0.50%       0.87%        ok
  2       0.74%       0.98%        ok
  ...
  PASS
```

Export works on **any** pict program, not only generated ones. It is the mirror
of `picts->pdf`:

```racket
(require pict glide-pptx/export)

(picts->pptx (list slide-1 slide-2) "out.pptx" #:width 960 #:height 540)
```

## What the generated code looks like

Positions, sizes, colors and text come from the file; everything else is
ordinary code, so a slide is a `pict` you can compose with anything. This is
`tests/decks/05-realistic.pptx`, slide 3, as `translate` writes it:

```rhombus
#lang rhombus/and_meta

import:
  lib("glide-pptx/runtime.rhm") open

def slide_width = 959.976
def slide_height = 540.0

// The theme font. Runs that name no typeface use this one, so restyling
// the whole deck is one edit.
current_default_font("Calibri")

def slide_3 = slide_canvas(
  ~width: slide_width, ~height: slide_height,
  ~background: hex("FFFFFF"),
  // TextBox 1 (id 2)
  at(50.4, 28.8, ~name: "TextBox 1",
     textbox(~width: 792.0, ~height: 72.0, ~wrap: #false, ~autofit: #'grow,
             para(run("Pipeline", ~size: 40.0, ~bold: #true, ~color: hex("1F3B63"))))),
  // Rounded Rectangle 2 (id 3)
  at(57.6, 158.4, ~name: "Rounded Rectangle 2",
     shape_pict(~width: 172.8, ~height: 86.4, ~shape: "roundRect", ~fill: hex("4472C4"),
                ~body: body(~anchor: #'center,
                            para(~align: #'center,
                                 run("pptx", ~size: 22.0, ~bold: #true,
                                     ~color: hex("FFFFFF")))))))
```

The visible `~name:` is only readable metadata. Identity is normally absent
from the program: Rhombus's `at` macro derives an opaque tag from the call's
source file and location, and Glide puts that tag in the exported shape's alt
text and object name. The duplication is intentional: PowerPoint preserves alt
text, while LibreOffice drops it from groups and preserves the object name. A
readable `~name:` is retained as the tag's suffix.

That source tag names a *code site*, not an element, so one `at` inside a loop
draws several elements under one tag. Dragging all of them the same way is one
correction on the one `at`; dragging or deleting only some of them is refused,
with the reason, because no single correction produces it.

The same source site can also be reused on several slides. It exports normally,
but an edit to only one page is refused because changing that source call would
change every use. Give the outer calls distinct proxy tags only when those uses
really need to be independently editable.

An explicit `~tag:` remains an escape hatch for an abstraction whose one inner
`at` is invoked as several independently editable objects. Put distinct literal
tags on those outer helper calls and pass each tag through to the inner `at`.
Glide also uses explicit tags for source it inserts after a shape or slide is
created in the editor, so that existing editor object keeps its identity across
the structural handoff. Ordinary authored and translated `at` calls need none.

### What an edit in the editor does

| in PowerPoint or Keynote | comes back as |
|---|---|
| move a shape | the `at` position, or a `~nudge:` when that position is computed |
| resize it, or drag a line's endpoint | the leaf's `~width:`/`~height:` |
| rotate it | `~rotate:`, added if it was not there |
| drag an endpoint past the other end | `~flip_h:`/`~flip_v:`, added if not there |
| retype text | the string literal, when the text is one run |
| draw a new shape | a new `at(...)`, where the deck draws it in the order |
| delete a shape | its `at` form removed |
| paste a slide in | a `def slide_N` and an entry in `all_slides` |
| move a group | the group, as one element |
| leave the editor on a slide | it is put back there when the deck is written again |
| move all of a repeated tag together | one correction on the one `at` |
| recolour a shape, its outline, or its text | the `hex("...")` where it stands |
| change an outline's width | the stroke's `~width:` |
| reorder the slides | `all_slides`, rewritten in the new order |
| duplicate a shape | a new `at(...)`, under a name of its own |
| recolour one that uses a named colour | the `def`, when everything using it changed with it |
| bring a shape to the front | the `at` forms, moved into the new order |
| delete a slide | its `def`, its comment, its `export:` entry and its `all_slides` entry |
| repaint a slide's background | the canvas's `~background:` |
| hide a slide | the canvas's `~hidden:`, and the show passes over it |
| change a font, a size, boldness, italics | that `run`'s own arguments, whichever run it is |
| make a line dashed, or put an arrowhead on it | `~dash:`/`~head:`/`~tail:`, added to the stroke |
| give a shape a fill or an outline it had none of | a whole `~fill:`/`~line:` argument, added |
| take a fill or an outline away | `~fill: #false`/`~line: #false` |
| make a fill translucent | `~alpha:` inside its colour |
| centre text, space its lines, space its paragraphs | that `para`'s own arguments, whichever paragraph it is |
| anchor text, unwrap it, autofit it, inset it | the `textbox`'s own arguments |
| retype a word of a styled line | the run the change fell inside |
| crop a picture, or fade it | the picture's `~crop:`/`~opacity:` |
| swap a picture's image | the new file, copied beside the program, and `media("...")` |
| group two shapes | their `at` forms, moved inside a `group_pict` |
| ungroup them | their `at` forms, lifted back out of it |

An argument the source does not state is **added** rather than reported: a
solid line has no `~dash:` to rewrite, and adding one is the answer. An
argument it does state is rewritten in place -- never both, since a second
`~width:` in one call would not compile.

A picture is known by the size of its file, which is the one thing the two
sides can compare: the program names a path and a deck names a part inside
itself. Swapping the image copies the new file next to the program under a name
that does not clobber what is there, and points `media("...")` at it -- two
pictures of exactly the same size are a miss worth taking for a stat instead of
a read of every image on every save.

Every run and every paragraph answers for itself: the k-th `run(...)` call in
the source is the k-th run of the body, because both sides read them paragraph
by paragraph. So bolding one word of a line lands on the run that word is in,
and a report that cannot write one says which -- "font of run 2", not "font".

Refused, with the reason, rather than guessed at: moving or deleting *one* of
several elements drawn by one source site, retyping that spans two runs or a
paragraph break, moving something whose position cannot be associated with a
parseable `at` call, a fill the editor made a gradient, grouping shapes whose
positions the program computes, and reordering `at` forms with a comment
standing between them.

Grouping and ungrouping move the `at` forms rather than writing new ones, which
is the difference between keeping what the code says about a shape and
replacing it with what the deck shows: a colour shared through a `def` stays
shared, a computed size stays computed, and the comment above a shape goes with
it.

Appearance follows the same rule as geometry: written where the source states
it, added where it does not, and **reported by name where neither is possible**.
A colour with a name of its own belongs to everything that uses it, so
`def brand = hex(...)` is rewritten only when every shape using it changed the
same way -- and when they did not, the report says which name and how many did
not change with it. What is still not carried back is what neither side can
say: which stops a gradient the editor made has, a bullet, a picture used as a
fill, and the styling of any run but the first. Those are reported and left,
never passed over in silence.

An action either lands whole or not at all. One that writes part of itself and
then finds it cannot write the rest is reported as refused and what it had
written is dropped -- a half-applied edit is how a program stops compiling.

**What the deck does not say is not an edit.** A shape states some of its
appearance and inherits the rest -- from its placeholder, the layout, the
master, the theme -- and editors differ in how much they bother to state.
Keynote's pptx export leaves out a great deal that ours writes. So the parse
records, per run and per paragraph and per body, which properties the shape
states *itself*, and only those are compared: an editor that says less than we
do produces no edits at all rather than hundreds of removals nobody made. Where
a deck does state a value and it is the one the format means when it says
nothing -- Calibri 18, left-aligned, not bold -- the difference is **noted**
rather than written, since those two cannot be told apart. A note is neither
applied nor refused and does not stop a save.

**And a save is one thing.** If any edit in it cannot be written, none of them
is: the program is left exactly as it was, the deck is not rewritten, and the
report names what stopped it. Writing four edits of five and reporting the
fifth would leave the program and the deck each holding part of what was done,
with nothing to say which part -- and the next regeneration would quietly
settle it in the program's favour. So the merge fails instead, and keeps
failing until what it names is resolved, in the editor or in the program.

Slides are matched on their contents, not by index, so pasting one in from
another deck does not shift the ones after it. A pasted slide becomes a
`def slide_N = slide_canvas(...)` and an entry in `all_slides` at the position it
sits in the deck -- the definition goes after the last existing one, since
`all_slides` is what carries the order and nothing else has to be renumbered.
A slide deleted in the editor takes its definition with it, and only that: a
program that names the slide anywhere else is one the merge would be rewriting
rather than following, and it says so and stops.

A merge that both loses a slide and gains one is a matching that could not
follow the deck, not a deletion and a new slide -- grouping two shapes of three
is enough to make a slide stop looking like itself. Both are reported and
nothing is rewritten, because guessing wrong there deletes a definition the
program still wants.

The program is the truth, so a session starts from it: the scratch beside it is
cleared, the deck is written again, and the two agree before the editor opens.
Anything left over is from a session that ended without merging -- a crash, or
a deck opened behind glide's back -- and merging that would be merging edits
against a program that has moved on. The scratch is cleared again when the
editor closes and on Ctrl-C, unless the last merge was refused, when it is kept
deliberately because the deck then holds something the program does not.

Which means: **edit through `raco glide`, and do not open the deck by hand.**
The deck in the scratch is derived, and anything done to it outside a session
is lost when the next one starts.

PowerPoint is the editor by default on macOS. Both are driven the same way, but
PowerPoint reads and writes its own format, while Keynote's export states less
than it shows -- a shape's typeface, its anchor, its spacing can all come back
missing, and a merge can only go on what the deck says. `--app keynote` still
works, and what Keynote leaves out is ignored rather than taken for an edit.

A `.key` is accepted anywhere a deck is: Keynote is asked to export a `.pptx`
first, which needs macOS.

Running the program shows the slides -- its `module main` is a
[slideshow](https://docs.racket-lang.org/rhombus-slideshow/), and each slide
reaches it through `Pict.from_handle`, which is also the bridge for refactoring
generated code into `rhombus/pict`'s animated picts. The backup PDF is a
submodule:

```shell
racket talk.rhm                                              # show the slides
racket -l racket/base -e '(require (submod (file "talk.rhm") pdf))'   # write the PDF
```

## Starting one

```shell
raco glide --new talk.rhm     # a slideshow to start from
raco glide talk.rhm           # open it, keep both in step
```

A new program is written by the same emitter that writes an imported deck, so
it is in the dialect Glide reads back: source-located `at` forms, a literal
`glide_slides` manifest, a slideshow module and a PDF submodule.

## The workflow

```shell
raco glide talk.pptx -o talk.rhm   # once, to get a program
raco glide talk.rhm                # then this, and edit either side
```

The second command exports the program to a deck, opens it in an editor
(Keynote on macOS, `--app` to choose), and keeps the two in step: save the
program and the deck is rewritten; save in the editor and the drag comes back as
a literal in the source. The deck, the editor's own document and the agreed base
are scratch, kept in `.glide/` -- what the talk folder holds is `talk.rhm`, any
locally imported `.rhm` modules, and its `media/`.

### Splitting a talk across source files

The program passed to `raco glide` is the root module. Glide follows its local
Rhombus imports transitively, so slide definitions can live in imported `.rhm`
files:

```rhombus
// talk.rhm
#lang rhombus/and_meta
import:
  lib("glide-pptx/runtime.rhm") open
  "intro.rhm" open
  "results.rhm" open
export:
  all_slides
glide_slides all_slides:
  [slide_1, slide_2, slide_20]
```

Each slide module exports the definitions used by the root and imports whatever
runtime or common helpers it needs. An `at(...)` remains owned by the file where
it is written: saving the deck patches that file, while reordering slides
patches the root's `all_slides`. Saving any local imported `.rhm` file regenerates
the deck. Imports such as `lib("...")` are installed libraries, not program
source, and are neither watched nor edited.

`glide_slides` makes `all_slides` an ordinary `List`, so existing show, PDF and
snapshot code uses it unchanged. It also records a private checked manifest
that connects each editor page to its source definition. An ambiguous computed
entry is rejected while the module is compiled instead of after an editor save.

A section decorator can stay directly in the running order. Its last argument
is the source owner; the decorated value is used by the show, while Glide loads
the undecorated slide for synchronization:

```rhombus
fun slide_20():
  slide_canvas(
    ~width: slide_width, ~height: slide_height,
    at(40.0, 60.0, title("Results"))
  )

glide_slides all_slides:
  [slide_1, slide_2, in_section(2, slide_20)]
```

`divider(section)` is the corresponding generated-page form. For another
decorator, use `show_as(slide_20, decorated(slide_20))`; for another generated
page, use `show_only(generated_page())`. These marker forms make ownership
explicit and do not exist as runtime functions. A plain literal
`def all_slides = [slide_1, slide_2]` remains supported for existing decks, but
newly emitted decks use the macro.

A slide-level structural edit -- adding an element, repainting or hiding the
slide -- additionally needs the named owner itself to contain one statically
recoverable `slide_canvas`; otherwise that edit is refused rather than assigned
to an inner slide that may be shared elsewhere.

Media paths are still relative to the root program.


Each element keeps the shape id and name PowerPoint gave it as a comment, and
the name as optional selection-pane metadata. Those are useful human landmarks;
the hidden source-location tag is what a future write-back matches.

## Commands

| Command | What it does |
| --- | --- |
| `program.rhm` | open it in an editor and keep the two in step (the default) |
| `translate deck.pptx [-o dir\|file.rhm]` | emit a Rhombus program plus the images it uses |
| `export program.rhm [-o out.pptx]` | write a `.pptx` from the program's slide picts |
| `sync program.rhm deck.pptx [-n]` | merge the deck's edits back into the program |
| `watch program.rhm [--app keynote]` | keep both in step, in both directions |
| `render deck.pptx [-o dir]` | render straight to PDF, skipping code generation |
| `verify deck.pptx ...` | render both ways and report per-page differences |
| `ir deck.pptx` | print the intermediate representation |
| `presets` | list the shape geometries drawn exactly |

Anything drawn approximately is reported on stderr rather than silently
approximated:

```console
$ raco glide translate deck.pptx
2 notes about approximate rendering:
  - graphicFrame Chart 4: unsupported content (chart or diagram), drawn as an empty box
  - picture Image 9: unresolved image rId7
```

## How export works

A pict is drawn through `record-dc%`, whose `get-recorded-datum` returns the
drawing as an interpretable command stream. That stream keeps **high-level
operations rather than flattened paths** — a rectangle records as
`draw-rectangle`, text as `draw-text` with its font — so each one becomes a real
PowerPoint object:

| recorded op | becomes |
| --- | --- |
| `draw-rectangle`, `draw-rounded-rectangle` | `prstGeom` rect / roundRect |
| `draw-ellipse` | `prstGeom` ellipse |
| `draw-path`, `draw-polygon`, `draw-lines`, `draw-arc` | `custGeom` with Beziers |
| `draw-text` | a text box you can retype, in the same typeface |
| `draw-bitmap` | a picture, with a PNG media part |
| brushes and pens | `solidFill`, `gradFill`, `a:ln` with dash and cap |
| transforms | folded into a matrix and baked into coordinates |

Exporting a slide of two shapes and two strings produces five `p:sp` shapes with
four `prstGeom` rects, one `prstGeom` ellipse and two editable text runs — no
`custGeom`, no rasterization.

Going through SVG would not work: `svg-dc%` renders that same slide as 24
`<path>` elements with **zero** `<rect>` and **zero** `<text>`, because cairo
outlines every glyph. See `docs/export-design.md`.

What the flattened path cannot do: text does not reflow (one box per `draw-text`
call, `wrap="none"`), DrawingML has no arbitrary clipping, and sheared text has
to rasterize.

### The editor only offers what can be honored

A pict with no descriptor -- `(vc-append 5 a b c)` inside an `at` -- has no
structure to sync. Flattening its drawing would give a pile of separate shapes,
every one draggable in Keynote and none of them movable back, because the only
thing the source names is the enclosing `at`. So it is exported as **one
picture** instead: one object per `at`, and dragging it lands on numbers that
exist.

The cost is that its text becomes pixels, which is why `--no-flatten` exists for
a one-way export where nothing will be synced and separate shapes are strictly
better. Each flattened element is reported.

### When the program's structure is known

A pict built with this runtime carries a description of how it was built, so it
does not have to be flattened at all. `roundRect` stays a `roundRect` rather than
becoming a path, a paragraph stays a paragraph, and every element carries the
shape name it had in PowerPoint:

```racket
(at 57.6 158.4 #:tag "Rounded Rectangle 2"
    (shape-pict #:width 172.8 #:height 86.4 #:shape "roundRect" ...))
```

becomes `<p:sp>` with `<p:cNvPr name="Rounded Rectangle 2"
descr="glide-pptx:Rounded Rectangle 2"/>`, a `prstGeom prst="roundRect"`, and a
reflowable `<a:txBody>`.

The effect is measurable. A deck exported from a generated program reproduces the
**original** `.pptx` better than our own renderer does, because PowerPoint is
handed back its own presets and text bodies rather than our approximation of
them:

| deck | our render vs original | exported deck vs original |
| --- | --- | --- |
| 01-placeholders | 0.20 – 0.74% | 0.00 – 0.16% |
| 02-text | 0.67% | 0.15% |
| 03-shapes | 0.41 – 1.48% | 0.40 – 0.45% |
| 04-pictures-groups | 0.30 – 0.35% | 0.00 – 0.11% |
| 05-realistic | 0.70 – 1.23% | 0.14 – 0.22% |

Two of those pages come out pixel-identical. The carrier is a `pict` subtype, so
it survives every pict combinator except `launder`; anything without a
descriptor still exports through the flattened path.

### A helper that says what can be edited

A talk draws things with helpers of its own: a bubble pinned to a token in a
code listing, a callout with a spike, a header built from a section list. What
those draw has nothing in the source an edit could be written into -- the
position is measured from the thing it points at, and the words are arguments
the helper lays out itself:

```rhombus
def (rx, ry) = pc.Find.right(token).in(code)
callout(~on: layer, ~at: pc.Find.abs(rx, ry), ~spike: #'w,
        prose([["Write ", #'plain], ["x", #'bold]]))
```

Drag that in the editor and there is no number to rewrite. So the call says
what it offers, by carrying the same two arguments an `at` carries:

```rhombus
callout(~on: layer, ~at: pc.Find.abs(rx, ry), ~spike: #'w,
        ~tag: "why x", ~nudge: [0.0, 0.0],
        prose([["Write ", #'plain], ["x", #'bold]]))
```

* `~tag:` makes what the helper draws one element with a name. The helper
  passes it on to the `at` it builds, so the element on the slide answers to
  the same name -- that is the whole of the helper's side of the bargain, along
  with taking `~nudge:` and passing it on too, or a correction would be written
  and never drawn.
* `~nudge:` is where a drag is recorded: `~nudge: [12.0, -8.0]`. The helper
  keeps working out where the bubble goes, and the correction says how far off
  that was -- which is what a hand adjustment to a pinned bubble is. It is one
  argument rather than a wrapper, so a second drag replaces those two numbers
  instead of stacking another correction on top.
* The strings the call holds are its runs, in the order they are written, so
  retyping a word rewrites the string it came from. A retyping that runs across
  two of them is refused, the way it is anywhere else -- which of them it
  belonged to is a guess.
* `~width:` and `~height:`, where the helper takes them, are written by a
  resize.

Deleting is not offered: the call may be named and drawn somewhere else, so
taking it out is a change to what the program does rather than to a literal in
it. That is said rather than done.

The helper's own `at` is not a site -- its arguments are variables -- which is
exactly why the call has to be one.

## Keeping a program and a deck in step

`watch` is the parent process for the whole loop:

```console
$ raco glide watch talk.rkt --app keynote
watching
  program talk.rkt
  deck    talk.key
  app     keynote
program changed -> regenerating talk.pptx
  14 slides written
deck changed -> merging into talk.rkt
  slide 3  moved        "Rounded Rectangle 2"  -> 100.0,200.0 172.8x86.4
  1 applied, 0 reported
```

Saving the program regenerates the deck and tells the app to show it. Saving the
deck merges its geometry back into the program's source.

Change detection is by content hash rather than filesystem event. That costs a
poll and buys three things: it works for a Keynote `.key`, which is a directory;
it collapses the burst of writes an editor makes when saving; and it is the loop
guard, since a file we wrote ourselves hashes to what we expect and so does not
look like someone else's edit.

### What a merge is allowed to do

A three-way merge against a base -- the state both sides agreed on at the last
sync, kept in `talk.sync.rktd` next to the program and belonging in version
control. The rule that keeps the loop stable falls out of the intent:
**PowerPoint owns geometry, code owns everything else.** With the two sides
owning disjoint properties, the ordinary cycle has no conflicts at all.

Every edit is one of two things: replace a literal, or wrap an expression in a
known form. Nothing restructures the program.

| what changed | position is a literal | position is computed |
| --- | --- | --- |
| drag | patch the two numbers on `at` | insert or update a `#:nudge` correction |
| resize | patch `#:width` / `#:height` | patched if the size is a literal |
| text | patch the string literal | reported |

When a position is computed there is no number to rewrite, so the drag is
recorded as a correction on `at` instead:

```racket
(at left 60.0 #:tag "Box" #:nudge (list 160.0 40.0)
    (shape-pict #:width 100.0 #:height 40.0 ...))
```

The program's own layout logic is untouched, and the correction says plainly
that a hand adjustment was made. Being one argument rather than a wrapper is
deliberate: **a second drag updates those two numbers**, so corrections cannot
stack up the way nested pads do. Dragging the same element twice leaves exactly
one `#:nudge`, which the tests check.

A drag on `(at 57.6 158.4 #:tag "Rounded Rectangle 2" ...)` changes exactly that
one line and no other; `(at margin (+ top 20) ...)` is reported and left alone,
because computed layout is a decision the code is making on purpose.

### Editors, and how much they can be trusted

| app | native format | how the deck is read back | tested |
| --- | --- | --- | --- |
| PowerPoint | `.pptx` | it *is* the deck — nothing to do | no (needs macOS) |
| Keynote | `.key` | AppleScript `export ... as Microsoft PowerPoint` | no (needs macOS) |
| LibreOffice | `.pptx` | it is the deck | yes |
| `none` | — | you drive your editor yourself | yes |

PowerPoint is the easiest target, since its own format is the interchange
format. Keynote never saves `.pptx`, so the loop exports one out of it, and it
has no reload API — the document is closed and reopened, which loses the current
slide and selection.

Identity is layered: `descr` (alt text) and the shape `name`, then conservative
signature matching. LibreOffice preserves generated names, but drops `descr`
from groups and can drop it from shapes it reconstructs, which is why automatic
source identities are written to both fields. Shape ids are renumbered, so they
are never a key. Untagged raw drawing is matched by the deterministic names the
writer gives it, with kind and size checks so a newly inserted object is not
mistaken for one that was deleted.

## Animated slideshows

A `slideshow` program does not provide a list of slides; it *calls* `slide`, and
an animated pict is many frames. `--slideshow` runs it through the same entry
point `raco slideshow --pdf` uses:

```console
$ raco glide export --slideshow talk.rhm -o talk.pptx
talk.pptx  (291 slides)
```

One slide per *advance*, not per animation frame. That is not an option: an
animated pict is many frames between advances, and on a real 25-minute Rhombus
talk the difference is 291 slides in 69 s and 3.5 MB against 4869 slides in
737 s and 135 MB. 291 is exactly the page count of that talk's own
`talk-backup.pdf`, so this agrees with what slideshow itself considers a slide. Across those 291
slides the exported deck renders at 2.36% mean error against our own render of
the same picts, median 2.21%, with 4 slides above 5%.

A slideshow talk builds picts with `pict` rather than this runtime, so it takes
the flattened path: shapes and text come out as real PowerPoint objects, but text
does not reflow, and a font given only as a *family* is exported as the
conventional face for it (Arial, Times New Roman, Courier New) since the generic
names a family resolves to locally mean nothing to PowerPoint. Not yet handled
there: `start-alpha`/`end-alpha` grouping, arbitrary clipping, and sheared text,
all of which are reported.

## How verification works

`verify` converts the deck to PDF with headless LibreOffice, renders our own
PDF, rasterizes both with the same rasterizer at the same resolution, and
compares pages pixel by pixel. It reports two numbers per page:

- **mean err** — average absolute channel difference over the page.
- **pixels off** — share of pixels differing by more than 25%, which is the
  number that tracks "something moved" rather than "text is antialiased
  differently".

It also writes, per page, a diff image (grey where the two agree, red where they
do not) and a montage stacking reference, ours, and the diff.

### The representation survives, field by field

Pixels are a blunt instrument: a colour reverting to black on a small shape, a
rotation lost on something square, or a run's boldness dropped can all hide
under 0.3%. `tests/structural.rkt` imports, exports, imports again, and compares
the two intermediate representations directly -- kind, geometry, text and paint
for every named element. Across the fixtures it reports **0 differences**.

That is only worth anything if it can detect loss, so it was checked against
deliberate breakage. Removing rotation from the writer produces

```
slide 2: "Rectangle 3" geometry (576 64.8 216 100.8 20) -> (576 64.8 216 100.8 0)
```

and removing fill colours names all seven affected shapes with their before and
after values.

### It converges rather than drifts

Round-tripping repeatedly does not accumulate error. Measured over five
generations of deck -> program -> deck:

| | |
| --- | --- |
| exported slide XML | **byte-identical from generation 1 onward** |
| generated program | a fixed point from generation 2, modulo the input filename |
| pixel error against the original | **0.182% at every generation** |

The residual is a one-time conversion difference, and it is not arithmetic: EMU
is a 12700th of a point and generated source rounds to a thousandth, about
0.0001pt, or 0.00013px at 96 dpi. It is **text laid out twice by two engines** --
our renderer measures with cairo to decide a box, and then PowerPoint re-lays the
paragraphs inside it. On a real talk with 203 text bodies, small disagreements
about line breaking, ascent and autofit scale account for nearly all of it.

Exact equality is not the goal and is not reachable. LibreOffice is itself an
approximation of PowerPoint, and two text engines will never agree to the pixel.
The current fixtures land between 0.2% and 1.5% mean error; `tests/fidelity.rkt`
pins a per-deck budget just above that, so a layout regression trips it while
rasterizer noise does not.

Font substitution is what makes the comparison meaningful at all: the fixtures
ask for Calibri, and both sides get Carlito through fontconfig. Install
`fonts-crosextra-carlito` and `fonts-crosextra-caladea` alongside
`fonts-liberation`, or the reference will use a fallback with different metrics
and every number above will be noise.

## What is handled

- **Package** — OPC container, relationship resolution, media extraction.
- **Inheritance** — a placeholder's geometry and text properties resolved
  through slide → layout → master → theme, including the master's `txStyles`
  and the presentation's `defaultTextStyle`. Most real slides state almost
  nothing on the shape itself, so this is the difference between usable output
  and garbage.
- **Theme** — color scheme through the master's `clrMap`, the `phClr`
  indirection inside the format scheme, `tint`/`shade`/`lumMod`/`lumOff`/
  `satMod`/`alpha` transforms, major and minor fonts, and `fillRef`/`lnRef`
  style references.
- **Shapes** — 53 preset geometries drawn exactly, custom geometry paths,
  solid/gradient/pattern/image fills (a picture used as a fill keeps its file
  rather than its pixels, in both directions), outlines with dash and cap, rotation,
  flips, groups with their own child coordinate space, connectors, tables.
- **Text** — runs with family, size, weight, slant, underline, strike, color,
  letter spacing, caps and super/subscript; paragraph alignment, indents,
  hanging bullets (character and auto-numbered), line spacing, space before and
  after; word wrap with in-word breaking, vertical anchoring, and PowerPoint's
  cached autofit scale.
- **Slides** — background fills, and the layout's and master's own graphics
  drawn behind the slide, kept separate from the slide's own shapes.

## What is not handled yet

- Charts and SmartArt (`graphicFrame` content other than tables) draw as an
  empty box and are reported.
- Effects: shadows, glow, reflection, 3-D, soft edges.
- Animations and transitions — deliberately, since they are the part that
  belongs in code rather than in the file.
- Vertical and rotated-90 text layout (`bodyPr` `vert`), and right-to-left runs.
- Justified text falls back to left alignment.

## Layout

```
glide-pptx/
  units.rkt        EMU, hundredths of a point, 60000ths of a degree
  xml-util.rkt     namespace-tolerant xexpr queries
  opc.rkt          zip container and relationship resolution
  ir.rkt           the intermediate representation, in points
  theme.rkt        color scheme, color map, transforms, format scheme
  drawing.rkt      fills, outlines, transforms
  text.rkt         text bodies and property inheritance
  shapes.rkt       shape tree to IR
  parse.rkt        pptx to deck IR
  geometry.rkt     preset shape names to drawing paths
  runtime.rkt      IR to picts: text layout, paint, PDF output
  draw-ir.rkt      the display list: a model of drawing, not of PowerPoint
  record-adapt.rkt record-dc% datum to display list; the only module that
                   knows the datum's shape
  pptx-write.rkt   display list to DrawingML and an OPC package
  export.rkt       picts->pptx, the mirror of picts->pdf
  runtime.rhm      Rhombus naming over the same runtime
  source-tag.rkt   hidden editor identities derived from source locations
  render.rkt       IR to picts, via the runtime
  emit-common.rkt  what to emit, and the pretty printer
  emit-rhombus.rkt Rhombus surface syntax
  verify.rkt       LibreOffice reference, rasterizing, image diffing
  tagged.rkt       a pict subtype carrying how it was built
  semantic.rkt     that structure to display-list items that keep their meaning
  sync-state.rkt   the vocabulary a program and a .pptx are compared in
  sync.rkt         three-way merge and source patching at syntax locations
  watch.rkt        the parent process, and the per-app adapters
  main.rkt         command line
tools/
  make-test-decks.py   generates the fixtures with python-pptx
tests/
  unit.rkt        units, xml, relationships, theme colors, geometry
  roundtrip.rkt   emitted programs reproduce the direct render
  fidelity.rkt    per-deck budgets against LibreOffice
  export.rkt      exported .pptx matches the picts it came from
  flatten.rkt     an unsyncable element is one draggable object, and syncs
  structural.rkt  the IR survives a round trip, field by field
  deck-edit.rkt   editing a .pptx from Racket, to simulate a drag
  corpus.rkt      the whole pipeline over several hundred real decks
  sync.rkt        a drag comes back as a literal, and computed layout does not
  watch.rkt       the loop, driven from both sides
```

The direct renderer and both emitters go through one runtime, so a fidelity fix
lands everywhere at once and `tests/roundtrip.rkt` can demand that a generated
program draw exactly what the renderer draws.

## Building from source

Racket and Rhombus, both from source:

```console
git clone --depth 1 https://github.com/racket/racket.git
cd racket && make base            # builds Chez Scheme, then Racket CS
export PATH=$PWD/racket/bin:$PATH
raco pkg install --auto pict-lib

git clone --depth 1 https://github.com/racket/rhombus.git
cd rhombus && raco pkg install --auto \
  ./enforest-lib ./shrubbery-lib ./rhombus-lib \
  ./rhombus-draw-lib ./rhombus-pict-lib ./rhombus-slideshow-lib
```

Then this package, and the tools verification needs:

```console
raco pkg install --auto --link /path/to/glide-pptx
sudo apt-get install libreoffice-impress python3-uno poppler-utils \
  fonts-liberation fonts-crosextra-carlito fonts-crosextra-caladea
```

`racket/draw` needs system cairo, pango and fontconfig; on Debian and Ubuntu
that is `libcairo2 libpango1.0-dev libgdk-pixbuf-2.0-0`.

Regenerating the fixtures needs `python-pptx`:

```console
python3 -m venv .venv && .venv/bin/pip install python-pptx
.venv/bin/python tools/make-test-decks.py
```

## Tests

```console
raco test tests/fast.rkt     # ~90s; includes real LibreOffice edits when available
raco test tests/slow.rkt     # ~4min, renders through LibreOffice and sweeps the corpus
raco test tests/all.rkt      # both
```

When LibreOffice is installed, `fast.rkt` also drives actual saves, edits,
copies, insertions, and a live UNO reload; without it those editor-specific
checks say that they were skipped.

To compare a source talk's settled epoch pictures with the editable deck as
LibreOffice renders it, include the talk in the slow suite:

```console
GLIDE_TALK=/path/to/talk.rhm xvfb-run -a raco test tests/talk-source.rkt
```

The talk itself is not copied into this repository. The test exports one slide
per epoch with slide numbers, renders that deck through LibreOffice, and checks
every shown page against the source picture. `GLIDE_TALK_INK`,
`GLIDE_TALK_MAE`, and `GLIDE_TALK_BAD` override the default eight-percent
structural-ink, six-percent mean-error, and eight-percent differing-pixel
tolerances. Those defaults sit just above this talk's measured text-engine
drift. Ink within four cells of a 700-cell sampling grid (about eleven pixels
on a full-HD page) counts as the same stroke, which absorbs glyph baseline and
antialiasing differences; missing or displaced slide content is much larger.

The corresponding editability check works on a copied source tree and adds a
real LibreOffice text box on the final epoch of every logical slide:

```console
GLIDE_LO_PROGRAM=/path/to/talk.rhm GLIDE_LO_STAGES=1 \
  GLIDE_LO_PROGRAM_MODE=add xvfb-run -a raco test tests/lo-edit.rkt
```

The split is what a test needs, not how long it takes: `fast.rkt` is everything
with an exact answer -- the round trip through the IR, the sync, the scenarios,
the fuzzer, the parser's units -- and `slow.rkt` is everything that renders
through LibreOffice to compare against, or sweeps the corpus.
`tools/fetch-corpus.sh` downloads the decks; without them those modules say so
and pass.

`tests/actions.rkt` walks the editors' own surfaces -- PowerPoint's ribbon and
Format pane, Keynote's inspector -- one action at a time: bold, a bulleted
list, a dashed outline, a crop, a group, a deleted slide, a resized deck.
Fifty-odd of them, each asserted to land in one of four places: written,
refused with a reason, noted, or deliberately invisible. The fourth column is
why the file exists -- what must never happen is *silence*, the deck and the
program disagreeing with nobody told.

`tests/scenarios.rkt` is a session in the editor rather than one action: a
handful of edits at once, across several slides, then one merge. Where
`sync.rkt` says each edit *can* be written, a scenario says a run of them lands
together -- and it ends by comparing what the program renders to against what
the deck holds, element by element and property by property. Syncing again and
hearing nothing says much the same thing, since the base a merge writes is the
program as it then reads; what the comparison adds is *which* property differs,
and a view the merge does not have -- an element the program carries under a tag
the deck does not know is a disagreement here and no action there.

`tests/corpus.rkt` runs the whole pipeline -- import, render, draw, export,
re-import -- over LibreOffice's own pptx regression suite, several hundred decks
each aimed at one awkward corner of the format. It checks crash-freedom rather
than fidelity, since those files are deliberately strange, and it prints an
inventory of everything drawn approximately. The decks are not committed; fetch
them with

```console
$ tools/fetch-corpus.sh
399 decks present
```

and the test says so and passes when they are absent. It found two real bugs on
its first run: a custom path that opens with a line rather than a move, and a
picture whose relationship resolves to nothing.

The fuzzer and the structural test compare through one function,
`tests/ir-diff.rkt`, and what it compares is the point. It used to be an
element's box and its flips, which is how a line spacing of 1.5 was written out
as 1.0 for as long as the writer had been clamping percentages: the generator
was producing those spacings and nothing was looking at them. It now compares
every property a merge can see, and then the element itself field by field --
the geometry and its adjustments, a gradient's stops and angle, a pattern's
colours, the pattern a dash spells out, where each point of a custom path sits
across its shape, what a group holds, a table's shape. Three bugs fell out: the
clamp; `sysDash` read back as a plain dash, so every short dash came home long;
and a `<a:path>` that declares no space -- which means its coordinates are EMU
inside the shape, and which several real decks write -- floored to a space of 1
and so stretched by the shape's own size, twelve thousand times too large.

That last one a round trip cannot find on its own: our writer always states a
space, so reading one back never exercises the case. `structural.rkt` builds
the file that does -- a deck exported, its `w` and `h` stripped, read again --
and checks the path lands inside the shape it belongs to.

## Where this is going

The loop works: geometry and text merge back on literals, a computed position
gets a correction that cannot stack, every draggable object maps to something
nameable, and the representation is checked field by field across a round trip.
What is left, in the order it is worth doing.

**Shapes added or deleted in the editor.** The only structural gap remaining.
Both are detected and reported; neither is applied. Adding should insert a
generated `at` form at the right z-position, and deleting should comment code
out rather than remove it. Reordering is used as a matching signal but never
applied either. How aggressive to be with someone's source is a judgement call
more than a technical one.

**Patch the original package** instead of synthesizing one. A deck with charts or
SmartArt loses them on the first cycle today -- 62 of those in the corpus draw as
empty boxes -- and patching keeps them untouched across the loop even though we
cannot render them. This is what makes the round trip safe on a deck that uses
features we do not model.

**Three gaps that are reported rather than right**, all found in real files:

- `start-alpha`/`end-alpha`, the grouping ops that make nested transparency
  composite correctly. Ignored, so a `cellophane`d group is drawn at full
  strength.
- arbitrary clipping, which DrawingML cannot express. The clip is dropped where
  it should rasterize the clipped span.
- sheared text, same.

**Coalescing adjacent `draw-text`.** On the flattened path a line is one box per
drawn run. That path is now only reached by a one-way `--slideshow` export --
everything in the round trip either has a real text body or is a single picture
-- but for an exported talk it is the difference between editable paragraphs and
a mosaic.

**Smaller known losses:** `a:sym` (the symbol font) is read but not carried;
effects, vertical and rotated-90 text, right-to-left runs, and justified
alignment are unhandled.

**Testing the Keynote and PowerPoint adapters**, which needs a Mac. The
AppleScript is written against their APIs and has never run, and neither has the
`.key` path end to end: export, AppleScript import, drag, AppleScript export,
merge.

### One item that flattening removed

A pict composed inside an `at` used to need its own correction, since a drag on
one of several `vc-append` children has no `at` to record against. That case is
gone: such a subtree is now a single picture, so its pieces are not separately
draggable, and there is nothing to record a correction for. It comes back only
under `--no-flatten`.
