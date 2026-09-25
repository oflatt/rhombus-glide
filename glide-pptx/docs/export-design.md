# Design: export to .pptx

Status: steps 1 to 7 below are **built** -- the display list, the writer,
`picts->pptx` with a `raco glide export` front end, and the export fidelity
test. Everything from step 5 on is still design. Every claim about `pict`,
`racket/draw` and `rhombus/pict` was probed against the installed versions.

Measured on the fixture decks, a `.pptx` exported from a generated program
reproduces the **original** deck to between **0.00% and 0.45%** mean error, two
pages of it pixel-identical -- better than our own renderer manages against the
same original, because the export hands PowerPoint back its own presets and text
bodies rather than our approximation of them.

## Requirement

Any pict program should export to PowerPoint and **look right**. Not just decks
this tool imported — arbitrary pict code, including programs that never heard of
this library. Animations are not expected to survive.

That is the same shape of problem as `picts->pdf`, which already works, so the
core API is its mirror:

```racket
(picts->pptx (list slide-1 slide-2) "out.pptx" #:width 960 #:height 540)
```

No cooperation required from the program: no tags, no wrapper functions, no
particular runtime.

## 1. `record-dc%` gives us the drawing model as data

The obvious implementation is a `dc<%>` that emits DrawingML, the way `pdf-dc%`
emits PDF. That is a sixty-method interface — and it turns out to be unnecessary,
because `racket/draw` already has a dc that records what it is asked to draw and
hands back an interpretable S-expression.

```racket
(define dc (new record-dc% [width 300] [height 200]))
(draw-pict p dc 10 20)
(send dc get-recorded-datum)
```

Probed output, abbreviated:

```racket
((do-set-brush! ((0 0 255 1.0) solid #f #f #f))
 (draw-rectangle 10.0 20.0 60 20)
 (draw-ellipse 20.0 46.0 40 30)
 (set-font (12 "Carlito" default normal normal #f default #t aligned #hash()))
 (draw-text "hello world" 13.0 82.0 #t 0 0)
 (draw-line 30.0 114.0 50.0 114.0))
```

The important part is that these are **high-level operations, not flattened
paths**. A rectangle is still a rectangle and text is still text with its font,
which is what makes the output real PowerPoint objects rather than path soup.

Everything else needed is captured too, each verified by probe:

- **Custom paths** — `(draw-path ((((0.0 . 0.0) (30.0 . 10.0) #(40.0 20.0 50.0 0.0)
  (60.0 . 10.0)))) 3 4 odd-even)`: subpaths as point lists, where a vector holds
  the two Bezier control points before the next point, plus the fill rule.
- **Transforms** — `(transform #(2 0 0 2 0 0))`, `set-origin`, `set-scale`,
  `set-rotation`, `set-initial-matrix`. Explicit ops, so the interpreter carries
  a current matrix and bakes it into emitted coordinates.
- **Bitmaps** — `draw-bitmap` embeds the ARGB bytes inline, with style, color and
  mask.
- **Gradients** — a brush records as
  `((0 0 0 1.0) solid #f (0 0 50 0 ((0 255 0 0 1.0) (1 0 0 255 1.0))) #f)`:
  endpoints plus stops.
- **Clipping** — `set-clipping-region` records the region as a path, with curves
  already flattened to Beziers.
- **Fonts** — `set-font` records size, face, family, style, weight, underline and
  `size-in-pixels?`.

So: **no `dc<%>` implementation, and no fork of Racket or Rhombus.**

### The mapping

| recorded op | DrawingML |
| --- | --- |
| `draw-rectangle` | `<p:sp>` with `prstGeom prst="rect"` |
| `draw-ellipse` | `prst="ellipse"` |
| `draw-rounded-rectangle` | `prst="roundRect"` with an adjustment |
| `draw-line`, `draw-lines`, `draw-polygon` | `prst="line"`, or `custGeom` |
| `draw-path` | `<a:custGeom>`: `moveTo`/`lnTo`/`cubicBezTo`/`close` |
| `draw-text` + font state | `<p:sp>` with a single-line `<a:txBody>`, `wrap="none"` |
| `draw-bitmap` | `<p:pic>` plus a PNG media part |
| `do-set-brush!` | `<a:solidFill>` or `<a:gradFill>` |
| `do-set-pen!` | `<a:ln w="EMU">` with dash and cap |
| `set-alpha` | `<a:alpha>` on the color |
| `transform` and friends | folded into a current matrix, baked into coordinates |

Two conversions worth writing down now, because both are easy to get wrong:

- `a:ln/@w` is **EMU**, not hundredths of a point. Same trap as the import side,
  in reverse.
- A recorded font size is in **device units**. Our slide unit is the point, so a
  size of 12 with `size-in-pixels? #t` is 12pt (`sz="1200"`); with
  `size-in-pixels? #f` the device size is `size * 96/72` and has to be converted
  back before writing `sz`.

### Where it is lossy, honestly

- **Text does not reflow.** Each `draw-text` becomes its own box, sized to the
  measured string and `wrap="none"`. It looks right and you can retype it, but it
  will not rewrap. Reflowable paragraphs need the semantic path below.
- **One box per `draw-text` call.** For ordinary pict code that is fine, because
  `(text "hello world")` is one call. Our own text renderer draws per word
  segment, so exporting our own output through this path would make one box per
  word. Two mitigations: coalesce adjacent `draw-text` ops sharing a baseline and
  font into one paragraph (worth doing generally, since it also fixes
  `hbl-append`ed text), and prefer the semantic path when it is available.
- **General clipping.** DrawingML has no arbitrary clip. A rectangular clip is
  intersected into the shape's geometry; a non-rectangular one falls back to
  rasterizing that span, reported rather than silent.
- **Shear.** A general affine cannot be expressed on a shape, but since
  `custGeom` coordinates are written explicitly we bake the matrix into the path,
  so geometry is fine. Sheared *text* cannot be, and rasterizes.
- **Shape count.** A busy pict becomes many shapes. Mitigated by coalescing runs
  of ops with identical state and by grouping per top-level slide element.

## 2. Optional upgrade: semantics for programs that have them

Programs generated by this tool call a known runtime, so we can do better than
the flattened form: one real text box with real paragraphs, and a named preset
shape instead of a path. That keeps imported decks editable and is what a
round trip back into the program would need.

This is an **upgrade layer, not the mechanism**. It applies when a pict was built
by our runtime and is skipped otherwise, so it can never be the reason an export
fails.

How the upgrade finds its elements: `pict` records a `children` tree of `child`
records carrying an affine placement, and all of `pict-children`, `child-pict`,
`child-dx`, `child-dy`, `child-sx`, `child-sy`, `child-sxy` and `child-syx` are
exported. Probing our runtime's leaves, recovery works through `pin-over`,
`scale` (uniform and not), `rotate`, the `*-append` family, `colorize`, `inset`,
`clip`, `frame`, `cellophane`, `scale-to-fit`, `refocus`, `panorama`, `ghost`,
`freeze` and nesting; only `launder` erases it, which is its documented purpose.
`child-dy` is measured bottom-up from the parent, which needs converting once.

The carrier is a `pict` subtype rather than a side table: the `pict` constructor
is exported (arity 8), a subtype survives composition, and the element becomes a
field of the value with no lifetime questions.

Rebuilding is less of a threat than it looks. Probed under `rhombus/pict`:

| case | tagged leaf |
| --- | --- |
| `stack`, `overlay`, `translate` | kept |
| `rebuildable` / `as_rebuildable` with the leaf as a dep | kept |
| `animate`, any epoch, leaf from enclosing scope | kept |
| `animate` whose proc constructs the element per epoch | fresh object, freshly tagged |
| `Pict.replace(leaf, other)` | substituted, as asked |
| `launder` | lost |

Identity is per construction and every construction goes through the runtime, so
whatever leaves exist in the snapshot being walked are tagged.

## 2.5 What level is this, and why not cairo

`record-dc%` records calls to **`racket/draw`'s `dc<%>` interface**, which sits a
level above cairo. Cairo would hand us path construction plus fill/stroke, and
glyph runs with positions. `dc<%>` hands us `draw-rectangle`, `draw-ellipse` and
`draw-text` with a font specification. That gap is the whole reason the output can
be real PowerPoint objects instead of freeform paths.

The difference is easy to see, because `racket/draw` also ships `svg-dc%`, which
goes through cairo's SVG surface. Rendering `(text "hello")` through it produces:

```xml
<g id="glyph-0-0"><path d="M 0.984375 0 L 0.984375 -9.71875 ..."/></g>
<g id="glyph-0-1"><path d="M 3.65625 -6.78125 C 4.0625 -6.78125 ..."/></g>
<use xlink:href="#glyph-0-0" x="26.5" y="48"/>
```

Five kilobytes of glyph outlines for one word: pixel-accurate and semantically
dead. No selectable text, no reflow, no font. The same drawing recorded at the dc
level stays `(draw-text "hello" 26.5 48.0 #t 0 0)` next to
`(set-font (14 "Carlito" ...))`.

So: dc-level, deliberately. Going lower would throw away exactly what we need.

## 2.6 Record and interpret, or a custom dc?

Emitting DrawingML "in the first place" means implementing `dc<%>` and writing
XML as the methods are called. It is the same interface either way, and the hard
part -- tracking pen, brush, font, matrix and clip state, converting paths,
measuring text, generating DrawingML -- is identical. The choice is only about
where the commands arrive from.

| | custom `dc<%>` | `record-dc%` then interpret |
| --- | --- | --- |
| interface to implement | ~60 methods | none |
| depends on | a documented public interface | the recorded datum's shape |
| stream transformations | awkward mid-stream | a pass over a list |
| testable as | rendered output | golden data files |

Coalescing adjacent text into one paragraph, dropping redundant state changes and
grouping shapes per element are where the output quality actually comes from, and
they are all list transformations. That argues for the datum.

The risk is that the datum's format is not a stability promise. So the datum is
normalized immediately into **our own display list**, and exactly one small
adapter module knows `record-dc%`'s shape. If it ever changes, or turns out to
lose something, the adapter is replaced by a `dc<%>` -- possibly a subclass of
`record-dc%`, to inherit its state management -- and no backend notices.

## 2.7 Layering, and other backends

```
  program (pict, rhombus/pict)
    |
    +-- semantic layer      only for programs built on our runtime
    |     elements, text bodies, preset shapes, tags
    |     -> pptx with reflowable text · HTML with real <text> · edit merge
    |
    +-- display list        from any pict at all
          -> pptx (flattened) · SVG DOM · canvas · TikZ, Typst, ...

  already free, no work needed:
    pdf-dc%   svg-dc%   post-script-dc%   bitmap-dc%
```

Two things follow.

**A pixel-accurate web target already exists.** `svg-dc%` renders any pict to SVG
today. What it does not give is a *document* -- text is outlines, per the probe
above. A web renderer worth having wants selectable, reflowable text, which is the
same thing the editable pptx wants. One semantic layer serves both, and pptx and
an SVG/DOM renderer are the two consumers that justify building the display list
at all.

**The display list should model drawing, not PowerPoint.** SVG and canvas support
arbitrary clipping and shear; DrawingML does not. If the IR is trimmed to what
pptx can express, every other backend inherits a limitation it does not have. So
the IR stays faithful and each backend degrades on its own terms -- pptx
rasterizing a non-rectangular clip, SVG simply not caring.

### Running in the browser instead

Racket has a portable-bytecode machine type (`pb`, which this build fetches to
bootstrap Chez), and that is the usual route to a WebAssembly Racket; cairo itself
compiles to WASM under Emscripten. If Racket and `racket/draw` run in the browser,
the *rendering* half of a web backend disappears -- draw the pict to a canvas with
the dc that already exists.

It does not make the semantic half disappear. Selectable text, real DOM nodes,
CSS and responsive layout still need to know that a run of glyphs is a paragraph,
which a dc cannot tell you. Worth checking the current state of Racket-on-WASM
before committing to either path.

## 3. Slides

`picts->pptx` takes the picts and writes the package: one master, one blank
layout, one theme, everything explicit on the slide. No placeholders and no
inheritance machinery, which also means our own importer can read back what we
write — the export is self-testing.

A `raco glide export program.rkt` front end can require the module and use a
provided `all-slides`, the same convention the generated programs already follow.
Hooking `slideshow`'s own `slide` function, so that `#lang slideshow` decks export
directly, is a later addition.

For a deck this tool imported, a second mode patches the **original** package
instead of synthesizing one: rewrite only the geometry and text of shapes we
know, leave every other part byte-identical. Masters, themes and fonts are
preserved, and anything we do not model — charts, SmartArt, shadows, transitions
— survives the round trip even though we cannot render it.

## 3.5 Why not go through SVG

`svg-dc%` exists, so SVG-to-pptx looks like a shortcut. It is not, for two
measured reasons.

**The SVG has already thrown away what we need.** Rendering a pict with two
filled rectangles and two strings through `svg-dc%` produces 24 `<path>`
elements, **zero** `<rect>`, **zero** `<text>`, 23 glyph symbols and 37 `<use>`
references. Cairo outlines every glyph and flattens every rectangle. Starting
from that, the best possible pptx is freeform paths with no text at all --
nothing to retype, nothing to merge, and a shape per glyph.

**There is no converter to reuse anyway.** LibreOffice will do it only with an
explicit filter, and the result is a 2.6 KB package containing *no slides*: it
imports the SVG as a Draw page and the Impress export drops the content. So the
"reuse an existing converter" argument evaporates, and writing SVG-to-DrawingML
ourselves would be strictly more work than display-list-to-DrawingML, starting
from strictly worse input.

SVG is a good *sibling* backend -- generated from our display list, with real
`<text>` -- not a step on the way to pptx.

## 3.6 Annotating shapes for the round trip

The plan was to attach a key to each exported shape. Measured against a
LibreOffice `pptx` to `pptx` rewrite, which stands in for "an editor opened and
saved the file", every carrier was destroyed:

```
BEFORE  <p:cNvPr id="2" name="Title 1" descr="glide-pptx:s1.title-1"/>
        <p:nvPr><p:ph type="ctrTitle"/>
                <p:custDataLst><p:tags r:id="rId99"/></p:custDataLst></p:nvPr>
AFTER   <p:cNvPr id="60" name="PlaceHolder 1"/>
        <p:nvPr><p:ph type="title"/></p:nvPr>
```

Shape ids renumbered, `name` replaced with "PlaceHolder 1", `descr` gone,
`custDataLst` and its tag part gone, `a:extLst` gone, and `ctrTitle` downgraded
to `title` with `idx` dropped.

**That result is narrower than it first appeared.** Repeating it on a deck *we*
exported -- plain shapes on a blank layout, nothing inherited -- every `name` and
every `descr` survived the same round trip intact. What LibreOffice destroys is
identity on **placeholder** shapes, which it re-derives from its own layout
model and renames. Since our export states everything explicitly and uses no
placeholders, it is on the good side of that line.

Ids are renumbered either way, so they are never a key.

An invisible run inside a shape's own text also survives, and travels with the
shape rather than sitting beside it:

```xml
<a:r><a:rPr sz="100"><a:noFill/></a:rPr><a:t>⸢pp:Rounded Rectangle 2⸣</a:t></a:r>
```

LibreOffice rewrote its fill to white at 1% alpha and kept the text. That is
worth having for an editor that strips `descr` -- Keynote, most likely -- and it
is only needed where a signature is genuinely ambiguous, so it can stay off by
default. A *separate* transparent marker element cannot work: it stops tracking
its shape the moment the shape is dragged.

So annotation is an optimization with a decent chance of working, and signature
matching is still the floor.

### Two flows, and only one of them needs annotation

**Deck-first.** A deck was imported. For a *single* pass no export is needed:
edit that same deck, re-import, diff against the stored IR, patch the program.
But the loop repeats -- edit the program, then the deck, then the program -- and
after the first program edit the deck is stale, so export is required here too.
See section 3.7.

**Code-first.** The slides were written in code, so there is nothing to edit but
an exported file.

Both flows need identification that survives a hostile editor.

### Identification, in order of preference

1. **Customer tags** (`p:custDataLst` to a `ppt/tags/tagN.xml` part) -- the
   mechanism PresentationML actually provides for third-party per-shape data.
2. **`descr`** (alt text) -- free, no extra parts.
3. **`name`** -- human-visible in the Selection Pane, so also self-documenting.
4. **Signature matching** -- needs no annotation at all, and is what makes the
   round trip work when the first three have been stripped.

### Signature matching

We know exactly what we exported, so matching edited shapes to exported ones is
an assignment problem over a few dozen items per slide:

```
cost(exported, edited) = infinity            if the kind differs
                       + w_text * text difference     (a strong signal)
                       + w_fill * color difference
                       + w_size * size difference
                       + w_z    * z-order difference
                       + w_pos  * position difference (weak -- this is what moved)
```

Position gets the *lowest* weight, because a shape moving is the thing we are
trying to detect. Text content is nearly unique per slide and does most of the
work. Solve it exactly -- even a cubic algorithm is free at this size -- then:
unmatched exported shapes were deleted, unmatched edited shapes were added, and
matched pairs with a geometry delta are the patch set. Every match carries a
confidence, and anything below threshold is reported rather than applied.

Two identical shapes that both moved are genuinely ambiguous. Say so; do not
guess.

### Testing it

Round-trip tests run through **LibreOffice** on purpose. It is the worst case we
can automate, and it strips everything: if merge works with zero surviving
annotations and renumbered ids, PowerPoint will not be the thing that breaks it.

## 3.7 Two-way sync, because the loop repeats

Editing the program and editing the PowerPoint alternate, so both sides drift
between syncs. That is not two one-way converters; it is synchronization, and the
only correct model for it is a **three-way merge against a stored base**.

```
    base            state at the last successful sync, on disk next to the program
      |
      +-- program-now   captured by running the program
      +-- pptx-now      parsed from the file the user just edited
```

Per element and per property:

| changed | result |
| --- | --- |
| program only | export writes it to the pptx |
| pptx only | recorded as the new value |
| both, to the same value | nothing to do |
| both, differently | conflict -- see the ownership rule below |

The base is a readable data file (`talk.sync.rktd`) holding, per slide and per
element: the identity key, kind, geometry, text, the style properties we sync,
z-order, **and the pre-override literal geometry** -- that last one is what makes
a program-side geometry edit detectable. It belongs in version control next to
the program, so a conflict is a diff a human can read.

Matching gets easier in this model, not harder. Neither side is matched against a
fresh export; both are matched against the base, which differs from each only by
the edits made since the last sync, and which carries the keys. Program side
matches by the source identity derived for each `at`; the pptx side uses that
identity from alt text if it survived, and signature matching otherwise. One
matcher, used on both sides.

### Ownership: PowerPoint owns geometry, code owns everything else

This is the rule that makes the loop stable, and it falls straight out of the
intent -- PowerPoint is the positioning tool.

- geometry conflict: the pptx wins, because you just dragged it
- text or content conflict: the program wins, unless `--take-text`
- structure -- a shape added or deleted in PowerPoint: reported, never applied
  silently; deleting code is not something a sync should do

With the two sides owning disjoint properties, the common cycle has no conflicts
at all. The one case that would silently lose work is editing a position literal
in the source while an override for it exists; that is why the base stores the
pre-override literal. If the literal moved, the user edited code on purpose, so
the override is stale and gets dropped.

### Where a merged edit is written

Every merge action is one of two small, reviewable source edits: **patch a
literal**, or **insert or update a known wrapper form**. Nothing restructures the
program.

| what changed | position is a literal | position is computed |
| --- | --- | --- |
| drag | patch the two numbers on `at` | insert or update a `nudge` wrapper |
| resize | patch `#:width` / `#:height` | reported for now |
| rotate | patch `#:rotate` | reported for now |
| text | patch the string literal | reported |

A computed position has no number to change. **Built:** the drag is recorded as a
`#:nudge` argument on `at`, which the runtime adds to the position. Being an
argument rather than a wrapper is the point -- a second drag rewrites those two
numbers, so corrections cannot nest. Measured: dragging twice leaves exactly one
`#:nudge`, accumulated to the right total, with the program's own computed
position untouched.

For a pict composed *inside* an `at` -- one of several children of a
`vc-append`, say -- there is no `at` to carry a correction, and there the wrapper
form is still the answer. A drag is expressible as a **local pad** that leaves
the surrounding layout alone:

```racket
;; move the drawing without changing the layout box
(define (nudge p dx dy) (inset p dx dy (- dx) (- dy)))
```

Measured on `(vc-append 5 a b)`, where `b` starts at x=15 in a 60x35 parent:

```
(inset b 20 0 0 0)      b at x=25   parent 60x35   <- box grew, vc-append
                                                      re-centered: moved only 10
(inset b 20 0 -20 0)    b at x=35   parent 60x35   <- moved exactly 20
```

The balanced form is the one to use; the naive positive inset is a trap, because
growing the box makes the enclosing combinator re-lay-out and the shape moves by
half what you asked. The drawing really moves, not just the finder: the recorded
`draw-rectangle` goes from `15 25` to `35 25`.

So a drag on a computed element rewrites `expr` to `(nudge expr 20.0 0.0)`, and a
second drag patches that wrapper's literals. **The system converges toward
patchability**: after one merge, a computed element is in the literal tier for
good.

Resize and rotation have analogues -- `scale-to-fit` for one, a `rotate` wrapper
for the other -- but rotation grows the bounding box and a scale changes text
size, so both need more thought than a drag does. Reported until then.

Text merging is deliberately literal-only. `(run* "Pipeline" ...)` can be
rewritten; `(run* (format "Slide ~a" n) ...)` cannot, and saying so is better than
any clever attempt.

`--overrides-only` remains for anyone who would rather no tool ever rewrote their
source: the value goes to a data file the runtime consults by tag instead. It is
the fallback, not the default, because the source staying true to what is drawn
is the whole point of generating readable code.

### What this means for scope

Export works for **any** pict program, through the display list. Two-way sync
does not: it needs a stable identity per element and somewhere to put the answer,
so it needs Rhombus source whose slide order and `at` sites can be recovered.
Ordinary `at` calls get identity from their source locations. A literal `~tag:`
is only the proxy escape hatch for an outer helper call whose one inner `at`
would otherwise be instantiated as several independently editable objects.

The running order is declared with `glide_slides`. The macro still produces an
ordinary `all_slides` list, but it also records the source owner behind each
presentation-only wrapper. In particular,
`in_section(0, slide_2)` is shown decorated and synchronized as `slide_2`, while
`divider(0)` is explicitly a generated page. Arbitrary computed entries are a
compile-time error, so an untraceable deck cannot appear to work until its first
editor save.

Patch-mode export also matters more here than it did for one pass. Across
repeated cycles a synthesized deck would replace the user's template every time;
patching the original package keeps the file *theirs*, with its master, theme and
anything we cannot render still intact.

## 3.8 Tagging on the pict side

There is no pict metadata field. The `pict` struct has exactly eight fields --
`draw`, `width`, `height`, `ascent`, `descent`, `children`, `panbox`, `last` --
none for user data, and no `tag-pict`/`find-tag` anywhere in the installed tree.

Two things that look like the missing feature, and why neither is:

- **`prop:pict-convertible`** (from `pict/convert`) does exist, and a struct
  carrying it can be handed to `vc-append` and friends. But it is a conversion
  protocol, not identity: after composing one, neither the struct **nor even its
  inner pict** appears anywhere in the resulting child tree. Conversion produces
  a fresh pict, so nothing survives to be found.
- **`Pict.configure(key, val)`** in `rhombus/pict` reads like a key/value store,
  but it only writes the key when the config already has it -- it adjusts
  configuration a pict declared, rather than accepting arbitrary user data.

So we make our own, and it takes three carriers because there are three different
jobs.

### 1. `at` source locations -- the sync identity

```rhombus
at(54.0, 167.75, ~name: "Title 1",
   textbox(~width: 612.0, ~height: 115.75, ...))
```

The Rhombus `at` macro captures this call's source location. Glide hashes the
file and character position into an opaque identity, writes it to the deck's alt
text, and recovers the same identity when it parses the source. The optional
`~name:` is only readable selection-pane metadata and is not part of identity.

`at` is therefore the **unit of sync**: the thing that has a position PowerPoint
is allowed to change. Elements composed together inside one `at` move as a unit,
which is the same bargain a PowerPoint group makes. If you want a piece
positioned independently, give it its own `at`.

One `at` used repeatedly on a single slide is one family and can only be edited
as a family. A source site may also be reused on several slides; it exports
normally, but a page-local edit is refused because one source change would
affect every occurrence. When a helper instead creates independently editable
objects at several outer call sites, each call states a distinct literal
`~tag:` and passes the tag and `~nudge:` through. A dynamic tag with no such
source site is rejected before an editing session starts.

### 2. A `pict` subtype -- the value tag, for capture

Capture has to find where an element ended up after arbitrary composition, so
the tag must travel on the value:

```racket
(struct tagged pict (tag element) #:transparent)

(define (with-tag p tag element)
  (tagged (pict-draw p) (pict-width p) (pict-height p)
          (pict-ascent p) (pict-descent p)
          (list (make-child p 0 0 1 1 0 0))
          (pict-panbox p) (pict-last p)
          tag element))
```

The `pict` constructor is exported (arity 8), so this works. Verified: the result
satisfies `pict?`, composes, and renders **once** -- reusing `pict-draw` and also
listing the original as a child does not double-draw, because `draw` is what
renders and `children` is only bookkeeping for finding and transforms. Two
rectangles produced exactly two draw operations.

It also means pict's own `lt-find` works on it, and agrees with our transform
walk to the digit, which makes a useful cross-check in tests.

Preferred over a weak `hasheq` side table: the tag is a field of the value, so
there is no lifetime question and nothing to keep in sync.

The leaf constructors (`shape-pict`, `textbox`, `image-pict`) attach this, and
`at` propagates its own tag down onto the value, so a placed element carries
both the source tag and the value tag.

### 3. `set_description` -- the Rhombus tag

Not `Pict.identity`. That is a *cross-slide substitution* key: `magic_move` and
`replace` deliberately give two picts on different slides the same identity to
say "this is the same shape, animate between them", so the same identity can
appear many times and it is not a per-element key.

`set_description` is, and its implementation is why:

```rhombus
method set_description(desc :: String) :~ Pict:
  _pad(0, 0, 0, 0, desc)
```

A fresh zero pad carrying the string, per call -- not a mutation of the shared
`PictIdentity`. So descriptions are per-element where identities are per-role.

While reading that file: `rhombus/pict` already has the nudge, and its comment
confirms the same trap measured in 3.7.

```rhombus
| // can't use `pad`, because we don't want to change the ascent or descent
  pin(this, ~on: blank(), ~at: Find.abs(dx, dy))
```

So the Rhombus side pairs up neatly with the Racket side: `set_description` where
we use a subtype, `Pict.translate` where we use a balanced `inset`.

### Scale is not size

A PowerPoint resize and a pict `scale` are different operations and must never be
merged into each other.

- **Resize** changes the shape's `ext`. Geometry stretches; **text size does not
  change**.
- **`scale`** multiplies everything, text included.

Two consequences. On the way *out*, a program that says `(scale tb 2)` has to
export with both the box and the font size doubled, or PowerPoint will not show
what the program draws -- so the emitted `sz` must carry the current matrix's
scale, not the run's nominal size. On the way *back*, a resize merges into
`#:width`/`#:height`, never into a scale factor: patching a scale would silently
change the font size, which is not what dragging a handle means.

When an element's size comes from a `scale` rather than from its own literals,
there is no width to patch, and that is the reported case rather than a guessed
one.

### Untagged elements

Plain pict code has neither tag. It exports -- the display list does not care --
and it does not sync, which is the same boundary as before.

Where we can do better than nothing: an untagged element still gets a key derived
from its slide and z-order, and the base stores its content signature, so the
same matcher used on the pptx side can follow it across an insertion. That gives
two tiers, worth naming honestly: **tagged is exact, untagged is best-effort.**
Code this tool generates is always in the exact tier, because the emitter always
writes a tag.

## 4. Animations

Not exported as animations: a deck is one slide per state.

For a `slideshow` program that is one slide per *advance*, which
`get-slides-as-picts` produces with condensing on. Without it the same talk came
out as 4869 slides rather than 291, because an animated pict is many frames
between advances -- the count matched the talk's own backup PDF exactly once
condensed.

`rhombus/pict`'s `snapshot(epoch)` makes a `--stages` mode cheap later: one slide
per animation step, which is how a deck with builds would be made presentable.
It is a different artifact from the round-trip export and should be kept separate.

## 5. Verification

The existing image-diff harness applies directly. Add:

- **`verify-export`** — our PDF of the picts against LibreOffice's render of the
  exported `.pptx`. This is the measurement of "do the elements look right", and
  it reuses the whole comparison pipeline, thresholds and diff images.
- **`verify-roundtrip`** — for imported decks, import, export, import again, and
  diff the two IRs structurally. Exact and fast, and it catches a dropped
  attribute that a pixel diff would wave through at 0.1%.

## 6. Build order

1. **Done** — `draw-ir.rkt` and `record-adapt.rkt`: the display list, matrix
   tracking, pen/brush/font state, path conversion, text metrics.
2. **Done** — `pptx-write.rkt`: DrawingML and the OPC package.
3. **Done** — `export.rkt` and `raco glide export`.
4. **Done** — `tests/export.rkt`, with per-deck budgets.
5. Quality passes: coalesce text runs, group per element, dedup state. Not done,
   and less pressing now that structured picts skip the flattened path entirely.
6. **Done** -- `tagged.rkt` and `semantic.rkt`: a pict carries how it was built,
   and exports as named presets with reflowable text, each carrying its tag.
7. **Done** -- `sync-state.rkt`, `sync.rkt` and `watch.rkt`: the base file, the
   matcher, the three-way merge with the ownership rule, source patching at
   syntax locations, and the parent process with per-app adapters.
8. Patch-the-original export mode, which is what makes repeated cycles keep the
   user's own template.
9. Source patching at syntax locations, then the overrides file, then `flatten`.

Steps 1 and 2 are the work; step 4 is what makes it believable. Everything from
5 on is refinement, and 6-7 only matter for decks that came from PowerPoint in
the first place.
