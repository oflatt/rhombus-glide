# Notes on reading .pptx

Things that cost time to work out, kept here so they only cost it once. Every
claim below was checked against real files and against LibreOffice's rendering
of them.

## Units are not consistent

| Where | Unit |
| --- | --- |
| `a:off/@x`, `a:ext/@cx`, `a:bodyPr/@lIns`, `a:pPr/@marL` | EMU (914400 per inch, 12700 per point) |
| **`a:ln/@w`** | **EMU** — not hundredths of a point |
| `a:rPr/@sz`, `a:rPr/@spc`, `a:spcPts/@val` | hundredths of a point |
| `a:xfrm/@rot`, `a:hslClr/@hue` | 60000ths of a degree, clockwise |
| `a:tint/@val`, `a:spcPct/@val`, `a:srcRect/@l` | 1000ths of a percent |

The line-width row is the trap. Every other `w`-ish attribute in the text world
is hundredths of a point, so reading `w="19050"` that way gives a 190-point
border instead of a 1.5-point one. It renders as solid color and looks like a
fill bug.

## Almost nothing is stated on the shape

A title on a stock layout is commonly this, in full:

```xml
<p:sp>
  <p:nvSpPr><p:cNvPr id="2" name="Title 1"/>
    <p:nvPr><p:ph type="ctrTitle"/></p:nvPr></p:nvSpPr>
  <p:spPr/>
  <p:txBody><a:bodyPr/><a:lstStyle/>
    <a:p><a:r><a:t>Some title</a:t></a:r></a:p></p:txBody>
</p:sp>
```

No position, no size, no font, no color, no alignment. All of it is inherited,
so a parser that reads only the slide produces 0x0 boxes of 18pt black text.
Character properties resolve through, most specific first:

1. `a:rPr` on the run
2. `a:pPr/a:defRPr` on the paragraph
3. `p:txBody/a:lstStyle/a:lvlNpPr/a:defRPr` on the slide shape
4. the same, on the matching placeholder in the slide **layout**
5. the same, on the matching placeholder in the slide **master**
6. the master's `p:txStyles`, picking `titleStyle`, `bodyStyle` or `otherStyle`
   by placeholder kind
7. the presentation's `p:defaultTextStyle` — this is the one non-placeholder
   text boxes land on, and where their default 18pt comes from

`N` is the paragraph's `lvl` plus one. Geometry follows a shorter version of the
same walk: shape, then layout placeholder, then master placeholder.

Matching a slide placeholder to a layout placeholder is by `idx`, which defaults
to 0 — except that a title matches by kind, because `title` and `ctrTitle` are
the same slot and several other shapes also carry index 0. Going from layout to
master, `ctrTitle` normalizes to `title`, and `subTitle`, `obj`, `tbl`, `chart`,
`dgm`, `pic` and friends all normalize to `body`.

## Colors go through two levels of indirection

`<a:schemeClr val="tx1"/>` does not name a color. It names a slot in the slide
master's `p:clrMap`:

```xml
<p:clrMap bg1="lt1" tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" .../>
```

which names an entry in the theme's `a:clrScheme`, which finally holds
something concrete — often `<a:sysClr val="windowText" lastClr="000000"/>`,
where the useful part is `lastClr`, the value the producer last saw.

Inside the theme's *format* scheme there is a third level: fills and lines there
are written against `<a:schemeClr val="phClr"/>`, a placeholder standing for
whatever color the referring shape's `<a:fillRef>` or `<a:lnRef>` supplied. So
resolving a color needs a context of (theme, color map, current `phClr`).

A shape with no fill element of its own is not unfilled — it means "ask my
`p:style`". Distinguishing "said nothing" from "said `<a:noFill/>`" matters, so
the fill and line parsers return a third answer, `'inherit`, for the first case.

## Text layout details that show up as a few points of drift

- **Space before is dropped for the first paragraph** of a text frame. Applying
  it puts every line 20% of a font size too low on a stock bulleted layout,
  which is where the master sets `<a:spcBef><a:spcPct val="20000"/>`.
- **A hanging bullet occupies the first-line indent.** `marL` is the left margin
  for every line and `indent` is the first line's offset from it, normally
  negative. Without a bullet the first line starts at `marL + indent`; with one,
  the bullet goes there and the text starts at `marL`.
- **Long words break mid-word.** A token wider than the box gets split at the
  last character that fits, as PowerPoint and LibreOffice both do. Without this
  a shape label like `RIGHT_TRIANGLE` runs out of its shape instead of wrapping.
- **`normAutofit/@fontScale`** is PowerPoint's cached shrink factor from the last
  time it laid the text out. Honoring it reproduces what the file was last seen
  to look like; ignoring it overflows every autofit box.
- **Rotation is about the shape's center**, clockwise, while `pict`'s `rotate` is
  counterclockwise and grows the bounding box — so the rotated pict has to be
  re-centered on the original center rather than pinned at the original corner.
- **A flip mirrors the geometry but not the text.** Applying the flip inside the
  path construction rather than as a device transform gets this right for free.

## racket/draw specifics

- **`pdf-dc%` scales drawing by 0.8 and insets it by a 16pt margin** unless
  `current-ps-setup` says otherwise. A slide has to land at exactly its stated
  size, so the dc is created under a `ps-setup%` with scaling 1.0 and zero
  margins. Left alone, everything is 80% size in the corner of the page.
- **Font sizes must be device units.** With `#:size-in-pixels? #f`, a "point" is
  a 96th of an inch, so a 20pt font comes out a third too large on a dc whose
  unit is the point. Pass `#:size-in-pixels? #t` and the size in points.
- **`get-text-extent` rounds to whole device units.** At slide font sizes that is
  a 1-2 point error in both advance width and line height. Measuring at 16 times
  the size and dividing recovers the real metrics: for 20pt Carlito it takes the
  measured advance from 253.0 to 246.1, against 246.4 from the same font in
  LibreOffice.
- **`draw-bitmap-section-smooth` is only on `bitmap-dc%`.** Scaling a source
  region onto a destination box on any dc goes through the dc's own
  transformation and plain `draw-bitmap-section`.
- **`pen%` caps width at 255.** Worth clamping, because exceeding it means an
  attribute was misread and a clamp keeps that visible instead of fatal.

## The XML reader is not namespace-aware

Racket's `xml` collection keeps qualified names verbatim, so a slide's offset
arrives as the symbol `a:off`. Matching on the local part keeps the parser
working across producers that bind DrawingML to another prefix — but it creates
one ambiguity worth knowing about:

```xml
<p:sldId id="256" r:id="rId7"/>
```

Both attributes have local name `id`. The relationship is the prefixed one, so
reading slide order needs an accessor that only matches a *qualified* name.
Getting this wrong silently yields zero slides.
