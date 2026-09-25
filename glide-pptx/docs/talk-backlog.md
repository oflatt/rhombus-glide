# What one would want to edit in the eggcc talk

A backlog, drawn from `oflatt/talks` `2026-eggcc/talk.rhm` as it stands on the
`glide-editable-nodes` branch. Each row is an edit somebody would plausibly
make in LibreOffice and where it stands. **Works** means a scripted run made
that exact edit in the deck exported from the talk and the merge wrote it into
the talk's source; the runs live in `tests/talk-edits.rkt`.

The talk exports two decks, and which one an edit needs is part of the answer:

| mode | slides | what it shows |
| --- | --- | --- |
| default | 16 | one frame per entry in `all_slides` -- the settled frame |
| `--stages` | 82 | one slide per stage of every animated slide |

## The slides

| deck slide (default) | program | built by |
| --- | --- | --- |
| 1 | `slide_1` | its own `slide_canvas`: 3 texts, 11 pictures |
| 2, 6, 10, 12 | `divider(0..3)` | `outline_to_header`, drawn by code and lazy |
| 3, 5 | `slide_2` | canvas + 8 icon/label groups, "…and more!" |
| 4 | `slide_12` | helper-drawn e-graph: nodes, class regions, arrows |
| 7 | `slide_20` | code box, highlight bands |
| 8 | `slide_update` | code box + e-graph |
| 9 | `slide_egraph` | the big e-graph (130 elements settled, 166 at its widest) |
| 11 | `slide_rue` | CDF plot picture, legend texts, four `Line`s |
| 13-16 | `slide_34`, `slide_36..39` | bar-chart pictures + axis labels |

## Text

| # | edit | status |
| --- | --- | --- |
| 1 | retype slide 1's title | **works** |
| 2 | retype one paragraph of the author list | **works** |
| 3 | retype a section name in a divider | drawn by code -- edit the `sections` list |
| 4 | retype an icon label ("SQL" → "Datalog") | **works** (a group child) |
| 5 | retype "…and more!" | **works** |
| 6 | retype the "data flow" caption on slide 4 | drawn by code: it is an `arrow`'s `~label:` |
| 7 | retype a line of the code box (slide 7) | drawn by code: `code_pict` lays out picts |
| 8 | retype a chart's axis label | **works** |
| 9 | change a node's operator | not supported by decision: the word is shared by three `update`s |

## Position

| # | edit | status |
| --- | --- | --- |
| 10 | drag slide 1's title, authors, a logo | **works** |
| 11 | drag "…and more!" | **works** |
| 12 | drag a whole icon group | **works** |
| 13 | drag a chart picture, the CDF plot | **works** |
| 14 | drag an e-graph node | **works** (`place(...)` takes `~nudge:`) |
| 15 | drag an e-class region | **works** in `--stages` (`region(...)` takes `~nudge:`) |
| 16 | drag an arrow between nodes | drawn by code, and computed from the nodes it joins |
| 17 | drag a legend text | **works** |
| 18 | drag a `Line` on the plot slide | **works** |
| 19 | drag something on one stage only | **works** in `--stages` |
| 20 | drag a bar in a bar-chart icon | refused, and the talk's own doing: `with_icon`'s groups are tagged "Group (2)"/"Group (3)", which slide 3's icon groups are tagged too, so two `at` forms in the file share each name. The report now says so by name |
| 21 | drag the slide-number placeholder | **works** |

## Size, shape, colour

| # | edit | status |
| --- | --- | --- |
| 22 | resize a node, a logo, a chart picture | **works** |
| 23 | resize an e-class region | reported (**fixed this round**): the helper works the size out, so there is no size in the source -- it used to count as applied and write nothing but a correction of nothing. The drag part of the same edit is still written |
| 24 | rotate a node | **works** |
| 25 | change a shape's roundness (the `adj` handle) | **fixed this round** where the source states the adjustments -- `~geom: preset_geom("roundRect", [pair("adj", ...)])`, which is how the talk's blobs are written: the change is compared and written into that list. A shape written as `~shape: "roundRect"` states none, and stating none means the preset's defaults, which a deck writes out in full -- so reshaping *that* one still cannot be told from the defaults being spelled out |
| 26 | recolour a shape | reported "fill is not a literal here" wherever the colour lives in a helper or a shared name. A call that states `~fill:` itself is now writable |
| 27 | change a run's size or colour | **works** |
| 27a | recolour a chart label | **works** |

## Structure

| # | edit | status |
| --- | --- | --- |
| 28 | add a text box, on every slide | **works** (`tests/lo-edit.rkt`) |
| 29 | add a shape on one stage only | **works**: written as `from_stage(...)` round the slide's entry |
| 30 | delete an element | **works** for a placed one; a drawn one is reported |
| 30a | drag or resize something *inside* a group | **not seen at all** -- see "Open" below |
| 31 | delete a group child | **works** (the group's own box is rewritten with it) |
| 32 | duplicate an element | **works**, under a name of its own |
| 33 | copy an element to another slide | **works** (a removal and an addition) |
| 34 | group two elements; ungroup a group | **works** |
| 35 | bring an element to the front | reported on this talk: the slides whose z-order changed are drawn by code |
| 36 | delete a slide | **fixed this round** -- an entry that is a call (`in_section(3, slide_39)`) is now found by position |
| 37 | reorder slides | **fixed this round** -- entries are rewritten in place, whatever shape they have |
| 38 | paste a whole new slide | **fixed this round**, twice over: `all_slides` written as a block (`def all_slides:`) was not recognised as a list at all, so no slide could be added to it; and a one-line `export: all_slides` had the new name written under it as a statement of its own, which stopped the program running and took the whole save with it |

## Open, and the biggest one

**Moving something inside a group is not seen at all**, and worse, a child that
moves out of the group's own box reads as the *group* moving -- which, on a
slide whose group the merge can find, would move the whole group. On this talk
it is refused instead, because `with_icon`'s groups share their names with slide
3's; renaming them would expose the wrong edit, so that rename is deliberately
not made until this is fixed.

A group is compared by its box and by the words it holds, so a child dragged or
resized inside one leaves both sides agreeing and the save says there is nothing
to merge. Every icon on slide 3, the rules group on slide 4 and the bar charts
on slides 8-9 are groups, so this is a large part of the talk.

The way in is clear -- a group's `at` form holds an `at` for each child, which is
where such an edit belongs, and `group-retexts` already writes a child's *words*
that way -- but comparing where the children *are* needs a space both sides
agree on. Neither of the obvious two works: the child's own coordinates change
when a group is dragged, and its coordinates within the group's child space are
renormalised by a round trip, so 15 of the fuzzer's 150 random decks disagreed
with themselves. A tried branch is in this session's history; what it needs is
the group child-space question settled first (`group-children-boxes` scales by
`ext/chExt` while the parser hands children back unscaled, and one of the two is
wrong).

## Known limits to keep in view

- **Drawn by code is the whole story.** Every refusal above comes down to one
  thing: the element has no `at` form and no `tag(...)` naming it, so there is
  nowhere in the source to write. The way out is the annotation the e-graph rows
  and the class regions already use -- `~tag:` to name what a helper draws,
  `~nudge:` to take a correction -- and it now extends to `~fill:` and the other
  style arguments a call states for itself.
- The four divider slides are `magic_move` animations over computed layouts;
  nothing on them is addressable, and the text they show comes from the
  `sections` list.
- Elements that appear only in a later frame are in the default deck as of this
  round: the frame that settles is the one showing the most elements an edit
  could be written to, not the earliest of the frames tied on canvas tags.
