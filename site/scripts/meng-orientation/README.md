# M.Eng. orientation deck

`site/public/meng/orientation.html` — Fall 2026 orientation for the MOVESAI,
CIVIC / CP Portal, and SAFECAST project teams.

## What it is

One self-contained HTML file, 84 KB. No build step, no dependencies, no
external assets. **20 main slides plus a 5-slide reference appendix**, at
1280×720, scaled to fit any viewport.

| Concern | How |
|---|---|
| Navigation | ← → / space / PageUp / PageDown, swipe, or the arrows |
| Jump anywhere | The dot rail at the bottom (diamonds = appendix) |
| Full contents | `T` |
| Print / PDF | `shift` + `P` — one slide per page, landscape, 25 pages |
| Deep link | The URL hash tracks the slide, e.g. `orientation.html#16` |

## Design

Same design language as the recruiting deck (`slides.html`) so the two read as
siblings — slate/ink palette, same stage, same card vocabulary. Each topic
carries its own accent and the progress bar picks it up.

**Type scale is deliberately large**: 17px body, 14px floor, 40px titles. This
is a deck to be read from the back of a room, not a document. Content is
fragments, not sentences — about 136 words per slide.

Adding a slide means adding a `<section class="slide">` with a `data-nav`
label; the contents list, dot rail and counter build themselves. Add
`data-apx` to put it in the appendix.

## Verify before you publish

Slides are a fixed 1280×720 box, so the failure modes are invisible in the
source: content silently clipped by a squeezed flex child, and text that
disappears against a dark card. Both have bitten this deck. Check by driving it
with Playwright rather than by reading the diff:

- **Overflow** — for every element inside `.pad`, flag `scrollHeight >
  clientHeight`, and flag any painted content below the pad's content box.
  Checking the pad alone misses clipping inside a flex child that was squeezed.
- **Contrast** — compute the WCAG ratio for every text node against its
  *composited* background. Semi-transparent and `color-mix()` backgrounds must
  be flattened first or the numbers are nonsense.
- **Dead space** — flag more than ~55px of unused height at the bottom.

The deck currently passes all three: no overflow, no dead space, zero contrast
failures.

## Publish

**GitHub Pages** is automatic — `site/public/**` is copied verbatim by the site
build, so merging to `main` publishes to
<https://timothyfraser.com/meng/orientation.html>.

**Posit Connect** is `deploy_orientation.R` here. Use it when the deck should
sit behind Connect, for access control or versioned rollback. Needs
`CONNECT_SERVER` and `CONNECT_API_KEY` from a **Publisher** account; a Viewer
key returns 403.

```bash
Rscript site/scripts/meng-orientation/deploy_orientation.R
```

## Two copies, and the drift risk

This deck also lives in the **movesai** repo at `meng/orientation.html`, which is
what deploys it to Posit Connect (that repo holds the Connect credentials).
**Nothing syncs the two.** Edit one, edit the other in the same sitting, or the
public page and the Connect page quietly diverge — the exact failure mode slide
24 of this deck is about. If that becomes annoying, collapse it: pick one
canonical copy and have the other mirror it.

## Keeping it true

Several slides cite live facts — acceptance-criterion counts, model accuracy,
cloud spend, metro counts, Safecast's dataset size and the state of their map
tiles. All were verified against the project repositories and public sources on
2026-09-11. When a number moves, update the slide. The deck makes rather a lot
of noise about verification, so it should hold itself to the same bar.
