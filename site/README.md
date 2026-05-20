# timothyfraser.com — site/

The React + Vite source for **timothyfraser.com**. Built and deployed by
`.github/workflows/deploy-site.yml`. Updating the site is a CSV / JSON / MD
edit + push — no local toolchain needed.

## What you edit

Everything in `site/content/` is the edit surface:

| File | What it drives |
|---|---|
| `site.json` | Name, nav, social links |
| `metrics.json` | Hand-maintained citation count + h-index |
| `publications.csv` | Canonical publication list (mirrors `docs/cv/publications.csv` — see CI sync below) |
| `press.csv` | Press feed (mirrors `docs/cv/press.csv`) |
| `software.csv` | Software / dashboards / packages |
| `students.csv` | Current student teams |
| `sites.csv` | Research-site map points |
| `projects.json` | MEng + research project cards |
| `teaching.json` | Teaching resources + courses |
| `pages/*.md` | Long-form prose for Home / Research / Teaching |

## What you do NOT edit by hand

`site/content/derived/*.json` is regenerated from the files above by
`scripts/build-content.mjs`, which runs automatically before every dev /
build. It is committed so the site can be served without re-running the
script.

## Local dev

```bash
cd site
npm install
npm run dev
```

`predev` and `prebuild` hooks regenerate `content/derived/` every time.

## Build

```bash
npm run build      # → site/dist
npm run preview    # serves the build locally
```

## Deploy

`.github/workflows/deploy-site.yml` builds `site/` on every push to `main`
touching `site/**` or `docs/cv/**`, then assembles a Pages artifact:

- Vite output at the artifact root.
- Independently-rendered sub-paths from `docs/` (cv, sigma, netsci,
  research_statement, teaching_statement, teaching_portfolio,
  diversity_statement, images) are copied in so their URLs keep working.
- `CNAME` + `.nojekyll` ensured.

**One-time setup:** in repo Settings → Pages, set Source = "GitHub
Actions" (instead of "Deploy from a branch").

## AI enrichment (optional)

`scripts/ai-enrich.mjs` produces *suggestion* JSON
(`content/derived/suggested-tags.json`, `blurbs.json`) using the Anthropic
API. It is **key-gated**: without `ANTHROPIC_API_KEY` the script exits
silently and the site builds normally.

Trigger it manually via the `AI content enrichment (manual)` workflow,
which opens a PR with the suggestions for review. Nothing is ever merged
automatically.

## Adding a press mention

```bash
# Edit site/content/press.csv — add a row
git add site/content/press.csv
git commit -m "press: add Bloomberg piece on congestion pricing"
git push
```

A few minutes later the row is live on `/press`.

## Architecture

- **Vite + React + TypeScript**, plain CSS with tokens (see
  `src/design/`). No UI framework. No Tailwind.
- **d3** for the coauthor network, press timeline, and the stylized
  research-sites map. Map fallback panel is hand-drawn — for richer
  cartography swap in `d3-geo + topojson`.
- **BrowserRouter** + `404.html === index.html` for clean deep links on
  GitHub Pages.

## Editing the CV CSVs and keeping them in sync

The CV (`docs/cv/index.Rmd`) reads `docs/cv/publications.csv` and
`docs/cv/press.csv`. The CI workflow copies those two files into
`site/content/` before building. To single-source completely, point the
CV's `index.Rmd` at `../../site/content/publications.csv` and
`../../site/content/press.csv` and delete the copies in `docs/cv/`. This
repo currently keeps both copies for safety.
