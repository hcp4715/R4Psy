# PROJECT KNOWLEDGE BASE

**Generated:** 2026-02-24 20:17:43 CST
**Commit:** 550749c
**Branch:** main

## OVERVIEW
Graduate-level R course materials (R for Psychology) with chapter slides in R Markdown, a single Quarto revealjs lecture, and pre-rendered HTML/PDF outputs.

## STRUCTURE
```
R4Psy/
├── chapter_*.Rmd            # Primary slide sources (xaringan)
├── chapter_*.html           # Rendered slide outputs
├── chapter_*.pptx           # Slide decks for early chapters
├── chapter_14.qmd           # Quarto revealjs lecture
├── Demo.Rmd                 # papaja demo manuscript
├── css/                     # Xaringan CSS themes
├── data/                    # Datasets + research folder templates
├── env/                     # Environment setup guide + screenshots
├── homeworks/               # Yearly homework folders
├── libs/                    # Xaringan/HTML dependencies (vendored)
├── output/                  # Generated outputs by chapter
├── picture/                 # Chapter images
├── chapter_13_guest_Du_XK/  # Guest lecture: network analysis
└── chapter_14_guest_Liu_Z/  # Guest lecture: meta-analysis
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Edit chapter slides | chapter_*.Rmd | Xaringan slides via `xaringan::moon_reader` |
| Render chapter slides | chapter_*.Rmd | Produces chapter_*.html in root |
| Quarto lecture | chapter_14.qmd | Revealjs, uses `theme.scss` |
| papaja manuscript | Demo.Rmd, chapter_13_papaja.Rmd | Renders to PDF/TeX |
| Data used in examples | data/match, data/penguin | Loaded via `here::here()` |
| Xaringan styling | css/Font_Style.css, css/zh-CN.css, css/Custumed_Style.css | Applied via YAML `css:` |
| Revealjs styling | theme.scss | Applied in `chapter_14.qmd` |
| Env setup guide | env/env_init.md | Screenshot-heavy tutorial |
| Guest network analysis | chapter_13_guest_Du_XK | `dag/`, `ggm/` scripts |
| Guest meta-analysis | chapter_14_guest_Liu_Z | `code/`, `datasheets/` |

## CONVENTIONS
- Slides use `output: xaringan::moon_reader` with `css: [default, css/... ]` and `lib_dir: libs`.
- Chapter code uses `here::here()` for data paths; avoid absolute paths.
- Chunk labels often include section numbers (e.g., `5.1`, `xaringan-panelset`).
- Packages loaded with `pacman::p_load(...)` or `library(...)`.

## ANTI-PATTERNS (THIS PROJECT)
- Do not hand-edit rendered artifacts (`chapter_*.html`, `*_files/`, `output/`); re-render from source instead.
- Do not edit `libs/` (vendored dependencies) unless intentionally updating generated outputs.
- Avoid absolute file paths in R code; use `here::here()` consistently.

## UNIQUE STYLES
- Xaringan CSS uses custom Chinese/English typography in `css/`.
- Quarto revealjs theme in `theme.scss` sets fonts/colors for Chapter 14.

## COMMANDS
```bash
Rscript -e "rmarkdown::render('chapter_3.Rmd')"
Rscript -e "rmarkdown::render('Demo.Rmd')"
quarto render chapter_14.qmd
```

## NOTES
- No CI/build automation; rendering is manual via RStudio or CLI.
- Repo includes many generated assets and large binaries.
- Shared helper functions are defined inline in chapters (e.g., `chapter_7.Rmd` for `convert_data_types`, `chapter_9_supplementary.Rmd` for datawizard-style helpers); there is no central utils file.
