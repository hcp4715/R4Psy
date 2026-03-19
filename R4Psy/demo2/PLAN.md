# Demo2 Reproduction Plan (Hu et al., 2020)

## Goal
Reproduce the **confirmatory study** analyses from Hu et al. (2020) using R (ignore results that used HDDM), then reprocude the Methods/Results section using a `papaja` R Markdown document and render to PDF.

### Sections of the PDF
- Introduction: briefly introduce the original study and explain importance of reproducibility.
- Methods: 
  - Data preprocessing: based on the code used in data preprocessing
  - Analysis: based on the code used in there analysis
  - Visualization: replotting all the figures in the original paper
- Results: Report the results as in the original paper, with tables/figures. Importantly, include a table that comparing the results of the original study and the reproduced results.
- Discussion: summarize the key findings and discuss potential limitations of the reproduction.

## Data + Code Sources (downloaded)
- OSF node: https://osf.io/4zvkm/
- Analysis repository: https://github.com/hcp4715/moralSelf_ddm
- Supplementary document (downloaded): `demo2/reference/SOM_R_GoodMe_submission_v10.docx`
- Full external repo (cloned): `demo2/external/moralSelf_ddm/`

## Local Folder Layout
```
demo2/
├── README.md
├── PLAN.md
├── data/
│   ├── raw/         # optional copy of raw CSVs (see external repo)
│   └── processed/   # derived data used for figures/tables
├── scripts/         # wrapper scripts for reproduction workflow
├── papaja/          # manuscript Rmd + rendered PDF
├── reference/       # paper, supplementary materials
├── external/        # cloned upstream analysis repo
└── logs/            # render logs / sessionInfo
```

## Reproduction Steps (high level)
1) **Inventory inputs (confirmatory study only)**
   - Raw CSVs and preprocessing scripts in `demo2/external/moralSelf_ddm/2_confirm_study/Results/1_preproc/`.
   - Preprocessed outputs and JASP files in `demo2/external/moralSelf_ddm/2_confirm_study/Results/2_trad_analysis/`.
   - HDDM inputs/scripts in `demo2/external/moralSelf_ddm/2_confirm_study/Results/3_hddm/` (optional scope).

2) **Preprocessing (R)**
   - Run `MS_rep_preproc.r` to generate wide-format datasets used for analysis.
   - Save outputs into `demo2/data/processed/` (copy from external repo outputs for stable references).

3) **Primary analyses (R)**
   - Reproduce the reported statistics in R (mirroring JASP outputs where applicable).
   - Store key tables/figures in `demo2/data/processed/` or `demo2/papaja/figures/`.

4) **Papaja manuscript**
   - Create rmd file using `papaja`
   - Start from loading R package using `pacman::p_load()`
   - Using `here::here()` for all paths.
   - Using `pkg::funname()` for all non-base R functions.
   - Create `demo2/papaja/hu2020_demo2.Rmd` with Methods + Results.
   - Render to PDF and store in `demo2/papaja/`.

5) **Reproducibility logging**
   - Save `sessionInfo()` to `demo2/logs/session-info.txt` after successful render.

## Notes
- This demo is scoped to the confirmatory study only.
- If raw data are needed outside the external repo, copy the relevant CSVs into `demo2/data/raw/` to keep the demo self-contained.
