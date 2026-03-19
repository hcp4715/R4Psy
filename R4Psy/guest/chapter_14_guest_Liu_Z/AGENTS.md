# GUEST LECTURE: META-ANALYSIS (CH14)

## OVERVIEW
Guest lecture materials on meta-analysis with an R Markdown workflow and sample dataset.

## STRUCTURE
```
chapter_14_guest_Liu_Z/
├── 元分析.pdf
├── code/
│   ├── formal_analysis.Rmd   # Meta-analysis workflow
│   └── formal_analysis.html  # Rendered output
└── datasheets/
    └── effect_size.csv       # Example data
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Analysis workflow | chapter_14_guest_Liu_Z/code/formal_analysis.Rmd | Reads datasheets via `../datasheets` |
| Input data | chapter_14_guest_Liu_Z/datasheets/effect_size.csv | 20-study example dataset |
| Slides | chapter_14_guest_Liu_Z/元分析.pdf | Lecture deck |

## CONVENTIONS
- formal_analysis.Rmd reads data via `read.csv("../datasheets/effect_size.csv")`.
- Output HTML is generated alongside the Rmd in `code/`.

## ANTI-PATTERNS
- Do not move datasheets or change relative paths without updating the Rmd.
