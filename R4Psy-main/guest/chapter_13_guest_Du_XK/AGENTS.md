# GUEST LECTURE: NETWORK ANALYSIS (CH13)

## OVERVIEW
Guest lecture materials on network analysis (DAG and GGM) with scripts and PDF outputs.

## STRUCTURE
```
chapter_13_guest_Du_XK/
├── README.md              # Lecture overview
├── 网络分析.pdf            # Slide deck
├── Guest_lec_Xinkai_R.Rproj
├── dag/
│   ├── dag.R              # Bayesian network/DAG tutorial
│   └── data_dag.csv        # Example data for dag.R
└── ggm/
    ├── ggm.R              # GGM tutorial (bootnet/psychonetrics/BGGM)
    ├── bootnet.pdf        # Output plot
    ├── psychonetrics.pdf  # Output plot
    └── bggm.pdf            # Output plot
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Lecture overview | chapter_13_guest_Du_XK/README.md | Describes folder contents |
| DAG tutorial | chapter_13_guest_Du_XK/dag/dag.R | Uses `dag/data_dag.csv` |
| GGM tutorial | chapter_13_guest_Du_XK/ggm/ggm.R | Writes PDF plots under ggm/ |

## CONVENTIONS
- dag.R expects `dag/data_dag.csv` relative to the project root of this lecture.
- ggm.R writes output PDFs with filenames under `ggm/` (relative paths).

## ANTI-PATTERNS
- Do not move `dag/data_dag.csv` or rename `ggm/` outputs without updating scripts.
