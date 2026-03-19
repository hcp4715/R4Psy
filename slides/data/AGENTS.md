# DATA KNOWLEDGE BASE

## OVERVIEW
Datasets and folder-structure templates used across course materials and exercises.

## STRUCTURE
```
data/
├── match/                               # Experimental match task data (.out, .csv)
├── penguin/                             # Penguin dataset used in examples
├── Template_Folder_System_Empirical_Chuan-Peng_Lab/
│   ├── 1_General_Protocol/
│   ├── 2_Study1_keyword1/
│   ├── 3_Study2_keyword2/
│   └── 4_Reports/
└── Template_Folder_System_Meta_Chuan-Peng_Lab/
    ├── 1_Protocol/
    ├── 2_Literature_Search/
    ├── 3_Article_Screen/
    ├── 4_Data_Extraction/
    ├── 5_Analysis/
    └── 6_Reports/
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Match-task examples | data/match | Used in multiple chapters via `here::here()` |
| Penguin examples | data/penguin | Used in data-import chapters |
| Empirical project template | data/Template_Folder_System_Empirical_Chuan-Peng_Lab/ReadMe.txt | Naming + folder guidance |
| Meta-analysis template | data/Template_Folder_System_Meta_Chuan-Peng_Lab/ReadMe.txt | Workflow + naming guidance |

## CONVENTIONS
- Keep dataset paths relative; course code expects `here::here('data', ...)`.
- Template folders include naming rules for project/study/files in ReadMe.txt.

## ANTI-PATTERNS
- Avoid renaming template folders or key datasets without updating all references.
- Do not move match/penguin datasets; many chapters assume current paths.
