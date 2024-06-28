# Computational Repeatability Test of the Results of the Kara Weisman (2021) Study

## Project Overview

This project aims to test the computational repeatability of the results presented in the study by Kara Weisman (2021). The purpose is to ensure that the findings can be reproduced using the provided data and code, adhering to best practices in research transparency and reliability.

We adopted the code from: <https://github.com/kgweisman/mental-life-culture-development>.

Weisman, K., Legare, C. H., Smith, R. E., Dzokoto, V. A., Aulino, F., Ng, E., ... & Luhrmann, T. M. (2021). Similarities and differences in concepts of mental life among adults and children in five cultures. *Nature Human Behaviour*, *5*(10), 1358-1368. (APA)

## Structure of the Repository

```plaintext
.
├── Doc_Re_Weisman_2021_Group1_2024.pdf
├── Report_Re_Weisman_2021_Group1_2024.pdf (PPT is too large;uploaded PDF instead.)
├── Script_Re_Weisman_2021_Group1_2024.Rmd
├── Group_1-r-references.bib
├── References
│   └── R_papaja_example.pdf
│ 
├── Supplementary_information
│   ├── Re_Supplementary_data
│   │   ├── fig_d_phi.png
│   │   ├── ...
│   │   └── plot02.png
│   ├── Script_Re_Weisman_2021_Group1_2024.ttt
│   ├── Group_1-r-references.bib
│   └── Script_Re_Weisman_2021_Group1_2024.tex
│   
├── Script_Re_Weisman_2021_Group1_2024_files
│   ├── Figure_latex
│   │   ├── alt fig 3-1.pdf
│   │   ├── ...
│   │   └── unnamed-chunk-20-1.png
│   ├── Repeatability_figures
│   │   ├── table1.png
│   │   ├── ...  
│   │   └── table18.png
│
├── data
│   ├── cong_df_adults_oblique.RDS
│   ├── ...
│   ├── d_ch_adults.csv
│   └── d_ch_children.csv
└── README.md    
```

- **Doc_Re_Weisman_2021_Group1_2024.pdf**: Document on repeatability for our group.
- **Report_Re_Weisman_2021_Group1_2024.pdf**: Presentation slides for the repeatability project of our group.
- **Script_Re_Weisman_2021_Group1_2024.Rmd**: Replication code for the main analysis and related code using R with the papaja package for repeatability document preparation.
- **Supplementary_information**: Includes analysis code, generated result images, and other information related to the main replication content in the paper's attachments, such as the .bib file for references.
- **Script_Re_Weisman_2021_Group1_2024_files**: Contains result figures generated from the Script_Re_Weisman_2021_Group1_2024.Rmd code and external images (see Repeatability_figures).
- **References**: Materials for learning and references related to the replication of R code.
- **data**: Contains all the necessary data for reproducing this project.

## Programming environment

All analyses were conducted in R (version 4.3.1); platform: arm64-apple-darwin; running under: macOS Sonoma 14.5.

## The analyses were built using the following packages

- tidyverse (version 2.0.0)
- lubridate (version 1.9.3)
- readxl (version 1.4.3)
- psych (version 2.3.9)
- cowplot (version 1.1.1)
- reshape2 (version 1.4.4)
- sjstats (version 0.19.0)
- lsa (version 0.73.3)
- langcog (version 0.1.9001; available at https://github.com/langcog/langcog-package)
- GPArotation (version 2024.3.1)
- dplyr (version 1.1.4)
- tidyr (version 1.3.0)
- ggplot2 (version 3.5.1)
- papaja (version 0.1.2)
- here (version 1.0.1)
- irr (version 0.84.1)
- kableExtra (version 1.4.0)
- janitor (version 2.2.0)
- knitr (version 1.45)

## Group Members

- **Shanshan Zhu**
  - Affiliation: Nanjing Normal University
  - Email: zhushanshan0717@gmail.com
  - Contributions: Data analysis
  
- **Lu Ao**
  - Affiliation: Nanjing Normal University
  - Contributions: PPT presentation
  
- **Mengyao Yang**
  - Affiliation: Nanjing Normal University
  - Contributions: Data analysis
  
- **Yueyang Yu**
  - Affiliation: Nanjing Normal University
  - Contributions: Make a PowerPoint
  
- **Huiling Zou**
  - Affiliation: Nanjing Normal University
  - Contributions: Summarize and organize