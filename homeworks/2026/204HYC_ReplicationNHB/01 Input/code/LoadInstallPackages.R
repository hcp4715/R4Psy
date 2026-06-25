#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
# --------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                               -------------
#--------------------                    Load and install libraries                        -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                               Version 1                              -----------------------------------#  
#--------------------                               June 2024                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### load or install libraries %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

#------- cran_packages ---------
library(pacman)
rm(list=ls())


# Packages for importing, storing and manipulating data

p_load(here, devtools,                 # Packages from git an others
       tidyverse,                # Tidyverse for data wrangling, ggplot2 etc.
       broom,                    # Broom for tidy methods
       lubridate,                # For Date-time formats
       readxl,                   # Read write excel files
       officer                   # Manipulate office documents from Rmarkdown
       )

# Packages used for models and estimations

p_load(fixest,                    # Fast fixed-effect models
       estimatr,                  # Wrappers of models for treatment effects
       hdm,                       # High dimensional models : lasso and post-lasso from Chernozukov et al.
       grf,                       # Generalised random forest from Athey et al.
       fwildclusterboot,          # Fast wild cluster bootstrap from McKinnon et al (2023)
       glmnet,                    # Generalised linear model with regularization (lasso, ridge and elasticnets)
       nnet,                      # Neural network models and multinomial logit
       multcomp,                  # Multiple comparisons
       twang,                     # Functions for propensity score estimations and weightings + non-response analysis and weighting
       caret,
       randomizr
      )


# Package used for generating, formatting and exporting plots and tables

p_load(modelsummary,              # data and model summary tables and plots
       gtsummary,                 # other package for summary tables and regressions (balance test easy !)
       flextable,                 # Rmarkdown tables pretty good with word exports (and other formats)
       #kableExtra,               # Good package for LaTeX export (and html) but not word documents 
       viridis,                   # Viridis palettes
       RColorBrewer,              # Predifined palettes 
       gglgbtq,                   # color palettes with LGBTQIA flags
       ggh4x,                     # Hack for some ggplot functions (in particular, useful for adapting facets)
       cowplot, patchwork,        # Two packages to build figures from ggplot objects
       gridExtra,                 # additional functions for grids of graphics
       hrbrthemes,                # Additional thems for ggplot
       extrafont, extrafontdb,
       patchwork, 
       grid,
       ggpubr,                    # Nice graphical possibilities for publication ready plots
       sf,                        # Simple features for spatial data
      spData)                     # Spatial data



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

select <- dplyr::select
summarize <- dplyr::summarize


## Colorblind-friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


custom.col <- c( "#C4961A","#FFDB6D",
                 "#D16103", "#F4EDCA",  "#52854C",  "#C3D7A4", "#293352","#4E84C4")


# Main colors using LGBTQ colors #Lobby ;-) 

ColorD2 <- palette_lgbtq("bisexual")

ColorZ <- cbPalette[c(8,6)]
ColorD <- palette_lgbtq("transgender")[c(1,2)]

# Flextable defaults font, fontsize...
set_flextable_defaults(font.family = "Times New Roman", font.size = 11,
                       padding=3,
                       opts_pdf=list(tabcolsep=3))


# Register Times New Roman (replace with the appropriate font name if necessary)
#font_import(pattern = "Times New Roman")

#GGplot default theme
Maintheme <-   theme_bw()+  
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        plot.title=element_text(hjust = 0.5,margin=unit(c(0, 0, .6, 0), "cm"),size=12, 
                                face="bold",family = "Times New Roman"),
        plot.subtitle=element_text(hjust = 0.2,margin=unit(c(0, 0, .3, 0), "cm"),size=11),
        plot.caption = element_text(hjust = 0, size=10,family = "Times New Roman"),
        #axis.text.x = element_text(angle = 45),
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_text(angle=90),
        strip.background = element_rect(fill = "lightgrey"),
        plot.margin = unit(c(0, 0, 1, 0), "cm")
  )

theme_set(Maintheme)

SourcesStacked <- "stacked database of pairwise comparisons."
SourcesMain <- "endline database."


