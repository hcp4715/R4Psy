
############################################################################
###                                                                      ###
###                               EXPERIMENT 1                           ###
###                                                                      ###
###                                                                      ###
############################################################################

#--------------------------------Import Data-------------------------------------------------------------------------------------------
#Import Intact Line Drawings Data
LD1 <- read.csv("LD1.csv",stringsAsFactors=TRUE)
#Import Color Photo Data
CP <- read.csv("CP.csv",,stringsAsFactors=TRUE)
#--------------------------------Load Library--------------------------------------------------------------------------------------------------
#basic tools
library(dplyr)
library(tidyverse)
#visualizations
library(ggplot2)
library(ggstatsplot)
library(ggpubr)
#data summary tools
library(summarytools)
library(psych)
#random forest model tools
library(randomForest)
library(randomForestExplainer)
#cohen's d
library(effectsize)
#Anova
library(car)
library(afex)

#-----------------------------------Functions-------------------------------------------------------------------------------------------
# Function to calculate the mean and the standard deviation for each group
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

############################################################################
###                                                                      ###
###                               STUDY 1:                               ###
###                         INTACT LINE DRAWINGS                         ###
###                                                                      ###
############################################################################

#-------------------------Study 1: Summarize LD Data ----------------------------------------------------------------------------
#Average ratings by category (raw scores)
by(LD1$pleasure_m,factor(LD1$category),describe)
#Average ratings by category (normalized scores)
by(LD1$Z_pleasure,factor(LD1$category),describe)

#-------------------------Study 1: Match LD with Color Photo Data-------------------------------------------------------------------
#combined color and LD dataset 
LD <- subset.data.frame(LD1, select = c("ImageName","pleasure_m"))
CPLD <- merge(x=CP,y=LD,by.x="imageName",by.y="ImageName",all.x=TRUE)
CPLD$pleasure_LD <- CPLD$pleasure_m
CPLD$pleasure_CP <- CPLD$mean_pleasure
library(reshape2)
CPLD_long <- melt(CPLD, id.vars=c("imageName"), 
                  measure.vars=c("pleasure_LD", "pleasure_CP"),
                  variable.name="ImageType",
                  value.name="response")
#add category information
CPLD_long <- CPLD_long %>% mutate(category = case_when(
  str_detect(.$imageName, "mountains_") ~ "mountains",
  str_detect(.$imageName, "city_") ~ "city",
  str_detect(.$imageName, "forests_") ~ "forests",
  str_detect(.$imageName, "beaches_") ~ "beaches",
  str_detect(.$imageName, "highways_") ~ "highways",
  str_detect(.$imageName, "offices_") ~ "offices",
  TRUE ~ as.character(.$imageName)))
CPLD_long$category <- as.factor(CPLD_long$category)

#add category information
CP <- CP %>% mutate(category = case_when(
  str_detect(.$imageName, "mountains_") ~ "mountains",
  str_detect(.$imageName, "city_") ~ "city",
  str_detect(.$imageName, "forests_") ~ "forests",
  str_detect(.$imageName, "beaches_") ~ "beaches",
  str_detect(.$imageName, "highways_") ~ "highways",
  str_detect(.$imageName, "offices_") ~ "offices",
  TRUE ~ as.character(.$imageName)))
CP$category <- as.factor(CP$category)

#-------------------------Study 1: Summarize ColorPhoto Data-------------------------------------------------------------------
#Average ratings by category (raw scores)
by(CP$mean_pleasure,factor(CP$category),describe)
#Average ratings by category (normalized scores)
by(CP$Z_pleasure,factor(CP$category),describe)

#-------------------------Study 1: Figure 3C -----------------------------------------------------------------
group.colors2 <- c(LD_pleasure = "steelblue", CP_pleasure = "darkred")
#dataframe summary
CPLD_summary <- data_summary(CPLD_long, varname="response", 
                            groupnames=c("ImageType", "category"))
#raw version of Figure 3C
p<- ggplot(CPLD_summary, aes(x=category, y=response, group=ImageType)) + 
  geom_line(aes(linetype=ImageType)) +
  geom_point(aes(shape=ImageType))
p+labs(title="Experiment 1: LD - CP", x="Category", y = "Response")+
  theme_classic() 
