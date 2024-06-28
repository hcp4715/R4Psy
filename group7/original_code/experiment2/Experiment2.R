
###########################################################################
###                                                                     ###
###                              EXPERIMENT 2:                          ###
###                   CONTOUR PROPERTIES MANIPULATION                   ###
###                                                                     ###
###########################################################################

#--------------------------------Import Data-------------------------------------------------------------------------------------------
#Import half-split data
df2 <- read.csv("LD2.csv",stringsAsFactors=TRUE)

#--------Library--------------------------------------------------------------------------------------------------
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
#linear mixed-effects modelling
library(lme4)
library(lmerTest)
library(MuMIn)
library(lmtest)
#random forest model tools
library(randomForest)
library(randomForestExplainer)
#cohen's d
library(effectsize)
#Anova
library(car)
library(afex)

#--------Functions-------------------------------------------------------------------------------------------
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
  return(data_sum)}

#-------------------------Study 2: Summarize Data --------------------------------------------------------------------
#mean and sd for top and bottom-ranked contours
by(df2$response,factor(df2$condition),describe)

#-------------------------Study 2: Top vs Bottom Mean Diff---------------------------------------------------------------
#make dataframe for paired samples t-test
df2_paired <- df2 %>%
  dplyr::select(Imageidentity,stimulusitem1,response,condition) %>%
  dplyr::group_by(condition,Imageidentity)%>%
  dplyr::summarize(response = mean(response))
#paired t statistic
t.test(df2_paired$response[df2_paired$condition=="top"],df2_paired$response[df2_paired$condition=="bottom"],paired=TRUE)
#effect size (cohen's d)
effsize::cohen.d(df2_paired$response, factor(df2_paired$condition))

#-------------------------Study 2: Visualize Conditions-----------------------------------------------------------------
group.colors <- c(bottom = "steelblue", top = "darkred")
#dataframe summary
df2_summary <- data_summary(df2, varname="response", 
                            groupnames=c("condition", "category"))
#violin plot Figure 4C Left
ggp <- ggplot(df2_summary, aes(condition, response, fill = condition)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()

#line plot Figure 4C right
df2_mean <- df2 %>%
  dplyr::group_by(condition,category) %>%
  dplyr::summarise(response = mean(response),
                   sd = sd(response))
p<- ggplot(df2_mean, aes(x=category, y=response, group=condition)) + geom_line(aes(linetype=condition))+
  geom_point(aes(shape=condition)) 
p+labs(title="Study 2: Category and Condition", x="Category", y = "Response")+ theme_classic() + scale_fill_manual(values=group.colors)

#-------------------------Study 2: Linear Mixed Effects Model-------------------------------------------------------------------
mod_fac_slp_exp2 <- lmer(response ~ condition + DATscore + training_response+ PositiveAffectScore+
                           NegativeAffectScore + regionpast_response + gender_response +experience_response
                         + age_response + (1+condition|subject),
                         na.action = "na.exclude", data = df2,REML=T)
summary(mod_fac_slp_exp2)
anova(mod_fac_slp_exp2)
#confidence intervals
#confint(mod_fac_slp_exp2)
r.squaredGLMM(mod_fac_slp_exp2)


