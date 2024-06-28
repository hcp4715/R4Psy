
###########################################################################
###                                                                     ###
###                       EXPERIMENT 3:                                 ###
###                    CONTOUR GROUPING MANIPULATION                    ###
###                                                                     ###
###########################################################################
#Import data
df3 <- read.csv("LD3.csv", stringsAsFactors=TRUE)
#Import random forest model residual data
cross <- read.csv("cross.csv")
#--------Library--------------------------------------------------------------------------------------------------
library(psych)
library(tidyverse)
#linear mixed-effects modelling
library(lme4)
library(lmerTest)
library(MuMIn)
library(lmtest)
#cohen's d
library(effectsize)
library(ppcor)

#-------------------------Study 3: Summarize Data-------------------------------------------------------------
by(df3$response,factor(df3$cond),describe)

#category information
df3 <- df3 %>% mutate(category = case_when(
  str_detect(.$stimulusitem1, "mountains_") ~ "mountains",
  str_detect(.$stimulusitem1, "city_") ~ "city",
  str_detect(.$stimulusitem1, "forests_") ~ "forests",
  str_detect(.$stimulusitem1, "beaches_") ~ "beaches",
  str_detect(.$stimulusitem1, "highways_") ~ "highways",
  str_detect(.$stimulusitem1, "offices_") ~ "offices",
  TRUE ~ as.character(.$stimulusitem1)))
df3$category <- as.factor(df3$category)
by(df3$response,factor(df3$category),describe)

#-------------------------Experiment 3: Anova (top vs bottom condition)----------------------------------------------------------------------------
interaction.plot(df3$cond,df3$manipulation,df3$response)
mod <- aov(response~cond+manipulation+cond:manipulation, data = df3)
anova(mod)
#mean comparisons
print(TukeyHSD(mod))
cohens_f(mod)

#-------------------------Experiment 3: Figure 7 C---------------------------------------------------------------
group.colors <- c(bottom = "steelblue", top = "darkred")
df3_upright <- df3 %>%
  dplyr::filter(manipulation=="upright")

df3_upright_mean <- df3_upright %>%
  dplyr::group_by(image,cond) %>%
  dplyr::summarise(mean_response = mean(response))

#upright condition
ggp <- ggplot(df3_upright_mean, aes(cond, mean_response, fill = cond)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()

#inverted condition
df3_inverted <- df3 %>%
  dplyr::filter(manipulation=="inverted")

df3_inverted_mean <- df3_inverted %>%
  dplyr::group_by(image,cond) %>%
  dplyr::summarise(mean_response = mean(response))
#plot
ggp <- ggplot(df3_inverted_mean, aes(cond, mean_response, fill = cond)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()

#-------------------------Experiment 3: Linear Mixed Effects Model-------------------------------------------------------------------
mod_fac_slp_exp3 <- lmer(response ~ cond + PositiveAffectScore+ CB + IAE + AA +
                           NegativeAffectScore + region + gender +
                           + age + (1+cond|subject),
                         na.action = "na.exclude", data = df3,REML=T)
summary(mod_fac_slp_exp3)
anova(mod_fac_slp_exp3)
#confint(mod_fac_slp_exp3)
r.squaredGLMM(mod_fac_slp_exp3)

#-------------------------Experiment 2 and 3 Comparison-------------------------------------------------------------------
## variance Partitioning

#Y column values are the predictions from intact line drawing model in experiment 1
#Y1 column values are predictions from experiment 2
#Y2 column values are predictions from experiment 3
mod <- lm(cross$Y ~ cross$Y1 + cross$Y2)
mod1 <- lm(cross$Y ~ cross$Y1)
mod2 <- lm(cross$Y ~ cross$Y2)

#partial correlation
r_partial <- pcor(cross)$estimate[3,c('Y1','Y2')]
r_partial^2
#semi-partial/part correlation
r_part <- spcor(cross)$estimate[3,c('Y1','Y2')]
r_part^2



