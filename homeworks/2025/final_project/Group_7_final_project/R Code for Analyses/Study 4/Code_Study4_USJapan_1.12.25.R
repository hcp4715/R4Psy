# Rodriguez, Schertz, & Kross, 2025, Nature Communications
# Study 4
# Cross-Cultural Generalizability (U.S & Japan)
# Last Revised January 12, 2025

### Packages ####
library(psych)
library(sjPlot)
library(lme4)
library(emmeans)
library(knitr)
library(MuMIn)
library(magrittr)
library(psych)
library(jtools)
library(ltm)
library(effects)
library(generics)
library(lsr)    
library(reshape2)
library(corrplot)
library(nlme)
library(tidyverse)
library(Hmisc)
library(scales)
library(MASS)
library(nnet)
library(RColorBrewer)
library(psy)
library(QuantPsyc)
library(afex)
library(interactions)
library(effsize)
library(effectsize)
library(pwr)

### Import Data ####
mega <- read.csv("../Data/Study 4/Study4_Data_9.11.24.csv", header=TRUE, sep=",") # The "sample" column indicates which sample it is: 4=US Sample 1; 5=Japan Sample 1; 6=US Sample 2; 7=Japan Sample 2
table(mega$sample) # Pre-exclusions
mega <- mega[mega$exclude ==! 1,] 
table(mega$sample) # Number of participants per sample (after exclusions)

### Main MS: Beliefs by Culture ####
# Across all four samples
t.test(BeliefsTotal ~ JapanOrUS, data=mega)
# Effect size
cohens_d_result <- cohen.d(BeliefsTotal ~ JapanOrUS, data=mega)
cohens_d_result

### Main MS: Beliefs & Loneliness Correlation ####

# U.S.
USonly <- subset(mega, sample == 4 | sample == 6)
cor.test(USonly$BeliefsTotal, USonly$Lonely)
# Japan
Japanonly <- subset(mega, sample == 5 | sample == 7)
cor.test(Japanonly$BeliefsTotal, Japanonly$Lonely)

### Main MS: Mega-Analysis Across Countries ####

## Moderation Analysis
mega_interaction <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1|sample), data = mega)
summary(mega_interaction)
tab_model(mega_interaction)
confint(mega_interaction)

## Simple slopes
sim_slopes(mega_interaction, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

## Effect size
R2_full <- r.squaredGLMM(mega_interaction)[1, "R2m"]  # Marginal R^2
model_mega_reduced <- lm(Lonely ~ SocialContactReverse + BeliefsTotal, data = mega)
R2_reduced <- summary(model_mega_reduced)$r.squared
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Culture moderation
culture_mod <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal * JapanOrUS + (1|sample), data = mega)
summary(culture_mod)
confint(culture_mod)

## Covariates added
culture_mod_covs <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal * JapanOrUS  + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + Age + Gender + (1|sample), data = mega)
summary(culture_mod_covs)

## Plot Interaction

# Find 1 SD above/below
mean(mega$BeliefsTotal, na.rm=TRUE) #Mean = 5.33
sd(mega$BeliefsTotal, na.rm=TRUE) # SD = 1.16
SD1_above <- mean(mega$BeliefsTotal, na.rm=TRUE) + sd(mega$BeliefsTotal, na.rm=TRUE)
SD1_below <- mean(mega$BeliefsTotal, na.rm=TRUE) - sd(mega$BeliefsTotal, na.rm=TRUE)

# Model Estimates
emm_result_mega <- emmeans(mega_interaction, 
                      ~ BeliefsTotal * SocialContactReverse, 
                      at=list(BeliefsTotal = c(SD1_below, SD1_above)), 
                      cov.reduce = FALSE)
emm_df_mega <- as.data.frame(emm_result_mega)
emm_df <- emm_df_mega %>% 
  mutate(lower = emmean - SE, upper = emmean + SE)

# Plot
my_colors <- c("#B20000", "steelblue2")
p_mega <- ggplot(emm_df, aes(x = SocialContactReverse, y = emmean, group = factor(BeliefsTotal))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(BeliefsTotal)), alpha = 0.2) +
  scale_fill_manual(values = my_colors, aesthetics = c("fill","color")) +
  geom_line(aes(color = factor(BeliefsTotal))) +
  labs(title = "", y = "", x = "") +
  coord_cartesian(ylim = c(10,26)) +
  scale_y_continuous(labels = c(10,15,20,25), breaks = c(10,15,20,25)) +
  scale_x_continuous(labels = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),  
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black")) 
print(p_mega)

## Does culture moderate this interaction effect?
mega_3way_interaction <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal * JapanOrUS + (1|sample), data = mega)
summary(mega_3way_interaction) #three way int not significant; culture does not moderate the interaction

### Main MS: U.S Only ####

USonly <- subset(mega, sample == 4 | sample == 6) #subset data
nrow(USonly)

## Moderation Analysis
US_interaction <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1|sample), data = USonly)
summary(US_interaction)

## Effect size of interaction
US_interaction_reduced <- lmer(Lonely ~ SocialContactReverse + BeliefsTotal + (1|sample), data = USonly)
r2_full <- r.squaredGLMM(US_interaction)
r2_reduced <- r.squaredGLMM(US_interaction_reduced)
R2_full <- r2_full[1]
R2_reduced <- r2_reduced[1]
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Simple slopes
sim_slopes(US_interaction, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

## Plot Interaction

# Find 1 SD above/below
mean(USonly$BeliefsTotal, na.rm=TRUE) #Mean = 5.13
sd(USonly$BeliefsTotal, na.rm=TRUE) # SD = 1.23
SD1_above <- mean(USonly$BeliefsTotal, na.rm=TRUE) + sd(USonly$BeliefsTotal, na.rm=TRUE)
SD1_below <- mean(USonly$BeliefsTotal, na.rm=TRUE) - sd(USonly$BeliefsTotal, na.rm=TRUE)

# Model Estimates
emm_result_US <- emmeans(US_interaction, 
                         ~ BeliefsTotal * SocialContactReverse, 
                         at=list(BeliefsTotal = c(SD1_below, SD1_above)), 
                         cov.reduce = FALSE)
emm_df_US <- as.data.frame(emm_result_US)
emm_df <- emm_df_US %>% 
  mutate(lower = emmean - SE, upper = emmean + SE)

# Plot
my_colors <- c("#B20000", "steelblue2")
p_US_only <- ggplot(emm_df, aes(x = SocialContactReverse, y = emmean, group = factor(BeliefsTotal))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(BeliefsTotal)), alpha = 0.2) +
  scale_fill_manual(values = my_colors, aesthetics = c("fill","color")) +
  geom_line(aes(color = factor(BeliefsTotal))) +
  labs(title = "", y = "", x = "") +
  coord_cartesian(ylim = c(10,26)) +
  scale_y_continuous(labels = c(10,15,20,25), breaks = c(10,15,20,25)) +
  scale_x_continuous(labels = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),  
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black")) 
print(p_US_only)

### Main MS: Japan Only ####

JAonly <- subset(mega, sample == 5 | sample == 7) #subset data
nrow(JAonly)

## Moderation Analysis
JA_interaction <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1|sample), data = JAonly)
summary(JA_interaction)

## Effect size of interaction
JA_interaction_reduced <- lmer(Lonely ~ SocialContactReverse + BeliefsTotal + (1|sample), data = JAonly)
r2_full <- r.squaredGLMM(JA_interaction)
r2_reduced <- r.squaredGLMM(JA_interaction_reduced)
R2_full <- r2_full[1]
R2_reduced <- r2_reduced[1]
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Simple slopes
sim_slopes(JA_interaction, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

## Plot Interaction

# Find 1 SD above/below
mean(JAonly$BeliefsTotal, na.rm=TRUE) 
sd(JAonly$BeliefsTotal, na.rm=TRUE) 
SD1_above <- mean(JAonly$BeliefsTotal, na.rm=TRUE) + sd(JAonly$BeliefsTotal, na.rm=TRUE)
SD1_below <- mean(JAonly$BeliefsTotal, na.rm=TRUE) - sd(JAonly$BeliefsTotal, na.rm=TRUE)

# Model Estimates
emm_result_JA <- emmeans(JA_interaction, 
                         ~ BeliefsTotal * SocialContactReverse, 
                         at=list(BeliefsTotal = c(SD1_below, SD1_above)), 
                         cov.reduce = FALSE)
emm_df_JA <- as.data.frame(emm_result_JA)
emm_df <- emm_df_JA %>% 
  mutate(lower = emmean - SE, upper = emmean + SE)

# Plot
my_colors <- c("#B20000", "steelblue2")
p_JA_only <- ggplot(emm_df, aes(x = SocialContactReverse, y = emmean, group = factor(BeliefsTotal))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(BeliefsTotal)), alpha = 0.2) +
  scale_fill_manual(values = my_colors, aesthetics = c("fill","color")) +
  geom_line(aes(color = factor(BeliefsTotal))) +
  labs(title = "", y = "", x = "") +
  coord_cartesian(ylim = c(10,26)) +
  scale_y_continuous(labels = c(10,15,20,25), breaks = c(10,15,20,25)) +
  scale_x_continuous(labels = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),  
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black")) 
print(p_JA_only)

### Supplement: U.S. Sample 1 ####

## Subset Data
USsample1 <- mega[mega$sample == 4, ] 
nrow(USsample1)

## Descriptives
descrip_USsample1 <- USsample1[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age")]
describe(descrip_USsample1)

## Moderation Effect
model_USsample1 <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data = USsample1)
summary(model_USsample1) 
## Effect size
model_USsample1_reduced <- lm(Lonely ~ SocialContactReverse + BeliefsTotal, data = USsample1)
R2_full <- summary(model_USsample1)$r.squared
R2_reduced <- summary(model_USsample1_reduced)$r.squared
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

# Plotting Raw Values
quantiles <- quantile(USsample1$BeliefsTotal, probs = c(0.25, 0.75), na.rm = TRUE)
USsample1$BeliefsGroup <- cut(USsample1$BeliefsTotal, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))
USsample1_filtered <- subset(USsample1, BeliefsGroup != "Middle 50%")

ggplot(USsample1_filtered, aes(x = SocialContactReverse, y = Lonely, color = BeliefsGroup)) +
  geom_smooth(method = "lm", aes(group = BeliefsGroup, fill = BeliefsGroup), se = TRUE) +  
  labs(title = "Time Spent Alone and Loneliness",
       x = "Time Spent Alone",
       y = "Loneliness",
       color = "Beliefs Group",
       fill = "Beliefs Group") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:7) + 
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5)) +
  scale_color_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000")) +  
  scale_fill_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000"), guide = FALSE) + theme(
    axis.text.x = element_text(size = 18, color = "black"),  
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.title.x = element_text(size = 20, color = "black"),  
    axis.title.y = element_text(size = 20, color = "black"), 
    plot.title = element_text(color = "black", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

## Demographics
describe(USsample1$Age)
sd(USsample1$Age)
USsample1$fGender <- factor(USsample1$Gender, levels = c(1,2,3), labels = c("male", "female", "non-binary"))
table(USsample1$fGender)
USsample1$fRace <- factor(USsample1$Race, levels = c(1:7), labels = c("White", "Black", "Latino", "Asian", "NativeAmerican", "MiddleEastern", "Other"))
table(USsample1$fRace)
USsample1$fEducation <- factor(USsample1$Education, levels = c(1:6), labels = c("some HS", "HS", "some college", "bachelors", "masters", "doctorate"))
table(USsample1$fEducation)

## Cronbach Alpha: Beliefs about Being Alone
beliefs_data <- USsample1[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(beliefs_data)

## Cronbach Alpha: Loneliness
lonely_data <- USsample1[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(lonely_data)

## Duration
USsample1$Duration_Minutes <- USsample1$Duration..in.seconds./60
psych::describe(USsample1$Duration_Minutes)

## Correlations
colnames(USsample1)
USsample1_ForCorr <- USsample1[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age", "Gender")]
rcorr(as.matrix(USsample1_ForCorr)) #gives correlation coefficients and p values

### Supplement: Japan Sample 1 ####

## Subset Data
JAsample1 <- mega[mega$sample == 5, ]
nrow(JAsample1) 

## Descriptives
descrip_JAsample1 <- JAsample1[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age")]
describe(descrip_JAsample1)

## Moderation Effect
model_JAsample1 <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data = JAsample1)
summary(model_JAsample1)
## Effect size
model_JAsample1_reduced <- lm(Lonely ~ SocialContactReverse + BeliefsTotal, data = JAsample1)
R2_full <- summary(model_JAsample1)$r.squared
R2_reduced <- summary(model_JAsample1_reduced)$r.squared
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Simple Slopes
sim_slopes(model_JAsample1, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

# Plotting Raw Values
quantiles <- quantile(JAsample1$BeliefsTotal, probs = c(0.25, 0.75), na.rm = TRUE)
JAsample1$BeliefsGroup <- cut(JAsample1$BeliefsTotal, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))
JAsample1_filtered <- subset(JAsample1, BeliefsGroup != "Middle 50%")

ggplot(JAsample1_filtered, aes(x = SocialContactReverse, y = Lonely, color = BeliefsGroup)) +
  geom_smooth(method = "lm", aes(group = BeliefsGroup, fill = BeliefsGroup), se = TRUE) +  
  labs(title = "Time Spent Alone and Loneliness",
       x = "Time Spent Alone",
       y = "Loneliness",
       color = "Beliefs Group",
       fill = "Beliefs Group") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:7) + 
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5)) +
  scale_color_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000")) +  
  scale_fill_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000"), guide = FALSE) + theme(
    axis.text.x = element_text(size = 18, color = "black"),  
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.title.x = element_text(size = 20, color = "black"),  
    axis.title.y = element_text(size = 20, color = "black"), 
    plot.title = element_text(color = "black", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

## Demographics
describe(JAsample1$Age)
sd(JAsample1$Age, na.rm=TRUE)
JAsample1$fGender <- factor(JAsample1$Gender, levels = c(1,2,3), labels = c("male", "female", "non-binary"))
table(JAsample1$fGender)
JAsample1$fRace <- factor(JAsample1$Race, levels = c(1,2), labels = c("Japanese", "not Japanese"))
table(JAsample1$fRace)
JAsample1$fEducation <- factor(JAsample1$Education, levels = c(1:6), labels = c("some HS", "HS", "some college", "bachelors", "masters", "doctorate"))
table(JAsample1$fEducation)

## Cronbach Alpha: Beliefs about Being Alone
beliefs_JAsample1 <- JAsample1[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(beliefs_JAsample1)

## Cronbach Alpha: Loneliness
lonely_JAsample1 <- JAsample1[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(lonely_JAsample1)

## Duration
JAsample1$Duration_Minutes <- JAsample1$Duration..in.seconds./60
psych::describe(JAsample1$Duration_Minutes)

## Correlations
colnames(JAsample1)
JAsample1_ForCorr <- JAsample1[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age", "Gender")]
rcorr(as.matrix(JAsample1_ForCorr)) #gives correlation coefficients and p values

### Supplement: U.S. Sample 2 ####

## Subset Data
USsample2 <- mega[mega$sample == 6, ]
nrow(USsample2)

## Descriptives
descrip_USsample2 <- USsample2[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age")]
describe(descrip_USsample2)

## Moderation Effect
model_USsample2 <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data = USsample2)
summary(model_USsample2)
## Effect size
model_USsample2_reduced <- lm(Lonely ~ SocialContactReverse + BeliefsTotal, data = USsample2)
R2_full <- summary(model_USsample2)$r.squared
R2_reduced <- summary(model_USsample2_reduced)$r.squared
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Simple Slopes
sim_slopes(model_USsample2, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

# Plotting Raw Values
quantiles <- quantile(USsample2$BeliefsTotal, probs = c(0.25, 0.75), na.rm = TRUE)
USsample2$BeliefsGroup <- cut(USsample2$BeliefsTotal, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))
USsample2_filtered <- subset(USsample2, BeliefsGroup != "Middle 50%")

ggplot(USsample2_filtered, aes(x = SocialContactReverse, y = Lonely, color = BeliefsGroup)) +
  geom_smooth(method = "lm", aes(group = BeliefsGroup, fill = BeliefsGroup), se = TRUE) +  
  labs(title = "Time Spent Alone and Loneliness",
       x = "Time Spent Alone",
       y = "Loneliness",
       color = "Beliefs Group",
       fill = "Beliefs Group") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:7) + 
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5)) +
  scale_color_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000")) +  
  scale_fill_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000"), guide = FALSE) + theme(
    axis.text.x = element_text(size = 18, color = "black"),  
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.title.x = element_text(size = 20, color = "black"),  
    axis.title.y = element_text(size = 20, color = "black"), 
    plot.title = element_text(color = "black", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

## Demographics
describe(USsample2$Age)
sd(USsample2$Age)
USsample2$fGender <- factor(USsample2$Gender, levels = c(1,2,3), labels = c("male", "female", "non-binary"))
table(USsample2$fGender)
USsample2$fRace <- factor(USsample2$Race, levels = c(1:7), labels = c("White", "Black", "Latino", "Asian", "NativeAmerican", "MiddleEastern", "Other"))
table(USsample2$fRace)
USsample2$fEducation <- factor(USsample2$Education, levels = c(1:6), labels = c("some HS", "HS", "some college", "bachelors", "masters", "doctorate"))
table(USsample2$fEducation)

## Cronbach Alpha: Beliefs about Being Alone
beliefs_USsample2 <- USsample2[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(beliefs_USsample2)

## Cronbach Alpha: Loneliness
lonely_USsample2 <- USsample2[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(lonely_USsample2)

## Duration
USsample2$Duration_Minutes <- USsample2$Duration..in.seconds./60
psych::describe(USsample2$Duration_Minutes)

## Correlations
colnames(USsample2)
USsample2_ForCorr <- USsample2[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age", "Gender")]
rcorr(as.matrix(USsample2_ForCorr)) #gives correlation coefficients and p values

### Supplement: Japan Sample 2 ####

## Subset Data
JAsample2 <- mega[mega$sample == 7, ]
nrow(JAsample2)

## Descriptives
descrip_JAsample2 <- JAsample2[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age")]
describe(descrip_JAsample2)

## Moderation Effect
model_JAsample2 <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data = JAsample2)
summary(model_JAsample2)
## Effect size
model_JAsample2_reduced <- lm(Lonely ~ SocialContactReverse + BeliefsTotal, data = JAsample2)
R2_full <- summary(model_JAsample2)$r.squared
R2_reduced <- summary(model_JAsample2_reduced)$r.squared
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

## Simple Slopes
sim_slopes(model_JAsample2, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)

# Plotting Raw Values
quantiles <- quantile(JAsample2$BeliefsTotal, probs = c(0.25, 0.75), na.rm = TRUE)
JAsample2$BeliefsGroup <- cut(JAsample2$BeliefsTotal, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))
JAsample2_filtered <- subset(JAsample2, BeliefsGroup != "Middle 50%")

ggplot(JAsample2_filtered, aes(x = SocialContactReverse, y = Lonely, color = BeliefsGroup)) +
  geom_smooth(method = "lm", aes(group = BeliefsGroup, fill = BeliefsGroup), se = TRUE) +  
  labs(title = "Time Spent Alone and Loneliness",
       x = "Time Spent Alone",
       y = "Loneliness",
       color = "Beliefs Group",
       fill = "Beliefs Group") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:7) + 
  scale_y_continuous(limits = c(10, 30), breaks = seq(10, 30, by = 5)) +
  scale_color_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000")) +  
  scale_fill_manual(values = c("Top 25%" = "steelblue2", "Bottom 25%" = "#B20000"), guide = FALSE) + theme(
    axis.text.x = element_text(size = 18, color = "black"),  
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.title.x = element_text(size = 20, color = "black"),  
    axis.title.y = element_text(size = 20, color = "black"), 
    plot.title = element_text(color = "black", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

## Demographics
describe(JAsample2$Age)
sd(JAsample2$Age, na.rm=TRUE)
JAsample2$fGender <- factor(JAsample2$Gender, levels = c(1,2,3), labels = c("male", "female", "non-binary"))
table(JAsample2$fGender)
JAsample2$fRace <- factor(JAsample2$Race, levels = c(1,2), labels = c("Japanese", "not Japanese"))
table(JAsample2$fRace)
JAsample2$fEducation <- factor(JAsample2$Education, levels = c(1:6), labels = c("some HS", "HS", "some college", "bachelors", "masters", "doctorate"))
table(JAsample2$fEducation)

## Cronbach Alpha: Beliefs about Being Alone
beliefs_JAsample2 <- JAsample2[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(beliefs_JAsample2)

## Cronbach Alpha: Loneliness
lonely_JAsample2 <- JAsample2[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(lonely_JAsample2)

## Duration
JAsample2$Duration_Minutes <- JAsample2$Duration..in.seconds./60
psych::describe(JAsample2$Duration_Minutes)

## Correlations
colnames(JAsample2)
JAsample2_ForCorr <- JAsample2[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "MedExp", "Age", "Gender")]
rcorr(as.matrix(JAsample2_ForCorr)) #gives correlation coefficients and p values

### Supplement: Other Analyses (U.S/Japan Sample 1) ####
## Subset data
sample1 <- subset(mega, sample == 4 | sample == 5) #subset data

#### Cultural differences in variables of interest
# Beliefs
t.test(BeliefsTotal ~ JapanOrUS, data=sample1)

# Loneliness
t.test(Lonely ~ JapanOrUS, data=sample1)
## Add covariates
lm_cov_lonely <- lm(Lonely ~ JapanOrUS + BeliefsTotal + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + MedExp + Age + Gender, data=sample1)
summary(lm_cov_lonely) 

# Time spent alone
t.test(SocialContactReverse ~ JapanOrUS, data=sample1)
## Add covariates
lm_cov_timealone <- lm(SocialContactReverse ~ JapanOrUS + BeliefsTotal + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + MedExp + Age + Gender, data=sample1)
summary(lm_cov_timealone) 

# Media Exposure
t.test(MedExp ~ JapanOrUS, data=sample1)
## Add covariates
lm_cov_medexp <- lm(MedExp ~ JapanOrUS + BeliefsTotal + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + Age + Gender, data=sample1)
summary(lm_cov_medexp) 

#### Does Culture Moderate the Relationship between Time Alone on Loneliness?
lm_cultmod <- lm(Lonely ~ SocialContactReverse * JapanOrUS + BeliefsTotal + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + MedExp + Age + Gender, data=sample1)
summary(lm_cultmod) 

sim_slopes(lm_cultmod, pred=SocialContactReverse, modx=JapanOrUS, confint = TRUE)

### Supplement: Other Analyses (U.S/Japan Sample 2) ####
## Subset data
sample2 <- subset(mega, sample == 6 | sample == 7)

## Cultural differences in variables of interest

# Beliefs
t.test(BeliefsTotal ~ JapanOrUS, data=sample2)

# Loneliness
t.test(Lonely ~ JapanOrUS, data=sample2)

## Diffs in loneliness, controlling for rel mob and social contact
lonely_culture_diff <- lm(Lonely ~ JapanOrUS + RelMob + SocialContact, data=sample2)
summary(lonely_culture_diff)

## Do beliefs predict loneliness in both cultures?
# U.S.
cor.test(USsample2$BeliefsTotal, USsample2$Lonely) 
# Japan
cor.test(JAsample2$BeliefsTotal, JAsample2$Lonely) 

## Does the effect of beliefs on loneliness differ by culture?
lm1 <- lm(Lonely ~ BeliefsTotal * JapanOrUS, data=sample2)
summary(lm1) 
  
## Does media exposure predict beliefs or loneliness in both cultures?
# U.S.
cor.test(USsample2$MedExp, USsample2$BeliefsTotal) 
cor.test(USsample2$MedExp, USsample2$Lonely) 
# Japan
cor.test(JAsample2$MedExp, JAsample2$BeliefsTotal) 
cor.test(JAsample2$MedExp, JAsample2$Lonely) 

### Supplement: Sensitivity Analyses ####

# All samples
SensitivityResult_Mega_80 <- pwr.f2.test(u=2, v=1586-2-1, sig.level=0.05, power=0.80)
SensitivityResult_Mega_95 <- pwr.f2.test(u=2, v=1586-2-1, sig.level=0.05, power=0.95)

# U.S. Sample 1 (N = 244)
SensitivityResult_USsample1_80 <- pwr.f2.test(u=2, v=244-2-1, sig.level=0.05, power=0.80)
SensitivityResult_USsample1_90 <- pwr.f2.test(u=2, v=244-2-1, sig.level=0.05, power=0.90)
SensitivityResult_USsample1_95 <- pwr.f2.test(u=2, v=244-2-1, sig.level=0.05, power=0.95)

# Japan Sample 1 (N = 255)
SensitivityResult_JAsample1_60 <- pwr.f2.test(u=2, v=255-2-1, sig.level=0.05, power=0.60)
SensitivityResult_JAsample1_80 <- pwr.f2.test(u=2, v=255-2-1, sig.level=0.05, power=0.80)
SensitivityResult_JAsample1_90 <- pwr.f2.test(u=2, v=255-2-1, sig.level=0.05, power=0.90)
SensitivityResult_JAsample1_95 <- pwr.f2.test(u=2, v=255-2-1, sig.level=0.05, power=0.95)

# U.S. Sample 2 (N = 576)
SensitivityResult_USsample2_80 <- pwr.f2.test(u=2, v=576-2-1, sig.level=0.05, power=0.80)
SensitivityResult_USsample2_90 <- pwr.f2.test(u=2, v=576-2-1, sig.level=0.05, power=0.90)
SensitivityResult_USsample2_95 <- pwr.f2.test(u=2, v=576-2-1, sig.level=0.05, power=0.95)

# Japan Sample 2 (N = 511)
SensitivityResult_JAsample2_65 <- pwr.f2.test(u=2, v=511-2-1, sig.level=0.05, power=0.65)
SensitivityResult_JAsample2_80 <- pwr.f2.test(u=2, v=511-2-1, sig.level=0.05, power=0.80)
SensitivityResult_JAsample2_90 <- pwr.f2.test(u=2, v=511-2-1, sig.level=0.05, power=0.90)
SensitivityResult_JAsample2_95 <- pwr.f2.test(u=2, v=511-2-1, sig.level=0.05, power=0.95)

# U.S Only (Samples 1 and 2, N = 820)
SensitivityResult_USonly_80 <- pwr.f2.test(u=2, v=820-2-1, sig.level=0.05, power=0.80)

# Japan Only (Samples 1 and 2, N = 766)
SensitivityResult_Japanonly_80 <- pwr.f2.test(u=2, v=766-2-1, sig.level=0.05, power=0.80)

