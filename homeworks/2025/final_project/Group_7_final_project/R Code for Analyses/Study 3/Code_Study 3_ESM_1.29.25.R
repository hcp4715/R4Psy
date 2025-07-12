# Rodriguez, Schertz, & Kross, 2025, Nature Communications
# Study 3
# Do Beliefs About Being Alone Shape the Influence of Time Alone on Loneliness in Daily Life?
# Last Revised January 29, 2025

# Packages ####
library(knitr)
library(magrittr)
library(psych)
library(emmeans)
library(interactions)
library(sjPlot)
library(jtools)
library(effects)
library(reghelper)
library(generics)
library(reshape2)
library(corrplot)
library(nlme)
library(lme4)
library(tidyverse)
library(Hmisc)
library(scales)
library(corrplot)
library(lmerTest) 
library(MASS)
library(nnet)
library(RColorBrewer)
library(psy)
library(QuantPsyc)
library(afex)
library(EMAtools)  #不存在叫‘EMAtools’这个名称的程序包
set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'sans',   #To change the font type
          axis.title.size = 1.3,  #To change axis title size
          axis.textsize.x = 1,  #To change x axis text size
          axis.textsize.y = 1) 

# Import & Center Data ####
esm_full <- read.csv("../Data/Study 3/Study3_ESM_Deid_8.26.24.csv", header=TRUE, sep=",")
View(esm_full)

# Main MS: Main Beliefs Model ####
## Time spent alone and beliefs predict change in loneliness
model.beliefs <- lmer(lonely_change ~ C_time_alone * C_beliefs_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs)
confint(model.beliefs)

# Simple Slopes Analysis
sim_slopes(model.beliefs, pred = C_time_alone, modx = C_beliefs_total)

# For Fig, plotting raw values (uncentered)
# Find 1 SD above/below of Beliefs
mean(esm_full$beliefs_total, na.rm=TRUE) # Mean = 4.84
sd(esm_full$beliefs_total, na.rm=TRUE) # SD = 1.12
SD1_above <- mean(esm_full$beliefs_total, na.rm=TRUE) + sd(esm_full$beliefs_total, na.rm=TRUE)
SD1_below <- mean(esm_full$beliefs_total, na.rm=TRUE) - sd(esm_full$beliefs_total, na.rm=TRUE)

# Main MS: Other Emotions ####

### Negative affect - Significant
# Model
model.beliefs.NegAffectChange <- lmer(NegAffect_change ~ C_time_alone * C_beliefs_total + C_neg_mood_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.NegAffectChange)
confint(model.beliefs.NegAffectChange, method = "Wald")

# Simple Slopes Analysis
sim_slopes(model.beliefs.NegAffectChange, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes  这一行也报错
emm_result_NegAffect <- emmeans(model.beliefs.NegAffectChange, 
                      ~ beliefs_total * time_alone, 
                      at=list(beliefs_total = c(SD1_above, SD1_below)), 
                      cov.reduce = FALSE)
emm_result_NegAffect

### Bored - Significant
# Model
model.beliefs.BoredChange <- lmer(bored_change ~ C_time_alone * C_beliefs_total + C_bored_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.BoredChange)
confint(model.beliefs.BoredChange, method = "Wald")
# Simple Slopes Analysis
sim_slopes(model.beliefs.BoredChange, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes  这一行也报错
emm_result_Bored <- emmeans(model.beliefs.BoredChange, 
                            ~ beliefs_total * time_alone, 
                            at=list(beliefs_total = c(SD1_above, SD1_below)),
                            cov.reduce = FALSE)
emm_result_Bored 

### Stressed - Significant
# Model
model.beliefs.StressChange <- lmer(stressed_change ~ C_time_alone * C_beliefs_total + C_stressed_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.StressChange)
confint(model.beliefs.StressChange, method = "Wald")
# Simple Slopes Analysis
sim_slopes(model.beliefs.StressChange, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes 这一行也报错
emm_result_Stress <- emmeans(model.beliefs.StressChange, 
                             ~ beliefs_total * time_alone, 
                             at=list(beliefs_total = c(SD1_above, SD1_below)),
                             cov.reduce = FALSE)
emm_result_Stress 

### Positive affect - Significant
# Model
model.beliefs.PosAffect.change <- lmer(PosAffect_change ~ C_time_alone * C_beliefs_total + C_pos_mood_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.PosAffect.change)
confint(model.beliefs.PosAffect.change, method = "Wald")
# Simple Slopes Analysis
sim_slopes(model.beliefs.PosAffect.change, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes 这一行也报错
emm_result_PosAffect <- emmeans(model.beliefs.PosAffect.change, 
                                ~ beliefs_total * time_alone, 
                                at=list(beliefs_total = c(SD1_above, SD1_below)), 
                                cov.reduce = FALSE) 
emm_result_PosAffect 

### Content - Significant
# Model
model.beliefs.ContentChange <- lmer(content_change ~ C_time_alone * C_beliefs_total + C_content_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.ContentChange)
confint(model.beliefs.ContentChange, method = "Wald")
# Simple Slopes Analysis
sim_slopes(model.beliefs.ContentChange, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes 这一行也报错
emm_result_Content <- emmeans(model.beliefs.ContentChange, 
                      ~ beliefs_total * time_alone, 
                      at=list(beliefs_total = c(SD1_above, SD1_below)), 
                      cov.reduce = FALSE)
emm_result_Content 

### Grateful - Significant
# Model
model.beliefs.GratefulChange <- lmer(gratitude_change ~ C_time_alone * C_beliefs_total + C_grateful_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.beliefs.GratefulChange)
confint(model.beliefs.GratefulChange, method = "Wald")
# Simple Slopes Analysis
sim_slopes(model.beliefs.GratefulChange, pred = C_time_alone, modx = C_beliefs_total)
# Model Estimates: Extracted for plotting purposes 这一行也报错
emm_result_Grateful <- emmeans(model.beliefs.GratefulChange, 
                      ~ beliefs_total * time_alone, 
                      at=list(beliefs_total = c(SD1_above, SD1_below)), 
                      cov.reduce = FALSE)
emm_result_Grateful 

# Supplement: Three-way Interactions ####
### Depression
model.3way.dep <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_PHQ_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.3way.dep) 
confint(model.3way.dep, method = "Wald")

# Simplified model with random intercept only
model.3way.dep.simple <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_PHQ_total + C_lonely_Tminus1 + (1 | prolific_id), data = esm_full, REML = FALSE)
confint(model.3way.dep.simple)

### Anxiety
model.3way.anx <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_GAD_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.3way.anx)
confint(model.3way.anx, method = "Wald")

### Trait Loneliness
model.3way.lonely <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_UCLA_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.3way.lonely) 
confint(model.3way.lonely, method = "Wald")

### Perceived Social Support
model.3way.pss <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_pss_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.3way.pss) 
confint(model.3way.pss, method = "Wald")

### Extraversion
model.3way.extra <- lmer(lonely_change ~ C_time_alone * C_beliefs_total * C_extra_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE)
summary(model.3way.extra)
confint(model.3way.extra, method = "Wald")

# Supplement: Beliefs Moderation Model w/ Covariates ####
model.beliefs.covariates <- lmer(lonely_change ~ C_time_alone * C_beliefs_total + C_PHQ_total + C_GAD_total + C_UCLA_total + C_pss_total + C_extra_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = esm_full, REML = FALSE) #PHQ=depression; GAD=anxiety, UCLA=loneliness; pss=perceived social support; extra=extraversion
summary(model.beliefs.covariates) # Interaction remains significant when controlling for theoretically relevant individual differences
confint(model.beliefs.covariates)

# Supplement: Most Socially Isolated Participants ####
## Subset 20% Most Socially Isolated Ps from Dataset
top_quintile <- quantile(esm_full$time_alone_person, probs = 0.80)
subset_avgtimealone_high <- esm_full[esm_full$time_alone_person >= top_quintile, ]
nrow(distinct(subset_avgtimealone_high, prolific_id)) #28 people
mean(subset_avgtimealone_high$lonely, na.rm=TRUE)
## Model
model.high.time.alone <- lmer(lonely_change ~ C_time_alone * C_beliefs_total + C_lonely_Tminus1 + (C_time_alone | prolific_id), data = subset_avgtimealone_high, control = lmerControl(optimizer ="Nelder_Mead"), REML = FALSE) 
summary(model.high.time.alone) #significant interaction
confint(model.high.time.alone, method="Wald")
## Simple Slopes Analysis
sim_slopes(model.high.time.alone, pred = C_time_alone, modx = C_beliefs_total)
## Model Estimates: Extracted for plotting
# Find 1 SD above/below
mean(subset_avgtimealone_high$beliefs_total, na.rm=TRUE) # Mean = 4.76
sd(subset_avgtimealone_high$beliefs_total, na.rm=TRUE) # SD = 1.26
SD1_above_SI <- mean(subset_avgtimealone_high$beliefs_total, na.rm=TRUE) + sd(subset_avgtimealone_high$beliefs_total, na.rm=TRUE)
SD1_below_SI <- mean(subset_avgtimealone_high$beliefs_total, na.rm=TRUE) - sd(subset_avgtimealone_high$beliefs_total, na.rm=TRUE)
# Extract estimated means for plotting 这一行也报错
emm_result_hightimealone <- emmeans(model.high.time.alone, 
                      ~ beliefs_total * time_alone, 
                      at=list(beliefs_total = c(SD1_below_SI, SD1_above_SI)), 
                      cov.reduce = FALSE)
emm_result_hightimealone 

## Average Loneliness in this Subsample of Participants
mean(subset_avgtimealone_high$lonely, na.rm=TRUE)

# Descriptives: Background Survey & Demographics ####

### Demographics
# Subset data
esm_demographics <- subset(esm_full, select=c("prolific_id", "age", "gender", "race", "education", "time_zone"))
esm_demographics <- unique(esm_demographics)
View(esm_demographics)
# Age
esm_demographics$age <- as.numeric(esm_demographics$age)
psych::describe(esm_demographics$age)
# Gender
esm_demographics$gender <- factor(esm_demographics$gender, levels = c(1,2,3), labels = c("male", "female", "non-binary"))
describe(esm_demographics$gender)
# Race
esm_demographics$race <- factor(esm_demographics$race, levels = c(1:6), labels = c("white", "black", "asian", "latino", "native", "other"))
describe(esm_demographics$race)
# Education
esm_demographics$education <- factor(esm_demographics$education, levels = c(1:6), labels = c("some HS", "HS", "some college", "bachelors", "masters", "doctorate"))
describe(esm_demographics$education)
# Time zone
esm_demographics$time_zone <- factor(esm_demographics$time_zone, levels = c(1:4), labels = c("EST", "CST", "MST", "PST"))
describe(esm_demographics$time_zone)  

### Background Measures
# Remove duplicates
esm_unique_subs <- esm_full %>% distinct(prolific_id, .keep_all = TRUE) 
# Extract background measures 
esm_traits <- subset(esm_unique_subs, select=c("beliefs_total", "UCLA_total", "pss_total", "PHQ_total", "GAD_total", "extra_total", "erq_total", "brood_total", "belong_total", "self_esteem", "comparison_total", "purpose_total", "lonely_person", "time_alone_person", "prolific_id"))
psych::describe(esm_traits)

# Descriptives: Experience Sampling Data ####
## Subset data
esm_phase2 <- subset(esm_full, select=c("lonely", "time_alone", "pos_mood", "neg_mood", "bored", "stressed", "content", "grateful"))
## Descriptives
psych::describe(esm_phase2)

# Cronbach Alpha for Individual Differences ####

#### Beliefs about being alone
# Subset data
esm1_beliefs <- subset(esm_unique_subs, select=c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6"))
# Cronbach alpha
psych::alpha(esm1_beliefs)
# Factor analysis
KMO(esm1_beliefs)
cortest.bartlett(esm1_beliefs)
scree(esm1_beliefs)
efa_result <- fa(esm1_beliefs, nfactors = 1, rotate = "varimax")
print(efa_result)
print(efa_result$loadings, cutoff = 0.3)
# Convergent validity
cor.test(esm_traits$beliefs_total, esm_traits$self_esteem)
cor.test(esm_traits$beliefs_total, esm_traits$pss_total)
cor.test(esm_traits$beliefs_total, esm_traits$purpose_total)
cor.test(esm_traits$beliefs_total, esm_traits$UCLA_total)
cor.test(esm_traits$beliefs_total, esm_traits$lonely_person)
cor.test(esm_traits$beliefs_total, esm_traits$belong_total)
cor.test(esm_traits$beliefs_total, esm_traits$brood_total)
cor.test(esm_traits$beliefs_total, esm_traits$PHQ_total)
# Discriminant validity
cor.test(esm_traits$beliefs_total, esm_traits$time_alone_person)

#### UCLA loneliness
# Subset data
esm1_lonely <- subset(esm_unique_subs, select=c("UCLA_1", "UCLA_2", "UCLA_3", "UCLA_4", "UCLA_5", "UCLA_6", "UCLA_7", "UCLA_8"))
# Cronbach alpha
psych::alpha(esm1_lonely)

#### Perceived social support
# Subset data
esm1_pss <- subset(esm_unique_subs, select=c("pss_1", "pss_2", "pss_3", "pss_4", "pss_5", "pss_6", "pss_7", "pss_8", "pss_9", "pss_10", "pss_11", "pss_12"))
# Cronbach alpha
psych::alpha(esm1_pss)

#### Depression
# Subset data
esm1_phq <- subset(esm_unique_subs, select=c("PHQ_1", "PHQ_2"))
# Cronbach alpha
psych::alpha(esm1_phq)

#### Anxiety
# Subset data
esm1_gad <- subset(esm_unique_subs, select=c("GAD_1", "GAD_2", "GAD_3", "GAD_4", "GAD_5", "GAD_6", "GAD_7"))
# Cronbach alpha
psych::alpha(esm1_gad)

#### Extraversion
# Subset data
esm1_extrav <- subset(esm_unique_subs, select=c("extrav_1", "extrav_2", "extrav_3", "extrav_4", "extrav_5", "extrav_6", "extrav_7", "extrav_8"))
# Cronbach alpha
psych::alpha(esm1_extrav)

# Code to Center Variables ####
# Person-Mean Centered
### Time Alone
esm_full$C_time_alone <- esm_full$time_alone - esm_full$time_alone_person
### Emotions
esm_full$C_lonely_Tminus1 <- esm_full$lonely_Tminus1 - esm_full$lonely_person
esm_full$C_neg_mood_Tminus1 <- esm_full$neg_mood_Tminus1 - esm_full$negmood_person
esm_full$C_bored_Tminus1 <- esm_full$bored_Tminus1 - esm_full$bored_person
esm_full$C_stressed_Tminus1 <- esm_full$stressed_Tminus1 - esm_full$stressed_person
esm_full$C_pos_mood_Tminus1 <- esm_full$pos_mood_Tminus1 - esm_full$posmood_person
esm_full$C_content_Tminus1 <- esm_full$content_Tminus1 - esm_full$content_person
esm_full$C_grateful_Tminus1 <- esm_full$grateful_Tminus1 - esm_full$grateful_person

# Grand Mean Centered
esm_unique_subs <- esm_full %>% distinct(prolific_id, .keep_all = TRUE) 
### Beliefs
grand_mean_beliefs <- mean(esm_unique_subs$beliefs_total, na.rm = TRUE)
esm_full$C_beliefs_total <- esm_full$beliefs_total - grand_mean_beliefs
### PHQ
grand_mean_PHQ <- mean(esm_unique_subs$PHQ_total, na.rm = TRUE)
esm_full$C_PHQ_total <- esm_full$PHQ_total - grand_mean_PHQ
### GAD
grand_mean_GAD <- mean(esm_unique_subs$GAD_total, na.rm = TRUE)
esm_full$C_GAD_total <- esm_full$GAD_total - grand_mean_GAD
### Trait loneliness
grand_mean_UCLA <- mean(esm_unique_subs$UCLA_total, na.rm = TRUE)
esm_full$C_UCLA_total <- esm_full$UCLA_total - grand_mean_UCLA
### PSS
grand_mean_pss <- mean(esm_unique_subs$pss_total, na.rm = TRUE)
esm_full$C_pss_total <- esm_full$pss_total - grand_mean_pss
### Extroversion
grand_mean_extra <- mean(esm_unique_subs$extra_total, na.rm = TRUE)
esm_full$C_extra_total <- esm_full$extra_total - grand_mean_extra

# Create Plot: Loneliness (Main MS) #####
# Run uncentered model
model.beliefs.uncentered <- lmer(lonely_change ~ time_alone * beliefs_total + lonely_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)
# 这一行也报错
emm_result <- emmeans(model.beliefs.uncentered, 
                      ~ beliefs_total * time_alone, 
                      at=list(beliefs_total = c(SD1_above, SD1_below)), 
                      cov.reduce = FALSE)
emm_result #shows the model predicted values at each level of time_alone and at 1 SD above/below mean of beliefs (uncentered) 

# Step 1: Generate marginal means using emmeans 这一行也报错
emm_result <- emmeans(
  model.beliefs.uncentered, 
  ~ time_alone * beliefs_total, 
  at = list(
    beliefs_total = c(SD1_below, SD1_above),  # Use ±1 SD values for beliefs_total
    time_alone = unique(esm_full_clean$time_alone)  # Include all levels of time_alone
  )
)

# Convert emmeans results to a data frame
marginal_means <- as.data.frame(emm_result)

# Add belief labels
marginal_means <- marginal_means %>%
  mutate(beliefs_label = ifelse(
    beliefs_total == SD1_below, 
    "Negative Beliefs", 
    "Positive Beliefs"
  ))

# Step 2: Simulate data for the violin plot based on emmeans
set.seed(123)  
simulated_violin_data <- marginal_means %>%
  rowwise() %>%
  mutate(
    simulated_values = list(rnorm(1000, mean = emmean, sd = SE)) 
  ) %>%
  unnest(simulated_values)  

# Step 3: Plot
ggplot() +
  geom_violin(
    data = simulated_violin_data, 
    aes(
      x = factor(time_alone), 
      y = simulated_values, 
      fill = beliefs_label), 
    alpha = 0.4, 
    position = position_dodge(0.8)) +
  geom_point(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      color = beliefs_label), 
    size = 3, 
    position = position_dodge(0.8)) +
  geom_errorbar(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      ymin = emmean - SE, 
      ymax = emmean + SE, 
      group = beliefs_label), 
    position = position_dodge(0.8), 
    width = 0.2) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  scale_fill_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  scale_color_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  labs(x = "Time Spent Alone Since Last Survey", 
    y = "Predicted Change in Loneliness", 
    title = "Beliefs about Being Alone") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16, color = "black"),
    panel.border = element_blank(),
    text = element_text(size = 18, color = "black"), 
    legend.title = element_blank()
  )

# Create Plots: Other Emotions (Main MS) ####

# Negative Affect
model.beliefs.uncentered <- lmer(NegAffect_change ~ time_alone * beliefs_total + neg_mood_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Boredom
model.beliefs.uncentered <- lmer(bored_change ~ time_alone * beliefs_total + bored_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Stressed
model.beliefs.uncentered <- lmer(stressed_change ~ time_alone * beliefs_total + stressed_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Positive Affect
model.beliefs.uncentered <- lmer(PosAffect_change ~ time_alone * beliefs_total + pos_mood_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Contentment
model.beliefs.uncentered <- lmer(content_change ~ time_alone * beliefs_total + content_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Gratitude
model.beliefs.uncentered <- lmer(gratitude_change ~ time_alone * beliefs_total + grateful_Tminus1 + (time_alone | prolific_id), data = esm_full, REML = FALSE)

# Run code below to plot (need to run desired model above first) 这一行也报错
emm_result <- emmeans(model.beliefs.uncentered, 
                      ~ beliefs_total * time_alone, 
                      at = list(
                        beliefs_total = c(SD1_below, SD1_above),
                        time_alone = unique(esm_full_clean$time_alone)
                      ))
marginal_means <- as.data.frame(emm_result)
marginal_means <- marginal_means %>%
  mutate(beliefs_label = ifelse(
    beliefs_total == SD1_below, 
    "Negative Beliefs", 
    "Positive Beliefs"
  ))
set.seed(123)
simulated_violin_data <- marginal_means %>%
  rowwise() %>%
  mutate(
    simulated_values = list(rnorm(1000, mean = emmean, sd = SE))
  ) %>%
  unnest(simulated_values)

ggplot() +
  geom_violin(
    data = simulated_violin_data, 
    aes(
      x = factor(time_alone), 
      y = simulated_values, 
      fill = beliefs_label
    ), 
    alpha = 0.4, 
    position = position_dodge(0.8)
  ) +
  geom_point(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      color = beliefs_label
    ), 
    size = 3, 
    position = position_dodge(0.8)
  ) +
  geom_errorbar(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      ymin = emmean - SE, 
      ymax = emmean + SE, 
      group = beliefs_label
    ), 
    position = position_dodge(0.8), 
    width = 0.2
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  scale_fill_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  scale_color_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  scale_y_continuous(
    breaks = c(-1, -0.5, 0, 0.5, 1), 
    limits = c(-1, 1)                 
  ) +
  labs(
    x = "Time Spent Alone Since Last Survey", 
    y = "Predicted Change in [Outcome]",  
    title = "Beliefs about Being Alone"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 20, color = "black"),
    panel.border = element_blank(),
    text = element_text(size = 18, color = "black"), 
    legend.title = element_blank()
  )


# Create Plot: Most Socially Isolated Participants (Supplement) ####

## Subset 20% Most Socially Isolated Participants
top_quintile <- quantile(esm_full$time_alone_person, probs = 0.80)
subset_avgtimealone_high <- esm_full[esm_full$time_alone_person >= top_quintile, ]

# Model
model.high.time.alone <- lmer(lonely_change ~ time_alone * beliefs_total + lonely_Tminus1 + (time_alone | prolific_id), data = subset_avgtimealone_high, control = lmerControl(optimizer ="Nelder_Mead"), REML = FALSE) 
summary(model.high.time.alone)

# Step 1: Generate marginal means using emmeans
emm_result <- emmeans(
  model.high.time.alone, 
  ~ time_alone * beliefs_total, 
  at = list(
    beliefs_total = c(SD1_below, SD1_above),  # Use ±1 SD values for beliefs_total
    time_alone = unique(subset_avgtimealone_high$time_alone)  # Include all levels of time_alone
  )
)

# Convert emmeans results to a data frame
marginal_means <- as.data.frame(emm_result)

# Add belief labels
marginal_means <- marginal_means %>%
  mutate(beliefs_label = ifelse(
    beliefs_total == SD1_below, 
    "Negative Beliefs", 
    "Positive Beliefs"
  ))

# Step 2: Simulate data for the violin plot based on emmeans
set.seed(123) 
simulated_violin_data <- marginal_means %>%
  rowwise() %>%
  mutate(
    simulated_values = list(rnorm(1000, mean = emmean, sd = SE))
  ) %>%
  unnest(simulated_values) 

# Step 3: Plot
ggplot() +
  geom_violin(
    data = simulated_violin_data, 
    aes(
      x = factor(time_alone), 
      y = simulated_values, 
      fill = beliefs_label
    ), 
    alpha = 0.4, 
    position = position_dodge(0.8)
  ) +
  geom_point(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      color = beliefs_label
    ), 
    size = 3, 
    position = position_dodge(0.8)
  ) +
  geom_errorbar(
    data = marginal_means, 
    aes(
      x = factor(time_alone), 
      y = emmean, 
      ymin = emmean - SE, 
      ymax = emmean + SE, 
      group = beliefs_label
    ), 
    position = position_dodge(0.8), 
    width = 0.2
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  scale_fill_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  scale_color_manual(values = c("Negative Beliefs" = "brown", "Positive Beliefs" = "skyblue2")) +
  labs(
    x = "Time Spent Alone Since Last Survey", 
    y = "Predicted Change in Loneliness", 
    title = "Beliefs about Being Alone"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16, color = "black"),
    panel.border = element_blank(),
    text = element_text(size = 18, color = "black"), 
    legend.title = element_blank()
  )

