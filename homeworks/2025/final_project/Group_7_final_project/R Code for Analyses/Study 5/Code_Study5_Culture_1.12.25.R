# Rodriguez, Schertz, & Kross, 2025, Nature Communications
# Study 5
# Cross-Cultural Generalizability (Across Seven Nations)
# Last Revised January 12, 2025

# Packages ####
library(psych)
library(sjPlot)
library(MuMIn)
library(lme4)
library(emmeans)
library(knitr)
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

# Import Data ####
data_full <- read.csv("../Data/Study 5/Study5_9.11.24.csv", header=TRUE, sep=",") 
nrow(data_full) # 1260 before exclusions, 
data <- data_full[data_full$exclude == 0,]
nrow(data) # 1215 after exclusions

# Subset into countries
brazil <- data[data$country=="Brazil",]
southafrica <- data[data$country=="SouthAfrica",]
uk <- data[data$country=="UK",]
mexico <- data[data$country=="Mexico",]
spain <- data[data$country=="Spain",]
australia <- data[data$country=="Australia",]
poland <- data[data$country=="Poland",]

# Descriptives ####
descriptives <- brazil[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "Age", "Duration..in.seconds.")] #change data to specific country
psych::describe(descriptives)

# Correlations ####

# Across countries
cor.test(data$BeliefsTotal, data$Lonely)

# In each individual country
cor.test(brazil$BeliefsTotal, brazil$Lonely)
cor.test(southafrica$BeliefsTotal, southafrica$Lonely)
cor.test(uk$BeliefsTotal, uk$Lonely)
cor.test(mexico$BeliefsTotal, mexico$Lonely)
cor.test(spain$BeliefsTotal, spain$Lonely)
cor.test(australia$BeliefsTotal, australia$Lonely)
cor.test(poland$BeliefsTotal, poland$Lonely)

# Mega-Analysis Across Countries ####
modelMLM <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1|country), data=data)
summary(modelMLM) 
confint(modelMLM)
## Simple slopes
sim_slopes(modelMLM, pred=SocialContactReverse, modx=BeliefsTotal, confint = TRUE)
## Effect size of interaction
modelMLM_reduced <- lmer(Lonely ~ SocialContactReverse + BeliefsTotal + (1|country), data=data)
r2_full <- r.squaredGLMM(modelMLM)
r2_reduced <- r.squaredGLMM(modelMLM_reduced)
R2_full <- r2_full[1]
R2_reduced <- r2_reduced[1]
f2_interaction <- (R2_full - R2_reduced) / (1 - R2_full)
f2_interaction

# Plot Mega-Analytic Interaction
## Find 1 SD Above/Below Mean
mean(data$BeliefsTotal, na.rm=TRUE) 
sd(data$BeliefsTotal, na.rm=TRUE) 
SD1_above <- mean(data$BeliefsTotal, na.rm=TRUE) + sd(data$BeliefsTotal, na.rm=TRUE)
SD1_below <- mean(data$BeliefsTotal, na.rm=TRUE) - sd(data$BeliefsTotal, na.rm=TRUE)
## Model Estimates
emm_result <- emmeans(modelMLM, 
                             ~ BeliefsTotal * SocialContactReverse, 
                             at=list(BeliefsTotal = c(SD1_below, SD1_above)), 
                             cov.reduce = FALSE)
emm_df <- as.data.frame(emm_result)
emm_df <- emm_df %>% 
  mutate(lower = emmean - SE, upper = emmean + SE)
## Plot
my_colors <- c("#B20000", "steelblue2")
plot <- ggplot(emm_df, aes(x = SocialContactReverse, y = emmean, group = factor(BeliefsTotal))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(BeliefsTotal)), alpha = 0.2) +
  scale_fill_manual(values = my_colors, aesthetics = c("fill","color")) +
  geom_line(aes(color = factor(BeliefsTotal))) +
  labs(title = "", y = "", x = "") +
  coord_cartesian(ylim = c(10,28), xlim = c(1,7)) +
  scale_y_continuous(labels = c(10,15,20,25), breaks = c(10,15,20,25)) +
  scale_x_continuous(labels = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18, color = "black"),  
        axis.text.y = element_text(size = 18, color = "black"),
        axis.line = element_line(color = "black")) 
print(plot)

# Plotting raw values for each country (For illustration purposes) ####
# Brazil shown below as example
quantiles <- quantile(brazil$BeliefsTotal, probs = c(0.25, 0.75), na.rm = TRUE)
brazil$BeliefsGroup <- cut(brazil$BeliefsTotal, 
                           breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))
brazil_filtered <- subset(brazil, BeliefsGroup != "Middle 50%")

ggplot(brazil_filtered, aes(x = SocialContactReverse, y = Lonely, color = BeliefsGroup)) +
  geom_smooth(method = "lm", aes(group = BeliefsGroup, fill = BeliefsGroup), se = TRUE, fullrange = TRUE) +
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

# Exploratory Analyses (Supplement) ####

# 1. Cultural Differences in Beliefs: ANOVA
aggregate(data$BeliefsTotal, list(data$country), FUN=mean) 
beliefsAnova <- aov(BeliefsTotal ~ country, data=data) 
summary(beliefsAnova)
TukeyHSD(beliefsAnova)
confint(beliefsAnova)

anova_summary <- summary(beliefsAnova)
SS_effect <- anova_summary[[1]]["country", "Sum Sq"]
SS_total <- sum(anova_summary[[1]]["Sum Sq"])
eta_squared <- SS_effect / SS_total
eta_squared

# Plot
data$country <- factor(data$country, levels = c("UK", "Poland", "Australia", "Spain", "SouthAfrica", "Brazil", "Mexico")) #Reorder by beliefs
agg_data_se <- aggregate(BeliefsTotal ~ country, data=data, FUN=function(x) c(mean=mean(x), se=sd(x)/sqrt(length(x))))
agg_data_se <- do.call(data.frame, agg_data_se)

ggplot(agg_data_se, aes(x=country, y=BeliefsTotal.mean)) +
  geom_bar(aes(fill=country), stat="identity", alpha=0.7) + 
  geom_errorbar(aes(ymin=BeliefsTotal.mean-BeliefsTotal.se, ymax=BeliefsTotal.mean+BeliefsTotal.se), width=.2) +
  labs(title="Beliefs by Country", x="Country", y="Beliefs about Being Alone") +
  geom_point(data=data, aes(x=country, y=BeliefsTotal, color=country), alpha=0.8, position=position_jitter(width=0.2)) +
  coord_cartesian(ylim=c(1, 7)) +
  scale_y_continuous(breaks=1:7) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size=16), 
      axis.title.y = element_text(size=16),
      axis.text.x = element_text(size=13), 
      axis.text.y = element_text(size=13))

# 2. Moderation of Beliefs for Individual Countries
## Brazil
brazilModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=brazil)
summary(brazilModel)
confint(brazilModel)
## South Africa
southafricaModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=southafrica)
summary(southafricaModel)
confint(southafricaModel)
## U.K.
ukModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=uk)
summary(ukModel)
confint(ukModel)
## Mexico
mexicoModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=mexico)
summary(mexicoModel)
confint(mexicoModel)
## Spain
spainModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=spain)
summary(spainModel)
confint(spainModel)
## Australia
australiaModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=australia)
summary(australiaModel)
confint(australiaModel)
## Poland
polandModel <- lm(Lonely ~ SocialContactReverse * BeliefsTotal, data=poland)
summary(polandModel)
confint(polandModel)

# Plot Model Estimates (Example below is Brazil)
## Find 1 SD Above/Below Mean
SD1_above <- mean(brazil$BeliefsTotal, na.rm=TRUE) + sd(poland$BeliefsTotal, na.rm=TRUE)
SD1_below <- mean(brazil$BeliefsTotal, na.rm=TRUE) - sd(poland$BeliefsTotal, na.rm=TRUE)
## Model Estimates
emm_result <- emmeans(brazilModel, 
                      ~ BeliefsTotal * SocialContactReverse, 
                      at=list(BeliefsTotal = c(SD1_below, SD1_above)), 
                      cov.reduce = FALSE)
emm_df <- as.data.frame(emm_result)
emm_df <- emm_df %>% 
  mutate(lower = emmean - SE, upper = emmean + SE)
View(emm_df)
## Plot
my_colors <- c("#B20000", "steelblue2")
plot <- ggplot(emm_df, aes(x = SocialContactReverse, y = emmean, group = factor(BeliefsTotal))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(BeliefsTotal)), alpha = 0.2) +
  scale_fill_manual(values = my_colors, aesthetics = c("fill","color")) +
  geom_line(aes(color = factor(BeliefsTotal))) +
  labs(title = "", y = "", x = "") +
  coord_cartesian(ylim = c(10,26)) +
  coord_cartesian(xlim = c(1,7)) +
  scale_y_continuous(labels = c(10,15,20,25), breaks = c(10,15,20,25)) +
  scale_x_continuous(labels = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),  
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black")) 
print(plot)

# 3. Cross-Cultural Variability in the Interaction Between Beliefs and Time Spent Alone
## Random intercept model, presented in main text
modelMLM <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1|country), data=data)
summary(modelMLM) 
## Random slope to see if culture affects the interaction
model_culture_randomslope <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + (1 + SocialContactReverse * BeliefsTotal | country), data=data)
summary(model_culture_randomslope)
confint(model_culture_randomslope)
## Compare random intercept and random slope models
anova(modelMLM, model_culture_randomslope) #Chi square test is not significant (p=.18). Means that the random slope model does not account for any additional variance.

# 4. Adding covariates
modelMLM_Covs <- lmer(Lonely ~ SocialContactReverse * BeliefsTotal + VertInd + HorizInd + VertColl + HorizColl + Tight + RelMob + Age + Gender  + (1|country), data=data)
summary(modelMLM_Covs) 
confint(modelMLM_Covs)

# 5. Bivariate Correlations
## Code for brazil shown below
data_ForCorr <- brazil[, c("BeliefsTotal", "Lonely", "SocialContactReverse", "SocialContact", "Tight", "RelMob", "VertInd", "HorizInd", "VertColl", "HorizColl", "Age", "Gender")]
rcorr(as.matrix(data_ForCorr)) 

# Cronbach Alphas for Key Measures (Supplement) ####

## Cronbach Alpha: Beliefs about Being Alone
brazil_beliefs <- brazil[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(brazil_beliefs)

southafrica_beliefs <- southafrica[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(southafrica_beliefs)

uk_beliefs <- uk[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(uk_beliefs)

mexico_beliefs <- mexico[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(mexico_beliefs)

spain_beliefs <- spain[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(spain_beliefs)

australia_beliefs <- australia[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(australia_beliefs)

poland_beliefs <- poland[, c("beliefs_1", "beliefs_2", "beliefs_3", "beliefs_4", "beliefs_5", "beliefs_6")]
psych::alpha(poland_beliefs)

## Cronbach Alpha: Loneliness
brazil_lonely <- brazil[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(brazil_lonely)

southafrica_lonely <- southafrica[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(southafrica_lonely)

uk_lonely <- uk[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(uk_lonely)

mexico_lonely <- mexico[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(mexico_lonely)

spain_lonely <- spain[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(spain_lonely)

australia_lonely <- australia[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(australia_lonely)

poland_lonely <- poland[, c("Lonely_1", "Lonely_2", "Lonely_3", "Lonely_4", "Lonely_5", "Lonely_6", "Lonely_7", "Lonely_8")]
psych::alpha(poland_lonely)

# Country Selection using GFS Data: K-Means Clustering Analyses #### ### Unavailable
data <- read.csv("ORIGINAL_gfs_all_countries_wave1.csv") 
data$LONELY <- 10 - data$LONELY #Reverse score so 10 is extremely lonely
head(data)
nrow(data)
table(data$COUNTRY)

# Calculate mean loneliness for each country and reorder the factor
data <- data %>%
  group_by(COUNTRY) %>%
  mutate(mean_loneliness = mean(LONELY, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(COUNTRY = reorder(COUNTRY, mean_loneliness))

# Create the stat_summary plot with reordered factor
ggplot(data, aes(x = COUNTRY, y = LONELY)) +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Loneliness by Country", x = "Country", y = "Mean Loneliness")

# Pairwise comparisons
anova_result <- aov(LONELY ~ COUNTRY, data = data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# K-means clustering
install.packages("factoextra")
library(factoextra)
library(ggplot2)

aggregated_data <- data %>%
  group_by(COUNTRY) %>%
  summarize(mean_loneliness = mean(LONELY, na.rm = TRUE))
View(aggregated_data)
aggregated_data_scaled <- aggregated_data
aggregated_data_scaled$mean_loneliness <- scale(aggregated_data$mean_loneliness)

kmeans_result <- kmeans(aggregated_data_scaled$mean_loneliness, centers = 3, nstart = 25)

aggregated_data_scaled$cluster <- kmeans_result$cluster

ggplot(aggregated_data_scaled, aes(x = COUNTRY, y = mean_loneliness, color = factor(cluster))) +
  geom_point(size = 5) +
  theme_minimal() +
  labs(title = "K-means Clustering on Loneliness by Country", x = "Country", y = "Mean Loneliness (Scaled)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = c("olivedrab", "firebrick", "darkgoldenrod2")[aggregated_data_scaled$cluster]),
    axis.text.y = element_text(size = 14), color=c("black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)) +
  scale_color_manual(values = c("olivedrab", "firebrick", "gold2"), labels = c("Low Loneliness", "High Loneliness", "Moderate Loneliness"))

# Sensitivity Analysis ####
library(effsize)
library(effectsize)
library(pwr)
SensitivityResult_Study5_99 <- pwr.f2.test(u=2, v=1215-2-1, sig.level=0.05, power=0.99)
