# Rodriguez, Schertz, & Kross, 2025, Nature Communications
# Study 2
# Does Exposure to Information About Being Alone Affect People's Beliefs?
# Last Revised January 12, 2025

## Packages ####
library(ggplot2)
library(psych)
library(irr)
library(irrCAC) 
library(lsr)
library(multcomp)
library(contrast)
library(jtools)
library(lavaan)
library(car) ## 原代码没有

## Import Data ####
data_before_exclusions <- read.csv("../Data/Study 2/Study2_Final_WithCodes_12.12.23.csv") 
nrow(data_before_exclusions) #N = 456 before exclusions (total completed survey)
data <- data_before_exclusions[data_before_exclusions$exclude ==! 1,] 
View(data)
nrow(data) #N = 439 after exclusions
table(data$condition)

## Planned Contrasts ####
aggregate(data$beliefsMean, list(data$condition), FUN=mean) #means of beliefs for each group
data$condition <- factor(data$condition, levels = c("AloneBenefitsCondition", "ControlCondition", "AloneRisksCondition"))
model <- lm(beliefsMean ~ condition, data)
summary(model)
confint(model)
leveneTest(beliefsMean ~ condition, data = data)  # 找不到这个功能could not find function "leveneTest"

## Plot ####
# First, reorder conditions for graphing purposes
data$condition <- factor(data$condition, levels = c("AloneRisksCondition", "ControlCondition", "AloneBenefitsCondition")) 
agg_data_se <- aggregate(beliefsMean ~ condition, data=data, FUN=function(x) c(mean=mean(x), se=sd(x)/sqrt(length(x))))
agg_data_se <- do.call(data.frame, agg_data_se)

p <- ggplot(agg_data_se, aes(x=condition, y=beliefsMean.mean)) +
  geom_bar(aes(fill=condition), stat="identity", alpha=0.7, color = NA) + 
  geom_errorbar(aes(ymin=beliefsMean.mean-beliefsMean.se, ymax=beliefsMean.mean+beliefsMean.se), width=.2) +
  labs(title="Mean Beliefs by Condition", x="Condition", y="Beliefs about Being Alone") +
  geom_point(data=data, aes(x=condition, y=beliefsMean, color=condition), alpha=0.8, position=position_jitter(width=0.2)) +
  coord_cartesian(ylim=c(1, 7)) +
  scale_fill_manual(values=c("#B20000", "gray", "steelblue2"), aesthetics = c("fill","color")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

## Calculate Effect Sizes
### Benefits vs. Risks
group1 <- data[data$condition == "AloneBenefitsCondition", ]
group2 <- data[data$condition == "AloneRisksCondition", ]
mean1 <- mean(group1$beliefsMean, na.rm = TRUE)
mean2 <- mean(group2$beliefsMean, na.rm = TRUE)
sd1 <- sd(group1$beliefsMean, na.rm = TRUE)
sd2 <- sd(group2$beliefsMean, na.rm = TRUE)
n1 <- length(group1$beliefsMean) #Pooled Standard Deviation
n2 <- length(group2$beliefsMean)
pooled_sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))
d <- (mean1 - mean2) / pooled_sd# Calculate Cohen's d
d
### Benefits vs. Control
group1 <- data[data$condition == "AloneBenefitsCondition", ]
group2 <- data[data$condition == "ControlCondition", ]
mean1 <- mean(group1$beliefsMean, na.rm = TRUE)
mean2 <- mean(group2$beliefsMean, na.rm = TRUE)
sd1 <- sd(group1$beliefsMean, na.rm = TRUE)
sd2 <- sd(group2$beliefsMean, na.rm = TRUE)
n1 <- length(group1$beliefsMean) #Pooled Standard Deviation
n2 <- length(group2$beliefsMean)
pooled_sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))
d <- (mean1 - mean2) / pooled_sd# Calculate Cohen's d
d
### Risks vs. Control
group1 <- data[data$condition == "AloneRisksCondition", ]
group2 <- data[data$condition == "ControlCondition", ]
mean1 <- mean(group1$beliefsMean, na.rm = TRUE)
mean2 <- mean(group2$beliefsMean, na.rm = TRUE)
sd1 <- sd(group1$beliefsMean, na.rm = TRUE)
sd2 <- sd(group2$beliefsMean, na.rm = TRUE)
n1 <- length(group1$beliefsMean) #Pooled Standard Deviation
n2 <- length(group2$beliefsMean)
pooled_sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))
d <- (mean1 - mean2) / pooled_sd# Calculate Cohen's d
d

## Exploratory ####
# Comparing Alone Risks vs. Control 
data$condition <- factor(data$condition, levels = c("ControlCondition", "AloneBenefitsCondition", "AloneRisksCondition"))
model2 <- lm(beliefsMean ~ condition, data)
summary(model2)
confint(model2)

## Demographics ####
data$age <- as.numeric(data$age)
psych::describe(data$age)

data$gender <- as.numeric(data$gender)
data$fgender <- factor(data$gender, levels = c(1,2,3), labels = c("male", "female", "other"))
table(data$fgender)

data$frace <- factor(data$race, levels = c(1:7), labels = c("white", "black", "asian", "latino", "native", "middle east", "other"))
table(data$frace)

data$feducation <- factor(data$education, levels = c(1,4,5,6,7,8,9,10), labels = c("less than HS", "high school", "some college", "associates", "bachelors", "masters", "professional", "doctorate"))
table(data$feducation)

## Inter-rater Reliability for Exclusion Criteria Coding ####
code <- data_before_exclusions
nrow(code) #contains both included & excluded participants (N = 456)

# Article Descriptions
## Total coder agreement is calculated by computing the mean of agreement for the six articles
### Filler Article 1: TikTok
tiktok_coders <- c("TikTok_coder1", "TikTok_coder2")
tiktok_coding <- code[tiktok_coders]
agree(tiktok_coding)
gwet.ac1.raw(tiktok_coding, categ.labels = NULL, conflev = 0.95, N = Inf)
### Filler Article 2: Red Wine
redwine_coders <- c("RedWine_coder1", "RedWine_coder2")
redwine_coding <- code[redwine_coders]
agree(redwine_coding)
gwet.ac1.raw(redwine_coding, categ.labels = NULL, conflev = 0.95, N = Inf)
### Filler Article 3: Cold Plunge
cold_coders <- c("Cold_coder1", "Cold_coder2")
cold_coding <- code[cold_coders]
agree(cold_coding)
gwet.ac1.raw(cold_coding, categ.labels = NULL, conflev = 0.95, N = Inf)
### Experimental Article 1: Alone Risks
risks <- code[code$condition == "AloneRisksCondition",] # Subset data to people in this condition
risks_coders <- c("Risks_coder1", "Risks_coder2")
risks_coding <- risks[risks_coders]
View(risks_coding)
agree(risks_coding)
gwet.ac1.raw(risks_coding, categ.labels = NULL, conflev = 0.95, N = Inf)
### Experimental Article 2: Alone Benefits
benefits <- code[code$condition == "AloneBenefitsCondition",] # Subset data to people in this condition
benefits_coders <- c("Benefits_coder1", "Benefits_coder2")
benefits_coding <- benefits[benefits_coders]
agree(benefits_coding)
gwet.ac1.raw(benefits_coding, categ.labels = NULL, conflev = 0.95, N = Inf) 
### Experimental Article 3: Control
control <- code[code$condition == "ControlCondition",] # Subset data to people in this condition
control_coders <- c("Control_coder1", "Control_coder2") 
control_coding <- control[control_coders]
agree(control_coding)
gwet.ac1.raw(control_coding, categ.labels = NULL, conflev = 0.95, N = Inf)

# Funneled Debrief
## Total coder agreement

FunDeb1_coders <- c("Fun1_Coder1", "Fun1_Coder2")
FunDeb1_coding <- code[FunDeb1_coders]
agree(FunDeb1_coding)
gwet.ac1.raw(FunDeb1_coding, weights = "unweighted", categ.labels = NULL,
             conflev = 0.95, N = Inf)

FunDeb2_coders <- c("Fun2_Coder1", "Fun2_Coder2")
FunDeb2_coding <- code[FunDeb2_coders]
agree(FunDeb2_coding)
gwet.ac1.raw(FunDeb2_coding, weights = "unweighted", categ.labels = NULL,
             conflev = 0.95, N = Inf)

FunDeb3_coders <- c("Fun3_Coder1", "Fun3_Coder2")
FunDeb3_coding <- code[FunDeb3_coders]
agree(FunDeb3_coding)
gwet.ac1.raw(FunDeb3_coding, weights = "unweighted", categ.labels = NULL,
             conflev = 0.95, N = Inf) 

## Cronbach Alpha for Beliefs Scale ####
describe(data$beliefsMean) #general descriptives for beliefs scale
beliefs <- data[, c("AloneBeliefs_1", "AloneBeliefs_2", "AloneBeliefs_3", "AloneBeliefs_4")]
View(beliefs)
alpha(beliefs)

## CFA for Beliefs Scale (Supplement) ####
cfa_model <- '
  Beliefs =~ AloneBeliefs_1 + AloneBeliefs_2 + AloneBeliefs_3 + AloneBeliefs_4
'
fit <- cfa(cfa_model, data = data)
summary(fit, fit.measures = TRUE, standardized = TRUE)

## Descriptives (Supplement) ####

data$ClimateBeliefs_3_afterRS <- 5 - data$ClimateBeliefs_3 #reverse coded
data$ClimateBeliefs_Mean <- rowMeans(data[, c("ClimateBeliefs_1", "ClimateBeliefs_2", "ClimateBeliefs_3_afterRS", "ClimateBeliefs_4")], na.rm = TRUE)
data$NatureBeliefs_Mean <- rowMeans(data[, c("NatureBeliefs_1", "NatureBeliefs_2", "NatureBeliefs_3", "NatureBeliefs_4")], na.rm = TRUE)
data$BusynessBeliefs_Mean <- rowMeans(data[, c("BusyBeliefs_1", "BusyBeliefs_2", "BusyBeliefs_3", "BusyBeliefs_4")], na.rm = TRUE)
data_descrip <- subset(data, select=c("beliefsMean", "ClimateBeliefs_Mean", "NatureBeliefs_Mean", "BusynessBeliefs_Mean"))
describe(data_descrip)   


