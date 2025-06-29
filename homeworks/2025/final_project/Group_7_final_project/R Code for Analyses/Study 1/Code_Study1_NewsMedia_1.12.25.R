# Rodriguez, Schertz, & Kross, 2025, Nature Communications
# Study 1 
# How Does U.S. News Media Portray Being Alone?
# Last Revised January 12, 2025

### Packages ####
library(irr)
library(RVAideMemoire)
library(tidyverse)
library(psych)
library(dplyr)
library(boot)
library(vcd)

### Article Filtering ####
  
### Step 1: Screening Articles
# First, coders reviewed article headlines to determine if relevant.
# Import data
step1 <- read.csv("../Data/Study 1/Study1_Step1_ArticleScreening_10.31.23.csv")
coders_step1 <- c("Coder_1", "Coder_2")
ratings_step1 <- step1[coders_step1]
kappa2(ratings_step1) #kappa = 0.65 

### Step 2: Eligibility
# Next, coders reviewed full texts of articles to see if relevant. 
step2 <- read.csv("../Data/Study 1/Study1_Step2_Eligibility_8.15.24.csv")
coders_step2 <- c("Step2_Coder1", "Step2_Coder2")
ratings_step2 <- step2[coders_step2]
kappa2(ratings_step2) #kappa = 0.77

### Analyses With Final Batch of 144 Articles ####
# Import data
media <- read.csv("../Data/Study 1/Study1_FinalAnalyses_8.21.24.csv")
nrow(media) #144 articles

### Headline Analysis ####

# Interrater reliability
## Four independent coders reviewed
myvars <- c("coder_1", "coder_2", "coder_3", "coder_4")
ratings <- media[myvars]
kappam.fleiss(ratings) #kappa = 0.82

# Sum of ratings
media$fmode <- factor(media$mode, levels = c(0,1,2,3), labels = c("Neutral", "Negatively", "Positively", "Both"))
table(media$fmode)

# Proportion of ratings
prop.table(table(media$fmode))

# Chi square test across all four categories
observed <- c(44, 85, 8, 7) # counts for each variable
expected <- c(.25, .25, .25, .25) # must add up to 1
chi_square_result <- chisq.test(x=observed, p=expected)
chisq.multcomp(x=observed, p.method = "none") #all significantly different except 8 versus 7 (positive vs. both pos/neg)

# Chi square test: Negative vs positive headlines
observed_PosNeg <- c(85, 8) # counts for each variable
expected_PosNeg <- c(.50, .50) # must add up to 1
chi_square_result_PosNeg <- chisq.test(x=observed_PosNeg, p=expected_PosNeg)
chi_square_result_PosNeg
  # Effect size (Cramér's v)
  chi_square_statistic_PosNeg <- chi_square_result_PosNeg$statistic
  degrees_of_freedom_PosNeg <- chi_square_result_PosNeg$parameter
  n_PosNeg <- sum(observed_PosNeg)
  cramers_v_PosNeg <- sqrt(chi_square_statistic_PosNeg / (n_PosNeg * (min(length(observed_PosNeg), length(expected_PosNeg)) - 1)))
  cramers_v_PosNeg
  
# Chi square test: Negative vs neutral headlines
observed_NegNeu <- c(85, 44) # counts for each variable
expected_NegNeu <- c(.50, .50) # must add up to 1
chi_square_result_NegNeu <- chisq.test(x=observed_NegNeu, p=expected_NegNeu)
chi_square_result_NegNeu
# Effect size (Cramér's v)
  chi_square_statistic_NegNeu <- chi_square_result_NegNeu$statistic
  degrees_of_freedom_NegNeu <- chi_square_result_NegNeu$parameter
  n_NegNeu <- sum(observed_NegNeu)
  cramers_v_NegNeu <- sqrt(chi_square_statistic_NegNeu / (n_NegNeu * (min(length(observed_NegNeu), length(expected_NegNeu)) - 1)))
  cramers_v_NegNeu

# Pie chart
data <- data.frame(
  category = c("Positive", "Negative", "Both Neg & Pos", "Neutral"),
  value = c(8, 85, 7, 44))

colors <- c("Positive" = "steelblue2",
            "Negative" = "#B20000", 
            "Both Neg & Pos" = "grey27", 
            "Neutral" = "lightgray")

data$category <- factor(data$category, levels = c("Neutral", "Both Neg & Pos", "Negative", "Positive"))

ggplot(data, aes(x = "", y = value, fill = colors)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_identity() #Labels to pie chart added in Powerpoint


data$category <- factor(data$category, levels = c("Neither", "Both Risks & Benefits", "Only Risks", "Only Benefits"))

ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = colors)

### Full Text Analysis ####

# Proportion of articles mentioning specific benefits/risks
mean(media$relax)
mean(media$growth)
mean(media$creativity)
mean(media$hobbies)
mean(media$other_benefits)
mean(media$death)
mean(media$loneliness)
mean(media$mental_health)
mean(media$physical_health)
mean(media$other_risks)

# Any benefits or any risks
media$any_benefits
sum(media$any_benefits)
media$any_risks
sum(media$any_risks)
media$RiskBenefit <- with(media, as.integer((any_risks == 0 & any_benefits == 0) * 0 + 
                                            (any_risks == 1 & any_benefits == 0) * 1 +
                                            (any_risks == 0 & any_benefits == 1) * 2 +
                                            (any_risks == 1 & any_benefits == 1) * 3))
media$RiskBenefit <- factor(media$RiskBenefit, 
                           levels = c(0, 1, 2, 3), 
                           labels = c("neither", "only risks", "only benefits", "both risks and benefits"))
table(media$RiskBenefit) 
prop.table(table(media$RiskBenefit)) 

# Create pie chart 
data <- data.frame(
  category = c( "Only Benefits", "Only Risks", "Both Risks & Benefits", "Neither"),
  value = c(16, 79, 37, 12))

colors <- c("Only Benefits" = "steelblue2",
            "Only Risks" = "#B20000", 
            "Both Risks & Benefits" = "grey27", 
            "Neither" = "lightgray")
data$category <- factor(data$category, levels = c("Neither", "Both Risks & Benefits", "Only Risks", "Only Benefits"))

ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = colors)

# Chi square test: All four categories
observed <- c(79, 16, 37, 12) # counts for each variable
expected <- c(.25, .25, .25, .25) # must add up to 1
chisq.test(x=observed, p=expected) # significant; means significant differences from expected
chisq.multcomp(x=observed, p.method = "none")

# Chi square test: Just risks vs benefits
observed_NegPos <- c(79, 16) # counts for each variable
expected_NegPos <- c(.50, .50) # must add up to 1
chi_square_result_NegPos <- chisq.test(x=observed_NegPos, p=expected_NegPos)
chi_square_result_NegPos
# Effect size (Cramér's v)
  chi_square_statistic_NegPos <- chi_square_result_NegPos$statistic
  degrees_of_freedom_NegPos <- chi_square_result_NegPos$parameter
  n_NegPos <- sum(observed_NegPos)
  cramers_v_NegPos <- sqrt(chi_square_statistic_NegPos / (n_NegPos * (min(length(observed_NegPos), length(expected_NegPos)) - 1)))
  cramers_v_NegPos

# Chi square test: Just risks vs. neither benefits nor risks
observed_NegNeu <- c(79, 12) # counts for each variable
expected_NegNeu <- c(.50, .50) # must add up to 1
chi_square_result_NegNeu <- chisq.test(x=observed_NegNeu, p=expected_NegNeu)
chi_square_result_NegNeu
# Effect size (Cramér's v)
  chi_square_statistic_NegNeu <- chi_square_result_NegNeu$statistic
  degrees_of_freedom_NegNeu <- chi_square_result_NegNeu$parameter
  n_NegNeu <- sum(observed_NegNeu)
  cramers_v_NegNeu <- sqrt(chi_square_statistic_NegNeu / (n_NegNeu * (min(length(observed_NegNeu), length(expected_NegNeu)) - 1)))
  cramers_v_NegNeu

# Chi square test: Just risks vs neutral

# Subset data
media_forboot <- media[, c("growth", "relax", "creativity", "hobbies", "other_benefits", "death", "loneliness", "physical_health", "mental_health", "other_risks")]
# Bootstrapping to Get Standard Errors
full_forboot_count <- media_forboot[,-1] #remove ID column
## Convert to percentages
convert_to_percentage <- function(counts) {
  (counts / 144) * 100
}
## Apply the function to the dataset
full_forboot_percent <- convert_to_percentage(full_forboot_count)
#View(full_forboot_percent)
## Bootstrap to get SEs
bootstrap_function <- function(full_forboot_count, indices) {
  boot_sample <- full_forboot_count[indices, ]
  return(colSums(boot_sample))}

set.seed(123) # For reproducibility
n_iterations <- 1000
boot_results <- boot(data=full_forboot_percent, statistic=bootstrap_function, R=n_iterations) 
boot_results # This is already in percentages. Use the "original" and "std. error" columns for the graph created in Excel


### Comparing Isolation articles to all articles ####
prop.table(table(media$FullRiskBenefit)) #full text risks/benefits

media$onlyIsolation <- ifelse(media$Full_Isolation_YesNo == 1 & media$Full_SolitudeOrSolitary_YesNo == 0 & 
    media$Full_Alone_YesNo == 0, 1, 0)

media_only_isolation <- subset(media, onlyIsolation == 1)
prop.table(table(media_only_isolation$FullRiskBenefit))
nrow(media_only_isolation) #49 articles

media_without_isolation <- subset(media, onlyIsolation == 0)
prop.table(table(media_without_isolation$FullRiskBenefit))

# Create a contingency table with the observed counts
observed_counts <- rbind(
  media_only_isolation = c(0.26530612, 0.08163265, 0.06122449, 0.59183673),
  media = c(0.25694444, 0.08333333, 0.11111111, 0.54861111)
)

# Multiply by total counts
total_media_only_isolation <- 49 
total_media <- 144  
total_media_no_isolation <- 95
  
observed_counts <- observed_counts * c(total_media_only_isolation, total_media)
observed_counts <- round(observed_counts) 

chi_square_result <- chisq.test(observed_counts)

# Print the results
print(chi_square_result)

# Counts for "only risks" category
count_only_risks_isolation <- round(0.59183673 * 49)
count_only_risks_media <- round(0.54861111 * 144)
count_only_risks_noisolation <- round(0.52631579 * 95)
# Total number of articles in each group
total_isolation <- 49
total_media <- 144
total_media_noisolation <- 95
# Perform proportion test
prop_test_risks <- prop.test(
  x = c(count_only_risks_isolation, count_only_risks_media),
  n = c(total_isolation, total_media)
)
prop_test_risks <- prop.test(
  x = c(count_only_risks_isolation, count_only_risks_noisolation),
  n = c(total_isolation, total_media_noisolation)
)
# Print the result
print(prop_test_risks)


# Counts for "only benefits" category
count_only_benefits_isolation <- round(0.06122449 * 49)
count_only_benefits_media <- round(0.11111111 * 144)
count_only_benefits_media_noisolation <- round(0.13684211 * 95)
# Total number of articles in each group
total_isolation <- 49
total_media <- 144
total_media_noisolation <- 95
# Perform proportion test
prop_test_benefits <- prop.test(
  x = c(count_only_benefits_isolation, count_only_benefits_media),
  n = c(total_isolation, total_media)
)
# Print the result
print(prop_test_benefits)


