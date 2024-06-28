## Script to reproduce main overperception analyses in studies 1-3
# uses Study 2 data as an example (the first pre-registered study)
# script can be applied to Studies 1-3

# load libraries ----
library(tidyverse)
library(kableExtra)
library(psych)
library(Hmisc)
library(lmerTest)

# read data ----
setwd("...")

# original author self reported emotions (from Twitter DMs)
self_report <- read_csv("study2_self_report.csv")

# participant judgment data
data <- read_csv("study2_data_raw.csv")


# some functions we will use below ----
frequencies.table <- function(variable, label) {
  freq <- table(variable)
  prop <- prop.table(table(variable))
  perc <- prop*100
  combined <- cbind(freq, perc)
  
  kable((combined), format = "html",col.names = c("Freq", "%"), digits = 2) %>%
    kable_styling(bootstrap_options = "striped",full_width = FALSE, position = "left")
}
corr.matrix <- function(x){ 
  x <- as.matrix(x) 
  R <- Hmisc::rcorr(x)$r 
  p <- Hmisc::rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew)
}
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 




# data cleaning ----

# create a few variables

# gender label
data <- data %>% mutate(gender_label = ifelse(gender == 1, "Male", 
                                                        ifelse(gender == 2, "Female", "Other")))

# party label
data <- data %>% mutate(party_label = ifelse(party == 1, "Democrat", 
                                                       ifelse(party == 2, "Republican",
                                                              ifelse(party == 3, "Indepedent",
                                                                     ifelse(party == 4, "Other", "None")))))

# partisan identity strength
data <- data %>% mutate(p_identity = ifelse(is.na(sis_dem) == TRUE, sis_rep, sis_dem))

# ideological extremity
data <- data %>% mutate(ideo_extr = abs(ideo))


# comprehension check - we see 8 Ps failed comp check
frequencies.table(data$comp_check)

# remove Ps who failed comp check
data_trim <- data %>% filter(comp_check == 1)

# remove Ps who are not partisan
data_trim <- data_trim %>% filter(party_label == "Democrat" | party_label == "Republican")

# descriptives ----

# gender
frequencies.table(data_trim$gender_label)

# age
hist(data_trim$age)

# ideology
frequencies.table(data_trim$ideo)

# party
frequencies.table(data_trim$party_label)

# political identity strength
hist(data_trim$p_identity)
mean(data_trim$p_identity, na.rm = TRUE)

data_trim %>% ggplot(aes(x = p_identity)) +
  geom_density(fill = "lightblue", alpha = .8) +
  xlab("Political Identity Strength") +
  ylab("Density") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 4, linetype = "dashed") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size = 15, face = 'bold'))


# main overperception analysis (data wrangling) ----

# select only tweet-level ratings, select emotion, transpose, arrange
data_or_trans <- data_trim %>% select(ends_with("_or")) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(tweet_id = as.integer(numextract(rownames(.))), tweet_id_char = rownames(.)) %>% 
  arrange(tweet_id)

data_hap_trans <- data_trim %>% select(ends_with("_hap")) %>%
  t() %>% 
  as.data.frame() %>% 
  mutate(tweet_id = as.integer(numextract(rownames(.))), tweet_id_char = rownames(.)) %>% 
  arrange(tweet_id)

# compute mean outrage perceived across participants
data_or_trans$mean_or <- data_or_trans %>% 
  select(-c(tweet_id, tweet_id_char)) %>% rowMeans(., na.rm = TRUE)

# compute mean happiness perceived across participants
data_hap_trans$mean_hap <- data_hap_trans %>% 
  select(-c(tweet_id, tweet_id_char)) %>% rowMeans(., na.rm = TRUE)

# bring in self-report ratings for comparision, join by tweet_id
data_or_trans <- data_or_trans %>% left_join(self_report, by = "tweet_id")
data_hap_trans <- data_hap_trans %>% left_join(self_report, by = "tweet_id")

# main overperception analysis (models), outrage ----

data_mlm <- data_or_trans %>% select(V1:V165, tweet_id, sr_outrage) %>% 
  pivot_longer(cols = -c(tweet_id, sr_outrage),
               values_to = "judgment",
               names_to = "pid",
               values_drop_na = TRUE)

data_mlm <- data_mlm %>% mutate(name = "perceiver")

self_report_trim <- self_report %>% select(tweet_id, sr_outrage) %>% 
  mutate(pid = as.character(row_number(tweet_id)))


data_mlm2 <- data_mlm %>% select(tweet_id) %>% 
  mutate(name = "author") %>% left_join(self_report_trim, by = "tweet_id") %>% 
  mutate(judgment = sr_outrage)


col_order <- c("tweet_id", "sr_outrage", "pid",
               "judgment", "name")

data_mlm2 <- data_mlm2[, col_order]

data_mlm3 <- rbind(data_mlm, data_mlm2) %>% arrange(tweet_id) %>% 
  mutate(name_dum = ifelse(name == "author", 0, 1))


options(scipen = 999)

# model random intercept for tweet_id x observer
summary(lmer(judgment ~ name + (1 | tweet_id) + (1 |pid), data = data_mlm3))

# main overperception analysis (models), happiness ----

data_mlmh <- data_hap_trans %>% select(V1:V165, tweet_id, sr_happy) %>% 
  pivot_longer(cols = -c(tweet_id, sr_happy),
               values_to = "judgment",
               names_to = "pid",
               values_drop_na = TRUE)

data_mlmh <- data_mlmh %>% mutate(name = "perceiver")


self_report_trimh <- self_report %>% select(tweet_id, sr_happy) %>% 
  mutate(pid = as.character(row_number(tweet_id)))


data_mlm2h <- data_mlmh %>% select(tweet_id) %>% 
  mutate(name = "author") %>% left_join(self_report_trimh, by = "tweet_id") %>% 
  mutate(judgment = sr_happy)


col_orderh <- c("tweet_id", "sr_happy", "pid",
                "judgment", "name")

data_mlm2h <- data_mlm2h[, col_orderh]


data_mlm3h <- rbind(data_mlmh, data_mlm2h) %>% arrange(tweet_id) %>% 
  mutate(name_dum = ifelse(name == "author", 0, 1))


options(scipen = 999)


# model random intercept for tweet_id x observer
summary(lmer(judgment ~ name + (1 | tweet_id) + (1 |pid), data = data_mlm3h))


## participant-level analysis, studies 1-2, and 3 ----

op1 <- read_csv("study1_overperception.csv")
op2 <- read_csv("study2_overperception.csv")
op3 <- read_csv("study3_overperception.csv")

op_final <- rbind(op1, op2)
op_final_all <- rbind(op1, op2, op3)

# correlation studies 1 2 
cor.test(op_final$sm_use_politics_slider, op_final$overperception, method = "pearson")

# regression adjusting studies 1 2 
summary(lm(overperception ~ scale(sm_use_politics_slider) + scale(ideo_extr) + scale(p_identity), data = op_final))

# correlation study 3 
cor.test(op3$sm_use_politics_slider, op3$overperception, method = "pearson")

# regression adjusting study 3 
summary(lm(overperception ~ scale(sm_use_politics_slider) + scale(ideo_extr) + scale(p_identity), data = op3))





