## Script to reproduce Study 4 analyses

# load libraries ----
library(tidyverse)
library(kableExtra)
library(psych)
library(Hmisc)
library(lsr)
library(rstatix)
library(car)
library(lmerTest)

## load data ----
setwd("...")
data <- read_csv("study4_data_raw.csv")
stim <- read_csv("stim_descriptives_data.csv")

## some functions we will use below ----
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
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}







## data cleaning ----

# how many Ps raw
data %>% nrow()

# remove people who do not identify was Rep or Dem
data_trim <- data %>% 
  filter(political_party == "Republican" | political_party == "Democrat")

# 27 removed
nrow(data) - nrow(data_trim) 

# remove ppl who failed comp check, 52 removed
data_trim <- data_trim %>% filter(barr_check + barrett_check == 2)

# final N = 523
nrow(data_trim)




## descriptives  ----


# age
hist(data_trim$age)
mean(data_trim$age, na.rm = TRUE)
sd(data_trim$age, na.rm = TRUE)

# ideology
hist(data_trim$political_ideology)

# party
frequencies.table(data_trim$political_party)

# familiar with Barr
frequencies.table(data_trim$barr_familiar)

# familiar with Barrett
frequencies.table(data_trim$barrett_familiar)

# gender
frequencies.table(data_trim$gender)


## set up some variables ----

data %>% write_csv("study4_data_raw.csv")

# make factor for plotting
data_trim <- data_trim %>%  
  mutate(condition_fac = factor(condition, levels = c("High Overperception", "Low Overperception")))


## t-test group differences ----

# examine distribution of DV
# overperception looks normal, note outlier in low overperception
hist(data_trim$network_outrage[data_trim$condition == "High Overperception"])
hist(data_trim$network_outrage[data_trim$condition == "Low Overperception"])

# test for homogeneity of variance (violated)
leveneTest(network_outrage ~ condition, data = data_trim)

# t-test, not assume equal variances
t.test(network_outrage ~ condition, data = data_trim, var.equal = FALSE)

# compute cohen's d
cohensD(network_outrage ~ condition, data = data_trim)


## t-test comparing message perception means ----

# get means for one sample t-test
over_stim <- stim %>% filter(over_under == "Overperceived")
under_stim <- stim %>% filter(over_under == "Underperceived")

# filter data
over <- data_trim %>% filter(condition == "High Overperception")
under <- data_trim %>% filter(condition == "Low Overperception")


# one-sample t-test
t.test(over$network_outrage, mu = mean(over_stim$or_mean), alternative = "two.sided")
cohensD(over$network_outrage, mu = mean(over_stim$or_mean))

t.test(under$network_outrage, mu = mean(under_stim$or_mean), alternative = "two.sided")
cohensD(under$network_outrage, mu = mean(under_stim$or_mean))

## plot group outrage judgments ----

data_trim %>% 
  ggplot(aes(x = condition_fac, y = network_outrage, fill = condition_fac)) +
  geom_jitter(aes(color = condition_fac),
              alpha = .3,
              width = .2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", 
               position = position_dodge(width=1),
               size=.3, width=.4, alpha=.6) +
  xlab("Condition") +
  ylab("Perceived Network Outrage") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  scale_fill_manual(values=c("#a82424", "#787776")) +
  scale_color_manual(values=c("#a82424", "#787776")) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_hline(yintercept = 5.30, linetype = "dashed", color = "#a82424", size = 1, alpha = .5) +
  geom_hline(yintercept = 3.41, linetype = "dashed", color = "#787776", size = 1, alpha = .5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size = 15, face = 'bold')) +
  #theme(legend.position = c(.82, .92)) +
  theme(legend.position = "none") +
  theme(legend.title = element_blank())




