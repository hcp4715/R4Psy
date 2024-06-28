## Study 5 twitter field study emotion perception

# load libraries ----
library(kableExtra)
library(psych)
library(Hmisc)
library(lsr)
library(rstatix)
library(tidyverse)
library(car)
library(lmerTest)




## load data ----
setwd("...")
data <- read_csv("study5_data_raw.csv")

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
data %>% group_by(id) %>% dplyr::summarize(n = n()) %>% nrow(.)

# remove people who do not identify was Rep or Dem: 100 Ps removed
data_trim <- data %>% 
  filter(political_party == "Republican" | political_party == "Democrat")

data_trim %>% group_by(id) %>% dplyr::summarize(n = n()) %>% nrow(.)

# comprehension check - we see 87 Ps failed comp checks, Final N = 1013
data_trim <- data_trim %>% filter(barr_check + barrett_check == 2)

data_trim %>% group_by(id) %>% dplyr::summarize(n = n()) %>% nrow(.)




## descriptives ----

descriptives <- data_trim %>% group_by(id) %>% 
  summarize_all(list(first))

# gender
frequencies.table(descriptives$gender)

# age
hist(descriptives$age)

# ideology
hist(descriptives$political_ideology)

# party
frequencies.table(descriptives$political_party)

# familiar with Barr
frequencies.table(descriptives$barr_familiar)

# familiar with Barrett
frequencies.table(descriptives$barrett_familiar)



## set up some variables ----

# recode political ideology network
descriptives <- descriptives %>% mutate(ideo_network_recode = 
                                          dplyr::recode(ideo_network, 
                                                 `1` = -3L,
                                                 `2`= -2L, 
                                                 `3` = -1L, 
                                                 `4` = 0L,
                                                 `5` = 1L,
                                                 `6` = 2L,
                                                 `7` = 3L))

# create own party vs other party ratings
descriptives <- descriptives %>% 
  mutate(ownparty_temp = 
           ifelse(political_party == "Democrat", dem_network_temp, rep_network_temp),
         otherparty_temp =
           ifelse(political_party == "Democrat", rep_network_temp, dem_network_temp),
         ideo_extr_network = abs(ideo_network_recode))

# make factor for plotting
descriptives <- descriptives %>%  
  mutate(condition_fac = factor(condition, levels = c("Overperception", "Accurate Perception")))


# group means
descriptives_plot <- descriptives %>% 
  filter(political_party == "Democrat" | political_party == "Republican") %>% 
  group_by(condition) %>% 
  dplyr::summarize(ownparty_temp = mean(ownparty_temp),
                   otherparty_temp = mean(otherparty_temp),
                   ideo_extr_network = mean(ideo_extr_network))



## plot network norms ----

data_trim <- data_trim %>% mutate(norm_stim_label_group = case_when(grepl("dem_high", norm_stim) ~ "dem_high",
                                                                    grepl("dem_low", norm_stim ) ~"dem_low",
                                                                    grepl("rep_high", norm_stim ) ~"rep_high",
                                                                    grepl("rep_low", norm_stim ) ~"rep_low")) 

# remove author tweet who opted out after experiment was run
data_trim <- data_trim %>% filter(norm_stim != "rep_high_64_1")


stim_approp <- data_trim %>% filter(political_party == "Democrat" | political_party == "Republican") %>% 
  group_by(id, condition, norm_stim_label_group) %>% 
  dplyr::summarize(mean = mean(appropriate_rating))


stim_approp_w <- spread(stim_approp, norm_stim_label_group, mean) %>% 
  mutate(dem_diff = dem_high - dem_low,
         rep_diff = rep_high - rep_low) %>% 
  mutate(diff = ifelse(is.na(dem_diff), rep_diff, dem_diff)) %>% 
  ungroup() %>% 
  select(id, diff)


stim_approp <- stim_approp %>% 
  left_join(stim_approp_w, by = "id")

stim_approp_plot <- stim_approp %>% 
  group_by(id) %>% dplyr::summarize(condition = first(condition),
                                       diff = first(diff))
stim_approp_plot <- stim_approp_plot %>%
  mutate(condition_fac = factor(condition, levels = c("High Overperception", "Low Overperception")))

# plot
stim_approp_plot %>% 
  ggplot(aes(x = condition_fac, y = diff, fill = condition_fac)) +
  geom_jitter(aes(color = condition_fac),
              alpha = .3,
              width = .27) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", 
               position = position_dodge(width=1),
               size=.3, width=.5, alpha=.6) +
  xlab("Condition") +
  ylab("Appropriate Rating Difference Score") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  scale_fill_manual(values=c("#a82424", "#787776")) +
  scale_color_manual(values=c("#a82424", "#787776")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size = 19, face = 'bold')) +
  #theme(legend.position = c(.82, .92)) +
  theme(legend.position = "none") +
  theme(legend.title = element_blank())

# test network norms ----


# examine distribution of DV
# overperception looks normal, note outlier in accurate perception
hist(stim_approp_plot$diff[stim_approp$condition == "High Overperception"])
hist(stim_approp_plot$diff[stim_approp$condition == "Low Overperception"])

# test for homogeneity of variance (violated)
leveneTest(diff ~ condition, data = stim_approp_plot)

# t-test, not assume equal variances
t.test(diff ~ condition, data = stim_approp_plot, var.equal = FALSE)

# compute cohen's d
cohensD(diff ~ condition, data = stim_approp_plot)



## plot network therm ----

ownparty <- descriptives %>% select(id, ownparty_temp) %>% 
  rename(temp = ownparty_temp) %>% 
  mutate(network = "Ingroup") %>% 
  cbind(descriptives$condition) %>% 
  rename(condition = `descriptives$condition`)

otherparty <- descriptives %>% select(id, otherparty_temp) %>% 
  rename(temp = otherparty_temp) %>% 
  mutate(network = "Outgroup") %>% 
  cbind(descriptives$condition) %>% 
  rename(condition = `descriptives$condition`)

plot_therm <- rbind(ownparty, otherparty)

plot_therm <- plot_therm %>%  
  mutate(condition_fac = factor(condition, levels = c("Overperception", "Accurate Perception")))
  
plot_therm %>% 
  ggplot(aes(x = network, y = temp, fill = condition_fac)) +
  geom_point(position = position_jitterdodge(dodge.width = 1, jitter.width = .5),
             aes(color = condition_fac, fill = condition_fac),
              alpha = .3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", 
               position = position_dodge(width=1),
               size=.3, width=.5, alpha=.6) +
  xlab("Political Group") +
  ylab("Percieved Network Feeling") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  scale_fill_manual(values=c("#a82424", "#787776")) +
  scale_color_manual(values=c("#a82424", "#787776")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size = 19, face = 'bold')) +
  theme(legend.position = c(.75, .92)) +
  theme(legend.title = element_blank())
  
# two-way mixed ANOVA, network therm ----
anova <- anova_test(
  data = plot_therm, dv = temp, wid = id,
  between = condition, within = network, effect.size = "pes")

get_anova_table(anova)






## plot ideo_extr ----

descriptives %>% 
ggplot(aes(x = condition_fac, y = ideo_extr_network, fill = condition_fac)) +
  geom_jitter(aes(color = condition_fac),
              alpha = .3,
              width = .27) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", 
               position = position_dodge(width=1),
               size=.3, width=.5, alpha=.6) +
  xlab("Condition") +
  ylab("Perceived Ideological Extremity") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  scale_fill_manual(values=c("#a82424", "#787776")) +
  scale_color_manual(values=c("#a82424", "#787776")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size = 19, face = 'bold')) +
  #theme(legend.position = c(.82, .92)) +
  theme(legend.position = "none") +
  theme(legend.title = element_blank())



# t-test ideo_extr ----

# examine distribution of DV
# overperception looks normal, note outlier in accurate perception
hist(descriptives$ideo_extr_network[descriptives$condition == "Overperception"])
hist(descriptives$ideo_extr_network[descriptives$condition == "Accurate Perception"])

# test for homogeneity of variance (violated)
leveneTest(ideo_extr_network ~ condition, data = descriptives)

# t-test, not assume equal variances
t.test(ideo_extr_network ~ condition, data = descriptives, var.equal = FALSE)

# compute cohen's d
cohensD(ideo_extr_network ~ condition, data = descriptives)







