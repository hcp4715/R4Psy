## LOAD PACKAGES ####
require(lme4)
require(sciplot)
require(languageR)
library(ggplot2)
library(dplyr)
####################

###########################################################################################
######################################### EXPERIMENT 1 ####################################
###########################################################################################

## READ DATA ####
data_exp1 <- read.table("./data/full_results_exp1_fwords.txt", header = TRUE, sep = "\t")

################################
### GRAPH RESULTS - EXP1 #######
################################

data_figs = data_exp1%>%
  group_by(subject, condition, phase)%>%
  summarise(looking_time=mean(looking_time))%>%
  ungroup()

data_figs_sum= data_figs%>%
  group_by(condition,phase)%>%
  summarise(mean_looking = mean(looking_time),
            sd_looking = sd(looking_time),
            count = n())%>%
  ungroup()%>%
  mutate(se_looking=sd_looking/sqrt(count))%>%
  mutate(se_looking_low=mean_looking - se_looking)%>%
  mutate(se_looking_high = mean_looking + se_looking)

graph_results_exp1 = ggplot(data_figs_sum, aes(x= condition, y=mean_looking, fill=phase)) +
  geom_bar(width= 0.5, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=se_looking_low, ymax=se_looking_high), position=position_dodge(0.5), width=0.2)+
  ylim(0, 25) +
  scale_fill_manual(values = c("#1f78b4", "#8dd3c7")) +
  xlab("Condition") +
  ylab("Mean looking time (seconds)") +
  theme_bw() +
  theme(text=element_text(size=25), title=element_text(size=25),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

graph_results_exp1

#######################################
### STATISTICAL ANALYSIS - EXP1 #######
#######################################

## Testing if data follow a normal distribution ##
variables_exp1 <-evalq(aggregate(looking_time,list(subject=subject, condition=condition, phase=phase),mean),data_exp1)

#### Q-Q Plot #### 
qqnorm(variables_exp1$x)
qqline(variables_exp1$x)

#### SHAPIRO TEST #### 
shapiro.test(variables_exp1$x)
# The shapiro.test showed that the data tested are not from a normally distributed population:
# W=0.94, p= <.0001

##### Log-transformed looking time ####
data_exp1_in_log = data_exp1%>%
  group_by(subject, condition, phase)%>%
  summarise(looking_time=log10(looking_time))%>%
  ungroup()

variables_exp1_log <-evalq(aggregate(looking_time,list(subject=subject, condition=condition, phase=phase),mean),data_exp1_in_log)

# Does data follow a normal distribution, now? ## YES! ##
qqnorm(variables_exp1_log$x)
qqline(variables_exp1_log$x)
shapiro.test(variables_exp1_log$x) 
## YES: W = 0.98384, p-value = 0.2867###

####################################
######### ANOVA - EXP1 #############
####################################
anova_exp1<-aov(x~condition*phase +Error(subject/phase),variables_exp1_log)
summary(anova_exp1)

### ANOVA SUMMARY EXP 1 #####
#Error: subject
#Df Sum Sq Mean Sq F value  Pr(>F)   
#condition  1 0.4041  0.4041   8.364 0.00583 **
#  Residuals 46 2.2223  0.0483                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: subject:phase
#Df Sum Sq Mean Sq F value Pr(>F)  
#phase            1 0.0797 0.07968   2.212 0.1438  
#condition:phase  1 0.2034 0.20337   5.646 0.0217 *
#  Residuals       46 1.6570 0.03602                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

######################################################
######### EFFECT SIZE - EXP1 (Cohen's d) #############
######################################################
# Calculating the effect size of the interaction found between condition and phase (F(1,46)=5.64, p =0.0217*)

# Comparisons of means for dishabituation (interaction) EXP1 in LOG
M1 = -0.03#avg of dishabituation for infants in the Noun-switch condition (avg looking time test - hab)
M2 = 0.15 #avg of dishabituation for infants in the Verb-switch condition (avg looking time test - hab)
n1 = 24#nb of participants in the Noun-switch condition
n2 = 24#nb of participants in the Verb-switch condition

##To calculate the spooled:
SD1=0.29#standard deviation of M1
SD2=0.25#Standard deviation of M2

#Calculating Cohen's d
#d=(M2 - M1) ⁄ SDpooled
SDpooled=sqrt((((n1-1)*SD1^2)+((n2-1)*SD2^2))/(n1+n2-2))
diff_means_exp1_log = M2-M1
d_exp1_interaction = diff_means_exp1_log / SDpooled
print(d_exp1_interaction)
# d = 0.6648452



###########################################################################################
######################################### EXPERIMENT 2 ####################################
###########################################################################################

## READ DATA ####
data_exp2 <- read.table("./data/full_results_exp2_prosody.txt", header = TRUE, sep = "\t")

################################
### GRAPH RESULTS - EXP2 #######
################################

data_figs_exp2 = data_exp2%>%
  group_by(subject, condition, phase)%>%
  summarise(looking_time=mean(looking_time))%>%
  ungroup()

data_figs_exp2_sum= data_figs_exp2%>%
  group_by(condition,phase)%>%
  summarise(mean_looking = mean(looking_time),
            sd_looking = sd(looking_time),
            count = n())%>%
  ungroup()%>%
  mutate(se_looking=sd_looking/sqrt(count))%>%
  mutate(se_looking_low=mean_looking - se_looking)%>%
  mutate(se_looking_high = mean_looking + se_looking)

graph_results_exp2 = ggplot(data_figs_exp2_sum, aes(x= condition, y=mean_looking, fill=phase)) +
  geom_bar(width= 0.5, stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=se_looking_low, ymax=se_looking_high), position=position_dodge(0.5), width=0.2)+
  ylim(0, 25) +
  scale_fill_manual(values = c("#1f78b4", "#8dd3c7")) +
  xlab("Condition") +
  ylab("Mean looking time (seconds)") +
  theme_bw() +
  theme(text=element_text(size=25), title=element_text(size=25),
        axis.line = element_line(colour = "black"),
        legend.position="top", legend.key=element_blank(),
        strip.background = element_rect(color="white", fill="white"))

graph_results_exp2

#######################################
### STATISTICAL ANALYSIS - EXP2 #######
#######################################

## Testing if data follow a normal distribution ##
variables_exp2 <-evalq(aggregate(looking_time,list(subject=subject, condition=condition, phase=phase),mean),data_exp2)

#### Q-Q Plot #### 
qqnorm(variables_exp2$x)
qqline(variables_exp2$x)

#### SHAPIRO TEST #### 
shapiro.test(variables_exp2$x)
# The shapiro.test showed that the data tested are not from a normally distributed population:
# W=0.92, p= <.0001

##### Log-transformed looking time ####
data_exp2_in_log = data_exp2%>%
  group_by(subject, condition, phase)%>%
  summarise(looking_time=log10(looking_time))%>%
  ungroup()

variables_exp2_log <-evalq(aggregate(looking_time,list(subject=subject, condition=condition, phase=phase),mean),data_exp2_in_log)
# Does data follow a normal distribution, now? ## YES! ##
qqnorm(variables_exp2_log$x)
qqline(variables_exp2_log$x)
shapiro.test(variables_exp2_log$x) 
## YES: W = 0.98499, p-value = 0.3448###

####################################
######### ANOVA - EXP2 #############
####################################
anova_exp2<-aov(x~condition*phase +Error(subject/phase),variables_exp2_log)
summary(anova_exp2)

### ANOVA SUMMARY EXP 2 #####
#Error: subject
#Df Sum Sq Mean Sq F value Pr(>F)  
#condition  1  0.319  0.3193   4.367 0.0422 *
#  Residuals 46  3.363  0.0731                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Error: subject:phase
#Df Sum Sq Mean Sq F value Pr(>F)  
#phase            1 0.0058 0.00583   0.126 0.7239  
#condition:phase  1 0.2348 0.23485   5.091 0.0288 *
#  Residuals       46 2.1220 0.04613                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


######################################################
######### EFFECT SIZE - EXP2 (Cohen's d) #############
######################################################
# Calculating the effect size of the interaction found between condition and phase (F(1,46)=5.091, p =0.0288*)

# Comparisons of means for dishabituation (interaction) EXP2 in LOG
M1 = -0.11#avg of dishabituation for infants in the Noun-switch condition (avg looking time test - hab)
M2 = 0.08 #avg of dishabituation for infants in the Verb-switch condition (avg looking time test - hab)
n1 = 24#nb of participants in the Noun-switch condition
n2 = 24#nb of participants in the Verb-switch condition

##To calculate the spooled:
SD1=0.28#standard deviation of M1
SD2=0.32#Standard deviation of M2

#Calculating Cohen's d
#d=(M2 - M1) ⁄ SDpooled
SDpooled=sqrt((((n1-1)*SD1^2)+((n2-1)*SD2^2))/(n1+n2-2))
diff_means_exp2_log = M2-M1
d_exp2_interaction = diff_means_exp2_log / SDpooled
print(d_exp2_interaction)
# d = 0.6319306


