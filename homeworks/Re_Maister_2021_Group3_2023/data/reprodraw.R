
rm(list=ls())
graphics.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(reshape2)
library(ggsci)
library(lmerTest)
library(sjPlot)
library(ggplot2)
library(effects)
library(Cairo)
library(ggeffects)
library(ggsci)
library(dplyr)


#Question 1: Do self-portraits look like the participant?
ALL <- read.csv("RDM.csv", header=TRUE)
real_vs_real<-read.csv('real_vs_real.csv', header = FALSE)
portrait_vs_portrait<-read.csv('portrait_vs_portrait.csv', header = FALSE)
gender_matrix<-read.csv('gender_matrix.csv', header= FALSE)
identity_matrix<-read.csv('identity_matrix.csv', header= FALSE)

gender<-ALL[,1]

dissim<-subset(ALL, select = -c(1,2))
gen_sort_dissim<-dissim[order(gender),]

gen_sort_dissim_2<-gen_sort_dissim[,order(gender)]

#portrait vs. real dissimilarity values
female<-subset(dissim, select = (gender==1))
just_female<-female[(gender==1),]
male<-subset(dissim, select = (gender==2))
just_male<-male[(gender==2),]

#real vs. real dissimilarity values
female_real<-subset(real_vs_real, select = (gender==1))
just_female_real<-female_real[(gender==1),]
male_real<-subset(real_vs_real, select = (gender==2))
just_male_real<-male_real[(gender==2),]

#portrait vs. portrait dissimilarity values
female_portrait<-subset(portrait_vs_portrait, select = (gender==1))
just_female_portrait<-female_portrait[(gender==1),]
male_portrait<-subset(portrait_vs_portrait, select = (gender==2))
just_male_portrait<-male_portrait[(gender==2),]

portrait_vs_real_other<-NULL;
portrait_vs_real_self<-NULL;

#self-dissimilarity scores were significantly lower
#than cross-individual non-self dissimilarity scores
for (ppt in 1:nrow(dissim))
{tem<-t(dissim[ppt,-ppt])
portrait_vs_real_other[ppt]<-mean(tem)
portrait_vs_real_self[ppt]<-dissim[ppt,ppt]}

library(effsize)
t.test(portrait_vs_real_self,portrait_vs_real_other,paired = TRUE)
cohen.d(portrait_vs_real_self,portrait_vs_real_other,pooled=TRUE,paired=TRUE)

#the real-face RDM was shown to significantly predict the portrait RDM
gen_vec<-unlist(gender_matrix)
real_vec<-unlist(real_vs_real)
portrait_vec<-unlist(portrait_vs_portrait)
female_portrait_vec<-unlist(just_female_portrait)
female_real_vec<-unlist(just_female_real)
male_portrait_vec<-unlist(just_male_portrait)
male_real_vec<-unlist(just_male_real)
ID_vec<-unlist(identity_matrix)

dissim_vec<-unlist(dissim)

indices <- which(portrait_vec==0)
gen_vec_nodiag<-gen_vec[-indices]
portrait_vec_nodiag<-portrait_vec[-indices]
real_vec_nodiag<-real_vec[-indices]

fem_indices <- which(female_portrait_vec==0)
fem_portrait_vec_nodiag<-female_portrait_vec[-indices]
fem_real_vec_nodiag<-female_real_vec[-indices]

male_indices <- which(male_portrait_vec==0)
male_portrait_vec_nodiag<-male_portrait_vec[-indices]
male_real_vec_nodiag<-male_real_vec[-indices]

M1<-lm(portrait_vec_nodiag[gen_vec_nodiag==1]~real_vec_nodiag[gen_vec_nodiag==1])
M1alt<-lm(portrait_vec_nodiag~real_vec_nodiag + gen_vec_nodiag)

summary(M1)
confint(M1, level=0.95)

#an alternative suggested to calculate averageness control variable (mean of similarity of 
#each participants face with all other participants real faces (same gender))
M_samegen_nonSelfmeanREAL<-c()
F_samegen_nonSelfmeanREAL<-c()

for (ppt in 1:nrow(just_female_real))
{F_samegen_nonSelfmeanREAL[ppt]<-mean(as.numeric(just_female_real[ppt,-ppt]))}
for (ppt in 1:nrow(just_male_real))
{M_samegen_nonSelfmeanREAL[ppt]<-mean(as.numeric(just_male_real[ppt,-ppt]))}

F_ppt_names<-as.numeric(rownames(just_female_real))
M_ppt_names<-as.numeric(rownames(just_male_real))

male_samegen_nonSelfmeanREAL<-cbind(M_ppt_names,M_samegen_nonSelfmeanREAL)
female_samegen_nonSelfmeanREAL<-cbind(F_ppt_names,F_samegen_nonSelfmeanREAL)

samegen_nonSelfmeanREAL<-rbind(male_samegen_nonSelfmeanREAL,female_samegen_nonSelfmeanREAL)
newsamegen_nonSelfmeanREAL<-samegen_nonSelfmeanREAL[order(samegen_nonSelfmeanREAL[,1]),]

averageness_control<-newsamegen_nonSelfmeanREAL[,2]

#mean accuracy score across human raters for each portrait was significantly higher than chance level 
#human control exp
human_class_acc<-read.csv('human_control_exp.csv', header = FALSE)
human_class_acc<-as.numeric(unlist(human_class_acc)) #this is a classification accuracy value for each of the 77 faces, across raters.
t.test(human_class_acc,mu = 0.5)
Mean<-mean(human_class_acc)
Mu<-0.5
Sd<-sd(human_class_acc)
CohenD = (Mean - Mu) / Sd

#classification accuracy was also derived for the Openface algorithm using a simulated experiment 
#"robot experiment"
set.seed(333)
grand_mean_robot_experiment<-c()
tstats<-c()
classification<-c()

for (portrait_n in c(1:43)) #for female faces
{correct_real_dissim<-just_female[portrait_n,portrait_n]
incorrect_real_dissim<-just_female[portrait_n,-portrait_n]
classification<-rbind(classification,correct_real_dissim<incorrect_real_dissim)
}
mean_classif_fem<-(rowSums(classification)/43)
grand_mean_fem<-mean(mean_classif_fem)

classification<-c()
for (portrait_n in c(1:34)) #for male faces
{correct_real_dissim<-just_male[portrait_n,portrait_n]
incorrect_real_dissim<-just_male[portrait_n,-portrait_n]
classification<-rbind(classification,correct_real_dissim<incorrect_real_dissim)
}

mean_classif_male<-(rowSums(classification)/34)
grand_mean_male<-mean(mean_classif_male)

### mimicking the human behavioural exp
for (RobotExperimentN in c(1:1000)) #repeating the experiment 1000 times
{classification<-c()
list_males<-c(1:34)
list_females<-c(1:43)
used_already<-c()
mean_classif<-c()
mean_classif_perface<-data.frame(matrix(NA, nrow = 77, 
                                        ncol = 40))
for (robot_ppt in c(1:40))
{used_already<-c()
classification<-vector(,77)

trial_stim<-cbind(list_males,sample(list_males))

while (mean(trial_stim[,1]==trial_stim[,2])>0)
{trial_stim<-cbind(list_males,sample(list_males))}
new_list<-trial_stim[,2]

#males
for (portrait_n in c(1:34))
{correct_real_dissim<-just_male[portrait_n,portrait_n]
to_select_from<-new_list[new_list!=portrait_n]
to_select_from_final<-to_select_from [! to_select_from %in% used_already]
randomized<-to_select_from_final
real_incorrect<-randomized[1]
used_already<-append(used_already,real_incorrect)
incorrect_dissim<-just_male[portrait_n,real_incorrect]
classification[portrait_n]<-correct_real_dissim<incorrect_dissim
}

used_already<-c() #resetting for female stimuli
trial_stim<-cbind(list_females,sample(list_females))

while (mean(trial_stim[,1]==trial_stim[,2])>0)
{trial_stim<-cbind(list_females,sample(list_females))}
new_list<-trial_stim[,2]

#females
for (portrait_n in c(1:43))
{correct_real_dissim<-just_female[portrait_n,portrait_n]
to_select_from<-new_list[new_list!=portrait_n]
to_select_from_final<-to_select_from [! to_select_from %in% used_already]
randomized<-to_select_from_final
real_incorrect<-randomized[1]
used_already<-append(used_already,real_incorrect)
incorrect_dissim<-just_female[portrait_n,real_incorrect]
classification[portrait_n+34]<-correct_real_dissim<incorrect_dissim
}

mean_classif[robot_ppt]<-mean(classification)
mean_classif_perface[,robot_ppt] = classification #gives us whether each robot got each face right or wrong, for a N=40 exp
robot_class_acc<-rowMeans(mean_classif_perface)
} #end loop for 40 robot ppt

grand_mean_robot_experiment[RobotExperimentN]<-mean(mean_classif) #from an experiment with 40 robots
tstats[RobotExperimentN]<-t.test(human_class_acc,robot_class_acc)$statistic #save the tstatistic for that experiment
}
grand_mean_1000RobotExperiments<-mean(grand_mean_robot_experiment) #take a look at grand mean across 1000 reps of the experiment
sd(grand_mean_robot_experiment)
robot_class_acc<-rowMeans(mean_classif_perface)

#one-sample t-test against chance, for a randomly-chosen N=40 robot sample
t.test(robot_class_acc, mu = 0.5, alternative = "two.sided")
Mean<-mean(robot_class_acc)
Mu<-0.5
Sd<-sd(robot_class_acc)
CohenD = (Mean - Mu) / Sd

#bootstrapped hypothesis test across 10,000 samples
#the difference in accuracy between the algorithm and the human participants was not significant

observed_tstat<-t.test(human_class_acc,robot_class_acc)$statistic
t.test(human_class_acc,robot_class_acc)[["parameter"]][["df"]]
overall_mean<-mean(rbind(robot_class_acc,human_class_acc))
null_robot_group<-(robot_class_acc-mean(robot_class_acc))+overall_mean
null_human_group<-(human_class_acc-mean(human_class_acc))+overall_mean

set.seed(444)
resamples_robot <- lapply(1:10000, function(i) sample(null_robot_group, replace = T))
resamples_human <- lapply(1:10000, function(i) sample(null_human_group, replace = T))

nullH_tstat<-c()
nullDF<-c()

for (resample in 1:length(resamples_robot)){
  nullH_tstat[resample]<-t.test(unlist(resamples_robot[resample]),unlist(resamples_human[resample]))$statistic
  nullDF[resample]<-t.test(unlist(resamples_robot[resample]),unlist(resamples_human[resample]))[["parameter"]][["df"]]
}

hist(nullH_tstat)
#find proportion of nullHstats >=observed tstats
estimated_p<-1-mean(nullH_tstat>=observed_tstat)

##Question 3: Are self-portraits influenced by the psychological self? 

ALL_TRIALS_BFI <- read.csv("EXP1_BFI.csv", header=TRUE) 

names(ALL_TRIALS_BFI)[1]<-'PPT'
ALL_TRIALS_BFI$personality<-factor(ALL_TRIALS_BFI$personality)
ALL_TRIALS_BFI<-ALL_TRIALS_BFI[1:385,]
ALL_TRIALS_BFI$GENDER<-factor(ALL_TRIALS_BFI$GENDER)
ALL_TRIALS_BFI$PPT<-factor(ALL_TRIALS_BFI$PPT)

set.seed(583) # just to make it reproducible
#randomizing control test
for (i in unique(ALL_TRIALS_BFI$PPT)) 
{ALL_TRIALS_BFI$BFI_1PP_rand[ALL_TRIALS_BFI$PPT==i]<-sample(ALL_TRIALS_BFI$BFI10_1PP[ALL_TRIALS_BFI$PPT==i])}

#Finding the null model: Winning model is BFI0c, results described in Table S2
BFI0<-lmer(BFI10_3PP_sp~BFI10_3PP_real  + (1|PPT), data=ALL_TRIALS_BFI) 
BFI0a<-lmer(BFI10_3PP_sp~BFI10_3PP_real + personality + (1|PPT), data=ALL_TRIALS_BFI) 
BFI0b<-lmer(BFI10_3PP_sp~BFI10_3PP_real + personality + GENDER+ (1|PPT), data=ALL_TRIALS_BFI) 
BFI0c<-lmer(BFI10_3PP_sp~BFI10_3PP_real + personality*GENDER+ (1|PPT), data=ALL_TRIALS_BFI) #null 

summary(BFI0c)

#testing the hypothesis
#a H1 model that additionally included self-ratings of the five personality traits (Self TRAITS) 
#explained significantly more variance in portrait ratings (BFI10_3PP_sp) than the H0 model
BFI1<-lmer(BFI10_3PP_sp~BFI10_3PP_real + personality*GENDER+ BFI10_1PP+(1|PPT), data=ALL_TRIALS_BFI) 
anova(BFI0c,BFI1)
fixef(BFI1)
anova(BFI1)
#randomised control model
BFI1_rand<-lmer(BFI10_3PP_sp~BFI10_3PP_real + personality*GENDER+ BFI_1PP_rand+(1|PPT), data=ALL_TRIALS_BFI)
anova(BFI1,BFI1_rand)
#check assumptions of fitted model
plot_residuals(BFI1)
plot(fitted(BFI1),residuals(BFI1))
hist(residuals(BFI1))
plot_model(BFI1, type='diag')
##

#plotting random effects
randomeffects <- ranef(BFI1)
str(randomeffects)
randomeffects$PPT

names(randomeffects$PPT) <- "Intercept"
randomeffects$PPT$Participant <- rownames(randomeffects[[1]])

p <- ggplot(data=randomeffects$PPT, aes(x=as.factor(Participant), y=Intercept)) + geom_point(stat="identity") 
p <- p + ylab("Deviation from Grand Mean Intercept (0)") + xlab("Participant")
p <- p + geom_hline(yintercept=0)
p <- p + ggtitle("Portrait ratings as deviations from the grand mean")
p <- p + theme_bw() + theme(text=element_text(size=14))
p

ranef(BFI1)

# fixed effects
fixef(BFI1)

pred_wholegroup <- ggpredict(BFI1, c("BFI10_1PP"),type = "fe", ci.lvl = 0.95)
pred_eachPPT<-ggpredict(BFI1, terms = c("BFI10_1PP", "PPT"), type = "re", pretty = FALSE)

#fig2a
###
fig2a<-ggplot(pred_eachPPT) +
geom_smooth(aes(x = x, y = predicted, group = group, colour = "individual participants"), method = "lm", size =0.4) +
geom_line(data = pred_wholegroup, aes(x = x, y = predicted, colour = "\ngroup mean\n"), size =2) +
labs(x="\nself-reported personality traits",y="personality traits of self-portrait\n") +
scale_colour_manual(name = '', values = c("individual participants"="#A3B0E0","\ngroup mean\n"='black')) +
theme_bw(base_size = 30)+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig2a <-fig2a+guides(color=guide_legend(override.aes=list(fill='white', size=2))) + theme(legend.position="bottom")
fig2a
CairoPNG(700,800, file="Fig2a.png", bg="white")
fig2a
dev.off()

pdf(file = "Fig_2A.pdf", 
    width = 10, # The width of the plot in inches
    height = 10)
fig2a
dev.off()

#Question 4: Is the accuracy of self-portraits related to self-reported personality or self-esteem?

data <- read.csv("acc_indivDiff.csv", header=TRUE) 
M1<-lm(SIM~nonself_SIM + SE_soc, data = data) #this is the winning model from stepwise procedure (run in SPSS)
summary(M1)

#additional control checks
B1<-lm( SE_soc~averageness_control, data = data) #real-face averageness was not significantly related to social self-esteem
confint(B1, level = 0.95)

#no significant relationship between social self-esteem and real-face attractiveness 
cor.test(data$SE_soc,data$real_att)

#significance of social self-esteem as a predictor of self-portrait accuracy remained unchanged when controlling for real face attractiveness
M2<-lm(SIM~nonself_SIM + real_att + SE_soc, data = data)

pred_scatter <- ggpredict(M1, c("SE_soc"),type = "fe", ci.lvl = 0.95)
data$fit<-predict(M1)

M0<-lm(SIM~nonself_SIM, data = data)
Y_controlled<-resid(M0)
x<-data$SE_soc
y<-Y_controlled #this controls similarity rating for general averageness of each face before plotting, as in 1st step of regression
mypal = pal_npg("nrc", alpha = 0.7)(5)

#fig 2b
##
myplot<-ggplot(data, aes(x = x, y = y)) + 
  geom_point(shape=16, size = 2, color=mypal[4], alpha = 1)+
  geom_smooth(method="lm",size = 0.5, color=mypal[4], fill=mypal[4], linetype = 1,  alpha = 0.2 , se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x="\nsocial self-esteem",y="self-portrait dissimilarity\n")
myplot<-myplot+theme_bw() +theme(axis.text.x = element_text(color = "grey20", size = 16, face = "plain"),
                                 axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
                                 axis.title.x = element_text(color = "grey20", size = 22, face = "plain"),
                                 axis.title.y = element_text(color = "grey20", size = 22, face = "plain"),
                                 legend.title = element_text(color = "grey20", size = 20, face = "plain"),
                                 legend.text = element_text(color = "grey20", size = 16, face = "plain", margin = margin(t = 10)),
                                 axis.line.x= element_line(size = 0.5, colour = "gray25"),
                                 axis.line.y= element_line(size = 1, colour = "gray25")) +
  theme_bw(base_size = 30)+
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Cairo(650,610, file="fig2b.png", type="png", bg="white")
myplot
dev.off()

pdf(file = "Fig_2b.pdf", 
    width = 10, # The width of the plot in inches
    height = 10)
myplot
dev.off()

#some tables for SI appendix
#uses newer sjPlot

tab_model(BFI1, dv.labels = c('Self-portrait Traits'),
          string.est =  "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p-value",
          show.re.var=TRUE,
          show.dev=FALSE, show.icc=FALSE, show.r2=FALSE,
          pred.labels = c('Intercept', 'Real Face Traits', 'Agreeableness', 'Conscientiousness', 'Neuroticism', 'Openness', 'Gender: Male',  'Agreeableness: Male', 'Conscientiousness: Male', 'Neuroticism: Male', 'Openness: Male'),
          file = 'S2.html')



###
###EXPERIMENT 2
#Question 1: Do self-portraits look like the participant? 

#no significant relationship between self-perceived hip-width from the self-portraits 
#and the participants' real hip measurements
final_LONG_TRAIT <- read.csv("EXP2_BODY.csv", header=TRUE) 
names(final_LONG_TRAIT)[1]<-'PPT'
final_LONG_TRAIT$SO<-factor(final_LONG_TRAIT$SO)
final_LONG_CLEAN<-final_LONG_TRAIT
final_LONG_CLEAN<-final_LONG_CLEAN[complete.cases(final_LONG_CLEAN[,1]),]
#check for outliers
boxplot(final_LONG_CLEAN$BESAA_total)
boxplot(final_LONG_CLEAN$PSE)
boxplot.stats(final_LONG_CLEAN$PSE)$out
source("http://goo.gl/UUyEzD")
outlierKD(final_LONG_CLEAN, PSE)
#exclude outlier
final_LONG_CLEAN<-final_LONG_CLEAN[final_LONG_CLEAN$PPT!=33,]
final_LONG_CLEAN$ZPSE<-scale(final_LONG_CLEAN$PSE, center = TRUE, scale = TRUE)
wide_form<-dcast(final_LONG_CLEAN, PPT + BESAA_total + hip~ SO, value.var="PSE")
cor.test(wide_form$hip, wide_form$"2") #typical is coded '1', self is coded '2'
#calculate difference between self and typical PSE
wide_form$diff<-wide_form$`2`-wide_form$`1` 
cor.test(wide_form$hip, wide_form$diff)

##
#Question 2: Are body self-portraits influenced by attitudes towards the self? 
final_LONG_CLEAN$SO<-relevel(final_LONG_CLEAN$SO,"2") #self is reference
#fit null
M0<-lmer(PSE~ (1|PPT), data=final_LONG_CLEAN) 
M0a<-lmer(PSE~ hip+(1|PPT), data=final_LONG_CLEAN) 
M0a1<-lmer(PSE~ SO+(1|PPT), data=final_LONG_CLEAN)
M0b<-lmer(PSE~ SO*hip+(1|PPT), data=final_LONG_CLEAN) #used as null model
#although these terms were not significant predictors of Hip PORTRAIT (PSE),
#they were included in H0 model to provide the strongest test for our hypothesis 

#hypothesis test
M1<-lmer(PSE~ SO*hip + BESAA_total +(1|PPT), data=final_LONG_CLEAN) 
anova(M0b,M1)
M2<-lmer(PSE~ SO*hip + BESAA_total*SO +(1|PPT), data=final_LONG_CLEAN) #winner
anova(M0b,M2)

#make more parsimonious
M1x<-lmer(ZPSE~SO*BESAA_total+ (1|PPT), data=final_LONG_CLEAN) #winner
anova(M2,M1x)
summary(M1x)

#diagnostic plots
plot_residuals(M1x)
plot(fitted(M1x),residuals(M1x))
plot(fitted(M1x),final_LONG_CLEAN$ZPSE)
hist(residuals(M1x))
plot_model(M1x, type='diag') 

##
#plotting random effects
randomeffects <- ranef(M1x)
str(randomeffects)
randomeffects$PPT

names(randomeffects$PPT) <- "Intercept"
randomeffects$PPT$Participant <- rownames(randomeffects[[1]])

p <- ggplot(data=randomeffects$PPT, aes(x=as.factor(Participant), y=Intercept)) + geom_point(stat="identity") 
p <- p + ylab("Deviation from Grand Mean Intercept (0)") + xlab("Participant")
p <- p + geom_hline(yintercept=0)
p <- p + ggtitle(" portrait hip-width as deviations from the grand mean")
p <- p + theme_bw() + theme(text=element_text(size=14))
p

#Fig 4
#plotting individual predicted values
final_LONG_CLEAN$fit<-predict(M1x)
pred <- ggpredict(M1x, c("BESAA_total", "SO"), type = "fe", ci.lvl = 0.95)

fig4<-plot(pred, rawdata  = FALSE, use.theme = TRUE, show.title = FALSE)
fig4<-fig4+ geom_point(data = final_LONG_CLEAN, shape=16, size = 2, alpha = 1, aes(x=BESAA_total,y=fit, colour=SO, fill = SO, alpha = 1),  inherit.aes = FALSE) +
  scale_color_npg(labels = c("self", "typical individual")) +
  scale_fill_npg(labels = c("self", "typical individual"))+
  theme_bw()+
  labs(x="\nbody self-esteem score",y="perceived hip width\n")+ 
  theme(axis.text.x = element_text(color = "grey20", size = 16, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 22, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 22, face = "plain"),
        legend.title = element_text(color = "grey20", size = 20, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16, face = "plain", margin = margin(t = 10))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig4<-fig4+guides(color=guide_legend(override.aes=list(fill=mypal[3:4], size=2)))
fig4<-fig4+scale_fill_manual(values = mypal[3:4],labels = c("self", "typical individual")) + scale_colour_manual(values = mypal[3:4],labels = c("self", "typical individual")) + labs(colour = "image type")
Cairo(861,708, file="Fig4_fit.png", type="png", bg="white")
fig4
dev.off() 

pdf(file = "Fig_4.pdf", 
    width = 9, # The width of the plot in inches
    height = 7.2)
fig4
dev.off()

#tables for SI Appendix
##uses newer version of sjPlot!

tab_model(M1x, df.method = "satterthwaite", show.df = TRUE, dv.labels = c('Portrait Hip Width'),
          string.est =  "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p-value",
          show.re.var=TRUE,
          show.dev=FALSE, show.icc=FALSE, show.r2=FALSE,
          pred.labels = c('Intercept', 'Image Type: Typical', 'Self-esteem', 'Typical: Self-esteem'),
          file = 'S4.html')

##supplementary results on attractiveness, trustworthiness, doiminance ratings of same portraits
final_LONG_TRAIT <- read.csv("EXP1_TRAIT.csv", header=TRUE) 
names(final_LONG_TRAIT)[1]<-'PPT'
final_LONG_TRAIT$GENDER<-as.factor(final_LONG_TRAIT$GENDER)
final_LONG_TRAIT$trait<-as.factor(final_LONG_TRAIT$trait)

M0a<-lmer(trait_3PP_sp~trait + (1|PPT), data=final_LONG_TRAIT) 
M0b<-lmer(trait_3PP_sp~trait+trait_3PP_real + (1|PPT), data=final_LONG_TRAIT) 
M0c<-lmer(trait_3PP_sp~trait_3PP_real +GENDER*trait+ (1|PPT), data=final_LONG_TRAIT)#null
summary(M0c)

M1a<-lmer(trait_3PP_sp~trait_3PP_real +GENDER*trait+ trait_1PP+(1|PPT), data=final_LONG_TRAIT) 
anova(M0c,M1a)
M1b<-lmer(trait_3PP_sp~trait_3PP_real +GENDER*trait+ trait_1PP+(1+trait_1PP|PPT), data=final_LONG_TRAIT)
anova(M1a,M1b)
summary(M1b)

#randomize the self-ratings to test specificity
set.seed(001) # just to make it reproducible
for (i in unique(final_LONG_TRAIT$PPT)) 
{final_LONG_TRAIT$trait_1PP_rand[final_LONG_TRAIT$PPT==i]<-sample(final_LONG_TRAIT$trait_1PP[final_LONG_TRAIT$PPT==i])}
M1a_rand<-lmer(trait_3PP_sp~trait_3PP_real +GENDER*trait+ trait_1PP_rand+(1|PPT), data=final_LONG_TRAIT)

anova(M1a,M1a_rand)
summary(M1a_rand)

#SI appendix tables
#traits

tab_model(M1a, dv.labels = c('Self-portrait Traits'),
          string.est =  "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p-value",
          show.re.var=TRUE,
          show.dev=FALSE, show.icc=FALSE, show.r2=FALSE,
          group.terms =TRUE,
          pred.labels = c('Intercept', 'Real Face Traits', 'Gender: Male', 'Trustworthiness', 'Dominance', 'Self-reported Traits','Trustworthiness: Male', 'Dominance: Male'),
          file = 'S3.html')
