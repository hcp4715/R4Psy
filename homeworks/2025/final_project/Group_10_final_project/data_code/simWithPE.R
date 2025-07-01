require(here)
require(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
source(here::here("Functions/simulate_withPE.R"))

dataPath = here::here("Data/")
load(file=paste(dataPath,"expFitNoiseRaw.RData",sep=""))
experiment = exp_fitNoiseRaw

experiment$correctFact = as.factor(experiment$discrimination_is_correct)
experiment$ID = as.factor(experiment$ID)
experiment$posteriorFactor = as.factor(experiment$posteriorFactor)
experiment$posteriorFactor = ordered(experiment$posteriorFactor, levels=c("L","M","H"))
experiment$condition = as.factor(experiment$condition)
experiment$same = as.integer(experiment$correct_answer_prior==experiment$correct_answer_target)

experiment$correct_answer_prior = as.factor(experiment$correct_answer_prior)
experiment$prior_key = as.factor(experiment$prior_key)

m1 = 1
m2 = 1
b = 2.18
internalNoise <<- 1
mode_m <<- 2
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13
pe = 2 # put negative for opposing choice bias

logAccsConfirmatory = glmer(correctFact ~ posteriorFactor*condition + same + (1|ID), data=experiment, family=binomial(link=logit))
car::Anova(logAccsConfirmatory)
emm = emmeans(logAccsConfirmatory, pairwise ~ same, adjust = "Bonferroni", lmer.df = "satterthwaite", type="response")
pairs(emm, simple="same", type="response") 
confint(emm, level=0.95, type="response")

simDataAll = simulateDistribution_withPE(m1, m2, b, internalNoise, right_coh, left_coh, 120000, FALSE, pe)
simDataAll$posteriorFactor = simDataAll$prec
simDataAll$discrimination_is_correct = simDataAll$correct2
simDataAll$conf = simDataAll$confTarget
simDataAll$choseSame = simDataAll$choseRight1==simDataAll$choseRight2
simDataAll$prior_direction = as.factor(simDataAll$prior_direction)
levels(simDataAll$prior_direction) = c("left", "right")

accSimSumm = simDataAll %>% dplyr::group_by(prior_direction, posteriorFactor) %>%
  dplyr::summarise(choseR = sum(choseRight2)/length(choseRight2))
accSimSumm$posteriorFactor = factor(accSimSumm$posteriorFactor, levels = c("L","M","H"))

accSimPlot = ggplot(accSimSumm, aes(x = posteriorFactor, y = choseR, color = prior_direction, group=prior_direction)) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_alpha_discrete(range=c(1,0.6)) +
  labs(x="Posterior Information", y = "P('Right') on Target", color="Lead Direction", linetype="") +
  guides(group="none",alpha="none") +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.6,0.9)

accSumm = experiment %>% dplyr::group_by(prior_key, posteriorFactor) %>%
  dplyr::summarise(choseR = sum(discrimination_key=="right")/length(discrimination_key=="right"))
accSumm$posteriorFactor = factor(accSumm$posteriorFactor, levels = c("L","M","H"))

accPlot = ggplot(accSumm, aes(x = posteriorFactor, y = choseR, color = prior_key, group=prior_key)) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_alpha_discrete(range=c(1,0.6)) +
  labs(x="Posterior Information", y = "P('Right') on Target", color="Lead Choice", linetype="") +
  guides(group="none",alpha="none") +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.5,1)
accPlot

accSimPlot + accPlot

#### Confidence Level 
experiment$choseSame = experiment$prior_key==experiment$discrimination_key
experiment$conf = experiment$conf/100

confSumm = experiment %>% dplyr::group_by(posteriorFactor, discrimination_is_correct, choseSame, ID) %>%
  dplyr::summarise(meanConf = mean(conf))
confSumm = confSumm %>% dplyr::group_by(posteriorFactor, discrimination_is_correct, choseSame) %>%
  dplyr::summarise(conf = mean(meanConf), sd=sd(meanConf))
confSumm$discrimination_is_correct = as.factor(confSumm$discrimination_is_correct)
levels(confSumm$discrimination_is_correct) = c("Incorrect","Correct")
confSumm$posteriorFactor = factor(confSumm$posteriorFactor, levels = c("L","M","H"))
confPlot = ggplot(confSumm, aes(x = posteriorFactor, y = conf, color = discrimination_is_correct, group=discrimination_is_correct)) +
  facet_wrap(~choseSame) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=conf-sd, ymax=conf+sd), width=.2,
                position=position_dodge(.4)) +
  labs(x="Posterior Information", y = "Mean Confidence", color="Response Accuracy",linetype="") +
  guides(group="none",alpha="none") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.6,0.9)
confPlot

confSimSumm = simDataAll %>% dplyr::group_by(posteriorFactor, discrimination_is_correct, choseSame) %>%
  dplyr::summarise(meanConf = mean(conf), se = sd(conf)/sqrt(length(conf)), sd=sd(conf))
confSimSumm$posteriorFactor = factor(confSimSumm$posteriorFactor, levels = c("L","M","H"))
confSimSumm$discrimination_is_correct = as.factor(confSimSumm$discrimination_is_correct)
levels(confSimSumm$discrimination_is_correct) = c("Incorrect", "Correct")

confPlotSim = ggplot(confSimSumm, aes(x = posteriorFactor, y = meanConf, color = discrimination_is_correct, group=discrimination_is_correct)) +
  facet_wrap(~choseSame) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(x="Posterior Information", y = "Mean Confidence", color="Response Accuracy",linetype="") +
  guides(group="none",alpha="none") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
 ylim(0.55,0.9)

confPlot + confPlotSim



