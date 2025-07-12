require(here)
require(dplyr)
library("ggplot2")
library(patchwork)
source(here::here("Functions/simulateDistribution.R"))

m1 = 1 
m2 = 1
b = 2
internalNoise <<- 1
metanoise <<- 1
mode_m <<- 2
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13

dataPath = here::here("Data/")
load(file=paste(dataPath,"expFitNoiseRaw.RData",sep=""))
experiment = exp_fitNoiseRaw

experiment$correctFact = as.factor(experiment$discrimination_is_correct)
experiment$ID = as.factor(experiment$ID)
experiment$posteriorFactor = as.factor(experiment$posteriorFactor)
experiment$posteriorFactor = ordered(experiment$posteriorFactor, levels=c("L","M","H"))
experiment$condition = as.factor(experiment$condition)
experiment$congruent = experiment$correct_answer_target=="right"

targAccPerCond_perSubj = experiment %>% dplyr::group_by(ID, priorLevel, targetLevel, congruent) %>%
  dplyr::summarize(priorAcc = sum(prior_is_correct)/length(prior_is_correct),targetAcc = sum(discrimination_is_correct)/length(discrimination_is_correct))
targAccPerCond_perSubj$priorLevel = ordered(targAccPerCond_perSubj$priorLevel, levels = c("low", "medium", "high"))
targAccPerCond_perSubj$targetLevel = ordered(targAccPerCond_perSubj$targetLevel, levels = c("low", "medium", "high"))

conditionAccSumm = targAccPerCond_perSubj %>% dplyr::group_by(congruent, priorLevel, targetLevel) %>%
  dplyr::summarise(mean = mean(targetAcc), sd=sd(targetAcc))

dodge <- position_dodge(width = 0.5)
nudge <- position_nudge(x = 0.1, y = 0)

accPlot = ggplot(conditionAccSumm, aes(x=priorLevel, y = mean, color=congruent, group=interaction(targetLevel, congruent))) +
  facet_wrap(~targetLevel) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0)) +
  geom_line(size=1) + 
  scale_fill_manual(values=c("#F4CC08", "#C73030")) + 
  labs(x = "Prior Level", y = "Target Decision Accuracy", color = "Congruent") +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.3,1)
accPlot

conf_perSubj_Cond = experiment %>% dplyr::group_by(ID, priorLevel, targetLevel, congruent, discrimination_is_correct) %>%
  dplyr::summarize(meanConf = mean(conf))

conf_perSubj_Cond$discrimination_is_correct = as.factor(conf_perSubj_Cond$discrimination_is_correct)
levels(conf_perSubj_Cond$discrimination_is_correct) = c("Incorrect", "Correct")
conf_perSubj_Cond$priorLevel = ordered(conf_perSubj_Cond$priorLevel, levels = c("low", "medium", "high"))
conf_perSubj_Cond$targetLevel = ordered(conf_perSubj_Cond$targetLevel, levels = c("low", "medium", "high"))
conf_perSubj_Cond$meanConf = conf_perSubj_Cond$meanConf/100

confSumm = conf_perSubj_Cond %>% dplyr::group_by(discrimination_is_correct, congruent, priorLevel, targetLevel) %>%
  dplyr::summarise(mean = mean(meanConf), sd = sd(meanConf))

dodge = position_dodge(width=0.8)
confPlot = ggplot(confSumm, aes(x=priorLevel, y = mean, color=congruent, shape=discrimination_is_correct, linetype=discrimination_is_correct, group=interaction(congruent, discrimination_is_correct))) +
  facet_wrap(~targetLevel)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.5), linetype="solid") +
  geom_line(size=1)+
  scale_linetype_manual(values=c("dotdash","solid")) +
  labs(x = "Prior Level", y = "Mean Confidence", color = "Prior-Congruent", shape="Response Accuracy", linetype="") +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), 
        strip.text = element_text(size=12),legend.position = "bottom") 
confPlot


simDataAll = simulateDistribution(m1, m2, b, internalNoise, right_coh, left_coh, 120000)
simDataAll$posteriorFactor = simDataAll$prec
simDataAll$discrimination_is_correct = simDataAll$correct2
simDataAll$conf = simDataAll$confTarget
simDataAll$congruent = simDataAll$target_direction==1

targAccPerCond_perCong = simDataAll %>% dplyr::group_by(priorLevel, targetLevel, congruent) %>%
  dplyr::summarize(priorAcc = sum(correct1)/length(correct1),targetAcc = sum(correct2)/length(correct2))
targAccPerCond_perCong$priorLevel = as.factor(targAccPerCond_perCong$priorLevel)
targAccPerCond_perCong$targetLevel = as.factor(targAccPerCond_perCong$targetLevel)
levels(targAccPerCond_perCong$priorLevel) = c("low", "medium", "high")
levels(targAccPerCond_perCong$targetLevel) = c("low","medium","high")
targAccPerCond_perCong$priorLevel = ordered(targAccPerCond_perCong$priorLevel, levels = c("low", "medium", "high"))
targAccPerCond_perCong$targetLevel = ordered(targAccPerCond_perCong$targetLevel, levels = c("low", "medium", "high"))

dodge <- position_dodge(width = 0.5)
nudge <- position_nudge(x = 0.1, y = 0)
accSimPlot = ggplot(targAccPerCond_perCong, aes(x=priorLevel, y = targetAcc, color=congruent, group=interaction(targetLevel, congruent))) +
  facet_wrap(~targetLevel) +
  geom_point(size=3) +
  geom_line(size=1) + 
  scale_fill_manual(values=c("#F4CC08", "#C73030")) + 
  labs(x = "Prior Level", y = "Target Decision Accuracy", color = "Congruent") +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.3,1)
accSimPlot

accPlot + accSimPlot

conf_perSubj_Cond = simDataAll %>% dplyr::group_by(priorLevel, targetLevel, congruent, discrimination_is_correct) %>%
  dplyr::summarize(meanConf = mean(conf))

conf_perSubj_Cond$discrimination_is_correct = as.factor(conf_perSubj_Cond$discrimination_is_correct)
levels(conf_perSubj_Cond$discrimination_is_correct) = c("Incorrect", "Correct")
conf_perSubj_Cond$priorLevel = as.factor(conf_perSubj_Cond$priorLevel)
conf_perSubj_Cond$targetLevel = as.factor(conf_perSubj_Cond$targetLevel)
levels(conf_perSubj_Cond$priorLevel) = c("low", "medium", "high")
levels(conf_perSubj_Cond$targetLevel) = c("low","medium","high")
conf_perSubj_Cond$priorLevel = ordered(conf_perSubj_Cond$priorLevel, levels = c("low", "medium", "high"))
conf_perSubj_Cond$targetLevel = ordered(conf_perSubj_Cond$targetLevel, levels = c("low", "medium", "high"))

dodge = position_dodge(width=0.8)
confSimPlot = ggplot(conf_perSubj_Cond, aes(x=priorLevel, y = meanConf, color=congruent, shape=discrimination_is_correct, linetype=discrimination_is_correct, group=interaction(congruent, discrimination_is_correct))) +
  facet_wrap(~targetLevel)+
  geom_point(size=3)+
  geom_line(size=1)+
  scale_linetype_manual(values=c("dotdash","solid")) +
  labs(x = "Prior Level", y = "Mean Confidence", color = "Congruent", shape="Response Accuracy") +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), 
        strip.text = element_text(size=12),legend.position = "none") +
  ylim(0.5,0.9)
confSimPlot

confPlot + confSimPlot
