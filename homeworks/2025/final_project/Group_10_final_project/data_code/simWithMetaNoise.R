require(here)
require(dplyr)
library("patchwork")
library("ggplot2")
source(here::here("Functions/simulate_withMetaNoise.R"))
source(here::here("Functions/simulate_withLogNormNoise.R"))

m1 = 2.17 # To test if metaN can account for decision level, simulate m1 = 1
m2 = 2.17 # To test if metaN can account for asymmetry between decision and conf, simulate m2 = m1 (both 2.17)
b = 2.18
internalNoise <<- 1
metanoise <<- 1 # Amount of *added* metacognitive noise: 1 ~= mratio of 0.5,  0.5 ~= mratio of 0.7
mode_m <<- 2
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13

metaNpriorChoice=TRUE # adds metacognitive noise in estimating the prior for the target decision
metaNpriorConf=TRUE # adds metacognitive noise in estimating the prior for target confidence
metaNLikChoice=FALSE # adds metacognitive noise in estimating the likelihood for target confidence
metaNpriorChoiceDouble=FALSE # adds metacognitive noise twice due to translation noise back to the Type 1 space

simDataAll = simulateDistribution_withMetaNoise(m1, m2, b, internalNoise, right_coh, left_coh, 120000, FALSE, metanoise, metaNpriorChoice, metaNpriorConf, metaNLikChoice, metaNpriorChoiceDouble)
simDataAll$posteriorFactor = simDataAll$prec
simDataAll$discrimination_is_correct = simDataAll$correct2
simDataAll$conf = simDataAll$confTarget

accSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor) %>%
  dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct))
accSimSumm$model = "Optimal"
accSimSumm$posteriorFactor = factor(accSimSumm$posteriorFactor, levels = c("L","M","H"))
accSimSumm$condition = as.factor(accSimSumm$condition)
levels(accSimSumm$condition) = c("Stronger-Lead","Stronger-Target")

accPlot = ggplot(accSimSumm, aes(x = posteriorFactor, y = acc, color = interaction(condition), group=interaction(condition,model))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_alpha_discrete(range=c(1,0.6)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#E5D499","#bf9494"
  labs(x="Posterior Information", y = "Accuracy", color="Condition", linetype="") +
  guides(group="none",alpha="none") +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.6,1)

accPlot 

confSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
  dplyr::summarise(meanConf = mean(conf), se = sd(conf)/sqrt(length(conf)), sd=sd(conf))
confSimSumm$model = "Predicted"
confSimSumm$posteriorFactor = factor(confSimSumm$posteriorFactor, levels = c("L","M","H"))
confSimSumm$condition = as.factor(confSimSumm$condition)
levels(confSimSumm$condition) = c("Stronger-Lead","Stronger-Target")

confPlot = ggplot(confSimSumm, aes(x = posteriorFactor, y = meanConf, color = interaction(condition), shape = discrimination_is_correct, group=interaction(condition, discrimination_is_correct))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#FFEEB2","#bf9494"
  labs(x="Posterior Information", y = "Mean Confidence", color="Condition", shape="Response Accuracy",linetype="") +
  guides(group="none",alpha="none",shape="none") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.55,0.85)

confPlot 

accPlot + confPlot 

###### Simulate with log normal metacognitive noise
m1 = 2.17
m2 = 2.17
b = 2.18
internalNoise <<- 1
metanoise <<- 0.5
mode_m <<- 2
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13

simDataAll = simulateDistribution_withLogNormNoise(m1, m2, b, internalNoise, right_coh, left_coh, 120000, FALSE, metanoise, metaNpriorChoice, metaNpriorConf)
simDataAll$posteriorFactor = simDataAll$prec
simDataAll$discrimination_is_correct = simDataAll$correct2
simDataAll$conf = simDataAll$confTarget

accSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor) %>%
  dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct))
accSimSumm$model = "Optimal"
accSimSumm$posteriorFactor = factor(accSimSumm$posteriorFactor, levels = c("L","M","H"))
accSimSumm$condition = as.factor(accSimSumm$condition)
levels(accSimSumm$condition) = c("Stronger-Lead","Stronger-Target")

accPlot = ggplot(accSimSumm, aes(x = posteriorFactor, y = acc, color = interaction(condition), group=interaction(condition,model))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_alpha_discrete(range=c(1,0.6)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#E5D499","#bf9494"
  labs(x="Posterior Information", y = "Accuracy", color="Condition", linetype="") +
  guides(group="none",alpha="none") +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.6,1)

accPlot 

confSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
  dplyr::summarise(meanConf = mean(conf), se = sd(conf)/sqrt(length(conf)), sd=sd(conf))
confSimSumm$model = "Predicted"
confSimSumm$posteriorFactor = factor(confSimSumm$posteriorFactor, levels = c("L","M","H"))
confSimSumm$condition = as.factor(confSimSumm$condition)
levels(confSimSumm$condition) = c("Stronger-Lead","Stronger-Target")

confPlot = ggplot(confSimSumm, aes(x = posteriorFactor, y = meanConf, color = interaction(condition), shape = discrimination_is_correct, group=interaction(condition, discrimination_is_correct))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#FFEEB2","#bf9494"
  labs(x="Posterior Information", y = "Mean Confidence", color="Condition", shape="Response Accuracy",linetype="") +
  guides(group="none",alpha="none",shape="none") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(0.55,0.85)

confPlot 

accPlot + confPlot 
