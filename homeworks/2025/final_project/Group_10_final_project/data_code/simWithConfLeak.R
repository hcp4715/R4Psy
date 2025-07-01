require(here)
require(dplyr)
library("ggplot2")
source(here::here("Functions/simulate_withConfLeak.R"))

m1 = 2.17 # confidence leak will have no impact on the decision level
m2 = 2.17 # to test if it can explain the asymmetry, simulate m2 = m1
b = 2.18
internalNoise <<- 1
mode_m <<- 2
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13
clFact = 2 # this controls the strength of the conf leak, with lower numbers being stronger
metanoise = 0 # to test conf leak alone, set to 0

metaNpriorChoice=TRUE # adds metacognitive noise in estimating the prior for the target decision
metaNpriorConf=TRUE # adds metacognitive noise in estimating the prior for target confidence
metaNLikChoice=FALSE # adds metacognitive noise in estimating the likelihood for target confidence
metaNpriorChoiceDouble=FALSE # adds metacognitive noise twice due to translation noise back to the Type 1 space

simDataAll = simulateDistribution_withConfLeak(m1, m2, b, internalNoise, right_coh, left_coh, 120000, FALSE, metanoise, metaNpriorChoice, metaNpriorConf, metaNLikChoice, metaNpriorChoiceDouble, clFact=clFact)

simDataAll$posteriorFactor = simDataAll$prec
simDataAll$discrimination_is_correct = simDataAll$correct2
simDataAll$conf = simDataAll$confTarget


confSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
  dplyr::summarise(meanConf = mean(conf), se = sd(conf)/sqrt(length(conf)), sd=sd(conf))
confSimSumm$model = "Predicted"
confSimSumm$posteriorFactor = factor(confSimSumm$posteriorFactor, levels = c("L","M","H"))
confPlot = ggplot(confSimSumm, aes(x = posteriorFactor, y = meanConf, color = interaction(condition), shape = discrimination_is_correct, group=interaction(condition, discrimination_is_correct))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#FFEEB2","#bf9494"
  labs(x="Posterior Information", y = "Mean Confidence", color="More Precise", shape="Response Accuracy",linetype="") +
  guides(group="none",alpha="none") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "none") +
 ylim(0.55,0.8)
confPlot

accSimSumm = simDataAll %>% dplyr::group_by(condition, posteriorFactor) %>%
  dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct))
accSimSumm$model = "Optimal"
accSimSumm$posteriorFactor = factor(accSimSumm$posteriorFactor, levels = c("L","M","H"))

accPlot = ggplot(accSimSumm, aes(x = posteriorFactor, y = acc, color = interaction(condition), group=interaction(condition,model))) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_alpha_discrete(range=c(1,0.6)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + #, "#E5D499","#bf9494"
  labs(x="Posterior Information", y = "Accuracy", color="More Precise", linetype="") +
  guides(group="none",alpha="none") +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "none") +
  ylim(0.6,1)
accPlot

accPlot + confPlot

