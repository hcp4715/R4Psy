# 模n并绘制模型预测的结果

library(ggplot2)
library(patchwork)
library(dplyr)
library(here)
source(here::here("Functions/simulateDistribution_forPredictions.R")) # 用此函数生成模拟数据

mode_m <<- 2
diffPrior_all = c(seq(0.01,0.71,0.05),seq(0.21,0.91,0.05))
diffTarget_all = c(seq(0.21,0.91,0.05),seq(0.01,0.71,0.05))
# R的内存不够用，原本的样本量是100000，现在改为了10000
data_opt2 = simulateDistribution_forPredictions(2.17,1,1,1,diffPrior_all, diffTarget_all,10000)
data_over2 = simulateDistribution_forPredictions(2.17,0.33,1,1,diffPrior_all, diffTarget_all,10000)
data_under2 = simulateDistribution_forPrdictions(2.17,3,1,1,diffPrior_all, diffTarget_all,10000)
data_fit2 = simulateDistribution_forPredictions(2.17,1.27,1,1,diffPrior_all, diffTarget_all,10000)

data_opt1 = simulateDistribution_forPredictions(1,1,1,1,diffPrior_all, diffTarget_all, 1000)
data_over1 = simulateDistribution_forPredictions(0.33,1,1,1,diffPrior_all, diffTarget_all, 10000)
data_under1 = simulateDistribution_forPredictions(3,1,1,1,diffPrior_all, diffTarget_all, 10000)

data_opt2$weighting = "Optimal (wConf=1)"
data_over2$weighting = "Overweighted (wConf<1)"
data_under2$weighting = "Underweighted (wConf>1)"
data_fit2$weighting = "Less Underweighted (wConf<wChoice)"
data2 = rbind(data_opt2, data_over2, data_under2, data_fit2)
data2$precision = abs(data2$s1)+abs(data2$s2)
data2$condition = as.factor(data2$condition)
levels(data2$condition) = c("Lead","Target")
data2$correct2 = as.factor(data2$correct2)
levels(data2$correct2) = c("Incorrect", "Correct")
data2$weighting = factor(data2$weighting, levels=c("Optimal (wConf=1)","Overweighted (wConf<1)","Underweighted (wConf>1)","Less Underweighted (wConf<wChoice)"))

data_opt1$weighting = "Optimal (wChoice=1)"
data_over1$weighting = "Overweighted (wChoice<1)"
data_under1$weighting = "Underweighted (wChoice>1)"
data1 = rbind(data_opt1, data_over1, data_under1)
data1$precision = abs(data1$s1)+abs(data1$s2)
data1$condition = as.factor(data1$condition)
levels(data1$condition) = c("Lead","Target")

## Plot accuracy predictions
dataSumm1 = data1 %>% dplyr::group_by(precision, condition, weighting) %>%
  dplyr::summarize(acc = sum(correct2)/length(correct2))

dataSumm1$condition = as.factor(dataSumm1$condition)
levels(dataSumm1$condition) = c("Stronger-Lead", "Stronger-Target")
accPreds = ggplot(dataSumm1, aes(x = precision, y = acc, color=condition)) +
  geom_point(size=2, alpha=1) +
  #geom_smooth() +
  facet_wrap(~ weighting) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) +
  labs(x = "Posterior Information", y = "Accuracy", color = "Condition",strip="Weighting of Prior") +
  theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=14), axis.title.y = element_text(size=16),
        strip.text = element_text(size=14),
        legend.text = element_text(size=14), legend.title = element_text(size=16), legend.position = "bottom")
accPreds

### Plot confidence predictions
dataSumm2 = data2 %>% dplyr::group_by(precision, condition, correct2, weighting) %>%
  dplyr::summarize(confTarget = mean(confTarget))

dataSumm2$posterior = as.factor(dataSumm2$precision)
levels(dataSumm2$posterior) = c("L","M","H")
dataSumm2$sd = NA
dataSumm2$condition = as.factor(dataSumm2$condition)
levels(dataSumm2$condition) = c("Stronger-Lead", "Stronger-Target")

confPreds = ggplot(dataSumm2, aes(x = precision, y = confTarget, color=condition, shape=correct2)) +
  geom_point(size=2,alpha=1) +
  #geom_smooth() +
  facet_wrap(~ weighting, ncol=4) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) + 
  labs(x = "Posterior Information", y = "Mean Confidence", color = "Condition",shape="", strip ="Weighting of Prior") +
  theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=14), axis.title.y = element_text(size=16),
        strip.text = element_text(size=14),
        legend.text = element_text(size=14), legend.title = element_text(size=16), legend.position = "bottom")
confPreds
