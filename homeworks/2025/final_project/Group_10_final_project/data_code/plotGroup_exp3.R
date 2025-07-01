library(ggplot2)
library(patchwork)
library(tidyr)
library(tidyverse)
library(dplyr)
library(sjPlot) # 统计模型可视化
library(here)
library(lme4) # 线性混合效应模型
library(metaSDT) # 元认知信号检测论
library(rstan) # 贝叶斯统计建模
# 导入自定义的函数
source(here::here("simulateProbabilityTask.R"))
source(here::here("fitPsychometric_exp3.R"))
source(here::here("Functions/Function_trials2counts.R"))

dataPath = here::here("Data/exp3/")
baseDir = here::here("Stan/exp3/")

# 加载数据
load(file=file.path(dataPath,'expFitNoiseRaw.RData'))

# 数据预处理：排除特点被试并初始化变量
IDs = unique(expDataAll$ID) # 获取所有被试ID
exclude = c("5WKGPD","HFUPCA","78M8N9") # 要排除的被试ID
rightCohs = c() # 向右刺激coherence存储向量
leftCohs = c() # 向左刺激coherence存储向量
IDs = IDs[!IDs %in% exclude] # 排除指定被试

# 循环处理每个被试的数据
for (i in 1:length(IDs)){
  mysubj = IDs[i] # 当前处理的被试ID
  
  # 筛选出当前处理被试
  expFitNoise = expDataAll %>% dplyr::filter(ID==mysubj)
  
  # 计算向左|右刺激的平均coherence
  right_coh = mean(expFitNoise$adjCoh[expFitNoise$coherentDir>0]) 
  left_coh =  mean(expFitNoise$adjCoh[expFitNoise$coherentDir<0])
  
  # 存储当前被试的coherence
  rightCohs = c(rightCohs, right_coh)
  leftCohs = c(leftCohs, left_coh)
  
  # 获取当前被试的模型参数
  addNoise = expFitNoise$addNoise[1]
  fitm1 = expFitNoise$m1[1] # 决策中的先验权重参数
  fitm2 = expFitNoise$m2[1] # 信心中的先验权重参数
  fitb = expFitNoise$b[1] # 信心偏差参数
  
  # 设置全局变量（用于模拟数据）
  mode_m <<- 2
  internalNoise <<- 1
  
  # 提取先验强度值（转换为比例）
  priorRs = unique(expFitNoise$priorRight)/100
  
  # 生成不同模型下的模拟数据——用拟合得到的参数生成数据
  # 最优模型，决策、信心和b都为1
  opt = simulateProbTask(1, 1, 1, internalNoise, right_coh, left_coh, priorRs, reps=10000, addNoise)
  # 添加上被试ID
  opt$ID = mysubj
  
  # wChoice=fitm1, wConf=1, b=1
  optConf = simulateProbTask(fitm1, 1, fitb, internalNoise, right_coh, left_coh, priorRs, reps=10000, addNoise)
  optConf$ID = mysubj
  
  # 相等模型
  sameConf = simulateProbTask(fitm1, fitm1, fitb, internalNoise, right_coh, left_coh, priorRs, reps=10000, addNoise)
  sameConf$ID = mysubj
  
  # 完整模型？
  fit = simulateProbTask(fitm1, fitm2, fitb, internalNoise, right_coh, left_coh, priorRs, reps=10000, addNoise)
  fit$ID = mysubj
  
  if (i==1){
    optAll = opt # 全部都最优
    optConfAll = optConf #信心最优
    sameConfAll = sameConf # 权重相等
    fitAll = fit # 拟合全部参数
  }else{
    optAll = rbind(optAll, opt)
    optConfAll = rbind(optConfAll, optConf)
    sameConfAll = rbind(sameConfAll, sameConf)
    fitAll = rbind(fitAll, fit)
  }
}

## Plot against optimal 
# 先验强度水平
priorRs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

# 处理实验条件：计算每个先验水平下选右的概率
probR = expDataAll %>% dplyr::group_by(priorRight, ID) %>%
  dplyr::summarise(prob = sum(discrimination_key=="right")/length(discrimination_key=="right"))
probR$priorRight = probR$priorRight/100 # 转换为比例
probR$priorRight = as.factor(probR$priorRight) # 转换为因子类型

# 计算实验数据的描述性统计
probRSumm = probR %>% dplyr::group_by(priorRight) %>%
  dplyr::summarise(mean = mean(prob), se=sd(prob)/sqrt(length(IDs)))
probRSumm$model = "Data" # 标记为实验数据

# 处理最优模型模拟数据
optAll$priorFactor = optAll$prior
optAll$priorFactor = as.factor(optAll$priorFactor)
optAll$conf = optAll$confTarget
optR = optAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(probR = sum(choseRight)/length(choseRight))

# 计算最优模型模拟数据的描述性统计
optSumm = optR %>% dplyr::group_by(priorFactor) %>% 
  dplyr::summarise(mean = mean(probR), se=sd(probR)/sqrt(length(IDs)))
optSumm$model = "Optimal" # 标记为最优模型

# 处理全拟合模型的模拟数据
fitAll$priorFactor = fitAll$prior
fitAll$priorFactor = as.factor(fitAll$priorFactor)
fitAll$conf = fitAll$confTarget
fitR = fitAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(probR = sum(choseRight)/length(choseRight))

# 计算其描述性统计
fitSumm = fitR %>% dplyr::group_by(priorFactor) %>% 
  dplyr::summarise(mean = mean(probR), se=sd(probR)/sqrt(length(IDs)))
fitSumm$model = "Fit" # 标记为fit

# 合并所有数据用于绘图 
probSumm = data.frame(priorRight = c(probRSumm$priorRight, optSumm$priorFactor, fitSumm$priorFactor), 
                      probR = c(probRSumm$mean, optSumm$mean, fitSumm$mean),
                      se = c(probRSumm$se, optSumm$se, fitSumm$se),
                      model = c(probRSumm$model, optSumm$model, fitSumm$model))

probRPlot = ggplot(probSumm, aes(x = priorRight, y = probR, color=model, group=model)) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=probR-se, ymax=probR+se), width=.2,
                position=position_dodge(0)) +
  labs(x="Prior Strength", y = "P('Right')", color="", linetype="") +
  guides(group="none",alpha="none") +
  scale_color_manual(values=c("#9E9E9E","#F8766D","#45888C")) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") 
print(probRPlot)

## Confidence 的计算与choice类似
expDataAll$priorFactor = as.factor(expDataAll$priorRight/100) # 将先验强度转换为因子类型
expDataAll$choseRight = expDataAll$discrimination_key=="right" # 标记选择右侧

# 处理最优模型信心数据
optConfAll$priorFactor = optConfAll$prior
optConfAll$priorFactor = as.factor(optConfAll$priorFactor)
optConfAll$conf = optConfAll$confTarget

# 处理相等模型信心数据
sameConfAll$priorFactor = sameConfAll$prior
sameConfAll$priorFactor = as.factor(sameConfAll$priorFactor)
sameConfAll$conf = sameConfAll$confTarget

# 处理实验数据：计算每个先验水平下的平均正确率
confDataAcc = expDataAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(meanConf = mean(conf))

# 计算基线信心（先验强度为0.5时的信心）
baseline = confDataAcc %>% dplyr::filter(priorFactor==0.5) %>% 
  dplyr::summarise(baseline = meanConf, ID=ID)
baseline = baseline[,c(2,3)]
confDataAcc = merge(confDataAcc, baseline, by="ID") # 合并基线数据
confDataAcc$confDiff = confDataAcc$meanConf - confDataAcc$baseline # 计算与基线的差异

# 计算实验数据信心的描述性统计
confDataSummAcc = confDataAcc %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(mean = mean(meanConf), meanDiff = mean(confDiff), se=sd(meanConf)/sqrt(length(IDs)), seDiff=sd(confDiff)/sqrt(length(IDs)))
confDataSummAcc$model = "Data" # 标记为实验数据

# 处理最优模型信心数据
optCAcc = optConfAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(meanConf = mean(confTarget))
# 计算最优模型的基线信心
baseline = optCAcc %>% dplyr::filter(priorFactor==0.5) %>% 
  dplyr::summarise(baseline = meanConf, ID=ID)
baseline = baseline[,c(2,3)]
optCAcc = merge(optCAcc, baseline, by="ID") # 合并基线数据
optCAcc$confDiff = optCAcc$meanConf - optCAcc$baseline # 计算与基线的差异

# 计算最优模型信心的描述性统计
optCSummAcc = optCAcc %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(mean = mean(meanConf), meanDiff = mean(confDiff), se=sd(meanConf)/sqrt(length(IDs)),seDiff=sd(confDiff)/sqrt(length(IDs)))
optCSummAcc$model = "Optimal" # 标记为最优模型

# 处理相等模型信心数据
sameCAcc = sameConfAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(meanConf = mean(confTarget))
# 计算相等模型的基线信心
baseline = sameCAcc %>% dplyr::filter(priorFactor==0.5) %>% 
  dplyr::summarise(baseline = meanConf, ID=ID)
baseline = baseline[,c(2,3)]
sameCAcc = merge(sameCAcc, baseline, by="ID") # 合并基线数据
sameCAcc$confDiff = sameCAcc$meanConf - sameCAcc$baseline # 计算与基线的差异

# 计算相等模型信心的描述性统计
sameCSummAcc = sameCAcc %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(mean = mean(meanConf), meanDiff = mean(confDiff), se=sd(meanConf)/sqrt(length(IDs)),seDiff=sd(confDiff)/sqrt(length(IDs)))
sameCSummAcc$model = "Equal" # 标记

# 处理全拟合模型的信心数据
fitCAcc = fitAll %>% dplyr::group_by(priorFactor, ID) %>%
  dplyr::summarise(meanConf = mean(confTarget))
baseline = fitCAcc %>% dplyr::filter(priorFactor==0.5) %>% 
  dplyr::summarise(baseline = meanConf, ID=ID)
# 计算全拟合模型的基线信心
baseline = baseline[,c(2,3)]
fitCAcc = merge(fitCAcc, baseline, by="ID") # 合并
fitCAcc$confDiff = fitCAcc$meanConf - fitCAcc$baseline # 计算差异

# 计算全拟合模型信心的描述性统计
fitCSummAcc = fitCAcc %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(mean = mean(meanConf), meanDiff = mean(confDiff), se=sd(meanConf)/sqrt(length(IDs)),seDiff=sd(confDiff)/sqrt(length(IDs)))
fitCSummAcc$model = "Fit" # 标记

# 合并所用数据，用于绘图
confSummAcc = rbind(optCSummAcc, sameCSummAcc, fitCSummAcc, confDataSummAcc)
confSummAcc = rbind(fitCSummAcc, sameCSummAcc, confDataSummAcc)

confDiffPlot = ggplot(confSummAcc, aes(x = priorFactor, y = meanDiff, color = model,  group=interaction(model))) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=meanDiff-seDiff, ymax=meanDiff+seDiff), width=.2,
                position=position_dodge(0)) +
  labs(x="Prior Strength", y = "Mean Conf Difference to Baseline", color="", shape="Response Accuracy",linetype="") +
  guides(group="none",alpha="none") + 
  scale_color_manual(values=c("#9E9E9E", "#45888C","#F8766D")) + # #115D97
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(-0.1,0.3) 
print(confDiffPlot)

#### Just Simulations
meanAddNoise = mean(expDataAll$addNoise) # 计算平均附加噪声
right_coh = 0.45 # 向右刺激的coherence
left_coh = -0.45 # 向左刺激的coherence
addNoise = 0 # 附加噪声设置为0

# 不同先验权重下的模拟
# 最优模型
opt = simulateProbTask(1, 1, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)

opt$priorFactor = opt$prior
opt$priorFactor = as.factor(opt$priorFactor)
opt$conf = opt$confTarget
optSumm = opt %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(probR = sum(choseRight)/length(choseRight))
optSumm$model = "Optimal"

# wChoice被低估
over = simulateProbTask(0.5, 1, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)

over$priorFactor = over$prior
over$priorFactor = as.factor(over$priorFactor)
over$conf = over$confTarget
overSumm = over %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(probR = sum(choseRight)/length(choseRight))
overSumm$model = "Overweighted"

# wChoice被高估
under = simulateProbTask(2, 1, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)

under$priorFactor = under$prior
under$priorFactor = as.factor(under$priorFactor)
under$conf = under$confTarget
underSumm = under %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(probR = sum(choseRight)/length(choseRight))
underSumm$model = "Underweighted"

# 合并数据
simSummary = rbind(optSumm, overSumm, underSumm)
probRSims = ggplot(simSummary, aes(x = priorFactor, y = probR, color = model, group=model)) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(x="Prior Strength", y = "P('Right')", color="", linetype="") +
  guides(group="none",alpha="none") +
  scale_color_manual(values=c("#45888C", "#422664", "#BDD353")) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") 
probRSims

print(probRSims + probRPlot)

### Equal, Over, Under
optC = simulateProbTask(2, 2, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)
optC$priorFactor = optC$prior
optC$priorFactor = as.factor(optC$priorFactor)
optC$conf = optC$confTarget
optCSumm = optC %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(meanConf = mean(confTarget))
optCSumm$baseline = optCSumm$meanConf[optCSumm$priorFactor==0.5]
optCSumm$confDiff = optCSumm$meanConf - optCSumm$baseline
optCSumm$model = "Equal"

overC = simulateProbTask(2, 1, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)
overC$priorFactor = overC$prior
overC$priorFactor = as.factor(overC$priorFactor)
overC$conf = overC$confTarget
overCSumm = overC %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(meanConf = mean(confTarget))
overCSumm$baseline = overCSumm$meanConf[overCSumm$priorFactor==0.5]
overCSumm$confDiff = overCSumm$meanConf - overCSumm$baseline
overCSumm$model = "Stronger"

underC = simulateProbTask(2, 4, 1, internalNoise, right_coh, left_coh, priorRs, reps=12000, addNoise)
underC$priorFactor = underC$prior
underC$priorFactor = as.factor(underC$priorFactor)
underC$conf = underC$confTarget
underCSumm = underC %>% dplyr::group_by(priorFactor) %>%
  dplyr::summarise(meanConf = mean(confTarget))
underCSumm$baseline = underCSumm$meanConf[underCSumm$priorFactor==0.5]
underCSumm$confDiff = underCSumm$meanConf - underCSumm$baseline
underCSumm$model = "Weaker"

simConfSummary = rbind(optCSumm, overCSumm, underCSumm)
simConfSummary$model = ordered(simConfSummary$model, levels = c("Weaker", "Stronger", "Equal"))

confSims = ggplot(simConfSummary, aes(x = priorFactor, y = confDiff, color = model, group=interaction(model))) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(x="Prior Strength", y = "Mean Conf Difference to Baseline", color="",linetype="") +
  guides(group="none",alpha="none") + 
  scale_color_manual(values=c("#BDD353","#422664","#45888C")) + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom") +
  ylim(-0.1,0.3)

confSims

print(confSims + confDiffPlot)

