library(ggplot2)
library(patchwork)
library(dplyr)
library(here)
source(here::here("Functions/simulateDistribution.R"))
dataDir = here::here("Data/")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))
modelFitDir = here::here("Results/modelFits/")
resultDir = here::here("Results/ppc/")
load(file=paste(modelFitDir, "full_h_posteriors_nor1r2_2000bi_2000s.RData", sep=""))

runSims = FALSE
sample = TRUE
nSamples = 10
if (sample==FALSE){
  nSamples = 1
}
mode_m <<- 2
m1 = 2.17
b = 2.18
m2 = 1.27

if (runSims){
  for (x in 1:nSamples){
    if (sample){
      m1  = sample(posteriors[,1],1)
      m2 = sample(posteriors[,2],1)
      b = sample(posteriors[,3],1)
    }
    
    subjs = unique(exp_fitNoiseRaw$ID)
    experiment = exp_fitNoiseRaw
    experiment$conf = experiment$conf/100
    for (subj in subjs){
      data = experiment %>% dplyr::filter(ID==subj)
      
      # Adjust their raw coherence values 
      data$coherentDir1 = -sign(data$prior_direction-1)
      data$signedCoh1 = data$prior_coherence*data$coherentDir1
      data$adjCoh1 = (data$signedCoh1-data$fitBiasRaw)/data$fitNoiseRaw
      data$coherentDir2 = -sign(data$target_direction-1)
      data$signedCoh2 = data$target_coherence*data$coherentDir2
      data$adjCoh2 = (data$signedCoh2-data$fitBiasRaw)/data$fitNoiseRaw
      data$choice = 1
      data$choice[data$discrimination_key=="left"] = 0
      data$choice1 = 1
      data$choice1[data$prior_key=="left"] = 0
      
      ## This takes the mean coherences per precision level (so, per staircase) 
      means = data %>% dplyr::group_by(priorLevel, prior_direction) %>%
        dplyr::summarize(meanCohs = mean(adjCoh1))
      means$priorLevel = factor(means$priorLevel, levels=c("low","medium","high"))
      means = means[order(means$priorLevel),]
      
      right_coh = means$meanCohs[means$prior_direction==0]
      left_coh = means$meanCohs[means$prior_direction==180]
      
      simData = simulateDistribution(m1,m2,b,1,right_coh,left_coh,120)
      
      simData$posteriorFactor = simData$prec
      simData$discrimination_is_correct = simData$correct2
      simData$conf = simData$confTarget
      modelSumm = simData %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
        dplyr::summarise(confidence = mean(conf))
      modelSumm$sim = x
      modelSumm$ID = subj
      
      modelAcc = simData %>% dplyr::group_by(condition, posteriorFactor) %>%
        dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct))
      modelAcc$sim = x
      modelAcc$ID = subj
      
      if (subj == subjs[1] & x == 1){
        all_right = right_coh
        all_left = left_coh
        modelSummAll = modelSumm
        modelAccAll = modelAcc
      }else{
        all_right = c(all_right, right_coh)
        all_left = c(all_left, left_coh)
        modelSummAll = rbind(modelSummAll, modelSumm)
        modelAccAll = rbind(modelAccAll, modelAcc)
      }
    }
    
  }

  save(modelSummAll, file=paste(resultDir,"confSimsFromSamples_1000s.RData",sep=""))
  save(modelAccAll, file=paste(resultDir,"accSimsFromSamples_1000s.RData",sep=""))

}else{
  load(paste(resultDir,"confSimsFromSamples_1000s.RData",sep=""))
  load(paste(resultDir,"accSimsFromSamples_1000s.RData",sep=""))
}

dataSumm = experiment %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct, ID) %>%
  dplyr::summarise(confidence = mean(conf))
dataSumm = dataSumm %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
  dplyr::summarise(meanConf = mean(confidence), sd = sd(confidence))
dataSumm$model = "Data"
dataSumm$posteriorFactor = factor(dataSumm$posteriorFactor, levels = c("L","M","H"))

modelSumm = modelSummAll %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct, ID) %>%
  dplyr::summarise(conf = mean(confidence))
modelSumm = modelSumm %>% dplyr::group_by(condition, posteriorFactor, discrimination_is_correct) %>%
  dplyr::summarise(meanConf = mean(conf), sd = sd(conf))
modelSumm$model = "Predicted"
modelSumm$posteriorFactor = factor(dataSumm$posteriorFactor, levels = c("L","M","H"))

summary = rbind(modelSumm, dataSumm)
summary$condition = as.factor(summary$condition)
summary$discrimination_is_correct = as.factor(summary$discrimination_is_correct)
levels(summary$condition) = c("Stronger-Cue","Stronger-Target")
levels(summary$discrimination_is_correct) = c("Incorrect","Correct")
confFitPlot = ggplot(summary, aes(x = posteriorFactor, y = meanConf, color = condition, shape = discrimination_is_correct, group=interaction(condition, discrimination_is_correct))) +
  facet_wrap(~model) +
  geom_line(size=1) +
  geom_point(size=3) +
  # geom_violin() +
  geom_errorbar(aes(ymin=meanConf-sd, ymax=meanConf+sd), width=.2,
                position=position_dodge(.4)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) +
  labs(x="Posterior Information", y = "Mean Confidence", color="Condition", shape="") +
  theme(axis.text.y = element_text(size=16), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=16), axis.title.x = element_text(size=18), strip.text = element_text(size=18), 
        legend.text = element_text(size=18), legend.title = element_text(size=18), legend.position = "bottom") +
  ylim(0.55,0.9) 
confFitPlot


dataAcc = experiment %>% dplyr::group_by(condition, posteriorFactor, ID) %>%
  dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct))
dataAcc = dataAcc %>% dplyr::group_by(condition, posteriorFactor) %>%
  dplyr::summarise(meanAcc = mean(acc), sd=sd(acc))
dataAcc$model = "Data"
dataAcc$posteriorFactor = factor(dataAcc$posteriorFactor, levels = c("L","M","H"))

modelAcc = modelAccAll %>% dplyr::group_by(condition, posteriorFactor, ID) %>%
  dplyr::summarise(accuracy = mean(acc))
modelAcc = modelAcc %>% dplyr::group_by(condition, posteriorFactor) %>%
  dplyr::summarise(meanAcc = mean(accuracy), sd=sd(accuracy))
modelAcc$model = "Predicted"
modelAcc$posteriorFactor = factor(modelAcc$posteriorFactor, levels = c("L","M","H"))

summary = rbind(modelAcc, dataAcc)
summary$condition = as.factor(summary$condition)
levels(summary$condition) = c("Stronger-Cue","Stronger-Target")
accFitPlot = ggplot(summary, aes(x = posteriorFactor, y = meanAcc, color = condition, group=condition)) +
  facet_wrap(~model) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=meanAcc-sd, ymax=meanAcc+sd), width=.2,
                position=position_dodge(.4)) +
  scale_color_manual(values=c("#F4CC08", "#C73030")) +
  labs(x="Posterior Information", y = "Accuracy", color="Condition") +
  theme(axis.text.y = element_text(size=16), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=16), axis.title.x = element_text(size=18), strip.text = element_text(size=18), 
        legend.text = element_text(size=18), legend.title = element_text(size=18), legend.position = "bottom") +
  ylim(0.6,0.95) 
accFitPlot


