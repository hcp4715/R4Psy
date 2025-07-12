library(metaSDT)
library(ggplot2)
library(patchwork)
library(dplyr)
library(here)
library(RColorBrewer)
library(distributional)
library(ggdist)
library(raincloudplots)
source(here::here("Functions/Function_trials2counts.R"))

run = TRUE

#8M3BVB and QI6I5D ruled out due to Type 1 HR and FAR from pooled analysis (L, M, H) 

dataPath = here::here("Data/")
resultsDir = here::here("Results/modelFree/")

load(file=file.path(dataPath, "expFitNoiseRaw.RData"))
exp_fitNoiseRaw$confidence = as.integer(exp_fitNoiseRaw$confidence)
subjs = unique(exp_fitNoiseRaw$ID)

if (run){
  for (subj in subjs){
    experiment = exp_fitNoiseRaw[exp_fitNoiseRaw$ID==subj,]
    subjID = subj
    
    confQuant = quantile(experiment$confidence, probs=seq(0,1,0.2))
    
    experiment$confQuant = 5
    experiment$confQuant[experiment$confidence<=confQuant[5]] = 4
    experiment$confQuant[experiment$confidence<=confQuant[4]] = 3
    experiment$confQuant[experiment$confidence<=confQuant[3]] = 2
    experiment$confQuant[experiment$confidence<=confQuant[2]] = 1 
    
    priorData = experiment %>% dplyr::filter(condition=="prior")
    targetData = experiment %>% dplyr::filter(condition=="target")
    
    priorData$stimulus = as.integer(priorData$correct_answer_target=="right")
    priorData$response = as.integer(priorData$discrimination_key=="right")
    
    targetData$stimulus = as.integer(targetData$correct_answer_target=="right")
    targetData$response = as.integer(targetData$discrimination_key=="right")
    
    
    nRatings = 5
    
    priorQuants = trials2counts(priorData$stimulus,
                                priorData$response,
                                priorData$confQuant,
                                nRatings = nRatings)
    
    targetQuants = trials2counts(targetData$stimulus,
                                 targetData$response,
                                 targetData$confQuant,
                                 nRatings = nRatings)
    
    nR_S1_prior <- priorQuants[[1]]
    nR_S2_prior <- priorQuants[[2]]
    
    nR_S1_target <- targetQuants[[1]]
    nR_S2_target <- targetQuants[[2]]
    
    priorFit_quant = fit_meta_d_SSE(nR_S1_prior, nR_S2_prior, s=1, add_constant=TRUE)
    targetFit_quant =  fit_meta_d_SSE(nR_S1_target, nR_S2_target, s=1, add_constant=TRUE)
    
    subjFits = data.frame(id = subj, priorMRatio = priorFit_quant$M_ratio[1], targetMRatio = targetFit_quant$M_ratio[1]) 
    
    if (subj == subjs[1]){
      mratios = subjFits  
    }else{
      mratios = rbind(mratios, subjFits)
    }
    
  }
  save(mratios, file=paste(resultsDir, "mratioFits_5Bins_subjectwise.RData", sep=""))
}else{
  load(file.path(resultsDir, "mratioFits_5Bins_subjectwise.RData"))
}

mratios_clean = mratios %>% dplyr::filter(!(id %in% c("8M3BVB","QI6I5D","LKERDX","9HEKVK")))
mean(mratios_clean$priorMRatio)
sd(mratios_clean$priorMRatio)
mean(mratios_clean$targetMRatio)
sd(mratios_clean$targetMRatio)
print(mratios_clean)
ttest = t.test(mratios_clean$priorMRatio,mratios_clean$targetMRatio,paired=TRUE)

dodge <- position_dodge(width = 0.8)
mratios_plotting = data.frame(mratio = c(mratios_clean$priorMRatio, mratios_clean$targetMRatio), condition = c(rep("Stronger-Lead",length(mratios_clean$priorMRatio)), rep("Stronger-Target", length(mratios_clean$targetMRatio))))
ggplot(mratios_plotting, aes(x=condition, y=mratio, fill=condition)) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.3, position=dodge, alpha=1, trim=FALSE) +
  stat_dotsinterval(side = "left", scale = 0.3, slab_size = NA, quantiles = length(mratios_clean$id), position=dodge, show_interval=TRUE) +
  #geom_boxplot(width=0.06) +
  scale_fill_manual(values=c("#F4CC08", "#C73030")) +
  labs(x = "Condition", y = "M-Ratio") +
  theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=14), axis.title.y = element_text(size=16), 
        legend.position = "none")
