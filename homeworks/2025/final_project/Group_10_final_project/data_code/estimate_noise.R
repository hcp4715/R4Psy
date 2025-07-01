library(ggplot2)
library(patchwork)
library(plyr)
library(dplyr)
library(sjPlot)
library(here)
library(truncnorm)
library(mvtnorm)
library(coda)
library(mcmcr)
source(here::here("Functions/noiseEstimation_adapted.R"))

## For loading in and prepping data for fitting
dataPath = here::here("Data/")
load(file=file.path(dataPath, "stairData.RData"))
load(file=file.path(dataPath, "expClean.RData"))
experiment = exp_clean
staircase_df = stair_combined

for (subj in unique(experiment$ID)){
  experiment_df = experiment %>% dplyr::filter(ID==subj)
  staircase_df = stair_combined %>% dplyr::filter(ID==subj)
  
  stairAccs = staircase_df %>% dplyr::group_by(coherence) %>%
    dplyr::summarise(acc = sum(prior_is_correct)/length(prior_is_correct))
  
  #mean values from staircasing in experiment
  mean_low = mean(c(experiment_df$prior_coherence[which(experiment_df$priorLevel=="low")]))
  mean_medium = mean(c(experiment_df$prior_coherence[which(experiment_df$priorLevel=="medium")]))
  mean_high = mean(c(experiment_df$prior_coherence[which(experiment_df$priorLevel=="high")]))
  mean_low
  mean_medium
  mean_high
  
  experiment_df$meanCohStair = ifelse(experiment_df$priorLevel=="medium", mean_medium, mean_high)
  experiment_df$meanCohStair[which(experiment_df$priorLevel=="low")] = mean_low
  
  # To use the means of the staircases for fitting the psychometrics, use meanCohStair instead of prior_coherence
  d = data.frame(coherence = c(staircase_df$coherence, experiment_df$prior_coherence),
                 coherentDir = c(staircase_df$direction, experiment_df$prior_direction),
                 rr = c(staircase_df$prior_key=="right", experiment_df$prior_key=="right"))
  
  d$signed_coherence = d$coherence*-sign(d$coherentDir-1)
  d$rr = as.integer(d$rr)
  
  ## This will fit the psychometric based on both training sessions and the experiment, all difficulty levels
  fit = estimateNoise(d)
  
  d$correct = c(staircase_df$prior_is_correct, experiment_df$prior_is_correct)
  d$task = c(rep("control",length(staircase_df$prior_is_correct)), rep("exp", length(experiment_df$prior_is_correct)))
  d$choice = c(staircase_df$prior_key, experiment_df$prior_key)

  choices = d %>% dplyr::group_by(signed_coherence, task) %>%
    dplyr::summarise(choice = sum(choice=="right")/length(choice))
  
  lambda = fit[[3]]
  mu = fit[[2]]
  sigma = fit[[1]]
  
  # To visualize the psychometric fit - y axis is probability of choosing right and x is signed coherence (neg for left)
  if (!(is.na(lambda)||is.na(mu)||is.na(sigma))){
    sim = function(x){lambda + (1-2*lambda) * 0.5 * Re((1+erf((x-mu)/(sqrt(2)*sigma))))}
    x = seq(0.01,1,0.01)
    x = seq(-1,1,0.01)
    xSim = lapply(x, sim)
    xSim = unlist(xSim,use.names=F)
    fitFun = data.frame(x=x, y=xSim)
    
    fitPlot = ggplot(fitFun, aes(x=x, y=y)) +
      geom_line() +
      geom_point(data=choices, aes(x=signed_coherence,y=choice, color=task))
    
  }
  
  fitPlot
  
  experiment_df$fitNoiseRaw = sigma
  experiment_df$fitBiasRaw = mu
  experiment_df$fitLapseRaw = lambda
  
  if (subj == unique(experiment$ID)[1]){
    exp_fitNoiseRaw = experiment_df
  }else{
    exp_fitNoiseRaw = rbind(exp_fitNoiseRaw, experiment_df)
  }
  
}

exp_fitNoiseRaw$priorLevelInt = 1
exp_fitNoiseRaw$priorLevelInt[exp_fitNoiseRaw$priorLevel=="medium"] = 2
exp_fitNoiseRaw$priorLevelInt[exp_fitNoiseRaw$priorLevel=="high"] = 3
exp_fitNoiseRaw$targetLevelInt = 1
exp_fitNoiseRaw$targetLevelInt[exp_fitNoiseRaw$targetLevel=="medium"] = 2
exp_fitNoiseRaw$targetLevelInt[exp_fitNoiseRaw$targetLevel=="high"] = 3
exp_fitNoiseRaw$condition = ifelse(exp_fitNoiseRaw$priorLevelInt>exp_fitNoiseRaw$targetLevelInt,"prior","target")
exp_fitNoiseRaw$condition = as.factor(exp_fitNoiseRaw$condition)
exp_fitNoiseRaw$conf = as.integer(exp_fitNoiseRaw$confidence)

save(exp_fitNoiseRaw, file=file.path(dataPath, "expFitNoiseRaw.RData"))


