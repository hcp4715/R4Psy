library(ggplot2)
library(patchwork)
library(plyr)
library(dplyr)
library(here)
library(truncnorm)
library(mvtnorm)
library(coda)
library(mcmcr)
source(here::here("Functions/noiseEstimation_exp3.R"))

fitPsychometric <- function(staircase, experiment)
{
  experiment_df = experiment
  staircase_df = staircase
  experiment_df$coherence = experiment_df$target_data$coherence
  experiment_df$coherent_direction = experiment_df$target_data$coherent_direction
  
  expNoPrior = experiment_df %>% dplyr::filter(priorLevel==50)
  expNoPrior$meanCoh = mean(expNoPrior$coherence)
  
  d = data.frame(coherence = c(staircase_df$coherence, expNoPrior$coherence),
                 coherentDir = c(staircase_df$direction, expNoPrior$coherent_direction),
                 rr = c(staircase_df$prior_key=="right", expNoPrior$discrimination_key=="right"))
  
  d$signed_coherence = d$coherence*-sign(d$coherentDir-1)
  d$rr = as.integer(d$rr)
  
  ## This will fit the psychometric based on both training sessions and the experiment, all difficulty levels
  fit = estimateNoise(d)
  
  d$correct = c(staircase_df$prior_is_correct, expNoPrior$discrimination_is_correct)
  d$task = c(rep("control",length(staircase_df$prior_is_correct)), rep("exp", length(expNoPrior$discrimination_is_correct)))
  d$choice = c(staircase_df$prior_key, expNoPrior$discrimination_key)
  
  choices = d %>% dplyr::group_by(signed_coherence, task) %>%
    dplyr::summarise(choice = sum(choice=="right")/length(choice))
  
  lambda = round(fit[[3]],3)
  mu = round(fit[[2]],3)
  sigma = round(fit[[1]],3)
  
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
  
  return(experiment_df)
}
