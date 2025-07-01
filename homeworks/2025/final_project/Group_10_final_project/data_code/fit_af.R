require(rstan)
require(shinystan)
require(parallel)
require(loo)
require(here)
require(dplyr)
library("ggplot2")
library("bayesplot")
library("rstanarm")
library("tictoc")
library(posterior)
library(cmdstanr)
library(bayestestR)
dataDir = here::here("Data/")
baseDir = here::here("Stan")
resultDir = here::here("Results/")
plotDir = here::here("Results/Plots")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))
load(file=paste(dataDir, "stairData.RData", sep=""))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##### Get real data for a subject to fit #####
control = stair_combined
experiment = exp_fitNoiseRaw

subjs = unique(experiment$ID)
control = control %>% dplyr::filter(ID %in% subjs)

i = 1
for (subj in subjs){
  subjExp = experiment %>% dplyr::filter(ID==subj)
  data = control %>% dplyr::filter(ID==subj)
  data$fitBiasRaw = subjExp$fitBiasRaw[1]
  data$fitNoiseRaw = subjExp$fitNoiseRaw[1]
  
  # Adjust their raw coherence values 
  data$cohDir = -sign(data$direction-1)
  data$signedCoh = data$coherence*data$cohDir
  
  data$adjCoh = (data$signedCoh-data$fitBiasRaw)/data$fitNoiseRaw
  
  data$choice = 1
  data$choice[data$prior_key=="left"] = 0
  
  ## Setup data
  coh_data = data$adjCoh
  choice_data = data$choice
  
  subjData = data.frame(coh_data, choice_data)
  subjData$id = subj
  
  if(i==1){
    fullData = subjData
  }else{
    fullData = rbind(fullData, subjData)
  }
  i = i+1
}

fullDataAll = fullData

N_data = length(fullData$id)
L_data = length(subjs)
coh_data = fullData$coh_data
choice_data = fullData$choice_data

file <- file.path(baseDir, "fit_af.stan")
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

alldata <- list(N=N_data, coh=coh_data, choice=choice_data)

Nchains <- 3
Niter <- 1000
Nburn <- 1000

# Compile and run
tic("model fitting")
fit <- mod$sample(alldata,
                  chains = 3,
                  parallel_chains = 3,
                  threads_per_chain = 3,
                  refresh = 100, 
                  iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
toc()

parameters = c("af")
summary = summarise_draws(bind_draws(fit$draws(variables=parameters), along = "chain"))
summary
fit$diagnostic_summary()
diag = fit$sampler_diagnostics(format="df")
mcmc_trace(fit$draws(variables=c("af")))

