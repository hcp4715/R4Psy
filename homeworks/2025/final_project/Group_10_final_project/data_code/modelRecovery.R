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
library(patchwork)
source(here::here("Functions/simulateDistribution_means.R"))
dataDir = here::here("Data/")
baseDir = here::here("Stan/individual")
resultDir = here::here("Results/modelComparison/")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
run = FALSE

right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13

internalNoise <<- 1
mode_m <<- 2
nreps = 10

## To redo model recovery, give settings and run the following for each true model
if (run){

  m1 = 1 # optimal = 1; equal = 1.5; flat = Inf; fullm1>m2 = 2; fullm1<m2 = 1
  m2 = 2 # optimal = 1; equal = 1.5; flat = Inf; fullm1>m2 = 1; fullm1<m2 = 2
  b = 1.5
  truemodel = "Full_m1<m2" # Full_m1>m2, Optimal, Equal, Flat
  
  for (i in 1:nreps){
    simData = simulateDistribution_means(m1, m2, b, internalNoise, right_coh, left_coh, reps=120)
    
    Ntrials = length(simData$s)
    N_data = Ntrials
    coh1_data = simData$s1
    coh2_data = simData$s2
    conf_data = simData$confTarget
    choice1_data = as.integer(simData$choseRight1)
    choice_data = as.integer(simData$choseRight2)
    
    simData$dir1 = ifelse(simData$prior_direction==1,"R","L")
    simData$priorFact = as.character(simData$priorLevel)
    simData$setting1 = paste(simData$dir1,simData$priorFact,sep="")
    simData$dir2 = ifelse(simData$target_direction==1,"R","L")
    simData$targetFact = as.character(simData$targetLevel)
    simData$setting2 = paste(simData$dir2,simData$targetFact,sep="")
    simData$setting = paste(simData$setting1,simData$setting2,sep="")
    
    settings = c("R1R2","R1R3","R2R3","R2R1","R3R1","R3R2",
                 "R1L2","R1L3","R2L3","L2R1","L3R1","L3R2",
                 "L1R2","L1R3","L2R3","R2L1","R3L1","R3L2",
                 "L1L2","L1L3","L2L3","L2L1","L3L1","L3L2")
    ind = c(1:24)
    
    match = function(x) ind[which(x==settings)]
    simData$level = lapply(simData$setting,match)
    
    integralcoh1s = c(right_coh[1],right_coh[1],right_coh[2],right_coh[2],right_coh[3],right_coh[3],
                      right_coh[1],right_coh[1],right_coh[2],left_coh[2],left_coh[3],left_coh[3],
                      left_coh[1],left_coh[1],left_coh[2],right_coh[2],right_coh[3],right_coh[3],
                      left_coh[1],left_coh[1],left_coh[2],left_coh[2],left_coh[3],left_coh[3])
    
    integralcoh2s = c(right_coh[2],right_coh[3],right_coh[3],right_coh[1],right_coh[1],right_coh[2],
                      left_coh[2],left_coh[3],left_coh[3],right_coh[1],right_coh[1],right_coh[2],
                      right_coh[2],right_coh[3],right_coh[3],left_coh[1],left_coh[1],left_coh[2],
                      left_coh[2],left_coh[3],left_coh[3],left_coh[1],left_coh[1],left_coh[2])
    
    levels = unlist(simData$level,use.names = FALSE)
    
    ## Full Model
    file <- file.path(baseDir, "fullModel_ind_nor1r2.stan")
    mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    grainsize = 1
    alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints = length(integralcoh1s), coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
    
    Nchains <- 3
    Niter <- 1000
    Nburn <- 1000
    
    parameters = c("m1","m2","b")
    initfun <- function() {
      list(m1=runif(1,0.5,1.5),m2=runif(1,0.5,1.5),b=runif(1,0.5,1.5))
    }
    
    init = list(initfun(),initfun(),initfun())
    
    # Compile and run
    tic("model fitting")
    fit <- mod$sample(alldata,
                      chains = 3,
                      parallel_chains = 3,
                      threads_per_chain = 3,
                      refresh = 100, 
                      init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    summary = summarise_draws(bind_draws(fit$draws(variables=parameters), along = "chain"))
    
    stanfit <- rstan::read_stan_csv(fit$output_files())
    log_lik = extract_log_lik(stanfit, parameter_name = "log_lik", merge_chains = FALSE)
    r_eff <- relative_eff(exp(log_lik), cores = 2) 
    loo <- loo(log_lik, r_eff = r_eff)
    
    ## Equal Model
    file <- file.path(baseDir, "equalModel_ind_nor1r2.stan")
    mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    grainsize = 1
    alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints = length(integralcoh1s),coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
    
    Nchains <- 3
    Niter <- 1000
    Nburn <- 1000
    
    parameters = c("m","b")
    initfun <- function() {
      list(m=runif(1,0.5,1.5),b=runif(1,0.5,1.5))
    }
    
    init = list(initfun(),initfun(),initfun())
    
    # Compile and run
    tic("model fitting")
    fit_eq <- mod$sample(alldata,
                         chains = 3,
                         parallel_chains = 3,
                         threads_per_chain = 3,
                         refresh = 100, 
                         init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    summary_eq = summarise_draws(bind_draws(fit_eq$draws(variables=parameters), along = "chain"))
    
    stanfit_eq <- rstan::read_stan_csv(fit_eq$output_files())
    log_lik_eq = extract_log_lik(stanfit_eq, parameter_name = "log_lik", merge_chains = FALSE)
    r_eff_eq <- relative_eff(exp(log_lik_eq), cores = 2) 
    loo_eq <- loo(log_lik_eq, r_eff = r_eff_eq)
    
    ## Optimal Model
    file <- file.path(baseDir, "optimalModel_ind_par_nor1r2.stan")
    mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    grainsize = 1
    alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints = length(integralcoh1s), coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
    
    Nchains <- 3
    Niter <- 1000
    Nburn <- 1000
    
    parameters = c("b")
    initfun <- function() {
      list(b=runif(1,0.5,1.5))
    }
    
    init = list(initfun(),initfun(),initfun())
    
    # Compile and run
    tic("model fitting")
    fit_opt <- mod$sample(alldata,
                          chains = 3,
                          parallel_chains = 3,
                          threads_per_chain = 3,
                          refresh = 100, 
                          init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    summary_opt = summarise_draws(bind_draws(fit_opt$draws(variables=parameters), along = "chain"))
    
    stanfit_opt <- rstan::read_stan_csv(fit_opt$output_files())
    log_lik_opt = extract_log_lik(stanfit_opt, parameter_name = "log_lik", merge_chains = FALSE)
    r_eff_opt <- relative_eff(exp(log_lik_opt), cores = 2) 
    loo_opt <- loo(log_lik_opt, r_eff = r_eff_opt)
    
    ## No Prior Model
    file <- file.path(baseDir, "noPrior_ind_par_nor1r2.stan")
    mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    grainsize = 1
    alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints=length(integralcoh1s),coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
    
    Nchains <- 3
    Niter <- 1000
    Nburn <- 1000
    
    parameters = c("b")
    initfun <- function() {
      list(b=runif(1,0.5,1.5))
    }
    
    init = list(initfun(),initfun(),initfun())
    
    # Compile and run
    tic("model fitting")
    fit_flat <- mod$sample(alldata,
                           chains = 3,
                           parallel_chains = 3,
                           threads_per_chain = 3,
                           refresh = 100, 
                           init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    summary_flat = summarise_draws(bind_draws(fit_flat$draws(variables=parameters), along = "chain"))
    
    stanfit_flat <- rstan::read_stan_csv(fit_flat$output_files())
    log_lik_flat = extract_log_lik(stanfit_flat, parameter_name = "log_lik", merge_chains = FALSE)
    r_eff_flat <- relative_eff(exp(log_lik_flat), cores = 2) 
    loo_flat <- loo(log_lik_flat, r_eff = r_eff_flat)
    
    if (truemodel=="Optimal"){
      vsFull = loo_compare(loo_opt, loo)
      vsFull = as.data.frame(vsFull)
      vsFull$model = "Optimal"
      vsFull$model[which(row.names(vsFull)=="model2")] = "Full"
      
      vsEq = loo_compare(loo_opt, loo_eq)
      vsEq = as.data.frame(vsEq)
      vsEq$model = "Optimal"
      vsEq$model[which(row.names(vsEq)=="model2")] = "Equal"
      
      vsFlat = loo_compare(loo_opt, loo_flat)
      vsFlat = as.data.frame(vsFlat)
      vsFlat$model = "Optimal"
      vsFlat$model[which(row.names(vsFlat)=="model2")] = "Flat"
      
      if (i == 1){
        summaryAll = summary
        summaryEqAll = summary_eq
        summaryOptAll = summary_opt
        summaryFlatAll = summary_flat
        vsFullAll = vsFull
        vsEqAll = vsEq
        vsFlatAll = vsFlat
      }else{
        summaryAll = rbind(summaryAll, summary)
        summaryEqAll = rbind(summaryEqAll, summary_eq)
        summaryOptAll = rbind(summaryOptAll, summary_opt)
        summaryFlatAll = rbind(summaryFlatAll, summary_flat)
        vsFullAll = rbind(vsFullAll, vsFull)
        vsEqAll = rbind(vsEqAll, vsEq)
        vsFlatAll = rbind(vsFlatAll, vsFlat)
      }
    }else if(truemodel=="Equal"){
      vsFull = loo_compare(loo_eq, loo)
      vsFull = as.data.frame(vsFull)
      vsFull$model = "Equal"
      vsFull$model[which(row.names(vsFull)=="model2")] = "Full"
      
      vsOpt = loo_compare(loo_eq, loo_opt)
      vsOpt = as.data.frame(vsOpt)
      vsOpt$model = "Equal"
      vsOpt$model[which(row.names(vsOpt)=="model2")] = "Optimal"
      
      vsFlat = loo_compare(loo_eq, loo_flat)
      vsFlat = as.data.frame(vsFlat)
      vsFlat$model = "Equal"
      vsFlat$model[which(row.names(vsFlat)=="model2")] = "Flat"
      
      if (i == 1){
        summaryAll = summary
        summaryEqAll = summary_eq
        summaryOptAll = summary_opt
        summaryFlatAll = summary_flat
        vsFullAll = vsFull
        vsOptAll = vsOpt
        vsFlatAll = vsFlat
      }else{
        summaryAll = rbind(summaryAll, summary)
        summaryEqAll = rbind(summaryEqAll, summary_eq)
        summaryOptAll = rbind(summaryOptAll, summary_opt)
        summaryFlatAll = rbind(summaryFlatAll, summary_flat)
        vsFullAll = rbind(vsFullAll, vsFull)
        vsOptAll = rbind(vsOptAll, vsOpt)
        vsFlatAll = rbind(vsFlatAll, vsFlat)
      }
    }else if(truemodel=="Flat"){
      vsFull = loo_compare(loo_flat, loo)
      vsFull = as.data.frame(vsFull)
      vsFull$model = "Flat"
      vsFull$model[which(row.names(vsFull)=="model2")] = "Full"
      
      vsOpt = loo_compare(loo_flat, loo_opt)
      vsOpt = as.data.frame(vsOpt)
      vsOpt$model = "Flat"
      vsOpt$model[which(row.names(vsOpt)=="model2")] = "Optimal"
      
      vsEq = loo_compare(loo_flat, loo_eq)
      vsEq = as.data.frame(vsEq)
      vsEq$model = "Flat"
      vsEq$model[which(row.names(vsEq)=="model2")] = "Equal"
      
      if (i == 1){
        summaryAll = summary
        summaryEqAll = summary_eq
        summaryOptAll = summary_opt
        summaryFlatAll = summary_flat
        vsFullAll = vsFull
        vsOptAll = vsOpt
        vsEqAll = vsEq
      }else{
        summaryAll = rbind(summaryAll, summary)
        summaryEqAll = rbind(summaryEqAll, summary_eq)
        summaryOptAll = rbind(summaryOptAll, summary_opt)
        summaryFlatAll = rbind(summaryFlatAll, summary_flat)
        vsFullAll = rbind(vsFullAll, vsFull)
        vsOptAll = rbind(vsOptAll, vsOpt)
        vsEqAll = rbind(vsEqAll, vsEq)
      }
    }else if(truemodel=="Full_m1>m2"){
      vsEq = loo_compare(loo, loo_eq)
      vsEq = as.data.frame(vsEq)
      vsEq$model = "Full_m1>m2"
      vsEq$model[which(row.names(vsEq)=="model2")] = "Equal"
      
      vsOpt = loo_compare(loo, loo_opt)
      vsOpt = as.data.frame(vsOpt)
      vsOpt$model = "Full_m1>m2"
      vsOpt$model[which(row.names(vsOpt)=="model2")] = "Optimal"
      
      vsFlat = loo_compare(loo, loo_flat)
      vsFlat = as.data.frame(vsFlat)
      vsFlat$model = "Full_m1>m2"
      vsFlat$model[which(row.names(vsFlat)=="model2")] = "Flat"
      
      if (i == 1){
        summaryAll = summary
        summaryEqAll = summary_eq
        summaryOptAll = summary_opt
        summaryFlatAll = summary_flat
        vsEqAll = vsEq
        vsOptAll = vsOpt
        vsFlatAll = vsFlat
      }else{
        summaryAll = rbind(summaryAll, summary)
        summaryEqAll = rbind(summaryEqAll, summary_eq)
        summaryOptAll = rbind(summaryOptAll, summary_opt)
        summaryFlatAll = rbind(summaryFlatAll, summary_flat)
        vsEqAll = rbind(vsEqAll, vsEq)
        vsOptAll = rbind(vsOptAll, vsOpt)
        vsFlatAll = rbind(vsFlatAll, vsFlat)
      }
    }else if(truemodel=="Full_m1<m2"){
      vsEq = loo_compare(loo, loo_eq)
      vsEq = as.data.frame(vsEq)
      vsEq$model = "Full_m1<m2"
      vsEq$model[which(row.names(vsEq)=="model2")] = "Equal"
      
      vsOpt = loo_compare(loo, loo_opt)
      vsOpt = as.data.frame(vsOpt)
      vsOpt$model = "Full_m1<m2"
      vsOpt$model[which(row.names(vsOpt)=="model2")] = "Optimal"
      
      vsFlat = loo_compare(loo, loo_flat)
      vsFlat = as.data.frame(vsFlat)
      vsFlat$model = "Full_m1<m2"
      vsFlat$model[which(row.names(vsFlat)=="model2")] = "Flat"
      
      if (i == 1){
        summaryAll = summary
        summaryEqAll = summary_eq
        summaryOptAll = summary_opt
        summaryFlatAll = summary_flat
        vsEqAll = vsEq
        vsOptAll = vsOpt
        vsFlatAll = vsFlat
      }else{
        summaryAll = rbind(summaryAll, summary)
        summaryEqAll = rbind(summaryEqAll, summary_eq)
        summaryOptAll = rbind(summaryOptAll, summary_opt)
        summaryFlatAll = rbind(summaryFlatAll, summary_flat)
        vsEqAll = rbind(vsEqAll, vsEq)
        vsOptAll = rbind(vsOptAll, vsOpt)
        vsFlatAll = rbind(vsFlatAll, vsFlat)
      }
    }    
    
  }
  
  if (truemodel=="Optimal"){
    modelRecovery = rbind(vsFullAll, vsEqAll, vsFlatAll)
    modelRecovery$trueModel = "Optimal"
    pairs = sort(rep(c(1:(length(modelRecovery$trueModel)/2)),2))
    modelRecovery$pairs = pairs 
    modelRecovery$won = 0
    modelRecovery$won[modelRecovery$elpd_diff==0] = 1
    modelRecoveryOpt = modelRecovery
    save(modelRecoveryOpt, file=paste(resultDir, "optimalModelRecoveryAll.RData",sep=""))
  }else if (truemodel=="Equal"){
    modelRecovery = rbind(vsFullAll, vsOptAll, vsFlatAll)
    modelRecovery$trueModel = "Equal"
    pairs = sort(rep(c(1:(length(modelRecovery$trueModel)/2)),2))
    modelRecovery$pairs = pairs 
    modelRecovery$won = 0
    modelRecovery$won[modelRecovery$elpd_diff==0] = 1
    modelRecoveryEq = modelRecovery
    save(modelRecoveryEq, file=paste(resultDir, "equalModelRecoveryAll.RData",sep=""))
  }else if (truemodel=="Flat"){
    modelRecovery = rbind(vsFullAll, vsOptAll, vsEqAll)
    modelRecovery$trueModel = "Flat"
    pairs = sort(rep(c(1:(length(modelRecovery$trueModel)/2)),2))
    modelRecovery$pairs = pairs 
    modelRecovery$won = 0
    modelRecovery$won[modelRecovery$elpd_diff==0] = 1
    modelRecoveryFlat = modelRecovery
    save(modelRecoveryFlat, file=paste(resultDir, "flatModelRecoveryAll.RData",sep=""))
  }else if (truemodel=="Full_m1>m2"){
    modelRecovery = rbind(vsEqAll, vsOptAll, vsFlatAll)
    modelRecovery$trueModel = "Full_m1>m2"
    pairs = rep(c(1:(length(modelRecovery$trueModel)/2)),2)
    pairs = sort(pairs)
    modelRecovery$pairs = pairs 
    modelRecovery$won = 0
    modelRecovery$won[modelRecovery$elpd_diff==0] = 1
    modelRecoveryFull = modelRecovery
    save(modelRecoveryFull, file=paste(resultDir, "full_m1>m2ModelRecoveryAll.RData",sep=""))
  }else if (truemodel=="Full_m1<m2"){
    modelRecovery = rbind(vsEqAll, vsOptAll, vsFlatAll)
    modelRecovery$trueModel = "Full_m1<m2"
    pairs = rep(c(1:(length(modelRecovery$trueModel)/2)),2)
    pairs = sort(pairs)
    modelRecovery$pairs = pairs 
    modelRecovery$won = 0
    modelRecovery$won[modelRecovery$elpd_diff==0] = 1
    modelRecoveryFull2 = modelRecovery
    save(modelRecoveryFull2, file=paste(resultDir, "full_m1<m2ModelRecoveryAll.RData",sep=""))
  }
  modelRecoveryAll = rbind(modelRecoveryFull, modelRecoveryFull2, modelRecoveryEq, modelRecoveryOpt, modelRecoveryFlat)
  save(modelRecoveryAll, file=paste(resultDir, "modelRecoveryAll.RData",sep=""))
}else{
  load(paste(resultDir, "modelRecoveryAll.RData",sep=""))
}

winning = modelRecoveryAll %>% dplyr::filter(elpd_diff == 0)
losing = modelRecoveryAll %>% dplyr::filter(elpd_diff != 0)
winning$elpd_diff = losing$elpd_diff
winning$se_diff = losing$se_diff
winning$elpd_diff[which(winning$model!=winning$trueModel)] = -1*winning$elpd_diff[which(winning$model!=winning$trueModel)]
recoveryForPlotting = rbind(winning, losing)

recoveryForPlotting = recoveryForPlotting %>% dplyr::filter(trueModel!=model)
recoveryForPlotting$elpd_diff[recoveryForPlotting$model==recoveryForPlotting$trueModel] = -1*recoveryForPlotting$elpd_diff[recoveryForPlotting$model==recoveryForPlotting$trueModel]
recoveryForPlotting$diffInSE = recoveryForPlotting$elpd_diff/recoveryForPlotting$se_diff
recoveryForPlotting$diffLarge = "Yes"
recoveryForPlotting$diffLarge[abs(recoveryForPlotting$elpd_diff)<4] = "No"
recoveryForPlotting$model[recoveryForPlotting$model=="Full"] = "Flexible"

recoveryFull = recoveryForPlotting %>% dplyr::filter(trueModel=="Full_m1>m2")
if (sum(recoveryFull$diffLarge=="No")==0){
  values=c("#D1E3ED","darkgray")
}else{
  values=c("darkgray", "#D1E3ED")
}
full = ggplot(recoveryFull, aes(x=model,y=diffInSE,group=pairs,fill=diffLarge)) +
  geom_bar(stat="identity", width=0.4,position=position_dodge(width=0.5)) +
  scale_fill_manual(values=values) +
  labs(title="Flexible Model (wChoice>wConf)", x="Model",y="elpd_diff / se_diff") +
  guides(fill="none") +
  theme_light()
full

recoveryFull2 = recoveryForPlotting %>% dplyr::filter(trueModel=="Full_m1<m2")
if (sum(recoveryFull2$diffLarge=="No")==0){
  values=c("#D1E3ED","darkgray")
}else{
  values=c("darkgray", "#D1E3ED")
}
full2 = ggplot(recoveryFull2, aes(x=model,y=diffInSE,group=pairs,fill=diffLarge)) +
  geom_bar(stat="identity", width=0.4,position=position_dodge(width=0.5)) +
  scale_fill_manual(values=values) +
  labs(title="Flexible Model (wChoice<wConf)", x="Model",y="elpd_diff / se_diff") +
  guides(fill="none") +
  theme_light()
full2

recoveryEq = recoveryForPlotting %>% dplyr::filter(trueModel=="Equal")
if (sum(recoveryEq$diffLarge=="No")==0){
  values=c("#D1E3ED","darkgray")
}else{
  values=c("darkgray", "#D1E3ED")
}
eq = ggplot(recoveryEq, aes(x=model,y=diffInSE,group=pairs,fill=diffLarge)) +
  geom_bar(stat="identity", width=0.4,position=position_dodge(width=0.5)) +
  scale_fill_manual(values=values) +
  labs(title="Equal", x="Model",y="elpd_diff / se_diff") +
  guides(fill="none") +
  theme_light()
eq

recoveryOpt = recoveryForPlotting %>% dplyr::filter(trueModel=="Optimal")
if (sum(recoveryOpt$diffLarge=="No")==0){
  values=c("#D1E3ED","darkgray")
}else{
  values=c("darkgray", "#D1E3ED")
}
opt = ggplot(recoveryOpt, aes(x=model,y=diffInSE,group=pairs,fill=diffLarge)) +
  geom_bar(stat="identity", width=0.4,position=position_dodge(width=0.5)) +
  scale_fill_manual(values=values) +
  labs(title="Optimal", x="Model",y="elpd_diff / se_diff") +
  guides(fill="none") +
  theme_light()
opt

recoveryFlat = recoveryForPlotting %>% dplyr::filter(trueModel=="Flat")
if (sum(recoveryFlat$diffLarge=="No")==0){
  values=c("#D1E3ED","darkgray")
}else{
  values=c("darkgray", "#D1E3ED")
}
flat = ggplot(recoveryFlat, aes(x=model,y=diffInSE,group=pairs,fill=diffLarge)) +
  geom_bar(stat="identity", width=0.4,position=position_dodge(width=0.5)) +
  scale_fill_manual(values=values) +
  labs(title="Flat", x="Model",y="elpd_diff / se_diff") +
  guides(fill="none") +
  theme_light()
flat

full + full2 + eq + opt + flat


