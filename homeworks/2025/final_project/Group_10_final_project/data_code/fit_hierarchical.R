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
baseDir = here::here("Stan/hierarchical")
resultDir = here::here("Results/modelFits/")
load(file=file.path(dataDir, "expFitNoiseRaw.RData"))
fitModel = TRUE

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##### Get real data for a subject to fit #####
experiment = exp_fitNoiseRaw

subjs = unique(experiment$ID)

# Exclude extreme outlier
subjs = subjs[c(1:8,10:21)]

i = 1
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
  
  ## Setup data
  coh1_data = data$adjCoh1
  coh2_data = data$adjCoh2
  conf_data = data$conf/100
  choice1_data = data$choice1
  choice_data = data$choice
  correct = data$discrimination_is_correct
  posteriorFactor = data$posteriorFactor
  condition = data$condition
  
  data$dir1 = ifelse(data$coherentDir1==1,"R","L")
  data$priorFact = "3"
  data$priorFact[data$priorLevel=="low"] = "1"
  data$priorFact[data$priorLevel=="medium"] = "2"
  data$setting1 = paste(data$dir1,data$priorFact,sep="")
  data$dir2 = ifelse(data$coherentDir2==1,"R","L")
  data$targetFact = "3"
  data$targetFact[data$targetLevel=="low"] = "1"
  data$targetFact[data$targetLevel=="medium"] = "2"
  data$setting2 = paste(data$dir2,data$targetFact,sep="")
  data$setting = paste(data$setting1,data$setting2,sep="")
  
  settings = c("R1R2","R1R3","R2R3","R2R1","R3R1","R3R2",
               "R1L2","R1L3","R2L3","L2R1","L3R1","L3R2",
               "L1R2","L1R3","L2R3","R2L1","R3L1","R3L2",
               "L1L2","L1L3","L2L3", "L2L1","L3L1","L3L2")
  ind = c(1:24)
  ind = (i-1)*24+ind
  
  match = function(x) ind[which(x==settings)]
  data$level = lapply(data$setting,match)
  
  integralcoh1s = c(right_coh[1],right_coh[1],right_coh[2],right_coh[2],right_coh[3],right_coh[3],
                    right_coh[1],right_coh[1],right_coh[2],left_coh[2],left_coh[3],left_coh[3],
                    left_coh[1],left_coh[1],left_coh[2],right_coh[2],right_coh[3],right_coh[3],
                    left_coh[1],left_coh[1],left_coh[2],left_coh[2],left_coh[3],left_coh[3])
  
  integralcoh2s = c(right_coh[2],right_coh[3],right_coh[3],right_coh[1],right_coh[1],right_coh[2],
                    left_coh[2],left_coh[3],left_coh[3],right_coh[1],right_coh[1],right_coh[2],
                    right_coh[2],right_coh[3],right_coh[3],left_coh[1],left_coh[1],left_coh[2],
                    left_coh[2],left_coh[3],left_coh[3],left_coh[1],left_coh[1],left_coh[2])
  
  levels = unlist(data$level,use.names = FALSE)
  
  subjData = data.frame(coh1_data, coh2_data, conf_data, choice1_data, choice_data, levels, correct, posteriorFactor, condition)
  subjData$id = i
  subjInts = data.frame(integralcoh1s, integralcoh2s)
  subjInts$id = i
  
  if(i==1){
    fullData = subjData
    fullInts = subjInts
  }else{
    fullData = rbind(fullData, subjData)
    fullInts = rbind(fullInts, subjInts)
  }
  i = i+1
}
  
N_data = length(fullData$id)
L_data = length(subjs)
ll_data = fullData$id
intmap_data = fullInts$id
coh1_data = fullData$coh1_data
coh2_data = fullData$coh2_data
choice1_data = fullData$choice1_data
choice_data = fullData$choice_data
conf_data = fullData$conf_data
levels = fullData$levels
coh1Ints = fullInts$integralcoh1s
coh2Ints = fullInts$integralcoh2s
posteriorFactor = fullData$posteriorFactor
condition = fullData$condition
correct = fullData$correct

if (fitModel){
  file <- file.path(baseDir, "fullModel_h_par_nor1r2.stan")
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  grainsize = (N_data/3)/2
  alldata <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)
  
  Nchains <- 3
  Niter <- 2000
  Nburn <- 2000
  
  parameters = c("m1_mu","m1_sd","m1","m2_mu","m2_sd","m2","b_mu","b_sd","b")
  
  initfun <- function() {
    list(m1_mu=runif(1,0.5,1.5), m1_sd=runif(1,0.5,1.5), m1=runif(L_data,0.5,1.5),
         m2_mu=runif(1,0.5,1.5), m2_sd=runif(1,0.5,1.5),m2=runif(L_data,0.5,1.5),
         b_mu=runif(1,0.5,1.5), b_sd=runif(1,0.5,1.5),b=runif(L_data,0.5,1.5)
    )
  }
  
  init = list(initfun(),initfun(),initfun())
  
  # Compile and run
  tic("model fitting")
  fit <- mod$sample(alldata,
                    chains = 3,
                    parallel_chains = 3,
                    threads_per_chain = 3,
                    refresh = 100, 
                    init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) 
  toc()
  
  summary = summarise_draws(bind_draws(fit$draws(variables=parameters), along = "chain"))
  fit$diagnostic_summary()
  diag = fit$sampler_diagnostics(format="df")
  mcmc_trace(fit$draws(variables=c("m2_mu","m2_sd")))
  
  stanfit <- rstan::read_stan_csv(fit$output_files())
  save(stanfit, file=file.path(resultDir, "full_h_stanfit_nor1r2_2000bi_2000s.RData"))
  params = rstan::summary(stanfit, pars=parameters,use_cache=FALSE)$summary
  save(params, file=file.path(resultDir, "full_h_summary_nor1r2_2000bi_2000s.RData"))
  
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m1_mu","m2_mu","b_mu"))
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m1_sd","m2_sd","b_sd"))
  
  m1_mu = rstan::extract(stanfit, "m1_mu", inc_warmup = FALSE,
                         include = TRUE)
  
  m2_mu = rstan::extract(stanfit, "m2_mu", inc_warmup = FALSE,
                         include = TRUE)
  
  b_mu = rstan::extract(stanfit, "b_mu", inc_warmup=FALSE,include=TRUE)
  
  posteriors = as.matrix(data.frame(wChoice_mu=unlist(m1_mu, use.names = FALSE), wConf_mu=unlist(m2_mu, use.names = FALSE),b_mu))
  save(posteriors, file=file.path(resultDir, "full_h_posteriors_nor1r2_2000bi_2000s.RData"))
  
}else{
  load(file.path(resultDir, "full_h_posteriors_nor1r2_2000bi_2000s.RData"))
  load(file.path(resultDir, "full_h_summary_nor1r2_2000bi_2000s.RData"))
}

ci(posteriors[,1], method = "HDI", ci=0.89) # wChoice_mu
ci(posteriors[,2], method = "HDI", ci=0.89) # wConf_mu
ci(posteriors[,3], method = "HDI", ci=0.89) # b_mu
ci(posteriors[,1]-posteriors[,2], method = "HDI", ci=0.89) # wChoice_mu - wConf_mu

mcmc_areas(posteriors,
           pars = c("wConf_mu", "b_mu"),
           prob = 0.89) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")

mcmc_areas(posteriors,
           pars = c("wChoice_mu"),
           prob = 0.89) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")

postdiff = as.matrix(data.frame(w_diff = posteriors[,1]-posteriors[,2]))
mcmc_areas(postdiff,
           pars = c("w_diff"),
           prob = 0.89) +
  geom_vline(xintercept=0, linetype="dashed", color="black", size=1)+
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")


####################### BOUNDED MODEL ######################################
if (fitModel){
  file <- file.path(baseDir, "fullModel_h_par_nor1r2_bounded.stan")
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  grainsize = (N_data/3)/2
  alldata <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)
  
  Nchains <- 3
  Niter <- 1000
  Nburn <- 2000
  
  parameters = c("m1_mu","m1_sd","m1","m2_mu","m2_sd","m2","b_mu","b_sd","b")
  
  initfun <- function() {
    list(m1_mu=runif(1,0.5,1.5), m1_sd=runif(1,0.5,1.5), m1=runif(L_data,0.5,1.5),
         m2_mu=runif(1,0.5,1.5), m2_sd=runif(1,0.5,1.5),m2=runif(L_data,0.5,1.5),
         b_mu=runif(1,0.5,1.5), b_sd=runif(1,0.5,1.5),b=runif(L_data,0.5,1.5)
    )
  }
  
  init = list(initfun(),initfun(),initfun())
  
  # Compile and run
  tic("model fitting")
  fit <- mod$sample(alldata,
                    chains = 3,
                    parallel_chains = 3,
                    threads_per_chain = 3,
                    refresh = 100, 
                    init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) 
  toc()
  
  summary = summarise_draws(bind_draws(fit$draws(variables=parameters), along = "chain"))
  fit$diagnostic_summary()
  diag = fit$sampler_diagnostics(format="df")
  mcmc_trace(fit$draws(variables=c("m2_mu","m2_sd")))
  
  stanfit <- rstan::read_stan_csv(fit$output_files())
  save(stanfit, file=file.path(resultDir, "full_h_stanfit_nor1r2_1000bi_2000s_bounded.RData"))
  params = summary(stanfit,pars=parameters,use_cache=FALSE)$summary
  save(params, file=file.path(resultDir, "full_h_summary_nor1r2_2000bi_2000s_bounded.RData"))
  
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m1_mu","m2_mu","b_mu"))
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m1_sd","m2_sd","b_sd"))
  
  m1_mu = rstan::extract(stanfit, "m1_mu", inc_warmup = FALSE,
                         include = TRUE)
  
  m2_mu = rstan::extract(stanfit, "m2_mu", inc_warmup = FALSE,
                         include = TRUE)
  
  b_mu = rstan::extract(stanfit, "b_mu", inc_warmup=FALSE,include=TRUE)
  
  posteriors = as.matrix(data.frame(wChoice_mu=unlist(m1_mu, use.names = FALSE), wConf_mu=unlist(m2_mu, use.names = FALSE),b_mu))
  save(posteriors, file=file.path(resultDir, "full_h_posteriors_nor1r2_2000bi_2000s_bounded.RData"))
  
}else{
  load(file.path(resultDir, "full_h_posteriors_nor1r2_2000bi_2000s_bounded.RData"))
  load(file.path(resultDir, "full_h_summary_nor1r2_2000bi_2000s_bounded.RData"))
}

ci(posteriors[,1], method = "HDI", ci=0.89) # wChoice_mu
ci(posteriors[,2], method = "HDI", ci=0.89) # wConf_mu
ci(posteriors[,3], method = "HDI", ci=0.89) # b_mu
ci(posteriors[,1]-posteriors[,2], method = "HDI", ci=0.89) # wChoice_mu - wConf_mu

mcmc_areas(posteriors,
           pars = c("wConf_mu", "b_mu"),
           prob = 0.89) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")

mcmc_areas(posteriors,
           pars = c("wChoice_mu"),
           prob = 0.89) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")

postdiff = as.matrix(data.frame(w_diff = posteriors[,1]-posteriors[,2]))
mcmc_areas(postdiff,
           pars = c("w_diff"),
           prob = 0.89) +
  geom_vline(xintercept=0, linetype="dashed", color="black", size=1)+
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")

##################################### EQUAL MODEL ##########################################

if (fitModel){
  file <- file.path(baseDir, "equalModel_h_par_nor1r2.stan")
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  grainsize = (N_data/3)/2
  alldata <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)
  
  Nchains <- 3
  Niter <- 2000
  Nburn <- 2000
  
  parameters = c("m_mu","m_sd","m","b_mu","b_sd","b")
  initfun <- function() {
    list(m_mu=runif(1,0.5,1.5), m_sd=runif(1,0.5,1.5), m=runif(L_data,0.5,1.5),
         b_mu=runif(1,0.5,1.5), b_sd=runif(1,0.5,1.5),b=runif(L_data,0.5,1.5)
    )
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
  fit$diagnostic_summary()
  diag = fit$sampler_diagnostics(format="df")
  mcmc_trace(fit$draws(variables=c("m_sd")))
  
  stanfit <- rstan::read_stan_csv(fit$output_files())
  save(stanfit, file=file.path(resultDir, "equal_h_stanfit_nor1r2.RData"))
  params = summary(stanfit,pars=parameters,use_cache=FALSE)$summary
  save(params, file=file.path(resultDir, "equal_h_summary_nor1r2_2000bi_2000s.RData"))
  
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m_mu","b_mu"))
  traceplot(stanfit, ask = T, inc_warmup=F, pars=c("m_sd","b_sd"))
  
  m_mu = rstan::extract(stanfit, "m_mu", inc_warmup = FALSE,
                         include = TRUE)

  b_mu = rstan::extract(stanfit, "b_mu", inc_warmup=FALSE,include=TRUE)
  
  posteriors = as.matrix(data.frame(w_mu=unlist(m_mu, use.names = FALSE),b_mu))
  save(posteriors, file=file.path(resultDir, "equal_h_posteriors_nor1r2_2000bi_2000s.RData"))
  
}else{
  load(file.path(resultDir, "equal_h_summary_nor1r2_2000bi_2000s.RData"))
  load(file.path(resultDir, "equal_h_posteriors_nor1r2_2000bi_2000s.RData"))
}

ci(posteriors[,1], method = "HDI", ci=0.89) # w_mu
ci(posteriors[,2], method = "HDI", ci=0.89) # b_mu

mcmc_areas(posteriors,
           pars = c("w_mu", "b_mu"),
           prob = 0.89) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")
