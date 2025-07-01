require(rstan)
require(shinystan)
require(parallel)
require(loo)
require(here)
require(dplyr)
library("bayesplot")
library("rstanarm")
library("ggplot2")
library("tictoc")
library("cmdstanr")
library("posterior")
dataDir = here::here("Data/")
baseDir = here::here("Stan/individual")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

nonsimplified = FALSE
##### Get real data for a subject to fit #####
experiment = exp_fitNoiseRaw

subj = unique(experiment$ID)[1]

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

means = data %>% dplyr::group_by(priorLevel, prior_direction) %>%
  dplyr::summarize(meanCohs = mean(adjCoh1))
means$priorLevel = factor(means$priorLevel, levels=c("low","medium","high"))
means = means[order(means$priorLevel),]

right_coh = means$meanCohs[means$prior_direction==0]
left_coh = means$meanCohs[means$prior_direction==180]

## Setup data
N_data = length(data$ID)
coh1_data = data$adjCoh1
coh2_data = data$adjCoh2
conf_data = data$conf/100
choice1_data = data$choice1
choice_data = data$choice

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
grainsize = 1 
ints = length(settings)
  
alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints=ints, coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
  
file <- file.path(baseDir, "fullModel_ind_nor1r2.stan")
if (nonsimplified==TRUE){
  file <- file.path(baseDir, "fullModel_ind_par.stan")
}
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

Nchains <- 3
Niter <- 2000
Nburn <- 1000

parameters = c("m1","m2","b")

initfun <- function() {
  list(m1=runif(1,0.5,1.5), m2=runif(1,0.5,1.5), b=runif(1,0.5,1.5))
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
mcmc_trace(fit$draws(variables=c("m2","b")))
diag = fit$sampler_diagnostics(format="df")

stanfit <- rstan::read_stan_csv(fit$output_files())

m1 = extract(stanfit, "m1", permuted = TRUE, inc_warmup = FALSE,
             include = TRUE)
m2 = extract(stanfit, "m2", permuted = TRUE, inc_warmup = FALSE,
             include = TRUE)
b = extract(stanfit, "b", permuted = TRUE, inc_warmup=FALSE,include=TRUE)

posteriorsubj = as.matrix(data.frame(wChoice=unlist(m1, use.names = FALSE), wConf=unlist(m2, use.names = FALSE)))
mcmc_areas(posteriorsubj,
           pars = c("wChoice", "wConf"),
           prob = 0.89) 

posteriordiff = as.matrix(data.frame(diff=unlist(m1, use.names = FALSE)-unlist(m2, use.names = FALSE)))
mcmc_areas(posteriordiff,
           pars = c("diff"),
           prob = 0.89) 
