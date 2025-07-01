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
baseDir = here::here("Stan/hierarchical")
gqsDir = here::here("Stan/gqs")
resultsDir = here::here("Results/modelComparison/")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##### Get real data for a subject to fit #####
experiment = exp_fitNoiseRaw
subjs = unique(experiment$ID)
experiment = experiment %>% dplyr::filter(ID!=subjs[9])

K = 10

Nchains <- 3
Niter <- 500
Nburn <- 500

experiment$fold <- kfold_split_grouped(K = K, x = experiment$ID)

log_pd_kfold <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_choice <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_conf <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))

log_pd_kfold_eq <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_eq_choice <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_eq_conf <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))

log_pd_kfold_flat <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_flat_choice <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_flat_conf <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))

log_pd_kfold_opt <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_opt_choice <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))
log_pd_kfold_opt_conf <- matrix(nrow = Niter*Nchains, ncol = nrow(experiment))

if (run){
  for (k in 1:K){
    for (i in 1:2){
      if (i==1){
        datatrain = experiment %>% dplyr::filter(fold!=k)
        
        subjs = unique(datatrain$ID)
        j = 1
        for (subj in subjs){
          data = datatrain %>% dplyr::filter(ID==subj)
          
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
          ind = (j-1)*24+ind
          
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
          
          subjData = data.frame(coh1_data, coh2_data, conf_data, choice1_data, choice_data, levels)
          subjData$id = j
          subjInts = data.frame(integralcoh1s, integralcoh2s)
          subjInts$id = j
          
          if(j==1){
            fullData = subjData
            fullInts = subjInts
          }else{
            fullData = rbind(fullData, subjData)
            fullInts = rbind(fullInts, subjInts)
          }
          j = j+1
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
        grainsize = round((N_data/3)/2)
        
        data_train <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)
      }else{
        datatest = experiment %>% dplyr::filter(fold==k)
        
        subjs = unique(datatest$ID)
        j = 1
        for (subj in subjs){
          data = datatest %>% dplyr::filter(ID==subj)
          
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
          # Ntrials = length(data$ID)
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
          ind = (j-1)*24+ind
          
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
          
          subjData = data.frame(coh1_data, coh2_data, conf_data, choice1_data, choice_data, levels)
          subjData$id = j
          subjInts = data.frame(integralcoh1s, integralcoh2s)
          subjInts$id = j
          
          if(j==1){
            fullData = subjData
            fullInts = subjInts
          }else{
            fullData = rbind(fullData, subjData)
            fullInts = rbind(fullInts, subjInts)
          }
          j = j+1
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
        grainsize = round((N_data/3)/2)  
        
        data_test <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)
      }
    }
    file <- file.path(baseDir, "fullModel_h_par_nor1r2.stan")
    mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    initlist = list(m1_mu=1,m1_sd=1,m1=rep(1,data_train$L),m2_mu=1,m2_sd=1,m2=rep(1,data_train$L),b_mu=1,b_sd=1,b=rep(1,data_train$L))
    init = list(initlist,initlist,initlist)
    
    # Compile and run
    tic("model fitting")
    fit <- mod$sample(data_train,
                      chains = 3,
                      parallel_chains = 3,
                      threads_per_chain = 3,
                      refresh = 10, 
                      init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    #stanfit <- rstan::read_stan_csv(fit$output_files())
    #posterior <- as.matrix(stanfit)
    posterior = fit$draws(format = "matrix")
    
    file <- file.path(gqsDir, "fullModel_h_nor1r2_gqs.stan")
    stanmod <- stan_model(file)
    
    gen_test <- gqs(stanmod, draws = posterior, data = data_test)
    
    log_pd_kfold_choice[, experiment$fold == k] <- unlist(extract(gen_test, "log_lik_choice", permuted = FALSE, inc_warmup = FALSE,
                                                                  include = TRUE),use.names = FALSE)
    
    log_pd_kfold_conf[, experiment$fold == k] <- unlist(extract(gen_test, "log_lik_conf", permuted = FALSE, inc_warmup = FALSE,
                                                                include = TRUE),use.names = FALSE)
    
    log_pd_kfold[, experiment$fold == k] <- extract_log_lik(gen_test)
    
    #####
    
    file <- file.path(baseDir, "equalModel_h_par_nor1r2.stan")
    mod_eq <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    initlist = list(m_mu=1,m_sd=1,m=rep(1,data_train$L),b_mu=1,b_sd=1,b=rep(1,data_train$L))
    init = list(initlist,initlist,initlist)
    
    # Compile and run
    tic("model fitting")
    fit_eq <- mod_eq$sample(data_train,
                            chains = 3,
                            parallel_chains = 3,
                            threads_per_chain = 3,
                            refresh = 100, 
                            init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    #stanfit_eq <- rstan::read_stan_csv(fit_eq$output_files())
    #posterior_eq <- as.matrix(stanfit_eq)
    posterior_eq = fit_eq$draws(format = "matrix")
    
    file <- file.path(gqsDir, "equalModel_h_nor1r2_gqs.stan")
    stanmod_eq <- stan_model(file)
    
    gen_test_eq <- gqs(stanmod_eq, draws = posterior_eq, data= data_test)
    log_pd_kfold_eq[, experiment$fold == k] <- extract_log_lik(gen_test_eq)
    
    log_pd_kfold_eq_choice[, experiment$fold == k] <- unlist(extract(gen_test_eq, "log_lik_choice", permuted = FALSE, inc_warmup = FALSE,
                                                                     include = TRUE),use.names = FALSE)
    
    log_pd_kfold_eq_conf[, experiment$fold == k] <- unlist(extract(gen_test_eq, "log_lik_conf", permuted = FALSE, inc_warmup = FALSE,
                                                                   include = TRUE),use.names = FALSE)
    #######
    
    file <- file.path(baseDir, "noPrior_h_par_nor1r2.stan")
    mod_flat <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    initlist = list(b_mu=1,b_sd=1,b=rep(1,data_train$L))
    init = list(initlist,initlist,initlist)
    
    # Compile and run
    tic("model fitting")
    fit_flat <- mod_flat$sample(data_train,
                                chains = 3,
                                parallel_chains = 3,
                                threads_per_chain = 3,
                                refresh = 100, 
                                init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    posterior_flat = fit_flat$draws(format = "matrix")
    
    file <- file.path(gqsDir, "noPrior_h_nor1r2_gqs.stan")
    stanmod_flat <- stan_model(file)
    
    gen_test_flat <- gqs(stanmod_flat, draws = posterior_flat, data= data_test)
    log_pd_kfold_flat[, experiment$fold == k] <- extract_log_lik(gen_test_flat)
    
    log_pd_kfold_flat_choice[, experiment$fold == k] <- unlist(extract(gen_test_flat, "log_lik_choice", permuted = FALSE, inc_warmup = FALSE,
                                                                       include = TRUE),use.names = FALSE)
    
    log_pd_kfold_flat_conf[, experiment$fold == k] <- unlist(extract(gen_test_flat, "log_lik_conf", permuted = FALSE, inc_warmup = FALSE,
                                                                     include = TRUE),use.names = FALSE)
    
    ####
    file <- file.path(baseDir, "optimalModel_h_par_nor1r2.stan")
    mod_opt <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
    
    initlist = list(b_mu=1,b_sd=1,b=rep(1,data_train$L))
    init = list(initlist,initlist,initlist)
    
    # Compile and run
    tic("model fitting")
    fit_opt <- mod_opt$sample(data_train,
                              chains = 3,
                              parallel_chains = 3,
                              threads_per_chain = 3,
                              refresh = 100, 
                              init=init, iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = 1) #, adapt_delta = 0.9
    toc()
    
    posterior_opt = fit_opt$draws(format = "matrix")
    #posterior_opt
    
    file <- file.path(gqsDir, "optimalModel_h_nor1r2_gqs.stan")
    stanmod_opt <- stan_model(file)
    
    gen_test_opt <- gqs(stanmod_opt, draws = posterior_opt, data= data_test)
    log_pd_kfold_opt[, experiment$fold == k] <- extract_log_lik(gen_test_opt)
    
    log_pd_kfold_opt_choice[, experiment$fold == k] <- unlist(extract(gen_test_opt, "log_lik_choice", permuted = FALSE, inc_warmup = FALSE,
                                                                      include = TRUE),use.names = FALSE)
    
    log_pd_kfold_opt_conf[, experiment$fold == k] <- unlist(extract(gen_test_opt, "log_lik_conf", permuted = FALSE, inc_warmup = FALSE,
                                                                    include = TRUE),use.names = FALSE)
  }
  save(log_pd_kfold, file=paste(resultsDir,"10foldCV_log_pd_kfold_all_full.RData",sep=""))
  save(log_pd_kfold_eq, file=paste(resultsDir,"10foldCV_log_pd_kfold_all_eq.RData",sep=""))
  save(log_pd_kfold_opt, file=paste(resultsDir,"10foldCV_log_pd_kfold_all_opt.RData",sep=""))
  save(log_pd_kfold_flat, file=paste(resultsDir,"10foldCV_log_pd_kfold_all_flat.RData",sep=""))
}else{
  load(paste(resultsDir,"10foldCV_log_pd_kfold_all_full.RData",sep=""))
  load(paste(resultsDir,"10foldCV_log_pd_kfold_all_eq.RData",sep=""))
  load(paste(resultsDir,"10foldCV_log_pd_kfold_all_opt.RData",sep=""))
  load(paste(resultsDir,"10foldCV_log_pd_kfold_all_flat.RData",sep=""))
}

(elpd_kfold <- elpd(log_pd_kfold))
(elpd_kfold_eq <- elpd(log_pd_kfold_eq))
(elpd_kfold_opt <- elpd(log_pd_kfold_opt))
(elpd_kfold_flat <- elpd(log_pd_kfold_flat))

loo_compare(elpd_kfold, elpd_kfold_eq)
loo_compare(elpd_kfold, elpd_kfold_opt)
loo_compare(elpd_kfold, elpd_kfold_flat)

full = data.frame(model = c("Flexible", "Optimal","Equal", "Flat"), elpd = c(elpd_kfold$estimates[1], elpd_kfold_opt$estimates[1], elpd_kfold_eq$estimates[1], elpd_kfold_flat$estimates[1]),
                  se = c(elpd_kfold$estimates[3], elpd_kfold_opt$estimates[3], elpd_kfold_eq$estimates[3], elpd_kfold_flat$estimates[3]))
full$model = ordered(full$model, c("Flexible", "Optimal", "Equal","Flat"))
ggplot(full, aes(x=model, y=elpd)) +
  geom_bar(stat="identity", width=0.6, fill="#D1E3ED",color="darkgray") +  
  geom_errorbar(aes(ymin=elpd-se, ymax=elpd+se), width=.3) +
  geom_rect(aes(xmin=0, xmax=5, ymin=-1110000, ymax=-1150000), fill="white") +
  coord_cartesian(ylim = c(-1900000, -1100000)) +
  labs(x="Model", y ="Expected Log Pointwise Predictive Density") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.position = "none")  +
  scale_y_continuous(breaks = c(-1100000, -1200000,-1300000,-1800000), labels=c("0", "-1200000", "-1300000","-1800000"))

ggplot(full, aes(x=model, y=elpd)) +
  geom_point(size=3.5, shape=21, fill="#D1E3ED",color="black") +  
  geom_errorbar(aes(ymin=elpd-se, ymax=elpd+se), width=.3) +
  geom_rect(aes(xmin=0, xmax=5, ymin=-1110000, ymax=-1150000), fill="white") +
  coord_cartesian(ylim = c(-1900000, -1100000)) +
  labs(x="Model", y ="Expected Log Pointwise Predictive Density") + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
        legend.position = "none")  +
  scale_y_continuous(breaks = c(-1100000, -1200000,-1300000,-1800000), labels=c("0", "-1200000", "-1300000","-1800000"))

save(experiment, file="experimentDataWithFolds.RData")
