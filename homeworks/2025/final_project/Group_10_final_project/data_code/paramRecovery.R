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
source(here::here("Functions/simulateDistribution.R"))
source(here::here("Functions/simulateDistribution_means.R"))
dataDir = here::here("Data/")
baseDir = here::here("Stan/individual")
resultDir = here::here("Results/paramRecovery/")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

run = FALSE
nReps = 20
right_coh = c(0.05,0.1,0.15)/0.13 
left_coh = -c(0.05,0.1,0.15)/0.13

internalNoise <<- 1
mode_m <<- 2

m1s = c(rep(c(0.5, 1, 2),3))
m2s = c(rep(c(0.5,1,2),3))
bs = c(rep(1,3),rep(1.5,3), rep(2,3))

simType = "internalSignals" #externalSignals

if (run){
  for (x in 1:length(m1s)){
    m1 = m1s[x]
    m2 = m2s[x]
    b = bs[x]
    
    for (i in 1:nReps){
      if (simType=="internalSignals"){
        simData = simulateDistribution(m1, m2, b, internalNoise, right_coh, left_coh, reps=120)
      }else{
        simData = simulateDistribution_means(m1, m2, b, internalNoise, right_coh, left_coh, reps=120)
      }
      
      Ntrials = length(simData$s)
      N_data = Ntrials
      coh1_data = simData$s1
      coh2_data = simData$s2
      conf_data = simData$confTarget
      choice1_data = as.integer(simData$choseRight1)
      choice_data = as.integer(simData$choseRight2)
      correct = as.integer(simData$correct2)
      
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
      alldata <- list(N=N_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),ints=24, coh1Int=integralcoh1s, coh2Int=integralcoh2s, levels=levels, grainsize=grainsize)
      
      Nchains <- 3
      Niter <- 1000
      Nburn <- 1000
      
      parameters = c("m1","m2","b")
      initlist = list(m1=1,m2=1,b=1) 
      init = list(initlist,initlist,initlist)
      
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
      m1_rec = summary$mean[1]
      m2_rec = summary$mean[2]
      b_rec = summary$mean[3]
      results = data.frame(setting = x, simType = simType, m1True = m1, m2True = m2, bTrue = b, m1Rec = m1_rec, m2Rec = m2_rec, bRec = b_rec)
      
      if (i == 1 && x == 1){
        summaryAll = summary
        resultsAll = results
      }else{
        summaryAll = rbind(summaryAll, summary)
        resultsAll = rbind(resultsAll, results)
      }
    }
  }
  
  if (simType=="internalSignals"){
    save(summaryAll, file=paste(resultDir,"simInternalSignals_recovery_Summary.RData",sep=""))
    save(resultsAll, file=paste(resultDir,"simInternalSignals_recovery_Results.RData",sep=""))
  }else{
    save(summaryAll, file=paste(resultDir,"simExternalSignals_recovery_Summary.RData",sep=""))
    save(resultsAll, file=paste(resultDir,"simExternalSignals_recovery_Results.RData",sep=""))
  }
}else{
  if (simType=="internalSignals"){
    load(paste(resultDir,"simInternalSignals_recovery_Summary.RData",sep=""))
    load(paste(resultDir,"simInternalSignals_recovery_Results.RData",sep=""))
  }else{
    load(paste(resultDir,"simExternalSignals_recovery_Summary.RData",sep=""))
    load(paste(resultDir,"simExternalSignals_recovery_Results.RData",sep=""))
  }
}

m2Rhats = summaryAll$rhat[which(summaryAll$variable=="m2")]
m1Rhats = summaryAll$rhat[which(summaryAll$variable=="m1")]
bRhats = summaryAll$rhat[which(summaryAll$variable=="b")]

resultsAll$m1Rhats = m1Rhats
resultsAll$m2Rhats = m2Rhats
resultsAll$bRhats = bRhats

resultsClean = resultsAll %>% dplyr::filter(m2Rhats<1.1, bRhats<1.1)

resultsSummary = resultsClean %>% dplyr::group_by(setting) %>%
  dplyr::summarise(m1true = m1True[1],m2true = m2True[1],btrue=bTrue[1], m1rec = mean(m1Rec), m2rec = mean(m2Rec), 
                   brec = mean(bRec), sem1 = sd(m1Rec)/sqrt(length(m1Rec)), sem2 = sd(m2Rec)/sqrt(length(m2Rec)), seb = sd(bRec)/sqrt(length(bRec)))
errorResults = resultsSummary
errorResults$m2Error = errorResults$m2rec - errorResults$m2true
errorResults$bError = errorResults$brec - errorResults$btrue

errorResultsSumm = data.frame(m2true = rep(errorResults$m2true,2), btrue = rep(errorResults$btrue,2),
                              error = c(errorResults$m2Error, errorResults$bError), param = c(rep("m2",length(errorResults$m2Error)),rep("b",length(errorResults$bError))))

ggplot(errorResultsSumm, aes(x=m2true, y=btrue, color=error, shape=param)) +
  geom_point(size=2.5, position=ggstance::position_dodgev(height=0.03)) +
  scale_color_viridis(discrete=FALSE, option="B") +
  labs(x="Simulated m2", y="Simulated b", color="Recovery Error", shape="Parameter")


plotResults = data.frame(true = c(resultsSummary$m1true, resultsSummary$m2true, resultsSummary$btrue), 
                         rec = c(resultsSummary$m1rec, resultsSummary$m2rec, resultsSummary$brec),
                         se = c(resultsSummary$sem1, resultsSummary$sem2, resultsSummary$seb),
                         param = c(rep("m1",length(resultsSummary$m1true)), rep("m2", length(resultsSummary$m2true)), rep("b",length(resultsSummary$btrue))))


mResults = plotResults %>% dplyr::filter(param!="b")
plotResults$param = as.factor(plotResults$param)
levels(plotResults$param) = c("b","w_choice","w_conf")

if (simType=="internalSignals"){
  titleText = "Free Individual Internal Signals"
}else{
  titleText = "Internal Signals Matched to External Stimulus Strengths"
}

ggplot(plotResults, aes(x=true, y=rec, color=param, group=param)) +
  geom_point() +
  geom_errorbar(aes(ymin=rec-se, ymax=rec+se), width=.2,
                position=position_dodge(0.1)) +
  geom_abline(slope=1,intercept=0) +
  scale_color_manual(values=c("#AA336A","#29b6f6","#000080"))+
  labs(title = titleText, x="Simulated Parameter Value", y = "Recovered Parameter Value", color="Parameter") +
  ylim(0,3.5) +
  xlim(0,3.5) +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=14), legend.title = element_text(size=14), legend.position = "bottom") 



