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
source(here::here("simulateProbabilityTask.R"))
dataDir = here::here("Data/exp3/")
baseDir = here::here("Stan/exp3/")
resultDir = here::here("Results/paramRecovery/")

rerun=FALSE

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

right_coh = 0.1/0.16 
left_coh =  -0.1/0.16 
priorRs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

internalNoise <<- 1
mode_m <<- 2

m1s = c(rep(c(0.5, 0.75, 1, 1.5, 2,3),6))
m2s = c(rep(c(0.5, 0.75, 1, 1.5,2,3),6))
bs = c(rep(c(rep(1,6), rep(1.5,6),rep(2,6)),2))
addNs = c(rep(0,18),rep(1,18))

if (rerun==TRUE){

  for (x in 1:length(m1s)){
    m1 = m1s[x]
    m2 = m2s[x]
    b = bs[x]
    addNoise = addNs[x]
    mratio = sqrt(1/(addNoise^2+1))
    metanoise = 1/mratio
    
    for (i in 1:10){
      simData = simulateProbTask(m1, m2, b, internalNoise, right_coh, left_coh, priorRs, reps=100, metanoise=addNoise)
      
      simData$priorFactor = simData$prior
      simData$priorFactor = as.factor(simData$priorFactor)
      simData$correct = simData$correct
      simData$conf = simData$confTarget
      
      Ntrials = length(simData$s)
      N_data = Ntrials
      coh_data = simData$s
      conf_data = simData$confTarget
      choice_data = as.integer(simData$choseRight)
      correct = as.integer(simData$correct)
      prior = simData$prior
      
      ## Full Model
      file <- file.path(baseDir, "modelProbFull.stan")
      mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
      
      grainsize = 1
      alldata <- list(N=N_data, coh=coh_data, choice=choice_data, conf=conf_data,trials=c(1:N_data), prior=prior, grainsize=grainsize, metaNoise=metanoise)
      
      Nchains <- 3
      Niter <- 1000
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
      
      m1_rec = summary$mean[1]
      m2_rec = summary$mean[2]
      b_rec = summary$mean[3]
      results = data.frame(setting = x, metaN = metanoise, mratio = mratio, m1True = m1, m2True = m2, bTrue = b, m1Rec = m1_rec, m2Rec = m2_rec, bRec = b_rec)
      
      if (i == 1 && x == 1){
        summaryAll = summary
        resultsAll = results
      }else{
        summaryAll = rbind(summaryAll, summary)
        resultsAll = rbind(resultsAll, results)
      }
    }
  }
  
  save(summaryAll, file=paste(resultDir, "recoverySummary_exp3.RData",sep=""))
  save(resultsAll, file=paste(resultDir, "recoveryResults_exp3.RData",sep=""))

}else{
  load(file=file.path(resultDir, "recoverySummary_exp3.RData"))
  load(file=file.path(resultDir, "recoveryResults_exp3.RData"))
}

m2Rhats = summaryAll$rhat[which(summaryAll$variable=="m2")]
m1Rhats = summaryAll$rhat[which(summaryAll$variable=="m1")]
bRhats = summaryAll$rhat[which(summaryAll$variable=="b")]

resultsAll$m1Rhats = m1Rhats
resultsAll$m2Rhats = m2Rhats
resultsAll$bRhats = bRhats

resultsClean = resultsAll %>% dplyr::filter(m1Rhats<1.1, m2Rhats<1.1, bRhats<1.1)

resultsSummary = resultsClean %>% dplyr::group_by(setting) %>%
  dplyr::summarise(m1true = m1True[1],m2true = m2True[1],btrue=bTrue[1], mRatio = mratio[1], m1rec = mean(m1Rec), m2rec = mean(m2Rec), 
                   brec = mean(bRec), sem1 = sd(m1Rec)/sqrt(length(m1Rec)), sem2 = sd(m2Rec)/sqrt(length(m2Rec)), seb = sd(bRec)/sqrt(length(bRec)))


plotResults = data.frame(true = c(resultsSummary$m1true, resultsSummary$m2true, resultsSummary$btrue), 
                         mratio = c(resultsSummary$mRatio, resultsSummary$mRatio, resultsSummary$mRatio),
                         rec = c(resultsSummary$m1rec, resultsSummary$m2rec, resultsSummary$brec),
                         se = c(resultsSummary$sem1, resultsSummary$sem2, resultsSummary$seb),
                         param = c(rep("m1",length(resultsSummary$m1true)), rep("m2", length(resultsSummary$m1true)), rep("b",length(resultsSummary$m1true))))


mResults = plotResults %>% dplyr::filter(param!="b")
plotResults$param = as.factor(plotResults$param)
levels(plotResults$param) = c("b","w_choice","w_conf")

plotResults$mratio[plotResults$mratio<1] = round(plotResults$mratio[plotResults$mratio<1], 2)
ggplot(plotResults, aes(x=true, y=rec, color=param, group=param)) +
  facet_wrap(~mratio) +
  geom_point() +
  geom_errorbar(aes(ymin=rec-se, ymax=rec+se), width=.2,
                position=position_dodge(0.1)) +
  geom_abline(slope=1,intercept=0) +
  scale_color_manual(values=c("#AA336A","#29b6f6","#000080"))+
  labs(x="Simulated Parameter Value", y = "Recovered Parameter Value", color="Parameter") +
  ylim(0,3.5) +
  xlim(0,3.5) +
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12), axis.title.y = element_text(size=14), 
        legend.text = element_text(size=14), legend.title = element_text(size=14), legend.position = "bottom") 



