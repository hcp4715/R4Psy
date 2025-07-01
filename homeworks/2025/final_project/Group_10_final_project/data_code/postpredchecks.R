require(rstan)
require(shinystan)
require(parallel)
require(loo)
require(here)
require(dplyr)
require(cmdstanr)
library("posterior")
library("bayesplot")
library("rstanarm")
library("ggplot2")
library("tictoc")
dataDir = here::here("Data/")
baseDir = here::here("Stan/ppc")
resultDir = here::here("Results/ppc/")
modelFitDir = here::here("Results/modelFits/")
load(file=paste(dataDir, "expFitNoiseRaw.RData", sep=""))

color_scheme_set("blue")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

runSamples = FALSE 

##### Get real data for a subject to fit #####
experiment = exp_fitNoiseRaw

subjs = unique(experiment$ID)
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
grainsize = (N_data/3)/2

alldata <- list(N=N_data, L=L_data, ll=ll_data, intMap=intmap_data, coh1=coh1_data, coh2=coh2_data, choice1=choice1_data, choice=choice_data, conf=conf_data,trials=c(1:N_data),coh1Int=coh1Ints, coh2Int=coh2Ints, levels=levels, grainsize=grainsize)

######## For producing PPCs that do not simulate trials but just sample distributions (for SI figures) 

file <- file.path(baseDir, "fullModel_h_nor1r2_ppc_nonSim.stan")
stanmod <- stan_model(file)

if (runSamples){
  gen_test <- gqs(stanmod, draws = as.matrix(stanfit), data= alldata)
  save(gen_test, file=paste(resultDir,"gen_test_ppcNonSim.RData",sep=""))
  
  choice_rep = extract(gen_test, "choice_rep", permuted = TRUE)$choice_rep
  conf_rep = extract(gen_test, "conf_rep", permuted = TRUE)$conf_rep
  correct_rep = extract(gen_test, "correct_rep", permuted = TRUE)$correct_rep
  
  save(choice_rep, file=paste(resultDir,"choice_rep_nonSim.RData",sep=""))
  save(conf_rep, file=paste(resultDir,"conf_rep_nonSim.RData",sep=""))
  save(correct_rep, file=paste(resultDir,"correct_rep_nonSim.RData",sep=""))
}else{
  load(paste(resultDir,"choice_rep_nonSim.RData",sep=""))
  load(paste(resultDir,"conf_rep_nonSim.RData",sep=""))
  load(paste(resultDir,"correct_rep_nonSim.RData",sep=""))
}

posteriorFact = as.factor(posteriorFactor)
posteriorFact = ordered(posteriorFactor, c("L","M","H"))
ppc_bars_grouped(choice_data, choice_rep[1:50,], group = posteriorFact) 

fullData$condition = as.factor(fullData$condition)
levels(fullData$condition) = c("Stronger-Cue", "Stronger-Target")

ppc_dens_overlay_grouped(conf_data, conf_rep[1:100,],group = fullData$condition)

