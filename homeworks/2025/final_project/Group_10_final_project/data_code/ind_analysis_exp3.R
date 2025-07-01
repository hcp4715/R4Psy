library(ggplot2)
library(patchwork)
library(dplyr)
library(sjPlot)
library(here)
library(lme4)
library(metaSDT)
library(rstan)
library(cmdstanr)
library(bayestestR)
library(posterior)
library(tictoc) # 这个包原来没加载，差评！
source(here::here("simulateProbabilityTask.R"))
source(here::here("fitPsychometric_exp3.R"))
source(here::here("Functions/Function_trials2counts.R"))

dataPath = here::here("Data/exp3/")
savePath = here::here("Data/exp3/")

load(file=file.path(dataPath, 'expData.RData'))
load(file=file.path(dataPath, 'stairData.RData'))

baseDir = here::here("Stan/exp3/")

# 获取所有被试ID
IDs = unique(experimentAll$ID)

# 遍历每个被试进行分析
for (i in 1:length(IDs)){
  mysubj = IDs[i] # 当前被试ID
 
  # 提取当前被试的阶梯和实验数据
  staircase = staircaseAll %>% dplyr::filter(ID==mysubj)
  experiment = print %>% dplyr::filter(ID==mysubj)
  
  #check probability sampling
  probs = experiment %>% dplyr::group_by(priorRight) %>% # 按先验概率分组
    dplyr::summarise(prob = sum(correct_answer_target=="right")/length(correct_answer_target=="right"), # 选右的正确率
                     total = length(correct_answer_target=="right")) # 选右的总trial数
  
  #staircase
  flatPrior = experiment %>% dplyr::filter(priorLevel==50) # 提取先验概率为50%的数据，即无信息性先验
  num = length(flatPrior$participant_nr) # 试次数
  ggplot(data=flatPrior, aes(x=c(1:num), y=coherence)) + # 绘制coherence随trial变化的趋势图
    geom_step()
  
  # 计算不同先验条件下的正确率
  accs = experiment %>% dplyr::group_by(priorLevel, priorDirection) %>% # 按先验水平和方向分组
    dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct)) # 计算平均正确率
  accsPerPrior = experiment %>% dplyr::group_by(priorLevel) %>% # 按先验水平分组
    dplyr::summarise(acc = sum(discrimination_is_correct)/length(discrimination_is_correct)) # 计算平均正确率
  probR = experiment %>% dplyr::group_by(priorRight) %>% # 按先验为右的概率分组
    dplyr::summarise(probR = sum(discrimination_key=="right")/length(discrimination_key=="right")) # 选择右的概率
  accsPerPrior
  probR
  
  # 计算不同条件下的平均信心水平
  # 按先验水平、方向、正确性分组
  confs = experiment %>% dplyr::group_by(priorLevel, priorDirection, discrimination_is_correct) %>%
    dplyr::summarise(meanConf = mean(conf)) # 计算平均信心值
  
  # 处理阶梯式实验的信心数据
  stairConf = staircase %>% dplyr::filter(confidence!="0") # 过滤掉信心为0的试次
  stairConf$conf = as.integer(stairConf$confidence)/100 # 将信心评分转换为0-1
  stairConf$stimulus = as.integer(stairConf$correct_answer=="right") # 刺激方向（正确答案是否为右）
  stairConf$response = as.integer(stairConf$prior_key=="right") # 被试的选择是否为右
  
  # 处理常规实验的平坦先验数据
  flatPrior$stimulus = as.integer(flatPrior$correct_answer_target=="right") # 刺激方向
  flatPrior$response = as.integer(flatPrior$discrimination_key=="right") # 被试选择
  
  # 合并阶梯式和平坦先验数据作为M-Ratio数据，包含信心评分、刺激方向和被试选择
  mRatioData = data.frame(confidence = c(stairConf$conf, flatPrior$conf), stimulus = c(stairConf$stimulus,flatPrior$stimulus), 
                          response = c(stairConf$response, flatPrior$response))
  
  
  # 将信心评分划分为5个分位数区间
  confQuant = quantile(mRatioData$confidence, probs=seq(0,1,0.2)) # 计算分位数切点
  confQuant
  
  # 根据分位数给每个被试分配信心等级（1-5级）
  mRatioData$confQuant = 5 # 初始化最高等级
  mRatioData$confQuant[mRatioData$confidence<=confQuant[5]] = 4 # 低于第80百分位设为4级
  mRatioData$confQuant[mRatioData$confidence<=confQuant[4]] = 3 # 低于第60百分位设为3级
  mRatioData$confQuant[mRatioData$confidence<=confQuant[3]] = 2 # 低于第40百分位设为2级
  mRatioData$confQuant[mRatioData$confidence<=confQuant[2]] = 1 # 低于第20百分位设为1级
  
  nRatings = 5 # 信心等级数
  
  # 将试验数据转换为计数数据（用于元认知分析）
  quants = trials2counts(mRatioData$stimulus, # 刺激方向（0/1）
                         mRatioData$response, # 反应（0/1）
                         mRatioData$confQuant, # 信心等级
                         nRatings = nRatings) # 等级数
  
  nR_S1 <- quants[[1]] # nR_S1：刺激为 S1 时，不同信心等级和反应类型的试次计数
  nR_S2 <- quants[[2]] # nR_S2：刺激为 S2 时，不同信心等级和反应类型的试次计数
  
  # 计算并提取M-ratio
  mratioInfo = fit_meta_d_SSE(nR_S1, nR_S2, s=1, add_constant=TRUE)
  mratioSDT = mratioInfo$M_ratio[1]
  mratioSDT
  
  # 拟合心理测量模型
  expFitNoise = fitPsychometric(staircase, experiment)
  expFitNoise$fitNoiseRaw[1] # 噪声参数
  expFitNoise$fitBiasRaw[1] # 偏差参数
  expFitNoise$fitLapseRaw[1] # lapse 率参数
  expFitNoise$mratioSDT = mratioSDT # 将M-ratio存入结果
  
  ### After fitting psychometric
  expFitNoise$coherentDir = -sign(expFitNoise$target_data$coherent_direction-1)
  expFitNoise$signedCoh = expFitNoise$coherence*expFitNoise$coherentDir
  expFitNoise$adjCoh = (expFitNoise$signedCoh-expFitNoise$fitBiasRaw)/expFitNoise$fitNoiseRaw
  
  right_coh = mean(expFitNoise$adjCoh[expFitNoise$coherentDir>0]) 
  left_coh =  mean(expFitNoise$adjCoh[expFitNoise$coherentDir<0])
  
  expFitNoise$adjCohMean = ifelse(expFitNoise$coherentDir>0,right_coh,left_coh)
  priorRs = probs$prob #c(0.1,0.2,0.3,0.4,0.5) #c(0.5,0.6,0.7,0.8,0.9)#c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)#
  #
  
  internalNoise <<- 1
  mode_m <<- 2
  
  ######### Fit added metanoise
  stairConf = staircase %>% dplyr::filter(confidence!="0")
  stairConf$coherentDir = -sign(stairConf$direction-1)
  stairConf$signedCoh = stairConf$coherence*stairConf$coherentDir
  stairConf$fitBiasRaw = expFitNoise$fitBiasRaw[1]
  stairConf$fitNoiseRaw = expFitNoise$fitNoiseRaw[1]
  stairConf$adjCoh = (stairConf$signedCoh-stairConf$fitBiasRaw)/stairConf$fitNoiseRaw
  stairConf$conf = as.integer(stairConf$confidence)/100
  stairConf$priorRight = 50
  
  expFlat = expFitNoise %>% dplyr::filter(priorLevel==50)
  
  Ntrials = length(c(expFlat$participant_nr, stairConf$participant_nr))
  N_data = Ntrials
  coh_data = c(expFlat$adjCoh, stairConf$adjCoh)
  conf_data = c(expFlat$conf, stairConf$conf)
  choice_data = c(as.integer(expFlat$discrimination_key=="right"), as.integer(stairConf$prior_key=="right"))
  correct = c(as.integer(expFlat$discrimination_is_correct), as.integer(stairConf$prior_is_correct))
  prior = c(expFlat$priorRight/100, stairConf$priorRight/100)
  
  file <- file.path(baseDir, "modelProbFitMetaN.stan")
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  grainsize = 1
  alldata <- list(N=N_data, coh=coh_data, choice=choice_data, conf=conf_data,trials=c(1:N_data), prior=prior, grainsize=grainsize)
  
  Nchains <- 3
  Niter <- 2000
  Nburn <- 2000
  
  parameters = c("b","metaN")
  initfun <- function() {
    list(b=runif(1,0.5,1.5), metaN=runif(1,0.5,3))
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
  
  #stanfit <- rstan::read_stan_csv(fit$output_files())
  #save(stanfit, file=paste("indfit_control_", ID, ".RData",sep=""))
  
  #mcmc_trace(fit$draws(variables=c("metaN")))
  
  metaNoise = summary$mean[2]
  metaNoise
  mratio = 1/metaNoise
  addNoise = sqrt((1/(mratio^2))-1)
  
  expFitNoise$metaNoise = metaNoise
  expFitNoise$mratioFit = mratio
  expFitNoise$addNoise = addNoise
  
  ### Model
  file <- file.path(baseDir, "modelProbFull.stan")
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  Ntrials = length(expFitNoise$participant_nr)
  N_data = Ntrials
  coh_data = expFitNoise$adjCohMean
  conf_data = expFitNoise$conf
  choice_data = as.integer(expFitNoise$discrimination_key=="right")
  correct = as.integer(expFitNoise$discrimination_is_correct)
  prior = expFitNoise$priorRight/100
  metaNoise = metaNoise
  
  grainsize = 1
  alldata <- list(N=N_data, coh=coh_data, choice=choice_data, conf=conf_data,trials=c(1:N_data), prior=prior, grainsize=grainsize, metaNoise=metaNoise)
  
  Nchains <- 3
  Niter <- 2000
  Nburn <- 2000
  
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
  View(summary)
  expFitNoise$m1 = summary$mean[1]
  expFitNoise$m2 = summary$mean[2]
  expFitNoise$b = summary$mean[3]
  
  if (i==1){
    expDataAll = expFitNoise
  }else{
    expDataAll = rbind(expDataAll, expFitNoise)
  }
  
}

save(expDataAll, file=file.path(savePath, 'expFitNoiseRaw.RData'))



