require(rstan)
require(shinystan)
require(parallel)
require(loo)
require(here)
require(dplyr)
library(bayestestR)
library("bayesplot")
library("rstanarm")
library("ggplot2")
library("tictoc")
library("cmdstanr")
library("posterior")
source(here::here("simulateProbabilityTask.R"))

refit=TRUE # 是否重新拟合模型
dataDir = here::here("Data/exp3/")
baseDir = here::here("Stan/exp3/")
saveDir = here::here("Results/modelFits/")

# 设置rstan选项
rstan_options(auto_write = TRUE) # 自动保存编译后的模型
options(mc.cores = parallel::detectCores()) # 使用所有可用CPU核心

# 加载数据
load(file=file.path(dataDir,'expFitNoiseRaw.RData')) # 这个数据是通过ind_analysis_exp3.R脚本得到的
IDs = unique(expDataAll$ID) # 获取所有被试ID
exclude = c("5WKGPD","HFUPCA","78M8N9") # 定义要排除的被试ID
expDataAll = expDataAll %>% dplyr::filter(!(ID %in% exclude)) # 过滤掉需要排除的被试。过滤前23名，过滤后20名。

if (refit==TRUE){
  # 加载stan模型文件
  file <- file.path(baseDir, "modelProbFull_h.stan")
  # 编译stan模型，启用多线程
  mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
  
  # 准备模型输入数据
  Ntrials = length(expDataAll$participant_nr) # 试次数
  N_data = Ntrials
  L_data = length(unique(expDataAll$ID)) # 被试数量
  ll_data = as.numeric(factor(expDataAll$ID)) #被试索引，这个是我们自己改的
  # ll_data = expDataAll$ID # expDataAll$subj
  coh_data = expDataAll$adjCohMean # 刺激强度
  conf_data = expDataAll$conf # 信心评分
  choice_data = as.integer(expDataAll$discrimination_key=="right") # 被试的选项，转换为0/1
  correct = as.integer(expDataAll$discrimination_is_correct) # 选项正确性，转换为0/1
  prior = expDataAll$priorRight/100 # 先验概率
  metaNoise = expDataAll$metaNoise # 元认知噪声
  
  # 设置并行计算粒度
  grainsize = 1
  # 构建传递给stan模型的数据列表
  alldata <- list(N=N_data, L=L_data, ll=ll_data, 
                  coh=coh_data, choice=choice_data, conf=conf_data,trials=c(1:N_data), prior=prior, grainsize=grainsize, metaNoise=metaNoise)
  
  # 设置mcmc采样参数
  Nchains <- 3
  Niter <- 2000
  Nburn <- 2000
  
  # 定义要提取的模型参数
  parameters = c("m1_mu","m2_mu","b_mu","m1_sd","m2_sd","b_sd","m1","m2","b")
  
  # Compile and run
  # 编译并运行stan模型，同时计时
  tic("model fitting")
  fit <- mod$sample(alldata,
                    chains = 3,
                    parallel_chains = 3, # 并行链数
                    threads_per_chain = 3, # 每个链使用的线程数
                    refresh = 100, # 每一百次迭代刷新一次输出
                    iter_sampling = Niter, iter_warmup = Nburn, max_treedepth = 10, save_warmup = TRUE) # 是否保存warmup样本，1表示保存
  toc() # 结束计时并输出模型拟合耗时
  
  summary = summarise_draws(bind_draws(fit$draws(variables=parameters), along = "chain"))
  View(summary)
  
  # 将后验样本转换为数据框格式
  draws_df <- fit$draws(format = "df", variables=parameters)
  
  # 可视化wChoice和wConf参数的后验分布
  posteriorsubj = as.matrix(data.frame(wChoice=draws_df$m1_mu, wConf=draws_df$m2_mu, b=draws_df$b_mu))
  mcmc_areas(posteriorsubj,
             pars = c("wChoice", "wConf", "b"),
             prob = 0.89) # 89%的最高密度区间
  
  posteriordiff = as.matrix(data.frame(diff=draws_df$m1_mu-draws_df$m2_mu))
  mcmc_areas(posteriordiff,
             pars = c("diff"),
             prob = 0.89) 
  
  # 这里不知道为什么拟合的结果保存不下来
  # 将cmdstanr的拟合结果转换为rstan格式
  stanfit <- rstan::read_stan_csv(fit$output_files())
  # 保存模型拟合结果
  save(stanfit, file=file.path(saveDir, "full_hFit_exp3.RData"))
  
  # 换一种方法保存
  # 保存模型
  saveRDS(fit, file = file.path(saveDir, "full_hFit_exp3.rds"))
  # 加载模型
  # my_fit <- readRDS(file.path(saveDir, "full_hFit_exp3.rds"))
  
}else{
  load(file=file.path(saveDir,"full_hFit_exp3.RData"))
  #summaryFit = summary(stanfit)
  #summaryFit = summaryFit$summary
  
  m1 = rstan::extract(stanfit, "m1_mu", permuted = TRUE, inc_warmup = FALSE,
                      include = TRUE)
  m2 = rstan::extract(stanfit, "m2_mu", permuted = TRUE, inc_warmup = FALSE,
                      include = TRUE)
  b = rstan::extract(stanfit, "b_mu", permuted = TRUE, inc_warmup=FALSE,include=TRUE)
  
  posteriors = as.matrix(data.frame(wChoice=unlist(m1, use.names = FALSE), wConf=unlist(m2, use.names = FALSE), b=unlist(b, use.names = FALSE)))
  mcmc_areas(posteriors,
             pars = c("wChoice"),
             prob = 0.89) +
    geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
          legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")
  mcmc_areas(posteriors,
             pars = c("wConf","b"),
             prob = 0.89) +
    geom_vline(xintercept=1, linetype="dashed", color = "black", size=1) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
          legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")
  
  postdiff = as.matrix(data.frame(w_diff = unlist(m1, use.names = FALSE)-unlist(m2, use.names = FALSE)))
  mcmc_areas(postdiff,
             pars = c("w_diff"),
             prob = 0.89) +
    geom_vline(xintercept=0, linetype="dashed", color="black", size=1)+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=12), axis.title.x = element_text(size=14), strip.text = element_text(size=12), 
          legend.text = element_text(size=12), legend.title = element_text(size=14), legend.position = "bottom")
  
  ci(posteriors[,1], method = "HDI", ci=0.89) # wChoice_mu
  ci(posteriors[,2], method = "HDI", ci=0.89) # wConf_mu
  ci(posteriors[,3], method = "HDI", ci=0.89) # b_mu
  ci(posteriors[,1]-posteriors[,2], method = "HDI", ci=0.89) # wChoice_mu - wConf_mu
}
