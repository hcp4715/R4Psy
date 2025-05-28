###### Script for Chapter 11 ######
# ---------- 准备工作 ----------
# Packages
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}

pacman::p_load(
  # 本节课需要用到的 packages
  here, tidyverse, ggplot2,
  # ANOVA & HLM
  bruceR, lmerTest, lme4, broom, afex, interactions, easystats, caret, pROC,
  # 生成课件
  xaringan, xaringanthemer, xaringanExtra, knitr)

options(scipen=99999,digits = 5)

# 读取并预处理数据，保留试次数据
df.match <- bruceR::import(here::here('data', 'match', 'match_raw.csv')) %>%
      tidyr::extract(
            Shape,
            into = c('Valence', 'Identity'),
            regex = '(moral|immoral)(Self|Other)',
            remove = FALSE) %>% # 将Shape列分为两列
      dplyr::mutate(
            Valence = factor(Valence,
                             levels = c('moral', 'immoral'),
                             labels = c('moral', 'immoral')),
            Identity = factor(Identity,
                              levels = c('Self', 'Other'),
                              labels = c('Self', 'Other'))) %>%
      dplyr::filter(ACC == 0 | ACC == 1, RT >= 0.2 & RT <= 1.5, Match == 'match', 
                    (!Sub %in% c(7302, 7303, 7338))) # 过滤无效被试的数据

# 生成新数据，为ANOVA做准备
df.match.aov <- df.match %>%
      dplyr::group_by(Sub, Valence, Identity) %>%
      dplyr::summarise(mean_ACC = mean(ACC)) %>%
      dplyr::ungroup()

# 查看数据
head(df.match[c(3,11:17)],5) 

df.match.aov %>%
  dplyr::select(1:4) %>%
  head(5)

# ---------- 关于二项分布的模拟 ----------
simulate_coin_toss <- function(prob_head, num_people, num_tosses) {
  # 初始化一个向量来存储每个人正面朝上的总次数
  total_heads <- rep(0, num_people)
  # 模拟每个人抛硬币的次数，并计算正面朝上的总次数
  for (i in 1:num_people) {
    tosses <- rbinom(num_tosses, size = 1, prob = prob_head)
    total_heads[i] <- sum(tosses)
  }
  
  # 绘制直方图
  hist(total_heads, main = "Coin Toss Results for All People", xlab = "硬币正面朝上的次数", ylab = "人数", col = 'white', border = 'black', breaks = seq(min(total_heads), max(total_heads) + 1, by = 1), xlim = c(0,max(total_heads) + 1))
  
  # 返回每个人正面朝上的总次数
}

simulate_coin_toss(prob_head = 0.5,num_people = 5, num_tosses = 10)

simulate_coin_toss(prob_head = 0.5,num_people = 10, num_tosses = 10)

simulate_coin_toss(prob_head = 0.5,num_people = 1000, num_tosses = 10)

# 选择一个被试的数据作为演示
df.match.7304 <- df.match %>%
      dplyr::filter(Sub == 7304) # 选择被试7304

mod_7304_full <- stats::glm(
      # 数据
      data = df.match.7304,
      # 模型
      formula = ACC ~ 1 + Identity * Valence,
      # 因变量为二项分布
      family = binomial) 

summary(mod_7304_full) %>% #查看模型信息
      capture.output() %>% .[c(6:11, 15:19)] #课堂展示重要结果

# ---------- 10.2 代码实操：完整数据的GLM模型 ----------

# 无固定效应的零模型
mod_null <- lme4::glmer(data = df.match, # 数据
                   formula = ACC ~ (1 + Identity * Valence|Sub), # 模型
                   family = binomial) # 因变量二项分布

summary(mod_null) 

#随机截距，固定斜率
mod <- lme4::glmer(
      # 数据
      data = df.match,
      # 模型
      formula = ACC ~ 1 + Identity * Valence + (1 | Sub),
      # 因变量二项分布
      family = binomial)

# performance::model_performance(mod)
summary(mod) %>%
  capture.output() %>% .[c(7:8,14:24,28:32)]


# 随机截距，随机斜率
mod_full <- lme4::glmer(data = df.match, # 数据
                        formula = ACC ~ 1 + Identity * Valence + (1 + Identity * Valence|Sub), # 模型
                        family = binomial) # 因变量二项分布

summary(mod_full) %>%
  capture.output() %>% .[c(6:8,13:18,21:26,30:34)]

# 比较三个模型
stats::anova(mod_null, mod, mod_full)

performance::compare_performance(mod_null, mod, mod_full, rank = TRUE, verbose = FALSE)

summary(mod_full) %>% capture.output() %>% .[c(21:27)]

#交互作用
interactions::cat_plot(model = mod_full,
                       pred = Identity,
                       modx = Valence)

# ---------- 10.3 代码实操：不同模型的比较 ----------
## 重复测量方差分析
res <- bruceR::MANOVA(data = df.match.aov, #数据
       subID = 'Sub', # 被试编号
       dv= 'mean_ACC', # 因变量
       within = c('Identity', 'Valence')) #自变量（被试内）

res %>%
  bruceR::EMMEANS(effect = 'Valence', by = 'Identity') 

## GLM中类似ANOVA的输出
stats::anova(mod_full)

## 基于试次级数据的HLM
mod_anova <- lme4::lmer(data = df.match,
                        formula = ACC ~ 1 + Identity * Valence + (1 + Identity * Valence|Sub))

stats::anova(mod_anova)

## 基于每个条件均值的HLM
mod_mean <- lme4::lmer(data = df.match.aov,
                       formula = mean_ACC ~ 1 + Identity * Valence + 
                                                (1|Sub) + 
                                                (1|Identity:Sub) + 
                                                (1|Valence:Sub))
stats::anova(mod_mean)

## 比较模型
performance::compare_performance(mod_full, mod_anova, rank = TRUE, verbose = FALSE)

stats::anova(mod_full, mod_anova)

## 使用更加偏向预测的方法——交叉验证法(Cross-Validation)

# 设置种子以确保结果的可重复性
set.seed(456)

# 随机选择70%的数据作为训练集，剩余的30%作为测试集
train_index <- caret::createDataPartition(df.match$Sub, p = 0.7, list = FALSE)
train_data <- df.match[train_index, ]
test_data <- df.match[-train_index, ]

# 根据训练集生成模型
model_full <- lme4::glmer(data = train_data,
                          formula = ACC ~ 1 + Identity * Valence + (1 + Identity * Valence|Sub), 
                          family = binomial)
model_anova <- lme4::lmer(data = train_data,
                          formula = ACC ~ 1 + Identity * Valence + (1 + Identity * Valence|Sub))

# 使用模型进行预测
pre_mod_full <- stats::predict(model_full, newdata = test_data, type = 'response')
pre_mod_anova <- stats::predict(model_anova, newdata = test_data)

# 计算模型的性能指标
performance_mod_full <- c(RMSE = sqrt(mean((test_data$ACC - pre_mod_full)^2)),
                R2 = cor(test_data$ACC, pre_mod_full)^2)
# 打印性能指标
print(performance_mod_full)

# 计算模型的性能指标
performance_mod_anova <- c(RMSE = sqrt(mean((test_data$ACC - pre_mod_anova)^2)),
                R2 = cor(test_data$ACC, pre_mod_anova)^2)

# 打印性能指标
print(performance_mod_anova)

# 将预测概率转换为分类结果
predicted_classes <- ifelse(pre_mod_full > 0.5, 1, 0)
# 计算混淆矩阵
confusion_matrix <- caret::confusionMatrix(as.factor(predicted_classes), as.factor(test_data$ACC))

# 打印混淆矩阵和性能指标
print(confusion_matrix)

# 计算ROC曲线和AUC
roc_result <- pROC::roc(test_data$ACC, pre_mod_full)
print(roc_result)

# 绘制ROC曲线
plot(roc_result, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2) # 添加对角线

# ---------- 10.4 代码实操：其他分布 ----------
set.seed(123) # 设置随机种子以获得可重复的结果
random_samples <- rpois(1000, lambda = 5)
hist(random_samples,col = 'white', border = 'black',)