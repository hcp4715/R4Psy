## LOAD PACKAGES ####
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(tidyr)
library(dplyr)
library(performance) 
library(ggsignif)

setwd("C:/Users/abc/Desktop/R final report")

####################################################################
########################### EXPERIMENT 1 ###########################
####################################################################

## Read Data ####
data_exp1 <- read.table("./Data Analysis/full_results_exp1_fwords.txt", 
                        header = TRUE, sep = "\t")
## Checking Data ####
str(data_exp1) # 检查变量类型

# 保证condition和phase为因子变量
data_exp1$condition <- as.factor(data_exp1$condition)
data_exp1$phase <- as.factor(data_exp1$phase)
data_exp1$subject <- as.factor(data_exp1$subject)

# 查看是否每个 subject 都有两行 (habituation/test)， 每人是否都有两阶段数据
table(table(data_exp1$subject))  # 核对无误，48个数据，每人2个phase

## Explore pic ####
# 可视化每个被试在两个阶段的注视时间变化
# 散点和折线图
ggplot(data_exp1, aes(x = phase, y = looking_time, 
                      color = condition, group = condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1), 
             alpha = 0.6) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(y = "looking time (seconds)", x = "phase") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14)

# Q-Q PLOT
qqnorm(data_exp1$looking_time)
qqline(data_exp1$looking_time)
#  注视时间呈现右偏态分布

## Log translation ####

exp1_clean <- data_exp1 %>%
  mutate(
    log_looking_time = log(data_exp1$looking_time + 1), # 避免 log(0) 错误
    # log转换（提高模型正态性，原文也这么做）  注视时间的数据呈现右偏态分布，因此需要进行对数转换保证方差的齐性
    # 根据韦伯-费希纳定，人对时间间隔的感知接近对数尺度
  ) %>%
  group_by(subject, condition, phase, log_looking_time) %>%
  summarise(mean_looking_time = mean(looking_time), .groups = "drop")

# Q-Q PLOT
qqnorm(exp1_clean$log_looking_time)
qqline(exp1_clean$log_looking_time)
# log转换后的注视时间呈现正态分布

## Summary average log_looking_time ####
# 汇总每个条件下每阶段的平均 log 注视时间
summary_data_1 <- exp1_clean %>%
  group_by(condition, phase) %>%
  summarise(mean_log = mean(log_looking_time),
            se_log = sd(log_looking_time)/sqrt(n()))

## LMM ####
#拟合线性混合效应模型
# 拟合模型：检验 phase（habituation vs test）和 condition（Noun vs Verb）交互作用
model_1 <- lmer(log_looking_time ~ condition * phase 
                + (1 | subject), data = exp1_clean)
# log_looking_time ~ condition * phase: 检查两个因素及其交互作用
# (1 | subject): 建模每个被试的随机截距，控制个体差异
# 如果每个条件下的多个试次数据，还可以加入 (1 | item)（此数据中没有）
# 主要关注交互项：conditionVerb-Switch:phaseTest
# 如果这个项显著（p < 0.05），说明 Verb 条件下从习惯化到测试的变化显著不同于 Noun 条件，支持你论文的理论假设。

# 输出模型结果（包含估计值、SE、t值、p值）
summary(model_1)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log_looking_time ~ condition * phase + (1 | subject)
#    Data: exp1_clean

# REML criterion at convergence: 118.4

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3003 -0.6703  0.0461  0.7012  2.5609 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 0.0272   0.1649  
#  Residual             0.1595   0.3994  
# Number of obs: 96, groups:  subject, 48

# Fixed effects:
#                                Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                     2.53269    0.08820 90.08714  28.716   <2e-16 ***
# conditionVerb-Switch            0.08337    0.12473 90.08714   0.668   0.5056    
# phasetest                      -0.05833    0.11529 46.00000  -0.506   0.6153    
# conditionVerb-Switch:phasetest  0.37710    0.16304 46.00000   2.313   0.0252 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
#             (Intr) cndV-S phstst
# cndtnVrb-Sw -0.707              
# phasetest   -0.654  0.462       
# cndtnVrb-S:  0.462 -0.654 -0.707

# 模型诊断
check_model(model_1)

# 简单效应分析
em_exp1 <- emmeans(model_1, ~ phase | condition)
pairs(em_exp1_condition)  # 条件内阶段间比较

# condition = Noun-Switch:
#   contrast           estimate    SE df t.ratio p.value
# habituation - test   0.0583 0.115 46   0.506  0.6153

# condition = Verb-Switch:
#   contrast           estimate    SE df t.ratio p.value
# habituation - test  -0.3188 0.115 46  -2.765  0.0082

# Degrees-of-freedom method: kenward-roger 

em_exp1_phase <- emmeans(model_1, ~ condition | phase)
pairs(em_exp1_phase)  # 阶段间条件内比较 

# phase = habituation:
#   contrast                      estimate    SE   df t.ratio p.value
# (Noun-Switch) - (Verb-Switch)  -0.0834 0.125 90.1  -0.668  0.5056

# phase = test:
#   contrast                      estimate    SE   df t.ratio p.value
# (Noun-Switch) - (Verb-Switch)  -0.4605 0.125 90.1  -3.692  0.0004

# Degrees-of-freedom method: kenward-roger 

# 计算效应量
effectsize_exp1 <- eff_size(em_exp1, sigma = sigma(model_1), 
                            edf = df.residual(model_1))
confint(effectsize_exp1)  # 效应量置信区间

# condition = Noun-Switch:
#   contrast           effect.size    SE   df lower.CL upper.CL
# habituation - test       0.146 0.289 90.1   -0.428    0.720

# condition = Verb-Switch:
#   contrast           effect.size    SE   df lower.CL upper.CL
# habituation - test      -0.798 0.295 90.1   -1.384   -0.213

# sigma used for effect sizes: 0.3994 
# Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
# Confidence level used: 0.95 

## draw picture ####
test_means_exp1 <- summary_data_1 %>% filter(phase == "test")

ggplot(summary_data_1, aes(x = phase, y = mean_log, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
  geom_errorbar(aes(ymin = mean_log - se_log, ymax = mean_log + se_log),
                position = position_dodge(0.9), width = 0.2) +
  geom_signif(comparisons = list(c("Noun-Switch", "Verb-Switch")),
              y_position = max(test_means_exp1$mean_log + test_means_exp1$se_log) + 0.2,
              map_signif_level = TRUE,
              position = position_dodge(0.9)) +
  geom_signif(
    annotations = "**",
    y_position = max(test_means_exp1$mean_log + test_means_exp1$se_log) + 0.35,
    xmin = 1.9, xmax = 2.1,
    tip_length = 0.01,
    textsize = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Experiment 1: Mean Log Looking Time",
    x = "Phase",
    y = "Mean log-transformed looking time (s)"
  ) +
  labs(
    title = "Experiment 1: Mean Log Looking Time",
    x = "Phase",
    y = "Mean log-transformed looking time (s)",
    caption = "** indicates p < .01"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(2, NA))


####################################################################
########################### EXPERIMENT 2 ###########################
####################################################################

## Read Data ####
data_exp2 <- read.table("./Data Analysis/full_results_exp2_prosody.txt", 
                        header = TRUE, sep = "\t")
## Checking Data ####
str(data_exp2) # 检查变量类型

# 保证condition和phase为因子变量
data_exp2$condition <- as.factor(data_exp2$condition)
data_exp2$phase <- as.factor(data_exp2$phase)
data_exp2$subject <- as.factor(data_exp2$subject)

# 查看是否每个 subject 都有两行 (habituation/test)， 每人是否都有两阶段数据
table(table(data_exp2$subject))  # 核对无误，48个数据，每人2个phase

## Explore pic ####
# 可视化每个被试在两个阶段的注视时间变化
# 散点和折线图
ggplot(data_exp2, aes(x = phase, y = looking_time, 
                      color = condition, group = condition)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1), 
             alpha = 0.6) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(#title = "实验2: 短语韵律和虚词限制词义的习得",
    y = "looking time (seconds)", x = "phase") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14)

# Q-Q PLOT
qqnorm(data_exp2$looking_time)
qqline(data_exp2$looking_time)
#  注视时间呈现右偏态分布

## Log translation ####

exp2_clean <- data_exp2 %>%
  mutate(
    log_looking_time = log(data_exp2$looking_time + 1), # 避免 log(0) 错误
# log转换（提高模型正态性，原文也这么做）  注视时间的数据呈现右偏态分布，因此需要进行对数转换保证方差的齐性
# 根据韦伯-费希纳定，人对时间间隔的感知接近对数尺度
  ) %>%
  group_by(subject, condition, phase, log_looking_time) %>%
  summarise(mean_looking_time = mean(looking_time), .groups = "drop")

# Q-Q PLOT
qqnorm(exp2_clean$log_looking_time)
qqline(exp2_clean$log_looking_time)
# log转换后的注视时间呈现正态分布

## Summary average log_looking_time ####
# 汇总每个条件下每阶段的平均 log 注视时间
summary_data_2 <- exp2_clean %>%
  group_by(condition, phase) %>%
  summarise(mean_log = mean(log_looking_time),
            se_log = sd(log_looking_time)/sqrt(n()))

## LMM ####
#拟合线性混合效应模型
# 拟合模型：检验 phase（habituation vs test）和 condition（Noun vs Verb）交互作用
model_2 <- lmer(log_looking_time ~ condition * phase 
              + (1 | subject), data = exp2_clean)
# log_looking_time ~ condition * phase: 检查两个因素及其交互作用
# (1 | subject): 建模每个被试的随机截距，控制个体差异
# 如果每个条件下的多个试次数据，还可以加入 (1 | item)（此数据中没有）
# 主要关注交互项：conditionVerb-Switch:phaseTest
# 如果这个项显著（p < 0.05），说明 Verb 条件下从习惯化到测试的变化显著不同于 Noun 条件，支持你论文的理论假设。

# 输出模型结果（包含估计值、SE、t值、p值）
summary(model_2)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log_looking_time ~ condition * phase + (1 | subject)
# Data: exp2_clean
# REML criterion at convergence: 148.7

# Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
# -2.1733 -0.6043  0.1741  0.5641  2.3184 

# Random effects:
# Groups   Name        Variance Std.Dev.
# subject  (Intercept) 0.0604   0.2458  被试间的标准差为 0.246，说明存在一定的个体差异，但不是非常大
# Residual             0.2035   0.4511  残差标准差为 0.451，用于标准化效应量
# Number of obs: 96, groups:  subject, 48

# Fixed effects:
#                              Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                     2.63974    0.10485 87.41905  25.175   <2e-16 ***
# conditionVerb-Switch            0.03495    0.14829 87.41905   0.236   0.8142    
# phasetest                      -0.23677    0.13021 46.00000  -1.818   0.0755 .  
# conditionVerb-Switch:phasetest  0.41889    0.18415 46.00000   2.275   0.0276 *  
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
#           (Intr) cndV-S phstst
# cndtnVrb-Sw -0.707              
# phasetest   -0.621  0.439       
# cndtnVrb-S:  0.439 -0.621 -0.707

# 模型诊断
check_model(model_2)

# 简单效应分析
em_exp2 <- emmeans(model_2, ~ phase | condition)
pairs(em_exp2_condition)  # condition

# condition = Noun-Switch:
# contrast           estimate   SE df t.ratio p.value
# habituation - test    0.237 0.13 46   1.818  0.0755

# condition = Verb-Switch:
# contrast           estimate   SE df t.ratio p.value
# habituation - test   -0.182 0.13 46  -1.399  0.1686

# Degrees-of-freedom method: kenward-roger 

em_exp2_phase <- emmeans(model_2, ~ condition | phase)
pairs(em_exp2_phase)  # phase 

# phase = habituation:
#   contrast                      estimate    SE   df t.ratio p.value
# (Noun-Switch) - (Verb-Switch)  -0.0349 0.148 87.4  -0.236  0.8142

# phase = test:
#   contrast                      estimate    SE   df t.ratio p.value
# (Noun-Switch) - (Verb-Switch)  -0.4538 0.148 87.4  -3.061  0.0029

# Degrees-of-freedom method: kenward-roger 

# 计算效应量
effectsize_exp2 <- eff_size(em_exp2, sigma = sigma(model_2), 
                            edf = df.residual(model_2))
confint(effectsize_exp2)  # 效应量置信区间

# condition = Noun-Switch:
# contrast           effect.size    SE   df lower.CL upper.CL
# habituation - test       0.525 0.291 87.4  -0.0541    1.104

# condition = Verb-Switch:
# contrast           effect.size    SE   df lower.CL upper.CL
# habituation - test      -0.404 0.290 87.4  -0.9806    0.173

# sigma used for effect sizes: 0.4511 
# Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
# Confidence level used: 0.95 

## draw picture ####
test_means_exp2 <- summary_data_2 %>% filter(phase == "test")

ggplot(summary_data_2, aes(x = phase, y = mean_log, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
  geom_errorbar(aes(ymin = mean_log - se_log, ymax = mean_log + se_log),
                position = position_dodge(0.9), width = 0.2) +
  geom_signif(comparisons = list(c("Noun-Switch", "Verb-Switch")),
              y_position = max(test_means_exp1$mean_log + test_means_exp1$se_log) + 0.2,
              map_signif_level = TRUE,
              position = position_dodge(0.9)) +
  geom_signif(
    annotations = "*",
    y_position = max(test_means_exp1$mean_log + test_means_exp1$se_log) + 0.35,
    xmin = 1.9, xmax = 2.1,
    tip_length = 0.01,
    textsize = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Experiment 2: Mean Log Looking Time",
    x = "Phase",
    y = "Mean log-transformed looking time (s)"
  ) +
  labs(
    title = "Experiment 2: Mean Log Looking Time",
    x = "Phase",
    y = "Mean log-transformed looking time (s)",
    caption = "* indicates p < .05"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(2, NA))
