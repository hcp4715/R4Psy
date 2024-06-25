
###########################################################################
###                                                                     ###
###                       EXPERIMENT 3:                                 ###
###                    CONTOUR GROUPING MANIPULATION                    ###
###                                                                     ###
###########################################################################
setwd("/Users/dengxinyu/group7/our_code/experiment3")
#Import data
df3 <- read.csv("LD3.csv", stringsAsFactors=TRUE)
#Import random forest model residual data
cross <- read.csv("cross.csv")


#--------Library---------------------------------------------------------------------------------------
library(psych)
library(tidyverse)
#linear mixed-effects modelling
library(lme4)
library(lmerTest)
library(MuMIn)
library(lmtest)
#cohen's d
library(effectsize)
library(ppcor)
#APA
library(papaja)

#-------------------------Study 3: Summarize Data----------------------------------------------------------
by(df3$response,factor(df3$cond),describe)

#category information
df3 <- df3 %>% mutate(category = case_when(
  str_detect(.$stimulusitem1, "mountains_") ~ "mountains",
  str_detect(.$stimulusitem1, "city_") ~ "city",
  str_detect(.$stimulusitem1, "forests_") ~ "forests",
  str_detect(.$stimulusitem1, "beaches_") ~ "beaches",
  str_detect(.$stimulusitem1, "highways_") ~ "highways",
  str_detect(.$stimulusitem1, "offices_") ~ "offices",
  TRUE ~ as.character(.$stimulusitem1)))
df3$category <- as.factor(df3$category)
by(df3$response,factor(df3$category),describe)

#-------------------------Experiment 3: Anova (top vs bottom condition)-------------------------------------------
interaction.plot(df3$cond,df3$manipulation,df3$response)
mod <- aov(response~cond+manipulation+cond:manipulation, data = df3)
anova(mod)
#mean comparisons
print(TukeyHSD(mod))
cohens_f(mod)

#-------------------------Experiment 3: Figure 7 C---------------------------------------------------------------
group.colors <- c(bottom = "steelblue", top = "darkred")
df3_upright <- df3 %>%
  dplyr::filter(manipulation=="upright")

df3_upright_mean <- df3_upright %>%
  dplyr::group_by(image,cond) %>%
  dplyr::summarise(mean_response = mean(response))

#upright condition
ggp <- ggplot(df3_upright_mean, aes(cond, mean_response, fill = cond)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()

#inverted condition
df3_inverted <- df3 %>%
  dplyr::filter(manipulation=="inverted")

df3_inverted_mean <- df3_inverted %>%
  dplyr::group_by(image,cond) %>%
  dplyr::summarise(mean_response = mean(response))
#plot
ggp <- ggplot(df3_inverted_mean, aes(cond, mean_response, fill = cond)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()
#-------------------------Experiment 3: Figure 8 b 修改 无虚线&星号-----------------------------------------------------
library(ggplot2)
library(dplyr)

# 创建一个包含正立和倒立数据的综合数据框
df3_combined <- df3 %>%
  mutate(condition = ifelse(manipulation == "upright", "Upright", "Inverted")) %>%
  dplyr::group_by(image, cond, condition) %>%
  dplyr::summarise(mean_response = mean(response))

# 确保Upright和Inverted顺序正确
df3_combined$condition <- factor(df3_combined$condition, levels = c("Upright", "Inverted"))

# 定义颜色
group.colors <- c("bottom" = "steelblue", "top" = "darkred")
# 绘制综合图表
ggp_combined <- ggplot(df3_combined, aes(cond, mean_response, fill = cond)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + 
  scale_fill_manual(values=group.colors) + 
  ylim(0,5) +
  theme_minimal() + 
  geom_boxplot(width=0.1, position=position_dodge(.9), outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none") + # 去掉图例
  xlab(NULL) + 
  ylab("Observed Aesthetic Value") +
  facet_wrap(~condition, scales = "free_x") +
  theme(strip.text.x = element_text(size=12, face="bold")) # 添加标签并调整字体大小
  papaja::theme_apa() # 应用APA风格的主题

print(ggp_combined)


#-------------------------Experiment 3: Linear Mixed Effects Model--------------------------------------------------------
mod_fac_slp_exp3 <- lmer(response ~ cond + PositiveAffectScore+ CB + IAE + AA +
                           NegativeAffectScore + region + gender +
                           + age + (1+cond|subject),
                         na.action = "na.exclude", data = df3,REML=T)
summary(mod_fac_slp_exp3)
anova(mod_fac_slp_exp3)
#confint(mod_fac_slp_exp3) #此处运行置信区间的代码会占用很长时间因此这里标签化隐藏处理,去#即可运行
r.squaredGLMM(mod_fac_slp_exp3)

#-------------------------Experiment 2 and 3 Comparison-------------------------------------------------------------------
## variance Partitioning

#Y column values are the predictions from intact line drawing model in experiment 1
#Y1 column values are predictions from experiment 2
#Y2 column values are predictions from experiment 3
mod <- lm(cross$Y ~ cross$Y1 + cross$Y2)
mod1 <- lm(cross$Y ~ cross$Y1)
mod2 <- lm(cross$Y ~ cross$Y2)

#partial correlation
r_partial <- pcor(cross)$estimate[3,c('Y1','Y2')]
r_partial^2
#semi-partial/part correlation
r_part <- spcor(cross)$estimate[3,c('Y1','Y2')]
r_part^2

#-------------------------Experiment 2 and 3 Comparison修改版---------------------------------------------------
#Y column values are the predictions from intact line drawing model in experiment 1
#Y1 column values are predictions from experiment 2
#Y2 column values are predictions from experiment 3
mod <- lm(cross$Y ~ cross$Y1 + cross$Y2) #评估实验2（Y1）和实验3（Y2）的预测值对实验1（Y）的预测值总方差的解释程度
mod1 <- lm(cross$Y ~ cross$Y1) #评估实验2对实验1预测值的方差的独立解释程度
mod2 <- lm(cross$Y ~ cross$Y2) #评估实验3对实验1预测值的方差的独立解释程度
# 计算和显示 mod1 的详细统计信息
summary(mod1)

# 计算和显示 mod2 的详细统计信息
summary(mod2)

# 提取 R-squared 值
r_squared_mod1 <- summary(mod1)$r.squared
r_squared_mod2 <- summary(mod2)$r.squared

# 提取 F-statistics 值
f_stat_mod1 <- summary(mod1)$fstatistic
f_stat_mod2 <- summary(mod2)$fstatistic

#partial correlation
r_partial <- pcor(cross)$estimate[3,c('Y1','Y2')] # 实验2的预测值（Y1） 和实验3的预测值（ Y2 ）之间的偏相关系数及其平方
r_partial2<-r_partial^2
r_partial 
r_partial2

#semi-partial/part correlation
r_part <- spcor(cross)$estimate[3,c('Y1','Y2')] # 实验2的预测值（Y1） 和实验3的预测值（ Y2 ）之间的半偏相关系数及其平方
r_part^2



