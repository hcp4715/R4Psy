
###########################################################################
###                                                                     ###
###                              EXPERIMENT 2:                          ###
###                   CONTOUR PROPERTIES MANIPULATION                   ###
###                                                                     ###
###########################################################################
setwd("/Users/dengxinyu/group7/our_code/experiment2")
#--------------------------------Import Data-----------------------------------------------------------------------
#Import half-split data
df2 <- read.csv("LD2.csv",stringsAsFactors=TRUE)

#--------Library--------------------------------------------------------------------------------------------------
#basic tools
library(dplyr)
library(tidyverse)
#visualizations
library(ggplot2)
library(ggstatsplot)
library(ggpubr)
#data summary tools
library(summarytools)
library(psych)
#linear mixed-effects modelling
library(lme4)
library(lmerTest)
library(MuMIn)
library(lmtest)
#random forest model tools
library(randomForest)
library(randomForestExplainer)
#cohen's d
library(effectsize)
#Anova
library(car)
library(afex)
#APA
library(papaja)

#--------Functions-------------------------------------------------------------------------------------------
# Function to calculate the mean and the standard deviation for each group
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}

#-------------------------Study 2: Summarize Data --------------------------------------------------------------------
#mean and sd for top and bottom-ranked contours
by(df2$response,factor(df2$condition),describe)

#-------------------------Study 2: Top vs Bottom Mean Diff---------------------------------------------------------------
#make dataframe for paired samples t-test
df2_paired <- df2 %>%
  dplyr::select(Imageidentity,stimulusitem1,response,condition) %>%
  dplyr::group_by(condition,Imageidentity)%>%
  dplyr::summarize(response = mean(response))
#paired t statistic
t.test(df2_paired$response[df2_paired$condition=="top"],df2_paired$response[df2_paired$condition=="bottom"],paired=TRUE)
#effect size (cohen's d)
effsize::cohen.d(df2_paired$response, factor(df2_paired$condition))

#-------------------------Study 2: Visualize Conditions-----------------------------------------------------------------
group.colors <- c(bottom = "steelblue", top = "darkred")
#dataframe summary
df2_summary <- data_summary(df2, varname="response", 
                            groupnames=c("condition", "category"))
#violin plot Figure 4C Left
ggp <- ggplot(df2_summary, aes(condition, response, fill = condition)) +            
  geom_violin(trim=FALSE, bw=0.2, position = "identity") + geom_jitter(alpha=0.1)
ggp <- ggp + scale_fill_manual(values=group.colors) + ylim(0,5)
ggp <- ggp + theme_minimal() + geom_boxplot(width=0.1, position=position_dodge(.9),outlier.shape = NA)
ggp + xlab("Contour Condition") + ylab("Mean Aesthetic Response") + theme_minimal()

#-------------------------Study 2: Figure 4C left 修改增加-----------------------------------------------------------------
# 重新映射条件标签
df2_paired$condition <- dplyr::recode(df2_paired$condition, "bottom" = "Low", "top" = "High")

# 设置颜色
group.colors <- c("Low" = "steelblue", "High" = "darkred")

# 计算平均值
low_mean <- mean(df2_paired$response[df2_paired$condition == "Low"])
high_mean <- mean(df2_paired$response[df2_paired$condition == "High"])

# 绘制风琴图和散点图
ggp <- ggplot(df2_paired, aes(x=condition, y=response, fill=condition)) +            
  geom_violin(trim=FALSE, bw=0.2, position="identity", alpha=0.5) +
  geom_jitter(aes(color=condition), width=0.4, height=0, alpha=0.5) +  # 添加散点
  geom_boxplot(width=0.1, position=position_dodge(0.9), outlier.shape=NA, alpha=0.5) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) +
  ylim(0, 5) +
  theme_minimal() +
  labs(x="Contour Condition", y="Observed Aesthetic Value") +
  theme(legend.position="none") +  # 隐藏图例
  geom_segment(aes(x=1, xend=2, y=low_mean, yend=high_mean), linetype="dashed", color="black") +  # 添加虚线
  annotate("text", x=1.5, y=(low_mean + high_mean) / 2 + 0.2, label="*", size=6)  # 添加星号
  papaja::theme_apa() # 应用APA风格的主题
  
# 显示图形
print(ggp)
#-------------------------Study 2: Figure 4C left 修改增加-----------------------------------------------------------------

#line plot Figure 4C right
df2_mean <- df2 %>%
  dplyr::group_by(condition,category) %>%
  dplyr::summarise(response = mean(response),
                   sd = sd(response))
p<- ggplot(df2_mean, aes(x=category, y=response, group=condition)) + geom_line(aes(linetype=condition))+
  geom_point(aes(shape=condition)) 
p+labs(title="Study 2: Category and Condition", x="Category", y = "Response")+ theme_classic() + scale_fill_manual(values=group.colors)

#-------------------------Study 2: Figure 4C right 修改-----------------------------------------------------------------
#自动调整标签位置
library(ggrepel)

df2_mean <- df2 %>%
  dplyr::group_by(condition, category) %>%
  dplyr::summarise(response = mean(response), sd = sd(response), .groups = 'drop') %>%
  mutate(
    xpos = ifelse(condition == "bottom", 1, 3),  # 为Low和High设置x位置
    text_xpos = xpos + ifelse(condition == "bottom", -0.15, 0.15)  # 调整标签x位置
  )

# 创建图表
p <- ggplot(df2_mean, aes(x=xpos, y=response, group=condition)) +
  geom_point(aes(color=condition), size=3) +
  geom_line(aes(group=category), color="grey") +  # 绘制分类组的连线
  geom_text_repel(
    aes(x=text_xpos, label=category, hjust=ifelse(condition=="bottom", 1, 0)),
    size=3, color="black", box.padding = 0.35, point.padding = 0.5,
    max.overlaps = Inf, direction = "y"  # 限制标签只在垂直方向上调整
  ) +
  scale_x_continuous(name="Condition", breaks=c(1, 3), labels=c("Low", "High"), limits=c(0.5, 3.5)) +
  scale_y_continuous(name="Observed Aesthetic Value", limits=c(1.4, 3.4)) +  # 设置Y轴范围为1到3.4
  labs(y = "Observed Aesthetic Value", title="Category and Condition") +
  theme_minimal() +
  scale_color_manual(values=c("bottom"="steelblue", "top"="darkred")) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        legend.position = "none")  # 设置图例不显示
  papaja::theme_apa() # 应用APA风格的主题
  
# 输出图表
print(p)

#-------------------------Study 2: Figure 4C right 修改-----------------------------------------------------------------

#-------------------------Study 2: Linear Mixed Effects Model---------------------------------------------------------------
mod_fac_slp_exp2 <- lmer(response ~ condition + DATscore + training_response+ PositiveAffectScore+
                           NegativeAffectScore + regionpast_response + gender_response +experience_response
                         + age_response + (1+condition|subject),
                         na.action = "na.exclude", data = df2,REML=T)
summary(mod_fac_slp_exp2)
anova(mod_fac_slp_exp2)
#confidence intervals #此处运行置信区间的代码会占用很长时间因此这里标签化隐藏处理,去#即可运行
confint(mod_fac_slp_exp2)
r.squaredGLMM(mod_fac_slp_exp2)


