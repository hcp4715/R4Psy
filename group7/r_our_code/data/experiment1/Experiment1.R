
############################################################################
###                                                                      ###
###                               EXPERIMENT 1                           ###
###                                                                      ###
###                                                                      ###
############################################################################

setwd("/Users/dengxinyu/group7/our_code/experiment1")
#--------------------------------Import Data-------------------------------------------------------------------------------------------
#Import Intact Line Drawings Data
LD1 <- read.csv("LD1.csv",stringsAsFactors=TRUE)
#Import Color Photo Data
CP <- read.csv("CP.csv",,stringsAsFactors=TRUE)
#--------------------------------Load Library--------------------------------------------------------------------------------------------------
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


#-----------------------------------Functions-------------------------------------------------------------------------------------------
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
  return(data_sum)
}

############################################################################
###                                                                      ###
###                               STUDY 1:                               ###
###                         INTACT LINE DRAWINGS                         ###
###                                                                      ###
############################################################################

#-------------------------Study 1: Summarize LD Data ----------------------------------------------------------------------------
#Average ratings by category (raw scores)
by(LD1$pleasure_m,factor(LD1$category),describe)
#Average ratings by category (normalized scores)
by(LD1$Z_pleasure,factor(LD1$category),describe)

#-------------------------Study 1: Match LD with Color Photo Data-------------------------------------------------------------------
#combined color and LD dataset 
LD <- subset.data.frame(LD1, select = c("ImageName","pleasure_m"))
CPLD <- merge(x=CP,y=LD,by.x="imageName",by.y="ImageName",all.x=TRUE)
CPLD$pleasure_LD <- CPLD$pleasure_m
CPLD$pleasure_CP <- CPLD$mean_pleasure
library(reshape2)
CPLD_long <- melt(CPLD, id.vars=c("imageName"), 
                  measure.vars=c("pleasure_LD", "pleasure_CP"),
                  variable.name="ImageType",
                  value.name="response")
#add category information
CPLD_long <- CPLD_long %>% mutate(category = case_when(
  str_detect(.$imageName, "mountains_") ~ "mountains",
  str_detect(.$imageName, "city_") ~ "city",
  str_detect(.$imageName, "forests_") ~ "forests",
  str_detect(.$imageName, "beaches_") ~ "beaches",
  str_detect(.$imageName, "highways_") ~ "highways",
  str_detect(.$imageName, "offices_") ~ "offices",
  TRUE ~ as.character(.$imageName)))
CPLD_long$category <- as.factor(CPLD_long$category)

#add category information
CP <- CP %>% mutate(category = case_when(
  str_detect(.$imageName, "mountains_") ~ "mountains",
  str_detect(.$imageName, "city_") ~ "city",
  str_detect(.$imageName, "forests_") ~ "forests",
  str_detect(.$imageName, "beaches_") ~ "beaches",
  str_detect(.$imageName, "highways_") ~ "highways",
  str_detect(.$imageName, "offices_") ~ "offices",
  TRUE ~ as.character(.$imageName)))
CP$category <- as.factor(CP$category)

#-------------------------Study 1: Summarize ColorPhoto Data-------------------------------------------------------------------
#Average ratings by category (raw scores)
by(CP$mean_pleasure,factor(CP$category),describe)
#Average ratings by category (normalized scores)
by(CP$Z_pleasure,factor(CP$category),describe)

#-------------------------Study 1: Figure 3C -----------------------------------------------------------------
group.colors2 <- c(LD_pleasure = "steelblue", CP_pleasure = "darkred")
#dataframe summary
CPLD_summary <- data_summary(CPLD_long, varname="response", 
                            groupnames=c("ImageType", "category"))
#raw version of Figure 3C
p<- ggplot(CPLD_summary, aes(x=category, y=response, group=ImageType)) + 
  geom_line(aes(linetype=ImageType)) +
  geom_point(aes(shape=ImageType))
p+labs(title="Experiment 1: LD - CP", x="Category", y = "Response")+
  theme_classic() 
#-------------------------Study 1: Figure 3C 修改版 ---------------------------------------------------------------
#自动调整标签位置
library(ggrepel)
# 设定颜色
group.colors2 <- c("Line Drawings" = "black", "Photographs" = "mediumturquoise")

# 数据汇总
CPLD_summary <- data_summary(CPLD_long, varname="response", groupnames=c("ImageType", "category"))

# 为每个类型分配固定的x值，并进行微调使散点不太靠近边缘
CPLD_summary$xpos <- ifelse(CPLD_summary$ImageType == "pleasure_LD", 2, 3)  # 适当增加x坐标

# 转换ImageType为更易理解的标签
CPLD_summary$ImageType <- factor(CPLD_summary$ImageType, levels = c("pleasure_LD", "pleasure_CP"),
                                 labels = c("Line Drawings", "Photographs"))

# 绘图
p <- ggplot(CPLD_summary, aes(x=xpos, y=response, group=category, color=ImageType)) +
  geom_point(size=4) +  # 绘制点
  geom_line(aes(group=category), linewidth=1, color="grey") +  # 连接线
  geom_text_repel(aes(label=category, x=ifelse(ImageType == "Line Drawings", xpos - 0.3, xpos + 0.3), hjust=ifelse(ImageType == "Line Drawings", 1, 0)), vjust=0, show.legend = FALSE, nudge_y=0.1, direction="y", segment.color = NA) +  # 添加类别标签，调整位置
  scale_color_manual(values=group.colors2) +
  labs(title="Experiment 1: LD - CP", y="Mean Aesthetic Value", color=NULL) +  # 去掉图例标题
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),  # 清除x轴相关元素
    panel.grid.major.y = element_blank(),  # 去掉主横向网格线
    panel.grid.minor.y = element_blank(),  # 去掉次横向网格线
    axis.line.x = element_line(color = "black"),  # 添加x轴线
    axis.line.y = element_line(color = "black")   # 添加y轴线
  ) +
  scale_x_continuous(breaks = c(2, 3), labels = c("Line Drawings", "Photographs"), limits = c(1, 4))  # 调整x轴范围和标签
  papaja::theme_apa() # 应用APA风格的主题
  
# 显示图形
print(p)
