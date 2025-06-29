.libPaths(c("D:/研究生/研一/R语言/R-library", .libPaths()))
# 加载必要的R包
library(mousetrap)  # 用于分析鼠标轨迹数据
library(ggplot2)    # 用于数据可视化
library(dplyr)      # 用于数据处理和转换
library(tidyr)      # 用于数据整理
library(afex)       # 用于高级统计分析
library(MBESS)      # 用于效应量计算
library(ordinal)    # 用于有序回归模型
library(emmeans)
# 设置自定义ggplot2主题，使图表更美观
theme_set(theme_classic()+ 
            theme(
              axis.line = element_line(colour = "black"),  # 设置坐标轴为黑色
              axis.ticks = element_line(colour = "black"), # 设置刻度线为黑色
              axis.text = element_text(colour = "black"),  # 设置坐标轴文本为黑色
              panel.border = element_rect(colour = "black", fill=NA)  # 设置面板边框为黑色
            ))

# 自定义函数：计算部分eta平方及其置信区间
get_partial_etas <- function(anova_table, conf.level=.90){
  partial_etas <- sapply(row.names(anova_table),function(i){
    F <- anova_table[i,"F"]  # 提取F值
    df1 <- anova_table[i,"num Df"]  # 提取分子自由度
    df2 <- anova_table[i,"den Df"]  # 提取分母自由度
    ci <- conf.limits.ncf(F.value=F,conf.level=conf.level,df.1=df1,df.2=df2)  # 计算非中心F分布的置信区间
    return(
      c(pes=((F*df1)/(F*df1+df2)),  # 计算部分eta平方
        lower=ci$Lower.Limit/(ci$Lower.Limit+df1+df2+1),  # 计算置信区间下限
        upper=ci$Upper.Limit/(ci$Upper.Limit+df1+df2+1)))  # 计算置信区间上限
  })
  return(t(partial_etas))  # 转置结果并返回
}

# 导入实验数据
raw_data <- read.csv(""D:/Documents/WeChat Files/wxid_14rv4xl3g24722/FileStorage/File/2025-06/osfstorage-archive/data/exp3.csv"")  # 从CSV文件读取数据
raw_data$Typicality <- factor(raw_data$Condition,levels=c("Typical","Atypical"))  # 创建Typicality因子变量
raw_data$group <- factor(raw_data$group,levels=c("static","rtmax","initmax","dynamic"))  # 设置group因子的水平顺序

# 分析正确率（包括所有试验）
with(raw_data,table(group, correct)/c(table(group)))  # 计算每个组的正确率

# 进行卡方检验，分析组别与正确率的关系
chisq.test(with(raw_data,table(group, correct)),correct = FALSE)

# 拟合广义线性混合模型，分析组别对正确率的影响
# 使用默认的对比编码（哑变量编码，以static组为基线）
contrasts(raw_data$group)  # 显示group变量的对比编码
summary(glmer(correct~(1|subject_nr)+group,family="binomial",data=raw_data))  # 拟合模型并显示结果

# 分析正确率（排除rtmax组中超过时间限制的试验）
# 统计rtmax组中符合条件和不符合条件的试验数量
n_eligible <- sum(with(raw_data,group=="rtmax" & response!="None"))  # 符合条件的试验数
n_noneligible <- sum(with(raw_data,group=="rtmax" & response=="None"))  # 不符合条件的试验数

# 计算rtmax组中超过总时间限制的试验百分比
n_noneligible/(n_eligible+n_noneligible)

# 排除不符合条件的试验
raw_data <- subset(raw_data, response!="None")

# 计算每个组的正确率（排除后）
with(raw_data,table(group, correct)/c(table(group)))

# 再次进行卡方检验
chisq.test(with(raw_data,table(group, correct)),correct = FALSE)

# 再次拟合广义线性混合模型
summary(glmer(correct~(1|subject_nr)+group,family="binomial",data=raw_data))

# 排除不正确的试验，仅保留正确的试验进行后续分析
raw_data <- subset(raw_data, correct==1)

# 鼠标轨迹数据预处理
mt_data <- mt_import_mousetrap(raw_data,  # 导入鼠标轨迹数据
                               xpos_label = c("xpos_initial_phase","xpos_get_response"),  # X坐标数据标签
                               ypos_label = c("ypos_initial_phase","ypos_get_response"),  # Y坐标数据标签
                               timestamps_label = c("timestamps_initial_phase","timestamps_get_response"))  # 时间戳标签
mt_data <- mt_remap_symmetric(mt_data)  # 对称重映射轨迹数据
mt_data <- mt_align_start(mt_data, start=c(0,0))  # 将轨迹起点对齐到(0,0)
mt_data <- mt_derivatives(mt_data)  # 计算轨迹的导数（速度和加速度）
mt_data <- mt_measures(mt_data)  # 计算轨迹的测量指标
mt_data <- mt_time_normalize(mt_data)  # 时间归一化处理

# 使用每个参与者的时间变量平均值进行操纵检查
# 添加时间变量到测量数据中
mt_data$measures$RT_initial <- mt_data$data$response_time_initial_phase  # 初始阶段反应时间
mt_data$measures$IT <- mt_data$measures$initiation_time  # 启动时间
mt_data$measures$RT_post <- mt_data$data$response_time_get_response  # 后续阶段反应时间

# 按参与者和条件聚合数据
agg_times <- mt_aggregate_per_subject(mt_data,
                                      use_variables = c("RT_initial","IT","RT","RT_post"),  # 要聚合的变量
                                      use2_variables = "group",  # 分组变量
                                      subject_id="subject_nr")  # 参与者ID变量

# 计算描述性统计量
mean_times <- agg_times %>%
  group_by(group) %>%
  summarize(
    N = n(),  # 每组样本量
    M_RT_inital = mean(RT_initial),  # 初始阶段平均反应时间
    SD_RT_initial = sd(RT_initial),  # 初始阶段反应时间标准差
    M_IT = mean(IT),  # 平均启动时间
    SD_IT = sd(IT),  # 启动时间标准差
    M_RT = mean(RT),  # 平均总反应时间
    SD_RT = sd(RT)  # 总反应时间标准差
  ) %>%
  as.data.frame()
print(mean_times, digits=5)  # 打印描述性统计结果

# 计算RT_post的描述性统计量（主要对dynamic组有意义）
agg_times %>%
  group_by(group) %>%
  summarize(
    M_RT_post = mean(RT_post),  # 平均后续阶段反应时间
    SD_RT_post = sd(RT_post)  # 后续阶段反应时间标准差
  )%>%
  as.data.frame()

# 指定用于对比分析的对比矩阵
contrast_matrix_separate <- list(
  rtmax_vs_static = c(-1,1,0,0),  # rtmax组与static组的对比
  initmax_vs_static = c(-1,0,1,0),  # initmax组与static组的对比
  dynamic_vs_static= c(-1,0,0,1))  # dynamic组与static组的对比

# 比较初始阶段反应时间
# 进行方差分析
anova_RT_initial <- aov_ez(data=agg_times,dv = "RT_initial", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_RT_initial,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_RT_initial$anova_table, conf.level=.90),2)

# 进行对比分析
anova_RT_initial_grid <- lsmeans(anova_RT_initial,~group)  # 计算最小二乘均值
contrast(anova_RT_initial_grid,contrast_matrix_separate)  # 执行指定的对比

# 比较启动时间
# 进行方差分析
anova_IT <- aov_ez(data=agg_times,dv = "IT", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_IT,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_IT$anova_table, conf.level=.90),2)

# 进行对比分析
anova_IT_grid <- lsmeans(anova_IT,~group)  # 计算最小二乘均值
contrast(anova_IT_grid,contrast_matrix_separate)  # 执行指定的对比

# 比较总反应时间
# 进行方差分析
anova_RT <- aov_ez(data=agg_times,dv = "RT", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_RT,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_RT$anova_table, conf.level=.90),2)

# 进行对比分析
anova_RT_grid <- lsmeans(anova_RT,~group)  # 计算最小二乘均值
contrast(anova_RT_grid,contrast_matrix_separate)  # 执行指定的对比

# 使用每个参与者的时间变量中位数进行操纵检查
# 按参与者和条件聚合数据（使用中位数）
agg_times <- mt_aggregate_per_subject(mt_data,
                                      use_variables = c("IT","RT_initial","RT"),  # 要聚合的变量
                                      use2_variables = "group",  # 分组变量
                                      subject_id="subject_nr",  # 参与者ID变量
                                      .funs="median")  # 使用中位数作为聚合函数

# 计算描述性统计量
mean_times <- agg_times %>%
  group_by(group) %>%
  summarize(
    N = n(),  # 每组样本量
    M_RT_inital = mean(RT_initial),  # 初始阶段平均反应时间
    SD_RT_initial = sd(RT_initial),  # 初始阶段反应时间标准差
    M_IT = mean(IT),  # 平均启动时间
    SD_IT = sd(IT),  # 启动时间标准差
    M_RT = mean(RT),  # 平均总反应时间
    SD_RT = sd(RT)  # 总反应时间标准差
  ) %>%
  as.data.frame()

print(mean_times, digits=5)  # 打印描述性统计结果

# 比较初始阶段反应时间（使用中位数聚合后的数据）
# 进行方差分析
anova_RT_initial <- aov_ez(data=agg_times,dv = "RT_initial", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_RT_initial,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_RT_initial$anova_table, conf.level=.90),2)

# 进行对比分析
anova_RT_initial_grid <- lsmeans(anova_RT_initial,~group)  # 计算最小二乘均值
contrast(anova_RT_initial_grid,contrast_matrix_separate)  # 执行指定的对比

# 比较启动时间（使用中位数聚合后的数据）
# 进行方差分析
anova_IT <- aov_ez(data=agg_times,dv = "IT", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_IT,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_IT$anova_table, conf.level=.90),2)

# 进行对比分析
anova_IT_grid <- lsmeans(anova_IT,~group)  # 计算最小二乘均值
contrast(anova_IT_grid,contrast_matrix_separate)  # 执行指定的对比

# 比较总反应时间（使用中位数聚合后的数据）
# 进行方差分析
anova_RT <- aov_ez(data=agg_times,dv = "RT", between = "group", id = "subject_nr")  # 拟合方差分析模型
nice(anova_RT,es = c("pes","ges"))  # 显示方差分析结果和效应量

# 计算部分eta平方及其90%置信区间
round(get_partial_etas(anova_RT$anova_table, conf.level=.90),2)

# 进行对比分析
anova_RT_grid <- lsmeans(anova_RT,~group)  # 计算最小二乘均值
contrast(anova_RT_grid,contrast_matrix_separate)  # 执行指定的对比