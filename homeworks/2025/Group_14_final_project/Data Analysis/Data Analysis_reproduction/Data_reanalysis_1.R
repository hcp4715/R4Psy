# 如果还没安装，请先安装 bruceR 和 tidyverse
install.packages("bruceR")
install.packages("tidyverse")

# 加载所需包
library(bruceR)
library(tidyverse)

# 读取数据
data_exp1 <- read.table("E:/R final report/Data/Experiment 1_ Function Words/full_results_exp1_fwords.txt",
                        header = TRUE, sep = "\t")
# 平均注视时长（每位被试、每个条件、每个阶段）
data_figs <- data_exp1 %>%
  group_by(subject, condition, phase) %>%
  summarise(looking_time = mean(looking_time)) %>%
  ungroup()

# 求组平均和标准误
data_figs_sum <- data_figs %>%
  group_by(condition, phase) %>%
  summarise(
    mean_looking = mean(looking_time),
    sd_looking = sd(looking_time),
    count = n()
  ) %>%
  mutate(
    se_looking = sd_looking / sqrt(count),
    se_looking_low = mean_looking - se_looking,
    se_looking_high = mean_looking + se_looking
  )

# 画图（与原图风格一致）
library(ggplot2)
graph_results_exp1 <- ggplot(data_figs_sum, aes(x=condition, y=mean_looking, fill=phase)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge()) +
  geom_errorbar(aes(ymin=se_looking_low, ymax=se_looking_high),
                position=position_dodge(0.5), width=0.2) +
  ylim(0, 25) +
  scale_fill_manual(values=c("#1f78b4", "#8dd3c7")) +
  labs(x="Condition", y="Mean looking time (seconds)") +
  theme_bw() +
  theme(text=element_text(size=25), title=element_text(size=25),
        axis.line = element_line(colour="black"),
        legend.position="top",
        legend.key=element_blank())

# 输出图像
print(graph_results_exp1)
# 先取对数变换（因为原分析做了对数转换使其符合正态性）
data_exp1_log <- data_exp1 %>%
  mutate(looking_time = log10(looking_time)) %>%
  group_by(subject, condition, phase) %>%
  summarise(looking_time = mean(looking_time)) %>%
  ungroup()

# 宽格式转换（每个阶段一列）
data_wide_log <- data_exp1_log %>%
  pivot_wider(names_from = phase, values_from = looking_time) %>%
  mutate(condition = factor(condition))  # 确保因子类型

# MANOVA 重复测量分析
MANOVA(
  data = data_exp1_log,
  dv = "looking_time",
  subID = "subject",
  within = "phase",
  between = "condition"
)

# 用于 Q-Q plot 和 Shapiro-Wilk 检验
variables_exp1_log <- data_exp1_log %>%
  group_by(subject, condition, phase) %>%
  summarise(looking_time = mean(looking_time)) %>%
  ungroup()

# 正态性检验
qqnorm(variables_exp1_log$looking_time)
qqline(variables_exp1_log$looking_time)
shapiro.test(variables_exp1_log$looking_time)
# 比较 log10 后的平均值（来自图中注释）
M1 <- -0.03  # Noun-switch
M2 <-  0.15  # Verb-switch
n1 <- 24
n2 <- 24
SD1 <- 0.29
SD2 <- 0.25

# 计算 pooled SD
SDpooled <- sqrt(((n1-1)*SD1^2 + (n2-1)*SD2^2) / (n1 + n2 - 2))
d <- (M2 - M1) / SDpooled
print(d)  # d ≈ 0.665
