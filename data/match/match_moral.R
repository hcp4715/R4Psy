# 删除缺失值，选择符合标准的被试 
df4 <- tidyr::drop_na(df10)
# 删除含有缺失值的行 
# 首先加载必要的包
library(dplyr)

# 步骤1：确保 ACC 和 RT 是数值型
df4 <- df4 %>% 
  mutate(
    ACC = as.numeric(as.character(ACC)),  # 处理字符/因子型
    RT = as.numeric(as.character(RT))     # 顺便检查 RT
  )  # 注意这里闭合了mutate函数

# 步骤2：筛选数据
df6 <- df4 %>% 
  filter(
    Hand == "R",                          # 右利手
    ACC %in% c(0, 1),                     # 排除 ACC = -1 或 2
    RT >= 0.2 & RT <= 1.5                 # RT 范围
  ) %>%
  group_by(Sub, Shape, Label, Match) %>%  # 分组
  summarise(
    mean_ACC = mean(ACC, na.rm = TRUE),   # 忽略 NA
    mean_RT = mean(RT, na.rm = TRUE),     # 忽略 NA
    .groups = "drop"                      # 取消分组
  )
df6 <- dplyr::ungroup(df6)
# 将Shape变量拆分
df6 <- tidyr::extract(df6, Shape, into = c("Valence", "Identity"),
                      regex = "(moral|immoral)(Self|Other)", remove = FALSE) 
df6 <- dplyr::filter(df6, Match == "match" & Valence == "moral")  
# 将长数据转为宽数据 
df6 <- dplyr::select(df6, Sub, Identity, mean_RT)
df6 <- tidyr::pivot_wider(df6, names_from = "Identity", values_from = "mean_RT")
# 计算SPE
df6 <- dplyr::mutate(df6, moral_SPE = Self - Other)
df6 <- dplyr::select(df6, Sub, moral_SPE) 
