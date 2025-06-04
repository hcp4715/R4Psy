library(here)
data_new <- read.csv(here("data", "match", "df_matching_raw.csv"))

# 删除缺失值，选择符合标准的被试 
df10 <- tidyr::drop_na(data_new)
# 删除含有缺失值的行 
# 首先加载必要的包
library(dplyr)
library(tidyr)
library(readr)

library(dplyr)
library(tidyr)
library(readr)

# 步骤1：计算各条件下的dprime（已去除trials列）
dprime_wide <- df10 %>% 
  filter(
    Hand == "R",
    ACC %in% c(0, 1),
    between(RT, 0.2, 1.5)
  ) %>%
  extract(
    Shape,
    into = c("Valence", "Identity"),
    regex = "(moral|immoral)(Self|Other)"
  ) %>%
  group_by(Subject, Valence, Identity) %>%
  summarise(
    hit = sum(Match == "match" & ACC == 1),
    fa = sum(Match == "mismatch" & ACC == 0),
    miss = sum(Match == "match" & ACC == 0),
    cr = sum(Match == "mismatch" & ACC == 1),
    hit_rate = (hit + 0.5) / (hit + miss + 1),
    fa_rate = (fa + 0.5) / (fa + cr + 1),
    dprime = qnorm(hit_rate) - qnorm(fa_rate),
    .groups = "drop"
  ) %>%
  select(-hit, -fa, -miss, -cr, -hit_rate, -fa_rate) %>%  # 移除中间计算列
  mutate(dprime = round(dprime, 3)) %>%  # 保留3位小数
  pivot_wider(
    names_from = c(Valence, Identity),
    values_from = dprime,
    names_glue = "{Identity}_{Valence}_dprime"
  )

# 步骤2：保存为CSV文件
write_csv(dprime_wide, "subject_dprime_results.csv")

# 查看结果
print(dprime_wide, width = Inf)