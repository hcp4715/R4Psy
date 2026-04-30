# R4Psy Chapter 6 & 7 数据分析 Agent 指南

本文档指导 AI Agent 帮助学生完成 R4Psy 课程第 6 章（数据预处理基础）和第 7 章（Tidyverse数据预处理实操）的分析任务。

> **注意**：第6章与第7章内容有较大重叠，第7章是对第6章的深化和补充。本指南将两章内容合并，提供完整的数据预处理指导。

---

## 学习目标总览

| 章节 | 核心技能 | 数据类型 |
|------|----------|----------|
| **第6章** | dplyr核心函数、问卷处理、LLM交互 | 问卷数据 |
| **第7章** | 批量读取、实验数据处理、完整管道 | 反应时数据(.out) |

---

## 核心数据

### 1. 问卷数据：penguin 数据集

**路径**：`slides/data/penguin/penguin_rawdata.csv`

```r
# 加载方式
df <- bruceR::import(here::here("slides", "data", "penguin", "penguin_rawdata.csv"))
```

**典型变量**：

| 变量名 | 类型 | 说明 |
|--------|------|------|
| Site | character | 站点：Tsinghua, Peking 等 |
| Sex | character | 性别：M, F |
| age | numeric | 年龄 |
| weight_kg | numeric | 体重(kg) |
| height_cm | numeric | 身高(cm) |
| DEQ | numeric | 抑郁问卷得分 |
| ALEX1:ALEX16 | numeric | ALEX问卷题项(1-5) |

### 2. 实验数据：反应时数据

**路径**：`slides/data/match/data_exp7_rep_match_*.out`

```r
# 查找文件
files <- list.files(
  here::here("slides", "data", "match"),
  pattern = "data_exp7_rep_match_.*\\.out$",
  full.names = TRUE
)
```

**变量说明**：

| 变量名 | 说明 |
|--------|------|
| Sub | 被试编号 |
| Shape | 刺激类型：moralSelf, moralOther, immoralSelf, immoralOther |
| Label | 标签 |
| Match | 匹配条件：match, mismatch |
| ACC | 正确率：0(错误), 1(正确), -1/2(无效) |
| RT | 反应时(ms)，有效范围[200, 1500] |

---

## 典型任务模式

### 任务 6.1：问卷数据处理

```
**背景**：处理 ALEX 问卷数据
**数据**：df 包含 Site, Sex, age, ALEX1:ALEX16
**需求**：
1. 选择需要的变量（Site, Sex, age, ALEX1:ALEX16）
2. 删除缺失值
3. 反向计分：ALEX4, ALEX12, ALEX14, ALEX16（6-原始分）
4. 计算 ALEX 总分（rowSums 或 across）
5. 按 Site 分组计算均值和标准差
```

**完整代码**：

```r
library(tidyverse)

# 数据类型转换（确保数值型正确）
df2 <- df %>%
  mutate(
    age = as.numeric(age),
    DEQ = as.numeric(DEQ)
  )

# 完整处理流程
df_result <- df2 %>%
  select(Site, Sex, age, ALEX1:ALEX16) %>%          # 1. 选择变量
  drop_na() %>%                                   # 2. 删除缺失值
  mutate(
    ALEX4 = 6 - ALEX4,                          # 3. 反向计分
    ALEX12 = 6 - ALEX12,
    ALEX14 = 6 - ALEX14,
    ALEX16 = 6 - ALEX16,
    ALEX_total = rowSums(across(starts_with("ALEX")))  # 4. 计算总分
  ) %>%
  group_by(Site) %>%                            # 5. 分组
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_ALEX = mean(ALEX_total, na.rm = TRUE),
    sd_ALEX = sd(ALEX_total, na.rm = TRUE),
    .groups = "drop"
  )
```

### 任务 6.2：实验数据处理（反应时）

```
**背景**：处理 match 实验的反应时数据
**数据**：多个 .out 文件（slides/data/match/）
**需求**：
1. 批量读取所有被试数据
2. 筛选有效试次（ACC==0|1, RT 200-1500ms）
3. 按被试(Sub)和条件(Shape,Label,Match)计算均值
4. 拆分 Shape 变量（moral/immoral + Self/Other）
5. 长转宽，计算自我优势效应 SPE
```

**第一步：定义数据类型转换函数**

```r
convert_data_types <- function(df) {
  df %>%
    mutate(
      Sub = as.numeric(Sub),
      Age = as.numeric(Age),
      Block = as.numeric(Block),
      Bin = as.numeric(Bin),
      Trial = as.numeric(Trial),
      Shape = as.character(Shape),
      Label = as.character(Label),
      Match = as.character(Match),
      CorrResp = as.character(CorrResp),
      Resp = as.character(Resp),
      ACC = as.numeric(ACC),
      RT = as.numeric(RT)
    )
}
```

**第二步：批量读取并清洗**

```r
# 方法1：for loop
df_all <- NULL
for (i in seq_along(files)) {
  df <- read.table(files[i], header = TRUE)
  df <- filter(df, Date != "Date")
  df <- convert_data_types(df)
  df_all <- bind_rows(df_all, df)
}

# 方法2：lapply（更简洁）
df_all <- files %>%
  lapply(function(f) {
    read.table(f, header = TRUE) %>%
      filter(Date != "Date") %>%
      convert_data_types()
  }) %>%
  bind_rows()
```

**第三步：筛选有效试次**

```r
df_clean <- df_all %>%
  filter(
    Hand == "R",                    # 只用右手
    ACC == 0 | ACC == 1,             # 正确或错误（有按键）
    RT >= 0.2 & RT <= 1.5           # RT范围200-1500ms
  ) %>%
  drop_na()
```

**第四步：分组计算均值**

```r
df_means <- df_clean %>%
  group_by(Sub, Shape, Label, Match) %>%
  summarise(
    mean_RT = mean(RT),
    mean_ACC = mean(ACC),
    .groups = "drop"
  )
```

**第五步：拆分变量**

```r
df_means <- df_means %>%
  extract(
    col = Shape,
    into = c("Valence", "Identity"),
    regex = "(moral|immoral)(Self|Other)",
    remove = FALSE
  )
```

**第六步：长转宽，计算 SPE**

```r
df_spe <- df_means %>%
  filter(Match == "match", Valence == "moral") %>%
  select(Sub, Identity, mean_RT) %>%
  pivot_wider(
    names_from = "Identity",
    values_from = "mean_RT"
  ) %>%
  mutate(moral_SPE = Self - Other)

# 查看结果
summary(df_spe$moral_SPE)
```

---

## dplyr 核心函数总结

| 函数 | 功能 | 典型用法 |
|------|------|----------|
| `filter()` | 筛选行 | `filter(df, condition1, condition2)` |
| `select()` | 选择列 | `select(df, col1, col2)` / `select(df, starts_with("ALEX"))` |
| `mutate()` | 创建/修改列 | `mutate(df, new = old * 2)` |
| `arrange()` | 排序 | `arrange(df, desc(x))` |
| `group_by()` | 分组 | `group_by(df, group_var)` |
| `summarise()` | 汇总 | `summarise(df, mean(x), .groups = "drop")` |
| `filter()` | 筛选行 | `filter(df, condition)` |
| `across()` | 批量操作 | `across(starts_with("ALEX"), mean)` |
| `case_when()` | 条件赋值 | `case_when(condition ~ value)` |

---

## tidyr 函数总结

| 函数 | 功能 | 典型用法 |
|------|------|----------|
| `pivot_wider()` | 长转宽 | `pivot_wider(names_from, values_from)` |
| `pivot_longer()` | 宽转长 | `pivot_longer(cols, names_to, values_to)` |
| `extract()` | 提取字符 | `extract(col, into, regex)` |
| `separate()` | 分列 | `separate(col, into, sep)` |
| `unite()` | 合列 | `unite(col, ...)` |
| `drop_na()` | 删除缺失值 | `drop_na()` |

---

## 完整管道汇总

### 问卷处理管道

```r
df_result <- df %>%
  select(Site, Sex, age, DEQ, ALEX1:ALEX16) %>%
  mutate(
    age = as.numeric(age),
    DEQ = as.numeric(DEQ)
  ) %>%
  drop_na() %>%
  mutate(
    ALEX4 = 6 - ALEX4,
    ALEX12 = 6 - ALEX12,
    ALEX14 = 6 - ALEX14,
    ALEX16 = 6 - ALEX16,
    ALEX_total = rowSums(across(starts_with("ALEX")))
  ) %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_DEQ = mean(DEQ, na.rm = TRUE),
    mean_ALEX = mean(ALEX_total, na.rm = TRUE),
    .groups = "drop"
  )
```

### 实验数据处理管道

```r
df_spe <- files %>%
  lapply(function(f) {
    read.table(f, header = TRUE) %>%
      filter(Date != "Date") %>%
      convert_data_types()
  }) %>%
  bind_rows() %>%
  filter(
    Hand == "R",
    ACC == 0 | ACC == 1,
    RT >= 0.2 & RT <= 1.5
  ) %>%
  drop_na() %>%
  group_by(Sub, Shape, Label, Match) %>%
  summarise(mean_RT = mean(RT), .groups = "drop") %>%
  extract(Shape, into = c("Valence", "Identity"),
          regex = "(moral|immoral)(Self|Other)") %>%
  filter(Match == "match", Valence == "moral") %>%
  select(Sub, Identity, mean_RT) %>%
  pivot_wider(names_from = "Identity", values_from = "mean_RT") %>%
  mutate(moral_SPE = Self - Other)
```

---

## LLM 交互 Prompt 模板

### 模板 1：背景/数据/需求/约束

```
**背景**：我正在学习第6/7章的数据预处理
**数据**：df 已加载，包含 [变量列表]
**需求**：
1. [具体操作1]
2. [具体操作2]
**约束**：使用 dplyr/tidyverse，正确处理缺失值
```

### 模板 2：先理顺思路

```
我现在想解决 [具体问题]，但还没有完全想清楚思路。
请先帮我理顺从初始状态到目标状态的过程：
1. 当前的初始状态是什么？
2. 目标状态是什么？
3. 需要哪几个步骤？
```

### 模板 3：analysis_notes.md 格式

```markdown
# Analysis Notes - [分析名称]

## 研究问题
[要回答的具体问题]

## 数据来源
- 文件位置：[路径]
- 文件格式：[格式]

## 关键变量
- [变量名]: [说明]
- ...

## 计算逻辑
1. [步骤1]
2. [步骤2]
3. [最终指标计算方式]
```

---

## 数据文件路径参考

```
# 问卷数据
here::here("slides", "data", "penguin", "penguin_rawdata.csv")

# 实验数据（反应时）
here::here("slides", "data", "match", "data_exp7_rep_match_*.out")
```

---

## 常见错误与排查

| 错误 | 原因 | 解决方案 |
|------|------|----------|
| `object not found` | 变量名拼写错误 | 检查 `names(df)` |
| `NA | TRUE` 缺失 | 使用 `na.rm = TRUE` |
| 类型不匹配 | 字符型用于数值计算 | 使用 `as.numeric()` 转换 |
| `Error in select` | 列名不在数据中 | 检查 `names(df)` |
| `Error in pivot_wider` | 唯一键不唯一 | 先 group_by 再 pivot |
| `.out 文件读取失败` | 分隔符问题 | 使用 `read.table()` 配合 `header = TRUE` |

---

## 评估标准

在帮助学生时确保：

- [ ] 代码能无报错运行
- [ ] 结果符合预期（验证行数、范围）
- [ ] 处理了缺失值（检查 `drop_na()` 或 `filter(!is.na())`）
- [ ] 添加了必要注释
- [ ] 使用了推荐的包（tidyverse/dplyr）
- [ ] 实验数据正确筛选了有效试次（ACC, RT 范围）