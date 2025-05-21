#chapter 9

# Packages
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}

pacman::p_load(
  # 本节课需要用到的 packages
  here, tidyverse, bruceR, DT, car,
  # 生成课件
  xaringan, xaringanthemer, xaringanExtra)

# 改变R在显示大数字和小数字时是选择常规格式还是科学计数法的倾向
options(scipen=999)

# 还原设置 options(scipen = 0)

# 8.0 线性回归回顾
# 8.0.2 一个简单的线性回归的R代码
df.penguin <- bruceR::import(here::here('data', 'penguin', 'penguin_rawdata.csv')) %>%
  dplyr::mutate(subjID = row_number()) %>%
  dplyr::select(subjID,Temperature_t1, Temperature_t2) %>%         # 选择变量
  dplyr::filter(!is.na(Temperature_t1) & !is.na(Temperature_t2)) 

DT::datatable(head(df.penguin),
              fillContainer = TRUE, options = list(pageLength = 4))

df.penguin %>%
  ggplot2::ggplot(aes(x = Temperature_t1, y = Temperature_t2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic()

m1 <- stats::lm(Temperature_t2 ~ Temperature_t1,  # 公式
                data = df.penguin,                # 数据框
)

# 使用 broom 包的 glance() 函数来提取模型摘要信息
m1_summary <- broom::tidy(m1)

# 使用 knitr 包的 kable() 函数来创建一个格式化的表格
knitr::kable(m1_summary, caption = "Model Summary")

bruceR::model_summary(m1)

# 8.0.3 bruceR::regress
m2 <- bruceR::regress(Temperature_t2 ~ Temperature_t1,  # 公式
                      data = df.penguin) # 数据

# 8.1 *t*-test
# 8.1.1 独立样本*t*检验(independent *t*-test)

df.penguin <- bruceR::import(here::here('data', 'penguin', 'penguin_rawdata.csv')) %>%
  dplyr::mutate(subjID = row_number()) %>%
  dplyr::select(subjID,Temperature_t1, Temperature_t2, socialdiversity, 
                Site, DEQ, romantic, ALEX1:ALEX16) %>%                                     # 选择变量
  dplyr::filter(!is.na(Temperature_t1) & !is.na(Temperature_t2) & !is.na(DEQ)) %>%         # 处理缺失值
  dplyr::mutate(romantic = factor(romantic, levels = c(1,2), labels = c("恋爱", "单身")),  # 转化为因子
                Temperature = rowMeans(select(., starts_with("Temperature"))),             # 计算两次核心温度的均值
                ALEX4  = case_when(TRUE ~ 6 - ALEX4),
                ALEX12 = case_when(TRUE ~ 6 - ALEX12),
                ALEX14 = case_when(TRUE ~ 6 - ALEX14),
                ALEX16 = case_when(TRUE ~ 6 - ALEX16),
                ALEX   = rowSums(select(., starts_with("ALEX")))) # 反向计分后计算总分

DT::datatable(head(df.penguin),
              fillContainer = TRUE, options = list(pageLength = 4))

stats::t.test(data = df.penguin,      # 数据框
              Temperature ~ romantic, # 因变量~自变量
              var.equal = TRUE) %>%
  capture.output()                    # 将输出变整齐

# 8.1 *t*-test作为回归模型的特例
# 8.1.2 线性回归(linear regression)

# t检验
stats::t.test(data = df.penguin,
              Temperature ~ romantic,
              var.equal = TRUE) %>%
  capture.output()                    # 将输出变整齐

# 线性回归
model.inde <- stats::lm(
  data = df.penguin,
  formula = Temperature ~ 1 + romantic
)
summary(model.inde)

# 8.1.3 单样本*t*检验(one sample *t*-test)

stats::t.test(
  x = df.penguin$Temperature, # 核心体温均值
  mu = 36.6) 

model.single <- lm(
  data = df.penguin,
  formula = Temperature - 36.6 ~ 1
) 
summary(model.single) 

# 8.1.4 配对样本*t*检验(paired *t*-test)

stats::t.test(
  x = df.penguin$Temperature_t1, 
  y = df.penguin$Temperature_t2, 
  paired = TRUE
)

model.paired <- lm(
  Temperature_t1 - Temperature_t2 ~ 1,
  data = df.penguin
)
summary(model.paired)

# 8.1.5 bruceR::TTEST

stats::t.test(
  data = df.penguin, 
  Temperature ~ romantic,
  var.equal = TRUE
) 

bruceR::TTEST(
  data = df.penguin, # 数据
  y = "Temperature", # 因变量
  x = "romantic"     # 自变量
) 

stats::t.test(
  x = df.penguin$Temperature, 
  mu = 36.6
)

bruceR::TTEST(
  data = df.penguin, # 数据
  y = "Temperature", # 确定变量
  test.value = 36.6, # 固定值
  test.sided = "=")  # 假设的方向

stats::t.test(
  x = df.penguin$Temperature_t1, #第1次
  y = df.penguin$Temperature_t2, #第2次
  paired = TRUE) 

bruceR::TTEST(
  data = df.penguin, # 数据
  y = c("Temperature_t1",  
        "Temperature_t2"), # 变量为两次核心体温
  paired = T)  # 配对数据，默认是FALSE

# 8.2  ANOVA 
# 8.2.2 代码实操|数据预处理

summary(df.penguin$DEQ)

# 设定分割点
# [0-23.5 热带， 23.5-35 亚热带]， [35-40 暖温带， 40-50 中温带]， [50-66.5 寒温带]
breaks <- c(0, 35, 50, 66.5)

# 设定相应的标签
labels <- c('热带', '温带', '寒温带')

# 创建新的变量
df.penguin$climate <- cut(df.penguin$DEQ, 
                          breaks = breaks, 
                          labels = labels)
summary(df.penguin$climate)

df <- df.penguin %>% 
  select(subjID, climate, romantic, Temperature) 

DT::datatable(head(df), fillContainer = TRUE)

# 8.2  ANOVA & linear regression
# 8.2.2 代码实操|正态性检验

# 正态性检验-Kolmogorov-Smirnov检验
# 若p >.05，不能拒绝数据符合正态分布的零假设
ks.test(df$Temperature, 'pnorm')

# 进行数据转换，转换后仍非正态分布
df$Temperature_log <- log(df$Temperature)
ks.test(df$Temperature_log, 'pnorm')

# 正态性检验-qq图
qqnorm(df$Temperature)
qqline(df$Temperature, col = "red") # 添加理论正态分布线

ggplot(df, aes(Temperature)) +
  geom_histogram(aes(y =..density..), color='black', fill='white', bins=30) +
  geom_density(alpha=.5, fill='red')

# 8.2.2 代码实操|双因素被试间方差分析
aov1 <- stats::aov(Temperature ~ climate * romantic, data = df)
summary(aov1)

# 结果不一致，原因PPT显示不全，请回到rmd文档查看
aov1 <- car::Anova(stats::aov(Temperature ~ climate * romantic, data = df))
aov1

# 原因debug
# 查看R的默认对比设置
options("contrasts")
# 从输出结果可知，无序默认为contr.treatment()，有序默认为contr.poly()
# factor()函数来创建无序因子，ordered()函数创建有序因子

is.factor(df$climate)
is.ordered(df$climate)
# climate是无序因子

# 创建一个3水平的因子的基准对比
c1 <- contr.treatment(3)

# 创建一个新的对比，这个编码假设分类水平之间的差异被等分，每一个水平与总均值的差异等于1/3
my.coding <- matrix(rep(1/3, 6), ncol=2)
# 将对比调整为每个水平与第一个水平的振幅减去1/3
# 可能的原因：除了关心每个水平对应的效果，同时也关心水平与水平之间的效果
my.simple <- c1-my.coding
my.simple

# 更改climate的对比
contrasts(df$climate) <- my.simple

# 将数据集df的romantic列的对比设为等距对比，它假设分类水平之间的差异为等距离
contrasts(df$romantic) <- contr.sum(2)/2

# 方差分析
aov1 <- car::Anova(lm(Temperature ~ climate * romantic, data = df),
                   type = 3)
aov1

afex::aov_ez(id = "subjID", 
             dv = "Temperature", 
             data = df, 
             between = c("climate", "romantic"), 
             type = 3)

# afex中的其他函数可以得到同样的结果
afex::aov_car(Temperature ~ climate * romantic + Error(subjID), data = df, type = 3)
afex::aov_4(Temperature ~ climate * romantic + (1|subjID), data = df)

# 8.2.3 线性回归

aov1 <- car::Anova(
  aov(Temperature ~ climate * romantic, 
      data = df), 
  type = 3
)
aov1

lm1 <- car::Anova(
  lm(Temperature ~ climate * romantic, 
     data = df), 
  type = 3
)
lm1

# 8.2.2 代码实操: bruceR::MANOVA

res1 <- bruceR::MANOVA(data = df, 
                       dv = "Temperature", 
                       between = c("climate", "romantic"))

res1 %>%
  capture.output()

# 8.2.2 代码实操: bruceR::EMMEANS

sim_eff <- res1 %>% 
  bruceR::EMMEANS("climate", by = "romantic") %>%
  bruceR::EMMEANS("romantic", by = "climate")

# 8.2.4 知识延申|单因素方差分析示例

# DEQ对Temperature的影响
res2 <- bruceR::MANOVA(
  data = df,
  dv = "Temperature",
  between = "climate") 

## install.packages
install.packages("easystats")
#加载easystats运行会需要到的辅助文件，确保运行不会出问题
easystats::install_suggested()
library(easystats)
###Data manipulation
##选择、过滤、筛选等操作
mtcars
data_match(mtcars, data.frame(vs = 0, am = 1))
data_filter(mtcars, vs == 0 & am == 1)
data_extract(mtcars, "gear")

###Statistical transformations数据转换
##标准化数据
summary(swiss)
summary(standardize(swiss))
##数据旋转或转置
data_rotate(swiss)

##各个值与均值的差，离均差
center(anscombe)
##描述性摘要
data(iris)
describe_distribution(iris)

results <- correlation(iris)
results
summary(results)
#可通过see包来进行可视化
library(tidyverse)
results %>%
  summary(redundant = TRUE) %>%
  plot()

#计算其它的相关
iris %>%
  correlation(partial = TRUE) %>%
  summary()

#Classical Regression Models
model <- lm(Sepal.Width ~ Petal.Length * Species + Petal.Width, data = iris)
model_parameters(model)

#Linear model
m1 <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(m1)
#Logistic regression
m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(m2)
#Linear mixed model
library(lme4)
m3 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
model_performance(m3)
#Models comparison
compare_performance(m1, m2, m3, rank = TRUE, verbose = FALSE)

## report
model <- glm(vs ~ mpg * drat, data = mtcars, family = "binomial")
report(model)

##Citation

citation("easystats")
citation("datawizard")

