library(here)
library(tidyverse)
# ---------- 1.3 重复测量方差分析 ----------
## 载入数据 并进行变量预处理
mt_raw <- bruceR::import(here::here('data','match','match_raw.csv')) %>% 
  tidyr::extract(Shape, 
                 into = c("Valence", "Identity"),
                 regex = "(moral|immoral)(Self|Other)",
                 remove = FALSE)

# 分被试和实验条件计算 Match条件下正确反应试次的RT均值
mt_mean <- mt_raw %>%
  dplyr::filter(!is.na(RT) & Match == "match" & ACC == 1) %>%
  dplyr::group_by(Sub,Identity,Valence) %>%
  dplyr::summarise(RT = mean(RT)) %>%
  dplyr::ungroup()

##  使用bruceR中的MANOVA进行重复测量方差分析：
bruceR::MANOVA(data = mt_mean,
               subID = 'Sub', # 被试编号
               dv= 'RT',      # dependent variable
               within = c('Identity', 'Valence'))

## 使用afex进行重复测量方差分析，比较两者的区别，可以看到MANOVA是对afex的封装
m_aov <- afex::aov_ez(
  data = mt_mean,
  id = 'Sub',
  dv = 'RT',
  within = c('Identity', 'Valence'))
# 打印afex的结果
m_aov

# ---------- 2 多层线性模型(HLM)简介 ----------
## 这里将以一个经典的示例数据为例展示不同效应随着分组变化的情况。
## 来源：https://github.com/mkfreeman/hierarchical-models/blob/master/generate-data.R
## 创建虚拟数据
set.seed(999)
departments <- c('sociology', 'biology', 'english', 'informatics', 'statistics')
base.salaries <- c(40000, 50000, 60000, 70000, 80000)
annual.raises <- c(2000, 500, 500, 1700, 500)
faculty.per.dept <- 20
total.faculty <- faculty.per.dept * length(departments)

# Generate dataframe of faculty and (random) years of experience
ids <- 1:total.faculty
department <- rep(departments, faculty.per.dept)
experience <- floor(runif(total.faculty, 0, 10))
bases <- rep(base.salaries, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
raises <- rep(annual.raises, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
df <- data.frame(ids, department, bases, experience, raises)

# Generate salaries (base + experience * raise)
df <- df %>% mutate(
  salary = bases + experience * raises
)

## Model without respect to grouping
m0 <- stats::lm(salary ~ experience, data=df)

df$simple.model <- predict(m0)

## Model with varying intercept
m1 <- lme4::lmer(salary ~ experience + (1|department), data = df)

df$random.intercpet.preds <- predict(m1)

# 可能会碰到错误
# remove.packages("Matrix")
# remove.packages("lme4")
# install.packages("lme4", type = "source")

## Model with varying slope
m2 <- lme4::lmer(salary ~ experience + (0 + experience|department), data=df)

df$random.slope.preds <- predict(m2)

## Model with varying slope and intercept
m3 <- lme4::lmer(salary ~ experience + (1 + experience|department), data=df)

df$random.slope.int.preds <- predict(m3)

##  对上述四种模型的可视化
## 没有随分组变化的效应,  以department为分组进行可视化
df %>% 
  ggplot(aes(x = experience,y = salary)) + 
  geom_point(aes(x = experience,
                 y = salary,
                 color = department),
             size = 5, alpha = 0.5) + 
  geom_smooth(method = 'lm',color = 'black',se=F) + 
  labs(x = 'Experience',y = 'Salary',legends = 'Department') + 
  scale_colour_discrete('Department') +
  ggtitle("Fixed Slope and Intercept") +
  bruceR::theme_bruce()

### 截距随分组变化的模型
df %>% 
  ggplot() +
  geom_point(data = df,
             aes(x = experience,y = salary,
                 color = department),
             size = 5,alpha = 0.4) + 
  geom_line(aes(x=experience, y=random.intercpet.preds,
                group = department, 
                colour = department)) +
  labs(x="Experience", y="Salary") +
  ggtitle("Varying Intercept") + 
  scale_colour_discrete('Department') +
  bruceR::theme_bruce()

### 斜率随分组变化的模型
df %>%
  ggplot() +
  geom_point(
    data = df,
    aes(x = experience, y = salary, color = department),
    size = 5,
    alpha = 0.4) +
  geom_line(aes(
    x = experience,
    y = random.slope.preds,
    group = department,
    colour = department)) +
  labs(x = "Experience", y = "Salary") +
  ggtitle("Varying Slope") +
  scale_colour_discrete('Department') +
  bruceR::theme_bruce()

### 截距和斜率随分组变化的模型
df %>%
  ggplot(aes(x = experience, y = salary, color = department)) +
  geom_point(size = 5, alpha = 0.4) +
  geom_smooth(method = 'lm', se = F, size = 0.5) +
  labs(x = "Experience", y = "Salary") +
  ggtitle("Varying Slope and Intercept") +
  bruceR::theme_bruce() 

# ---------- 3 多层线性模型的应用 ----------
# match数据集中对个体数据的可视化，看到截距/斜率的个体差异
mt_sample_df <- mt_raw %>%
  dplyr::filter(Match == 'match' & ACC == 1) %>%
  dplyr::filter(Sub %in% c(7311, 7313, 7307, 7324)) %>%
  dplyr::mutate(
    Sub = factor(Sub),
    Identity = factor(Identity, levels = c("Self", "Other")),
    Valence = factor(Valence, levels = c("moral", "immoral"))
  )

mt_sample_df %>%
  dplyr::filter(Identity == 'Self') %>%
  ggplot(aes(x = Valence, color = Valence, y = RT))  +
  geom_point(position = position_jitter(0.2), alpha = 1) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 18,
    size = 4,
    color = "darkred"
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "line",
    aes(group = 1),
    color = "darkred"
  ) +
  facet_wrap( ~ Sub, nrow = 1) +
  scale_color_brewer(palette = 'Accent') +
  theme_bw(base_size = 13)

## 变化截距 固定斜率
model <- lme4::lmer(data = mt_raw, RT ~ Identity * Valence + (1 | Sub))

## 随机截距 随机斜率
model_full <- lme4::lmer(data = mt_raw, 
                         RT ~ Identity * Valence + (1 + Identity * Valence |Sub)) 

## 模型比较
stats::anova(model, model_full) %>% capture.output()

# 建立没有固定效应的“空模型”
model_null <- lme4::lmer(data = mt_raw,
                         RT ~ (1 + Identity*Valence|Sub))

## 根据似然比进行模型比较
stats::anova(model_full, model_null)

# 也可以使用afex包完成多层模型
afex::mixed(data = mt_raw,
            RT ~ Identity * Valence + (1 + Identity*Valence|Sub),
            method = 'LRT')

## 使用lmerTest包获得有统计显著性检验的结果
lmer_model <- lmerTest::lmer(data = mt_raw,
                             RT ~ Identity * Valence + (1 + Identity * Valence|Sub))

# 如果使用lmerTest包进行建模，可以使用bruceR::HLM_summary()进行输出
##  RUN IN CONSOLE 
install.packages("MuMIn")
library(MuMIn)
bruceR::HLM_summary(lmer_model)

# 交互效应的可视化
interactions::cat_plot(model = model_full,
                       pred = Identity,
                       modx = Valence)
# 比较不同分析方式的结果
m_aov

model_full %>% anova()

lmer_model %>% anova()

# http://www.dwoll.de/rexrepos/posts/anovaMixed.html#two-way-repeated-measures-anova-rbf-pq-design
model_aov <- mt_mean %>%
  lmerTest::lmer(
    data = .,
    RT ~ Identity * Valence + (1|Sub) + (1|Identity:Sub) + (1|Valence:Sub)
  )

model_aov %>% anova()
