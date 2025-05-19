#Chapter 8

# Packages
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}

pacman::p_load(
  # 本节课需要用到的 packages
  here,skimr,quartets,GGally,showtext,bruceR,tidyverse,DataExplorer,
  # 生成课件
  xaringan,xaringanthemer,xaringanExtra)

# 8.0.1  回顾: 批量导入数据

xaringanExtra::use_panelset()

# 所有数据路径
files <- list.files(  #<<
  ## <- & =
  here::here("data", "match"), 
  pattern = "data_exp7_rep_match_.*\\.out$", 
  full.names = TRUE)

#数据类型转换
convert_data_types <- function(df) {
  df <- df %>% 
    dplyr::mutate(Date = as.character(Date),Prac = as.character(Prac),
                  Sub = as.numeric(Sub),Age = as.numeric(Age),
                  Sex = as.character(Sex),Hand = as.character(Hand),
                  Block = as.numeric(Block),Bin = as.numeric(Bin),
                  Trial = as.numeric(Trial),Shape = as.character(Shape),
                  Label = as.character(Label),Match = as.character(Match),
                  CorrResp = as.character(CorrResp),Resp = as.character(Resp),
                  ACC = as.numeric(ACC),RT = as.numeric(RT))
  return(df)
}

#批量合并
df3 <- data.frame() 

for (i in seq_along(files)) {
  # 读取
  df <- read.table(files[i], header = TRUE) %>%  
    dplyr::filter(Date != "Date") %>%  
    convert_data_types() 
  # 合并
  df3 <- dplyr::bind_rows(df3, df) 
}
# 删除临时变量
rm(df, files, i)

#保存数据

# 上节课介绍了write.csv,也可使用bruceR::export
bruceR::export(
  df3, 
  file = here::here("data", "match","match_raw.csv"))

## 当然，export 不仅可以保存数据，也可以输出模型结果

#修改列名

# 修改第一列列名 Date 为小写 date
df3 %>% dplyr::rename(  ## new_name = old_name
  date = Date
) %>% colnames()  

# 将全部列名都变成小写
df3 %>% dplyr::rename_with(
  ## 将字符向量全部变成小写； ~ 声明这是一个函数，.代表前面的数据(df3)传到.所在的位置
  ~tolower(.)   #<<
  ## 即使用 tolower()对所有列名进行批量处理
  ##
) %>% colnames()

# 8.0.2  代码书写规范

## 是不是更整洁一些
iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarize_if(is.numeric, mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(measure, value, -Species) %>%
  dplyr::arrange(value)
## 有没有自动挡…

# 8.0.2  代码书写规范


```{r eval=FALSE}
## 使用 Ctrl + Shift + A  (cmd+shift+A in Mac) 
## 或者点击 Code --> Reformat Code (快捷键冲突)
## 自动增加空格与缩进
## 选中下面代码看看效果，尽管效果有限
## 还是平时养成书写习惯更好

files <- list.files(
  here::here("data","match"),
  pattern = "data_exp7_rep_match_.*\\.out$",
  full.names = TRUE
)

# 8.0.4  大作业

knitr::include_graphics("picture/chp7/opportunity_rep.jpg")

quartets::datasaurus_dozen %>%  ## 包中的数据
  head(10)

quartets::datasaurus_dozen %>%  ## 包中的数据
  str()

summary(datasaurus_dozen)
# PLOT GIF
pacman::p_load(camcorder, tidyverse, bruceR)
data = quartets::datasaurus_dozen
colors = c('#FF5733','#00CED1','#FFD700',
           '#8A2BE2','#32CD32','#4682B4',
           '#FF4500','#9400D3','#00FF00',
           '#FF1493','#8B0000','#FF69B4','#20B2AA')

orders <- data$dataset %>% unique()
orders <- c(orders[-1],orders[1])

pic <- list()
for(i in 1:13) {
  
  pic[[i]] <- data %>% filter(dataset == orders[i]) %>%
    ggplot(aes(x, y)) +
    geom_point(color = colors[i]) +
    geom_smooth(method = "lm", color = 'black')  +
    labs(title = paste0('dataset:  ', orders[i])) +
    bruceR::theme_bruce() +
    theme(title = element_text(size = 20)) 
}

gg_record(
  dir <- file.path(tempdir(), "recording"), 
  device = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 500
)

for(i in 1:13){
  pic[[i]] %>% print()
}

gg_playback(name = file.path(paste0(getwd(),'/picture/chp7/'), "dino.gif"),
            first_image_duration = 1,
            last_image_duration = 3,
            frame_duration = 1,loop = F,
            playback = T,last_as_first = F,
)
gg_stop_recording()

## NOT RUN
quartets::datasaurus_dozen %>% 
  ggplot(aes(x, y, color = dataset)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ dataset)

# 8.1  探索性数据分析

knitr::include_graphics("picture/chp7/Data_visualization_process_v1.png")

# 读取数据
pg_raw <- bruceR::import(here::here(
  "data", "penguin","penguin_rawdata.csv"))

mt_raw <- bruceR::import(here::here(
  "data", "match","match_raw.csv")) 

DT::datatable(head(mt_raw, 3))  # 注：datatable 这里只为了在网页中输出

mt_raw %>% 
  str() 

summary(mt_raw) %>% 
  knitr::kable() # 注：kable函数只为了输出

skimr::skim(mt_raw) %>% 
  capture.output() %>% 
  .[1:12]

skimr::skim(mt_raw) %>% 
  capture.output() %>% 
  .[13:24]

skimr::skim(mt_raw) %>% 
  capture.output() %>% 
  .[25:41]

bruceR::Describe(mt_raw) %>% 
  capture.output() %>% 
  .[2:17] ## 可以使用 file参数输出 Word

# 8.2.1 可视化的重要性

library(showtext)
showtext::showtext_auto(enable = T)
quartets::datasaurus_dozen %>% 
  filter(dataset == 'dino') %>% 
  ggplot(aes(x,y)) + geom_point(color ='#20B2AA') + 
  theme_bruce() + 
  labs(title = '请记住这只 DINO !!!') + 
  theme(plot.title = element_text(size = 20)) 

# 8.2.2 为何使用 ggplot2?

# 以penguin问卷中前后体温为例

p1 <- pg_raw %>%
  ggplot(aes(x = Temperature_t1, # 确定映射到xy轴的变量
             y = Temperature_t2)) 
p1 ## 坐标轴名称已对应，虽然图片为空

p1 + geom_point() #添加图层-散点
p1 + geom_point() + geom_smooth(method = 'lm') #添加图层-拟合曲线

pg_raw %>% 
  drop_na(Temperature_t1,Temperature_t2,sex) %>% 
  ggplot(aes(x = Temperature_t1, 
             y = Temperature_t2,
             color = factor(sex))) +
  geom_point() + 
  geom_smooth()  #改变映射

# 直方图

pic_his <- mt_raw %>% 
  # 确定映射到x轴的变量
  ggplot(aes(x = RT)) + 
  geom_histogram(bins = 40) +
  theme_classic()

pic_his 

# 密度图

pic_dens <- mt_raw %>% 
  ggplot() +
  # 绘制密度曲线 
  geom_density(aes(x = RT)) +
  theme_classic()

pic_dens

# 直方图 + 密度图

## 尝试将两个图层叠加在一起
mt_raw %>% 
  ggplot(aes(x = RT))+
  geom_histogram(bins = 40) + 
  geom_density() +
  theme_classic()

mt_raw %>% 
  ggplot(aes(x = RT)) +
  geom_histogram(bins = 40) + 
  theme_classic()

# 直方图 + 密度图
pic_mix <- mt_raw %>% 
  ggplot(aes(x = RT,
             ## 直方图的统计结果通过after_stat(density)传递给了密度图
             y = after_stat(density))) +  
  geom_histogram() +
  geom_density() +
  # 设定绘图风格
  theme_classic()
pic_mix

# 箱线图

pic_box <- mt_raw %>% 
  ggplot(aes(x = Label, y = RT)) +
  geom_boxplot(staplewidth = 1) +
  # 绘制箱线图并添加上下边缘线 
  theme_classic()
pic_box

# 8.4 Explore data with DataExplorer

DataExplorer::plot_str(mt_raw)

DataExplorer::plot_intro(mt_raw)

DataExplorer::plot_missing(mt_raw)

DataExplorer::plot_bar(mt_raw)

DataExplorer::plot_bar(mt_raw, by="Match")

DataExplorer::plot_histogram(mt_raw)

DataExplorer::plot_qq(pg_raw[,2:10])

DataExplorer::plot_correlation(na.omit(pg_raw[, 2:30]))

# 使用ggpairs

## 以 penguin project 数据中 ALEX, stress和 ECR 为例
pg_raw %>% 
  mutate(
    # 计算均值
    m_ALEX = bruceR::MEAN(.,
                          var = 'ALEX',items = 1:16,rev = c(4,12,14,16)),
    m_stress = bruceR::MEAN(.,
                            var = 'stress',items = 1:14,rev = c(4:7,9,10,13)
    ),
    m_ECR = bruceR::MEAN(.,
                         var = 'ECR',items = 1:36
    )
  ) %>% 
  select(contains('m_')) %>% 
  GGally::ggpairs()

