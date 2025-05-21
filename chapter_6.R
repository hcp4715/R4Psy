#Chapter 6

#6.1数据预处理

# 可以将清华的镜像设置为下载的镜像
# options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
#下载、导入所需R包
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(bruceR,tidyverse)

df1 <- bruceR::import(here::here('data', 'penguin', 'penguin_rawdata.csv')) # 导入数据

penguin_data <- read.csv('./data/penguin/penguin_rawdata.csv')  # 仅用基础包的做法

DT::datatable(head(df1),
              fillContainer = TRUE, options = list(pageLength = 4))

#了解数据
#查看变量名（列名）
colnames(df1) 

#数据预处理的R base操作

# 反向计算: 1 --> 5; 2 --> 4; 3 --> 3; 4 --> 2; 5 --> 1
df1$ALEX4_new <- 6 - df1$ALEX4
df1$ALEX12_new <- 6 - df1$ALEX12
df1$ALEX14_new <- 6 - df1$ALEX14
df1$ALEX16_new <- 6 - df1$ALEX16

# 计算反向计算分ALEX得分:
df1$ALEX <- df1$ALEX1 + df1$ALEX2 + df1$ALEX3 + df1$ALEX4_new + df1$ALEX5 + 
  df1$ALEX6 + df1$ALEX7 + df1$ALEX8 + df1$ALEX9 + df1$ALEX10 + df1$ALEX11 + 
  df1$ALEX12_new + df1$ALEX13 + df1$ALEX14_new + df1$ALEX15 + df1$ALEX16_new

# 用rowSum 计算反向计算分ALEX得分:
df1$ALEX <- rowSums(df1[, c("ALEX1", "ALEX2", "ALEX3", "ALEX4_new", "ALEX5", 
                            "ALEX6", "ALEX7", "ALEX8", "ALEX9", "ALEX10", 
                            "ALEX11", "ALEX12_new", "ALEX13", "ALEX14_new", 
                            "ALEX15", "ALEX16_new")])

# 创建一个包含需要反向计分的变量的列表
vars_to_reverse <- c("ALEX4", "ALEX12", "ALEX14", "ALEX16")

# 对列表中的变量进行反向计分
df1$ALEX <- bruceR::SUM(df1, 
                        varrange = "ALEX1:ALEX16", 
                        rev = vars_to_reverse, 
                        likert = 1:5)

#假设需找到`data`中`age`大于`30`的所有行，并按照年龄排序，代码如下：
filtered_data <- dplyr::filter(df1, age > 30)
filtered_sorted_data <- dplyr::arrange(filtered_data, age)

#使用管道操作符后，代码变为：
filtered_sorted_data <- df1 %>%
  dplyr::filter(age > 30) %>%
  dplyr::arrange(age)

# 创建dataframe
data <- data.frame(
  "grammer" = c("R","SPSS","Python","R",NA,"Matlab","Python","R"),
  "score" = c(4,2,5,4.5,5,4,2,5),
  "popularity" = c(1,2,NA,4,5,6,7,10)
)
# 提取前两列
select_data1 <- data[ ,1:2]
# 提取含字符串"R"的行
select_data2 <- select_data1[select_data1$grammer == 'R', ]

# 6.1 Tidyverse的反向分操作

# 将4, 12, 14, 16题反向计分，计算ALEX，保存为ALEX
df2 <- df1 %>%
  dplyr::mutate(
    # 使用 casewhen反向计分：6减去原始值
    ALEX4 = case_when(TRUE ~ 6 - ALEX4),
    ALEX12 = case_when(TRUE ~ 6 - ALEX12),
    ALEX14 = case_when(TRUE ~ 6 - ALEX14),
    ALEX16 = case_when(TRUE ~ 6 - ALEX16)
  )
#也可以写成 case_when(ALEX4 == '1' ~ '5',ALEX4 == '2' ~ '4', ALEX4 == '3' ~ '3', ALEX4 == '4' ~ '2', ALEX4 == '5' ~ '1',TRUE ~ as.character(ALEX4))

df2 <- df1 %>%
  dplyr::mutate(ALEX4 = case_when(TRUE ~ 6 - ALEX4),
                ALEX12 = case_when(TRUE ~ 6 - ALEX12),
                ALEX14 = case_when(TRUE ~ 6 - ALEX14),
                ALEX16 = case_when(TRUE ~ 6 - ALEX16)
  )
#也可以写成 case_when(ALEX4 == '1' ~ '5',ALEX4 == '2' ~ '4', ALEX4 == '3' ~ '3', ALEX4 == '4' ~ '2', ALEX4 == '5' ~ '1',TRUE ~ as.character(ALEX4))

# age为num
case_when(
  age < 18 ~ "Child",
  age >= 18 & age < 65 ~ "Adult",
  age >= 65 ~ "Senior"
)

# age为chr
case_when(
  age < "18" ~ "Child",
  age >= "18" & age < "65" ~ "Adult",
  age >= "65" ~ "Senior"
)

# 6.2 基于Tidyverse处理问卷数据

# 加载包后函数前不需要注明包，此处只是为了提示函数属于哪个包
# 选择我们需要的变量：Temperature_t1, Temperature_t2, SNI28-32, DEQ, romantic, ALEX1-16
df2 <- df1 %>%
  dplyr::select(Temperature_t1, Temperature_t2, 
                socialdiversity, Site, DEQ, 
                romantic,ALEX1:ALEX16)

# 检查变量的数据类型
base::summary(df2)

# 转换数据类型
# 这里数据类型是正确的，只是示例
df2 <- df2 %>%
  dplyr::mutate(Temperature_t1_new = as.numeric(Temperature_t1),
                Temperature_t2 = as.numeric(Temperature_t2))

# 按照Temperature, DEQ处理缺失值
df2 <- df2 %>%
  dplyr::filter(!is.na(Temperature_t1) & !is.na(Temperature_t2) & !is.na(DEQ))

# 计算每个被试两次核心温度的均值，保存为Temperature
df2 <- df2 %>%
  dplyr::mutate(Temperature = rowMeans(select(df2, starts_with("Temperature"))))

# 计算ALEX
df2 <- df2 %>%
  dplyr::mutate(ALEX = rowSums(select(df2, starts_with("ALEX"))))

# 查看数据
df2

# 按Site计算Temperature的平均值
df2 <- dplyr::group_by(df2, Site)
df3 <- dplyr::summarise(df2, mean_Temperature = mean(Temperature), n = n())
df2 <- dplyr::ungroup(df2)

# 用管道操作符合并以上代码
# 使用管道操作符时建议先单独查看变量的数据类型，转换完毕后在进行操作
# dplyr::glimpse(penguin_data)

df3 <- df1 %>%
  dplyr::select(Temperature_t1, Temperature_t2, socialdiversity, Site, 
                DEQ, romantic, ALEX1:ALEX16) %>%
  dplyr::filter(!is.na(Temperature_t1) & !is.na(Temperature_t2) & !is.na(DEQ)) %>%
  dplyr::mutate(Temperature = rowMeans(dplyr::select(., dplyr::starts_with("Temperature"))),
                ALEX4 = dplyr::case_when(TRUE ~ 6 - ALEX4),
                ALEX12 = dplyr::case_when(TRUE ~ 6 - ALEX12),
                ALEX14 = dplyr::case_when(TRUE ~ 6 - ALEX14),
                ALEX16 = dplyr::case_when(TRUE ~ 6 - ALEX16),
                ALEX = rowSums(dplyr::select(., dplyr::starts_with("ALEX")))) %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(mean_Temperature = mean(Temperature),
                   mean_ALEX = mean(ALEX)) %>%
  dplyr::ungroup()

# 查看数据
df3

#6.3 反应时数据

a1 <- utils::read.table("data/match/data_exp7_rep_match_7302.out", header = TRUE)
DT::datatable(head(a1),
              fillContainer = TRUE, options = list(pageLength = 4))

# 查看单个被试的数据
# 查看数据时要注意所需变量的数据类型，如果存在问题需要提前转换
p1 <- utils::read.table("data/match/data_exp7_rep_match_7302.out", header = TRUE)
p2 <- utils::read.table("data/match/data_exp7_rep_match_7303.out", header = TRUE)
p1

#合并两个被试数据
tmp <- base::rbind(p1, p2)
tmp <- dplyr::bind_rows(p1, p2)

# 单个操作循环，打印i + 1
for (i in 1:10) {
  print(i + 1)
}

# variable in sequence

for (i in 1:5) {print(i)}

for (i in seq(1, 5)) {print(i) }

my_vector <- c(1, 2, 3, 4, 5)
for (i in my_vector) {print(i) }

my_list <- list(a = 1, b = 2, c = 3) 
for (element in my_list) {print(element) }

my_string <- "world"
for (i in 1:nchar(my_string)) {print(substr(my_string, i, i)) }

# variable in sequence

my_list <- list(a = 1, b = 2, c = 3)
for (element in my_list) {print(element)}

my_string <- "world"
for (i in 1:nchar(my_string)) {print(substr(my_string, i, i))}

# 加简单条件
for (i in 1:10) {
  if (i > 5) {
    print(i + 1)
  }
}

# 找到所有要读取的文件名
# 使用 full.names 参数获取完整路径的文件列表
files <- list.files(here::here("data", "match"), pattern = "data_exp7_rep_match_.*\\.out$", full.names = TRUE)
head(files, n = 10L)
str(files)

# 定义函数用于数据类型转换，可以暂时不管
convert_data_types = function(df) {
  df <- df %>%
    dplyr::mutate(Date = as.character(Date),
                  Prac = as.character(Prac),
                  Sub = as.numeric(Sub),
                  Age = as.numeric(Age),
                  Sex = as.character(Sex),
                  Hand = as.character(Hand),
                  Block = as.numeric(Block),
                  Bin = as.numeric(Bin),
                  Trial = as.numeric(Trial),
                  Shape = as.character(Shape),
                  Label = as.character(Label),
                  Match = as.character(Match),
                  CorrResp = as.character(CorrResp),
                  Resp = as.character(Resp),
                  ACC = as.numeric(ACC),
                  RT = as.numeric(RT))
  return(df)
}

# 创建一个空的数据框来存储读取的数据
df3 <- data.frame()

# 循环读取每个文件，处理数据并添加到数据框中
for (i in seq_along(files)) { # 重复"读取到的.out个数"的次数
  # 读取数据文件
  df <- read.table(files[i], header = TRUE) 
  # 使用 filter 函数过滤掉 Date 列值为 "Date" 的行
  df <- dplyr::filter(df, Date != "Date") 
  # 调用函数进行数据类型转换
  df <- convert_data_types(df)
  # 使用 bind_rows() 函数将当前数据框与之前的数据框合并
  df3 <- dplyr::bind_rows(df3, df)
}

# 清除中间变量
rm(df, files, i)

# 获取所有的.out文件名
files <- list.files(here::here("data", "match"), pattern = "data_exp7_rep_match_.*\\.out$", full.names = TRUE)

# 读取每个.out文件，并进行数据清洗
df3 <- lapply(files, function(file) {
  df <- read.table(file, header = TRUE)
  df <- dplyr::filter(df, Date != "Date") # 过滤掉 Date 列值为 "Date" 的行
  df <- mutate(df, 
               convert_data_types(df)
  ) # 进行数据类型转换和数据清洗
  return(df)
})

# 合并所有数据框
df3 <- dplyr::bind_rows(df3)

# 清除中间变量
rm(files)

# 选择我们需要的变量
df4 <- dplyr::select(df3,
                     Sub, Age, Sex, Hand, #人口统计学
                     Block, Bin, Trial, # 试次
                     Shape, Label, Match, # 刺激
                     Resp, ACC, RT) # 反应结果

# 删除缺失值，选择符合标准的被试
df4 <- tidyr::drop_na(df4) # 删除含有缺失值的行
df4 <- dplyr::filter(df4, Hand == "R",      # 选择右利手被试
                     ACC == 0 | ACC == 1 ,   # 排除无效应答（ACC = -1 OR 2)
                     RT >= 0.2 & RT <= 1.5)  # 选择RT属于[200,1500]

DT::datatable(head(df4, 24),
              fillContainer = TRUE, options = list(pageLength = 5))

# 分实验条件计算
df4 <- dplyr::group_by(df4, Sub, Shape, Label, Match)
df4 <- dplyr::summarise(df4, mean_ACC = mean(ACC), mean_RT = mean(RT))
df4 <- dplyr::ungroup(df4)

DT::datatable(head(df4, 24),
              fillContainer = TRUE, options = list(pageLength = 5))

# 将Shape变量拆分
df4 <- tidyr::extract(df4, Shape, into = c("Valence", "Identity"),
                      regex = "(moral|immoral)(Self|Other)", remove = FALSE)
df4 <- dplyr::filter(df4, Match == "match" & Valence == "moral") 

DT::datatable(head(df4, 24),
              fillContainer = TRUE, options = list(pageLength = 5))

# 将长数据转为宽数据
df4 <- dplyr::select(df4, Sub, Identity, mean_RT)
df4 <- tidyr::pivot_wider(df4, names_from = "Identity", values_from = "mean_RT")

DT::datatable(head(df4, 24),
              fillContainer = TRUE, options = list(pageLength = 5))

# 用管道操作符合并以上代码
df4 <- df3 %>%
  dplyr::select(Sub, Age, Sex, Hand, #人口统计学
                Block, Bin, Trial,   # 试次
                Shape, Label, Match, # 刺激
                Resp, ACC, RT,       # 反应结果
  ) %>% 
  tidyr::drop_na() %>%                  #删除缺失值
  dplyr::filter(Hand == "R",          # 选择右利手被试
                ACC == 0 | ACC == 1 , # 排除无效应答（ACC = -1 OR 2)
                RT >= 0.2 & RT <= 1.5 # 选择RT属于[200,1500]
  ) %>%
  dplyr::group_by(Sub, 
                  Shape, Label, Match) %>%
  dplyr::summarise(mean_ACC = mean(ACC),
                   mean_RT = mean(RT)) %>%
  dplyr::ungroup() %>%
  tidyr::extract(Shape, into = c("Valence", "Identity"),
                 regex = "(moral|immoral)(Self|Other)", remove = FALSE) %>%
  dplyr::filter(Match == "match" & Valence == "moral") %>%
  dplyr::select(Sub, Identity, mean_RT) %>%
  tidyr::pivot_wider(names_from = "Identity", values_from = "mean_RT") %>%
  dplyr::mutate(moral_SPE = Self - Other) %>%
  dplyr::select(Sub, moral_SPE) 

# 去掉下面#的部分，将***替换成合适的变量，补全代码
dplyr::summarise(
  # *** = length(ACC[Match == "match" & ACC == 1]),
  # *** = length(ACC[Match == "mismatch" & ACC == 0]),
  # *** = length(ACC[Match == "match" & ACC == 0]),
  # *** = length(ACC[Match == "mismatch" & ACC == 1]),
  Dprime = qnorm(
    ifelse(hit / (hit + miss) < 1,
           hit / (hit + miss),
           1 - 1 / (2 * (hit + miss))
    )
  ) 
  - qnorm(
    ifelse(fa / (fa + cr) > 0,
           fa / (fa + cr),
           1 / (2 * (fa + cr))
    )
  )
) 