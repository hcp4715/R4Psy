#Chapter 7

# 可以将清华的镜像设置为下载的镜像
# options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
# 导入所需R包
library(tidyverse)

# 找到所有要读取的文件名
# 使用 full.names 参数获取完整路径的文件列表
files <- list.files(here::here("data", "match"), pattern = "data_exp7_rep_match_.*\\.out$", full.names = TRUE)

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

a1 <- utils::read.table("data/match/data_exp7_rep_match_7302.out", header = TRUE)
DT::datatable(head(a1),
              fillContainer = TRUE, options = list(pageLength = 4))

# 7.1 反应时数据
# 选择我们需要的变量
df4 <- dplyr::select(df3,
                     Sub, Age, Sex, Hand, #人口统计学
                     Block, Bin, Trial,   # 试次
                     Shape, Label, Match, # 刺激
                     Resp, ACC, RT)       # 反应结果

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

# 计算SPE
df4 <- dplyr::mutate(df4, moral_SPE = Self - Other)
df4 <- dplyr::select(df4, Sub, moral_SPE) 

DT::datatable(head(df4, 24),
              fillContainer = TRUE, options = list(pageLength = 5))

# 用管道操作符合并以上代码
df4 <- df3 %>%
  dplyr::select(Sub, Age, Sex, Hand, #人口统计学
                Block, Bin, Trial,   # 试次
                Shape, Label, Match, # 刺激
                Resp, ACC, RT) %>%  # 反应结果
  tidyr::drop_na() %>%               #删除缺失值
  dplyr::filter(Hand == "R",         # 选择右利手被试
                ACC == 0 | ACC == 1, # 排除无效应答（ACC = -1 OR 2)
                RT >= 0.2 & RT <= 1.5) %>%  # 选择RT属于[200,1500]
  dplyr::group_by(Sub,Shape, Label, Match) %>%
  dplyr::summarise(mean_ACC = mean(ACC), mean_RT = mean(RT)) %>%
  dplyr::ungroup() %>%
  tidyr::extract(Shape, into = c("Valence", "Identity"),
                 regex = "(moral|immoral)(Self|Other)", remove = FALSE) %>%
  dplyr::filter(Match == "match" & Valence == "moral") %>%
  dplyr::select(Sub, Identity, mean_RT) %>%
  tidyr::pivot_wider(names_from="Identity", values_from="mean_RT") %>%
  dplyr::mutate(moral_SPE=Self - Other)

# 7.2.2 函数调用——为了避免调用错误，推荐使用：R包::函数名称

df.pg.raw <- utils::read.csv(file = './data/penguin/penguin_rawdata.csv',
                             header = T, 
                             sep=",",
                             stringsAsFactors = FALSE) 

# 7.2.3 自定义函数

# 定义一个函数：输入x和y，返回3倍x和5倍y的和
mysum <- function(x,y){
  result = 3*x+5*y
  return(result)
}
# mysum: 自定义的函数名
# x,y:  形式参数
# result = 3*x+5*y: 函数体
# return(result): 返回值

#调用函数,x=1,y=2
mysum(1, 2)
mysum(y=1, x=2)

7.2.3 自定义函数（小练习）

# 自定义函数：计算 (a+b)/c
# myabc <- ***{
# result = ****
# return(result)}

# 计算abc分别为1,2,3时的值
#myabc(1, 2, 3)  # 结果：1

#用合理的代码替换以上“***”,删除每行前的“#”,即可运行


mysum3 <- function(x = 6,y = 7){
  if(is.numeric(x) & is.numeric(y)){
    result = 3*x+5*y
    return(result)}
  else{print("x and y must be number")}
}
# print：输出指定的内容
# is.numeric:判断是否为数值型。是则返回T，否则返回F
# & : 表示“且”
mysum3(5,6)
mysum3('a','b')

#练习示例
myabc <- function(a = 3, b = 2, c = 1){
  if(c!=0){
    result = (a+b)/c
    return(result)
  }
  else{print('c should not be 0')}
}

#函数在数据处理中的运用
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

#以下是计算击中(hit)、虚报(fa)、漏报(miss)和正确否定(cr)的代码
# 去掉下面#的部分，将***替换成合适的变量，补全代码
dplyr::summarise(
  # *** = length(ACC[Match == "match" & ACC == 1]),
  # *** = length(ACC[Match == "mismatch" & ACC == 0]),
  # *** = length(ACC[Match == "match" & ACC == 0]),
  # *** = length(ACC[Match == "mismatch" & ACC == 1]),
  
#可以使用正态分布的累积分布函数（qnorm）来计算 $d′$ 值，以下为代码
  
  Dprime = qnorm(
    ifelse(hit / (hit + miss) < 1,
           hit / (hit + miss),        # 击中率
           1 - 1 / (2 * (hit + miss)) # 避免击中率为1时的极端情况
    )
  ) 
  - qnorm(
    ifelse(fa / (fa + cr) > 0,
           fa / (fa + cr),     # 虚报率
           1 / (2 * (fa + cr)) # 避免虚报率为0时的极端情况
    )
  )
) 