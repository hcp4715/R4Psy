#Chapter 5

# 可以将清华的镜像设置为下载的镜像
# options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
#下载、导入所需R包
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(bruceR,tidyverse)

# 5.1读取数据
# 当前工作目录为R4Psy,依次打开'data''penguin'文件夹获取'penguin_rawdata.csv'数据
penguin_data <- bruceR::import(here::here('data', 'penguin', 'penguin_rawdata.csv'))

# 仅用基础包的做法：
# penguin_data <- read.csv('./data/penguin/penguin_rawdata.csv')

x <- c('good','better','best','bad','worse','worst')
sort(x) # 排序

x1 <- factor(x, levels = c('best','better','good','bad','worse','worst'))
sort(x1) # 排序

penguin_data[1:2,1:3]

# 也可以使用减号
penguin_data[1:2,-c(4:ncol(penguin_data))]

# 取最后三列
penguin_data[1:2, (ncol(penguin_data)-2):ncol(penguin_data)]

# 同样选取前 2 行以及前 4 列数据
penguin_data[1:2, c('age','ALEX1','ALEX2')]

#完全匹配
'c' %in% c('abc','cd','c')
#部分匹配
base::grepl('c',c('abc','cd','c'))
#部分匹配之开头匹配
base::startsWith(c('abc','cd','c'),'c')
#部分匹配之结尾匹配
base::endsWith(c('abc','cd','c'),'c')

# stress_data <- penguin_data[***,***]
selected_cols <- c('Site', colnames(penguin_data)[base::startsWith(colnames(penguin_data),'stress')])

stress_data <- penguin_data[ , selected_cols]

# alternative, 注意，grep返回的是列的序号
stress_data2 <- penguin_data[ , c(1, grep("^stress", names(penguin_data)))]

#将“***”部分替换为适合的代码，并删去行首的“#”,即可运行

#使用字符的匹配将"Site"列中取值为"Tsinghua"的列选择出来
stress_data_thu <- stress_data[stress_data$Site == 'Tsinghua', ]

#计算每个被试"stress"的总分
stress_data_thu$stress_sum <- rowSums(stress_data_thu[,grep("^stress", names(stress_data_thu))])

#计算所有被试的"stress"总分的均值与标准差
mean(stress_data_thu$stress_sum, na.rm = TRUE)
sd(stress_data_thu$stress_sum, na.rm = TRUE)

# 5.2数据类型的判断与转换

# 数据类型的判断:is族函数

is.numeric(1.0)#判断是否为数值型
is.numeric('a')
is.character(c('a','b'))#判断是否为字符型
is.complex(1+1i)#判断是否为复数型
is.null(NULL)#判断是否为null值
is.na(c(1,NA))#判断是否为NA值

# 数据类型的转化:as族函数
as.numeric(c('1','2','3'))
as.numeric(c('1','2','a'))
as.numeric(c(T,F)) #转化为数值

as.character(c(1,2,3))
as.character(c(T,F))
as.character(as.numeric(c('1','2','3'))) #转化为字符串

#转化为因子
class(penguin_data$sex)
unique(penguin_data$sex)
sex1 <- factor(penguin_data$sex,levels = c(1,2),labels = c('male','female'))
unique(sex1)

#函数调用
here::here()#::左边的here指名为here的R包，::右边的here指这个包中名为here的函数
library(here)
here()

df.pg.raw <- utils::read.csv(file = './data/penguin/penguin_rawdata.csv',
                             header = T, 
                             sep=",",
                             stringsAsFactors = FALSE) 

#定义一个函数：输入x和y，返回3倍x和5倍y的和
mysum <- function(x,y){
  result = 3*x+5*y
  return(result)
}
#mysum:自定义的函数名
#x,y:形式参数
#result = 3*x+5*y:函数体
#return(result):返回值

#调用函数,x=1,y=2
mysum(1,2)
mysum(y=1,x=2)

mysum2 <- function(x = 6,y = 7){
  result = 3*x+5*y
  return(result)
}
mysum2()
mysum2(5)

# 自定义函数
mysum3 <- function(x = 6,y = 7){
  if(is.numeric(x) == T & is.numeric(y) == T){
    result = 3*x+5*y
    return(result)}
  else{print("x and y must be number")}
}
#print：输出指定的内容
#is.numeric:判断是否为数值型。是则返回T，否则返回F
# & : 表示“且”
mysum3(5,6)
mysum3('a','b')
