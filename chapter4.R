#Chapter 4

#正式开始前的tips
#set local encoding to English（将当前系统编码设为英文）
if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1250')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}
# set the feedback language to English（将报错语言设为英文）
Sys.setenv(LANG = "en") 


#使用pacman管理包，p_load自动安装没有安装的包
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(bruceR)

# 可以将清华的镜像设置为下载的镜像
# options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load(bruceR)

#4.1.2 绝对路径/相对路径

# r语言中的地址
first_path <- getwd()
cat(first_path,"\n")

# Windows的地址
normalized_path <- normalizePath(first_path, winslash = "\\")
cat(normalized_path)

#here包处理路径问题
pacman::p_load(here)
here::here("Book","data","penguin","penguin_rawdata.csv")

#4.1.3 设定工作目录——手动挡与自动挡

#手动挡
setwd('D:/R4Psy/data/penguin/')
# or 
setwd('D:\\R4Psy\\data\\penguin')

#半自动档——使用Brucer包
# 两个函数等价，ask = T设置弹出交互式窗口选择文件夹
bruceR::set.wd(ask = T)
bruceR::set_wd(ask = T)

#4.2 读取数据——手动挡

# 读取数据,命名为penguin_data
penguin_data = import(here::here("Book",'data', 'penguin', 'penguin_rawdata.csv'))
# 查看头(head) 5 行，有头就有尾(tail)
head(penguin_data,n = 3)
tail(penguin_data,n = 3)
#查列（column）与行（row）
ncol(penguin_data)
nrow(penguin_data)

#4.3数据类型
# getwd 返回当前工作目录，返回的数据类型是？
class(getwd())
# 字符串的数字与数值型的数字；
# 注意区别== 与 =
class('1' == 1)
# 导入数据的类型
class(penguin_data)

#4.4 数据结构

#创建向量
v1
v2 <- c('apple','pear','banana','strawberry','lemon') # 字符型向量
v2
v3 <- c(T,F,F,T,T) # 逻辑型向量
v3
v4 <- c(1:3) # 数值型向量
v4
#创建矩阵
m1 <- matrix(c(1:9), nrow=3)
m1

#创建三维数组
a1 <- array(1:24, dim=c(3,4,2))
a1

d1 <- data.frame(v1,v2,v3)
d1

# 创建空的dataframe，但有列名。
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("name", "age", "gender")
colnames(df) <- x

# 4.5.1 数据索引(中括号)

# 根据行与列的编号
penguin_data[1:2, 1:3]

# 也可以使用减号
penguin_data[1:2, -c(4:ncol(penguin_data))]

# 同样选取前 2 行以及前 4 列数据
penguin_data[1:2, c('age','ALEX1','ALEX2')]

c('age','ALEX1','ALEX2') == cc('age,ALEX1,ALEX2')

# 4.5.2 数据索引($)  

# 根据列名进行索引
head(penguin_data$age)

## 如果数据类型的格式是 ***data.frame***
## 则使用$提取和中括号提取是等价的

class(penguin_data$age)
class(penguin_data[,1])

# 4.5.3 数据索引(逻辑值)  

## 输出逻辑值
head(penguin_data$age >1980)

## 筛选出生年份大于 1980 且(&)小于 1990 的数据
agedata = penguin_data[
  penguin_data$age >1980 & 
    penguin_data$age < 1990 ,]
unique(agedata$age)

## 逻辑运算： 且(&)、或(|)、非(!)、%in%(属于)

# 4.7.2  缺失值(NA)与 NULL
unique(penguin_data$ALEX1)
#直接计算会得到 NA
mean(penguin_data$ALEX1)
# 因此计算时需要移除 NA(remove)
mean(penguin_data$ALEX1,na.rm = T)

#4.7.3 数据类型的转换 
x = TRUE
x = as.numeric(x)
class(x)
x = as.character(x)
class(x)
