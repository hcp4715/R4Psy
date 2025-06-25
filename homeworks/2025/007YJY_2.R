title: "Test2"
author: "Fangru"
date: "`r Sys.Date()`"
output:
  html_document: default
pdf_document: default
---
  #1.读取 penguin_rawdata.csv(data/penguin/ penguin_rawdata.csv),并赋值给Q1
  Q1 <- read.csv("F:/4Rclass_yjy/P4Psy/data/penguin/penguin_rawdata.csv")


#2.根据下列图片中自己所在的位置选择“XX”这一站点后，再从该站点数据中选择前XX行的数据，对Q1进行筛选
#阎佳怡的横坐标是（13,32），纵坐标为Tsinghua
#提示：在Tidyverse中可以调用slice函数来选定相应的列
library(dplyr)
Q2 <- Q1 %>% 
  filter(Site == "Tsinghua") %>%
  slice(13:32)

#3.在Q2中，进一步选择保留“health”和“phone1-9”等变量，赋值给Q3
#查看这几个变量的数据类型，用注释进行记录#
#如果有数据类型为文字，需要转化为数字
Q3 <- Q2 %>% select(health, starts_with("phone"))
selected_cols <- c('health', colnames(Q2)[startsWith(colnames(Q2),'phone')])
Q3 <- Q2[ , selected_cols]
str(Q3)
Q3[i] <- as.numeric(Q3[i]) ### 已改for循环以及unlist ###
str(Q3)

#4.将“health”中的数值改为文字（5= Excelent; 4 = Very Good; 3 = Good; 2 = Fair; 1 = Poor）
#并记录为Q4（tips：可参考chapter_6所学内容）
# 推荐用tidyverse进行操作
Q4 <- Q3 %>%
  dplyr::mutate(health = case_when(
    health == '1' ~ 'Poor',
    health == '2' ~ 'Fair', 
    health == '3' ~ 'Good',
    health == '4' ~ 'Very Good',
    health == '5' ~ 'Excelent')
  )

#5.将 health 赋值给Q5，并按从“Poor”到“Excelent”进行排序
#(tips:通过转化成因子型，可以按照特定方式排序）
Q5 <- factor(Q4$health, 
             levels = c('Poor','Fair','Good','Very Good','Excelent'),
             ordered = TRUE
)
Q5 <- sort(Q5)
print(Q5)
#6.判断'Poor'和'Fair'是否存在于Q5中，输出逻辑值为Q6_1和Q6_2
Q6_1 <- c("Poor" %in% Q5)
Q6_2 <- c("Fair" %in% Q5)
Q6_1
Q6_2

#7.用paste()函数将Q5中的元素连接成一个字符串，中间用英文逗号隔开，并输出为Q7(tips:可以使用"?"查看函数功能)
Q7 <- paste(Q5, collapse = ",")
print(Q7)

#8.使用str_length()函数计算Q5中每个元素的长度，并输出为Q8
library(stringr)
Q8 <- str_length(Q5)
print(Q8)

#9.使用str_sub()函数，提取Q5中每个元素的第一个字母，并输出为Q9
Q9 <- str_sub(Q5, 1, 1)
Q9

#10.在数据框Q4中新增一列，列名为Num
#即这组数据的被试编号，其值为当前所在行
#输出新数据框为Q10
#(tips:可用dplyr中的row_number()函数，或Base R中的nrow()函数,得到数据框的行数)
Q10 <- Q4 %>% mutate(Num = row_number())

#11.找出数据框Q10中健康在'Good'及以上的被试的编号，输出为Q11
Q11 <- Q10$Num[Q10$health == 'Good' | Q10$health == 'Very Good' | Q10$health == 'Excelent']
Q11

#12.在数据框Q10中新增一列"phone_total"，计算被试的phone总分
#(tips:一个一个录入的效率较低，可参考chapter6提供的简便方法)
Q10 <- Q10 %>% 
  mutate(
    phone_total = rowSums(select(., starts_with("phone")))
  )

#13.在数据框中查找：健康在'Good'及以上，
#且phone_total >= 36的所有被试信息，并输出为数据框 Q13。
Q13 <- Q10[(Q10$health == 'Good' | Q10$health == 'Very Good' | Q10$health == 'Excelent') & Q10$phone_total >= 36, ]
Q13

#----------------------------独立题目--------------------------------#

#14.创建一个名为func的函数，输入两个参数 a 和 b (默认值：a=1,b=1)，返回它们的和的平方((a + b)^2)。并使用func函数，计算 a=3 和 b=4 时的值，输出为Q16。
func <- function(a = 1, b = 1) {
  return((a + b) ^ 2)
}
Q16 <- func(3, 4)
Q16

#15.创建一个名为 func2 的函数，该函数接受一个数值向量x作为输入，并返回一个数据框
#要求：第一列为该向量的均值(列名为mean)，第二列为该向量的标准差(列名为sd)。使用该函数计算向量 c(1, 2, 3, 4, 5) 的平均值和标准差，并将结果输出为Q17。(tips:函数mean()和sd()分别用于计算向量的均值和标准差)
func2 <- function(x) {
  mean_val <- mean(x)
  sd_val <- sd(x)
  result_df <- data.frame(mean = mean_val, sd = sd_val)
  return(result_df)
}
Q17 <- func2(c(1, 2, 3, 4, 5))
Q17

#16.创建一个名为 fun3 的函数，该函数接受一个整数x作为输入(默认值：x=10)
#并返回 TRUE（如果输入是偶数）或 FALSE（如果输入是奇数）
#并检验该函数对输入 22 的返回值，输出为Q18。(tips:函数%%用于计算两个数相除的余数)
func3 <- function(x = 10) {
  return(x %% 2 == 0)
}
Q18 <- func3(22)
Q18

#17.编写一个函数 func4，接受一个整数x作为输入，并返回Positive（如果输入是正数）
#Negative（如果输入是负数），Zero（如果输入是零）。
#并检验该函数对输入 -3 的返回值，输出为Q19。
#(tips:if...else语法可以用于根据条件返回不同的值，else if语法可以用于多个条件的判断)
func4 <- function(x) {
  if (x > 0) {
    return("Positive")
  } 
  else if (x < 0) {
    return("Negative")
  } 
  else if (x = 0) {
    return("Zero")
  }
}          ###修复了可能出现文字的错误###
Q19 <- func4(-3)
Q19

#18.编写一个函数 func5，接受一个数值x作为输入，表示学生的分数。该函数的功能是将分数转换成对应的等级，分数大于等于90为"A"，80到89为"B"，70到79为"C"，60到69为"D"，小于60为"E"。然后使用该函数将95分转换成等级，输出为 Q20。(tips:if...else语法可以用于根据条件返回不同的值，else if语法可以用于多个条件的判断)
func5 <- function(x) {
  if (x >= 90) {
    return("A")
  }
  else if (x >= 80) {
    return("B")
  } 
  else if (x >= 70) {
    return("C")
  } 
  else if (x >= 60) {
    return("D")
  } 
  else {        
    return("E")
  }
}
Q20 <- func5(95)
Q20