title: "Test1"
author: "Fangru"
date: "`r Sys.Date()`"
output:
  html_document: default
pdf_document: default
---

library(tidyverse)
library(here)
# 检查是否安装成功（应返回TRUE）
"here" %in% installed.packages()

# 强制重新加载包（解决缓存问题）
unloadNamespace("here")
library(here)
install.packages("here")  # 如果尚未安装
# 先确保已安装（如果未安装过）
install.packages("tidyverse")  # 或者 install.packages("readr")

# 然后加载包
library(tidyverse)  # 这会自动加载readr
# 或者单独加载readr
library(readr)

# 现在可以使用read_csv()
Q1 <- read_csv(here("data", "penguin", "penguin_rawdata.csv"))

# 方法1：使用here包
Q1 <- read_csv(here("data", "penguin", "penguin_rawdata.csv"))

# 方法2：基础R
 Q1 <- read.csv("data/penguin/penguin_rawdata.csv")

head(Q1)

# 假设你需要选择"Tsinghua"站点的第1到第20行
# 同样存在多种方法
# dplyr
library(dplyr)
Q2 <- Q1 %>% 
  filter(Site == "Zurich") %>% 
  slice(11:30)
# Base R

# 选择变量
Q3 <- Q2 %>% select(health, starts_with("phone"))

# 查看数据类型
str(Q3)

# 如果有字符型需要转换为数字型
# Q3 <- Q3 %>% mutate(across(where(is.character), as.numeric))

# 再次检查数据类型
str(Q3)

Q4 <- Q3 %>%
  mutate(health = case_when(
    health == 5 ~ "Excelent",
    health == 4 ~ "Very Good",
    health == 3 ~ "Good",
    health == 2 ~ "Fair",
    health == 1 ~ "Poor",
    TRUE ~ as.character(health)
  ))
  
  head(Q4)
  
  Q5 <- Q4 %>%
    mutate(health = factor(health, 
                           levels = c("Poor", "Fair", "Good", "Very Good", "Excelent"),
                           ordered = TRUE)) %>%
    arrange(health) %>%
    pull(health)
  
  Q5
  
  Q6_1 <- "Poor" %in% Q5
  Q6_2 <- "Fair" %in% Q5
  
  Q6_1
  Q6_2
  
  Q7 <- paste(Q5, collapse = ", ")
  
  Q7
  
  library(stringr)
  Q8 <- str_length(Q5)
  
  Q8
  
  Q9 <- str_sub(Q5, 1, 1)
  
  Q9
  
  Q10 <- Q4 %>%
    mutate(Num = row_number())
  
  head(Q10)

  Q11 <- Q10 %>%
    filter(health %in% c("Good", "Very Good", "Excelent")) %>%
    pull(Num)
  
 # 在数据框Q10中新增一列"phone_total"，计算被试的phone总分(tips:一个一个录入的效率较低，可参考chapter6提供的简便方法)
  phone_cols <- paste0("phone", 1:9)  # 生成 phone1 到 phone9
  Q10 <- Q10 %>% 
    mutate(phone_total = rowSums(select(., all_of(phone_cols)), na.rm = TRUE))
  Q12``
  
  phone_cols <- grep("^phone[1-9]$", names(Q10), value = TRUE)
  Q10 <- Q10 %>%
    mutate(across(all_of(phone_cols), ~ as.numeric(as.character(.)))) %>%
    mutate(phone_total = rowSums(select(., all_of(phone_cols)), na.rm = TRUE))
  Q13
  
  func <- function(a = 1, b = 1) {
    (a + b)^2
  }
  
  Q14 <- func(3, 4)
  Q14
  
  func2 <- function(x) {
    data.frame(mean = mean(x), sd = sd(x))
  }
  
  Q15 <- func2(c(1, 2, 3, 4, 5))
  Q15
  
  fun3 <- function(x = 10) {
    x %% 2 == 0
  }
  
  Q16 <- fun3(22)
  Q16
  
  func4 <- function(x) {
    if (x > 0) {
      "Positive"
    } else if (x < 0) {
      "Negative"
    } else {
      "Zero"
    }
  }
  
  Q17 <- func4(-3)
  Q17
  
  func5 <- function(x) {
    if (x >= 90) {
      "A"
    } else if (x >= 80) {
      "B"
    } else if (x >= 70) {
      "C"
    } else if (x >= 60) {
      "D"
    } else {
      "E"
    }
  }
  
  Q18 <- func5(95)
  Q18
  
  func6 <- function(n) {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    result
  }
  
  Q19 <- func6(5)
  Q19