setwd("./data/match")  # "./" 表示当前目录
# 获取所有match条件的.out文件
match_files <- list.files(pattern = "data_exp7_rep_match_(.*)\\.out$", full.names = TRUE)

# 初始化一个空列表来存储所有数据
all_data <- list()

# 循环读取每个文件
for (i in seq_along(match_files)) {
  # 从文件名中提取被试编号
  subject_id <- sub("data_exp7_rep_match_(.*)\\.out", "\\1", basename(match_files[i]))
  
  # 读取文件内容（空格分隔）
  data <- read.table(
    match_files[i], 
    header = TRUE, 
    sep = " ", 
    stringsAsFactors = FALSE,
    row.names = NULL,
    fill = TRUE,  # 自动填充缺失值
    comment.char = "#",  # 跳过 # 开头的行
    strip.white = TRUE  # 删除多余空格
  )
  # 添加被试编号列
  data$subject_id <- subject_id
  
  # 将数据添加到列表中
  all_data[[i]] <- data  # 这里应该是使用索引i而不是file
  print(match_files[i])  # 在循环里添加，查看报错时 i 的值
}

# 合并所有数据框
combined_data <- do.call(rbind, all_data)
df3<-combined_data
# 查看合并后的数据结构
str(combined_data)
