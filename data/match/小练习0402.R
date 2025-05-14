calculate_abc <- function(a = 3, b = 2, c = 1) {
  # 计算并返回结果
  abc <- (a + b) / c
  if(c==0) print("c should not be 0")
  return(abc)
}

calculate_abc(1,2,0)
