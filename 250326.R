library(bruceR)
pacman::p_load(here)

# here::here()
here::here("Book","data","penguin","penguin_rawdata.csv")
# 读取数据,命名为penguin_data
penguin_data = import(here::here("Book",'data', 'penguin', 'penguin_rawdata.csv'))
# 查看头(head) 5 行，有头就有尾(tail)
# head(penguin_data,n = 3)
# tail(penguin_data,n = 3)
# 不要忘记加载包 
library(tidyverse)
## Warning:程辑包'tidyverse'是用R版本4.3.3来建造的
# 加载包后函数前不需要注明包，此处只是为了提示函数属于哪个包 
# 选择我们需要的变量：Temperature_t1, Temperature_t2, SNI28-32, DEQ, romantic, ALEX1-16 
df1 <- dplyr::select(df1,
                     Temperature_t1,Temperature_t2,socialdiversity,Site, DEQ, ALEX1:ALEX16,langfamily)
