4.1
gender<-c(0,1,0,1)
stat<-c(97,99,95,96)
H<-c("A","A","B","B")
T<-c("T","F","T","F")
l1<-data.frame(gender,stat,H,T)

5.1
penguin_data <- bruceR::import(here::here('data', 'penguin', 'penguin_rawdata.csv'))
br>
  - ## <font size = 5.0> (1): 选择主观压力(Perceived Stress Scale)条目的列；</font>
  br>
  - ## <font size = 5.0> (2): 选择"Tsinghua”这个站点的行;</font>
  br>
  - ## <font size = 5.0> (3): 计算每个被试"stress"的总分;</font>
  br>
  - ## <font size = 5.0> (4): 计算所有被试的"stress"总分的均值与标准差.</font>
  br>
  #practice
(1)
stress_columns<-c("Site",colnames(penguin_data)[grepl("stress",colnames(penguin_data))])
stress_data<-penguin_data[stress_columns]
(2)
stress_data$Site=="Tsinghua"
stress_data.thu<-stress_data[stress_data$Site=="Tsinghua", ]
(3)
stress_data$sum<-rowSums(stress_data[grepl("stress",colnames(stress_data))], )
(4)
mean(stress_data$sum,na.rm = TRUE)
sd(stress_data$sum,na.rm = TRUE)

6.1
#管道符一般用向右操作符，select专门选择列，filter专门选择行
#管道操作小练习
# 创建dataframe(原始数据)
data <- data.frame(
  "grammer" = c("R","SPSS","Python","R",NA,"Matlab","Python","R"),
  "score" = c(4,2,5,4.5,5,4,2,5),
  "popularity" = c(1,2,NA,4,5,6,7,10)
)
# 提取前两列
select_data1 <- data[ ,1:2]
# 提取含字符串"R"的行
select_data2 <- select_data1[select_data1$grammer == 'R', ]
#practice
select_data2<-data%>%
  dplyr::select(grammer,score)%>%#选择所需要的列
  dplyr::filter(grammer=="R")#选择所需要的行
colnames(df1)#查询df1的列名

6.2
按照langfamily进行分组计算DEQ, CSI(变量名"socialdiversity")的均值
#量表数据导入清理
df1<-bruceR::import(here::here("data","penguin","penguin_rawdata.csv"))
select_DC<-df1%>%
  dplyr::select(DEQ,socialdiversity,langfamily)%>%dplyr::filter(!is.na(DEQ)&!is.na(socialdiversity))%>%group_by(langfamily)%>%summarise(mean_DEQ=mean(DEQ)
,mean_socialdiversity=mean(socialdiversity))%>%dplyr::ungroup()
#课件里的
df3 <- df1 %>%
  dplyr::select(Temperature_t1, Temperature_t2, socialdiversity, Site, 
                DEQ, romantic, ALEX1:ALEX16) %>%#选择指定数据
  dplyr::filter(!is.na(Temperature_t1) & !is.na(Temperature_t2) & !is.na(DEQ)) %>%
  dplyr::mutate(Temperature = rowMeans(dplyr::select(., dplyr::starts_with("Temperature"))),
                ALEX4 = dplyr::case_when(TRUE ~ 6 - ALEX4),
                ALEX12 = dplyr::case_when(TRUE ~ 6 - ALEX12),
                ALEX14 = dplyr::case_when(TRUE ~ 6 - ALEX14),
                ALEX16 = dplyr::case_when(TRUE ~ 6 - ALEX16),
                ALEX = rowSums(dplyr::select(., dplyr::starts_with("ALEX")))) %>% #反向计分
  dplyr::group_by(Site) %>%#分组
  dplyr::summarise(mean_Temperature = mean(Temperature),
                   mean_ALEX = mean(ALEX)) %>%#根据site分组后求其均值
  dplyr::ungroup()#取消分组，还原数据

7.1
#实验数据导入清理
# 获取所有的.out文件名
files <- list.files(here::here("data", "match"), pattern = "data_exp7_rep_match_.*\\.out$", full.names = TRUE)
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
df3 <- data.frame()
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

df4 <- df3 %>%
  dplyr::select(Sub, Age, Sex, Hand, #人口统计学
                Block, Bin, Trial,   # 试次
                Shape, Label, Match, # 刺激
                Resp, ACC, RT) %>%  # 反应结果
  tidyr::drop_na() %>%               #删除缺失值（整行删除，要慎用）
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
  tidyr::pivot_wider(names_from="Identity", values_from="mean_RT") %>%#行列倒置，长数据转化为宽数据
  dplyr::mutate(moral_SPE=Self - Other)

7.2
- 小练习: 定义一个函数,输入值a,b, c, 返回`(a+b)/c`.计算a = 1, b = 2, c = 3时返回的值<br>
- 小练习：<br>
  &emsp;1 定义一个函数,输入值a,b,c,返回(a+b)/c;<br>
  &emsp;2 设置a、b、c的默认值为3、2、1;<br>
  **&emsp;当c为0时报错“c should not be 0”**
  myF<-function(a=3,b=2,c=1){
    if(c!=0){
    result=(a+b)/c
    return(result)
    }else{print("c should not be 0")}
    }
myF(c=0)
综合练习</span></center><br>
- 定义一个函数用于计算$d、$
-计算不同Shape情况下(immoralself，moralself，immoralother，moralother)
<br> 基于信号检测论(match为信号，mismatch噪音)的$d、$
  提示：<br> 
  - (1) $d′$ = ${Z_{击中率}−Z_{虚报率}}$ = $\frac{M_{SN}-M_N}{\sigma_N}$ <br><br>
  - (2) $击中率$ = $\frac{击中次数}{信号总次数}$ <br><br>
  - (3) $虚报率$ = $\frac{虚报次数}{噪音总次数}$ 
  
  calculate_dprime <- function(hit, miss, fa, cr) {#定义函数
    Zhit <- pnorm(hit / (hit + miss))
    Zfa <- pnorm(fa / (fa + cr))
    return(Zhit - Zfa)
  }
  df4 <- df3 %>%
  dplyr::select(Sub, Age, Sex, Hand, #人口统计学
                Block, Bin, Trial,   # 试次
                Shape, Label, Match, # 刺激
                Resp, ACC, RT) %>%  # 反应结果
  tidyr::drop_na() %>%               #删除缺失值（整行删除，要慎用）
  dplyr::filter(Hand == "R",         # 选择右利手被试
                ACC == 0 | ACC == 1, # 排除无效应答（ACC = -1 OR 2)
                RT >= 0.2 & RT <= 1.5) %>%  # 选择RT属于[200,1500]
  tidyr::extract(Shape, into = c("Valence", "Identity"),
                 regex = "(moral|immoral)(Self|Other)", remove = FALSE) %>%
  dplyr::summarise(
    hit = length(ACC[Match == "match" & ACC == 1]),
    cr= length(ACC[Match == "mismatch" & ACC == 0]),
    m= length(ACC[Match == "match" & ACC == 0]),
    fa= length(ACC[Match == "mismatch" & ACC == 1]),)%>%#计算击中(hit)、虚报(fa)、漏报(miss)和正确否定(cr)
  dplyr::mutate(d_prime = calculate_dprime(hit, m, fa, cr))
  
8.4  
  #练习
  .bigfont[
    1. 读取match数据，对不同shape的击中率进行分组绘图，可使用boxplot观察差异。
    
    2. 在上一题中，如何按照特定的顺序来展现 boxplot，比如按照moralSelf - immoralSelf - moralOther - immoralOther(提示：设置因子)
    
    3. 读取penguin数据，选择自己感兴趣的两个变量进行处理并画出散点图。
----------------1.    
    df5 <- df3 %>%
      dplyr::select(Sub, Age, Sex, Hand, #人口统计学
                    Block, Bin, Trial,   # 试次
                    Shape, Label, Match, # 刺激
                    Resp, ACC, RT) %>%  # 反应结果
      tidyr::drop_na() %>%               #删除缺失值（整行删除，要慎用）
      dplyr::filter(Hand == "R",         # 选择右利手被试
                    ACC == 0 | ACC == 1, # 排除无效应答（ACC = -1 OR 2)
                    RT >= 0.2 & RT <= 1.5) %>%  # 选择RT属于[200,1500]
      dplyr::group_by(Shape) %>%         # 按 Shape 分组
      dplyr::summarise(
        hit = sum(Match == "match" & ACC == 1),
        .groups = "drop")                # 取消分组，避免后续操作受影响
    
    
    pic_box <- df5%>% 
    ggplot(aes(x = Shape, y = hit))+
      geom_boxplot(staplewidth = 1) +
      # 绘制箱线图并添加上下边缘线 
      theme_classic()
    pic_box
-----------------------2.
    df5 <- df5 %>%
      mutate(Shape = factor(Shape, 
                            levels = c("moralSelf", "immoralSelf", "moralOther", "immoralOther")))
    picbox <- df5 %>%
      ggplot(aes(x = Shape, y = hit)) +
      geom_boxplot(staplewidth = 1) +
      theme_classic() +
      labs(x = "Shape Category", y = "Hit Rate", title = "Hit Rate by Shape Category")
    
    picbox
-----------------------3.
    df5 <- df3 %>%
      dplyr::select( Hand, ACC) %>%  # 反应结果
      tidyr::drop_na() %>%               #删除缺失值（整行删除，要慎用）
      dplyr::filter( RT >= 0.2 & RT <= 1.5)%>% # 选择RT属于[200,1500]
      ggplot(aes(x = Hand, # 确定映射到xy轴的变量
                 y = RT))+ geom_point()
    df5