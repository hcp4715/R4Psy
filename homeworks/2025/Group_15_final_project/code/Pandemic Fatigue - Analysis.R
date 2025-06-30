#########################################
## Shedding Light on Pandemic Fatigue ##
#######################################

################
## Libraries ## 
##############

library(readr)
library(psych)
library(paran)
library(lavaan)
library(semTools)
library(corrplot)
library(jtools)
library(lme4)
library(doBy)
library(ggplot2)
library(sjPlot)
library(reshape2)
library(ggpubr)
library(PupillometryR)
library(extrafont)
library(JWileymisc)
library(multilevelTools)
library(report)

#################
## Load Data  ##
###############

D <- read_csv("C:/Users/Administrator/Desktop/Group/data/DEN.csv")
G <- read_csv("C:/Users/Administrator/Desktop/Group/data/GER.csv")
P <- read_csv("C:/Users/Administrator/Desktop/Group/data/PAN SYNTHETIC.csv")
E <- read_csv("C:/Users/Administrator/Desktop/Group/data/EXP.csv")
O <- read_csv("C:/Users/Administrator/Desktop/Group/data/OWD.csv")

################
## Functions ##
##############

# Scree plot function from psych package with xlab = "Factors" and ylab = "Eigenvalues" 
Scree <- function (rx, factors = TRUE, pc = TRUE, main = "Scree plot", hline = NULL, add = FALSE) {
  cl <- match.call()
  nvar <- dim(rx)[2]
  if (nvar != dim(rx)[1]) {
    rx <- cor(rx, use = "pairwise")
  }
  if (pc) {
    values <- eigen(rx)$values
    if (factors) {
      ylab = "Eigen values of factors and components"
      xlab = "factor or component number"
    }
    else {
      ylab = "Eigen values of components"
      xlab = " component number"
    }
  }
  else {
    values <- fa(rx)$values
    ylab = "Eigenvalues"
    xlab = "Factors"
    factors <- FALSE
  }
  max.value <- max(values)
  if (!add) {
    plot(values, type = "b", main = main, pch = 16, ylim = c(-.5, max.value), ylab = ylab, xlab = xlab)
  }
  else {
    points(values, type = "b", pch = 16)
  }
  if (factors) {
    fv <- fa(rx)$values
    points(fv, type = "b", pch = 21, lty = "dotted")
  }
  else {
    fv <- NULL
  }
  if (is.null(hline)) {
    abline(h = 1)
  }
  else {
    abline(h = hline)
  }
  if (factors & pc) {
    legend("topright", c("PC ", "FA"), pch = c(16, 21), text.col = "green4", 
           lty = c("solid", "dotted"), merge = TRUE, bg = "gray90")
  }
  if (pc) {
    results <- list(fv = fv, pcv = values, call = cl)
  }
  else {
    results <- list(fv = values, pcv = NULL, call = cl)
  }
  class(results) <- c("psych", "scree")
  invisible(results)
}

#######################
## Factor Variables ##
#####################

# German data 
G$GENDER <- factor(G$GENDER)
G$EDUCATION <- factor(G$EDUCATION)
G$EMPLOYMENT<- factor(G$EMPLOYMENT)
G$CHRONIC <- factor(G$CHRONIC, levels = c("Yes", "No", "Don´t know"))

# Danish data 
D$GENDER <- factor(D$GENDER)
D$EDUCATION <- factor(D$EDUCATION)
D$EMPLOYMENT<- factor(D$EMPLOYMENT)
D$CHRONIC <- factor(D$CHRONIC, levels = c("Yes", "No", "Don´t know"))

# Panel data 
P$GENDER <- factor(P$GENDER)
P$EDUCATION <- factor(P$EDUCATION)
P$CHRONIC <- factor(P$CHRONIC, levels = c("Yes", "No", "Don´t know"))

# Experimental data 
E$GENDER <- factor(E$GENDER)
E$EDUCATION <-  factor(E$EDUCATION, levels = c("Elementary-Secondary School", "High School", "University", "Other"))
E$CONDITION <-  factor(E$CONDITION, levels = c("Low Pandemic Fatigue", "Control", "High Pandemic Fatigue"))

#################################################################################################
## Scale Development and Validation - Exploratory Factor Analysis using Pearson's Correlation ## 
###############################################################################################

# Extract relevant data 
EF <-  D[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_2", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_4", "PANDEMIC_FATIGUE_5", 
           "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
EF <- EF[complete.cases(EF),]

# Inspect the data 
pairs.panels(EF, method = "pearson", pch = '.', jiggle = T) # Correlation and distribution
describe(EF) # Univariate skew and kurtosis 
mardia(EF) # Multivariate skew and kurtosis 

# Kaiser-Meyer-Olkin statistic (KMOS) and Bartlett’s test of sphericity (BTS) and
KMO(EF) # KMOS 
cortest.bartlett(cor(EF), n = 923) # BTS

# Scree plot 
Scree(EF, factors = TRUE, pc=FALSE, hline = -1)

install.packages("C:/Users/Administrator/Downloads/paran-master.zip", repos = NULL, type = "win.binary")
library(paran)

library(psych)

# 设置随机种子保证可重复性
set.seed(3947)

fa.parallel(
  EF, fa = "fa", n.iter = 5000, quant = 0.95,
  pc.col = "black", fa.col = "#00BFC4", sim.col = "#F8766D",
  # 自定义图形参数
  main = "",          # 去除默认标题
  add.legend = FALSE, # 不显示图例
  # 调整坐标轴范围（根据实际数据特征值动态设置）
  ylim = c(0, max(principal(EF, nfactors=1)$values[1] + 0.5)
  )
  
  result <- fa.parallel(EF, fa="fa", n.iter=5000, quant=0.95, plot=FALSE)
  plot(result$fa.value) 
  # Parallel analysis 
  #源代码为paran(EF, cfa = TRUE, graph = TRUE, iterations = 5000, centile = 95, seed = 3947, color = TRUE, col =c("black", "#00BFC4", "#F8766D"))
  
  # Very Simple Structure and Velicer's MAP
  vss(EF, n = 5, fm="minres", maxit = 5000, rotate = "oblimin")
  
  # One factor solution 
  fa(EF, nfactors = 1, fm="minres", rotate="oblimin")
  
  # Two factor solution 
  fa(EF, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Export scree plot to pdf 
  setwd("C:/Users/Administrator/Desktop/Group/Figures")
  pdf("Figure S21AB - Scree plot and parallel analysis using pearsons correlation.pdf", width = 5, height = 5)
  Scree(EF, factors = TRUE, pc=FALSE, hline = -1)
  mtext(expression(paste(bold("A"))), side = 3, line = 2, adj = 0, cex = 1.20)
  fa.parallel(EF, fa="fa", n.iter=5000, quant=0.95)
  mtext(expression(paste(bold("B"))), side = 3, line = 2, adj = 0, cex = 1.20)
  dev.off()
  
  #################################################################################### 
  ## Scale Development and Validation - Item Reduction using Pearson's Correlation ## 
  ##################################################################################
  
  # Dropping PANDEMIC_FATIGUE_4
  EF <-  EF[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_2", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6",
              "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  fa(EF, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_2
  EF <-  EF[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5",  "PANDEMIC_FATIGUE_6",
              "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  fa(EF, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_1
  EF <-  EF[c("PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7", 
              "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  fa(EF, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_8
  EF <-  EF[c("PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7",  
              "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  fa(EF, nfactors = 2, fm="minres", rotate="oblimin")
  rm(EF)
  
  ##################################################################################################
  ## Scale Development and Validation - Exploratory Factor Analysis using Polychoric Correlation ## 
  ################################################################################################
  
  # Extract relevant data 
  EF <-  D[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_2", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_4", "PANDEMIC_FATIGUE_5", 
             "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  EF <- EF[complete.cases(EF),]
  
  # Polychoric correlation matrix 
  POL <- polychoric(EF)
  
  # Kaiser-Meyer-Olkin statistic (KMOS) and Bartlett’s test of sphericity (BTS) and
  KMO(POL$rho) # KMOS 
  cortest.bartlett(POL$rho, n = 923) # BTS
  
  # Scree plot 
  Scree(POL$rho, factors = TRUE, pc=FALSE, hline = -1)
  
  # Parallel analysis 
  library(psych)
  
  # 设置随机种子保证可重复性
  set.seed(3947)
  
  # 执行平行分析
  library(psych)
  
  # 设置随机种子（与paran的seed=3947一致）
  set.seed(3947)
  
  # 执行平行分析
  fa.parallel(
    x = POL$rho,          # 输入相关矩阵（对应mat参数）
    n.obs = 923,          # 样本量（对应n参数）
    fa = "fa",            # 因子分析（对应cfa=TRUE）
    n.iter = 5000,        # 迭代次数（对应iterations）
    quant = 0.95,         # 百分位（对应centile=95）
    show.legend = TRUE,   # 显示图例
    main = "Parallel Analysis"
  )
  #原代码为paran(mat = POL$rho, n = 923, cfa = TRUE, graph = TRUE, iterations = 5000, centile = 95, seed = 3947, color = TRUE, col =c("black", "#00BFC4", "#F8766D"))
  
  # Very Simple Structure and Velicer's MAP
  vss(POL$rho, n.obs = 923, n = 5, fm="minres", maxit = 5000, rotate = "oblimin")
  
  # One factor solution 
  fa(POL$rho, n.obs = 923, nfactors = 1, fm="minres", rotate="oblimin")
  
  # Two factor solution 
  fa(POL$rho, n.obs = 923, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Three factor solution 
  fa(POL$rho, n.obs = 923, nfactors = 3, fm="minres", rotate="oblimin")
  
  # Export scree plot to pdf 
  setwd("C:/Users/Administrator/Desktop/Group/Figures")
  pdf("Figure S21CD - Scree plot and parallel analysis using polychoric correlation.pdf", width = 5, height = 5)
  Scree(POL$rho, factors = TRUE, pc=FALSE, hline = -1)
  mtext(expression(paste(bold("C"))), side = 3, line = 2, adj = 0, cex = 1.20)
  fa.parallel(
    x = POL$rho,          # 输入相关矩阵（对应mat参数）
    n.obs = 923,          # 样本量（对应n参数）
    fa = "fa",            # 因子分析（对应cfa=TRUE）
    n.iter = 5000,        # 迭代次数（对应iterations）
    quant = 0.95,         # 百分位（对应centile=95）
    show.legend = TRUE,   # 显示图例
    main = "Parallel Analysis"
  )
  mtext(expression(paste(bold("D"))), side = 3, line = 2, adj = 0, cex = 1.20)
  dev.off()
  
  ##################################################################################### 
  ## Scale Development and Validation - Item Reduction using Polychoric Correlation ## 
  ###################################################################################
  
  # Dropping PANDEMIC_FATIGUE_4
  EF <-  EF[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_2", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6",
              "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  POL <- polychoric(EF)
  fa(POL$rho, n.obs = 923, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_2
  EF <-  EF[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5",  "PANDEMIC_FATIGUE_6",
              "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  POL <- polychoric(EF)
  fa(POL$rho, n.obs = 923, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_1
  EF <-  EF[c("PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7", 
              "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  POL <- polychoric(EF)
  fa(POL$rho, n.obs = 923, nfactors = 2, fm="minres", rotate="oblimin")
  
  # Dropping PANDEMIC_FATIGUE_8
  EF <-  EF[c("PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_5", "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7",  
              "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  POL <- polychoric(EF)
  fa(POL$rho, n.obs = 923, nfactors = 2, fm="minres", rotate="oblimin")
  rm(EF, POL)
  
  ######################################################################################### 
  ## Scale Development and Validation - Correlation Between Initial and Final PFS Scale ## 
  #######################################################################################
  
  # Extract relevant data
  COR <-  D[c("PANDEMIC_FATIGUE_1", "PANDEMIC_FATIGUE_2", "PANDEMIC_FATIGUE_3", "PANDEMIC_FATIGUE_4", "PANDEMIC_FATIGUE_5", 
              "PANDEMIC_FATIGUE_6", "PANDEMIC_FATIGUE_7", "PANDEMIC_FATIGUE_8", "PANDEMIC_FATIGUE_9", "PANDEMIC_FATIGUE_10")]
  COR <- COR[complete.cases(COR),]
  
  # Calculate composite scores of the initial pandemic fatigue scale  
  COR$INITIAL_SCALE <- (COR$PANDEMIC_FATIGUE_1 + COR$PANDEMIC_FATIGUE_2 + COR$PANDEMIC_FATIGUE_3  + COR$PANDEMIC_FATIGUE_4 + COR$PANDEMIC_FATIGUE_5 +
                          COR$PANDEMIC_FATIGUE_6 +  COR$PANDEMIC_FATIGUE_7 + COR$PANDEMIC_FATIGUE_8 +  COR$PANDEMIC_FATIGUE_9 +  COR$PANDEMIC_FATIGUE_10)/10 
  
  # Calculate composite scores of the final pandemic fatigue scale  
  COR$FINAL_SCALE <- (COR$PANDEMIC_FATIGUE_3 + COR$PANDEMIC_FATIGUE_5 + COR$PANDEMIC_FATIGUE_6 + COR$PANDEMIC_FATIGUE_7 + COR$PANDEMIC_FATIGUE_9 + COR$PANDEMIC_FATIGUE_10)/6 
  
  # Correlation analysis 
  cor.test(COR$INITIAL_SCALE, COR$FINAL_SCALE)
  rm(COR)
  
  #############################################################################################################
  ## Scale Development and Validation - Confirmatory Factor Analysis - Robust Maximum Likelihood Estimation ## 
  ###########################################################################################################
  
  # Extract relevant data 
  DEN_CFA <- subset(D, Wave != 19, select = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                              "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  GER_CFA <- subset(G, select = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  # Inspect the data - Germany
  pairs.panels(GER_CFA, method = "pearson", pch = '.', jiggle = T) # Correlation and distribution
  describe(GER_CFA) # Univariate skew and kurtosis 
  mardia(GER_CFA) # Multivariate skew and kurtosis 
  
  # Inspect the data - Denmark 
  pairs.panels(DEN_CFA, method = "pearson", pch = '.', jiggle = T) # Correlation and distribution
  describe(DEN_CFA) # Univariate skew and kurtosis 
  mardia(DEN_CFA) # Multivariate skew and kurtosis 
  
  # Define two factor model 
  Model_1 <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
            BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB'
  
  # Windows系统（.zip文件）
  install.packages("C:/Users/Administrator/Downloads/OpenMx-master.zip", 
                   repos = NULL, 
                   type = "win.binary",
                   dependencies = c("Depends", "Suggests"))
  #require(OpenMx)
  #source('http://openmx.psyc.virginia.edu/getOpenMx.R')
  #install.packages(c("Rcpp", "Matrix", "mvtnorm", "numDeriv", "rpf"))
  #library(OpenMx)
  #library(semPlot)
  # Fit two factor model - Germany 
  #源代码Fit_1_GER <- cfa(Model_1, data=GER_CFA, std.lv = TRUE, estimator = "MLM")
  library(lavaan)
  library(semPlot)
  # 2. 运行CFA分析
  Fit_1_GER <- cfa(
    model = Model_1,        # 模型语法
    data = GER_CFA,        # 数据框
    std.lv = TRUE,         # 标准化潜在变量（均值=0，方差=1）
    estimator = "ML",     # 稳健最大似然估计（处理非正态数据）
    missing = "ml"         # 处理缺失值（可选）
  )
  # 3. 查看结果
  output <- capture.output(
    summary(Fit_1_GER, 
          standardized = TRUE,  # 显示标准化系数
          fit.measures = TRUE,  # 显示拟合指标
          rsquare = TRUE)
    )# 显示R²
  writeLines(output, "双因素GER_summary.txt")
  semPaths(Fit_1_GER, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Germany (n = 17,946)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("C"))), side = 3, line = 2, adj = .25, cex = 1.20)
  library(lavaan)
  # Fit two factor model - Denmark
  Fit_1_DK <- cfa(Model_1, data=DEN_CFA, std.lv = TRUE, estimator = "MLM")
  Fit_1_DK <- cfa(
    model = Model_1,           # 模型语法
    data = DEN_CFA,           # 数据框
    std.lv = TRUE,            # 标准化潜在变量（均值=0，方差=1）
    estimator = "ML",        # 稳健最大似然估计（处理非正态数据）
    missing = "ml"            # 全息极大似然处理缺失值（若数据有缺失）
  )
  std_est <- standardizedSolution(Fit_1_DK)
  output <- capture.output(
    summary(Fit_1_DK, 
            standardized = TRUE,
            fit.measures = TRUE,
            rsquare = TRUE)
  )
  writeLines(output, "双因素DK_summary.txt")
  semPaths(Fit_1_DK, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  # 添加标题的正确方式
  title("Germany (n = 17,946)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("C"))), side = 3, line = 2, adj = .25, cex = 1.20)
  
  
  # 1. 运行CFA模型（假设已完成）
 Fit_1_DK <- cfa(Model_1, data=DEN_CFA, std.lv=TRUE, estimator="MLM")
  
  # 2. 绘制路径图（lavaanPlot实现）
  # 查看PhantomJS安装路径
  phantom_path <- Sys.which("phantomjs")
  if(phantom_path == "") {
    cat("PhantomJS未安装或未在PATH中\n")
  } else {
    cat("PhantomJS路径:", phantom_path, "\n")
  }

  library(htmlwidgets)
  library(lavaanPlot)
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  
  # 处理数据
  model_data <- parameterEstimates(Fit_1_DK, standardized = TRUE) %>% 
    filter(op == "=~") %>%  
    mutate(
      # 创建标签映射
      lhs_label = case_when(
        lhs == "F1" ~ "Information\nfatigue",
        lhs == "F2" ~ "Behavioral\nfatigue",
        TRUE ~ lhs
      ),
      rhs_label = case_when(
        rhs == "item1" ~ "Item 1",
        rhs == "item2" ~ "Item 2",
        rhs == "item3" ~ "Item 3", 
        rhs == "item4" ~ "Item 4",
        rhs == "item5" ~ "Item 5",
        rhs == "item6" ~ "Item 6",
        TRUE ~ rhs
      ),
      # 转换为数值用于定位
      lhs_num = as.numeric(factor(lhs, levels = unique(lhs))),
      rhs_num = as.numeric(factor(rhs, levels = unique(rhs)))
    )
  
  # 绘制图形
  ggplot(model_data) +
    # 添加连接线
    geom_segment(
      aes(x = 1, xend = 2, y = lhs_num, yend = rhs_num),
      arrow = arrow(length = unit(0.3, "cm"))
    ) +
    # 添加因子载荷标签
    geom_label(
      aes(x = 1.5, y = (lhs_num + rhs_num)/2, 
          label = round(std.all, 2)),
      size = 3
    ) +
    # 添加左边标签（潜变量）
    geom_text(
      aes(x = 0.9, y = lhs_num, label = lhs_label),
      hjust = 1, size = 4
    ) +
    # 添加右边标签（观测变量）
    geom_text(
      aes(x = 2.1, y = rhs_num, label = rhs_label),
      hjust = 0, size = 4
    ) +
    # 调整坐标轴和主题
    scale_x_continuous(limits = c(0.5, 2.5)) +
    theme_void() +
    theme(
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  # 单因子模型定义
  Model_2 <- '
  PF =~ PANDEMIC_FATIGUE_1_INF + 
        PANDEMIC_FATIGUE_3_INF + 
        PANDEMIC_FATIGUE_5_INF + 
        PANDEMIC_FATIGUE_2_MB + 
        PANDEMIC_FATIGUE_4_MB + 
        PANDEMIC_FATIGUE_6_MB
'
 ######################################### Fit one factor model - Germany 
  Fit_2_GER <- cfa(Model_2, data=GER_CFA, std.lv = TRUE, estimator = "MLM")
  output <- capture.output(
    summary(Fit_2_GER, 
            standardized = TRUE,
            fit.measures = TRUE,
            rsquare = TRUE)
  )
  writeLines(output, "真正单因素GER_summary.txt")

  # 获取标准化估计值
  std_est <- standardizedSolution(Fit_2_GER)
  
  semPaths(
    object = Fit_2_GER,
    what = "std",           # 显示标准化系数
    whatLabels = "std",     # 标签显示标准化值
    layout = "tree",        # 树状布局
    
    # ===== 节点设置 =====
    shapeMan = "square",    # 观测变量形状改为square
    shapeLat = "ellipse",   # 潜变量形状
    sizeMan = 7,            # 观测变量大小
    sizeLat = 10,           # 潜变量大小
    color = list(           # 颜色设置
      man = rgb(224, 255, 255, maxColorValue = 255),  # 观测变量颜色
      lat = rgb(255, 250, 205, maxColorValue = 255)   # 潜变量颜色
    ),
    
    # ===== 边设置 =====
    edge.color = "black",   # 边颜色
    edge.width = 1,         # 边宽度
    edge.label.cex = 0.8,   # 边标签大小
    
    # ===== 标签设置 =====
    nodeLabels = c("Item 1", "Item 2", "Item 3", 
                   "Item 4", "Item 5", "Item 6", 
                   "Pandemic\nfatigue"),
    label.cex = 1.2,        # 标签大小
    
    # ===== 其他设置 =====
    rotation = 2,           # 图形旋转
    mar = c(7, 7, 7, 7),    # 边距设置
    optimizeLatRes = TRUE,  # 优化潜变量布局
    residuals = FALSE,      # 不显示残差
    thresholds = FALSE      # 不显示阈值
  )
  
  # 如果需要更高分辨率输出
  # 可以先保存为PDF或PNG
  # pdf("model_plot.pdf", width=10, height=8)
  # semPaths(...)
  # dev.off()
  # 查看变量顺序（确保标签匹配）
  lavNames(Fit_2_GER)
  
  library(lavaanPlot)
  ############################################ Fit one factor model - Denmark 
  Fit_2_DK <- cfa(Model_2, data=DEN_CFA, std.lv = TRUE, estimator = "MLM")
  summary(Fit_2_DK, fit.measures=TRUE, standardized=TRUE)
  library(semPlot)

  std_est <- standardizedSolution(Fit_2_DK)
  output <- capture.output(
    summary(Fit_2_DK, 
            standardized = TRUE,
            fit.measures = TRUE,
            rsquare = TRUE)
  )
  writeLines(output, "真正单因素DK_summary.txt")
  
  semPaths(
    object = Fit_2_DK,
    what = "std",           # 显示标准化系数
    whatLabels = "std",     # 标签显示标准化值
    layout = "tree",        # 树状布局
    
    # ===== 节点设置 =====
    shapeMan = "square",    # 观测变量形状改为square
    shapeLat = "ellipse",   # 潜变量形状
    sizeMan = 7,            # 观测变量大小
    sizeLat = 10,           # 潜变量大小
    color = list(           # 颜色设置
      man = rgb(224, 255, 255, maxColorValue = 255),  # 观测变量颜色
      lat = rgb(255, 250, 205, maxColorValue = 255)   # 潜变量颜色
    ),
    
    # ===== 边设置 =====
    edge.color = "black",   # 边颜色
    edge.width = 1,         # 边宽度
    edge.label.cex = 0.8,   # 边标签大小
    
    # ===== 标签设置 =====
    nodeLabels = c("Item 1", "Item 2", "Item 3", 
                   "Item 4", "Item 5", "Item 6", 
                   "Pandemic\nfatigue"),
    label.cex = 1.2,        # 标签大小
    
    # ===== 其他设置 =====
    rotation = 2,           # 图形旋转
    mar = c(7, 7, 7, 7),    # 边距设置
    optimizeLatRes = TRUE,  # 优化潜变量布局
    residuals = FALSE,      # 不显示残差
    thresholds = FALSE      # 不显示阈值
  )
  
  # Define second-order model 
  Model_3 <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
            BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB
            PF =~   a*IF + a*BF'
  
  ######################################### Fit second-order model - Germany 
  Fit_3_GER <- cfa(Model_3, data=GER_CFA, std.lv = TRUE, estimator = "MLM")
  output <- capture.output(
    summary(Fit_3_GER, 
            standardized = TRUE,
            fit.measures = TRUE,
            rsquare = TRUE)
  )
  writeLines(output, "单因素GER_summary.txt")
  semPaths(Fit_3_GER, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Fit second-order model - Denmark 
  Fit_3_DK <- cfa(Model_3, data=DEN_CFA, std.lv = TRUE, estimator = "MLM")
  output <- capture.output(
    summary(Fit_3_DK, 
            standardized = TRUE,
            fit.measures = TRUE,
            rsquare = TRUE)
  )
  writeLines(output, "单因素DK_summary.txt")
  semPaths(Fit_3_DK, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  par(mfrow=c(1,2))
  semPaths(Fit_1_DK, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Denmark (n = 15,062)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("C"))), side = 3, line = 2, adj = .25, cex = 1.20)
  semPaths(Fit_1_GER, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Germany (n = 17,946)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("D"))), side = 3, line = 2, adj = .25, cex = 1.20)
  dev.off()
  rm(Fit_1_DK, Fit_1_GER, Fit_2_DK, Fit_2_GER, Fit_3_DK, Fit_3_GER)
  
  ############################################################################################################################
  ## Scale Development and Validation - Confirmatory Factor Analysis - Robust Diagonally Weighted Least Squares Estimation ## 
  ##########################################################################################################################
  
  # Fit two factor model - Germany 
  Fit_1_GER <- cfa(Model_1, data=GER_CFA, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                     "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                     "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_1_GER, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_1_GER, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8,sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Fit two factor model - Denmark
  Fit_1_DK <- cfa(Model_1, data=DEN_CFA, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                    "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                    "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_1_DK, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_1_DK, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Fit one factor model - Germany 
  Fit_2_GER <- cfa(Model_2, data=GER_CFA, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                   "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                     "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  std_est <- standardizedSolution(Fit_2_DK)
  summary(Fit_2_GER, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_2_GER,  intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Fit one factor model - Denmark 
  Fit_2_DK <- cfa(Model_2, data=DEN_CFA, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                    "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                    "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_2_DK, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_2_DK, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE,esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Export plots to pdf 
  par(mfrow=c(1,2))
  semPaths(Fit_1_DK, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Denmark (n = 15,062)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("A"))), side = 3, line = 2, adj = .25, cex = 1.20)
  semPaths(Fit_1_GER, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8,sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Germany (n = 17,946)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("B"))), side = 3, line = 2, adj = .25, cex = 1.20)
  par(mfrow=c(1,2))
  semPaths(Fit_3_DK, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Denmark (n = 15,062)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("C"))), side = 3, line = 2, adj = .25, cex = 1.20)
  semPaths(Fit_3_GER, intercepts = FALSE, thresholds = FALSE, what="std", edge.label.cex=1, edge.color="black", sizeMan=8, sizeLat=12, fade=FALSE, esize=1, asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Germany (n = 17,946)", line = -20.5, cex = 1)
  mtext(expression(paste(bold("D"))), side = 3, line = 2, adj = .25, cex = 1.20)
  dev.off()
  rm(Fit_1_DK, Fit_1_GER, Fit_2_DK, Fit_2_GER, Fit_3_DK, Fit_3_GER)
  
  ##########################################################
  ## Scale Development and Validation - Cronbach´s Alpha ## 
  ########################################################
  
  # Alpha full pandemic fatigue scale - Germany
  psych::alpha(data.frame(GER_CFA[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                    "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # Alpha - Information fatigue factor - Germany 
  psych::alpha(data.frame(GER_CFA[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF")]), check.keys=TRUE) 
  
  # Alpha - Behavioral fatigue factor - Germany
  psych::alpha(data.frame(GER_CFA[c("PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # Alpha full pandemic fatigue scale - Denmark
  psych::alpha(data.frame(DEN_CFA[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                    "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # Alpha - Information fatigue factor - Denmark
  psych::alpha(data.frame(DEN_CFA[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF")]), check.keys=TRUE) 
  
  # Alpha - Behavioral fatigue factor - Denmark
  psych::alpha(data.frame(DEN_CFA[c("PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  ###########################################################
  ## Scale Development and Validation - MacDonald´s Omega ## 
  #########################################################
  
  # MacDonald´s omega - Germany 
  reliability(cfa(Model_3, data=GER_CFA, std.lv = TRUE)) # IF + BF 
  reliabilityL2(cfa(Model_3, data=GER_CFA, std.lv = TRUE), secondFactor = "PF") # PF
  
  # MacDonald´s omega - Denmark
  reliability(cfa(Model_3, data=DEN_CFA, std.lv = TRUE)) # IF + BF
  reliabilityL2(cfa(Model_3, data=DEN_CFA, std.lv = TRUE), secondFactor = "PF") # PF
  rm(Model_1, Model_2, Model_3, GER_CFA, DEN_CFA)
  
  #######################################################################################################
  ## Scale Development and Validation - Measurement Invariance - Robust Maximum Likelihood Estimation ##
  #####################################################################################################
  
  # Extract relevant data 
  DEN_MI <- subset(D,  select = c("Wave", "GENDER", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  GER_MI <- subset(G, select = c("Wave", "GENDER", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                 "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  DEN_MI$COUNTRY <- "Denmark"
  GER_MI$COUNTRY <- "Germany"
  MI <- rbind(DEN_MI, GER_MI)
  MI$COUNTRY <- factor(MI$COUNTRY)
  rm(DEN_MI, GER_MI)
  
  # Two factor model configural invariance - Denmark vs Germany 
  conInvTwo <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
              BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB'
  
  fitConInvTwo <- cfa(conInvTwo, data = MI , group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitConInvTwo, fit.measures = TRUE, standardized = TRUE) 
  # -->  Configural invariance achieved: CFI =  0.987, TLI =  0.975, RMSEA =  0.064, SRMR = 0.024. 
  
  # Two factor metric invariance - Denmark vs Germany 
  metTwo <- '# Constrain loadings to equality 
             IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
             BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
            
           # Fix the factor variances to 1 in the reference group only (see https://doi.org/10.1080/10705510701301677) 
             IF ~~ c(NA, 1)*IF
             BF ~~ c(NA, 1)*BF'
  install.packages("lavaan.mi")
  library(lavaan.mi)
  library(semTools)
  fitMetTwo <- cfa(metTwo, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitMetTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvTwo, fitMetTwo))
  # -->  Metric invariance not achieved: Difference in CFI >  -0.01 (see https://doi.org/10.1207/S15328007SEM0902_5)     
  
  # Look for sources of non-invariance 
  lavTestScore(fitMetTwo)
  partable(fitMetTwo)
  # --> The results suggest that PANDEMIC_FATIGUE_2_MB is a source of non-invariance 
  
  # Two factor model partial metric invariance - Denmark vs Germany
  parMetTwo <- '# Constrain loadings to equality but free loadings of PANDEMIC_FATIGUE_2_MB
                IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
                BF =~ c(NA, NA)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
             
              # Fix the factor variances to 1 in the reference group only 
                IF ~~ c(NA, 1)*IF
                BF ~~ c(NA, 1)*BF'
  
  fitParMetTwo <- cfa(parMetTwo, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitParMetTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvTwo, fitParMetTwo))
  # -->  Partial metric invariance achieved: Difference in CFI < -0.01    
  
  # Two factor model partial scalar invariance 
  parScalTwo <- '# Constrain loadings to equality but free loadings of PANDEMIC_FATIGUE_2_MB
                 IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
                 BF =~ c(NA, NA)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
             
               # Fix the factor variances to 1 in the reference group only 
                 IF ~~ c(NA, 1)*IF
                 BF ~~ c(NA, 1)*BF
            
               # Constrain intercepts to equality except for PANDEMIC_FATIGUE_2_MB
                 PANDEMIC_FATIGUE_1_INF ~ c(g,g)*1
                 PANDEMIC_FATIGUE_3_INF ~ c(h,h)*1
                 PANDEMIC_FATIGUE_5_INF ~ c(i,i)*1
                 PANDEMIC_FATIGUE_2_MB ~ c(NA,NA)*1
                 PANDEMIC_FATIGUE_4_MB ~ c(j,j)*1
                 PANDEMIC_FATIGUE_6_MB ~ c(k,k)*1

               # Fix the factor means to 0 in the reference group only 
                 IF ~ c(NA, 0)*1
                 BF ~ c(NA, 0)*1'
  
  fitParScalTwo <- cfa(parScalTwo, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitParScalTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitParMetTwo, fitParScalTwo))
  # -->  Partial scalar invariance achieved: Difference in CFI < -0.01
  
  rm(fitConInvTwo, fitMetTwo, fitParMetTwo, fitParScalTwo,
     conInvTwo, metTwo, parMetTwo, parScalTwo)
  
  # Second-order model configural invariance - Denmark vs Germany 
  conInvSec <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
              BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB
              PF =~ c(x,z)*BF + c(x,z)*IF' 
  fitConInvSec <- cfa(conInvSec, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitConInvSec, fit.measures = TRUE, standardized = TRUE) 
  # -->  Configural invariance achieved: CFI =  0.987, TLI = 0.975, RMSEA = 0.064, SRMR = 0.024.  
  
  # Second-order model metric invariance - Denmark vs Germany 
  metSec <- '# Constrain loadings to equality 
             IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
             BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
             PF =~ c(x,x)*BF + c(x,x)*IF

           # Fix the factor variances to 1 in the reference group only (see https://doi.org/10.1080/10705510701301677) 
             IF ~~ c(NA, 1)*IF
             BF ~~ c(NA, 1)*BF
             PF ~~ c(NA, 1)*PF'
  
  fitMetSec <- cfa(metSec, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitMetSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvSec, fitMetSec))
  
  # Look for sources of non-invariance 
  lavTestScore(fitMetSec)
  partable(fitMetSec)
  # --> The results suggest that PANDEMIC_FATIGUE_2_MB is a source of non-invariance 
  
  # Second-order model partial metric invariance - Denmark vs Germany
  parMetSec <- '# Constrain loadings to equality but free loadings of PANDEMIC_FATIGUE_2_MB
                IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
                BF =~ c(NA,NA)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
                PF =~ c(x,x)*BF + c(x,x)*IF
                
              # Fix the factor variances to 1 in the reference group only
                IF ~~ c(NA, 1)*IF
                BF ~~ c(NA, 1)*BF
                PF ~~ c(NA, 1)*PF'
  
  fitParMetSec <- cfa(parMetSec, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitParMetSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvSec, fitParMetSec))
  # -->  Partial metric invariance achieved: Difference in CFI < -0.01   
  
  # Second-order model partial scalar invariance 
  parScalSec <- '# Constrain loadings to equality but free loadings of PANDEMIC_FATIGUE_2_MB
                 IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
                 BF =~ c(NA,NA)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
                 PF =~ c(x,x)*BF + c(x,x)*IF
                 
               # Fix the factor variances to 1 in the reference group only
                 IF ~~ c(NA, 1)*IF
                 BF ~~ c(NA, 1)*BF
                 PF ~~ c(NA, 1)*PF

               # Constrain intercepts to equality except for PANDEMIC_FATIGUE_2_MB
                 PANDEMIC_FATIGUE_1_INF ~ c(g,g)*1
                 PANDEMIC_FATIGUE_3_INF ~ c(h,h)*1
                 PANDEMIC_FATIGUE_5_INF ~ c(i,i)*1
                 PANDEMIC_FATIGUE_2_MB ~ c(NA,NA)*1
                 PANDEMIC_FATIGUE_4_MB ~ c(j,j)*1
                 PANDEMIC_FATIGUE_6_MB ~ c(k,k)*1
               
               # Fix the sub-factor means to 0 in the reference group only 
                 IF ~ c(NA, 0)*1
                 BF ~ c(NA, 0)*1
               
               # Fix the the mean of PF to 0 and equality for identification 
                 PF ~ c(0, 0)*1'
  
  fitParScalSec <- cfa(parScalSec, data = MI, group = "COUNTRY", std.lv = TRUE, estimator = 'MLM')
  summary(fitParScalSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitParMetSec, fitParScalSec))
  # -->  Partial scalar invariance achieved: Difference in CFI < -0.01 
  
  rm(fitConInvSec, fitMetSec, fitParMetSec, fitParScalSec, MI,
     conInvSec, metSec, parMetSec, parScalSec)
  
  ######################################################################################################################
  ## Scale Development and Validation - Measurement Invariance - Robust Diagonally Weighted Least Squares Estimation ##
  ####################################################################################################################
  
  # Extract relevant data 
  DEN_MI <- subset(D,  select = c("Wave", "GENDER", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  GER_MI <- subset(G, select = c("Wave", "GENDER", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                 "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  DEN_MI$COUNTRY <- "Denmark"
  GER_MI$COUNTRY <- "Germany"
  MI <- rbind(DEN_MI, GER_MI)
  MI$COUNTRY <- factor(MI$COUNTRY)
  rm(DEN_MI, GER_MI)
  
  # Two factor model configural invariance - Denmark vs Germany 
  conInvTwo <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
              BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB

            # Fix residual variances to 1
              PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
              PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
              PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
              PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
              PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
              PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB'
  
  fitConInvTwo <- cfa(conInvTwo, data = MI , group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                      ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                  "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                  "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitConInvTwo, fit.measures = TRUE, standardized = TRUE) 
  # -->  Configural invariance achieved: CFI =  0.992, TLI = 0.985, RMSEA = 0.087, SRMR = 0.025. 
  
  # Two factor metric invariance - Denmark vs Germany 
  metTwo <- '# Constrain loadings to equality 
             IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
             BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
         
           # Fix residual variances to 1
             PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
             PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
             PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
             PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
             PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
             PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB
            
           # Fix the factor variances to 1 in the reference group only 
             IF ~~ c(NA, 1)*IF
             BF ~~ c(NA, 1)*BF'
  
  fitMetTwo <- cfa(metTwo, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                   ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                               "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                               "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitMetTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvTwo, fitMetTwo))
  # -->  Metric invariance achieved: Difference in CFI < -0.01 (see https://doi.org/10.1207/S15328007SEM0902_5)     
  
  # Two factor model scalar invariance - Denmark vs Germany 
  scalTwo <- '# Constrain loadings to equality
              IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
              BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
            
            # Fix residual variances to 1
              PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
              PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
              PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
              PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
              PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
              PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB
          
            # Fix the factor variances to 1 in the reference group only 
              IF ~~ c(NA, 1)*IF
              BF ~~ c(NA, 1)*BF
                
            # Free factor means in reference group 
              IF ~ c(NA, 0)*1
              BF ~ c(NA, 0)*1'
  
  fitScalTwo <- cfa(scalTwo, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                    group.equal = c("thresholds"), # Fix thresholds to equality
                    ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitScalTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitMetTwo, fitScalTwo))
  # -->  Scalar invariance not achieved: Difference in CFI > -0.01
  
  # Look for sources of non-invariance 
  lavTestScore(fitScalTwo)
  partable(fitScalTwo)
  # --> The results suggest that PANDEMIC_FATIGUE_2_MB is a source of non-invariance 
  
  # Two factor model partial scalar invariance - Denmark vs Germany 
  parScalTwo <- '# Constrain loadings to equality
                 IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
                 BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
            
               # Fix residual variances to 1
                 PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
                 PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
                 PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
                 PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
                 PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
                 PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB
          
               # Fix the factor variances to 1 in the reference group only 
                 IF ~~ c(NA, 1)*IF
                 BF ~~ c(NA, 1)*BF
                
               # Free factor means in reference group 
                 IF ~ c(NA, 0)*1
                 BF ~ c(NA, 0)*1'
  
  fitParScalTwo <- cfa(parScalTwo, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta", 
                       group.equal = c("thresholds"), # Fix thresholds to equality 
                       group.partial = c("PANDEMIC_FATIGUE_2_MB|t1", "PANDEMIC_FATIGUE_2_MB|t2", # Free thresholds for PANDEMIC_FATIGUE_2_MB
                                         "PANDEMIC_FATIGUE_2_MB|t3", "PANDEMIC_FATIGUE_2_MB|t4", 
                                         "PANDEMIC_FATIGUE_2_MB|t5", "PANDEMIC_FATIGUE_2_MB|t6"),
                       ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                   "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                   "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitParScalTwo, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitMetTwo, fitParScalTwo))
  # -->  Partial scalar invariance achieved: Difference in CFI = -0.01
  
  rm(fitConInvTwo, fitMetTwo, fitScalTwo, fitParScalTwo, conInvTwo, metTwo, scalTwo, parScalTwo)
  
  # Second-order model configural invariance - Denmark vs Germany 
  conInvSec <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
              BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB
              PF =~ c(x,z)*BF + c(x,z)*IF
              
            # Fix residual variances to 1
              PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
              PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
              PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
              PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
              PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
              PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB' 
  
  fitConInvSec <- cfa(conInvSec, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                      ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                  "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                  "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitConInvSec, fit.measures = TRUE, standardized = TRUE) 
  # -->  Configural invariance achieved: CFI =  0.992, TLI = 0.985, RMSEA = 0.087, SRMR = 0.025. 
  
  # Second-order model metric invariance - Denmark vs Germany 
  metSec <- '# Constrain loadings to equality 
             IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
             BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
             PF =~ c(x,x)*BF + c(x,x)*IF
             
           # Fix residual variances to 1
             PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
             PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
             PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
             PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
             PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
             PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB

           # Fix the factor variances to 1 in the reference group only
             IF ~~ c(NA, 1)*IF
             BF ~~ c(NA, 1)*BF
             PF ~~ c(NA, 1)*PF'
  
  fitMetSec <- cfa(metSec, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                   ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                               "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                               "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitMetSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitConInvSec, fitMetSec))
  # -->  Metric invariance achieved: Difference in CFI < -0.01 (see https://doi.org/10.1207/S15328007SEM0902_5) 
  
  # Second-order model scalar invariance - Denmark vs Germany
  scalSec <- '# Constrain loadings to equality 
              IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
              BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
              PF =~ c(x,x)*BF + c(x,x)*IF
                
            # Fix residual variances to 1
              PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
              PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
              PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
              PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
              PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
              PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB
                
            # Fix the factor variances to 1 in the reference group only
              IF ~~ c(NA, 1)*IF
              BF ~~ c(NA, 1)*BF
              PF ~~ c(NA, 1)*PF
            
            # Free factor means in reference group 
              IF ~ c(NA, 0)*1
              BF ~ c(NA, 0)*1  
              
            # Fix factor means for PF to zero for identification  
              PF ~ c(0, 0)*1'
  
  fitScalSec <- cfa(scalSec, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization ="theta",
                    group.equal = c("thresholds"), # Fix thresholds to equality
                    ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitScalSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitMetSec, fitScalSec))
  # -->  Scalar invariance not achieved: Difference in CFI > -0.01
  
  # Look for sources of non-invariance 
  lavTestScore(fitScalSec)
  partable(fitScalSec)
  # --> The results suggest that PANDEMIC_FATIGUE_2_MB is a source of non-invariance 
  
  # Second-order model partial scalar invariance - Denmark vs Germany
  parScalSec <- '# Constrain loadings to equality 
              IF =~ c(a,a)*PANDEMIC_FATIGUE_1_INF + c(b,b)*PANDEMIC_FATIGUE_3_INF + c(c,c)*PANDEMIC_FATIGUE_5_INF
              BF =~ c(d,d)*PANDEMIC_FATIGUE_2_MB + c(e,e)*PANDEMIC_FATIGUE_4_MB + c(f,f)*PANDEMIC_FATIGUE_6_MB
              PF =~ c(x,x)*BF + c(x,x)*IF
                
            # Fix residual variances to 1
              PANDEMIC_FATIGUE_1_INF ~~ c(1,1)*PANDEMIC_FATIGUE_1_INF
              PANDEMIC_FATIGUE_3_INF ~~ c(1,1)*PANDEMIC_FATIGUE_3_INF
              PANDEMIC_FATIGUE_5_INF ~~ c(1,1)*PANDEMIC_FATIGUE_5_INF
              PANDEMIC_FATIGUE_2_MB ~~ c(1,1)*PANDEMIC_FATIGUE_2_MB
              PANDEMIC_FATIGUE_4_MB ~~ c(1,1)*PANDEMIC_FATIGUE_4_MB
              PANDEMIC_FATIGUE_6_MB ~~ c(1,1)*PANDEMIC_FATIGUE_6_MB
                
            # Fix the factor variances to 1 in the reference group only
              IF ~~ c(NA, 1)*IF
              BF ~~ c(NA, 1)*BF
              PF ~~ c(NA, 1)*PF
            
            # Free factor means in reference group 
              IF ~ c(NA, 0)*1
              BF ~ c(NA, 0)*1  
              
            # Fix factor means for PF to zero for identification  
              PF ~ c(0, 0)*1'
  
  fitParScalSec <- cfa(parScalSec, data = MI, group = "COUNTRY", std.lv = TRUE, parameterization = "theta",
                       group.equal = c("thresholds"), # Fix thresholds to equality
                       group.partial = c("PANDEMIC_FATIGUE_2_MB|t1", "PANDEMIC_FATIGUE_2_MB|t2", # Free thresholds for PANDEMIC_FATIGUE_2_MB
                                         "PANDEMIC_FATIGUE_2_MB|t3", "PANDEMIC_FATIGUE_2_MB|t4", 
                                         "PANDEMIC_FATIGUE_2_MB|t5", "PANDEMIC_FATIGUE_2_MB|t6"),
                       ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                   "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                   "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(fitParScalSec, fit.measures = TRUE, standardized = TRUE) 
  summary(compareFit(fitMetSec, fitParScalSec))
  # -->  Partial scalar invariance achieved: Difference in CFI = -0.01
  
  rm(conInvSec, metSec, scalSec, parScalSec, fitConInvSec, fitMetSec, fitScalSec, fitParScalSec, MI)
  
  ###########################################################################################################################
  ## Scale Development and Validation - Confirmatory Factor Analysis for each Wave - Robust Maximum Likelihood Estimation ## 
  #########################################################################################################################
  
  # Extract relevant data 
  DEN_CFA <- subset(D, select = c("Wave", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  GER_CFA <- subset(G, select = c("Wave", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  # Define model 
  Model <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
          BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB
          PF =~   a*IF + a*BF'
  
  # Fit model wave 20 - Denmark 
  Fit_20_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 20), std.lv = TRUE, estimator = "MLM")
  summary(Fit_20_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 21 - Denmark 
  Fit_21_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 21), std.lv = TRUE, estimator = "MLM")
  summary(Fit_21_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 22 - Denmark 
  Fit_22_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 22), std.lv = TRUE, estimator = "MLM")
  summary(Fit_22_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 23 - Denmark 
  Fit_23_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 23), std.lv = TRUE, estimator = "MLM")
  summary(Fit_23_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 24 - Denmark 
  Fit_24_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 24), std.lv = TRUE, estimator = "MLM")
  summary(Fit_24_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 25 - Denmark 
  Fit_25_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 25), std.lv = TRUE, estimator = "MLM")
  summary(Fit_25_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 26 - Denmark 
  Fit_26_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 26), std.lv = TRUE, estimator = "MLM")
  summary(Fit_26_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 27 - Denmark 
  Fit_27_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 27), std.lv = TRUE, estimator = "MLM")
  summary(Fit_27_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 28 - Denmark 
  Fit_28_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 28), std.lv = TRUE, estimator = "MLM")
  summary(Fit_28_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 29 - Denmark 
  Fit_29_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 29), std.lv = TRUE, estimator = "MLM")
  summary(Fit_29_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 30 - Denmark 
  Fit_30_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 30), std.lv = TRUE, estimator = "MLM")
  summary(Fit_30_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 31 - Denmark 
  Fit_31_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 31), std.lv = TRUE, estimator = "MLM")
  summary(Fit_31_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 32 - Denmark 
  Fit_32_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 32), std.lv = TRUE, estimator = "MLM")
  summary(Fit_32_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 33 - Denmark 
  Fit_33_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 33), std.lv = TRUE, estimator = "MLM")
  summary(Fit_33_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 34 - Denmark 
  Fit_34_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 34), std.lv = TRUE, estimator = "MLM")
  summary(Fit_34_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 35 - Denmark 
  Fit_35_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 35), std.lv = TRUE, estimator = "MLM")
  summary(Fit_35_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 36 - Denmark 
  Fit_36_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 36), std.lv = TRUE, estimator = "MLM")
  summary(Fit_36_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 37 - Denmark 
  Fit_37_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 37), std.lv = TRUE, estimator = "MLM")
  summary(Fit_37_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 38 - Denmark 
  Fit_38_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 38), std.lv = TRUE, estimator = "MLM")
  summary(Fit_38_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 39 - Denmark 
  Fit_39_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 39), std.lv = TRUE, estimator = "MLM")
  summary(Fit_39_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 40 - Denmark 
  Fit_40_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 40), std.lv = TRUE, estimator = "MLM")
  summary(Fit_40_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 41 - Denmark 
  Fit_41_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 41), std.lv = TRUE, estimator = "MLM")
  summary(Fit_41_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 42 - Denmark 
  Fit_42_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 42), std.lv = TRUE, estimator = "MLM")
  summary(Fit_42_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 43 - Denmark 
  Fit_43_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 43), std.lv = TRUE, estimator = "MLM")
  summary(Fit_43_DK, fit.measures=TRUE, standardized=TRUE)
  
  
  # Fit model wave 24 - Germany 
  Fit_24_GER <- cfa(Model, data=subset(GER_CFA, Wave == 24), std.lv = TRUE, estimator = "MLM")
  summary(Fit_24_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 25 - Germany 
  Fit_25_GER <- cfa(Model, data=subset(GER_CFA, Wave == 25), std.lv = TRUE, estimator = "MLM")
  summary(Fit_25_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 26 - Germany 
  Fit_26_GER <- cfa(Model, data=subset(GER_CFA, Wave == 26), std.lv = TRUE, estimator = "MLM")
  summary(Fit_26_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 31 - Germany 
  Fit_31_GER <- cfa(Model, data=subset(GER_CFA, Wave == 31), std.lv = TRUE, estimator = "MLM")
  summary(Fit_31_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 32 - Germany 
  Fit_32_GER <- cfa(Model, data=subset(GER_CFA, Wave == 32), std.lv = TRUE, estimator = "MLM")
  summary(Fit_32_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 33 - Germany 
  Fit_33_GER <- cfa(Model, data=subset(GER_CFA, Wave == 33), std.lv = TRUE, estimator = "MLM")
  summary(Fit_33_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 34 - Germany 
  Fit_34_GER <- cfa(Model, data=subset(GER_CFA, Wave == 34), std.lv = TRUE, estimator = "MLM")
  summary(Fit_34_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 35 - Germany 
  Fit_35_GER <- cfa(Model, data=subset(GER_CFA, Wave == 35), std.lv = TRUE, estimator = "MLM")
  summary(Fit_35_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 37 - Germany 
  Fit_37_GER <- cfa(Model, data=subset(GER_CFA, Wave == 37), std.lv = TRUE, estimator = "MLM")
  summary(Fit_37_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 38 - Germany 
  Fit_38_GER <- cfa(Model, data=subset(GER_CFA, Wave == 38), std.lv = TRUE, estimator = "MLM")
  summary(Fit_38_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 39 - Germany 
  Fit_39_GER <- cfa(Model, data=subset(GER_CFA, Wave == 39), std.lv = TRUE, estimator = "MLM")
  summary(Fit_39_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 40 - Germany 
  Fit_40_GER <- cfa(Model, data=subset(GER_CFA, Wave == 40), std.lv = TRUE, estimator = "MLM")
  summary(Fit_40_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 41 - Germany 
  Fit_41_GER <- cfa(Model, data=subset(GER_CFA, Wave == 41), std.lv = TRUE, estimator = "MLM")
  summary(Fit_41_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 42 - Germany 
  Fit_42_GER <- cfa(Model, data=subset(GER_CFA, Wave == 42), std.lv = TRUE, estimator = "MLM")
  summary(Fit_42_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 43 - Germany 
  Fit_43_GER <- cfa(Model, data=subset(GER_CFA, Wave == 43), std.lv = TRUE, estimator = "MLM")
  summary(Fit_43_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 44 - Germany 
  Fit_44_GER <- cfa(Model, data=subset(GER_CFA, Wave == 44), std.lv = TRUE, estimator = "MLM")
  summary(Fit_44_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 49 - Germany 
  Fit_49_GER <- cfa(Model, data=subset(GER_CFA, Wave == 49), std.lv = TRUE, estimator = "MLM")
  summary(Fit_49_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 51 - Germany 
  Fit_51_GER <- cfa(Model, data=subset(GER_CFA, Wave == 51), std.lv = TRUE, estimator = "MLM")
  summary(Fit_51_GER, fit.measures=TRUE, standardized=TRUE)
  rm("Fit_20_DK", "Fit_21_DK", "Fit_22_DK", "Fit_23_DK", "Fit_24_DK", "Fit_24_GER", "Fit_25_DK", "Fit_25_GER", 
     "Fit_26_DK", "Fit_26_GER", "Fit_27_DK", "Fit_28_DK", "Fit_29_DK", "Fit_30_DK", "Fit_31_DK", "Fit_31_GER", "Fit_32_DK", 
     "Fit_32_GER", "Fit_33_DK", "Fit_33_GER", "Fit_34_DK", "Fit_34_GER", "Fit_35_DK", "Fit_35_GER", "Fit_36_DK", "Fit_37_DK",
     "Fit_37_GER", "Fit_38_DK", "Fit_38_GER", "Fit_39_DK", "Fit_39_GER", "Fit_40_DK", "Fit_40_GER", "Fit_41_DK", "Fit_41_GER",
     "Fit_42_DK", "Fit_42_GER", "Fit_43_DK", "Fit_43_GER", "Fit_44_GER", "Fit_49_GER", "Fit_51_GER", "DEN_CFA", "GER_CFA", "Model")
  
  ##########################################################################################################################################
  ## Scale Development and Validation - Confirmatory Factor Analysis for each Wave - Robust Diagonally Weighted Least Squares Estimation ## 
  ########################################################################################################################################
  
  # Extract relevant data 
  DEN_CFA <- subset(D, select = c("Wave", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  GER_CFA <- subset(G, select = c("Wave", "PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                                  "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  
  # Define model 
  Model <- 'IF =~ PANDEMIC_FATIGUE_1_INF + PANDEMIC_FATIGUE_3_INF + PANDEMIC_FATIGUE_5_INF
          BF =~ PANDEMIC_FATIGUE_2_MB + PANDEMIC_FATIGUE_4_MB + PANDEMIC_FATIGUE_6_MB
          PF =~   a*IF + a*BF'
  
  # Fit model wave 20 - Denmark 
  Fit_20_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 20), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_20_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 21 - Denmark 
  Fit_21_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 21), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_21_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 22 - Denmark 
  Fit_22_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 22), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_22_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 23 - Denmark 
  Fit_23_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 23), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_23_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 24 - Denmark 
  Fit_24_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 24), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_24_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 25 - Denmark 
  Fit_25_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 25), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_25_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 26 - Denmark 
  Fit_26_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 26), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_26_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 27 - Denmark 
  Fit_27_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 27), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_27_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 28 - Denmark 
  Fit_28_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 28), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_28_DK, fit.measures=TRUE, standardized=TRUE)
  
  
  # Fit model wave 29 - Denmark 
  Fit_29_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 29), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_29_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 30 - Denmark 
  Fit_30_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 30), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_30_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 31 - Denmark 
  Fit_31_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 31), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_31_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 32 - Denmark 
  Fit_32_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 32), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_32_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 33 - Denmark 
  Fit_33_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 33), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_33_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 34 - Denmark 
  Fit_34_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 34), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_34_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 35 - Denmark 
  Fit_35_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 35), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_35_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 36 - Denmark 
  Fit_36_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 36), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_36_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 37 - Denmark 
  Fit_37_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 37), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_37_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 38 - Denmark 
  Fit_38_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 38), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_38_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 39 - Denmark 
  Fit_39_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 39), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_39_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 40 - Denmark 
  Fit_40_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 40), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_40_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 41 - Denmark 
  Fit_41_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 41), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_41_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 42 - Denmark 
  Fit_42_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 42), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_42_DK, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 43 - Denmark 
  Fit_43_DK <- cfa(Model, data=subset(DEN_CFA, Wave == 43), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                       "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                       "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_43_DK, fit.measures=TRUE, standardized=TRUE)
  
  
  # Fit model wave 24 - Germany
  Fit_24_GER <- cfa(Model, data=subset(GER_CFA, Wave == 24), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_24_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 25 - Germany
  Fit_25_GER <- cfa(Model, data=subset(GER_CFA, Wave == 25), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_25_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 26 - Germany
  Fit_26_GER <- cfa(Model, data=subset(GER_CFA, Wave == 26), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_26_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 31 - Germany
  Fit_31_GER <- cfa(Model, data=subset(GER_CFA, Wave == 31), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_31_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 32 - Germany
  Fit_32_GER <- cfa(Model, data=subset(GER_CFA, Wave == 32), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_32_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 33 - Germany
  Fit_33_GER <- cfa(Model, data=subset(GER_CFA, Wave == 33), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_33_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 34 - Germany
  Fit_34_GER <- cfa(Model, data=subset(GER_CFA, Wave == 34), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_34_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 35 - Germany
  Fit_35_GER <- cfa(Model, data=subset(GER_CFA, Wave == 35), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_35_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 37 - Germany
  Fit_37_GER <- cfa(Model, data=subset(GER_CFA, Wave == 37), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_37_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 38 - Germany
  Fit_38_GER <- cfa(Model, data=subset(GER_CFA, Wave == 38), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_38_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 39 - Germany
  Fit_39_GER <- cfa(Model, data=subset(GER_CFA, Wave == 39), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_39_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 40 - Germany
  Fit_40_GER <- cfa(Model, data=subset(GER_CFA, Wave == 40), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_40_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 41 - Germany
  Fit_41_GER <- cfa(Model, data=subset(GER_CFA, Wave == 41), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_41_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 42 - Germany
  Fit_42_GER <- cfa(Model, data=subset(GER_CFA, Wave == 42), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_42_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 43 - Germany
  Fit_43_GER <- cfa(Model, data=subset(GER_CFA, Wave == 43), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_43_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 44 - Germany
  Fit_44_GER <- cfa(Model, data=subset(GER_CFA, Wave == 44), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_44_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 49 - Germany
  Fit_49_GER <- cfa(Model, data=subset(GER_CFA, Wave == 49), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_49_GER, fit.measures=TRUE, standardized=TRUE)
  
  # Fit model wave 51 - Germany
  Fit_51_GER <- cfa(Model, data=subset(GER_CFA, Wave == 51), std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF",
                                                                                        "PANDEMIC_FATIGUE_5_INF", "PANDEMIC_FATIGUE_2_MB", 
                                                                                        "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB"))
  summary(Fit_51_GER, fit.measures=TRUE, standardized=TRUE)
  # 获取所有匹配的变量名
  cfa_vars <- ls(pattern = "^Fit_|^DEN$|^GER$")
  cfa_vars <- gsub("DEN", "GER", cfa_vars)

  library(forestplot)
  # 确保只获取真正的CFA模型对象
  cfa_vars <- ls(pattern = "^Fit_\\d+_(DK|GER)$")
  cfa_models <- sapply(cfa_vars, function(x) inherits(get(x), "lavaan"))
  cfa_vars <- cfa_vars[cfa_models]
  cfa_vars

  fit_results <- data.frame(
    Model = character(),
    Country = character(),
    Wave = numeric(),
    RMSEA = numeric(),
    RMSEA_lower = numeric(),
    RMSEA_upper = numeric(),
    Chisq_df = numeric(),
    CFI = numeric(),
    TLI = numeric(),
    SRMR = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 3. 安全地提取拟合指标
  for (model_name in cfa_vars) {
    model <- get(model_name)
    
    if (!inherits(model, "lavaan")) next  # 跳过非lavaan对象
    
    tryCatch({
      fit <- fitMeasures(model)
      
      fit_results <- rbind(fit_results, data.frame(
        Model = model_name,
        Country = strsplit(model_name, "_")[[1]][3],
        Wave = as.numeric(strsplit(model_name, "_")[[1]][2]),
        RMSEA = fit["rmsea"],
        RMSEA_lower = fit["rmsea.ci.lower"],
        RMSEA_upper = fit["rmsea.ci.upper"],
        Chisq_df = fit["chisq"]/fit["df"],
        CFI = fit["cfi"],
        TLI = fit["tli"],
        SRMR = fit["srmr"],
        AIC = fit["aic"],
        BIC = fit["bic"]
      ))
    }, error = function(e) {
      message("Error processing ", model_name, ": ", e$message)
    })
  }
  
  # 4. 检查结果
  if (nrow(fit_results) > 0) {
    # 按Wave排序
    fit_results <- fit_results[order(fit_results$Wave, fit_results$Country), ]
    
    # 查看结果
    print(fit_results)
    install.packages("writexl")
    library(writexl)
    write_xlsx(fit_results, "fit_results.xlsx")
    library(forestplot)
    # 调整后的森林图代码
    forestplot(
      labeltext = fit_results$Model,
      mean = fit_results$RMSEA,
      lower = fit_results$RMSEA_lower,
      upper = fit_results$RMSEA_upper,
      is.summary = FALSE,
      graphwidth = unit(0.6, "npc"),  # 增加图形宽度占比
      xlab = "RMSEA (95% CI)",
      zero = 0.08,
      col = fpColors(
        box = ifelse(fit_results$Country == "DK", "blue", "green"),
        lines = ifelse(fit_results$Country == "DK", "blue", "green")
      ),
      boxsize = 0.3,  # 调整点的大小
      txt_gp = fpTxtGp(
        label = gpar(cex = 0.8),      # 纵坐标标签字号
        ticks = gpar(cex = 0.7),      # 刻度标签字号
        xlab = gpar(cex = 0.9)        # x轴标题字号
      ),
      lineheight = unit(0.8, "cm"),   # 行间距
      clip = c(0, 0.15),              # 限制x轴范围
      xticks = seq(0, 0.15, by = 0.02) # 自定义x轴刻度
    )
    
    library(lavaan)
    library(ggplot2)
    library(tidyr)
    
 
    # 为每个模型计算bootstrap置信区间
    compute_ci <- function(model, n_bootstrap = 1000) {
      bootstrap_fits <- bootstrapLavaan(model, 
                                        R = n_bootstrap,
                                        FUN = function(x) {
                                          c(
                                            rmsea = fitMeasures(x, "rmsea"),
                                            chisq_df = fitMeasures(x, "chisq")/fitMeasures(x, "df"),
                                            cfi = fitMeasures(x, "cfi"),
                                            tli = fitMeasures(x, "tli"),
                                            srmr = fitMeasures(x, "srmr"),
                                            aic = fitMeasures(x, "aic"),
                                            bic = fitMeasures(x, "bic")
                                          )
                                        })
      
      # 计算95%置信区间，处理可能的NA值
      ci <- apply(bootstrap_fits, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
      colnames(ci) <- c("RMSEA", "Chisq_df", "CFI", "TLI", "SRMR", "AIC", "BIC")
      return(as.data.frame(t(ci)))
    }

    # 创建存储所有结果的数据框
    all_ci_results <- list()
    
    for (model_name in cfa_vars) {
      model <- get(model_name)
      ci <- compute_ci(model)
      ci$Model <- model_name
      ci$Country <- strsplit(model_name, "_")[[1]][3]
      ci$Wave <- as.numeric(strsplit(model_name, "_")[[1]][2])
      ci$Metric <- rownames(ci)
      all_ci_results[[model_name]] <- ci
    }
    
    # 合并所有结果
    CI_final_results <- do.call(rbind, all_ci_results)
    rownames(CI_final_results) <- NULL
    names(CI_final_results)[1:3] <- c("Lower", "Median", "Upper")
    
    # 查看结果
    head(CI_final_results)
    
    # 为每个指标创建单独的可视化
    plot_metric_ci <- function(metric, data = CI_final_results) {
      df <- data[data$Metric == metric, ]
      
      ggplot(df, aes(x = factor(Wave), y = Median, color = Country)) +
        geom_point(position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = Lower, ymax = Upper),
                      position = position_dodge(width = 0.5), width = 0.2) +
        labs(title = paste(metric, "with 95% CI across Waves"),
             x = "Wave", y = metric) +
        theme_minimal() +
        scale_color_manual(values = c("GER" = "green")) +
        geom_hline(yintercept = switch(metric,
                                       "RMSEA" = 0.08,
                                       "CFI" = 0.90,
                                       "TLI" = 0.90,
                                       "SRMR" = 0.08,
                                       0), 
                   linetype = "dashed", color = "red")
    }
    
    # 生成所有指标的图形
    metrics <- c("RMSEA", "Chisq_df", "CFI", "TLI", "SRMR")
    plots <- lapply(metrics, plot_metric_ci)
    
    # 使用patchwork包组合图形
    library(patchwork)
    combined_plot <- wrap_plots(plots, ncol = 2) + 
      plot_annotation(title = "Model Fit Indices with 95% Confidence Intervals")
    print(combined_plot)
    
    # 保存图形
    ggsave("fit_indices_ci.png", combined_plot, width = 12, height = 10, dpi = 300)
    
    ggplot(df, aes(x = factor(Wave), y = Median, color = Country)) +
      geom_point(position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper),
                    position = position_dodge(width = 0.5), width = 0.2) +
      labs(title = paste(metric, "with 95% CI across Waves"),
           x = "Wave", y = metric) +
      theme_minimal() +
      scale_color_manual(values = c("DK" = "blue", "GER" = "green")) +
      geom_hline(yintercept = switch(metric,
                                     "RMSEA" = 0.08,
                                     "CFI" = 0.90,
                                     "TLI" = 0.90,
                                     "SRMR" = 0.08,
                                     0), 
                 linetype = "dashed", color = "red")
  }
  
  # 生成所有指标的图形
  metrics <- c("RMSEA", "Chisq_df", "CFI", "TLI", "SRMR")
  plots <- lapply(metrics, plot_metric_ci)
  
  # 使用patchwork包组合图形
  library(patchwork)
  combined_plot <- wrap_plots(plots, ncol = 2) + 
    plot_annotation(title = "Model Fit Indices with 95% Confidence Intervals")
  print(combined_plot)
  
  # 保存图形
  ggsave("fit_indices_ci.png", combined_plot, width = 12, height = 10, dpi = 300)
  
  
  rm("Fit_20_DK", "Fit_21_DK", "Fit_22_DK", "Fit_23_DK", "Fit_24_DK", "Fit_24_GER", "Fit_25_DK", "Fit_25_GER", 
     "Fit_26_DK", "Fit_26_GER", "Fit_27_DK", "Fit_28_DK", "Fit_29_DK", "Fit_30_DK", "Fit_31_DK", "Fit_31_GER", "Fit_32_DK", 
     "Fit_32_GER", "Fit_33_DK", "Fit_33_GER", "Fit_34_DK", "Fit_34_GER", "Fit_35_DK", "Fit_35_GER", "Fit_36_DK", "Fit_37_DK",
     "Fit_37_GER", "Fit_38_DK", "Fit_38_GER", "Fit_39_DK", "Fit_39_GER", "Fit_40_DK", "Fit_40_GER", "Fit_41_DK", "Fit_41_GER",
     "Fit_42_DK", "Fit_42_GER", "Fit_43_DK", "Fit_43_GER", "Fit_44_GER", "Fit_49_GER", "Fit_51_GER", "DEN_CFA", "GER_CFA", "Model")
  

  ############################################################
  ## Correlates of Pandemic Fatigue - Cross-Sectional Data ##
  ##########################################################
  
  # Correlation matrix:  
  # Extract relevant variables 
  GER <-subset(G, select = c("PANDEMIC_FATIGUE", "INFORMATION_FATIGUE", "BEHAVIORAL_FATIGUE", "Wave", "GENDER", "AGE", "EDUCATION", "EMPLOYMENT", "CHRONIC", "COGNITIVE_RISK", "AFFECTIVE_RISK",
                             "TRUST", "WORRIES","new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING",
                             "FREQ_INFO"))
  
  DEN <- subset(D, GENDER != "Other", select = c("PANDEMIC_FATIGUE", "INFORMATION_FATIGUE", "BEHAVIORAL_FATIGUE", "Wave", "GENDER", "AGE", "EDUCATION", "EMPLOYMENT", "CHRONIC", "COGNITIVE_RISK",
                                                 "AFFECTIVE_RISK", "TRUST", "WORRIES",  "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million","reproduction_rate", "stringency_index",
                                                 "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX", "AG", "CO", "OP", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING", "INFO_SEEK"))
  
  # Prepare data 
  DEN <- subset(DEN, GENDER != "Other") # Drop gender other 
  DEN$GENDER <- as.character(DEN$GENDER)
  DEN$GENDER[DEN$GENDER == "Male"] <- 1 
  DEN$GENDER[DEN$GENDER == "Female"] <- 0
  DEN$GENDER <- as.numeric(DEN$GENDER)
  
  DEN$EDUCATION <- as.character(DEN$EDUCATION)
  DEN$EDUCATION[DEN$EDUCATION == "> 10 years"] <- 1 
  DEN$EDUCATION[DEN$EDUCATION == "< 10 years"] <- 0
  DEN$EDUCATION  <- as.numeric(DEN$EDUCATION)
  
  DEN$EMPLOYMENT <- as.character(DEN$EMPLOYMENT)
  DEN$EMPLOYMENT[DEN$EMPLOYMENT == "Unemployed"] <- 1 
  DEN$EMPLOYMENT[DEN$EMPLOYMENT == "Employed"] <- 0
  DEN$EMPLOYMENT  <- as.numeric(DEN$EMPLOYMENT)
  
  DEN <- subset(DEN, CHRONIC != "Don´t know") # Drop chronic don't know 
  DEN$CHRONIC <- as.character(DEN$CHRONIC) 
  DEN$CHRONIC[DEN$CHRONIC == "No"] <- 1 
  DEN$CHRONIC[DEN$CHRONIC == "Yes"] <- 0
  DEN$CHRONIC  <- as.numeric(DEN$CHRONIC)
  
  GER$GENDER <- as.character(GER$GENDER)
  GER$GENDER[GER$GENDER == "Male"] <- 1 
  GER$GENDER[GER$GENDER == "Female"] <- 0
  GER$GENDER <- as.numeric(GER$GENDER)
  
  GER$EDUCATION <- as.character(GER$EDUCATION)
  GER$EDUCATION[GER$EDUCATION == "> 10 years"] <- 1 
  GER$EDUCATION[GER$EDUCATION == "< 10 years"] <- 0
  GER$EDUCATION  <- as.numeric(GER$EDUCATION)
  
  GER$EMPLOYMENT <- as.character(GER$EMPLOYMENT)
  GER$EMPLOYMENT[GER$EMPLOYMENT == "Unemployed"] <- 1 
  GER$EMPLOYMENT[GER$EMPLOYMENT == "Employed"] <- 0
  GER$EMPLOYMENT  <- as.numeric(GER$EDUCATION)
  
  GER<- subset(GER, CHRONIC != "Don´t know") # Drop chronic don't know 
  GER$CHRONIC <- as.character(GER$CHRONIC) 
  GER$CHRONIC[GER$CHRONIC == "No"] <- 1 
  GER$CHRONIC[GER$CHRONIC == "Yes"] <- 0
  GER$CHRONIC  <- as.numeric(GER$CHRONIC)
  
  # Rename data 
  names(DEN)[names(DEN) == "PANDEMIC_FATIGUE"] <- "Pandemic fatigue"
  names(DEN)[names(DEN) == "INFORMATION_FATIGUE"] <- "Information fatigue"
  names(DEN)[names(DEN) == "BEHAVIORAL_FATIGUE"] <- "Behavioral fatigue"
  names(DEN)[names(DEN) == "Wave"] <- "Time (survey wave)"
  names(DEN)[names(DEN) == "GENDER"] <- "Gender"
  names(DEN)[names(DEN) == "AGE"] <- "Age"
  names(DEN)[names(DEN) == "EDUCATION"] <- "Education"
  names(DEN)[names(DEN) == "EMPLOYMENT"] <- "Employment"
  names(DEN)[names(DEN) == "CHRONIC"] <- "Chronic disease"
  names(DEN)[names(DEN) == "COGNITIVE_RISK"] <- "Cognitive risk perceptions"
  names(DEN)[names(DEN) == "AFFECTIVE_RISK"] <- "Affective risk perceptions"
  names(DEN)[names(DEN) == "TRUST"] <- "Institutional trust"
  names(DEN)[names(DEN) == "WORRIES"] <- "Worries - personal/societal"
  names(DEN)[names(DEN) == "new_cases_smoothed_per_million"] <- "New cases per million"
  names(DEN)[names(DEN) == "new_deaths_smoothed_per_million"] <- "New deaths per million"
  names(DEN)[names(DEN) == "reproduction_rate"] <- "Reproduction rate"
  names(DEN)[names(DEN) == "stringency_index"] <- "Policy stringency index"
  names(DEN)[names(DEN) == "OPTIMISTIC"] <- "Optimism about the future"
  names(DEN)[names(DEN) == "NEGATIVE_AFFECT"] <- "Negative affect"
  names(DEN)[names(DEN) == "EMPATHY"] <- "Empathy - most vulnerable"
  names(DEN)[names(DEN) == "HH"] <- "Honesty-humility"
  names(DEN)[names(DEN) == "EM"] <- "Emotionality"
  names(DEN)[names(DEN) == "EX"] <- "Extraversion"
  names(DEN)[names(DEN) == "AG"] <- "Agreeableness vs. anger"
  names(DEN)[names(DEN) == "CO"] <- "Conscientiousness"
  names(DEN)[names(DEN) == "OP"] <- "Openness to experience"
  names(DEN)[names(DEN) == "PHYSICAL_DISTANCING"] <- "Physical distancing"
  names(DEN)[names(DEN) == "HYGIENE"] <- "Hygiene"
  names(DEN)[names(DEN) == "MASK_WEARING"] <- "Mask wearing"
  names(DEN)[names(DEN) == "INFO_SEEK"] <- "Information seeking"
  
  names(GER)[names(GER) == "PANDEMIC_FATIGUE"] <- "Pandemic fatigue"
  names(GER)[names(GER) == "INFORMATION_FATIGUE"] <- "Information fatigue"
  names(GER)[names(GER) == "BEHAVIORAL_FATIGUE"] <- "Behavioral fatigue"
  names(GER)[names(GER) == "Wave"] <- "Time (survey wave)"
  names(GER)[names(GER) == "GENDER"] <- "Gender"
  names(GER)[names(GER) == "AGE"] <- "Age"
  names(GER)[names(GER) == "EDUCATION"] <- "Education"
  names(GER)[names(GER) == "EMPLOYMENT"] <- "Employment"
  names(GER)[names(GER) == "CHRONIC"] <- "Chronic disease"
  names(GER)[names(GER) == "COGNITIVE_RISK"] <- "Cognitive risk perceptions"
  names(GER)[names(GER) == "AFFECTIVE_RISK"] <- "Affective risk perceptions"
  names(GER)[names(GER) == "TRUST"] <- "Institutional trust"
  names(GER)[names(GER) == "WORRIES"] <- "Worries - personal/societal"
  names(GER)[names(GER) == "new_cases_smoothed_per_million"] <- "New cases per million"
  names(GER)[names(GER) == "new_deaths_smoothed_per_million"] <- "New deaths per million"
  names(GER)[names(GER) == "reproduction_rate"] <- "Reproduction rate"
  names(GER)[names(GER) == "stringency_index"] <- "Policy stringency index"
  names(GER)[names(GER) == "PHYSICAL_DISTANCING"] <- "Physical distancing"
  names(GER)[names(GER) == "HYGIENE"] <- "Hygiene"
  names(GER)[names(GER) == "MASK_WEARING"] <- "Mask wearing"
  names(GER)[names(GER) == "FREQ_INFO"] <- "Information seeking"
  install.packages("C:/Users/Administrator/Downloads/Hmisc_5.2-3.zip",
                   repos = NULL,
                   type = "win.binary",
                   dependencies = c("Depends", "Suggests"))
  library(Hmisc)
  # Pearsons r and p-values 
  C_DEN <- corr.test(DEN, method = "pearson")
  R_DEN <- C_DEN$r
  P_DEN <- C_DEN$p
  # 计算相关性矩阵及 p 值
  C_GER <- rcorr(as.matrix(GER), type = "pearson") 
  #C_GER <- corr.test(GER, method = "pearson")
  R_GER <- C_GER$r
  P_GER <- C_GER$p
  trace(corrplot, edit=TRUE)
  # To fully reproduce the correlogram the corrplot function needs to be customized as follows. 
  # 
  # Run: trace(corrplot, edit=TRUE) 
  #
  # Then replace on line 443: 
  #
  #     place_points = function(sig.locs, point) {
  #      text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
  #       labels = point, col = pch.col, cex = pch.cex, 
  #       lwd = 2)
  #
  #  with:
  #
  #    place_points = function(sig.locs, point) {
  #      text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
  #           labels = point, col = pch.col, cex = pch.cex, 
  #           lwd = 2)
  #
  # and then hit the "Save" button.
  place_points = function(sig.locs, point) {
      text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
      labels = point, col = pch.col, cex = pch.cex, 
      lwd = 2)
  }
  thresholds = FALSE 
  library(psych)
  library(corrplot)
  # Plot correlograms 
   corrplot(R_DEN, method="color", type = "lower" , addCoef.col = "black", diag = FALSE,
           p.mat = P_DEN, insig = "label_sig", sig.level = c(0.001, 0.01, 0.05), number.cex = .4,
           cl.ratio = 0.01,
           tl.col = "black", tl.cex = 0.4, number.digits = 2, na.label = "NA", pch.cex = 0.1, title = "\nCorrelogram - Denmark")
  
   corrplot(R_GER, method="color", type = "lower" , addCoef.col = "black", diag = FALSE,
           p.mat = P_GER, insig = "label_sig", sig.level = c(0.001, 0.01, 0.05), number.cex = .4,
           cl.ratio = 0.05,
           tl.col = "black", tl.cex = 0.5, number.digits = 2, pch.cex = 1, na.label = "NA", title = "\nCorrelogram - Germany")
  
  # Export plots to pdf 
  pdf("Figure S2 and S3 - Correlograms for Germany and Denmark.pdf", height = 15, width = 15)
  rm(GER, DEN, C_DEN, C_GER, P_DEN, P_GER, R_DEN, R_GER)
  
  # Regression analyses:  
  
  # Extract relevant variables 
  GER <-subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "PANDEMIC_FATIGUE", "Wave", "AGE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES",
                             "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "PANDEMIC_FATIGUE", "Wave", "AGE",  "COGNITIVE_RISK",
                                                 "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX", "AG", "CO", "OP",
                                                 "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index"))
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  
  GER[6:15] <- scale(GER[6:15])
  DEN[6:24] <- scale(DEN[6:24])
  
  # Create Wave^2 variable 
  GER$Wave2 <- GER$Wave^2
  DEN$Wave2 <- DEN$Wave^2
  
  # Model 1 in Germany - Pandemic fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors
  PF_GER_1 <-  lm(PANDEMIC_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Pandemic fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors
  PF_DEN_1<-  lm(PANDEMIC_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                   COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Pandemic fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors + HEXACO and Additional Emotions
  PF_DEN_2 <-  lm(PANDEMIC_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index +
                    OPTIMISTIC + NEGATIVE_AFFECT + EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  library(jtools)
  
  # Print results
  export_summs(PF_GER_1, PF_DEN_1, PF_DEN_2, model.names = c("Pandemic fatigue (Model 1) - GER", "Pandemic fatigue (Model 1) - DEN", "Pandemic fatigue (Model 2) - DEN"), error_format = "[{conf.low}, {conf.high}]")
 library(styler)
  APAStyler(modelTest(PF_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(PF_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(PF_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plots
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PF_DEN_2, PF_DEN_1, PF_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Pandemic fatigue - Model 2\nDenmark (n = 15,891)",  "Pandemic fatigue - Model 1\nDenmark (n = 15,891)", " Pandemic fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure 2 - OLS regressions predicting pandemic fatigue in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PF_DEN_2, PF_DEN_1, PF_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Pandemic fatigue - Model 2\nDenmark (n = 15,891)",  "Pandemic fatigue - Model 1\nDenmark (n = 15,891)", " Pandemic fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(PF_DEN_1, PF_DEN_2, PF_GER_1, DEN, GER)
 #######################################################################
  ## Development of Recommended Health Protective Behaviors Over Time ##
  #####################################################################
  
  # Extract relevant data 
  DEN <- subset(D, select = c("Wave", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING", "INFO_SEEK"))
  GER <- subset(G, select = c("Wave", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING", "FREQ_INFO"))

  # Scale and standardize cross-sectional data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  DEN$Wave <- scale(DEN$Wave)
  GER$Wave <- scale(GER$Wave)
  
  
  # Regressions Denmark 
  PD_DEN <-lm(PHYSICAL_DISTANCING ~ Wave, data = DEN)
  summ(PD_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(PD_DEN), digits = 3) # Standardized effect sizes
  
  H_DEN <- lm(HYGIENE ~ Wave, data = DEN)
  summ(H_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(H_DEN), digits = 3) # Standardized effect sizes
  
  MW_DEN <- lm(MASK_WEARING ~ Wave, data = DEN)
  summ(MW_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(MW_DEN), digits = 3) # Standardized effect sizes
  
  IS_DEN <- lm(INFO_SEEK ~ Wave, data = DEN)
  summ(IS_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(IS_DEN), digits = 3) # Standardized effect sizes
  
  # Regressions Germany 
  PD_GER <-lm(PHYSICAL_DISTANCING ~ Wave, data = GER)
  summ(PD_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(PD_GER), digits = 3) # Standardized effect sizes
  
  H_GER <- lm(HYGIENE ~ Wave, data = GER)
  summ(H_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(H_GER), digits = 3) # Standardized effect sizes
  
  MW_GER <- lm(MASK_WEARING ~ Wave, data = GER)
  summ(MW_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(MW_GER), digits = 3) # Standardized effect sizes
  
  IS_GER <- lm(FREQ_INFO ~ Wave, data = GER)
  summ(IS_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(IS_GER), digits = 3) # Standardized effect sizes
  

  # Plot development over time Denmark 
  DEN <- subset(D, select = c("Wave", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING", "INFO_SEEK"))
  DEN_N <- DEN %>%
    count(Wave)
  DEN <- DEN %>%
    group_by(Wave) %>%
    summarise(PHYSICAL_DISTANCING = mean(PHYSICAL_DISTANCING, na.rm = TRUE),
              HYGIENE = mean(HYGIENE, na.rm = TRUE),
              MASK_WEARING = mean(MASK_WEARING, na.rm = TRUE),
              INFO_SEEK = mean(INFO_SEEK, na.rm = TRUE))
  DEN <- merge(DEN, DEN_N, by = "Wave")
  rm(DEN_N)
  DEN$Wave <- car::recode(DEN$Wave, "'19'='2020-10-19'; '20'='2020-11-02'; '21'='2020-11-16'; '22'='2020-11-30'; '23'='2020-12-14'; '24'='2020-12-28'; 
                                   '25'='2021-01-11'; '26'='2021-01-25 '; '27'='2021-02-08'; '28'='2021-02-22'; '29'='2021-03-08'; '30'='2021-03-22';
                                   '31'='2021-04-06'; '32'='2021-04-20'; '33'='2021-05-03'; '34'='2021-05-17'; '35'='2021-05-31'; '36'='2021-06-14'; 
                                   '37'='2021-06-28'; '38'='2021-07-12'; '39'='2021-07-26'; '40'='2021-08-09'; '41'='2021-08-23'; '42'='2021-09-06'; 
                                   '43'='2021-09-20'")
  DEN$Wave <- as.Date(DEN$Wave)
  a <- ggplot(DEN, aes(x=Wave, y=PHYSICAL_DISTANCING, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#F8766D", size = .5, alpha = .7)+
    geom_point(color = "#F8766D", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Physical distancing")+
    labs(title = "Physical distancing")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  b <- ggplot(DEN, aes(x=Wave, y=HYGIENE, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#F8766D", size = .5, alpha = .7)+
    geom_point(color = "#F8766D", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Hygiene")+
    labs(title = "Hygiene")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  c <- ggplot(DEN, aes(x=Wave, y=MASK_WEARING, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#F8766D", size = .5, alpha = .7)+
    geom_point(color = "#F8766D", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Mask wearing")+
    labs(title = "Mask wearing")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  d <- ggplot(DEN, aes(x=Wave, y=INFO_SEEK, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#F8766D", size = .5, alpha = .7)+
    geom_point(color = "#F8766D", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Information seeking")+
    labs(title = "Information seeking")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  ggarrange(a, b, c, d, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
  rm(DEN)
  
  # Plot development over time Germany
  GER <- subset(G, select = c("Wave", "PHYSICAL_DISTANCING", "HYGIENE", "MASK_WEARING", "FREQ_INFO"))
  GER_N <- GER %>%
    count(Wave)
  GER <- GER %>%
    group_by(Wave) %>%
    summarise(PHYSICAL_DISTANCING = mean(PHYSICAL_DISTANCING, na.rm = TRUE),
              HYGIENE = mean(HYGIENE, na.rm = TRUE),
              MASK_WEARING = mean(MASK_WEARING, na.rm = TRUE),
              FREQ_INFO = mean(FREQ_INFO, na.rm = TRUE))
  GER <- merge(GER, GER_N, by = "Wave")
  rm(GER_N)
  GER$Wave <- car::recode(GER$Wave, "'24'='2020-10-27'; '25'='2020-11-03'; '26'='2020-11-10'; '31'='2020-12-22'; '32'='2020-12-29'; '33'='2021-01-12';
                                  '34'='2021-01-26'; '35'='2021-02-09'; '37'='2021-02-23'; '38'='2021-03-09'; '39'='2021-03-23';'40'='2021-04-06';
                                  '41'='2021-04-20'; '42'='2021-05-04'; '43'='2021-05-18'; '44'='2021-06-01 '; '49'='2021-08-10'; '51'='2021-09-07'")
  GER$Wave <- as.Date(GER$Wave)
  e <- ggplot(GER, aes(x=Wave, y=PHYSICAL_DISTANCING, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#00BFC4", size = .5, alpha = .7)+
    geom_point(color = "#00BFC4", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Physical distancing")+
    labs(title = "Physical distancing")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  f <- ggplot(GER, aes(x=Wave, y=HYGIENE, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#00BFC4", size = .5, alpha = .7)+
    geom_point(color = "#00BFC4", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Hygiene")+
    labs(title = "Hygiene")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  g <- ggplot(GER, aes(x=Wave, y=MASK_WEARING, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#00BFC4", size = .5, alpha = .7)+
    geom_point(color = "#00BFC4", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Mask wearing")+
    labs(title = "Mask wearing")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  
  h <- ggplot(GER, aes(x=Wave, y=FREQ_INFO, size = n)) +
    theme_classic(base_size = 15)+
    geom_line(color = "#00BFC4", size = .5, alpha = .7)+
    geom_point(color = "#00BFC4", alpha = .7)+
    geom_smooth(method = "lm", size = 1)+
    coord_cartesian(ylim = c(1, 7))+
    scale_y_continuous(breaks = seq(1,7, by=1))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y",limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")))+
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "bottom")+ 
    xlab("")+
    ylab("Information seeking")+
    labs(title = "Information seeking")+
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 14))+
    labs(size = "n  =")
  ggarrange(e, f, g, h, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
  rm(GER)
  

  # Export plots to pdf 
  pdf("Figure S8 - Development of health-protective behaviors over time in Germany and Denmark.pdf",  width = 15, height = 5)
  ggarrange(a, b, c, d, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "", "", ""))
  ggarrange(e, f, g, h, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("B", "", "", ""))
  dev.off()
  rm(a, b, c, d, e, f, g, h)
  
  #######################################################################################
  ## The Link between Pandemic Fatigue and Physical Distancing - Cross Sectional Data ##
  #####################################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "PHYSICAL_DISTANCING", "Wave",
                              "AGE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "PANDEMIC_FATIGUE",
                              "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate",
                              "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "PHYSICAL_DISTANCING", "Wave", "AGE", "COGNITIVE_RISK", 
                                                 "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX",
                                                 "AG", "CO", "OP", "PANDEMIC_FATIGUE", "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                                                 "reproduction_rate", "stringency_index"))
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:16] <- scale(GER[6:16])
  DEN[6:25] <- scale(DEN[6:25])
  
  # Regression analysis controlling only for time - Germany 
  PD_BI_GER <-  lm(PHYSICAL_DISTANCING ~ Wave + PANDEMIC_FATIGUE, data = GER)
  summ(PD_BI_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(PD_BI_GER), digits = 3) # Standardized effect sizes
  
  # Regression analysis controlling only for time - Denmark 
  PD_BI_DEN <-  lm(PHYSICAL_DISTANCING ~ Wave + PANDEMIC_FATIGUE, data = DEN)
  summ(PD_BI_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(PD_BI_DEN), digits = 3) # Standardized effect sizes
  
  # Model 1 in Germany - Physical distancing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  PD_GER_1 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + 
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Physical distancing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  PD_DEN_1 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Physical distancing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  PD_DEN_2 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index + 
                   OPTIMISTIC +  NEGATIVE_AFFECT + EMPATHY +
                   HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results 
  export_summs(PD_GER_1, PD_DEN_1, PD_DEN_2, model.names = c("Physical distancing - GER", "Physical distancing - DEN", "Physical distancing - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(PD_GER_1), digits = 3) # Standardized effect sizes model 1 Germany
  APAStyler(modelTest(PD_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark
  APAStyler(modelTest(PD_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PD_DEN_2, PD_DEN_1, PD_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Physical distancing - Model 2\nDenmark (n = 15,891)",  "Physical distancing - Model 1\nDenmark (n = 15,891)", "Physical distancing - Model 1\nGermany (n = 11,652)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure 4 - OLS regressions predicting physical distancing in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PD_DEN_2, PD_DEN_1, PD_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Physical distancing - Model 2\nDenmark (n = 15,891)",  "Physical distancing - Model 1\nDenmark (n = 15,891)", "Physical distancing - Model 1\nGermany (n = 11,652)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(PD_BI_GER, PD_BI_DEN, PD_GER_1, PD_DEN_1, PD_DEN_2, DEN, GER)
  
  ###########################################################################
  ## The Link between Pandemic Fatigue and Hygiene - Cross-Sectional Data ##
  #########################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC",  "HYGIENE", "Wave", "AGE", 
                              "PANDEMIC_FATIGUE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES",
                              "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "HYGIENE", "Wave", "AGE", "PANDEMIC_FATIGUE", 
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", 
                                                 "HH", "EM", "EX","AG", "CO", "OP", "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                                                 "reproduction_rate", "stringency_index"))  
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:16] <- scale(GER[6:16])
  DEN[6:25] <- scale(DEN[6:25])
  
  # Regression analysis controlling only for time - Germany 
  H_BI_GER <-  lm(HYGIENE ~ Wave + PANDEMIC_FATIGUE, data = GER)
  summ(H_BI_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(H_BI_GER), digits = 3) # Standardized effect sizes
  
  # Regression analysis controlling only for time - Denmark 
  H_BI_DEN <-  lm(HYGIENE ~ Wave + PANDEMIC_FATIGUE, data = DEN)
  summ(H_BI_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(H_BI_DEN), digits = 3) # Standardized effect sizes
  
  # Model 1 in Germany - Hygiene <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  H_GER_1 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                  PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                  new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                  reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Hygiene <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  H_DEN_1 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                  PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                  new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                  reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Hygiene <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  H_DEN_2 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                  PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                  new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                  reproduction_rate + stringency_index +  OPTIMISTIC +  NEGATIVE_AFFECT + 
                  EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results
  export_summs(H_GER_1, H_DEN_1, H_DEN_2, model.names = c("Hygiene - GER", "Hygiene - DEN", "Hygiene - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(H_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(H_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(H_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark 
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(H_DEN_2, H_DEN_1, H_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Hygiene - Model 2\nDenmark (n = 15,891)",  "Hygiene - Model 1\nDenmark (n = 15,891)", "Hygiene - Model 1\nGermany (n = 6,462)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure 5 - OLS regressions predicting hygiene in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(H_DEN_2, H_DEN_1, H_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Hygiene - Model 2\nDenmark (n = 15,891)",  "Hygiene - Model 1\nDenmark (n = 15,891)", "Hygiene - Model 1\nGermany (n = 6,462)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(H_BI_GER, H_BI_DEN, H_GER_1, H_DEN_1, H_DEN_2, GER, DEN)
  
  ################################################################################
  ## The Link between Pandemic Fatigue and Mask Wearing - Cross-Sectional Data ##
  ##############################################################################
  
  # Extract relevant variables
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "MASK_WEARING", "Wave", "AGE",
                              "PANDEMIC_FATIGUE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", 
                              "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "MASK_WEARING", "Wave", "AGE", "PANDEMIC_FATIGUE",
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY",
                                                 "HH", "EM", "EX","AG", "CO", "OP",  "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                                                 "reproduction_rate", "stringency_index"))  
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:16] <- scale(GER[6:16])
  DEN[6:25] <- scale(DEN[6:25])
  
  # Regression analysis controlling only for time - Germany 
  MW_BI_GER <-  lm(MASK_WEARING ~ Wave + PANDEMIC_FATIGUE, data = GER)
  summ(MW_BI_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(MW_BI_GER), digits = 3) # Standardized effect sizes 
  
  # Regression analysis controlling only for time - Denmark 
  MW_BI_DEN <-  lm(MASK_WEARING ~ Wave + PANDEMIC_FATIGUE, data = DEN)
  summ(MW_BI_DEN, digits = 3, confint = TRUE)
  APAStyler(modelTest(MW_BI_DEN), digits = 3) # Standardized effect sizes 
  
  # Model 1 - Mask wearing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  MW_GER_1 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + 
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = GER)
  
  # Model 1 - Mask wearing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  MW_DEN_1 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + 
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 - Mask wearing <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  MW_DEN_2 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT + 
                   EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results
  export_summs(MW_GER_1, MW_DEN_1, MW_DEN_2,  model.names = c("Mask wearing - GER", "Mask wearing - DEN", "Mask wearing - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(MW_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(MW_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark
  APAStyler(modelTest(MW_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark 
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(MW_DEN_2, MW_DEN_1, MW_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Mask wearing - Model 2\nDenmark (n = 15,891)",  "Mask wearing - Model 1\nDenmark (n = 15,891)", "Mask wearing - Model 1\nGermany (n = 13,875)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure 6 - OLS regressions predicting mask wearing in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(MW_DEN_2, MW_DEN_1, MW_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Mask wearing - Model 2\nDenmark (n = 15,891)",  "Mask wearing - Model 1\nDenmark (n = 15,891)", "Mask wearing - Model 1\nGermany (n = 13,875)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, MW_BI_GER, MW_BI_DEN, MW_GER_1, MW_DEN_1, MW_DEN_2)
  
  #######################################################################################
  ## The Link between Pandemic Fatigue and Information Seeking - Cross-Sectional Data ##
  #####################################################################################
  
  # Extract relevant variables
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "FREQ_INFO", "Wave", "AGE",
                              "PANDEMIC_FATIGUE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES",
                              "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "INFO_SEEK",  "Wave", "AGE", "PANDEMIC_FATIGUE", 
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", 
                                                 "HH", "EM", "EX","AG", "CO", "OP", "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                                                 "reproduction_rate", "stringency_index"))  
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:16] <- scale(GER[6:16])
  DEN[6:25] <- scale(DEN[6:25])
  
  # Regression analysis controlling only for time - Germany 
  IS_BI_GER <-  lm(FREQ_INFO ~ Wave + PANDEMIC_FATIGUE, data = GER)
  summ(IS_BI_GER, digits = 3, confint = TRUE)
  APAStyler(modelTest(IS_BI_GER), digits = 3) # Standardized effect sizes
  
  # Regression analysis controlling only for time - Denmark 
  IS_BI_DEN <-  lm(INFO_SEEK ~ Wave + PANDEMIC_FATIGUE, data = DEN)
  summ(IS_BI_DEN,  digits = 3, confint = TRUE)
  APAStyler(modelTest(IS_BI_DEN), digits = 3) # Standardized effect sizes
  
  # Model 1 in Germany - Information seeking <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  IS_GER_1 <- lm(FREQ_INFO ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Information seeking <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  IS_DEN_1 <- lm(INFO_SEEK ~  Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + 
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Information seeking <- Pandemic fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  IS_DEN_2 <- lm(INFO_SEEK ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + 
                   PANDEMIC_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +
                   new_cases_smoothed_per_million + new_deaths_smoothed_per_million +
                   reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT +
                   EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results
  export_summs(IS_GER_1, IS_DEN_1, IS_DEN_2, model.names = c("Information seeking - GER", "Information seeking - DEN", "Information seeking - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(IS_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(IS_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark
  APAStyler(modelTest(IS_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark  
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS_DEN_2, IS_DEN_1, IS_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information seeking - Model 2\nDenmark (n = 14,972)",  "Information seeking - Model 1\nDenmark (n = 14,972)", "Information seeking - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf
  pdf("Figure 7 - OLS regressions predicting information seeking in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS_DEN_2, IS_DEN_1, IS_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information seeking - Model 2\nDenmark (n = 14,972)",  "Information seeking - Model 1\nDenmark (n = 14,972)", "Information seeking - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", 
                              "Affective risk perceptions", "Cognitive risk perceptions", "Pandemic fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", 
                              "Gender (male)", "Age","Time (survey wave)", "Intercept"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, IS_BI_GER, IS_BI_DEN, IS_GER_1, IS_DEN_1, IS_DEN_2)
  
  #####################################################################################################################################
  ## The Causal Impact of Pandemic Fatigue on People's Intentions to Adhere to Recommended Health-Protective Behaviors - Experiment ##  
  ###################################################################################################################################
  
  # Cronbach´s alpha - Behavioral intentions
  psych::alpha(data.frame(E[c("PHYSICAL_DISTANCING", "MASK_WEARING", "HYGIENE", "INFORMATION_SEEKING")]), check.keys=TRUE)
  
  # Factor condition variable 
  E$CONDITION[E$CONDITION==1] <-"Control"
  E$CONDITION[E$CONDITION==2] <-"High Pandemic Fatigue"
  E$CONDITION[E$CONDITION==3] <-"Low Pandemic Fatigue"
  
  # Manipulation check - High pandemic fatigue vs low pandemic fatigue 
  E$CONDITION <-  factor(E$CONDITION, levels = c("High Pandemic Fatigue", "Low Pandemic Fatigue", "Control"))
  report(t.test(E$PANDEMIC_FATIGUE[E$CONDITION != "Control"] ~ E$CONDITION[E$CONDITION != "Control"]))
  cohen.d(E$PANDEMIC_FATIGUE[E$CONDITION != "Control"], E$CONDITION[E$CONDITION != "Control"])
  
  # Manipulation check - Low pandemic fatigue vs control 
  E$CONDITION <-  factor(E$CONDITION, levels = c("Low Pandemic Fatigue", "Control", "High Pandemic Fatigue"))
  report(t.test(E$PANDEMIC_FATIGUE[E$CONDITION != "High Pandemic Fatigue"] ~ E$CONDITION[E$CONDITION != "High Pandemic Fatigue"]))
  cohen.d(E$PANDEMIC_FATIGUE[E$CONDITION != "High Pandemic Fatigue"], E$CONDITION[E$CONDITION != "High Pandemic Fatigue"])
  
  # Manipulation check -  High pandemic fatigue vs control 
  E$CONDITION <-  factor(E$CONDITION, levels = c( "High Pandemic Fatigue", "Control", "Low Pandemic Fatigue"))
  report(t.test(E$PANDEMIC_FATIGUE[E$CONDITION != "Low Pandemic Fatigue"] ~ E$CONDITION[E$CONDITION != "Low Pandemic Fatigue"]))
  cohen.d(E$PANDEMIC_FATIGUE[E$CONDITION != "Low Pandemic Fatigue"], E$CONDITION[E$CONDITION != "Low Pandemic Fatigue"])
  
  # Bonferroni correction manipulation check
  P_MAN <- c(0.00000007041, 0.01292, 0.003523)
  p.adjust(P_MAN, method = "bonferroni")
  
  # Hypothesis testing - High pandemic fatigue vs low pandemic fatigue 
  E$CONDITION <-  factor(E$CONDITION, levels = c("Low Pandemic Fatigue", "Control", "High Pandemic Fatigue"))
  report(t.test(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "Control"] ~ E$CONDITION[E$CONDITION != "Control"]))
  cohen.d(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "Control"], E$CONDITION[E$CONDITION != "Control"])
  
  # Hypothesis testing - High pandemic fatigue vs control 
  E$CONDITION <-  factor(E$CONDITION, levels = c( "Control", "Low Pandemic Fatigue", "High Pandemic Fatigue"))
  report(t.test(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "Low Pandemic Fatigue"] ~ E$CONDITION[E$CONDITION != "Low Pandemic Fatigue"]))
  cohen.d(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "Low Pandemic Fatigue"], E$CONDITION[E$CONDITION != "Low Pandemic Fatigue"])
  
  # Hypothesis testing - Low pandemic fatigue vs control 
  E$CONDITION <-  factor(E$CONDITION, levels = c( "Low Pandemic Fatigue", "Control", "High Pandemic Fatigue"))
  report(t.test(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "High Pandemic Fatigue"] ~ E$CONDITION[E$CONDITION != "High Pandemic Fatigue"]))
  cohen.d(E$BEHAVIORAL_INTENTIONS[E$CONDITION != "High Pandemic Fatigue"], E$CONDITION[E$CONDITION != "High Pandemic Fatigue"])
  
  # Bonferroni correction hypothesis testing
  P_EXP <- c(0.00003867, 0.2134, 0.002983)
  p.adjust(P_EXP, method = "bonferroni")
  
  # Raincloud plot - Pandemic fatigue 
  ggplot(E, aes(y = PANDEMIC_FATIGUE, x = CONDITION , fill = CONDITION)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8,  trim = FALSE) +
    geom_point(aes(y = PANDEMIC_FATIGUE, color = CONDITION), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.7) +
    labs(x = "", y = "") +
    theme_classic(base_size = 15)+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(size = 15))+
    scale_y_continuous(breaks=seq(0,7,.5))+
    scale_x_discrete(labels=c("Control"="Control\n(n = 552)", "High Pandemic Fatigue"="High pandemic fatigue\n(n = 502)",
                              "Low Pandemic Fatigue"="Low pandemic fatigue\n(n = 530)"))+
    scale_fill_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+ 
    scale_color_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+
    geom_segment(aes(x = 1.25, y = 7.6, xend = 2.15, yend = 7.6))+
    geom_segment(aes(x = 2.25, y = 7.6, xend = 3.15, yend = 7.6))+
    geom_segment(aes(x = 1.2, y = 8.2, xend = 3.2, yend = 8.2))+
    annotate("text", x = 1.7, y = 7.65, label = "*", size = 10)+
    annotate("text", x = 2.7, y = 7.65, label = "**", size = 10)+
    annotate("text", x = 2.2, y = 8.25, label = "***", size = 10)+
    ggtitle("Pandemic fatigue")
  
  # Raincloud plot - Behavioral intentions 
  ggplot(E, aes(y = BEHAVIORAL_INTENTIONS, x = CONDITION , fill = CONDITION)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim = FALSE) +
    geom_point(aes(y = BEHAVIORAL_INTENTIONS, color = CONDITION), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.7) +
    labs(x = "", y = "") +
    theme_classic(base_size = 15)+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(size = 15))+
    scale_y_continuous(breaks=seq(0,7,.5))+
    scale_x_discrete(labels=c("Control"="Control\n(n = 552)", "High Pandemic Fatigue"="High pandemic fatigue\n(n = 502)",
                              "Low Pandemic Fatigue"="Low pandemic fatigue\n(n = 530)"))+
    scale_fill_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+ 
    scale_color_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+
    geom_segment(aes(x = 1.25, y = 7.6, xend = 2.15, yend = 7.6))+
    geom_segment(aes(x = 2.25, y = 7.6, xend = 3.15, yend = 7.6))+
    geom_segment(aes(x = 1.2, y = 8.1, xend = 3.2, yend = 8.1))+
    annotate("text", x = 1.7, y = 7.9, label = "ns.", size = 7)+
    annotate("text", x = 2.7, y = 7.65, label = "**", size = 10)+
    annotate("text", x = 2.2, y = 8.15, label = "***", size = 10) + 
    ggtitle("Behavioral intentions")
  
  # Export plots 
  pdf("Figure 8 - Pandemic fatigue and behavioral intentions per condition.pdf", width = 15, height = 7 )
  a <- ggplot(E, aes(y = PANDEMIC_FATIGUE, x = CONDITION , fill = CONDITION)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8,  trim = FALSE) +
    geom_point(aes(y = PANDEMIC_FATIGUE, color = CONDITION), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.7) +
    labs(x = "", y = "") +
    theme_classic(base_size = 15)+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(size = 15))+
    scale_y_continuous(breaks=seq(0,7,.5))+
    scale_x_discrete(labels=c("Control"="Control\n(n = 552)", "High Pandemic Fatigue"="High pandemic fatigue\n(n = 502)",
                              "Low Pandemic Fatigue"="Low pandemic fatigue\n(n = 530)"))+
    scale_fill_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+ 
    scale_color_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+
    geom_segment(aes(x = 1.25, y = 7.6, xend = 2.15, yend = 7.6))+
    geom_segment(aes(x = 2.25, y = 7.6, xend = 3.15, yend = 7.6))+
    geom_segment(aes(x = 1.2, y = 8.2, xend = 3.2, yend = 8.2))+
    annotate("text", x = 1.7, y = 7.65, label = "*", size = 10)+
    annotate("text", x = 2.7, y = 7.65, label = "**", size = 10)+
    annotate("text", x = 2.2, y = 8.25, label = "***", size = 10)+
    ggtitle("Pandemic fatigue")
  
  b <- ggplot(E, aes(y = BEHAVIORAL_INTENTIONS, x = CONDITION , fill = CONDITION)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim = FALSE) +
    geom_point(aes(y = BEHAVIORAL_INTENTIONS, color = CONDITION), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.7) +
    labs(x = "", y = "") +
    theme_classic(base_size = 15)+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(size = 15))+
    scale_y_continuous(breaks=seq(0,7,.5))+
    scale_x_discrete(labels=c("Control"="Control\n(n = 552)", "High Pandemic Fatigue"="High pandemic fatigue\n(n = 502)",
                              "Low Pandemic Fatigue"="Low pandemic fatigue\n(n = 530)"))+
    scale_fill_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+ 
    scale_color_manual(values=c("#F8766D", "#00BFC4", "#00C094"))+
    geom_segment(aes(x = 1.25, y = 7.6, xend = 2.15, yend = 7.6))+
    geom_segment(aes(x = 2.25, y = 7.6, xend = 3.15, yend = 7.6))+
    geom_segment(aes(x = 1.2, y = 8.1, xend = 3.2, yend = 8.1))+
    annotate("text", x = 1.7, y = 7.9, label = "ns.", size = 7)+
    annotate("text", x = 2.7, y = 7.65, label = "**", size = 10)+
    annotate("text", x = 2.2, y = 8.15, label = "***", size = 10) + 
    ggtitle("Behavioral intentions")
  ggarrange(a, b, labels = c("A", "B"))
  dev.off()
  rm(a,b)
  
  #####################################
  ## Robustness checks - Experiment ##
  ###################################
  
  # Extract relevant data 
  EXP <- subset(E, GENDER != "Other", select = c("CONDITION", "GENDER", "EDUCATION", "AGE", "COGNITIVE_RISK", "BEHAVIORAL_INTENTIONS"))
  
  # Recode education - University yes/no
  EXP$EDUCATION <-  as.character(EXP$EDUCATION)
  EXP$EDUCATION[EXP$EDUCATION == "Other"] <-  "University - No"
  EXP$EDUCATION[EXP$EDUCATION == "Elementary-Secondary School"] <-  "University - No"
  EXP$EDUCATION[EXP$EDUCATION == "High School"] <-  "University - No"
  EXP$EDUCATION[EXP$EDUCATION == "University"] <-  "University - Yes"
  
  # Scale data 
  EXP[4:5] <- scale(EXP[4:5])
  
  # Regression analysis - Control vs high and low pandemic fatigue - Controlling for age, gender, education and cognitive risk 
  EXP$CONDITION <-  factor(EXP$CONDITION, levels = c( "Control","Low Pandemic Fatigue", "High Pandemic Fatigue"))
  Model1 <- lm(BEHAVIORAL_INTENTIONS ~ CONDITION + AGE + GENDER + EDUCATION + COGNITIVE_RISK, data = EXP)
  summ(Model1, digits = 3)
  export_summs(Model1,  error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(Model1), digits = 3) # Standardized effect sizes 
  
  # Regression analysis - High pandemic fatigue vs low pandemic fatigue and control - Controlling for age, gender, education and cognitive risk 
  EXP$CONDITION <-  factor(EXP$CONDITION, levels = c( "High Pandemic Fatigue","Low Pandemic Fatigue", "Control"))
  Model2 <- lm(BEHAVIORAL_INTENTIONS ~ CONDITION + AGE + GENDER + EDUCATION + COGNITIVE_RISK, data = EXP)
  summ(Model2, digits = 3)
  export_summs(Model2,  error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(Model2), digits = 3) # Standardized effect sizes 
  
  # Bonferroni correction robustness checks
  P_ROB <- c(0.00407, 0.13574, 0.00001717)
  p.adjust(P_ROB, method = "bonferroni")
  rm(Model1, Model2, EXP, P_MAN, P_EXP, P_ROB)
  
  ###########################################################
  ## Cronbach´s Alpha for all Other Scales Used - Germany ##
  #########################################################
  
  # Affective risk 
  psych::alpha(data.frame(G[c("AFF_DISTANCE", "AFF_SPREAD", "AFF_THINK", "AFF_FEAR", "AFF_WORRY", "AFF_HELPLESS")]), check.keys=TRUE) 
  
  # Trust 
  psych::alpha(data.frame(G[c("TRUST_DOCTOR", "TRUST_MEDIA", "TRUST_HOSPITAL", "TRUST_LOCAL_HEALTH", "TRUST_STATE_HEALTH", 
                              "TRUST_FEDERAL_HEALTH", "TRUST_RKI", "TRUST_BZGA", "TRUST_SCIENCE", "TRUST_GOVERN", "TRUST_WHO")]), check.keys=TRUE) 
  
  # Worries 
  psych::alpha(data.frame(G[c("WORRY_LOSS", "WORRY_HEALTH_SYSTEM", "WORRY_BANKRUPTCY", "WORRY_RECESSION", 
                              "WORRY_EGOISM", "WORRY_MONEY", "WORRY_GAP", "WORRY_GETILL", "WORRY_SOCIETY")]), check.keys=TRUE) 
  
  # Physical distancing  
  psych::alpha(data.frame(G[c("USE2_HANDSHAKE", "USE2_SPACE150", "USE2_AVOID", "USE2_AVOID_PARTY", "USE2_NEC_WAYS", "USE2_TRIPS", 
                              "USE2_CROWDED_SPACES", "USE2_CROWDED_PLACES_G", "USE2_CC_SETTINGS_G", "USE2_AVOID_CONTACTS")]), check.keys=TRUE) 
  
  # Hygiene 
  psych::alpha(data.frame(G[c("USE2_TOUCHING", "USE2_SANITIZER", "USE2_COVERING", "USE2_HANDWASH20",  "USE2_AIRROOMS")]), check.keys=TRUE) 
  
  # Full pandemic fatigue scale 
  psych::alpha(data.frame(G[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                              "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # Information fatigue factor 
  psych::alpha(data.frame(G[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF")]), check.keys=TRUE) 
  
  # Behavioral fatigue factor 
  psych::alpha(data.frame(G[c("PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  ###########################################################
  ## Cronbach´s Alpha for all Other Scales Used - Denmark ##
  #########################################################
  
  # Affective risk 
  psych::alpha(data.frame(D[c("AFF_DISTANCE", "AFF_SPREAD", "AFF_THINK", "AFF_FEAR", "AFF_WORRY", "AFF_HELPLESS")]), check.keys=TRUE)
  
  # Negative affect 
  psych::alpha(data.frame(D[c("BOREDOM", "LONELY", "ISOLATED", "STRESSED")]), check.keys=TRUE)
  
  # Trust 
  psych::alpha(data.frame(D[c("TRUST_POLICE", "TRUST_LOCAL_BUSINESS", "TRUST_HOSPITALS_DOCTORS", "TRUST_GOVERMENT", "TRUST_EXPERTS", "TRUST_POLITICIANS")]), check.keys=TRUE) 
  
  # Worries 
  psych::alpha(data.frame(D[c("WORRY_LOSE", "WORRY_SYSTEMS", "WORRY_COMPANIES", "WORRY_RECESSION", "WORRY_FOODS", "WORRY_BLACKOUT", "WORRY_EGOISM")]), check.keys=TRUE) 
  
  # Physical distancing 
  psych::alpha(data.frame(D[c("USE_CONTACT", "USE_AVOID_ELDERLY_SICK")]), check.keys=TRUE) 
  
  # Hygiene 
  psych::alpha(data.frame(D[c("USE_HANDWASH", "USE_COUGH", "USE_BEHAVIOR")]), check.keys=TRUE) 
  
  # Empathy 
  psych::alpha(data.frame(D[c("EMPATHY1", "EMPATHY2", "EMPATHY3")]), check.keys=TRUE) 
  
  # Information seeking 
  psych::alpha(data.frame(D[c("INFORMATION_SEEKING_1", "INFORMATION_SEEKING_2", "INFORMATION_SEEKING_3", "INFORMATION_SEEKING_4", "INFORMATION_SEEKING_5")]), check.keys=TRUE) 
  
  # Full pandemic fatigue scale 
  psych::alpha(data.frame(D[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF",
                              "PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # Information fatigue factor 
  psych::alpha(data.frame(D[c("PANDEMIC_FATIGUE_1_INF", "PANDEMIC_FATIGUE_3_INF", "PANDEMIC_FATIGUE_5_INF")]), check.keys=TRUE) 
  
  # Behavioral fatigue factor 
  psych::alpha(data.frame(D[c("PANDEMIC_FATIGUE_2_MB", "PANDEMIC_FATIGUE_4_MB", "PANDEMIC_FATIGUE_6_MB")]), check.keys=TRUE) 
  
  # HH - Denmark 
  psych::alpha(data.frame(D[c("HEXACO_HOHU1", "HEXACO_HOHU2_R", "HEXACO_HOHU3_R", "HEXACO_HOHU4_R")]),  check.keys=TRUE) 
  
  # EM - Denmark 
  psych::alpha(data.frame(D[c( "HEXACO_EMOT1" , "HEXACO_EMOT2_R" , "HEXACO_EMOT3_R" , "HEXACO_EMOT4")]),  check.keys=TRUE) 
  
  # EX - Denmark 
  psych::alpha(data.frame(D[c("HEXACO_XTRA1_R" , "HEXACO_XTRA2" , "HEXACO_XTRA3" , "HEXACO_XTRA4_R")]),  check.keys=TRUE) 
  
  # AG - Denmark 
  psych::alpha(data.frame(D[c("HEXACO_AGRE1_R" , "HEXACO_AGRE2_R" , "HEXACO_AGRE3", "HEXACO_AGRE4")]), check.keys=TRUE) 
  
  # CO - Denmark 
  psych::alpha(data.frame(D[c("HEXACO_CONS1" , "HEXACO_CONS2_R" , "HEXACO_CONS3" , "HEXACO_CONS4_R")]), check.keys=TRUE) 
  
  # OP - Denmark 
  psych::alpha(data.frame(D[c("HEXACO_OPEN1" , "HEXACO_OPEN2_R" , "HEXACO_OPEN3" , "HEXACO_OPEN4")]), check.keys=TRUE)
  
  ###############################################################
  ## Correlates of Information Fatigue - Cross-Sectional Data ## 
  #############################################################
  
  # Extract relevant variables 
  GER <-subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "INFORMATION_FATIGUE", "Wave", 
                             "AGE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "new_cases_smoothed_per_million",
                             "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "INFORMATION_FATIGUE", "Wave", 
                                                 "AGE", "COGNITIVE_RISK","AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", 
                                                 "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX", "AG", "CO", "OP", 
                                                 "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million",
                                                 "reproduction_rate", "stringency_index" ))
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  
  GER[6:15] <- scale(GER[6:15])
  DEN[6:24] <- scale(DEN[6:24])
  
  # Create Wave^2 variable 
  GER$Wave2 <- GER$Wave^2
  DEN$Wave2 <- DEN$Wave^2
  
  # Model 1 in Germany - Information fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors 
  IF_GER_1 <-  lm(INFORMATION_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER )
  
  # Model 1 in Denmark - Information fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors 
  IF_DEN_1<-  lm(INFORMATION_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                   COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Information fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors + HEXACO and Additional Emotions
  IF_DEN_2 <-  lm(INFORMATION_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC +
                    NEGATIVE_AFFECT + EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results
  export_summs(IF_GER_1, IF_DEN_1, IF_DEN_2, model.names = c("Information fatigue - GER", "Information fatigue - DEN", "Information fatigue - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(IF_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(IF_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(IF_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark  
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IF_DEN_2, IF_DEN_1, IF_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information fatigue - Model 2\nDenmark (n = 15,891)",  "Information fatigue - Model 1\nDenmark (n = 15,891)", " Information fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions","Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure S9 - OLS regressions predicting information fatigue in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IF_DEN_2, IF_DEN_1, IF_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information fatigue - Model 2\nDenmark (n = 15,891)",  "Information fatigue - Model 1\nDenmark (n = 15,891)", " Information fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions","Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, IF_GER_1, IF_DEN_1, IF_DEN_2)
  
  ##############################################################
  ## Correlates of Behavioral Fatigue - Cross-Sectional Data ## 
  ############################################################
  
  # Extract relevant variables 
  GER <-subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "BEHAVIORAL_FATIGUE", "Wave", "AGE", "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST",
                             "WORRIES", "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "BEHAVIORAL_FATIGUE", "Wave", "AGE",
                                                 "COGNITIVE_RISK","AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", 
                                                 "EMPATHY", "HH", "EM", "EX", "AG", "CO", "OP",  "new_cases_smoothed_per_million",
                                                 "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index"))
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  
  GER[6:15] <- scale(GER[6:15])
  DEN[6:24] <- scale(DEN[6:24])
  
  # Create Wave^2 variable 
  GER$Wave2 <- GER$Wave^2
  DEN$Wave2 <- DEN$Wave^2
  
  # Model 1 in Germany - Behavioral fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors 
  BF_GER_1 <-  lm(BEHAVIORAL_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER )
  
  # Model 1 in Denmark - Behavioral fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors 
  BF_DEN_1<-  lm(BEHAVIORAL_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                   COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Behavioral fatigue <- Sociodemographics + Emotions + Perception + Contextual Factors + HEXACO and Additional Emotions
  BF_DEN_2 <-  lm(BEHAVIORAL_FATIGUE ~ Wave + Wave2 + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +
                    COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +new_cases_smoothed_per_million + 
                    new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC + 
                    NEGATIVE_AFFECT + EMPATHY + HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results
  export_summs(BF_GER_1, BF_DEN_1, BF_DEN_2, model.names = c("Behavioral fatigue - GER", "Behavioral fatigue - DEN", "Behavioral fatigue - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(BF_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(BF_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(BF_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark  
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(BF_DEN_2, BF_DEN_1, BF_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Behavioral Fatigue - Model 2\nDenmark (n = 15,891)",  "Behavioral fatigue - Model 1\nDenmark (n = 15,891)", " Behavioral fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions","Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure S10 - OLS regressions predicting behavioral fatigue in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(BF_DEN_2, BF_DEN_1, BF_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Behavioral Fatigue - Model 2\nDenmark (n = 15,891)",  "Behavioral fatigue - Model 1\nDenmark (n = 15,891)", " Behavioral fatigue - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million", "Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions","Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", expression("Time (survey wave)"^2), "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, BF_GER_1, BF_DEN_1, BF_DEN_2)
  
  ###########################################################################################################################
  ## The Link between Behavioral/Information Fatigue and Physical Distancing - Germany and Denmark - Cross-Sectional Data ##
  #########################################################################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "PHYSICAL_DISTANCING", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                              "COGNITIVE_RISK",  "AFFECTIVE_RISK", "TRUST", "WORRIES", "new_cases_smoothed_per_million","new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC",  "PHYSICAL_DISTANCING", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX","AG", "CO", "OP",
                                                 "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index")) 
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:17] <- scale(GER[6:17])
  DEN[6:26] <- scale(DEN[6:26])
  
  # Bivariate regression analyses - Germany 
  PD_BF_BI_GER <-  lm(PHYSICAL_DISTANCING ~ BEHAVIORAL_FATIGUE, data = GER)
  summ(PD_BF_BI_GER, digits = 3)
  APAStyler(modelTest(PD_BF_BI_GER), digits = 3) # Standardized effect sizes 
  
  PD_IF_BI_GER <-  lm(PHYSICAL_DISTANCING ~ INFORMATION_FATIGUE, data = GER)
  summ(PD_IF_BI_GER, digits = 3)
  APAStyler(modelTest(PD_IF_BI_GER), digits = 3) # Standardized effect sizes 
  
  # Bivariate regression analyses - Denmark 
  PD_BF_BI_DEN <-  lm(PHYSICAL_DISTANCING ~ BEHAVIORAL_FATIGUE, data = DEN)
  summ(PD_BF_BI_DEN, digits = 3)
  APAStyler(modelTest(PD_BF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  PD_IF_BI_DEN <-  lm(PHYSICAL_DISTANCING ~ INFORMATION_FATIGUE, data = DEN)
  summ(PD_IF_BI_DEN, digits = 3)
  APAStyler(modelTest(PD_IF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  # Model 1 in Germany - Physical distancing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  PD_GER_1 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million +
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Physical distancing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  PD_DEN_1 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Physical distancing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  PD_DEN_2 <- lm(PHYSICAL_DISTANCING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES +  new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT + EMPATHY + 
                   HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results 
  export_summs(PD_GER_1, PD_DEN_1, PD_DEN_2, model.names = c("Physical distancing - GER", "Physical distancing - DEN", "Physical istancing - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(PD_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(PD_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(PD_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PD_DEN_2, PD_DEN_1, PD_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Physical distancing - Model 2\nDenmark (n = 15,891)",  "Physical distancing - Model 1\nDenmark (n = 15,891)", "Physical distancing - Model 1\nGermany (n = 11,652)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure S13 - OLS regressions predicting physical distancing in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(PD_DEN_2, PD_DEN_1, PD_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Physical distancing - Model 2\nDenmark (n = 15,891)",  "Physical distancing - Model 1\nDenmark (n = 15,891)", "Physical distancing - Model 1\nGermany (n = 11,652)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, PD_BF_BI_GER, PD_BF_BI_DEN, PD_IF_BI_GER, PD_IF_BI_DEN, PD_GER_1, PD_DEN_1, PD_DEN_2)
  
  ###############################################################################################################
  ## The Link between Behavioral/Information Fatigue and Hygiene - Germany and Denmark - Cross-Sectional Data ##
  #############################################################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "HYGIENE", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                              "COGNITIVE_RISK",  "AFFECTIVE_RISK", "TRUST", "WORRIES", "new_cases_smoothed_per_million","new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC",  "HYGIENE", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX","AG", "CO", "OP",
                                                 "new_cases_smoothed_per_million","new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index")) 
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:17] <- scale(GER[6:17])
  DEN[6:26] <- scale(DEN[6:26])
  
  # Bivariate regression analyses - Germany 
  H_BF_BI_GER <-  lm(HYGIENE ~ BEHAVIORAL_FATIGUE, data = GER)
  summ(H_BF_BI_GER, digits = 3)
  APAStyler(modelTest(H_BF_BI_GER), digits = 3) # Standardized effect sizes 
  
  H_IF_BI_GER <-  lm(HYGIENE ~ INFORMATION_FATIGUE, data = GER)
  summ(H_IF_BI_GER, digits = 3)
  APAStyler(modelTest(H_IF_BI_GER), digits = 3) # Standardized effect sizes 
  
  # Bivariate regression analyses - Denmark 
  H_BF_BI_DEN <-  lm(HYGIENE ~ BEHAVIORAL_FATIGUE, data = DEN)
  summ(H_BF_BI_DEN, digits = 3)
  APAStyler(modelTest(H_BF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  H_IF_BI_DEN <-  lm(HYGIENE ~ INFORMATION_FATIGUE, data = DEN)
  summ(H_IF_BI_DEN, digits = 3)
  APAStyler(modelTest(H_IF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  # Model 1 in Germany - Hygiene <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  H_GER_1 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                  INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                  new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Hygiene <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  H_DEN_1 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                  INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                  new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Hygiene <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + HEXACO 
  H_DEN_2 <- lm(HYGIENE ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + BEHAVIORAL_FATIGUE +
                  INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                  new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT + EMPATHY + 
                  HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results 
  export_summs(H_GER_1, H_DEN_1, H_DEN_2, model.names = c("Hygiene - GER", "Hygiene - DEN", "Hygiene - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(H_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(H_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(H_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(H_DEN_2, H_DEN_1, H_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Hygiene - Model 2\nDenmark (n = 15,891)",  "Hygiene - Model 1\nDenmark (n = 15,891)", "Hygiene - Model 1\nGermany (n = 6,462)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure S14 - OLS regressions predicting hygiene in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(H_DEN_2, H_DEN_1, H_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Hygiene - Model 2\nDenmark (n = 15,891)",  "Hygiene - Model 1\nDenmark (n = 15,891)", "Hygiene - Model 1\nGermany (n = 6,462)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, H_BF_BI_GER, H_IF_BI_GER, H_BF_BI_DEN, H_IF_BI_DEN, H_GER_1, H_DEN_1, H_DEN_2)
  
  ####################################################################################################################
  ## The Link between Behavioral/Information Fatigue and Mask Wearing - Germany and Denmark - Cross-Sectional Data ##
  ##################################################################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "MASK_WEARING", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                              "COGNITIVE_RISK",  "AFFECTIVE_RISK", "TRUST", "WORRIES", "new_cases_smoothed_per_million","new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "MASK_WEARING", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX","AG", "CO", "OP",
                                                 "new_cases_smoothed_per_million","new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index")) 
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:17] <- scale(GER[6:17])
  DEN[6:26] <- scale(DEN[6:26])
  
  # Bivariate regression analyses - Germany 
  MW_BF_BI_GER <-  lm(MASK_WEARING ~ BEHAVIORAL_FATIGUE, data = GER)
  summ(MW_BF_BI_GER, digits = 3)
  APAStyler(modelTest(MW_BF_BI_GER), digits = 3) # Standardized effect sizes 
  
  MW_IF_BI_GER <-  lm(MASK_WEARING ~ INFORMATION_FATIGUE, data = GER)
  summ(MW_IF_BI_GER, digits = 3)
  APAStyler(modelTest(MW_IF_BI_GER), digits = 3) # Standardized effect sizes 
  
  # Bivariate regression analyses - Denmark 
  MW_BF_BI_DEN <-  lm(MASK_WEARING ~ BEHAVIORAL_FATIGUE, data = DEN)
  summ(MW_BF_BI_DEN, digits = 3)
  APAStyler(modelTest(MW_BF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  MW_IF_BI_DEN <-  lm(MASK_WEARING ~ INFORMATION_FATIGUE, data = DEN)
  summ(MW_IF_BI_DEN, digits = 3)
  APAStyler(modelTest(MW_IF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  # Model 1 in Germany - Mask wearing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors
  MW_GER_1 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Mask wearing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  MW_DEN_1 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Mask wearing <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  MW_DEN_2 <- lm(MASK_WEARING ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT + EMPATHY + 
                   HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results 
  export_summs(MW_GER_1, MW_DEN_1, MW_DEN_2, model.names = c("Mask wearing - GER", "Mask wearing - DEN", "Mask wearing - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(MW_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(MW_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(MW_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(MW_DEN_2, MW_DEN_1, MW_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Mask wearing - Model 2\nDenmark (n = 15,891)",  "Mask wearing - Model 1\nDenmark (n = 15,891)", "Mask wearing - Model 1\nGermany (n = 13,875)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure S15 - OLS regressions predicting mask wearing in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(MW_DEN_2, MW_DEN_1, MW_GER_1,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Mask wearing - Model 2\nDenmark (n = 15,891)",  "Mask wearing - Model 1\nDenmark (n = 15,891)", "Mask wearing - Model 1\nGermany (n = 13,875)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, MW_BF_BI_GER, MW_IF_BI_GER, MW_BF_BI_DEN, MW_IF_BI_DEN, MW_GER_1, MW_DEN_1, MW_DEN_2)
  
  ###########################################################################################################################
  ## The Link between Behavioral/Information Fatigue and Information Seeking - Germany and Denmark - Cross-Sectional Data ##
  #########################################################################################################################
  
  # Extract relevant variables 
  GER <- subset(G, select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "FREQ_INFO", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                              "COGNITIVE_RISK",  "AFFECTIVE_RISK", "TRUST", "WORRIES", "new_cases_smoothed_per_million","new_deaths_smoothed_per_million",
                              "reproduction_rate", "stringency_index"))
  
  DEN <- subset(D, GENDER != "Other", select = c("GENDER", "EDUCATION", "EMPLOYMENT", "CHRONIC", "INFO_SEEK", "Wave", "AGE", "BEHAVIORAL_FATIGUE", "INFORMATION_FATIGUE",
                                                 "COGNITIVE_RISK", "AFFECTIVE_RISK", "TRUST", "WORRIES", "OPTIMISTIC", "NEGATIVE_AFFECT", "EMPATHY", "HH", "EM", "EX","AG", "CO", "OP",
                                                 "new_cases_smoothed_per_million","new_deaths_smoothed_per_million", "reproduction_rate", "stringency_index")) 
  
  # Scale and standardize data 
  DEN$Wave <- DEN$Wave-19 
  GER$Wave <- GER$Wave-24
  GER[6:17] <- scale(GER[6:17])
  DEN[6:26] <- scale(DEN[6:26])
  
  # Bivariate regression analyses - Germany 
  IS_BF_BI_GER <-  lm(FREQ_INFO ~ BEHAVIORAL_FATIGUE, data = GER)
  summ(IS_BF_BI_GER, digits = 3)
  APAStyler(modelTest(IS_BF_BI_GER), digits = 3) # Standardized effect sizes 
  
  IS_IF_BI_GER <-  lm(FREQ_INFO ~ INFORMATION_FATIGUE, data = GER)
  summ(IS_IF_BI_GER, digits = 3)
  APAStyler(modelTest(IS_IF_BI_GER), digits = 3) # Standardized effect sizes 
  
  # Bivariate regression analyses - Denmark 
  IS_BF_BI_DEN <-  lm(INFO_SEEK ~ BEHAVIORAL_FATIGUE, data = DEN)
  summ(IS_BF_BI_DEN, digits = 3)
  APAStyler(modelTest(IS_BF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  IS_IF_BI_DEN <-  lm(INFO_SEEK ~ INFORMATION_FATIGUE, data = DEN)
  summ(IS_IF_BI_DEN, digits = 3)
  APAStyler(modelTest(IS_IF_BI_DEN), digits = 3) # Standardized effect sizes 
  
  # Model 1 in Germany - Information seeking <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  IS_GER_1 <- lm(FREQ_INFO ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = GER)
  
  # Model 1 in Denmark - Information seeking <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors 
  IS_DEN_1 <- lm(INFO_SEEK ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC +  BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index, data = DEN)
  
  # Model 2 in Denmark - Information seeking <- Behavioral fatigue + Information fatigue + Emotions + Perception + Sociodemographics + Contextual Factors + HEXACO and Additional Emotions
  IS_DEN_2 <- lm(INFO_SEEK ~ Wave + AGE + GENDER + EDUCATION + EMPLOYMENT + CHRONIC + BEHAVIORAL_FATIGUE +
                   INFORMATION_FATIGUE + COGNITIVE_RISK + AFFECTIVE_RISK + TRUST + WORRIES + new_cases_smoothed_per_million + 
                   new_deaths_smoothed_per_million + reproduction_rate + stringency_index + OPTIMISTIC +  NEGATIVE_AFFECT + EMPATHY + 
                   HH + EM + EX + AG + CO + OP, data = DEN)
  
  # Print results 
  export_summs(IS_GER_1, IS_DEN_1, IS_DEN_2, model.names = c("Information seeking - GER", "Information seeking - DEN", "Information seeking - DEN"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(IS_GER_1), digits = 3) # Standardized effect sizes model 1 Germany 
  APAStyler(modelTest(IS_DEN_1), digits = 3) # Standardized effect sizes model 1 Denmark 
  APAStyler(modelTest(IS_DEN_2), digits = 3) # Standardized effect sizes model 2 Denmark
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS_DEN_2, IS_DEN_1, IS_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information seeking - Model 2\nDenmark (n = 14,972)",  "Information seeking - Model 1\nDenmark (n = 14,972)", "Information seeking - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plot to pdf 
  pdf("Figure S16 - OLS regressions predicting information seeking in Germany and Denmark.pdf", height = 15, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS_DEN_2, IS_DEN_1, IS_GER_1, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey", 
              m.labels = c("Information seeking - Model 2\nDenmark (n = 14,972)",  "Information seeking - Model 1\nDenmark (n = 14,972)", "Information seeking - Model 1\nGermany (n = 13,978)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#00BFC4", "#F8766D", "#F8766D"), ci.lvl = .95, 
              axis.labels = c("Openness to experience", "Conscientiousness", "Agreeableness vs. anger", "Extraversion", "Emotionality", "Honesty-humility", "Empathy - most vulnerable", "Negative affect",
                              "Optimism about the future", "Policy stringency index", "Reproduction rate", "New deaths per million","New cases per million","Worries - personal/societal","Institutional trust", "Affective risk perceptions",
                              "Cognitive risk perceptions", "Information fatigue", "Behavioral fatigue", "Chronic disease (Don't know)", "Chronic disease (no)", "Employment (unemployed)", "Education (10 years or more)", "Gender (male)", "Age", "Time (survey wave)"))+ 
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(GER, DEN, IS_BF_BI_GER, IS_IF_BI_GER, IS_BF_BI_DEN, IS_IF_BI_DEN, IS_GER_1, IS_DEN_1, IS_DEN_2)
  

  ################################################################################
  ## Confirmatory Factor Analysis - USA - Robust Maximum Likelihood Estimation ## 
  ##############################################################################
  
  # Define two factor model 
  Model_1 <- 'IF =~ PANDEMIC_FATIGUE_1_IF + PANDEMIC_FATIGUE_2_IF + PANDEMIC_FATIGUE_3_IF
            BF =~ PANDEMIC_FATIGUE_4_BF + PANDEMIC_FATIGUE_5_BF + PANDEMIC_FATIGUE_6_BF'
  
  # Fit two factor model - USA 
  Fit_1_USA <- cfa(Model_1, data = E, std.lv = TRUE, estimator = "MLM")
  summary(Fit_1_USA, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_1_USA, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Define higher order model 
  Model_2 <- 'IF =~ PANDEMIC_FATIGUE_1_IF + PANDEMIC_FATIGUE_2_IF + PANDEMIC_FATIGUE_3_IF
            BF =~ PANDEMIC_FATIGUE_4_BF + PANDEMIC_FATIGUE_5_BF + PANDEMIC_FATIGUE_6_BF
            PF =~   a*IF + a*BF'
  
  # Fit higher order model 
  Fit_2_USA <- cfa(Model_2, data = E, std.lv = TRUE, estimator = "MLM")
  summary(Fit_2_USA, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_2_USA, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Export plots to pdf 
  pdf("Figure S25AB - Two factor and second-order model of pandemic fatigue fitted with data from USA.pdf", width = 15, height = 8)
  par(mfrow=c(1,2))
  semPaths(Fit_1_USA, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Two-factor model (n = 1,584)", line = -35)
  mtext(expression(paste(bold("A"))), side = 3, line = 0, adj = .25, cex = 1.20)
  semPaths(Fit_2_USA, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Second-order model (n = 1,584 )", line = -35)
  mtext(expression(paste(bold("B"))), side = 3, line = 0, adj = .25, cex = 1.20)
  dev.off()
  rm(Fit_1_USA, Fit_2_USA)
  
  ###############################################################################################
  ## Confirmatory Factor Analysis - USA - Robust Diagonally Weighted Least Squares Estimation ##
  #############################################################################################
  
  # Define two factor model 
  Model_1 <- 'IF =~ PANDEMIC_FATIGUE_1_IF + PANDEMIC_FATIGUE_2_IF + PANDEMIC_FATIGUE_3_IF
            BF =~ PANDEMIC_FATIGUE_4_BF + PANDEMIC_FATIGUE_5_BF + PANDEMIC_FATIGUE_6_BF'
  
  # Fit two factor model - USA 
  Fit_1_USA <- cfa(Model_1, data = E, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_IF", "PANDEMIC_FATIGUE_2_IF",
                                                                 "PANDEMIC_FATIGUE_3_IF", "PANDEMIC_FATIGUE_4_BF", 
                                                                 "PANDEMIC_FATIGUE_5_BF", "PANDEMIC_FATIGUE_6_BF"))
  summary(Fit_1_USA, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_1_USA, intercepts = FALSE, thresholds = FALSE, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Define higher order model 
  Model_2 <- 'IF =~ PANDEMIC_FATIGUE_1_IF + PANDEMIC_FATIGUE_2_IF + PANDEMIC_FATIGUE_3_IF
            BF =~ PANDEMIC_FATIGUE_4_BF + PANDEMIC_FATIGUE_5_BF + PANDEMIC_FATIGUE_6_BF
            PF =~   a*IF + a*BF'
  
  # Fit higher order model 
  Fit_2_USA <- cfa(Model_2, data = E, std.lv = TRUE, ordered = c("PANDEMIC_FATIGUE_1_IF", "PANDEMIC_FATIGUE_2_IF",
                                                                 "PANDEMIC_FATIGUE_3_IF", "PANDEMIC_FATIGUE_4_BF", 
                                                                 "PANDEMIC_FATIGUE_5_BF", "PANDEMIC_FATIGUE_6_BF"))
  summary(Fit_2_USA, fit.measures=TRUE, standardized=TRUE)
  semPaths(Fit_2_USA, intercepts = FALSE, thresholds = FALSE, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  
  # Export plots to pdf 
  pdf("Figure S25CD - Polychoric two factor and second-order model of pandemic fatigue fitted with data from USA.pdf", width = 15, height = 8)
  par(mfrow=c(1,2))
  semPaths(Fit_1_USA,intercepts = FALSE, thresholds = FALSE, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2,
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Two-factor model (n = 1,584)", line = -35)
  mtext(expression(paste(bold("C"))), side = 3, line = 0, adj = .25, cex = 1.20)
  semPaths(Fit_2_USA, intercepts = FALSE, thresholds = FALSE, what="std",edge.label.cex=1,edge.color="black",sizeMan=8,sizeLat=12,fade=FALSE,esize=1,asize=2, label.cex = 1.2, 
           nodeLabels = (c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Information\nfatigue", "Behavioral\nfatigue", "Pandemic\nfatigue")),
           color = list(lat = rgb(255,250,205, maxColorValue = 255), man = rgb(224,255,255, maxColorValue = 255)), mar=c(7, 7, 7,7))
  title("Second-order model (n = 1,584 )", line = -35)
  mtext(expression(paste(bold("D"))), side = 3, line = 0, adj = .25, cex = 1.20)
  dev.off()
  rm(Fit_1_USA, Fit_2_USA)
  
  ################################################
  ## Cronbach´s Alpha - Pandemic Fatigue - USA ##
  ##############################################
  
  # Alpha full pandemic fatigue scale
  psych::alpha(data.frame(E[c("PANDEMIC_FATIGUE_1_IF", "PANDEMIC_FATIGUE_2_IF", "PANDEMIC_FATIGUE_3_IF",
                              "PANDEMIC_FATIGUE_4_BF", "PANDEMIC_FATIGUE_5_BF", "PANDEMIC_FATIGUE_6_BF")]), check.keys=TRUE) 
  
  # Alpha - Information fatigue factor
  psych::alpha(data.frame(E[c("PANDEMIC_FATIGUE_1_IF", "PANDEMIC_FATIGUE_2_IF", "PANDEMIC_FATIGUE_3_IF")]), check.keys=TRUE) 
  
  # Alpha - Behavioral fatigue factor
  psych::alpha(data.frame(E[c("PANDEMIC_FATIGUE_4_BF", "PANDEMIC_FATIGUE_5_BF", "PANDEMIC_FATIGUE_6_BF")]), check.keys=TRUE) 
  
  #################################################
  ## MacDonald´s Omega - Pandemic Fatigue - USA ##
  ###############################################
  
  # MacDonald´s omega 
  reliability(cfa(Model_2, data = E, std.lv = TRUE)) # IF + BF 
  reliabilityL2(cfa(Model_2, data = E, std.lv = TRUE), secondFactor = "PF") # PF
  rm(Model_1, Model_2)
  
  ########################################################################################
  ## Correlates of Pandemic Fatigue, Behavioral Fatigue, and Information Fatigue - USA ##
  ######################################################################################
  
  # Extract relevant data 
  USA <- subset(E, GENDER != "Other", select = c("GENDER", "EDUCATION", "AGE", "COGNITIVE_RISK", "PANDEMIC_FATIGUE", "INFORMATION_FATIGUE", "BEHAVIORAL_FATIGUE")) 
  
  # Recode education - University yes/no
  USA$EDUCATION <-  as.character(USA$EDUCATION)
  USA$EDUCATION[USA$EDUCATION == "Other"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "Elementary-Secondary School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "High School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "University"] <-  "University - Yes"
  USA$EDUCATION <-  factor(USA$EDUCATION)
  
  # Scale data 
  USA[3:4] <- scale(USA[3:4])
  
  # Regression models 
  PF <- lm(PANDEMIC_FATIGUE ~ AGE + GENDER + EDUCATION + COGNITIVE_RISK, data = USA)
  BF <- lm(BEHAVIORAL_FATIGUE ~ AGE + GENDER + EDUCATION + COGNITIVE_RISK, data = USA)
  IF <- lm(INFORMATION_FATIGUE ~ AGE + GENDER + EDUCATION + COGNITIVE_RISK, data = USA)
  
  # Print results
  export_summs(PF, BF, IF, model.names = c("Pandemic fatigue","Behavioral fatigue", "Information fatigue"), error_format = "[{conf.low}, {conf.high}]")
  summ(PF, digits = 3)
  summ(BF, digits = 3)
  summ(IF, digits = 3)
  APAStyler(modelTest(PF), digits = 3) # Standardized effect sizes PF 
  APAStyler(modelTest(BF), digits = 3) # Standardized effect sizes BF 
  APAStyler(modelTest(IF), digits = 3) # Standardized effect sizes IF 
  
  # Forest plots 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IF, BF, PF,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",           
              m.labels = c( "Information fatigue - USA\n(n = 1,557)", "Behavioral fatigue - USA\n(n = 1,557)", "Pandemic fatigue - USA\n(n = 1,557"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure S26 - OLS regressions predicting pandemic fatigue, behavioral fatigue and information fatigue in USA.pdf", height = 10, width = 13)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IF, BF, PF,  show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",           
              m.labels = c( "Information fatigue - USA\n(n = 1,557)", "Behavioral fatigue - USA\n(n = 1,557)", "Pandemic fatigue - USA\n(n = 1,557)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(PF, BF, IF, USA)
  
  ###########################################################################################
  ## The Link between Pandemic Fatigue and Recommended Health-Protective Behaviors - USA  ##
  #########################################################################################
  
  # Extract relevant data 
  USA <- subset(E, GENDER != "Other", select = c("GENDER", "EDUCATION", "PHYSICAL_DISTANCING", "MASK_WEARING", "HYGIENE", "INFORMATION_SEEKING", "AGE",
                                                 "BEHAVIORAL_INTENTIONS", "PANDEMIC_FATIGUE","INFORMATION_FATIGUE", "BEHAVIORAL_FATIGUE", "COGNITIVE_RISK")) 
  
  # Recode education - University yes/no
  USA$EDUCATION <-  as.character(USA$EDUCATION)
  USA$EDUCATION[USA$EDUCATION == "Other"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "Elementary-Secondary School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "High School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "University"] <-  "University - Yes"
  USA$EDUCATION <-  factor(USA$EDUCATION)
  
  # Scale data 
  USA[7:12] <- scale(USA[7:12])
  
  # Regression models 
  PD <- lm(PHYSICAL_DISTANCING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + PANDEMIC_FATIGUE, data = USA)
  H  <- lm(HYGIENE ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + PANDEMIC_FATIGUE, data = USA)
  MW <- lm(MASK_WEARING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + PANDEMIC_FATIGUE, data = USA)
  IS <- lm(INFORMATION_SEEKING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + PANDEMIC_FATIGUE, data = USA)
  
  # Print results 
  export_summs(PD, H, MW, IS,  model.names = c("Physical distancing", "Hygiene", "Mask wearing", "Information seeking"), error_format = "[{conf.low}, {conf.high}]")
  summ(PD, digits = 3)
  summ(H, digits = 3)
  summ(MW, digits =3)
  summ(IS, digits =3)
  APAStyler(modelTest(PD), digits = 3) # Standardized effect sizes PD 
  APAStyler(modelTest(H), digits = 3) # Standardized effect sizes H 
  APAStyler(modelTest(MW), digits = 3) # Standardized effect sizes MW 
  APAStyler(modelTest(IS), digits = 3) # Standardized effect sizes IS
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS, MW, H, PD, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",            
              m.labels = c("Information seeking - USA\n(n = 1,557)","Mask wearing - USA\n(n = 1,557)", "Hygiene - USA\n(n = 1,557)", "Physical distancing - USA\n(n = 1,557)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Pandemic fatigue","Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure S22 - OLS regressions predicting physical distancing, hygiene, mask wearing and information seeking in USA.pdf", height = 10, width = 16)
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS, MW, H, PD, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",            
              m.labels = c("Information seeking - USA\n(n = 1,557)","Mask wearing - USA\n(n = 1,557)", "Hygiene - USA\n(n = 1,557)", "Physical distancing - USA\n(n = 1,557)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Pandemic fatigue","Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(USA, PD, H, MW, IS)
  
  ########################################################################################################
  ## The Link between Behavioral/Information Fatigue and Recommended Health-Protective Behaviors - USA ##
  ######################################################################################################
  
  # Extract relevant data 
  USA <- subset(E, GENDER != "Other", select = c("GENDER", "EDUCATION", "PHYSICAL_DISTANCING", "MASK_WEARING", "HYGIENE", "INFORMATION_SEEKING", "AGE",
                                                 "BEHAVIORAL_INTENTIONS", "PANDEMIC_FATIGUE","INFORMATION_FATIGUE", "BEHAVIORAL_FATIGUE", "COGNITIVE_RISK")) 
  
  # Recode education - University yes/no
  USA$EDUCATION <-  as.character(USA$EDUCATION)
  USA$EDUCATION[USA$EDUCATION == "Other"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "Elementary-Secondary School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "High School"] <-  "University - No"
  USA$EDUCATION[USA$EDUCATION == "University"] <-  "University - Yes"
  USA$EDUCATION <-  factor(USA$EDUCATION)
  
  # Scale data 
  USA[7:12] <- scale(USA[7:12])
  
  # Regression models 
  PD <- lm(PHYSICAL_DISTANCING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + BEHAVIORAL_FATIGUE + INFORMATION_FATIGUE, data = USA)
  H  <- lm(HYGIENE ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + BEHAVIORAL_FATIGUE + INFORMATION_FATIGUE, data = USA)
  MW <- lm(MASK_WEARING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + BEHAVIORAL_FATIGUE + INFORMATION_FATIGUE, data = USA)
  IS <- lm(INFORMATION_SEEKING ~  AGE + GENDER + EDUCATION + COGNITIVE_RISK + BEHAVIORAL_FATIGUE + INFORMATION_FATIGUE, data = USA)
  
  # Print results 
  export_summs(PD, H, MW, IS,  model.names = c("Physical distancing", "Hygiene", "Mask wearing", "Information seeking"), error_format = "[{conf.low}, {conf.high}]")
  APAStyler(modelTest(PD), digits = 3) # Standardized effect sizes PD 
  APAStyler(modelTest(H), digits = 3) # Standardized effect sizes H 
  APAStyler(modelTest(MW), digits = 3) # Standardized effect sizes MW 
  APAStyler(modelTest(IS), digits = 3) # Standardized effect sizes IS
  
  # Forest plot 
  set_theme(base = theme_bw(base_size = 17))
  plot_models(IS, MW, H, PD, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",            
              m.labels = c("Information seeking - USA\n(n = 1,557)","Mask wearing - USA\n(n = 1,557)", "Hygiene - USA\n(n = 1,557)", "Physical distancing - USA\n(n = 1,557)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Information fatigue", "Behavioral fatigue", "Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  
  # Export plots to pdf 
  pdf("Figure S23 - OLS regressions predicting physical distancing, hygiene, mask wearing and information seeking in USA.pdf", height = 10, width = 16)
  plot_models(IS, MW, H, PD, show.values = TRUE, show.intercept = FALSE, vline.color = "darkgrey",            
              m.labels = c("Information seeking - USA\n(n = 1,557)","Mask wearing - USA\n(n = 1,557)", "Hygiene - USA\n(n = 1,557)", "Physical distancing - USA\n(n = 1,557)"),
              grid.breaks = .5, legend.title = "", grid = TRUE, show.legend = FALSE, wrap.labels = 200, dot.size = 3, 
              line.size = 1, value.size = 5, colors = c("#C77CFF","#C77CFF", "#C77CFF", "#C77CFF"), ci.lvl = .95,
              axis.labels = c("Information fatigue", "Behavioral fatigue", "Cognitive risk perceptions", "Education (university - yes)", "Gender (male)", "Age")) +
    scale_y_continuous(limits = c(-1,1),breaks = c(-1, -.75, -.50, -0.25,  0, 0.25, .50, .75, 1), labels = c("-1", "", "-.50", "", "0", "", ".50", "", "1"))
  dev.off()
  rm(USA, PD, H, MW, IS)
  