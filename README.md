# R语言在心理学研究中的应用 (R for psychological research)

## 关于(About)

This is a repo for the R course for graduate students, School of Psychology, Nanjing Normal University, Nanjing, China.

Instructor: Prof. Hu Chuan-Peng

Teaching assistants (2024 Spring): Yiqun Chen; Jiahui Wen; Tingting Wu

Teaching assistants (2023 Spring): Yuki; Hejia Sun; Zheng Cai; Songshi Bai; Caiyu Tian

此仓库为南京师范大学心理学研究生课程相关课件，授课内容所转为文字稿，见[https://github.com/hcp4715/R4PsyBook](https://github.com/hcp4715/R4PsyBook); 2024年春季课程的录屏见[这里](https://space.bilibili.com/252509184/lists/2314135)

教师：胡传鹏博士(hcp4715 AT hotmail DOT com)

助教(2025 Spring)：周方茹

助教(2024 Spring)：陈逸群; 温佳慧；武婷婷

助教(2023 Spring)：yuki; 孙禾嘉；蔡镇；柏松石；田彩玉


## 版权与许可(License)

本仓库中的代码与文字，均由胡传鹏教授与助教团队所创建，采用CC-BY-4.0的版本许可，如需要使用，请引用本仓库网址。

本仓库内容对资料出处均进行详细引用，如果侵权，请随时联系。

Shield: [![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)

This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

[![CC BY 4.0](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)

## 本Repo的文件夹结构(folder structure of this repo)

```         
root_dir
|
|----chapter_1.pptx   # slides for chapter 1
|----chapter_2.pptx   # slides for chapter 2
|----chapter_3.Rmd    # Rmarkdown for chapter 3
|----chapter_3.html   # html for chapter 3
|----chapter_4.Rmd    # Rmarkdown for chapter 4
|----chapter_4.html   # html for chapter 4
|----chapter_5.Rmd    # Rmarkdown for chapter 5
|----chapter_5.html   # html for chapter 5
|----chapter_6.Rmd    # Rmarkdown for chapter 6
|----chapter_6.html   # html for chapter 6
|----chapter_7.Rmd    # Rmarkdown for chapter 7
|----chapter_7.html   # html for chapter 7
|---- ...
|----Demo.Rmd         # Rmarkdown for demostration
|
|----css/             # folder for Xaringan
|----data/            # folder for data used in the lecture
|     |----- match    # folder for match data
|     |----- penguin  # folder for penguin data
| 
|----libs/            # folder for Xaringan
|
|----output/          # folder for Xaringan output?
|
|----picture/         # folder for picture in html
|     |----- chp3     # folder for pictures in chapter 3
|     |----- chp4     # folder for pictures in chapter 4
|     |----- ...
|
|....
```

## 课程大纲

|      | 主题  | 子主题 |
|------|------|--------|
| 第一讲 | 为什么要学习R | 1.1 R在心理科学及社会科学中的运用<br>1.2 R语言使用的示例展示<br>1.3 课程安排<br>1.4 如何学好这门课 |
| 第二讲 | 开始使用R | 2.1 数据与问题<br>2.2 如何安装？<br>2.3 如何方便使用？Rstudio的安装与界面介绍<br>2.4 和鲸平台 |
| 第三讲 | 利用好本课程资源--Git与Github | 3.1 Git与GitHub<br>3.2 项目、文件与代码的规范化 |
| 第四讲 | 导入数据 | 4.1 路径与工作目录<br>4.2 读取数据<br>4.3 初识R语言的对象 |
| 第五讲 | 清理数据(一)--R语言编程基础 | 5.1 R对象的操控<br>5.2 逻辑运算<br>5.3 函数 |
| 第六讲 | 清理数据(二)--数据的预处理 | 6.1 `tidyverse`简介<br>6.2 问卷数据预处理：基本操作<br>6.3 数据预处理的进阶操作 |
| 第七讲 | 清理数据(三)--数据的预处理 | 7.1 认知实验数据预处理<br>7.2 函数(function) |
| 第八讲 | 探索数据 -- 描述性统计与数据可视化基础 | 7.1 描述性统计<br>7.2 探索性数据分析(`DataExplorer`)<br>7.3 `ggplot2`的基本语法 |
| 第九讲 | R语言中回归模型(一) -- 基本回归模型与常见统计检验(*t*-test/ANOVA) | 8.1 语法实现<br>8.2 分析的流程 |
| 第十讲 | R语言中回归模型(二) -- 层级线性模型 | 9.1 语法实现<br>9.2 分析的流程 |
| 第十一讲 | R语言中回归模型(三) -- 广义线性模型 | 10.1 语法实现<br>10.2 分析的流程 |
| 第十二讲 | 获得可发表的图像 -- 数据可视化进阶 |  |
| 第十三讲 | 专题1: 从代码到论文 -- papaja包介绍 |  |
| 第十四讲 | 特邀报告(待定) |  |
| 第十五讲 | 特邀报告(待定) |  |
| 第十六讲 | 大作业 |  |
