# R语言在心理学研究中的应用 (R for psychological research)

## 关于(About)

This is a repo for the coming R course for graduate students, School of Psychology, Nanjing Normal University, Nanjing, China.

Instructor: Prof. Hu Chuan-Peng

Teaching assistants: Yuki; Hejia Sun; Zheng Cai; Songshi Bai; Caiyu Tian

此仓库为南京师范大学心理学研究生课程，2023年春季学期的授课内容转为文字稿，见https://github.com/hcp4715/R4PsyBook。

教师：胡传鹏(hcp4715 AT hotmail DOT com)

助教：yuki; 孙禾嘉；蔡镇；柏松石；田彩玉


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

## 课程大纲 (Syllabus)

#### 第一讲：为什么要学习R（3学时）

1.1 R在心理科学及社会科学中的运用

1.2 R语言使用的示例展示

1.3 课程安排

1.4 如何学好这门课

#### 第二讲：如何开始使用R:（3学时）

2.1 要解决的数据分析问题简介？

2.1 如何安装？

2.2 如何方便使用？Rstudio的安装与界面介绍

#### 第三章：如何导入数据（3学时）

3.1 路径与工作目录

3.2 读取数据

3.3 了解R里的数据 （R语言中的对象）

#### 第四章：如何清理数据一 R语言编程基础（3学时）

4.1 R对象的操控

4.2 逻辑运算

4.3 函数

#### 第五章：如何清理数据二 数据的预处理（3学时）

5.1 数据预处理准备

5.2 数据预处理的基本操作

5.3 数据预处理的进阶操作

#### 第六章：如何探索数据: 描述性统计与数据可视化基础（3学时）

6.1 描述性统计

6.2 ggplot2的基本使用

6.3 探索性数据分析(DataExplorer)

#### 第七章：如何进行基本的数据分析: *t*-test和anova（3学时）

7.1 语法实现

7.2 分析的流程

#### 第八章：如何进行基本的数据分析: 相关与回归（3学时）

8.1 语法实现

8.2 分析的流程

#### 第九章：如何进行基本的数据分析: 中介分析（3学时）

9.1 语法实现

9.2 分析的流程

#### 第十章：Git、Github与代码的规范化（3学时）

10.1. Git与GitHub

10.2 项目、文件与代码的规范化

#### 第十一章: 如何得到可发表的图像: 数据可视化进阶（3学时）

11.1 ggplot2的图层与面板控制

11.2 ggplot2与其他工具的结合

#### 第十二章：从分析到手稿（3学时）

12.1 papaja工具包的简介及使用

#### 第十三章：基于估计的统计：Effect size与Meta-analysis（3学时）

13.1 Effect size

13.2 Meta-analysis

#### 第十四章：计划样本量：Power analysis （3学时）

14.1 什么是Power

14.2 G\*Power中的曲线如何得来？以*t*-test为例 （模拟）

14.3 如何计划分析方法：以ANOVA为例（模拟）

#### 第十五章：如何让导师/合作者完全重复我的分析？（3 学时，学期结束，无法讲到了）

15.1 软件版本记录

15.2 容器技术与docker的使用