# 📘 R语言在心理学研究中的应用 (R for Psychological Research)

> Graduate course materials for the School of Psychology, Nanjing Normal University.
> 南京师范大学心理学院研究生课程材料仓库。

**Last updated / 最后更新**: 2026-03-06

## 🧭 Quick Access / 快速导航

- [📖 Course Overview / 课程简介](#-course-overview--课程简介)
- [👩‍🏫 Teaching Team / 授课团队](#-teaching-team--授课团队)
- [📂 Repository Structure / 仓库结构](#-repository-structure--仓库结构)
- [🗂️ What to Edit / 编辑哪些文件](#️-what-to-edit--编辑哪些文件)
- [🚦 Source vs Rendered Policy / 源文件与产物规范](#-source-vs-rendered-policy--源文件与产物规范)
- [🛠️ Beginner Setup Guide / 新手环境配置图文](#️-beginner-setup-guide--新手环境配置图文)
- [❓ FAQ / 常见问题](#-faq--常见问题)
- [📜 License / 许可协议](#-license--许可协议)

## 📖 Course Overview / 课程简介

This repository contains slides, practice materials, demo materials, and guest lecture content for the graduate-level R course.

本仓库包含研究生课程《R语言在心理学研究中的应用》的课件、练习、演示与嘉宾讲座材料。

课程文字稿请见: [GitHub](https://github.com/hcp4715/R4PsyBook); [Bookdown电子书](https://bookdown.org/hcp4715/R4PsyBook/)

2025春季录屏：[Bilibili 列表](https://space.bilibili.com/252509184/lists/4897459)

2024春季录屏: [Bilibili 列表](https://space.bilibili.com/252509184/lists/2314135)

**2026年更新：2026年将整合vibe coding，探索在“心理学数据分析中使用R语言”这一场景下如何通过AI工具高效地从新手成长为专家**

## 👩‍🏫 Teaching Team / 授课团队

- **Instructor / 教师**: Prof. Hu Chuan-Peng (胡传鹏)
- **Contact / 联系方式**: `hcp4715 AT hotmail DOT com`
- **TA (2026 Spring)**: 孙心茹,陈思羽
- **TA (2025 Spring)**: 周方茹
- **TA (2024 Spring)**: 陈逸群, 温佳慧, 武婷婷
- **TA (2023 Spring)**: Yuki, 孙禾嘉, 蔡镇, 柏松石, 田彩玉

## 📂 Repository Structure / 仓库结构

```text
R4Psy/
├── slides/
│   ├── chapter_*.Rmd                # 主课件源文件 (xaringan)
│   ├── chapter_14.qmd               # Quarto revealjs 课件
│   ├── chapter_*.html               # 渲染后的章节HTML（保留）
│   ├── chapter_*.pptx               # 早期章节PPT
│   ├── chapter_13-*.bib             # Chapter 13 参考文献
│   ├── pure_code/chapter_*.R        # 章节纯代码脚本
│   ├── data/                        # 课件数据
│   ├── picture/                     # 课件图片资源
│   └── css/                         # 样式与 theme.scss
├── practice/                        # 练习/作业模板
├── demo/                            # 演示材料
├── guest/                           # 嘉宾讲座
├── homeworks/                       # 作业目录（按年份）
├── libs/                            # HTML共享依赖（保留）
└── README.md
```

### 🧩 Top-level Folders at a Glance / 顶层目录说明

- `slides/`: core teaching slides and shared assets / 主课件与共享素材
- `practice/`: exercises and templates / 练习与模板
- `demo/`: demonstration-only materials / 课堂演示材料
- `guest/`: guest lecture packages / 嘉宾讲座材料
- `homeworks/`: yearly homework folders / 各年度作业目录
- `libs/`: JS/CSS libs required by existing HTML / 现有HTML依赖库

## 🗂️ What to Edit / 编辑哪些文件

### ✅ Recommended to edit / 建议编辑

- `slides/chapter_*.Rmd`
- `slides/chapter_14.qmd`
- `slides/pure_code/chapter_*.R`
- `practice/*.Rmd`
- `demo/*.Rmd`
- `guest/**/code/*.Rmd`

### ⚠️ Usually do not hand-edit / 通常不要手改

- `slides/chapter_*.html` (rendered outputs)
- `libs/` (shared dependency bundles)

## 🚦 Source vs Rendered Policy / 区分源文件与渲染文件

- Keep source files as the single source of truth.
  - 以源文件为准，渲染文件由源文件生成。
- Keep chapter HTML outputs in `slides/`.
  - `slides/` 章节HTML暂时保留。
- Remove generated intermediates (`*_files/`, `output/`).
  - 中间产物目录不用上传。
- Keep `libs/` to support existing HTML without immediate re-render.
  - 为保证现有HTML可用，暂时保留 `libs/`。

## 🧑‍🏫 Teaching Workflow / 学习协作建议

### 👩‍🎓 Students / 学生

- Follow the setup guide below.
- Use fork + pull request workflow.
- Submit homework in designated yearly folders.

### 🧑‍💻 TAs / 助教

- Maintain `practice/` and assignment templates.
- Keep instructions consistent with folder layout.
- Avoid moving shared data/image paths casually.

### 👨‍🏫 Instructor / 教师

- Maintain slide sources in `slides/`.
- Maintain historical semester packages via GitHub Releases.
- Keep README as the single onboarding入口文档。

## 🛠️ Beginner Setup Guide / 新手环境配置图文

> Source: originally from `env/env_init.md`, now integrated here for easier onboarding.
> 原始来源为 `env/env_init.md`，现整合到 README 中，方便新手一步到位。

### 1) Git

#### 1.1 下载（Windows）

https://git-scm.com/downloads/win

选择 64-bit Git for Windows Setup

![image-20250307190757209](./env/assets/image-20250307190757209.png)

#### 1.2 安装

![image-20250307192748213](./env/assets/image-20250307192748213.png)

👇安装位置默认即可，路径中不要含有中文

![image-20250307192816626](./env/assets/image-20250307192816626.png)

一路按下 Next，不需要修改配置。

![image-20250307193527231](./env/assets/image-20250307193527231.png)

#### 1.3 验证

出现 git version 即安装成功。

![image-20250307193818104](./env/assets/image-20250307193818104.png)

#### 1.4 配置

- 打开 Git Bash

![image-20250307194156303](./env/assets/image-20250307194156303.png)

- 配置用户名与邮箱

```bash
git config --global user.name "Your Name"
git config --global user.email "email@example.com"
git config --global --list
```

![image-20250307194420511](./env/assets/image-20250307194420511.png)
![image-20250307194745078](./env/assets/image-20250307194745078.png)

- 生成 SSH key

```bash
ssh-keygen -t rsa -C "你的邮箱"
```

![image-20250307195053105](./env/assets/image-20250307195053105.png)
![image-20250307195105952](./env/assets/image-20250307195105952.png)

- 查看公钥

```bash
cd ~/.ssh
cat id_rsa.pub
```

![image-20250307195348204](./env/assets/image-20250307195348204.png)

- 在 GitHub 添加 SSH key

![](./env/assets/image-20250307195510975.png)
![image-20250307195606871](./env/assets/image-20250307195606871.png)
![image-20250307195724695](./env/assets/image-20250307195724695.png)
![image-20250307195917406](./env/assets/image-20250307195917406.png)

- SSH 连通性验证

```bash
ssh -T git@github.com
```

![image-20250307195949093](./env/assets/image-20250307195949093.png)
![image-20250307200855073](./env/assets/image-20250307200855073.png)

### 2) R

CRAN 镜像： https://mirrors.tuna.tsinghua.edu.cn/CRAN/

![image-20250307202508101](./env/assets/image-20250307202508101.png)
![image-20250307202620224](./env/assets/image-20250307202620224.png)

### 3) RStudio

![image-20250307203407124](./env/assets/image-20250307203407124.png)
![image-20250307203547558](./env/assets/image-20250307203547558.png)
![image-20250307203659882](./env/assets/image-20250307203659882.png)
![image-20250307203745989](./env/assets/image-20250307203745989.png)

### 4) Fork + Clone + PR Workflow

- Fork 老师仓库：<https://github.com/hcp4715/R4Psy>

![image-20250307204126256](./env/assets/image-20250307204126256.png)
![image-20250307204214450](./env/assets/image-20250307204214450.png)

- 在 RStudio 中新建 Version Control Project

![image-20250308103154502](./env/assets/image-20250308103154502.png)
![image-20250308103214321](./env/assets/image-20250308103214321.png)
![image-20250308110119700](./env/assets/image-20250308110119700.png)
![image-20250308103239969](./env/assets/image-20250308103239969.png)
![image-20250308104245304](./env/assets/image-20250308104245304.png)

- 提交并 push

![image-20250308104544472](./env/assets/image-20250308104544472.png)
![image-20250308104655642](./env/assets/image-20250308104655642.png)
![image-20250308104619551](./env/assets/image-20250308104619551.png)
![image-20250308104726895](./env/assets/image-20250308104726895.png)

- 创建 pull request

![image-20250308105505033](./env/assets/image-20250308105505033.png)
![image-20250308105526108](./env/assets/image-20250308105526108.png)

## ❓ FAQ / 常见问题

### Q1. 电脑中文用户名，SSH 密钥创建失败怎么办？

- 在 `C:/Users/` 下创建 `.ssh` 文件夹
- 以管理员权限打开 Git Bash
- 使用 `-f` 参数指定路径

```bash
ssh-keygen -t rsa -C "邮箱" -f "C:/Users/.ssh/id_rsa"
```

![image-20250310221357043](./env/assets/image-20250310221357043.png)
![image-20250310221500039](./env/assets/image-20250310221500039.png)
![image-20250310221652036](./env/assets/image-20250310221652036.png)

### Q2. Why keep `libs/` now? / 为什么暂时保留 `libs/`？

Because existing HTML files depend on it (`lib_dir`).
Current policy prioritizes stability before full re-render.

因为现有HTML课件依赖 `libs/`（`lib_dir` 指向）。
当前策略是先保证可用，再按教学节奏逐步重渲染。

## 📦 Release Policy / 发布策略

Historical semester materials are preserved via GitHub Releases.

历年学期版本通过 GitHub Releases 管理，主分支保留当前活跃教学材料。

## 📜 License / 许可协议

本仓库中的代码与文字采用 **CC-BY-4.0** 许可。

This work is licensed under the **Creative Commons Attribution 4.0 International License**.

[![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)
