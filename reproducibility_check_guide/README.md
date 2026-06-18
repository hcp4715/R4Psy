# 计算可复现性检验指南文档体系

本文件夹把《心理学院 R 编程语言分析与计算可复现性检验指南》整理成一套便于后续 Agent 直接阅读和执行的 Markdown 文档。当前任务只准备文档体系，不开始复现任何具体论文。

## 文件说明

| 文件 | 用途 |
|---|---|
| `00_original_full_text.md` | Word 指南的完整转换稿，是后续判断课程要求的原始依据。 |
| `media/` | Word 中提取出的图片，供 `00_original_full_text.md` 引用。 |
| `README.md` | 说明本文件夹用途、文件关系和推荐阅读顺序。 |
| `01_agent_execution_guide.md` | 面向 Agent 的执行指南，把原指南整理成可操作步骤和判断规则。 |
| `02_report_template.md` | 最终报告模板，包含可填写的 Markdown 表格。 |
| `03_checklist_and_rubric.md` | 交付前自查清单和等级式评价标准。 |

## 推荐阅读顺序

1. 先读 `README.md`，了解文件夹结构。
2. 再读 `01_agent_execution_guide.md`，明确复现任务的流程、规则和交付物。
3. 遇到课程要求不清楚时，回到 `00_original_full_text.md` 查原文。
4. 写最终报告时，使用 `02_report_template.md`。
5. 交付前，使用 `03_checklist_and_rubric.md` 逐项自查。

## 后续 Agent 使用方式

后续 Agent 在完成一篇心理学量化研究的计算可复现性检验时，应先根据 `01_agent_execution_guide.md` 检查输入材料、确定文献和研究假设，再进行数据预处理、描述性统计复现、推断性统计复现和结果比较。

写报告时，应以 `02_report_template.md` 为主体，把原文献报告值、复现值、PE / δ、评级和推论一致性并列填写。完成后，应使用 `03_checklist_and_rubric.md` 检查文件是否齐全、代码是否可运行、结果是否可追溯、无法复现之处是否如实记录。

若 `01_agent_execution_guide.md` 与 `00_original_full_text.md` 的理解出现冲突，应以 `00_original_full_text.md` 为准，并在报告或工作记录中说明判断依据。
