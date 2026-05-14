# R4Psy 课程 Agent 示例指令书（实验类）

## 使用规则

```text
# 你将收到一个实验数据分析任务。先读取并遵守下面所有约束，再开始回答。
# 把方括号中的占位符视为调用者随后会补充的任务上下文；不要自行补全未提供的信息。
# 正文只保留你需要执行或遵守的指令；补充说明只保留在注释里。
# 包、路径、变量名优先与课件保持一致；不要擅自更换包、改写路径或重命名变量。
# 默认流程：先澄清关键前提，再给代码；如果任务是多步骤，先给计划，再分步实现。
```

---

## 1. 批量导入与清洗实验数据

```text
# 适用场景：多个 .out / .csv 文件；试次级数据；需要先合并，再清洗。
# 课件中常见操作：list.files()、read.table()、lapply()/for loop、bind_rows()、filter()、drop_na()。
# 推荐包：tidyverse；如果已有课件路径，优先保持 here::here("slides", "data", "match", ...)。

你现在要协助处理实验数据。先进入 Planning 模式，不要直接写代码。

已知数据：
- 文件目录：[here::here(...) 或实际目录]
- 文件格式：[.out / .csv / 其他]
- 关键变量：[例如 Sub, Shape, Match, ACC, RT]

目标：
1. 批量读取所有文件；
2. 合并为一个数据框；
3. 统一变量类型；
4. 排除无效试次；
5. 输出清洗后的试次级数据。

执行约束：
- 请使用 tidyverse；
- 如果需要 read.table() / list.files() / bind_rows()，请优先沿用课件写法；
- 每一步都加注释；
- 先追问筛选标准（例如 ACC、RT、Hand、Date 等）再写代码；
- 在计划确认前，不要直接给完整实现。
```

## 2. 分条件汇总、长宽转换与效应指标

```text
# 适用场景：已经拿到清洗后的试次级数据，需要按被试与条件汇总，再转宽表并计算指标。
# 课件中常见指标：mean_RT、mean_ACC、SPE；常见函数：group_by()、summarise()、extract()、pivot_wider()、mutate()。

你将接收一个已经清洗好的实验数据框 [数据框名]。

请只完成下面这些步骤，不要继续做未要求的后续分析：
1. 按 [被试变量] 和 [条件变量] 计算 [mean_RT / mean_ACC / 其他指标]；
2. 如果需要，从 [原变量名] 中拆出 [新变量名 1] 和 [新变量名 2]；
3. 将长数据转成宽数据；
4. 计算 [目标效应指标，例如 moral_SPE]。

执行约束：
- 请使用 tidyverse；
- 先说明每一步的输入和输出；
- 再给代码；
- 如果某一步依赖上一步的数据结构，请明确写出来；
- 不要越级继续做未要求的后续分析。
```

## 3. 实验类 EDA 与可视化

```text
# 适用场景：试次级或按条件汇总后的实验数据；关注 RT、ACC、条件差异、被试差异。
# 推荐包：ggplot2（tidyverse 内）；如果需要课件风格，可补 bruceR::theme_bruce()。

你现在要对实验数据做 EDA / 可视化。

已知数据：
- 数据框：[数据框名]
- x 变量：[变量名 + 类型]
- y 变量：[变量名 + 类型]
- 分组变量：[变量名 + 类型；如果没有就写“无”]
- 数据层级：[试次级 / 被试汇总级 / 条件汇总级]

目标：
- 请先判断最合适的图形类型；
- 再给一个基础的 ggplot2 版本；
- 如果当前图不足以支持结论，请直接指出。

执行约束：
- 先解释为什么选这类图；
- 再给代码；
- 如果需要先聚合再画图，请明确说明；
- 不要把试次级数据和汇总级数据混在同一层解释。
```

## 4. 实验类报错排查

```text
# 适用场景：批量读取失败、列名不一致、类型转换失败、group_by/summarise 输出不对、pivot_wider 后结构异常。

你现在要排查一个实验数据处理流程中的报错。

当前代码：
[粘贴最小可复现代码]

报错信息：
[原样粘贴]

数据与任务信息：
- 文件来源：[目录或文件名模式]
- 当前数据框：[数据框名]
- 关键变量：[变量名列表]
- 我当前所在步骤：[批量读取 / 清洗 / 汇总 / 拆分变量 / 转宽 / 计算指标]
- 我原本想得到的结果：[预期输出]

请按下面顺序处理：
1. 先判断错误更可能来自哪一步；
2. 再解释报错含义；
3. 给出最小修复方案；
4. 告诉我修复后应该检查什么结果。

请不要直接重写整套分析流程。
```

## 5. 使用 `analysis_notes.md` 约束实验分析逻辑

```text
# 适用场景：实验分析步骤较长，包含筛选规则、变量定义、计算顺序、长宽转换规则。
# 推荐做法：如果沿用课件示例文件名，先读取 analysis_notes.md，再基于其中的约束继续当前小任务。

Read analysis_notes.md first.

This file defines the research question, data source, key variables, and analysis logic.
Use it as the binding specification for the current R analysis step.

Current task:
[只写当前这一步，例如“按被试和条件计算平均 RT”]

Requirements:
- Only complete this step;
- Explain the input and expected output first;
- Then give the code;
- If analysis_notes.md is ambiguous or inconsistent, point it out before coding.
```

---

## 提交前自检

```text
# 1. 路径是否与仓库一致（优先 here::here("slides", "data", ...)）。
# 2. 包是否与课件一致（实验优先 tidyverse，必要时沿用课件中的 read.table / list.files / bind_rows / pivot_wider）。
# 3. 变量名、条件名、筛选标准、数据层级是否已经明确写入任务上下文。
# 4. 是否要求你先澄清再写代码；多步骤任务是否要求你先给计划。
# 5. 是否把解释性内容压缩到注释中，而不是散落在正文。
```
