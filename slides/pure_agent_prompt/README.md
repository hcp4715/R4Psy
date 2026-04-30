# pure_agent_prompt

本文件夹存放的是提供给 **agent** 使用的 prompt 文件，而不是面向普通用户的提示词模板。

## 作用

- 存放 R4Psy 课程材料中使用的 agent-facing prompt 示例；
- 将这些 prompt 放在 `slides/` 目录下，便于与课件内容保持相邻，减少路径变动；
- 将纯 agent 指令与一般课程说明、代码示例区分开。

## 如何理解本文件夹中的文件

- 本文件夹中的 `.md` 文件应被视为 **直接写给 agent 的指令**；
- 其中的注释、约束和要求，都是为了直接约束 agent 的行为；
- 方括号中的占位符表示调用者后续需要补充的上下文信息，不应由 agent 自行猜测补全。

## 当前内容

- `llm_prompt_guide.md`：入口页，用于链接到具体的 prompt 示例文件；
- `questionnaire_example_AGENTs.md`：问卷分析场景的 agent prompt 示例；
- `experiment_example_AGENTs.md`：实验分析场景的 agent prompt 示例。

## 命名说明

文件名中带有 `_example_AGENTs.md` 的文件，表示这些文件是专门写给 agent 阅读和执行的 prompt 示例。
