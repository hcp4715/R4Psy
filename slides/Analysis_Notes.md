# Analysis Notes - RT Data Processing

## 研究问题
计算 match-moral 条件下的自我优势效应(SPE)

## 数据来源
- 文件位置：slides/data/match/
- 文件格式：.out 文件

## 关键变量
- Sub: 被试编号
- Shape: 刺激类型（moralSelf, moralOther, immoralSelf, immoralOther）
- Match: 匹配条件（match, mismatch）
- RT: 反应时（ms）
- ACC: 正确率（0, 1）

## 计算逻辑
1. 筛选有效试次：ACC == 0|1 & RT in [200, 1500]
2. 按被试和条件计算平均 RT
3. 筛选 match + moral 条件
4. 长转宽：Self 和 Other 分别作为列
5. SPE = Self RT - Other RT