# SE_data.csv：Experiment 1 原始数据
# SE_data.Rda：与SE_data.csv相同，用于R数据分析

subjectGroup：被试组别，包括1-24组；

subject：被试编号；

age：被试年龄，取值在18-28岁之间；

sex：被试性别，male(男性)，famale(女性)；

education：受教育水平，包括Bachelor degree(学士学位)、College or Technical school(大学或技术学校)、High school or equivalent(高中或同等学历)、Master/Doctorate(硕士/博士学位)；

handedness：利手，ambidextrous(双手均利手), right-handed(右利手)；

TaskName：任务类型，包括形状-标签匹配训练任务：PMT1, PMT2(按键相反), 分类任务：RG1YH（配对：自我-快乐/朋友-中性）, RG1YN（配对：自我-中性/朋友-快乐）, RG2YH（按键相反）RG2YN（按键相反）

BlockNumber：区块号，1,2,3；

stimPos：刺激呈现位置；包括center(中心),-400(中心左侧400), 400(中心右侧400), 0 -250; 0 250(中心左侧250，中心右侧250各1刺激), 0 50(中心右侧50), 400;-400(中心左侧400，中心右侧400各一刺激)；

stim1：刺激，diamond(菱形), kite(风筝型), octagon(八边形)，pentagon(五边形), rhomboid(平行四边形)，square(正方形), trapeze(梯形), triangle(三角形)；

stim2：刺激，diamond(菱形), kite(风筝型), octagon(八边形)，pentagon(五边形), rhomboid(平行四边形)，square(正方形), trapeze(梯形), triangle(三角形), label_friend(朋友_文字标签_PMT任务), label_happy(开心_文字标签_PMT任务), label_neutral(中性_文字标签_PMT任务), label_you(你_文字标签_PMT任务);

shape：diamond(菱形), kite(风筝型), octagon(八边形)，pentagon(五边形), rhomboid(平行四边形)，square(正方形), trapeze(梯形), triangle(三角形), friend(朋友), friendhappy(朋友_快乐), friendneutral(朋友_中性), happy(快乐), happyfriend(快乐_朋友), happyyou(快乐_自我), neutral(中性), neutralfriend(中性_朋友), neutralyou(中性_自我), you(自我), youhappy(自我_快乐), youneutral(自我_中性);

word：标签， label_friend(朋友_文字标签_PMT任务), label_happy(开心_文字标签_PMT任务), label_neutral(中性_文字标签_PMT任务), label_you(你_文字标签_PMT任务);

MATCHkey：按键，PMT1按键: c, x, m, n; Classification Task按键: Yhappy_G, Yhappy_h, Yneutral_j,  Yneutral_k;

RT：反应时；

correct：正确率，其中0为反应错误，1为反应正确；

question：问题；

response：被试按键，c, x, m, n, g, h, j, k；

condition：条件，match(匹配), mismatch(不匹配), pair(成对), single(单个)；

TaskType：任务类型，PMT1(形状-标签匹配训练任务), PMT2(形状-标签匹配训练任务), RG_congruent(形状-标签反应一致), RG_incongruent(形状-标签反应不一致)；

Association：联结，同shape, 包括diamond(菱形), kite(风筝型), octagon(八边形)，pentagon(五边形), rhomboid(平行四边形)，square(正方形), trapeze(梯形), triangle(三角形), friend(朋友), friendhappy(朋友_快乐), friendneutral(朋友_中性), happy(快乐), happyfriend(快乐_朋友), happyyou(快乐_自我), neutral(中性), neutralfriend(中性_朋友), neutralyou(中性_自我), you(自我), youhappy(自我_快乐), youneutral(自我_中性)