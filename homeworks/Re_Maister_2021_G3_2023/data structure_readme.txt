#acc_indivDiff.csv中的变量
SIM:自画像与真实照片的自我特定差异评分;
nonself_SIM:自画像与样本其他被试真实照片进行跨个体比较的评分;
SIM_controlled:自我特定差异评分与跨个体比较得分的的差异,数值等于SIM-nonself_SIM;
SE_perf:表现自尊(performance self-esteem)得分;
SE_soc:社交自尊(social self-esteem)得分;
SE_phys:外表自尊(appearance self-esteem)得分;
EXT:大五人格外倾性得分;
AGR:大五人格宜人性得分;
CON:大五人格责任心得分;
NEU:大五人格神经质得分;
OPE:大五人格开放性得分;

#EXP1_BFI.csv中的变量
id:被试号编号;
GENDER:性别,女性编码为1,男性编码为2;
SE_perf:表现自尊(performance self-esteem)得分;
SE_soc:社交自尊(social self-esteem)得分;
SE_phys:外表自尊(appearance self-esteem)得分;
SSES:状态自尊总分;
personality:大五人格中的五个维度;
BFI10_1PP:大五人格的自我评分;
BFI10_3PP_sp:外部评分者对自画像的大五人格评分;
BFI10_3PP_real:外部评分者对真实照片的大五人格评分;

#EXP1_TRAIT.csv中的变量
id:被试号编号;
GENDER:性别,女性编码为1,男性编码为2;
trait:状态自尊的三个维度;
trait_1PP:状态自尊的自我评分;
trait_3PP_sp:外部评分者对自画像的状态自尊评分;
trait_3PP_real:外部评分者对真实照片的状态自尊评分;

#gender_marix.csv
行号和列号都表示被试编号,若被试的性别相同,则编码为1,不同则编码为0;

#human_control_exp.csv
实验1b中使用40名外部评分者完成强制选择分类任务,即对出现的1张自画像的上方的两张真实面孔照片进行选择;
一共完成对所有被试（77名）的分类,对该被试分类正确准确率则为1,否则为0;
表格中每一行数据代表40名评分者能够从自画像中推断出每个被试的准确率;

#identity_marix.csv
行号和列号都表示被试编号,若被试的编号相同,则编码为1,不同则编码为0;

#portrait_vs_portrait.csv
行号和列号都表示被试编号,使用人脸识别算法计算了每个被试自画像与其他每个被试自画像的面部特征差异;
最终得到自画像的差异分数矩阵;

#RDM.csv中的变量
gender:被试性别, 女性编码为1,男性编码为2;
ppt:被试编号;
列名1-77:表示被试编号;
矩阵表示被试的自画像和单个被试的真实照片的差异分数

#real_vs_real.csv
行号和列号都表示被试编号,使用人脸识别算法计算了每个被试真实照片与其他每个被试真实照片的面部特征差异;
最终得到真实照片的差异分数矩阵;
