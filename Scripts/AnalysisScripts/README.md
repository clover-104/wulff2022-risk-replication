这是分析脚本：

读取 Data/InputData/ 中的数据 生成报告中的图与表 输出到 Output/ 

0_text复现.R————论文图1 自己对照原代码复现所得 于左下角加了自己署名

1_network（fix）.R————论文图3、4 对作者原代码补充修复所得 使其能够正常运行 但是由于聚类随机性未得到与作者一样的结果图
自己的聚类结果输出到 Data/IntermediateData/risk_nodes.csv

2_multiling(fix).R————论文图5 需先下载 SWOW_EN.csv 与 SWOW_DU.csv 放入 Data/InputData/

3_lifespan(fix).R————论文图6

4_gender(fix).R————论文图7

2、3、4 这三个读的是 Data/InputData/risk_nodes.csv 也就是作者的聚类结果 所以产出图和作者一致

Substitute.R————替换 f.utils 与 memnet 两个不可得的包里的函数

helper.cpp————作者提供的 C++ 函数 运行代码时需要编译
