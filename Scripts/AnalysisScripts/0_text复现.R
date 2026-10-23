# 0_text.R —— 复现论文图 1：文本方法与联想法对 risk 的联想词对比
#
# 用法：把工作目录设为仓库根目录（wulff2022-risk-replication/），再运行本脚本。
# 依赖：tidyverse、Rcpp、RcppArmadillo、viridis，以及 Rtools（用于编译 helper.cpp）
# 输入：Data/InputData/ 下的 swow_risk_associates.csv、word2vec_risk_vectors.txt、sentiment_en.txt
# 输出：Output/Figures/Figure1_text_vs_association.pdf

library(tidyverse)
Rcpp::sourceCpp('Scripts/AnalysisScripts/helper.cpp')

swow_risk = read.csv('Data/InputData/swow_risk_associates.csv')%>%
  pull(frequency,word)
head(swow_risk,5)

word2vec_risk = read.table('Data/InputData/word2vec_risk_vectors.txt')
dim(word2vec_risk)

word2vec_risk_cos = word2vec_risk[-1]%>% as.matrix() %>%arma_cosine()
rownames(word2vec_risk_cos)=colnames(word2vec_risk_cos)=word2vec_risk[[1]]

sim_word2vec=word2vec_risk_cos['risk', colnames(word2vec_risk_cos)!='risk']%>%
  sort(decreasing = TRUE)
sim_swow = log(swow_risk[names(sim_word2vec)] + 1) %>%
  sort(decreasing = TRUE)

sim_word2vec_top10 = sim_word2vec[1:10]
sim_swow_top10 = sim_swow[1:10]

sim_word2vec_top10
sim_swow_top10


sent = read_delim('Data/InputData/sentiment_en.txt', delim="\t",col_names = F)%>%
  mutate(X1 = str_replace_all(X1, "#[:alpha:]+",""))%>%
  pull(X2,X1)

head(sent, 5)
length(sent)
sent['danger']

cols = colorRampPalette(viridis::cividis(11)[c(2,10)])(141)

pdf('Output/Figures/Figure1_text_vs_association.pdf',width = 5,height = 5)
par(mar=c(1,0,2,0))
plot.new()
plot.window(xlim = c(.5,2.5),ylim = c(.5,10.5))

w = rev(sim_word2vec_top10)
w = 20*w/sum(w)
s=round(sent[names(sim_word2vec_top10)%>%rev()]*100)
text(1, 1:10, labels = rev(names(sim_word2vec_top10)), cex = w, col = cols[s+71], font = 2)


w = rev(sim_swow_top10)
w = 20 * w / sum(w)
s = round(sent[names(sim_swow_top10) %>% rev()] * 100)
text(2, 1:10, labels = rev(names(sim_swow_top10)), cex = w, col = cols[s+71], font = 2)

mtext(c(expression(underline("Text-based")),
        expression(underline("Association-based"))),
      at=1:2, font=1,cex=1.5)
mtext("Clover",side=1,line = 0,adj = 0,col = "green",cex=0.7)

dev.off()
