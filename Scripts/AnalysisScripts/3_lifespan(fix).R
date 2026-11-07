require(tidyverse)
# 加载替代代码包 以下代码已将memnet包删除
source("Scripts/AnalysisScripts/Substitute.R")

# load data
assos = read_csv('Data/InputData/risk_associations.csv')
nodes = read_csv('Data/InputData/risk_nodes.csv')
risk_asso = assos %>% filter(from_correct == 'risiko')
  
# Define age groups
groups = c("18-27","28-37","38-47","48-57","58-67","68+")

# Define colors and labels
cols = cmix(viridis::viridis(5)[c(3,2,1,5,4)], "black", c(0,0,0,.11,0))
labs = c('Threat','Investment','Analysis','Activity','Fortune')

# extract node information
sents = nodes %>% pull(sent, asso)
risk_sim = nodes %>% pull(risk_sim, asso)
clust = nodes %>% pull(cluster, asso) 

# determine average values across cluster and age
freq_mat = matrix(0, 5, 6)
sent_mat = matrix(0, 5, 6)
risk_mat = matrix(0, 5, 6)
trial_mat = matrix(0, 5, 6)
for(i in 1:6){
  tmp = risk_asso %>% 
    filter(age_group == i,
           to_correct %in% nodes$asso) %>% 
    left_join(nodes, by = c('to_correct' = "asso"))
  freq_tab = tapply(tmp$to_correct, clust[tmp$to_correct], length)
  freq_mat[,i] = freq_tab / sum(freq_tab)
  sent_tab = tapply(sents[tmp$to_correct], clust[tmp$to_correct], mean, na.rm=T)
  sent_mat[,i] = sent_tab
  risk_tab = tapply(risk_sim[tmp$to_correct], clust[tmp$to_correct], mean, na.rm=T)
  risk_mat[,i] = risk_tab
  trial_tab = tapply(tmp$trial, tmp$cluster, mean, na.rm=T)
  trial_mat[,i] = trial_tab
  }
  
# determine aggregates
agg = risk_asso %>% 
  filter(to_correct %in% nodes$asso) %>% 
  group_by(age_group) %>% 
  summarize(sent = mean(sents[to_correct], na.rm=T),
            risk = mean(risk_sim[to_correct], na.rm=T))

png('Output/Figures/5_lifespan.png',width=4,height=6,res=300,unit='in')

layout(matrix(c(1,2),nrow=2,ncol=2),height=c(.3,.65))

mtext2 = function(x) {
  corners = par("usr") 
  par(xpd = TRUE) 
  text(x = corners[2]+.5, y = mean(corners[3:4]), "Strength", srt = 270)
}

par(mar=c(3,4,1,4))
plot.new();plot.window(xlim=c(.7,6.3),ylim=c(0,.42))
sapply(seq(0,.4,.05), function(x) lines(c(.8,6.2),c(x, x), lty=1, col='grey85', lwd=.4))
for(i in 1:5) lines(freq_mat[i,],col=cols[i], lwd=3, type='b', pch=16, cex=1.5)
mtext(groups, side=1, at = 1:6,cex=.8)
mtext(seq(0,.4,.05),side=2,at=seq(0,.4,.05),las=1,cex=.8)
mtext(labs, side=4, at = freq_mat[,6],las=1,adj=0, line=-.5,cex=.8,col=cols,font=2)
mtext(c('Age group','Proportion'),side=c(1,2),cex=1,line=c(1.5,2))
mtext('A',side=3,font=2,at=-.6)

n = tapply(risk_asso$to_correct, risk_asso$age_group, length)
effects = matrix(nrow=nrow(nodes),ncol=4)
for(i in 2:nrow(nodes)){
  tmp = risk_asso %>% filter(to_correct == nodes$asso[i])
  tab = tapply(tmp$to_correct, tmp$age_group, length)[as.character(1:6)] / n
  effects[i, 1]=coef(lm(log(tab) ~ I(seq(-2.5,2.5,1))))[-1]
  effects[i, 2] = sum(!is.na(tab))
  effects[i, 3] = mean(tab,na.rm=T)
  tmp = risk_asso %>% 
    mutate(retrieved = to_correct == nodes$asso[i]) %>% 
    dplyr::select(age_group, retrieved) %>% 
    table()
  tmp = as.data.frame(tmp) %>% mutate(age_group = as.numeric(as.character(age_group)))
  m = glm(Freq~age_group*retrieved, data = tmp, family = "poisson") %>% summary()
  effects[i, 4] = m$coefficients[4,4]
}

effects =  effects %>% 
  as_tibble() %>%
  rename(linear = V1,
         groups = V2,
         freq = V3,
         sig = V4) %>% 
  mutate(word = nodes$eng,
         cluster = nodes$cluster) %>% 
  filter(!is.na(groups) & groups == 6) %>% 
  dplyr::select(word, linear, freq, cluster, sig) %>% 
  mutate(freq = rank(freq))


par(mar=c(3,3,2,2))
xlim=c(-.46,.46)
ylim=range(effects$freq)
plot.new();plot.window(xlim=xlim,
                       ylim=ylim)
sapply(seq(-.5,.5,.05), function(x) lines(c(x, x),ylim, lty=1, col='grey85', lwd=.4))
mtext(round(seq(-.45,.45,.1),2),side=1,at=seq(-.45,.45,.1),cex=.6)
lines(c(0,0),ylim,col='grey25',lty=1,lwd=.8)
cex = effects[[3]] ; cex  = (cex - 1) /( max(cex - 1)*3) ; cex = cex + .66 
text(effects$linear,effects$freq,labels=effects$word,cex=cex,col = cols[effects$cluster], font=(effects$sig<.05)+1)

mtext(c('Linear effect of age','Frequency rank'),
      side=1:2,line=c(1.5,1.2))
mtext('B',side=3,font=2,at=-.6)
dev.off()

