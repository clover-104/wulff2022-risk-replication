require(tidyverse)
# 同上一个代码
source("Scripts/AnalysisScripts/Substitute.R")

# load data
assos = read_csv('Data/InputData/risk_associations.csv')
nodes = read_csv('Data/InputData/risk_nodes.csv')
risk_asso = assos %>% filter(from_correct == 'risiko')
  
# Define age groups
groups = c("male","female")

# Define colors and labels
cols = cmix(viridis::viridis(5)[c(3,2,1,5,4)], "black", c(0,0,0,.11,0))
labs = c('Threat','Investment','Analysis','Activity','Fortune')

# extract node information
sents = nodes %>% pull(sent, asso)
risk_sim = nodes %>% pull(risk_sim, asso)
clust = nodes %>% pull(cluster, asso) 

# determine average values across cluster and age
freq_mat = matrix(0, 5, 2)
sent_mat = matrix(0, 5, 2)
risk_mat = matrix(0, 5, 2)
trial_mat = matrix(0, 5, 2)
for(i in 1:2){
  tmp = risk_asso %>% 
    filter(gender == c("male","female")[i],
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
  group_by(gender) %>% 
  summarize(sent = mean(sents[to_correct], na.rm=T),
            risk = mean(risk_sim[to_correct], na.rm=T))
agg = agg[c(2,1),]

png('Output/Figures/6_gender.png',width=4,height=6,res=300,unit='in')

layout(matrix(c(1,2),nrow=2,ncol=2),height=c(.3,.65))

mtext2 = function(x) {
  corners = par("usr") 
  par(xpd = TRUE) 
  text(x = corners[2]+.5, y = mean(corners[3:4]), "Strength", srt = 270)
}

par(mar=c(3,4,1,4))
plot.new();plot.window(xlim=c(.8,2.2),ylim=c(0,.42))
sapply(seq(0,.4,.05), function(x) lines(c(.9,2.1),c(x, x), lty=1, col='grey85', lwd=.4))
for(i in 1:5) lines(freq_mat[i,],col=cols[i], lwd=3, type='b', pch=16, cex=1.5)
mtext(groups, side=1, at = 1:2,cex=.8)
mtext(seq(0,.4,.05),side=2,at=seq(0,.4,.05),las=1,cex=.8)
mtext(labs, side=4, at = freq_mat[,2],las=1,adj=0, line=-.5,cex=.8,col=cols,font=2)
mtext(c('Gender','Proportion'),side=c(1,2),cex=1,line=c(1.5,2))
mtext('A',side=3,font=2,at=.43)

n = tapply(risk_asso$to_correct, risk_asso$gender, length)
effects = matrix(nrow=nrow(nodes),ncol=5)
for(i in 2:nrow(nodes)){
  tmp = risk_asso %>% filter(to_correct == nodes$asso[i])
  tab = tapply(tmp$to_correct, tmp$gender, length)[c('male','female')]
  tab[is.na(tab)] = 0
  effects[i, 1] = log(tab[2]/tab[1])
  effects[i, 2] = sum(!is.na(tab))
  effects[i, 3] = sum(tab,na.rm=T)
  effects[i, 4] = mean(tab/n,na.rm=T)
  tmp = risk_asso %>% 
    mutate(retrieved = to_correct == nodes$asso[i]) %>% 
    dplyr::select(gender, retrieved) %>% 
    table()
  effects[i, 5] =chisq.test(tmp)$p.value
}

effects =  effects %>% 
  as_tibble() %>%
  rename(linear = V1,
         groups = V2,
         freq = V3,
         sig = V5) %>% 
  mutate(word = nodes$eng,
         cluster = nodes$cluster) %>% 
  filter(groups == 2, freq > 10) %>% 
  dplyr::select(word, linear, freq, cluster, sig) %>%
  mutate(freq = rank(freq))


par(mar=c(3,2,2,2))
xlim=c(-1.28,1.28)
ylim=range(effects$freq)
plot.new();plot.window(xlim=xlim,
                       ylim=ylim)
sapply(seq(-1,1,.05), function(x) lines(c(x, x),ylim, lty=1, col='grey85', lwd=.4))
mtext(round(seq(-1,1,.2),2),side=1,at=seq(-1,1,.2),cex=.6)
lines(c(0,0),ylim,col='grey25',lty=1,lwd=.8)
effects$cex = effects[[3]] ; effects$cex  = (effects$cex - 1) /( max(effects$cex - 1)*3) ; effects$cex = effects$cex + .6 
with(effects %>% filter(linear > -1 & linear < 1), 
     text(linear + ifelse(word == 'no risk no fun',-.035,0),freq + ifelse(word == 'no risk no fun',2,0),
          labels=word,cex=cex,col = cols[cluster]),font=(sig<.05)+1)

with(effects %>% filter(linear > 1), 
     text(1.18,freq,labels=word,cex=cex,col = cols[cluster],font=(sig<.05)+1)) 
with(effects %>% filter(linear > 1), 
     text(1.18,freq-2.5,labels=paste0('(',round(linear,2),')'),cex=cex * .8,font=(sig<.05)+1)) 

with(effects %>% filter(linear < -1), 
     text(-1.18,freq + c(2.5,0,-2.5),labels=word,cex=cex,col = cols[cluster],font=(sig<.05)+1)) 
with(effects %>% filter(linear < -1), 
     text(-1.18,freq-2.5 + c(2.5,0,-2.5),labels=paste0('(',round(linear,2),')'),cex=cex * .8,font=(sig<.05)+1)) 

mtext(c('Female - Male','Frequency rank'),
      side=1:2,line=c(1.6,0))
mtext('B',side=3,font=2,at=-1.55)
dev.off()

