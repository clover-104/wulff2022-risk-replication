# 注意 跑此脚本前请先下载SWOW数据！
require(tidyverse)
require(igraph)
#没用到 好像也没有这个文件 因此省略
#Rcpp::sourceCpp("2 Code/_helper/jaccard.cpp")

# get nodes
nodes = read_csv("Data/InputData/risk_nodes.csv")

# process SWOW ----

# get english swow
swow_en = read_csv("Data/InputData/SWOW_EN.csv") %>% 
  mutate(cue = str_to_lower(cue))

# get dutch swow
swow_du = read_delim("Data/InputData/SWOW_DU.csv",delim=";")

# get dutch translations
dutch = openxlsx::read.xlsx("Data/InputData/translations_dutch.xlsx") %>% 
  mutate_at(2:3,function(x) str_to_lower(str_trim(x))) %>% 
  mutate(du_swow = ifelse(deepl %in% str_to_lower(swow_du$cue), deepl, ifelse(google %in% str_to_lower(swow_du$cue), google, NA)))

# combine
nodes = nodes %>% 
  mutate(eng_swow = ifelse(eng %in% str_to_lower(swow_en$cue), eng, NA)) %>% 
  left_join(dutch %>% select(orig, du_swow), by = c("asso" = "orig")) %>% 
  mutate(cluster_recode = case_when(
    cluster == 1 ~ 4,
    cluster == 2 ~ 3,
    cluster == 3 ~ 5,
    cluster == 4 ~ 2,
    cluster == 5 ~ 1))


# cluster frequencies -----

en_clust = nodes %>% filter(!is.na(eng_swow)) %>%  pull(cluster, eng_swow)
du_clust = nodes %>% filter(!is.na(du_swow)) %>%  pull(cluster, du_swow)
de_clust = nodes %>% pull(cluster, asso)

risk_en = swow_en %>% 
  filter(cue == "risk") %>% 
  select(R1, R2, R3) %>% 
  mutate(R1 = en_clust[R1],
         R2 = en_clust[R2],
         R3 = en_clust[R3])
tab_en = table(risk_en %>% unlist())
tab_en = tab_en / sum(tab_en)

# 1 = threat
# 2 = finance
# 3 = anlaysis
# 4 = activity
# 5 = fortune

risk_du = swow_du %>% 
  filter(cue == "risico") %>% 
  select(asso1, asso2, asso3) %>% 
  mutate(asso1 = du_clust[asso1],
         asso2 = du_clust[asso2],
         asso3 = du_clust[asso3])
tab_du = table(risk_du %>% unlist())
tab_du = tab_du / sum(tab_du)

# read in risk associations
assos = read_csv('Data/InputData/risk_associations.csv')

risk_de = assos %>% 
  filter(trial %in% 1:5) %>% 
  select(subject, trial, to_correct) %>% 
  filter(!duplicated(cbind(subject, trial))) %>% 
  pivot_wider(names_from = trial, values_from = to_correct) %>% 
  select(-subject) %>% 
  mutate(R1 = de_clust[`1`],
         R2 = de_clust[`2`],
         R3 = de_clust[`3`],
         R4 = de_clust[`4`],
         R5 = de_clust[`5`]) %>% 
  select(R1:R5)
tab_de = table(risk_de %>% unlist())
tab_de = tab_de / sum(tab_de)


tab_de
tab_du
tab_en

cor(tab_de, tab_en)
cor(tab_de, tab_du)
cor(tab_en, tab_du)


# # ENGLISH ----
# 
# swow_en_sel = swow_en %>% 
#   filter(cue %in% nodes$eng_swow) %>% 
#   pivot_longer(c(R1, R2, R3), names_to = "response", values_to = "asso") %>% 
#   select(id, cue, response, asso)
# 
# # get all cues and responses
# cues = unique(swow_en_sel$cue)
# resp = unique(swow_en_sel$asso)
# 
# # create association matrix
# asso_mat = matrix(0, nrow=length(cues), ncol = length(resp))
# rownames(asso_mat) = cues
# colnames(asso_mat) = resp
# 
# # fill matrix
# for(i in 1:length(cues)){
#   
#   # keep track
#   if(i %% 100 == 0) print(i)
#   
#   # fill matrix
#   tab = table(swow_en_sel[swow_en_sel$cue == cues[i], "asso"])
#   asso_mat[i, names(tab)] = as.numeric(tab)
#   
# }
# 
# # get logarithm
# log_asso_mat = log(asso_mat + 1)
# 
# # determine jaccard matrix
# jacc_mat = jacc_abs_mat = matrix(nrow = length(cues), ncol = length(cues))
# rownames(jacc_mat) = colnames(jacc_mat) = rownames(jacc_abs_mat) = colnames(jacc_abs_mat) = cues
# for(i in 1:(length(cues)-1)){
#   print(i)
#   for(j in (i+1):length(cues)){
#     jacc = jaccard_cpp(log_asso_mat[i,], log_asso_mat[j,])
#     jacc_mat[i,j] = jacc_mat[j,i] = jacc
#   }
# }
# 
# saveRDS(jacc_mat, "1_data/risk_similarity_mat_english.RDS")
# 
# # DUTCH ----
# 
# swow_du_sel = swow_du %>% 
#   filter(cue %in% nodes$du_swow) %>% 
#   pivot_longer(c(asso1, asso2, asso3), names_to = "response", values_to = "asso") %>% 
#   select(ctr, cue, response, asso)
# 
# # get all cues and responses
# cues = unique(swow_du_sel$cue)
# resp = unique(swow_du_sel$asso)
# 
# # create association matrix
# asso_mat = matrix(0, nrow=length(cues), ncol = length(resp))
# rownames(asso_mat) = cues
# colnames(asso_mat) = resp
# 
# # fill matrix
# for(i in 1:length(cues)){
#   
#   # keep track
#   if(i %% 100 == 0) print(i)
#   
#   # fill matrix
#   tab = table(swow_du_sel[swow_du_sel$cue == cues[i], "asso"])
#   asso_mat[i, names(tab)] = as.numeric(tab)
#   
# }
# 
# # get logarithm
# log_asso_mat = log(asso_mat + 1)
# 
# # determine jaccard matrix
# jacc_mat = jacc_abs_mat = matrix(nrow = length(cues), ncol = length(cues))
# rownames(jacc_mat) = colnames(jacc_mat) = rownames(jacc_abs_mat) = colnames(jacc_abs_mat) = cues
# for(i in 1:(length(cues)-1)){
#   print(i)
#   for(j in (i+1):length(cues)){
#     jacc = jaccard_cpp(log_asso_mat[i,], log_asso_mat[j,])
#     jacc_mat[i,j] = jacc_mat[j,i] = jacc
#   }
# }
# 
# saveRDS(jacc_mat, "1_data/risk_similarity_mat_dutch.RDS")


# SIM MATRICES ----

# get similarity mats
jacc_mat_de = readRDS('Data/InputData/risk_similarity_mat.RDS')
jacc_mat_en = readRDS("Data/InputData/risk_similarity_mat_english.RDS")
jacc_mat_du = readRDS("Data/InputData/risk_similarity_mat_dutch.RDS")

# get containers
jacc_mat_de_en = jacc_mat_de_du = matrix(nrow = nrow(jacc_mat_de),
                                         ncol = ncol(jacc_mat_de))

# align similarity mats
for(i in 1:306){
  for(j in (i+1):307){
    
    n_i = nodes$eng_swow[i]
    n_j = nodes$eng_swow[j]
    if(!is.na(n_i) & !is.na(n_j)){
      jacc_mat_de_en[i, j] = jacc_mat_de_en[j, i] = jacc_mat_en[n_i, n_j]
      }
    
    n_i = nodes$du_swow[i]
    n_j = nodes$du_swow[j]
    if(!is.na(n_i) & !is.na(n_j)){
      jacc_mat_de_du[i, j] = jacc_mat_de_du[j, i] = jacc_mat_du[n_i, n_j]
      }
    
    }
  }

# get averages
de = en = du = matrix(nrow=5, ncol=5)
rownames(de) = rownames(en) = rownames(du) = c("F","Ac","I","T","An") 
colnames(de) = colnames(en) = colnames(du) = c("F","Ac","I","T","An") 
for(i in 1:5){
  for(j in 1:5){
    sel_i = nodes$cluster_recode == i
    sel_j = nodes$cluster_recode == j
    de[i, j] = de[j, i] = jacc_mat_de[sel_i, sel_j] %>% mean(na.rm=T)
    en[i, j] = en[j, i] = jacc_mat_de_en[sel_i, sel_j] %>% mean(na.rm=T)
    du[i, j] = du[j, i] = jacc_mat_de_du[sel_i, sel_j] %>% mean(na.rm=T)
    }
  }
# 应该先定义cs 应该是作者环境问题
cs = viridis::viridis(5)[c(3,2,1,5,4)] %>% rev()
names(cs) = c("F","Ac","I","T","An") 

# get pairs
get_pair = function(x, y) {
  tab = cbind(expand.grid(x = 1:nrow(x), y = 1:ncol(x)), 
              expand.grid(xnam = rownames(x), ynam = colnames(x)), 
              val_1 = c(x), val_2 = c(y))
  tab = tab[tab$x >= tab$y,]
  tab$nam = paste0(tab$ynam, "-", tab$xnam)
  tab$col = ifelse(tab$ynam == tab$xnam, cs[tab$xnam], "black")
  tab[,c("val_1", "val_2", "nam", "col")]
  }
pairs = list(get_pair(de, du), get_pair(de, en), get_pair(du, en))

# plot settings
cs = viridis::viridis(5)[c(3,2,1,5,4)] %>% rev()
names(cs) = c("F","Ac","I","T","An") 
xlims = list(c(0,.04),c(0, .04),c(0, .04))
ylims = list(c(.01,.04),c(.01, .04),c(.01, .04))
labs = list(c("German","Dutch SWOW"),c("German","English SWOW"), c("Dutch SWOW","English SWOW"))


# plot
png("Output/Figures/3_multiling.png",width=9,height=8,res=300,unit='in')

top = list(jacc_mat_de, jacc_mat_de_du, jacc_mat_de_en)
bot = list(de, du, en)

top_mar = list(c(0,7,2,0),c(0,0,2,0),c(0,0,2,0))
bot_mar = list(c(2,7,0,0),c(2,0,0,0),c(2,0,0,0))
bot2_mar = list(c(3,7,0,0),c(3,0,0,0),c(3,0,0,0))

s1 = c(1, cumsum(table(nodes$cluster_recode)))
s2 = c(1, cumsum(rev(table(nodes$cluster_recode))))
 
layout(matrix(1:9, nrow=3),width=c(3,2.2,2.2))

base_col = viridis::cividis(10)[10:2]

labels = c("Fortune","Activity","Investment","Threat","Analysis") %>% rev()
lab_col = viridis::viridis(5)[c(3,2,1,5,4)]

lang = c("German","Dutch SWOW","English SWOW")


for(i in 1:3){

  # top

  par(mar=top_mar[[i]])
  
  plot.new();plot.window(xlim=c(.5,307.5),ylim=c(.5,307.5))
  
  if(i == 1) mtext(labels, side=2, las=1, adj=1, font=2,
                   at = s2[-6] + diff(s2)/2, col = lab_col, cex=.9)
  mtext(lang[i], side=3, font=1, cex=1.2)
  
  
  ord = nodes$cluster_recode %>% order()
  mat = top[[i]][ord, ord]
  
  pos = expand.grid(x = 1:nrow(mat),
                    y = 1:nrow(mat))
  pos$y = 308 - pos$y
  
  val = (c(mat**.7) * 100) %>% round()
  col = colorRampPalette(base_col)(max(val,na.rm=T)+1)
  cols = col[val + 1]
  cols[is.na(cols)] = "grey80" #base_col[1]
  
  w = .5
  rect(pos[,1] - w, pos[,2] - w, 
       pos[,1] + w, pos[,2] + w,
       col = cols, border = NA)
  
  for(j in 1:5){
    for(k in 1:5){
      rect(s1[j], s2[k], s1[j+1], s2[k+1], border="white")
    }
  }
  
  # bottom
  
  par(mar=bot_mar[[i]])
  
  plot.new();plot.window(xlim=c(.5,307.5),ylim=c(.5,307.5))
  
  if(i == 1) mtext(labels, side=2, las=1, adj=1, font=2,
                   at = s2[-6] + diff(s2)/2, col = lab_col) 
  if(i %in% c(1, 2, 3)) mtext(rev(str_sub(labels,1,1)), side=1, las=1, font=2,
                   at = s1[-6] + diff(s1)/2, col = lab_col) 
  
  
  mat = bot[[i]]
  
  val = (c(mat) * 1000) %>% round()
  val = val - min(val)
  col = colorRampPalette(base_col)(max(val,na.rm=T)+1)
  cols = col[val + 1]
  
  ind = 0
  for(j in 1:5){
    for(k in 5:1){
      ind = ind + 1
      rect(s1[j], s2[k], s1[j+1], s2[k+1], 
           border="white", col = cols[ind])
      text(s1[j] + (s1[j+1] - s1[j])/2,
           s2[k] + (s2[k+1] - s2[k])/2,
           labels = mat[j,6-k] %>% round(3), col = "white",
           cex = sqrt((s1[j+1] - s1[j]) * (s2[k+1] - s2[k]))/60)
    }
  }
  
  par(mar=bot2_mar[[i]])
  p = pairs[[i]]
  plot.new();plot.window(xlim=xlims[[i]],ylim=ylims[[i]])
  rect(xlims[[i]][1],ylims[[i]][1], xlims[[i]][2],ylims[[i]][2], border=NA, col = "grey97")
  pos = seq(xlims[[i]][1], xlims[[i]][2], .01)
  mtext(round(pos, 3), at=pos, side = 1, cex=.6, adj=seq(0,1,length=length(pos)), line=-.3)
  if(i == 1){
    pos = seq(ylims[[i]][1], ylims[[i]][2], .005)
    mtext(round(pos, 3), at=pos, side = 2, las=1, cex=.6, padj=seq(0,1,length=length(pos)), line=-.3)
    mtext(labs[[i]],side=c(1,2), line=c(1.5,3))
    } else {
    mtext(labs[[i]],side=c(1,2), line=c(1.5,-.6), cex=.9)
    }
  set.seed(107)
  lyt = wordcloud::wordlayout(p[[1]],p[[2]],words = p[[3]],cex=1.2,xlim = xlims[[i]], ylim=ylims[[i]])
  text(lyt[,1],lyt[,2],labels = p[[3]], col=p[[4]], font=2,cex=1.2)
  }

dev.off()

# get correlation of averages
cor(get_pair(de, du)[1:2])
cor(get_pair(de, en)[1:2])
cor(get_pair(en, du)[1:2])

