require(tidyverse)
require(igraph)

Rcpp::sourceCpp('Scripts/AnalysisScripts/helper.cpp')
#source('Scripts/AnalysisScripts/helper.cpp')

# --------------------------------------------------------------------
#      READ & PROCESS DATA
# --------------------------------------------------------------------

# read assos
asso = read_csv('Data/InputData/risk_associations.csv')

# reduce to words that occur more than twice
from_tab = table(asso$from_correct)
asso = asso %>% 
  filter(from_correct %in% names(from_tab[from_tab > 10]))

# get all cues and responses
cues = unique(asso$from_correct)
resp = unique(asso$to_correct)

# create association matrix
asso_mat = matrix(0, nrow=length(cues), ncol = length(resp))
rownames(asso_mat) = cues
colnames(asso_mat) = resp

# fill matrix
for(i in 1:length(cues)){

  # keep track
  if(i %% 100 == 0) print(i)

  # fill matrix
  tab = table(asso[asso$from_correct == cues[i], "to_correct"])
  asso_mat[i, names(tab)] = as.numeric(tab)

  }

# ---- compute weight between cues

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
#     }
#   }
# saveRDS(jacc_mat,'1 Data/risk_similarity_mat.RDS')

# get co-occurrence and jaccard mat
jacc_mat = readRDS('Data/InputData/risk_similarity_mat.RDS')
edg_list = to_edge_list(jacc_mat)

# create graph
edges = cbind(cues[edg_list[,1]], cues[edg_list[,2]])
net = graph_from_edgelist(edges, directed = F)
net = set_edge_attr(net, "weight", value = edg_list[,3])

# cluster
# 在聚类之前添加一个种子 将其固定为5组
set.seed(1) 
nodes = tibble('asso' = cues, 'cluster' = cluster_louvain(net)$membership)

# add frequency
cue_freq = table(asso$to_correct[asso$from_correct == 'risiko'])
# 改变格式为数字而不是表
nodes$frequency_wave_1 = as.numeric(cue_freq[nodes$asso])
cue_freq = table(asso$to_correct[asso$from_correct != 'risiko'])
nodes$frequency_wave_2 = as.numeric(cue_freq[nodes$asso])

# add centrality
page_rank = page_rank(net)$vec
nodes$page_rank = page_rank[nodes$asso]

# add subclusters
nodes$page_rank_cluster = NA
for(i in  1:max(nodes$cluster)){
  g = induced_subgraph(net, which(nodes$cluster == i))
  pg = page_rank(g)$vector
  nodes$page_rank_cluster[nodes$asso %in% names(pg)] = pg
  }

# read translations 
trans = readxl::read_excel('Data/InputData/translations.xlsx', col_names = F) %>% 
  mutate_all(stringr::str_to_lower)
nodes$eng = trans[[3]]

# load sentiment
sent = read_csv('Data/InputData/sentiment_de.csv') %>% pull(sent, word)

# add sentiment to nodes
nodes$sent = sent[nodes$asso]

# get composite
get_sent = function(x) {v = mean(sent[x],na.rm=T);ifelse(length(v)==0,NA,v)}
nodes = nodes %>% left_join(asso %>% 
  filter(from_correct %in% nodes$asso) %>% 
  group_by(from_correct) %>% 
  summarize(sent_2 = get_sent(to_correct)), by = c("asso" = "from_correct"))
combine = function(x, y) ifelse(is.na(x), ifelse(is.na(y), NA, y), (x + y)/2)
nodes = nodes %>% mutate(sent_comp = combine(sent, sent_2))


# add risk sim
nodes$risk_sim = jacc_mat[nodes$asso, 'risiko']

# add average position
pos = asso %>% 
  filter(to_correct %in% nodes$asso, trial <= 5) %>% 
  group_by(to_correct) %>% 
  summarize(pos = mean(trial))
nodes = nodes %>% left_join(pos, by = c('asso' = 'to_correct'))

# save node data frame
write_csv(nodes, 'Data/IntermediateData/risk_nodes.csv')
  
head(nodes,5)
# --------------------------------------------------------------------
#      Network plot
# --------------------------------------------------------------------
#由于无法获取f.utils这个包 因此跑了一个等价实现函数
# ---- f.utils 的替代实现（原包不在 CRAN）----

# 把 x 里的每个值，按 old→new 的对应关系换掉
replace_cn <- function(x, old, new) new[match(x, old)]

# 按 weight 比例混合两种颜色（0 = 全取 col1，1 = 全取 col2）
colormixer <- function(col1, col2, weight, format = "hex") {
  a <- col2rgb(col1); b <- col2rgb(col2)
  n <- ncol(a)
  b <- b[, rep(seq_len(ncol(b)), length.out = n), drop = FALSE]
  w <- rep(weight, length.out = n)
  out <- a * (1 - w) + b * w
  rgb(out[1, ], out[2, ], out[3, ], maxColorValue = 255)
}

set.seed(8)

# within cluster amplifier
within_clique_weight = .006
edge_cex = c(1.5, .008, .06)


# colors
cols = viridis::viridis(5)[c(3,2,1,5,4)]

# get edges
edges = igraph::get.edgelist(net)

#————————————————以下原代码已将f.utils删除——————————————————
# set edge weights
cl_from = replace_cn(edges[, 1], nodes$asso, nodes$cluster)
cl_to = replace_cn(edges[, 2], nodes$asso, nodes$cluster)
cl_weight = E(net)$weight + ifelse(cl_from == cl_to, within_clique_weight, 0)


# ----- vertex settings

centr_id = rep(F, nrow(nodes))
for(i in 1:max(nodes$cluster)){
  cl_ind = which(nodes$cluster == i)
  centrs = nodes$page_rank[nodes$cluster == i]
  centr_ind = which(centrs >= centrs[order(centrs,decreasing = T)][5])
  centr_id[cl_ind][centr_ind] = T
  }

vcol = colormixer(cols[nodes$cluster], 'white', rep(.1,nrow(nodes)),format = "hex")
vcex = nodes$page_rank * 800 + .5

# ----- edge settings

# edge pos indices
e_pos_from = replace_cn(edges[,1], nodes$asso, 1:nrow(nodes))
e_pos_to = replace_cn(edges[,2], nodes$asso, 1:nrow(nodes))

# edge colors
within = cl_from == cl_to
col_e = ifelse(within, cols[nodes$cluster[e_pos_from]], 'grey75')
col_e = colormixer(col_e,'black',
                             weight=rep(.1,length(col_e)),'hex')

# edge counts and size
lwd = E(net)$weight**edge_cex[1] / ifelse(within,edge_cex[2],edge_cex[3])


# layout
cl_weight[cl_weight <= 0] = 1
lyt = igraph::layout_with_fr(net, weights = cl_weight)

png('Output/Figures/Figure3_network.png',width = 2800, height = 2200)

par(mar=c(0,0,0,0));plot.new();plot.window(xlim=range(lyt[,1]),ylim=range(lyt[,2]))

# draw between edges
for(i in which(!within)){
  pos_from = lyt[e_pos_from[i], ]
  pos_to = lyt[e_pos_to[i], ]
  lines(c(pos_from[1],pos_to[1]),c(pos_from[2],pos_to[2]),
        lwd=lwd[i],col=col_e[i])
}

# draw within edges
for(i in which(within)){
  pos_from = lyt[e_pos_from[i],]
  pos_to = lyt[e_pos_to[i],]
  lines(c(pos_from[1],pos_to[1]),c(pos_from[2],pos_to[2]),
        lwd=lwd[i],col=col_e[i])
}


#——————下面这个应该是也要依赖f.utils这个包——————
# ---- helper.R 里的函数（原文件依赖 f.utils，这里重写）----

colTrans <- function(col, perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue = 255, alpha = perc * 255)
}

globe <- function(x, y, col, prc = 1, cex, min_cex = .01,
                  bias = c(0, 0), res = 10, sh = 0, sh_bias = c(0, 0)) {
  cexs <- seq(cex, cex * min_cex, length = res)
  cols <- colormixer(col, par()$bg, seq(prc, 0, length = res))
  xs <- seq(x, x + bias[1], length = res)
  ys <- seq(y, y + bias[2], length = res)
  if (sh != 0) points(xs[1] + sh_bias[1], ys[1] + sh_bias[2],
                      cex = cexs[1], col = colTrans(par()$bg, 1 - sh), pch = 16)
  for (i in 1:res) {
    points(xs[i], ys[i], cex = cexs[i], col = cols[i], pch = 16)
  }
}

# plot points
for(i in which(!centr_id)) globe(lyt[i,1],lyt[i,2],col=vcol[i],prc=.3,cex=vcex[i],res=2,sh=.5,sh_bias=c(-.01,-.01))


grd_res = 25
grd_margin = 0
box = par()$usr
horiz_lim = 2
vert_lim = 2
cex_lab = c(1, 1)
lwd_labline = 4


# ------ create grid

grd    = expand.grid('x'=seq(box[1] + grd_margin, box[2] - grd_margin, length = grd_res),
                     'y'=seq(box[3] + grd_margin, box[4] - grd_margin, length = grd_res))
grd[,c('row','col')] = expand.grid('row' = 1:grd_res,'col' = 1:grd_res)
grd$id   = 1:nrow(grd)
grd$pos  = apply(expand.grid(1:grd_res,1:grd_res),1,paste0,collapse='_')
grd$free = rep(T,nrow(grd))


# ------ clear vertices space

# limits of vertex block
vert_lim = 5
horiz_lim = 5

# iterate over vertices
for(i in which(centr_id)){
  
  # find closest grid point
  pnt = lyt[i,]
  dif = sqrt((grd$x - pnt[1])**2 + (grd$y - pnt[2])**2)
  sel = which(dif==min(dif))[1]
  
  # clear rectangle around layout point
  pos = unlist(grd[sel,c('row','col')])
  rct = expand.grid(pos[1]+c(-horiz_lim : -1, 0, 1 : horiz_lim),
                    pos[2]+c(-vert_lim  : -1, 0, 1 : vert_lim))
  rem = apply(rct,1,paste0,collapse='_')
  grd$free[grd$pos %in% rem] <- F
}

# ------ place labels

# init
vert_lim = 1.4
txt.pos = lyt

# iterate over vertices
for(i in which(centr_id)){
  
  # determine horizontal space around label
  wid = (.025 *nchar(nodes$eng[i])**.92)/2
  horiz_lim  = ceiling(wid*grd_res)
  
  # find closes non-taken grid position
  pnt = lyt[i,]
  tst = subset(grd,free == T)
  dif = sqrt((tst$x - pnt[1])**2 + (tst$y - pnt[2])**2)
  sel = which(grd$free)[which.min(dif)]
  
  # clear space around label
  pos = unlist(grd[sel,c('row','col')])
  rct = expand.grid(pos[1]+c(-horiz_lim : -1, 0, 1 : horiz_lim),
                    pos[2]+c(-vert_lim  : -1, 0, 1 : vert_lim))
  rem = apply(rct,1,paste0,collapse='_')
  grd$free[ grd$pos %in% rem] = F
  
  # set text position
  txt.pos[i,] = c(grd[sel, 1], grd[sel, 2])
}

# ------ place labels

# draw label lines
lab_lines = cbind(lyt,txt.pos)[centr_id,]
apply(lab_lines,1,
      function(x) lines(c(x[1],x[3]),c(x[2],x[4]),lty=3,lwd=lwd_labline,col='grey75'))
#points(txt.pos,pch=16,lwd=1,col='white',cex=1.5)


# draw points
for(i in which(centr_id)[-1]) globe(lyt[i,1], lyt[i,2], col=vcol[i],
                                    prc=.3,cex=vcex[i],res=2,sh=.5,sh_bias=c(-.01,-.01))

# draw label background
for(i in which(centr_id)){
  nch = nchar(nodes$eng[i])
  rect(txt.pos[i,1]-.007*nch**.92,
       txt.pos[i,2]-.015,
       txt.pos[i,1]+.007*nch**.92,
       txt.pos[i,2]+.015,
       col=rgb(1,1,1,alpha=.3),border=NA)
}

# draw
text(txt.pos[centr_id,1],txt.pos[centr_id,2],nodes$eng[centr_id],
     font=2,cex=(1100*nodes$page_rank[centr_id])**.9,col=
       colormixer(cols[nodes$cluster[centr_id]],'black',rep(.1,length(cols[nodes$cluster[centr_id]])),format = 'hex'))

dev.off()

# --------------------------------------------------------------------
#      WORD CLOUDS
# --------------------------------------------------------------------
# 以下需要装新的包
#install.packages(c("memnet", "wordcloud"),
                 #repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# memnet无法安装了 应该是因为版本问题 
#  但好像和之前定义是类似的工具 因此命个名好像就好了
#————————以下已将comnet删除掉——————————
cmix <- colormixer
#col = memnet:::cmix(viridis::viridis(5)[c(3,2,1,5,4)],"black",.1)

cols = cmix(viridis::viridis(5)[c(3,2,1,5,4)],"black",c(0,0,0,.13,0))
cols_a = viridis::viridis(5,alpha=.5)[c(3,2,1,5,4)]
labs = c('Threat','Investment','Analysis','Activity','Fortune')


png('Output/Figures/Figure4_wordclouds.png',width=9,height=5.5,res=300,unit='in')

ord = c(5, 4, 2, 1, 3)
tab = table(nodes$cluster)

layout(matrix(c(rep(1:5, rep(3, 5)), rep(6:8, rep(5, 3))), nrow=2, byrow = T), height = c(.55,.45))
par(mar=c(0,0,1.3,0))
for(i in ord){
  nodes_tmp = nodes %>% filter(cluster == i, asso != 'risiko')
  wordcloud::wordcloud(nodes_tmp$eng, freq = rank(nodes_tmp$frequency_wave_1), 
                       min.freq=0, col = cols[i],scale=c(1.5,.3),random.order=F,
                       fixed.asp = F,rot.per = 0)
  mtext(labs[i],cex=1,font=2,line=-.3,col=cols[i])
  if(i == 5) mtext('A',side=3,font=2,at=.02,line=.1)
}

corrs = readRDS('Data/InputData/stability_confusion.RDS')
ord = c(5, 4, 2, 1, 3)
corrs = corrs[ord, ord]
corrs = corrs / rowSums(corrs)

par(mar=c(1, 7, 2, 1))
plot.new();plot.window(xlim=c(.5, 5.5), ylim=c(.5, 5.5))
pos = 1:5
w = .5 ; h = .45
for(i in 1:5){
  v = corrs[i,]
  row_cols = "white" #grey(1-v/.9)
  row_cols = cmix(rep(cols[ord[i]],5), "white", weight = (1-corrs[i,]/.9)*.9)
  for(j in 1:5){
    rect(pos[j] - w, pos[6-i]-h, pos[j]+w, pos[6-i]+h, col = row_cols[j], border=NA)
  }
  text(pos, pos[6-i], labels = paste(round(v * 100), "%"), cex=1, 
       col = ifelse(v > .4,"white", 'grey70'), font = ifelse(which.max(v)==(1:5), 2, 1))
}
mtext(labs[ord], side=2, at = 5:1, adj=1, col = cols[ord], las=1, cex=1, font=2)
mtext(str_sub(labs[ord],1,1), side=3, at = 1:5, adj=.5, col = cols[ord], las=1, cex=1, font=2)
mtext('B',side=3,font=2,at=-1.9,line=.5)

get_max = function(x) {tab = table(x) ; names(tab)[which.max(tab)]}
nodes_agg = asso %>% 
  right_join(nodes, by=c("to_correct" = "asso")) %>% 
  filter(trial <= 5) %>%
  group_by(trial, cluster) %>% 
  summarize(n = n(),
            w = get_max(eng)) %>% 
  pivot_wider(names_from = trial, values_from = c(n,w)) %>% 
  ungroup() %>% 
  mutate_if(is.integer, function(x) x / sum(x))


par(mar=c(3.5,4,2,1))
plot.new();plot.window(xlim = c(.5,5.5), ylim=c(0,1))

s = c(1, 5, 4, 2, 3)
#s = ord
nodes_agg = nodes_agg[s,]

for(i in 1:5){
  rect(i - .45, c(0, cumsum(nodes_agg[[paste0("n_",i)]])[-5]), 
       i + .45, cumsum(nodes_agg[[paste0("n_",i)]]), col = cols[s], border=NA)
  pos = c(0, cumsum(nodes_agg[[paste0("n_",i)]])[-5]) + nodes_agg[[paste0("n_",i)]] / 2 + .002
  ws = nodes_agg[[paste0("w_",i)]]
  #text(rep(i,5), pos, labels = ws,cex = ifelse(8.5/nchar(ws) > 1.5,1.5,8.5/nchar(ws)),col='white')
}


mtext(seq(0,1,.1),at=seq(0,1,.1),side=2,las=1,cex=.8)
mtext(1:5, at = 1:5,side=1,las=1,cex=.8)
mtext(c('Response index','Proportion'),side=c(1,2),line=c(1.7,2.2),cex=1)
mtext('C',side=3,font=2,at=-.64,line=.5)



# stats
nodes_agg = nodes %>% 
  group_by(cluster) %>% 
  summarize(sent_m = mean(sent_comp, na.rm=T),
            sent_q025 = quantile(sapply(1:10000, function(x) mean(sample(sent_comp,replace=T),na.rm=T)),.025),
            sent_q975 = quantile(sapply(1:10000, function(x) mean(sample(sent_comp,replace=T),na.rm=T)),.975),
            risk_m = mean(risk_sim, na.rm=T),
            risk_q025 = quantile(sapply(1:10000, function(x) mean(sample(risk_sim,replace=T),na.rm=T)),.025),
            risk_q975 = quantile(sapply(1:10000, function(x) mean(sample(risk_sim,replace=T),na.rm=T)),.975),
            pg = mean(page_rank),
            key = asso[which.max(page_rank)],
            n = n())

par(mar=c(3.5,5,2,1))
plot.new();plot.window(xlim=c(-.35,.15),ylim=c(0,.026))

sapply(seq(-.3,.2,.1),function(x) lines(c(x,x),c(0,.026),lwd=.7, col = 'grey85',lty=1))
sapply(seq(.005,.025,.005),function(x) lines(c(-.35,.15),c(x,x),lwd=.7, col = 'grey85',lty=1))

for(i in 1:5) lines(nodes_agg[i,c('sent_q025','sent_q975')],rep(nodes_agg[i,'risk_m'],2),col=cols_a[i],lwd=10,lend=2,lty=1)
for(i in 1:5) lines(rep(nodes_agg[i,'sent_m'],2),nodes_agg[i,c('risk_q025','risk_q975')],col=cols_a[i],lwd=10,lend=2,lty=1)
points(nodes_agg %>% select(sent_m, risk_m), cex = 4, col = cols, pch=16,lwd=2)

mtext(seq(0,.025,.005),at=seq(0,.025,.005),side=2,las=1,cex=.8)
mtext(round(seq(-.3,.1,.1),2),at=seq(-.3,.1,.1),side=1,las=1,cex=.8)
mtext(c('Sentiment','Risk similarity'),side=c(1,2),line=c(1.7,3),cex=1)
mtext('D',side=3,font=2,at=-.52,line=.5)

ord = c(1, 4, 2, 3, 5)
text(c(-.21, -.11, -.27, -.005, .115), c(.02, .012, .004, .003, .014), 
     labels = labs[ord], col  = cols[ord], font=1)


dev.off()


# COMPONENT SIZE ------

asso %>% 
  right_join(nodes, by=c("to_correct" = "asso")) %>% 
  filter(trial <=5) %>%
  group_by(cluster) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(n = n/sum(n))

asso_v = asso %>% 
  right_join(nodes, by=c("to_correct" = "asso")) %>% 
  filter(trial <= 5) %>% 
  pull(to_correct) %>% 
  table()
asso_v = asso_v / sum(asso_v)
asso_v["gefahr"]

asso_v[nodes$asso[nodes$cluster == 2]] %>% sort()
asso_v[nodes$asso[nodes$cluster == 3]] %>% sort()
asso_v[nodes$asso[nodes$cluster == 4]] %>% sort()


asso %>% 
  filter(trial <= 5) %>% 
  group_by(subject) %>% 
  summarize(exist = "gefahr" %in% to_correct) %>% 
  summarize(mean(exist))

asso %>% 
  filter(trial <= 5) %>% 
  group_by(subject) %>% 
  summarize(exist = "geld" %in% to_correct) %>% 
  summarize(mean(exist))

nodes %>% filter(cluster == 1) %>% arrange(desc(frequency_wave_1)) %>% pull(eng) %>% head()
nodes %>% filter(cluster == 2) %>% arrange(desc(frequency_wave_1)) %>% pull(eng) %>% head()
nodes %>% filter(cluster == 3) %>% arrange(desc(frequency_wave_1)) %>% pull(eng) %>% head()
nodes %>% filter(cluster == 4) %>% arrange(desc(frequency_wave_1)) %>% pull(eng) %>% head()
nodes %>% filter(cluster == 5) %>% arrange(desc(frequency_wave_1)) %>% pull(eng) %>% head()


# --------------------------------------------------------------------
#      HIERARCHICAL CLUSTERING
# --------------------------------------------------------------------

# get jacc mat
jacc_mat = readRDS('Data/InputData/risk_similarity_mat.RDS')

# get nodes
nodes_hclust = readr::read_csv('Data/IntermediateData/risk_nodes.csv') 

# run hclust
cl = fastcluster::hclust(as.dist(1-jacc_mat), method = "ward.D")

# set first clustering
prev = cutree(cl, 1)

# iterate through k = 2:10
for(i in 2:10){
  
  # keep track
  print(i)
  
  # set colors
  cols = viridis::viridis(i)
  cols[length(cols)] = cmix(cols[length(cols)],"black",.13)

  # start plot  
  png(paste('Output/Appendix/wordclouds_',i,'.png'),width=15,height=3,res=300,unit='in')
  par(mar=c(0,0,0,0))
  
  # get and sort clustering
  curr = cutree(cl, i)
  pos = sapply(1:i, function(x) {n = names(curr[curr == x]); mean(prev[n])}) %>% order()
  curr_ord = curr
  for(k in 1:i) curr_ord[curr == pos[k]] = k
  
  # set clusters
  nodes = nodes_hclust %>% mutate(cluster = curr_ord)
  
  # update clustering
  prev = curr_ord
  
  # determine cluster proportions
  p = (table(nodes$cluster) / nrow(nodes) + .2*rep(1/i, i))/1.2
  
  # plot wordclouds
  layout(matrix(1:i,nrow=1), width=p)
  for(j in 1:i){
    nodes_tmp = nodes %>% filter(cluster == j, asso != 'risiko')
    wordcloud::wordcloud(nodes_tmp$eng, freq = rank(nodes_tmp$frequency_wave_1)**1.5, 
                         min.freq=0, col = cols[j], scale=c(2,.2),random.order=F,
                         fixed.asp = F,rot.per = 0)
  }  
  dev.off()
}

