read_nodes.csv由代码1_network产生 为307 个一级联想词的节点属性表
## 列说明
| `asso` | 一级联想词（德语） |
| `cluster` | Louvain 聚类得到的成分编号（1–5） |
| `frequency_wave_1` | 作为 "risiko" 的直接回答出现的次数 |
| `frequency_wave_2` | 作为二级回答出现的次数 |
| `page_rank` | 在整个网络中的 PageRank 中心性 |
| `page_rank_cluster` | 在本成分内部的 PageRank 中心性 |
| `eng` | 英文翻译 |
| `sent` | 词本身的情感值（SentiWS 德语词典） |
| `sent_2` | 由该词引出的二级回答的平均情感值 |
| `sent_comp` | 上两者的合成值 |
| `risk_sim` | 与 "risiko" 的 Jaccard 相似度 |
| `pos` | 出现在前 5 个回答中的平均位置 |
