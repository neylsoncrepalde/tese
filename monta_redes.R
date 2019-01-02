# Monta as redes

library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(xtable)

filarmonica = fread('~/tese_files/filarmonica_network.csv', header = T) %>% as_tibble %>% 
  select(1:4) %>% slice(1:(n()-2))
filarmonica %>% tail


gfil = graph_from_edgelist(as.matrix(filarmonica[1:2]), directed = F)
E(gfil)$relation = filarmonica$relation

V(gfil)$cor = c("z", filarmonica$relation)

plot(gfil, vertex.shape = "none", edge.color = as.factor(E(gfil)$relation),
     vertex.label.color = as.integer(as.factor(V(gfil)$cor)))

png('rede_filarmonica.png', height = 600, width = 600)
plot(gfil, vertex.shape = "none", edge.color = as.factor(E(gfil)$relation),
     vertex.label.color = as.integer(as.factor(V(gfil)$cor)))
dev.off()

filarmonica %>% group_by(relation) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% xtable %>% print.xtable(include.rownames = F)

filarmonica %>% filter(relation == 'terceirizado')
