# Monta as redes

library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(xtable)

## Filarmônica ####

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

filarmonica %>% filter(relation == "parceria institucional")

filarmonica %>% filter(relation == "patrocinador") %>% group_by(activity) %>% summarise(n = n()) %>% 
  arrange(desc(n))

## Sinfônica ####

sinfonica = fread('~/tese_files/sinfonica_network.csv', header = T) %>% as_tibble %>% 
  select(1:4)
sinfonica

gsinf = graph_from_edgelist(as.matrix(sinfonica[1:2]), directed = F)
E(gsinf)$relation = sinfonica$relation

V(gsinf)$cor = c("z", sinfonica$relation)

plot(gsinf, vertex.shape = "none", edge.color = as.factor(E(gsinf)$relation),
     vertex.label.color = as.integer(as.factor(V(gsinf)$cor)))

png('rede_sinfonica.png', height = 600, width = 600)
plot(gsinf, vertex.shape = "none", edge.color = as.factor(E(gsinf)$relation),
     vertex.label.color = as.integer(as.factor(V(gsinf)$cor)))
dev.off()

## OPUS ####
opus = fread('~/tese_files/opus_network.csv', header = T) %>% as_tibble %>% 
  select(1:4)
opus

gopus = graph_from_edgelist(as.matrix(opus[1:2]), directed = F)
E(gopus)$relation = opus$relation

V(gopus)$cor = c("z", opus$relation)

plot(gopus, vertex.shape = "none", edge.color = as.factor(E(gopus)$relation),
     vertex.label.color = as.integer(as.factor(V(gopus)$cor)))

png('rede_opus.png', height = 600, width = 600)
plot(gopus, vertex.shape = "none", edge.color = as.factor(E(gopus)$relation),
     vertex.label.color = as.integer(as.factor(V(gopus)$cor)))
dev.off()

## Ouro Preto ####
ouropreto = fread('~/tese_files/ouro_preto_network.csv', header = T) %>% as_tibble %>% 
  select(1:4)
ouropreto

gouropreto = graph_from_edgelist(as.matrix(ouropreto[1:2]), directed = F)
E(gouropreto)$relation = ouropreto$relation

V(gouropreto)$cor = c("z", ouropreto$relation)

plot(gouropreto, vertex.shape = "none", edge.color = as.factor(E(gouropreto)$relation),
     vertex.label.color = as.integer(as.factor(V(gouropreto)$cor)))

png('rede_ouropreto.png', height = 600, width = 600)
plot(gouropreto, vertex.shape = "none", edge.color = as.factor(E(gouropreto)$relation),
     vertex.label.color = as.integer(as.factor(V(gouropreto)$cor)))
dev.off()

## SESI ####
sesi = fread('~/tese_files/sesi_network.csv', header = T) %>% as_tibble %>% 
  select(1:4)
sesi

gsesi = graph_from_edgelist(as.matrix(sesi[1:2]), directed = F)
E(gsesi)$relation = sesi$relation

V(gsesi)$cor = c("z", sesi$relation)




# Teste rede completa
gcompleto = gfil %u% gsinf %u% gouropreto %u% gsesi %u% gopus
E(gcompleto)$relation_1; E(gcompleto)$relation_2
E(gcompleto)$relation = c(E(gcompleto)$relation_2[1:8], E(gcompleto)$relation_1[9:58]) 

plot(gcompleto, vertex.size = 6, 
     #edge.color = as.factor(E(gcompleto)$relation),
     vertex.color = adjustcolor('red', .6))



