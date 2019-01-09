## Analises de Redes
## Tese Neylson

library(intergraph)
library(statnet)
source("monta_nivel1.R")

## Nível 1 ####
# Estatísticas descritivas das redes
tabela_descritivas = cbind(
  sapply(list(g1,g2,g3,g4), function(x) length(V(x))),
  sapply(list(g1,g2,g3,g4), function(x) length(E(x))),
  sapply(list(g1,g2,g3,g4), edge_density),
  sapply(list(g1,g2,g3,g4), diameter),
  sapply(list(g1,g2,g3,g4), mean_distance),
  sapply(list(g1,g2,g3,g4), function(x) mean(degree(x, mode = "in")))
  #sapply(list(g1,g2,g3,g4), transitivity)
)
colnames(tabela_descritivas) = c("N","Laços","Densidade", "Diâmetro", "Distância média", "Grau Médio")
rownames(tabela_descritivas) = c("Aconselhamento", "Amizade", "Indicação", "Convite")
print.xtable(xtable(tabela_descritivas, digits = 4))

## Correlação das redes
## Estimar através de ERGM