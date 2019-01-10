## Analises de Redes
## Tese Neylson

source("monta_nivel1.R")

## Nível 1 ####
# Estatísticas descritivas das redes
tabela_descritivas = cbind(
  sapply(list(g1,g2,g3,g4,gprestigio), function(x) length(V(x))),
  sapply(list(g1,g2,g3,g4,gprestigio), function(x) length(E(x))),
  sapply(list(g1,g2,g3,g4,gprestigio), edge_density),
  sapply(list(g1,g2,g3,g4,gprestigio), diameter),
  sapply(list(g1,g2,g3,g4,gprestigio), mean_distance),
  sapply(list(g1,g2,g3,g4,gprestigio), function(x) mean(degree(x, mode = "in")))
  #sapply(list(g1,g2,g3,g4), transitivity)
)
colnames(tabela_descritivas) = c("N","Laços","Densidade", "Diâmetro", "Distância média", "Grau Médio")
rownames(tabela_descritivas) = c("Aconselhamento", "Amizade", "Indicação", "Convite", "Multiplexo - Prestígio")
print.xtable(xtable(tabela_descritivas, digits = 4))


## Centralidades e Constraint
calc_centralidades = function(x, ordered = TRUE) {
  deg = degree(x, mode = "in")
  bet = round(betweenness(x), 3)
  clos = round(closeness(x, normalized = T), 3)
  const = round(constraint(x),3)
  id = paste0("V", 1:length(deg))
  res = as_tibble(cbind(id, deg, bet, clos, const))
  if (ordered){
    return(res %>% arrange(desc(bet)))
  }
  else {
    return(res)
  }
}

# Exporta Latex
print.xtable(xtable(calc_centralidades(g1, ord=F)), include.rownames = F)
print.xtable(xtable(calc_centralidades(g2, ord=F)), include.rownames = F)
print.xtable(xtable(calc_centralidades(g3, ord=F)), include.rownames = F)
print.xtable(xtable(calc_centralidades(g4, ord=F)), include.rownames = F)
print.xtable(xtable(calc_centralidades(g, ord=F)), include.rownames = F)
print.xtable(xtable(calc_centralidades(gprestigio, ord=F)), include.rownames = F)

# HUBS ####
# g1: 7, 31 e 38
V(g1)$name[c(7,31,38)]
# g2: 7,8,2,42
V(g2)$name[c(7,8,2,42)]
# g3: 4,6,7,9
V(g3)$name[c(4,6,7,9)]
# g4: 5,7,16,22,35
V(g4)$name[c(5,7,16,22,35)]
# g: 5,7,8,13,21,31,38,41,67,44,55
hubs = V(gprestigio)$name[c(5,7,8,13,21,31,38,41,67,44,55)]

afiliacoes %>% filter(sender %in% hubs) %>% 
  group_by(receiver) %>% summarise(n = n()) %>%
  arrange(desc(n))


bd %>% select(`Qual é o seu nome?`,`Para o(a) senhor(a), quais são as orquestras em Belo Horizonte que possuem a melhor qualidade? Por favor, cite da que possui a melhor qualidade para a que possui a pior dentre elas.`, `Porque a orquestra que o(a) senhor(a) disse ser a melhor é a melhor? Cite quantas razões o(a) senhor(a) quiser.`)
bd$`Para o(a) senhor(a), quais são as orquestras em Belo Horizonte que possuem a melhor qualidade? Por favor, cite da que possui a melhor qualidade para a que possui a pior dentre elas.`


# Faz o Ranking das votações entre os entrevistados
o1 = 5+5+5+5+5+5+5+5+0+5+5+4+5+5+5+5+5+5
o2 = 3+4+0+4+4+4+3+0+5+3+0+5+4+4+4+0+4+0
o3 = 4+0+0+3+3+3+0+0+0+0+0+1+0+0+3+0+3+0
o4 = 0+3+0+2+2+2+4+0+0+4+0+3+0+0+1+0+1+0
o5 = 2+0+0+0+1+0+0+0+0+0+0+2+0+0+2+0+2+0

o1;o2;o3;o4;o5


## Blockmodel individuos
# Infelizmente o pacote mixer saiu do CRAN. Será necessário instalá-lo pelo
# espelho oficial do CRAN no github
#devtools::install_github("cran/mixer")
library(mixer)
# Extrai o componente principal
prestigiogc = decompose(gprestigio, mode = "weak", min.vertices = 2)[[1]]

# Roda o blockmodel
set.seed(123)
sbmout = mixer(as.matrix(get.adjacency(prestigiogc)), qmin=2, qmax=5)
m = getModel(sbmout)
plot(sbmout)

pertencimento = c()
for (col in 1:ncol(m$Taus)) {
  if (sum(m$Taus[,col]) == 0) {
    pertencimento[col] = 4
  } else {
    pertencimento[col] = which.max(m$Taus[,col])
  }
}

corblock = ifelse(pertencimento==1, adjustcolor('red', 6), 
                  ifelse(pertencimento == 2, adjustcolor('blue', .6), 
                         ifelse(pertencimento == 3, adjustcolor('orange', .6), adjustcolor('grey', .6))))

plot(prestigiogc, vertex.label = NA, 
     #vertex.size = degree(prestigiogc, mode = "in")*2,
     vertex.size = 6,
     vertex.color = corblock,
     #edge.color = multiplexo$cor[multiplexo$cor != 'green'],
     edge.arrow.size = .2,
     layout = layout_with_kk)
title("Componente principal da rede de prestígio")
legend("bottomright", c("Conselho", "Indicação", "Convite"), 
       col = c(adjustcolor('red', .6), adjustcolor('blue', .6),
               adjustcolor('orange', .6), adjustcolor('grey', .6)),
       pt.cex = 1.5, pch = 19)
