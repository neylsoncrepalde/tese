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

