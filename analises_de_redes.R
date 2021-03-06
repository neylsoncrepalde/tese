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
o1 = 5+5+5+5+5+5+5+5+0+5+5+4+5+5+5+5+5+5 #F
o2 = 3+4+0+4+4+4+3+0+5+3+0+5+4+4+4+0+4+0 #Si
o3 = 4+0+0+3+3+3+0+0+0+0+0+1+0+0+3+0+3+0 #OrP
o4 = 0+3+0+2+2+2+4+0+0+4+0+3+0+0+1+0+1+0 #Se
o5 = 2+0+0+0+1+0+0+0+0+0+0+2+0+0+2+0+2+0 #Op

o1;o2;o3;o4;o5

# Correlação ####
# Estimar com qap
library(statnet)
library(intergraph)

# Extrai as redes da completa para que todas tenham os mesmos nós
conselho  = delete_edges(g, which(E(g)$relation != "Conselho"))
amizade   = delete_edges(g, which(E(g)$relation != "Amizade"))
indicacao = delete_edges(g, which(E(g)$relation != "Indicação"))
convite   = delete_edges(g, which(E(g)$relation != "Convite"))

# Transforma em network (para sna)
n1 = asNetwork(conselho)
n2 = asNetwork(amizade)
n3 = asNetwork(indicacao)
n4 = asNetwork(convite)

# Testa as correlações. Com n1 não deu certo
matcor = matrix(c(1, NA, NA, NA,
                  gcor(n2, n1), 1, NA, NA,
                  gcor(n3, n1), gcor(n3, n2), 1, NA,
                  gcor(n4, n1), gcor(n4, n2), gcor(n4, n3), 1),
                ncol = 4, byrow = T)
rownames(matcor) = c("Aconselhamento", "Amizade", "Indicação", "Convite")
colnames(matcor) = c("Aconselhamento", "Amizade", "Indicação", "Convite")
xtable(matcor, digits=3)

# qaptest(list(n1, n2), gcor, g1=1, g2=2, reps=1000)
# qaptest(list(n1, n3), gcor, g1=1, g2=2, reps=1000)
# qaptest(list(n1, n4), gcor, g1=1, g2=2, reps=1000)
# qaptest(list(n2, n3), gcor, g1=1, g2=2, reps=1000)
# qaptest(list(n2, n4), gcor, g1=1, g2=2, reps=1000)
# qaptest(list(n3, n4), gcor, g1=1, g2=2, reps=1000)


# model1 = formula(n4 ~ edges + edgecov(n1) + edgecov(n2) + edgecov(n3) +
#                    gwidegree(1, fixed=T) + gwesp(1, fixed=T))
# summary.statistics(model1)
# fit1 = ergm(model1)
# summary(fit1)
# gof1 = gof(fit1)
# par(mfrow = c(1,4))
# plot(gof1)
# par(mfrow = c(1,1))





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

# png('sbm_individual_out.png', height = 450, width = 450)
# plot(sbmout)
# dev.off()


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
legend("topright", c("Bloco 1", "Bloco 2", "Bloco 3", "Bloco residual"), 
       col = c(adjustcolor('red', .6), adjustcolor('blue', .6),
               adjustcolor('orange', .6), adjustcolor('grey', .6)),
       pt.cex = 1.5, pch = 19)

# png("prestigio_blocos.png", height=600, width=600)
# plot(prestigiogc, vertex.label = NA, 
#      #vertex.size = degree(prestigiogc, mode = "in")*2,
#      vertex.size = 6,
#      vertex.color = corblock,
#      #edge.color = multiplexo$cor[multiplexo$cor != 'green'],
#      edge.arrow.size = .2,
#      layout = layout_with_kk)
# title("Componente principal da rede de prestígio")
# legend("topright", c("Bloco 1", "Bloco 2", "Bloco 3", "Bloco residual"), 
#        col = c(adjustcolor('red', .6), adjustcolor('blue', .6),
#                adjustcolor('orange', .6), adjustcolor('grey', .6)),
#        pt.cex = 1.5, pch = 19)
# dev.off()

indblocos = tibble(nome = V(prestigiogc)$name, pertencimento)
indblocos %>% filter(pertencimento == 1)


# Estimações ####
# Indivíduos

# Prepara atributo "prestígio da orquestra" de acordo com o achado
aff_para_modelar = tibble(sender = V(gprestigiosi)$name) %>% 
  left_join(afiliacoes %>% select(-relation)) %>% 
  mutate(prestorq = case_when(
    receiver == "Orquestra Filarmônica de MG" ~ 5L,
    receiver == "Orquestra Sinfônica de MG" ~ 4L,
    receiver == "Orquestra Ouro Preto" ~ 3L,
    receiver == "Orquestra SESIMINAS" ~ 2L,
    receiver == "OPUS" ~ 1L
  )) %>% 
  group_by(sender) %>% summarise(prest = sum(prestorq, na.rm = T))

V(gprestigiosi)$prestorq = tibble(sender = V(gprestigiosi)$name) %>% 
  left_join(aff_para_modelar) %>% pull(prest)
# Estimação
nprestigio = asNetwork(simplify(gprestigiosi))
triad.census(nprestigio)

set.seed(123)
model_prestigio = formula(nprestigio ~ edges + 
                            #gwdsp(1, fixed=F, cutoff=2) +
                            gwesp(1, fixed=F, cutoff=2) + 
                            gwidegree(1, fixed=F, cutoff=6) +
                            #transitive +
                            #ttriple + 
                            #istar(2) + 
                            m2star + 
                            nodematch("sexo") + 
                            nodecov("prestorq") +
                            edgecov(n2))
summary.statistics(model_prestigio)

#fit_prestigio = ergm(model_prestigio)
#saveRDS(fit_prestigio, "fit_prestigio.rds")
fit_prestigio = readRDS("fit_prestigio.rds")
summary(fit_prestigio)
plot(fit_prestigio)
gof_prestigio = gof(fit_prestigio)
# Plot gof
png("gof_individuos.png", height=400, width = 500)
par(mfrow = c(2,3))
plot(gof_prestigio)
par(mfrow = c(1,1))
dev.off()

# Export table with the results
texreg::texreg(fit_prestigio, single.row = T)

# # Investiga rede de conselho
# triad.census(n1)
# n1 = asNetwork(simplify(conselho))
# plot(n1)
# model_conselho = formula(n1 ~ edges +
#                            gwidegree(1, fixed=F, cutoff=3) +
#                            m2star + 
#                            nodematch("sexo") + 
#                            isolates +
#                            edgecov(n2) + edgecov(n3) + edgecov(n4))
# summary.statistics(model_conselho)
# fit_conselho = ergm(model_conselho)
# summary(fit_conselho)
# plot(gof(fit_conselho))


# meso-level 1 ####
mesonivel1 = mode_transformation(extract_mesolevel(gcompletao), "low")
mesonivel1 = decompose(mesonivel1, "weak")[[1]]
# png("mesonivel1.png", height=600, width=600)
# plot(mesonivel1, 
#      vertex.label = NA, 
#      vertex.size = igraph::betweenness(mesonivel1)/50, 
#      edge_width = E(mesonivel1)$weight,
#      layout = layout_with_kk)
# dev.off()


# Nível 2 ####
organizacoes = extract_highlevel(gcompletao)
organizacoes = decompose(organizacoes, "weak")[[1]]
# png("nivel2.png", height=600, width=600)
plot(organizacoes,
     vertex.size = igraph::degree(organizacoes)/4,
     vertex.label=NA,
     vertex.label.size = igraph::degree(organizacoes)/10,
     edge.arrow.size = .3)
# dev.off()


# meso-level 2 ####
mesonivel2 = mode_transformation(extract_mesolevel(gcompletao), "high")
mesonivel2 = decompose(mesonivel2, "weak", 2)[[1]]

# png("mesonivel2.png", height=600, width=600)
# plot(mesonivel2, 
#      vertex.size = igraph::degree(mesonivel2)*1.5, 
#      #vertex.size = igraph::betweenness(mesonivel2), 
#      #layout = layout_with_kk,
#      vertex.label = NA,
#      edge.width = E(mesonivel2)$weight)
# dev.off()
set.seed(2)
sbmnivel2 = mixer(as.matrix(get.adjacency(mesonivel2)), qmin = 2, qmax = 5)

# png("blockmesonivel2.png", height = 450, width = 450)
# plot(sbmnivel2)
# dev.off()

bestsbm = getModel(sbmnivel2)
bestsbm$Taus
pertencimento = c()
for (col in 1:ncol(bestsbm$Taus)) {
  pertencimento[col] = which.max(bestsbm$Taus[,col])
}

corblock = ifelse(pertencimento==1, adjustcolor('red', 6), adjustcolor('blue', .6))
# png("block_rede_mesonivel2.png", height=600, width=600)
# plot(mesonivel2,
#      vertex.size = igraph::degree(mesonivel2)*1.5,
#      vertex.color = corblock,
#      #vertex.size = igraph::betweenness(mesonivel2),
#      #layout = layout_with_kk,
#      vertex.label = NA,
#      edge.width = E(mesonivel2)$weight)
# dev.off()


# SBM no nível 2 observado
plot(organizacoes, vertex.size = igraph::degree(organizacoes)/3, vertex.label=NA,
     edge.arrow.size = .3)
set.seed(123)
sbmorg = mixer(as.matrix(get.adjacency(organizacoes)), qmin=2, qmax=5)
sbmorgout = getModel(sbmorg)
# png("blockoutorgs.png", height = 450, width = 450)
# plot(sbmorg)
# dev.off()

sbmorgout$Taus
pertencimento = c()
for (col in 1:ncol(sbmorgout$Taus)) {
  pertencimento[col] = which.max(sbmorgout$Taus[,col])
}

corblock = ifelse(pertencimento==1, adjustcolor('red', 6),
                  ifelse(pertencimento == 2, adjustcolor('blue', .6),
                         ifelse(pertencimento == 3, adjustcolor('orange', .6), adjustcolor('violet', .6))))

# png("block_nivel2_observado.png", height=600, width=600)
# plot(organizacoes,
#      vertex.size = 5,
#      vertex.color = corblock,
#      edge.arrow.size = .3,
#      #vertex.size = igraph::betweenness(mesonivel2),
#      #layout = layout_with_kk,
#      vertex.label = NA)
# legend("bottomleft", c("Bloco 1", "Bloco 2", "Bloco 3", "Bloco 4"), 
#        col = c(adjustcolor('red', 6),adjustcolor('blue', .6),
#                adjustcolor('orange', .6),adjustcolor('violet', .6)),
#        pch = 15, pt.cex = 1.5)
# dev.off()

# Ergm organizaçoes
norganizacoes = asNetwork(as.undirected(organizacoes))
triad.census(norganizacoes)
plot(norganizacoes)
model_org = formula(norganizacoes ~ edges +
                      kstar(2)
)
summary.statistics(model_org)
set.seed(123)
fit_org = ergm(model_org)
summary(fit_org)
texreg::texreg(fit_org, single.row = T)
gof_org = gof(fit_org)
# png('gof_org.png', height = 400, width = 500)
# par(mfrow = c(2,2))
# plot(gof_org)
# par(mfrow = c(1,1))
