## Monta redes nível 1
## Redes de músicos
## Tese Neylson

# install development version of multinet
#devtools::install_github("neylsoncrepalde/multinets")

library(googlesheets)
library(tidyverse)
library(reshape2)
library(igraph)
library(descr)
library(stringr)
library(multinets)
source("sheet_key.R")
source("monta_redes.R")

#gs_auth(new_user = TRUE)
ss = gs_key(sheet_key)
bd = gs_read(ss)
bd = bd[-10,]

# Quais redes vamos montar?
names(bd)[18:21]
# Aconselhamento, Amizade, Indicação, Convite pra tocar

# Rede Aconselhamento ####
nomes = bd %>% select(3, 18) %>% 
  # tira os instrumentos, deixa só nomes
  mutate(receiver = str_remove_all(`Se o(a) senhor(a) precisasse de aconselhamento sobre a interpretação de alguma peça, independente do período ou do estilo da obra, a quem o(a) senhor(a) pediria conselho? Mencione quantas pessoas o(a) senhor(a) quiser.`, " \\(\\w+\\)")) %>% 
  # limpa \n e .
  mutate(receiver = str_remove_all(receiver, "\n")) %>% 
  mutate(receiver = str_remove_all(receiver, "\\.")) %>%
  # Tira a coluna com o nome da variável
  select(-2)

# Verifica 
n_max = melt(strsplit(nomes$receiver, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>% 
  pull(n) %>% max

rede1 = cbind(
  sender = nomes$`Qual é o seu nome?`,
  colsplit(nomes$receiver, ", ", sapply(1:n_max, function(x) paste0("col", x)))
) %>% as_tibble

rede1 = rede1 %>% gather(var, receiver, -sender) %>% select(-var) %>% 
  filter(receiver != "")
#rede1 %>% View

g1 = graph_from_edgelist(as.matrix(rede1), directed = T)
E(g1)$relation = "Conselho"

# Rede Amizade ####
nomes = bd %>% select(3, 19) %>% 
  # tira os instrumentos, deixa só nomes
  mutate(receiver = str_remove_all(`O(a) senhor(a) costuma se encontrar com outros músicos em ocasiões sociais fora do horário de trabalho? Com quem o(a) senhor(a) se encontra? Mencione quantas pessoas o(a) senhor(a) quiser.`, " \\(\\w+\\)")) %>% 
  # limpa \n e .
  mutate(receiver = str_remove_all(receiver, "\n")) %>% 
  mutate(receiver = str_remove_all(receiver, "\\.")) %>%
  # Tira a coluna com o nome da variável
  select(-2)

# Verifica 
n_max = melt(strsplit(nomes$receiver, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>% 
  pull(n) %>% max

rede2 = cbind(
  sender = nomes$`Qual é o seu nome?`,
  colsplit(nomes$receiver, ", ", sapply(1:n_max, function(x) paste0("col", x)))
) %>% as_tibble

rede2 = rede2 %>% gather(var, receiver, -sender) %>% select(-var) %>% 
  filter(receiver != "")
#rede2 %>% View

g2 = graph_from_edgelist(as.matrix(rede2), directed = T)
g2 = delete_vertices(g2, "Não")
E(g2)$relation = "Amizade"


# Rede Indicação
nomes = bd %>% select(3, 20) %>% 
  # tira os instrumentos, deixa só nomes
  mutate(receiver = str_remove_all(`Se o(a) senhor(a) fosse indicar um músico para uma excelente posição em uma orquestra, a quem o(a) senhor(a) indicaria? Mencione quantas pessoas o(a) senhor(a) quiser independente do instrumento.`, " \\(\\w+\\)")) %>% 
  # limpa \n e .
  mutate(receiver = str_remove_all(receiver, "\n")) %>% 
  mutate(receiver = str_remove_all(receiver, "\\.")) %>%
  # Tira a coluna com o nome da variável
  select(-2)

# Verifica 
n_max = melt(strsplit(nomes$receiver, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>% 
  pull(n) %>% max

rede3 = cbind(
  sender = nomes$`Qual é o seu nome?`,
  colsplit(nomes$receiver, ", ", sapply(1:n_max, function(x) paste0("col", x)))
) %>% as_tibble

rede3 = rede3 %>% gather(var, receiver, -sender) %>% select(-var) %>% 
  filter(receiver != "")
#rede3 %>% View

g3 = graph_from_edgelist(as.matrix(rede3), directed = T)
g3 = delete_vertices(g3, "Não")
E(g3)$relation = "Indicação"

# Rede Convite
nomes = bd %>% select(3, 21) %>% 
  # tira os instrumentos, deixa só nomes
  mutate(receiver = str_remove_all(`Se o(a) senhor(a) fosse responsável por organizar um recital ou um concerto no qual o(a) senhor(a) fosse tocar, independente da instrumentação das obras que o senhor poderia escolher, a quem o(a) senhor(a) convidaria para tocar com o(a) senhor(a)? Mencione quantas pessoas o(a) senhor(a) quiser.`, " \\(\\w+\\)")) %>% 
  # limpa \n e .
  mutate(receiver = str_remove_all(receiver, "\n")) %>% 
  mutate(receiver = str_remove_all(receiver, "\\.")) %>%
  # Tira a coluna com o nome da variável
  select(-2)

# Verifica 
n_max = melt(strsplit(nomes$receiver, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>% 
  pull(n) %>% max

rede4 = cbind(
  sender = nomes$`Qual é o seu nome?`,
  colsplit(nomes$receiver, ", ", sapply(1:n_max, function(x) paste0("col", x)))
) %>% as_tibble

rede4 = rede4 %>% gather(var, receiver, -sender) %>% select(-var) %>% 
  filter(receiver != "")
#rede4 %>% View

g4 = graph_from_edgelist(as.matrix(rede4), directed = T)
g4 = delete_vertices(g4, "Não")
E(g4)$relation = "Convite"

par(mfrow = c(2,2))
sapply(list(g1,g2,g3,g4), function(x) {plot(x, 
                        vertex.label = NA, vertex.size = 8,
                        edge.arrow.size = .2,
                        main = E(x)$relation[1])})
par(mfrow = c(1,1))

sapply(list(g1,g2,g3,g4), edge_density)

# Montando a rede multiplexo com as quatro relações
rede1$relation = "Conselho"
rede2$relation = "Amizade"
rede3$relation = "Indicação"
rede4$relation = "Convite"

multiplexo = rbind(rede1, rede2, rede3, rede4)
multiplexo = filter(multiplexo, receiver != "Não")
g = graph_from_edgelist(as.matrix(multiplexo[,1:2]), directed = T)
E(g)$relation = multiplexo$relation


multiplexo = multiplexo %>% 
  mutate(cor = case_when(
    relation == "Conselho" ~ 'red',
    relation == "Amizade" ~ 'green',
    relation == "Indicação" ~ 'darkblue',
    relation == "Convite" ~ 'yellow'
  ))

plot(g, vertex.label = NA, vertex.size = 5,
     vertex.color = adjustcolor("red", .6),
     edge.color = multiplexo$cor,
     edge.arrow.size = .2)
title("Rede Multiplexo")


## Afiliações ####
# #Já rodei
# affs = bd %>% select(`Qual é o seu nome?`, `O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`)
# affs
# n_max = melt(strsplit(affs$`O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>% 
#   pull(n) %>% max
# redeaff = cbind(
#   sender = nomes$`Qual é o seu nome?`,
#   colsplit(affs$`O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`, ", ", sapply(1:n_max, function(x) paste0("col", x)))
# ) %>% as_tibble
# 
# redeaff = redeaff %>% gather(var, receiver, -sender) %>% select(-var) %>% 
#   filter(receiver != "", receiver != "Não", receiver != "Freelancer") %>% 
#   mutate(sender = as.character(sender))
# #redeaff %>% View
# 
# 
# afiliacoes = data_frame(sender = V(g)$name) %>% left_join(redeaff)
# afiliacoes %>% filter(is.na(receiver)) %>% write_csv("paracompletar.csv")
afiliacoes = read_csv("paracompletar.csv") %>% filter(!is.na(aff))
afiliacoes = cbind(sender = afiliacoes$sender, colsplit(afiliacoes$aff, ", ", c("aff1", "aff2"))) %>% 
  gather(var, receiver, -sender) %>% select(-var) %>% filter(receiver != "")

afiliacoes$relation = "Afiliação"

tipos = data_frame(nos = c(as.character(afiliacoes$sender), afiliacoes$receiver),
                   type = rep(c(FALSE, TRUE), each = nrow(afiliacoes)))


# Multinível ####
# Junta as redes em uma multinível

gnivel2 = as.directed(gnivel2, "mutual")
V(g)$type = FALSE

gcompleto = g %u% gnivel2
gcompleto

V(gcompleto)$type = c(V(gcompleto)$type_1[1:161], V(gcompleto)$type_2[162:231])

gmaisedges = graph_from_edgelist(as.matrix(afiliacoes[,1:2]), directed = T)

gcompletao = gcompleto %u% gmaisedges
gcompletao

semtipo = which(is.na(V(gcompletao)$type))
V(gcompletao)$type[semtipo] = TRUE

V(gcompletao)$type

## Prepara a multinível
is_multilevel(gcompletao) #OK

gcompletao = set_color_multilevel(gcompletao)
gcompletao = set_shape_multilevel(gcompletao)
mlayout = layout_multilevel(gcompletao)

plot(gcompletao, 
     #vertex.size = 5, 
     vertex.size = degree(gcompletao, mode = "in")/4,
     vertex.label = NA,
     edge.arrow.size = .3, layout = mlayout
     )
title("Rede Multinível")
legend("topleft", c("Músicos", "Organizações"), pch = 19, col = c('red', 'blue'), pt.cex = 1.5)



individuos = extract_lowlevel(gcompletao)
plot(individuos, vertex.size = degree(individuos, mode = "in"), 
     edge.arrow.size = .2, 
     vertex.label.cex = (degree(individuos, mode = "in")+.1)/7)
