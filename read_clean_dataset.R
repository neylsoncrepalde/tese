## Banco de dados de indivíduos
## Tese Neylson

library(googlesheets)
library(tidyverse)
library(reshape2)
library(igraph)
library(descr)
library(stringr)
source("sheet_key.R")

gs_auth(new_user = TRUE)
ss = gs_key(sheet_key)
bd = gs_read_csv(ss)
#bd %>% View

names(bd)

# Corrige a renda
bd = bd %>% mutate(renda_orq = if_else(`Qual foi a sua renda total no último mês considerando todas as orquestras nas quais o(a) senhor(a) tocou?` < 1000, 
                                          `Qual foi a sua renda total no último mês considerando todas as orquestras nas quais o(a) senhor(a) tocou?` * 1000, 
                                          `Qual foi a sua renda total no último mês considerando todas as orquestras nas quais o(a) senhor(a) tocou?`))

# Limpa rede1 ####
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
rede1

rede1 = gather(rede1) %>% pull(value) %>% freq(., p=F)
rede1

# Limpa rede2 ####
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
rede2

rede2 = gather(rede2) %>% pull(value) %>% freq(., p=F)
rede2

# Limpa rede3 ####
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
rede3

rede3 = gather(rede3) %>% pull(value) %>% freq(., p=F)
rede3


# Limpa rede4 ####
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
rede4

rede4 = gather(rede4) %>% pull(value) %>% freq(., p=F)
rede4

# Olha tudo ####
freq(c(rownames(rede1),
       rownames(rede2),
       rownames(rede3),
       rownames(rede4)), p=F)
