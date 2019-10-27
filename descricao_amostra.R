## Analise da amostra de musicos

library(googlesheets)
library(reshape2)
library(tidyverse)
library(igraph)
library(descr)
source("sheet_key.R")
ss = gs_key(sheet_key)
bd = gs_read(ss)
bd = bd[-10,]

freq(bd$`Qual é a sua escolaridade?`)

bd %>% 
  group_by(`Qual é a sua escolaridade?`) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=`Qual é a sua escolaridade?`,
             y=n)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(y='')


freq(bd$`Quanto à cor da pele, o(a) senhor(a) se considera...`)
summary(bd$`Qual é a sua idade?`)

freq(bd$`O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`)
affs = bd %>% select(`Qual é o seu nome?`, `O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`)
affs
n_max = melt(strsplit(affs$`O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`, ", ")) %>% group_by(L1) %>% summarise(n=n()) %>%
  pull(n) %>% max
redeaff = cbind(
  sender = affs$`Qual é o seu nome?`,
  colsplit(affs$`O(a) senhor(a) toca em alguma orquestra? Qual ou quais?`, ", ", sapply(1:n_max, function(x) paste0("col", x)))
) %>% as_tibble

redeaff = redeaff %>% gather(var, receiver, -sender) %>% select(-var) %>%
  filter(receiver != "", receiver != "Não", receiver != "Freelancer") %>%
  mutate(sender = as.character(sender))
orquestras = redeaff$receiver

freq(orquestras)




