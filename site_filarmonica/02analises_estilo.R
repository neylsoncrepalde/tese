# Análises referente ao estilo
# Tese - Neylson
# Objeto: repertório da Orquestra Filarmônica de MG
####################################################

library(dplyr)
library(tidyr)
library(readr)
library(descr)
library(ggplot2)
#--------------------
# Multiplot Function
source("https://gist.githubusercontent.com/jgarces02/dc7683d7ff464042e3b9da12ce8dbe97/raw/d2f9b00528138b65a329205b47ca70f4984ca952/multiplot.R")
#--------------------

fil_completo = read_csv("concertos_filarmonica_completo.csv")
compositores = read_csv("fil_compositores_completo.csv")

# Contabilizando os períodos contemplados por número de compositores
freq(compositores$periodo)

#-------------------------
# Separando as obras e os programas de execução, sem repetições
obras_feitas = fil_completo %>% 
  select(ano, mes, dia, local, compositor, serie, obra, periodo) %>% unique

# Contabilizando os períodos por obras feitas
freq(obras_feitas$periodo)
obras_feitas %>% filter(!is.na(periodo)) %>% group_by(periodo) %>% count
  
# Contabilizando os períodos por obras feitas excluindo concertos de câmara
freq(obras_feitas$periodo[!is.na(obras_feitas$periodo) & 
                            obras_feitas$serie != "Concertos de Câmara" &
                            obras_feitas$serie != "Concertos Didáticos"])

obras_feitas %>% filter(!is.na(periodo)) %>%
  filter(serie != "Concertos de Câmara", serie != "Concertos Didáticos") %>%
  group_by(periodo) %>% count

#Elabora os plots
periodo_semna = obras_feitas %>% filter(!is.na(periodo)) %>% 
  ggplot(aes(periodo))+geom_bar()+scale_y_continuous(limits = c(0, 800))

periodo_sem_camara = obras_feitas %>% filter(!is.na(periodo)) %>%
  filter(serie != "Concertos de Câmara", serie != "Concertos Didáticos") %>%
  ggplot(aes(periodo))+geom_bar()+scale_y_continuous(limits = c(0, 800))
  
multiplot(periodo_semna, periodo_sem_camara, cols = 2)

#---------------------------------------
# Testando algumas hipóteses

# Testando a associação entre períodos e série
table(obras_feitas$periodo, obras_feitas$serie)
fisher.test(table(obras_feitas$periodo, obras_feitas$serie),
            simulate.p.value = T, B=10000)
