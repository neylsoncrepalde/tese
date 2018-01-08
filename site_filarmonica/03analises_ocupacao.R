# Análises referentes à ocupação
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
ocupacao = read_csv("occupancy_completo.csv")
names(ocupacao)
names(fil_completo)

# Faz o merge colocando a informação da ocupação
fil_ocupacao = left_join(fil_completo, ocupacao)

#-------------------------------------------------

### ANÁLISES
View(fil_ocupacao)

fil_2016 = fil_ocupacao %>% filter(ano == 2016)
fil_2016$tx_ocup = (fil_2016$publico / fil_2016$pot_ocup) * 100
summary(fil_2016$tx_ocup)

analise_variancia = aov(formula = tx_ocup ~ periodo, data = fil_2016)
summary(analise_variancia)

fit1 = lm(tx_ocup ~ periodo + serie, data = fil_2016)
summary(fit1)
