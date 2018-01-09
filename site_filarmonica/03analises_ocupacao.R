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

# 2016
fil_2016 = fil_ocupacao %>% filter(ano == 2016)
fil_2016$tx_ocup = (fil_2016$publico / fil_2016$pot_ocup) * 100
summary(fil_2016$tx_ocup[fil_2016$local == "Sala Minas Gerais"])

analise_variancia = aov(formula = tx_ocup ~ periodo, data = fil_2016)
summary(analise_variancia)

#Exclui a quarta pq só tem 5 casos
fil_2016 = fil_2016 %>% filter(dia_semana != "quarta")
freq(fil_2016$dia_semana)

fit1 = lm(tx_ocup ~ relevel(factor(periodo), "ROMANTISMO") + 
            relevel(factor(dia_semana), "quinta"), 
          data = fil_2016)
summary(fit1)
car::vif(fit1)
texreg::screenreg(fit1)

# Verificando a média de lugares disponíveis na sala minas gerais
SMG2016 = fil_2016 %>% filter(local == "Sala Minas Gerais") %>%
  group_by(local) %>% summarise(mean = mean(pot_ocup, na.rm = T), 
                                sd = sd(pot_ocup, na.rm = T),
                                median = median(pot_ocup, na.rm = T))

# 2015
fil_2015 = fil_ocupacao %>% filter(ano == 2015, dia_semana != "quarta",
                                   local == "Sala Minas Gerais",
                                   serie != "Especial")
SMG2015 = fil_2015 %>% filter(local == "Sala Minas Gerais") %>%
  group_by(local) %>% summarise(mean = mean(publico, na.rm = T), 
                                sd = sd(publico, na.rm = T),
                                median = median(publico, na.rm = T))

# Razão entre médias
ocupacao_media_2015 = fil_2015$publico / SMG2016$mean
ocupacao_mediana_2015 = fil_2015$publico / SMG2016$median

mediadasduas = apply(cbind(ocupacao_media_2015, ocupacao_mediana_2015),
                     1, mean)

# ----------------------------------------
# Investigando a lógica de oferta de lugares na sala minas gerais em 2016
# "Antedizendo" para 2016

# Usando regressão linear
fit.oferta = lm(pot_ocup ~ serie + relevel(factor(dia_semana), "quinta"), 
                fil_2016)
summary(fit.oferta)
plot(fit.oferta)
car::vif(fit.oferta)

pot_ocup_chapeu = predict.lm(fit.oferta, newdata = fil_2015) %>% round
mean(pot_ocup_chapeu)


# Tentando construir um modelo com Random Forests


