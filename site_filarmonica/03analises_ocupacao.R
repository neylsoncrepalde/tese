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

# Usando regressão logística multinomial
library(nnet)
library(caret)
fil_2016 = fil_2016 %>% filter(!is.na(pot_ocup))
fil_2016$pot_ocup = as.factor(fil_2016$pot_ocup)

train.multi = sample(1:nrow(fil_2016), nrow(fil_2016) * .7)
fit.oferta = multinom(factor(pot_ocup) ~ factor(serie) + 
                  relevel(factor(dia_semana), "quinta") + publico, 
                fil_2016, model = T, subset = train.multi)
summary(fit.oferta)
# Testando o modelo
pred.multi = predict(fit.oferta, fil_2016[-train.multi,])
confusion.multi = table(pred.multi, fil_2016$pot_ocup[-train.multi])
confusionMatrix(confusion.multi)

pot_ocup_chapeu = predict(fit.oferta, newdata = fil_2015) # BOM!


# Tentando construir um modelo com Random Forests
library(randomForest)
library(RFinfer)
rf.dataset = fil_2016 %>% 
  select(pot_ocup, serie, dia_semana, periodo, publico) %>%
  filter(complete.cases(.) == T)

rf.dataset$serie = as.factor(rf.dataset$serie)
rf.dataset$dia_semana = as.factor(rf.dataset$dia_semana)
rf.dataset$periodo = as.factor(rf.dataset$periodo)

train = sample(1:nrow(rf.dataset), nrow(rf.dataset) * .8)

Xtrain = model.matrix(pot_ocup ~ serie + dia_semana + periodo + publico, 
                 data = rf.dataset[train,])
Ytrain = rf.dataset$pot_ocup[train]
  
Xtest = model.matrix(pot_ocup ~ serie + dia_semana + periodo + publico, 
                     data = rf.dataset[-train,])
Ytest = rf.dataset$pot_ocup[-train]


rf.fit = randomForest(x = Xtrain, y = Ytrain, xtest = Xtest, ytest = Ytest, 
                      mtry = 3, data = rf.dataset, importance = T,
                      ntree = 10000, keep.forest = T, keep.inbag = T)
rf.fit
# Monta a confusion matrix da Random Forest
rf.confmat = table(rf.fit$test$predicted, Ytest)
# Testa a confusion matrix
confusionMatrix(rf.confmat)


# Testando o erro
oob.err = double(3)
oob.test = double(3)
for(mtry in 1:3){
  fit = randomForest(x = Xtrain, y = Ytrain, xtest = Xtest, ytest = Ytest, 
                   mtry = mtry, data = rf.dataset, importance = T)
  oob.err[mtry] = fit$err.rate[500]
  oob.test[mtry] = fit$test$err.rate[500]
  cat(mtry," ")
}
matplot(1:mtry, cbind(oob.err,oob.test) ,pch=19,col=c("red", "blue"),type="b",
        ylab="Mean Squared Error")
legend("topright",legend=c("OOB train", "OOB test"),pch=19,col=c("red", "blue"))
cbind(oob.err,oob.test)
#####

# Predição para 2015
pred.2015 = fil_2015 %>% 
  select(ano, mes, publico, serie, dia_semana, periodo)
pred.2015$serie = factor(pred.2015$serie)
pred.2015$dia_semana = factor(pred.2015$dia_semana)
pred.2015$periodo = factor(pred.2015$periodo)

X2015 = model.matrix(ano~serie + dia_semana + periodo, data = pred.2015)
X2015



