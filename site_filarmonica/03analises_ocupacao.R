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

# Razão entre médias - método ruim
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

# Monta o modelo com fórmula para predição
fil_2016$pot_ocup = as.factor(fil_2016$pot_ocup)
fil_2016$serie = as.factor(fil_2016$serie)
fil_2016$dia_semana = as.factor(fil_2016$dia_semana)
fil_2016$periodo = as.factor(fil_2016$periodo)
fil_2015$serie = as.factor(fil_2015$serie)
fil_2015$dia_semana = as.factor(fil_2015$dia_semana)
fil_2015$periodo = as.factor(fil_2015$periodo)
fil_2015$tx_ocup = NA
rf.pred = rbind(fil_2016, fil_2015)

limite2016 = rf.pred %>% filter(ano == 2016) %>% nrow
train.formula = sample(1:limite2016, limite2016 * .8)

rf.fit.formula = randomForest(pot_ocup ~ serie + dia_semana + periodo + publico,
                              mtry = 3, data = rf.pred[1:limite2016, ], 
                              importance = T, ntree = 10000, 
                              keep.forest = T, keep.inbag = T,
                              subset = train.formula)
rf.fit.formula

#saveRDS(rf.fit.formula, "rf_fit.rds") # Salva o modelo
#rf.fit.formula = readRDS("rf_fit.rds") # Importa o modelo salvo

testset = fil_2016[1:limite2016,]
testecomx = predict(rf.fit.formula, testset[-train.formula,])
confusionMatrix(table(testecomx, testset$pot_ocup[-train.formula]))
# Predicting
pot_ocup_chapeu.rf = predict(rf.fit.formula,
                             newdata = rf.pred[limite2016+1:nrow(rf.pred), ] )


########################################
# Testando o erro para diversos mtry
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
#######################################

# Atribuindo ao dataset e calcula tx_ocup
fil_2015$pot_ocup = pot_ocup_chapeu.rf[1:nrow(fil_2015)]
class(fil_2015$pot_ocup)
fil_2015$pot_ocup = fil_2015$pot_ocup %>% as.character %>% as.integer
fil_2015$tx_ocup = fil_2015$publico / fil_2015$pot_ocup
summary(fil_2015$tx_ocup)

rf.pred$tx_ocup[(limite2016+1):nrow(rf.pred)] = fil_2015$tx_ocup
rf.pred$pot_ocup[(limite2016+1):nrow(rf.pred)] = fil_2015$pot_ocup

# Juntando no banco original
names(rf.pred)
names(fil_completo)

rf.pred = unique(rf.pred)
rf.pred$pot_ocup = rf.pred$pot_ocup %>% as.character %>% as.integer
dados_prajuntar = rf.pred %>% select(ano, mes, dia_sep, dia_semana, periodo,
                                     compositor, publico, pot_ocup, tx_ocup) %>%
  unique

fil_completo_1516 = left_join(fil_completo, dados_prajuntar)
nrow(fil_completo_1516) == nrow(fil_completo)

# Exportando o banco completo
#write_excel_csv(fil_completo_1516, "concertos_filarmonica_ocupacao1516.csv")



