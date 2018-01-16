# Junta receitas de concertos com banco completo
# Tese Neylson Crepalde
# Ainda na CSO - SciencesPo / Paris
################################################

library(readr)
library(dplyr)
library(descr)

filarmonica = read_csv("concertos_filarmonica_ocupacao1516.csv")
receitas = read.table("receitas_concertos.csv", sep = ",", dec = ".", header = T)
head(receitas)

novo = left_join(filarmonica, receitas)
novo$receita_total = novo$bilheteria + novo$assinaturas

#---------------------------------------------------
# Junta os potenciais de ocupação para antes de 2014

freq(filarmonica$local[filarmonica$ano != 2016 &
                         filarmonica$ano != 2015], plot=F)

#Locais para juntar
FEA = 200
PALACIODASARTES = 1707
SALASAOPAULO = 1484
SESCPALLADIUM = 1321
BRADESCO = 602
OIFUTURO = 329
TEATROSESIMINAS = 660
TEATROMUNICIPALDORIO = 2252

novo$pot_ocup[novo$local == "Fundação de Educação Artística"] = FEA
novo$pot_ocup[novo$local == "Palácio das Artes"] = PALACIODASARTES
novo$pot_ocup[novo$local == "Sala São Paulo"] = SALASAOPAULO
novo$pot_ocup[novo$local == "Sesc Palladium"] = SESCPALLADIUM
novo$pot_ocup[novo$local == "Teatro Bradesco"] = BRADESCO
novo$pot_ocup[novo$local == "Teatro do Oi Futuro Klauss Vianna"] = OIFUTURO
novo$pot_ocup[novo$local == "Teatro Sesiminas"] = TEATROSESIMINAS
novo$pot_ocup[novo$local == "Theatro Municipal do Rio de Janeiro"] = TEATROMUNICIPALDORIO
#------------------------------------------------

publico = read_csv("publico_2014paratras.csv")
completo = left_join(novo, publico, by = c("ano", "mes", "dia_sep",
                                           "serie", "dia_semana"))
limite2014 = completo %>% filter(ano == 2016 | ano == 2015) %>% nrow

completo$publico.x[limite2014:nrow(completo)] = completo$publico.y[limite2014:nrow(completo)]
names(completo)[12] = "publico"
completo = completo %>% select(-publico.y)

# corrige tx_ocup
completo$tx_ocup = (completo$publico / completo$pot_ocup) * 100


# Exporta
#write_excel_csv(novo, "concertos_filarmonica_receitas.csv") antes da correção final
#write_excel_csv(completo, "dataset_filarmonica.csv") banco completo!
#------------------------------------------------------------