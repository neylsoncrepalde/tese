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

# Exporta
#write_excel_csv(novo, "concertos_filarmonica_receitas.csv")

#---------------------------------------------------
# Junta os potenciais de ocupação para antes de 2014

freq(filarmonica$local[filarmonica$ano != 2016 &
                         filarmonica$ano != 2015], plot=F)

#Locais para juntar
FEA = 0
PALACIODASARTES = 0
SALASAOPAULO = 0
SESCPALLADIUM = 0
BRADESCO = 0
OIFUTURO = 0
TEATROSESIMINAS = 0
TEATROMUNICIPALDORIO = 0



