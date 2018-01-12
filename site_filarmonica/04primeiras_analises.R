# 4 - Primeiras analises
# Orquestra Filarmônica
# Tese - Neylson Crepalde
##########################

library(readr)
library(dplyr)
filarmonica = read_csv("concertos_filarmonica_ocupacao1516.csv")
fil1516 = filarmonica %>% filter(ano == 2015 | ano == 2016)

