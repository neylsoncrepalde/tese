# Processa dados repertório Filarmônica
# Neylson Crepalde
# Tese
#######################################

library(dplyr)
library(tidyr)
library(readr)

fil2016 = read_csv("concertos_filarmonica_2016.csv")
names(fil2016)

# Separa compositor e obra
fil2016 = fil2016 %>% separate(repertorio, c("compositor", "obra"), sep = "\\|")

# Separa os dias
fil2016 = fil2016 %>% mutate(V2 = strsplit(dia, " e ")) %>% unnest

# Exporta um csv com ano, mes e dias separados para juntar com 
# taxa de ocupação
fil2016 %>% select(ano, mes, V2) %>% unique %>% 
  write_excel_csv(., "occupancy_2016.csv")
