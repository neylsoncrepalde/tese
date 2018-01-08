# Processa dados repertório Filarmônica
# Neylson Crepalde
# Tese
#######################################

library(dplyr)
library(tidyr)
library(readr)

fil2016 = read_csv("concertos_filarmonica_2016_corrigido.csv")
fil2015 = read_csv("concertos_filarmonica_2015_corrigido.csv")
fil2014 = read_csv("concertos_filarmonica_2014_corrigido.csv")
fil2013 = read_csv("concertos_filarmonica_2013_corrigido.csv")
fil2012 = read_csv("concertos_filarmonica_2012_corrigido.csv")
fil2011 = read_csv("concertos_filarmonica_2011_corrigido.csv")
fil2010 = read_csv("concertos_filarmonica_2010_corrigido.csv")
fil2009 = read_csv("concertos_filarmonica_2009_corrigido.csv")
fil2008 = read_csv("concertos_filarmonica_2008_corrigido.csv")

# Junta todos num banco de dados completo
fil_completo = rbind(fil2016, fil2015, fil2014, fil2013, fil2012, fil2011,
                     fil2010, fil2009, fil2008) #OK

# Separa compositor e obra
fil_completo = fil_completo %>% 
  separate(repertorio, c("compositor", "obra"), sep = "[[:space:]]\\|")

# Exporta um csv com compositores para classificação de estilo
#fil_completo %>% select(compositor) %>% unique %>% arrange(compositor) %>%
#  write_excel_csv(., "fil_compositores.csv") #OK

# Após completar as informações, lê o banco das classificações do repertório
compositores = read_csv("fil_compositores_completo.csv")

# Merge classificação do período com o banco de dados completo
fil_completo = left_join(fil_completo, compositores, by = "compositor")

#############################
# Corrigindo um problema de codificacao de dias errado
fil_completo = read_csv("concertos_filarmonica_paracorrigir.csv")
# Separa os dias
fil_completo = fil_completo %>% unique %>% 
  mutate(dia_sep = strsplit(dia, " e ")) %>% unnest


# Exporta banco de dados completo
write_csv(fil_completo, "concertos_filarmonica_completo.csv")

# Exporta um csv com ano, mes e dias separados para juntar com 
# taxa de ocupação
fil_completo %>% select(ano, mes, dia_sep, serie) %>% unique %>% 
  write_excel_csv(., "occupancy_2016.csv")
