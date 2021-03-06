# 4 - Primeiras analises
# Orquestra Filarmônica
# Tese - Neylson Crepalde
##########################

library(readr)
library(dplyr)
library(ggplot2)
library(descr)
library(xtable)
library(FactoMineR)
library(factoextra)
library(forcats)

##########################################
# Corrige problema na escala para 2015
# Não rodar. Foi feito apenas uma vez!
#filarmonica = read_csv("concertos_filarmonica_ocupacao1516.csv")
#banco_corrigir = filarmonica %>% filter(ano == 2015) %>% select(tx_ocup)
#filarmonica$tx_ocup[filarmonica$ano == 2015] = banco_corrigir$tx_ocup * 100
#filarmonica %>% filter(ano == 2015) %>% View
#write_excel_csv(filarmonica, "concertos_filarmonica_ocupacao1516.csv")
#########################################

filarmonica = read_csv(paste0(getwd(), "/site_filarmonica/concertos_filarmonica_ocupacao1516.csv"))
fil1516 = filarmonica %>% filter(ano == 2015 | ano == 2016)

# Número de concertos realizados nos dois anos
fil1516 %>% select(ano, mes, dia_sep) %>% unique %>% nrow

# Número de concertos realizados na sala Minas Gerais
fil1516 %>% filter(local == "Sala Minas Gerais") %>% 
  select(ano, mes, dia_sep) %>% unique %>% nrow

# Períodos dos compositores tocados por ano
fil1516 %>% select(ano, mes, dia_sep, periodo) %>% unique %>% 
  ggplot(aes(periodo))+geom_bar()+coord_flip()+facet_wrap(~ano)+
  labs(x="Período", y="")

# Períodos dos compositores por serie
pdf("periodo_perserie_year.pdf", height = 5, width = 7)
fil1516 %>% select(ano, mes, dia_sep, serie, periodo) %>% 
  filter(serie != "Concertos de Cãmara", serie != "Concertos Didáticos", 
         serie != "Especial", serie != "Festival Tinta Fresca", 
         serie != "Laboratório de Regência", 
         serie != "Inhotim", serie != "Turnê Estadual",
         serie != "Clássicos na Praça", serie != "Concertos de Câmara",
         serie != "Fora de Série") %>%
  unique %>% 
  ggplot(aes(periodo))+geom_bar()+coord_flip()+facet_grid(ano~serie)+
  labs(x="Período", y="")
dev.off()


# Compositores mais tocados por ano
fil1516 %>% select(ano, mes, dia_sep, compositor) %>% unique %>% 
  group_by(ano) %>% count(compositor) %>% arrange(desc(ano), desc(n)) %>% View

# Investigando fatores de impacto no consumo
freq(fil1516$serie, plot=F)
regdata = fil1516 %>% filter(ano == 2016, serie == "Concertos para a Juventude" |
                               serie == "Fora de Série" | 
                               serie == "Allegro/Vivace" |
                               serie == "Presto/Veloce") %>%
  select(-artista, -compositor, -obra, -periodo) %>% unique

fit_consumo = lm(tx_ocup ~ relevel(factor(dia_semana), ref = "sexta") + serie,
                 data = regdata); summary(fit_consumo)
fit_consumo_dia = lm(tx_ocup ~ relevel(factor(dia_semana), ref = "sexta"),
                 data = regdata); summary(fit_consumo_dia)
fit_consumo_serie = lm(tx_ocup ~ serie,
                 data = regdata); summary(fit_consumo_serie)
# Exporta 3 modelos para Latex
texreg::texreg(list(fit_consumo_dia, fit_consumo_serie),
               caption = "Concert consumption - Linear models", 
               caption.above = T, center = F)

## Conselho do Silvio - fazer pairwise t testes
pairwise.t.test(regdata$tx_ocup, regdata$dia_semana)$p.value %>% xtable

pairwise.t.test(regdata$tx_ocup, regdata$serie)$p.value %>% xtable


## Figura com médias de taxa de ocupação por categorias diversas
regdata %>% select(tx_ocup, dia_semana, serie) %>% tidyr::gather(key, value, -tx_ocup) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(#x = value,
    x = fct_rev(fct_relevel(value, c("domingo", "sábado", "sexta", "quinta",
                                      "Concertos para a Juventude", "Fora de Série",
                                      "Presto/Veloce"))),
             y = tx_ocup)) +
  geom_boxplot() +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y="Taxa de Ocupação")
ggsave("~/tese/taxa_ocup_dia_serie.png", height=4, width=6, dpi = 100)


# Verificando a taxa de ocupação por dia da semana
fil1516 %>% group_by(dia_semana) %>% 
  summarise(mean = mean(tx_ocup, na.rm = T), 
            median = median(tx_ocup, na.rm = T), 
            sd = sd(tx_ocup, na.rm = T),
            min = min(tx_ocup, na.rm = T), 
            max = max(tx_ocup, na.rm = T))

# Verificando a taxa de ocupação por dia da semana para 2016
fil1516 %>% filter(ano == 2016) %>% 
  group_by(dia_semana) %>% 
  summarise(mean = mean(tx_ocup, na.rm = T), 
            median = median(tx_ocup, na.rm = T), 
            sd = sd(tx_ocup, na.rm = T),
            min = min(tx_ocup, na.rm = T), 
            max = max(tx_ocup, na.rm = T)) %>% xtable %>% 
  print.xtable(include.rownames = F)

# Verificando a taxa de ocupação por serie para 2016
fil1516 %>% filter(ano == 2016) %>% 
  group_by(serie) %>% 
  summarise(mean = mean(tx_ocup, na.rm = T), 
            median = median(tx_ocup, na.rm = T), 
            sd = sd(tx_ocup, na.rm = T),
            min = min(tx_ocup, na.rm = T), 
            max = max(tx_ocup, na.rm = T)) %>% xtable %>% 
  print.xtable(include.rownames = F)

##
# Análise de correspondência entre
# tx de ocupação, dia da semana e serie
# Verificar como inserir tx de ocupação na estimação