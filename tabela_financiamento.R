### Tabela de financiamento
library(dplyr)
library(tidyr)
library(xtable)

dados = tibble(total = 29744228.41,
`Governo de Minas` = 18298315.20,
#Rouanet
`Mercantil do Brasil` = 299000,
`Banco Votorantim` = 229000,
`Anglo Gold Ashanti` = 700000,
`Instituto Unimed-BH` = 370133.10,
`Aliança Energia` = 850000,
Picchioni = 180000,
`BTG Pactual` = 350000,
`Banco Intermedium` = 100000,
CEMIG = 150000,
Kinross = 688629.26,
`Banco Itaú` = 1000000,
CBMM = 500000,
# Lei Estado Manutenção
`Telefônica Brasil SA` = 320000,
#Outras fontes
`Venda de Concertos` = 783200,
`Sindicato Hospitais MG` = 120000,
SESI = 485000,
`FUNDEP UFMG` = 58200,
`Cons. Reg. de Contabilidade MG` = 120000,
`Embaixada Americana` = 135291.20,
`Doações livres` = 162255.10,
`Banco Mercantil` = 172000,
`YPO - Capítulo BH` = 10000,
`Diversas doações` = 5916.55,
`Líder Taxi Aéreo` = 200000,
`Bilheteria e Assinaturas` = 2504687.24
)

# Imprime a tabela em formato LaTeX
dados %>% gather(Fonte, Montante, -total) %>% 
  mutate(Perc = (Montante/total)*100) %>% 
  select(-total) %>% 
  mutate(Montante = prettyNum(Montante, big.mark = ".", decimal.mark = ",", digits = 4),
         Perc = prettyNum(Perc, big.mark = ".", decimal.mark = ",", digits = 4)) %>% 
  xtable %>% print.xtable(include.rownames = F)
