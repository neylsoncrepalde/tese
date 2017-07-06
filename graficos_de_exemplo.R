# Gráficos de exemplo da tese
# Neylson Crepalde
############################

setwd("~/Documentos/Neylson Crepalde/Doutorado/Tese/tese")
library(ggplot2)
library(reshape2)

funcaoCustos = function(W, r, t){
  custoNoTempo = c()
  for (i in 1:t){
    custoNoTempo[i] = (W*(1+r)^i)/1
  }
  return(custoNoTempo)
}

Custos = 900
Custos = c(Custos, funcaoCustos(900, .12, 11))
Custo_Fixo = rep(900, 12)
Tempo = 0L:11L
dados = data.frame(Custos = Custos, Custo_Fixo = Custo_Fixo, Tempo = Tempo)
dados2 = melt(dados, id.vars = "Tempo")

ggplot(dados2, aes(x=Tempo, y=value, color = variable))+geom_line(lwd=1)+scale_x_continuous(breaks = 0:11)+
  scale_color_discrete(name=NULL, labels = c("Setor Arcaico", "Setor Progressista"))+labs(y="Custos")+
  theme_bw(base_size = 12)+theme(legend.position = "bottom")
