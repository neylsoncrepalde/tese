# Tese Neylson
# ALAAM Model

library(tnam)

plot(organizacoes, vertex.label=NA, vertex.size = 5, edge.arrow.size=.3)
plot.network(norganizacoes)

# Preparar vetores: ####
# Qualidade percebida
# Proximidade com o Estado
# Prestígio dos músicos que pertencem a ela
# Complexidade organizacional

norganizacoes = asNetwork(organizacoes)
atributos = tibble(name = V(organizacoes)$name,
                   qualidadeperc = case_when(
                     name == "Orquestra Filarmônica de MG" ~ 5,
                     name == "Orquestra Sinfônica de MG" ~ 4,
                     name == "Orquestra Ouro Preto" ~ 3,
                     name == "Orquestra SESIMINAS" ~ 2,
                     name == "OPUS" ~ 1,
                     TRUE ~ 0
                   )
)

# Prestígio dos músicos
central_musicos = afiliacoes %>% 
  left_join(tibble(sender=V(gprestigio)$name, 
                   centralidade = igraph::degree(gprestigio, mode="in"))) %>% 
  group_by(receiver) %>% summarise(central_musicos = sum(centralidade))
names(central_musicos)[1] = "name"

atributos = atributos %>% left_join(central_musicos) %>% 
  mutate(central_musicos = if_else(is.na(central_musicos), 0, central_musicos))
central_musicos_fit = atributos$central_musicos
names(central_musicos_fit) = atributos$name

# Complexidade Organizacional


# Fit the model
Y = data.frame(qualidadeperc = atributos$qualidadeperc)
rownames(Y) = V(organizacoes)$name

alaamfit = tnam(Y ~ 
                  centrality(norganizacoes, type = "outdegree") +
                  netlag(atributos$qualidadeperc, norganizacoes) +
                  netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
                  covariate(central_musicos_fit, coefname = "central_musicos"))


summary(alaamfit)
texreg::screenreg(alaamfit, single.row = T)
