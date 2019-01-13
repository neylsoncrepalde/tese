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

# Incentivos financeiros - renda
salmed = tibble(sender = c("Orquestra Filarmônica de MG", "Orquestra Sinfônica de MG",
                               "Orquestra Ouro Preto", "Orquestra SESIMINAS", "OPUS"),
               salario = c(7000, mean(c(3000,2300)), 3000, 1500, 1500))

renda = tibble(sender=V(organizacoes)$name) %>% left_join(salmed)
salario = renda$salario
salario[is.na(salario)] = 0
names(salario) = renda$sender

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
comp_org = tibble(name = c("Orquestra Filarmônica de MG",
                           "Orquestra Sinfônica de MG",
                           "Orquestra Ouro Preto",
                           "Orquestra SESIMINAS",
                           "OPUS"),
                  complexidade = c())

# Fit the model
Y = data.frame(qualidadeperc = atributos$qualidadeperc)
rownames(Y) = V(organizacoes)$name

alaamfit = tnam(Y ~ 
                  centrality(norganizacoes, type = "outdegree") +
                  netlag(atributos$qualidadeperc, norganizacoes) +
                  netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
                  covariate(central_musicos_fit, coefname = "central_musicos") +
                  covariate(salario, coefname = "salario"))


summary(alaamfit)
texreg::screenreg(alaamfit, single.row = T)
