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
# Orçamento anual

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
               salario = c(7000, mean(c(3000,2300)), 2000, 1500, 500))

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
summary(central_musicos_fit)

# Complexidade Organizacional
# Fil = 8 setores e 1 passo entre os músicos e o Diretor Presidente
# Sinf = 1 diretoria artística e 2 passos entre os músicos e o diretor artístico
# Ouro Preto = 8 setores e 1 passo até a Diretoria Artística
# SESI = 1 setor (cooperativa) e 1 passo entre os músicos e o gerente.
# OPUS = 1 setor e 2 passos entre os músicos e o diretor artístico
comp_org = tibble(name = c("Orquestra Filarmônica de MG",
                           "Orquestra Sinfônica de MG",
                           "Orquestra Ouro Preto",
                           "Orquestra SESIMINAS",
                           "OPUS"),
                  complexidade = c(9,3,9,2,3))
atributos = atributos %>% left_join(comp_org) %>% 
  mutate(complexidade = if_else(is.na(complexidade), 0, complexidade))
complexidade_fit = atributos$complexidade
names(complexidade_fit) = atributos$name

# Orçamento Anual (em milhoes de reias)
orca_org = tibble(name = c("Orquestra Filarmônica de MG",
                           "Orquestra Sinfônica de MG",
                           "Orquestra Ouro Preto",
                           "Orquestra SESIMINAS",
                           "OPUS"),
                  orcamento = c(29.74,3.13,2.5,0.75,0.15))
atributos = atributos %>% left_join(orca_org) %>% 
  mutate(orcamento = if_else(is.na(orcamento), 0, orcamento))
orcamento_fit = atributos$orcamento
names(orcamento_fit) = atributos$name


# Verificando a distribuição
Y = data.frame(qualidadeperc = atributos$qualidadeperc+.1)
rownames(Y) = V(organizacoes)$name
vedist1 = fitdistrplus::fitdist(Y$qualidadeperc, "gamma", "mle")
vedist2 = fitdistrplus::fitdist(Y$qualidadeperc, "exp", "mle")
fitdistrplus::gofstat(vedist1)

# MELHOR RESULTADO!!! GAMMA
formulas = list(
  Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
    netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1)
    #covariate(central_musicos_fit, coefname = "central_musicos") +
    #covariate(salario, coefname = "salario") +
    #covariate(orcamento_fit, coefname = "orcamento") +
    #covariate(complexidade_fit, coefname = "complexidade" +
    #centrality(norganizacoes, type = "outdegree") +
    ,
  Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
    netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
    covariate(central_musicos_fit, coefname = "central_musicos")
  #covariate(salario, coefname = "salario") +
  #covariate(orcamento_fit, coefname = "orcamento") +
  #covariate(complexidade_fit, coefname = "complexidade" +
  #centrality(norganizacoes, type = "outdegree") +
  ,
Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
  netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
  covariate(central_musicos_fit, coefname = "central_musicos") +
  covariate(salario, coefname = "salario")
  #covariate(orcamento_fit, coefname = "orcamento") +
  #covariate(complexidade_fit, coefname = "complexidade" +
  #centrality(norganizacoes, type = "outdegree") +
  ,
Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
  netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
  covariate(central_musicos_fit, coefname = "central_musicos") +
  #covariate(salario, coefname = "salario")
  covariate(orcamento_fit, coefname = "orcamento")
  #covariate(complexidade_fit, coefname = "complexidade" +
  #centrality(norganizacoes, type = "outdegree") +
,
Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
  netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
  covariate(central_musicos_fit, coefname = "central_musicos") +
  covariate(salario, coefname = "salario") +
  covariate(orcamento_fit, coefname = "orcamento")
  #covariate(complexidade_fit, coefname = "complexidade" +
  #centrality(norganizacoes, type = "outdegree") +
  ,
  Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
    netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
  covariate(central_musicos_fit, coefname = "central_musicos") +
  covariate(salario, coefname = "salario") +
  covariate(orcamento_fit, coefname = "orcamento") +
  covariate(complexidade_fit, coefname = "complexidade")
  #centrality(norganizacoes, type = "outdegree") +
  ,
    Y ~ netlag(atributos$qualidadeperc, norganizacoes) +
    netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
    covariate(central_musicos_fit, coefname = "central_musicos") +
    covariate(salario, coefname = "salario") +
    covariate(orcamento_fit, coefname = "orcamento") +
    covariate(complexidade_fit, coefname = "complexidade") +
    centrality(norganizacoes, type = "outdegree")
)
models = list()
for (i in 1:length(formulas)) {
  tentativa = try(tnam(formulas[[i]], family = Gamma(link = "log")))
  if ("try-error" %in% class(tentativa)) {
    models[[i]] = tnam(formulas[[i]], family = Gamma(link = "identity"))
  } else {
    models[[i]] = tnam(formulas[[i]], family = Gamma(link = "log"))
  }
}

sapply(models, function(x) print(x$family))
ggplot(NULL, aes(x = 1:length(models))) +
  geom_line(aes(y = sapply(models, AIC)), col = "red") +
  geom_line(aes(y = sapply(models, BIC)), col = "blue")

hist(residuals(models[[7]]))

texreg::screenreg(models, single.row = F)

# Nao deu certo
alaamdata = tnamdata(Y ~ 
                   #centrality(norganizacoes, type = "outdegree") +
                   netlag(atributos$qualidadeperc, norganizacoes) +
                   netlag(atributos$qualidadeperc, norganizacoes, pathdist = 2, decay=1) +
                   covariate(central_musicos_fit, coefname = "central_musicos") +
                   covariate(salario, coefname = "salario") +
                   covariate(orcamento_fit, coefname = "orcamento") +
                   covariate(complexidade_fit, coefname = "complexidade"))
alaamdata$response = as.factor(alaamdata$response)

alaamordlog = MASS::polr(response ~. -node-time, data = alaamdata, Hess = T)
summary(alaamordlog)


texreg::screenreg(list(alaamgamma, alaamordlog), single.row = T)
lmtest::lrtest(alaamgamma, alaamordlog) # teste de likelihood
