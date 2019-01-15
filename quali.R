# Respostas quali ####
# Verificando algumas respotas qualitativas no banco
freq(bd$`Quanto à cor da pele, o(a) senhor(a) se considera...`, p=F)
summary(bd$`Qual é a sua idade?`)

freq(bd$`Com que frequência o(a) senhor(a) assiste a concertos?`, p=F)

bd$`Porque a orquestra que o(a) senhor(a) disse ser a melhor é a melhor? Cite quantas razões o(a) senhor(a) quiser.`

library(tm)
library(wordcloud)

texto = bd$`Porque a orquestra que o(a) senhor(a) disse ser a melhor é a melhor? Cite quantas razões o(a) senhor(a) quiser.` %>% 
  removePunctuation() %>% removeWords(stopwords("pt")) %>% removeWords("[Q|q]ualidade") %>% 
  removeWords("[O|o]rquestra")
texto = gsub("regente", "maestro", texto)
texto = gsub("[A|a]dm ", "administrativa ", texto)
texto = gsub("staff adm", "staff administrativa ", texto)
#texto = stemDocument(texto, "pt")

pal = RColorBrewer::brewer.pal(9, "Blues")[5:9]
wordcloud(texto, min.freq = 2, max.words = 100, random.order = F, colors = pal)
# png("qualidade_wordcloud.png", height=450, width=450)
# wordcloud(texto, min.freq = 2, max.words = 100, random.order = F, colors = pal)
# dev.off()


corpus = Corpus(VectorSource(enc2native(texto)))
tdm = TermDocumentMatrix(corpus)
tdm = removeSparseTerms(tdm, .9)
dim(tdm)
df <- as.data.frame(as.matrix(tdm))
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, main = "")
# png("qualidade_cluster.png", height=400, width=500)
# plot(fit.ward2, main = "")
# dev.off()

## Prof. qualidade ####
bd$`Caso o(a) senhor(a) seja também professor(a), como o(a) senhor(a) faz seus alunos entenderem o que é necessário para uma boa performance?`
