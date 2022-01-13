#Twitter e Text Mining - happy
#https://www.outspokenmarket.com/blog
#Leandro Guerra
#Adaptação Johnathan R. S. Borba
#Instalando as bibliotecas necessarias
# install.packages("rtweet")
# install.packages("wordcloud")
# install.packages("tm")

#Carregando as libraries
library(rtweet)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(cluster)   
library(fpc)

#####
#Carregando os Tweets
#Voce precisar ter uma conta no Twitter e autorizar
#Limite de 18.000 tweets a cada 15 minutos
Happy_tweets <- search_tweets(
  "#Happy", n = 1000, include_rts = FALSE,lang = "en")

#Rapida visualizaçao - exemplo tirado da propia documentaçao da rtweet
Happy_tweets %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequencia da palavra #happy Twitter posts",
    subtitle = "Tweets a cada 1 hora",
    caption = "\nSource: Dados coletados da Twitter's REST API via rtweet"
  )

#####
#O trabalho de Mineraçao de Textos - Text Mining
happy_text <- Happy_tweets$text

#Criando e limpando o corpus
happy_text_corpus <- VCorpus(VectorSource(happy_text))
happy_text_corpus <- tm_map(happy_text_corpus,
                                     content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
happy_text_corpus <- tm_map(happy_text_corpus, content_transformer(tolower))
happy_text_corpus <- tm_map(happy_text_corpus, removePunctuation)
happy_text_corpus <- tm_map(happy_text_corpus,removeWords, stopwords("english"))

#Primeira visualizaçao
wordcloud(happy_text_corpus,min.freq=2,max.words=100)
formatacao <- brewer.pal(8,"Dark2")
wordcloud(happy_text_corpus,min.freq=2,max.words=100, random.order=T, colors=formatacao)

#Mas ainda aparece muito lixo

#####
#Limpeza do texto com a Document Term Matrix
happy_dtm <- DocumentTermMatrix(happy_text_corpus)   
happy_dtm

happy_frequencia <- colSums(as.matrix(happy_dtm))   
length(happy_frequencia) 
tail(happy_frequencia,10)

#Removendo termos esparços
happy_dtms <- removeSparseTerms(happy_dtm, 0.98) 
happy_dtms

happy_frequencia <- colSums(as.matrix(happy_dtms))   
length(happy_frequencia) 

happy_frequencia <- sort(colSums(as.matrix(happy_dtms)), decreasing=TRUE) 
happy_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
happy_plot <- data.frame(word=names(happy_frequencia), freq=happy_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(happy_plot, happy_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico   

#Removendo palavras especificas e limpando novamente o corpus
happy_text_corpus <- tm_map(happy_text_corpus, removeWords, c("not","sad", "lohri", "instagood"))
happy_dtms <- removeSparseTerms(DocumentTermMatrix(happy_text_corpus) , 0.98) 
happy_dtms

happy_frequencia <- colSums(as.matrix(happy_dtms))   
length(happy_frequencia) 

happy_frequencia <- sort(colSums(as.matrix(happy_dtms)), decreasing=TRUE) 
happy_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
happy_plot <- data.frame(word=names(happy_frequencia), freq=happy_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(happy_plot, happy_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico   


#Nova nuvem de palavras
wordcloud(names(happy_frequencia),happy_frequencia,min.freq=2,max.words=150, random.order=T, colors=formatacao)

#####
#Aplicando um pouco de machine learning - Clustering
happy_dtms2 <- removeSparseTerms(happy_dtms, 0.95)
happy_dtms2

#Clustering 1 - Dendograma
distancia <- dist(t(happy_dtms2), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Tweets happy",
     xlab = "Distancia",
     ylab = "Altura")  

#Para ler melhor o Dendograma
groups <- cutree(dendograma, k=5)
rect.hclust(dendograma, k=5, border="red")   

#Clustering 2 - K-Means
kmeans_btc <- kmeans(distancia, 5)   
clusplot(as.matrix(distancia), kmeans_btc$cluster, color=T, shade=T, labels=3, lines=0,
         main = "K-Means Tweets happy",
         xlab = "PC1",
         ylab = "PC2") 

