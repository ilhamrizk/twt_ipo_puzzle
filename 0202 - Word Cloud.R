library(dplyr)
library(tm)
library(naivebayes)
library(rminer)
library(wordcloud)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/Output")
getwd()
df =  read.csv("Data Gabungan Analisis.csv")

#Fase 1
df = df[df$Actual.fase == 'fase 2',]
nrow(df)
head(df,10)
df_pos = df[df$Predicted_class == 1,]
nrow(df_pos)
head(df_pos,10)
df_net = df[df$Predicted_class == 0,][1:70000,]
nrow(df_net)
df_neg = df[df$Predicted_class == -1,]
nrow(df_neg)


#Build Corpus
library(tm)
corpus_pos=iconv(df_pos$Actual.tweet)
corpus_pos=Corpus(VectorSource(corpus_pos))
inspect(corpus_pos[1:5])

corpus_neg=iconv(df_neg$Actual.tweet)
corpus_neg=Corpus(VectorSource(corpus_neg))
inspect(corpus_neg[1:5])

corpus_net = iconv(df_net$Actual.tweet)
corpus_net=Corpus(VectorSource(corpus_net))
inspect(corpus_net[1:5])



#Clean Review
corpus_pos=tm_map(corpus_pos,tolower)
corpus_pos=tm_map(corpus_pos,removePunctuation)
corpus_pos=tm_map(corpus_pos,removeNumbers)

corpus_net=tm_map(corpus_net,tolower)
corpus_net=tm_map(corpus_net,removePunctuation)
corpus_net=tm_map(corpus_net,removeNumbers)



#Menghapus white space
corpus_pos=tm_map(corpus_pos,stripWhitespace)
inspect(corpus_pos[1:5])
corpus_neg=tm_map(corpus_neg,stripWhitespace)
inspect(corpus_neg[1:5])
corpus_net=tm_map(corpus_net,stripWhitespace)
inspect(corpus_net[1:5])

#Document Term Matrix
dtm_pos=TermDocumentMatrix(corpus_pos)
m_pos=as.matrix(dtm_pos)
v_pos=sort(rowSums(m_pos),decreasing=TRUE)
d_pos=data.frame(word=names(v_pos),freq=v_pos)
head(d_pos,20)

dtm_neg=TermDocumentMatrix(corpus_neg)
m_neg=as.matrix(dtm_neg)
v_neg=sort(rowSums(m_neg),decreasing=TRUE)
d_neg=data.frame(word=names(v_neg),freq=v_neg)
head(d_neg,20)

dtm_net=TermDocumentMatrix(corpus_net)
m_net=as.matrix(dtm_net)
v_net=sort(rowSums(m_net),decreasing=TRUE)
d_net=data.frame(word=names(v_net),freq=v_net)
head(d_net,25)


#wordcloud
wordcloud(corpus_pos,random.order=F,min.freq=100,
          max.words=Inf,colors=brewer.pal(8,"Dark2"))
wordcloud(corpus_neg,random.order=F,min.freq=100,
          max.words=Inf,colors=brewer.pal(8,"Dark2"))
wordcloud(corpus_net,random.order=F,min.freq=400,
          max.words=Inf,colors=brewer.pal(8,"Dark2"))

