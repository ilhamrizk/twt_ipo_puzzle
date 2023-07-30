#=============================================#
#==== DATA TESTING DAN TRAINING 80 : 20 ======#
#=============================================#

library(dplyr)
library(tm)
library(naivebayes)
library(rminer)
library(wordcloud)
library(e1071)


#===========================================#
#   Split Data to Data Training & Testing   #
#===========================================#

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Dengan Saham Teknologi")
df<- read.csv("labeling_result.csv", stringsAsFactors = FALSE)
glimpse(df)
colnames(df)
colnames(df)[1] = 'sentiment'
nrow(df)
colnames(df)
sapply(df,class)

#Define Data And Split
df_pos = df[df$sentiment == 1,]
glimpse(df_pos)
df_neg = df[df$sentiment == -1,]
glimpse(df_neg)
df_net = df[df$sentiment == 0,]
glimpse(df_net)


#Data Training
set.seed(100)
df_pos = df_pos[sample(nrow(df_pos)), ]
df_neg = df_neg[sample(nrow(df_neg)), ]
df_net = df_net[sample(nrow(df_net)), ]

#Ngambil 80% data
df_pos_nrow = nrow(df_pos)
train_n_pos = round(df_pos_nrow * 0.8)
df_neg_nrow = nrow(df_neg)
train_n_neg = round(df_neg_nrow * 0.8)
df_net_nrow = nrow(df_net)
train_n_net = round(df_net_nrow * 0.8)

#Buat data train dan test
df.train_pos=df_pos[1:train_n_pos,]
df.test_pos=df_pos[(train_n_pos+1):df_pos_nrow,]
df.train_neg=df_neg[1:train_n_neg,]
df.test_neg=df_neg[(train_n_neg+1):df_neg_nrow,]
df.train_net=df_net[1:train_n_net,]
df.test_net=df_net[(train_n_net+1):df_net_nrow,]

#Union 
reviewTrain=merge(x=df.train_pos,y=df.train_neg,all=TRUE)
reviewTrain=merge(x=reviewTrain,y=df.train_net,all=TRUE)
glimpse(reviewTrain)
reviewTest=merge(x=df.test_pos,y=df.test_neg,all=TRUE)
reviewTest=merge(x=reviewTest,y=df.test_net,all=TRUE)
glimpse(reviewTest)

reviewTrain$class=as.factor(reviewTrain$sentiment)
reviewTest$class=as.factor(reviewTest$sentiment)
nrow(reviewTest)
nrow(reviewTrain)


#Buat Corpus
corpus.train <- Corpus(VectorSource(reviewTrain$tweet))
inspect(corpus.train[1:3])
corpus.test <- Corpus(VectorSource(reviewTest$tweet))
inspect(corpusTest[1:3])

#Clean Review
corpus.train=tm_map(corpus.train,tolower)
corpus.test=tm_map(corpus.test,tolower)
inspect(corpus.train[1:5])

corpus.train=tm_map(corpus.train,removePunctuation)
corpus.trest=tm_map(corpus.test,removePunctuation)
inspect(corpus.train[1:5])

corpus.train=tm_map(corpus.train,removeNumbers)
corpus.test=tm_map(corpus.test,removeNumbers)
inspect(corpus.train[1:5])


#Menghapus white space
corpus.train=tm_map(corpus.train,stripWhitespace)
corpus.test=tm_map(corpus.test,stripWhitespace)
inspect(corpus.train[1:5])



#Membuat Dokumen Term matrix dari corpus
dtm.train <- DocumentTermMatrix(corpus.train)
dtm.test <- DocumentTermMatrix(corpus.test)
inspect(dtm.train[40:50, 10:15])
nrow(dtm.train)
nrow(dtm.test)

#Membuat Dokumen Term matrix dari corpus
dtm.train <- DocumentTermMatrix(corpus.train)
dtm.test <- DocumentTermMatrix(corpus.test)
inspect(dtm.train[40:50, 10:15])
nrow(dtm.train)

#Five Freq
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
head(fivefreq)
dtm.train.fivefreq <- DocumentTermMatrix(corpus.train, control=list(dictionary = 
                                                                      fivefreq))
inspect(dtm.train.fivefreq[40:50, 10:15])


#Five Freq
length((fivefreq))
head(fivefreq)
dtm.test.fivefreq <- DocumentTermMatrix(corpus.test, control=list(dictionary = 
                                                                      fivefreq))
inspect(dtm.test.fivefreq[40:50, 10:15])




#Klasifikasi dengan SVM
library(e1071)
library(caTools)
library(tm)
library(SnowballC)
library(randomForest)


dfSparse.train=as.data.frame(as.matrix(dtm.train.fivefreq))
colnames(dfSparse.train)=make.names(colnames(dfSparse.train),unique = TRUE)

dfSparse.test=as.data.frame(as.matrix(dtm.test.fivefreq))
colnames(dfSparse.test)=make.names(colnames(dfSparse.test),unique = TRUE)
head(dfSparse.train,10)

dfSparse.train$class=reviewTrain$class

dfSparse.test$class=reviewTest$class
head(dfSparse.train$class,10)


sentimenSVM=svm(class~.,data=dfSparse.train, kernel = "radial", cost = 50, gamma = 0.009)
sentimenSVM

# cost = 20 , gamma = 0.005 = 50%
# cost = 50, gamma = 0.009 = 63.9%

predictSVM_test=predict(sentimenSVM,newdata=dfSparse.test)
cmat_sentimenSVM_test=table(dfSparse.test$class,predictSVM_test)
cmat_sentimenSVM_test


predictSVM_train=predict(sentimenSVM,newdata=dfSparse.train)
cmat_sentimenSVM_train=table(dfSparse.train$class,predictSVM_train)
cmat_sentimenSVM_train

