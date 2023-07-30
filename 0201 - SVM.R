#===========================================#
#         Labeling Data Keseluruhan         #
#===========================================#
library(dplyr)
library(tm)
library(naivebayes)
library(rminer)
library(wordcloud)
library(e1071)
require(dplyr)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
df<- read.csv("labeling_result.csv", stringsAsFactors = FALSE)

#Fase 1
df = df[df$fase == 'fase 2',]

glimpse(df)
colnames(df)
colnames(df)[1] = 'sentiment'
nrow(df)


setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
data_sektor_1 = read.csv("Segmentasi_Kode_List.csv", stringsAsFactors = FALSE)

data_sektor = data_sektor_1[,c("kode","Sektor")]
nrow(data_sektor)

df_test_join = merge(x=df, y = data_sektor,by = "kode", all.x = TRUE)
df_test = df_test_join %>% filter(Sektor != "Technology")
nrow(df_test)

df = df_test
nrow(df)
head(df)
unique(df$Sektor)


#Data Training
df$class=as.factor(df$sentiment)
sapply(df, class)

reviewTrain = df
nrow(reviewTrain)

#Membuat Vektor dari msg
corpus.train <- Corpus(VectorSource(reviewTrain$tweet))
inspect(corpus.train[1:3])

#Clean Review
corpus.train=tm_map(corpus.train,tolower)
inspect(corpus.train[1:5])
corpus.train=tm_map(corpus.train,removePunctuation)
inspect(corpus.train[1:5])
corpus.train=tm_map(corpus.train,removeNumbers)
inspect(corpus.train[1:5])
#Menghapus white space
corpus.train=tm_map(corpus.train,stripWhitespace)
inspect(corpus.train[1:5])


#Membuat Dokumen Term matrix dari corpus
dtm.train <- DocumentTermMatrix(corpus.train)
inspect(dtm.train[40:50, 10:15])
nrow(dtm.train)

#
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
head(fivefreq)
dtm.train.fivefreq <- DocumentTermMatrix(corpus.train, control=list(dictionary = 
                                                                      fivefreq))
inspect(dtm.train.fivefreq[40:50, 10:15])


#Klasifikasi dengan SVM
library(e1071)
library(caTools)
library(tm)
library(SnowballC)
library(randomForest)

#Merubah data menjadi matrix
dfSparse.train=as.data.frame(as.matrix(dtm.train.fivefreq))
colnames(dfSparse.train)=make.names(colnames(dfSparse.train),unique = TRUE)
head(dfSparse.train,10)

#Mengambil label class
dfSparse.train$class=reviewTrain$class
head(dfSparse.train$class,10)

#Pembuatan Model
sentimenSVM=svm(class~.,data=dfSparse.train,  kernel = "radial", cost = 50, gamma = 0.009)
sentimenSVM

#Melakukan prediksi
predictSVM_train=predict(sentimenSVM,newdata=dfSparse.train)

#Menghitung Confusion Matrix
cmat_sentimenSVM=table(dfSparse.train$class,predictSVM_train)
cmat_sentimenSVM

training_data = cbind(Actual=df,Predicted_class=predictSVM_train)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/Output")
write.csv(training_data, file = paste("Sample - Output SVM.csv"))




#Data Testing
setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/preprocessed")

list_tabel = list("preprocessed_2011.csv",
                  "preprocessed_2013.csv",
                  "preprocessed_2014.csv",
                  "preprocessed_2015.csv",
                  "preprocessed_2016.csv",
                  "preprocessed_2017.csv",
                  "preprocessed_2018.csv",
                  "preprocessed_2019.csv",
                  "preprocessed_2020.csv",
                  "preprocessed_2021.csv",
                  "preprocessed_2012.csv")

for (i in list_tabel){
  
  setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/preprocessed/")
  getwd()
  df_test_1 =  read.csv(i, stringsAsFactors = FALSE)
  colnames(df_test_1)
  nrow(df_test_1)
  
  setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
  data_sektor_1 = read.csv("Segmentasi_Kode_List.csv", stringsAsFactors = FALSE)
  
  data_sektor = data_sektor_1[,c("kode","Sektor")]
  nrow(data_sektor)
  
  df_test_join = merge(x=df_test_1, y = data_sektor,by = "kode", all.x = TRUE)
  df_test = df_test_join %>% filter(Sektor != "Technology")
  nrow(df_test)
  
  #Buat Corpus
  corpusTest <- Corpus(VectorSource(df_test$tweet))
  inspect(corpusTest[1:3])
  dtmTest <- DocumentTermMatrix(corpusTest)
  inspect(dtmTest[40:50, 10:15])
  nrow(dtmTest)
  dtm.test <- dtmTest[1:nrow(dtmTest),]
  corpus.test <- corpusTest[1:nrow(dtmTest)]
  
  fivefreq <- findFreqTerms(dtm.train, 5)
  dtm.test.fivefreq <- DocumentTermMatrix(corpus.test, control=list(dictionary = 
                                                                      fivefreq))
  
  dfSparse.test=as.data.frame(as.matrix(dtm.test.fivefreq))
  colnames(dfSparse.test)=make.names(colnames(dfSparse.test),unique = TRUE)
  
  dfSparse.test$class=""
  #Melakukan prediksi
  
  predictSVM_test=predict(sentimenSVM,newdata=dfSparse.test)
  
  testing_data = cbind(Actual=df_test,Predicted_class=predictSVM_test)
  
  setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/Output")
  write.csv(testing_data, file = paste(i,"- Output SVM.csv"))
  
  print("Done")
} 




#======================================================#
#Membaca Data Seluruhnya Hasil Predict
#======================================================#
library(dplyr)
setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Dengan Saham Teknologi/Hasil")
setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/Output")
getwd()
gc()

# menghitung sentiment & jumlah tweet
is_first <- TRUE
for (x in 2011:2021) {
  print(x)
  filename <- sprintf('preprocessed_%s.csv - Output SVM.csv', x)
  df_temp <- read.csv(filename)
  if (is_first == TRUE){
    df_all <- df_temp
    is_first <- FALSE
  } else {
    df_all <- union_all(df_all, df_temp)
  }
}
nrow(df_all)
colnames(df_all)

#Dengan Teknologi : 125126
#Tanpa Teknologi : 121945

colnames(df_all)

#Menyimpan Data pada CSV
write.csv(df_all, file = "Data Gabungan Analisis.csv")


#Jumlah Perusahaan

length(unique(df_all$Actual.kode))
#Dengan Teknologi : 382
#Tanpa Teknologi : 360


#Data 2012
setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/preprocessed/")
getwd()
df_test_1 =  read.csv("preprocessed_2012.csv", stringsAsFactors = FALSE)
colnames(df_test_1)
nrow(df_test_1)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
data_sektor_1 = read.csv("Segmentasi_Kode_List.csv", stringsAsFactors = FALSE)

data_sektor = data_sektor_1[,c("kode","Sektor")]
nrow(data_sektor)

df_test_join = merge(x=df_test_1, y = data_sektor,by = "kode", all.x = TRUE)
df_test = df_test_join %>% filter(Sektor != "Technology")
nrow(df_test)

#Buat Corpus
corpusTest <- Corpus(VectorSource(df_test$tweet))
inspect(corpusTest[1:3])
dtmTest <- DocumentTermMatrix(corpusTest)
inspect(dtmTest[40:50, 10:15])
nrow(dtmTest)
dtm.test <- dtmTest[1:nrow(dtmTest),]
corpus.test <- corpusTest[1:nrow(dtmTest)]

fivefreq <- findFreqTerms(dtm.train, 5)
dtm.test.fivefreq <- DocumentTermMatrix(corpus.test, control=list(dictionary = 
                                                                    fivefreq))

dfSparse.test=as.data.frame(as.matrix(dtm.test.fivefreq))
colnames(dfSparse.test)=make.names(colnames(dfSparse.test),unique = TRUE)

dfSparse.test$class=""
#Melakukan prediksi

predictSVM_test=predict(sentimenSVM,newdata=dfSparse.test)

testing_data = cbind(Actual=df_test,Predicted_class=predictSVM_test)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi/Output")
write.csv(testing_data, file = paste("preprocessed_2012.csv - Output SVM.csv"))

print("Done")










#===========================================#
#                   80 : 20                 #
#===========================================#
library(dplyr)
library(tm)
library(naivebayes)
library(rminer)
library(wordcloud)
library(e1071)
require(dplyr)

setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
df<- read.csv("labeling_result.csv", stringsAsFactors = FALSE)


#Fase 1
df = df[df$fase == 'fase 2',]

glimpse(df)
colnames(df)
colnames(df)[1] = 'sentiment'
nrow(df)


setwd("D:/Kerjaan/Analisa Data Client/Hasil Final/Tanpa Saham Teknologi")
data_sektor_1 = read.csv("Segmentasi_Kode_List.csv", stringsAsFactors = FALSE)

data_sektor = data_sektor_1[,c("kode","Sektor")]
nrow(data_sektor)

df_test_join = merge(x=df, y = data_sektor,by = "kode", all.x = TRUE)
df_test = df_test_join %>% filter(Sektor != "Technology")
nrow(df_test)

df = df_test
nrow(df)
head(df)
unique(df$Sektor)


#Data Training
df$class=as.factor(df$sentiment)
sapply(df, class)

reviewTrain = df
nrow(reviewTrain)

#Membuat Vektor dari msg
corpus.train <- Corpus(VectorSource(reviewTrain$tweet))
inspect(corpus.train[1:3])

#Clean Review
corpus.train=tm_map(corpus.train,tolower)
inspect(corpus.train[1:5])
corpus.train=tm_map(corpus.train,removePunctuation)
inspect(corpus.train[1:5])
corpus.train=tm_map(corpus.train,removeNumbers)
inspect(corpus.train[1:5])
#Menghapus white space
corpus.train=tm_map(corpus.train,stripWhitespace)
inspect(corpus.train[1:5])


#Membuat Dokumen Term matrix dari corpus
dtm.train <- DocumentTermMatrix(corpus.train)
inspect(dtm.train[40:50, 10:15])
nrow(dtm.train)

#
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
head(fivefreq)
dtm.train.fivefreq <- DocumentTermMatrix(corpus.train, control=list(dictionary = 
                                                                      fivefreq))
inspect(dtm.train.fivefreq[40:50, 10:15])


#Klasifikasi dengan SVM
library(e1071)
library(caTools)
library(tm)
library(SnowballC)
library(randomForest)

#Merubah data menjadi matrix
dfSparse.train=as.data.frame(as.matrix(dtm.train.fivefreq))
colnames(dfSparse.train)=make.names(colnames(dfSparse.train),unique = TRUE)
head(dfSparse.train,10)

#Mengambil label class
dfSparse.train$class=reviewTrain$class
head(dfSparse.train$class,10)

#Pembuatan Model
sentimenSVM=svm(class~.,data=dfSparse.train,  kernel = "radial", cost = 50, gamma = 0.009)
sentimenSVM

#Melakukan prediksi
predictSVM_train=predict(sentimenSVM,newdata=dfSparse.train)

#Menghitung Confusion Matrix
cmat_sentimenSVM=table(dfSparse.train$class,predictSVM_train)
cmat_sentimenSVM
