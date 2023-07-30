library(dplyr)
library(tm)
library(naivebayes)
library(rminer)
library(wordcloud)
library(e1071)

df <- read.csv('df_text_only_20230226.csv')
tweets <- df_union$renderedContent %>% 
  as.character()
head(tweets)