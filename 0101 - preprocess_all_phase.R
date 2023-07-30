install.packages('textclean')
install.packages("devtools")
library(devtools)
install_github("nurandi/katadasaR", force = TRUE)
# install.packages("RColorBrewer")
# install.packages("wordcloud")

library(textclean)
library(katadasaR)
library(tokenizers)
#library(wordcloud)
library(dplyr)

# Gunakan regex ini untuk menghilangkan URL pada text
regexp_ajaib <- "\\b(?:(?:\\S+)?(?:https?|www)\\S+|(?:\\S+)?bit\\.ly\\S+|(?:\\S+)?dlvr\\.it\\S+|(?:\\S+)?\\S+\\.(?:com|co)\\S+|(?:\\S+)?goo\\.gl\\S+)\\b"
myStopwords <- read.csv("stopword_id.csv")

for (x in 2011:2022) {
  print(x)
  print(Sys.time())
  filename <- sprintf('data_text_only_excld7emiten_yearly/df_text_only_excld7emiten_%s.csv', x)
  outputfile <- sprintf('preprocessed_excld7emiten/preprocessed_%s.csv', x)
  
  df <- read.csv(filename)
  tweets <- df$renderedContent %>% 
    as.character()
  tweets <- gsub(regexp_ajaib, "", tweets, ignore.case = TRUE)
  tweets <- tweets %>% 
    replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
    replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement="")
  tweets <- gsub("\\$\\w+", "", tweets)
  tweets <- strip(tweets)
  stemming <- function(x){
    paste(lapply(x,katadasar),collapse = " ")}
  tweets <- lapply(tokenize_words(tweets[]), stemming)
  tweets <- tokenize_words(tweets)
  tweets <- as.character(tweets)
  tweets <- tokenize_words(tweets, stopwords = myStopwords)
  tweets <- lapply(tweets, paste, collapse = " ")
  tweets <- substr(tweets, 3, nchar(tweets))
  
  df$tweet <- tweets
  write.csv(df, outputfile, row.names=FALSE)
}
