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

df <- read.csv('df_text_only_20230226.csv') 

df_fase1 <- df %>%
  filter(fase == 'fase 1')
head(df)

# ambil hanya kolom tweet
tweets <- df_fase1$renderedContent %>% 
  as.character()
head(tweets)

# remove html & URL
#tweets <- tweets %>% 
#  replace_html() %>% # replace html with blank 
#  replace_url()   # replace URLs with blank
#head(tweets)
regexp_ajaib <- "\\b(?:(?:\\S+)?(?:https?|www)\\S+|(?:\\S+)?bit\\.ly\\S+|(?:\\S+)?dlvr\\.it\\S+|(?:\\S+)?\\S+\\.(?:com|co)\\S+|(?:\\S+)?goo\\.gl\\S+)\\b"
tweets <- gsub(regexp_ajaib, "", tweets, ignore.case = TRUE)
head(tweets)

# remove mentions, hashtags, cashtags
tweets <- tweets %>% 
  replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # remove mentions
  replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement="")
tweets <- gsub("\\$\\w+", "", tweets)
head(tweets)

## import Indonesian lexicon
#spell.lex <- read.csv("colloquial-indonesian-lexicon.csv")
## replace internet slang
#tweets <- replace_internet_slang(tweets, slang = paste0("\\b",
#                                                        spell.lex$slang, "\\b"),
#                                 replacement = spell.lex$formal, ignore.case = TRUE)
#head(tweets)

# Test stripping
tweets <- strip(tweets)
head(tweets)

# stemming
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")}
tweets <- lapply(tokenize_words(tweets[]), stemming)

# Tokenizer
tweets <- tokenize_words(tweets)
#library(stopwords)
myStopwords <- read.csv("stopword_id.csv")
tweets <- as.character(tweets)
tweets <- tokenize_words(tweets, stopwords = myStopwords)
head(tweets, 3)

tweets <- lapply(tweets, paste, collapse = " ")
tweets <- substr(tweets, 3, nchar(tweets))
head(tweets)

## remove emoji
#tweets <- tweets %>% 
#  replace_emoji(.) %>% 
#  replace_html(.)
#head(tweets)

## Randomizer
#rand_df <- df[sample(nrow(df), size=3), ]

df_fase1$tweet <- tweets
head(df_fase1)
write.csv(df_fase1, "preprocessed_fase1.csv", row.names=FALSE)
