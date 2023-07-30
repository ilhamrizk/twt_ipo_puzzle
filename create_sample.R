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

df <- read.csv('data_text_only_excld7emiten.csv')

# mengambil sample fase 1
df_fase1 <- df %>%
    filter(fase == 'fase 1')
nrows <- nrow(df_fase1)
nsample <- ceiling(nrows/100) #160
df_fase1_sample <- df_fase1[sample(nrows, size=nsample), ]
nrow(df_fase1_sample)

# mengambil sample fase 2
df_fase2 <- df %>%
  filter(fase == 'fase 2')
nrows <- nrow(df_fase2)
nsample <- ceiling(nrows/100) #1142
df_fase2_sample <- df_fase2[sample(nrows, size=nsample), ]
head(df_fase2_sample)
nrow(df_fase2_sample)
head(df$renderedContent)

# union fase 1 & 2
df_union <- union(df_fase1_sample,df_fase2_sample)
nrow(df_union)
colnames(df_union)
head(df_union)

# save dan rewrite sample
write.csv(df_union, "df_union_new.csv", row.names=FALSE)
df_union <- read.csv('df_union_new.csv')
head(df_union)

# ambil hanya kolom tweet
tweets <- df_union$renderedContent %>% 
  as.character()
head(tweets, 100)

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

df_union$tweet <- tweets
head(df_union)
write.csv(df_union, "resampling_exlcd7emiten.csv", row.names=FALSE)
