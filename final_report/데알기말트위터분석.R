#install.packages("remotes")
#remotes::install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))
library(syuzhet)
library(tidytext)
library(wordcloud)
library(tm)
library(stringr)
library(rtweet)
library(dplyr)
library(twitteR)
library(dplyr)
library(RColorBrewer)
library(KoNLP)
library(SnowballC)
library(ggplot2)
library(SentimentAnalysis)
library(tidyverse)
useSejongDic()

#source(file = "TwitterAPIKey.R", echo = FALSE)

#childbirth <- enc2utf8("childbirth")
#data <- search_tweets(childbirth, n=10000, lang="en")
#data <- unique(data)
#data <- data[,c(1,2,3,4,5,6,7,67)]
#write_csv(data, "childbirthdata.csv")

data <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/childbirthdata.csv")

text <- str_replace_all(data$text, "@\\w+", "")
text <- str_replace_all(data$text, "[[:punct:]]", "") 
wordCorpus <- Corpus(VectorSource(text))
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("https"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, removeNumbers)
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("childbirth"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("nhs", "realdonaldtrump", "tcoqzrnig"))
wordCorpus[[1]]$content

pal <- brewer.pal(8, "Dark2")
wordcloud(words=wordCorpus, scale=c(4,0.5), max.words = 300,
          random.order = F, rot.per=0.35, use.r.layout = F, min.freq = 50, colors=pal)


encodesentiment <- function(x) {
  if(x <= -1){
    "1) very negative"
  }else if(x > -1 & x < 0){
    "2) negative"
  }else if(x > 0 & x < 1){
    "4) positive"
  }else if(x >= 1){
    "5) very positive"
  }else {
    "3) neutral"
  }
}

tweetsentiment <- get_sentiment(data$text, method= "syuzhet")
tweets <- cbind(data, tweetsentiment)
tweets$Sentiments <- sapply(tweets$tweetsentiment, encodesentiment)

qplot(tweets$tweetsentiment) + theme(legend.position = "none")+
  xlab("sentiment score") + ylab("number of tweets") +
  ggtitle("childbirth tweets by sentiment score")

ggplot(tweets, aes(Sentiments)) +
  geom_bar(fill="orange") +
  theme(legend.position = "none", axis.title = element_blank())+
  ylab("number of tweets") +
  ggtitle("tweets about childbirth")


# =============================================

#parenting <- enc2utf8("parenting")
#data2 <- search_tweets(parenting, n=10000, lang="en")
#data2 <- unique(data2)
#data2 <- data2[,c(1,2,3,4,5,6,7,67)]
#write_csv(data2, "parentingdata.csv")

data2 <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/parentingdata.csv")

text2 <- str_replace_all(data2$text, "@\\w+", "")
text2 <- str_replace_all(data2$text, "[[:punct:]]", "")
wordCorpus2 <- Corpus(VectorSource(text2))
wordCorpus2 <- tm_map(wordCorpus2, content_transformer(tolower))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, stopwords("english"))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("https"))
wordCorpus2 <- tm_map(wordCorpus2, stripWhitespace)
wordCorpus2 <- tm_map(wordCorpus2, removePunctuation)
wordCorpus2 <- tm_map(wordCorpus2, removeNumbers)
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("parenting"))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("tcolukpux", "yoongi"))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("httpstcolukpux"))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("amp"))
wordCorpus2[[1]]$content


set.seed(1234)
wordcloud(words=wordCorpus2, scale=c(4,0.5), max.words = 200,
          random.order = F, rot.per=0.35, use.r.layout = F, min.freq = 10, colors=pal)


# 감정분석 

tweetsentiment2 <- get_sentiment(data2$text, method= "syuzhet")
tweets2 <- cbind(data2, tweetsentiment2)
tweets2$Sentiments2 <- sapply(tweets2$tweetsentiment2, encodesentiment)

qplot(tweets2$tweetsentiment2) + theme(legend.position = "none")+
  xlab("sentiment score") + ylab("number of tweets") +
  ggtitle("parenting tweets by sentiment score")

ggplot(tweets2, aes(Sentiments2)) +
  geom_bar(fill="aquamarine4") +
  theme(legend.position = "none", axis.title = element_blank())+
  ylab("number of tweets") +
  ggtitle("tweets about parenting")


# =====================================================


#childbirth_k <- enc2utf8("출산")
#Kdata <- search_tweets(childbirth_k, n=10000, lang="ko")
#Kdata <- unique(Kdata)
#Kdata <- Kdata[,1:13]
#write_csv(Kdata, "출산.csv")
Kdata <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/%E1%84%8E%E1%85%AE%E1%86%AF%E1%84%89%E1%85%A1%E1%86%AB.csv")

Kdata_text <- Kdata$text
Kdata_text <- sapply(Kdata_text, extractNoun, USE.NAMES = F)
Kdata_word <- unlist(Kdata_text)
Kdata_word <- Filter(function(x){nchar(x)>=2},Kdata_word)

Kdata_word <- gsub("\n", "", Kdata_word)
Kdata_word <- gsub("\r", "", Kdata_word)
Kdata_word <- gsub("https://", "", Kdata_word)
Kdata_word <- gsub("@", "", Kdata_word)
Kdata_word <- gsub("[[:punct:]]", "", Kdata_word)
Kdata_word <- str_replace_all(Kdata_word,"[A-Za-z0-9]","")
Kdata_word <- gsub("[ㄱ-ㅎ]", "", Kdata_word)
Kdata_word <- gsub("[ㅜ|ㅠ]", "", Kdata_word)
Kdata_word <- gsub("로", "", Kdata_word)
Kdata_word <- gsub("합사시켜", "", Kdata_word)
Kdata_word <- gsub("해서", "", Kdata_word)
Kdata_word <- gsub("출산", "", Kdata_word)
Kdata_word <- gsub("쿠오모가", "", Kdata_word)
Kdata_word <- gsub("까지", "", Kdata_word)
Kdata_word <- gsub("들이", "", Kdata_word)
Kdata_word <- gsub("그래서", "", Kdata_word)
Kdata_word <- gsub("그걸", "", Kdata_word)

head(Kdata_word)

set.seed(1234)
wordcloud(words=Kdata_word, scale=c(5,0.5),
          min.freq = 30, random.order = F, max.words=200, family="AppleGothic", colors=pal)

# 감정분석 

senti_words_kr <- readr::read_delim("https://raw.githubusercontent.com/park1200656/KnuSentiLex/master/SentiWord_Dict.txt", delim='\t', col_names=c("term", "score"))
head(senti_words_kr)

x <- duplicated(senti_words_kr$term)
senti_words_kr2 <- senti_words_kr[!x, ]
senti_dic_kr <- SentimentDictionaryWeighted(words = senti_words_kr2$term, 
                                            scores = senti_words_kr2$score)
senti_dic_kr <- SentimentDictionary(senti_words_kr2$term[senti_words_kr2$score > 0], 
                                    senti_words_kr2$term[senti_words_kr2$score < 0])

res_sentiment <- analyzeSentiment(Kdata_word,
                                  language="korean",
                                  rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)
theme_set(theme_minimal(base_family = "AppleGothic"))

df <- data.frame(res_sentiment, Kdata_word)
df <- df[!(df$Kdata_word==""),]

df[is.na(df$KoreanSentiment),1] <- "neutral"
df[df$KoreanSentiment == 0,1] <- "neutral"
df[df$KoreanSentiment == 1,1] <- "positive"
df[df$KoreanSentiment == 0.125,1] <- "positive"
df[df$KoreanSentiment == 0.111111111111111,1] <- "positive"
df[df$KoreanSentiment == 0.0833333333333333,1] <- "positive"
df[df$KoreanSentiment < 0,1] <- "negative"

df_final <- df[!(df$KoreanSentiment=="neutral"),] #중립이 압도적으로 많아 빼고 시각화

ggplot(df_final, aes(x = KoreanSentiment)) + 
  geom_bar(stat = "count", width = 0.7, fill = "orange") + 
  theme_minimal() + ggtitle("tweets about childbirth in korea")

# ===================================================


#childcare_k <- enc2utf8("육아")
#Kdata2 <- search_tweets(childcare_k, n=10000, lang="ko")
#Kdata2 <- unique(Kdata2)
#Kdata2 <- Kdata2[,1:13]
#write_csv(Kdata2, "육아.csv")

Kdata2 <- read.csv("https://raw.githubusercontent.com/jkworldchampion/Data_analytics/main/final_report/data_set/%E1%84%8B%E1%85%B2%E1%86%A8%E1%84%8B%E1%85%A1.csv")

Kdata_text2 <- Kdata2$text

Kdata_text2 <- sapply(Kdata_text2, extractNoun, USE.NAMES = F)
Kdata_word2 <- unlist(Kdata_text2)
Kdata_word2 <- Filter(function(x){nchar(x)>=2},Kdata_word2)

Kdata_word2 <- gsub("\n", "", Kdata_word2)
Kdata_word2 <- gsub("\r", "", Kdata_word2)
Kdata_word2 <- gsub("https://", "", Kdata_word2)
Kdata_word2 <- gsub("@", "", Kdata_word2)
Kdata_word2 <- gsub("[[:punct:]]", "", Kdata_word2)
Kdata_word2 <- str_replace_all(Kdata_word2,"[A-Za-z0-9]","")
Kdata_word2 <- gsub("[ㄱ-ㅎ]", "", Kdata_word2)
Kdata_word2 <- gsub("[ㅜ|ㅠ]", "", Kdata_word2)
Kdata_word2 <- gsub("로", "", Kdata_word2)
Kdata_word2 <- gsub("해서", "", Kdata_word2)
Kdata_word2 <- gsub("육아", "", Kdata_word2)
Kdata_word2 <- gsub("키선배님이", "", Kdata_word2)
Kdata_word2 <- gsub("놀토가", "", Kdata_word2)
Kdata_word2 <- gsub("호시는", "", Kdata_word2)
Kdata_word2 <- gsub("트니트니", "", Kdata_word2)
Kdata_word2 <- gsub("세븐", "", Kdata_word2)
Kdata_word2 <- gsub("그거", "", Kdata_word2)
Kdata_word2 <- gsub("갖다대니까", "", Kdata_word2)
Kdata_word2 <- gsub("하게", "", Kdata_word2)

set.seed(1234)
wordcloud(words=Kdata_word2, scale=c(4,0.5),
          min.freq = 40, random.order = F, max.words=200, family="AppleGothic", colors=pal)


# 감정분석 
res_sentiment2 <- analyzeSentiment(Kdata_word2,
                                   language="korean",
                                   rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                   removeStopwords = F, stemming = F)

df2 <- data.frame(res_sentiment2, Kdata_word2)
df2 <- df2[!(df2$Kdata_word2==""),]

df2[is.na(df2$KoreanSentiment),1] <- "neutral"
df2[df2$KoreanSentiment == 0,1] <- "neutral"
df2[df2$KoreanSentiment == 1,1] <- "positive"
df2[df2$KoreanSentiment == 0.0555555555555556,1] <- "positive"
df2[df2$KoreanSentiment == 0.333333333333333,1] <- "positive"
df2[df2$KoreanSentiment == 0.142857142857143,1] <- "positive"
df2[df2$KoreanSentiment == 0.1,1] <- "positive"
df2[df2$KoreanSentiment == 0.0833333333333333,1] <- "positive"
df2[df2$KoreanSentiment == 0.0769230769230769,1] <- "positive"
df2[df2$KoreanSentiment == 0.0588235294117647,1] <- "positive"
df2[df2$KoreanSentiment == 0.05,1] <- "positive"
df2[df2$KoreanSentiment < 0,1] <- "negative"
df2[df2$KoreanSentiment == 0.0714285714285714,1] <- "negative"

df2_final <- df2[!(df2$KoreanSentiment=="neutral"),]

ggplot(df2_final, aes(x = KoreanSentiment)) + 
  geom_bar(stat = "count", width = 0.7, fill = "aquamarine4") + 
  theme_minimal() + ggtitle("tweets about parenting in korea")



