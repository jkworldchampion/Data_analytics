############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and perform sentiment analysis 
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

search()
library(rtweet)
library(tm)
library(ggplot2)
library(stringr)
# install.packages("syuzhet")
library(syuzhet)
library(wordcloud)
library(RColorBrewer)

source(file = "TwitterAPIKey.R", echo = FALSE)


############################################################################
#       Utility Functions
############################################################################

# optional
# library(httr)
# Set proxy options
#set_config( use_proxy( url  = "http://proxy-chain.intel.com", port = 911));
# set_config( use_proxy( url  = "http://189.50.4.170", port = 8080));


#extract timeline tweets
extractTimelineTweets <- function(username,tweetCount){
  # timeline tweets
  twitterUser <- lookup_users(username)[[1]] #get user id
  tweets <- get_timeline(twitterUser, n=tweetCount)
  # tweets$text <- sapply(tweets['text'], function(x) iconv(enc2utf8(x), sub="byte"))
  
  return(tweets)
}


encodeSentiment <- function(x) {
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


############################################################################
#             Sentiment Analysis
############################################################################

# connect to twitter app
# twitter_token <- create_token(app = APP_NAME, consumer_key = API_KEY, consumer_secret = API_KEY_SECRET,
                              # access_token = ACCESS_TOKEN, access_secret = ACCESS_TOKEN_SECRET)
#Access token and secret should be provided due to API change in July 2018.

# tweetsDF <- extractTimelineTweets("POTUS", 3200)
tweetsDF <- extractTimelineTweets("BarackObama", 3200)
dim(tweetsDF)
range(tweetsDF$created_at)
View(tweetsDF)
tweetsDF$text <- sapply(tweetsDF['text'], function(x) iconv(enc2utf8(x), sub="byte"))
head(tweetsDF$text, 30)
i <- 9
tweetsDF$text[i]
nohandles <- str_replace_all(tweetsDF$text, "@\\w+", "")
nohandles[i]
wordCorpus <- Corpus(VectorSource(nohandles))
class(wordCorpus)
wordCorpus[[i]]$content
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus[[i]]$content
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus[[i]]$content
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus[[i]]$content
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))  #manual assignment
wordCorpus[[i]]$content
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus[[i]]$content
str(wordCorpus)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
par(mar = c(0,0,0,0), mfrow = c(1, 1))
wordcloud(words = wordCorpus, scale=c(3,0.5), max.words=300, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

## Sentiment analysis using syuzhet
# tweetSentiments <- get_sentiment(tweetsDF$text, method = "syuzhet")
tweetSentiments <- get_sentiment(content(wordCorpus), method = "syuzhet")
get_sentiment_dictionary(dictionary = 'syuzhet')
tweets <- cbind(tweetsDF, tweetSentiments)
names(tweetsDF)
names(tweets)
tweets$sentiment <- sapply(tweets$tweetSentiments,encodeSentiment)
head(tweets, n = 1)

#plot by sentiment score
qplot(tweets$tweetSentiments) + theme(legend.position="none")+
  xlab("Sentiment Score") +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment Score") 

#plot by sentiment category
ggplot(tweets, aes(sentiment)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment") 


# NRC Sample (various emotions such as anger, fear, joy, ...)
library(stringi)
# tweetsDF$text <- stri_trans_general(tweetsDF$text, "latin-ascii") #to remove non-English text
tweetsDF$text <- stri_trans_general(content(wordCorpus), "latin-ascii") #to remove non-English text
tweetSentiments <- get_nrc_sentiment(tweetsDF$text)
head(tweetSentiments)
head(tweetsDF$text)
tweets <- cbind(tweetsDF, tweetSentiments)
names(tweets)
tweets[1:6, c(5, 91:100)]

sentimentTotals <- data.frame(colSums(tweets[,c(91:100)]))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
sentimentTotals

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

