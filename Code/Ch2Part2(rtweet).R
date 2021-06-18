############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and perform trend analysis using
#                  different visualizations
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

# install.packages("tm")
library(tm)
library(ggplot2)
library(rtweet)
# install.packages("wordcloud")
library(wordcloud)

source(file = "TwitterAPIKey.R", echo = FALSE)

############################################################################
#       Utility Functions
############################################################################

# optional
# library(httr)
# Set proxy options
# set_config( use_proxy(url  = "http://proxy-chain.intel.com", port = 911));

# plot by source
# encode tweet source as iPhone, iPad, Android or Web
# gsub (global substitute): Replace 1st arg with 2nd arg in 3rd arg string
tweets$source
encodeSource <- function(x) {
  if(grepl("Twitter for iPhone", x)){
    "iphone"
  }else if(grepl("Twitter for iPad", x)){
    "ipad"
  }else if(grepl("Twitter for Android", x)){
    "android"
  }else if(grepl("Twitter Web App", x)){
    "Web"
  }else if(grepl("Hootsuite Inc.", x)){
    "hootsuite"
  }else if(grepl("TweetDeck", x)){
    "tweetdeck"
  }else {
    "others"
  }
}


############################################################################
#             Trend Analysis
############################################################################

# connect to twitter app
# twitter_token <- create_token(app = APP_NAME, consumer_key = API_KEY, consumer_secret = API_KEY_SECRET,
#                               access_token = ACCESS_TOKEN, access_secret = ACCESS_TOKEN_SECRET)
#Access token and secret should be provided due to API change in July 2018.

# extract tweets based on a search term
# searchTerm <- "#BTS"
# searchTerm <- enc2utf8("#방탄소년단")
# searchTerm <- enc2utf8("トゥワイス")
# searchTerm <- "#earthquake"
# searchTerm <- "#COVID19 PENCE"
# searchTerm <- "#COVID19 OR PENCE"
searchTerm <- "#SaveMyanmar"
trendingTweets = search_tweets(searchTerm, n=18000, lang = "en")  #around 10 seconds?
save(trendingTweets, file = "SaveMyanmar.Rda")
load("trendingTweets20210309.Rda")
load("SaveMyanmar.Rda")
class(trendingTweets)
head(trendingTweets)
dim(trendingTweets)

# check current rate limit
rateLimit <- rate_limit()
rateLimit[rateLimit$limit > rateLimit$remaining, ]

# perform a quick cleanup/transformation
View(trendingTweets)
names(trendingTweets)
head(trendingTweets$text)
#trendingTweets['text'] instead of trendingTweeets$text to avoid duplicate tweets
trendingTweets$text <- sapply(trendingTweets['text'], function(x) iconv(enc2utf8(x), sub="byte"))  #this works fine!!
head(trendingTweets$text)
head(trendingTweets$created_at)
class(trendingTweets$created_at)
save(trendingTweets, file = "trendingTweets20210309.Rda")

# see how many missing values are there on a per column basis
sapply(trendingTweets, function(x) sum(is.na(x)))
# user_id: ID of this status' user
# status_id: ID of this status
# created_at: creation timestamp
# screen_name: Screen name of the user who posted this status
# text: The text of the status
# source: Where it was posted from(iPhone, ...)
# reply_to_sreen_name: Screen name of the user this is in reply to
# reply_to_user_id: ID of the user this was in reply to
# is_retweet: TRUE if this status is retweet of the original tweet)
# retweet_count: The number of times this status has been retweeted (This belongs to the original tweet)
# favorite_count: The number of times this status has been favorited
# longitude
# latitude


############################################################################

# plot on tweets by time
ggplot(data = trendingTweets, aes(x = created_at)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


############################################################################

# plot tweets by source system (android, iphone, web, etc)
trendingTweets$tweetSource = sapply(trendingTweets$source, encodeSource)
unique(trendingTweets$source)
table(trendingTweets$tweetSource)

ggplot(trendingTweets[trendingTweets$tweetSource != 'others',], aes(tweetSource)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Source") 



############################################################################

# accounts which tweet about Covid19

namesCorpus <- Corpus(VectorSource(trendingTweets$screen_name))  #using ScreenName
class(trendingTweets$screen_name)
class(VectorSource(trendingTweets$screen_name))
str(namesCorpus)
class(namesCorpus)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
pal[5:9]
pal

set.seed(42)
par(mar = c(0,0,0,0), mfrow = c(1,1))
wordcloud(words = namesCorpus, scale=c(2,0.5), min.freq=5, max.words=100, 
          random.order=F, rot.per=0.3, use.r.layout=TRUE, colors=pal)
