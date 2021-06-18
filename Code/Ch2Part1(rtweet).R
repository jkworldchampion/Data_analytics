
data = read.csv("Inner_Datasets_in_R.csv")
rm(list = ls())

############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and extract sample tweets
############################################################################

# load the package
# install.packages("rtweet")
library(rtweet)
library(dplyr)

source(file = "Code/TwitterAPIKey(temp).R", echo = FALSE)

# set twitter user
twitterUser <- lookup_users("john") #can be multiple users
class(twitterUser)
str(twitterUser)

# extract a few sample tweets from this user's timeline
tweets <- get_timeline(twitterUser$user_id, n = 10)
tweets %>% select(c("created_at", "is_retweet", "text"))
tweets <- get_timeline(twitterUser$user_id, n = 3200)  #around one minute
#The Search API is not complete index of all Tweets, but instead an index of recent Tweets. 
#At the moment that index includes between 6-9 days of Tweets.
#This method can only return up to 3,200 of a user's most recent Tweets. Native retweets of other statuses by the user is included in this total, regardless of whether include_rts is set to false when requesting this resource.

# display main body(text) of tweet
tweets$text[1]

# display favorite count
tweets$favorite_count[1]

# display retweet count
tweets$retweet_count[1]

names(tweets)

# check current rate limit (Check /statuses/user_timeline)
rateLimit <- rate_limit()
rateLimit[which(rateLimit$limit > rateLimit$remaining), ]


