rm(list = ls())
library(rtweet)
library(dplyr)
source(file = "Code/TwitterAPIKey(temp).R", echo = FALSE)

search_term = "camera"

result = search_tweets(search_term, n = 50, type="recent", include_rts=F)
result$country[!is.na(result$country)]
