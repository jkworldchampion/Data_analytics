############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and build a follower graph
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

library(rtweet)
library(data.table)  #for rbindlist
library(igraph)
library(RColorBrewer)
library(dplyr)

source(file = "TwitterAPIKey.R", echo = FALSE)


############################################################################
#       Utility Functions
############################################################################

# get follower names
# get_follower_list <- function(userName){
#   # get user data
#   twitterUser <- getUser(userName)
#   twitterUserFollowerIDs<-twitterUser$getFollowers(retryOnRateLimit=1)
#   
#   # extract the list of followers
#   return (sapply(twitterUserFollowerIDs,screenName))
# }

# append rows to dataframe
append_to_df<-function(dt, elems)
{ 
  return(rbindlist(list(dt,  elems),use.names = TRUE))
}

############################################################################
#             Follower Graph Analysis
############################################################################

# connect to twitter app
# twitter_token <- create_token(app = APP_NAME, consumer_key = API_KEY, consumer_secret = API_KEY_SECRET,
#                               access_token = ACCESS_TOKEN, access_secret = ACCESS_TOKEN_SECRET)
#Access token and secret should be provided due to API change in July 2018.

# Begin with a certain username
#coreUserName <- "jack"  #more than 4M followers... too big for an exercise
coreUserName <- "Ajou_University"

twitterUser <- lookup_users(coreUserName)
names(twitterUser)
twitterUser$screen_name

# Extract Followers for the core user
twitterUser_follower_IDs <- get_followers(twitterUser$user_id, retryonratelimit = 10)
str(twitterUser_follower_IDs)
head(twitterUser_follower_IDs)
twitterUser_followers_df <- lookup_users(twitterUser_follower_IDs$user_id)
head(twitterUser_followers_df)
names(twitterUser_followers_df)

# filter dummy accounts (and reduce the number of followers for performance)
filtered_df <- filter(twitterUser_followers_df, 
                      followers_count < 100 &
                      followers_count > 50 &
                        #statuses_count > 10000 & #to reduce number of followers
                        # statuses_count > 100 & 
                        # statuses_count < 5000 & #too many tweets from bots?
                        protected==FALSE) 
#statusesCount: number of tweets
#follwersCount: number of follwers (who follows this user)
#favoritesCount
#friendsCount: number of followees (whom this user follows)
#name: profile name that you can change
#protected: public or not
#verified: authentic (for celeb or organization)
#screenName: twitter ID (handle)
#location:
#language:
#id: integers (system-purpose key values?)
#listedCount: number of lists
#followRequestSent: ?
#profileImageUrl: profile image

filtered_follower_IDs <- filtered_df$screen_name
length(filtered_follower_IDs)

# prepare edge data frame (edges to coreUserName)
edge_df<-data.frame(from=filtered_follower_IDs,
                   to=rep(coreUserName, 
                          length(filtered_follower_IDs)), 
                   stringsAsFactors=FALSE)
head(edge_df)
tail(edge_df)
# edge_df <- append_to_df(edge_df,list(from=filtered_follower_IDs,
#                                      to=rep(coreUserName, 
#                                             length(filtered_follower_IDs))))
# above lines were used to add edges to coreUserName later

# Iterate and extract list of followers of followers
counter = 1
for(follower in filtered_follower_IDs){  #filtered_follower_IDs * 60x seconds -> stop it and use saved file!
  # fetch follower list for current user
  followerScreenNameList <- lookup_users(get_followers(follower)$user_id)$screen_name
  Sys.sleep(61)  #twitter API limit is 15 for 15 mins
  print(paste("Processing completed for:",
              follower,
              "(",counter,"/",
              length(filtered_follower_IDs),")"
  ))
  # append to edge list
  edge_df <- append_to_df(edge_df,list(from=followerScreenNameList,
                                       to=rep(follower, 
                                              length(followerScreenNameList))))
  counter <- counter + 1
}
save(edge_df, file = "edge_df20210323.Rda")
load("edge_df20210323.Rda")

# prepare network object

#net <- graph.data.frame(edge_df, directed=T)  #same with graph_from_data_frame()
net <- graph_from_data_frame(edge_df, directed=T)
class(net)
sort(table(edge_df$to), decreasing = TRUE)  #Ajou Univ's followers with many followers
sort(table(edge_df$from), decreasing = TRUE)  #following Ajou Univ's followers
sort(edge_df[to=="Ajou_University"]$from)
sort(edge_df[from=="Ajou_University"]$to)


# simplify network
net <- simplify(net, remove.multiple = F, remove.loops = T)
# temp -> temp is deleted later using remove.loops option

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg*0.05 + 3
V(net)[name == coreUserName]$size <- 15
V(net)[size >= 5]$name
V(net)[name == coreUserName]$size

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
op <- par(mar = c(0, 0, 0, 0))
plot(net, edge.arrow.size=0.1,
     #vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA),
     vertex.label = ifelse(V(net)$size >= 5, V(net)$name, NA),
     vertex.color = pal3)
par(op)

############################################################################
#             (reciprocal) Friends Among Followers (optional)
############################################################################


# Plot to highlight Followers with large number of followers
deg <- degree(net, mode="out")
V(net)$size <- deg*0.05+2
V(net)[size==max(V(net)$size)]  #the most ties

# Highlight the coreUser
V(net)[coreUserName]$size <- 15

# identify friend vertices (the vertices coreUserName is also following)
friendVertices <- ends(net, es=E(net)[from(coreUserName)])[,2]   #ends finds vertices at the ends of edges
ends(net, es=E(net)[from(coreUserName)])[,2]

# Generate edge color variable: (normal: grey80, friend: red)
ecol <- rep("grey80", ecount(net))
ecol[which (V(net)$name %in% friendVertices)] <- 'red'

# Generate edge width variable: (normal: 2, friend: 4)
ew <- rep(2, ecount(net))
ew[which (V(net)$name %in% friendVertices)] <- 4

# add core_user for vertex coloring
friendVertices <- append(friendVertices,coreUserName)

# Generate node color variable: (normal: grey80, friend & coreUser: gold)
vcol <- rep("grey80", vcount(net))
vcol[which (V(net)$name %in% friendVertices)] <- "gold"

# vertex label size
V(net)$label.cex <- 1.2

plot(net, 
     vertex.color=vcol, 
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0, 
     vertex.label = ifelse(V(net)$name %in% friendVertices, V(net)$name, NA), 
     vertex.label.color="black",
     vertex.label.font=2,
     edge.curved=0.1
)
