#TWEETS

#Creating OAUTH

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "ObRDBlfaLjCcrKIRKCGHTmnFK"
consumerSecret <- "FIVRQsYAy9Xl3XVNCl05ig27OhPcC640NexwG6tk1BSrWA8FIe"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")

#Function to remove links, RT, @names
clean_links = function(x) {
  y = unlist(strsplit(x, "\\s+"))
  paste(y[!grepl("@|\\.com|\\.org|http|RT|-", y)], collapse = " ") # Change arguments of grepl to filter different targets
}

#remove trailing / leading white space, currently unused by may be useful
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Downloading Tweets By Key Word and Filtering
library(streamR)
load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 10, 
             oauth = my_oauth)

# Filtering
tweets.df <- parseTweets("tweets.json", simplify = FALSE)
trimmed_tweets = select(tweets.df, text, time_zone, lang)    #Different arguments to select for different variables in table
geo_tweets = filter(trimmed_tweets, time_zone != "<NA>", lang == "en") #Filter by multiple arguments combines by & operator
geo_tweets$text = sub("&amp;#039;", "'", geo_tweets$text) #Currently substitues code for apostrophe with an apostrophe, may do more later
text = unlist(lapply(geo_tweets$text, clean_links)) #Removes all links, RT
linkless_tweets = data.frame(text, geo_tweets[,-1])

#Downloading Tweets by location, for Stat 479
library(zipcode)
library(streamR)
load("my_oauth.Rdata")
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 300, 
             oauth = my_oauth)

#filtering
tweets.zip <- parseTweets("tweetsUS.json")
located_tweets = tweets.zip %>% select(text, lat, lon) %>% filter(lat != "NA", lon != "NA") #tweets.zip %>% select(stuff) == select(tweets.zip, stuff)
zipcodeplace = select(zipcode,zip, latitude, longitude)
zip = rep(0, length(located_tweets[,2]))  #Future list of zip codes
ziplocs = data.matrix(zipcodeplace[,2:3]) #List of zip code lat/lon
n = length(zipcodeplace[,1])  #setting rep later possibly for speed
numtweets = length(located_tweets[,2]) #setting loop length later possibly for speed
for(j in 1:numtweets) {
  distances = matrix(unlist(rep(located_tweets[j,2:3], n)), nrow = n, byrow = TRUE) #why do i need unlist?
  distances = euc.dist(ziplocs, distances)
  zip[j] = zipcodeplace[which.min(distances), 1]   # 1 for zip code column
}

#find distance between two matrices of points
euc.dist <- function(x1, x2) sqrt(rowSums((x1 - x2) ^ 2))
