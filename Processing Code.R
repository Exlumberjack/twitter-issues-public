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

# Functions 

#Function to remove links, RT, @names
clean_links = function(x) {
  y = unlist(strsplit(x, "\\s+"))
  paste(y[!grepl("@|\\.com|\\.org|http|RT|-", y)], collapse = " ") #Change these to filter different links
}

#remove trailing / leading white space, currently unused
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#find closest point
euc.dist <- function(x1, x2) sqrt(rowSums((x1 - x2) ^ 2))

#filter html and emoticons
filter.h = function(x) {
  x = sub("&amp;#039;", "'", x)
  x = sub("&amp;", "&", x)
  x = iconv(x, "latin1", "ASCII", sub = "")
}


#Downloading Tweets By Key Word and Filtering
library(streamR)
load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 10, 
             oauth = my_oauth)

# Filtering
tweets.df <- parseTweets("tweets.json", simplify = FALSE)

trimmed_tweets = select(tweets.df, text, time_zone, lang) #select the variables / values we want
geo_tweets = filter(trimmed_tweets, time_zone != "<NA>", lang == "en")

geo_tweets$text = filter.h(geo_tweets$text) #Filter links and html codes from text
text = unlist(lapply(geo_tweets$text, clean_links))
linkless_tweets = data.frame(text, geo_tweets[,-1])

i <- sapply(linkless_tweets, is.factor) #convert factor back to character for future splitting
linkless_tweets[i] <- lapply(linkless_tweets[i], as.character)

#Downloading Tweets by location, for Stat 479
library(zipcode)
library(streamR)
library(dplyr)
load("my_oauth.Rdata")
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 500, 
             oauth = my_oauth)

#filtering
tweets.zip <- parseTweets("tweetsUS.json")
located_tweets = tweets.zip %>% select(text, lat, lon, followers_count) %>% filter(lat != "NA", lon != "NA") #Condensed choose variables / filter by values
data(zipcode)
zipcodeplace = select(zipcode,zip, latitude, longitude)

located_tweets$text = filter.h(located_tweets$text)   #filter links and code from text
text = unlist(lapply(located_tweets$text, clean_links))
located_tweets = data.frame(text, located_tweets[,-1])

i <- sapply(located_tweets, is.factor) #convert factor back to character for future splitting
located_tweets[i] <- lapply(located_tweets[i], as.character)

zip = rep(0, length(located_tweets[,2]))  #Future list of zip codes
ziplocs = data.matrix(zipcodeplace[,2:3]) #List of zip code lat/lon
n = length(zipcodeplace[,1])  #setting rep later possibly for speed
numtweets = length(located_tweets[,2]) #setting loop length later possibly for speed

for(j in 1:numtweets) {
  distances = matrix(unlist(rep(located_tweets[j,2:3], n)), nrow = n, byrow = TRUE) #why do i need unlist?
  distances = euc.dist(ziplocs, distances)
  zip[j] = zipcodeplace[which.min(distances), 1]   # 1 for zip code column
}

zipped_tweets = data.frame(located_tweets, zip)
