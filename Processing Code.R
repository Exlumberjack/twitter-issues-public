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
  paste(y[!grepl("@|\\.com|\\.org|http|RT|-", y)], collapse = " ") #Change these to remove different links
}

#remove trailing / leading white space, currently unused
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#find closest point, now unused
euc.dist <- function(x1, x2) sqrt(rowSums((x1 - x2) ^ 2))

#filter html code and emoticons
filter.h = function(x) {
  x = sub("&amp;#039;", "'", x)
  x = sub("&amp;", "&", x)
  x = iconv(x, "latin1", "ASCII", sub = "")
}

# Remove extra information from dates and sort them
clean_dates = function(x) {
  x = paste(x[c(2,3,6,4)], collapse = ".")
}


#Downloading Tweets By Key Word and Filtering
library(streamR)
load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 10, 
             oauth = my_oauth)

# Filtering
library(streamR)
tweets.df <- parseTweets("tweets.json", simplify = FALSE)

trimmed_tweets = select(tweets.df, text, created_at, time_zone, lang) #select the variables / values we want
geo_tweets = filter(trimmed_tweets, time_zone != "<NA>", lang == "en")

#Filter links and html codes from text
geo_tweets$text = filter.h(geo_tweets$text) 
text = unlist(lapply(geo_tweets$text, clean_links))
linkless_tweets = data.frame(text, geo_tweets[,-1])

#convert factor back to character for future splitting
linkless_tweets$text <- unlist(lapply(linkless_tweets$text, as.character))

#Split one tweet into characters
#strsplit(tweet_text, " ")

#Split the tweets into characters
linkless_tweets$text = strsplit(linkless_tweets$text, " ")

#Converting Time stamps into R standard
word_dates = strsplit(linkless_tweets$created_at, " ")
word_dates = sapply(word_dates, clean_dates)
word_dates = gsub(":", ".", word_dates)
num_dates = strptime(word_dates, "%B.%d.%Y.%H.%M.%OS", tz = "GMT")
linkless_tweets$created_at = num_dates



#Downloading Tweets by location
library(streamR)
load("my_oauth.Rdata")
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 500, 
             oauth = my_oauth)


#Converting tweets with lon/lat into tweets with zip codes
library(sp)
library(streamR)
library(dplyr)
tweets.zip <- parseTweets("tweetsUS.json")
located_tweets = tweets.zip %>% select(text, lon, lat, created_at) %>% filter(lat != "NA", lon != "NA")

located_tweets$text = filter.h(located_tweets$text)   #filter links and code from text
text = unlist(lapply(located_tweets$text, clean_links))
located_tweets = data.frame(text, located_tweets[,-1])

i <- sapply(located_tweets, is.factor) #convert factor back to character for future splitting
located_tweets[i] <- lapply(located_tweets[i], as.character)

#ShapeFile Assignment: More accurate and now done! 

#Shapefile from https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html All files in the folder are the shapefile
#Read the polygons for the zip code areas
areas = readShapeSpatial("cb_2013_us_zcta510_500k.shp")

#Convert the lon/lat points to coords
points = located_tweets[,c(2:3)]
points = SpatialPoints(points)

#Overlay the points into the polygons, mapping the coordinates to zip codes
zipdata = over(points, areas)

#Filter the zip codes from the data frame, attach them to the tweets, and remove null zip codes
zip = zipdata$ZCTA5CE10
zipped_tweets = data.frame(located_tweets, zip)
zipped_tweets = filter(zipped_tweets, zip != "NA")
zipped_tweets$zip = as.numeric(levels(zipped_tweets$zip))[zipped_tweets$zip]

#Filter Zip codes to codes only in Wisconsin

library(zipcode) #Unneeded, simply a database of zipcode / city / state assignments

wisconsin_tweets = filter(zipped_tweets, zip >= 52820 & zip <= 54990)



#Map zip codes by distance to centroid: Unused as inaccurate

#data(zipcode)
#zipcodeplace = select(zipcode,zip, latitude, longitude)
#zip = rep(0, length(located_tweets[,2]))  #Future list of zip codes
#ziplocs = data.matrix(zipcodeplace[,2:3]) #List of zip code lon/lat
#n = length(zipcodeplace[,1])  #setting rep later possibly for speed
#numtweets = length(located_tweets[,2]) #setting loop length later possibly for speed

#for(j in 1:numtweets) {
#  distances = matrix(unlist(rep(located_tweets[j,2:3], n)), nrow = n, byrow = TRUE) #why do i need unlist?
#  distances = euc.dist(ziplocs, distances)
#  zip[j] = zipcodeplace[which.min(distances), 1]   # 1 for zip code column
#}

#zipped_tweets = data.frame(located_tweets, zip)



