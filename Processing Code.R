# Helper Functions 

#Function to remove links, RT, @names
clean_links = function(x) {
  y = unlist(strsplit(x, "\\s+"))
  paste(y[!grepl("@|\\.com|\\.org|http|RT|-", y)], collapse = " ") #Change these to remove different links
}

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

#Functions

#Variables must be selected before calling this
clean.tweets = function(tweets.df) {
library(dplyr)
geo_tweets = tweets.df

if(!is.null(tweets.df$time_zone)) {
  geo_tweets = filter(tweets.df, time_zone != "<NA>") 
}

#Filter links and html codes from text
geo_tweets$text = filter.h(geo_tweets$text) 
text = unlist(lapply(geo_tweets$text, clean_links))
geo_tweets$text = text
linkless_tweets = geo_tweets

#convert factor back to character for future splitting
linkless_tweets$text <- unlist(lapply(linkless_tweets$text, as.character))

#Split the tweets into characters
linkless_tweets$text = strsplit(linkless_tweets$text, " ")

#Converting Time stamps into R standard
if(!is.null(linkless_tweets$created_at)) {
word_dates = strsplit(linkless_tweets$created_at, " ")
word_dates = sapply(word_dates, clean_dates)
word_dates = gsub(":", ".", word_dates)
num_dates = as.POSIXct(strptime(word_dates, "%B.%d.%Y.%H.%M.%OS", tz = "GMT"))
linkless_tweets$created_at = num_dates
}
return(linkless_tweets)
}


#Converting tweets with lon/lat into tweets with zip codes, should clean before doing this
locate.tweets = function(located_tweets) {
library(sp)
library(dplyr)
library(maptools)
if(is.null(located_tweets$lat) || is.null(located_tweets$lon)) {
  print("Error, can't assign zip codes to tweets with no geo tags")
  return(NULL)
}

#ShapeFile Assignment: More accurate and now done! 

#Shapefile from https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html Every file in the folder is part of the shapefile
#Read the polygons for the zip code areas
areas = readShapeSpatial("cb_2013_us_zcta510_500k.shp")

#Convert the lon/lat points to coords
temp = filter(located_tweets, lon != "NA", lat != "NA")
if(nrow(temp) == 0) {
  print("Error, all tweets non-geolocated")
  return(located_tweets)
}
located_tweets = temp

points = data.frame(located_tweets$lon, located_tweets$lat)
points = SpatialPoints(points)

#Overlay the points into the polygons, mapping the coordinates to zip codes
zipdata = over(points, areas)

#Filter the zip codes from the data frame, attach them to the tweets, and remove null zip codes
zip = zipdata$ZCTA5CE10
zipped_tweets = data.frame(located_tweets, zip)
zipped_tweets = filter(zipped_tweets, zip != "NA")
return(zipped_tweets)
}

#Filter Zip codes to codes only in a region
filter.zips = function(zipped_tweets, low, high) {
zipped_tweets$zip = as.numeric(zipped_tweets$zip)
wisconsin_tweets = filter(zipped_tweets, zip >= low & zip <= high)
return(wisconsin_tweets)
}

process.tweets = function(tweets.df, vars = c("text", "created_at", "lang", "time_zone", "lat", "lon"), lan = "en") {
  
  tweets = subset(tweets.df, select = vars)
  
if("lang" %in% vars) {
  tweets = filter(tweets, lang %in% lan)
}

  tweets = clean.tweets(tweets)

if("lat" %in% vars & "lon" %in% vars) {
  tweets = locate.tweets(tweets)
}

  return(tweets)
}

save(clean_dates, clean_links, clean.tweets, filter.h, filter.zips, locate.tweets, process.tweets, file = "twitterFunctions.Rdata")
