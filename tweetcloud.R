library(lda)
library(wordcloud)
library(dplyr)

english.stoplist <- scan("http://bridge.library.wisc.edu/jockersStopList.txt", 
                         what = "character")
aa <- strsplit(english.stoplist, ",")
english.stoplist <- sapply(X = aa, FUN = function(x) {
  x[[1]]
})

#process.files("~/Downloads/fred-twitter-downloaded-files-oct27tonov4", "~/Documents/R/Tweets/Tweet_Downloader/Fred_Tweets",
#              stoplist = english.stoplist)

process.files("~/Documents/R/Tweets/Tweet_Downloader/Israel_Tweets", "~/Documents/R/Tweets/Tweet_Downloader/Edited_Tweets",
                             stoplist = english.stoplist, vars = c("text","lang","time_zone"))


#test = make.tweet.df("~/Documents/R/Tweets/Tweet_Downloader/Fred_Tweets")
test = make.tweet.df("~/Documents/R/Tweets/Tweet_Downloader/Edited_Tweets")
test = filter(test, lang == "en")
test = select(test, text)
test = test$text[!duplicated(test$text)]
test = as.character(test)
tweets = paste(test, collapse = " ")
tweet.words = strsplit(tweets, "\\W")
tweet.vector = unlist(tweet.words)

not.blanks = which(tweet.vector != "")
tweet.vector = tweet.vector[not.blanks]

chunk.size = 100
num.chunks = length(tweet.vector)/chunk.size

x = seq_along(tweet.vector)

chunks = split(tweet.vector, ceiling(x/chunk.size))
chunks.as.strings = lapply(chunks, paste, collapse = " ")
chunk.vector = unlist(chunks.as.strings)

doclines = lexicalize(chunk.vector)
set.seed = (123456)
K = 5
num.iterations = 250
result <- lda.collapsed.gibbs.sampler(doclines$documents, K, doclines$vocab, 
                                      num.iterations, 0.1, 0.1, compute.log.likelihood = TRUE)

library(wordcloud)
i = 1
cloud.data = sort(result$topics[i,], decreasing = TRUE)[1:50]
wordcloud(names(cloud.data), freq = cloud.data, scale = c(4, 0.1), min.freq = 1, rot.per = 0, random.order = FALSE)

