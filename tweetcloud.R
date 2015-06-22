library(lda)
library(wordcloud)
library(dplyr)
library(parseTweetFiles)

english.stoplist <- scan("http://bridge.library.wisc.edu/jockersStopList.txt", 
                         what = "character")
aa <- strsplit(english.stoplist, ",")
english.stoplist <- sapply(X = aa, FUN = function(x) {
  x[[1]]
})

process_files("~/Documents/R/Tweets/Tweet_Downloader/Robinson_Tweets", "~/Documents/R/Tweets/Tweet_Downloader/LDA_Tweets",
                             stoplist = english.stoplist, vars = c("text","lang","lat","lon"), loc = TRUE)

test = make_tweet_df("~/Documents/R/Tweets/Tweet_Downloader/LDA_Tweets")
save(test, file = "editedRobinsonTweets.Rdata")

load(file = "editedRobinsonTweets.Rdata")
library(dplyr)
library(tm)
test = filter(test, lang == "en")
test = filter(test, !is.na(state))
test = test$text
test = test[!duplicated(test)]
test = as.character(test)
test = test[which(test != "")]

#LDA vis
#Create 10 folds
set.seed(1234)
test = sample(test, length(test))
folds <- cut(seq(1,length(test)),breaks=10,labels=FALSE)
corp = Corpus(VectorSource(test))
dtm = DocumentTermMatrix(corp)

perplexities = matrix(, nrow = 10, ncol = 29)
for(i in 1:10) {
  testIndices = which(folds==i,arr.ind=TRUE)
  trainData = dtm[-testIndices,]
  testData = dtm[testIndices,]
  
  trainData = removeSparseTerms(trainData, 0.99)
  rowTotals = apply(trainData, 1, sum)
  trainData = trainData[rowTotals > 0,]
  
  testData = testData[,Terms(testData) %in% Terms(trainData)]
  testData = removeSparseTerms(testData, 0.99)
  rowTotals = apply(testData, 1, sum)
  testData = testData[rowTotals > 0,]

  
  for(k in 2:30) {
    fitted = LDA(trainData, k = k, method = "Gibbs")
    perplexities[i,(k-1)] = perplexity(fitted, testData)
  }
}
perplexities = colMeans(perplexities)

index = seq_along(perplexities)
index = index + 1
plot(index, perplexities, xlab = "# of Topics", ylab = "Perplexity", main = "Perplexity of Topic Model by # of Topics", type = "l")
which(perplexities == min(perplexities))

dtm = removeSparseTerms(dtm, 0.99)
rowTotals = apply(dtm, 1, sum)
dtm = dtm[rowTotals > 0,]

#Wordcloud of Everything
library(wordcloud);
m = as.matrix(dtm);
v = sort(colSums(m), decreasing=TRUE);
myNames = names(v);
d = data.frame(word=myNames, freq=v);
wordcloud(d$word, colors=c(1,2), random.color=FALSE, d$freq, min.freq=20);
title(main = "Word Cloud of All Tweets")

p = ggplot(filter(d, freq > 50), aes(word, freq))
p = p + geom_bar(stat="identity")   
p = p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p = p + ggtitle(paste("Histogram of all Tweets", sep = " "))
p

k = 12
fitted = LDA(dtm, k = k, method = "Gibbs")

json = topicmodels_json_ldavis(fitted, corp, dtm)
out.dir = paste("LDAvis", k, sep = "")
serVis(json, out.dir = out.dir, open.browser = FALSE)

#wordclouds of topics
json = rjson::fromJSON(file = paste(getwd(), out.dir, "lda.json", sep = "/"))
terms = json$tinfo$Term
freq = json$tinfo$Freq
topic = json$tinfo$Category
data = data.frame(terms, freq, topic)
library(ggplot2)   

for(i in 1:1) {
  temp = filter(data, topic == paste("Topic", i, sep = ""))
  p = ggplot(filter(temp, freq > 1), aes(terms, freq))    
  p = p + geom_bar(stat="identity")   
  p = p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
  p = p + ggtitle(paste("Histogram of Topic", i, sep = " "))
  ggsave(p, filename = paste(k, " Topic LDA Histogram of Topic ", i, ".png", sep = ""))
  wordcloud(temp$terms, colors = c(1,2), random.color = FALSE, temp$freq, min.freq = 0)
  title(main = paste("Topic", i, "Word Cloud with", k, "Topic Model", sep = " "))
}


findAssocs(dtm, "job", corlimit = 0.3)


library(cluster)   
d <- dist(t(dtm), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
plot(fit, hang=-1, xlab = "Term", main = "Cluster Dendrogram of Tweets") 


topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- rowSums(as.matrix(doc_term))
#  for (i in 1:length(corpus)) {
#    temp <- paste(corpus[[i]]$content, collapse = ' ')
#    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
#  }
  temp_frequency = as.matrix(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}


#wordclouds of topics
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
K = 4
num.iterations = 250
result <- lda.collapsed.gibbs.sampler(doclines$documents, K, doclines$vocab, 
                                      num.iterations, 0.1, 0.1, compute.log.likelihood = TRUE)
#ldavis using lda package
fit = result
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))



#wordcloud
library(wordcloud)
i = 1
cloud.data = sort(result$topics[i,], decreasing = TRUE)[1:50]
wordcloud(names(cloud.data), freq = cloud.data, scale = c(4, 0.1), min.freq = 1, rot.per = 0, random.order = FALSE)

