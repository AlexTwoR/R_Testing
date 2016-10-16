library(twitteR)

if (!require("twitteR")) {
  install.packages("twitteR", repos="http://cran.rstudio.com/") 
  library("twitteR")
}


#------------------------------------
api_key = "uKJsEX20qddn6HpJJw3tuWeDm"
api_secret = "e5MYsre2C4LBWfmXrXJBWXkV5GEM3VYLFUZU9NJH1nwxj1r6O3"
access_token = "140447227-RNX2nbOk6xZGzPqGi6YY2BVTuLbeMML85Fz2SL3y"
access_token_secret = "dxqSYm8qUSVkUxLgKxGGmLk2ROCKkAp5OYN4IFuQCgnKf"
#options(httr_oauth_cache=T)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
?setup_twitter_oauth

#------------------------------------

tweets = searchTwitter("Putin", n=2000)
data = twListToDF(tweets)

summary(data)

library(ggplot2)
c <- ggplot(data, aes(created))
c + geom_bar()

library(ggplot2)
data$month=sapply(data$created, function(x) {p=as.POSIXlt(x);p$mon})
data$hour=sapply(data$created, function(x) {p=as.POSIXlt(x);p$hour})
data$wday=sapply(data$created, function(x) {p=as.POSIXlt(x);p$wday})
ggplot(data)+geom_jitter(aes(x=wday,y=hour))


library(tm)
text = Corpus(DataframeSource(data.frame(data[1])))
text = tm_map(text, removePunctuation)
text = tm_map(text, tolower)
tdm = TermDocumentMatrix(text)
m = as.matrix(tdm)
v = sort(rowSums(m),decreasing=TRUE)
library("wordcloud")
wordcloud(names(v), v^0.3, scale=c(5,0.5),random.order=F, colors="black")
