#List Of Packages Used
library(RColorBrewer) 
library(tm)
library(twitteR)
library(ROAuth)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
library(base64enc)
library(SnowballC)
library(ggplot2)
library(maps)
library("httr")
library("openssl")
library("httpuv")
library(rtweet)
install.packages("rtweet")

install.packages("tm")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("SnowballC")
install.packages("maps")
install.packages("httr")
install.packages("openssl")
install.packages("httpuv")

consumerKey <- "JjpffUD0iPyRRsG1xbJLB1GjW"
requestURL<- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

consumerSecret <- "JYgGbJvpPn6v7EXWdzy7zGuy42rAa5jcEyK90mzcB17xpMs8Sp"
accessToken <- "1133384037660340224-fsnV31ZiUF4gE9EoN9rmxMvUrYoGjt"
accessTokenSecret <- "qtLqrPWk9ju7PsefS5JGsbnlTRu9P0lceefNupMdaqhAL"

twitCred<-OAuthFactory$new(consumerKey=consumerKey,
                           consumerSecret=consumerSecret,
                           requestURL=requestURL,
                           accessURL=accessURL,
                           authURL=authURL)

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem") #downloads the certificate

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))






setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#search TWitter
tweet1<-userTimeline("@IndianCricNews", n=100)


#Converting into Dataframe 
tweet.df = do.call("rbind",lapply(tweet1,as.data.frame))


#Plotting data on map
map('world')
points(tweet.df$longitude,tweet.df$latitude, pch=20, cex=1, col="red")

#Viewing the data
View(tweet.df)

#Reading sentiment analysis data from Txt document
pos.words = scan('./positive-words.txt', what='character', comment.char=';')
neg.words = scan('./negative-words.txt', what='character', comment.char=';')

#Appending some more words to actual words
pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly','not', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')

#converting Into dataFrame
test <-ldply(tweet1,function(t)t$toDataFrame())

#calcuating result
result <- score.sentiment(test$text,pos.words,neg.words)

#summarlizing data
summary(result$score)

#Histogram
hist(result$score,col="yellow", main="Score of tweets",ylab=" Count of tweets")

#Count No of Tweets
count(result$score)

#ploting the tweets on qplot
qplot(result$score,xlab = "Score of tweets")

#score Sentiment function 
#Used to remove all unwanted data 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

    





