#Authentication from twitter API
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
reqURL <- "https://api.twitter.com/oauth/request_token"

accessURL <- "http://api.twitter.com/oauth/access_token"

authURL <- "http://api.twitter.com/oauth/authorize"

consumer_key <- "KmUBeDd2SxHEQ8wEjR4EKa1xI"

#consumer_secret<- "VwKDPXKyQsfAP9oB5HkRMH8dE9r7ppoanjZiIEB7zDmdrMz4Xp"
#twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)

#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#twitCred$handshake(cainfo="cacert.pem")

setup_twitter_oauth("KmUBeDd2SxHEQ8wEjR4EKa1xI", "VwKDPXKyQsfAP9oB5HkRMH8dE9r7ppoanjZiIEB7zDmdrMz4Xp")

#sentiment.words <- function(file) {
 # return(scan(file, what="character", comment.char=";"))
#}
# store it in upper case
#ucfirst <- function(txt) {
  #uc_text <- paste(
    #u_to_upper_case(substring(txt, 1, 1)), 
    ##substring(txt, 2),
    #sep="", collapse=""
 # )
  
  #return(uc_text)
##}
# unicode string
#getText <- function(txt) {
  #txt$getText()}
# Load Positive And Negative Words
sentiment.words.negative <- sentiment.words('negative-words.txt')
sentiment.words.positive <- sentiment.words('positive-words.txt')

pos = scan('positive-words.txt', what='character',comment.char=';')
neg = scan('negative-words.txt', what='character',comment.char=';')

# Scoring Each Tweet

sentiment.score <- function(sentences, pos.words, neg.words,companyName, .progress='none')
{  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    
    # We need this for broken tweets with random chars. 
    sentence = try(u_to_lower_case(sentence), TRUE)
    
    # We need to implement an iconv language identification
    # to then load the proper words lists. Right now, 
    # it only supports english.
    word.list = str_split(sentence, '\\s+')
    
    # Second level lists are teh suck. Unlist all of the things.
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # They are not nothing.
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences, name=companyName)
  return(scores.df)
}

aapl.tweets <- searchTwitter("Apple Stock OR ApplOR #AAPL", n=100, lang="en")
aapl.df <- twListToDF(aapl.tweets)
aapl.text = laply(aapl.tweets,function(t)t$getText())
aapl.feel   <- sentiment.score(aapl.df, pos, neg,'AAPL')

amzn.tweets <- searchTwitter("Amazon Stock OR Amzn OR #AMZN", n=100, lang="en")
amzn.df <- twListToDF(amzn.tweets)
aapl.text = laply(amzn.tweets,function(t)t$getText())
amzn.feel   <- sentiment.score(amzn.df, pos.words, neg.words)

XOM.tweets <- searchTwitter("Exxon Stock OR XOM OR #XOM", n=100, lang="en")
XOM.df <- twListToDF(XOM.tweets)
XOM.text = laply(amzn.tweets,function(t)t$getText())
XOM.feel   <- sentiment.score(amzn.df, pos.words, neg.words)

