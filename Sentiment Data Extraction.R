#Clear R Environment
rm(list=ls())

# Load the required R libraries

install.packages("RColorBrewer")
install.packages("tm")
install.packages("wordcloud")
install.packages('base64enc')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')
install.packages('twitteR')

library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)


download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")


consumerKey <- "6FM0Y8jJR34uKy0lxSWgDe07a"
consumerSecret <- "tY3BWT7IgQjIMfbkMd7XpOudh32z1IGr6VRIgzqWKOMpxPvVjg"
accessToken <- "371128987-snfInkFPgFsKPexGX1IXnloxGS8CGh6hrLebbCpU"
accessTokenSecret <- "vrKu8mhsfxC7ViHa0fkmIA690Wgv5zevHC1MZCJh5oAE7"

# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessTokenSecret)


#Objectname <- searchTwitter(searchString, n=no.of tweets, lang=NULL)
# 
namo <- searchTwitter('narendra modi', n=3000, lang="en")
# length (namo)
# namo
# #homeTimeline (n=15) #tweets from own timeline
# #mentions (n=15) #tweets where you have been tagged
# 
# tweet <- userTimeline('@harshbg',n=100)


no.of.tweets <- 1000

ktk <- searchTwitter('#karnatka', n=no.of.tweets, lang="en")

ktk2 <- searchTwitter('#karnatkaelections2018', n=no.of.tweets, lang="en")

ktk3 <- searchTwitter('#karnatkaelection', n=no.of.tweets, lang="en")

ktk4 <- searchTwitter('#battleforkarnatka', n=no.of.tweets, lang="en")

ktk5 <- searchTwitter('#karnatkakurukshetra', n=no.of.tweets, lang="en")

ktk6 <- searchTwitter('#KarnatkaAssembly', n=no.of.tweets, lang="en")

ktk7 <- searchTwitter('#karnatkavoting', n=no.of.tweets, lang="en")

ktk8 <- searchTwitter('#karnatkapolling', n=no.of.tweets, lang="en")

bjp <- searchTwitter('bjp', n=10000, lang="en")

congress <- searchTwitter('congress', n=2000, lang="en")

namo <- searchTwitter('narendra modi', n=2000, lang="en")

raga <- searchTwitter('rahul gandhi', n=2000, lang="en")


install.packages("SnowballC")
library(wordcloud)
library(SnowballC)
library(tm)
namo

namo_text_corpus <- iconv(namo_text_corpus, 'UTF-8', 'ASCII')

namo_text <- sapply(namo, function(x) x$getText())

namo_text_corpus <- Corpus(VectorSource(namo_text))

namo_text_corpus <- tm_map(namo_text_corpus, removePunctuation)

namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(tolower))

namo_text_corpus <- tm_map(namo_text_corpus, function(x)removeWords(x,stopwords()))

namo_text_corpus <- tm_map(namo_text_corpus, removeWords, c('RT', 'are','that'))


removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(removeURL))

insta_2 <- TermDocumentMatrix(namo_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)



namo_text_corpus=str_replace_all(namo_text_corpus,"[^[:graph:]]", " ") 

tm_map(namo_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

namo_text <- sapply(bjp, function(x) x$getText())

namo_text_corpus <- Corpus(VectorSource(namo_text))

namo_text_corpus <- tm_map(namo_text_corpus, removePunctuation)

namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(tolower))

namo_text_corpus <- tm_map(namo_text_corpus, function(x)removeWords(x,stopwords()))

namo_text_corpus <- tm_map(namo_text_corpus, removeWords, c('RT', 'are','that'))


removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(removeURL))

insta_2 <- TermDocumentMatrix(namo_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)


deleteStatus(kapil9994)


##############Sentiment Analysis###########

getwd()
setwd('D:\\Project\\Sentiment Analysis')


pos.words <- read.csv('positive.csv')
neg.words <- read.csv('negative.csv')

pos.words <- scan('positive.csv',what = 'character')
neg.words <- scan('negative.csv',what = 'character')


pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')

neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 
              'freaking','sucks','horrible')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


##Narendra Modi

namog <- ldply(namo,function(t) t$toDataFrame() )

result1 <- score.sentiment(namog$text,pos.words,neg.words)
summary(result1$score)

hist(result1$score,col = 'dark orange', main = 'Sentiment Analysis for Narendra Modi ', ylab = 'Count of tweets')

count(result1$score)

library(xlsx)
write.xlsx(result1, "myResults.xlsx")


##BJP

bjpg <- ldply(bjp,function(t) t$toDataFrame() )

result2 <- score.sentiment(bjpg$text,pos.words,neg.words)
summary(result2$score)

hist(result2$score,col = 'dark orange', main = 'Sentiment Analysis for BJP ', ylab = 'Count of tweets')

count(result2$score)

##Congress

congg <- ldply(congress,function(t) t$toDataFrame() )

result3 <- score.sentiment(congg$text,pos.words,neg.words)
summary(result3$score)

hist(result3$score,col = '  blue', main = 'Sentiment Analysis for Congress ', ylab = 'Count of tweets')

count(result3$score)

##Rahul Gandhi

ragag <- ldply(raga,function(t) t$toDataFrame() )

usableText=str_replace_all(ragag$text,"[^[:graph:]]", " ") 

result4 <- score.sentiment(usableText,pos.words,neg.words)

summary(result4$score)

hist(result4$score,col = ' blue', main = 'Sentiment Analysis for Rahul Gandhi ', ylab = 'Count of tweets')

count(result4$score)



