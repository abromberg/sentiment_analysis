library(twitteR)
library(plyr)
library(stringr)

posTerms <- scan('positive-words.txt', what='character',comment.char=';')
negTerms <- scan('negative-words.txt', what='character',comment.char=';')
posTerms <- c(posTerms, '#great')
negTerms <- c(negTerms, 'bug', 'wtf')

searchTerm <- '@netflix'

tweets <- searchTwitter(searchTerm, n=100)

text <- laply(tweets, function(x) { x$getText() })

sentimentScore <- function(sentences, posTerms, negTerms, .progress='none'){
  scores <- laply(sentences, function(sentence, posTerms, negTerms){
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    posMatches <- match(words, posTerms)
    negMatches <- match(words, negTerms)
    posMatches <- !is.na(posMatches)
    negMatches <- !is.na(negMatches)
    score <- sum(posMatches) - sum(negMatches)
    return(score)
  }, posTerms, negTerms, .progress=.progress)
  scores <- data.frame(score=scores, text=sentences)
  return(scores)
}