library(plyr)
library(stringr)
library(e1071)

setwd("~/Documents/GitHub/sentiment_analysis")

afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1]
posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1]
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]

posText <- read.delim(file='rt-polaritydata/rt-polarity-pos.txt', header=FALSE, stringsAsFactors=FALSE)
negText <- read.delim(file='rt-polaritydata/rt-polarity-neg.txt', header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
negText <- negText$V1

sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))

posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')

results <- rbind(posResult, negResult)

classifier <- naiveBayes(results[,2:5], results[,6])

table(predict(classifier, results[,-6]), results[,6], dnn=list('predicted','actual'))

