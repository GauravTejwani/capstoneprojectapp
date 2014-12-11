library(shiny)
#Invoke Libraries
library(tm)
library(stringi)
library(SnowballC)
library(caTools)
library(ggplot2)
library(dplyr)
library(RWeka)
library(slam)

#Set working Directory
setwd("C:/Users/gt8616/Desktop/Coursera/Projects/Capstone/final/en_US/19112014")
tdm_2 <- readRDS("12dectdm_2.rds")
tdm_3 <- readRDS("12dectdm_3.rds")
tdm_4 <- readRDS("12dectdm_4.rds")
df_2 <- readRDS("12decdf_2.rds")
df_3 <- readRDS("12decdf_3.rds")
df_4 <- readRDS("12decdf_4.rds")
highfreqcorpus <- readRDS("12dechighfreqcorpus.rds")

#Convert tdms to matrix
tdm_2 <- as.matrix(tdm_2)
tdm_3 <- as.matrix(tdm_3)
tdm_4 <- as.matrix(tdm_4)

#Input the sentence and find the prediction
predict <- function(sentence) {
  # given a sentence/phrase, extract the last two words
  tospace <- function(sample,pattern) gsub(pattern," ",sample)
  sentence <- tospace(sentence,"\\S{0,}[^[:alpha:][:space:]]\\S{0,}\\s?")
  sentence <- tospace(sentence,"\\b[a-zA-Z]{1}\\b")
  sentence <- removeWords(sentence,c(stopwords("english"),"2f4u","4yeo fyeo","4yeo fyeo",
                                     "aamof","ack","afaik","afair","afk","aka","b2k btk","btt","btw","b/c",
                                     "bc","c&p","cnp","cu","cys","diy","eobd","eod","eom","eot","faq","fack",
                                     "fka","fwiw","jfyi","fyi","hf","hth","iirc","imho","imo",
                                     "imnsho","iow","itt","lol","mmw","n/a","na","nan","nntr","noob",
                                     "n00b","noyb","nrn","omg","op","ot","otoh","pebkac","pov",
                                     "rotfl","rsvp","rtfm","scnr","sflr","spoc","tba","tbc","tia",
                                     "tnx","thx","tq","tyvm","tyt","ttyl","w00t","wfm","wrt","wth",
                                     "ymmd","ymmv","yam"))
  sentence <- tospace(sentence,"([[:space:]])\\1{2,}")
  sentence <- stemDocument(sentence)
  #print(sentence)
  sl <- unlist(strsplit(sentence," "))
  len <- length(sl)
  trigram <- paste(sl[len-2],sl[len-1],sl[len])
  #print(trigram)
  # get the subset of the tetragram data table with a matching trigram start
  match_4 <- df_4[df_4$start == trigram,]
  head(match_4)
  #check if trigram was found in the tetragram table. If found, select top 10 words and display
  if(nrow(match_4)>0){
  #  print("Based on tetragram analysis, looks like this is the word you want next")
    match_4 <- match_4[order(-match_4$freq),]
    n <- min(10,nrow(match_4))
    predictions <- vector()
    for(i in 1:n){
      #predictions[i] <- stemCompletion(ftdf[i,4],highfreqcorpus)
      predictions[i] <- stemCompletion(match_4[i,4],highfreqcorpus)
    }
    return(predictions)
  } else {
    bigram <- paste(sl[len-1],sl[len])
   # print(bigram)
    match_3 <- df_3[df_3$start == bigram,]
    head(match_3)
    if(nrow(match_3)>0){
    #  print("Based on trigram analysis, looks like this is the word you want next")
      match_3 <- match_3[order(-match_3$freq),]
      n <- min(10,nrow(match_3))
      predictions <- vector()
      for(i in 1:n){
        #predictions[i] <- stemCompletion(ftdf[i,4],highfreqcorpus)
        predictions[i] <- stemCompletion(match_3[i,4],highfreqcorp)
      }
      return(predictions)
    } else {
      unigram <- sl[len]
     # print(unigram)
      # get the subset of the bigram data table with a matching unigram start
      match_2 <- df_2[df_2$start == unigram,]
      head(match_2)
      #check if unigram was found in the bigram table. If found, select top 10 words and display
      if(nrow(match_2)>0){
      #  print("Based on bigram analysis, looks like this is the word you want next")
        match_2 <- match_2[order(-match_2$freq),]
        n <- min(10,nrow(match_2))
        predictions <- vector()
        for(i in 1:n){
          #predictions[i] <- stemCompletion(ftdf[i,4],highfreqcorpus)
          predictions[i] <- stemCompletion(match_2[i,4],highfreqcorpus)
        }
        return(predictions)
      } else {
        return("Sorry! No Donuts for U")
      }
    }
  }
}


shinyServer(
  
  function(input, output) {
    output$text1 <- renderText({paste(predict(input$text1),sep=",",collapse=", ")})
  }
)
