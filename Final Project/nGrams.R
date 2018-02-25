
library(ggplot2); 
library(NLP); 
library("tm"); 
library("RWeka"); 
library(wordcloud); 
library("gridExtra");
library("rJava")
library("tokenizers")
setwd("H:\\Data Science Johns Hopkins\\Capstone Project\\Coursera-SwiftKey\\final\\en_US")
blogs<-readLines("en_US.blogs.txt",encoding="UTF-8")
news<-readLines("en_US.news.txt",encoding="UTF-8")
twitter<-readLines("en_US.twitter.txt",encoding="latin1")

## Data Cleansing
sampleSize<-5000 
cleanData<-function(x) { 
  # x<-tm_map(x,removeWords, badtext)
  # x<-tm_map(x, content_transformer(tolower))
  myCorpus <- sample(x,sampleSize) 
  myCorpus <- Corpus(VectorSource(myCorpus))
  myCorpus <- tm_map(myCorpus, tolower)
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  myCorpus <- tm_map(myCorpus,removePunctuation,preserve_intra_word_dashes = TRUE)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords,stopwords("en"))
  myCorpus <- tm_map(myCorpus, stripWhitespace)
}
blogsClean<-cleanData(blogs); newsClean<-cleanData(news); twitterClean<-cleanData(twitter)
data<-c(blogsClean,newsClean,twitterClean)

## 2. What are the frequencies of 2-grams and 3-grams in the dataset?

gramToken<-function(x,n){
  df<-NGramTokenizer(x)
  df<-NGramTokenizer(df, Weka_control(min=n, max=n))
  df<-data.frame(table(df))
  df<-df[order(df$Freq, decreasing = TRUE),]
}
FourgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=4, max=4))}
options(mc.cores=1)
dtm.docs.4g <- DocumentTermMatrix(data, control=list(tokenize=FourgramTokenizer))
gramOne<-gramToken(data,1)
gramTwo<-gramToken(data,2)
gramThree<-gramToken(data,3)
gramFour<-gramToken(data,4)

saveRDS(ngramOne, file = "gramOne.rds")
saveRDS(ngramTwo, file = "gramTwo.rds")
saveRDS(ngramThree, file = "gramThree.rds")
saveRDS(ngramFour, file = "gramFour.rds")

