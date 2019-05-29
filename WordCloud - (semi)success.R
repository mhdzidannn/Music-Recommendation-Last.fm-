library(shiny)
library(dplyr)
library(readr)
library(DT)
library(data.table)
library(shinyWidgets)
library(markdown)
library(tm)
library(wordcloud)
library(memoise)
library(SnowballC)
library(RColorBrewer)


#tag <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tags.csv", 
#                    header=FALSE, sep = ",", stringsAsFactors = FALSE)

#user_ta <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/user_taggedartists.csv", 
#                header=FALSE, sep = ",", stringsAsFactors = FALSE)


#tag_user <- merge(x=user_ta, y=tag, by="tagID")
#print(tag_user)
#write.csv(tag_user,'tag_user.csv')


tag_user_github <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tag_user.csv", 
                    header=FALSE, stringsAsFactors = FALSE)


#head(tag_user_github)

tag_user_github <- as.character(tag_user_github)

tag_user_github.corpus<-Corpus(VectorSource(tag_user_github))

tag_user_github.corpus <- tag_user_github.corpus%>%
  tm_map(removePunctuation)%>% ##eliminate punctuation
  tm_map(removeNumbers)%>% #no numbers
  tm_map(stripWhitespace)#white spaces

tag_user_github.corpus <- tag_user_github.corpus%>%
  tm_map(tolower)%>% ##make all words lowercase
  tm_map(removeWords, stopwords("english"))

tag_user_github.corpus <- tm_map(tag_user_github.corpus, stemDocument)
tag_user_github.corpus <- tm_map(tag_user_github.corpus, removeWords, c("progress", "music industry", "singersongwrit", "vocalist", "fuck", "song")) 

tag_user_github.counts<-as.matrix(TermDocumentMatrix(tag_user_github.corpus))
tag_user_github.freq<-sort(rowSums(tag_user_github.counts), decreasing=TRUE)
#head(tag_user_github.freq)##what are the top words?


set.seed(32) #be sure to set the seed if you want to reproduce the same again

wordcloud(words=names(tag_user_github.freq), freq=tag_user_github.freq, scale=c(3,.5),max.words = 100, random.order = TRUE)
