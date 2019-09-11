
#Packages required:
library(twitteR)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(gridExtra)
library(RTextTools)
library(e1071)
library(readxl)
library(ggplot2)
library(stringr)
library(lubridate)
library(scales)
library(rio)

#Data collection
setup_twitter_oauth('No3KXNJFUEHzPtdSfNwcl9BTW', 
                    'QME0yYDgeBAG32MpwDSacTHNSQphuuRE2t6Vftrn4Z2WPcHgDG',
                    '52910408-dm0xWnph1Wjj3DcW8e5W2fHE9iF2lcSseUzecKL6r',
                    'h09rAFJe8Yy3bURERAPuSlAI1XoTanvVjS9WbH3b5Javt')

bestpic = searchTwitter('Best Picture #Oscars -RT', n = 7000)
bestpicDF = twListToDF(bestpic)
export(bestpicDF,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_PictureDF2.csv')

bestpichash = searchTwitter('#TheShapeOfWater -RT', n = 7000)
bestpicDF2 = twListToDF(bestpichash)
export(bestpicDF2,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_PictureHTDF.csv')

bestactorhash = searchTwitter('Best Actor #Oscars -RT', n = 7000)
bestactorDF2 = twListToDF(bestactorhash)
export(bestactorDF2,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_ActorDF.csv')

bestactor = searchTwitter('#BestActor OR #GaryOldmen -RT', n = 7000) # 2555 points
bestactDF = twListToDF(bestactor)
export(bestactorDF,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_ActorHTDF.csv')

bestactoranimated = searchTwitter('Best Animated #Oscars -RT', n = 7000) # 4164 points
bestanimatedDF = twListToDF(bestactoranimated)
export(bestanimatedDF,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_AnimatedDF.csv')

bestanimated = searchTwitter('(#Coco OR #Cocomovie) AND (#Oscars OR Oscars90) OR #BestAnimated', n = 7000)
bestanimatedDF2 = twListToDF(bestanimated)
export(bestanimatedDF2,'E:/Desktop Stuff/Ryerson/DS 8006/Project/Best_AnimatedDF2.csv')

#Reading csv data files
BestPicture <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/BestPicSample.csv",head=TRUE,sep=",")
TheShapeOfWater <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/Best_PictureHTDF.csv",head=TRUE,sep=",")

BestActor <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/Best_ActorDF.csv",head=TRUE,sep=",")
GaryOldmen <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/Best_ActorHTDF.csv",head=TRUE,sep=",")

BestAnimated <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/Best_AnimatedDF.csv",head=TRUE,sep=",")
CocoMovie <- read.csv(file="/Users/Luojiefan/Desktop/AOscars Datasets/Best_AnimatedDF2.csv",head=TRUE,sep=",")

#Time Series Analysis
#Conversion time format
BestPicture$timestamp<-ymd_hms(BestPicture$created)
TheShapeOfWater$timestamp<-ymd_hms(TheShapeOfWater$created)
BestAnimated$timestamp<-ymd_hms(BestAnimated$created)
CocoMovie$timestamp<-ymd_hms(CocoMovie$created)

#Plot histograms of the number of tweets over time
ggplot(data = BestPicture, aes(x = BestPicture$timestamp)) +
  geom_histogram(aes(fill = ..count..), bins = 30) +
  theme(legend.position= "none") +  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  xlab("") + ylab("Number of tweets") + ggtitle("Number of Tweets Changes Over Time in Best Picture Dataset")
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

ggplot(data = TheShapeOfWater, aes(x = TheShapeOfWater$timestamp)) +
  geom_histogram(aes(fill = ..count..), bins = 30) +
  theme(legend.position= "none") +  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  xlab("Time") + ylab("Number of tweets") + ggtitle("Number of Tweets Changes Over Time in The Shape Of Water Dataset")
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

  
ggplot(data = BestAnimated, aes(x = BestAnimated$timestamp)) +
  geom_histogram(aes(fill = ..count..), bins = 30) +
  theme(legend.position= "none") +  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  xlab("") + ylab("Number of tweets") + ggtitle("Number of Tweets Changes Over Time in Best Animated Dataset")
scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

ggplot(data = CocoMovie, aes(x = CocoMovie$timestamp)) +
  geom_histogram(aes(fill = ..count..), bins = 30) +
  theme(legend.position= "none") +  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  xlab("") + ylab("Number of tweets") + ggtitle("Number of Tweets Changes Over Time in Coco Movie Dataset")
scale_fill_gradient(low = "midnightblue", high = "aquamarine4")  
  

#Text Analysis  
removeEmoticon <- function(dataframe) {
  dataframe$text <-  sapply(dataframe$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  return(dataframe)
}

BestPicture <- removeEmoticon(BestPicture)
TheShapeOfWater <- removeEmoticon(TheShapeOfWater)

BestActor <- removeEmoticon(BestActor)
GaryOldmen <- removeEmoticon(GaryOldmen)

BestAnimated <- removeEmoticon(BestAnimated)
CocoMovie <- removeEmoticon(CocoMovie)

# Create term-document matrix
tdmCreator <- function(dataframe, addstopwords, stemDoc = F){
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  removeURL <- function(x) gsub("https[^[:space:]]*", "",x)
  tdm <- tm_map(tdm, content_transformer(removeURL))
  tdm <- TermDocumentMatrix(tdm, control = list(wordLengths = c(3, Inf),
                            removePunctuation = TRUE,tolower = TRUE,  
                            stopwords = c(addstopwords,stopwords("english"))))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}


#Add stopwords base on noise words
stopwordsBestPicture <- c("oscars","picture","wins","Oscars","best","winning","win","winner","get","just","ever")
BestPicture_tdm <- tdmCreator(BestPicture, stopwordsBestPicture)
stopwordsTheShapeOfWater <- c("theshapeofwater","los","por","del","que")
TheShapeOfWater_tdm <- tdmCreator(TheShapeOfWater, stopwordsTheShapeOfWater)

addstopwordsBestActor <- c("best","actor","oscars","won","win")
BestActor_tdm <- tdmCreator(BestActor, addstopwordsBestActor)
addstopwordsGaryOldmen <- c("bestactor","oscar","gary","oldman","best","actor","wins","winner","win","ever","will","winning","film")
GaryOldmen_tdm <- tdmCreator(GaryOldmen, addstopwordsGaryOldmen)

stopwordsBestAnimated <- c("best", "animated","oscars","feature","film","won","win","goes")
BestAnimated_tdm <- tdmCreator(BestAnimated, stopwordsBestAnimated)
stopwordsCocoMovie <- c("coco", "cocomovie","oscars","oscars90","best","animated","feature","film","won","win","goes","winning","winner")
CocoMovie_tdm <- tdmCreator(BestAnimated, stopwordsCocoMovie)


#Visualize word frequency using Word Cloud
BestPicture_wc <- wordcloud(words = BestPicture_tdm$term,freq = BestPicture_tdm$freq, scale=c(3,.6),min.freq=3,
          max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))
TheShapeOfWater_wc <- wordcloud(words = TheShapeOfWater_tdm$term,freq = TheShapeOfWater_tdm$freq, scale=c(2,.6),min.freq=3,
                                max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))

BestActor_wc <- wordcloud(words =BestActor_tdm$term,freq = BestActor_tdm$freq, scale=c(2,.6),min.freq=3,
                               max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))
GaryOldmen_wc <- wordcloud(words = GaryOldmen_tdm$term,freq = GaryOldmen_tdm$freq, scale=c(1.8,.6),min.freq=3,
          max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))

BestAnimated_wc <- wordcloud(words = BestAnimated_tdm$term,freq = BestAnimated_tdm$freq, scale=c(2,.6),min.freq=3,
                               max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))
CocoMovie_wc <- wordcloud(words = CocoMovie_tdm$term,freq = CocoMovie_tdm$freq, scale=c(2,.6),min.freq=3,
                             max.words=100, random.order=F, colors=brewer.pal(8, "Dark2"))


#Selects the 20 most used words.
BestPicture_tdm20 <- BestPicture_tdm[1:20,]
TheShapeOfWater_tdm20 <- TheShapeOfWater_tdm[1:20,]

BestActor_tdm20 <- BestActor_tdm[1:20,]
GaryOldmen_tdm20 <- GaryOldmen_tdm[1:20,]

BestAnimated_tdm20 <- BestAnimated_tdm[1:20,]
CocoMovie_tdm20 <- CocoMovie_tdm[1:20,]

#Create bar charts based on the term frequency
BestPicture_tdm20_plot <- ggplot(BestPicture_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity",aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") + 
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in Best Picture Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
BestPicture_tdm20_plot 

TheShapeOfWater_tdm20_plot <- ggplot(TheShapeOfWater_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity",aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") + 
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in The Shape Of Water Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
TheShapeOfWater_tdm20_plot 


BestActor_tdm20_plot <- ggplot(BestActor_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") +
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in Best Actor Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
BestActor_tdm20_plot

GaryOldmen_tdm20_plot <- ggplot(GaryOldmen_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") +
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in Gary Oldmen Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
GaryOldmen_tdm20_plot


BestAnimated_tdm20_plot <- ggplot(BestAnimated_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") +
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in Best Animated Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
BestAnimated_tdm20_plot

CocoMovie_tdm20_plot <- ggplot(CocoMovie_tdm20, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", aes(fill = freq)) + scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  xlab("Most Used Words") + ylab("How Often") +
  coord_flip() + ggtitle("The Top 20 High-Frequency Words in Coco Movie Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold"))
CocoMovie_tdm20_plot




######Sentiment analysis using Bayesian classifier
#Load training set
training_set<- read.delim("/Users/Luojiefan/OneDrive/2018WinterStudyRyerson/DS8006 Social Media Analytics/GroupProject/4A-English/SemEval2017-task4-dev.subtask-A.english.INPUT.txt", header = FALSE, sep = "\t", dec = ".")
training_set <- as.data.frame(training_set)
training_set$V4 <- NULL 
colnames(training_set) <- c("id", "sentiment","text")
training_set <- training_set[!(training_set$sentiment == "neutral"), ]
training_set$sentiment <- factor(training_set$sentiment)
training_set <- training_set[sample(nrow(training_set)),]

# build the training set dtm 
train_matrix= create_matrix(training_set[,3], language="english", 
                            removeStopwords=TRUE, removeNumbers=TRUE, stemWords=FALSE) 
#train the model
train_mat <- as.matrix(train_matrix)
classifier <- naiveBayes(train_mat , as.factor(training_set[,2]) )


#In order to reduce the training time sample 3000 tweets
BestPicture <- BestPicture[sample(nrow(BestPicture), 3000), ]
#Build the test set dtm for best picture dataset
BestPicture_matrix <- create_matrix(BestPicture[,1], language="english", 
                                removeStopwords=TRUE, removeNumbers=TRUE, 
                                stemWords=FALSE) 
BestPicture_matrix <- as.matrix(BestPicture_matrix)
#Classify positive and negative tweets in data sets
predicted <- predict(classifier, BestPicture_matrix)
BestPicture$sentiment <- predicted
BestPicture_sentiment_plot<- ggplot(BestPicture, aes(x = BestPicture$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in Best Picture Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
BestPicture_sentiment_plot


TheShapeOfWater <- TheShapeOfWater[sample(nrow(TheShapeOfWater), 3000), ]
TheShapeOfWater_matrix <- create_matrix(TheShapeOfWater[,1], language="english", 
                                    removeStopwords=TRUE, removeNumbers=TRUE, 
                                    stemWords=T) 
TheShapeOfWater_matrix <- as.matrix(TheShapeOfWater_matrix)
predicted <- predict(classifier, TheShapeOfWater_matrix)
TheShapeOfWater$sentiment <- predicted
TheShapeOfWater_sentiment_plot<- ggplot(TheShapeOfWater, aes(x = TheShapeOfWater$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in The Shape Of Water Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
TheShapeOfWater_sentiment_plot


BestActor <- BestActor[sample(nrow(BestActor), 3000), ]
BestActor_matrix <- create_matrix(BestActor[,1], language="english", 
                        removeStopwords=TRUE, removeNumbers=TRUE, 
                        stemWords=FALSE) 
BestActor_matrix<- as.matrix(BestActor_matrix)
predicted <- predict(classifier, BestActor_matrix)
BestActor$sentiment <- predicted
BestActor_sentiment_plot<- ggplot(BestActor, aes(x = BestActor$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in Best Actor Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
BestActor_sentiment_plot



GaryOldmen <- GaryOldmen[sample(nrow(GaryOldmen), 3000), ]
GaryOldmen_matrix <- create_matrix(GaryOldmen[,1], language="english", 
                                removeStopwords=TRUE, removeNumbers=TRUE, 
                                stemWords=FALSE) 
GaryOldmen_matrix <- as.matrix(GaryOldmen_matrix)
predicted <- predict(classifier, GaryOldmen_matrix )
GaryOldmen$sentiment <- predicted
GaryOldmen_sentiment_plot<- ggplot(GaryOldmen, aes(x = GaryOldmen$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in Gary Oldmen Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
GaryOldmen_sentiment_plot


BestAnimated <- BestAnimated[sample(nrow(BestAnimated), 3000), ]
BestAnimated_matrix <- create_matrix(BestAnimated[,1], language="english", 
                                removeStopwords=TRUE, removeNumbers=TRUE, 
                                stemWords=FALSE) 
BestAnimated_matrix <- as.matrix(BestAnimated_matrix)
predicted <- predict(classifier, BestAnimated_matrix )
BestAnimated$sentiment <- predicted
BestAnimated_sentiment_plot<- ggplot(BestAnimated, aes(x = BestAnimated$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in Best Animated Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
BestAnimated_sentiment_plot


CocoMovie <- CocoMovie[sample(nrow(CocoMovie), 3000), ]
CocoMovie_matrix <- create_matrix(CocoMovie[,1], language="english", 
                                     removeStopwords=TRUE, removeNumbers=TRUE, 
                                     stemWords=FALSE) 
CocoMovie_matrix <- as.matrix(CocoMovie_matrix)
predicted <- predict(classifier, CocoMovie_matrix )
CocoMovie$sentiment <- predicted
CocoMovie_sentiment_plot<- ggplot(CocoMovie, aes(x = CocoMovie$sentiment)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) +
  ylab("Percentage of tweets") +xlab("") + ggtitle( "Negative Tweets Vs Positive Tweets in Coco Movie Dataset") +
  theme(plot.title = element_text(hjust=0.5, face="bold")) 
CocoMovie_sentiment_plot


#Network Analysis
library(igraph)
#Need export the Edgelist formatting file from Netlytic to create the network object
#For Best_Picture
BestPicNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/R Edgelist Files/net_Project-Best_Picture.csv",header=T,sep=",")
BestPicg <- graph.edgelist(as.matrix(BestPicNet[,c(2,3)]),directed=T)

#Simplify Network
BestPicg <- simplify(BestPicg, remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(BestPicg)

#Use community detection algorithm
comm <- label.propagation.community(as.undirected(BestPicg ,mode="collapse"))
V(BestPicg)$color <- comm$membership+1

plot(BestPicg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(BestPicg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(BestPicg)>100,V(BestPicg)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Best Picture Dataset")


#For Shape Of Water
ShapeOfWaterNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/R Edgelist Files/net_Project-ShapeOfWater.csv",header=T,sep=",")
ShapeOfWaterg <- graph.edgelist(as.matrix(ShapeOfWaterNet[,c(2,3)]),directed=T)

ShapeOfWaterg <- simplify(ShapeOfWaterg, remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(ShapeOfWaterg)
comm <- label.propagation.community(as.undirected(ShapeOfWaterg ,mode="collapse"))
V(ShapeOfWaterg)$color <- comm$membership+1

plot(ShapeOfWaterg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(ShapeOfWaterg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(ShapeOfWaterg)>300,V(ShapeOfWaterg)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Shape Of Water Dataset")


BestActorNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/R Edgelist Files/net_Project-Best_Actor.csv",header=T,sep=",")
BestActorNetg <- graph.edgelist(as.matrix(BestActorNet[,c(2,3)]),directed=T)
BestActorNetg <- simplify(BestActorNetg, remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(BestActorNetg)
comm <- label.propagation.community(as.undirected(BestActorNetg ,mode="collapse"))
V(BestActorNetg)$color <- comm$membership+1

plot(BestActorNetg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(BestActorNetg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(BestActorNetg)>150,V(BestActorNetg)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Best Actor Dataset")


#For Gary Oldman (Best_ActorHTDF.csv)
GaryOldmanNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/R Edgelist Files/net_Project-Gary_Oldman.csv",header=T,sep=",")
GaryOldmang <- graph.edgelist(as.matrix(GaryOldmanNet[,c(2,3)]),directed=T)
GaryOldmang  <- simplify(GaryOldmang , remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(GaryOldmang)
comm <- label.propagation.community(as.undirected(GaryOldmang  ,mode="collapse"))
V(GaryOldmang)$color <- comm$membership+1

plot(GaryOldmang, layout=layout, vertex.label.color= "black",vertex.frame.color=V(GaryOldmang)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(GaryOldmang)>100,V(GaryOldmang)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Gary Oldman Dataset")


#For Best Animated 
BestAnimatedNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/net_Project-Best_Animated_Fil.csv",header=T,sep=",")
BestAnimatedg <- graph.edgelist(as.matrix(BestAnimatedNet[,c(2,3)]),directed=T)
BestAnimatedg <- simplify(BestAnimatedg, remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(BestAnimatedg)
comm <- label.propagation.community(as.undirected(BestAnimatedg ,mode="collapse"))
V(BestAnimatedg)$color <- comm$membership+1

plot(BestAnimatedg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(BestAnimatedg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(BestAnimatedg)>200,V(BestAnimatedg)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Best Animated Dataset")


#For Coco Movie 
CocoMovieNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/net_Project-CocoMovie.csv",header=T,sep=",")
CocoMovieg <- graph.edgelist(as.matrix(CocoMovieNet[,c(2,3)]),directed=T)
CocoMovieg <- simplify(CocoMovieg, remove.multiple = FALSE, remove.loops = TRUE)
layout <- layout.auto(CocoMovieg)
comm <- label.propagation.community(as.undirected(CocoMovieg ,mode="collapse"))
V(CocoMovieg)$color <- comm$membership+1

plot(CocoMovieg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(CocoMovieg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(CocoMovieg)>180,V(CocoMovieg)$name,NA),edge.width=1,vertex.label.font=0.001,
     vertex.label.cex=0.8, main="Network for Coco Movie")


