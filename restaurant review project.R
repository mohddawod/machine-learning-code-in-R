#PROJECTS



#1.restaurant liked and not liked based on reviews 
#import data
library(readr)
res <- read_csv("Desktop/output.csv")
View(res)

#install and load the packages
library(tm)
library(SnowballC)

#cleaning the text
corpus<-VCorpus(VectorSource(res$Review))
#converting into the lower case
corpus<-tm_map(corpus,content_transformer(tolower))->corpus
#removing the numbers
corpus<-tm_map(corpus, removeNumbers)
#removing puctuation
corpus<-tm_map(corpus, removePunctuation)
#remove the stopwords
corpus<-tm_map(corpus, removeWords, stopwords())
#stemming the words
corpus<-tm_map(corpus, stemDocument)
#removing the extar space
corpus<-tm_map(corpus, stripWhitespace)

#creating the word matrix (document term matrix)
dtm<-DocumentTermMatrix(corpus )
#remove column with sparsh term from dtm
dtm<-removeSparseTerms(dtm, .999)
#convert dtm into matrix
data<-as.data.frame(as.matrix(dtm)) 
#converting the target var as factor
data$liked<-as.factor(res$Liked)

#traning the random forest
#spliting the data
library(caTools)
set.seed(123)
split<-sample.split(data$liked, SplitRatio = .7)
train<-subset(data, split==TRUE)
test<-subset(data, split==FALSE)
library(randomForest)
#creating the model
set.seed(212)
rf<-randomForest(x=train[-692],
                 y=train$liked, ntree=40, mtry = 52)
#predict and confusion matrix  based on train data
pre<-predict(rf, train)
library(caret)
confusionMatrix(pre, train$liked)

#predict and confusion matrix  based on train data
pre<-predict(rf, test)
library(caret)
confusionMatrix(pre, test$liked)

#error rate
plot(rf)

#tuning the model
trf<-tuneRF(train[,-692], train[,692],
            stepFactor = .5,
            plot = TRUE,
            ntreeTry = 40,
            trace = TRUE,
            improve = .1)
#no of nodes for the tress
hist(treesize(rf), 
     main='no of nodes ',
     col='yellow')

#variable importance
varImpPlot(rf,
           sort=TRUE, 
           main = 'top variable')# shows important variable


