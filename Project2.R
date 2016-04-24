#Project 2 - Models to automate essay evaluation process

#This code has 4 parts

# First - Using (K Nearest Neighbor) KNN to Grit Essays
# Second - Using Naive Bayes to Grit Essays
# Third - Using (K Nearest Neighbor) KNN to Fit Essays
# Fourth - Using (K Nearest Neighbor) KNN to Grit and Fit both Essays

#Document Classification

# -----------------------------------------First Part --------------------------- #

#Using K Nearest Neighbour Algorithm
#Load Packages
install.packages('tm')
install.packages('plyr')
install.packages('class')

#Importing Data Set
SelectionRound1_MLDataset <- read.csv("C:/Users/Divyansh/Downloads/SelectionRound1_MLDataset.csv", stringsAsFactors=FALSE)
essaydata <- SelectionRound1_MLDataset
dim(essaydata)

#Cleaning Data as per usecase
essaydata <- essaydata[which(essaydata$First.Review.Complete==1),]
for(i in 1:1450)
{
if(nchar(essaydata$Eligibility.Status[i])<75 & grepl('Reject',essaydata$Contact.Master.Status[i],fixed = FALSE))
  essaydata$FinalResult[i] <- 'Reject'
else
  essaydata$FinalResult[i] <- 'Accept'
}

#Creating Dimension Matrix which consists of rows as essay number and columns as list of words
essays <- essaydata$Grit.Essay
write.csv(essays,file = 'essays.csv') #Saving the file externally
#Loading necessary packages
library(tm)
library(plyr)
library(class)

libs=c("tm","plyr","class")
lapply(libs,require,character.only = T)
options(stringsAsFactors= F)
#Importing the CSV File
cat = c("essays.csv")
pathname = "C:/Users/Divyansh/Documents"

#Funciton to clean the document matrix
cleanCorpus <-function(corpus) {
  corpus.tmp = tm_map(corpus,removePunctuation)
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace)  
  corpus.tmp = tm_map(corpus.tmp,tolower)
  corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
  corpus.tmp = tm_map(corpus.tmp,stemDocument)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  return(corpus.tmp)
}

#Function to generate the matrix 
generateTDM <- function(cate,path) {
  s.path =  sprintf("%s/%s",path,cate)
  csv = read.csv(s.path)
  s.cor = Corpus(DataframeSource(csv))
  s.cor.cl = cleanCorpus(s.cor)
  s.tdm= TermDocumentMatrix(s.cor.cl)
  s.tdm = removeSparseTerms(s.tdm,0.7)
  result <-list(name= cate,tdm = s.tdm)
}
tdm = lapply(cat,generateTDM,path = pathname)

# Attach Final Result Column
bindCategoryTDM <- function(tdm) {
  s.mat = t(data.matrix(tdm[["tdm"]]))
  s.df = as.data.frame(s.mat,stringsAsFactors = F)
  s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "FinalResult"
  return(s.df)
  
}

#Finding row numbers of Rejects and Accepts
catTDM = lapply(tdm,bindCategoryTDM)
wq <- which(essaydata$FinalResult=='Reject')
wq1 <- which(essaydata$FinalResult =='Accept')

#Adding a new column with Final results into the matrix
catTDM1 <- as.list(catTDM)
catTDM1[[1]][[32]][wq] <- 'Reject'
catTDM1[[1]][[32]][wq1] <- 'Accept'

lapply(catTDM, function(x) { x["FinalResult"] <- NULL; x })

#Stack 
tdm.stack = do.call(rbind.fill,catTDM2)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.70))
text.idx = (1:nrow(tdm.stack))[-train.idx]

#Fitting Model
tdm.cat = tdm.stack[,"FinalResult"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "FinalResult"]
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx],k = 7)

#accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat))/length(text.idx) *100
accuracy
conf.mat 

#Removing unnecessary terms like a, the, can, will etc.
catTDM2 <- catTDM1
catTDM2[[1]][[1]] <- NULL
catTDM2[[1]][[1]] <- NULL
catTDM2[[1]][[1]] <- NULL
catTDM2[[1]][[4]] <- NULL
catTDM2[[1]][[7]] <- NULL
catTDM2[[1]][[24]] <- NULL
catTDM2[[1]][[5]] <- NULL
View(catTDM2)

#---------------------------------------Second Part-------------------------------------#
#Using Naive Bayes Algorithm

#Loading necessary package
library(e1071)

#Importing dataset
catTDM2 <- as.data.frame(catTDM1)
catTDM2$FinalResult <- as.factor(catTDM2$FinalResult)

#Selecting 70% of data randomly and training
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.70))

#Selecting test data
text.idx = (1:nrow(tdm.stack))[-train.idx]
traindata <- cbind(tdm.stack,catTDM2$FinalResult[1:1450])
textdata <- cbind(tdm.stack.nl[text.idx,],catTDM2$FinalResult[text.idx])


#Using Naiva Bayes algorithm
nb <- naiveBayes(traindata$`catTDM2$FinalResult[1:1450]`~.,data=traindata)
predict(nb,textdata)
predict(nb, textdata, type="raw")
pred <- predict(nb, newdata=textdata)

#Code to increase accuracy using tuning method (Failed)
tune.control <- tune.control(random = FALSE, nrepeat = 1, repeat.aggregate = min,sampling = c("cross"), sampling.aggregate = mean, cross = 10, best.model = TRUE, performances = TRUE)
tune(naiveBayes,train.x = traindata$`catTDM3$FinalResult[1:1450]`~.,  predict.fun=pred, tune.control)
table(pred)
conf.mat = table("predictions" = pred,"Actual" = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat))/length(text.idx) *100
accuracy
actreject <- which(tdm.cat[text.idx]=='Reject')
falsepositive <- sum(which(pred[actreject]=='Accept')>0)
percentfp <- falsepositive/length(actreject)*100
overallfbper <- falsepositive/435*100
overallfbper

#-----------------------------------------------Third Part-------------------------------#
#KNN Prediction Model for Fit Essays
essays <- essaydata$Fit.With.TFI.Essay
write.csv(essays,file = 'essays.csv')

#Importing necessary packages
library(tm)
library(plyr)
library(class)
libs=c("tm","plyr","class")
lapply(libs,require,character.only = T)
options(stringsAsFactors= F)
cat = c("essays.csv")
pathname = "C:/Users/Divyansh/Documents"

#Function to clean the document
cleanCorpus <-function(corpus) {
  corpus.tmp = tm_map(corpus,removePunctuation)
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp = tm_map(corpus.tmp,tolower)
  corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
  corpus.tmp = tm_map(corpus.tmp,stemDocument)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  return(corpus.tmp)
}
#Funciton to create the matrix
generateTDM <- function(cate,path) {
  s.path =  sprintf("%s/%s",path,cate)
  csv = read.csv(s.path)
  s.cor = Corpus(DataframeSource(csv))
  s.cor.cl = cleanCorpus(s.cor)
  s.tdm= TermDocumentMatrix(s.cor.cl)
  s.tdm = removeSparseTerms(s.tdm,0.7)
  result <-list(name= cate,tdm = s.tdm)
}
tdm = lapply(cat,generateTDM,path = pathname)

# attach name
bindCategoryTDM <- function(tdm) {
  s.mat = t(data.matrix(tdm[["tdm"]]))
  s.df = as.data.frame(s.mat,stringsAsFactors = F)
  s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "FinalResult"
  return(s.df)
  
}

catTDM = lapply(tdm,bindCategoryTDM)
wq <- which(essaydata$FinalResult=='Reject')
wq1 <- which(essaydata$FinalResult =='Accept')

catTDM1 <- as.list(catTDM)
catTDM1[[1]][[64]][wq] <- 'Reject'
catTDM1[[1]][[64]][wq1] <- 'Accept'

lapply(catTDM, function(x) { x["FinalResult"] <- NULL; x })

#Stack 
tdm.stack = do.call(rbind.fill,catTDM1)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.70))
text.idx = (1:nrow(tdm.stack))[-train.idx]

#model
tdm.cat = tdm.stack[,"FinalResult"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "FinalResult"]
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx],k = 7)

#accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat))/length(text.idx) *100
accuracy
conf.mat 

#Removing unnecessary terms from the matrix like a, the, will, also etc.
catTDM2 <- catTDM1
catTDM2[[1]][[3]] <- NULL
catTDM2[[1]][[7]] <- NULL
catTDM2[[1]][[30]] <- NULL
catTDM2[[1]][[64]][wq] <- 'Reject'
catTDM2[[1]][[64]][wq1] <- 'Accept'
catTDM2[[1]][[59]] <- NULL
catTDM2[[1]][[56]] <- NULL

#Stack 
tdm.stack = do.call(rbind.fill,catTDM2)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.70))
text.idx = (1:nrow(tdm.stack))[-train.idx]

#model
tdm.cat = tdm.stack[,"FinalResult"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "FinalResult"]
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx],k = 7)

#accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
#Calculating accuracy
accuracy = sum(diag(conf.mat))/length(text.idx) *100
accuracy
conf.mat 

#--------------------------------------------------Fourth Part--------------------------------#
#Combining Grit and Fit Essays
essays2 <- c(essays,essaydata$Fit.With.TFI.Essay)
result <- c(essaydata$FinalResult,essaydata$FinalResult)
essays3 <- cbind(essays2, result)

#Exporting dataset into CSV File.
write.csv(essays2,file = 'essays2.csv')

library(tm)
library(plyr)
library(class)
libs=c("tm","plyr","class")
lapply(libs,require,character.only = T)
options(stringsAsFactors= F)
cat = c("essays2.csv")
pathname = "C:/Users/Divyansh/Documents"
cleanCorpus <-function(corpus) {
  corpus.tmp = tm_map(corpus,removePunctuation)
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp = tm_map(corpus.tmp,tolower)
  corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
  corpus.tmp = tm_map(corpus.tmp,stemDocument)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  return(corpus.tmp)
}
#Funciton to clean the document
generateTDM <- function(cate,path) {
  s.path =  sprintf("%s/%s",path,cate)
  csv = read.csv(s.path)
  s.cor = Corpus(DataframeSource(csv))
  s.cor.cl = cleanCorpus(s.cor)
  s.tdm= TermDocumentMatrix(s.cor.cl)
  s.tdm = removeSparseTerms(s.tdm,0.7)
  result <-list(name= cate,tdm = s.tdm)
}
#Generating TD Matrix
tdm = lapply(cat,generateTDM,path = pathname)

# attach Final Result
bindCategoryTDM <- function(tdm) {
  s.mat = t(data.matrix(tdm[["tdm"]]))
  s.df = as.data.frame(s.mat,stringsAsFactors = F)
  s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "FinalResult"
  return(s.df)
  
}

catTDM = lapply(tdm,bindCategoryTDM)
essays3 <- as.data.frame(essays3)
wq <- which(essays3$result=='Reject')
wq1 <- which(essays3$result =='Accept')

catTDM1 <- as.list(catTDM)
catTDM1[[1]][[67]][wq] <- 'Reject'
catTDM1[[1]][[67]][wq1] <- 'Accept'

lapply(catTDM, function(x) { x["FinalResult"] <- NULL; x })

#Stack 
tdm.stack = do.call(rbind.fill,catTDM1)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.70))
text.idx = (1:nrow(tdm.stack))[-train.idx]

#Making Model
tdm.cat = tdm.stack[,"FinalResult"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "FinalResult"]
#Using KNN Algorithm on the train, test data to predict
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx],k = 7)

#Finding Accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat))/length(text.idx) *100
accuracy
conf.mat 

#-----------------------------------------------END OF PROJECT 2-------------------------------------#
