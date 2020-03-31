#Reading Data
library(readr)
winedataset <- read_csv("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/wine.data")
str(winedataset)
trainData
#Splitting Data into Training and Testing Dataset
library(caTools)

s <-sample.split(winedataset$Category, SplitRatio = 0.82)
t1<-cbind(winedataset,s=s)
trainData<-subset(t1,s==TRUE)[1:14]
testData<-subset(t1,s==FALSE)[1:14]
library(MASS)
trainData.lda<-lda(Category ~.,trainData)
trainData.lda
trainData.ldascores<-predict(trainData.lda)

plot(trainData.lda)
dat<-cbind.data.frame(y=trainData[,1],LD1=trainData.ldascores$x[,1],LD2=trainData.ldascores$x[,2])

testData.ldascores <- predict(trainData.lda,testData)
testData.ldascores
data1<-cbind.data.frame(y=testData[,1],LD1=testData.ldascores$x[,1],LD2=testData.ldascores$x[,2])

library(e1071)
svm1 <- svm(y ~ .,data=dat, type="C-classification", kernel="radial", gamma=0.1, cost=10)
svm1$SV
plot(svm1,dat,LD2~LD1)
prediction<-predict(svm1,data1)
xtab<-table(data1$y,prediction)
xtab
sum(diag(xtab))/nrow(data1)




