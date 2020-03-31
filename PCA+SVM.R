#Reading Data
library(readr)
winedataset <- read_csv("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/wine.data")
str(winedataset)

#Splitting Data into Training and Testing Dataset
library(caTools)
set.seed(1)
s <-sample.split(winedataset$Category, SplitRatio = 0.8)
t1<-cbind(winedataset,s=s)
trainData<-subset(t1,s==TRUE)[1:14]
testData<-subset(t1,s==FALSE)[1:14]

#Applying PCA
d01<-trainData[,2:14]
d01.pca<-princomp(d01,cor=TRUE, score=TRUE)
summary(d01.pca)
d01.pca

plot(d01.pca)
d01.pca$loadings
d01.pca$scores
plot(d01.pca$scores[,1:2])

d02<-testData[,2:14]
d02.pca<-princomp(d02,cor=TRUE, score=TRUE)
summary(d02.pca)

plot(d02.pca)
d02.pca$loadings
d02.pca$scores
plot(d02.pca$scores[,1:2])
#Using SVM
library(e1071)

library(ggplot2)
dat<-cbind.data.frame(y=trainData[,1],d01.pca$scores[,1:2])
dat1<-cbind.data.frame(y=testData[,1],d02.pca$scores[,1:2])


qplot(Comp.1, Comp.2, data =dat , color = factor(y)) + geom_point(shape = 1) +scale_colour_manual(values = c("#0000FF", "#00FF00","#FF0000"), labels = c("1", "2","3"))
svm1 <- svm(y ~ .,data=dat, type="C-classification", kernel="radial", gamma=0.1, cost=10)
svm1$SV
plot(svm1,dat,Comp.2~Comp.1)
prediction<-predict(svm1,dat1)
xtab<-table(dat1$y,prediction)
xtab
sum(diag(xtab))/nrow(dat1)
class(xtab)
class(svm1)

