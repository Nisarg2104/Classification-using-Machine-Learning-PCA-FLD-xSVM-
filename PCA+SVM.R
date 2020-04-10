#Reading Data
library(readr)
winedataset <- read_csv("C:/Users/Khushi/Desktop/Studies 2-2/Machine Learning Assignment/wine.data")
str(winedataset)

#Splitting Data into Training and Testing Dataset
library(caTools)
set.seed(1)
s <-sample.split(winedataset$Category, SplitRatio = 0.82)
t1<-cbind(winedataset,s=s)
trainData<-subset(t1,s==TRUE)[1:14]

mean(trainData1$Proline)

trainData[2:14]= as.data.frame(scale(trainData[2:14],center=TRUE, scale = TRUE))
#for(i in c(2:14)){
 # m1<-mean(trainData[,i])
  #for(j in c(1:nrow(trainData)))
 # {
#    k<-(trainData[j,i]-m1)
 #   trainData[j,i]<-k
#  }
#}

m<-cov(trainData[,2:14])

ev<-eigen(m)
values<-ev$values
vectors<-ev$vectors

l<-matrix(1:(nrow(trainData)*2),nrow=nrow(trainData))

for(i in c(1:nrow(trainData))){
  sum<-0
  for(j in c(2:14)){
    sum<-(sum+vectors[j-1,2]*trainData[i,j])
  }
  l[i,2] <- sum
}
for(i in c(1:nrow(trainData))){
  sum<-0
  for(j in c(2:14)){
    sum<-(sum+vectors[j-1,1]*trainData[i,j])
  }
  l[i,1] <- sum
}


plot(l)
testData<-subset(t1,s==FALSE)[1:14]
testData[2:14]= as.data.frame(scale(testData[2:14],center=TRUE, scale = TRUE))

lt<-matrix(1:(nrow(testData)*2),nrow=nrow(testData))

for(i in c(1:nrow(testData))){
  sum<-0
  for(j in c(2:14)){
    sum<-(sum+vectors[j-1,2]*testData[i,j])
  }
  lt[i,2] <- sum
}
for(i in c(1:nrow(testData))){
  sum<-0
  for(j in c(2:14)){
    sum<-(sum+vectors[j-1,1]*testData[i,j])
  }
  lt[i,1] <- sum
}



#Using SVM
library(e1071)

library(ggplot2)
d<-data.frame(Comp.1=l[,1],Comp.2=l[,2])
dt<-data.frame(Comp.1=lt[,1],Comp.2=lt[,2])
e<-as.factor(trainData[,1])
dat<-cbind.data.frame(y=e,d)
dat1<-cbind.data.frame(y=testData[,1],dt)



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


