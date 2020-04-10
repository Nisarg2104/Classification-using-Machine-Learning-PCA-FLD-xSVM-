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
trainData[2:14]= as.data.frame(scale(trainData[2:14],center=TRUE, scale = TRUE))


testData<-subset(t1,s==FALSE)[1:14]
testData[2:14]= as.data.frame(scale(testData[2:14],center=TRUE, scale = TRUE))


class1<-as.matrix(subset(trainData,Category==1)[2:14])
class2<-as.matrix(subset(trainData,Category==2)[2:14])
class3<-as.matrix(subset(trainData,Category==3)[2:14])
class<-as.matrix(trainData[2:14])
m1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for(i in c(1:nrow(class1))){
  m1=m1+class1[i,]
}
m1<-m1/nrow(class1)
m2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for(i in c(1:nrow(class2))){
  m2=m2+class2[i,]
}
m2<-m2/nrow(class2)
m3<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for(i in c(1:nrow(class3))){
  m3=m3+class3[i,]
}
m3<-m3/nrow(class3)

m<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for(i in c(1:nrow(class))){
  m=m+class[i,]
}
m<-m/nrow(class)


for(i in c(1:nrow(class1))){
  class1[i,]=class1[i,]-m1
}

for(i in c(1:nrow(class2))){
  class2[i,]=class2[i,]-m2
}

for(i in c(1:nrow(class3))){
  class3[i,]=class3[i,]-m3
}


sw1=0
for(i in c(1:nrow(class1))){
  sw1=sw1+((class1[i,]-m1)%*%t((class1[i,]-m1)))
}

sw2=0
for(i in c(1:nrow(class2))){
  sw2=sw2+((class2[i,]-m2)%*%t((class2[i,]-m2)))
}

sw3=0
for(i in c(1:nrow(class3))){
  sw3=sw3+((class3[i,]-m3)%*%t((class3[i,]-m3)))
}

sw=sw1+sw2+sw3

sb1=(nrow(class1)*((m1-m)%*%t((m1-m))))
sb2=(nrow(class2)*((m2-m)%*%t((m2-m))))
sb3=(nrow(class3)*((m3-m)%*%t((m3-m))))

sb=(sb1+sb2+sb3)
library(matlib)
swI<-inv(sw)
a<-swI%*%sb
b<-eigen(a)
vectors<-b$vectors
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
plot(l[,2]~l[,1])
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


dat<-cbind.data.frame(y=trainData[,1],data.frame(LD1=as.numeric(l[,1]),LD2=as.numeric(l[,2])))


data1<-cbind.data.frame(y=testData[,1],data.frame(LD1=as.numeric(lt[,1]),LD2=as.numeric(lt[,2])))

library(e1071)
svm1 <- svm(y ~ .,data=dat, type="C-classification", kernel="radial", gamma=0.1, cost=10)
svm1$SV
plot(svm1,dat,LD2~LD1)
prediction<-predict(svm1,data1)
xtab<-table(data1$y,prediction)
xtab
sum(diag(xtab))/nrow(data1)




