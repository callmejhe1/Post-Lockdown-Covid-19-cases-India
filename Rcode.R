library(ggplot2)
library(caret)
#creating training and test set
z<-read.csv("C:/Users/Jayesh/Desktop/Covid data/India-Manuscript/Post Lock down/India1.csv")

inTrain<-createDataPartition(y=z$new_cases,p=0.7,list=FALSE)
training<-z[inTrain,];testing<-z[-inTrain,]

#dimensions of traning and test data
dim(training);dim(testing)
featurePlot(x=training[,c("new_cases","new_deaths","new_tests")],y=training$new_cases,plot="pairs")
par(mfrow=c(1, 1))
qplot(new_tests,new_cases,data=training)

qplot(date,new_cases,colour=new_tests,data=training)
qplot(date,new_tests,colour=new_cases,data=training)

modFit<-train(new_tests~new_cases,method="lm",data=training)
finMod<-modFit$finalModel
print(modFit)

plot(finMod,1,pch=19,cex=0.5,col="#00000010")

qplot(finMod$fitted,finMod$residuals,colour=new_cases,data=training)
qplot(finMod$residuals)

pred<-predict(modFit,testing)
qplot(new_tests,pred,colour=new_cases,data=testing)

qplot(new_cases,pred,colour=new_tests,data=testing)







