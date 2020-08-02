library(ggplot2)
z<-read.csv("C:/Users/Jayesh/Desktop/Covid data/India-Manuscript/Post Lock down/India1.csv")




r<-lm(tpm~cpm, data=z)
print(r)
summary(r)

par(mfrow=c(1, 1))

scatter.smooth(x=z$tpm, y=z$cpm,xlab="Test per Million", ylab="case per million") 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(z$cpm, main="case per mn", sub=paste("Outlier rows: ", boxplot.stats(z$cpm)$out))  # box plot for 'speed'
boxplot(z$tpm, main="test per mn", sub=paste("Outlier rows: ", boxplot.stats(z$tpm)$out))  # box plot for 'distance'

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(z$tpm), main="Density Plot: test per mn", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(z$tpm), 2)))  # density plot for 'speed'
polygon(density(z$tpm), col="grey")
plot(density(z$cpm), main="Density Plot: case per mn", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(z$cpm), 2)))  # density plot for 'dist'
polygon(density(z$cpm), col="grey")

