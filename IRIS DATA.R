#loading the data
data("iris")
X=iris$Sepal.Length
#plotting a histogram
hist(X,breaks=50,freq =FALSE)
lines(density(X),col=2,lwd=2)
#plotting a box plot
boxplot(X,horizontal = TRUE)
#scatter plot
plot(iris[,-5])
#correlation table
cor(iris[,-5])
#covariance table
cov(iris[,-5])
#scatter plot adding the category as a color 
plot(iris[,-5],col=as.numeric(iris$Species))
#calculating the mean of each column 
colMeans(iris[,-5])
#supervised classification
#LDA
library(MASS)
x=iris[,-5]
y=as.numeric(iris$Species)
f=lda(x,y)
f
#finding the number of observations 
n=nrow(x)
n
#splitting the data into learning and validation 
learn=sample(1:n,2/3*n)
#fitting the data to the model (learning)
f=lda(x[learn],y[learn])
f
#predicting using the lda model and the validation data 
ystar=predict(f,x[-learn,])
ystar
#calculating the error between the y validation and the y predicted by the LDA 
error=sum(ystar$class!=y[-learn]/length(ystar$class))
error

for (i in 1:10) {
 learn=sample(1:n,2/3*n) 
 f=lda(x[learn,],y[learn])
 ystar=predict(f,x[-learn,])
 err=sum(ystar$class!=y[-learn])/length(ystar$class)
 err
}

boxplot(err)







