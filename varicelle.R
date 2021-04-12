
#installing required packages
install.packages("forecast")
install.packages("ggplot2")
install.packages('fpp2')

#calling the libraries
library(forecast)
library(ggplot2)

#Load this data set
A=read.csv("C:\\Users\\gt\\Desktop\\varicelle.csv")
A

#build a time series object
varicelle<-ts(A$x,start=c(1931,1),end=c(1972,6),freq=12)
plot(varicelle)

#the mean mensual number of varicella
mean(varicelle)

#the auto correlation and plotting the correlogram
tmp=acf(varicelle,type="cor",plot = FALSE)
plot(tmp)

#Plot the seasonal plot
ggseasonplot(varicelle,year.labels= TRUE,year.labels.left=TRUE)

#Plot the polar seasonal plot
ggseasonplot(varicelle,polar=TRUE)


head(varicelle,12)


#Compute the annual numbers of varicella and plot them from 1931 to 1972.
help(aggregate)
b=aggregate(varicelle,frequency(1))
plot(b)


