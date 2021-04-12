data=read.csv(file="C:\\Users\\gt\\Desktop\\varicelle.csv")
plot(data$x)
#time series creation using ts 
varicelle<-ts(data$x,start=c(1931,1),end=c(1972,6),freq=12)
plot(varicelle)
library(forecast)
library(ggplot2)
#to check the size of the seasonality use ggplot
ggseasonplot(varicelle,year.labels=TRUE,year.labels.left=TRUE)
ggseasonplot(varicelle,polar=TRUE)
mean(varicelle)
var(varicelle)
#auto correlation function 
tmp=acf(varicelle,type='cor',plot=FALSE)
plot(tmp)
#partial auto correlation function
tmp=pacf(varicelle,type='cor',plot=FALSE)
plot(tmp)

aggserie=aggregate(varicelle)
plot(aggserie)


#Forecasting with SES 
LES=HoltWinters(varicelle,alpha=NULL,beta=FALSE,gamma=FALSE)
plot(varicelle)
p=predict(LES,n.ahead=12)
lines(p,col=2)
#Forcasting with SES using the SES function from the Forecast library 
sesfor=ses(varicelle,h=12)
autoplot(sesfor)

#Forecasting with Non-seasonal HoltWinters 
LES2=HoltWinters(varicelle,alpha=NULL,beta=NULL,gamma=FALSE)
P2=predict(LES2,n.ahead=12)
plot(varicelle,xlim=c(1930,1980))
lines(P2,col=2)
#Forecasting using Non-seasonal HoltWinters using the FORECAST library 
HOLT=holt(varicelle,h=12)
autoplot(HOLT)
#Using the damped option to lessen the linear trend in Non-seasonal Holt winters 
HOLT2=holt(varicelle,damped=TRUE,phi=0.9,h=12)
autoplot(HOLT2)


#comparing SES with HW and Damped HW using tsCV (cross-validation)
install.packages('fpp')
library(fpp)
data(livestock)
autoplot(livestock)
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)

#compare the mean squared error 
mean(e1^2,na.rm=TRUE)
mean(e2^2,na.rm=TRUE)
mean(e3^2,na.rm=TRUE)
#Forecasting with the best one (Damped HW)
HOLT3=holt(livestock,damped=TRUE,h=12)
autoplot(HOLT3)

#************

#splitting my varicelle data into learn and test to avoid overfitting 
learning=window(varicelle,start=c(1930,1),end=c(1960,12))
test=window(varicelle,start=c(1961,1))
test


#Forecasting using the Additive seasonal HoltWinters 
LES4=HoltWinters(learning,alpha=NULL,beta=NULL,gamma=NULL)
plot(learning)
p4=predict(LES4,n.ahead=12)
lines(p4,col=2)

#forecasting using the additive seasonal HW from the Forecast library
fit1=hw(learning,seasonal = 'additive',h=12)
autoplot(fit1)

#Forecasting using the Multiplicative Holt Winters 
LES5=HoltWinters(learning,alpha=NULL,beta=NULL,gamma=NULL,seasonal = 'multi')
plot(learning)
p5=predict(LES5,n.ahead=12)
lines(p5,col=2)

#Forecasting using the Multiplicative  seasonal HoltWinters from the Forecast library 
fit2=hw(learning,seasonal = 'multiplicative',h=12)
autoplot(fit2)


#comparing fit1 (additive) with fit2 (multiplicative) using the rmse
print(sqrt(mean((fit1$mean-test)^2)))
print(sqrt(mean((fit2$mean-test)^2))) #the multiplicative seems best 














