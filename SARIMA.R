data=scan(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/sanfran.dat",skip=1)
sanfran<-ts(data,start=c(1932,1),end=c(1966,12),freq=12)
autoplot(sanfran)
acf(sanfran)
pacf(sanfran)


sanfran_train=window(sanfran,,start=c(1932,1),end=c(1963,12))
sanfran_test=window(sanfran,,start=c(1964,1),end=c(1966,12))



diffsanfran=diff(sanfran_train,lag=12)
autoplot(diffsanfran)
acf(diffsanfran)
pacf(diffsanfran)

library(forecast)
par(mfrow=c(2,1))

#building the SARIMA MODEL 
seasonsanfranauto=auto.arima(sanfran_train)
seasonsanfranauto

seasonsanfran=arima(sanfran_train,order=c(0,1,1),seasonal = c(1,1,1))
seasonsanfran
#forecasting using the SARIMA model and plotting 
y=forecast(seasonsanfran,h=12*3)
autoplot(sanfran_train)+autolayer(y,series='forecast')+autolayer(sanfran_test,series='true data')

#look at the risuals after the  SARIMA 
checkresiduals(seasonsanfran)

seasonsanfran %>% residuals() %>% ggtsdisplay()




print(sqrt(mean((y$mean-sanfran_test)^2)))






#*********************************************************
#Load this data set
varicelle=read.csv("C:\\Users\\gt\\Desktop\\varicelle.csv")
varicelle


#build a time series object
varicelle<-ts(A$x,start=c(1931,1),end=c(1972,6),freq=24)
plot(varicelle)
acf(varicelle)
pacf(varicelle)
#splitting my varicelle data into learn and test to avoid overfitting (subsitting a time serie)
learning=window(varicelle,start=c(1931,1),end=c(1970,12))
test=window(varicelle,start=c(1971,1),end=c(1972,6))


#try the auto.arima 
auto=auto.arima(learning,lambda='auto')
auto
#forecast with auto.arima 
yhat=forecast(auto,h=18)

#RMSE for auto.arima 

print(sqrt(mean((yhat$mean-test)^2)))

#diff to reach stationary 
varicelledf=diff(learning,lag=12,differences = 1)
plot(varicelledf)
acf(varicelledf)
pacf(varicelledf)


order=c(1,0,0),seasonal = c(0,1,1))


#build SARIMA 
seasonsanvaricelle=arima(learning,order=c(0,0,25),seasonal = c(0,1,1))
seasonsanvaricelle

#forecasting with SARIMA 
yhat2=forecast(seasonsanvaricelle,h=18)
#check residuals OF SARIMA
checkresiduals(seasonsanvaricelle)
#make sure that the residuals are white noise 
Box.test(seasonsanvaricelle$residuals,lag=10,type='Ljung-Box')

print(sqrt(mean((yhat2$mean-test)^2)))


BoxCox.lambda(ts)





