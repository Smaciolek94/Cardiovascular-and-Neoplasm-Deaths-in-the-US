######STEFAN MACIOLEK - 19:690:565 - TIME SERIES ANALYSIS - CODE

#IMPORT SECTION
#In order for this code to be reproducible, these will need to be updated to the locations 
#of the saved data files.  Nothing else in the code will need to be updated
library(readxl)
cmonth <- read_xlsx("C:\\Users\\Stvma\\Documents\\GitHub\\TSA-final-project\\Final Files\\Stefan Maciolek - Final Project Data.xlsx",sheet=2)
nmonth <- read_xlsx("C:\\Users\\Stvma\\Documents\\GitHub\\TSA-final-project\\Final Files\\Stefan Maciolek - Final Project Data.xlsx",sheet=4)
cyear <- read_xlsx("C:\\Users\\Stvma\\Documents\\GitHub\\TSA-final-project\\Final Files\\Stefan Maciolek - Final Project Data.xlsx",sheet=1)
nyear <-  read_xlsx("C:\\Users\\Stvma\\Documents\\GitHub\\TSA-final-project\\Final Files\\Stefan Maciolek - Final Project Data.xlsx",sheet=3)

#DATA PRE-PROCESSING
#adding the population to the monthly files
cmonth$pop <- rep(0,240)
nmonth$pop <- rep(0,240)
for (i in 1:240){
  index <- (i%/%12) + 1
  cmonth$pop[i] <- cyear$Population[index]
  nmonth$pop[i] <- nyear$Population[index]
}

cyear$prop <- cyear$Deaths / cyear$Population * 100
nyear$prop <- nyear$Deaths / nyear$Population * 100

#PRELIMINARY ANALYSIS
#plotting deaths by year
plot(cyear$Year,(cyear$Deaths/1000),type="o",main="Deaths Per Year",
      xlab="Year",ylab="Deaths x 1000",ylim=c(0,1000),col="blue",xaxs="i",yaxs="i")
lines(nyear$Year,(nyear$Deaths/1000),,type="o",col="red")
grid(col="black",nx=19,ny=10)
legend("bottomleft",legend=c("Cardiovascular Issues","Neoplasm"),col=c("blue","red"),
       pch=c(1,1))

#plotting proportion dead by year
plot(cyear$Year,cyear$prop,type="o",main="Percent Population Death per Year",
      xlab="Year",ylab="% of Population Dying per Year",ylim=c(0,.35),col="blue",xaxs="i",yaxs="i")
lines(nyear$Year,nyear$prop,type="o",col="red")
grid(col="black",nx=19,ny=10)
legend("bottomleft",legend=c("Cardiovascular Issues","Neoplasm"),col=c("blue","red"),
       pch=c(1,1))

cmonth$prop <- cmonth$Deaths / cmonth$pop * 100
nmonth$prop <- nmonth$Deaths / nmonth$pop * 100
cmonth$month <- seq(as.Date("1999/1/1"),as.Date("2018/12/1"),by="month")
nmonth$month <- seq(as.Date("1999/1/1"),as.Date("2018/12/1"),by="month")

#proportion dying per month
plot(cmonth$month,cmonth$prop,type="l",xlab="Date",ylab="% of Population Dying per Month",
     main="Percent Population Death per Month",ylim=c(0,.036),col="blue",xaxs="i",yaxs="i")
lines(nmonth$month,nmonth$prop,type="l",col="red")
grid(col="black",nx=20,ny=10)
legend("bottomleft",legend=c("Cardiovascular Issues","Neoplasm"),col=c("blue","red"),
       pch=c(1,1))

#CARDIOVASCULAR ANALYSIS
library(TSA)
cv<- ts(data<-cmonth$prop,start=c(1999,1),end=c(2018,12),frequency=12)

monthlab <- c("J","F","M","A","M","J","J","A","S","O","N","D")

plot(cv,type="l",xlab="Date",ylab="Percent",
     main = "Percent Population Death",xaxs="i")
points(cv,pch=monthlab)

cdiff <- diff(cv)
plot(cdiff,type="l",xlab="Date",ylab="Percent",
     main = "First Difference Percent Population Death",xaxs="i",)
points(cdiff,pch=monthlab)

scdiff <- diff(cdiff,12)
plot(scdiff,type="l",xlab="Date",ylab="Percent",
     main = "First and Sesaonal Differences Percent Population Death",xaxs="i")
points(scdiff,pch=monthlab)

#acf
acf(na.omit(as.vector(scdiff)),main="Autocorrelation",lag.max=40)

#pacf
pacf(na.omit(as.vector(scdiff)),main="Partial Autocorreleation", lag.max=40)

#spectral
periodogram(na.omit(as.vector(scdiff)),main="Periodogram")

#*not every model that was tested appears here
cvmodel <- arima(cv, order=c(0,1,1),seasonal=list(order=c(0,0,1),period=12))
cvmodel
cvmodel2 <- arima(cv, order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))
cvmodel2
cvmodel3 <- arima(cv, order=c(0,1,3),seasonal=list(order=c(0,1,1),period=12))
cvmodel3
cvmodel4 <- arima(cv, order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12))
cvmodel4 #this is the first one without a significant paramter

cvmodel32 <- arima(cv, order=c(0,1,3),seasonal=list(order=c(0,1,2),period=12))
cvmodel32
cvmodel33 <- arima(cv, order=c(0,1,3),seasonal=list(order=c(0,1,3),period=12))
cvmodel33
cvmodel34 <- arima(cv, order=c(0,1,3),seasonal=list(order=c(0,1,4),period=12))
cvmodel34 #this model was the first seasonal one without a significant parameter

ar1cvmodel33 <- arima(cv, order=c(1,1,3),seasonal=list(order=c(0,1,3),period=12))
ar1cvmodel33 
ar2cvmodel33 <- arima(cv, order=c(2,1,3),seasonal=list(order=c(0,1,3),period=12))
ar2cvmodel33 #this model was the first AR one without a significant parameter

#The ARIMA(1,1,3) x (0,1,3) is the final model 

#diagnostics
plot(ar1cvmodel33$residuals,main="Residual plot, Final Model",ylab="Residuals")
acf(as.vector(na.omit(ar1cvmodel33$residuals)),main="Autocorrelation of Residuals, Final Model",
      lag.max=40)
qqnorm(residuals(ar1cvmodel33),main="QQ Plot of Residuals, Final Model",
        ylab="Theoretical Quantiles")
qqline(residuals(ar1cvmodel33))
hist(ar1cvmodel33$residuals,main="Histogram of Residuals",xlab="Residuals")
#shapiro-wilk test not used here due to large sample size

#predicted plots
cvpredict <- predict(ar1cvmodel33,n.ahead = 60)
cvout<-ts(cvpredict$pred,start=c(2019,1),end=c(2023,12),frequency = 12)
cvupper<-ts(cvpredict$pred + 1.96 * cvpredict$se,
            start=c(2019,1),end=c(2023,12),frequency = 12)
cvlower<-ts(cvpredict$pred - 1.96 * cvpredict$se,
            start=c(2019,1),end=c(2023,12),frequency = 12)
plot(cvout,type="o",ylim=c(.01,.03))
lines(cvupper,type="l",col="red")
lines(cvlower,type="l",col="red")

#NEOPLASM ANALYSIS
np <- ts(data=nmonth$prop,start=c(1999,1),end=c(2018,12),frequency=12)

plot(np,type="l",xlab="Date",ylab="Percent",
     main = "Percent Population Death",xaxs="i")
points(np,pch=monthlab)
ndiff <- diff(np,12)
plot(ndiff,type="l",xlab="Date",ylab="Percent",
     main = "Seasonal Difference Percent Population Death",xaxs="i")
points(ndiff,pch=monthlab)

#acf
acf(na.omit(as.vector(ndiff)),main="Autocorrelation",lag.max=40)

#pacf
pacf(na.omit(as.vector(ndiff)),main="Partial Autocorreleation", lag.max=40)

#spectral
periodogram(na.omit(as.vector(ndiff)),main="Periodogram")

npmodel <- arima(np, order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
npmodel
npmodel2 <- arima(np, order=c(0,0,4),seasonal=list(order=c(0,1,1),period=12))
npmodel2
npmodel2 <- arima(np, order=c(0,0,4),seasonal=list(order=c(0,1,2),period=12))
npmodel2
npmodel3 <- arima(np, order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12))
npmodel3
#final model is an ARIMA(1,0,1) X (0,1,1)

plot(npmodel3$residuals,main="Residual Plot, Final Model",ylab="Residuals")
acf(as.vector(na.omit(npmodel3$residuals)),main="Autocorrelation of Residuals, Final Model",
    lag.max=40)
qqnorm(residuals(npmodel3),main="QQ Plot of Residuals, Final Model",
       ylab="Theoretical Quantiles")
qqline(residuals(npmodel3))
hist(npmodel3$residuals,main="Histogram of Residuals, Final Model",xlab="Residuals")

#predicted plots
nppredict <- predict(npmodel3,n.ahead = 60)
npout<-ts(nppredict$pred,start=c(2019,1),end=c(2023,12),frequency = 12)
npupper<-ts(nppredict$pred + 1.96 * nppredict$se,
            start=c(2019,1),end=c(2023,12),frequency = 12)
nplower<-ts(nppredict$pred - 1.96 * nppredict$se,
            start=c(2019,1),end=c(2023,12),frequency = 12)
plot(npout,type="o",ylim=c(.01,.03))
lines(npupper,type="l",col="red")
lines(nplower,type="l",col="red")

#FINAL PREDICTED PLOTS
plot(cv,type="l",xlim=c(1999,2024),ylim=c(.01,.035),col="blue",
     main = "Predicted Monthly Proportion of Cardiovascular and Neoplasm Deaths",
      ylab="% of Population Dying per Month",xlab="Deaths",xaxs="i")
lines(np,type="l",col="red",xaxs="i")
lines(cvout,type="l",col="black",xaxs="i")
lines(cvupper,type="l",col="blue",xaxs="i")
lines(cvlower,type="l",col="blue",xaxs="i")
lines(npout,type="l",col="black",xaxs="i")
lines(npupper,type="l",col="red",xaxs="i")
lines(nplower,type="l",col="red",xaxs="i")
grid(col="black",nx=25,ny=10)
legend("bottomleft",legend=c("Cardiovascular Issues","Neoplasm"),col=c("blue","red"),
       pch=c(1,1))