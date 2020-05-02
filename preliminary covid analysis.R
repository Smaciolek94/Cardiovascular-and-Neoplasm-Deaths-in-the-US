#test to see if TSA works for Covid data:
uscases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
usdeaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
uscases <- uscases[,-c(1:10)]
usdeaths <- usdeaths[,-c(1:11)]

n <- ncol(uscases)
date <- 1:n
date <- as.Date(date,origin = "2020-01-21")
format(date,format = "%b %d %y")

totcases <- colSums(uscases[2:n])
totdeaths <- colSums(usdeaths[2:n])
newcases <- diff(totcases)
newdeaths <- diff(totdeaths)

plot(date[3:n],newcases,type="o")
plot(date[3:n],log(newcases),type="o")
plot(date[4:n],diff(log(newcases)),type="o")
plot(date[5:n],diff(diff(log(newcases))),type="o")

plot(date[4:n],diff(newcases),type="o")
plot(date[4:n],log(diff(newcases)),type="o")

library(TSA)
arima(log(newcases),model=c(1,0,0))

