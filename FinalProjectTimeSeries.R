#IST687 Project

library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

dfRaw <- read.csv("time_series_19-covid-Confirmed.csv")
#Code for China for Test of Concept
#dfChina <- dfRaw[dfRaw$Country.Region == "Mainland China",]
#dfChinaTotal <- data.frame(Province.State="Total",
#                          Country.Region="Mainland China", 
#                          t(colSums(dfChina[,-c(1,2)])))

create_dfCountries <- function(){
  #List of all Countries
  allCountries <- unique(dfRaw$Country.Region)
  #Create empty dataframe to store results
  dfCountries <- dfRaw[1,]
  dfCountries <- dfCountries[-1,]
  #Iterate through unique countries and add to the dataframe
  for (x in allCountries){
    tempdf <- data.frame(Province.State="Total",
                         Country.Region=x,
                         t(colSums(dfRaw[dfRaw$Country.Region == x,][,-c(1,2)])))
    dfCountries <- rbind(dfCountries, tempdf)
  }
  #Remove Province, Lat/Lon, and Mar.10 data
  dfCountries <- dfCountries[,-c(1,3,4,53)]
  #Remove rows where no cases have been found as of 3/9/2020
  dfCountries <- dfCountries[dfCountries$X3.9.20 > 0,]
  return(dfCountries)
}

dfCountries <- create_dfCountries()
tempNames <-colnames(dfCountries)
noX <- gsub("X","",tempNames)
noX <- gsub("\\.","/",noX)
colnames(dfCountries) <- as.Date(noX, format =  "%m/%d/%y")
RenameA <- colnames(dfCountries)
RenameA[1] <- "Country"
colnames(dfCountries) <- RenameA


## Creating a totals Row
totsRow <-colSums(dfCountries[,-1])
Total.Row <- c('Total',totsRow)
dfCountries <-rbind(dfCountries, Total.Row)

## Time Plot for Italy
library(ggplot2)
library(dplyr)

dfCountry <- dfCountries[which(dfCountries$Country == "Italy"),]
dates <-as.data.frame(colnames(dfCountries))
dfTimeIt <- data.frame(t(dfCountry),dates)
colnames(dfTimeIt) <- c("inf","day")

dfTimeIt <- dfTimeIt[-1,]
dfTime
rownames(dfTimeIt) <- c()
dfTimeIt$inf
dfTimeIt$inf <- as.numeric(levels(dfTimeIt$inf))[dfTimeIt$inf]
dfTimeIt$day <- as.Date(dfTimeIt$day)

str(dfTime)
ItalyTimeSeries <- ggplot(dfTimeIt, aes(y =inf, x = day)) +
  geom_line() + 
  xlab("")
ItalyTimeSeries

## Time Plot for US
library(ggplot2)
library(dplyr)

dfCountry <- dfCountries[which(dfCountries$Country == "US"),]
dates <-as.data.frame(colnames(dfCountries))
dfTimeUS <- data.frame(t(dfCountry),dates)
colnames(dfTimeUS) <- c("inf","day")

dfTimeUS <- dfTimeUS[-1,]
dfTime
rownames(dfTimeUS) <- c()
dfTimeUS$inf
dfTimeUS$inf <- as.numeric(levels(dfTimeUS$inf))[dfTimeUS$inf]
dfTimeUS$day <- as.Date(dfTimeUS$day)

str(dfTimeUS)
USTimeSeries <- ggplot(dfTimeUS, aes(y =inf, x = day)) +
  geom_line() + 
  xlab("")
USTimeSeries

## Time Plot for China
library(ggplot2)
library(dplyr)

dfCountry <- dfCountries[which(dfCountries$Country == "Mainland China"),]
dates <-as.data.frame(colnames(dfCountries))
dfTimeCh <- data.frame(t(dfCountry),dates)
colnames(dfTimeCh) <- c("inf","day")

dfTimeCh <- dfTimeCh[-1,]
dfTimeCh
rownames(dfTimeCh) <- c()
dfTimeCh$inf
dfTimeCh$inf <- as.numeric(levels(dfTimeCh$inf))[dfTimeCh$inf]
dfTimeCh$day <- as.Date(dfTimeCh$day)


ChinaTimeSeries <- ggplot(dfTimeCh, aes(y =inf, x = day)) +
  geom_line() + 
  xlab("")
ChinaTimeSeries

## Time Plot for South Korea
library(ggplot2)
library(dplyr)

dfCountry <- dfCountries[which(dfCountries$Country == "South Korea"),]
dates <-as.data.frame(colnames(dfCountries))
dfTimeSK <- data.frame(t(dfCountry),dates)
dfTimeSK
colnames(dfTimeSK) <- c("inf","day")
dfTimeSK

dfTimeSK <- dfTimeSK[-1,]
rownames(dfTimeSK) <- c()
dfTimeSK
dfTimeSK$inf <- as.numeric(levels(dfTimeSK$inf))[dfTimeSK$inf]
dfTimeSK$day <- as.Date(dfTimeSK$day)
dfTimeSK

KoreaTimeSeries <- ggplot(dfTimeSK, aes(y =inf, x = day)) +
  geom_line() + 
  xlab("")
KoreaTimeSeries

## Plotted all together

TotalPlot = ggplot() + 
  geom_line(data = dfTimeSK, aes(x = day, y = inf), color = "blue") +
  geom_line(data = dfTimeCh, aes(x = day, y = inf), color = "red") +
  geom_line(data = dfTimeUS, aes(x = day, y = inf), color = "green") +
  geom_line(data = dfTimeIt, aes(x = day, y = inf), color = "red") +
  xlab('Day') +
  ylab('Infected')


TotalPlot


## Minus China
NoChplot = ggplot() + 
  geom_line(data = dfTimeSK, aes(x = day, y = inf), color = "blue") +
  geom_line(data = dfTimeUS, aes(x = day, y = inf), color = "green") +
  geom_line(data = dfTimeIt, aes(x = day, y = inf), color = "red") +
  xlab('Day') +
  ylab('Infected')


NoChplot


## Linear Model China
## Suprisingly High R coefficent and low p value


ChinaTimeSeries
chinaLM <- lm(inf ~ day, dfTimeCh)
summary(chinaLM)


ggplotRegression <- function (fit) {
  require(ggplot2)
  
ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(chinaLM)

### Forecasting models


## Naive Model

NaiveChina <- naive(y =dfTimeCh$inf, h=12 )
summary(NaiveChina)

## Smoothing Exponential 
SmoothingChina <- ses(y=dfTimeCh$inf, h = 12)
summary(SmoothingChina)
plot(SmoothingChina)

## Holt's Method
holtUS <- holt(y=dfTimeUS$inf, h = 12)
summary(holtUS)

plot(holtUS)


## Arima
arimaChina <- auto.arima(dfTimeCh$inf)
summary(arimaChina)



