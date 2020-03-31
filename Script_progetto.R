##### Progetto di Business Intelligence - Zhou Jia Liang 816019 #####

# Setto la working directory
setwd("/Users/tiberio/Desktop/Progetto Business")

# Importo i dati direttamente in formato zoo
library(zoo)
library(quantmod)
start_stream = "2017-10-1"
end_stream = "2018-10-1"

############################## Carico i titoli scelti ###################################

# Tencent Hong Kong - Hang Seng Hong Kong
TENC.z <- read.zoo("Stocks/TENC.HK_month.csv", header=TRUE, sep=",")
TENC.xts <- as.xts(TENC.z)
TENC_monthly.xts <- to.monthly(TENC.xts)
TENC_monthly_adjClose.xts <- TENC_monthly.xts$TENC.xts.Close
head(TENC_monthly.xts, 5)

# Netflix - NASDAQ
NFLX.z <- read.zoo("Stocks/NFLX.NASDAQ_month.csv", header=TRUE, sep=",")
NFLX.xts <- as.xts(NFLX.z)
NFLX_monthly.xts <- to.monthly(NFLX.xts)
NFLX_monthly_adjClose.xts <- NFLX_monthly.xts$NFLX.xts.Close
head(NFLX_monthly.xts, 5)

# Tesla - NASDAQ
TSLA.xts <- getSymbols("TSLA", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
TSLA_monthly.xts <- to.monthly(TSLA.xts)
TSLA_monthly_adjClose.xts <- TSLA_monthly.xts$TSLA.xts.Close
head(TSLA_monthly.xts)

# BYD - Hang Seng Hong Kong
BYD.xts <- getSymbols("1211.HK", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
BYD_monthly.xts <- to.monthly(BYD.xts)
BYD_monthly_adjClose.xts <- BYD_monthly.xts$BYD.xts.Close
head(BYD_monthly.xts)

# USD/EUR - DXY
USDEUR.xts <- getSymbols("EUR=X", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
USDEUR_monthly.xts <- to.monthly(USDEUR.xts)

USDEUR_monthly_adjClose.xts <- USDEUR_monthly.xts$USDEUR.xts.Close
head(USDEUR_monthly.xts)

# USD/CNY - DXY
USDCNY.xts <- getSymbols("CNY=X", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
USDCNY_monthly.xts <- to.monthly(USDCNY.xts)
USDCNY_monthly_adjClose.xts <- USDCNY_monthly.xts$USDCNY.xts.Close
head(USDCNY_monthly.xts)

################################ 1 - DATA SUMMARY #######################################

# Merging the stocks in one data structure
all_assets_daily_adjClose.xts <- merge(TENC.xts$Adj.Close, NFLX.xts$Adj.Close, 
                                        TSLA.xts$TSLA.Close, BYD.xts$`1211.HK.Close`,
                                        USDEUR.xts$`EUR=X.Close`, USDCNY.xts$`CNY=X.Close`)
head(all_assets_daily_adjClose.xts, 10)

all_assets_monthly_adjClose.xts <- merge(TENC_monthly_adjClose.xts, NFLX_monthly_adjClose.xts, 
                                       TSLA_monthly_adjClose.xts, BYD_monthly_adjClose.xts,
                                       USDEUR_monthly_adjClose.xts, USDCNY_monthly_adjClose.xts)
head(all_assets_monthly_adjClose.xts, 10)

# Sostituisco i valori NA
all_assets_daily_adjClose.xts <- na.approx(all_assets_daily_adjClose.xts)
head(all_assets_daily_adjClose.xts, 10)

all_assets_monthly_adjClose.xts <- na.approx(all_assets_monthly_adjClose.xts)
head(all_assets_monthly_adjClose.xts, 10)


# Renaming the columns
colnames(all_assets_monthly_adjClose.xts) <- c("TENC", "NFLX", "TSLA", "BYD", "USDEUR", "USDCNY")
head(all_assets_monthly_adjClose.xts, 10)

colnames(all_assets_daily_adjClose.xts) <- c("TENC", "NFLX", "TSLA", "BYD", "USDEUR", "USDCNY")
head(all_assets_daily_adjClose.xts, 10)

# Plotting all assets monthly adjusted close
plot.xts(all_assets_monthly_adjClose.xts, main="Assets Adjusted Close", 
         auto.legend=TRUE, legend.loc="topleft")

plot.xts(USDCNY_monthly_adjClose.xts, main="USD/CNY Monthly AdjClose")
plot.xts(USDEUR_monthly_adjClose.xts, main="USD/EUR Monthly AdjClose")


################################ 2 - DESCRIPTIVE ANALYTICS #######################################

############ CALCOLO I RITORNI #############

# Compute monthly Simple Gross/CC Return using package "PerformanceAnalytics"
library(PerformanceAnalytics)
all_assets_rtn_simple <- CalculateReturns(all_assets_monthly_adjClose.xts, method = "simple") # gross return
all_assets_rtn_cc <- CalculateReturns(all_assets_monthly_adjClose.xts, method = "compound") # cc return
head(all_assets_rtn_cc, 10)

# Faccio vedere quello mensile perch?? pi?? riassuntivo 
plot.xts(all_assets_rtn_cc, main="CC Return ", auto.legend=TRUE, legend.loc="topleft")
# TENCENT e NETFLIX sono abbastanza correlate come si pu?? vedere dal plot, quindi ?? probabile che 
# azioni dello stesso settore siano correlate. Anche TESLA sembra seguire lo stesso andamento


############# 4 PANEL DIAGNOSTIC PLOTS ###########
all_assets_rtn_cc <- na.omit(all_assets_rtn_cc)

TENC_rtn <- all_assets_rtn_cc$TENC
NFLX_rtn <- all_assets_rtn_cc$NFLX
TSLA_rtn <- all_assets_rtn_cc$TSLA
BYD_rtn <- all_assets_rtn_cc$BYD
USDEUR_rtn <- all_assets_rtn_cc$USDEUR
USDCNY_rtn <- all_assets_rtn_cc$USDCNY

#### TENCENT
par(mfrow=c(2,2))
hist(TENC_rtn, freq = FALSE, main="Distribution of CC Return about TENC stock", xlab="CC Return")
boxplot(as.numeric(TENC_rtn), outlier.symbol = "O", main = "TENC CC Return", ylab="cc return")
TENC_density <- density(TENC_rtn)
plot(TENC_density, type="l", col="blue", main="Smoothed Density")
qqnorm(TENC_rtn, main="TENC", col="blue")
qqline(TENC_rtn)

#### NETFLIX
par(mfrow=c(2,2))
hist(NFLX_rtn, freq = FALSE, main="Distribution of CC Return about NFLX stock", xlab="CC Return")
boxplot(as.numeric(NFLX_rtn), outlier.symbol = "O", main = "NFLX CC Return", ylab="cc return")
NFLX_density <- density(NFLX_rtn)
plot(NFLX_density, type="l", col="blue", main="Smoothed Density")
qqnorm(NFLX_rtn, main="NFLX", col="blue")
qqline(NFLX_rtn)

#### TESLA
par(mfrow=c(2,2))
hist(TSLA_rtn, freq = FALSE, main="Distribution of CC Return about TSLA stock", xlab="CC Return")
boxplot(as.numeric(TSLA_rtn), outlier.symbol = "O", main = "TSLA CC Return", ylab="cc return")
TSLA_density <- density(TSLA_rtn)
plot(TSLA_density, type="l", col="blue", main="Smoothed Density")
qqnorm(TSLA_rtn, main="TSLA", col="blue")
qqline(TSLA_rtn)

#### BYD
par(mfrow=c(2,2))
hist(BYD_rtn, freq = FALSE, main="Distribution of CC Return about BYD stock", xlab="CC Return")
boxplot(as.numeric(BYD_rtn), outlier.symbol = "O", main = "BYD CC Return", ylab="cc return")
BYD_density <- density(BYD_rtn)
plot(BYD_density, type="l", col="blue", main="Smoothed Density")
qqnorm(BYD_rtn, main="BYD", col="blue")
qqline(BYD_rtn)

#### USDEUR
par(mfrow=c(2,2))
hist(USDEUR_rtn, freq = FALSE, main="Distribution of CC Return about USD/EUR stock", xlab="CC Return")
boxplot(as.numeric(USDEUR_rtn), outlier.symbol = "O", main = "USD/EUR CC Return", ylab="cc return")
USDEUR_density <- density(USDEUR_rtn)
plot(USDEUR_density, type="l", col="blue", main="Smoothed Density")
qqnorm(USDEUR_rtn, main="USD/EUR", col="blue")
qqline(USDEUR_rtn)

#### USDCNY
par(mfrow=c(2,2))
hist(USDCNY_rtn, freq = FALSE, main="Distribution of CC Return about USD/CNY stock", xlab="CC Return")
boxplot(as.numeric(USDCNY_rtn), outlier.symbol = "O", main = "USD/CNY CC Return", ylab="cc return")
USDCNY_density <- density(USDCNY_rtn)
plot(USDCNY_density, type="l", col="blue", main="Smoothed Density")
qqnorm(USDCNY_rtn, main="USD/CNY", col="blue")
qqline(USDCNY_rtn)

# multiple boxplot for distribution comparison
par(mfrow=c(1,1))
TENC.mat <- as.numeric(TENC_rtn)
NFLX.mat <- as.numeric(NFLX_rtn)
TSLA.mat <- as.numeric(TSLA_rtn)
BYD.mat <- as.numeric(BYD_rtn)
USDEUR.mat <- as.numeric(USDEUR_rtn)
USDCNY.mat <- as.numeric(USDCNY_rtn)
boxplot(TENC.mat, NFLX.mat, TSLA.mat, BYD.mat, USDEUR.mat, USDCNY.mat, names=c("TENC", "NFLX", "TSLA", "BYD", "USD/EUR", "USD/CNY"), main="CC Return Boxplot")



# Compute univariate descriptive statistics
mean(TENC_rtn)
var(TENC_rtn)[1]
sd(TENC_rtn)
q1 <- quantile(TENC_rtn)
q1

mean(NFLX_rtn)
var(NFLX_rtn)[1]
sd(NFLX_rtn)
q2 <- quantile(NFLX_rtn)
q2

mean(TSLA_rtn)
var(TSLA_rtn)[1]
sd(TSLA_rtn)
q3 <- quantile(TSLA_rtn)
q3

mean(TSLA_rtn)
var(TSLA_rtn)[1]
sd(TSLA_rtn)
q4 <- quantile(TSLA_rtn)
q4

mean(BYD_rtn)
var(BYD_rtn)[1]
sd(BYD_rtn)
q5 <- quantile(BYD_rtn)
q5

mean(USDCNY_rtn)
var(USDCNY_rtn)[1]
sd(USDCNY_rtn)
q6 <- quantile(USDCNY_rtn)
q6

# skewness -> measures symmetry of a distribution around its mean
#             = 0 symmetric (normal distribution)
#             > 0 longer right tail than normal distribution
#             < 0 longer left tail than normal distribution
skewness(TENC_rtn)
skewness(NFLX_rtn)
skewness(TSLA_rtn)
skewness(BYD_rtn)
skewness(USDEUR_rtn)
skewness(USDCNY_rtn)

# by package "PerformanceAnalytics"
# kurtosis -> measures tail thickness of distribution
#             > 0 tail fatter than normal distribution tail
#             < 0 tail thinner than normal distribution tail
#             = 0 tail as normal distribution tail
kurtosis(TENC_rtn)
kurtosis(NFLX_rtn)
kurtosis(TSLA_rtn)
kurtosis(BYD_rtn)
kurtosis(USDEUR_rtn)
kurtosis(USDCNY_rtn)



# Sample Covariance Matrix
cov(cbind(TENC.mat, NFLX.mat, TSLA.mat, BYD.mat, USDEUR.mat, USDCNY.mat))

# Sample Correlation Matrix
cor(cbind(TENC.mat, NFLX.mat, TSLA.mat, BYD.mat, USDEUR.mat, USDCNY.mat))

# pairwise scatter plot
pairs(cbind(TENC.mat, NFLX.mat, TSLA.mat, BYD.mat, USDEUR.mat, USDCNY.mat), pch=18, 
      col="blue", main="Pairwise Correlation Scatterplot")


 ################################ 3 - PREDICTIVE ANALYTICS #######################################

library(tseries)
library(zoo)
library(forecast)

# Get dataset from 2009 to 2019
# Get dataset


# La funzione ritorna la configurazione ottima per i parametri del modello arima
find_optimal_arima_model <- function(train_set, test_set) {
  optimum_params <- c(0, 0, 0)
  min_RMSE <- 1
  trace_table <- matrix(ncol = 4)
  print("Ready to find optimal arima configuration")
  for (p in 0:5) {
    for (d in 0:1) {
      for (q in 0:5) {
        fit <- arima(train_set, order = c(p, d, q), method = "ML")
        arma.predictions <- predict(fit, n.ahead = length(test_set))$pred
        RMSE <- accuracy(arma.predictions, returnsTest)[2]
        trace_table <- rbind(trace_table, c(p,d,q,RMSE))
        if (RMSE < min_RMSE) {
          min_RMSE <- RMSE
          optimum_params <- c(p, d, q)
        }
      }
    }
  }
  print(trace_table)
  print(paste("Optimal configuration: ", 
              as.character(optimum_params[1]), ", ",
              as.character(optimum_params[2]), ", ",
              as.character(optimum_params[3]), ", ",
              " with RMSE = ", as.character(min_RMSE)))
  return(optimum_params)
}

##### TENCENT
TENC.z <- get.hist.quote( instrument="0700.HK", start="2009-02-01", end="2019-01-01", quote="AdjClose", 
                             provider="yahoo", origin="1970-01-01", compression="month")
# Change format index for monthly data
index(TENC.z) <- as.yearmon(index(TENC.z))
colnames(TENC.z) <- "TENC"
TENC.z <- aggregate(TENC.z, index(TENC.z), tail, 1) # to manage duplicate index
index(TENC.z) <- as.yearmon(index(TENC.z))
nrow(TENC.z)
head(TENC.z)

# Time series decomposition of AAPL adjusted close price time series
fit <- stl(TENC.z[,1], s.window="period" )
# "data" plot is the regular time series
# "seasonal" plot is the seasonal behaviour of time series
# "trend" plot is the global trend of entire time series
# "remainder" (aka: error/random walk) plot is the random component of noise inside of time series
plot(fit, main=paste("Seasonal Decomposition of TENC Adjusted Close Price Time Series"))

TENC_rtn <- diff( log(TENC.z[,1]) )

returnsTrain <- TENC_rtn[1:80]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- TENC_rtn[81:110]  # Test 30 mesi
length(returnsTest)
returnsValidation <- TENC_rtn[111:120]

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = 10)$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for TENC returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values



#________ NETFLIX

NFLX.z <- get.hist.quote( instrument="NFLX", start="2009-01-01", end="2019-01-01", quote="AdjClose", 
                          provider="yahoo", origin="1970-01-01", compression="month")
# Change format index for monthly data
index(NFLX.z) <- as.yearmon(index(NFLX.z))
colnames(NFLX.z) <- "NFLX"
NFLX.z <- aggregate(NFLX.z, index(NFLX.z), tail, 1) # to manage duplicate index
index(NFLX.z) <- as.yearmon(index(NFLX.z))
head(NFLX.z)

# Time series decomposition
fit <- stl(NFLX.z[,1], s.window="period" )
plot(fit, main=paste("Seasonal Decomposition of NFLX Adjusted Close Price Time Series"))

NFLX_rtn <- diff( log(NFLX.z[,1]) )

returnsTrain <- NFLX_rtn[1:80]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- NFLX_rtn[81:110]  # Test 30 mesi
length(returnsTest)
returnsValidation <- NFLX_rtn[111:120]

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = 10)$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for NFLX returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values



#________ TESLA ---> Dal 2010 in poi
TSLA.z <- get.hist.quote( instrument="TSLA", start="2010-01-01", end="2019-01-01", quote="AdjClose", 
                          provider="yahoo", origin="1970-01-01", compression="month")
# Change format index for monthly data
index(TSLA.z) <- as.yearmon(index(TSLA.z))
colnames(TSLA.z) <- "TSLA"
TSLA.z <- aggregate(TSLA.z, index(TSLA.z), tail, 1) # to manage duplicate index
index(TSLA.z) <- as.yearmon(index(TSLA.z))
head(TSLA.z)

# Time series decomposition
fit <- stl(TSLA.z[,1], s.window="period" )

plot(fit, main=paste("Seasonal Decomposition of TSLA Adjusted Close Price Time Series"))

TSLA_rtn <- diff( log(TSLA.z[,1]) )
head(TSLA_rtn)

returnsTrain <- TSLA_rtn[1:floor(80/120 * length(TSLA_rtn))]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- TSLA_rtn[(length(returnsTrain) + 1):(length(returnsTrain) + floor(30/120*length(TSLA_rtn)))]  # Test 30 mesi
length(returnsTest)
returnsValidation <- TSLA_rtn[(length(returnsTrain) + length(returnsTest) + 1): length(TSLA_rtn)] # Validation 10 mesi
length(returnsValidation)

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = length(returnsValidation))$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for TSLA returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values


#________ BYD
BYD.z <- get.hist.quote( instrument="1211.HK", start="2009-01-01", end="2019-01-01", quote="AdjClose", 
                          provider="yahoo", origin="1970-01-01", compression="month")
# Change format index for monthly data
index(BYD.z) <- as.yearmon(index(BYD.z))
colnames(BYD.z) <- "BYD"
length(BYD.z)
BYD.z <- aggregate(TSLA.z, index(BYD.z), tail, 1) # to manage duplicate index
index(BYD.z) <- as.yearmon(index(BYD.z))
head(BYD.z)

# Time series decomposition
fit <- stl(BYD.z[,1], s.window="period" )

plot(fit, main=paste("Seasonal Decomposition of BYD Adjusted Close Price Time Series"))

BYD_rtn <- diff( log(BYD.z[,1]) )

returnsTrain <- BYD_rtn[1:80]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- BYD_rtn[81:110]  # Test 30 mesi
length(returnsTest)
returnsValidation <- BYD_rtn[111:120]

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = 10)$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for BYD returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values



#________ USD/EUR
USDEUR.z <- get.hist.quote(instrument="EUR=X", start="2009-01-01", end="2019-01-01", quote="AdjClose", 
                         provider="yahoo", origin="1970-01-01", compression="month")
length(USDEUR.z)
# Change format index for monthly data
index(USDEUR.z) <- as.yearmon(index(USDEUR.z))
colnames(USDEUR.z) <- "USDEUR"
USDEUR.z <- aggregate(USDEUR.z, index(USDEUR.z), tail, 1) # to manage duplicate index
length(USDEUR.z) # La lunghezza ora si ?? ridotta a 111
# Creo un oggetto zoo vuoto con tutte i mesi 
z <- zoo(NA, order.by = seq.Date(from=as.Date(start(USDEUR.z)), to=as.Date(end(USDEUR.z)), by="month"))
index(z) <- as.yearmon(index(z))
temp <- merge(USDEUR.z, z)
length(temp[,1]) # ora i mesi mancanti sono tornati
USDEUR.z <- temp[,1]
USDEUR.z <- na.approx(USDEUR.z)
index(USDEUR.z) <- as.yearmon(index(USDEUR.z))
head(USDEUR.z)
tail(USDEUR.z)

# Time series decomposition
fit <- stl(USDEUR.z[,1], s.window="period" )

plot(fit, main=paste("Seasonal Decomposition of USD/EUR Adjusted Close Price Time Series"))

USDEUR_rtn <- diff( log(USDEUR.z[,1]) )
length(USDEUR_rtn)

returnsTrain <- USDEUR_rtn[1:80]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- USDEUR_rtn[81:110]  # Test 30 mesi
length(returnsTest)
returnsValidation <- USDEUR_rtn[111:length(USDEUR_rtn)]
length(returnsValidation)

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = 10)$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for USD/EUR returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values


#________ USD/CNY
USDCNY.z <- get.hist.quote( instrument="CNY=X", start="2009-01-01", end="2019-01-01", quote="AdjClose", 
                            provider="yahoo", origin="1970-01-01", compression="month")
# Change format index for monthly data
index(USDCNY.z) <- as.yearmon(index(USDCNY.z))
colnames(USDCNY.z) <- "USDCNY"
USDCNY.z <- aggregate(USDCNY.z, index(USDCNY.z), tail, 1) # to manage duplicate index
# Creo un oggetto zoo vuoto con tutte i mesi 
z <- zoo(NA, order.by = seq.Date(from=as.Date(start(USDCNY.z)), to=as.Date(end(USDCNY.z)), by="month"))
index(z) <- as.yearmon(index(z))
temp <- merge(USDCNY.z, z)
length(temp[,1]) # ora i mesi mancanti sono tornati
USDCNY.z <- temp[,1]
USDCNY.z <- na.approx(USDCNY.z)
index(USDCNY.z) <- as.yearmon(index(USDCNY.z))


# Time series decomposition
fit <- stl(USDCNY.z[,1], s.window="period" )

plot(fit, main=paste("Seasonal Decomposition of USD/EUR Adjusted Close Price Time Series"))

USDCNY_rtn <- diff( log(USDCNY.z[,1]) )
length(USDCNY_rtn)

returnsTrain <- USDCNY_rtn[1:80]  # Train 80 mesi
length(returnsTrain)
tail(returnsTrain)
returnsTest <- USDCNY_rtn[81:110]  # Test 30 mesi
length(returnsTest)
returnsValidation <- USDCNY_rtn[111:length(USDCNY_rtn)]
length(returnsValidation)

fit <- arima(rbind(returnsTrain, returnsTest), order = find_optimal_arima_model(returnsTrain, returnsTest))
fit
arma.predictions <- predict(fit, n.ahead = 10)$pred
# forecast function sets level of predictions confindence
arma.forecast <- forecast(fit, h = length(returnsValidation), level = c(95,80))
arma.forecast
plot(arma.forecast, main = "ARMA forecasts for USD/CNY returns")
# add to plot the test values (the real returns inside returnsTest)
lines(returnsValidation)
# Metrics to compare the regression task
accuracy(arma.predictions, returnsValidation) 
accuracy(arma.predictions, returnsValidation)[2] # RMSE values



############# BETA COMPUTATION ############
library(quantmod)
library(PerformanceAnalytics)
start_stream <- '2011-01-01'
end_stream = Sys.Date()

# Bivariate Distribution analysis

# TENCENT - HANG SENG HONG KONG
TENC.xts <- getSymbols("0070.HK", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
TENC.xts <- to.monthly(TENC.xts)
TENC <- na.omit(diff(log(TENC.xts$TENC.xts.Adjusted)))
colnames(TENC) <- c("0070.HK")

#  NETFLIX - NASDAQ
NFLX.xts <- getSymbols("NFLX", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
NFLX.xts <- to.monthly(NFLX.xts)
NFLX <- na.omit(diff(log(NFLX.xts$NFLX.xts.Adjusted)))
colnames(NFLX) <- c("NFLX")

# Tesla - NASDAQ
TSLA.xts <- getSymbols("TSLA", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
TSLA.xts <- to.monthly(TSLA.xts)
TSLA_adjClose <- TSLA.xts$TSLA.xts.Adjusted
TSLA <- na.omit(diff(log(TSLA_adjClose)))
colnames(TSLA) <- c("TSLA")

# BYD - Hang Seng Hong Kong
BYD.xts <- getSymbols("1211.HK", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
BYD.xts <- to.monthly(BYD.xts)
BYD <- na.omit(diff(log(BYD.xts$BYD.xts.Adjusted)))
colnames(BYD) <- c("BYD")

# USD/EUR - DXY
USDEUR.xts <- getSymbols("EUR=X", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
USDEUR.xts <- to.monthly(USDEUR.xts)
USDEUR <- na.omit(diff(log(USDEUR.xts$USDEUR.xts.Adjusted)))
colnames(USDEUR) <- c("USDEUR")

# USD/CNY - DXY
USDCNY.xts <- getSymbols("CNY=X", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
USDCNY.xts <- to.monthly(USDCNY.xts)
USDCNY <- na.omit(diff(log(USDCNY.xts$USDCNY.xts.Adjusted)))
colnames(USDCNY) <- c("USDCNY")

# HANG SENG HONK KONG
HSI.xts <- getSymbols("^HSI", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
HSI.xts <- to.monthly(HSI.xts)
HSI <- na.omit(diff(log(HSI.xts$HSI.xts.Adjusted)))
colnames(HSI) <- c("HSI")

# NASDAQ
IXIC.xts <- getSymbols("^IXIC", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
IXIC.xts <- to.monthly(IXIC.xts)
IXIC <- na.omit(diff(log(IXIC.xts$IXIC.xts.Adjusted)))
colnames(IXIC) <- c("IXIC")


# DX-Y.NYB
DXY.xts <- getSymbols("DX-Y.NYB", from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE )
DXY.xts <- to.monthly(DXY.xts)
DXY <- na.omit(diff(log(DXY.xts$DXY.xts.Adjusted)))
colnames(DXY) <- c("DXY")


# Beta function to calculate beta value
beta_function <- function(stock, market_index){
  beta <- cov(stock, market_index)/var(market_index)
  return(beta)
}

# Calculate beta values of stocks using a time window of 20 months
TENC_betas.xts <- NULL # time series to save beta's values
NFLX_betas.xts <- NULL
TSLA_betas.xts <- NULL
BYD_betas.xts <- NULL
USDEUR_betas.xts <- NULL
USDCNY_betas.xts <- NULL
delta_t <- 20 # move time windows for beta value
length_period = dim(DXY)[1] # length period 

# n.b. remove the first "delta_t" values to calculate the first beta at time delta_t + 1
start <- delta_t+1 # first month after the 20 months to calculate the first value of beta


for (i in start:length_period){
  # Beta value <- beta_function(AAPL[time_windows_20_months], SP500[time_windows_20_months])
  beta_val_TENC <- beta_function(TENC[(i-delta_t):(i-1)], HSI[(i-delta_t):(i-1)])
  beta_val_NFLX <- beta_function(NFLX[(i-delta_t):(i-1)], IXIC[(i-delta_t):(i-1)])
  beta_val_TSLA <- beta_function(TSLA[(i-delta_t):(i-1)], IXIC[(i-delta_t):(i-1)])
  beta_val_BYD <- beta_function(BYD[(i-delta_t):(i-1)], HSI[(i-delta_t):(i-1)])
  beta_val_USDEUR <- beta_function(USDEUR[(i-delta_t):(i-1)], DXY[(i-delta_t):(i-1)])
  beta_val_USDCNY <- beta_function(USDCNY[(i-delta_t):(i-1)], DXY[(i-delta_t):(i-1)])
  
  # create a time series of one beta values for each stock
  beta_xts_TENC <- as.xts(beta_val_TENC, order.by = index(TENC[(i-1)]))
  beta_xts_NFLX <- as.xts(beta_val_NFLX, order.by = index(NFLX[(i-1)]))
  beta_xts_TSLA <- as.xts(beta_val_TSLA, order.by = index(TSLA[(i-1)]))
  beta_xts_BYD <- as.xts(beta_val_BYD, order.by = index(BYD[(i-1)]))
  beta_xts_USDEUR <- as.xts(beta_val_USDEUR, order.by = index(USDEUR[(i-1)]))
  beta_xts_USDCNY <- as.xts(beta_val_USDCNY, order.by = index(USDCNY[(i-1)]))
  
  # Create a time series of beta for each stock 
  if(is.null(TENC_betas.xts)){
    TENC_betas.xts <- beta_xts_TENC
    NFLX_betas.xts <- beta_xts_NFLX
    TSLA_betas.xts <- beta_xts_TSLA
    BYD_betas.xts <- beta_xts_BYD
    USDEUR_betas.xts <- beta_xts_USDEUR
    USDCNY_betas.xts <- beta_xts_USDCNY
  }else{
    
    TENC_betas.xts <- rbind(TENC_betas.xts, beta_xts_TENC)
    NFLX_betas.xts <- rbind(NFLX_betas.xts, beta_xts_NFLX)
    TSLA_betas.xts <- rbind(TSLA_betas.xts, beta_xts_TSLA)
    BYD_betas.xts <- rbind(BYD_betas.xts, beta_xts_BYD)
    USDEUR_betas.xts <- rbind(USDEUR_betas.xts, beta_xts_USDEUR)
    USDCNY_betas.xts <- rbind(USDCNY_betas.xts,beta_xts_USDCNY )
  }

  
}

plot(TENC_betas.xts, main="TENC Beta (^HSI)")
plot(NFLX_betas.xts, main="NFLX Beta (^IXIC)")
plot(TSLA_betas.xts, main="TSLA Beta (^IXIX)")
plot(BYD_betas.xts, main="BYD Beta (^HSI)")
plot(USDEUR_betas.xts, main="USD/EUR Beta (DX-Y.NYB)")
plot(USDCNY_betas.xts, main="USD/CNY Beta (DX-Y.NYB)")


######### PORTFOLIO MANAGEMENT ############
# Load tseries
library(tseries)
library(timeSeries)
# install.packages("fPortfolio")
library(fPortfolio)
library(fAssets)
library(quantmod)
# install.packages("caTools")
library(caTools)
# install.packages("dplyr")
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)


# Create an optimized portfolio of returns
portfolioReturns <- na.omit(all_assets_rtn_cc)
target_return = mean(portfolioReturns)
opt <- portfolio.optim(portfolioReturns, pm = target_return, shorts = FALSE) # allow short selling
head(opt)

# Create pf_weights
pf_weights <- opt$pw
pf_weights
sum(pf_weights)

# Assign asset names
names(pf_weights) <- colnames(all_assets_rtn_cc)
head(pf_weights)

# Barplot of opt_weights
barplot(pf_weights, col = rainbow(6))

# Print expected portfolio return and volatility
opt$pm 
opt$ps


#Calculate and Plot Frontier and Efficient Portfolios
portfolioReturns <- as.timeSeries(all_assets_rtn_cc)
# calculate the efficient frontier
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")

# 1:   Plot Efficient Frontier
# 2:   Add Minimum Risk Portfolio
# 3:   Add Tangency Portfolio
# 4:   Add Risk/Return of Single Assets
# 5:   Add Equal Weights Portfolio
# 6:   Add Two Asset Frontiers [LongOnly Only]
# 7:   Add Monte Carlo Portfolios
# 8:   Add Sharpe Ratio [Markowitz PF Only]
options <- c(1,2,3,4)
plot(effFrontier, options)



computePortfolioReturn <- function(budget, weights, rtn) {
  startDate_assetPrices <- tail(rtn, 10)[1,] # ultimi 10 mesi
  print("Initial price:")
  print(startDate_assetPrices)
  
  endDate_assetPrices <- tail(rtn, 10)[10,]
  print("Final price:")
  print(endDate_assetPrices)
  
  budget_assets <- weights * budget
  print("Budget for every asset:")
  print(budget_assets)
  bought_assets <- floor(budget_assets / startDate_assetPrices)
  print("Number of bought assets:")
  print(bought_assets)
  
  bought_assets_initial_values <- data.matrix(as.data.frame(bought_assets)) * data.matrix(as.data.frame(startDate_assetPrices))
  print("Bought assets initial value:")
  print(bought_assets_initial_values)
  
  bought_assets_final_values <- data.matrix(as.data.frame(bought_assets)) * data.matrix(as.data.frame(endDate_assetPrices))
  print("Bought assets final value:")
  print(bought_assets_final_values)
  
  transactionCost = 0.01
  print("Transaction costs:")
  print(sum(bought_assets_initial_values)*(transactionCost))
  finalReturn = sum(bought_assets_final_values) - sum(bought_assets_initial_values)*(1+transactionCost)
  print(class(finalReturn))
  return(finalReturn)
}

start_stream = "1970-01-01"
end_stream = "2019-01-01"
tickers <- c("0700.HK", "NFLX", "TSLA", "1211.HK", "EUR=X", "CNY=X")
portfolioAdjClose <- NULL

for (Ticker in tickers) {
  asset.xts <- getSymbols(Ticker, from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
  asset_monthly.xts <- to.monthly(asset.xts)
  asset_monthly_adjClose.xts <- asset_monthly.xts$asset.xts.Close
  portfolioAdjClose <- cbind(portfolioAdjClose, asset_monthly_adjClose.xts)
}
portfolioAdjClose <- na.approx(na.omit(portfolioAdjClose))
colnames(portfolioAdjClose) <- tickers

rtn <- CalculateReturns(portfolioAdjClose, method = "compound") # cc return
rtn <- na.omit(rtn)

target_return = mean(rtn)
opt <- portfolio.optim(rtn, pm = target_return, shorts = FALSE) # allow short selling
weights <- opt$pw

portfolioReturn = computePortfolioReturn(10000, weights, portfolioAdjClose)
portfolioReturn # Portfolio return 





