###########################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1
###########################################

#SOURCE: https://www.youtube.com/watch?v=dBNy_A6Zpcc

# Initial setup
# Load necessary libraries
library(tidyverse)
library(fpp2)
library(tseries)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')

# Data acquisition
data <- read_csv("teleco_time_series.csv")

#declare as time series data
Y <- ts(data$Revenue, start = c(1,1), frequency = 365) #just looking for the second (revenue column)

###########################################
# preliminary analysis
###########################################

#time plot
autoplot(Y)+
  ggtitle("Time Plot: Daily Reveune For Telecom Data") +
  ylab("millions of dollars") +
  xlab("Years")

length(Y)  # Sequence length
any(is.na(Y))  # Check for missing values

#Data appears to have a trend.

#Take the first difference of the data to remove the trend (change in revenue from day to day)
#Removing the trend
DY <- diff(Y)

autoplot(DY)+
  ggtitle("Time Plot: Daily Reveune For Telecom Data") +
  ylab("millions of dollars") +
  xlab("Years")

#Now that it appears trend-stationary, looking for seasonality
#ggseasonplot(DY) +
#  ggtitle("Seasonal Plot: Change in Daily Revenue") +
#  ylab("millions of dollars")

#Checking the average change per day
#ggsubseriesplot(DY) +
#  ggtitle("Seasonal Plot: Change in Daily Revenue") +
#  ylab("millions of dollars")
#Does not appear to be any seasonality


###########################################
#Evaluating stationarity with adf
###########################################
adf.test(Y)   # Original series
adf.test(DY)  # Differenced series

###########################################
# Fit on ARIMA model
###########################################
#splitting the data using the last 30 days of data
N <- length(Y)
test_size <- 30
train <- Y[1:(N - test_size)] #selecting all the data points except for the last 30 and keeping them for the training set
test <- Y[(N - test_size + 1):N] #selecting the last 30 data points for the test set

sum(is.na(Y))  # Check for missing values
# Export cleaned data to CSV
write.csv(Y, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Clean/cleaned_ts_data.csv", row.names = FALSE)

ggAcf(Y)  # Autocorrelation
spectrum(Y)  # Spectral density
decomposed <- stl(Y, s.window="periodic")
autoplot(decomposed)  # Decomposition
autoplot(decomposed$time.series[, "remainder"])  # Residuals

#using the original data with the trend. setting d=1 to get the difference from one point to the next and removing the trend
fit_arima <- auto.arima(train, d=1, D=1, stepwise = FALSE, approximation = TRUE, trace = TRUE)

#d=1 tells arima that before it fits the data, take the difference of the data
#D=1 gets rid of the seasonality by taking the first seasonal difference
#stepwise=FALSE instead of trying all combination of arima models, it will only try a few. 
##approximation=TRUE To save time, I will use approximation=TRUE to approximate the AIC
print(summary(fit_arima)) #Residual sd = 0.4218 Fits better than the benchmark method. 
#sigma^2 = 0.222
sqrt(0.222) # residual sd = 0.4711688

checkresiduals(fit_arima)

###########################################
#Forecasting with ARIMA model
###########################################

fcast <- forecast(fit_arima, h=30)
autoplot(fcast)
autoplot(fcast, include=365)
print(summary(fcast))

summary(fit_arima)  # Model parameters
fcast$mean  # Forecast values
fcast$lower  # Lower CI
fcast$upper  # Upper CI

accuracy(fcast, test)  # RMSE, MAE, etc.

