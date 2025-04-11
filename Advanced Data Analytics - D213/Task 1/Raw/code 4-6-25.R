#####################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1
#####################################
#https://www.youtube.com/watch?v=kyPg3jV4pJ8&t=1016s

#####################################
# Initial setup 
#####################################
# Load necessary libraries
library(tidyverse)
library(forecast)
library(tseries)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')
data <- read_csv("teleco_time_series.csv")

# Create a date sequence for 731 days starting from 2023-01-01
data$Day <- seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 731)

# Verify the date range
range(data$Day)

data<-as.data.frame(data)

str(data)
#view(data)

#converting to ts
Y <- ts(data$Revenue, start = c(2023, 1), frequency = 365)

str(Y)
#####################################
#C1 plot ts data
#####################################
autoplot(Y, main = "Time series of Revenue")

#####################################
#C2 explain gaps and length
#####################################
#checking for length and missing values
cat('There are', length(data$Day),'rows of data.')  # Sequence length
cat('Missing values:',any(is.na(data)))  # Check for missing values

#####################################
#C3 evaluate stationarity
#####################################
adf.test(Y)   # adf states that the data is stationary (p-value = 0.02431) but there is a clear upward trend
#Removing the trend

ndiffs(Y) #ndiffs function still recommends that one difference is required to make the data stationary. 

DY <- diff(Y, s = 1) #s=1 meaning that the data has a seasonal patteren that happens 1 time per year
adf.test(DY)  # still shows stationarity but there is no longer a trend. p-value = 0.01
plot(DY)

#####################################
#C4 Training and Test split
#####################################
#splitting the data using the last 30 days of data for testing
train <- Y[1:(length(Y) - 30)] #selecting all the data points except for the last 30 and keeping them for the training set
test <- Y[((length(Y) - 30 + 1):length(Y))] #selecting the last 30 data points for the test set
plot(train, main = "Training set", type='l')
plot(test, main = "Testing set (Last 30 days of daily revenue data)", type='l')

#####################################
#C5 export cleaned data
#####################################
# Export cleaned data to CSV
write.csv(Y, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Clean/cleaned_ts_data.csv", row.names = FALSE)

#####################################
#D1 plot the following: 
#####################################
#the decomposed ts, 
#SOURCE: https://otexts.com/fpp2/stl.html
decomposed <- stl(Y,t.window=365, s.window="periodic", robust=TRUE)
plot(decomposed)
summary(decomposed)

#seasonality. There apears to be seasonality
seasonal <- decomposed$time.series[, "seasonal"]
plot(seasonal, main = "Seasonality of decomposed time series", ylab = "Seasonality", xlab = "Time", type = "l")
summary(seasonal)

#trend
trend <- decomposed$time.series[, "trend"]
plot(trend, main = "Trend of decomposed time series", ylab = "Trend", xlab = "Time", type = "l")
summary(trend)

#autocorrelation
ggAcf(DY)
ggPacf(DY)

#spectral density 
spectrum(Y)

#confirm lack of trend in residuals of decomposed series. 
#Stationarity Check with ADF Test
adf.test(decomposed$time.series[, "remainder"])

#Ljung-Box test to check for autocorrelation in residuals. 
Box.test(decomposed$time.series[, "remainder"], lag = 1, type = "Ljung-Box")

#Autocorrelation plot with ACF
ggAcf(decomposed$time.series[, "remainder"])

plot(decomposed$time.series[, "remainder"])
ggAcf(decomposed$time.series[, "remainder"])
ggPacf(decomposed$time.series[, "remainder"])

ggtsdisplay(DY)

#####################################
#D2 ARIMA model that accounts for trend aand seasonality of the ts data
#####################################
fit_arima <- auto.arima(train, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)

#accounting for trend with d=1. Tells arima that before it fits the data, take the difference of the data
#accounting for seasonality with D=1. Gets rid of the seasonality by taking the first seasonal difference
#stepwise=FALSE trying all models to get the most accurate results
#approximation=FALSE because time isnt an issue. If it were an issue then approximation could be set to TRUE but it would come at the cost of approximated AIC values.
summary(fit_arima)

#####################################
#D3 forecast using the D2 model
#####################################
fcast <- forecast(fit_arima, h=30) #forecasting 30 days in advance
autoplot(fcast)

#####################################
#D4 code outputs for analysis
#####################################
print(summary(fcast))
ggAcf(fcast$residuals)

#####################################
#D5 provide code for ts model 
#####################################
#The code used for this assessment will be included in the submission files and is named D213_code.R

#####################################
#E1 Results need to include the following: 
#####################################
#selection of an ARIMA model

#prediction interval of the forecast

#justification of the forecast length

#model evaluation procedure and error metric
# Summarize key accuracy metrics
acc <- accuracy(fcast, test)  # RMSE, MAE, etc.

cat("Forecast Accuracy Metrics:\n")
cat("MASE:", acc[2, "MASE"],"\n")
cat("RMSE:", acc[2, "RMSE"], "\n")
cat("MAE:", acc[2, "MAE"], "\n")

#####################################
#E2 plot annotated visualization of the forecast of final model compared to the test set
#####################################
autoplot(fcast)

#####################################
#E3 recommended course of action
#####################################
