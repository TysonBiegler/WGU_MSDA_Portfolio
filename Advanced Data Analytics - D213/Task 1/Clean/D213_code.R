#####################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1
#####################################

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
# C4 Training and Test split - 80/20 Split
#####################################

train <- Y[1:floor(0.8 * length(Y))]  #the first 80% of the data for training
test <- Y[(floor(0.8 * length(Y)) + 1):length(Y)]  #the remaining 20% for testing

# Plot the splits
plot(train, main = "Training set (80% of data)", type = 'l')
plot(test, main = "Testing set (20% of data)", type = 'l')


#####################################
#C5 export cleaned data
#####################################

view(Y)
# Export cleaned data to CSV
write.csv(Y, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Clean/cleaned_ts_data.csv", row.names = FALSE)

#####################################
#D1 plot the following: 
#####################################
#the decomposed ts
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
fcast <- forecast(fit_arima, h=180) #forecasting 90 days in advance
autoplot(fcast)

#####################################
#D4 code outputs for analysis
#####################################
print(summary(fcast))
ggAcf(fcast$residuals)

#####################################
#E1 Results need to include the following: 
#####################################

#model evaluation procedure and error metric
acc <- accuracy(fcast, test)  # RMSE, MAE, etc.

print(acc)
#####################################
#E2 plot annotated visualization of the forecast of final model compared to the test set
#####################################
n <- length(train) + length(test)
train_size <- length(train)


plot(1:n, c(train, test), 
     type = "l", 
     xlab = "Date", 
     ylab = "Revenue", 
     main = "Forecast with Test Data Overlay",
     xaxt = "n")


axis(1, at = seq(1, n, by = 100), labels = data$Day[seq(1, n, by = 100)])


lines((train_size + 1):n, test, col = "red")


polygon(c((n + 1):(n + length(fcast$mean)), rev((n + 1):(n + length(fcast$mean)))), 
        c(fcast$lower[,1], rev(fcast$upper[,1])), 
        col = rgb(1, 0, 0.5, alpha = 0.2), 
        border = NA)

polygon(c((n + 1):(n + length(fcast$mean)), rev((n + 1):(n + length(fcast$mean)))), 
        c(fcast$lower[,2], rev(fcast$upper[,2])), 
        col = rgb(1, 0, 0.5, alpha = 0.19), 
        border = NA)


lines((n + 1):(n + length(fcast$mean)), fcast$mean, col = "blue")


legend("topleft", 
       legend = c("Actual", "Test", "Forecast", "80% CI", "95% CI"),
       col = c("black", "red", "blue", "pink", "pink"), 
       lty = c(1, 1, 1, NA, NA), 
       fill = c(NA, NA, NA, "pink", "pink"))
