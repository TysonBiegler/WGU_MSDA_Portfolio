# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1

# Initial setup -----------------------------------------------------------
# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')

# Data acquisition
teleco_data <- read.csv("teleco_time_series.csv")

# ---- C1: Line graph visualizing the time series ----
plot(teleco_data$Day, teleco_data$Revenue, type = "l",
     main = "Daily Telecommunications Revenue", 
     xlab = "Day", ylab = "Revenue")

# ---- C2: Time step formatting analysis ----
# Check for missing days/gaps
all_days <- 1:max(teleco_data$Day)
missing_days <- setdiff(all_days, teleco_data$Day)
print(missing_days)  # Should print integer(0) if no missing days
print(length(teleco_data$Day))  # Length of the sequence

# Convert to time series object
revenue_ts <- ts(teleco_data$Revenue, frequency = 7)  # Weekly seasonality

# ---- C3: Evaluate stationarity ----
# ADF test for stationarity
adf.test(revenue_ts)

# Visual confirmation of trend
plot(revenue_ts, main = "Visual Trend Inspection")


# ---- C4: Data preparation including train/test split ----
# Determine split point (80% for training)
split_point <- round(0.8 * length(revenue_ts))
train_ts <- window(revenue_ts, end = c(1, split_point))
test_ts <- window(revenue_ts, start = c(1, split_point + 1))

# ---- D1: Analyze time series components ----
# Decompose the series to show seasonal component, trend, etc.
decomp <- stl(revenue_ts, s.window = "periodic")
plot(decomp)  # This will clearly show the trend component

# ACF - autocorrelation function of the original series 
acf(revenue_ts, main = "ACF of Original Series")

# ACF after differencing to address the trend
acf(diff(revenue_ts), main = "ACF after Differencing")

# PACF - partial autocorrelation function
pacf(revenue_ts, main = "PACF of Original Series")
pacf(diff(revenue_ts), main = "PACF after Differencing")

# Spectral density
spectrum(revenue_ts, main = "Spectral Density")

# Check residuals for trends after decomposition
residual_ts <- remainder(decomp)
plot(residual_ts, main = "Residuals after Decomposition")
adf.test(residual_ts)  # Check lack of trend in residuals

# ---- D2: Identify ARIMA model ----
# Auto ARIMA to find optimal parameters
auto_arima_model <- auto.arima(train_ts)
print(summary(auto_arima_model))

# Create a manual model with differencing
# This ensures we account for the trend we observed visually
manual_arima_model <- Arima(train_ts, order=c(1, 1, 1))
print(summary(manual_arima_model))

# Compare AIC scores
print(AIC(auto_arima_model))
print(AIC(manual_arima_model))

# Use auto_arima_model for forecasting becuase it has a lower AIC

# ---- D3: Perform forecast ----
forecast_horizon <- length(test_ts)
arima_forecast <- forecast(auto_arima_model, h = forecast_horizon)
print(arima_forecast)

# ---- D4: Provide output and calculations ----
# Plot forecast
plot(arima_forecast, main = "ARIMA Forecast", 
     xlab = "Time", ylab = "Revenue")
lines(test_ts, col = "red")  # Add actual test data
legend("topright", legend = c("Forecast", "Actual Test Data"), 
       col = c("blue", "red"), lty = 1)

# Accuracy metrics 
forecast_accuracy <- accuracy(arima_forecast, test_ts)
print(forecast_accuracy)

# Diagnostic check on residuals
checkresiduals(auto_arima_model)

# ---- E2: Annotated visualization of forecast vs test set ----
# Create a time index for plotting
time_index <- (split_point + 1):(split_point + forecast_horizon)

# Plot with annotations
plot(time_index, as.numeric(test_ts), type = "l", col = "red",
     main = "Forecast vs Actual Values",
     xlab = "Day", ylab = "Revenue")
lines(time_index, as.numeric(arima_forecast$mean), col = "blue")
lines(time_index, as.numeric(arima_forecast$lower[,2]), col = "lightblue", lty = 2)
lines(time_index, as.numeric(arima_forecast$upper[,2]), col = "lightblue", lty = 2)
abline(v = split_point, lty = 2, col = "gray")
legend("topright", 
       legend = c("Actual", "Forecast", "95% Prediction Interval"),
       col = c("red", "blue", "lightblue"), 
       lty = c(1, 1, 2))

# Complete plot showing the entire series
plot(revenue_ts, main = "Complete Time Series with Forecast", col = "black")
lines(time_index, as.numeric(arima_forecast$mean), col = "blue")
lines(time_index, as.numeric(test_ts), col = "red")
abline(v = split_point, lty = 2, col = "gray")
legend("topright", 
       legend = c("Training Data", "Test Data", "Forecast"),
       col = c("black", "red", "blue"), 
       lty = 1)