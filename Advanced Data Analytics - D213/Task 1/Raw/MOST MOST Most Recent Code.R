###########################################
# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1
###########################################

# Load necessary libraries
library(tidyverse)
library(fpp2)
library(tseries)
library(lubridate)
library(tsibble)
library(ggplot2)

# Set working directory
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')

# Data acquisition
data <- read_csv("teleco_time_series.csv")

str(data)

# Check the number of rows in the data
nrow(data)  # Should return 731

# Create a date sequence for 731 days starting from 2023-01-01
dates <- seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 731)

# Check the length of dates
length(dates)  # Should return 731

# Verify the date range
range(dates) 

# Add dates to the data frame
data <- data %>%
  mutate(Date = dates)

str(data)

# Convert to a tsibble for time series analysis
data_tsibble <- as_tsibble(data, index = Date)

# Convert to a time series object
Y <- ts(data$Revenue, start = c(2023, 1), frequency = 2)

# Preliminary Analysis
# Time plot
autoplot(Y) +
  ggtitle("Time Plot: Daily Revenue For Telecom Data") +
  ylab("Millions of Dollars") +
  xlab("Date") +
  theme_minimal()

# Check data properties
length(Y)  # 731 days of daily data
any(is.na(Y))  # Check for missing values


# Evaluating Stationarity
adf.test(Y)  # p-value = 0.02431, suggests stationarity, but trend is present
ndiffs(Y)  # Returns 1

# Difference the data to remove the trend
Y_diff <- diff(Y)
adf.test(Y_diff)  # After differencing the p-value is 0.01,  and alternative hypothesis is stationary

# Check for seasonality with weekly frequency
ggseasonplot(Y,  year.labels = TRUE) +
  ggtitle("Seasonal Plot: Weekly Change in Daily Revenue") +
  ylab("Millions of Dollars")

ggsubseriesplot(Y) +
  ggtitle("Seasonal Subseries Plot: Weekly Change in Daily Revenue") +
  ylab("Millions of Dollars")

# Confirm seasonality with ACF and PACF
Acf(Y, main = "ACF: Checking for Seasonal Lags")
Pacf(Y, main = "PACF: Identifying AR Order")

nsdiffs(Y)  # Check for seasonal differencing

# Fit ARIMA Model
# Split data: Use last 30 days for testing
N <- length(Y)  # 731
test_size <- 30
train <- Y[1:(N - test_size)]  # Training set: First 701 days
test <- Y[(N - test_size + 1):N]  # Test set: Last 30 days

# Export cleaned data to CSV
write.csv(data.frame(Date = dates, Revenue = Y), 
          "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Clean/cleaned_ts_data.csv", 
          row.names = FALSE)

# Decomposition to understand trend, seasonality, and residuals
decomposed <- stl(Y, s.window = "periodic")
autoplot(decomposed) +
  ggtitle("Decomposition of Daily Revenue") +
  xlab("Date")

#plot(ggAcf(Y))  # Autocorrelation
#spectrum(Y)  # Spectral density

# Fit ARIMA model, allowing for seasonality
fit_arima <- auto.arima(train, seasonal = TRUE, stepwise = TRUE, trace = TRUE, approximation = FALSE, D = 1)  # Force seasonal differencing

# Compare alternative models
fit_arima_alt1 <- Arima(train, order = c(2,1,1))
fit_arima_alt2 <- Arima(train, order = c(1,1,1))
fit_arima_alt3 <- Arima(train, order = c(0,1,1))
print(AIC(fit_arima, fit_arima_alt1, fit_arima_alt2, fit_arima_alt3))

print(summary(fit_arima))
checkresiduals(fit_arima)
Box.test(fit_arima$residuals, lag = 10, type = "Ljung-Box")

# Forecasting with ARIMA Model
fcast <- forecast(fit_arima, h = 30)# Forecast for 30 days

# Convert test data to time series with correct start time
test_ts <- ts(test, start = start(fcast$mean), frequency = frequency(Y))

# Create data frames for plotting
# Training data
train_df <- data.frame(
  Date = dates[1:(N - test_size)],
  Revenue = as.numeric(train),
  Series = "Train"
)

# Test data
test_df <- data.frame(
  Date = dates[(N - test_size + 1):N],
  Revenue = as.numeric(test),
  Series = "Test"
)

# Forecast data
forecast_dates <- seq.Date(from = dates[N] + 1, by = "day", length.out = 30)
forecast_df <- data.frame(
  Date = forecast_dates,
  Revenue = as.numeric(fcast$mean),
  Series = "Forecast",
  Lower80 = as.numeric(fcast$lower[, "80%"]),
  Upper80 = as.numeric(fcast$upper[, "80%"]),
  Lower95 = as.numeric(fcast$lower[, "95%"]),
  Upper95 = as.numeric(fcast$upper[, "95%"])
)

# Combine data for plotting
plot_data <- bind_rows(train_df, test_df, forecast_df)

# Plot the data
ggplot(plot_data, aes(x = Date, y = Revenue, color = Series)) +
  geom_line() +
  # Add confidence intervals
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95, fill = "95% CI"), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80, fill = "80% CI"), 
              alpha = 0.3, inherit.aes = FALSE) +
  scale_color_manual(values = c("Train" = "Black", "Test" = "red", "Forecast" = "blue")) +
  scale_fill_manual(values = c("80% CI" = "blue", "95% CI" = "red"), 
                    guide = guide_legend(title = "Confidence Intervals")) +
  labs(title = "30 Day Forecast",
       x = "Date",
       y = "Revenue in Million $") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())


zoom_start_date <- max(dates) - days(60)

ggplot(plot_data %>% filter(Date >= zoom_start_date), aes(x = Date, y = Revenue, color = Series)) +
  geom_line() +
  geom_point(size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95, fill = "95% CI"), 
              alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80, fill = "80% CI"), 
              alpha = 0.3, inherit.aes = FALSE) +
  scale_color_manual(values = c("Train" = "Black", "Test" = "red", "Forecast" = "blue")) +
  scale_fill_manual(values = c("80% CI" = "blue", "95% CI" = "red"), 
                    guide = guide_legend(title = "Confidence Intervals")) +
  labs(title = "30 Day Forecast (Zoomed)",
       subtitle = paste("From", format(zoom_start_date, "%Y-%m-%d"), "to", format(max(forecast_dates), "%Y-%m-%d")),
       x = "Date",
       y = "Revenue in Million $") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())

# Display forecast summary and accuracy
print(summary(fcast))
acc <- accuracy(fcast, test)
print(acc)

# Summarize key accuracy metrics
cat("Forecast Accuracy Metrics:\n")
cat("RMSE:", acc[2, "RMSE"], "\n")
cat("MAE:", acc[2, "MAE"], "\n")

