# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1

# Initial setup -----------------------------------------------------------
# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(forecast)   # For time series forecasting and ARIMA models
library(tseries)    # For stationarity tests (e.g., ADF test)
library(ggplot2)    # For enhanced plotting capabilities

# Set working directory (adjust path to your dataset location)
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')

# Load the dataset
data <- read_csv("teleco_time_series.csv")

# --- Part III: Data Preparation ---

# C1: Visualize the time series with a line graph
ggplot(data, aes(x = Day, y = Revenue)) +
  geom_line() +
  labs(title = "Daily Revenue", 
       x = "Day", 
       y = "Revenue") +
  theme_minimal()

# C2: Describe time step formatting (gaps and sequence length)
print(diff(data$Day)) # No gaps
total_days<- nrow(data)
print(total_days) #731 total days of revenue


# C3: Evaluate stationarity using the Augmented Dickey-Fuller (ADF) test
adf.test(data$Revenue)

# C4: Prepare data by splitting into training and test sets (80% train, 20% test)
train_size <- floor(0.8 * total_days)
test_size <- total_days - train_size

# Convert to time series object (daily data with yearly seasonality)
ts_data <- ts(data$Revenue, frequency = 365)
str(ts_data)

# Define training and test sets
ts_train <- window(ts_data, start = 1, end = train_size / 365)
ts_test <- window(ts_data, start = (train_size + 1) / 365, end = total_days / 365)

#p-value is less than 0.5 and is statistically stationary. However, there is a visual upward trend 
df <- diff(data$Revenue)
plot(df)

# C5: Provide the cleaned dataset
cleaned_ts_data <- data
write.csv(cleaned_ts_data, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Clean/cleaned_ts_data.csv", row.names = FALSE)

# --- Part IV: Model Identification and Analysis ---

# D1: Analyze the time series with visualizations
# Decomposition to identify components
decomp <- decompose(ts_data)
plot(decomp)

# Autocorrelation Function (ACF)
acf(ts_data, main = "ACF of Revenue")

# Partial Autocorrelation Function (PACF)
pacf(ts_data, main = "PACF of Revenue")

# Spectral density to identify dominant frequencies
spectrum(ts_data, main = "Spectral Density of Revenue")

# Check residuals for trends after decomposition
residuals <- decomp$random
adf.test(residuals[!is.na(residuals)]) #p-value is less than .05 suggesting data is stationary

# D2: Identify an ARIMA model
model <- auto.arima(ts_train)

# D3: Perform forecast for the test set length
forecast_horizon <- test_size
forecast_result <- forecast(model, h = forecast_horizon)

# D4: Provide model and forecast outputs
summary(model)
forecast_result

# D5: Code for the time series model is included above

# --- Part V: Data Summary and Implications ---

# E1: Model evaluation metrics
accuracy_metrics <- accuracy(forecast_result, ts_test)
accuracy_metrics


# E2: Visualization of forecast compared to test set
plot(forecast_result, main = "Revenue Forecast vs Actual Test Data",
     xlab = "Time", ylab = "Revenue (in USD)", col = "blue")
lines(ts_test, col = "red")
legend("topleft", legend = c("Forecast", "Actual Test Data"), 
       col = c("blue", "red"), lty = 1)

