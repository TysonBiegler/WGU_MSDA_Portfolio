#####################################
# Tyson Biegler
# Student ID: 012170282
# Data Analytics Graduate Capstone - D214 Task 1
#####################################

#SOURCE: 
#Data: https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset

#####################################
# Initial setup 
#####################################
# Load necessary libraries
library(tidyverse)
library(prophet)
library(lubridate)

# set the wd and load the data
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Analytics Graduate Capstone - D214/Raw/Datasets/bike_share')
data <- read_csv("day.csv")

# ensure that the date is the correct format
data$dteday <- ymd(data$dteday)

# plot data
qplot(dteday, cnt, data = data,
      main = "Bike Rentals in Washington DC",
      xlab = "Date", ylab = "Count of Rentals")

# prepare dataframe
df <- data.frame(
  ds = data$dteday,
  y = data$cnt,
  temp = data$temp,
  hum = data$hum
)

# Build Enhanced Model (with holidays, temp, hum)
m_enhanced <- prophet()
m_enhanced <- add_country_holidays(m_enhanced, country_name = 'US')
m_enhanced <- add_regressor(m_enhanced, 'temp')
m_enhanced <- add_regressor(m_enhanced, 'hum')
m_enhanced <- fit.prophet(m_enhanced, df)

# Build Baseline Model (without holidays, temp, hum)
df_base <- df[, c("ds", "y")]  # only date and y
m_base <- prophet()
m_base <- fit.prophet(m_base, df_base)

# Cross-validation for both models
cv_enhanced <- cross_validation(m_enhanced, initial = 540, period = 30, horizon = 90, units = "days")
cv_base <- cross_validation(m_base, initial = 540, period = 30, horizon = 90, units = "days")

# Performance metrics
perf_enhanced <- performance_metrics(cv_enhanced)
perf_base <- performance_metrics(cv_base)

# Paired t-test on RMSE
t_test_result <- t.test(perf_base$rmse, perf_enhanced$rmse, paired = TRUE)
print(t_test_result)

# visualize RMSE comparison
boxplot(perf_base$rmse, perf_enhanced$rmse,
        names = c("Baseline", "Enhanced"),
        main = "RMSE Comparison",
        ylab = "RMSE")

# Forecast plot from Enhanced Model
future <- make_future_dataframe(m_enhanced, periods = 90)

# Sample from Jan–Mar historical data since that is what is being forecasted
winter_data <- data %>% filter(month(dteday) %in% c(1, 2, 3))

# Add temp and hum to future
future$temp <- c(df$temp, runif(90, min(winter_data$temp), max(winter_data$temp)))
future$hum <- c(df$hum, runif(90, min(winter_data$hum), max(winter_data$hum)))


# Forecast
forecast <- predict(m_enhanced, future)

# Plot forecast
plot(m_enhanced, forecast) +
  xlab("Date") + ylab("Count of Rentals") + ggtitle("Bike Rentals Forecast")

# Prophet components
prophet_plot_components(m_enhanced, forecast)
