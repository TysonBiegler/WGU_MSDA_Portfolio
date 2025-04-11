# Tyson Biegler
# Student ID: 012170282
# Advanced Data Analytics - D213 Task 1

# Initial setup -----------------------------------------------------------
# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(forecast)   # For time series forecasting and ARIMA models
library(tseries)    # For stationarity tests (e.g., ADF test)
library(ggplot2)    # For enhanced plotting capabilities
library(lubridate)
library(zoo) #for converting to date
library(xts)

# Set working directory (adjust path to your dataset location)
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Analytics - D213/Task 1/Raw')

# Load the dataset
data <- read_csv("teleco_time_series.csv")

sum(is.na(data)) #checking for missing values

data$Date <- as.Date("2023-01-01") + (data$Day - 1) #converting Day to a date
str(data)

xts_data <- xts(data$Revenue, data$Date)

#view(xts_data)

colnames(xts_data) <- c('Revenue')

plot(tail(diff(xts_data), 90), main = "Last 90 Differenced Values", type = "l") 

xts_diff <- diff(xts_data)

#convert to timeSeries
ts_data <- ts(xts_data, frequency = 365)

plot(ts_data)

plot(decompose(ts_data))


#ARIMA
auto.arima(ts_data, stepwise = TRUE, trace = TRUE)

results<-arima(ts_data, order = c(1,1,0))
arima(ts_data, order = c(2,1,2))
arima(ts_data, order = c(2,1,0))

xts_fitted <- xts(fitted(results),index(ts_data))
