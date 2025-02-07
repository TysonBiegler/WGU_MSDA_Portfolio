options(warn = -1) #disable warning

library(tidyverse)
library(tidymodels)


# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/task 1')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


# Research Question -------------------------------------------------------

# Which factors are most strongly associated with customer churn, and how do they influence the likelihood of churn?

# Exploring the Data ------------------------------------------------------
#getting a general understanding of the data

glimpse(churn)

#checking for na and duplicates
sum(is.na(churn))
sum(duplicated(churn))


# Determining number of neighbors (K) -------------------------------------
# K = Square Root (Number of Rows)
K <- sqrt(nrow(churn))