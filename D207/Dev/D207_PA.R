# Tyson Biegler
# Student ID: 012170282
# D207 Exploratory Data Analysis

#Initial Setup------------------------------------------------------------------
    # Setting Working Directory
        setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Dev')

install.packages("tidyverse")

library(tidyverse)


churn <- read_csv("C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Raw/churn_clean.csv")
glimpse(churn)#quick look at the data


# Export cleaned data to CSV ---------------------------------------------------
write.csv(churn, 'C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Cleaned/churn_cleaned_data.csv', row.names = FALSE)
