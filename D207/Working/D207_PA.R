# Tyson Biegler
# Student ID: 012170282
# D207 Exploratory Data Analysis

#Initial Setup------------------------------------------------------------------
    # Setting Working Directory
        setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Working')

install.packages("tidyverse")

# Export cleaned data to CSV ---------------------------------------------------
      write.csv(churn, "C:/Users/tyson/WGU/R/D206_PA/Cleaned/churn_cleaned_data_final.csv", row.names = FALSE)
