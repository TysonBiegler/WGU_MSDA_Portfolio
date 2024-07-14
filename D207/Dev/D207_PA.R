# Tyson Biegler
# Student ID: 012170282
# D207 Exploratory Data Analysis

#Initial Setup------------------------------------------------------------------
# Setting Working Directory
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Dev')


# Installing Packages
install.packages("tidyverse")

# Loading Libraries
library(tidyverse)

# Importing CSV
churn <- read_csv("C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Raw/churn_clean.csv")
glimpse(churn)#quick look at the data


# A1. ---- 
  #Which independent variables are associated with churn ----

# A3. ---- 
  #clean data size: 2,1xx rows, 28 columns. Describe dependent variable. Identify the independent variables state which categorical, which are continuous. Which statistical method will you use? Why? 
  
# B1. ---- 
  #and B2 provide statistical analysis. show the code!  Chi square (maybe do a loop and loop through the categorical variables rather than doing just one through the dependant) “Heres my dependent variable churn, here are the independent categorical variables and the p values.. Of those these 3 have the only significant p values”

# B3. ---- 
  #chi-square is performed using the schipy package and requires no normality trest liekt he other methods. Enables the analyst to…

# C1. ---- 
  #4 univariate graphs : 2 categorical and 2 continuous

# D1. ---- 
  #2 Bivariate graphs : 1 categorical and 1 continuous

# E1. ---- 
  #Talk about the hypothesis test (null alt, findings) was there a significant difference? What might the bivariate analysis indicate?



















# Export cleaned data to CSV ---------------------------------------------------
write.csv(churn, 'C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D207/Cleaned/churn_cleaned_data.csv', row.names = FALSE)
