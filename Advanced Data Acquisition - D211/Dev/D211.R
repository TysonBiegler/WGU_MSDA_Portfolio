#SOURCES:
#https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*cnaw4s*_ga*Mjc2MTgyNDI2LjE3MjI4OTk3NjY.*_ga_J4698JNNFT*MTcyMjg5OTc2NS4xLjEuMTcyMjkwMzAzNy40NC4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNl0sImRhdGEiOltbIlRhYmxlSWQiLCI1MjUiXSxbIk1ham9yX0FyZWEiLCIwIl0sWyJTdGF0ZSIsWyIwIl1dXX0=11

# Tyson Biegler
# Student ID: 012170282
# D211 Advanced Data Acquisition

#Initial Setup------------------------------------------------------------------
# Setting Working Directory

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Acquisition - D211/Raw')

# Install and load packages
install.packages('tidyverse')
install.packages("factoextra")

# Load necessary libraries
library(tidyverse)
library(plyr)
library(factoextra)
library(stats)

# Loading data from CSV
churn <- read_csv('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Acquisition - D211/Raw/churn_clean.csv')
Alternative <- read_csv('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Advanced Data Acquisition - D211/Raw/SAPCE2.csv')


glimpse(churn)#quick look at the data

glimpse(Alternative)

