# Initial Setup -----------------------------------------------------------

install.packages("gridExtra")
install.packages("janitor")

library(tidyverse)
library(MASS)
library(car)

library(corrplot)

library(performance)
library(ggeffects)
library(sjPlot)
library(gtsummary)
library(flextable)
library(gridExtra)



tidyverse_packages(include_self = TRUE)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/R/Raw')


# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


set.seed(0)


# Explore Data ------------------------------------------------------------
str(churn)
summary(churn)
sum(is.na(churn))
sum(duplicated(churn))

# Research Question -------------------------------------------------------

#"What factors impact customer tenure?"


