
#https://www.youtube.com/watch?v=_yNWzP5HfGw


#install.packages("tidyverse")

library(tidyverse)
library(tidymodels)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Raw/task2')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


# Research Question -------------------------------------------------------

#"What efffect does x have on y"

# Exploring the Data ------------------------------------------------------
#getting a general understanding of the data

glimpse(churn)

#checking for na and duplicates
sum(is.na(churn))
sum(duplicated(churn))

# Clearn/Prepare Data -----------------------------------------------------

# Convert Specific Columns to binary/factor
churn <- churn %>%
  mutate(across(c(Churn, 
                  Techie, 
                  Port_modem, 
                  Tablet, 
                  Phone, 
                  Multiple,
                  OnlineSecurity, 
                  OnlineBackup, 
                  DeviceProtection, 
                  TechSupport,
                  StreamingTV, 
                  StreamingMovies, 
                  PaperlessBilling),
                ~ as.integer(. == 'Yes')))

# Convert Specific Columns to Factors
churn <- churn %>%
  mutate_at(vars(City,
                 State,
                 Zip,
                 Gender,
                 Marital,
                 Contract,
                 Area,
                 County,
                 PaymentMethod,
                 InternetService
  ),
  as.factor)

# Convert Specific Columns to Numeric
churn <- churn %>%
  mutate_at(vars(Tenure,
                 MonthlyCharge,
                 Bandwidth_GB_Year,
                 Outage_sec_perweek
  ),
  as.numeric)

# Convert Specific Columns to Integer
churn <- churn %>% 
  mutate_at(vars(Children,
                 Age,
                 Population,
                 Email,
                 Contacts,
                 Yearly_equip_failure),
            as.integer)

#renaming the survey response columns to be more intuitive
churn <- churn %>%
  mutate_at(vars(43:50), as.factor) %>%
  rename_at(vars(43:50), ~ c(
    "Timely_response",
    "Timely_fixes",
    "Timely_replacements",
    "Reliability",
    "Options",
    "Respectful",
    "Courteous",
    "Active_listening"
  ))

glimpse(churn)
str(churn)


#Removing columns im not going to use
churn <- churn[, !(names(churn) %in% c("CaseOrder", 
                                       "Customer_id", 
                                       "Interaction",
                                       "UID",
                                       "Lat", 
                                       "Lng",
                                       "County",
                                       "TimeZone", 
                                       "Job",
                                       "City",
                                       "State",
                                       "Zip"
))]

str(churn)

# Convert Specific Columns to Numeric
churn <- churn %>%
  mutate_at(vars(Tenure,
                 MonthlyCharge,
                 Bandwidth_GB_Year,
                 Outage_sec_perweek
  ),
  as.numeric)

# Convert Specific Columns to Integer
churn <- churn %>% 
  mutate_at(vars(Children,
                 Age,
                 Population,
                 Email,
                 Contacts,
                 Yearly_equip_failure),
            as.integer)



# The initial model -------------------------------------------------------

set.seed(123)

split <- initial_split(churn,
                       prop = .80,
                       strata = Churn)

churn_train <- training(split)
churn_test <- testing(split)


# visualize the data ------------------------------------------------------

ggplot(churn_train, aes(x = MonthlyCharge,
                        y = Churn)) + 
  geom_jitter(height = .05,
              alpha = .5)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)
