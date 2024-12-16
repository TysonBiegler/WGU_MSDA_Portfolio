# Initial Setup -----------------------------------------------------------

# Install required packages. only installs if not already installed. 
# Data manipulation and visualization
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")  

# Stepwise regression (stepAIC)
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")    

# Multicollinearity diagnostics (VIF)
if (!requireNamespace("car", quietly = TRUE)) install.packages("car") 

# Model  diagnostics and assumption checks
if (!requireNamespace("performance", quietly = TRUE)) install.packages("performance") 

# Train-test splits and model utility functions
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret") 

# Summary tables for models
if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")

# Visualize model predictions
if (!requireNamespace("ggeffects", quietly = TRUE)) install.packages("ggeffects")

# Create polished tables for reports
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")  



library(tidyverse)    
library(MASS)         # Stepwise regression with (stepAIC)
library(car)          # Multicollinearity with (VIF)
library(performance)  # Model diagnostics and assumption checks
library(caret)        # Train-test splits
library(gtsummary)    # Summary tables for models
library(ggeffects)    # Visualize model predictions
library(flextable)    # Create tables

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/R/Raw')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())

# Research Question -------------------------------------------------------

#"What factors impact customer tenure?"

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
                ~ as.factor(ifelse(trimws(.) == 'Yes', 1, 0))))

# Convert Specific Columns to Factors
churn <- churn %>%
  mutate_at(vars(City,
                 State,
                 Gender,
                 Marital,
                 Contract,
                 Area,
                 PaymentMethod,
                 InternetService
  ),
  as.factor)

# Convert Specific Columns to Numeric
churn <- churn %>%
  mutate_at(vars(Tenure,
                 MonthlyCharge,
                 Bandwidth_GB_Year,
                 Outage_sec_perweek,
                 Children,
                 Age,
                 Population,
                 Email,
                 Contacts,
                 Yearly_equip_failure
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

glimpse(churn)
str(churn)


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



#Removing columns im not going to use
churn <- churn[, !(names(churn) %in% c("CaseOrder", 
                                       "Customer_id", 
                                       "Interaction",
                                       "UID",
                                       "Lat", 
                                       "Lng",
                                       "County", 
                                       "Zip", 
                                       "TimeZone", 
                                       "Job",
                                       "City",
                                       "State" 
))]

str(churn)

write.csv(churn, "CLEANED_churn.csv")



# Correlations  -----------------------------------------------------------
#Continuous variables

# Subset numeric columns
numeric_vars <- churn[sapply(churn, is.numeric)]

# Compute correlations with MonthlyCharge
correlations <- sapply(names(numeric_vars), function(var) {
  cor(numeric_vars[[var]], churn$Tenure, use = "complete.obs")
})

# Filter significant correlations
significant_continuous_vars <- names(correlations[abs(correlations) > 0.1])
print("Significant continuous variables:")
print(significant_continuous_vars)




#Categorical variables
# Subset categorical variables
categorical_vars <- churn[sapply(churn, is.factor)]

# Perform Kruskal-Wallis test for each categorical variable
kruskal_results <- lapply(names(categorical_vars), function(var) {
  formula <- as.formula(paste("Tenure ~", var))
  kruskal_result <- kruskal.test(formula, data = churn)
  kruskal_result$p.value
})

# Extract p-values and filter significant variables (adjust threshold to 0.1)
kruskal_p_values <- unlist(kruskal_results)
significant_categorical_vars <- names(which(kruskal_p_values < 0.1))

print("Significant categorical variables from Kruskal-Wallis test with adjusted threshold:")
print(significant_categorical_vars)



install.packages("pheatmap")
install.packages("reshape2")
library(reshape2)
library(pheatmap)

# Get numeric columns
numeric_columns <- churn %>%
  select(where(is.numeric))

# Calculate correlation matrix
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Convert correlation matrix to long format for ggplot (if needed)
cor_melted <- melt(correlation_matrix)

# Plot heatmap using pheatmap
pheatmap(correlation_matrix, display_numbers = TRUE, 
         color = colorRampPalette(c("blue", "white", "red"))(50), 
         main = "Correlation Matrix Heatmap")












