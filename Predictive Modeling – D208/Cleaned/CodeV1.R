#Tyson Biegler
#D208 Task1
#Student ID: 012170282


# Initial Setup -----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("MASS")# For stepwise regression (stepAIC)
#install.packages("car") # For VIF calculation (detecting multicollinearity)
#install.packages("ggfortify")# For visualizing model diagnostic plots
#install.packages("gridExtra")# For arranging multiple ggplot graphs in a grid
#install.packages("performance")# For checking model assumptions
#install.packages("caTools")

library(tidyverse) # Includes ggplot2, dplyr, readr, and more for data manipulation and visualization
library(ggplot2)
library(MASS) # For stepwise model selection using stepAIC()
library(car) # For calculating VIF (Variance Inflation Factor)
library(ggfortify) # For diagnostic plots of linear models
library(gridExtra) # For arranging multiple plots in a grid
library(performance) # For checking model assumptions visually (check_model)
library(caTools)


# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Raw')

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
                  PaperlessBilling
  ),
  ~ as.factor(ifelse(trimws(.) == 'Yes', 1, 0))))


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
                                       "TimeZone", 
                                       "Job",
                                       "City",
                                       "State",
                                       "Zip"
))]

str(churn)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Cleaned')

write.csv(churn, "CLEANED_churn.csv")

# Create Test and Train sets ----------------------------------------------

set.seed(123)
split <- sample.split(churn$Tenure, SplitRatio = 0.7)
training_set <- subset(churn, split == TRUE)
test_set <- subset(churn, split == FALSE)



# Initial model --------------------------------------------------------

Initial_model <- lm(Tenure ~ ., data = training_set)

summary(Initial_model)

Initial_model <- stepAIC(object = Initial_model, direction = "both") #(Larose & Larose, 2019)

summary(Initial_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(Initial_model)


# Reduced model ------------------------------------------------------------
vif_values <- vif(Initial_model)
vif_values #Looking for VIF values above 5. 

# Removed MonthlyCharge since it was such a high VIF and then i will check VIF again to see if the others are ok
reduced_model <- lm(formula = Tenure ~ Area + Children + Age + Gender + InternetService + Multiple + OnlineSecurity + 
                      OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + 
                      PaperlessBilling + PaymentMethod + Bandwidth_GB_Year, data = churn)

#checking to make sure all high VIF values have been removed
vif_values <- vif(reduced_model)
vif_values #Looking for VIF values above 5 again. 

summary(reduced_model)

reduced_model <- stepAIC(object = reduced_model, direction = "both")

reduced_model <- lm(formula = Tenure ~ Children + Age + Gender + InternetService + 
                      Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                      TechSupport + StreamingTV + StreamingMovies + Bandwidth_GB_Year, 
                    data = churn)

#plotting the reduced model
par(mfrow = c(2, 2))
plot(reduced_model)

summary(reduced_model)

#Getting the predicted values with the test data
y_pred = predict(reduced_model, newdata = test_set)
#print the predicted values
y_pred
view(test_set)

# Comparing models --------------------------------------------------------

#Using non-parametric test due to normality assumption being violated
# Geting the residuals from both models
residuals_initial <- resid(Initial_model)
residuals_reduced <- resid(reduced_model)

# Combining the residuals into one data frame
residuals_df <- data.frame(
  Residuals = c(residuals_initial, residuals_reduced),
  Model = rep(c("Initial_model", "Reduced_model"), 
              times = c(length(residuals_initial), length(residuals_reduced)))
)

# comparing the residuals of each model.  Anova assumes normal dist. So im using a non parametric test instead. 
kruskal.test(Residuals ~ Model, data = residuals_df)


# univariate plots --------------------------------------------------------
# Continuous variables
univariate1 <- ggplot(churn, aes(x = Tenure)) + 
  geom_histogram(bins = 20, fill = "#e28743", alpha = 0.8) + 
  ggtitle("Tenure")

univariate2 <- ggplot(churn, aes(x = Children)) + 
  geom_histogram(bins =11, fill = "#e28743", alpha = 0.8) + 
  ggtitle("Children")

univariate3 <- ggplot(churn, aes(x = Age)) + 
  geom_histogram(bins = 20, fill = "#e28743", alpha = 0.8) + 
  ggtitle("Age")

univariate4 <- ggplot(churn, aes(x = Bandwidth_GB_Year)) + 
  geom_histogram(bins = 20, fill = "#e28743", alpha = 0.8) + 
  ggtitle("Bandwidth_GB_Year")

univariate5 <- ggplot(churn, aes(x = Gender)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle("Gender")

univariate6 <- ggplot(churn, aes(x =  InternetService)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle(" InternetService")

univariate7 <- ggplot(churn, aes(x = Multiple)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle("Multiple")

univariate8 <- ggplot(churn, aes(x = OnlineSecurity)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle("OnlineSecurity")

univariate9 <- ggplot(churn, aes(x = OnlineBackup)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle("OnlineBackup")

univariate10 <- ggplot(churn, aes(x = DeviceProtection)) +
  geom_bar(fill = "#e28743", alpha = 0.8) +
  ggtitle("DeviceProtection")


# Arrange all plots into a grid
gridExtra::grid.arrange(
  univariate1, univariate2, 
  univariate3, univariate4,
  univariate5, univariate6, 
  univariate7, univariate8,
  univariate9, univariate10,
  ncol = 2
)



# bivariate plots ---------------------------------------------------------
# Categorical variables

bivariate1 <- ggplot(churn, aes(x = Bandwidth_GB_Year, y = Tenure)) + 
  geom_point(color = "#154c79", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs Bandwidth_GB_Year") +
  xlab("Bandwidth_GB_Year") +
  ylab("Tenure")

bivariate2 <- ggplot(churn, aes(x = Children, y = Tenure)) +
  geom_point(color = "#154c79", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs Children") +
  xlab("Number of Children") +
  ylab("Tenure")

bivariate3 <- ggplot(churn, aes(x = Age, y = Tenure)) +
  geom_point(color = "#154c79", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs Age") +
  xlab("Age") +
  ylab("Tenure")

bivariate4 <- ggplot(churn, aes(x = as.factor(Gender), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs Gender") +
  xlab("Gender") +
  ylab("Tenure")

bivariate5 <- ggplot(churn, aes(x = as.factor(InternetService), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs InternetService") +
  xlab("InternetService (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate6 <- ggplot(churn, aes(x = as.factor(Multiple), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs Multiple") +
  xlab("Multiple (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate7 <- ggplot(churn, aes(x = as.factor(OnlineSecurity), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs OnlineSecurity") +
  xlab("OnlineSecurity (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate8 <- ggplot(churn, aes(x = as.factor(OnlineBackup), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs OnlineBackup") +
  xlab("OnlineBackup (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate9 <- ggplot(churn, aes(x = as.factor(DeviceProtection), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs DeviceProtection") +
  xlab("DeviceProtection (0 = No, 1 = Yes)") +
  ylab("Tenure")




# Arrange all plots into a grid
gridExtra::grid.arrange(
  bivariate1, bivariate2, bivariate3,
  bivariate4, bivariate5, bivariate6,
  bivariate7, bivariate8, bivariate9,
  ncol = 3
)