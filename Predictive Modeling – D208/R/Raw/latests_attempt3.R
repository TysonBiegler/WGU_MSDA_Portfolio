
# Initial Setup -----------------------------------------------------------
install.packages("tidyverse")
install.packages("MASS")# For stepwise regression (stepAIC)
install.packages("car") # For VIF calculation (detecting multicollinearity)
install.packages("ggeffects") # For visualizing model predictions
install.packages("ggfortify") # For visualizing model diagnostic plots
install.packages("gridExtra") # For arranging multiple ggplot graphs in a grid
install.packages("performance") # For checking model assumptions
install.packages("caret") # For data splitting into training and testing subsets 

library(tidyverse) # Includes ggplot2, dplyr, readr, and more for data manipulation and visualization
library(MASS) # For stepwise model selection using stepAIC()
library(car) # For calculating VIF (Variance Inflation Factor)
library(ggeffects) # For creating prediction plots from regression models
library(ggfortify) # For diagnostic plots of linear models
library(gridExtra) # For arranging multiple plots in a grid
library(performance) # For checking model assumptions visually (check_model)
library(caret)

tidyverse_packages(include_self = TRUE)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/OneDrive/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/R/Raw')

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



# Clearn/Prepare Data -----------------------------------------------------
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

names(churn)


# Converting to Logical Data Type
churn <- churn %>%
  mutate(across(c(Churn, Techie, Port_modem, Tablet, Phone, Multiple,
                  OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
                  StreamingTV, StreamingMovies, PaperlessBilling),
                ~ as.factor(ifelse(trimws(.) == 'Yes', 1, 0))))

# Convert Specific Columns to Factors
churn <- churn %>%
  mutate_at(vars(Area, 
                 Marital, 
                 Gender,
                 Contract,
                 InternetService,
                 PaymentMethod, 
                 Churn,
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
                 PaperlessBilling,
                 City,
                 State,
                 Timely_response,
                 Timely_fixes,
                 Timely_replacements,
                 Reliability,
                 Options,
                 Respectful,
                 Courteous,
                 Active_listening
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

churn <- churn %>% 
  mutate_at(vars(Children,
                 Age,
                 Population),
            as.integer)




glimpse(churn)
str(churn)

#removing columns im not going to use
churn <- churn[, !(names(churn) %in% c("CaseOrder", 
                                       "Customer_id", 
                                       "Interaction",
                                       "UID",
                                       "Lat", 
                                       "Lng",
                                       "County", 
                                       "Zip",
                                       "Area", 
                                       "TimeZone", 
                                       "Job",
                                       "City",
                                       "State"
                                       ))]



str(churn)

# Select Variables --------------------------------------------------------

Initial_model <- lm(Tenure ~ ., data = churn)

summary(Initial_model)

stepwise_model <- stepAIC(object = Initial_model, direction = "backward") #(Larose & Larose, 2019)

stepwise_model

summary(stepwise_model) #(Larose & Larose, 2019)
plot(stepwise_model)

stepwise_model <- lm(formula = Tenure ~ Children + Age + Gender + InternetService + 
     Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
     TechSupport + StreamingTV + StreamingMovies + PaperlessBilling + 
     MonthlyCharge + Bandwidth_GB_Year, data = churn)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(stepwise_model)
summary(stepwise_model)

# Reduce model ------------------------------------------------------------
vif_values <- vif(stepwise_model)
vif_values #Looking for VIF values above 5. 

# Removed StreamingTV, StreamingMovies, and MonthlyCharge
vif_model <- lm(formula = Tenure ~ Children + Age + Gender + InternetService + 
                      Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                      TechSupport + PaperlessBilling + Bandwidth_GB_Year, data = churn)

vif_values <- vif(vif_model)
vif_values #Looking for VIF values above 5. 

plot(vif_model)
summary(vif_model)

#removing StreamingTV, StreamingMovies, and MonthlyCharge because of a VIF above 5
updated_model <- lm(formula = Tenure ~ Children + Age + Gender + InternetService + 
                      Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                      Bandwidth_GB_Year, data = churn)

vif_values <- vif(updated_model)
vif_values #Looking for VIF values above 5. 

plot(updated_model)
summary(updated_model)

# Checking assumptions with visuals
check_model(updated_model)

# Visualize model predictions
ggeffect(updated_model)

summary(updated_model)

autoplot(updated_model, which = 1:6, ncol = 2, label.size = 3)

updated_model

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

# Arrange all plots into a grid
gridExtra::grid.arrange(
  univariate1, univariate2, 
  univariate3, univariate4,
  ncol = 2
)




# bivariate plots ---------------------------------------------------------
# Categorical variables
bivariate1 <- ggplot(churn, aes(x = MonthlyCharge, y = Tenure)) + 
  geom_point(color = "#154c79", alpha = 1/10) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs MonthlyCharge") +
  xlab("MonthlyCharge") +
  ylab("Tenure")

bivariate2 <- ggplot(churn, aes(x = Bandwidth_GB_Year, y = Tenure)) + 
  geom_point(color = "#154c79", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs Bandwidth_GB_Year") +
  xlab("Bandwidth_GB_Year") +
  ylab("Tenure")

bivariate3 <- ggplot(churn, aes(x = Children, y = Tenure)) +
  geom_point(color = "#154c79", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#e28743") +
  ggtitle("Tenure vs Children") +
  xlab("Number of Children") +
  ylab("Tenure")

bivariate4 <- ggplot(churn, aes(x = as.factor(Churn), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs Churn") +
  xlab("Churn (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate5 <- ggplot(churn, aes(x = as.factor(Multiple), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs Multiple") +
  xlab("Multiple (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate6 <- ggplot(churn, aes(x = as.factor(OnlineSecurity), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs OnlineSecurity") +
  xlab("OnlineSecurity (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate7 <- ggplot(churn, aes(x = as.factor(OnlineBackup), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs OnlineBackup") +
  xlab("OnlineBackup (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate8 <- ggplot(churn, aes(x = as.factor(DeviceProtection), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs DeviceProtection") +
  xlab("DeviceProtection (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate9 <- ggplot(churn, aes(x = as.factor(TechSupport), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs TechSupport") +
  xlab("TechSupport (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate10 <- ggplot(churn, aes(x = as.factor(StreamingTV), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs StreamingTV") +
  xlab("StreamingTV (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate11 <- ggplot(churn, aes(x = as.factor(StreamingMovies), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs StreamingMovies") +
  xlab("StreamingMovies (0 = No, 1 = Yes)") +
  ylab("Tenure")

bivariate12 <- ggplot(churn, aes(x = as.factor(PaperlessBilling), y = Tenure)) +
  geom_boxplot(fill = "#e28743", alpha = 0.7) +
  ggtitle("Tenure vs PaperlessBilling") +
  xlab("PaperlessBilling (0 = No, 1 = Yes)") +
  ylab("Tenure")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  bivariate1, bivariate2, bivariate3,
  bivariate4, bivariate5, bivariate6,
  bivariate7, bivariate8, bivariate9,
  bivariate10, bivariate11, bivariate12,
  ncol = 3
)