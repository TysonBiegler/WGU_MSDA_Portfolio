# SOURCES
# https://www.youtube.com/watch?v=_yNWzP5HfGw
# https://www.youtube.com/watch?v=mMm8Xt0HUkI&t=3113s
# https://www.youtube.com/watch?v=QyvYrVoZ-sk
# https://www.youtube.com/watch?v=y4FY0KNJ6nk&t=1466s

#install.packages("tidyverse")

library(tidyverse)
library(tidymodels)
library(MASS)
library(car)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Raw/task2')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


# Research Question -------------------------------------------------------

# What variabales contribute most to Churn?

# Exploring the Data ------------------------------------------------------
#getting a general understanding of the data

glimpse(churn)

#checking for na and duplicates
sum(is.na(churn))
sum(duplicated(churn))

# Clearn/Prepare Data -----------------------------------------------------

# Convert Specific Columns to Binary Factor with levels 1 and 0
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
                ~ factor(. == 'Yes', levels = c(FALSE, TRUE), labels = c(0, 1))))


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




# Scaling variables -------------------------------------------------------

churn$MonthlyCharge <- scale(churn$MonthlyCharge, center = TRUE, scale = TRUE)

churn$Income <- scale(churn$Income, center = TRUE, scale = TRUE)

churn$Population <- scale(churn$Population, center = TRUE, scale = TRUE)

churn$Outage_sec_perweek <- scale(churn$Outage_sec_perweek, center = TRUE, scale = TRUE)

churn$Tenure <- scale(churn$Tenure, center = TRUE, scale = TRUE)

churn$Bandwidth_GB_Year <- scale(churn$Bandwidth_GB_Year, center = TRUE, scale = TRUE)

str(churn)


# The initial model -------------------------------------------------------

set.seed(123)

split <- initial_split(churn,
                       prop = .80,
                       strata = Churn)

churn_train <- training(split)
churn_test <- testing(split)

# initial model -----------------------------------------------------------

initial_model <- glm(Churn ~. , data = churn_train, family = 'binomial')
summary(initial_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(initial_model)

initial_model <- stepAIC(object = initial_model, direction = "backward")
summary(initial_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(initial_model)

#################################

initial_model <- glm(formula = Churn ~ Children + Age + Techie + Contract + InternetService + 
                       Phone + Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                       StreamingTV + StreamingMovies + PaperlessBilling + PaymentMethod + 
                       Tenure + MonthlyCharge + Bandwidth_GB_Year, family = "binomial", 
                     data = churn_train)

summary(initial_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(initial_model)

#################################
vif_values <- vif(initial_model)
vif_values #Looking for VIF values above 5. 

# Removed Bandwidth_GB_Year because of a VIF of 2656.593644 and MonthlyCharge with 16.889045
initial_model <- glm(formula = Churn ~ Children + Age + Techie + Contract + InternetService + 
                       Phone + Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                       StreamingTV + StreamingMovies + PaperlessBilling + PaymentMethod + 
                       Tenure, family = "binomial", 
                     data = churn_train)

summary(initial_model)

# Removed values that did not have a statistically significant coefficient
reduced_model <- glm(formula = Churn ~ Techie + Contract + InternetService + 
                       Phone + Multiple + OnlineBackup + DeviceProtection + 
                       StreamingTV + StreamingMovies + PaymentMethod + 
                       Tenure, family = "binomial", 
                     data = churn_train)
summary(reduced_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(reduced_model)


#################################

# predict -----------------------------------------------------------------
p1 <- predict(reduced_model, churn_train, type='response')
p2 <- predict(reduced_model, churn_test, type = 'response')

# confusion matrix --------------------------------------------------------

pred1 <- ifelse(p1 > 0.5, 1, 0) #if p1 is greater than 0.5 then return 1 else 0
pred2 <- ifelse(p2 > 0.5, 1,0)


table(Predicted = pred1, Actual = churn_train$Churn) #confusion matrix
# (5401+1702)/8000 = 0.90375 model is 90.375% accurate
# 1 - 0.90375 = 0.0965 misclassification error. 9.65% of the time the prediction is wrong

table(Predicted = pred2, Actual = churn_test$Churn) #confusion matrix
# (1349+416)/2000 = 0.9025 The model is 90.25% accurate in its predictions
# 1 - 0.9025 = 0.0.975 misclassification error. 9.75% of the time the prediction is wrong 

#Both models are similar in accuracy and misclassification

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Cleaned/task2')

write.csv(churn, "CLEANED_churn.csv")

# Plots -------------------------------------------------------------------
# univariate plots --------------------------------------------------------

univariate1 <- ggplot(churn, aes(x = Age)) + 
  geom_histogram(bins = 30, fill = "#e28743") + 
  ggtitle("Age")

univariate2 <- ggplot(churn, aes(x = Techie)) + 
  geom_bar(fill = "#e28743") + 
  ggtitle("Techie")

univariate3 <- ggplot(churn, aes(x = Contract)) +
  geom_bar(fill = "#e28743") +
  ggtitle("Contract")

univariate4 <- ggplot(churn, aes(x = Multiple)) +
  geom_bar(fill = "#e28743") +
  ggtitle("Multiple")

univariate5 <- ggplot(churn, aes(x = OnlineSecurity)) +
  geom_bar(fill = "#e28743") +
  ggtitle("OnlineSecurity")

univariate6 <- ggplot(churn, aes(x = StreamingTV)) +
  geom_bar(fill = "#e28743") +
  ggtitle("StreamingTV")

univariate7 <- ggplot(churn, aes(x = StreamingMovies)) +
  geom_bar(fill = "#e28743") +
  ggtitle("StreamingMovies")

univariate8 <- ggplot(churn, aes(x = PaymentMethod)) +
  geom_bar(fill = "#e28743") +
  ggtitle("PaymentMethod")

univariate9 <- ggplot(churn, aes(x = MonthlyCharge)) +
  geom_histogram(bins = 30, fill = "#e28743") + 
  ggtitle("MonthlyCharge")

univariate10 <- ggplot(churn, aes(x = Churn)) + 
  geom_bar(fill = "#e28743") + 
  ggtitle("Churn")

univariate10 <- ggplot(churn, aes(x = Churn)) + 
  geom_bar(fill = "#e28743") + 
  ggtitle("Churn")

univariate11 <- ggplot(churn, aes(x = Children)) + 
  geom_histogram(bins = 30, fill = "#e28743") + 
  ggtitle("Children")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  univariate1, univariate2, 
  univariate3, univariate4,
  univariate5, univariate6, 
  univariate7, univariate8,
  univariate9, univariate10,
  univariate11,
  ncol = 2
)



# bivariate plots ---------------------------------------------------------

bivariate1 <- ggplot(churn, aes(x = Children, fill = Churn)) +
  geom_histogram(alpha = 0.8, position = "identity", bins = 30) +
  ggtitle("Children by Churn") +
  xlab("Number of Children") +
  ylab("Count") +
  scale_fill_manual(values = c("0" = "#D55E00", "1" = "#0072B2"), name = "Churn")


bivariate2 <- ggplot(churn, aes(x = Age, fill = Churn)) +
  geom_histogram(alpha = 0.8, position = "identity", bins = 30) +
  ggtitle("Age by Churn") +
  xlab("Age") +
  ylab("Count") +
  scale_fill_manual(values = c("0" = "#D55E00", "1" = "#0072B2"), name = "Churn")


bivariate3 <- ggplot(churn, aes(x = Techie, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Techie") +
  xlab("Techie") +
  ylab("Count of Customers") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), name = "Churn") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


bivariate4 <- ggplot(churn, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Contract") +
  xlab("Contract") +
  ylab("Count of Customers") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), name = "Churn")


bivariate5 <- ggplot(churn, aes(x = Churn, fill = Multiple)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Multiple") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), name = "Churn")


bivariate6 <- ggplot(churn, aes(x = Churn, fill = OnlineSecurity)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by OnlineSecurity") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), name = "Churn")


bivariate7 <- ggplot(churn, aes(x = Churn, fill = StreamingTV)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by StreamingTV") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), name = "Churn")


bivariate8 <- ggplot(churn, aes(x = Churn, fill = StreamingMovies)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by StreamingMovies") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), name = "Churn")


bivariate9 <- ggplot(churn, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Payment Method") +
  xlab("Payment Method") +
  ylab("Proportion") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), name = "Churn")


bivariate10 <- ggplot(churn, aes(x = Churn, y = MonthlyCharge, fill = Churn)) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Monthly Charge by Churn Status") +
  xlab("Churn") +
  ylab("Monthly Charge") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), name = "Churn")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  bivariate1, bivariate2, bivariate3,
  bivariate4, bivariate5, bivariate6,
  bivariate7, bivariate8, bivariate9,
  bivariate10,
  ncol = 2
)



