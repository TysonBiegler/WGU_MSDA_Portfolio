options(warn = -1) #disable warning
#install.packages("tidyverse")

library(tidyverse)
library(tidymodels)
library(MASS)
library(car)
library(viridis)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Raw/task2')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


# Research Question -------------------------------------------------------

# Which factors are most strongly associated with customer churn, and how do they influence the likelihood of churn?

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


# Plots -------------------------------------------------------------------
# univariate plots --------------------------------------------------------

univariate1 <- ggplot(churn, aes(x = Churn)) + 
  geom_bar(aes(fill = Churn)) + 
  ggtitle("Churn") +
  scale_fill_viridis_d()

univariate2 <- ggplot(churn, aes(x = Techie)) + 
  geom_bar(aes(fill = Techie)) + 
  ggtitle("Techie") +
  scale_fill_viridis_d()

univariate3 <- ggplot(churn, aes(x = Contract)) +
  geom_bar(aes(fill = Contract)) +
  ggtitle("Contract") +
  scale_fill_viridis_d()

univariate4 <- ggplot(churn, aes(x = InternetService)) +
  geom_bar(aes(fill = InternetService)) +
  ggtitle("InternetService") +
  scale_fill_viridis_d()

univariate5 <- ggplot(churn, aes(x = Phone)) +
  geom_bar(aes(fill = Phone)) +
  ggtitle("Phone") +
  scale_fill_viridis_d()

univariate6 <- ggplot(churn, aes(x = Multiple)) +
  geom_bar(aes(fill = Multiple)) +
  ggtitle("Multiple") +
  scale_fill_viridis_d()

univariate7 <- ggplot(churn, aes(x = OnlineBackup)) +
  geom_bar(aes(fill = OnlineBackup)) +
  ggtitle("OnlineBackup") +
  scale_fill_viridis_d()

univariate8 <- ggplot(churn, aes(x = DeviceProtection)) +
  geom_bar(aes(fill = DeviceProtection)) +
  ggtitle("DeviceProtection") +
  scale_fill_viridis_d()

univariate9 <- ggplot(churn, aes(x = StreamingTV)) +
  geom_bar(aes(fill = StreamingTV)) +
  ggtitle("StreamingTV") +
  scale_fill_viridis_d()

univariate10 <- ggplot(churn, aes(x = StreamingMovies)) +
  geom_bar(aes(fill = StreamingMovies)) +
  ggtitle("StreamingMovies") +
  scale_fill_viridis_d()

univariate11 <- ggplot(churn, aes(x = PaymentMethod)) +
  geom_bar(aes(fill = PaymentMethod)) +
  ggtitle("PaymentMethod") +
  scale_fill_viridis_d()

univariate12 <- ggplot(churn, aes(x = Tenure)) +
  geom_histogram(aes(fill = ..count..), binwidth = 5) +
  ggtitle("Tenure (In Months)") +
  scale_fill_viridis_c()


# Arrange all plots into a grid
gridExtra::grid.arrange(
  univariate1, univariate2, 
  univariate3, univariate4,
  univariate5, univariate6,
  univariate7, univariate8,
  univariate9, univariate10,
  univariate11, univariate12,
  ncol = 2
)

# bivariate plots ---------------------------------------------------------

bivariate1 <- ggplot(churn, aes(x = Techie, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Techie") +
  xlab("Techie") +
  ylab("Count of Customers") +
  scale_fill_viridis_d(name = "Churn") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bivariate2 <- ggplot(churn, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Contract") +
  xlab("Contract") +
  ylab("Count of Customers") +
  scale_fill_viridis_d(name = "Churn")

bivariate3 <- ggplot(churn, aes(x = Churn, fill = InternetService)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by InternetService") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "InternetService")

bivariate4 <- ggplot(churn, aes(x = Churn, fill = Phone)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Phone") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "Phone")

bivariate5 <- ggplot(churn, aes(x = Churn, fill = Multiple)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Multiple") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "Multiple")

bivariate6 <- ggplot(churn, aes(x = Churn, fill = OnlineBackup)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by OnlineBackup") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "OnlineBackup")

bivariate7 <- ggplot(churn, aes(x = Churn, fill = DeviceProtection)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by DeviceProtection") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "DeviceProtection")

bivariate8 <- ggplot(churn, aes(x = Churn, fill = StreamingTV)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by StreamingTV") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "StreamingTV")

bivariate9 <- ggplot(churn, aes(x = Churn, fill = StreamingMovies)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by StreamingMovies") +
  xlab("Churn") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "StreamingMovies")

bivariate10 <- ggplot(churn, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  ggtitle("Churn by Payment Method") +
  xlab("Payment Method") +
  ylab("Proportion") +
  scale_fill_viridis_d(name = "Churn")

bivariate11 <- ggplot(churn, aes(x = Tenure, fill = Churn)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
  ggtitle("Churn by Tenure") +
  xlab("Tenure") +
  ylab("Count") +
  scale_fill_viridis_d(name = "Churn")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  bivariate1, bivariate2, bivariate3,
  bivariate4, bivariate5, bivariate6,
  bivariate7, bivariate8, bivariate9,
  bivariate10, bivariate11,
  ncol = 2
)



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

reduced_model <- stepAIC(object = initial_model, direction = "backward", trace = FALSE)
summary(reduced_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(reduced_model)

vif_values <- vif(reduced_model)
vif_values #Looking for VIF values above 5. 

# Removed Bandwidth_GB_Year because of a VIF of 2656.593644 and MonthlyCharge with 16.889045
reduced_model <- glm(formula = Churn ~ Children + Age + Techie + Contract + InternetService + 
                       Phone + Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
                       StreamingTV + StreamingMovies + PaperlessBilling + PaymentMethod + 
                       Tenure, family = "binomial", 
                     data = churn_train)

summary(reduced_model)

# Removed values that did not have a statistically significant coefficient
reduced_model <- glm(formula = Churn ~ Techie + Contract + InternetService + 
                       Phone + Multiple + OnlineBackup + DeviceProtection + 
                       StreamingTV + StreamingMovies + PaymentMethod + 
                       Tenure, family = "binomial", 
                     data = churn_train)
summary(reduced_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(reduced_model)


# comparing the models ----------------------------------------------------

anova(initial_model, reduced_model, "Chaisq")

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


# Exponentiate the coefficients to get the odds ratios
odds_ratios <- exp(coef(reduced_model))

odds_ratios

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/Cleaned/task2')

write.csv(churn, "CLEANED_churn.csv")
write.csv(pred1, "PredicitonData.csv")
write.csv(churn_train, "trainingData.csv")



