install.packages("MASS")
install.packages("car")
install.packages("viridis")
install.packages("ROCR")

library(tidyverse)
library(tidymodels) 
library(MASS) # Stepwise Regression using AIC
library(car) # Calculating VIF
library(viridis) # color-blind color scheme for plots
library(ROCR) # Evaluating model performance with ROC
library(caret)
library(pROC)


# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/task 1')

# Get data ----------------------------------------------------------------
data <- read_csv("medical_clean.csv")
theme_set(theme_minimal())

str(data)
glimpse(data)


# Research Question -------------------------------------------------------


# Exploring the Data ------------------------------------------------------
#checking for na and duplicates
sum(is.na(data))
sum(duplicated(data))

# Clearn/Prepare Data -----------------------------------------------------

# Convert Specific Columns to Binary Factor with levels 1 and 0
data <- data %>%
  mutate(across(c(ReAdmis,
                  Soft_drink,
                  HighBlood,
                  Stroke,
                  Overweight,
                  Arthritis,
                  Diabetes,
                  Hyperlipidemia,
                  BackPain,
                  Anxiety,
                  Allergic_rhinitis,
                  Reflux_esophagitis,
                  Asthma,
                  ),
                ~ factor(. == 'Yes', levels = c(FALSE, TRUE), labels = c(0, 1))))

str(data)

# Convert Specific Columns to Factors
data <- data %>%
  mutate_at(vars(City,
                 State,
                 County,
                 Zip,
                 Gender,
                 Marital,
                 Area,
                 TimeZone,
                 Initial_admin,
                 Complication_risk,
                 Services
  ),
  as.factor)

str(data)

# Convert Continuous Columns to Numeric
data <- data %>%
  mutate_at(vars(Income,
                 VitD_levels,
                 Initial_days,
                 TotalCharge,
                 Additional_charges), 
            as.numeric)

str(data)

# Convert Columns to Integer for Count Variables
data <- data %>% 
  mutate_at(vars(Population,
                 Children, 
                 Age, 
                 Doc_visits,
                 Full_meals_eaten,
                 vitD_supp), 
            as.integer)

str(data)

#renaming the survey response columns to be more intuitive
data <- data %>%
  mutate_at(vars(43:50), as.numeric) %>%
  rename_at(vars(43:50), ~ c(
    "Timely_admission",
    "Timely_treatment",
    "Timely_visits",
    "Reliability",
    "Options",
    "Hours_of_treatment",
    "Courteous",
    "Active_listening"
  ))

#Removing columns im not going to use
data <- data[, !(names(data) %in% c("CaseOrder", 
                                    "Customer_id", 
                                    "Interaction",
                                    "UID",
                                    "City",
                                    "State",
                                    "County",
                                    "Zip",
                                    "Lat", 
                                    "Lng",
                                    "TimeZone",
                                    "Job"
))]

str(data)






# LOGISTIC  MODEL FROM D208 -----------------------------------------------

# splitting the data ------------------------------------------------------

set.seed(123)

split <- initial_split(data,
                       prop = .80,
                       strata = ReAdmis)

train <- training(split)
test <- testing(split)

# initial model -----------------------------------------------------------

initial_model <- glm(ReAdmis ~ . , data = train, family = 'binomial')
summary(initial_model)

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(initial_model)


# reduced model -----------------------------------------------------------

reduced_model <- stepAIC(object = initial_model, direction = "backward", trace = FALSE)
summary(reduced_model)

vif_values <- vif(reduced_model)
vif_values #Looking for VIF values above 10.

reduced_model <- update(reduced_model, . ~ . - TotalCharge)

vif_values <- vif(reduced_model)
vif_values #Looking for VIF values above 10.

summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Hyperlipidemia)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Diabetes)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Allergic_rhinitis)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Item2)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - BackPain)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Item5)
summary(reduced_model)

reduced_model <- update(reduced_model, . ~ . - Item8)
summary(reduced_model)

anova(initial_model, reduced_model, "Chaisq")
AIC(initial_model)
AIC(reduced_model)

p1 <- predict(reduced_model, train, type='response')

p2 <- predict(reduced_model, test, type = 'response')

pred <- prediction(p2, test$ReAdmis)
perf <- performance(pred, "tpr", "fpr")

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(reduced_model)

plot(perf, col = "blue", main = "ROC Curve")
abline(0, 1, col = "red", lty = 2)

auc <- performance(pred, "auc")@y.values[[1]]
auc

#Model Im using is from the Logistic regression from D208
#glm(formula = ReAdmis ~ Children + Soft_drink + Initial_admin + 
#  HighBlood + Stroke + Complication_risk + Reflux_esophagitis + 
#  Asthma + Services + Initial_days + Timely_admission + Timely_treatment + 
#  Courteous + Active_listening, family = "binomial", data = train)



# data prep ---------------------------------------------------------------
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.8, .2))
training <- data[ind == 1,]
test <- data[ind == 2,]


# KNN model ---------------------------------------------------------------

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)

fit <- train(ReAdmis ~ Children + Soft_drink + Initial_admin + 
               HighBlood + Stroke + Complication_risk + Reflux_esophagitis + 
               Asthma + Services + Initial_days + Timely_admission + Timely_treatment + 
               Courteous + Active_listening,
               data = training,
               method = 'knn',
               tuneLength = 20,
               trControl = trControl,
               preProcess = c("center", "scale"))

#model performance
fit

plot(fit)

pred <- predict(fit, newdata = test)
table(pred)

confusionMatrix(pred, test$ReAdmis)




