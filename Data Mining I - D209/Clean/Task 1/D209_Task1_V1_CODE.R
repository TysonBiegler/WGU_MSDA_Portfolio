# Tyson Biegler D209 Task 1
# Student ID: 012170282

#install.packages("tidyverse")
#install.packages("tidymodels")
library(caret) # Used for building the KNN model (train function), performing cross-validation (trainControl), and evaluating performance (confusionMatrix).
library(ROCR) # Evaluating model performance with ROC and AUC
library(tidyverse) # Used for data wrangling

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/task 1')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())

# Exploring the Data ------------------------------------------------------
#glimpse(churn)
str(churn)

#checking for na and duplicates
sum(is.na(churn))
sum(duplicated(churn))

# Clean/Prepare Data -----------------------------------------------------

# Convert Qualitative Columns to Factors
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
                 InternetService,#
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
                 PaperlessBilling
  ),
  as.factor)

# Convert Continuous Columns to Numeric
churn <- churn %>%
  mutate_at(vars(Tenure, 
                 MonthlyCharge, 
                 Bandwidth_GB_Year, 
                 Outage_sec_perweek), 
            as.numeric)

# Convert Columns to Integer
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
  mutate_at(vars(43:50), as.numeric) %>%
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


# KNN model ---------------------------------------------------------------
# data prep ---------------------------------------------------------------
set.seed(1234)
ind <- sample(2, nrow(churn), replace = T, prob = c(.8, .2))
training <- churn[ind == 1,]
test <- churn[ind == 2,]


trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)

#My reduced model from D208
fit <- train(Churn ~ Techie + Contract + InternetService + Phone + 
               Multiple + OnlineBackup + DeviceProtection + StreamingTV + 
               StreamingMovies + PaymentMethod + Tenure,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProcess = c("center", "scale"))

#model performance
fit

plot(fit)

pred <- predict(fit, newdata = test)
head(pred)
table(pred)

confusionMatrix(pred, test$Churn)

# ROC and AUC -------------------------------------------------------------
#Checking the ROC curve. Hoping to see a curve that hugs the top left corner

p1 <- predict(fit, test, type = 'prob')[,2]

pred2 <- prediction(p1, test$Churn)

perf <- performance(pred2, "tpr", "fpr")

plot(perf, colorize=TRUE, main = "ROC Curve")
abline(0, 1, col = "red", lty = 2)

auc <- performance(pred2, "auc")@y.values[[1]]
auc # AUC = 0.9391026



# exporting the cleaned csv, training data, and test data -----------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Clean/Task 1')
write.csv(churn, "CLEANED_churn.csv")
write.csv(training, "trainingData.csv")
write.csv(test, "testingData.csv")
