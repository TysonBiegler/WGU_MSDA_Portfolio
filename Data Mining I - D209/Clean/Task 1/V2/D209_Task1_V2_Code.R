#Student ID: 012170282

#install.packages("tidyverse")
#install.packages("tidymodels")
library(caret) # Used for building the KNN model (train function), performing cross-validation (trainControl), and evaluating performance (confusionMatrix).
library(ROCR) # Evaluating model performance with ROC and AUC
library(tidyverse) # Used for data wrangling

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/Task 1')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


# Exploring the Data ------------------------------------------------------
#getting a general understanding of the data

#checking for na and duplicates
sum(is.na(churn))
sum(duplicated(churn))

# Clean/Prepare Data -----------------------------------------------------
#Removing columns im not going to use
churn$Churn <- as.factor(churn$Churn)

churn <- churn %>%
  select(Churn, Techie, Contract, InternetService, Phone, Multiple, 
         OnlineBackup, DeviceProtection, StreamingTV, StreamingMovies, 
         PaymentMethod, Tenure)

# Convert binary categorical variables to 0 and 1
churn <- churn %>%
  mutate(across(c(Techie, Phone, Multiple, OnlineBackup, 
                  DeviceProtection, StreamingTV, StreamingMovies), 
                ~ ifelse(. == "Yes", 1, 0)))

# One-hot encoding only multi-category categorical variables
dummies <- dummyVars(~ Contract + InternetService + PaymentMethod, data = churn)
dummy_data <- predict(dummies, newdata = churn)
dummy_data <- as.data.frame(dummy_data)

# Remove original multi-category variables and others I wont be using before merging into churn
churn <- churn %>%
  select(-Contract, -InternetService, -PaymentMethod) %>%
  bind_cols(dummy_data)

# Convert Continuous Columns to Numeric
churn$Tenure <- as.numeric(scale(churn$Tenure))

str(churn)



# KNN model ---------------------------------------------------------------
# data prep
set.seed(1234)

#80/20 split
ind <- sample(2, nrow(churn), replace = T, prob = c(.8, .2))
training <- churn[ind == 1,]
test <- churn[ind == 2,]

# cross validation 10 fold 3 times
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

# Knn Model
fit <- train(Churn ~ .,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl)

# model performance
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

plot(perf, colorize=FALSE, main = "ROC Curve")
abline(0, 1, col = "red", lty = 2)

auc <- performance(pred2, "auc")@y.values[[1]]
auc # AUC = 0.9422046

glimpse(churn)

# exporting the cleaned csv, training data, and test data -----------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Clean/Task 1/V2')
write.csv(churn, "CLEANED_churn.csv")
write.csv(training, "trainingData.csv")
write.csv(test, "testingData.csv")
