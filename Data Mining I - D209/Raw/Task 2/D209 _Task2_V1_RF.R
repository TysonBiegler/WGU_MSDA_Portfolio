# RANDOM FOREST
#https://www.youtube.com/watch?v=dJclNIN-TPo

#install.packages("randomForest")

library(randomForest) #creating the random forest model
library(tidyverse) #
library(caret) #making predictions and dummy variables and confusion matrix

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/Task 2')

# Get data ----------------------------------------------------------------
data <- read_csv("churn_clean.csv")

str(data)

# cleaning ----------------------------------------------------------------
#Removing columns im not going to use
data$Churn <- as.factor(data$Churn)

data <- data %>%
  select(Churn, Techie, Contract, InternetService, Phone, Multiple, 
         OnlineBackup, DeviceProtection, StreamingTV, StreamingMovies, 
         PaymentMethod, Tenure)

# Convert binary categorical variables to 0 and 1
data <- data %>%
  mutate(across(c(Techie, Phone, Multiple, OnlineBackup, 
                  DeviceProtection, StreamingTV, StreamingMovies), 
                ~ ifelse(. == "Yes", 1, 0)))

# One-hot encoding only multi-category categorical variables
dummies <- dummyVars(~ Contract + InternetService + PaymentMethod, data = data)
dummy_data <- predict(dummies, newdata = data)
dummy_data <- as.data.frame(dummy_data)

# Remove original multi-category variables and others I wont be using before merging into data
data <- data %>%
  select(-Contract, -InternetService, -PaymentMethod) %>%
  bind_cols(dummy_data)

# Convert Continuous Columns to Numeric
data$Tenure <- as.numeric(scale(data$Tenure))

colnames(data) <- make.names(colnames(data))


str(data)


# RF Model ----------------------------------------------------------------


set.seed(123)
independant_sample <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))

train <- data[independant_sample == 1,]
test <- data[independant_sample == 2,]

#y variable is a factor so I will be using random forest for classification
#sq.root(p) where p is the number of freatures in the model

set.seed(456)

random_forest <- randomForest(Churn~., data = train)


sqrt(19) #19 variables in the data set - mtry
#Number of trees: 500 = ntree

print(random_forest)
#OOB estimate of  error rate: 10.56% (out of bag estimate error: data that the particual tree has not seen.  Different than the confusion matrix becuase the cm is based on the training data that was provided. )
 


# prediction with train data ----------------------------------------------

train_predict <- predict(random_forest, train)

confusionMatrix(train_predict, train$Churn)

# Accuracy:Correct predictions - no/no = 5091+217/7048 = 0.9554
# Sensitivity: how often Churn_no was correctly classified as Churn_no


# Prediction with test data -----------------------------------------------

test_predict <- predict(random_forest, test)

confusionMatrix(test_predict, test$Churn)

# Accuracy:Correct predictions - no/no = 2037+591/2952 = 0.8902
# Sensitivity: how often Churn_no was correctly classified as Churn_no


# Error rate --------------------------------------------------------------

plot(random_forest) #model doesnt seem to improve after about 100 trees
str(data) #Churn is the first variable

tune <- tuneRF(train[,-1], train[,1],
       stepFactor = .5,
       plot = TRUE,
       ntreeTry = 100,
       trace = TRUE,
       improve = 0.05)
