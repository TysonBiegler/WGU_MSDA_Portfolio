# D209 Task 2
#Student ID:012170282

#install.packages("tidyverse")
#install.packages("rpart")
#install.packages("rpart.plot")

library(tidyverse) # For cleaning and preparing the data
library(caret) # creating dummy variables
library(rpart) # creating the model
library(rpart.plot) # plotting the model in a tree structure

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/Task 2')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())

# Clean/Prepare Data -----------------------------------------------------
#Removing columns im not going to use
churn$Churn <- as.factor(churn$Churn)

churn <- churn %>%
  select(Churn, Techie, Contract, InternetService, Phone, Multiple, 
         OnlineBackup, DeviceProtection, StreamingTV, StreamingMovies, 
         PaymentMethod, Tenure)

# Converting binary categorical variables to 0 and 1
churn <- churn %>%
  mutate(across(c(Techie, Phone, Multiple, OnlineBackup, 
                  DeviceProtection, StreamingTV, StreamingMovies), 
                ~ ifelse(. == "Yes", 1, 0)))

# One-hot encoding for categorical variables with multiple values
dummies <- dummyVars(~ Contract + InternetService + PaymentMethod, data = churn)
dummy_data <- predict(dummies, newdata = churn)
dummy_data <- as.data.frame(dummy_data)

# Remove original multi-category variables before merging into churn
churn <- churn %>%
  select(-Contract, -InternetService, -PaymentMethod) %>%
  bind_cols(dummy_data)

# Convert Continuous Column to Numeric
churn$Tenure <- as.numeric(churn$Tenure)

str(churn)



# Decision Tree -----------------------------------------------------------

glimpse(churn)

#80/20 split
Independant_sample <- sample(2, 
                             nrow(churn), 
                             replace = T, 
                             prob = c(.8, .2))

training <- churn[Independant_sample == 1,]
test <- churn[Independant_sample == 2,]

decision_tree <- rpart(Churn ~ ., 
                       data = training, 
                       method = "class",
                       cp = 0.0005,
                       maxdepth = 6, 
                       minbucket = 20)


#plot the decision tree (GeeksforGeeks, 2024)
rpart.plot(decision_tree,
           main = "Factors contributing to churn",
           type = 3, 
           fallen.leaves = TRUE, # Put leaves at the bottom of the plot
           shadow.col = "gray",  # Add shadows for better visualization
           cex = 0.6)           # text size


# test data predictions
test_predict <- predict(decision_tree, newdata = test)
head(test_predict)

# Accuracy ----------------------------------------------------------------
test_predict_class <- factor(ifelse(test_predict[, 2] > 0.5, "Yes", "No"), levels = c("No", "Yes"))

# Compute confusion matrix
cm <- confusionMatrix(test_predict_class, test$Churn)
cm
# Extract accuracy
accuracy <- cm$overall["Accuracy"]
print(paste("Accuracy: ", round(accuracy, 4))) # 87.29% of the time, the model correctly predicts whether a customer will churn or not

# exporting the cleaned csv, training data, and test data -----------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Clean/Task 2')
write.csv(churn, "CLEANED_churn.csv")
write.csv(training, "trainingData.csv")
write.csv(test, "testingData.csv")
