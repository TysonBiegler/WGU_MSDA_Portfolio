
library(tidyverse)
library(caret)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data Mining I - D209/Raw/task 1/V2')
churn <- read_csv("churn_clean.csv")
glimpse(churn)


# clean the data ----------------------------------------------------------

# Convert dependent variable to numeric
# Ensure "No" is 0 and "Yes" is 1
churn$Churn <- ifelse(churn$Churn == "Yes", 1, 0)


# Convert binary categorical variables to 0 and 1
churn <- churn %>%
  mutate(across(c(Techie, Phone, Multiple, OnlineBackup, 
                  DeviceProtection, StreamingTV, StreamingMovies, Port_modem, Tablet, OnlineSecurity, PaperlessBilling, TechSupport), 
                ~ ifelse(. == "Yes", 1, 0)))

# One-hot encode only multi-category categorical variables
dummies <- dummyVars(~ Contract + InternetService + PaymentMethod + Gender + Marital + Area, data = churn)
dummy_data <- predict(dummies, newdata = churn)
dummy_data <- as.data.frame(dummy_data)

# Remove original multi-category variables and others I wont be using before merging
churn <- churn %>%
  select(-Contract, -InternetService, -PaymentMethod, -Gender, -Marital, -Area, -Job, -Lat, -Lng, -Zip, -County, -State, -City, -UID, -Interaction, -Customer_id, -CaseOrder, -TimeZone) %>%
  bind_cols(dummy_data)

glimpse(churn)


# Convert Likert scale columns (Item1 to Item8) to numeric
churn <- churn %>%
  mutate(across(c(Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8), as.numeric))

# Standardization (Z-score scaling) converts to a matrix by default
# Apply Z-score standardization and directly convert the result to numeric
churn <- churn %>%
  mutate(across(c(MonthlyCharge, Bandwidth_GB_Year, Tenure, Outage_sec_perweek, Income, Population, Email, Age, Children, Contacts, Yearly_equip_failure, 
                  Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8),
                ~ as.numeric(scale(.))))

glimpse(churn)

# feature selection -------------------------------------------------------

glimpse(churn)
set.seed(5)

correlationMatrix <- cor(churn)
correlationMatrix

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)

# Highly correlated features
colnames(correlationMatrix)[highlyCorrelated]

# removing highly correlated features
churn <- churn %>% select(-all_of(colnames(correlationMatrix)[highlyCorrelated]))

glimpse(churn)

# Creating train and test sets --------------------------------------------

set.seed(1234)

# Creating an 80/20 split for the data
train_ind <- sample(1:nrow(churn), 0.7 * nrow(churn))

# Known examples (70% for training)
known_examples <- churn[train_ind, ]

# Unknown customers (20% for testing)
unknown_customers <- churn[-train_ind, ]

#True labels for the training set
known_labels <- known_examples$Churn

head(known_labels)

#ggplot(data = churn, mapping = aes(x = MonthlyCharge, y = Bandwidth_GB_Year))+ 
  #geom_point()

# KNN model ---------------------------------------------------------------
known_examples$Churn <- factor(known_examples$Churn)
unknown_customers <- as.factor(unknown_customers$Churn)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)

knn_model <- train(Churn ~ ., 
                   data = known_examples,
                   method = 'knn',
                   trControl = trControl)

prediction <- predict(knn_model, newdata = unknown_customers)

#predictions for churn based on the testing set
head(prediction)
table(prediction)

# Cross-validation results visualization
ggplot(knn_model$results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Cross-validation Results",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

confusionMatrix(prediction, unknown_customers$Churn)

# ROC and AUC -------------------------------------------------------------
#Checking the ROC curve. Hoping to see a curve that hugs the top left corner

p1 <- predict(knn_model, unknown_customers, type = 'prob')[,2]

pred2 <- prediction(p1, unknown_customers$Churn)

perf <- performance(pred2, "tpr", "fpr")

plot(perf, colorize=FALSE, main = "ROC Curve")
abline(0, 1, col = "red", lty = 2)

auc <- performance(pred2, "auc")@y.values[[1]]
auc # AUC = 0.8905176

