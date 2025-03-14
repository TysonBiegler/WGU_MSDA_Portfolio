library(tidyverse)    # For data manipulation and visualization (includes dplyr, ggplot2)
library(cluster)      # For silhouette analysis to evaluate cluster quality
library(factoextra)   # Elbow plot


# Initial setup -----------------------------------------------------------
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Raw')


# Data acquisition --------------------------------------------------------
churn <- read_csv("churn_clean.csv")

# Data exploration --------------------------------------------------------
glimpse(churn)

#Checking for missing or duplicate values
sum(is.na(churn))
sum(duplicated(churn))

head(churn)

#summary statistics about the variables in the dataset
#summary(churn)

# Preparing the data ------------------------------------------------------

churn <- churn[, c("Tenure", "Email")]  #picked only 2 variables (Kamara, 2023)

#scaling the data
churn <- as.data.frame(scale(churn))

glimpse(churn)


# Plots -------------------------------------------------------------------

ggplot(churn, aes(x = Tenure)) +
  geom_histogram(binwidth = 0.5, fill = "#2E9FDF", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Tenure", x = "Tenure", y = "Count")

ggplot(churn, aes(x = Email)) +
  geom_histogram(binwidth = 0.5, fill = "#2E9FDF", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Email", x = "Email", y = "Count")


# Create model ------------------------------------------------------------
set.seed(1234)

#scree plot to find the elbow and  best K value
fviz_nbclust(churn, kmeans, method = "wss") + # SOURCE: (Bobbitt,2022)
  labs(title = "Elbow Method for Best k")

km <- kmeans(churn, centers = 4, nstart = 20)

# k means with 3 clusters based on the elbow plot
print(km$centers)

#creating a datafram with the cluster assignments so i can plot 
final_data <- cbind(churn, cluster = km$cluster)

# Export cleaned data to CSV ---------------------------------------------------
write.csv(final_data, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Cleaned/Task1/churn_cleaned_data.csv", row.names = FALSE)


ggplot(final_data, aes(x = Tenure, y = Email, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_color_manual(values = c("Orange", "Purple", "Blue", "Red")) +
  labs(title = "Scatter Plot of Tenure vs. Email by Cluster", 
       x = "Tenure", 
       y = "Email", 
       color = "Cluster")

#silhouette score -1 not good to +1 good. 
sil <- silhouette(km$cluster, dist(churn))
fviz_silhouette(sil)