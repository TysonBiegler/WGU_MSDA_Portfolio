#Data Mining II - D212 Task 1
#Tyson Biegler
#Student ID: 012170282

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

# Preparing the data ------------------------------------------------------

churn <- churn[, c("Tenure", "Bandwidth_GB_Year")]  #picked only 2 variables (Kamara, 2023)
range(churn$Bandwidth_GB_Year)
range(churn$Tenure)
#scaling the data
churn <- as.data.frame(scale(churn))

glimpse(churn)

# Plots -------------------------------------------------------------------

ggplot(churn, aes(x = Tenure)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Tenure", x = "Tenure", y = "Count")

ggplot(churn, aes(x = Bandwidth_GB_Year)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Bandwidth_GB_Year", x = "Bandwidth_GB_Year", y = "Count")


# Create model ------------------------------------------------------------
set.seed(1234)

#scree plot to find the elbow and  best K value
fviz_nbclust(churn, kmeans, method = "wss") + # SOURCE: (Bobbitt,2022)
  labs(title = "Elbow Method for Best k")

km <- kmeans(churn, centers = 2, nstart = 20)

# k means with 2 clusters based on the elbow plot
print(km$centers)

#creating a datafram with the cluster assignments so i can plot 
final_data <- cbind(churn, cluster = km$cluster)

head(final_data)

# Export cleaned data to CSV ---------------------------------------------------
write.csv(final_data, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Cleaned/Task1/churn_cleaned_data.csv", row.names = FALSE)


ggplot(final_data, aes(x = Tenure, y = Bandwidth_GB_Year, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_color_manual(values = c("#E69F00", "#0072B2")) +
  labs(title = "Scatter Plot of Tenure vs. Bandwidth_GB_Year by Cluster", 
       x = "Tenure", 
       y = "Bandwidth_GB_Year", 
       color = "Cluster")

#silhouette score -1 not good to +1 good. 
sil <- silhouette(km$cluster, dist(churn))
fviz_silhouette(sil)
