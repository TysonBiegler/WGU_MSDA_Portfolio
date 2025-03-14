library(tidyverse)    # For data manipulation and visualization (includes dplyr, ggplot2)
library(cluster)      # For silhouette analysis to evaluate cluster quality
library(factoextra)   # Elbow plot

# Get data ----------------------------------------------------------------
# Set wd 
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Raw')

churn <- read_csv("churn_clean.csv")
glimpse(churn)

sum(is.na(churn))
sum(duplicated(churn))

#Selecting all numeric columns
churn <- churn %>%
  select(Churn, Techie, Contract, InternetService, Phone, Multiple, 
         OnlineBackup, DeviceProtection, StreamingTV, StreamingMovies, 
         PaymentMethod, Tenure)

str(churn)

#Removing the survey responses
churn <- churn[, -((ncol(churn) - 7):ncol(churn))]

#scaling the data
churn <- as.data.frame(scale(churn))

str(churn)

ggplot(churn, aes(x = Tenure, y = Bandwidth_GB_Year, color = Churn)) +
  geom_point(alpha = 0.7) +
  labs(title = "Tenure vs Bandwidth Usage",
       x = "Tenure (Months)",
       y = "Bandwidth Used (GB per Year)",
       color = "Churn Status") +
  theme_minimal()


#export CSV of scaled data
setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Cleaned/Task1')
write.csv(churn, "CLEANED_churn.csv")

# making it reproducible
set.seed(1234)

#scree plot to find the elbow and  best K value
fviz_nbclust(churn, kmeans, method = "wss") + # SOURCE: (statology,2022)
  labs(title = "Elbow Method for Best k")


km <- kmeans(churn, centers = 4, nstart = 25)

# k means with 3 clusters based on the elbow plot
print(km$centers)

#creating a datafram with the cluster assignments so i can plot 
final_data <- cbind(churn, cluster = km$cluster)



fviz_cluster(km, data = final_data, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#67B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



sil <- silhouette(km$cluster, dist(churn))
fviz_silhouette(sil)







