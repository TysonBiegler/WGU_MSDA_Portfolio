# Tyson Biegler
# Student ID: 012170282
# D212 Data Mining II Task 2


# Load necessary libraries
library(tidyverse)
library(plyr)
library(factoextra)
library(stats)

options(scipen = 999) #To prevent scientific notation in the charts

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Raw')

# Data acquisition --------------------------------------------------------
churn <- read_csv("churn_clean.csv")

# Cleaning Data -----------------------------------------------------------
boxplot(churn$Age)  #No outliers

# Income
boxplot(churn$Income,
        main = "Boxplot of Income",
        ylab = "Value (Income in dollars)")
boxplot.stats(churn$Income) # looking for the value of the upper whisker
sum(churn$Income > 104166.70) #336 values above the upper whisker value
churn$Income <- pmin(churn$Income, 104166.70)

#NOTE:
#If I impute with the mean then high income earners have an artificially low income and the customer similarities would be inaccurate. 
#If I remove all outliers then I loose 336 rows (not a lot concerning the 10000 rows total), but im still loosing data. 
#So Ive decided to use the pmin function to cap all the income above the upper whisker to the value of the upper whisker. All rows are retained, and the extreme outliers are capped at a more realistic value as opposed to the mean which would be alot lower. 

boxplot(churn$Outage_sec_perweek,
        main = "Outage_sec_perweek boxplot",
        ylab = "Value (Seconds)")
boxplot.stats(churn$Outage_sec_perweek)
sum(churn$Outage_sec_perweek > 17.861530) #retaining the values because of the small number (43 values)

# PCA ---------------------------------------------------------------------

#Selecting only the numeric variables
churn <- select(churn, where(is.numeric))
glimpse(churn)

# Remove discrete numeric variables
churn <- churn %>%
  select(-c(CaseOrder,
            Lat,
            Lng,
            Zip,
            Population,
            Children,
            Email, 
            Contacts, 
            Yearly_equip_failure, 
            Item1, 
            Item2, 
            Item3, 
            Item4, 
            Item5, 
            Item6, 
            Item7, 
            Item8))


# Running PCA and scaling the data
scaled_churn <- as.data.frame(scale(churn))
names(scaled_churn)#"Age","Income", "Outage_sec_perweek","Tenure","MonthlyCharge","Bandwidth_GB_Year" 

## Export cleaned data to CSV ---------------------------------------------------
write.csv(scaled_churn, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Cleaned/Task2/churn_cleaned_data.csv", row.names = FALSE)

pca <- prcomp(scaled_churn, center = FALSE, scale = FALSE)

# Summary and loadings
summary(pca)

#Loading Matrix
print(pca$rotation)


# Elbow Method ------------------------------------------------------------
# Summary and scree plot
summary(pca)

fviz_eig(pca, choice = "variance", addlabels = TRUE, 
         main = "Scree Plot(Elbow Method)", xlab = "Principal Component", ylab = "Variance Explained (%)")

#Selecting the first 2 PCs based on the elobw plot
print(pca$rotation[, 1:2])
#Explained variance for the selected pcs
explained_variance <- pca$sdev^2 / sum(pca$sdev^2)
print(explained_variance[1:2])
#Total variance captured by PCA
print(sum(explained_variance[1:2]))

#NOTE:                                                                                                             
#Elbow method explains 0.503114 of total variance. Because the total explained variance is so low I will try the Kaiser method because the second through the fifth principal components are essentially the same explained variance, between 17.1% - 16.2%. So I want to see if the Kaiser method will suggest keeping more than 2 PCs. 


# Kaiser Method -----------------------------------------------------------

#Calculating eigenvalues for Kaiser method
eigenvalues <- pca$sdev^2
print(eigenvalues)
#Select components with eigenvalues > 1
kaiser_method <- sum(eigenvalues > 1)
cat('Number of components to keep:', kaiser_method)

#Selected PCs
print(pca$rotation[, 1:kaiser_method]) #selecting values 1 through the number returned by the sum(eigenvalues > 1) formula.

#Explained variance for the selected components
explained_variance <- eigenvalues / sum(eigenvalues)
print(explained_variance[1:kaiser_method])

# Total variance captured
pca_summary <- summary(pca)
print(pca_summary$importance[, 1:3])

print(sum(explained_variance[1:kaiser_method]))
total_variance <- sum(explained_variance[1:kaiser_method])

percentage <- round(sum(explained_variance[1:kaiser_method]) * 100, 2)

cat('Total variance explained in the first',kaiser_method,'components:',percentage,"%", '(',total_variance, ')')

#plotting the Kaiser method scree plot
fviz_eig(pca, choice = "eigenvalue", addlabels = TRUE,
         main = "Scree Plot (Kaiser Method)", xlab = "Principal Component", ylab = "Eigenvalue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")

#NOTE:
#The Kaiser method suggest keeping 3 principal components and results in an explained total variance of 67.08%. So I will use the Kaiser method in my write up. 
