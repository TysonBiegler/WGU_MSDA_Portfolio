# Tyson Biegler
# Student ID: 012170282
# D212 Data Mining II Task 3

#Code source: 
#Kamara, K. (2022, August 28). Data mining II - D212 [Video]. Panopto. https://wgu.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5674b196-a9f1-4e85-a322-af0000021f3f

#The following code is from the D212 Task 3 webinar video from Dr Kamara. 


# Initial setup -----------------------------------------------------------
# Load necessary libraries
library(tidyverse)
library(arules)

setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Raw/Task 3')

# Data acquisition --------------------------------------------------------
df <- read_csv("medical_market_basket.csv")
dim(df)

glimpse(df)
str(df)

#Finding the unique values for each variable
lapply(df, unique)

# Cleaning ----------------------------------------------------------------
cleaned_med <- df[!apply(df == "", 1, all),] #removing blanks
dim(cleaned_med)

cleaned_med$Id <- factor(seq.int(nrow(cleaned_med))) #adding index column
names(cleaned_med)

cleaned_med <- as.data.frame(unclass(cleaned_med),stringsAsFactors = TRUE) #converting categoricals to factors
glimpse(cleaned_med)

pre_trans <- pivot_longer(cleaned_med, cols = 1:20, names_to = "ItemNo", values_to = "Rx") #converting the original 20 columns into 2 columns, ItemNo and Rx (This will also have the index column)
head(pre_trans)

pre_trans <- pre_trans[,c(1,3)] #Keeping only the Id, Rx, and ItemNo columns

pre_trans <- pre_trans[!(pre_trans$Rx == ""),] #remove rows where Rx is empty

list_data <- as.data.frame(pre_trans) #Converting pre_trans to a dataframe

list_data <- split(list_data$Rx, list_data$Id) #Groups prescriptions (Rx) by Id so that each element is a customer transaction
str(list_data)


basket <- as(list_data, "transactions") #converting the the list into a transaction object

basket  <- as(basket, "matrix") #convert to matrix format

str(basket)
dim(basket)
head(basket)

# Export cleaned data to CSV ---------------------------------------------------
write.csv(basket, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/Data mining II - D212/Cleaned/Task3/cleaned_basket.csv", row.names = FALSE)


# Analysis ----------------------------------------------------------------
sort(colSums(basket), decreasing = TRUE) #sorting to see what prescription is most common

arules <- apriori(
  basket, 
  control = list(verbose = FALSE), 
  parameter = list(supp = 0.008, conf = 0.4, minlen = 2),
  appearance = list(rhs = "abilify", default = "lhs")
)#The apriori algorithm. 

redundant_r <- is.redundant(arules) #finding the redundant rules

refined_arules <- arules[!redundant_r] #removing the redundant rules

inspect(sort(refined_arules, by = 'lift', decreasing = TRUE)) #showing the strongest association rules sorted by lift

print("Code ran successfully without errors.") #to demonstrate that the code ran in full without errors. 
