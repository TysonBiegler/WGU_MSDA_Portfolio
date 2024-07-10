#Initial Setup--------------------------------------------------------------------------
  setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D206')
  
  #Install and load packages
    install.packages("ggplot2")
    install.packages('tidyverse')
    install.packages("factoextra") # For PCA
    install.packages("dplyr")
    
    library(ggplot2)
    library(tidyverse)
    library(plyr)
    library(dplyr)
    library(stringr)
    library(factoextra)
    library(stats)

#Loading data from csv##########################################################

  rm(list = ls())
  
  churn <- read_csv("C:/Users/tyson/WGU/R/D206/raw_data/churn_raw_data.csv") #importing csv file
  glimpse(churn) #initial inspection of the data set

#Dupilicates--------------------------------------------------------------------
  sum(duplicated(churn)) #Returns 0 duplicates

#Missing Values-----------------------------------------------------------------
  #looking for the amount of NA in each column
    colSums(is.na(churn)) 
    #Columns with NA values:
        # Children = NA 2495
        # Age = NA 2475
        # Income = NA 2490
        # Techie = NA 2477
        # Phone = NA 1026
        # TechSupport = NA 991
        # Tenure = NA 931
        # Bandwidth_GB_Year  = NA 1021
  
  #CHILDREN dealing with NA values
    hist(churn$Children)
    summary(churn$Children) #checking the median - 1.000
    #Its right skewed so im going to impute the median
    churn$Children[is.na(churn$Children)] <- median(churn$Children, na.rm = TRUE)
    summary(churn$Children) #Median remains the same
    #making sure NA values are dealt with
    sum(is.na(churn$Children)) #NA values are now 0
    #checking the new distribution after imputation
  hist(churn$Children)
  
  #AGE Dealing with NA values
    sum(is.na(churn$Age))
    hist(churn$Age)
    summary(churn$Age) #checking the mean - 53.28
    #Because of the uniform distribution I will impute with the mean
    churn$Age[is.na(churn$Age)] <- mean(churn$Age, na.rm = TRUE)
    hist(churn$Age)
    summary(churn$Age) #Mean remains the same
  
  #INCOME dealing with NA
    sum(is.na(churn$Income))
    class(churn$Income)
    summary(churn$Income) #checking the median value - 33186.8
    hist(churn$Income)
    #right skewed so Im going to impute the median. The min and max are significantly different
    churn$Income[is.na(churn$Income)] <- median(churn$Income, na.rm = TRUE)
    hist(churn$Income)
    #checking that the NA values are gone
    sum(is.na(churn$Income))
    summary(churn$Income) #Median remains the same
    
  #Phone dealing with NA    
    sum(is.na(churn$Phone))
    class(churn$Phone) #Not numeric so I need to using encoding
    unique(churn$Phone)
    #Imputation using the mode due to this variable being categorical
    #ordinal encoding #citation -> https://wgu.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=bd7b8541-77ba-42e0-80a4-b059010bc790
    churn$Phone[is.na(churn$Phone)] <- (names(which.max(table(churn$Phone))))
    #verifiying that NA values are gone
    sum(is.na(churn$Phone))
    #viewing the new distribution
    churn %>% 
    ggplot(aes(Phone)) +
    geom_bar() +
    theme_bw()
  
  #TechSupport Dealing with NA
    unique(churn$TechSupport)
    class(churn$TechSupport)
    #Imputation using the mode due to this variable being categorical
    churn$TechSupport[is.na(churn$TechSupport)] <- (names(which.max(table(churn$TechSupport))))
    #verifiying that NA values are gone
    sum(is.na(churn$TechSupport))
    #checking the distribution after imputation
    churn %>% 
    ggplot(aes(TechSupport)) +
    geom_bar() +
    theme_bw()
  
  #TENURE Dealing with NA
    class(churn$Tenure)
    #looking at distribution
    hist(churn$Tenure)
    #Imputation using the median
    churn$Tenure[is.na(churn$Tenure)] <- median(churn$Tenure, na.rm = TRUE)
    #verifiying that NA values are gone
    sum(is.na(churn$Tenure))
    #checking the distribution after imputation
    hist(churn$Tenure)
  
  #BANDWIDTH_GB_YEAR Dealing with NA
    glimpse(churn$Bandwidth_GB_Year) #numeric class
    summary(churn$Bandwidth_GB_Year) #shows 1021 NA values 
    hist(churn$Bandwidth_GB_Year) #bi-modal distribution
    #Imputation using the median
    churn$Bandwidth_GB_Year[is.na(churn$Bandwidth_GB_Year)] <- median(churn$Bandwidth_GB_Year, na.rm = TRUE)
    #verifiying that NA values are gone
    sum(is.na(churn$Bandwidth_GB_Year))
    hist(churn$Bandwidth_GB_Year) #checking distribution after imputation
    sum(is.na(churn$Bandwidth_GB_Year)) #verifying that NA values are gone
  
  #TECHIE Dealing with Na  
    summary(churn$Techie) #Character type
    unique(churn$Techie) #Contains NA values
    #Imputation using the mode due to this variable being categorical
    churn$Techie[is.na(churn$Techie)] <- (names(which.max(table(churn$Techie))))
    #verifiying that NA values are gone
    sum(is.na(churn$Techie))
    unique(churn$Techie)


#Outliers-----------------------------------------------------------------------

  #population
    boxplot(churn$Population)
    hist(churn$Population)
    boxplot.stats(churn$Population) #looking for the value of the upper whisker
    sum(churn$Population > 31795) # 9.37% are outliers. I will retain these outliers due to the high amount.
    
  #age
    boxplot(churn$Age)
    hist(churn$Age)
    
  #email
    boxplot(churn$Email)
    hist(churn$Email)
    boxplot.stats(churn$Email) #looking for the value of the upper whisker
    sum(churn$Email > 20 | churn$Email < 4) 
    churn$Email[churn$Email < 4 | churn$Email > 20] <- NA
    churn$Email[is.na(churn$Email)] <- mean(churn$Email, na.rm = TRUE)
    boxplot(churn$Email)
    
  #contacts
    boxplot(churn$Contacts)
    hist(churn$Contacts)
    boxplot.stats(churn$Contacts) #looking for the value of the upper whisker
    sum(churn$Contacts > 5) 
    churn$Contacts[churn$Contacts > 5 | churn$Contacts > 20] <- NA
    churn$Contacts[is.na(churn$Contacts)] <- median(churn$Contacts, na.rm = TRUE)
    
  #yearly_equip_failure
    boxplot(churn$Yearly_equip_failure)
    hist(churn$Yearly_equip_failure)
    boxplot.stats(churn$Yearly_equip_failure) #looking for the value of the upper whisker
    sum(churn$Yearly_equip_failure > 2) 
    churn$Yearly_equip_failure[churn$Yearly_equip_failure > 2] <- NA
    churn$Yearly_equip_failure[is.na(churn$Yearly_equip_failure)] <- median(churn$Yearly_equip_failure, na.rm = TRUE)
    
  #children
    class(churn$Children) #Numeric type
    hist(churn$Children) #looking at the distribution before imputation
    boxplot(churn$Children) #Any amount above 6 appears to be an outlier
    sum(churn$Children > 6)
    churn$Children[churn$Children > 6] <- NA #converting values above 6 to NA
    churn$Children[is.na(churn$Children)] <- median(churn$Children, na.rm = TRUE) #Imputing the mean to deal with the newly added NA values
  
  #Income
    class(churn$Income) #Numeric type
    hist(churn$Income) #looking at distribution. Right skewed
    boxplot(churn$Income) #looking for outliers with a boxplot
    boxplot.stats(churn$Income) #looking for the value of the upper whisker
    sum(churn$Income > 78272.96)
    summary(churn$Income)
  
  #Tenure
    class(churn$Tenure) #numeric type
    boxplot(churn$Tenure) #does not appear to be outliers present
    hist(churn$Tenure)
    
  #Bandwidth_GB_Year
    class(churn$Bandwidth_GB_Year) #Numeric type
    boxplot(churn$Bandwidth_GB_Year) #does not appear to be outliers present



#Other Data Quality Issues------------------------------------------------------

#ZIP converting to 5 digits 
class(churn$Zip) #Numeric Type
sum(is.na(churn$Zip)) #No NA values
churn$Zip <- as.character(churn$Zip) #Converting to character because trimws expects a character input
#R documentation (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/trimws)
churn$Zip <- trimws(churn$Zip) #Removing white space from the front and back of Zip
churn$Zip <- str_pad(churn$Zip, width = 5, side = "left", pad = 0)#padding the cells with 0s to a fixed width of 5 digits and converting to character
class(churn$Zip) #confirming that Zip is now a character type

#GENDER updating the options and converting to a factor
churn$Gender <- as.factor(churn$Gender) #converting to a factor
levels(churn$Gender) #checking the factor levels 'male', 'female' and 'Prefer not to answer'
churn <- churn %>% #converting 'Prefer not to answer' to 'Nonbinary'
mutate(Gender = recode(Gender,
               "Prefer not to answer" = "Nonbinary"))
levels(churn$ Gender) #checking that mutate was successful

#EDUCATION LEVEL ordinal encoding 1-12 and changing to factor

class(churn$Education) #Character type
Edu_num <- revalue(x=churn$Education, replace = c( #Re-encoding the education levels to number values from lowest level to highest
"Doctorate Degree" = 1,
"Master's Degree" = 2,
"Bachelor's Degree" = 3,
"Associate's Degree" = 4,
"Professional School Degree" = 5,
"Some College, 1 or More Years, No Degree" = 6,
"Some College, Less than 1 Year" = 7,
"Regular High School Diploma" = 8,
"GED or Alternative Credential" = 9,
"9th Grade to 12th Grade, No Diploma" = 10,
"Nursery School to 8th Grade" = 11,
"No Schooling Completed" = 12))

Edu_num <- as.numeric(Edu_num) #Converting to numeric
class(Edu_num) #confirming that Edu_num is numeric
unique(Edu_num)
churn$Education_num <- Edu_num #adding Edu_num to churn as a new column named Education_num
class(churn$Education_num) #confirming that Edu_num is still numeric
hist(churn$Education_num) #inspecting the distribution of education levels


#RENAME SURVEY RESPONSES
names(churn)[45] <- "Timely_response"
names(churn)[46] <- "Timely_fixes"
names(churn)[47] <- "Timely_replacements"
names(churn)[48] <- "Reliability"
names(churn)[49] <- "Options"
names(churn)[50] <- "Respectful"
names(churn)[51] <- "Courteous"
names(churn)[52] <- "Active_listening"
names(churn) #ensuring that all name changes were successful 


churn <- churn %>% #Converting to logical data type. 
  mutate(
    Churn = ifelse(trimws(tolower(Churn)) == 'Yes', TRUE, FALSE),
    Techie = ifelse(trimws(tolower(Techie)) == 'Yes', TRUE, FALSE),
    Port_modem = ifelse(trimws(tolower(Port_modem)) == 'Yes', TRUE, FALSE),
    Tablet = ifelse(trimws(tolower(Tablet)) == 'Yes', TRUE, FALSE),
    Phone = ifelse(trimws(tolower(Phone)) == 'Yes', TRUE, FALSE),
    Multiple = ifelse(trimws(tolower(Multiple)) == 'Yes', TRUE, FALSE),
    OnlineSecurity = ifelse(trimws(tolower(OnlineSecurity)) == 'Yes', TRUE, FALSE),
    OnlineBackup = ifelse(trimws(tolower(OnlineBackup)) == 'Yes', TRUE, FALSE),
    DeviceProtection = ifelse(trimws(tolower(DeviceProtection)) == 'Yes', TRUE, FALSE),
    TechSupport = ifelse(trimws(tolower(TechSupport)) == 'Yes', TRUE, FALSE),
    StreamingTV = ifelse(trimws(tolower(StreamingTV)) == 'Yes', TRUE, FALSE),
    StreamingMovies = ifelse(trimws(tolower(StreamingMovies)) == 'Yes', TRUE, FALSE),
    PaperlessBilling = ifelse(trimws(tolower(PaperlessBilling)) == 'Yes', TRUE, FALSE)
  )

churn$PaymentMethod <- tolower(churn$PaymentMethod) #making lower case to match data dictionary
class(churn$PaymentMethod)
churn <- churn %>% #change value to (bank (automatic bank transfer) to match data dictionary
mutate(PaymentMethod = recode(PaymentMethod,
                      "bank transfer(automatic)" = "bank (automatic bank transfer)"))
unique(churn$PaymentMethod) #ensuring that the name change was sucessful


#############    ~~~~ LEFT OFF HERE ~~~~    ####################################

#Rounding variables to 2 decimal points
churn <- churn %>% #using spritf to round to 2 decimal places. Each of these will need to be converted back to numeric later
mutate(
Tenure = sprintf('%#.2f', Tenure),
MonthlyCharge = sprintf('%#.2f', MonthlyCharge),
Bandwidth_GB_Year = sprintf('%#.0f', Bandwidth_GB_Year),
Income = sprintf('%#.2f', Income),
Outage_sec_perweek = sprintf('%#.0f', Outage_sec_perweek)
)



#Creating Age and Income Bins
#Age
churn$Age <- floor(churn$Age) #rounding age down to the the nearest whole number because people typically go by a whole number age rather than a decimal version.
sum(is.na(churn$Age))

#creating age groups -> source (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut)
Age_groups <- cut(churn$Age, breaks = c(18, 25, 35, 45, 55, 65, Inf), #Adding age groups will make analysis of ages easier in future tasks
        labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
        right = TRUE,
        include.lowest = TRUE) #ensuring that the younger ages are included and not turned into NA values


churn$Age_groups <- Age_groups

churn$Age_groups <- as.factor(churn$Age_groups)

sum(is.na(Age_groups))#Making sure that there are no NA values that didnt fit into these groups
churn$Age_groups <- Age_groups #adding 'Age_groups' to churn as a column
names(churn) #Making sure Age_groups was added successfully to churn
class(churn$Age_groups)#factor type
levels(churn$Age_groups)
churn %>%#looking at the distribution of ages
ggplot(aes(Age_groups)) +
geom_bar() +
theme_bw()
glimpse(churn)

#Incomme
class(churn$Income)#character type
churn$Income <- as.numeric(churn$Income) #converting to numeric

#creating income groups that incriment by $20K
Income_groups <- cut(churn$Income, breaks = c(0, 20000,40000,60000,80000,100000,120000,140000,160000,180000,200000,220000,240000, Inf), 
           labels = c("$0-$20K", 
                      "$20k-$40k", 
                      "$40k-$60k", 
                      "$60k-$80k", 
                      "$80k-$100k",
                      "$100k-$120K", 
                      "$120k-$140k", 
                      "$140k-$160k", 
                      "$160k-$180k", 
                      "$180k-$200k", 
                      "$200k-$220k", 
                      "$220k-$240k", 
                      "$240k+"),
           right = TRUE,
           include.lowest = TRUE)


churn$Income_groups <- as.factor(Income_groups) #Converting Income_groups to a factor with these groups as levels

churn %>%   #checking out the distribution of income groups
ggplot(aes(Income_groups)) +
geom_bar() +
theme_bw()

levels(churn$Income_groups) #making sure the levels are correct


#Survey Data- adding a sum of scores column to determine total approval ratings
churn$Sum_survey_scores <- rowSums(churn[(45:52)]) #selecting the survey responses and adding the sum of each row to a new column named Sum_survey_scores
hist(churn$Sum_survey_scores) #inspecting the distribution of survey score totals. 
names(churn)

#TYPE CONVERSIONS
churn <- churn %>% #changing the following variables to factors 
mutate(Employment = as.factor(Employment),
Contract = as.factor(Contract),
InternetService = as.factor(InternetService),
Area = as.factor(Area),
Timezone = as.factor(Timezone),
Marital = as.factor(Marital),
State = as.factor(State),
PaymentMethod = as.factor(PaymentMethod))

#checking the levels of each of the following factor variables
levels(churn$Employment)
levels(churn$Contract)
levels(churn$InternetService)
levels(churn$Area)
levels(churn$Timezone)
levels(churn$Marital)
levels(churn$State)
levels(churn$PaymentMethod) 

churn <- churn %>% #converting the following variables to numeric
mutate(Tenure = as.numeric(Tenure),
       MonthlyCharge = as.numeric(MonthlyCharge),
       Bandwidth_GB_Year = as.numeric(Bandwidth_GB_Year),
       Outage_sec_perweek = as.numeric(Outage_sec_perweek))

glimpse(churn) #checking if the types have been changed

#Renaming columns to have similar naming conventions
names(churn)
churn <- churn %>% #I had to specify dplyr package for rename because it appeared to want to use another package and was causing errors
  dplyr::rename(Internet_service = InternetService, #source -> (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ns-dblcolon)
                Online_security = OnlineSecurity,
                Tech_support = TechSupport,
                Paperless_billing = PaperlessBilling,
                Index = CaseOrder,
                Online_backup = OnlineBackup,
                Streaming_TV = StreamingTV,
                Payment_method = PaymentMethod,
                Customer_ID = Customer_id,
                Device_protection = DeviceProtection,
                Streaming_movies = StreamingMovies,
                Monthly_charge = MonthlyCharge,
                Bandwidth_GB_year = Bandwidth_GB_Year,
                Outage_sec_per_week = Outage_sec_perweek)

names(churn)#confirming that the name changes were successful


#Removing column "...1" because it was auto generated as a row name column
churn <- churn[-1]
names(churn)



#PCA----------------------------------------------------------------------------
#(WGU Courseware, 2024)

glimpse(churn) #checking which variables will be appropriate for PCA
names(churn) #Looking for the index of each column I will use. 

churn_num <- churn %>% select_if(is.numeric) #getting all the numeric columns
names(churn_num) #getting the index number of the ones I will be using

churn_pca_data <- churn_num[,4:14] #storing these 11 variables in churn_pca_data

glimpse(churn_pca_data) #making sure they are all there and their data type is correct

pca <- prcomp(churn_pca_data, center = TRUE, scale = TRUE) # Performing the PCA

plot(pca, type = 'l') #Showing the variance in a line plot

eig_plot <- fviz_eig(pca, choice = "eigenvalue", addlabels = TRUE, ncp = 11) #creating a scree plot 
eig_plot + geom_hline(yintercept = 1, linetype = "dashed", color = "red") #distinguishing where 1 eigenvalue is

summary(pca)

# calculating the eigenvalues to show all the eigenvalues above 1. source: (StatQuest with Josh Starmer, 2017)

std_dev<-pca$sdev
eigenvalues<-std_dev^2
eigenvalues

fviz_pca_biplot(pca, label = "var") #the biplot graph

# PCA Loadings
pca$rotation # seeing the contribution from each variable

summary(pca) # seeing the proportion of variance by each component as well as the cumulative proportion of explained variance
#Exporting to CSV---------------------------------------------------------------
write.csv(churn, "C:/Users/tyson/WGU/R/D206_PA/Cleaned/churn_cleaned_data.csv", row.names=FALSE) #exporting the cleaned data to a CSV
