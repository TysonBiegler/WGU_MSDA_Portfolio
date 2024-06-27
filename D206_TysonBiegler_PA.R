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
  churn <- read_csv("C:/Users/tyson/WGU/R/D206/raw_data/churn_raw_data.csv")
  names(churn)
  glimpse(churn)
  
  
  
  
  
#Dupilicates--------------------------------------------------------------------
  sum(duplicated(churn)) #Returns 0 duplicates
  
  
  
  
  
#Missing Values-----------------------------------------------------------------
  #looking for the amount of NA in each column
    colSums(is.na(churn)) 
    #Columns with NA values
    churn$Children #NA 2495
    churn$Age #NA 2475
    churn$Income #NA 2490
    churn$Techie #NA 2477
    churn$Phone #NA 1026
    churn$TechSupport #NA 991
    churn$Tenure #NA 931
    churn$Bandwidth_GB_Year #NA 1021
    
  #Dealing with NA values
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
      summary(churn$Income) #Mean remains the same
    #Phone dealing with NA    
      sum(is.na(churn$Phone))
      class(churn$Phone)
      summary(churn$Phone)
      #ordinal encoding #citation -> https://wgu.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=bd7b8541-77ba-42e0-80a4-b059010bc790
      unique(churn$Phone)
      #viewing the distribution
      churn %>% 
        ggplot(aes(Phone,
                   color=Phone,
                   fill=Phone)) +
        geom_bar() +
        theme_bw()
      #Imputation using the mode due to this variable being categorical
      churn$Phone[is.na(churn$Phone)] <- (names(which.max(table(churn$Phone))))
      #verifiying that NA values are gone
      sum(is.na(churn$Phone))
      #viewing the new distribution
      churn %>% 
        ggplot(aes(Phone,
                   color=Phone,
                   fill=Phone)) +
        geom_bar() +
        theme_bw()

    #TechSupport Dealing with NA
      unique(churn$TechSupport)
      summary(churn$TechSupport)
      #checking initial distribution
      churn %>% 
        ggplot(aes(TechSupport,
                   color=TechSupport,
                   fill=TechSupport)) +
        geom_bar() +
        theme_bw()
      #Imputation using the mode due to this variable being categorical
      churn$TechSupport[is.na(churn$TechSupport)] <- (names(which.max(table(churn$TechSupport))))
      #verifiying that NA values are gone
      sum(is.na(churn$TechSupport))
      #checking the distribution after imputation
      churn %>% 
        ggplot(aes(TechSupport,
                   color=TechSupport,
                   fill=TechSupport)) +
        geom_bar() +
        theme_bw()
      
    #TENURE Dealing with NA
      glimpse(churn$Tenure)
      summary(churn$Tenure)
      #looking at distribution
      hist(churn$Tenure)
      #Imputation using the median
      churn$Tenure[is.na(churn$Tenure)] <- median(churn$Tenure, na.rm = TRUE)
      #verifiying that NA values are gone
      sum(is.na(churn$Tenure))
      #checking the distribution after imputation
      hist(churn$Tenure)
      
    #BANDWIDTH_GB_YEAR Dealing with NA
      glimpse(churn$Bandwidth_GB_Year)
      summary(churn$Bandwidth_GB_Year)
      #Imputation using the median
      churn$Bandwidth_GB_Year[is.na(churn$Bandwidth_GB_Year)] <- median(churn$Bandwidth_GB_Year, na.rm = TRUE)
      #verifiying that NA values are gone
      sum(is.na(churn$Bandwidth_GB_Year))
      hist(churn$Bandwidth_GB_Year)
      
    #TECHIE Dealing with Na  
      summary(churn$Techie)
      unique(churn$Techie)
      #Imputation using the mode due to this variable being categorical
      churn$Techie[is.na(churn$Techie)] <- (names(which.max(table(churn$Techie))))
      #verifiying that NA values are gone
      sum(is.na(churn$Techie))
      unique(churn$Techie)
      
      
      
      
      
#Outliers-----------NOT DONE YET------------------------------------------------
    
    #children
    boxplot(churn$Children)
    
    #Income
    #Retaining Ouliers becuase it seams reasonable that the income range of telecom users would be vastly different
    boxplot(churn$Income)
    
    #Tenure
    boxplot(churn$Tenure) #does not appear to be outliers present
    
    #Bandwidth_GB_Year
    boxplot(churn$Bandwidth_GB_Year) #does not appear to be outliers present
    
    
    
    
    
#Other Data Quality Issues------------------------------------------------------
  
    #ZIP converting to 5 digits 
      class(churn$Zip)
      sum(is.na(churn$Zip))
      #Removing white space
      churn$Zip <- trimws(churn$Zip)
      #padding the cells with 0s to a fixed width of 5 digits and converting to character
      churn$Zip <- str_pad(churn$Zip, width = 5, side = "left", pad = 0)
      churn$Zip
      class(churn$Zip)
      
    #GENDER updating the options and converting to a factor
      churn$Gender <- as.factor(churn$Gender) #converting to a factor
      levels(churn$Gender) #checking the factor levels 'male', 'female' and 'Prefer not to answer'
      churn <- churn %>% #converting 'Prefer not to answer' to 'Nonbinary'
        mutate(Gender = recode(Gender,
                               "Prefer not to answer" = "Nonbinary"))
      levels(churn$ Gender) #checking that mutate was successful
      
    #EDUCATION LEVEL ordinal encoding 1-12 and changing to factor
      churn$Education <- as.factor(churn$Education)
      Edu_num <- revalue(x=churn$Education, replace = c(
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
      
      churn$Edu_num <- Edu_num
      glimpse(churn$Edu_num)
      
    #RENAME SURVEY RESPONSES
      names(churn)[45] <- "Timely_response"
      names(churn)[46] <- "Timely_fixes"
      names(churn)[47] <- "Timely_replacements"
      names(churn)[48] <- "Reliability"
      names(churn)[49] <- "Options"
      names(churn)[50] <- "Respectful"
      names(churn)[51] <- "Courteous"
      names(churn)[52] <- "Active_listening"
      names(churn)
      
    #TYPE CONVERSIONS
      churn <- churn %>%
        mutate(Employment = as.factor(Employment),
               Contract = as.factor(Contract),
               Internet_service = as.factor(Internet_service),
               Area = as.factor(Area),
               Timezone = as.factor(Timezone),
               Marital = as.factor(Marital),
               State = as.factor(State))
      
      levels(churn$Employment)
      levels(churn$Contract)
      levels(churn$Internet_service)
      levels(churn$Area)
      levels(churn$Timezone)
      levels(churn$Marital)
      levels(churn$State)
      
      churn <- churn %>%
        mutate(Churn = factor(Churn, levels = c("0", "1"), labels = c("No", "Yes")),
               Techie = factor(Techie, levels = c("0", "1"), labels = c("No", "Yes")),
               Port_modem = factor(Port_modem, levels = c("0", "1"), labels = c("No", "Yes")),
               Tablet = factor(Tablet, levels = c("0", "1"), labels = c("No", "Yes")),
               Phone = factor(Phone, levels = c("0", "1"), labels = c("No", "Yes")),
               Multiple = factor(Multiple, levels = c("0", "1"), labels = c("No", "Yes")),
               Online_security = factor(Online_security, levels = c("0", "1"), labels = c("No", "Yes")),
               Online_backup = factor(Online_backup, levels = c("0", "1"), labels = c("No", "Yes")),
               Device_protection = factor(Device_protection, levels = c("0", "1"), labels = c("No", "Yes")),
               Tech_support = factor(Tech_support, levels = c("0", "1"), labels = c("No", "Yes")),
               Streaming_TV = factor(Streaming_TV, levels = c("0", "1"), labels = c("No", "Yes")),
               Streaming_movies = factor(Streaming_movies, levels = c("0", "1"), labels = c("No", "Yes")),
               Paperless_billing = factor(Paperless_billing, levels = c("0", "1"), labels = c("No", "Yes")))
      
      churn$PaymentMethod <- tolower(churn$PaymentMethod) #making lower case to match data dictionary
      churn$PaymentMethod <- as.factor(churn$PaymentMethod)
      levels(churn$PaymentMethod)
      churn <- churn %>% #change value to (bank (automatic bank transfer) to match data dictionary
        mutate(PaymentMethod = recode(PaymentMethod,
                                      "bank transfer(automatic)" = "bank (automatic bank transfer)"))
      
      
      churn <- churn %>%
        mutate(Outage_sec_per_week = as.numeric(Outage_sec_per_week),
               Tenure = as.numeric(Tenure),
               Monthly_charge = as.numeric(Monthly_charge),
               Bandwidth_GB_year = as.numeric(Bandwidth_GB_year))
      
    #Rounding variables to 2 decimal points
      churn <- churn %>%
        mutate(
          Tenure = sprintf('%#.2f', Tenure),
          MonthlyCharge = sprintf('%#.2f', MonthlyCharge),
          Bandwidth_GB_Year = sprintf('%#.2f', Bandwidth_GB_Year),
          Income = sprintf('%#.2f', Income),
          Outage_sec_perweek = sprintf('%#.2f', Outage_sec_perweek)
        )
      
      
      
    #Creating Age and Income Bins
      #Age
      churn$Age <- floor(churn$Age) #rounding age down to the the nearest whole number
      sum(is.na(churn$Age))
      
      #creating age groups
      Age_groups <- cut(churn$Age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                        labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                        right = TRUE,
                        include.lowest = TRUE)
      
      sum(is.na(Age_groups))
      #adding 'Age_groups' to the churn dataframe as a column
      churn$Age_groups <- Age_groups
      names(churn)
      #converting 'Age_groups' to a factor
      churn$Age_groups <- as.factor(churn$Age_groups)
      levels(churn$Age_groups)
      #checking for NA values in Age_group
      sum(is.na(churn$Age_groups))
      #looking at the distribution of ages
      churn %>%
        ggplot(aes(Age_groups,
                   color=Age_groups,
                   fill=Age_groups)) +
        geom_bar() +
        theme_bw()
      
      #Incomme
      churn$Income <- as.numeric(churn$Income)
      
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
      
      churn$Income_groups <- as.factor(Income_groups)
      
      churn %>% 
        ggplot(aes(Income_groups,
                   color=Income_groups,
                   fill=Income_groups)) +
        geom_bar() +
        theme_bw()
      
      levels(churn$Income_groups)
      
    #Survey Data- adding a sum of scores column to determine total approval ratings
      churn$Sum_survey_scores <- rowSums(churn[,44:51]) #selecting the survey responses and adding the sum of each row to a new column named Sum_survey_scores
      
      
      summary(churn$Sum_survey_scores)
      glimpse(churn)
      
      hist(churn$Sum_survey_scores)
      
      class(churn$Sum_survey_scores)
      
      
      
      
    #Renaming columns to have similar naming conventions
      names(churn)
      churn <- churn %>%
        dplyr::rename(Internet_service = InternetService,
               Online_security = OnlineSecurity,
               Tech_support = TechSupport,
               Paperless_billing = PaperlessBilling,
               Monthly_charge = MonthlyCharge,
               Index = CaseOrder,
               Outage_sec_per_week = Outage_sec_perweek,
               Online_backup = OnlineBackup,
               Streaming_TV = StreamingTV,
               Payment_method = PaymentMethod,
               Bandwidth_GB_year = Bandwidth_GB_Year,
               Customer_ID = Customer_id,
               Device_protection = DeviceProtection,
               Streaming_movies = StreamingMovies)
      #REMOVING column "...1" because it was auto generated as a row name column
      churn <- churn[-1]
      names(churn)

      
      
      
#PCA----------------------------------------------------------------------------
    #(WGU Courseware, 2024)
      
      glimpse(churn) #checking which variables will be appropriate for PCA
      names(churn) #Looking for the index of each column I will use. 
    #options for the pca
      # class(churn$Lat) #8
      # class(churn$Lng) #9
      # class(churn$Age) #15
      # class(churn$Income) #18
      # class(churn$Outage_sec_per_week) #22
      # class(churn$Tenure) # 41
      # class(churn$Monthly_charge) #42
      # class(churn$Bandwidth_GB_year) #43

      churn_num <- churn %>% select_if(is.numeric)
      names(churn_num)
      churn_pca_data <- churn_num[, 2:14]
      
      cor(churn_pca_data)
      mean(cor(churn_pca_data))
      summary(churn_pca_data)
      
      
      names(churn_pca_data)    
      # Perform PCA
      pca <- prcomp(churn_pca_data, center = TRUE, scale. = TRUE)
      
      # Summary of PCA
      summary(pca)
  
      plot(pca, type = 'l')
      
      fviz_eig(pca, choice = "eigenvalue", addlabels = TRUE)
      
      pca$rotation
      
      
      
      
      
#------------------------------------------------------------------    
      


      
      # Improving Service Quality:
      #   Example: The high positive loadings for attributes like SR_timely_response, SR_timely_fixes, and SR_respectful in Comp.1 indicate that these aspects significantly influence overall service quality.
      # Benefit: By focusing on these attributes, your company can enhance customer satisfaction and loyalty.
      # Addressing Reliability Issues:
      #   Example: The strong negative loading for SR_reliability in Comp.2 suggests that reliability is inversely related to this component.
      # Benefit: Your organization can prioritize reliability improvements to positively impact this component.
      # Timely Replacements Strategy:
      #   Example: The positive loading for SR_timely_replacements in Comp.3 highlights the importance of timely replacements.
      # Benefit: Implement efficient replacement processes to enhance customer experience.
      # Customization and Options:
      #   Example: The positive loading for SR_options in Comp.5 indicates that customization options matter.
      # Benefit: Offering more choices or personalized solutions can attract and retain customers.
      # Active Listening Skills Training:
      #   Example: The strong negative loading for SR_active_listening in Comp.8 suggests that active listening skills are crucial.
      # Benefit: Invest in training programs to improve communication and understanding with customers.
