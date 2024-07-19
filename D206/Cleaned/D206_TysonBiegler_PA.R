# Tyson Biegler
# Student ID: 012170282
# D206 Data Cleaning

#Initial Setup------------------------------------------------------------------
    # Setting Working Directory
        setwd('C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D206/Working')

    # Install and load packages
        install.packages('tidyverse')
        install.packages("factoextra")

    # Load necessary libraries
        library(tidyverse)
        library(plyr)
        library(factoextra)
        library(stats)

    # Loading data from CSV
        churn <- read_csv("C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D206/Raw/churn_raw_data.csv")
        glimpse(churn)#quick look at the data
        
    # Checking for duplicates
        sum(duplicated(churn))

    # Checking for missing values
        colSums(is.na(churn))

# Dealing with NA's ------------------------------------------------------------

    # Children
        hist(churn$Children)
        churn$Children[is.na(churn$Children)] <- median(churn$Children, na.rm = TRUE)
        hist(churn$Children)

    # Age
        hist(churn$Age)
        churn$Age[is.na(churn$Age)] <- mean(churn$Age, na.rm = TRUE)
        hist(churn$Age)

    # Income
        hist(churn$Income)
        churn$Income[is.na(churn$Income)] <- median(churn$Income, na.rm = TRUE)
        hist(churn$Income)


    # Tenure
        hist(churn$Tenure)
        churn$Tenure[is.na(churn$Tenure)] <- median(churn$Tenure, na.rm = TRUE)
        hist(churn$Tenure)

    # Bandwidth_GB_Year
        hist(churn$Bandwidth_GB_Year)
        churn$Bandwidth_GB_Year[is.na(churn$Bandwidth_GB_Year)] <- median(churn$Bandwidth_GB_Year, na.rm = TRUE)
        hist(churn$Bandwidth_GB_Year)

    # Techie
    #categorical variable so I will impute with the mode
        churn$Techie[is.na(churn$Techie)] <- names(which.max(table(churn$Techie)))
    # Phone
    #categorical variable so I will impute with the mode
        churn$Phone[is.na(churn$Phone)] <- names(which.max(table(churn$Phone)))

    # TechSupport
    #categorical variable so I will impute with the mode
        churn$TechSupport[is.na(churn$TechSupport)] <- names(which.max(table(churn$TechSupport)))



# Handling outliers ------------------------------------------------------------
    # Population
        boxplot(churn$Population)
        boxplot.stats(churn$Population) #looking for the value of the upper whisker
        sum(churn$Population > 31795) # 9.37% are outliers. I will retain these outliers due to the high amount.

    # Email
        boxplot(churn$Email)
        hist(churn$Email)
        boxplot.stats(churn$Email) #looking for the value of the upper whisker
        sum(churn$Email > 20 | churn$Email < 4)
        churn$Email[churn$Email < 4 | churn$Email > 20] <- NA
        churn$Email[is.na(churn$Email)] <- mean(churn$Email, na.rm = TRUE)
        boxplot(churn$Email)

    # Contacts
        boxplot(churn$Contacts)
        boxplot.stats(churn$Contacts) #looking for the value of the upper whisker
        sum(churn$Contacts > 5)
        churn$Contacts[churn$Contacts > 5] <- NA
        churn$Contacts[is.na(churn$Contacts)] <- median(churn$Contacts, na.rm = TRUE)
        boxplot(churn$Contacts)

    # Yearly_equip_failure
        boxplot(churn$Yearly_equip_failure)
        boxplot.stats(churn$Yearly_equip_failure) #looking for the value of the upper whisker
        sum(churn$Yearly_equip_failure > 2)
        churn$Yearly_equip_failure[churn$Yearly_equip_failure > 2] <- NA
        churn$Yearly_equip_failure[is.na(churn$Yearly_equip_failure)] <- median(churn$Yearly_equip_failure, na.rm = TRUE)
        boxplot(churn$Yearly_equip_failure)

    # Children
        boxplot(churn$Children) #Any amount above 6 appears to be an outlier
        boxplot.stats(churn$Children)
        churn$Children[churn$Children > 6] <- NA
        churn$Children[is.na(churn$Children)] <- median(churn$Children, na.rm = TRUE)
        boxplot(churn$Children)

    # Income
        boxplot(churn$Income) # looking for outliers with a boxplot
        boxplot.stats(churn$Income) # looking for the value of the upper whisker
        sum(churn$Income > 78272.96) # 7.58% of all income values are outliers
        # Retaining outliers due to the large amount

# Other Data Quality Issues ----------------------------------------------------
    # Zip Code Processing
    # SOURCE (trimws, n.d.)
        churn$Zip <- str_pad(trimws(as.character(churn$Zip)), width = 5, side = "left", pad = "0")

    # Gender Conversion
    # changing 'Prefer not to answer' to 'Nonbinary'
        churn$Gender <- recode(churn$Gender, "Prefer not to answer" = "Nonbinary")
        churn$Gender <- as.factor(churn$Gender)
        levels(churn$Gender)


    # Education Level Encoding
        churn <- churn %>%
          mutate(
            Education_num = revalue(Education, c(
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
              "No Schooling Completed" = 12
            )),
            Education_num = as.numeric(Education_num)
          )

    # Renaming Survey Response Columns
        churn <- churn %>%
          rename_at(vars(45:52), ~ c(
            "Timely_response",
            "Timely_fixes",
            "Timely_replacements",
            "Reliability",
            "Options",
            "Respectful",
            "Courteous",
            "Active_listening"
          ))

    # Converting to Logical Data Type
        churn <- churn %>%
          mutate(
            Churn = ifelse(trimws(Churn) == 'Yes', TRUE, FALSE),
            Techie = ifelse(trimws(Techie) == 'Yes', TRUE, FALSE),
            Port_modem = ifelse(trimws(Port_modem) == 'Yes', TRUE, FALSE),
            Tablet = ifelse(trimws(Tablet) == 'Yes', TRUE, FALSE),
            Phone = ifelse(trimws(Phone) == 'Yes', TRUE, FALSE),
            Multiple = ifelse(trimws(Multiple) == 'Yes', TRUE, FALSE),
            OnlineSecurity = ifelse(trimws(OnlineSecurity) == 'Yes', TRUE, FALSE),
            OnlineBackup = ifelse(trimws(OnlineBackup) == 'Yes', TRUE, FALSE),
            DeviceProtection = ifelse(trimws(DeviceProtection) == 'Yes', TRUE, FALSE),
            TechSupport = ifelse(trimws(TechSupport) == 'Yes', TRUE, FALSE),
            StreamingTV = ifelse(trimws(StreamingTV) == 'Yes', TRUE, FALSE),
            StreamingMovies = ifelse(trimws(StreamingMovies) == 'Yes', TRUE, FALSE),
            PaperlessBilling = ifelse(trimws(PaperlessBilling) == 'Yes', TRUE, FALSE)
          )

    # Updating Payment Method To Match Data Dictionary
        churn <- churn %>%
          mutate(
            PaymentMethod = tolower(PaymentMethod), # data dictionary has all values as lowercase
            PaymentMethod = recode(PaymentMethod, "bank transfer(automatic)" = "bank (automatic bank transfer)"),
            PaymentMethod = as.factor(PaymentMethod)
          )

    # Round Numeric Columns
        churn <- churn %>%
          mutate(
            Tenure = round(Tenure, 2), #rounding to 2 decimal places
            MonthlyCharge = round(MonthlyCharge, 2), #rounding to 2 decimal places
            Bandwidth_GB_Year = round(Bandwidth_GB_Year, 0), #rounding to a whole number
            Income = round(Income, 2), #rounding to 2 decimal places
            Outage_sec_perweek = round(Outage_sec_perweek, 0) #rounding to a whole number
          )

    # Create Age Groups
    # Age groups will make analysis of customer ages easier
        churn <- churn %>%
          mutate(
            Age = floor(Age), #rounding down to the nearest whole number
            Age_groups = cut(Age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                             labels = c("18-24",
                                        "25-34",
                                        "35-44",
                                        "45-54",
                                        "55-64",
                                        "65+"),
                             right = TRUE, include.lowest = TRUE),
            Age_groups = as.factor(Age_groups)
          )
        
        ggplot(data = churn, aes(x = Age_groups)) +
          geom_bar(fill = "grey") +
          theme_minimal() +
          labs(title = "Distribution of Age Groups",
               x = "Age Groups",
               y = "Count")

    # Create Income Groups
    # SOURCE (cut, n.d)
        churn <- churn %>%
          mutate(
            Income = as.numeric(Income),
            Income_groups = cut(Income, breaks = c(0, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000, Inf),
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
                                right = TRUE, include.lowest = TRUE),
            Income_groups = as.factor(Income_groups)
          )
        
        ggplot(data = churn, aes(x = Income_groups)) +
          geom_bar(fill = "grey") +
          theme_minimal() +
          labs(title = "Distribution of Income Groups",
               x = "Income Groups",
               y = "Count")

    # Add Survey Scores Sum As Customer Satisfaction Scores
        churn$Sum_survey_scores <- rowSums(churn[(45:52)]) #selecting the survey responses and adding the sum of each row to a new column named Sum_survey_scores
        hist(churn$Sum_survey_scores) #inspecting the distribution of survey score totals.

    # Convert Specific Columns to Factors
        churn <- churn %>%
          mutate_at(vars(Employment,
                         Contract,
                         InternetService,
                         Area,
                         Timezone,
                         Marital,
                         State,
                         PaymentMethod),
                    as.factor)

    # Convert Specific Columns to Numeric
        churn <- churn %>%
          mutate_at(vars(Tenure,
                         MonthlyCharge,
                         Bandwidth_GB_Year,
                         Outage_sec_perweek),
                    as.numeric)

    # Renaming Columns to Consistent Naming Conventions
    # SOURCE (ns-dblcolon, n.d)
        churn <- churn %>%
          dplyr::rename(
            Internet_service = InternetService,
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
            Outage_sec_per_week = Outage_sec_perweek
          )

    # Remove the auto-generated row name column '...1'
        churn <- churn [-1]

# PCA --------------------------------------------------------------------------
    #Selecting Numeric Columns
      names(numeric_columns)

    # Saving The Numerica Columns To 'churn_pca_data'
      churn_pca_data <- numeric_columns[, 4:14]

    # Running PCA with pcrcomp
      pca <- prcomp(churn_pca_data, center = TRUE, scale = TRUE)

    # Plotting PCA
      plot(pca, type = 'l')

      Scree_plot <- fviz_eig(pca, choice = "eigenvalue", addlabels = TRUE) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red")

    # Viewing the scree plot
      Scree_plot

    # Double checking Eigenvalues
      # Saving the standard deviations of pca
      pca$sdev
      std_dev <- pca$sdev

      # Squaring the standard deviations to get the eigenvalues
      std_dev^2
      eigenvalues <- std_dev^2

      # Checking the eigenvalues
      eigenvalues

    # PCA Summary and Loadings
      summary(pca)
      pca$rotation

# Export cleaned data to CSV ---------------------------------------------------
      write.csv(churn, "C:/Users/tyson/Documents/GitHub/WGU_MSDA_Portfolio/D206/Cleaned/churn_cleaned_data.csv", row.names = FALSE)
      glimpse(churn)
      