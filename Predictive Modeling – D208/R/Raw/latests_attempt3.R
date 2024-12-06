
# Initial Setup -----------------------------------------------------------
install.packages("tidyverse")
install.packages("MASS")# For stepwise regression (stepAIC)
install.packages("car") # For VIF calculation (detecting multicollinearity)
install.packages("ggeffects") # For visualizing model predictions
install.packages("ggfortify") # For visualizing model diagnostic plots
install.packages("gridExtra") # For arranging multiple ggplot graphs in a grid
install.packages("performance") # For checking model assumptions

library(tidyverse) # Includes ggplot2, dplyr, readr, and more for data manipulation and visualization
library(MASS)           # For stepwise model selection using stepAIC()
library(car)            # For calculating VIF (Variance Inflation Factor)
library(ggeffects)      # For creating prediction plots from regression models
library(ggfortify)      # For diagnostic plots of linear models
library(gridExtra)      # For arranging multiple plots in a grid
library(performance)    # For checking model assumptions visually (check_model)

tidyverse_packages(include_self = TRUE)

# Set wd ------------------------------------------------------------------
setwd('C:/Users/tyson/OneDrive/Documents/GitHub/WGU_MSDA_Portfolio/Predictive Modeling – D208/R/Raw')

# Get data ----------------------------------------------------------------
churn <- read_csv("churn_clean.csv")
theme_set(theme_minimal())


set.seed(0)


# Explore Data ------------------------------------------------------------
str(churn)
summary(churn)
sum(is.na(churn))
sum(duplicated(churn))

# Research Question -------------------------------------------------------

#"What factors impact customer tenure?"



# Clearn/Prepare Data -----------------------------------------------------
churn <- churn %>%
  mutate_at(vars(43:50), as.factor) %>%
  rename_at(vars(43:50), ~ c(
    "Timely_response",
    "Timely_fixes",
    "Timely_replacements",
    "Reliability",
    "Options",
    "Respectful",
    "Courteous",
    "Active_listening"
  ))

names(churn)


# Converting to Logical Data Type
churn <- churn %>%
  mutate(
    Churn = as.factor(ifelse(trimws(Churn) == 'Yes', 1, 0)),
    Techie = as.factor(ifelse(trimws(Techie) == 'Yes', 1, 0)),
    Port_modem = as.factor(ifelse(trimws(Port_modem) == 'Yes', 1, 0)),
    Tablet = as.factor(ifelse(trimws(Tablet) == 'Yes', 1, 0)),
    Phone = as.factor(ifelse(trimws(Phone) == 'Yes', 1, 0)),
    Multiple = as.factor(ifelse(trimws(Multiple) == 'Yes', 1, 0)),
    OnlineSecurity = as.factor(ifelse(trimws(OnlineSecurity) == 'Yes', 1, 0)),
    OnlineBackup = as.factor(ifelse(trimws(OnlineBackup) == 'Yes', 1, 0)),
    DeviceProtection = as.factor(ifelse(trimws(DeviceProtection) == 'Yes', 1, 0)),
    TechSupport = as.factor(ifelse(trimws(TechSupport) == 'Yes', 1, 0)),
    StreamingTV = as.factor(ifelse(trimws(StreamingTV) == 'Yes', 1, 0)),
    StreamingMovies = as.factor(ifelse(trimws(StreamingMovies) == 'Yes', 1, 0)),
    PaperlessBilling = as.factor(ifelse(trimws(PaperlessBilling) == 'Yes', 1, 0))
  )

# Convert Specific Columns to Factors
churn <- churn %>%
  mutate_at(vars(Area, 
                 Marital, 
                 Gender,
                 Contract,
                 InternetService,
                 PaymentMethod, 
                 Churn,
                 Techie,
                 Port_modem,
                 Tablet,
                 Phone,
                 Multiple,
                 OnlineSecurity,
                 OnlineBackup,
                 DeviceProtection,
                 TechSupport,
                 StreamingTV,
                 StreamingMovies,
                 PaperlessBilling,
                 City,
                 State
                 ),
            as.factor)

# Convert Specific Columns to Numeric
churn <- churn %>%
  mutate_at(vars(Tenure,
                 MonthlyCharge,
                 Bandwidth_GB_Year,
                 Outage_sec_perweek
                 ),
            as.numeric)

churn <- churn %>% 
  mutate_at(vars(Children,
                 Age,
                 Population,
                 Timely_response,
                 Timely_fixes,
                 Timely_replacements,
                 Reliability,
                 Options,
                 Respectful,
                 Courteous,
                 Active_listening),
            as.integer)




glimpse(churn)
str(churn)

#removing columns im not going to use
churn <- churn[, !(names(churn) %in% c("CaseOrder", 
                                       "Customer_id", 
                                       "Interaction",
                                       "UID",
                                       "Lat", 
                                       "Lng",
                                       "County", 
                                       "Zip",
                                       "Area", 
                                       "TimeZone", 
                                       "Job",
                                       "City",
                                       "State"
                                       ))]
#"Timely_response",
#"Timely_fixes",
#"Timely_replacements",
#"Reliability",
#"Options",
#"Respectful",
#"Courteous",
#"Active_listening",
#"Marital",
#"Contract",
#"InternetService",
#"Gender",
#"PaymentMethod", 


str(churn)

# Select Variables --------------------------------------------------------

Initial_model <- lm(Tenure ~ ., data = churn)

summary(Initial_model)

reduced_model <- stepAIC(object = Initial_model)

reduced_model

summary(reduced_model) #(Larose & Larose, 2019)
plot(reduced_model)

reduced_model <- lm(formula = Tenure ~ Children + Age + Gender + InternetService + 
     Multiple + OnlineSecurity + OnlineBackup + DeviceProtection + 
     TechSupport + StreamingTV + StreamingMovies + MonthlyCharge + Bandwidth_GB_Year, data = churn)
plot(reduced_model)

summary(reduced_model)

reduced_model2 <- lm(formula = Tenure ~ Children + Age + Bandwidth_GB_Year + Gender + 
                       Churn + Techie + Contract + Tablet + InternetService + Multiple + 
                       OnlineSecurity + OnlineBackup + DeviceProtection + PaperlessBilling, data = churn)
plot(reduced_model2)

# Reduce model ------------------------------------------------------------

par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
plot(reduced_model2)


vif_values <- vif(reduced_model)
vif_values

vif_values <- vif(reduced_model2)
vif_values

# Checking assumptions with visuals
check_model(reduced_model2)

# Visualize model predictions
ggeffect(reduced_model2)

summary(reduced_model2)

autoplot(reduced_model, which = 1:6, ncol = 2, label.size = 3)
autoplot(reduced_model2, which = 1:6, ncol = 2, label.size = 3)

# Analysis ----------------------------------------------------------------


ind_var <- churn_num[,c("MonthlyCharge",
                        "Bandwidth_GB_Year",
                        "Children",
                        "Churn",
                        "Multiple",
                        "OnlineSecurity",
                        "OnlineBackup",
                        "DeviceProtection",
                        "TechSupport",
                        "StreamingTV",
                        "StreamingMovies",
                        "PaperlessBilling"
                        )]

names(ind_var)



# Converting back to factor for visualizations
#ind_var <- ind_var %>%
#  mutate(
#    Churn = as.factor(ifelse(trimws(Churn) == 1 ,"Yes", "No")),
#    Multiple = as.factor(ifelse(trimws(Multiple) == 1 ,"Yes", "No")),
#    OnlineSecurity = as.factor(ifelse(trimws(OnlineSecurity) == 1 ,"Yes", "No")),
#    OnlineBackup = as.factor(ifelse(trimws(OnlineBackup) == 1 ,"Yes", "No")),
#    DeviceProtection = as.factor(ifelse(trimws(DeviceProtection) == 1 ,"Yes", "No")),
#    TechSupport = as.factor(ifelse(trimws(TechSupport) == 1 ,"Yes", "No")),
#    StreamingTV = as.factor(ifelse(trimws(StreamingTV) == 1 ,"Yes", "No")),
#    StreamingMovies = as.factor(ifelse(trimws(StreamingMovies) == 1 ,"Yes", "No")),
#    Phone = as.factor(ifelse(trimws(StreamingMovies) == 1 ,"Yes", "No")),
#    Tablet = as.factor(ifelse(trimws(StreamingMovies) == 1 ,"Yes", "No")),
#    Port_modem = as.factor(ifelse(trimws(StreamingMovies) == 1 ,"Yes", "No"))
#  )


# univariate plots --------------------------------------------------------

# Continuous variables
uni1 <- ggplot(reduced_model, aes(x = Tenure)) + 
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + 
  ggtitle("Tenure")

uni2 <- ggplot(reduced_model, aes(x = MonthlyCharge)) + 
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + 
  ggtitle("MonthlyCharge")

uni3 <- ggplot(reduced_model, aes(x = Bandwidth_GB_Year)) + 
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + 
  ggtitle("Bandwidth_GB_Year")

uni4 <- ggplot(reduced_model, aes(x = Children)) + 
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + 
  ggtitle("Children") +
  xlab("Number of Children") +
  ylab("Count")

# Categorical variables
uni5 <- ggplot(churn, aes(x = as.factor(Churn))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("Churn") +
  xlab("Churn (1 = Yes, 0 = No)") +
  ylab("Count")

uni6 <- ggplot(churn, aes(x = as.factor(Multiple))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("Multiple") +
  xlab("Multiple (1 = Yes, 0 = No)") +
  ylab("Count")

uni7 <- ggplot(churn, aes(x = as.factor(OnlineSecurity))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("OnlineSecurity") +
  xlab("OnlineSecurity (1 = Yes, 0 = No)") +
  ylab("Count")

uni8 <- ggplot(churn, aes(x = as.factor(OnlineBackup))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("OnlineBackup") +
  xlab("OnlineBackup (1 = Yes, 0 = No)") +
  ylab("Count")

uni9 <- ggplot(churn, aes(x = as.factor(DeviceProtection))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("DeviceProtection") +
  xlab("DeviceProtection (1 = Yes, 0 = No)") +
  ylab("Count")

uni10 <- ggplot(churn, aes(x = as.factor(TechSupport))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("TechSupport") +
  xlab("TechSupport (1 = Yes, 0 = No)") +
  ylab("Count")

uni11 <- ggplot(churn, aes(x = as.factor(StreamingTV))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("StreamingTV") +
  xlab("StreamingTV (1 = Yes, 0 = No)") +
  ylab("Count")

uni12 <- ggplot(churn, aes(x = as.factor(StreamingMovies))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("StreamingMovies") +
  xlab("StreamingMovies (1 = Yes, 0 = No)") +
  ylab("Count")

uni13 <- ggplot(churn, aes(x = as.factor(PaperlessBilling))) +
  geom_bar(fill = "orange", alpha = 0.7) +
  ggtitle("PaperlessBilling") +
  xlab("PaperlessBilling (1 = Yes, 0 = No)") +
  ylab("Count")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  uni1, uni2, uni3, uni4, 
  uni5, uni6, uni7, 
  uni8, uni9, uni10, 
  uni11, uni12, uni13,
  ncol = 3
)



# bivariate plots ---------------------------------------------------------

# Continuous Variables
bi1 <- ggplot(reduced_model, aes(x = MonthlyCharge, y = Tenure)) + 
  geom_point(color = "dodgerblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  ggtitle("Tenure vs MonthlyCharge") +
  xlab("MonthlyCharge") +
  ylab("Tenure")

bi2 <- ggplot(reduced_model, aes(x = Bandwidth_GB_Year, y = Tenure)) + 
  geom_point(color = "dodgerblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  ggtitle("Tenure vs Bandwidth_GB_Year") +
  xlab("Bandwidth_GB_Year") +
  ylab("Tenure")

bi3 <- ggplot(reduced_model, aes(x = Children, y = Tenure)) +
  geom_point(color = "dodgerblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  ggtitle("Tenure vs Children") +
  xlab("Number of Children") +
  ylab("Tenure")

# Categorical Variables
bi4 <- ggplot(reduced_model, aes(x = as.factor(Churn), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs Churn") +
  xlab("Churn (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi5 <- ggplot(reduced_model, aes(x = as.factor(Multiple), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs Multiple") +
  xlab("Multiple (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi6 <- ggplot(reduced_model, aes(x = as.factor(OnlineSecurity), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs OnlineSecurity") +
  xlab("OnlineSecurity (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi7 <- ggplot(reduced_model, aes(x = as.factor(OnlineBackup), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs OnlineBackup") +
  xlab("OnlineBackup (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi8 <- ggplot(reduced_model, aes(x = as.factor(DeviceProtection), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs DeviceProtection") +
  xlab("DeviceProtection (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi9 <- ggplot(reduced_model, aes(x = as.factor(TechSupport), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs TechSupport") +
  xlab("TechSupport (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi10 <- ggplot(reduced_model, aes(x = as.factor(StreamingTV), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs StreamingTV") +
  xlab("StreamingTV (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi11 <- ggplot(reduced_model, aes(x = as.factor(StreamingMovies), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs StreamingMovies") +
  xlab("StreamingMovies (0 = No, 1 = Yes)") +
  ylab("Tenure")

bi12 <- ggplot(reduced_model, aes(x = as.factor(PaperlessBilling), y = Tenure)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle("Tenure vs PaperlessBilling") +
  xlab("PaperlessBilling (0 = No, 1 = Yes)") +
  ylab("Tenure")

# Arrange all plots into a grid
gridExtra::grid.arrange(
  bi1, bi2, bi3,
  bi4, bi5, bi6,
  bi7, bi8, bi9,
  bi10, bi11, bi12,
  ncol = 3
)











churn %>% 
  ggplot(aes(x = Bandwidth_GB_Year, y = Tenure)) +
  geom_point(color = "dodgerblue", alpha = 0.75, size = 1.5) +
  geom_smooth(method = lm, se = FALSE, color = "darkorange", linewidth = 1.5) + 
  labs(
    title = "Scatter Plot of Tenure vs Bandwidth Usage",
    subtitle = "Exploring the relationship between Tenure and Bandwidth (GB per Year)",
    x = "Tenure (Months)",
    y = "Bandwidth (GB per Year)",
    caption = "Source: Churn Dataset"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkblue"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(size = 12, face = "bold")
  )




