# Load necessary libraries
library(dplyr)
library(downloader)
library(rafalib)

# Setting up the seed for reproducibility
set.seed(123)

# Simulating unclean data with missing values and outliers, ensuring 100 rows for each variable
branch_number <- 1:100
median_years <- sample(0:10, 100, replace = TRUE)
median_satisfaction <- round(runif(100, 1, 10), 0)

# Simulating unclean additional variables (random missing values, outliers), ensuring 100 rows
customer_age <- sample(c(18:80, NA), 100, replace = TRUE)
income_level <- sample(c("Low", "Medium", "High", NA), 100, replace = TRUE)
num_transactions <- c(sample(1:20, 98, replace = TRUE), NA, NA)  # Introduce 2 missing values
branch_size <- c(sample(10:50, 98, replace = TRUE), 9999, 10000)  # Introduce outliers and ensure length is 100
location_type <- sample(c("Urban", "Suburban", "Rural", NA), 100, replace = TRUE)

# Ensure all variables have 100 rows (adding NAs where necessary)
branch_size <- ifelse(length(branch_size) != 100, c(branch_size, rep(NA, 100 - length(branch_size))), branch_size)

# Creating a data frame with unclean data, ensuring all variables have 100 rows
survey_data_unclean <- data.frame(
  Branch_Number = branch_number,
  Median_Years = median_years,
  Median_Satisfaction = median_satisfaction,
  Customer_Age = customer_age,
  Income_Level = income_level,
  Num_Transactions = num_transactions,
  Branch_Size = branch_size,
  Location_Type = location_type
)

# Check for any inconsistencies in the length of data columns
sapply(survey_data_unclean, length)

# Running regression analysis on unclean data
fit_unclean <- lm(Median_Satisfaction ~ Median_Years + Customer_Age + Income_Level + Num_Transactions + Branch_Size + Location_Type, data = survey_data_unclean)
summary(fit_unclean)

# Removing duplicates and irrelevant observations
survey_data_clean <- survey_data_unclean[!duplicated(survey_data_unclean), ]

# Fixing structural errors and removing outliers
survey_data_clean$Num_Transactions[is.na(survey_data_clean$Num_Transactions)] <- median(survey_data_clean$Num_Transactions, na.rm = TRUE)
survey_data_clean$Branch_Size[survey_data_clean$Branch_Size > 100] <- NA  # Removing outliers
survey_data_clean$Branch_Size[is.na(survey_data_clean$Branch_Size)] <- median(survey_data_clean$Branch_Size, na.rm = TRUE)

# Running regression analysis on the clean data
fit_clean <- lm(Median_Satisfaction ~ Median_Years + Customer_Age + Income_Level + Num_Transactions + Branch_Size + Location_Type, data = survey_data_clean)
summary(fit_clean)

# Handling missing data
missing_data_summary <- sapply(survey_data_clean, function(x) sum(is.na(x)))
print(missing_data_summary)
survey_data_clean$Median_Satisfaction[is.na(survey_data_clean$Median_Satisfaction)] <- median(survey_data_clean$Median_Satisfaction, na.rm = TRUE)

# Validation
final_validation <- sum(is.na(survey_data_clean)) == 0
if (final_validation) {
  print("Validation successful: No missing data.")
} else {
  print("Validation failed: There are still missing values.")
}

# Multivariable Analysis
fit_multivariate <- lm(Median_Satisfaction ~ Median_Years + Customer_Age + Income_Level + Num_Transactions + Branch_Size + Location_Type, data = survey_data_clean)
summary(fit_multivariate)

# Simulating and cleansing data for a stratified sample, ensuring 100 rows
set.seed(123)

branch_number <- 1:100
median_years <- sample(0:10, 100, replace = TRUE)
median_satisfaction <- round(runif(100, 1, 10), 0)
customer_age <- sample(18:80, 100, replace = TRUE)
income_level <- sample(c("Low", "Medium", "High"), 100, replace = TRUE)
num_transactions <- sample(1:20, 100, replace = TRUE)
branch_size <- sample(10:50, 100, replace = TRUE)
location_type <- sample(c("Urban", "Suburban", "Rural"), 100, replace = TRUE)

survey_data <- data.frame(
  Branch_Number = branch_number, 
  Median_Years = median_years, 
  Median_Satisfaction = median_satisfaction,
  Customer_Age = customer_age,
  Income_Level = income_level,
  Num_Transactions = num_transactions,
  Branch_Size = branch_size,
  Location_Type = location_type
)

head(survey_data)

# Check for duplicates and remove them
duplicates <- survey_data[duplicated(survey_data), ]
survey_data <- survey_data[!duplicated(survey_data), ]

# Checking for structural errors and fixing them
str(survey_data)
survey_data$Median_Years <- as.integer(survey_data$Median_Years)
survey_data$Num_Transactions <- as.integer(survey_data$Num_Transactions)

# Identify and handle outliers
outliers_satisfaction <- boxplot.stats(survey_data$Median_Satisfaction)$out
survey_data$Outlier <- ifelse(survey_data$Median_Satisfaction %in% outliers_satisfaction, TRUE, FALSE)
outliers <- survey_data[survey_data$Outlier == TRUE, ]
print(outliers)

# Handling missing data
missing_data_summary <- sapply(survey_data, function(x) sum(is.na(x)))
print(missing_data_summary)
survey_data$Median_Satisfaction[is.na(survey_data$Median_Satisfaction)] <- median(survey_data$Median_Satisfaction, na.rm = TRUE)

# Final validation
final_validation <- sum(is.na(survey_data)) == 0
if (final_validation) {
  print("Validation successful: No missing data.")
} else {
  print("Validation failed: There are still missing values.")
}


