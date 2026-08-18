# Load the Adult dataset

adult <- read.csv(
  "adult.data.txt",
  header = FALSE,
  na.strings = "?",
  strip.white = TRUE
)

# Inspect the dataset

str(adult)
dim(adult)
head(adult)


# Add column names

names(adult) <- c(
  "age",
  "workclass",
  "fnlwgt",
  "education",
  "education.num",
  "marital.status",
  "occupation",
  "relationship",
  "race",
  "sex",
  "capital.gain",
  "capital.loss",
  "hours.per.week",
  "native.country",
  "income"
)

# Check column names and first 10 rows

names(adult)
head(adult, 10)

# Check missing values in each column

missing_values <- colSums(is.na(adult))
missing_values

# Calculate percentage of missing values

missing_percent <- round(
  colSums(is.na(adult)) / nrow(adult) * 100,
  2
)

missing_percent

# Handle missing categorical values using the mode

get_mode <- function(x) {
  x <- na.omit(x)
  names(sort(table(x), decreasing = TRUE))[1]
}

adult$workclass[is.na(adult$workclass)] <- get_mode(adult$workclass)
adult$occupation[is.na(adult$occupation)] <- get_mode(adult$occupation)
adult$native.country[is.na(adult$native.country)] <- get_mode(adult$native.country)


# Verify missing values after cleaning

colSums(is.na(adult))

# Identify numerical variables

numeric_vars <- sapply(adult, is.numeric)
numeric_vars



# Detect outliers using the IQR method

find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  sum(x < lower | x > upper, na.rm = TRUE)
}

outlier_counts <- sapply(adult[, numeric_vars], find_outliers)

outlier_counts


# Visualize outliers in hours worked per week

boxplot(
  adult$hours.per.week,
  main = "Boxplot of Hours Worked per Week",
  ylab = "Hours per Week"
)


# Generate summary statistics

summary(adult)

# Distribution of income groups

table(adult$income)



# Visualize income distribution

income_counts <- table(adult$income)

barplot(
  income_counts,
  main = "Income Group Distribution",
  xlab = "Income Group",
  ylab = "Number of Individuals"
)


# Compare age by income group

aggregate(age ~ income, data = adult, FUN = mean)



# Visualize age distribution by income group

boxplot(
  age ~ income,
  data = adult,
  main = "Age Distribution by Income Group",
  xlab = "Income Group",
  ylab = "Age"
)


# Compare education level by income group

aggregate(education.num ~ income, data = adult, FUN = mean)



# Visualize education level by income group

boxplot(
  education.num ~ income,
  data = adult,
  main = "Education Level by Income Group",
  xlab = "Income Group",
  ylab = "Education Level"
)



# Analyze income distribution by occupation

occupation_income <- table(adult$occupation, adult$income)

occupation_income

getwd()

setwd("C:/Users/Shreya/OneDrive/Desktop/Week1_R_Internship")
getwd()


occupation_income_pct <- prop.table(occupation_income, margin = 1) * 100
round(occupation_income_pct, 2)

barplot(
  t(occupation_income_pct),
  beside = TRUE,
  main = "Income Distribution by Occupation",
  xlab = "Occupation",
  ylab = "Percentage (%)",
  las = 2,
  col = c("lightgray", "darkgray")
)

# Compare working hours by income group

aggregate(hours.per.week ~ income, data = adult, FUN = mean)

# Visualize working hours by income group

boxplot(
  hours.per.week ~ income,
  data = adult,
  main = "Working Hours by Income Group",
  xlab = "Income Group",
  ylab = "Hours per Week"
)


# Analyze income distribution by marital status

marital_income <- table(adult$marital.status, adult$income)

marital_income_pct <- prop.table(marital_income, margin = 1) * 100

round(marital_income_pct, 2)


# Visualize income distribution by marital status

barplot(
  t(marital_income_pct),
  beside = TRUE,
  main = "Income Distribution by Marital Status",
  xlab = "Marital Status",
  ylab = "Percentage (%)",
  las = 2,
  legend.text = c("<=50K", ">50K")
)



# Analyze income distribution by sex

sex_income <- table(adult$sex, adult$income)

sex_income_pct <- prop.table(sex_income, margin = 1) * 100

round(sex_income_pct, 2)


# Visualize income distribution by sex

barplot(
  t(sex_income_pct),
  beside = TRUE,
  main = "Income Distribution by Sex",
  xlab = "Sex",
  ylab = "Percentage (%)",
  legend.text = c("<=50K", ">50K")
)


# Correlation analysis of numerical variables

correlation_data <- adult[, c(
  "age",
  "education.num",
  "capital.gain",
  "capital.loss",
  "hours.per.week"
)]

correlation_matrix <- cor(correlation_data)

round(correlation_matrix, 2)


# Save cleaned dataset

write.csv(
  adult,
  "adult_cleaned.csv",
  row.names = FALSE
)


# Identify numerical variables
numeric_vars <- sapply(adult, is.numeric)

# Normalize numerical variables using Min-Max normalization
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

adult_normalized <- adult

adult_normalized[numeric_vars] <- lapply(
  adult_normalized[numeric_vars],
  normalize
)

# Check normalized numerical variables
summary(adult_normalized[numeric_vars])


# Encode categorical variables

categorical_vars <- sapply(adult, is.character)

adult_encoded <- adult

adult_encoded[categorical_vars] <- lapply(
  adult_encoded[categorical_vars],
  as.factor
)

# Verify encoding
str(adult_encoded[categorical_vars])


# Save normalized dataset
write.csv(
  adult_normalized,
  "adult_normalized.csv",
  row.names = FALSE
)

# Save encoded dataset
write.csv(
  adult_encoded,
  "adult_encoded.csv",
  row.names = FALSE
)


list.files()
