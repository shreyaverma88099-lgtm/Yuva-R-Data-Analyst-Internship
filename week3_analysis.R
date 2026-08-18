getwd()
list.files()

setwd("C:/Users/Shreya/OneDrive/Desktop/Week1_R_Internship")
getwd()
list.files()


# Load cleaned Adult dataset
adult <- read.csv("adult_cleaned.csv")

# Inspect dataset
str(adult)
dim(adult)
head(adult)



# Check income distribution

income_counts <- table(adult$income)
income_counts

# Calculate percentages

income_percent <- round(
  prop.table(income_counts) * 100,
  2
)

income_percent



# Hypothesis Test: Age vs Income

age_test <- t.test(age ~ income, data = adult)

age_test



# Correlation between education level and age

cor_test <- cor.test(
  adult$education.num,
  adult$age,
  method = "pearson"
)

cor_test



# Normality check for age
shapiro.test(sample(adult$age, 5000))



# Histogram of age
hist(
  adult$age,
  main = "Distribution of Age",
  xlab = "Age",
  breaks = 30
)



# Hypothesis Test: Education Level vs Income

education_test <- t.test(
  education.num ~ income,
  data = adult
)

education_test



# Chi-square Test: Sex vs Income

sex_income_table <- table(adult$sex, adult$income)

sex_income_table

chi_test <- chisq.test(sex_income_table)

chi_test




# Prepare target variable for classification

adult$income <- factor(
  adult$income,
  levels = c("<=50K", ">50K")
)

# Check target variable
str(adult$income)
table(adult$income)


# Missing values in the dataset
colSums(is.na(adult))


# Install caret if needed
if (!require(caret)) {
  install.packages("caret")
}

library(caret)

# Stratified 80-20 train-test split

set.seed(123)

# Get row indices for each income class
low_income <- which(adult$income == "<=50K")
high_income <- which(adult$income == ">50K")

# Select 80% from each class
train_low <- sample(
  low_income,
  size = floor(0.80 * length(low_income))
)

train_high <- sample(
  high_income,
  size = floor(0.80 * length(high_income))
)

# Combine training indices
train_index <- c(train_low, train_high)

# Create training and testing datasets
train_data <- adult[train_index, ]
test_data <- adult[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)

# Check class proportions
prop.table(table(train_data$income))
prop.table(table(test_data$income))





# Final Logistic Regression Model

logistic_model_final <- glm(
  income ~ age + education.num + hours.per.week +
    capital.gain + capital.loss +
    workclass + marital.status +
    relationship + race + sex,
  data = train_data,
  family = binomial
)

summary(logistic_model_final)


# Check income distribution of rare workclass categories

table(train_data$workclass, train_data$income)




# Combine very rare workclass categories

train_data$workclass <- as.character(train_data$workclass)
test_data$workclass <- as.character(test_data$workclass)

rare_workclass <- c(
  "Never-worked",
  "Without-pay"
)

train_data$workclass[
  train_data$workclass %in% rare_workclass
] <- "Other"

test_data$workclass[
  test_data$workclass %in% rare_workclass
] <- "Other"

# Convert back to factor
train_data$workclass <- factor(train_data$workclass)
test_data$workclass <- factor(
  test_data$workclass,
  levels = levels(train_data$workclass)
)

# Check result
table(train_data$workclass)





# Rebuild Logistic Regression after handling rare categories

logistic_model_final <- glm(
  income ~ age + education.num + hours.per.week +
    capital.gain + capital.loss +
    workclass + marital.status +
    relationship + race + sex,
  data = train_data,
  family = binomial
)

summary(logistic_model_final)



# Predict income probabilities on test data

test_prob <- predict(
  logistic_model_final,
  newdata = test_data,
  type = "response"
)

# View first few predicted probabilities
head(test_prob)



# Convert probabilities into predicted income classes

test_pred <- ifelse(
  test_prob >= 0.5,
  ">50K",
  "<=50K"
)

# Convert to factor with same levels as actual income
test_pred <- factor(
  test_pred,
  levels = levels(test_data$income)
)

# Check predictions
head(test_pred)

# Check predicted class distribution
table(test_pred)



# Create confusion matrix

confusion_matrix <- table(
  Actual = test_data$income,
  Predicted = test_pred
)

confusion_matrix



# Calculate model performance metrics

TN <- confusion_matrix["<=50K", "<=50K"]
FP <- confusion_matrix["<=50K", ">50K"]
FN <- confusion_matrix[">50K", "<=50K"]
TP <- confusion_matrix[">50K", ">50K"]

accuracy <- (TP + TN) / (TP + TN + FP + FN)

precision <- TP / (TP + FP)

recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)

accuracy
precision
recall
f1_score



# ROC Curve and AUC

roc_curve <- roc(
  test_data$income,
  test_prob,
  levels = c("<=50K", ">50K"),
  direction = "<"
)

auc_value <- auc(roc_curve)

auc_value



# ROC Curve and AUC without pROC

actual <- ifelse(test_data$income == ">50K", 1, 0)

thresholds <- sort(unique(test_prob), decreasing = TRUE)

TPR <- numeric(length(thresholds))
FPR <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  
  predicted <- ifelse(test_prob >= thresholds[i], 1, 0)
  
  TP <- sum(predicted == 1 & actual == 1)
  FN <- sum(predicted == 0 & actual == 1)
  FP <- sum(predicted == 1 & actual == 0)
  TN <- sum(predicted == 0 & actual == 0)
  
  TPR[i] <- TP / (TP + FN)
  FPR[i] <- FP / (FP + TN)
}

# Add starting and ending points
FPR <- c(0, FPR, 1)
TPR <- c(0, TPR, 1)

# Sort by FPR
ord <- order(FPR)
FPR <- FPR[ord]
TPR <- TPR[ord]

# Calculate AUC using trapezoidal rule
auc_value <- sum(
  diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2
)

auc_value



# Plot ROC Curve

plot(
  FPR,
  TPR,
  type = "l",
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "ROC Curve - Logistic Regression"
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

text(
  0.6,
  0.2,
  paste("AUC =", round(auc_value, 3))
)




# Calculate odds ratios for the final logistic regression model

odds_ratios <- exp(coef(logistic_model_final))

# Display odds ratios
round(odds_ratios, 3)

levels(train_data$workclass)
levels(train_data$marital.status)
levels(train_data$relationship)
levels(train_data$race)
levels(train_data$sex)

logistic_model_final$xlevels
round(odds_ratios, 3)




# Important Predictors - Odds Ratio Plot

# Remove intercept
or_data <- odds_ratios[names(odds_ratios) != "(Intercept)"]

# Select 10 predictors with largest absolute effect
top_predictors <- sort(
  abs(log(or_data)),
  decreasing = TRUE
)[1:10]

# Get their actual odds ratios
top_or <- or_data[names(top_predictors)]

# Sort for plotting
top_or <- sort(top_or)

# Horizontal bar plot
barplot(
  top_or,
  horiz = TRUE,
  las = 1,
  xlab = "Odds Ratio",
  main = "Top Predictors of Income >50K",
  border = "black"
)

# Reference line: OR = 1
abline(v = 1, lty = 2)

# Add values
text(
  x = top_or,
  y = seq_along(top_or),
  labels = round(top_or, 2),
  pos = 4,
  cex = 0.8
)



# Improved Odds Ratio Plot

par(mar = c(5, 12, 4, 2))

barplot(
  top_or,
  horiz = TRUE,
  las = 1,
  xlab = "Odds Ratio",
  main = "Top Predictors of Income >50K",
  border = "black",
  cex.names = 0.8
)

abline(v = 1, lty = 2)

text(
  x = top_or,
  y = seq_along(top_or),
  labels = round(top_or, 2),
  pos = 4,
  cex = 0.8
)



# Graph 4: Actual vs Predicted Income

actual_counts <- table(test_data$income)
predicted_counts <- table(test_pred)

comparison <- rbind(
  Actual = actual_counts,
  Predicted = predicted_counts
)

barplot(
  comparison,
  beside = TRUE,
  names.arg = c("<=50K", ">50K"),
  legend.text = TRUE,
  args.legend = list(
    x = "topright",
    bty = "n"
  ),
  ylab = "Number of Individuals",
  main = "Actual vs Predicted Income Classes",
  border = "black"
)


# Identify influential observations using Cook's Distance

cooks_d <- cooks.distance(logistic_model_final)

# Top 10 most influential observations
top_influential <- order(cooks_d, decreasing = TRUE)[1:10]

# Display their Cook's Distance values
data.frame(
  Observation = top_influential,
  Cooks_Distance = round(cooks_d[top_influential], 5)
)


# Common Cook's Distance cutoff
cook_cutoff <- 4 / nrow(train_data)

cook_cutoff

# Count observations above the cutoff
sum(cooks_d > cook_cutoff)


# Check multicollinearity

library(car)

vif(logistic_model_final)



# Manual VIF check using predictor correlations

# Create model matrix for numeric + categorical predictors
x <- model.matrix(
  ~ age + education.num + hours.per.week +
    capital.gain + capital.loss +
    workclass + marital.status +
    relationship + race + sex,
  data = train_data
)[, -1]

# Calculate VIF for each predictor
vif_values <- numeric(ncol(x))

for (i in 1:ncol(x)) {
  other_vars <- x[, -i, drop = FALSE]
  
  temp_model <- lm(
    x[, i] ~ other_vars
  )
  
  r_squared <- summary(temp_model)$r.squared
  vif_values[i] <- 1 / (1 - r_squared)
}

vif_results <- data.frame(
  Predictor = colnames(x),
  VIF = round(vif_values, 3)
)

vif_results