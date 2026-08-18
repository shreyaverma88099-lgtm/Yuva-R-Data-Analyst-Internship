# ==========================================
# WEEK 2 - DATA VISUALIZATION AND
# INSIGHT COMMUNICATION USING R
# ==========================================

# Load required library
library(ggplot2)

# Set working directory
setwd("C:/Users/Shreya/OneDrive/Desktop/Week1_R_Internship")

# Load the cleaned Adult dataset
adult <- read.csv(
  "adult_cleaned.csv",
  stringsAsFactors = FALSE
)

# Inspect the dataset
str(adult)
dim(adult)
head(adult)




# ==========================================
# VISUALIZATION 1: INCOME DISTRIBUTION
# ==========================================

ggplot(adult, aes(x = income)) +
  geom_bar() +
  labs(
    title = "Income Group Distribution",
    x = "Income Group",
    y = "Number of Individuals"
  ) +
  theme_minimal()



# ==========================================
# VISUALIZATION 2: AGE DISTRIBUTION
# ==========================================

ggplot(adult, aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age Distribution of Individuals",
    x = "Age",
    y = "Number of Individuals"
  ) +
  theme_minimal()



# ==========================================
# VISUALIZATION 3: AGE VS INCOME
# ==========================================

ggplot(adult, aes(x = income, y = age)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Income Group",
    x = "Income Group",
    y = "Age"
  ) +
  theme_minimal()




# ==========================================
# VISUALIZATION 4: EDUCATION VS INCOME
# ==========================================

ggplot(adult, aes(x = income, y = education.num)) +
  geom_boxplot() +
  labs(
    title = "Education Level by Income Group",
    x = "Income Group",
    y = "Education Level"
  ) +
  theme_minimal()



# ============================================
# VISUALIZATION 5: AGE VS WORKING HOURS
# ============================================

ggplot(adult, aes(x = age, y = hours.per.week, color = income)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Age vs Working Hours by Income Group",
    x = "Age",
    y = "Hours Worked per Week",
    color = "Income Group"
  ) +
  theme_minimal()



# ==========================================
# VISUALIZATION 6: INCOME BY OCCUPATION
# ==========================================

ggplot(adult, aes(x = occupation, fill = income)) +
  geom_bar(position = "fill") +
  labs(
    title = "Income Distribution by Occupation",
    x = "Occupation",
    y = "Proportion",
    fill = "Income Group"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# ==========================================
# VISUALIZATION 7: WORKING HOURS VS INCOME
# ==========================================

ggplot(adult, aes(x = income, y = hours.per.week)) +
  geom_boxplot() +
  labs(
    title = "Working Hours by Income Group",
    x = "Income Group",
    y = "Hours Worked per Week"
  ) +
  theme_minimal()



# ============================================
# VISUALIZATION 8: INCOME BY MARITAL STATUS
# ============================================

ggplot(adult, aes(x = marital.status, fill = income)) +
  geom_bar(position = "fill") +
  labs(
    title = "Income Distribution by Marital Status",
    x = "Marital Status",
    y = "Proportion",
    fill = "Income Group"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# ============================================
# VISUALIZATION 9: INCOME BY SEX
# ============================================

ggplot(adult, aes(x = sex, fill = income)) +
  geom_bar(position = "fill") +
  labs(
    title = "Income Distribution by Sex",
    x = "Sex",
    y = "Proportion",
    fill = "Income Group"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()




# ============================================
# VISUALIZATION 10: CORRELATION HEATMAP
# ============================================

correlation_data <- adult[, c(
  "age",
  "education.num",
  "capital.gain",
  "capital.loss",
  "hours.per.week"
)]

correlation_matrix <- cor(correlation_data)

correlation_df <- as.data.frame(as.table(correlation_matrix))

ggplot(correlation_df,
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 4) +
  labs(
    title = "Correlation Heatmap of Numerical Variables",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )