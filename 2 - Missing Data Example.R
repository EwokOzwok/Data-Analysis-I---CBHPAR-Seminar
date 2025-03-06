##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- MISSING DATA ANALYSIS-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot all missing data (light blue)
missing_plot(df)



##~~~~~~~~~~~~~~
##  ~ MCAR  ----
##~~~~~~~~~~~~~~


# Load necessary libraries
# install.packages("tidyverse") # UNCOMMENT AND INSTALL IF NEEDED
library(tidyverse)

# Set a random seed for reproducibility
set.seed(123)

# Step 1: Simulate a dataset with 1000 observations
n <- 1000
data <- tibble(
  age = sample(18:80, n, replace = TRUE),  # Age: random between 18 and 80
  income = sample(c(20000, 30000, 50000, 70000, 100000), n, replace = TRUE),  # Income: discrete values
  satisfaction = sample(1:5, n, replace = TRUE)  # Satisfaction: scale from 1 to 5
)



# Load the necessary package for Little's MCAR test
# install.packages("naniar") # UNCOMMENT AND INSTALL IF NEEDED
library(naniar)

# Simulate data with MCAR: Introduce missingness randomly
data_mcar <- data %>%
  mutate(income = ifelse(runif(n) < 0.1, NA, income))  # 10% missingness randomly in 'income'

finalfit::missing_plot(data_mcar)

# Perform Little's MCAR test
mcar_test <- mcar_test(data_mcar)
print(mcar_test)

# Non-Significant Results means we cannot REJECT THE NULL HYPOTHESIS!
# The null hypothesis for Little's MCAR test is "The Data is Missing Completely at Random"

# Therefore, Non-significant is GOOD! --> It means our data is MCAR!


# List-wise deletion (Delete rows with any missingness)
data_mcar_nona <- na.omit(data_mcar)

# If your sample size drops significantly, you can use Multiple Imputation as well (see example in MAR below)

finalfit::missing_plot(data_mcar_nona)
print(paste("Number of missing rows removed = ", nrow(data_mcar)-nrow(data_mcar_nona), sep = ""))



##~~~~~~~~~~~~~
##  ~ MAR  ----
##~~~~~~~~~~~~~


set.seed(123)  # For reproducibility

# Simulate data
data <- tibble(
  age = sample(18:80, 1000, replace = TRUE),  # Age between 18 and 80
  satisfaction = sample(1:5, 1000, replace = TRUE),  # Satisfaction on a scale of 1 to 5
  income = sample(20000:100000, 1000, replace = TRUE)  # Income between 20k and 100k
)

# Simulate MAR: Probability of missing 'income' depends on 'age'
# Older individuals have a higher probability of missingness
data_mar <- data %>%
  mutate(
    # Simulate missingness using a logistic function
    prob_missing_income = plogis(age - 50),  # Age > 50 increases the probability of missing income
    income = ifelse(runif(n()) < prob_missing_income, NA, income)  # Simulate missingness based on probability
  )

data_mar <- select(data_mar, -prob_missing_income) # remove the probability of missingness variable

finalfit::missing_plot(data_mar)
finalfit::missing_pattern(data_mar)

# Perform Little's MCAR test
mcar_test <- mcar_test(data_mar)
print(mcar_test)

# Significant Results means we MUST REJECT THE NULL HYPOTHESIS!
# The null hypothesis for Little's MCAR test is "The Data is Missing Completely at Random"

# Therefore, Significant is BAD! --> It means our data is NOT Missing Completely at random (MCAR)!
# It is either MAR or MNAR! Fingers crossed for MAR...


# MAR Tests
data_mar_test <- data_mar

# Create a missingness indicator for 'income'
data_mar_test$missing_income <- is.na(data_mar_test$income)



# Run a logistic regression model predicting missingness in income 
# Since we only have 1 variable with missingness, we can run it once
# If multiple variables have missingness, you would need to run this for each variable with missingness
glm_test_mar <- glm(missing_income ~ age + satisfaction, data = data_mar_test, family = binomial)

# Summary of the model
summary(glm_test_mar)

# The intercept is significant, suggesting that the baseline likelihood of missing income is not trivially close to 50% (i.e., Missingness is not completely random)
# Age is significant, with a coefficient of .89 
# In other words, for each additional year of age, the log-odds of missing income increases by 0.890027.
# Converted to an Odds ratio: exp(.89)
exp(.89)

# This means that for each additional year of age, the odds of missing income are 2.43 times higher 
# compared to individuals who are one year younger, holding satisfaction constant.


# Perform multiple imputation using MICE
# Specify which variables are to be imputed, here we are imputing 'income'
# install.packages("mice")
library(mice)  # UNCOMMENT AND INSTALL IF NEEDED
mice_imputation <- mice(data_mar, method = 'pmm', m = 5, seed = 123)

# Check the imputed values
summary(mice_imputation)

# Generate a completed dataset
completed_data <- complete(mice_imputation)

# View the completed data
finalfit::missing_plot(completed_data)




##~~~~~~~~~~~~~~
##  ~ MNAR  ----
##~~~~~~~~~~~~~~

set.seed(123)  # For reproducibility

# Simulate data
data <- tibble(
  age = sample(18:80, 1000, replace = TRUE),  # Age between 18 and 80
  satisfaction = sample(1:5, 1000, replace = TRUE),  # Satisfaction on a scale of 1 to 5
  income = sample(20000:100000, 1000, replace = TRUE)  # Income between 20k and 100k
)




# Simulate MNAR: Probability of missing 'income' depends on the unobserved 'income' values themselves
# Lower income individuals are more likely to have missing income

data_mnar <- data %>%
  mutate(
    # Simulate missingness using a logistic function based on income (MNAR)
    prob_missing_income = plogis(50000 - income),  # Lower income increases the probability of missingness
    income = ifelse(runif(n()) < prob_missing_income, NA, income)  # Simulate missingness based on the unobserved income
  )


finalfit::missing_plot(data_mnar)

mcar_test <- mcar_test(data_mnar)
print(mcar_test)

data_mnar <- select(data_mnar, -prob_missing_income)


# Create a missingness indicator for 'income'
data_mnar$missing_income <- is.na(data_mnar$income)

# Run a logistic regression model to test if missingness is related to observed variables (age, satisfaction)
glm_test_mnar <- glm(missing_income ~ age + satisfaction, data = data_mnar, family = binomial)

# Summary of the model
summary(glm_test_mnar)


# The intercept is significant, suggesting that the baseline likelihood of missing income is not trivially close to 50%.

# Both Age & Satisfaction are not significant in predicting missingness. 
# This implies that, in this model, neither age nor satisfaction does not have a clear influence on whether income is missing or not.
