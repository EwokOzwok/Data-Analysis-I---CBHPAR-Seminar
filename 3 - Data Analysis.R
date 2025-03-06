
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  ART of R - Organizational Headings Tool                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages("ARTofR") # Install only once
# library(ARTofR)
# ARTofR::xxx_title1('Clean Data (EASY)')
# ARTofR::xxx_title3('View Results')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------- CLEAN DATA (EASY)-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This command runs the entirety of '1 - Data Cleaning.R' in the current session.
source("1 - Data Cleaning.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- SELECT VARIABLES OF INTEREST--------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


colnames(df)

data<-df[,c(4,5,283:291,295:304,347:354)]

head(data)
colnames(data)

# Keep only rows where gender is either male or female

data$remove<-ifelse(data$Gender_Female == 0 & data$Gender_Male==0,1,0)

table(data$remove)

data<- data[data$remove == 0, ]

data<-dplyr::select(data,-remove)

str(data)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Compute Variables  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUDIT

# Select the AUDIT columns (AUDIT_1 to AUDIT_10)
audit_columns <- paste0("AUDIT_", 1:10)

# Create a new variable audit_total that sums the AUDIT scores for rows with no NAs
data$audit_total <- rowSums(data[, audit_columns], na.rm = FALSE)

# For rows with NAs in any of the AUDIT columns, set audit_total to NA
data$audit_total[!complete.cases(data[, audit_columns])] <- NA

describe(data$audit_total)
hist(data$audit_total)


# Depression

# Select the PHQ9 columns (PHQ9_1 to PHQ9_9)
phq9_columns <- paste0("PHQ9_", 1:9)

# Create a new variable phq9_total that sums the PHQ9 scores for rows with no NAs
data$phq9_total <- rowSums(data[, phq9_columns], na.rm = FALSE)

# For rows with NAs in any of the PHQ9 columns, set phq9_total to NA
data$phq9_total[!complete.cases(data[, phq9_columns])] <- NA

describe(data$phq9_total)
hist(data$phq9_total)


# Flourishing

# Select the Diener columns (Diener1 to Diener8)
diener_columns <- paste0("Diener", 1:8)

# Create a new variable diener_mean that calculates the mean of the Diener scores for rows with no NAs
data$diener_mean <- rowMeans(data[, diener_columns], na.rm = FALSE)

# For rows with NAs in any of the Diener columns, set diener_mean to NA
data$diener_mean[!complete.cases(data[, diener_columns])] <- NA


describe(data$diener_mean)
hist(data$diener_mean)


# Clean up data to remove individual items
data<-dplyr::select(data, Gender_Male, audit_total, phq9_total, diener_mean)




# Examine missingness
finalfit::missing_plot(data)

mcar_test(data)
# Non-Significant MCAR Test -> Data is missing completely at random
# List-wise deletion or Multiple Imputation appropriate
data <- na.omit(data)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ View Descriptives  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~
describe(data)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Examine Histograms  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfrow = c(1,3))
hist(data$audit_total, main = "AUDIT Scores")
hist(data$diener_mean, main = "Flourishing Scores")
hist(data$phq9_total, main = "PHQ-9 Scores")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ View Correlation Matrix  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cor.plot(data[,2:4])
lowerCor(data[,2:4])


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Compare Means (t-tests)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Test for equality of variances

var.test(data$audit_total ~ data$Gender_Male) # Unequal Variance
var.test(data$phq9_total ~ data$Gender_Male) # Equal Variance
var.test(data$diener_mean ~ data$Gender_Male) # Unequal Variance

# the variacne of audit_total & diener_mean differ signficantly by Male/Female (non-equal variance)

var(na.omit(data[data$Gender_Male==1, "audit_total"]))
var(na.omit(data[data$Gender_Male==0, "audit_total"]))


var(na.omit(data[data$Gender_Male==1, "phq9_total"]))
var(na.omit(data[data$Gender_Male==0, "phq9_total"]))


var(na.omit(data[data$Gender_Male==1, "diener_mean"]))
var(na.omit(data[data$Gender_Male==0, "diener_mean"]))


t.test(data$audit_total ~ data$Gender_Male, var.equal = F) 
t.test(data$phq9_total ~ data$Gender_Male, var.equal = T)
t.test(data$diener_mean ~ data$Gender_Male, var.equal = F) 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Multiple Linear Regression (MLR)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DV = Depression
# IVs = Gender, AUDIT, and Flourishing

par(mfrow=c(1,1))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Assumptions of MLR  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1: Independence of observations (study design)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2: No measurement error in X (reliable / valid measures used given target pop)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3: Model is correctly specified  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4: Linear Relationship between each X & Y  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The 4th assumption must be tested before running the analysis

plot(data$audit_total, data$phq9_total, xlab = "AUDIT", ylab = "Depression", main= "Plot of AUDIT and Depression")
fit<-lm(phq9_total~audit_total, data)
abline(fit, col="red")

plot(data$diener_mean, data$phq9_total, xlab = "Flourishing", ylab = "Depression", main= "Plot of Flourishing and Depression")
fit<-lm(phq9_total~diener_mean, data)
abline(fit, col="red")

# NOT LINEAR?: Look into exponential or quadradic regression (i.e., poloynomial regression)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 5: Homoskedasticity of residuals  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# You must fit the model to test the 5th assumption 
fit<-lm(phq9_total~audit_total+diener_mean+Gender_Male, data)


# Generate the diagnostic plots
par(mfrow=c(2,2))
plot(fit)


# Look at the plot(fit) matrix
# Uniform distribution of residuals around Y (0) in Resid. vs. Fitted?
# Scale-Location graph's line sticking close to center?  (some ups & down's are ok)

# Violated the 5th Assumption? Look into Weighted Least Sq. Regression (WLS)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 6: Independence of Residuals - related to 1st assumption  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To test for the 6th assumption, run a Durbin-Watson test (car package)
# The Durbin-Watson statistic tests for autocorrelation in the residuals (ERROR) of a regression model
# install.packages("car") # Uncomment and install if needed
library(car)
durbinWatsonTest(fit)

# Null Hypothesis is "There is no autocorrelation in the residuals of a regression model"
# Therefore, we MUST SEE NON-SIGNIFICANT RESULTS 
# This test is sensitive with very large sample sizes

# Violated the 6th Assumption?
# Check for model misspecification
# Consider nonlinear relationships
# Try Autoregressive (AR) or Generalized Least Squares (GLS) Models
# Log Transform data
# STILL SIGNIFICANT? Use HAC (Heteroskedasticity and Autocorrelation Consistent) standard errors (e.g., Newey-West estimator)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 7: Normally Distributed Residuals  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To satisfy the 7th Assumption, the Q-Q plot from plot(fit) should not deviate much from the diagonal
# Often there is some deviation at the tails, this is mostly ok...

# DEVIATING TOO MUCH (i.e., violated the 7th assumption)? 
# Increase Sample size
# Transform the data and re-test
# Still violating this assumption? Try WLS (NOT IDEAL... to say the least)

plot(fit)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 8: No Multicollinearity  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variable Inflation Factor (VIF) should be less than 5 for all predictors
# VIF MUST BE less than 10
vif(fit)

# Violated the 8th assumption?
# Remove Highly Correlated Predictors
# Combine Highly Correlated Predictors (assumping they're measuring the same thing...)
# Increase Sample Size


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 9: No influential Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Leverage: X-AXIS (i.e., Independent Variable) Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get the hat values (leverage values)
leverage_values <- hatvalues(fit)

# Display leverage values
print(leverage_values)

# Calculate the average leverage value
avg_leverage <- mean(leverage_values)

# Set a threshold for high leverage (commonly 2 * p / n)
# p is the number of predictors, and n is the number of observations
threshold <- 2 * length(coef(fit)) / nrow(data)

# Find data points with leverage greater than the threshold
high_leverage_points <- which(leverage_values > threshold)

# Display high leverage points
cat("High leverage points are at indices:", high_leverage_points, "\n")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Discrepancy: Y-AXIS (i.e., Dependent Variable) Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages('olsrr') # Uncomment and install if needed
library(olsrr)


# olsrr package: Studentized deleted residuals
# Generate Studentized residual plot

ols_plot_resid_stud(fit)

# olsrr's default threshold is for large samples 
# The more conservative values are: 
#                                   - "2" (small n) 
#                                   - "3-4" (n>100)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Influence: Captures BOTH Leverage & Discrepancy  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DFFIT (Standardized Difference in Fit): Measures the change in fitted values when an observation is removed. 
ols_plot_dffits(fit)
# Large values (typically >1) indicate influential points.


# Cook's Distance: Combines leverage and residual to detect influential points. 
ols_plot_cooksd_bar(fit)
# Values >1 (or 4/n) suggest strong influence.



# Standardized DFBETA: Measures the change in each coefficient when an observation is removed. 
ols_plot_dfbetas(fit)
# Large values (>1) indicate influential points.


# Influential Outliers?
# Remove them and re-run analysis

# Use Row Numbers in -c() for outliers you want to remove
data_rvmd_outliers <- data[-c(5,6,82) , ]

# Re-run the regression analysis with the new data with outliers removed and test all assumptions again...
fit<-lm(phq9_total~audit_total+diener_mean+Gender_Male, data_rvmd_outliers)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##           SATISFIED THE ASSUMPTIONS OF MLR? Display your results         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~
##  ~ View Results  ----
##~~~~~~~~~~~~~~~~~~~~~~

summary(fit)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PART 1 of Output: Model Specification  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Call:
#   lm(formula = phq9_total ~ audit_total + diener_mean + Gender_Male, 
#      data = data_rvmd_outliers)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PART 2 of Output: Residuals  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Min = further point below the regression line
# Max = point that is furthest above the regression line

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.2695 -3.4080  0.0787  2.9830  9.4016 

# We want the Median of Residuals to be close to 0
# We want 1Q and 3Q to be roughly the same magnitude
# We want Min and Max to be roughly the same magnitude



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PART 3 of Output: Coefficients  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Coefficients:
#              Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)  27.2243     4.0426    6.734   1.88e-08 ***
# audit_total   0.1640     0.1356    1.209   0.2325    
# diener_mean  -3.3400     0.7062   -4.730   2.01e-05 ***
# Gender_Male  -2.8686     1.3805   -2.078   0.0431 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Interpretations:
# B0(intercept) = 27.22, t(48) = 6.734, p < .0001
# The intercept represents the expected PHQ-9 total score when all predictors are equal to zero. 
# The result is statistically significant, indicating that the intercept is different than (i.e., not equal to) zero.

# B1(audit_total) = 0.1640, t(48) = 1.209, p = 0.2325
# The coefficient for audit_total indicates that for each one-unit increase in audit_total, the PHQ-9 score is expected to increase by 0.164. 
# However, the t-test is not statistically significant (p > 0.05), so there is no evidence to suggest that audit_total is a meaningful predictor of PHQ-9 total score.

# B2(diener_mean) = -3.3400, t(48) = -4.730, p < .0001
# The coefficient for diener_mean indicates that for each one-unit increase in diener_mean, the PHQ-9 score is expected to decrease by 3.34. 
# This result is statistically significant (p < 0.0001), suggesting that diener_mean is a strong negative predictor of PHQ-9 total score.

# B3(Gender_Male) = -2.8686, t(48) = -2.078, p = 0.0431 
# The coefficient for Gender_Male (where 0 = female, 1 = male) indicates that, on average, males are expected to have a PHQ-9 score that is 2.87 points lower than females, 
# when holding the other variables constant. This result is statistically significant (p < 0.05), suggesting gender (male vs. female) is an important factor in predicting PHQ-9 scores.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PART 4 of Output: Goodness of Fit  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Residual standard error: 4.696 on 48 degrees of freedom
# Multiple R-squared:  0.3676,	Adjusted R-squared:  0.3281 
# F-statistic: 9.302 on 3 and 48 DF,  p-value: 5.879e-05


# Residual standard error = 4.696 on 48 degrees of freedom
# The residual standard error represents the average deviation of the observed PHQ-9 scores from the predicted values. 
# On average, the predictions deviate from the actual PHQ-9 scores by 4.696 units.

# Multiple R-squared = 0.3676
# Approximately 36.76% of the variability in PHQ-9 scores can be explained by the predictors in the model (audit_total, diener_mean, and Gender_Male).

# Adjusted R-squared = 0.3281
# After adjusting for the number of predictors in the model, about 32.81% of the variability in PHQ-9 scores is explained. 
# Adjusted R-squared accounts for the number of predictors, providing a more accurate measure of model fit.

# F-statistic = 9.302 on 3 and 48 DF, p-value = 5.879e-05
# The F-statistic tests the overall significance of the model. A value of 9.302 with a p-value < 0.0001 indicates that the model is statistically significant, 
# meaning that at least one of the predictors (audit_total, diener_mean, or Gender_Male) is significantly related to the PHQ-9 score.


library(rempsyc)
lm_results_table <- nice_lm(fit)
lm_results_table <- dplyr::select(lm_results_table, -`Dependent Variable`)
lm_results_table <- nice_table(lm_results_table, title = "Multiple Linear Regression Results")

print(lm_results_table, preview = "docx")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Robust Regression  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ONE OF THE WAYS TO DEAL WITH INFLUENTIAL OUTLIERS WITHOUT DELETING THEM
# OR TO REPORT THE DIFFERENCES IN ROBUST REG OUTCOMES WITH OUTLIERS PRESENT OR ABSENT


# Use the summaryR function (lower-case 's')
# FIRST, run the function below
# Chose the type of robust regression you wish to use "hc3", "hc0...1...2...4" hc3 is most common.
#     Best bet, choose the one with the most conservative output? - just my approach


# RUN THIS FUNCTION FIRST OR summaryR won't work!
summaryR <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
  
  if (!require(car)) stop("Required car package is missing.")
  
  type <- match.arg(type)
  V <- hccm(model, type=type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[,2] <- sqrt(diag(V))
  table[,3] <- table[,1]/table[,2]
  table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
  
  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- cbind(0, diag(p - 1))
  sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
  sumry$type <- type
  
  return(sumry)
}


summaryR(fit, type="hc3")

confint(fit, level=0.95)

