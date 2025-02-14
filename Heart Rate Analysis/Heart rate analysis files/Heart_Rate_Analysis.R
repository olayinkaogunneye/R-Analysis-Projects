# ----------------------------------------------
# Heart Disease Analysis - R Script
# Author: Olayinka Ogunneye
# Date: "01/02/2025"
# ----------------------------------------------

# PROJECT INTRODUCTION  
# Millions of people develop heart disease each year, making it one of the leading causes of death worldwide.  
# This project analyzes the Cleveland heart disease dataset to determine the relationship between maximum heart rate (thalach)  
# and the likelihood of heart disease.  

# We will:
# 1. Perform statistical tests to identify key predictors.
# 2. Build a logistic regression model to predict heart disease.
# 3. Evaluate the model using accuracy and a confusion matrix.

# ----------------------------------------------
# Load the necessary packages
# ----------------------------------------------
install.packages("Metrics")
install.packages("yardstick")
install.packages("rmarkdown")
library(rmarkdown)
library(tidyverse)
library(yardstick)
library(Metrics)

# Load the datasets
hd_data <- read.csv("C:/Users/olayi/Desktop/Data/R/Heart Rate Analysis/Cleveland_hd.csv")

# Inspect the first five rows
head(hd_data, 5)

# Convert 'class' column to binary (0 = No Disease, 1 = Disease)
hd_data <- hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))

# ----------------------------------------------
# Statistical Tests to Identify Key Features
# ----------------------------------------------

#Chi-square test (Categorical Variable: sex)

# The Chi-square test is used to examine the relationship between two categorical variables.
# Since sex is categorical (0 = female, 1 = male) and hd (heart disease) is also categorical 
# (0 = No Disease, 1 = Disease), we use a Chi-square test to determine whether sex distribution 
# is significantly different between heart disease and non-heart disease groups.

# Chi-square test for sex vs heart disease
hd_sex <- chisq.test(hd_data$sex, hd_data$hd)
print(hd_sex)

# T-tests (Continuous Variables: age, thalach)

# A T-test compares the means of a continuous variable between two groups.
# Since age and thalach (maximum heart rate) are numerical variables, we use a 
# T-test to check if their means are significantly different between people with 
# and without heart disease.

# T-test for age vs heart disease
hd_age <- t.test(hd_data$age ~ hd_data$hd)
print(hd_age)

# T-test for thalach (max heart rate) vs heart disease
hd_heartrate <- t.test(hd_data$thalach ~ hd_data$hd)
print(hd_heartrate)

# Save the highly signficant features to a list
highly_significant <- list("age", "sex", "thalach")

# ----------------------------------------------
# Data Visualization
# ----------------------------------------------

ggplot(hd_data, aes(x = factor(hd), fill = factor(sex))) + 
  geom_bar(position = "fill") + 
  ylab("Sex %") + 
  xlab("Heart Disease (0 = No, 1 = Yes)")

ggplot(hd_data, aes(x = factor(hd), y = age)) + geom_boxplot()
ggplot(hd_data, aes(x = factor(hd), y = thalach)) + geom_boxplot()


# Build a model to predict heart disease using the significant features as predictors
model <- glm(data = hd_data, hd ~ age + sex + thalach, family = "binomial" )

# Extract the model summary
summary(model)


## Extracting useful information from the model output

"In medical research, it is common practice to report the Odds Ratio (OR) to measure how strongly 
a particular factor (A) is associated with an outcome (B). An OR greater than 1 indicates a positive 
association, meaning the presence of A increases the likelihood of outcome B occurring. Conversely, 
an OR less than 1 suggests a negative association, meaning A decreases the likelihood of B occurring.

In R, the logistic regression output provides log(Odds Ratios) the estimate column of the coefficient 
table. Since these values arein logarithmic form, they must be exponentiated (using exp()) to convert 
them into the original Odds Ratio (OR) scale. Additionally, a 95% Confidence Interval (CI) for the OR 
should be computed to quantify the uncertainty in the estimate. This helps in accurately interpreting 
the effect of each predictorin the model."

# load the broom package
library(broom)

# tidy up the coefficient table
tidy_m <- tidy(model)
tidy_m

# calculate OR
tidy_m$OR <- exp(tidy_m$estimate)

# calculate 95% CI and save as lower CI and upper CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)

# display the updated coefficient table
tidy_m

# Predict the probability of heart disease
pred_prob <- predict(model, hd_data, type="response")

# Create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
hd_data$pred_hd <- ifelse(pred_prob >= 0.5, 1, 0)

"To evaluate the performance of our model, we will use several common metrics. The simplest one is Accuracy, 
which measures the proportion of correct predictions out of all predictions made. We can also calculate the 
Classification Error Rate as 1− Accuracy, which represents the proportion of incorrect predictions.

However, Accuracy alone can be misleading, especially when dealing with imbalanced data (i.e., when one class 
is much more common than the other). A more reliable metric is the Area Under the ROC Curve (AUC), which 
measures the model's ability to distinguish between classes. AUC values range from 0 to 1, with values closer 
to 1 indicating better model performance.

Lastly, we will examine the Confusion Matrix, a 𝑁×𝑁table (where𝑁 is the number of outcome categories). 
Since we are dealing with a binary classification problem (𝑁= 2), our confusion matrix will be 2 × 2. 
This table compares the predicted outcome with the actual outcome, helping us understand where the model 
is making errors".


# Calculate and print the accuracy score
accuracy <- accuracy(hd_data$hd, hd_data$pred_hd)
print(paste("Accuracy=", accuracy))

# Calculate and print the auc score
auc <- auc(hd_data$hd,hd_data$pred_hd)
print(paste("AUC=", auc))

# Calculate and print the accuracy score
classification_error <- ce(hd_data$hd,hd_data$pred_hd)
print(paste("Classification Error=", classification_error))

# Calculate and print the confusion matrix
confusion <- conf_mat(table(hd_data$hd, hd_data$pred_hd))
confusion