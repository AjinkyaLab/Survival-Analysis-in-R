

#*******************************************************
# title: "Survival Analysis in R"
# author: "Ajinkya"
# date: "2024-12-31"
# output: html_document
#*******************************************************

## Step 1: Setting Up the Environment
# install.packages("flexsurv")
# installed.packages("survminer")
install.packages("ggsurvplot")

# Load required libraries
library(survival)
library(tidyverse)
library(GGally)
library(survminer)
library(flexsurv)
library(ggplot2)
library (ggcorrplot)

# Set the working directory (update this path as needed)
setwd("C:/Users/Ajinkyaa/OneDrive/Stata to R/New folder/Survival analysis")

## Step 2: Lead the dataset
dialysis <- read_csv("C:/Users/Ajinkyaa/OneDrive/Stata to R/New folder/Survival analysis/dialysis survival dataset.csv")


## Step 3: Data Inspection and Cleaning

# Preview the data
head(dialysis)
str(dialysis)
summary(dialysis)

# Check for missing values
colSums(is.na(dialysis))

# Impute or remove missing values (example: removing rows with missing data)
dialysis <- dialysis %>% drop_na()


# Step 4: (Exploratory Data Analysis (EDA)) Data Distribution Visualizations 
cr <- round(cor(dialysis), 2) #Store correlation matrix
cr

#Visualize your correlations (Creates a visual heatmap of the correlation matrix)
ggcorrplot(cr,title = "correlogram", lab_col = "black",
           lab = TRUE, legend.title = "Pearson Correlation",
           lab_size=2, ggtheme = theme_classic(),
           outline.color = "black",
           colors = c("orange", "green", "blue"))

# 1. Histogram for Age
ggplot(dialysis, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# 2. Boxplot for Time by Diabetes Status
ggplot(dialysis, aes(x = factor(disease_diabetes), y = time, fill = factor(disease_diabetes))) +
  geom_boxplot() +
  labs(title = "Time Distribution by Diabetes Status", x = "Diabetes (0 = No, 1 = Yes)", y = "Time") +
  scale_fill_manual(values = c("orange", "purple")) +
  theme_minimal()

# 3. Density Plot for Time
ggplot(dialysis, aes(x = time, fill = factor(disease_diabetes))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Time by Diabetes Status", x = "Time", y = "Density", fill = "Diabetes") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

# 4. Bar Plot for Diabetes Status
ggplot(dialysis, aes(x = factor(disease_diabetes), fill = factor(disease_diabetes))) +
  geom_bar() +
  labs(title = "Bar Plot of Diabetes Status", x = "Diabetes (0 = No, 1 = Yes)", y = "Count") +
  scale_fill_manual(values = c("green", "yellow")) +
  theme_minimal()

# 5. Scatter Plot for Age vs. Time
ggplot(dialysis, aes(x = age, y = time, color = factor(disease_diabetes))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot of Age vs. Time", x = "Age", y = "Time", color = "Diabetes") +
  theme_minimal()

# 6. Pairwise Plot for Selected Variables
library(GGally)
ggpairs(dialysis, columns = c("age", "time", "begin"), aes(color = factor(disease_diabetes)))

# 7. Stacked Bar Plot
ggplot(dialysis, aes(x = factor(disease_diabetes), fill = factor(disease_hypert))) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Hypertension by Diabetes", x = "Diabetes", y = "Count", fill = "Hypertension") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()

## Step 5: Kaplan-Meier Model

# Fit Kaplan-Meier model
km_model <- survfit(Surv(time, event) ~ disease_diabetes, data = dialysis)

# Summary of the model
summary(km_model)

# Kaplan-Meier survival plot
ggsurvplot(km_model, 
           data = dialysis, 
           conf.int = TRUE, 
           risk.table = TRUE,
           pval = TRUE,
           title = "Kaplan-Meier Survival Curves by Diabetes Status",
           xlab = "Time (days)", 
           ylab = "Survival Probability",
           palette = c("blue", "red"))


## Step 6: Log-Rank Test
# Test survival differences between groups
log_rank <- survdiff(Surv(time, event) ~ disease_diabetes, data = dialysis)
log_rank

## Step 7: Cox Proportional Hazards Model
cox_model <- coxph(Surv(time, event) ~ disease_hypert + disease_renal + begin + center, data = dialysis)

# Summary of the model
summary(cox_model)

# Test proportional hazards assumption
ph_test <- cox.zph(cox_model)
ph_test

# Visualize Schoenfeld residuals
ggcoxzph(ph_test)

# Plot survival curves based on Cox model
ggsurvplot(survfit(cox_model), 
           data = dialysis, 
           conf.int = TRUE, 
           risk.table = TRUE, 
           title = "Cox Model Survival Curves",
           xlab = "Time (days)", 
           ylab = "Survival Probability")

## Step 8: Parametric Survival Models
# 1. Exponential Model
exp_model <- flexsurvreg(Surv(time, event) ~ disease_hypert + disease_renal + begin + center, 
                         data = dialysis, dist = "exponential")
summary(exp_model)

# 2. Weibull Model
weibull_model <- flexsurvreg(Surv(time, event) ~ disease_hypert + disease_renal + begin + center, 
                             data = dialysis, dist = "weibull")
summary(weibull_model)

# 3. Compare Models
# Compare AIC values
model_comparison <- data.frame(
  Model = c("Exponential", "Weibull"),
  AIC = c(AIC(exp_model), AIC(weibull_model))
)
model_comparison

## Step 9: Advanced Analysis
# Predict survival probabilities at specific times using Weibull model
predict(weibull_model, newdata = dialysis, type = "survival", times = c(10, 20, 30))

# Kaplan-Meier for subgroups analysis
km_model_subgroup <- survfit(Surv(time, event) ~ disease_renal, data = dialysis)
ggsurvplot(km_model_subgroup, 
           data = dialysis, 
           conf.int = TRUE, 
           risk.table = TRUE, 
           pval = TRUE, 
           title = "Survival Curves by Renal Disease",
           palette = c("green", "purple"))

## Step 10: Export Results
# Save plots and tables
ggsave("km_survival_plot.png")
write_csv(model_comparison, "model_comparison.csv")


























