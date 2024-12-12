# Load required libraries
install.packages("lubridate")
install.packages("broom")
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(broom)
library(dplyr)

# Load the dataset (Replace 'path_to_csv' with the actual file path)
stock_data <- read_csv("C:/Users/Kunal Vishwa/OneDrive/Desktop/all_stocks_5yr.csv")

# View the first few rows of the dataset
head(stock_data)

# Data Cleaning and Preprocessing
# Convert 'date' column to date type
stock_data <- stock_data %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Calculate daily returns for each stock
stock_data <- stock_data %>% 
  group_by(Name) %>%
  arrange(date) %>%
  mutate(Return = (close - lag(close)) / lag(close) * 100) %>%
  ungroup()

# Drop NA values introduced by lag()
stock_data <- na.omit(stock_data)

# Sample 1000 random instances from the large dataset
set.seed(123)  # Set seed for reproducibility
sampled_data <- stock_data[sample(1:nrow(stock_data), 1000), ]

# Create new predictors directly from the existing columns
sampled_data <- sampled_data %>%
  mutate(
    Price_Range = high - low,
    Daily_Change = close - open,
    Relative_Change = (close - open) / open * 100
  )

# Check for missing values after creating new columns
sum(is.na(sampled_data$Return))  # Check for missing values in Return
sum(is.na(sampled_data$Price_Range))  # Check for missing values in new columns

# Descriptive Statistics - Filter companies with insufficient data (less than 2 observations)
returns_summary <- sampled_data %>%
  group_by(Name) %>%
  summarize(Average_Return = mean(Return, na.rm = TRUE),
            SD_Return = sd(Return, na.rm = TRUE),
            Observations = n()) %>%
  filter(Observations > 1)  # Only include companies with more than 1 observation

# View the updated summary
print(returns_summary)

# Perform ANOVA (Analysis of Variance)
anova_result <- aov(Return ~ Name, data = sampled_data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Post-hoc analysis (if ANOVA is significant)
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  post_hoc <- TukeyHSD(anova_result)
  print(post_hoc)
} else {
  print("ANOVA did not show significant differences between groups.")
}

# Build the regression model
regression_model <- lm(Return ~ volume + Price_Range + Daily_Change + Relative_Change, data = sampled_data)

# Summarize the regression model
summary(regression_model)

# Find top 30 companies based on high volume (using highest average volume per company)
top_companies <- sampled_data %>%
  group_by(Name) %>%
  summarize(Avg_Volume = mean(volume, na.rm = TRUE)) %>%
  arrange(desc(Avg_Volume)) %>%
  slice(1:30) %>%
  pull(Name)

# Filter the data to include only the top 30 companies
top_companies_data <- sampled_data %>%
  filter(Name %in% top_companies)

# Boxplot of returns by company (for top 30 companies)
ggplot(top_companies_data, aes(x = Name, y = Return, fill = Name)) +
  geom_boxplot() +
  labs(title = "Distribution of Daily Returns by Top 30 Companies",
       x = "Company",
       y = "Daily Return (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Format y-axis labels as percentages

# Regression Plot
ggplot(sampled_data, aes(x = volume, y = Return)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Regression of Daily Returns on Volume",
       x = "Trading Volume",
       y = "Daily Return (%)") +
  theme_minimal()

# Save results for submission
# Export cleaned dataset
write_csv(sampled_data, "C:/Users/Kunal Vishwa/OneDrive/Documents/cleaned_stock_data.csv")

# Make predictions using the regression model
predicted_returns <- predict(regression_model, newdata = sampled_data)

# Calculate R-squared
r_squared <- summary(regression_model)$r.squared
cat("R-squared: ", r_squared, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(sampled_data$Return - predicted_returns))
cat("Mean Absolute Error (MAE): ", mae, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((sampled_data$Return - predicted_returns)^2))
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")

# Add the predicted returns to the dataset
sampled_data <- sampled_data %>%
  mutate(Predicted_Return = predicted_returns)

# View the predictions
head(sampled_data)

# Plot Actual vs Predicted Returns
ggplot(sampled_data, aes(x = Return, y = Predicted_Return)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  labs(title = "Actual vs Predicted Daily Returns",
       x = "Actual Daily Returns",
       y = "Predicted Daily Returns") +
  theme_minimal()

# Create new data frame for predictions
new_data <- data.frame(
  volume = c(5000000, 10000000, 15000000, 2015566),
  Price_Range = c(1.2, 1.8, 2.5, 0.870),
  Daily_Change = c(0.3, 0.5, 0.8, 0.840),
  Relative_Change = c(1.5, 2.0, 2.8, 1.47)
)

# Predict future returns
future_predictions <- predict(regression_model, newdata = new_data)

# Add predictions to the new data for better visualization
new_data <- new_data %>%
  mutate(Predicted_Return = future_predictions)

# View the results
print(new_data)
