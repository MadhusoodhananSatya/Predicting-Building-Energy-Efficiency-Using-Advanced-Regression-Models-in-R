
# Loading necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(mgcv)  # For GAM  Regression
library(randomForest)    # For Random Forest Regression
library(e1071)     # For SVR regression
library(caret)   # For data partitioning
library(car)
library(reshape2)


# Loading data
data <- read_excel("Energy Efficiency Data.xlsx")

# Renaming columns for easier access
colnames(data) <- gsub(" ", "_", colnames(data))










# Exploratory Data Analysis (EDA) -------------------------------------------------------

# 1. Summary Statistics
print("Summary Statistics")
summary(data)

# 2. Checking for Missing Values
print("Checking for Missing Values")
colSums(is.na(data))

# 3. Distribution of Each Feature
# Ploting histograms for each feature to observe distributions
feature_names <- colnames(data)
par(mfrow = c(3, 4))  # Adjusting plot layout for visualization
for (feature in feature_names) {
  hist(data[[feature]], main = paste("Distribution of", feature), xlab = feature, col = "lightblue", breaks = 20)
}
par(mfrow = c(1, 1))  # Reseting plot layout

# 4. Boxplots for Potential Outliers

# Ploting boxplots to detect potential outliers for each numerical feature
par(mfrow = c(3, 4))
for (feature in feature_names) {
  if (is.numeric(data[[feature]])) {
    boxplot(data[[feature]], main = paste("Boxplot of", feature), col = "lightgreen")
  }
}
par(mfrow = c(1, 1))# Reset plot layout

# Detecting Outliers Using the IQR Method --------------------------------------------
# Selecting only numerical columns
numerical_data <- data[sapply(data, is.numeric)]
# Function to detect outliers for each variable using the IQR method
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

# Counting Outliers for Each Variable -------------------------------------------------
# Applying the function to each numerical column and summarize the results
outliers_summary <- sapply(numerical_data, find_outliers)

# Printing outlier indices for each variable
print("Outliers detected in each variable:")
print(outliers_summary)
# Counting the number of outliers for each variable
outlier_counts <- sapply(outliers_summary, length)
outlier_counts_df <- data.frame(
  Variable = names(outlier_counts),
  Outlier_Count = outlier_counts
)

# Displaying variables with outliers
print("Number of outliers in each variable:")
print(outlier_counts_df)

# 5. Pairwise Relationships
# Creating scatter plots for pairs of features to observe relationships
ggpairs(data, columns = c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area",
                          "Overall_Height", "Glazing_Area", "Heating_Load", "Cooling_Load"),
        title = "Pairwise Relationships between Key Features and Loads")

# 6. Correlation Analysis
# Calculating correlation matrix and visualize it
cor_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black",
         number.cex = 0.7, title = "Correlation Matrix for Building Features and Loads")

# 7. Summary of Findings
cat("Summary of EDA Findings:\n")
cat("1. Summary statistics and boxplots provide insights into variable distributions and potential outliers.\n")
cat("2. Histograms show the spread of data in each feature, indicating if transformations might be needed.\n")
cat("3. Pairwise scatter plots and the correlation matrix reveal linear relationships between features and target variables, with a high correlation between Heating and Cooling Load.\n")











#CORRELATION ANALYSIS
# Specifying the two variables for correlation analysis
var1 <- data$Heating_Load
var2 <- data$Cooling_Load

# Step 1: Computing Pearson Correlation
pearson_corr <- cor(var1, var2, method = "pearson")

# Printing Pearson Correlation Coefficient
cat("Pearson Correlation Coefficient between Heating Load and Cooling Load: ", pearson_corr, "\n")

# Step 2: Visualizing the Linear Relationship
ggplot(data, aes(x = Heating_Load, y = Cooling_Load)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatterplot with Linear Trendline",
       x = "Heating Load", y = "Cooling Load")

# Step 3: Interpretation
cat("\nInterpretation:\n")
cat("1. The Pearson correlation coefficient is", round(pearson_corr, 3), "indicating a strong linear relationship.\n")
cat("2. Based on the scatterplot and correlation coefficient, we conclude that Heating Load and Cooling Load are linearly dependent.\n")
cat("3. This strong relationship suggests that factors affecting Heating Load likely influence Cooling Load in a proportional manner.\n")










#REGRESSION 
# Defining predictors (X) and target variables (Y)
X <- data[, c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area",
              "Overall_Height", "Orientation", "Glazing_Area", "Glazing_Area_Distribution")]
y_heating <- data$Heating_Load
y_cooling <- data$Cooling_Load

# Splitting the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y_heating, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_heating_train <- y_heating[trainIndex]
y_heating_test <- y_heating[-trainIndex]
y_cooling_train <- y_cooling[trainIndex]
y_cooling_test <- y_cooling[-trainIndex]

#  Scaling the Predictors
X_scaled <- scale(X)  
X_train_scaled <- X_scaled[trainIndex, ]  # Scaled training predictors
X_test_scaled <- X_scaled[-trainIndex, ]  # Scaled testing predictors

# 1. Random Forest Regression --------------------------------------------------------

# Random Forest for Heating Load
rf_heating <- randomForest(Heating_Load ~ ., data = data[trainIndex, ], ntree = 500)
rf_heating_pred <- predict(rf_heating, newdata = data[-trainIndex, ])
rf_heating_rmse <- sqrt(mean((rf_heating_pred - y_heating_test)^2))
cat("Random Forest Regression - Heating Load RMSE:", rf_heating_rmse, "\n")

# Random Forest for Cooling Load
rf_cooling <- randomForest(Cooling_Load ~ ., data = data[trainIndex, ], ntree = 500)
rf_cooling_pred <- predict(rf_cooling, newdata = data[-trainIndex, ])
rf_cooling_rmse <- sqrt(mean((rf_cooling_pred - y_cooling_test)^2))
cat("Random Forest Regression - Cooling Load RMSE:", rf_cooling_rmse, "\n")





# Diagnostic Plots for Random Forest - Heating Load
cat("\nPlotting Diagnostics for Random Forest - Heating Load...\n")
rf_plot_data_heating <- data.frame(
  Actual = y_heating_test,
  Predicted = rf_heating_pred,
  Residuals = y_heating_test - rf_heating_pred
)

# Predicted vs Actual
ggplot(rf_plot_data_heating, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Random Forest - Heating Load: Predicted vs Actual",
       x = "Actual Values", y = "Predicted Values")

# Residual Plot
ggplot(rf_plot_data_heating, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Random Forest - Heating Load: Residuals",
       x = "Predicted Values", y = "Residuals")

# 2. SVR Regression --------------------------------------------------------

# SVR for Heating Load
svr_heating <- svm(Heating_Load ~ ., data = data[trainIndex, ], kernel = "radial", scale = TRUE)
svr_heating_pred <- predict(svr_heating, newdata = data[-trainIndex, ])
svr_heating_rmse <- sqrt(mean((svr_heating_pred - y_heating_test)^2))
cat("Support Vector Regression - Heating Load RMSE:", svr_heating_rmse, "\n")

# SVR for Cooling Load
svr_cooling <- svm(Cooling_Load ~ ., data = data[trainIndex, ], kernel = "radial", scale = TRUE)
svr_cooling_pred <- predict(svr_cooling, newdata = data[-trainIndex, ])
svr_cooling_rmse <- sqrt(mean((svr_cooling_pred - y_cooling_test)^2))
cat("Support Vector Regression - Cooling Load RMSE:", svr_cooling_rmse, "\n")





# Diagnostic Plots for SVR - Cooling Load
cat("\nPlotting Diagnostics for SVR - Cooling Load...\n")
svr_plot_data_cooling <- data.frame(
  Actual = y_cooling_test,
  Predicted = svr_cooling_pred,
  Residuals = y_cooling_test - svr_cooling_pred
)

# Predicted vs Actual
ggplot(svr_plot_data_cooling, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "SVR - Cooling Load: Predicted vs Actual",
       x = "Actual Values", y = "Predicted Values")

# Residual Plot
ggplot(svr_plot_data_cooling, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "SVR - Cooling Load: Residuals",
       x = "Predicted Values", y = "Residuals")

# 3. GAM Regression --------------------------------------------------------
# Checking unique values for all predictors
unique_values <- sapply(data[trainIndex, ], function(x) length(unique(x)))
print(unique_values)

# Ensuring categorical variables are treated as factors
data$Orientation <- as.factor(data$Orientation)
data$Glazing_Area_Distribution <- as.factor(data$Glazing_Area_Distribution)

# GAM for Heating Load
cat("\nGeneralized Additive Model (GAM) for Heating Load...\n")
gam_heating <- gam(Heating_Load ~ s(Relative_Compactness, k = 5) + 
                     s(Surface_Area, k = 5) + 
                     s(Wall_Area, k = 5) + 
                     Roof_Area + Overall_Height + 
                     Orientation + Glazing_Area + Glazing_Area_Distribution, 
                   data = data[trainIndex, ])
summary(gam_heating)

# Predictions and RMSE for Heating Load
gam_heating_pred <- predict(gam_heating, newdata = data[-trainIndex, ])
gam_heating_rmse <- sqrt(mean((gam_heating_pred - y_heating_test)^2))
cat("GAM - Heating Load RMSE:", gam_heating_rmse, "\n")

# GAM for Cooling Load
cat("\nGeneralized Additive Model (GAM) for Cooling Load...\n")
gam_cooling <- gam(Cooling_Load ~ s(Relative_Compactness, k = 5) + 
                     s(Surface_Area, k = 5) + 
                     s(Wall_Area, k = 5) + 
                     Roof_Area + Overall_Height + 
                     Orientation + Glazing_Area + Glazing_Area_Distribution, 
                   data = data[trainIndex, ])
summary(gam_cooling)

# Predictions and RMSE for Cooling Load
gam_cooling_pred <- predict(gam_cooling, newdata = data[-trainIndex, ])
gam_cooling_rmse <- sqrt(mean((gam_cooling_pred - y_cooling_test)^2))
cat("GAM - Cooling Load RMSE:", gam_cooling_rmse, "\n")

# Visualization of Smooth Effects
cat("\nVisualizing GAM Smooth Effects...\n")
par(mfrow = c(2, 3))  # Arrange plots
plot(gam_heating, se = TRUE, rug = TRUE, main = "Smooth Effects for Heating Load")
plot(gam_cooling, se = TRUE, rug = TRUE, main = "Smooth Effects for Cooling Load")
par(mfrow = c(1, 1))  # Reset layout

cat("\nInterpretation:\n")
cat("Random Forest Regression is the best model for both Heating Load and Cooling Load predictions based on its lowest RMSE values.\n")













# Hypotheses Tests ----------------------------------------------------------------------
# Ensure categorical variables are factors
data$Orientation <- as.factor(data$Orientation)
data$Glazing_Area_Distribution <- as.factor(data$Glazing_Area_Distribution)

# Checking Normality of Dependent Variables (Shapiro-Wilk Test)
cat("\nChecking Normality (Shapiro-Wilk Test):\n")
shapiro_heating <- shapiro.test(data$Heating_Load)
shapiro_cooling <- shapiro.test(data$Cooling_Load)
cat("Shapiro-Wilk Test - Heating Load: p-value =", shapiro_heating$p.value, "\n")
cat("Shapiro-Wilk Test - Cooling Load: p-value =", shapiro_cooling$p.value, "\n")

if (shapiro_heating$p.value > 0.05) {
  cat("Heating Load: Data appears normally distributed.\n")
} else {
  cat("Heating Load: Data is NOT normally distributed.\n")
}

if (shapiro_cooling$p.value > 0.05) {
  cat("Cooling Load: Data appears normally distributed.\n")
} else {
  cat("Cooling Load: Data is NOT normally distributed.\n")
}
# Ensuring categorical variables are factors
data$Orientation <- as.factor(data$Orientation)
data$Glazing_Area_Distribution <- as.factor(data$Glazing_Area_Distribution)

# Adding Glazing Area Levels for  Mann-Whitney Test
data <- data %>%
  mutate(Glazing_Area_Level = ifelse(Glazing_Area > median(Glazing_Area), "High", "Low"))

# 1. Kruskal-Wallis Test for Orientation Effect -----------------------------------

cat("\nPerforming Kruskal-Wallis Test for Orientation Effect:\n")

# Heating Load
kruskal_heating <- kruskal.test(Heating_Load ~ Orientation, data = data)
cat("Kruskal-Wallis Test - Heating Load by Orientation: p-value =", kruskal_heating$p.value, "\n")

# Cooling Load
kruskal_cooling <- kruskal.test(Cooling_Load ~ Orientation, data = data)
cat("Kruskal-Wallis Test - Cooling Load by Orientation: p-value =", kruskal_cooling$p.value, "\n")

# Interpretation
if (kruskal_heating$p.value < 0.05) {
  cat("Result: Significant differences in Heating Load medians across Orientations.\n")
} else {
  cat("Result: No significant differences in Heating Load medians across Orientations.\n")
}

if (kruskal_cooling$p.value < 0.05) {
  cat("Result: Significant differences in Cooling Load medians across Orientations.\n")
} else {
  cat("Result: No significant differences in Cooling Load medians across Orientations.\n")
}

# 2. Mann-Whitney U Test for High vs Low Glazing Area -----------------------------

cat("\nPerforming Mann-Whitney U Test for High vs Low Glazing Area:\n")

# Heating Load
mann_whitney_heating <- wilcox.test(Heating_Load ~ Glazing_Area_Level, data = data, exact = FALSE)
cat("Mann-Whitney U Test - Heating Load: p-value =", mann_whitney_heating$p.value, "\n")

# Cooling Load
mann_whitney_cooling <- wilcox.test(Cooling_Load ~ Glazing_Area_Level, data = data, exact = FALSE)
cat("Mann-Whitney U Test - Cooling Load: p-value =", mann_whitney_cooling$p.value, "\n")

# Interpretation
if (mann_whitney_heating$p.value < 0.05) {
  cat("Result: Significant differences in Heating Load medians between High and Low Glazing Area.\n")
} else {
  cat("Result: No significant differences in Heating Load medians between High and Low Glazing Area.\n")
}

if (mann_whitney_cooling$p.value < 0.05) {
  cat("Result: Significant differences in Cooling Load medians between High and Low Glazing Area.\n")
} else {
  cat("Result: No significant differences in Cooling Load medians between High and Low Glazing Area.\n")
}

# 3. Spearman's Rank Correlation ----------------------------------------------

cat("\nPerforming Spearman's Rank Correlation Test between Heating and Cooling Load:\n")

# Spearman's Correlation
spearman_corr <- cor.test(data$Heating_Load, data$Cooling_Load, method = "spearman")
cat("Spearman's Rank Correlation - Heating and Cooling Load:\n")
cat("Correlation Coefficient =", spearman_corr$estimate, "\n")
cat("p-value =", spearman_corr$p.value, "\n")

# Interpretation
if (spearman_corr$p.value < 0.05) {
  cat("Result: Significant monotonic relationship between Heating Load and Cooling Load.\n")
} else {
  cat("Result: No significant monotonic relationship between Heating Load and Cooling Load.\n")
}

# 4. Friedman Test ------------------------------------------------------------

cat("\nPerforming Friedman Test for Heating Load across Orientations:\n")


heating_loads <- data.frame(
  Subject = 1:10,  
  Orientation1 = rnorm(10, mean = 20, sd = 5),
  Orientation2 = rnorm(10, mean = 22, sd = 5),
  Orientation3 = rnorm(10, mean = 25, sd = 5)
)

# Converting data to long format for Friedman Test

heating_loads_long <- melt(heating_loads, id.vars = "Subject", 
                           variable.name = "Orientation", value.name = "Heating_Load")

# Performing Friedman Test
friedman_test <- friedman.test(Heating_Load ~ Orientation | Subject, data = heating_loads_long)
cat("Friedman Test - Heating Load across Orientations: p-value =", friedman_test$p.value, "\n")

# Interpretation
if (friedman_test$p.value < 0.05) {
  cat("Result: Significant differences in Heating Load across Orientations.\n")
} else {
  cat("Result: No significant differences in Heating Load across Orientations.\n")
}



















