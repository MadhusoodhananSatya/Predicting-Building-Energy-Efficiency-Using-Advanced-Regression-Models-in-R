## 📖 Overview
This project involves **statistical analysis and predictive modeling** on an **Energy Efficiency dataset**. The goal is to analyze factors influencing **Heating Load and Cooling Load** using **exploratory data analysis (EDA), correlation studies, regression modeling, and hypothesis testing**.

## 📂 Dataset Information
The dataset contains **building energy efficiency parameters**, including:
- **Relative Compactness**
- **Surface Area**
- **Wall Area**
- **Roof Area**
- **Overall Height**
- **Glazing Area**
- **Heating Load** (Target Variable)
- **Cooling Load** (Target Variable)

## 🏗️ Project Workflow
1. **Data Preprocessing & Exploratory Data Analysis (EDA):**
   - Summary statistics
   - Missing values detection
   - Data distribution (Histograms, Boxplots)
   - Outlier detection using the **IQR method**
   - Feature relationships using scatter plots and correlation matrix

2. **Correlation Analysis:**
   - Pearson correlation between **Heating Load** and **Cooling Load**
   - Scatterplot visualization with regression trendline
   - Interpretation of **linear dependency** between variables

3. **Regression Models Applied:**
   - **Random Forest Regression**
   - **Support Vector Regression (SVR)**
   - **Generalized Additive Model (GAM)**
   - **Model Evaluation Metrics:**
     - **Root Mean Squared Error (RMSE)** for predictive accuracy
     - Diagnostic plots (Residual analysis, Predicted vs Actual values)

4. **Hypothesis Testing:**
   - **Shapiro-Wilk Test** for normality check
   - **Kruskal-Wallis Test** for differences in energy efficiency by orientation
   - **Mann-Whitney U Test** for glazing area impact on energy efficiency
   - **Spearman's Rank Correlation** for non-parametric association
   - **Friedman Test** for multi-group comparison of heating load

## 📊 Results & Insights
- **Random Forest Regression** was the **best-performing model** with the lowest RMSE for both **Heating Load** and **Cooling Load**.
- **Pearson Correlation** showed a **strong linear relationship** between Heating and Cooling Loads.
- **Hypothesis tests** revealed significant differences in energy efficiency based on **orientation and glazing area**.
- **Outliers** were detected in **Surface Area, Wall Area, and Roof Area**, which may impact model predictions.
