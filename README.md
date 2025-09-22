🌟 **Predictive Modeling of Building Energy Efficiency**

This project performs a comprehensive statistical analysis and predictive modeling of building energy efficiency using the **Energy Efficiency Data Set**. The primary goal is to predict two key target variables---**Heating Load** and **Cooling Load**---based on various building design features. The analysis uses a range of techniques, from exploratory data analysis and hypothesis testing to the application and comparison of advanced regression models.

The project is conducted in **R** and follows a structured pipeline. It begins with an in-depth **Exploratory Data Analysis (EDA)** to understand the dataset's characteristics, including distributions, correlations, and outliers. It then transitions to **statistical hypothesis testing** to formally assess relationships between variables. Finally, it builds and evaluates three distinct machine learning models---**Random Forest**, **Support Vector Regression (SVR)**, and **Generalized Additive Models (GAM)**---to determine the most effective method for predicting building energy loads.

* * * * *

### ✨ Key Features & Technical Details

-   **Exploratory Data Analysis (EDA)**: The notebook performs a thorough EDA, including:

    -   **Summary Statistics and Missing Value Checks**: Provides a high-level overview of the data and confirms its cleanliness.

    -   **Visualizations**: Uses histograms, boxplots, pairwise scatter plots, and correlation heatmaps to visualize data distributions, detect outliers, and understand relationships between features. A strong positive correlation between `Heating_Load` and `Cooling_Load` is identified and analyzed.

    -   **Outlier Detection**: The **Interquartile Range (IQR)** method is used to systematically identify and count outliers for each numerical feature.

-   **Statistical Hypothesis Testing**: The project uses **non-parametric tests** due to the non-normal distribution of the target variables, as confirmed by the **Shapiro-Wilk test**.

    -   **Kruskal-Wallis Test**: Used to determine if there are significant differences in energy loads across various building orientations.

    -   **Mann-Whitney U Test**: Compares the medians of energy loads between buildings with high vs. low glazing areas.

    -   **Spearman's Rank Correlation**: Measures the strength and direction of the monotonic relationship between `Heating_Load` and `Cooling_Load`.

    -   **Friedman Test**: A paired-sample non-parametric test to check for significant differences in heating loads across different orientations (using simulated data to illustrate the concept).

-   **Regression Modeling and Comparison**: The dataset is split into training and testing sets to evaluate model performance.

    -   **Random Forest Regression**: An ensemble learning method that builds multiple decision trees to produce robust predictions. It is evaluated using **Root Mean Squared Error (RMSE)** and proved to be the best-performing model with the lowest RMSE for both heating and cooling loads.

    -   **Support Vector Regression (SVR)**: A powerful model that finds a hyperplane to minimize prediction error. It is also evaluated using RMSE.

    -   **Generalized Additive Models (GAM)**: A flexible model that uses smooth functions to capture non-linear relationships between predictors and target variables. The notebook visualizes these smooth effects to provide interpretability.

-   **Model Evaluation**: All models are evaluated based on **RMSE** to compare their predictive accuracy. Diagnostic plots, including **Predicted vs. Actual** and **Residuals vs. Predicted** plots, are generated for Random Forest and SVR to visually assess their performance and check for biases.

* * * * *

### 🚀 Getting Started

To run this project, you will need an **R** environment with the following libraries:

-   `readxl`

-   `ggplot2`

-   `dplyr`

-   `GGally`

-   `corrplot`

-   `mgcv`

-   `randomForest`

-   `e1071`

-   `caret`

-   `car`

-   `reshape2`

You can install these packages using the `install.packages()` command in R. The project also requires the `Energy Efficiency Data.xlsx` file, which should be in the same directory as the script.

* * * * *

### 📊 Project Workflow

The `Statistical Analysis.R` script follows a comprehensive workflow:

1.  **Library and Data Loading**: Loads all necessary libraries and the `Energy Efficiency Data.xlsx` file.

2.  **Exploratory Data Analysis**: Conducts summary statistics, checks for missing values and outliers, and generates various plots to visualize data characteristics.

3.  **Correlation Analysis**: Specifically examines the relationship between `Heating_Load` and `Cooling_Load` using Pearson correlation and a scatter plot.

4.  **Data Preparation**: The data is partitioned into training and testing sets, and predictor variables are scaled for effective model training.

5.  **Regression Modeling**: Three regression models (Random Forest, SVR, and GAM) are trained and used to predict `Heating_Load` and `Cooling_Load`. Their performance is assessed using RMSE.

6.  **Hypothesis Testing**: Performs a series of non-parametric tests to statistically validate relationships between categorical and continuous variables.

7.  **Conclusion**: Summarizes the findings, declaring Random Forest as the superior model for this dataset based on its performance.

* * * * *

### 📈 Final Thoughts

This project serves as a robust example of a data science pipeline, from foundational data exploration to advanced predictive modeling. It demonstrates how to choose and apply appropriate statistical tests, build and compare a variety of machine learning models, and use diagnostic plots to ensure a model's reliability. The combination of regression analysis and statistical hypothesis testing provides a thorough and evidence-based approach to understanding and predicting building energy performance.

* * * * *

### 🙏 Acknowledgments

I would like to thank the creators of the `randomForest`, `e1071`, `mgcv`, `ggplot2`, and `caret` R packages for providing the powerful and versatile tools that were essential for this project's analysis and modeling. I also acknowledge the **Energy Efficiency Data Set** from the UCI Machine Learning Repository, which served as a rich and foundational resource for this study.
