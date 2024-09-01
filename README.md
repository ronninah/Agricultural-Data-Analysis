# Global Data Analysis on Crop Yield Change in No-Tillage Management

## Overview
This project analyzes the impact of switching from conventional tillage to no-tillage practices on crop yields using interpretable machine learning (IML) tools. The study is part of the Chair of Environmental Data Science at Brandenburg University of Technology, supervised by Prof. Dr. Masahiro Ryo.

The primary goal is to determine how no-tillage affects crop yields across different crop types and geographic regions. The project uses global datasets and applies various machine learning algorithms to model and interpret the effects of no-tillage practices.

## Project Structure

- **data/**: Contains the global crop yield datasets.
- **scripts/**: Includes R scripts used for data analysis and modeling.
  - `Study_Project3.R`: The main script for performing the analysis.
- **results/**: Outputs from the analysis, including plots and tables.
- **docs/**: Documentation related to the project.
  - `Study Project report_Ronnie_final.pdf`: The final report summarizing the findings.

## Methodology
The research methodology is divided into two main sections:

1. **Global Mapping and Histogram Analysis**:
   - Visualizes crop data on a global map to understand spatial patterns in crop cultivation.
   - Analyzes the distribution of crop yield changes using histograms.

2. **Model Selection and Implementation**:
   - Applies various machine learning models (Linear Regression, Decision Tree, Random Forest, Gradient Boosting) to predict crop yield changes.
   - Evaluates the models using R-squared and RMSE metrics to determine the best-performing model for each crop type.

## Getting Started

### Prerequisites
- **R** (version 4.x or later)
- **RStudio** (optional, but recommended)
- Required R packages:
  ```r
  install.packages(c("dplyr", "ggplot2", "caret", "rpart", "randomForest", "gbm", "rpart.plot"))
