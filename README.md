# Heart Disease Prediction Model

## Table of Contents

- Introduction
- Dataset
- Methodology
- Results

## Introduction
This repository contains the code and documentation for a research project on predicting heart disease using statistical learning techniques.
Cardiovascular diseases are the leading cause of death globally, responsible for nearly 18 million deaths per year. This project aims to develop a predictive model that can help identify individuals at risk of heart disease based on various medical observations.

## Dataset

The dataset is publically available and can be found at: https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction. The parameters taken into consideration are: *age, sex, chest pain type, resting blood pressure, cholesterol, fasting blood sugar, resting ECG, maximum heart rate, exercise-induced angina, oldpeak (ST depression), ST slope*. The target variable is *hearth disease* (boolean)

## Methodology

We developed and compared several logistic regression models using different selection techniques:

1. Full model
2. Step-wise selection
3. LASSO regularization

The models were evaluated based on accuracy and McFadden's pseudo-R².

## Results

- The step-wise selection model performed best, achieving 91% accuracy with 11 covariates.
- All models showed good fit, with McFadden's pseudo-R² values around 0.51.
- The study identified potential limitations, including geographical bias and gender imbalance in the dataset.

For a detailed analysis, please refer to the full research paper in the docs folder.
