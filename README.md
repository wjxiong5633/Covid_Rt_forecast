# Forecasting Framework with Model Selection and Prediction Intervals (R Implementation)

This repository contains R code for a forecasting project that evaluates individual models, performs model selection (top-N and window size tuning), builds ensembles, and produces point forecasts as well as prediction intervals. The framework supports simulation, baseline comparisons, and result visualisation.

---

## Overview

### Data
All used datasets are in `data/`. 

### Simulation
The `sim/` folder contains the main simulation. 

### Individual Models
- `Single_model_loop/`: Train and test each model separately  
- `run_model/`: Wrapper scripts to launch model runs  

### Model Selection
The `choose_ws_choose_topn_model_code/` folder implements:
- Rolling window size selection  
- Top-N model selection (e.g., based on out-of-sample performance)  

### Ensemble
The `ensemble/` folder contains code to combine forecasts from multiple models (e.g., simple averaging).

### Predictions
Results are organised into:
- **Point forecasts** (`_point/`)  
- **Prediction intervals** (`_interval/`)  

Each is further split into:
- `reportcase`: report case number
- `rt`: effective reproductive number 

### Baselines
The `baseline/` folder stores results from baseline methods.

### Training Coefficients
The `train_coef/` directory contains estimated coefficients from training dataset.

### Figures
The `Figures_script/` folder provides scripts to reproduce figures.

---

## Requirements

- R (version 4.0 or higher recommended)

### Required Packages

tidyverse
forecast
tseries
ggplot2
caret

---
