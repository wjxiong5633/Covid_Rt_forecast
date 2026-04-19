Forecasting Framework with Model Selection and Prediction Intervals (R Implementation)
This repository contains R code for a forecasting project that evaluates individual models, performs model selection (top‑N and window size tuning), builds ensembles, and produces point forecasts as well as prediction intervals. The framework supports simulation, baseline comparisons, and result visualisation.

Repository Structure
text
├── data/                                   # Datasets required by the project
├── sim/                                    # Simulation code (R scripts)
├── Single_model_loop/                      # Code for training/evaluating individual models
├── run_model/                              # Code to execute individual model runs
├── choose_ws_choose_topn_model_code/       # Code for selecting window size and top‑N models
├── ensemble/                               # Code for ensemble methods (simple average)
├── pred_reportcase_point/                  # Point prediction results (report case)
├── pred_rt_point/                          # Point prediction results (Rt)
├── pred_reportcase_interval/               # Prediction interval results (report case)
├── pred_rt_interval/                       # Prediction interval results (Rt)
├── baseline/                               # Baseline method results
├── train_coef/                             # Coefficients estimated from training sets
└── Figures_script/                         # Scripts to generate figures from results
Overview
Data – Place your datasets in data/. The code expects a specific format (see usage notes).

Simulation – The sim/ folder contains the main simulation routines that orchestrate model training, forecasting, and evaluation.

Individual Models – Use Single_model_loop/ to train and test each model separately. The run_model/ scripts provide convenient wrappers to launch these runs.

Model Selection – The choose_ws_choose_topn_model_code/ folder implements:

Rolling window size selection.

Top‑N model selection (e.g., based on out‑of‑sample performance).

Ensemble – The ensemble/ folder contains code to combine forecasts from multiple models.

Predictions – Results are organised into point forecasts (*_point/) and prediction intervals (*_interval/), separately for the final report case and for real‑time (rolling) evaluation.

Baselines – Results of baseline method.

Training Coefficients – train_coef/ stores estimated coefficients from each training window (useful for analysing parameter stability).

Figures – Figures_script/ provides R scripts to reproduce all plots shown in the paper/report.


Requirements
R (version 4.0 or higher recommended)

Essential R packages:
tidyverse
forecast 
tseries 
ggplot2
caret
