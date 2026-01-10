import pandas as pd
import numpy as np
import pyreadr
from datetime import datetime, timedelta
from functools import reduce
import random
# from pygam import LinearGAM, s
from scipy.stats import pearsonr
from sklearn.metrics import mean_squared_error
from sklearn.metrics import roc_auc_score
from sklearn.metrics import accuracy_score
import torch
import numpy as np

from statsmodels.stats.correlation_tools import cov_nearest
import sys
import os
sys.path.append(os.path.join(os.getcwd(), 'functions'))
from funcs import *
##from run_model.models.single_model import *
import pickle
import statsmodels.api as sm

from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV
from xgboost.sklearn import XGBRegressor


def gru_fit(training_set, vars_final):
    import numpy as np
    import torch
    import torch.nn as nn
    from sklearn.preprocessing import StandardScaler

    # Define the GRU network
    class GRUNet(nn.Module):
        def __init__(self, input_size, hidden_size, num_layers):
            super(GRUNet, self).__init__()
            self.gru = nn.GRU(input_size=input_size, hidden_size=hidden_size, num_layers=num_layers, batch_first=True)
            self.fc = nn.Linear(hidden_size, 1)

        def forward(self, x):
            out, _ = self.gru(x)
            out = out[:, -1, :]  # use the last time step
            out = self.fc(out)
            return out

    # Set seeds for reproducibility
    np.random.seed(123)
    torch.manual_seed(123)

    # Prepare the data
    df = training_set.copy()
    x_data = df[vars_final].values
    y_data = df["y_pred"].values.reshape(-1, 1)

    # Normalize inputs
    scaler = StandardScaler()
    x_scaled = scaler.fit_transform(x_data)

    # Reshape for GRU: [samples, time_steps=1, features]
    x_seq = x_scaled.reshape(x_scaled.shape[0], 1, x_scaled.shape[1])
    X = torch.tensor(x_seq, dtype=torch.float32)
    y = torch.tensor(y_data, dtype=torch.float32)

    # Define model parameters (fixed)
    hidden_size = 512
    num_layers = 3

    # Initialize model
    model = GRUNet(input_size=X.shape[2], hidden_size=hidden_size, num_layers=num_layers)
    criterion = nn.MSELoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=0.001)

    # Training loop
    model.train()
    for epoch in range(100):
        optimizer.zero_grad()
        output = model(X)
        loss = criterion(output, y)
        loss.backward()
        optimizer.step()

        if (epoch + 1) % 10 == 0:
            print(f"Epoch {epoch+1}/100 - Loss: {loss.item():.4f}")

    print("Training complete. Model parameters:")
    print(f"Hidden size: {hidden_size}, Num layers: {num_layers}")

    return model, scaler


def gru_predict(model, new_data, vars_final, scaler):
   
    x_new = new_data[vars_final].values
    x_scaled = scaler.transform(x_new)
    x_seq = x_scaled.reshape(x_scaled.shape[0], 1, x_scaled.shape[1])
    X_tensor = torch.tensor(x_seq, dtype=torch.float32)

    model.eval()
    with torch.no_grad():
        preds = model(X_tensor).numpy().flatten()

    return preds

def compute_FI_saliency_map(model, scaler, training_set,vars_final,):
    x_new = training_set[vars_final].values
    x_scaled = scaler.transform(x_new)
    x_seq = x_scaled.reshape(x_scaled.shape[0], 1, x_scaled.shape[1])
    X_tensor = torch.tensor(x_seq, dtype=torch.float32)

    model.eval()
    # Require gradients for input
    X_tensor = X_tensor.clone().detach().requires_grad_(True)

    # Forward pass
    output = model(X_tensor).squeeze()

    # If multiple samples, compute gradients one by one (optional)
    loss = output  # use the raw output (or loss = (output - y_tensor.squeeze())**2)

    # Backward pass
    model.zero_grad()
    loss.backward(torch.ones_like(loss))

    # Get gradients of inputs
    saliency = X_tensor.grad.abs().detach().numpy()

    # Collapse time dimension (your time step = 1), return saliency per feature
    saliency_per_feature = saliency[:, 0, :]  # shape: (n_samples, n_features)

    feature_importance = saliency_per_feature.mean(axis=0)
    return feature_importance

#####################################################
d1 = pyreadr.read_r('./data/data_case.rds') 
data_case = d1[None]
data_case['date'] = pd.to_datetime(data_case['date'])
true_dat = data_case.copy()

d2 = pyreadr.read_r('./data/other_cov.Rds') 
other_cov  = d2[None]
other_cov ['date'] = pd.to_datetime(other_cov ['date'])

# Define variables
stringency = ["StringencyIndex_WeightedAverage", "GovernmentResponseIndex_WeightedAverage", "ContainmentHealthIndex_WeightedAverage",
]
meteorology = ["temperature", "humidity", "wind_speed"]
pollutant = [ "ozone", "nitrogen_dioxide", "nitrogen_oxides", "carbon_monoxide",
    "fine_suspended_particulates", "respirable_suspended_particulates", "sulphur_dioxide"]

vec_need_scale = ["ct_sm_7"] + meteorology + pollutant + stringency
ahead_horizon_case = 14
modelname = "gru"

def run_model_multipleY(modelname): 
    test_start_period = pd.date_range(start="2022-05-28", end="2022-12-31", freq="d")
    test_df = None
    fi_all = None
    i_date =0

    for i_date in range(0,len(test_start_period)) :
        test_start_date = test_start_period[i_date]
        test_end_date = test_start_date +timedelta(ahead_horizon_case)
        train_start_date = datetime.strptime("2022-02-15", "%Y-%m-%d").date()
        train_end_date = test_start_date - timedelta(1)
        
        print("-----------------------")
        print(test_start_date)

        # Filter data based on date range

        date1 = pd.date_range(train_start_date, test_start_date, freq='D')
        dt_case2 = data_case[data_case['date'].isin(date1)]

        ## observed_data till 0527
        observed_data  = pd.merge(dt_case2, other_cov, on="date", how="left")
        raw_data = observed_data 


        test_pred = []
        for h in range(0,15):
            print("horizon = ", h)
            C0_raw = raw_data.copy()
            C0_raw['log_case'] = raw_data['log_case'].shift(-h)
            C0_raw['scaled_case'] = C0_raw['log_case']
            C0_raw['y_pred'] = C0_raw['scaled_case']
        
            ## do scale for all observed data till 0527:
            ## scale for dataset

            # scale for dataset
            need_scale_cov = C0_raw[vec_need_scale]
            train_cov_date = pd.date_range(train_start_date, train_end_date, freq="D")
            train_index = len(train_cov_date)
            scale_cov = pd.DataFrame(C0_raw[["date", "report_case","log_case","y_pred","scaled_case"]])
            scale_cov[need_scale_cov.columns] = need_scale_cov.apply(lambda column: scale_fun(column, train_index), axis=0)
        
            epis_vec = ["scaled_case","ct_sm_7"]
            cov_lag_weather_policy = obtain_data_alllag(scale_cov,stringency,meteorology,pollutant,epis_vec)

            cov_lag_df = pd.merge(cov_lag_weather_policy, scale_cov[['date',"report_case","log_case"]], on='date', how='left')
            full_date = pd.DataFrame({'date': pd.date_range(start=train_start_date, 
                                                            end=test_start_date + pd.Timedelta(days=ahead_horizon_case), freq='D')})
            cov_lag_NA_df = pd.merge(full_date, cov_lag_df, on='date', how='outer')
            cov_lag_final_df = extrapolate_fun(cov_lag_NA_df)       


            cols_order = ["date", "report_case", "log_case","y_pred"] + \
                [col for col in cov_lag_final_df.columns if col not in ["date", "report_case", "log_case", "y_pred"]]

            cov_lag_df = cov_lag_final_df[cols_order]
            cov_lag_final_df = cov_lag_df.copy()


            if modelname in ["arima", "arima2", "gru"]:
                reduced_vars = get_reduced_vars_single(C0_raw,stringency,meteorology,pollutant,epis_vec)
                vars_final = reduced_vars 
            elif modelname in ["xx"]:
                reduced_vars = get_reduced_vars_tillOptimal(C0_raw,stringency,meteorology,pollutant,epis_vec)
                vars_final = reduced_vars 
            else:
                vars_final =  cov_lag_final_df.columns[4:].tolist()

            inf_columns = cov_lag_final_df.columns[cov_lag_final_df.isin([-np.inf]).any()]
            vars = vars_final 
            vars_final = [x for x in vars if x not in inf_columns]
            print(vars_final)

            ######################################################################################
            #------------------------------------ training  -----------------------
            #########################################################################################################
            train_run_date =  pd.date_range(train_start_date, train_end_date-timedelta(h), freq="D")
            training_set = cov_lag_final_df [cov_lag_final_df["date"].isin(train_run_date)].dropna()

            model, scaler = gru_fit(training_set, vars_final)

            FI = compute_FI_saliency_map(model,scaler, training_set,vars_final)
            fi_df = pd.DataFrame({
                "var": vars_final,
                "coef": FI,
                "proj": h,
                "modelname": modelname
            })

            ## fi_df.to_csv(f"./train_coef/{modelname}_coef.csv", index=False)
            fi_all = pd.concat([fi_all, fi_df], ignore_index=True)

            train_data_x = training_set[vars_final]
            train_pred = gru_predict(model, train_data_x, vars_final, scaler)

            pd.DataFrame({"pred_reportcase": train_pred,"true_reportcase": training_set["log_case"]})
            pd.DataFrame({"pred_reportcase": np.exp(train_pred),"true_reportcase": training_set["report_case"]})


            test_date =  pd.date_range(test_start_date, test_start_date, freq="D")
            test_set = cov_lag_final_df[cov_lag_final_df["date"].isin(test_date)]
            test_forecast_data_x = test_set[vars_final]
            test_forecast_res = gru_predict(model, test_forecast_data_x, vars_final, scaler) 
            test_pred.append(test_forecast_res[0])

        test_period = pd.date_range(test_start_date, test_end_date, freq="D")
        test_reportcase = pd.DataFrame({
                                "mytestdate": [test_start_date] * (ahead_horizon_case + 1),
                                "date": [test_start_date + pd.Timedelta(days=i) for i in range(ahead_horizon_case + 1)],
                                "pred_reportcase": np.exp(test_pred),
                                "true_reportcase": true_dat[true_dat['date'].isin(test_period)]['report_case'].values,
                                "proj": list(range(ahead_horizon_case + 1)),
                                "const_reportcase": [raw_data.dropna()['report_case'].iloc[-2]] * (ahead_horizon_case + 1)
                            })                           
        rmse_df = (test_reportcase.groupby("proj").apply(lambda group: pd.Series({
            "rmse1": rmse(group["pred_reportcase"], group["true_reportcase"]),
            "rmse2": rmse(group["const_reportcase"], group["true_reportcase"])
            })).reset_index()
            )

        print("RMSE of prediction:")
        print(rmse_df)
        test_df = pd.concat([test_df, test_reportcase], ignore_index=True)
        
    print(test_df)
    test_df.to_csv(f"./pred_reportcase_point/{modelname}_reportcase_point.csv", index=False)

    fi_all.to_csv(f"./train_coef/{modelname}_coef_all.csv", index=False)
    return(test_df)



gru_all = run_model_multipleY("gru")


gru_all = pd.read_csv("./pred_reportcase_point/gru_reportcase_point.csv")
(gru_all.groupby("proj").apply(lambda group: pd.Series({
            "rmse1": rmse(group["pred_reportcase"], group["true_reportcase"]),
            "rmse2": rmse(group["const_reportcase"], group["true_reportcase"])
            })).reset_index()
            )


rmse(gru_all["pred_reportcase"],gru_all["true_reportcase"])

rmse(gru_all["const_reportcase"],gru_all["true_reportcase"])