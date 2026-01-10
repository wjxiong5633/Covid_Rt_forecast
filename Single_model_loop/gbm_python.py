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
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.preprocessing import StandardScaler

def gbm_fit(training_set, vars_final):
    df = training_set.copy()
    x_data = df[vars_final].values
    y_data = df["y_pred"].values

    # Normalize inputs
    scaler = StandardScaler()
    x_scaled = scaler.fit_transform(x_data)

    # Define and train Gradient Boosting model
    model = GradientBoostingRegressor(
        n_estimators=500,
        learning_rate=0.01,
        max_depth=3,
        subsample=0.8,
        min_samples_split=2,
        min_samples_leaf=1,
        max_features='sqrt',
        random_state=123
    )
    model.fit(x_scaled, y_data)
    return model, scaler


def gbm_predict(model, new_data, vars_final, scaler):
    x_new = new_data[vars_final].values
    x_scaled = scaler.transform(x_new)
    preds = model.predict(x_scaled)
    return preds

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
modelname = "gbm"

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
        h = 1
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


            if modelname in ["arima", "arima2", "xx"]:
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
            #print(vars_final)

            ######################################################################################
            #------------------------------------ training  -----------------------
            #########################################################################################################
            train_run_date =  pd.date_range(train_start_date, train_end_date-timedelta(h), freq="D")
            training_set = cov_lag_final_df [cov_lag_final_df["date"].isin(train_run_date)].dropna()

            model, scaler = gbm_fit(training_set, vars_final)

            FI = model.feature_importances_
            fi_df = pd.DataFrame({
                "var": vars_final,
                "coef": FI,
                "proj": h,
                "modelname": modelname
            })

            ## fi_df.to_csv(f"./train_coef/{modelname}_coef.csv", index=False)
            fi_all = pd.concat([fi_all, fi_df], ignore_index=True)

            train_data_x = training_set[vars_final]
            train_pred = gbm_predict(model, train_data_x, vars_final, scaler)

            pd.DataFrame({"pred_reportcase": train_pred,"true_reportcase": training_set["log_case"]})
            pd.DataFrame({"pred_reportcase": np.exp(train_pred),"true_reportcase": training_set["report_case"]})


            test_date =  pd.date_range(test_start_date, test_start_date, freq="D")
            test_set = cov_lag_final_df[cov_lag_final_df["date"].isin(test_date)]
            test_forecast_data_x = test_set[vars_final]
            test_forecast_res = gbm_predict(model, test_forecast_data_x, vars_final, scaler) 
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



gbm_all = run_model_multipleY("gbm")


gbm_all = pd.read_csv("./pred_reportcase_point/gbm_reportcase_point.csv")
print((gbm_all.groupby("proj").apply(lambda group: pd.Series({
            "rmse1": rmse(group["pred_reportcase"], group["true_reportcase"]),
            "rmse2": rmse(group["const_reportcase"], group["true_reportcase"])
            })).reset_index()
            ))


print(rmse(gbm_all["pred_reportcase"],gbm_all["true_reportcase"]))
print(rmse(gbm_all["const_reportcase"],gbm_all["true_reportcase"]))