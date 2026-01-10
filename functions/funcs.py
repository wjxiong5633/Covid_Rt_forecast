import pandas as pd 
import numpy as np
from scipy.stats import pearsonr
from scipy.stats import norm
from datetime import datetime, timedelta
from functools import reduce
import random
## from pygam import LinearGAM, s
from scipy.stats import pearsonr
from sklearn.metrics import mean_squared_error
from sklearn.metrics import roc_auc_score
from sklearn.metrics import accuracy_score

from statsmodels.stats.correlation_tools import cov_nearest
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
## import matplotlib.pyplot as plt
## import plotly.express as px
import pyreadr
from sklearn.metrics import mean_squared_error
from sklearn.metrics import roc_auc_score
from sklearn.metrics import accuracy_score
from sklearn import preprocessing
from datetime import datetime, timedelta
import os  
from itertools import chain
from statsmodels.stats.correlation_tools import cov_nearest
from sklearn.model_selection import train_test_split
import sys
import os


# def create_sequences(data, previous, target_column_idx):
#     """
#     Creates sequences of features (X) and targets (y) from multivariate time-series data.

#     Args:
#         data (np.ndarray): Multivariate time-series data as a NumPy array.
#         previous (int): Number of previous time steps to include in each sequence.
#         target_column_idx (int): Index of the target column in the NumPy array.

#     Returns:
#         X (np.array): Sequences of covariates with shape (n_samples, previous, n_features).
#         y (np.array): Corresponding target values with shape (n_samples,).
#     """
#     # Ensure the input data is 2D
#     if data.ndim == 1:
#         data = data.reshape(-1, 1)

#     X, y = [], []

#     # Loop through the dataset to create sequences
#     for i in range(len(data) - previous):
#         # Select the covariates for the current sequence
#         x_sequence = data[i:(i + previous), :]  # Include all columns
#         # Select the target value for the current sequence
#         y_value = data[i + previous, target_column_idx]
        
#         X.append(x_sequence)
#         y.append(y_value)
    
#     # Convert to NumPy arrays
#     X = np.array(X)
#     y = np.array(y)

#     return X, y



# ### RNN
# class RNN(nn.Module):
#   def __init__(self, input_size, hidden_size, num_layers, num_class):
#     super(RNN, self).__init__()
#     self.hidden_size = hidden_size
#     self.num_layers = num_layers
#     self.rnn = nn.RNN(input_size, hidden_size, num_layers, batch_first=True)
#     self.fc = nn.Linear(hidden_size, num_class)
    
#   def forward(self, x):
#     out, _ = self.rnn(x)
#     out = out[:, -1, :]
#     out = self.fc(out)
#     return out
     
# ## LSTM
# class LSTM(nn.Module):
#   def __init__(self, input_size, hidden_size, num_layers, num_class):
#     super(LSTM, self).__init__()
#     self.hidden_size = hidden_size
#     self.num_layers = num_layers
#     self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True)
#     self.fc = nn.Linear(hidden_size, num_class)
    
#   def forward(self, x):
#     h0 = Variable(torch.zeros(num_layers, x.size(0), hidden_size))
#     c0 = Variable(torch.zeros(num_layers, x.size(0), hidden_size))
#     out, _ = self.lstm(x, (h0, c0))
#     out = out[:, -1, :]
#     out = self.fc(out)
#     return out


# ## GRU
# class GRU(nn.Module):
#     def __init__(self, input_size, hidden_size, num_layers, num_class):
#         super(GRU, self).__init__()
#         self.num_layers = num_layers
#         self.hidden_size = hidden_size

#         self.gru = nn.GRU(input_size, hidden_size, num_layers, batch_first=True)
#         self.linear = nn.Linear(hidden_size, num_class)
        
#     def forward(self, x):
#         out, _ = self.gru(x)
#         out = out[:, -1, :]
#         out = self.linear(out)
#         return out


# def train_model(model, X_train, y_train, X_test=None, y_test=None):
#   loss_fn = nn.MSELoss()
#   optimizer = opt.Adam(model.parameters(), lr = 0.001)
#   num_epoches = 200

#   train_loss_hist = np.zeros(num_epoches)
#   test_loss_hist = np.zeros(num_epoches)

#   for epoch in range(num_epoches):
#     optimizer.zero_grad()
#     y_pred = model(X_train)
#     loss = loss_fn(y_pred, y_train)
#     loss.backward()
#     optimizer.step()
#     if X_test is not None:
#       with torch.no_grad():
#         y_test_pred = model(X_test)
#         test_loss = loss_fn(y_test_pred, y_test)
#       test_loss_hist[epoch] = test_loss.data

#       if (epoch+1)%10 == 0:
#         print('Epoch: %d, Train Loss: %.4f, Test Loss: %.4f' %  (epoch+1, loss.data, test_loss.data))
#     else:
#       if (epoch+1)%10 == 0:
#         print('Epoch: %d, Loss: %.4f' %  (epoch+1, loss.data))
#     train_loss_hist[epoch] = loss.data 
    
#   return y_pred, train_loss_hist, test_loss_hist  


# def loss_plot(model, X_train, y_train, X_test, y_test):
#   y_pred, train_loss_hist, test_loss_hist = train_model(model, X_train, y_train, X_test, y_test)
#   plt.plot(train_loss_hist, label='Train Loss')
#   plt.plot(test_loss_hist, label='Test Loss')
#   plt.legend()


def rmse(y_pred, y_true):
    return np.sqrt(mean_squared_error(y_true, y_pred))

def test_pred_true(model, X_test, y_test,valid_date_all,T):
        y_test_scaled = y_test.reshape(-1, 1)  # Reshape to column vector
        # Create zero placeholders for the other features
        zero_features = np.zeros((y_test_scaled.shape[0], len(vars_final)))
        # Inverse transform predictions
        y_test_true= scaler.inverse_transform(np.hstack((y_test_scaled, zero_features)))[:, 0] 

        y_test_pred_raw = model(X_test).detach().cpu().numpy().flatten()  # Move to CPU if using PyTorch
        y_test_pred_scaled = y_test_pred_raw.reshape(-1, 1)  # Reshape to column vector
        # Create zero placeholders for the other features
        # Inverse transform predictions
        y_test_pred = scaler.inverse_transform(np.hstack((y_test_pred_scaled, zero_features)))[:, 0]  # Extract only y_pred
       
        combine_df =  pd.DataFrame({"date":valid_date_all[T:], "true": y_test_true, "pred": y_test_pred})
        return(combine_df)

  


def extrapolate_fun(cov_lag_NA_df):
    # Assuming 'cov_lag_NA_df' has a 'date' column in datetime format
    cov_lag_NA_df['date'] = pd.to_datetime(cov_lag_NA_df['date'])
    
    # Ensure that covariates (columns excluding 'date' and 'date_analysis') are present in the dataframe
    covariates = cov_lag_NA_df.columns[2:]  # Skip the first two columns (date and date_analysis)
    
    # Set the target date for extrapolation (last date in the dataframe)
    target_date = cov_lag_NA_df['date'].iloc[-1]
    
    # Create an empty dataframe to store the extrapolated results
    extrapolated_df = cov_lag_NA_df[['date','y_pred']].copy()  
    
    # Loop through each covariate to perform the extrapolation
    for cov in covariates:
        # Create a temporary data frame for the covariate, removing NaN values
        temp_data = cov_lag_NA_df[['date', cov]].dropna()
        
        if len(temp_data) > 1 and temp_data['date'].iloc[-1] < target_date:
            # Predict the future values until the target date
            future_dates = pd.date_range(start=temp_data['date'].max() + pd.Timedelta(days=1), end=target_date)
            
            # Use the mean of the last 3 available values for extrapolation
            forecasted_values = np.repeat(temp_data[cov].tail(1).mean(), len(future_dates))
            
            # Combine the forecasted values and dates
            forecasted_data = pd.DataFrame({'date': future_dates, cov: forecasted_values})
            
            # Combine the original data with the forecasted data
            cov_merge_df = pd.concat([temp_data, forecasted_data], ignore_index=True)
        else:
            cov_merge_df = temp_data
        
        # Merge with the extrapolated dataframe
        extrapolated_df = pd.merge(extrapolated_df, cov_merge_df, on='date', how='left')
    
    # Return the extrapolated dataframe
    return extrapolated_df


def select_na_inf_columns(df):
    # List to store column names with NaN or inf values
    na_inf_columns = []

    # Iterate over columns
    for col in df.columns:
        if df[col].isnull().any() or np.isinf(df[col]).any():
            na_inf_columns.append(col)

    return na_inf_columns



def scale_fun(column, train_index):
    #print(column)
    train_mean = column[0:train_index].mean()
    train_sd = column[0:train_index].std()
    scaled_column = (column - train_mean) / train_sd
    return scaled_column

def find_highly_correlated_variables(df, threshold):
    corr_matrix = df.corr().abs()  # Calculate absolute correlation matrix
    upper_triangle = corr_matrix.where(
        pd.np.triu(pd.np.ones(corr_matrix.shape), k=1).astype(bool)
    )  # Exclude lower triangle (including diagonal) for symmetry
    
    # Find variables with correlation above the threshold
    correlated_vars = [
        column
        for column in upper_triangle.columns
        if any(upper_triangle[column] > threshold)
    ]
    
    return correlated_vars


def obtain_best_lag(C0_raw,stringency,meteorology,pollutant,epis_vec):
    mypredictors = epis_vec + meteorology + pollutant + stringency
    res_lag = pd.DataFrame()

    for i in range(len(mypredictors)):
        for lag in range(1, 8):
            sub1 = C0_raw.copy()
            sub2 = sub1.copy()
            sub2['date'] = sub2['date'] + pd.DateOffset(days=lag)
            sub = pd.merge(sub1[['date', 'y_pred']], sub2[['date', mypredictors[i]]], on='date')
            x1 = sub['y_pred']
            x2 = sub[mypredictors[i]]
            nas = np.logical_or(np.isnan(x1), np.isnan(x2))
            cor = pearsonr(x1[~nas], x2[~nas])[0]
            res_lag_tmp = pd.DataFrame({'pearson': cor, 'lag': lag, 'var': mypredictors[i]}, index=[0])
            res_lag = pd.concat([res_lag, res_lag_tmp], ignore_index=True)

    best_lag = (res_lag.groupby('var', group_keys=False).apply(lambda x: x.loc[x['pearson'].abs().idxmax()]).reset_index(drop=True))
    
    return best_lag


def obtain_data_alllag(data_raw,stringency,meteorology,pollutant,epis_vec):

    mypredictors = epis_vec + meteorology + pollutant + stringency
    dt_update = data_raw[['date', 'y_pred']].copy()

    for i in range(len(mypredictors)):
        for k in range(1, 8):
            dt_var = data_raw[['date', mypredictors[i]]].copy()
            dt_var2 = dt_var.copy()
            dt_var2['date'] = dt_var['date'] + pd.DateOffset(days=k)
            dt_var2.columns = ['date', mypredictors[i] + '_lag' + str(k)]
            dt_update = pd.merge(dt_update, dt_var2, on='date', how = 'outer')

    other_var_final = dt_update.copy()

    return other_var_final


def obtain_data_bestlag(best_lag, data_raw):
    mypredictors = best_lag['var'].values 
    dt_update = data_raw[['date', 'y_pred']].copy()

    for i in range(len(mypredictors)):
        #print(i)
        dt_var = data_raw[['date', mypredictors[i]]].copy()
        dt_var2 = dt_var.copy()
        k = best_lag.loc[best_lag['var'] == mypredictors[i], 'lag'].values[0]
        dt_var2['date'] = dt_var['date'] + pd.DateOffset(days=k)
        dt_var2.columns = ['date', mypredictors[i] + '_lag' + str(k)]
        dt_update = pd.merge(dt_update, dt_var2, on='date', how = "outer")

    other_var_final = dt_update.copy()

    return other_var_final



def get_reduced_vars_single(C0_raw,stringency,meteorology,pollutant,epis_vec):

    best_lag_raw = obtain_best_lag(C0_raw,stringency,meteorology,pollutant,epis_vec).sort_values(by='pearson', ascending=False,key=abs)
    best_lag_meteo = best_lag_raw[best_lag_raw['var'].isin(meteorology)].head(1)
    best_lag_pollu = best_lag_raw[best_lag_raw['var'].isin(pollutant)].head(1)
    best_lag_policy = best_lag_raw[best_lag_raw['var'].isin(stringency)].head(1)
    best_lag_case_ct = best_lag_raw[best_lag_raw['var'].isin(epis_vec)]
    best_lag2 = pd.concat([best_lag_meteo,best_lag_pollu, best_lag_policy,best_lag_case_ct]).query('pearson != 0')
    
    best_lag = best_lag2[abs(best_lag2["pearson"])>=0.3]

    cov_lag = obtain_data_bestlag(best_lag, C0_raw).dropna()
    cov_lag_1 = cov_lag.iloc[:, 2:]
    # Find highly correlated features and exclude them
    # Calculate the correlation matrix
    correlation_matrix = cov_lag_1.corr().abs()

    # Find highly correlated variables
    correlated_features = set()
    for i in range(len(correlation_matrix.columns)):
        for j in range(i):
            if correlation_matrix.iloc[i, j] >= 0.95:  # Set the correlation threshold as desired
                colname = correlation_matrix.columns[i]
                correlated_features.add(colname)

    # Exclude highly correlated variables from the dataframe
    cov_lag_all = cov_lag.drop(columns=correlated_features)
    
    reduced_vars = cov_lag_all.columns[2:]

    return reduced_vars.tolist()


def get_reduced_vars_more(C0_raw,stringency,meteorology,pollutant,epis_vec):

    best_lag_raw = obtain_best_lag(C0_raw,stringency,meteorology,pollutant,epis_vec).sort_values(by='pearson', ascending=False,key=abs)
    best_lag_meteo = best_lag_raw[best_lag_raw['var'].isin(meteorology)]
    best_lag_pollu = best_lag_raw[best_lag_raw['var'].isin(pollutant)]
    best_lag_policy = best_lag_raw[best_lag_raw['var'].isin(stringency)]
    best_lag_case_ct = best_lag_raw[best_lag_raw['var'].isin(epis_vec)]
    best_lag2 = pd.concat([best_lag_meteo,best_lag_pollu, best_lag_policy,best_lag_case_ct]).query('pearson != 0')
    
    best_lag = best_lag2[abs(best_lag2["pearson"])>=0.3]

    cov_lag = obtain_data_bestlag(best_lag, C0_raw).dropna()
    cov_lag_1 = cov_lag.iloc[:, 2:]
    # Find highly correlated features and exclude them
    # Calculate the correlation matrix
    correlation_matrix = cov_lag_1.corr().abs()

    # Find highly correlated variables
    correlated_features = set()
    for i in range(len(correlation_matrix.columns)):
        for j in range(i):
            if correlation_matrix.iloc[i, j] >= 0.9:  # Set the correlation threshold as desired
                colname = correlation_matrix.columns[i]
                correlated_features.add(colname)

    # Exclude highly correlated variables from the dataframe
    cov_lag_all = cov_lag.drop(columns=correlated_features)
    
    reduced_vars = cov_lag_all.columns[2:]

    return reduced_vars.tolist()




def get_reduced_vars_tillOptimal(C0_raw,stringency,meteorology,pollutant,epis_vec):
    best_lag_raw = obtain_best_lag(C0_raw,stringency,meteorology,pollutant,epis_vec).sort_values(by='pearson', ascending=False,key=abs)
    best_lag_meteo = best_lag_raw[best_lag_raw['var'].isin(meteorology)].head(1)
    best_lag_pollu = best_lag_raw[best_lag_raw['var'].isin(pollutant)].head(1)
    best_lag_policy = best_lag_raw[best_lag_raw['var'].isin(stringency)].head(1)
    best_lag_case_ct = best_lag_raw[best_lag_raw['var'].isin(epis_vec)]
    best_lag2 = pd.concat([best_lag_meteo,best_lag_pollu, best_lag_policy,best_lag_case_ct]).query('pearson != 0')
    
    best_lag = best_lag2

    cov_vars = []
    for i in range(len(best_lag)):
        var = best_lag['var'].values[i]
        lag = best_lag['lag'].values[i]
        cov_vars.extend([f"{var}_lag{j}" for j in range(1, lag + 1)])
    
    reduced_vars = cov_vars
    return reduced_vars




# def get_vars_highcor_lag(C0_raw,stringency,meteorology,epis_vec):
#     best_lag_raw = obtain_best_lag(C0_raw,stringency,meteorology,epis_vec).sort_values(by='pearson', ascending=False,key=abs)
#     best_lag_meteo = best_lag_raw[best_lag_raw['var'].isin(meteorology)].head(3)
#     best_lag_policy = best_lag_raw[best_lag_raw['var'].isin(stringency)].head(1)
#     best_lag_case_ct = best_lag_raw[best_lag_raw['var'].isin(epis_vec)]
#     best_lag2 = pd.concat([best_lag_meteo, best_lag_policy,best_lag_case_ct]).query('pearson != 0')
    
#     best_lag = best_lag2
    
#     mypredictors = best_lag['var'].values 
#     dt_update = C0_raw[['date', 'y_pred']].copy()

#     for i in range(len(mypredictors)):
#         for k in range(1, 15):
#             dt_var = C0_raw[['date', mypredictors[i]]].copy()
#             dt_var2 = dt_var.copy()
#             dt_var2['date'] = dt_var['date'] + pd.DateOffset(days=k)
#             dt_var2.columns = ['date', mypredictors[i] + '_lag' + str(k)]
#             dt_update = pd.merge(dt_update, dt_var2, on='date')

#     other_var_final = dt_update.copy()

#     return other_var_final



def get_combine_pred_df(model_res_df, ride_bp_rt_pred14, true_rt_dat):
    
    df_test = model_res_df.merge(ride_bp_rt_pred14, on=["date_analysis", "date", "proj"], how="left")
    sub1 = true_rt_dat
    sub2 = df_test
    sub3 = sub2.merge(sub1, on="date", how="left").sort_values("date_analysis")
    return sub3


def auc_calculate(pred, true):
    true_fct = np.where(true >= 1, 1, 0)
    pred_fct = np.where(pred >= 1, 1, 0)
    
    if len(np.unique(true_fct)) == 1 or len(np.unique(pred_fct)) == 1:
        true_fct = np.where(true > 1, 1, 0)
        pred_fct = np.where(pred > 1, 1, 0)
        auc_res = accuracy_score(true_fct, pred_fct)  ## return accuracy
    else:
        auc_res = roc_auc_score(true_fct, pred_fct)
    
    return auc_res


def rmse(y_pred, y_true):
    return np.sqrt(mean_squared_error(y_true, y_pred))


def mape(predicted, true):
    mape_value = np.mean(np.abs((true - predicted) / true))
    return mape_value

def get_error_df(model_res):
    dict = {'type': 'pred',
            'rmse': np.sqrt(mean_squared_error(model_res['pred_rt'], model_res['true_rt'])),
            'mape': mape(model_res['pred_rt'], model_res['true_rt']),
            'auc': auc_calculate(model_res['pred_rt'], model_res['true_rt'])}
    error_df = pd.DataFrame.from_dict(dict,orient='index').T
    return error_df

def get_error_ride_df(model_res):
    dict = {'type': 'ride',
            'rmse': np.sqrt(mean_squared_error(model_res['rt_temp_RIDE'], model_res['true_rt'])),
            'mape': mape(model_res['rt_temp_RIDE'], model_res['true_rt']),
            'auc': auc_calculate(model_res['rt_temp_RIDE'], model_res['true_rt'])}
    error_df = pd.DataFrame.from_dict(dict,orient='index').T
    return error_df

def get_error_bp_df(model_res):
    dict = {'type': 'bp',
            'rmse': np.sqrt(mean_squared_error(model_res['rt_temp_BP'], model_res['true_rt'])),
            'mape': mape(model_res['rt_temp_BP'], model_res['true_rt']),
            'auc': auc_calculate(model_res['rt_temp_BP'], model_res['true_rt'])}
    error_df = pd.DataFrame.from_dict(dict,orient='index').T
    return error_df


def get_error_by_proj(model_res):
    split_list = model_res.groupby('proj')
    error_proj_df = pd.concat([get_error_df(df) for _, df in split_list]).reset_index(drop=True)
    error_proj_df['proj'] = range(min(model_res['proj']), max(model_res['proj']) + 1)
    return error_proj_df

def get_error_ride_by_proj(model_res):
    split_list = model_res.groupby('proj')
    error_proj_df = pd.concat([get_error_ride_df(df) for _, df in split_list]).reset_index(drop=True)
    error_proj_df['proj'] = range(min(model_res['proj']), max(model_res['proj']) + 1)
    return error_proj_df

def get_error_bp_by_proj(model_res):
    split_list = model_res.groupby('proj')
    error_proj_df = pd.concat([get_error_bp_df(df) for _, df in split_list]).reset_index(drop=True)
    error_proj_df['proj'] = range(min(model_res['proj']), max(model_res['proj']) + 1)
    return error_proj_df


def quant2(x, a):
    return np.quantile(x, a)


## only predict for point estimation
## fit or prediction 
def pred_fun(modelname, models, dat, data_type):
    df = dat
    if modelname in ["arima"]:
        if data_type in ["train"]:
            predicted_mean = models.predict_in_sample(exogenous=df, return_conf_int=False)  
        else:
            print("forecast:", len(df))
            predicted_mean = models.predict(n_periods = len(df),exogenous=np.array(df), return_conf_int=False)  
    elif modelname in ["nn"]:
        predicted_mean = models.predict(df).flatten()
    else:
        predicted_mean = models.predict(df)
    return predicted_mean


def pred_fun_nowcast(modelname,nowcast_model,dat):
    if modelname in ["arima"]:
        #print("arima")
        df = dat
        predicted_mean = nowcast_model.predict_in_sample(df)  
        nowcast_df = pd.DataFrame({"proj":0, "pred_rt" : predicted_mean})
    elif modelname in ["nn"]:
        df = dat
        predicted_mean = nowcast_model.predict(df).flatten()
        nowcast_df = pd.DataFrame({"proj":0, "pred_rt" : predicted_mean})
    else:
        df = dat
        predicted_mean = nowcast_model.predict(df)
        nowcast_df = pd.DataFrame({"proj":0, "pred_rt" : predicted_mean})
    return nowcast_df

def pred_fun_singleY(modelname,model,dat):
    df = dat
    predicted_mean = model.predict(df)
    pred_df = pd.DataFrame({"pred_rt" : predicted_mean})
    return pred_df


def pred_fun_multipleY(modelname, multipleY_models, dat):
    df = dat
    predicted_mean = pd.DataFrame(multipleY_models.predict(df))
    pred_all = pd.melt(predicted_mean, var_name="proj").rename(columns={"value": "pred_case"})
    pred_all['proj'] = pred_all['proj']
    return pred_all



def pred_fun_multipleY_2(modelname, multipleY_models, dat):
    df = dat
    predicted_mean = pd.DataFrame(multipleY_models.predict(df))
    pred_all = pd.melt(predicted_mean, var_name="proj").rename(columns={"value": "pred_case"})
    pred_all['proj'] = pred_all['proj']+1
    return pred_all


# ## fit or prediction 
# def pred_fun(modelname, models, dat, data_type, n_bootstrap):
#     alpha = np.array([0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]) / 2
#     alpha2 = 1 - alpha
#     df = dat
#     alpha_length = len(alpha)   

#     if modelname in ["arima"]:
#         pred_rt_lwr = np.full((df.shape[0], len(alpha)), np.nan)
#         pred_rt_upr = np.full((df.shape[0], len(alpha)), np.nan)
        
#         if data_type in ["train"]:
#             predicted_mean = models.predict_in_sample(exogenous=df, return_conf_int=False)  
#             for m in range(alpha_length):
#                 conf_int= models.predict_in_sample(exogenous=df, return_conf_int=True, alpha=alpha[m]*2)[1] 
#                 pred_rt_lwr[:, m] = conf_int[:,0]
#                 pred_rt_upr[:, m] = conf_int[:,1]
#         else:
#             predicted_mean = models.predict(n_periods = len(df),exogenous=np.array(df), return_conf_int=False)  
#             for m in range(alpha_length):
#                 conf_int= models.predict(n_periods = len(df),exogenous=np.array(df), return_conf_int=True, alpha=alpha[m]*2)[1] 
#                 pred_rt_lwr[:, m] = conf_int[:,0]
#                 pred_rt_upr[:, m] = conf_int[:,1]

#         colnames_pred_rt_lwr = ["pred_rt_lwr" + str(m) for m in range(1, alpha_length + 1)]
#         colnames_pred_rt_upr = ["pred_rt_upr" + str(m) for m in range(1, alpha_length + 1)]

#         dd = pd.DataFrame(data=np.column_stack((predicted_mean, pred_rt_lwr, pred_rt_upr)),
#                 columns=["predicted_mean"] + colnames_pred_rt_lwr + colnames_pred_rt_upr)
#     elif modelname in ["gam"]:    
#         pred = np.full((n_bootstrap, df.shape[0]), np.nan)
#         for k in range(n_bootstrap):
#             pred[k, :] = models[k].predict(df)

#         predicted_mean = np.mean(pred, axis=0)
#         pred_rt_lwr = np.full((df.shape[0], len(alpha)), np.nan)
#         pred_rt_upr = np.full((df.shape[0], len(alpha)), np.nan)

        
#         for m in range(alpha_length):
#             pred_rt_lwr[:, m] = np.apply_along_axis(quant2, axis=0, arr=pred, a=alpha[m])
#             pred_rt_lwr[:, m] = np.where(pred_rt_lwr[:, m] < 0, 0, pred_rt_lwr[:, m])
#             pred_rt_upr[:, m] = np.apply_along_axis(quant2, axis=0, arr=pred, a=alpha2[m])
#             pred_rt_upr[:, m] = np.where(pred_rt_upr[:, m] < 0, 0, pred_rt_upr[:, m])

#         colnames_pred_rt_lwr = ["pred_rt_lwr" + str(m) for m in range(1, alpha_length + 1)]
#         colnames_pred_rt_upr = ["pred_rt_upr" + str(m) for m in range(1, alpha_length + 1)]

#         dd = pd.DataFrame(data=np.column_stack((predicted_mean, pred_rt_lwr, pred_rt_upr)),
#             columns=["predicted_mean"] + colnames_pred_rt_lwr + colnames_pred_rt_upr)
        
#     return dd


# def pred_fun_multipleY(modelname, models, dat, n_bootstrap):
#     alpha = np.array([0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]) / 2
#     alpha2 = 1 - alpha
#     df = dat
#     alpha_length = len(alpha)   

#     ## i = pred_horizon 
#     pred_all = []
#     for i in range(0,15):
#         pred_i = np.full((n_bootstrap, df.shape[0]), np.nan)
#         for k in range(n_bootstrap):
#             boot_k_res = pd.DataFrame(models[k].predict(df))
#             pred_i[k,:] = boot_k_res.iloc[:,i].values

#         predicted_mean = np.mean(pred_i, axis=0)
#         pred_rt_lwr = np.full((df.shape[0], len(alpha)), np.nan)
#         pred_rt_upr = np.full((df.shape[0], len(alpha)), np.nan)

#         for m in range(alpha_length):
#             pred_rt_lwr[:, m] = np.apply_along_axis(quant2, axis=0, arr=pred_i, a=alpha[m])
#             pred_rt_lwr[:, m] = np.where(pred_rt_lwr[:, m] < 0, 0, pred_rt_lwr[:, m])
#             pred_rt_upr[:, m] = np.apply_along_axis(quant2, axis=0, arr=pred_i, a=alpha2[m])
#             pred_rt_upr[:, m] = np.where(pred_rt_upr[:, m] < 0, 0, pred_rt_upr[:, m])

#         colnames_pred_rt_lwr = ["pred_rt_lwr" + str(m) for m in range(1, alpha_length + 1)]
#         colnames_pred_rt_upr = ["pred_rt_upr" + str(m) for m in range(1, alpha_length + 1)]

#         dd_i = pd.DataFrame(data=np.column_stack((predicted_mean, pred_rt_lwr, pred_rt_upr)),
#             columns=["predicted_mean"] + colnames_pred_rt_lwr + colnames_pred_rt_upr)
#         dd_i["proj"] = i
#         pred_all.append(dd_i)

#     return pred_all
