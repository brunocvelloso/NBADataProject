#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 19 10:09:47 2023

@author: ricardovelloso
"""

import pandas as pd
import pyarrow
import numpy
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split, KFold, GridSearchCV
from sklearn.metrics import make_scorer, mean_absolute_error
import dill
import os
import re
import requests
#from sklearn.ensemble import RandomForestRegressor

os.getcwd()
#INPUT WORKING DIRECTORY HERE
#os.chdir("/Users/ToGithubRepo/NBADATAProject/py/")


# Create the scoring function for Mean Absolute Error (MAE)
scoring_function = make_scorer(mean_absolute_error, greater_is_better=False)

#load necessary dataframes from dropbox
file_links = ["regdata_FINAL.feather","pred_dataACTUAL.feather","pred_dataALLCOMBS.feather", "pred_team.feather"]

for file_link in file_links:
    link = f"https://www.dropbox.com/scl/fo/51g9vud1seqtjmyl2gdxt/h/{file_link}?rlkey=w73ql5hcbve241kau127t2a2h&dl=1"
    response = requests.get(link)
    if file_link.endswith(".feather"):
        file_name = file_link.replace(".feather", "")
    elif file_link.endswith(".pkd"):
        file_name = file_link.replace(".pkd", "")
    else:
        print(f"Unsupported file format: {file_link}")
        continue
    
    if file_link.endswith(".feather"):
        data = pd.read_feather(link)
    elif file_link.endswith(".pkd"):
        data = dill.loads(response.content)
    else:
        print(f"Unsupported file format: {file_link}")
        continue
    globals()[file_name] = data #rename file
    print(f"Loaded data from {file_link}:")

#regdata_FINAL = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/my_dataframe.feather")
#pred_dataACTUAL = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_dataACTUAL.feather")
#pred_dataALLCOMBS = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_dataALLCOMBS.feather")
#pred_team = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_team.feather")

ind_variable_names = [col for col in regdata_FINAL.columns if col.startswith("IND_")]
team_variable_names = [col for col in regdata_FINAL.columns if col.startswith("TEAM_")]
comb_variable_names = [col for col in regdata_FINAL.columns if col.startswith("COMB_")]
indteam_variable_names = [col for col in regdata_FINAL.columns if col.startswith("IND_") or col.startswith("TEAM_")]
all_variable_names = [col for col in regdata_FINAL.columns if col.startswith("IND_") or col.startswith("TEAM_") or col.startswith("COMB_")]
mask = regdata_FINAL['WGT_MIN'] < 5
regdata_FINAL.loc[mask, 'WGT_MAIN'] = 0
weights = regdata_FINAL['WGT_MAIN']

def alterdata(DATASET):
    pred_dataACTUAL2 = DATASET.copy()
    # Group by "SP_Team" and calculate the sum of "WGT_MIN" for each group
    pred_dataACTUAL2['SP_TotalMin'] = pred_dataACTUAL2.groupby('SP_Team')['WGT_MIN'].transform('sum')
    pred_dataACTUAL2['SP_ThreePct_reg'] = pred_dataACTUAL2['WGT_MIN']*pred_dataACTUAL2['IND_ThreePApm_reg']
    pred_dataACTUAL2['SP_TwoPct_reg'] = pred_dataACTUAL2['WGT_MIN']*pred_dataACTUAL2['IND_TwoPApm_reg']
    pred_dataACTUAL2['SP_ThreePct_reg'] = pred_dataACTUAL2.groupby('SP_Team')['SP_ThreePct_reg'].transform('sum')
    pred_dataACTUAL2['SP_TwoPct_reg'] = pred_dataACTUAL2.groupby('SP_Team')['SP_TwoPct_reg'].transform('sum')
    
    
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_MINPG_reg'] * row['WGT_MIN'],
                                                                TEAM_MINPG_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_Guard_reg'] * row['WGT_MIN'],
                                                                TEAM_Guard_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_Forward_reg'] * row['WGT_MIN'],
                                                                TEAM_Forward_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_Center_reg'] * row['WGT_MIN'],
                                                                TEAM_Center_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_TwoPApm_reg'] * row['WGT_MIN'],
                                                                TEAM_TwoPApm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_ThreePApm_reg'] * row['WGT_MIN'],
                                                                TEAM_ThreePApm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_FTMpm_reg'] * row['WGT_MIN'],
                                                                TEAM_FTMpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_Age_reg'] * row['WGT_MIN'],
                                                                TEAM_Age_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_NonShooter_reg'] * row['WGT_MIN'],
                                                                TEAM_NonShooter_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_ImpactPlayspm_reg'] * row['WGT_MIN'],
                                                                TEAM_ImpactPlayspm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_PTSpm_reg'] * row['WGT_MIN'],
                                                                TEAM_PTSpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_OREBpm_reg'] * row['WGT_MIN'],
                                                                TEAM_OREBpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_DREBpm_reg'] * row['WGT_MIN'],
                                                                TEAM_DREBpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_ASTpm_reg'] * row['WGT_MIN'],
                                                                TEAM_ASTpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_STLpm_reg'] * row['WGT_MIN'],
                                                                TEAM_STLpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_BLKpm_reg'] * row['WGT_MIN'],
                                                                TEAM_BLKpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_TOpm_reg'] * row['WGT_MIN'],
                                                                TEAM_TOpm_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_TSPct_reg'] * row['WGT_MIN'],
                                                                TEAM_TSPct_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_RAPM5_off'] * row['WGT_MIN'],
                                                                TEAM_RAPM5_off  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_RAPM5_def'] * row['WGT_MIN'],
                                                                TEAM_RAPM5_def  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_rapmoffXmin'] * row['WGT_MIN'],
                                                                TEAM_rapmoffXmin  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_rapmdefXmin'] * row['WGT_MIN'],
                                                                TEAM_rapmdefXmin  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_ThreePct_reg'] * row['WGT_MIN'] * row['IND_ThreePApm_reg'],
                                                                TEAM_ThreePct_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_ThreePct_reg'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_TwoPct_reg'] * row['WGT_MIN'] * row['IND_TwoPApm_reg'],
                                                                TEAM_TwoPct_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TwoPct_reg'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: ((row['IND_ThreePct_reg']>.35).astype(int)) * row['WGT_MIN'],
                                                                TEAM_NumShooters_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: ((row['IND_PTSpm_reg']>20).astype(int)) * row['WGT_MIN'],
                                                                TEAM_NumScorers_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: ((row['IND_ASTpm_reg']>4.5).astype(int)) * row['WGT_MIN'],
                                                                TEAM_NumPassers_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: (((row['IND_OREBpm_reg'] + row['IND_DREBpm_reg'])>8.5).astype(int)) * row['WGT_MIN'],
                                                                TEAM_NumRebounders_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_PlusMinRaw5_reg'] * row['WGT_MIN'],
                                                                TEAM_PlusMinRaw5_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_PlusMinRaw3_reg'] * row['WGT_MIN'],
                                                                TEAM_PlusMinRaw3_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_PlusMinRaw1_reg'] * row['WGT_MIN'],
                                                                TEAM_PlusMinRaw1_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: row['IND_MINPG_reg'] * row['WGT_MIN'],
                                                                TEAM_MINPG_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
    pred_dataACTUAL2 = pred_dataACTUAL2.assign(
                                                                SP_prod_c_d = lambda row: (row['IND_PF_reg'] + row['IND_C_reg'])  * row['WGT_MIN'],
                                                                TEAM_ShareBig_reg  = lambda row: (row.groupby(['SP_Team']).transform('sum')['SP_prod_c_d'] - row['SP_prod_c_d']) / row['SP_TotalMin'])
        
    
    
    columns_to_square = ["TEAM_Guard_reg", "TEAM_Forward_reg", "TEAM_Center_reg", "TEAM_NumShooters_reg",
                         "TEAM_NumScorers_reg", "TEAM_NumPassers_reg", "TEAM_NumRebounders_reg", "TEAM_ShareBig_reg",
                         "TEAM_NonShooter_reg"]
    
    for i in columns_to_square:
        new_var_name = i + "_SQ"
        pred_dataACTUAL2[new_var_name] = pred_dataACTUAL2[i] * pred_dataACTUAL2[i]
        
    pred_dataACTUAL2['SP_PositionDet'] = numpy.where(pred_dataACTUAL2['IND_PG_reg'] == 1, 'PG', 'G')
    pred_dataACTUAL2['SP_PositionDet'] = numpy.where(pred_dataACTUAL2['IND_SG_reg'] == 1, 'SG', pred_dataACTUAL2['SP_PositionDet'])
    pred_dataACTUAL2['SP_PositionDet'] = numpy.where(pred_dataACTUAL2['IND_SF_reg'] == 1, 'SF', pred_dataACTUAL2['SP_PositionDet'])
    pred_dataACTUAL2['SP_PositionDet'] = numpy.where(pred_dataACTUAL2['IND_PF_reg'] == 1, 'PF', pred_dataACTUAL2['SP_PositionDet'])
    pred_dataACTUAL2['SP_PositionDet'] = numpy.where(pred_dataACTUAL2['IND_C_reg'] == 1, 'C', pred_dataACTUAL2['SP_PositionDet'])
    
    
    counter = 1
    # Loop through columns that start with 'IND_'
    for i, col_i in pred_dataACTUAL2.filter(regex=r'^IND_').iteritems():
        # Check if the column starts with 'IND_SeasD'
        if i.startswith('IND_SeasD'):
            continue
        for j, col_j in pred_dataACTUAL2.filter(regex=r'^TEAM_').iteritems():
            # Create the new interaction term column name
            newname = f"COMB_{i}_INT_{j}"
            pred_dataACTUAL2[newname] = col_i * col_j
            counter += 1
    counter
    import re
    filtered_words = [col for col in pred_dataACTUAL2.columns if re.search(r'^COMB_.*_INT_Guard_reg_.*', col)]
    filtered_words2 = [col for col in pred_dataACTUAL2.columns if re.search(r'^COMB_.*_INT_Forward_reg_.*', col)]
    filtered_words3 = [col for col in pred_dataACTUAL2.columns if re.search(r'^COMB_.*_INT_Center_reg_.*', col)]
    pred_dataACTUAL2 = pred_dataACTUAL2.drop(filtered_words, axis=1)
    pred_dataACTUAL2 = pred_dataACTUAL2.drop(filtered_words2, axis=1)
    pred_dataACTUAL2 = pred_dataACTUAL2.drop(filtered_words3, axis=1)
    
    return pred_dataACTUAL2


pred_dataACTUAL = alterdata(pred_dataACTUAL)
unique_teams = pred_team['SP_Team'].unique()
team_list1 = team_variable_names

for j in unique_teams:
    team_mask = pred_dataACTUAL['SP_Team'] == j
    # Calculate the sumproduct for each column in team_list1
    for i in team_list1:
        sumproduct_result = numpy.sum(pred_dataACTUAL[i][team_mask] * pred_dataACTUAL['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL['IND_MINPG_reg'][team_mask])
        pred_team.loc[pred_team['SP_Team'] == j, i] = sumproduct_result
pred_dataALLCOMBS2 = pd.merge(pred_dataALLCOMBS.drop(columns=team_variable_names), pred_team, on="SP_Team", how="left")
pred_teamFINAL = pred_team
with open('pred_teamFINAL.pkd', 'wb') as f:
    dill.dump(pred_teamFINAL, f)
# Loop through columns that start with 'IND_'
for i, col_i in pred_dataALLCOMBS2.filter(regex=r'^IND_').iteritems():
    # Check if the column starts with 'IND_SeasD'
    if i.startswith('IND_SeasD'):
        continue
    for j, col_j in pred_dataALLCOMBS2.filter(regex=r'^TEAM_').iteritems():
        # Create the new interaction term column name
        newname = f"COMB_{i}_INT_{j}"
        pred_dataALLCOMBS2[newname] = col_i * col_j
        

filtered_words = [col for col in pred_dataALLCOMBS2.columns if re.search(r'^COMB_.*_INT_Guard_reg_.*', col)]
filtered_words2 = [col for col in pred_dataALLCOMBS2.columns if re.search(r'^COMB_.*_INT_Forward_reg_.*', col)]
filtered_words3 = [col for col in pred_dataALLCOMBS2.columns if re.search(r'^COMB_.*_INT_Center_reg_.*', col)]
pred_dataALLCOMBS2 = pred_dataALLCOMBS2.drop(filtered_words, axis=1)
pred_dataALLCOMBS2 = pred_dataALLCOMBS2.drop(filtered_words2, axis=1)
pred_dataALLCOMBS2 = pred_dataALLCOMBS2.drop(filtered_words3, axis=1)

with open('pred_dataALLCOMBS2.pkd', 'wb') as f:
    dill.dump(pred_dataALLCOMBS2, f)
    
pred_dataALLCOMBS = pred_dataALLCOMBS2.copy()

##IND ONLY
X = regdata_FINAL[ind_variable_names]
X_pred = pred_dataACTUAL[ind_variable_names]
y = regdata_FINAL['Y_PlusMin2']
ridge_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('ridge', Ridge())
])
alphas = [10000]
#kfold = KFold(n_splits=5, shuffle=True, random_state=42)
grid_search = GridSearchCV(estimator=ridge_pipeline, param_grid={'ridge__alpha': alphas}, cv=5, scoring='neg_mean_squared_error')
grid_result1 = grid_search.fit(X, y)
best_ridge_model1 = grid_result1.best_estimator_
best_alpha1 = grid_result1.best_params_
coef_names1 = X.columns
coef_values1 = best_ridge_model1[1].coef_
coef_estimates1 = pd.DataFrame({'Coefficient': coef_values1}, index=coef_names1)
X_pred = pred_dataACTUAL[ind_variable_names]
predictions1 = best_ridge_model1.predict(X_pred)
MIN = pred_dataACTUAL['SP_MIN_5y']
MIN2 = pred_dataACTUAL['SP_MIN_1y']
Player = pred_dataACTUAL['ID_PlayersFull']
PlayerTeam = pred_dataACTUAL['ID_team']
Team = pred_dataACTUAL['SP_Team']
pred_estimates1 = pd.DataFrame({'Est_PM': predictions1,'MIN': MIN.values ,'MIN2': MIN2.values,'Player': Player.values,'PlayerTeam': PlayerTeam.values,'Team': Team.values}, index=pred_dataACTUAL['ID_PlayersFull'])
mean_est_pm_by_team = pred_estimates1.groupby('Team')['Est_PM'].median()
sorted_est_pm = mean_est_pm_by_team.sort_values(ascending=False)
centered_est_pm1 = sorted_est_pm - sorted_est_pm.mean()
mean_est_pm_by_player = pred_estimates1.groupby('Player')['Est_PM'].median()
sorted_est_player = mean_est_pm_by_player.sort_values(ascending=False)
centered_est_player1 = sorted_est_player - sorted_est_player.median()
best_players1 = pd.DataFrame({'Est_PM': centered_est_player1})
best_teams1 = pd.DataFrame({'Est_PM': centered_est_pm1})

##IND AND TEAM ONLY
X = regdata_FINAL[indteam_variable_names]
X_pred = pred_dataACTUAL[indteam_variable_names]
y = regdata_FINAL['Y_PlusMin2']
ridge_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('ridge', Ridge())
])
alphas = [10000,20000]
#kfold = KFold(n_splits=5, shuffle=True, random_state=42)
grid_search = GridSearchCV(estimator=ridge_pipeline, param_grid={'ridge__alpha': alphas}, cv=5, scoring='neg_mean_squared_error')
grid_result2 = grid_search.fit(X, y)
best_ridge_model2 = grid_result2.best_estimator_
best_alpha2 = grid_result2.best_params_
coef_names2 = X.columns
coef_values2 = best_ridge_model2[1].coef_
coef_estimates2 = pd.DataFrame({'Coefficient': coef_values2}, index=coef_names2)
X_pred = pred_dataALLCOMBS[indteam_variable_names]
predictions2 = best_ridge_model2.predict(X_pred)
MIN = pred_dataALLCOMBS['SP_MIN_5y']
MIN2 = pred_dataALLCOMBS['SP_MIN_1y']
Player = pred_dataALLCOMBS['ID_PlayersFull']
PlayerTeam = pred_dataALLCOMBS['ID_team']
Team = pred_dataALLCOMBS['SP_Team']
pred_estimates2 = pd.DataFrame({'Est_PM': predictions2,'MIN': MIN.values ,'MIN2': MIN2.values,'Player': Player.values,'PlayerTeam': PlayerTeam.values,'Team': Team.values}, index=pred_dataALLCOMBS['ID_PlayersFull'])
mean_est_pm_by_team = pred_estimates2.groupby('Team')['Est_PM'].median()
sorted_est_pm = mean_est_pm_by_team.sort_values(ascending=False)
centered_est_pm2 = sorted_est_pm - sorted_est_pm.mean()
mean_est_pm_by_player = pred_estimates2.groupby('Player')['Est_PM'].median()
sorted_est_player = mean_est_pm_by_player.sort_values(ascending=False)
centered_est_player2 = sorted_est_player - sorted_est_player.median()
best_players2 = pd.DataFrame({'Est_PM': centered_est_player2})
best_teams2 = pd.DataFrame({'Est_PM': centered_est_pm2})


with open('best_players1.pkd', 'wb') as f:
    dill.dump(best_players1, f)
with open('best_teams1.pkd', 'wb') as f:
    dill.dump(best_teams1, f)
with open('pred_estimates1.pkd', 'wb') as f:
    dill.dump(pred_estimates1, f)
with open('coef_estimates1.pkd', 'wb') as f:
    dill.dump(coef_estimates1, f)
with open('best_alpha1.pkd', 'wb') as f:
    dill.dump(best_alpha1, f)
with open('coef_estimates1.pkd', 'wb') as f:
    dill.dump(coef_estimates1, f)
with open('best_ridge_model1.pkd', 'wb') as f:
    dill.dump(best_ridge_model1, f)
# with open('best_players1.pkd', 'rb') as f:
#     best_players1.pkd = dill.load(f)
# with open('best_teams1.pkd', 'rb') as f:
#     best_teams1.pkd = dill.load(f)
# with open('pred_estimates1.pkd', 'rb') as f:
#     pred_estimates1.pkd = dill.load(f)
# with open('coef_estimates1.pkd', 'rb') as f:
#     coef_estimates1.pkd = dill.load(f)
# with open('best_alpha1.pkd', 'rb') as f:
#     best_alpha1.pkd = dill.load(f)
# with open('coef_estimates1.pkd', 'rb') as f:
#     coef_estimates1.pkd = dill.load(f)
# with open('best_ridge_model1.pkd', 'rb') as f:
#     best_ridge_model1.pkd = dill.load(f)
    
#save model2
with open('best_players2.pkd', 'wb') as f:
    dill.dump(best_players2, f)
with open('best_teams2.pkd', 'wb') as f:
    dill.dump(best_teams2, f)
with open('pred_estimates2.pkd', 'wb') as f:
    dill.dump(pred_estimates2, f)
with open('coef_estimates2.pkd', 'wb') as f:
    dill.dump(coef_estimates2, f)
with open('best_alpha2.pkd', 'wb') as f:
    dill.dump(best_alpha2, f)
with open('coef_estimates2.pkd', 'wb') as f:
    dill.dump(coef_estimates2, f)
with open('best_ridge_model2.pkd', 'wb') as f:
    dill.dump(best_ridge_model2, f)
    
# with open('best_players2.pkd', 'rb') as f:
#     best_players2.pkd = dill.load(f)
# with open('best_teams2.pkd', 'rb') as f:
#     best_teams2.pkd = dill.load(f)
# with open('pred_estimates2.pkd', 'rb') as f:
#     pred_estimates2.pkd = dill.load(f)
# with open('coef_estimates2.pkd', 'rb') as f:
#     coef_estimates2.pkd = dill.load(f)
# with open('best_alpha2.pkd', 'rb') as f:
#     best_alpha2.pkd = dill.load(f)
# with open('coef_estimates2.pkd', 'rb') as f:
#     coef_estimates2.pkd = dill.load(f)
# with open('best_ridge_model2.pkd', 'rb') as f:
#     best_ridge_model2.pkd = dill.load(f)


# %reset_selective -f alphas best_alpha1 best_alpha2 best_players best_players1 best_players2 best_ridge_model1 best_ridgemodel2 mask ridge_pipeline

##ALL COMBS
X = regdata_FINAL[all_variable_names]
X_pred = pred_dataALLCOMBS[all_variable_names]
y = regdata_FINAL['Y_PlusMin2']
# %reset_selective -f regdata_FINAL
ridge_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('ridge', Ridge())
])
alphas = [100000]
#kfold = KFold(n_splits=5, shuffle=True, random_state=42)
grid_search = GridSearchCV(estimator=ridge_pipeline, param_grid={'ridge__alpha': alphas}, cv=5,n_jobs = 3, scoring='neg_mean_squared_error')
grid_result3 = grid_search.fit(X, y)
best_ridge_model3 = grid_result3.best_estimator_
best_alpha3 = grid_result3.best_params_
coef_names3 = X.columns
coef_values3 = best_ridge_model3[1].coef_
coef_estimates3 = pd.DataFrame({'Coefficient': coef_values3}, index=coef_names3)
X_pred = pred_dataALLCOMBS[all_variable_names]
predictions3 = best_ridge_model3.predict(X_pred)
MIN = pred_dataALLCOMBS['SP_MIN_5y']
MIN2 = pred_dataALLCOMBS['SP_MIN_1y']
Player = pred_dataALLCOMBS['ID_PlayersFull']
PlayerTeam = pred_dataALLCOMBS['ID_team']
Team = pred_dataALLCOMBS['SP_Team']
pred_estimates3 = pd.DataFrame({'Est_PM': predictions3,'MIN': MIN.values ,'MIN2': MIN2.values,'Player': Player.values,'PlayerTeam': PlayerTeam.values,'Team': Team.values}, index=pred_dataALLCOMBS['ID_PlayersFull'])
mean_est_pm_by_team = pred_estimates3.groupby('Team')['Est_PM'].median()
sorted_est_pm = mean_est_pm_by_team.sort_values(ascending=False)
centered_est_pm3 = sorted_est_pm - sorted_est_pm.mean()
mean_est_pm_by_player = pred_estimates3.groupby('Player')['Est_PM'].median()
sorted_est_player = mean_est_pm_by_player.sort_values(ascending=False)
centered_est_player3 = sorted_est_player - sorted_est_player.median()
best_players3 = pd.DataFrame({'Est_PM': centered_est_player3})
best_teams3 = pd.DataFrame({'Est_PM': centered_est_pm3})

#save model3
with open('best_players3.pkd', 'wb') as f:
    dill.dump(best_players3, f)
with open('best_teams3.pkd', 'wb') as f:
    dill.dump(best_teams3, f)
with open('pred_estimates3.pkd', 'wb') as f:
    dill.dump(pred_estimates3, f)
with open('coef_estimates3.pkd', 'wb') as f:
    dill.dump(coef_estimates3, f)
with open('best_alpha3.pkd', 'wb') as f:
    dill.dump(best_alpha3, f)
with open('coef_estimates3.pkd', 'wb') as f:
    dill.dump(coef_estimates3, f)
with open('best_ridge_model3.pkd', 'wb') as f:
    dill.dump(best_ridge_model3, f)
    
# with open('best_players3.pkd', 'rb') as f:
#     best_players3.pkd = dill.load(f)
# with open('best_teams3.pkd', 'rb') as f:
#     best_teams3.pkd = dill.load(f)
# with open('pred_estimates3.pkd', 'rb') as f:
#     pred_estimates3.pkd = dill.load(f)
# with open('coef_estimates3.pkd', 'rb') as f:
#     coef_estimates3.pkd = dill.load(f)
# with open('best_alpha3.pkd', 'rb') as f:
#     best_alpha3.pkd = dill.load(f)
# with open('coef_estimates3.pkd', 'rb') as f:
#     coef_estimates3.pkd = dill.load(f)
# with open('best_ridge_model3.pkd', 'rb') as f:
#     best_ridge_model3.pkd = dill.load(f)

##K-Means
X = regdata_FINAL[ind_variable_names]
#X_pred = pred_dataALLCOMBS[all_variable_names]
y = regdata_FINAL['Y_PlusMin2']
# %reset_selective -f regdata_FINAL\
from sklearn.cluster import KMeans
kmeans_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('kmeans', KMeans(n_clusters=10,random_state=42))
])
alphas = [5,10,15]
#kfold = KFold(n_splits=5, shuffle=True, random_state=42)
#grid_search = GridSearchCV(estimator=kmeans_pipeline, param_grid={'kmeans__n_clusters': alphas}, cv=5,n_jobs = 3, scoring='neg_mean_squared_error')
grid_result4 = kmeans_pipeline.fit(X)

df_cluster_centers = pd.DataFrame(grid_result4[-1].cluster_centers_.transpose(),index = coef_names1)
df_cluster_centers.columns = ['Cluster 1', 'Cluster 2', ..., 'Cluster K']


##PCA
X = regdata_FINAL[ind_variable_names]
#X_pred = pred_dataALLCOMBS[all_variable_names]
y = regdata_FINAL['Y_PlusMin2']
# %reset_selective -f regdata_FINAL\
from sklearn.decomposition import PCA
pca_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('PCA', PCA(n_components = 10))
])
grid_result5 = pca_pipeline.fit(X)
components = pd.DataFrame(grid_result5[-1].components_.transpose(), columns=['Feature1', 'Feature2', 'Feature3', 'Feature4', 'Feature5', 'Feature6', 'Feature7', 'Feature8', 'Feature9', 'Feature10'],index = coef_names1)
grid_result5[-1].explained_variance_

components2 = pd.DataFrame(grid_result5[-1].components_, columns=coef_names1,index=['Feature1', 'Feature2', 'Feature3', 'Feature4', 'Feature5', 'Feature6', 'Feature7', 'Feature8', 'Feature9', 'Feature10'])

grid_result5[-1].explained_variance_ratio_


X_pca = pd.DataFrame(pca_pipeline.transform(pred_dataACTUAL[ind_variable_names]),columns=['Feature1', 'Feature2', 'Feature3', 'Feature4', 'Feature5', 'Feature6', 'Feature7', 'Feature8', 'Feature9', 'Feature10'])
X_pca2 = pd.DataFrame(grid_result5.transform(pred_dataACTUAL[ind_variable_names]),columns=['Feature1', 'Feature2', 'Feature3', 'Feature4', 'Feature5', 'Feature6', 'Feature7', 'Feature8', 'Feature9', 'Feature10'])

pred_dataACTUAL2 = pd.concat([pred_dataACTUAL,X_pca], axis=1)
pred_dataACTUAL2['Shooting'] = pred_dataACTUAL2['Feature1'] - pred_dataACTUAL2['Feature5'] + pred_dataACTUAL2['Feature9']
pred_dataACTUAL2['Experience'] = -1*pred_dataACTUAL2['Feature3'] + pred_dataACTUAL2['Feature4'] - pred_dataACTUAL2['Feature6'] + pred_dataACTUAL2['Feature8']
pred_dataACTUAL2['Rebounding']= -1*pred_dataACTUAL2['Feature1'] + pred_dataACTUAL2['Feature5'] + pred_dataACTUAL2['Feature9'] + pred_dataACTUAL2['Feature2']
pred_dataACTUAL2['Blocking']= pred_dataACTUAL2['Feature2'] - pred_dataACTUAL2['Feature1']
pred_dataACTUAL2['Passing']= pred_dataACTUAL2['Feature7'] + pred_dataACTUAL2['Feature1'] - pred_dataACTUAL2['Feature10']
pred_dataACTUAL2['Stealing'] = pred_dataACTUAL2['Feature7'] + pred_dataACTUAL2['Feature6']
pred_dataACTUAL2['FreeThrowGeneration'] = pred_dataACTUAL2['Feature10'] + pred_dataACTUAL2['Feature4'] - pred_dataACTUAL2['Feature3']
pred_dataACTUAL2['ScoringEfficiency'] = pred_dataACTUAL2['Feature10'] - pred_dataACTUAL2['Feature7'] - pred_dataACTUAL2['Feature3'] - pred_dataACTUAL2['Feature5']
pred_dataACTUAL2['OnOffImpact'] = -1*pred_dataACTUAL2['Feature3'] - pred_dataACTUAL2['Feature4'] + pred_dataACTUAL2['Feature6'] + pred_dataACTUAL2['Feature5']
pred_dataACTUAL2['RawOffense'] = pred_dataACTUAL2['Feature6'] + pred_dataACTUAL2['Feature8'] - pred_dataACTUAL2['Feature3']
pred_dataACTUAL2['RawDefense'] = pred_dataACTUAL2['Feature7'] + pred_dataACTUAL2['Feature6'] - pred_dataACTUAL2['Feature4']
pred_dataACTUAL2['Scoring'] = -1*pred_dataACTUAL2['Feature3'] + pred_dataACTUAL2['Feature4'] + pred_dataACTUAL2['Feature9'] + pred_dataACTUAL2['Feature10'] - pred_dataACTUAL2['Feature5']


for j in unique_teams:
    team_mask = pred_dataACTUAL['SP_Team'] == j
    # Calculate the sumproduct for each column in team_list1
    pred_team['Shooting'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Shooting'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Experience'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Experience'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Rebounding'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Rebounding'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Blocking'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Blocking'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Passing'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Passing'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Stealing'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Stealing'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['FreeThrowGeneration'][pred_team['SP_Team'] == j]= numpy.sum(pred_dataACTUAL2['FreeThrowGeneration'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['ScoringEfficiency'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['ScoringEfficiency'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['OnOffImpact'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['OnOffImpact'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['RawOffense'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['RawOffense'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['RawDefense'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['RawDefense'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
    pred_team['Scoring'][pred_team['SP_Team'] == j] = numpy.sum(pred_dataACTUAL2['Scoring'][team_mask] * pred_dataACTUAL2['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUAL2['IND_MINPG_reg'][team_mask])
  

numerical_cols = pred_team.select_dtypes(include=['int64', 'float64']).columns
string_cols = pred_team.select_dtypes(include=['object']).columns
pred_team[numerical_cols] = StandardScaler().fit_transform(pred_team[numerical_cols])
pred_team2 = pd.concat([pred_team[numerical_cols], pred_team[string_cols]], axis=1)



#savefeatures
with open('pred_team2.pkd', 'wb') as f:
    dill.dump(pred_team2, f)
with open('pred_dataACTUAL2.pkd', 'wb') as f:
    dill.dump(pred_dataACTUAL2, f)
with open('pca_pipeline.pkd', 'wb') as f:
    dill.dump(pca_pipeline, f)
with open('grid_result5.pkd', 'wb') as f:
    dill.dump(grid_result5, f)

# with open('best_teams3.pkd', 'rb') as f:
#     best_teams3.pkd = dill.load(f)
# with open('pred_estimates3.pkd', 'rb') as f:
#     pred_estimates3.pkd = dill.load(f)
# with open('coef_estimates3.pkd', 'rb') as f:
#     coef_estimates3.pkd = dill.load(f)
# with open('best_alpha3.pkd', 'rb') as f:
#     best_alpha3.pkd = dill.load(f)
# with open('coef_estimates3.pkd', 'rb') as f:
#     coef_estimates3.pkd = dill.load(f)
# with open('best_ridge_model3.pkd', 'rb') as f:
#     best_ridge_model3.pkd = dill.load(f)

