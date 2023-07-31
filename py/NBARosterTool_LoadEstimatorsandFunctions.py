#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 25 15:57:13 2023

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
import statistics
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
import warnings
import matplotlib.pyplot as plt
import re
import requests
warnings.filterwarnings("ignore")

os.getcwd()
#INPUT WORKING DIRECTORY HERE
#os.chdir("/Users/ToGithubRepo/NBADATAProject/py/")



file_links = ["pred_dataACTUAL.feather","pred_dataALLCOMBS.feather", "pred_team.feather","pred_dataALLCOMBS2.pkd","pred_dataACTUAL2.pkd"]

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
    globals()[file_name] = data
    print(f"Loaded data from {file_link}:")

with open('best_players1.pkd', 'rb') as f:
    best_players1 = dill.load(f)
with open('best_teams1.pkd', 'rb') as f:
    best_teams1 = dill.load(f)
with open('pred_estimates1.pkd', 'rb') as f:
    pred_estimates1 = dill.load(f)
with open('coef_estimates1.pkd', 'rb') as f:
    coef_estimates1 = dill.load(f)
with open('best_alpha1.pkd', 'rb') as f:
    best_alpha1 = dill.load(f)
with open('coef_estimates1.pkd', 'rb') as f:
    coef_estimates1 = dill.load(f)
with open('best_ridge_model1.pkd', 'rb') as f:
    best_ridge_model1 = dill.load(f)

with open('best_players2.pkd', 'rb') as f:
    best_players2 = dill.load(f)
with open('best_teams2.pkd', 'rb') as f:
    best_teams2 = dill.load(f)
with open('pred_estimates2.pkd', 'rb') as f:
    pred_estimates2 = dill.load(f)
with open('coef_estimates2.pkd', 'rb') as f:
    coef_estimates2 = dill.load(f)
with open('best_alpha2.pkd', 'rb') as f:
    best_alpha2 = dill.load(f)
with open('coef_estimates2.pkd', 'rb') as f:
    coef_estimates2 = dill.load(f)
with open('best_ridge_model2.pkd', 'rb') as f:
    best_ridge_model2 = dill.load(f)

with open('best_players3.pkd', 'rb') as f:
    best_players3 = dill.load(f)
with open('best_teams3.pkd', 'rb') as f:
    best_teams3 = dill.load(f)
with open('pred_estimates3.pkd', 'rb') as f:
    pred_estimates3 = dill.load(f)
with open('coef_estimates3.pkd', 'rb') as f:
    coef_estimates3 = dill.load(f)
with open('best_alpha3.pkd', 'rb') as f:
    best_alpha3 = dill.load(f)
with open('coef_estimates3.pkd', 'rb') as f:
    coef_estimates3 = dill.load(f)
with open('best_ridge_model3.pkd', 'rb') as f:
    best_ridge_model3 = dill.load(f)
    
with open('pred_team2.pkd', 'rb') as f:
    pred_team2 = dill.load(f)
# with open('pred_dataACTUAL2.pkd', 'rb') as f:
#     pred_dataACTUAL2 = dill.load(f)
# with open('pred_dataALLCOMBS2.pkd', 'rb') as f:
#     pred_dataALLCOMBS2 = dill.load(f)
with open('pca_pipeline.pkd', 'rb') as f:
    pca_pipeline = dill.load(f)
with open('grid_result5.pkd', 'rb') as f:
    grid_result5 = dill.load(f)
with open('pred_teamFINAL.pkd', 'rb') as f:
    pred_teamFINAL = dill.load(f)
    
# pred_dataACTUAL = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_dataACTUAL.feather")
# pred_dataALLCOMBS = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_dataALLCOMBS.feather")
# pred_team = pd.read_feather("/Users/ricardovelloso/Dropbox/Projects/NBAGambling/pred_team.feather")
numeric_columns = pred_team.select_dtypes(include='number')
string_columns = pred_team.select_dtypes(include='object')
scaler = StandardScaler()
numeric_columns_standardized = pd.DataFrame(scaler.fit_transform(numeric_columns), columns=numeric_columns.columns)
df_standardized = pd.concat([string_columns, numeric_columns_standardized], axis=1)

ind_variable_names = [col for col in pred_dataACTUAL.columns if col.startswith("IND_")]
team_variable_names = [col for col in pred_dataACTUAL.columns if col.startswith("TEAM_")]
comb_variable_names = [col for col in pred_dataACTUAL.columns if col.startswith("COMB_")]
indteam_variable_names = [col for col in pred_dataACTUAL.columns if col.startswith("IND_") or col.startswith("TEAM_")]
all_variable_names = [col for col in pred_dataACTUAL.columns if col.startswith("IND_") or col.startswith("TEAM_") or col.startswith("COMB_")]


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


#Team = 'Washington Wizards'
#CustomTeam = []

def summarizeteam(Team, CustomTeam):
    if (len(Team)>0):
        players = pred_dataACTUAL2.loc[pred_dataACTUAL2['SP_Team']==Team,['ID_PlayersFull']]
        players = players.values.tolist()
        players = [item for sublist in players for item in sublist]
    else:
        players = CustomTeam
    display = pred_dataACTUAL2.loc[pred_dataACTUAL2['ID_PlayersFull'].isin(players),['ID_PlayersFull','SP_PositionDet','IND_MINPG_reg','SP_RPMScore','IND_PTSpm_reg']]    
    display = display.sort_values(by=['IND_MINPG_reg'],ascending=False)
    display['SP_RPMScore'] = round(display['SP_RPMScore'],1)
    display['IND_MINPG_reg'] = round(display['IND_MINPG_reg'],1)
    display['IND_PTSpm_reg'] = round(display['IND_PTSpm_reg'],1)
    return display.head(len(display))

def findstrandweak(Team, CustomTeam):
    if (len(Team)>0):
        players = pred_dataACTUAL2.loc[pred_dataACTUAL2['SP_Team']==Team,['ID_PlayersFull']]
        players = players.values.tolist()
        players = [item for sublist in players for item in sublist]
    else:
        players = CustomTeam
    pred_customteam = pred_dataACTUAL.loc[pred_dataACTUAL['ID_PlayersFull'].isin(players),:].copy()
    pred_customteam['SP_Team'] = "Custom"
    pred_customteam = pd.concat([pred_dataACTUAL.copy(),pred_customteam], axis=0).reset_index()
    pred_customteam = alterdata(pred_customteam)
    returnobj = pred_customteam.copy()
    X_pca = pd.DataFrame(grid_result5.transform(pred_customteam[ind_variable_names]),columns=['Feature1', 'Feature2', 'Feature3', 'Feature4', 'Feature5', 'Feature6', 'Feature7', 'Feature8', 'Feature9', 'Feature10'])
    pred_dataACTUALtemp = pd.concat([pred_customteam,X_pca], axis=1)
    pred_dataACTUALtemp['Shooting'] = pred_dataACTUALtemp['Feature1'] - pred_dataACTUALtemp['Feature5'] + pred_dataACTUALtemp['Feature9']
    pred_dataACTUALtemp['Experience'] = -1*pred_dataACTUALtemp['Feature3'] + pred_dataACTUALtemp['Feature4'] - pred_dataACTUALtemp['Feature6'] + pred_dataACTUALtemp['Feature8']
    pred_dataACTUALtemp['Rebounding']= -1*pred_dataACTUALtemp['Feature1'] + pred_dataACTUALtemp['Feature5'] + pred_dataACTUALtemp['Feature9'] + pred_dataACTUALtemp['Feature2']
    pred_dataACTUALtemp['Blocking']= pred_dataACTUALtemp['Feature2'] - pred_dataACTUALtemp['Feature1']
    pred_dataACTUALtemp['Passing']= pred_dataACTUALtemp['Feature7'] + pred_dataACTUALtemp['Feature1'] - pred_dataACTUALtemp['Feature10']
    pred_dataACTUALtemp['Stealing'] = pred_dataACTUALtemp['Feature7'] + pred_dataACTUALtemp['Feature6']
    pred_dataACTUALtemp['FreeThrowGeneration'] = pred_dataACTUALtemp['Feature10'] + pred_dataACTUALtemp['Feature4'] - pred_dataACTUALtemp['Feature3']
    pred_dataACTUALtemp['ScoringEfficiency'] = pred_dataACTUALtemp['Feature10'] - pred_dataACTUALtemp['Feature7'] - pred_dataACTUALtemp['Feature3'] - pred_dataACTUALtemp['Feature5']
    pred_dataACTUALtemp['OnOffImpact'] = -1*pred_dataACTUALtemp['Feature3'] - pred_dataACTUALtemp['Feature4'] + pred_dataACTUALtemp['Feature6'] + pred_dataACTUALtemp['Feature5']
    pred_dataACTUALtemp['RawOffense'] = pred_dataACTUALtemp['Feature6'] + pred_dataACTUALtemp['Feature8'] - pred_dataACTUALtemp['Feature3']
    pred_dataACTUALtemp['RawDefense'] = pred_dataACTUALtemp['Feature7'] + pred_dataACTUALtemp['Feature6'] - pred_dataACTUALtemp['Feature4']
    pred_dataACTUALtemp['Scoring'] = -1*pred_dataACTUALtemp['Feature3'] + pred_dataACTUALtemp['Feature4'] + pred_dataACTUALtemp['Feature9'] + pred_dataACTUALtemp['Feature10'] - pred_dataACTUALtemp['Feature5']
    unique_teams = pred_dataACTUALtemp['SP_Team'].unique()
    pred_teamtemp = pred_team.loc[[0]]
    pred_teamtemp['SP_Team'] = "Custom"
    pred_teamtemp = pd.concat([pred_team,pred_teamtemp], axis=0,ignore_index=True)
    pred_teamtemp['Shooting'] = 0
    pred_teamtemp['Experience'] = 0
    pred_teamtemp['Rebounding'] = 0
    pred_teamtemp['Blocking'] = 0
    pred_teamtemp['Passing'] = 0
    pred_teamtemp['Stealing'] = 0
    pred_teamtemp['FreeThrowGeneration'] = 0
    pred_teamtemp['ScoringEfficiency'] = 0
    pred_teamtemp['OnOffImpact'] = 0
    pred_teamtemp['RawOffense'] = 0
    pred_teamtemp['RawDefense'] = 0
    pred_teamtemp['Scoring'] = 0
    for j in unique_teams:
        team_mask = pred_dataACTUALtemp['SP_Team'] == j
        # Calculate the sumproduct for each column in team_list1
        pred_teamtemp['Shooting'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Shooting'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Experience'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Experience'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Rebounding'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Rebounding'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Blocking'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Blocking'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Passing'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Passing'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Stealing'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Stealing'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['FreeThrowGeneration'][pred_teamtemp['SP_Team'] == j]= numpy.sum(pred_dataACTUALtemp['FreeThrowGeneration'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['ScoringEfficiency'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['ScoringEfficiency'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['OnOffImpact'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['OnOffImpact'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['RawOffense'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['RawOffense'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['RawDefense'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['RawDefense'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
        pred_teamtemp['Scoring'][pred_teamtemp['SP_Team'] == j] = numpy.sum(pred_dataACTUALtemp['Scoring'][team_mask] * pred_dataACTUALtemp['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_dataACTUALtemp['IND_MINPG_reg'][team_mask])
      
        
    numerical_cols = pred_teamtemp.select_dtypes(include=['int64', 'float64']).columns
    string_cols = pred_teamtemp.select_dtypes(include=['object']).columns
    pred_teamtemp[numerical_cols] = StandardScaler().fit_transform(pred_teamtemp[numerical_cols])
    pred_teamtemp2 = pd.concat([pred_teamtemp[numerical_cols], pred_teamtemp[string_cols]], axis=1)
    show_columns = ['Shooting',
    'Experience',
    'Rebounding',
    'Blocking',
    'Passing',
    'Stealing',
    'FreeThrowGeneration',
    'ScoringEfficiency',
    'OnOffImpact',
    'RawOffense',
    'RawDefense',
    'Scoring']
    weaknesses = ""
    strengths = ""
    for i in show_columns:
        if pred_teamtemp2.loc[30,i]>0.9:
            strengths = i + ", " + strengths
        if pred_teamtemp2.loc[30,i]<(-0.9):
            weaknesses = i + ", " + weaknesses
    strong = f"Strengths : {strengths}"
    weak = f"Weaknesses : {weaknesses}"
    strong = strong.rstrip(strong[-1])
    strong = strong.rstrip(strong[-1])
    weak = weak.rstrip(weak[-1])
    weak = weak.rstrip(weak[-1])
    print(strong)
    print(weak)
    show_columns.append("SP_Team")
    chartdata = pred_teamtemp2[show_columns]
    chartdata = chartdata.drop(columns=['SP_Team'])
    min_values = chartdata.min()
    max_values = chartdata.max()

    # Get the 'Custom' row for SP_Team
    chartdata = chartdata[chartdata.iloc[30].sort_values(ascending=False).index]
    custom_data = chartdata.loc[chartdata.index == 30]
    
    plt.figure(figsize=(12, 6))
    plt.bar(chartdata.columns, min_values, color='red', alpha=0.5, label='Min. Score among All Current Teams')
    plt.bar(chartdata.columns, max_values, color='blue', alpha=0.5, label='Max. Score among All Current Teams')
    plt.scatter(chartdata.columns, custom_data.values[0], color='green', label='Your Team\'s Score', zorder=5)
    plt.xlabel('Features Captured by Unsupervised Clustering')
    plt.ylabel('Normalized Score (Z-Score)')
    plt.title('How Custom Team Compares to Other Teams')
    plt.legend()
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()
    returnobj = returnobj
    return None

#summarizeteam(Team, CustomTeam)
#pred_teamcustom = findstrandweak(Team, CustomTeam)

def bestplayerfits(Team, CustomTeam):
    if (len(Team)>0):
        players = pred_dataACTUAL2.loc[pred_dataACTUAL2['SP_Team']==Team,['ID_PlayersFull']]
        players = players.values.tolist()
        players = [item for sublist in players for item in sublist]
    else:
        players = CustomTeam
    pred_customteam = pred_dataACTUAL.loc[pred_dataACTUAL['ID_PlayersFull'].isin(players),:].copy()
    pred_customteam['SP_Team'] = "Custom"
    pred_customteam = pd.concat([pred_dataACTUAL.copy(),pred_customteam], axis=0,ignore_index=True).reset_index()
    pred_customteam = pred_customteam.drop(columns=['index'])
    pred_customteam = alterdata(pred_customteam)
    team_list1 = team_variable_names
    unique_teams2 = ['Custom']
    pred_teamtemp = pred_teamFINAL.loc[[0]].copy()
    pred_teamtemp['SP_Team'] = "Custom"
    pred_teamtemp = pd.concat([pred_teamFINAL.copy(),pred_teamtemp], axis=0,ignore_index=True)
    for j in unique_teams2:
        team_mask = pred_customteam['SP_Team'] == j
        for i in team_list1:
            sumproduct_result = numpy.sum(pred_customteam[i][team_mask] * pred_customteam['IND_MINPG_reg'][team_mask]) / numpy.sum(pred_customteam['IND_MINPG_reg'][team_mask])
            #print(f"{i}:{sumproduct_result}")
            pred_teamtemp.loc[pred_teamtemp['SP_Team'] == j, i] = sumproduct_result
    pred_dataALLCOMBStoadd = pred_customteam.copy()
    pred_dataALLCOMBStoadd['SP_Team'] = "Custom"
    pred_dataALLCOMBStoadd = pd.merge(pred_dataALLCOMBStoadd.drop(columns=team_variable_names), pred_teamtemp, on="SP_Team", how="left")
    # Loop through columns that start with 'IND_'
    for i, col_i in pred_dataALLCOMBStoadd.filter(regex=r'^IND_').iteritems():
        # Check if the column starts with 'IND_SeasD'
        if i.startswith('IND_SeasD'):
            continue
        for j, col_j in pred_dataALLCOMBStoadd.filter(regex=r'^TEAM_').iteritems():
            # Create the new interaction term column name
            newname = f"COMB_{i}_INT_{j}"
            pred_dataALLCOMBStoadd[newname] = col_i * col_j

    
    filtered_words = [col for col in pred_dataALLCOMBStoadd.columns if re.search(r'^COMB_.*_INT_Guard_reg_.*', col)]
    filtered_words2 = [col for col in pred_dataALLCOMBStoadd.columns if re.search(r'^COMB_.*_INT_Forward_reg_.*', col)]
    filtered_words3 = [col for col in pred_dataALLCOMBStoadd.columns if re.search(r'^COMB_.*_INT_Center_reg_.*', col)]
    pred_dataALLCOMBStoadd = pred_dataALLCOMBStoadd.drop(filtered_words, axis=1)
    pred_dataALLCOMBStoadd = pred_dataALLCOMBStoadd.drop(filtered_words2, axis=1)
    pred_dataALLCOMBStoadd = pred_dataALLCOMBStoadd.drop(filtered_words3, axis=1)
    
    pred_dataALLCOMBS2['SP_PositionDet'] = numpy.where(pred_dataALLCOMBS2['IND_PG_reg'] == 1, 'PG', 'G')
    pred_dataALLCOMBS2['SP_PositionDet'] = numpy.where(pred_dataALLCOMBS2['IND_SG_reg'] == 1, 'SG', pred_dataALLCOMBS2['SP_PositionDet'])
    pred_dataALLCOMBS2['SP_PositionDet'] = numpy.where(pred_dataALLCOMBS2['IND_SF_reg'] == 1, 'SF', pred_dataALLCOMBS2['SP_PositionDet'])
    pred_dataALLCOMBS2['SP_PositionDet'] = numpy.where(pred_dataALLCOMBS2['IND_PF_reg'] == 1, 'PF', pred_dataALLCOMBS2['SP_PositionDet'])
    pred_dataALLCOMBS2['SP_PositionDet'] = numpy.where(pred_dataALLCOMBS2['IND_C_reg'] == 1, 'C', pred_dataALLCOMBS2['SP_PositionDet'])
    pred_dataALLCOMBS2['SP_prod_c_d'] = 0
    
    pred_dataALLCOMBStemp = pd.concat([pred_dataALLCOMBS2,pred_dataALLCOMBStoadd],axis = 0,ignore_index=True)
    
    X_pred = pred_dataALLCOMBStemp[all_variable_names]
    predictions4 = best_ridge_model3.predict(X_pred)
    MIN = pred_dataALLCOMBStemp['SP_MIN_5y']
    MIN2 = pred_dataALLCOMBStemp['SP_MIN_1y']
    Player = pred_dataALLCOMBStemp['ID_PlayersFull']
    PlayerTeam = pred_dataALLCOMBStemp['ID_team']
    TeamName = pred_dataALLCOMBStemp['SP_Team']
    pred_estimates4 = pd.DataFrame({'Est_PM': predictions4,'MIN': MIN.values ,'MIN2': MIN2.values,'Player': Player.values,'PlayerTeam': PlayerTeam.values,'TeamName': TeamName.values}, index=pred_dataALLCOMBStemp['ID_PlayersFull'])
    mean_est_pm_by_team = pred_estimates4.groupby('TeamName')['Est_PM'].median()
    sorted_est_pm = mean_est_pm_by_team.sort_values(ascending=False)
    centered_est_pm4 = sorted_est_pm - sorted_est_pm.mean()
    mean_est_pm_by_player = pred_estimates4.groupby('Player')['Est_PM'].median()
    sorted_est_player = mean_est_pm_by_player.sort_values(ascending=False)
    centered_est_player4 = sorted_est_player - sorted_est_player.median()
    best_players4 = pd.DataFrame({'Est_PM': centered_est_player4})
    best_teams4 = pd.DataFrame({'Est_PM': centered_est_pm4})
    
    pred_estimates4['ID_PlayersFull'] = pred_estimates4.index
    pred_estimates4['PlayerTeam2'] = pred_estimates4['PlayerTeam'].str.split('_', 1).str[1]
    pred_estimates4['PlayerID'] = pred_estimates4['PlayerTeam'].str.split('_', 1).str[0]
    pred_estimates4['StDev'] = statistics.stdev(pred_estimates4['Est_PM'])
    pred_estimates4['Mean_Est_PM_player'] = pred_estimates4.groupby('PlayerID')['Est_PM'].transform('median')
    pred_estimates4['Mean_Est_PM_ALL'] = statistics.median(pred_estimates4['Est_PM'])
    pred_estimates4['TotalImpact'] = (pred_estimates4['Est_PM']-pred_estimates4['Mean_Est_PM_ALL'])
    pred_estimates4['TeamGain'] = (pred_estimates4['Est_PM']-pred_estimates4['Mean_Est_PM_player'])
    pred_estimates4['IndividualRating'] = (pred_estimates4['Mean_Est_PM_player']-pred_estimates4['Mean_Est_PM_ALL'])
    pred_estimates4 = pred_estimates4.sort_values(by=['TeamGain'],ascending=False)
    pred_estimates4['ID_PlayerID'] = pred_estimates4['PlayerID']
    subset_pred_dataACTUAL = pred_dataACTUAL2[["ID_PlayerID", "SP_PositionDet"]]
    pred_estimates4 = pred_estimates4.merge(subset_pred_dataACTUAL, on='ID_PlayerID', how='left')
    pred_estimates4['Pos'] = pred_estimates4['SP_PositionDet']
    display = pred_estimates4.loc[((~(pred_estimates4['ID_PlayersFull'].isin(players))) & (pred_estimates4['TeamName']=='Custom')),:].copy()
    display = display.loc[(display['MIN']>=500) & (display['MIN2']>=100),['Player','Pos','TeamGain','IndividualRating']]
    display['TeamGain'] = display['TeamGain'] - statistics.mean(display['TeamGain'])
    display['TeamGain'] = round(display['TeamGain'],1)
    display['IndividualRating'] = round(display['IndividualRating'],1)
    #print(display.head(10))
    return display.head(10)

if __name__ == "__main__":
   
    print("Team: Washington Wizards")
    Team = 'Washington Wizards'
    CustomTeam = []
    summarizeteam(Team, CustomTeam)
    pred_teamcustom = findstrandweak(Team, CustomTeam)
    bestplayerfits(Team,CustomTeam)
    
    
    Team = ''
    CustomTeam = ['Joel Embiid','James Harden','Tyrese Maxey','Tobias Harris','P.J. Tucker']
    print(f"CustomTeam = {CustomTeam}")
    summarizeteam(Team, CustomTeam)
    pred_teamcustom = findstrandweak(Team, CustomTeam)
    bestplayerfits(Team,CustomTeam)
    
    
