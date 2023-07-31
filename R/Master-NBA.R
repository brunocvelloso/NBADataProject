#0. Clean up and erase

rm(list=ls())

#1. Load Relevant Packages and Set WD
library(pdftools)
library(tidyverse)
library(gdata)                   
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(stringr)
library(selectr)
library(xml2)
library(rvest)
library(jsonlite)
library("xlsx")
library("writexl")
library("haven")
library(pdftools)
library(tidyverse)
library(gdata)                   
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(stringr)
library(selectr)
library(xml2)
library(rvest)
library(jsonlite)
library("RSelenium")
library(R.utils)
library("ds4psy")
library(foreign)
library("haven")
library(plm)
library(dplyr)
library("tictoc")
library("hoopR")
library("pacman")

#INPUT: Go into housekeeping, and set relevant variables to files where project contained
source("Housekeeping.R")
setwd(working)

#######################################################################
######2. Code to create initial box score  and play by play data#######
#######################################################################
#NOTE: I already have initial datasets, so no need to run this (just if you want
#to start from scratch). I EXCLUDED THESE FROM GITHUB REPOSITORY

# #Download ESPN raw play by play and box score data
# source("datadownload.R")
# 
# #Find missing games (using my web-scraped list of games) and fill in missing information
# source("completedata.R")
# 
# #Fill in missing plus-minus from ESPN box scores, use basketball reference for missing play-by-play
# #these are the main raw data files that will be updated and form the beginning of any additional
# #code I run on top of this
# #Outputs: nba_pbpCOMPLETE, nba_playerboxCOMPLETE, nba_teamboxCOMPLETE
# source("completedata_part2.R")


#######################################################################
#########2. Code to add new game updates to master data################
#######################################################################


#Get recent game links for new data and add to master data
#Inputs from working: NBA_ESPNgamelinks, nba_pbpCOMPLETE, nba_playerboxCOMPLETE, nba_teamboxCOMPLETE
  #IF NONE: creates empty version and simply scrapes entire dataset from scratch
  #which takes much more time
#Outputs to working: 
# save(nba_pbpCOMPLETE, file = paste("nba_pbpCOMPLETE.RData", sep=""))
# save(nba_playerboxCOMPLETE, file = paste("nba_playerboxCOMPLETE.RData", sep=""))
# save(nba_teamboxCOMPLETE, file = paste("nba_teamboxCOMPLETE.RData", sep=""))
# save(NBA_ESPNgamelinks, file = paste("NBA_ESPNgamelinks.RData", sep=""))
# save(nba_pbpNEW, file = paste("nba_pbpNEW.RData", sep=""))
# save(nba_playerboxNEW, file = paste("nba_playerboxNEW.RData", sep=""))
# save(nba_teamboxNEW, file = paste("nba_teamboxNEW.RData", sep=""))
# save(NBA_ESPNgamelinksSUB, file = paste("NBA_ESPNgamelinksSUB.RData", sep=""))
# save(startdate, file = paste("startdate.RData", sep=""))
# save(enddate, file = paste("enddate.RData", sep=""))
# save(homecourtrefs, file = paste("homecourtrefs.RData", sep=""))
source("UpdateLinks.R")

#From raw data create a cleaned version of box score data
# INPUTS:
# load("nba_teamboxNEW.RData")
# load("nba_playerboxNEW.RData")
# load("NBA_ESPNgamelinks.RData")
# load("NBA_ESPNgamelinksSUB.RData")
# load("startdate.RData")
# load("enddate.RData")
#OUTPUTS:
# save(boxscoredataUPDATE, file = paste("NBAboxscoresRECENTUPDATE.RData", sep=""))
# save(boxscoredata, file = paste("master_boxscores.RData", sep=""))
# save(teamboxscoredataUPDATE, file = paste("NBAteamboxscoresRECENTUPDATE.RData", sep=""))
# save(teamboxscoredata, file = paste("master_teamboxscores.RData", sep=""))
source("CleanBoxScoreData.R")

#From raw data create a cleaned version of play-by-play data
#Detailed info on every shot/location, and stints for EVERY combination of players
#on the floor at any given time
# INPUTS:
# load("nba_teamboxNEW.RData")
# load("nba_playerboxNEW.RData")
# load("NBA_ESPNgamelinks.RData")
# load("NBA_ESPNgamelinksSUB.RData")
# load("startdate.RData")
# load("enddate.RData")
# load("master_adjplusminus.RData")
# load("master_detailed_teambox.RData")
# load("master_detailedboxscore.RData")
# OUTPUTS:
# save(adjplusminusUPDATE, file = paste("adjplusminusUPDATE.RData", sep=""))
# save(adjplusminus, file = paste("master_adjplusminus.RData", sep=""))
# save(detailedboxscoreUPDATE, file = paste("detailedboxscoreUPDATE.RData", sep=""))
# save(detailedboxscore, file = paste("master_detailedboxscore.RData", sep=""))
# save(detailed_teamboxUPDATE, file = paste("detailed_teamboxUPDATE.RData", sep=""))
# save(detailed_teambox, file = paste("master_detailed_teambox.RData", sep=""))
source("CleanPlaybyPlayv2.R")

#From cleaned play by play, create stints and estimates ON-OFF contribution using
#Ridge regression (dummies for whenever a player is playing in a stint)
# INPUTS:
# load("master_detailed_teambox.RData")
# load("master_adjplusminus.RData")
# load("master_detailedboxscore.RData")
# load("master_teamboxscores.RData")
# load("master_boxscores.RData")
# load("coefALL.RData")
# load("coef2ALL.RData")
# load("rollingboxALL.RData")
# load("playerages.RData")
# load("playersnames.RData")
# OUTPUTS:
# save(playerages,file="playerages.RData")
# save(playersnames,file="playersnames.RData")
# save(coefALL,file="coefALL.RData")
# save(coef2ALL,file="coef2ALL.RData")
# save(rollingboxALL,file="rollingboxALL.RData")
source("OnOff_AdjPM.R")


#Computes the estimated plus/minus of every player for the next year, using play-by-play data and
#also using box score data. Idea is to "predict" future points added for every player
#using how the team performs when they are on vs. off the court and using the box
#score data of the player. These player ratings will feed into team variables later
#(i.e. I want to note how good the players on a team are)
# INPUTS:
# load("coefALL.RData")
# load("coef2ALL.RData")
# load("rollingboxALL.RData")
# OUTPUTS:
# save(vellplusmin,file="vellplusmin.RData")
# save(repl_values,file="repl_values.RData")
# save(currentranksTOT,file="currentranksTOT.RData")
# ggsave("RAPMoverTime.png", w = 12, h = 6, dpi = 75) MAIN OUTPUT
source("ComputeBestPlayers.R")

#4. Create team-level variables as the season goes on 

#creates a bunch of basic team-level vairables for teams for every game
#INPUTS:
# load("master_teamboxscores.RData")
# load("master_boxscores.RData")
# load("NBA_ESPNgamelinks.RData")
# load("NBA_ESPNgamelinksSUB.RData")
# load("NBAdistance.RData")
# load("startdate.RData")
# load("enddate.RData")
# load("data_modelalphaV4.RData")
# OUTPUTS:
# save(seasondata_master, file = paste("data_modelalphaV4OLD.RData", sep="")) #this is in case something goes wrong with update
# save(box_nopreseason, file = paste("box_nopreseason.RData", sep=""))
# save(seasondata, file = paste("data_modelalphaV4.RData", sep=""))
source("UpdateTeamVariables.R") 

#scrapes ESPN for any NBA games in next three dates, and uploads relevent variables
#before prediction a basic regression model for game outcomes on these future games
# INPUTS:
# load("data_modelalphaV4.RData")
# OUTPUTS
# save(seasondata, file = paste("data_modelalphaV4Orig.RData", sep="")) #precaution in case a mistake is made
# save(NBA_ESPNgamelinksFORPRED, file = paste("NBA_ESPNgamelinksFORPRED.RData", sep=""))  
# save(seasondata, file = paste("allregdata_modelalphaV4.RData", sep="")) #includes future games now
# save(preddata_final, file = paste("testdata_modelalphaV4.RData", sep=""))
# save(predictions, file = paste("predictions.RData", sep=""))
source("RegressionModelPredictions_alpha.R")

#scrapes PointsBet for current lines on games and then ESPN for injury report (which will feed into later regression models)
#NOTE: errors likely if websites don't update on time/correctly, in which case error message is returned
#NO games so don't worry, since you need to set up docker within R
# source("ScrapeLines.R")

#constructs various measure to deduce a team's average player quality (weighted by minutes played)
#because we are dealing with player game-level data, this section takes the longest to update
#INPUTS:
# load("box_nopreseason.RData")
# load("startdate.RData")
# load("data_modelalphaV4.RData")
# load("vellplusmin.RData")
# load("repl_values.RData")
# load("testdata_modelalphaV4.RData") #for new games
# load("modelbetaV1_PlayerData.RData")
# load("todaystable.RData")
# load("injurytable.RData")
# load("injuryhistory.RData")
#OUTPUTS:
# save(seasonplayerdata, file = paste("seasonsummaries_PlayerData.RData", sep=""))
# save(regseasonplayerdata, file = paste("regseasonsummaries_PlayerData.RData", sep=""))
# save(box_nopreseason, file = paste("box_nopreseasonBetaUpdate.RData", sep=""))
# save(teamroster, file = paste("teamroster.RData", sep=""))
# save(playerdata,file="playerdata_backup.RData")
# save(playoffmindata,file="playoffmindata.RData")
# save(table, file = paste("injurytable.RData", sep=""))
# save(playerdata, file = paste("modelbetaV1_PlayerData.RData", sep=""))
# save(playerdataMERGE, file = paste("modelbetaV1_dataformerge.RData", sep=""))
# save(playerdataVIEW, file = paste("checkteamadjustment.RData", sep=""))
# save(playerdataTODAY, file = paste("checkinjuries.RData", sep=""))
# save(injuryhistory, file = paste("injuryhistory.RData", sep=""))
source("AddPlayerDatatoTeamData.R")

#small additional cleaning steps for player data
#INPUTS:
# load("modelbetaV1_dataformerge.RData")
# load("seasonsummaries_PlayerData.RData")
# load("teamroster.RData")
# load("modelbetaV1_PlayerData.RData")
# load("allregdata_modelalphaV4.RData")
# load("box_nopreseason.RData")
# load("startdate.RData")
# load("box_nopreseasonBetaUpdate.RData")
# load("modelbetaV1_teamlist.RData")
# OUTPUTS:
# save(teamlistTOADD, file = paste("modelbetaV1_teamlistTOADD.RData", sep=""))
# save(teamlistMERGE, file = paste("modelbetaV1_teamlist.RData", sep=""))
source("AdditionalTeamDataCleaning.R")

#adds some additional variables adhoc (eg, whether a team is tanking, info on team series results
#for playoff games, lots of pace-adjusted team variables. Also it merges in the player quality
#measures from previous two files into the season data at the team-level)
#INPUT:
# load("modelbetaV1_dataformerge.RData")
# load("allregdata_modelalphaV4.RData")
# load("allregdata_modelbetaV1FORMERGE.RData")
# load("modelbetaV1_teamlist.RData")
# load("predictionsAlphaV4.RData")
# load("predictions.RData")
# load("todayslines.RData")
#OUTPUT:
# save(seasondata2, file = paste("allregdata_modelbetaV1FORMERGE.RData", sep=""))
# save(seasondata, file = paste("allregdata_modelbetaV1.RData", sep=""))
# save(seasondataRF, file = paste("allregdata_modelgammaV1.RData", sep=""))
# save(preddata_final, file = paste("testdata_modelbetaV1.RData", sep=""))
# save(preddata_finalRF, file = paste("testdata_modelgammaV1.RData", sep=""))
# save(seasondataREGRF, file = paste("regdata_modelgammaV1.RData", sep=""))
# save(predictionsBetaV3, file = paste("predictionsBetaV3.RData", sep=""))
# save(predictionsAlphaV4, file = paste("predictionsAlphaV4.RData", sep=""))
# save(predictions_final, file = paste("predictions_final.RData", sep=""))
source("ModelBetaUpdate.R")
#INPUT:
# load("modelbetaV1_dataformerge.RData")
# load("allregdata_modelalphaV4.RData")
# load("modelgammaV2additions1.RData")
# load("modelgammaV2additions2.RData")
# load("modelgammaV2additions3.RData")
# load("modelgammaV2additions4.RData")
# load("allregdata_modelbetaV1.RData")
# load("modelbetaV1_teamlist.RData")
#OUTPUT:
# save(seasondata_add1,file="modelgammaV2additions1.RData")
# save(seasondata_add2,file="modelgammaV2additions2.RData")
# save(seasondata_add3,file="modelgammaV2additions3.RData")
# save(seasondata_add4,file="modelgammaV2additions4.RData")
# save(seasondata, file = paste("allregdata_modelgammaV2.RData", sep=""))
source("ModelBetaUpdatePart2.R")

#finish creating regression variables, in particular adding gambling lines since 2007 to data
#INPUT:
# load("modelbetaV1_dataformerge.RData")
# load("allregdata_modelgammaV2.RData")
# load("modelbetaV1_teamlist.RData")
# load("OldLinesForGammaV2.RData")
# load("todayslines.RData")
# load("pointsbetlines.RData")
#OUTPUT:
# save(lines_all,file="pointsbetlines.RData")
# save(dfdata2,file="OldLinesForGammaV2.RData")
# save(seasondata, file = paste("allregdata_withlines_FINAL.RData", sep=""))
source("FinalizeRegressionData.R")

#Most experimental variables. Started with finding ways to impute team performance
#at the beginning of the season (since there is no data from that season). You 
#start by assuming the team will be as good as last year, with adjustments for
#roster additions. Then you Assume normal priors and posteriors and update the
#posterior distribution as more games come in to properly capture uncertainty.
#This smoothing is very important when no data at beginning and improves model
#INPUT:
# load("allregdata_withlines_FINAL.RData")
# load("seasonpreds.RData")
# load("modelgammaV2additions5.RData")
# load("modelgammaV2additions6.RData")
# load("additions_teammerge.RData")
# load("modelgammaV2additions10.RData")
# load("modelgammaV2additions20.RData")
# load("homecourtrefs.RData")
# load("modelgammaV2additions30.RData")
# load("MarginConvFactors.RData")
#OUTPUT:
# save(seasondata_add2,file="modelgammaV2additions6.RData")
# save(seasondata_add5,file="modelgammaV2additions5.RData")
# save(seasondata_add10,file="modelgammaV2additions10.RData")
# save(seasondata_add20,file="modelgammaV2additions20.RData")
# save(seasondata_add30,file="modelgammaV2additions30.RData")
# save(teammerge,file="additions_teammerge.RData")
# save(seasondata,file="allregdata_FINALV2.RData")
source("EmpiricalBayesTeamPerformance.R")

#5. Run some regression models, and compare ability to predict game and player outcomes

#INPUT: load("allregdata_FINALV2.RData")
#OUTPUT:
source("LinRegModels.R")
# source("crossvalidation_headtohead.R") #compare out-of-sample accuracy of two models
# source("crossvalidation.R") #compare out-of-sample accuracy of two models
# source("AddPlayerData_foroldpreds.R") #if i want to backtest a model
# source("GetOldPredictions.R") #compute estimates of current models for old games


