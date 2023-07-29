#0. Clean up and erase

rm(list=ls())

#1 Set folders
library(httr)

#USER: CHANGE THIS FOR YOUR COMPUTER/LOCAL GIT REPOSITORY
main <- "/Users/ricardovelloso/git-projects/NBA_Data_Project/"
rcode <- "/Users/ricardovelloso/git-projects/NBA_Data_Project/R/"
pycode <- "/Users/ricardovelloso/git-projects/NBA_Data_Project/py/"
output <- "/Users/ricardovelloso/git-projects/NBA_Data_Project/Output/"
working <- "/Users/ricardovelloso/git-projects/NBA_Data_Project/R/"
setwd(working)

#change to YES if you want to re-run everything in MASTER yourself
#(STRONGLY RECOMMEND AGAINST)
Rerun <- "NO"
if(Rerun=="YES") {
  #Download data
  fileLinks <- c(
    "master_teamboxscores.RData",
    "master_boxscores.RData",
    "NBA_ESPNgamelinks.RData",
    "NBA_ESPNgamelinksWITHDATES.RData",
    "nba_pbpCOMPLETE.RData",
    "nba_playerboxCOMPLETE.RData",
    "nba_teamboxCOMPLETE.RData",
    "coefALL.RData",
    "coef2ALL.RData",
    "playerages.RData",
    "playersnames.RData",
    "master_detailed_teambox",
    "master_detailedboxscore.RData",
    "master_adjplusminus.RData",
    "rollingboxALL.RData",
    "data_modelalphaV4.RData",
    "NBAdistance.RData",
    "injuryhistory.RData",
    "injurytable.RData",
    "modelbetaV1_PlayerData.RData",
    "todaystable.RData",
    "todayslines.RData",
    "modelbetaV1_teamlist.RData",
    "allregdata_modelbetaV1FORMERGE.RData",
    "predictions_final.RData",
    "modelgammaV2additions1.RDatat.RData",
    "modelgammaV2additions2.RDatat.RData",
    "modelgammaV2additions3.RDatat.RData",
    "modelgammaV2additions4.RDatat.RData",
    "OldLinesForGammaV2.RData",
    "pointsbetlines.RData",
    "seasonpreds.RData",
    "modelgammaV2additions5.RData",
    "modelgammaV2additions6.RData",
    "modelgammaV2additions10.RData",
    "modelgammaV2additions20.RData",
    "modelgammaV2additions30.RData",
    "additions_teammerge.RData",
    "MarginConvFactors.RData",
    "combdata.RData",
    "combdataplayoffs.RData")
  for (i in fileLinks) {
    link <- paste0("https://www.dropbox.com/scl/fo/51g9vud1seqtjmyl2gdxt/h/",i,"?rlkey=w73ql5hcbve241kau127t2a2h&dl=1",sep="")
    download.file(link,i)
    load(i)
    variablename <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
    save(list = variablename,file=i)
    rm(list = variablename)
  }
}
