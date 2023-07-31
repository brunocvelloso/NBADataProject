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
library('RSelenium')
library(R.utils)
library('ds4psy')
library(foreign)
library("haven")
library(plm)
library(dplyr)
library("tictoc")
library("hoopR")
library("pacman")
library(stringi)
library('glmnet')
library(parallel)
library(MASS)
library(tidyverse)
library(jsonlite)
library(janitor)
library(extrafont)
library(ggrepel)
library(scales)
library(zoo)
library(feather)
library("httr")
#setwd(working)


#1. Load Relevant Packages and Set WD
fileLinks <- c("regdataFORTOOL.Rdata", #created as output in "ComputerBestPlayers.R"
               "vellplusmin.RData")
for (i in fileLinks) {
  link <- paste0("https://www.dropbox.com/scl/fo/51g9vud1seqtjmyl2gdxt/h/",i,"?rlkey=w73ql5hcbve241kau127t2a2h&dl=1",sep="")
  download.file(link,i)
  load(i)
}
#load("regdataFORTOOL.Rdata")
#load("vellplusmin.RData")

regdata <- regdata[!(regdata$PlayersFull=="Luka Doncic"& regdata$Forward_reg==1),]
regdata <- regdata[!(regdata$PlayersFull=="Anthony Edwards"& regdata$Forward_reg==1),]
regdata <- regdata %>% left_join(vellplusmin[,c("PlayerID","Date","VellosoPlusMinus_total","VellPMOff_PaceAdj","VellPMDef_PaceAdj","Repl_off","Repl_def")],by=c("PlayerID","Date"))
regdata$VellPMTotal_PaceAdj <- regdata$VellPMOff_PaceAdj + regdata$VellPMDef_PaceAdj
regdata$PG_reg <- ifelse((regdata$PG.x/(regdata$PG.x+regdata$SG.x+regdata$SF.x+regdata$PF.x+regdata$C.x))>=.25,1,0)
regdata$SG_reg <- ifelse((regdata$SG.x/(regdata$PG.x+regdata$SG.x+regdata$SF.x+regdata$PF.x+regdata$C.x))>=.25,1,0)
regdata$SF_reg <- ifelse((regdata$SF.x/(regdata$PG.x+regdata$SG.x+regdata$SF.x+regdata$PF.x+regdata$C.x))>=.25,1,0)
regdata$PF_reg <- ifelse((regdata$PF.x/(regdata$PG.x+regdata$SG.x+regdata$SF.x+regdata$PF.x+regdata$C.x))>=.25,1,0)
regdata$C_reg <- ifelse((regdata$C.x/(regdata$PG.x+regdata$SG.x+regdata$SF.x+regdata$PF.x+regdata$C.x))>=.25,1,0)
regdata$PG_reg <- ifelse(is.na(regdata$PG_reg),regdata$Guard_reg/2,regdata$PG_reg)
regdata$SG_reg <- ifelse(is.na(regdata$SG_reg),regdata$Guard_reg/2,regdata$SG_reg)
regdata$SF_reg <- ifelse(is.na(regdata$SF_reg),regdata$Forward_reg/2,regdata$SF_reg)
regdata$PF_reg <- ifelse(is.na(regdata$PF_reg),regdata$Forward_reg/2,regdata$PF_reg)
regdata$C_reg <- ifelse(is.na(regdata$C_reg),regdata$Center_reg,regdata$C_reg)
regdata <- dplyr::select(regdata,-PTSpm_regWin95,-ASTpm_regWin95,-STLpm_regWin95,-BLKpm_regWin95,-TOpm_regWin95,- OREBpm_regWin95,-DREBpm_regWin95,-PFpm_regWin95,-PG.x,-SG.x,-SF.x,-PF.x,-C.x,-off_error,-def_error)
for (j in c("Guard_reg","Forward_reg","Center_reg")) {
  for (i in c("ThreePct_reg","TwoPct_reg","FTPct_reg","PTSpm_reg","OREBpm_reg","DREBpm_reg","ASTpm_reg","STLpm_reg","BLKpm_reg","TOpm_reg","PFpm_reg","ImpactPlayspm_reg","ThreePMpm_reg","ThreePApm_reg","TwoPApm_reg","TwoPMpm_reg","FTApm_reg","FTMperFG_reg","TSPct_reg","NonShooter_reg","NonShooterXThreePct_reg","RAPM5_off","RAPM5_def","LowMin","LowMedMin")) {
    new_var_name <- paste(i, "_INT_", j, sep = "")
    print(new_var_name)
    regdata[[new_var_name]] <- regdata[[i]] * regdata[[j]]
  }
}
for (i in unique(regdata$SeasNum)) {
  new_var_name <- paste("SeasDummy_",i, sep = "")
  regdata[[new_var_name]] <- as.numeric(regdata$SeasNum==i)
}

#regdata <- regdata[regdata$PlayersFull=="LeBron James"|regdata$PlayersFull=="Stephen Curry",]
#regdata$Date <- regdata$Date+1
#regdata2 <- regdata
regdata_TOPROJ <- regdata %>% 
  dplyr::group_by(PlayerID)  %>%
  arrange(PlayerID, Date) %>%
  complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
  fill(everything()) %>%
  ungroup()
regdataADD <- regdata_TOPROJ[regdata_TOPROJ$Date==max(regdata_TOPROJ$Date),]
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
regdataADD$Date <- today-35
regdata_TOPROJ <- rbind(regdata_TOPROJ,regdataADD)
regdata_TOPROJ <- regdata_TOPROJ %>%
 dplyr::group_by(PlayerID) %>%
 arrange(PlayerID, Date) %>%
 complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
 fill(everything()) %>%
 ungroup()
regdata_TOPROJ <- rename(regdata_TOPROJ,SP_MIN_1y=MIN,SP_MIN_5y=MIN.x,SP_Teams_New=Teams,SP_year=year,SP_yearsq=yearsq,SP_Month=Month,SP_Year2=Year,SP_Day=Day,SP_Season=Season,SP_SeasonNum=SeasNum,SP_wgt=wgt,SP_RAPM1_defFWD1=RAPM1_defFWD1,SP_RAPM1_defFWD99=RAPM1_defFWD99,SP_RAPM1_offFWD1=RAPM1_offFWD1,SP_RAPM1_offFWD99=RAPM1_offFWD99,
                         SP_VellosoPlusMinus_off=VellosoPlusMinus_off,SP_VellosoPlusMinus_def=VellosoPlusMinus_def,SP_VellosoPlusMinus_total=VellosoPlusMinus_total,
                         SP_VellPMDef_PaceAdj=VellPMDef_PaceAdj,SP_VellPMOff_PaceAdj=VellPMOff_PaceAdj,SP_Repl_off=Repl_off,SP_Repl_def=Repl_def,SP_VellPMTotal_PaceAdj=VellPMTotal_PaceAdj,SP_PositionIndicator=PositionIndicator,SP_RAPM1_total=RAPM1_total,SP_RAPM5_total=RAPM5_total,SP_RepDef5=RepDef5,SP_RepOff5=RepOff5,SP_RepTot5=RepTot5,SP_RepDef1=RepDef1,SP_RepOff1=RepOff1,SP_RepTot1=RepTot1,
                         SP_FwdDate=FwdDate,SP_RAPM1_offFWD=RAPM1_offFWD,SP_RAPM1_defFWD=RAPM1_defFWD,
                         SP_RAPM1_off=RAPM1_off,SP_RAPM1_def=RAPM1_def)
regdata_TOPROJ <- rename(regdata_TOPROJ,ID_PlayerID=PlayerID,ID_Date=Date,ID_Players=Players,ID_PlayersFull=PlayersFull)
regdata_TOPROJ <- regdata_TOPROJ %>% 
  rename_with(~paste0("IND_", .), !starts_with("SP_") & !starts_with("ID_"))
regdata_TOPROJ <- dplyr::select(regdata_TOPROJ,-SP_Season)

#create eason dummies

#save(regdata_TOPROJ,file="regdata_TOPROJ.RData")

fileLinks <- c("modelbetaV1_PlayerData.RData")
for (i in fileLinks) {
  link <- paste0("https://www.dropbox.com/scl/fo/51g9vud1seqtjmyl2gdxt/h/",i,"?rlkey=w73ql5hcbve241kau127t2a2h&dl=1",sep="")
  download.file(link,i)
  load(i)
}
# load("modelbetaV1_PlayerData.RData")
playerdata <- dplyr::select(playerdata,Date,Season,ID_team,GameID,PlayerID,Team,MIN,MIN.xy,`+/-`,MIN3,MINPG3,OnOffGame,RPMScore,ORPMScore,DRPMScore,FinalStatus,InclDummy1,ProjMINFinal)
playerdata <- playerdata[playerdata$MIN>0,]
playerdata <- dplyr::rename(playerdata,ID_Date=Date,SP_Season = Season,SP_GameID=GameID,ID_PlayerID=PlayerID,SP_Team = Team,WGT_MIN = MIN,WGT_MIN.xy = MIN.xy,SP_PlusMin = `+/-`,WGT_MIN3 = MIN3,SP_MINPG3 = MINPG3,SP_OnOffGame=OnOffGame,SP_RPMScore=RPMScore,SP_ORPMScore=ORPMScore,SP_DRPMScore=DRPMScore,SP_FinalStatus=FinalStatus,SP_InclDummy1=InclDummy1,SP_ProjMINFinal=ProjMINFinal)
playerdata <- playerdata %>% ungroup()
regdata_TOPROJ <- regdata_TOPROJ %>% ungroup()
playerdata$Y_PlusMin2 <- (playerdata$SP_PlusMin/playerdata$WGT_MIN)*36
playerdata <- playerdata %>%
  dplyr::group_by(SP_Season) %>%
  mutate(PlusMin90 = quantile(Y_PlusMin2[WGT_MIN>=15],c(.90),na.rm=TRUE),
         PlusMin10 = quantile(Y_PlusMin2[WGT_MIN>=15],c(.10),na.rm=TRUE)) %>% ungroup()
playerdata$Y_PlusMin2[playerdata$WGT_MIN<15] <- pmin(playerdata$Y_PlusMin2[playerdata$WGT_MIN<15],playerdata$PlusMin90[playerdata$WGT_MIN<15])
playerdata$Y_PlusMin2[playerdata$WGT_MIN<15] <- pmax(playerdata$Y_PlusMin2[playerdata$WGT_MIN<15],playerdata$PlusMin10[playerdata$WGT_MIN<15])
playerdata$WGT_MAIN <- ifelse(playerdata$WGT_MIN<10,1,ifelse(playerdata$WGT_MIN<20,2,3))
playerdata$WGT_MAIN[playerdata$WGT_MIN<=2] <- 0
playerdata <- playerdata %>%
  dplyr::group_by(SP_Season) %>%
  mutate(PlusMin99 = quantile(Y_PlusMin2[WGT_MIN>=15],c(.99),na.rm=TRUE),
         PlusMin01 = quantile(Y_PlusMin2[WGT_MIN>=15],c(.01),na.rm=TRUE)) %>% ungroup()
playerdata$Y_PlusMin2 <- pmin(playerdata$Y_PlusMin2,playerdata$PlusMin99)
playerdata$Y_PlusMin2 <- pmax(playerdata$Y_PlusMin2,playerdata$PlusMin01)
playerdata <- dplyr::select(playerdata,-PlusMin99,-PlusMin01)
playerdata <- dplyr::select(playerdata,-PlusMin90,-PlusMin10)

regdata_FINAL <- dplyr::left_join(playerdata,regdata_TOPROJ,by=c("ID_PlayerID","ID_Date")) %>% ungroup()

regdata_FINAL <- regdata_FINAL %>%
  dplyr::group_by(ID_Date,SP_Team) %>%
  dplyr::mutate(TEAM_TotalMin = sum(WGT_MIN,na.rm = TRUE),
                TEAM_MINPG_reg = sum(IND_MINPG_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Guard_reg = sum(IND_Guard_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Forward_reg = sum(IND_Forward_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Center_reg = sum(IND_Center_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TwoPApm_reg = sum(IND_TwoPApm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ThreePApm_reg = sum(IND_ThreePApm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_FTMpm_reg = sum(IND_FTMpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Age_reg = sum(IND_Age_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NonShooter_reg = sum(IND_NonShooter_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ImpactPlayspm_reg = sum(IND_ImpactPlayspm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PTSpm_reg = sum(IND_PTSpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_OREBpm_reg = sum(IND_OREBpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_DREBpm_reg = sum(IND_DREBpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ASTpm_reg = sum(IND_ASTpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_STLpm_reg = sum(IND_STLpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_BLKpm_reg = sum(IND_BLKpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TOpm_reg = sum(IND_TOpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TSPct_reg = sum(IND_TSPct_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_RAPM5_off = sum(IND_RAPM5_off*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_RAPM5_def = sum(IND_RAPM5_def*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ThreePct_reg = sum(IND_ThreePct_reg*WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE)/sum(WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE),
                TEAM_TwoPct_reg = sum(IND_TwoPct_reg*WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE)/sum(WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE),
                TEAM_NumShooters_reg = sum((IND_ThreePct_reg>.35&IND_NonShooter_reg==0)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumScorers_reg = sum((IND_PTSpm_reg>20)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumPassers_reg = sum((IND_ASTpm_reg>4.5)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumRebounders_reg = sum(((IND_OREBpm_reg + IND_DREBpm_reg)>8.5)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_SharePG_reg = sum((IND_PG_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareSG_reg = sum((IND_SG_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareSF_reg = sum((IND_SF_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_SharePF_reg = sum((IND_PF_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareC_reg = sum((IND_C_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw5_reg = sum(IND_PlusMinRaw5_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw3_reg = sum(IND_PlusMinRaw3_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw1_reg = sum(IND_PlusMinRaw1_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareBig_reg = TEAM_SharePF_reg + TEAM_ShareC_reg,
                SP_ThreePct_reg = sum(WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE),
                SP_TwoPct_reg = sum(WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE),
                TEAM_rapmoffXmin = (sum(IND_rapmoffXmin*WGT_MIN,na.rm = TRUE))/(sum(WGT_MIN,na.rm = TRUE)),
                TEAM_rapmdefXmin = (sum(IND_rapmdefXmin*WGT_MIN,na.rm = TRUE))/(sum(WGT_MIN,na.rm = TRUE))) %>% ungroup()
  
for (i in c("TEAM_Guard_reg","TEAM_Forward_reg","TEAM_Center_reg","TEAM_SharePG_reg","TEAM_ShareSG_reg","TEAM_ShareSF_reg","TEAM_SharePF_reg","TEAM_ShareC_reg","TEAM_NumShooters_reg","TEAM_NumScorers_reg","TEAM_NumPassers_reg","TEAM_NumRebounders_reg","TEAM_ShareBig_reg","TEAM_NonShooter_reg")) {
  new_var_name <- paste(i,"_SQ", sep = "")
  #new_var_name2 <- paste(i,"_CUB", sep = "")
  regdata_FINAL[[new_var_name]] <- regdata_FINAL[[i]]*regdata_FINAL[[i]]
  #regdata_FINAL[[new_var_name2]] <- regdata_FINAL[[i]]*regdata_FINAL[[i]]*regdata_FINAL[[i]]
}
regdata_FINAL <- dplyr::select(regdata_FINAL,-SP_FinalStatus)
test <- regdata_FINAL[regdata_FINAL$ID_Date>="2023-06-09",]

na_counts <- colSums(is.na(regdata_FINAL))
regdata_FINAL <- regdata_FINAL[complete.cases(regdata_FINAL), ]
#save(regdata_FINAL,file="regdata_FINAL.RData")

variable_list <- names(regdata_FINAL)[grep("^TEAM_", names(regdata_FINAL))]
variable_list <- variable_list[2:length(variable_list)]
variable_list <- variable_list[!grepl("_SQ$|_CUB$", variable_list)]
variable_list1 <- variable_list[-c(grep("^TEAM_Num", variable_list))]
variable_list1 <- variable_list1[-c(grep("^TEAM_Sha", variable_list1))]
variable_list1 <- variable_list1[!grepl("Pct_reg$", variable_list1)]
for (i in variable_list1) {
  # if (i=="TEAM_ThreePct_reg") {
  #   replacestr <- str_replace(i,"TEAM_","IND_")
  #   regdata_FINAL[[i]] <- (regdata_FINAL[[i]]*regdata_FINAL[["SP_ThreePct_reg"]] - regdata_FINAL[[replacestr]]*regdata_FINAL[["WGT_MIN"]]*regdata_FINAL[["IND_ThreePApm_reg"]])/(regdata_FINAL[["SP_ThreePct_reg"]]-regdata_FINAL[["WGT_MIN"]]*regdata_FINAL[["IND_ThreePApm_reg"]])
  # } else if (i=="TEAM_TwoPct_reg") {
  #   replacestr <- str_replace(i,"TEAM_","IND_")
  #   regdata_FINAL[[i]] <- (regdata_FINAL[[i]]*regdata_FINAL[["SP_TwoPct_reg"]] - regdata_FINAL[[replacestr]]*regdata_FINAL[["WGT_MIN"]]*regdata_FINAL[["IND_TwoPApm_reg"]])/(regdata_FINAL[["SP_TwoPct_reg"]] -regdata_FINAL[["WGT_MIN"]]*regdata_FINAL[["IND_TwoPApm_reg"]])
  # } else {
    replacestr <- str_replace(i,"TEAM_","IND_")
    regdata_FINAL[[i]] <- (regdata_FINAL[[i]]*regdata_FINAL[["TEAM_TotalMin"]] - regdata_FINAL[[replacestr]]*regdata_FINAL[["WGT_MIN"]])/(regdata_FINAL[["TEAM_TotalMin"]]-regdata_FINAL[["WGT_MIN"]])
    
  # }
}


regdata_FINAL <- regdata_FINAL %>%
  dplyr::mutate(TEAM_ThreePct_reg = (TEAM_ThreePct_reg*SP_ThreePct_reg-IND_ThreePct_reg*WGT_MIN*IND_ThreePApm_reg)/(SP_ThreePct_reg-WGT_MIN*IND_ThreePApm_reg),
                TEAM_TwoPct_reg = (TEAM_TwoPct_reg*SP_TwoPct_reg-IND_TwoPct_reg*WGT_MIN*IND_TwoPApm_reg)/(SP_TwoPct_reg-WGT_MIN*IND_TwoPApm_reg),
                TEAM_NumShooters_reg = (TEAM_NumShooters_reg*TEAM_TotalMin - (IND_ThreePct_reg>.35&IND_NonShooter_reg==0)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_NumScorers_reg = (TEAM_NumShooters_reg*TEAM_TotalMin - (IND_PTSpm_reg>20)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_NumPassers_reg = (TEAM_NumPassers_reg*TEAM_TotalMin - (IND_ASTpm_reg>4.5)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_NumRebounders_reg = (TEAM_NumRebounders_reg*TEAM_TotalMin - ((IND_OREBpm_reg + IND_DREBpm_reg)>8.5)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_SharePG_reg = (TEAM_SharePG_reg*TEAM_TotalMin - (IND_PG_reg==1)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_ShareSG_reg = (TEAM_ShareSG_reg*TEAM_TotalMin - (IND_SG_reg==1)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_ShareSF_reg = (TEAM_ShareSF_reg*TEAM_TotalMin - (IND_SF_reg==1)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_SharePF_reg = (TEAM_SharePF_reg*TEAM_TotalMin - (IND_PF_reg==1)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_ShareC_reg = (TEAM_ShareC_reg*TEAM_TotalMin - (IND_C_reg==1)*WGT_MIN)/(TEAM_TotalMin-WGT_MIN),
                TEAM_ShareBig_reg = TEAM_SharePF_reg + TEAM_ShareC_reg)

for (i in c("TEAM_Guard_reg","TEAM_Forward_reg","TEAM_Center_reg","TEAM_SharePG_reg","TEAM_ShareSG_reg","TEAM_ShareSF_reg","TEAM_SharePF_reg","TEAM_ShareC_reg","TEAM_NumShooters_reg","TEAM_NumScorers_reg","TEAM_NumPassers_reg","TEAM_NumRebounders_reg","TEAM_ShareBig_reg","TEAM_NonShooter_reg")) {
  new_var_name <- paste(i,"_SQ", sep = "")
  #new_var_name2 <- paste(i,"_CUB", sep = "")
  regdata_FINAL[[new_var_name]] <- regdata_FINAL[[i]]*regdata_FINAL[[i]]
  #regdata_FINAL[[new_var_name2]] <- regdata_FINAL[[i]]*regdata_FINAL[[i]]*regdata_FINAL[[i]]
}



#save(regdata_FINAL,file="regdata_FINAL.RData")
rm(regdata_TOPROJ)
#load("regdata_FINAL.RData")
regdata_FINAL <- rename(regdata_FINAL,SP_TotalMin = TEAM_TotalMin)
variable_list <- names(regdata_FINAL)[!grepl("_CUB$|TEAM_ShareS|TEAM_ShareP|TEAM_ShareC", names(regdata_FINAL))]
regdata_FINAL <- regdata_FINAL[,variable_list]
#save(regdata_FINAL,file="regdata_FINAL.RData")

pred_data <- regdata_FINAL %>%
  group_by(ID_PlayerID) %>%
  filter(ID_Date == max(ID_Date) & SP_Season == "2022-23") %>%
  ungroup()

pred_data$WGT_MIN = pred_data$IND_MINPG_reg
pred_data <- pred_data %>%
  dplyr::group_by(SP_Team) %>%
  dplyr::mutate(SP_TotalMin = sum(WGT_MIN,na.rm = TRUE),
                TEAM_MINPG_reg = sum(IND_MINPG_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Guard_reg = sum(IND_Guard_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Forward_reg = sum(IND_Forward_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Center_reg = sum(IND_Center_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TwoPApm_reg = sum(IND_TwoPApm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ThreePApm_reg = sum(IND_ThreePApm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_FTMpm_reg = sum(IND_FTMpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_Age_reg = sum(IND_Age_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NonShooter_reg = sum(IND_NonShooter_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ImpactPlayspm_reg = sum(IND_ImpactPlayspm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PTSpm_reg = sum(IND_PTSpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_OREBpm_reg = sum(IND_OREBpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_DREBpm_reg = sum(IND_DREBpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ASTpm_reg = sum(IND_ASTpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_STLpm_reg = sum(IND_STLpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_BLKpm_reg = sum(IND_BLKpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TOpm_reg = sum(IND_TOpm_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_TSPct_reg = sum(IND_TSPct_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_RAPM5_off = sum(IND_RAPM5_off*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_RAPM5_def = sum(IND_RAPM5_def*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ThreePct_reg = sum(IND_ThreePct_reg*WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE)/sum(WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE),
                TEAM_TwoPct_reg = sum(IND_TwoPct_reg*WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE)/sum(WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE),
                TEAM_NumShooters_reg = sum((IND_ThreePct_reg>.35&IND_NonShooter_reg==0)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumScorers_reg = sum((IND_PTSpm_reg>20)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumPassers_reg = sum((IND_ASTpm_reg>4.5)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_NumRebounders_reg = sum(((IND_OREBpm_reg + IND_DREBpm_reg)>8.5)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_SharePG_reg = sum((IND_PG_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareSG_reg = sum((IND_SG_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareSF_reg = sum((IND_SF_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_SharePF_reg = sum((IND_PF_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareC_reg = sum((IND_C_reg==1)*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw5_reg = sum(IND_PlusMinRaw5_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw3_reg = sum(IND_PlusMinRaw3_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_PlusMinRaw1_reg = sum(IND_PlusMinRaw1_reg*WGT_MIN,na.rm = TRUE)/sum(WGT_MIN,na.rm = TRUE),
                TEAM_ShareBig_reg = TEAM_SharePF_reg + TEAM_ShareC_reg,
                SP_ThreePct_reg = sum(WGT_MIN*IND_ThreePApm_reg,na.rm = TRUE),
                SP_TwoPct_reg = sum(WGT_MIN*IND_TwoPApm_reg,na.rm = TRUE),
                TEAM_rapmoffXmin = (sum(IND_rapmoffXmin*WGT_MIN,na.rm = TRUE))/(sum(WGT_MIN,na.rm = TRUE)),
                TEAM_rapmdefXmin = (sum(IND_rapmdefXmin*WGT_MIN,na.rm = TRUE))/(sum(WGT_MIN,na.rm = TRUE))) %>% ungroup()

for (i in c("TEAM_Guard_reg","TEAM_Forward_reg","TEAM_Center_reg","TEAM_SharePG_reg","TEAM_ShareSG_reg","TEAM_ShareSF_reg","TEAM_SharePF_reg","TEAM_ShareC_reg","TEAM_NumShooters_reg","TEAM_NumScorers_reg","TEAM_NumPassers_reg","TEAM_NumRebounders_reg","TEAM_ShareBig_reg","TEAM_NonShooter_reg")) {
  new_var_name <- paste(i,"_SQ", sep = "")
  #new_var_name2 <- paste(i,"_CUB", sep = "")
  pred_data[[new_var_name]] <- pred_data[[i]]*pred_data[[i]]
  #regdata_FINAL[[new_var_name2]] <- regdata_FINAL[[i]]*regdata_FINAL[[i]]*regdata_FINAL[[i]]
}

variable_list <- names(pred_data)[!grepl("_CUB$|TEAM_ShareS|TEAM_ShareP|TEAM_ShareC", names(pred_data))]
pred_data <- pred_data[,variable_list]
team_list <- names(regdata_FINAL)[grepl("_CUB$|TEAM_ShareS|TEAM_ShareP|TEAM_ShareC", names(regdata_FINAL))]
team_list1 <- grep("^TEAM_|^SP_Team$", names(pred_data), value = TRUE)
team_list2 <- grep("^TEAM_", names(pred_data), value = TRUE)

pred_team <- distinct(pred_dataACTUAL[team_list1] ,SP_Team,.keep_all = TRUE)


for (j in unique(pred_team$SP_Team)) {
  for (i in team_list1[2:length(team_list1)]){
    pred_team[[i]][pred_team[["SP_Team"]]==j] <- sum((pred_dataACTUAL[[i]][pred_dataACTUAL[["SP_Team"]]==j])*(pred_dataACTUAL[["IND_MINPG_reg"]][pred_dataACTUAL[["SP_Team"]]==j]))/sum((pred_dataACTUAL[["IND_MINPG_reg"]][pred_dataACTUAL[["SP_Team"]]==j]))
    
  }
}
pred_dataACTUAL <- pred_data
pred_dataTEMP <- pred_data
for (i in unique(pred_team$SP_Team)) {
  pred_dataTEMP$SP_Team <- i
  pred_data <- rbind(pred_data,pred_dataTEMP)
}
pred_dataALLCOMBS <- distinct(pred_data)
pred_dataALLCOMBS <- dplyr::left_join(dplyr::select(pred_dataALLCOMBS,-(team_list2)),pred_team,by=c("SP_Team"))

#save(pred_team,file="pred_team.RData")
#save(pred_dataACTUAL,file="pred_dataACTUAL.RData")
#save(pred_dataALLCOMBS,file="pred_dataALLCOMBS.RData")
rm(playerdata,pred_data)
rm(pred_dataTEMP,regdata,regdataADD,vellplusmin,test)

counter <- 1
for (i in names(pred_dataACTUAL)[grep("^IND_", names(pred_dataACTUAL))]) {
  for (j in names(pred_dataACTUAL)[grep("^TEAM_", names(pred_dataACTUAL))]) {
  if (substr(i,1,9)=="IND_SeasD") {
    next
  }
  newname <- paste0("COMB_",i,"_INT_",j,sep="")
  pred_dataACTUAL[[newname]] <- pred_dataACTUAL[[i]]*pred_dataACTUAL[[j]] 
  counter <- counter+1
  }
}

counter <- 1
for (i in names(pred_dataALLCOMBS)[grep("^IND_", names(pred_dataALLCOMBS))]) {
  for (j in names(pred_dataALLCOMBS)[grep("^TEAM_", names(pred_dataALLCOMBS))]) {
    if (substr(i,1,9)=="IND_SeasD") {
      next
    }
    newname <- paste0("COMB_",i,"_INT_",j,sep="")
    pred_dataALLCOMBS[[newname]] <- pred_dataALLCOMBS[[i]]*pred_dataALLCOMBS[[j]] 
    counter <- counter+1
  }
}

save(pred_dataACTUAL,file="pred_dataACTUAL.RData")
save(pred_dataALLCOMBS,file="pred_dataALLCOMBS.RData")
rm(pred_team)
rm(pred_dataACTUAL)
rm(pred_dataALLCOMBS)

regdata_FINAL2 <- regdata_FINAL
save(regdata_FINAL2,file="regdata_FINAL2.RData")
rm(regdata_FINAL2)

#load("regdata_FINAL2.RData")
#regdata_FINAL <- regdata_FINAL2
#rm(regdata_FINAL2)
counter <- 1
for (i in names(regdata_FINAL)[grep("^IND_", names(regdata_FINAL))]) {
  for (j in names(regdata_FINAL)[grep("^TEAM_", names(regdata_FINAL))]) {
    if (substr(i,1,9)=="IND_SeasD") {
      next
    }
    newname <- paste0("COMB_",i,"_INT_",j,sep="")
    regdata_FINAL[[newname]] <- regdata_FINAL[[i]]*regdata_FINAL[[j]] 
    counter <- counter+1
  }
}

save(regdata_FINAL,file="regdata_FINAL.RData")
write_feather(regdata_FINAL, "my_dataframe.feather")
load("pred_dataACTUAL.RData")
load("pred_dataALLCOMBS.RData")
load("pred_team.RData")
#load("regdata_FINAL.RData")



#add last interaction terms

# regdata_FINAL <- regdata_FINAL %>%
#   dplyr::group_by(ID_Date,SP_Team) %>%
#   dplyr::mutate(TEAM_rapmoffXmin = (sum(IND_rapmoffXmin*WGT_MIN,na.rm = TRUE) - IND_rapmoffXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN),
#                 TEAM_rapmdefXmin = (sum(IND_rapmdefXmin*WGT_MIN,na.rm = TRUE) - IND_rapmdefXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN)) %>% ungroup()
# pred_dataALLCOMBS <- pred_dataALLCOMBS %>%
#   dplyr::group_by(SP_Team) %>%
#   dplyr::mutate(TEAM_rapmoffXmin = (sum(IND_rapmoffXmin*WGT_MIN,na.rm = TRUE) - IND_rapmoffXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN),
#                 TEAM_rapmdefXmin = (sum(IND_rapmdefXmin*WGT_MIN,na.rm = TRUE) - IND_rapmdefXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN)) %>% ungroup()
# pred_dataACTUAL <- pred_dataACTUAL %>%
#   dplyr::group_by(SP_Team) %>%
#   dplyr::mutate(TEAM_rapmoffXmin = (sum(IND_rapmoffXmin*WGT_MIN,na.rm = TRUE) - IND_rapmoffXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN),
#                 TEAM_rapmdefXmin = (sum(IND_rapmdefXmin*WGT_MIN,na.rm = TRUE) - IND_rapmdefXmin*WGT_MIN)/(sum(WGT_MIN,na.rm = TRUE) - WGT_MIN)) %>% ungroup()
# 
# counter <- 1
# for (i in names(pred_dataACTUAL)[grep("^IND_", names(pred_dataACTUAL))]) {
#   for (j in c("TEAM_rapmoffXmin","TEAM_rapmdefXmin")) {
#     if (substr(i,1,9)=="IND_SeasD") {
#       next
#     }
#     newname <- paste0("COMB_",i,"_INT_",j,sep="")
#     pred_dataACTUAL[[newname]] <- pred_dataACTUAL[[i]]*pred_dataACTUAL[[j]] 
#     counter <- counter+1
#   }
# }
# 
# counter <- 1
# for (i in names(pred_dataALLCOMBS)[grep("^IND_", names(pred_dataALLCOMBS))]) {
#   for (j in c("TEAM_rapmoffXmin","TEAM_rapmdefXmin")) {
#     if (substr(i,1,9)=="IND_SeasD") {
#       next
#     }
#     newname <- paste0("COMB_",i,"_INT_",j,sep="")
#     pred_dataALLCOMBS[[newname]] <- pred_dataALLCOMBS[[i]]*pred_dataALLCOMBS[[j]] 
#     counter <- counter+1
#   }
# }
# 
# 
# counter <- 1
# for (i in names(regdata_FINAL)[grep("^IND_", names(regdata_FINAL))]) {
#   for (j in c("TEAM_rapmoffXmin","TEAM_rapmdefXmin")) {
#     if (substr(i,1,9)=="IND_SeasD") {
#       next
#     }
#     newname <- paste0("COMB_",i,"_INT_",j,sep="")
#     regdata_FINAL[[newname]] <- regdata_FINAL[[i]]*regdata_FINAL[[j]] 
#     counter <- counter+1
#   }
# }


regdata_FINAL$IND_NonShooter_reg <- as.numeric(regdata_FINAL$IND_NonShooter_reg)
pred_dataACTUAL$IND_NonShooter_reg <- as.numeric(pred_dataACTUAL$IND_NonShooter_reg)
pred_dataALLCOMBS$IND_NonShooter_reg <- as.numeric(pred_dataALLCOMBS$IND_NonShooter_reg)

filtered_words <- grep("^COMB_.*_INT_Guard_reg_*", names(pred_dataACTUAL), value = TRUE)
filtered_words2 <- grep("^COMB_.*_INT_Forward_reg_*", names(pred_dataACTUAL), value = TRUE)
filtered_words3 <- grep("^COMB_.*_INT_Center_reg_*", names(pred_dataACTUAL), value = TRUE)

pred_dataACTUAL <- dplyr::select(pred_dataACTUAL,-filtered_words)
pred_dataACTUAL <- dplyr::select(pred_dataACTUAL,-filtered_words2)
pred_dataACTUAL <- dplyr::select(pred_dataACTUAL,-filtered_words3)
pred_dataALLCOMBS <- dplyr::select(pred_dataALLCOMBS,-filtered_words)
pred_dataALLCOMBS <- dplyr::select(pred_dataALLCOMBS,-filtered_words2)
pred_dataALLCOMBS <- dplyr::select(pred_dataALLCOMBS,-filtered_words3)
regdata_FINAL <- dplyr::select(regdata_FINAL,-filtered_words)
regdata_FINAL <- dplyr::select(regdata_FINAL,-filtered_words2)
regdata_FINAL <- dplyr::select(regdata_FINAL,-filtered_words3)

#write_feather(regdata_FINAL, "my_dataframe.feather")
write_feather(pred_dataACTUAL, "pred_dataACTUAL.feather")
write_feather(pred_dataALLCOMBS, "pred_dataALLCOMBS.feather")
write_feather(pred_team, "pred_team.feather")
