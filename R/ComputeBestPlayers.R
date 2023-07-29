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
#library("naniar")
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
#setwd(working)


# set theme
theme_bruno <- function () { 
  theme_minimal(base_size=12, base_family="Tahoma") %+replace% 
    theme(
      
      panel.grid.major=element_line(color="azure2"),
      plot.background = element_rect(fill = 'cornsilk1', color = "cornsilk1"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}


load("coefALL.RData")
load("coef2ALL.RData")
load("rollingboxALL.RData")
# rollingboxALL$PointGuard <- ifelse((rollingboxALL$G.x>=rollingboxALL$`F.x`)&(rollingboxALL$G.x>=rollingboxALL$`C.x`),1,0)
# rollingboxALL$Forward <- ifelse((rollingboxALL$`F.x`>=rollingboxALL$`C.x`)&(rollingboxALL$`F.x`>rollingboxALL$G.x),1,0)
# rollingboxALL$Center <- ifelse((rollingboxALL$C.x>rollingboxALL$`F.x`)&(rollingboxALL$C.x>rollingboxALL$G.x),1,0)
# summary <- rollingboxALL %>%
#   dplyr::group_by(PlayerID,PlayersFull) %>%
#   dplyr::summarize(PG=sum(PG.x,na.rm=TRUE)/5,
#                    SG=sum(SG.x,na.rm=TRUE)/5,
#                    SF=sum(SF.x,na.rm=TRUE)/5,
#                    PF=sum(PF.x,na.rm=TRUE)/5,
#                    C=sum(C.x,na.rm=TRUE)/5,
#                    G=sum(G.x,na.rm=TRUE)/5,
#                    F=sum(F.x,na.rm=TRUE)/5)

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
# coef2ALL$RAPM1_off[is.na(coef2ALL$RAPM1_off)] <- coef2ALL$RAPM5_off[is.na(coef2ALL$RAPM1_off)]
# coef2ALL$RAPM1_def[is.na(coef2ALL$RAPM1_def)] <- coef2ALL$RAPM5_def[is.na(coef2ALL$RAPM1_def)]
# coef2ALL$RAPM1_total[is.na(coef2ALL$RAPM1_total)] <- coef2ALL$RAPM5_total[is.na(coef2ALL$RAPM1_total)]
# coef2ALL <- dplyr::select(coef2ALL,-RAPM5_total,-RAPM5_off,-RAPM5_def)
coefALL <- coefALL %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(RepOff5 = mean(RAPM5_off[PlayerID=="Replacement"]),
         RepDef5 = mean(RAPM5_def[PlayerID=="Replacement"]),
         RepTot5 = mean(RAPM5_total[PlayerID=="Replacement"]))
coef2ALL <- coef2ALL %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(RepOff1 = mean(RAPM1_off[PlayerID=="Replacement"]),
         RepDef1 = mean(RAPM1_def[PlayerID=="Replacement"]),
         RepTot1 = mean(RAPM1_total[PlayerID=="Replacement"]))
coefALL$RAPM5_def[is.na(coefALL$RAPM5_def)] <- coefALL$RepDef5[is.na(coefALL$RAPM5_def)]
coefALL$RAPM5_off[is.na(coefALL$RAPM5_off)] <- coefALL$RepOff5[is.na(coefALL$RAPM5_off)]
coefALL$RAPM5_total[is.na(coefALL$RAPM5_total)] <- coefALL$RepTot5[is.na(coefALL$RAPM5_total)]
coef2ALL$RAPM1_def[is.na(coef2ALL$RAPM1_def)] <- coef2ALL$RepDef1[is.na(coef2ALL$RAPM1_def)]
coef2ALL$RAPM1_off[is.na(coef2ALL$RAPM1_off)] <- coef2ALL$RepOff1[is.na(coef2ALL$RAPM1_off)]
coef2ALL$RAPM1_total[is.na(coef2ALL$RAPM1_total)] <- coef2ALL$RepTot1[is.na(coef2ALL$RAPM1_total)]
regdata <- dplyr::select(rollingboxALL,c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN","PG.x","SG.x","SF.x","PF.x","C.x",names(rollingboxALL)[!is.na(stri_extract_first_regex(names(rollingboxALL),"_reg"))]))
regdata <- regdata %>% left_join(coefALL[,c("PlayerID","Date","RAPM5_off","RAPM5_def","RAPM5_total","RepOff5","RepDef5","RepTot5")],by=c("PlayerID","Date"))
regdata <- regdata %>% left_join(coef2ALL[,c("PlayerID","Date","RAPM1_off","RAPM1_def","RAPM1_total","RepOff1","RepDef1","RepTot1")],by=c("PlayerID","Date"))

regdata$PlusMinRaw1_reg <- pmax(regdata$PlusMinRaw1_reg,-20)
regdata$PlusMinRaw1_reg <- pmin(regdata$PlusMinRaw1_reg,20)
regdata$PlusMinRaw3_reg <- pmax(regdata$PlusMinRaw3_reg,-20)
regdata$PlusMinRaw3_reg <- pmin(regdata$PlusMinRaw3_reg,20)
regdata$PlusMinRaw5_reg <- pmax(regdata$PlusMinRaw5_reg,-20)
regdata$PlusMinRaw5_reg <- pmin(regdata$PlusMinRaw5_reg,20)

regdata$rapmXmin <- regdata$RAPM5_total*regdata$MINPG_reg
regdata$rapmoffXmin <- regdata$RAPM5_off*regdata$MINPG_reg
regdata$rapmdefXmin <- regdata$RAPM5_def*regdata$MINPG_reg
regdata$rapmoffXexp <- regdata$RAPM5_off*regdata$Exp3_reg
regdata$rapmdefXexp <- regdata$RAPM5_def*regdata$Exp3_reg
regdata$Age_regSq <- regdata$Age_reg*regdata$Age_reg
regdata2 <- regdata[(regdata$SG.x>regdata$SF.x)|(regdata$SF.x>regdata$SG.x)|(regdata$PF.x>regdata$C.x)|(regdata$C.x>regdata$PF.x),]
regdata2$Guard_reg[(regdata2$SG.x>regdata2$SF.x)] <- 0
regdata2$Forward_reg[(regdata2$SG.x>regdata2$SF.x)] <- 1
regdata2$Center_reg[(regdata2$SG.x>regdata2$SF.x)] <- 0
regdata2$PlayerID[(regdata2$SG.x>regdata2$SF.x)] <- paste(regdata2$PlayerID[(regdata2$SG.x>regdata2$SF.x)],"_F",sep="")
regdata2$Guard_reg[(regdata2$SF.x>regdata2$SG.x)] <- 1
regdata2$Forward_reg[(regdata2$SF.x>regdata2$SG.x)] <- 0
regdata2$Center_reg[(regdata2$SF.x>regdata2$SG.x)] <- 0
regdata2$PlayerID[(regdata2$SF.x>regdata2$SG.x)] <- paste(regdata2$PlayerID[(regdata2$SF.x>regdata2$SG.x)],"_G",sep="")
regdata2$Guard_reg[(regdata2$PF.x>regdata2$C.x)] <- 0
regdata2$Forward_reg[(regdata2$PF.x>regdata2$C.x)] <- 0
regdata2$Center_reg[(regdata2$PF.x>regdata2$C.x)] <- 1
regdata2$PlayerID[(regdata2$PF.x>regdata2$C.x)] <- paste(regdata2$PlayerID[(regdata2$PF.x>regdata2$C.x)],"_C",sep="")
regdata2$Guard_reg[(regdata2$C.x>regdata2$PF.x)] <- 0
regdata2$Forward_reg[(regdata2$C.x>regdata2$PF.x)] <- 1
regdata2$Center_reg[(regdata2$C.x>regdata2$PF.x)] <- 0
regdata2$PlayerID[(regdata2$C.x>regdata2$PF.x)] <- paste(regdata2$PlayerID[(regdata2$C.x>regdata2$PF.x)],"_F",sep="")

#winsorize for players <2000 minutes over last 5 years, to avoid outliers
regdata$PositionIndicator <- ifelse(regdata$Guard_reg==1,"G",ifelse(regdata$Forward_reg==1,"F","C"))
regdata <- regdata %>%
  dplyr::group_by(Date,PositionIndicator) %>%
  mutate(PTSpm_regWin95 = quantile(PTSpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         ASTpm_regWin95 = quantile(ASTpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         STLpm_regWin95 = quantile(STLpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         BLKpm_regWin95 = quantile(BLKpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         TOpm_regWin95 = quantile(TOpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         OREBpm_regWin95 = quantile(OREBpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         DREBpm_regWin95 = quantile(DREBpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE),
         PFpm_regWin95 = quantile(PFpm_reg[MIN.x>=2000],c(.95),na.rm=TRUE)) %>% ungroup()
regdata$ImpactPlayspm_reg[regdata$MIN.x<2000] <- regdata$ImpactPlayspm_reg[regdata$MIN.x<2000] - (regdata$ASTpm_reg[regdata$MIN.x<2000] + regdata$STLpm_reg[regdata$MIN.x<2000] + regdata$BLKpm_reg[regdata$MIN.x<2000] + regdata$OREBpm_reg[regdata$MIN.x<2000] + regdata$DREBpm_reg[regdata$MIN.x<2000] + regdata$PFpm_reg[regdata$MIN.x<2000] + regdata$TOpm_reg[regdata$MIN.x<2000] -pmin(regdata$ASTpm_regWin95[regdata$MIN.x<2000],regdata$ASTpm_reg[regdata$MIN.x<2000])-pmin(regdata$STLpm_regWin95[regdata$MIN.x<2000],regdata$STLpm_reg[regdata$MIN.x<2000])-pmin(regdata$OREBpm_regWin95[regdata$MIN.x<2000],regdata$OREBpm_reg[regdata$MIN.x<2000])-pmin(regdata$DREBpm_regWin95[regdata$MIN.x<2000],regdata$DREBpm_reg[regdata$MIN.x<2000])-pmin(regdata$PFpm_regWin95[regdata$MIN.x<2000],regdata$PFpm_reg[regdata$MIN.x<2000])-pmin(regdata$BLKpm_regWin95[regdata$MIN.x<2000],regdata$BLKpm_reg[regdata$MIN.x<2000])-pmin(regdata$TOpm_regWin95[regdata$MIN.x<2000],regdata$TOpm_reg[regdata$MIN.x<2000]))
regdata$PTSpm_reg[regdata$MIN.x<2000] <- pmin(regdata$PTSpm_regWin95[regdata$MIN.x<2000],regdata$PTSpm_reg[regdata$MIN.x<2000])
regdata$ASTpm_reg[regdata$MIN.x<2000] <- pmin(regdata$ASTpm_regWin95[regdata$MIN.x<2000],regdata$ASTpm_reg[regdata$MIN.x<2000])
regdata$STLpm_reg[regdata$MIN.x<2000] <- pmin(regdata$STLpm_regWin95[regdata$MIN.x<2000],regdata$STLpm_reg[regdata$MIN.x<2000])
regdata$BLKpm_reg[regdata$MIN.x<2000] <- pmin(regdata$BLKpm_regWin95[regdata$MIN.x<2000],regdata$BLKpm_reg[regdata$MIN.x<2000])
regdata$TOpm_reg[regdata$MIN.x<2000] <- pmin(regdata$TOpm_regWin95[regdata$MIN.x<2000],regdata$TOpm_reg[regdata$MIN.x<2000])
regdata$OREBpm_reg[regdata$MIN.x<2000] <- pmin(regdata$OREBpm_regWin95[regdata$MIN.x<2000],regdata$OREBpm_reg[regdata$MIN.x<2000])
regdata$DREBpm_reg[regdata$MIN.x<2000] <- pmin(regdata$DREBpm_regWin95[regdata$MIN.x<2000],regdata$DREBpm_reg[regdata$MIN.x<2000])
regdata$PFpm_reg[regdata$MIN.x<2000] <- pmin(regdata$PFpm_regWin95[regdata$MIN.x<2000],regdata$PFpm_reg[regdata$MIN.x<2000])

regdata$STLpmSQ_reg <- regdata$STLpm_reg^2
regdata$ASTpmSQ_reg <- regdata$ASTpm_reg^2
regdata2$STLpmSQ_reg <- regdata2$STLpm_reg^2
regdata2$ASTpmSQ_reg <- regdata2$ASTpm_reg^2

regdata$LowMin <- ifelse(regdata$MIN.x<=2000,1,0)
regdata$LowMedMin <- ifelse(regdata$MIN.x>2000&regdata$MIN.x<=4000,1,0)
regdata2$LowMin <- ifelse(regdata2$MIN.x<=2000,1,0)
regdata2$LowMedMin <- ifelse(regdata2$MIN.x>2000&regdata2$MIN.x<=4000,1,0)

regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1] <- pmax(0.25,regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1])
regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Forward_reg==1] <- pmax(0.2,regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Forward_reg==1])
regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Center_reg==1] <- pmax(0.15,regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Center_reg==1])
regdata$NonShooterXThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1] <- regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1]*regdata$NonShooter_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1]
regdata$NonShooterXThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Forward_reg==1] <- regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Forward_reg==1]*regdata$NonShooter_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Forward_reg==1]
regdata$NonShooterXThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Center_reg==1] <- regdata$ThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Center_reg==1]*regdata$NonShooter_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Center_reg==1]
regdata$NonShooterXThreePct_reg[regdata$NonShooter_reg==1&!is.na(regdata$NonShooter_reg)&regdata$Guard_reg==1] <- 0

regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1] <- pmax(0.25,regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1])
regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Forward_reg==1] <- pmax(0.2,regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Forward_reg==1])
regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Center_reg==1] <- pmax(0.15,regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Center_reg==1])
regdata2$NonShooterXThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1] <- regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1]*regdata2$NonShooter_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1]
regdata2$NonShooterXThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Forward_reg==1] <- regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Forward_reg==1]*regdata2$NonShooter_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Forward_reg==1]
regdata2$NonShooterXThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Center_reg==1] <- regdata2$ThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Center_reg==1]*regdata2$NonShooter_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Center_reg==1]
regdata2$NonShooterXThreePct_reg[regdata2$NonShooter_reg==1&!is.na(regdata2$NonShooter_reg)&regdata2$Guard_reg==1] <- 0

regformulaOFF <- "MINPG_reg + Forward_reg + Center_reg + ThreePct_reg + 
TwoPct_reg + FTPct_reg + PTSpm_reg + OREBpm_reg + DREBpm_reg + ASTpm_reg + STLpm_reg + 
BLKpm_reg + TOpm_reg + PFpm_reg + ImpactPlayspm_reg + ThreePMpm_reg + ThreePApm_reg + TwoPApm_reg + 
TwoPMpm_reg + FTApm_reg + FTMperFG_reg + TSPct_reg + Age_reg + Age_regSq + Age24_reg + Age30_reg + 
AgeXAge24_reg + AgeXAge30_reg + Exp3_reg + DraftVal_reg + DraftValXExp3_reg + NonShooter_reg + 
TotalImpactChg_reg + ImpChgXDraftVal_reg + ImpChgXExp3_reg + ImpChgXExp3XDraftValue_reg +
NonShooterXThreePct_reg + RAPM5_off + rapmoffXmin + rapmoffXexp + PlusMinRaw5_reg + PlusMinRaw3_reg +PlusMinRaw1_reg
+ ThreePct_reg*Forward_reg + 
TwoPct_reg*Forward_reg + FTPct_reg*Forward_reg + PTSpm_reg*Forward_reg + OREBpm_reg*Forward_reg
+ DREBpm_reg*Forward_reg + ASTpm_reg*Forward_reg + STLpm_reg*Forward_reg + 
BLKpm_reg*Forward_reg + TOpm_reg*Forward_reg + PFpm_reg*Forward_reg + ImpactPlayspm_reg*Forward_reg + ThreePMpm_reg*Forward_reg
+ ThreePApm_reg*Forward_reg + TwoPApm_reg*Forward_reg + 
TwoPMpm_reg*Forward_reg + FTApm_reg*Forward_reg + FTMperFG_reg*Forward_reg + TSPct_reg*Forward_reg +
NonShooter_reg*Forward_reg + NonShooterXThreePct_reg*Forward_reg + 
RAPM5_off*Forward_reg
+ ThreePct_reg*Center_reg + 
TwoPct_reg*Center_reg + FTPct_reg*Center_reg + PTSpm_reg*Center_reg + OREBpm_reg*Center_reg
+ DREBpm_reg*Center_reg + ASTpm_reg*Center_reg + STLpm_reg*Center_reg + 
BLKpm_reg*Center_reg + TOpm_reg*Center_reg + PFpm_reg*Center_reg + ImpactPlayspm_reg*Center_reg + ThreePMpm_reg*Center_reg
+ ThreePApm_reg*Center_reg + TwoPApm_reg*Center_reg + 
TwoPMpm_reg*Center_reg + FTApm_reg*Center_reg + FTMperFG_reg*Center_reg + TSPct_reg*Center_reg +
NonShooter_reg*Center_reg + NonShooterXThreePct_reg*Center_reg + RAPM5_off*Center_reg
+ LowMin + LowMedMin + LowMin*Forward_reg + LowMedMin*Forward_reg + LowMin*Center_reg + LowMedMin*Center_reg"
regformulaDEF <- "MINPG_reg + Forward_reg + Center_reg + ThreePct_reg + 
TwoPct_reg + FTPct_reg + PTSpm_reg + OREBpm_reg + DREBpm_reg + ASTpm_reg + STLpm_reg + 
BLKpm_reg + TOpm_reg + PFpm_reg + ImpactPlayspm_reg + ThreePMpm_reg + ThreePApm_reg + TwoPApm_reg + 
TwoPMpm_reg + FTApm_reg + FTMperFG_reg + TSPct_reg + Age_reg + Age_regSq + Age24_reg + Age30_reg + 
AgeXAge24_reg + AgeXAge30_reg + Exp3_reg + DraftVal_reg + DraftValXExp3_reg + NonShooter_reg + 
TotalImpactChg_reg + ImpChgXDraftVal_reg + ImpChgXExp3_reg + ImpChgXExp3XDraftValue_reg + 
NonShooterXThreePct_reg + RAPM5_def + rapmdefXmin + rapmdefXexp + PlusMinRaw5_reg + PlusMinRaw3_reg +PlusMinRaw1_reg
+ ThreePct_reg*Forward_reg + 
TwoPct_reg*Forward_reg + FTPct_reg*Forward_reg + PTSpm_reg*Forward_reg + OREBpm_reg*Forward_reg
+ DREBpm_reg*Forward_reg + ASTpm_reg*Forward_reg + STLpm_reg*Forward_reg + 
BLKpm_reg*Forward_reg + TOpm_reg*Forward_reg + PFpm_reg*Forward_reg + ImpactPlayspm_reg*Forward_reg + ThreePMpm_reg*Forward_reg
+ ThreePApm_reg*Forward_reg + TwoPApm_reg*Forward_reg + 
TwoPMpm_reg*Forward_reg + FTApm_reg*Forward_reg + FTMperFG_reg*Forward_reg + TSPct_reg*Forward_reg +
NonShooter_reg*Forward_reg + NonShooterXThreePct_reg*Forward_reg + 
RAPM5_def*Forward_reg
+ ThreePct_reg*Center_reg + 
TwoPct_reg*Center_reg + FTPct_reg*Center_reg + PTSpm_reg*Center_reg + OREBpm_reg*Center_reg
+ DREBpm_reg*Center_reg + ASTpm_reg*Center_reg + STLpm_reg*Center_reg + 
BLKpm_reg*Center_reg + TOpm_reg*Center_reg + PFpm_reg*Center_reg + ImpactPlayspm_reg*Center_reg + ThreePMpm_reg*Center_reg
+ ThreePApm_reg*Center_reg + TwoPApm_reg*Center_reg + 
TwoPMpm_reg*Center_reg + FTApm_reg*Center_reg + FTMperFG_reg*Center_reg + TSPct_reg*Center_reg +
NonShooter_reg*Center_reg + NonShooterXThreePct_reg*Center_reg + RAPM5_def*Center_reg
+ LowMin + LowMedMin + LowMin*Forward_reg + LowMedMin*Forward_reg + LowMin*Center_reg + LowMedMin*Center_reg"

regformulaOFF <- str_replace_all(regformulaOFF,"\n","")
regformulaDEF <- str_replace_all(regformulaDEF,"\n","")

regformulaOFFv2 <- "MINPG_reg + Guard_reg + Forward_reg + Center_reg + ThreePct_reg*Guard_reg + 
TwoPct_reg*Guard_reg + FTPct_reg*Guard_reg + PTSpm_reg*Guard_reg + OREBpm_reg*Guard_reg + DREBpm_reg*Guard_reg + ASTpm_reg*Guard_reg + STLpm_reg*Guard_reg + 
BLKpm_reg*Guard_reg + TOpm_reg*Guard_reg + PFpm_reg*Guard_reg + ImpactPlayspm_reg*Guard_reg + ThreePMpm_reg*Guard_reg + ThreePApm_reg*Guard_reg + TwoPApm_reg*Guard_reg + 
TwoPMpm_reg*Guard_reg + FTApm_reg*Guard_reg + FTMperFG_reg*Guard_reg + TSPct_reg*Guard_reg + Age_reg + Age_regSq + Age24_reg + Age30_reg + 
AgeXAge24_reg + AgeXAge30_reg + Exp3_reg + DraftVal_reg + DraftValXExp3_reg + NonShooter_reg*Guard_reg + 
TotalImpactChg_reg + ImpChgXDraftVal_reg + ImpChgXExp3_reg + ImpChgXExp3XDraftValue_reg +
NonShooterXThreePct_reg*Guard_reg + RAPM5_off*Guard_reg + rapmoffXmin + rapmoffXexp + PlusMinRaw5_reg + PlusMinRaw3_reg +PlusMinRaw1_reg
+ ThreePct_reg*Forward_reg + 
TwoPct_reg*Forward_reg + FTPct_reg*Forward_reg + PTSpm_reg*Forward_reg + OREBpm_reg*Forward_reg
+ DREBpm_reg*Forward_reg + ASTpm_reg*Forward_reg + STLpm_reg*Forward_reg + 
BLKpm_reg*Forward_reg + TOpm_reg*Forward_reg + PFpm_reg*Forward_reg + ImpactPlayspm_reg*Forward_reg + ThreePMpm_reg*Forward_reg
+ ThreePApm_reg*Forward_reg + TwoPApm_reg*Forward_reg + 
TwoPMpm_reg*Forward_reg + FTApm_reg*Forward_reg + FTMperFG_reg*Forward_reg + TSPct_reg*Forward_reg +
NonShooter_reg*Forward_reg + NonShooterXThreePct_reg*Forward_reg + 
RAPM5_off*Forward_reg
+ ThreePct_reg*Center_reg + 
TwoPct_reg*Center_reg + FTPct_reg*Center_reg + PTSpm_reg*Center_reg + OREBpm_reg*Center_reg
+ DREBpm_reg*Center_reg + ASTpm_reg*Center_reg + STLpm_reg*Center_reg + 
BLKpm_reg*Center_reg + TOpm_reg*Center_reg + PFpm_reg*Center_reg + ImpactPlayspm_reg*Center_reg + ThreePMpm_reg*Center_reg
+ ThreePApm_reg*Center_reg + TwoPApm_reg*Center_reg + 
TwoPMpm_reg*Center_reg + FTApm_reg*Center_reg + FTMperFG_reg*Center_reg + TSPct_reg*Center_reg +
NonShooter_reg*Center_reg + NonShooterXThreePct_reg*Center_reg + RAPM5_off*Center_reg
+ LowMin*Guard_reg + LowMedMin*Guard_reg + LowMin*Forward_reg + LowMedMin*Forward_reg + LowMin*Center_reg + LowMedMin*Center_reg +0"
regformulaDEFv2 <- "MINPG_reg + Guard_reg + Forward_reg + Center_reg + ThreePct_reg*Guard_reg + 
TwoPct_reg*Guard_reg + FTPct_reg*Guard_reg + PTSpm_reg*Guard_reg + OREBpm_reg*Guard_reg + DREBpm_reg*Guard_reg + ASTpm_reg*Guard_reg + STLpm_reg*Guard_reg + 
BLKpm_reg*Guard_reg + TOpm_reg*Guard_reg + PFpm_reg*Guard_reg + ImpactPlayspm_reg*Guard_reg + ThreePMpm_reg*Guard_reg + ThreePApm_reg*Guard_reg + TwoPApm_reg*Guard_reg + 
TwoPMpm_reg*Guard_reg + FTApm_reg*Guard_reg + FTMperFG_reg*Guard_reg + TSPct_reg*Guard_reg + Age_reg + Age_regSq + Age24_reg + Age30_reg + 
AgeXAge24_reg + AgeXAge30_reg + Exp3_reg + DraftVal_reg + DraftValXExp3_reg + NonShooter_reg*Guard_reg + 
TotalImpactChg_reg + ImpChgXDraftVal_reg + ImpChgXExp3_reg + ImpChgXExp3XDraftValue_reg + 
NonShooterXThreePct_reg*Guard_reg + RAPM5_def*Guard_reg + rapmdefXmin + rapmdefXexp + PlusMinRaw5_reg + PlusMinRaw3_reg +PlusMinRaw1_reg
+ ThreePct_reg*Forward_reg + 
TwoPct_reg*Forward_reg + FTPct_reg*Forward_reg + PTSpm_reg*Forward_reg + OREBpm_reg*Forward_reg
+ DREBpm_reg*Forward_reg + ASTpm_reg*Forward_reg + STLpm_reg*Forward_reg + 
BLKpm_reg*Forward_reg + TOpm_reg*Forward_reg + PFpm_reg*Forward_reg + ImpactPlayspm_reg*Forward_reg + ThreePMpm_reg*Forward_reg
+ ThreePApm_reg*Forward_reg + TwoPApm_reg*Forward_reg + 
TwoPMpm_reg*Forward_reg + FTApm_reg*Forward_reg + FTMperFG_reg*Forward_reg + TSPct_reg*Forward_reg +
NonShooter_reg*Forward_reg + NonShooterXThreePct_reg*Forward_reg + 
RAPM5_def*Forward_reg
+ ThreePct_reg*Center_reg + 
TwoPct_reg*Center_reg + FTPct_reg*Center_reg + PTSpm_reg*Center_reg + OREBpm_reg*Center_reg
+ DREBpm_reg*Center_reg + ASTpm_reg*Center_reg + STLpm_reg*Center_reg + 
BLKpm_reg*Center_reg + TOpm_reg*Center_reg + PFpm_reg*Center_reg + ImpactPlayspm_reg*Center_reg + ThreePMpm_reg*Center_reg
+ ThreePApm_reg*Center_reg + TwoPApm_reg*Center_reg + 
TwoPMpm_reg*Center_reg + FTApm_reg*Center_reg + FTMperFG_reg*Center_reg + TSPct_reg*Center_reg +
NonShooter_reg*Center_reg + NonShooterXThreePct_reg*Center_reg + RAPM5_def*Center_reg
+ LowMin*Guard_reg + LowMedMin*Guard_reg + LowMin*Forward_reg + LowMedMin*Forward_reg + LowMin*Center_reg + LowMedMin*Center_reg +0"

regformulaOFFv2 <- str_replace_all(regformulaOFFv2,"\n","")
regformulaDEFv2 <- str_replace_all(regformulaDEFv2,"\n","")


#for players <500min (aka replacement players and all rookies)
regformulaOFF_alt <- "year"
regformulaDEF_alt <- "year"
regdata$year <- year(regdata$Date)-2002
regdata$yearsq <- regdata$year^2
regdata$FwdDate <- regdata$Date+364

regdata <- regdata %>% left_join(dplyr::rename(regdata[,c("PlayerID","Date","RAPM1_off","RAPM1_def")],FwdDate=Date,RAPM1_offFWD=RAPM1_off,RAPM1_defFWD=RAPM1_def),by=c("PlayerID","FwdDate"))

# regdata_test <-dplyr::select(regdata[regdata$Date<=x&regdata$MIN.x>500,],RAPM1_offFWD,MINPG_reg,Forward_reg,Center_reg,ThreePct_reg,TwoPct_reg,FTPct_reg,PTSpm_reg,OREBpm_reg,DREBpm_reg,ASTpm_reg,STLpm_reg,BLKpm_reg,TOpm_reg,ImpactPlayspm_reg,ThreePMpm_reg,ThreePApm_reg,TwoPApm_reg,TwoPMpm_reg,FTApm_reg,FTMperFG_reg,TSPct_reg,Age_reg,Age_regSq,Age24_reg,Age30_reg,AgeXAge24_reg,AgeXAge30_reg,Exp3_reg,DraftVal_reg,DraftValXExp3_reg,NonShooter_reg,TotalImpactChg_reg,ImpChgXDraftVal_reg,ImpChgXExp3_reg,ImpChgXExp3XDraftValue_reg,NonShooterXThreePct_reg,RAPM5_off,rapmoffXmin,rapmoffXexp,PlusMinRaw5_reg,PlusMinRaw3_reg,PlusMinRaw1_reg,ThreePct_reg,TwoPct_reg,FTPct_reg,PTSpm_reg,OREBpm_reg,DREBpm_reg,ASTpm_reg,STLpm_reg,BLKpm_reg,TOpm_reg,ImpactPlayspm_reg,ThreePMpm_reg,ThreePApm_reg,TwoPApm_reg,TwoPMpm_reg,FTApm_reg,FTMperFG_reg,TSPct_reg,NonShooter_reg,NonShooterXThreePct_reg,RAPM5_off,ThreePct_reg,TwoPct_reg,FTPct_reg,PTSpm_reg,OREBpm_reg,DREBpm_reg,ASTpm_reg,STLpm_reg,BLKpm_reg,TOpm_reg,ImpactPlayspm_reg,ThreePMpm_reg,ThreePApm_reg,TwoPApm_reg,TwoPMpm_reg,FTApm_reg,FTMperFG_reg,TSPct_reg,NonShooter_reg,NonShooterXThreePct_reg,RAPM5_off,year)
regdata$Month <- month(regdata$Date)
regdata$Day <- day(regdata$Date)
regdata$Year <- year(regdata$Date)
regdata2$Month <- month(regdata2$Date)
regdata2$Day <- day(regdata2$Date)
regdata2$Year <- year(regdata2$Date)
regdata$Season <- ifelse((regdata$Month>=10&regdata$Day>=12)|regdata$Month>=11,paste(regdata$Year,"-",substr(regdata$Year+1,3,4),sep=""),paste(regdata$Year-1,"-",substr(regdata$Year,3,4),sep=""))
regdata2$Season <- ifelse((regdata2$Month>=10&regdata2$Day>=12)|regdata2$Month>=11,paste(regdata2$Year,"-",substr(regdata2$Year+1,3,4),sep=""),paste(regdata2$Year-1,"-",substr(regdata2$Year,3,4),sep=""))
regdata <- regdata %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = group_indices()) %>% ungroup()
regdata2 <- regdata2 %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = group_indices()) %>% ungroup()
regformulaOFF <- paste("RAPM1_offFWD ~ ",regformulaOFF,sep="")
regformulaDEF <- paste("RAPM1_defFWD ~ ",regformulaDEF,sep="")
regformulaOFF_alt <- paste("RAPM1_offFWD ~ ",regformulaOFF_alt,sep="")
regformulaDEF_alt <- paste("RAPM1_defFWD ~ ",regformulaDEF_alt,sep="")
regformulaOFF <- paste(regformulaOFF," + factor(SeasNum)",sep="")
regformulaDEF <- paste(regformulaDEF," + factor(SeasNum)",sep="")
regformulaOFF_alt <- paste(regformulaOFF_alt," + factor(SeasNum)",sep="")
regformulaDEF_alt <- paste(regformulaDEF_alt," + factor(SeasNum)",sep="")
regformulaOFF
regformulaDEF
regformulaOFF_alt
regformulaDEF_alt
regformulaOFFv2 <- paste("RAPM1_offFWD ~ ",regformulaOFFv2,sep="")
regformulaDEFv2 <- paste("RAPM1_defFWD ~ ",regformulaDEFv2,sep="")
regformulaOFFv2 <- paste(regformulaOFFv2," + factor(SeasNum)",sep="")
regformulaDEFv2 <- paste(regformulaDEFv2," + factor(SeasNum)",sep="")


regdata <- regdata[regdata$Date>"2004-10-01",]

regdata <- do.call(data.frame,                      # Replace Inf in data by NA
                       lapply(regdata,
                              function(x) replace(x, is.infinite(x), NA)))
regdata <- do.call(data.frame,                      # Replace Inf in data by NA
                       lapply(regdata,
                              function(x) replace(x, is.nan(x), NA)))
regdata$year[regdata$year==max(regdata$year)] <- max(regdata$year)-1
regdata$SeasNum[regdata$SeasNum==max(regdata$SeasNum)] <- max(regdata$SeasNum)-1
regdata2$year <- year(regdata2$Date)-2002
regdata2$year[regdata2$year==max(regdata2$year)] <- max(regdata2$year)-1
regdata2$SeasNum[regdata2$SeasNum==max(regdata2$SeasNum)] <- max(regdata2$SeasNum)-1
regdata$SeasNum[regdata$SeasNum==1|regdata$SeasNum==2] <- 3
regdata2$SeasNum[regdata2$SeasNum==1|regdata2$SeasNum==2] <- 3
regdata$wgt <- ifelse(regdata$MIN.x>=10000,2,ifelse(regdata$MIN.x<10000&regdata$MIN.x>=8000,2,ifelse(regdata$MIN.x<8000&regdata$MIN.x>=6000,2,ifelse(regdata$MIN.x<6000&regdata$MIN.x>=4000,2,ifelse(regdata$MIN.x<6000&regdata$MIN.x>=4000,2,ifelse(regdata$MIN.x<4000&regdata$MIN.x>=1500,1.5,1))))))
regdata2$wgt <- ifelse(regdata2$MIN.x>=10000,2,ifelse(regdata2$MIN.x<10000&regdata2$MIN.x>=8000,2,ifelse(regdata2$MIN.x<8000&regdata2$MIN.x>=6000,2,ifelse(regdata2$MIN.x<6000&regdata2$MIN.x>=4000,2,ifelse(regdata2$MIN.x<6000&regdata2$MIN.x>=4000,2,ifelse(regdata2$MIN.x<4000&regdata2$MIN.x>=1500,1.5,1))))))

#some ajdustments for positional anomolies
regadd <- regdata[regdata$Players=="L. Doncic"|regdata$Players=="A. Edwards",]
regadd$Guard_reg[regadd$Players=="L. Doncic"] <- 0
regadd$Forward_reg[regadd$Players=="L. Doncic"] <- 1
regadd$Guard_reg[regadd$Players=="A. Edwards"] <- 1
regadd$Forward_reg[regadd$Players=="A. Edwards"] <- 0

regdata <- rbind(regdata,regadd)

regdata <- regdata %>%
  group_by(Date) %>%
  mutate(RAPM1_defFWD1 = quantile(RAPM1_defFWD,c(.005),na.rm=TRUE),
         RAPM1_defFWD99 = quantile(RAPM1_defFWD,c(.995),na.rm=TRUE),
         RAPM1_offFWD1 = quantile(RAPM1_offFWD,c(.005),na.rm=TRUE),
         RAPM1_offFWD99 = quantile(RAPM1_offFWD,c(.995),na.rm=TRUE))
regdata$RAPM1_defFWD <- pmin(regdata$RAPM1_defFWD,regdata$RAPM1_defFWD99)
regdata$RAPM1_defFWD <- pmax(regdata$RAPM1_defFWD,regdata$RAPM1_defFWD1)
regdata$RAPM1_offFWD <- pmin(regdata$RAPM1_offFWD,regdata$RAPM1_offFWD99)
regdata$RAPM1_offFWD <- pmax(regdata$RAPM1_offFWD,regdata$RAPM1_offFWD1)

regfunction <- function(x) {
  model_offreg <- lm(formula = regformulaOFF, data = regdata[regdata$Date<=x&regdata$MIN.x>500&regdata$Date>="2006-10-01",],weights=wgt)
  players_pred1 <- regdata[regdata$Date<=x&regdata$MIN.x>500,c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN")]
  players_pred1$VellosoPlusMinus_off <- predict(model_offreg,newdata = regdata[regdata$Date<=x&regdata$MIN.x>500,])
  model_defreg <- lm(formula = regformulaDEF, data = regdata[regdata$Date<=x&regdata$MIN.x>500&regdata$Date>="2006-10-01",],weights=wgt)
  players_pred2 <- regdata[regdata$Date<=x&regdata$MIN.x>500,c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN")]
  players_pred2$VellosoPlusMinus_def <- predict(model_defreg,newdata = regdata[regdata$Date<=x&regdata$MIN.x>500,])
  players_pred1 <- players_pred1 %>% left_join(players_pred2,by=c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN"))
  model_offreg2 <- lm(formula = regformulaOFF_alt, data = regdata[regdata$Date<=x&regdata$MIN.x<=500&regdata$Date>="2006-10-01",])
  players_pred3 <- regdata[regdata$Date<=x&regdata$MIN.x<=500,c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN")]
  players_pred3$VellosoPlusMinus_off <- predict(model_offreg2,newdata = regdata[regdata$Date<=x&regdata$MIN.x<=500,])
  model_defreg2 <- lm(formula = regformulaDEF_alt, data = regdata[regdata$Date<=x&regdata$MIN.x<=500&regdata$Date>="2006-10-01",])
  players_pred4 <- regdata[regdata$Date<=x&regdata$MIN.x<=500,c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN")]
  players_pred4$VellosoPlusMinus_def <- predict(model_defreg2,newdata = regdata[regdata$Date<=x&regdata$MIN.x<=500,])
  players_pred3 <- players_pred3 %>% left_join(players_pred4,by=c("PlayerID","Players","PlayersFull","Date","Teams","MIN.x","MIN"))
  players_pred <- dplyr::bind_rows(players_pred1,players_pred3)
  return(players_pred)
}
numCores=detectCores()
tic()
datevec <- max(as.Date(unique(regdata$Date),origin="1970-01-01"))
players_predALL <- mclapply(datevec, regfunction,mc.cores = numCores/2)
players_predALL <- dplyr::bind_rows(players_predALL, .id = "column_label")
toc()
#adjust for pace
players_predALL$VellosoPlusMinus_total <- players_predALL$VellosoPlusMinus_off + players_predALL$VellosoPlusMinus_def
players_predALL <- players_predALL %>% left_join(dplyr::select(rollingboxALL,PlayerID,Date,RAPM_Mult),by=c("PlayerID","Date"))
players_predALL$VellPMOff_PaceAdj <- ifelse(is.na(players_predALL$RAPM_Mult),players_predALL$VellosoPlusMinus_off,players_predALL$VellosoPlusMinus_off*players_predALL$RAPM_Mult)
players_predALL$VellPMDef_PaceAdj <- ifelse(is.na(players_predALL$RAPM_Mult),players_predALL$VellosoPlusMinus_def,players_predALL$VellosoPlusMinus_def*players_predALL$RAPM_Mult)
players_predALL <- players_predALL %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(Repl_off = mean(VellPMOff_PaceAdj[MIN.x<=500],na.rm=TRUE),
         Repl_def = mean(VellPMDef_PaceAdj[MIN.x<=500],na.rm=TRUE))

#finish adjustments for positional anomolies like Luka
players_predALL <- players_predALL %>%
  dplyr::group_by(Players,Teams,Date) %>%
  dplyr::mutate(VellosoPlusMinus_off = (2/3)*dplyr::first(VellosoPlusMinus_off)+(1/3)*dplyr::last(VellosoPlusMinus_off),
                VellosoPlusMinus_def = (2/3)*dplyr::first(VellosoPlusMinus_def)+(1/3)*dplyr::last(VellosoPlusMinus_def),
                VellosoPlusMinus_total = (2/3)*dplyr::first(VellosoPlusMinus_total)+(1/3)*dplyr::last(VellosoPlusMinus_total),
                VellPMOff_PaceAdj = (2/3)*dplyr::first(VellPMOff_PaceAdj)+(1/3)*dplyr::last(VellPMOff_PaceAdj),
                VellPMDef_PaceAdj = (2/3)*dplyr::first(VellPMDef_PaceAdj)+(1/3)*dplyr::last(VellPMDef_PaceAdj))
players_predALL <- distinct(players_predALL,.keep_all = TRUE)

vellplusmin <- players_predALL
vellplusmin <- vellplusmin %>%
  dplyr::group_by(Date) %>%
  mutate(   VellosoPlusMinus_offNorm = (VellosoPlusMinus_off-mean(VellosoPlusMinus_off,na.rm=TRUE))/sd(VellosoPlusMinus_off,na.rm=TRUE),
            VellosoPlusMinus_defNorm = (VellosoPlusMinus_def-mean(VellosoPlusMinus_def,na.rm=TRUE))/sd(VellosoPlusMinus_def,na.rm=TRUE),
            VellosoPlusMinus_totalNorm = (VellosoPlusMinus_total-mean(VellosoPlusMinus_total,na.rm=TRUE))/sd(VellosoPlusMinus_total,na.rm=TRUE),
            VellPMOff_PaceAdjNorm = (VellPMOff_PaceAdj-mean(VellPMOff_PaceAdj,na.rm=TRUE))/sd(VellPMOff_PaceAdj,na.rm=TRUE),
            VellPMDef_PaceAdjNorm = (VellPMDef_PaceAdj-mean(VellPMDef_PaceAdj,na.rm=TRUE))/sd(VellPMDef_PaceAdj,na.rm=TRUE))

repl_values <- vellplusmin %>%
  dplyr::group_by(Date) %>%
  dplyr::summarize(Repl_offExp3 = min(VellPMOff_PaceAdj[MIN.x<=500],na.rm=TRUE),
         Repl_defExp3 = min(VellPMDef_PaceAdj[MIN.x<=500],na.rm=TRUE),
         Repl_off = max(VellPMOff_PaceAdj[MIN.x<=500],na.rm=TRUE),
         Repl_def = max(VellPMDef_PaceAdj[MIN.x<=500],na.rm=TRUE))


model_reploff <- lm(RAPM1_offFWD ~ factor(SeasNum),regdata,weights=MIN)
model_repldef <- lm(RAPM1_defFWD ~ factor(SeasNum),regdata,weights=MIN)
repl_values$Month <- month(repl_values$Date)
repl_values$Day <- day(repl_values$Date)
repl_values$Year <- year(repl_values$Date)

repl_values$Season <- ifelse((repl_values$Month>=10&repl_values$Day>=12)|repl_values$Month>=11,paste(repl_values$Year,"-",substr(repl_values$Year+1,3,4),sep=""),paste(repl_values$Year-1,"-",substr(repl_values$Year,3,4),sep=""))
repl_values <- repl_values %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = group_indices()) %>% ungroup()
repl_values$SeasNum[repl_values$SeasNum==max(repl_values$SeasNum)] <- max(repl_values$SeasNum)-1
repl_values$SeasNum[repl_values$SeasNum==1|repl_values$SeasNum==2] <- 3
repl_values$OffAvg <- predict(model_reploff,newdata=repl_values)
repl_values$DefAvg <- predict(model_repldef,newdata=repl_values)
repl_values$Repl_offExp3v2 <- repl_values$Repl_offExp3-repl_values$OffAvg
repl_values$Repl_defExp3v2 <- repl_values$Repl_defExp3-repl_values$DefAvg
repl_values$Repl_offv2 <- repl_values$Repl_off-repl_values$OffAvg
repl_values$Repl_defv2 <- repl_values$Repl_def-repl_values$DefAvg
repl_values$Repl_offExp3v2 <- mean(repl_values$Repl_offExp3v2)
repl_values$Repl_defExp3v2 <- mean(repl_values$Repl_defExp3v2)
repl_values$Repl_offv2 <- mean(repl_values$Repl_offv2)
repl_values$Repl_defv2 <- mean(repl_values$Repl_defv2)
repl_values$Repl_offExp3v2 <- repl_values$Repl_offExp3v2+repl_values$OffAvg
repl_values$Repl_defExp3v2 <- repl_values$Repl_defExp3v2+repl_values$DefAvg
repl_values$Repl_offv2 <- repl_values$Repl_offv2+repl_values$OffAvg
repl_values$Repl_defv2 <- repl_values$Repl_defv2+repl_values$DefAvg
ma <- function(x, n = 17){stats::filter(x, rep(1 / n, n), sides = 1)}
repl_values$Repl_offExp3v3 <- ma(repl_values$Repl_offExp3v2,17)
repl_values$Repl_defExp3v3 <- ma(repl_values$Repl_defExp3v2,17)
repl_values$Repl_offv3 <- ma(repl_values$Repl_offv2,17)
repl_values$Repl_defv3 <- ma(repl_values$Repl_defv2,17)

repl_values$Repl_offExp3 <- repl_values$Repl_offExp3v3
repl_values$Repl_defExp3 <- repl_values$Repl_defExp3v3
repl_values$Repl_off <- repl_values$Repl_offv3
repl_values$Repl_def <- repl_values$Repl_defv3
repl_values <- repl_values[,c(1:5)]
repl_values$Repl_offExp3[1:16] <- -1.2559553
repl_values$Repl_defExp3[1:16] <- 0.292233660
repl_values$Repl_off[1:16] <- -0.9725298
repl_values$Repl_def[1:16] <- 0.42872762

repl_values2 <- vellplusmin %>%
  dplyr::group_by(Date) %>%
  dplyr::summarize(Repl_offExp3 = min(VellPMOff_PaceAdjNorm[MIN.x<=500],na.rm=TRUE),
                   Repl_defExp3 = min(VellPMDef_PaceAdjNorm[MIN.x<=500],na.rm=TRUE),
                   Repl_off = max(VellPMOff_PaceAdjNorm[MIN.x<=500],na.rm=TRUE),
                   Repl_def = max(VellPMDef_PaceAdjNorm[MIN.x<=500],na.rm=TRUE))


model_reploff <- lm(RAPM1_offFWD ~ factor(SeasNum),regdata,weights=MIN)
model_repldef <- lm(RAPM1_defFWD ~ factor(SeasNum),regdata,weights=MIN)
repl_values2$Month <- month(repl_values2$Date)
repl_values2$Day <- day(repl_values2$Date)
repl_values2$Year <- year(repl_values2$Date)

repl_values2$Season <- ifelse((repl_values2$Month>=10&repl_values2$Day>=12)|repl_values2$Month>=11,paste(repl_values2$Year,"-",substr(repl_values2$Year+1,3,4),sep=""),paste(repl_values2$Year-1,"-",substr(repl_values2$Year,3,4),sep=""))
repl_values2 <- repl_values2 %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = group_indices()) %>% ungroup()
repl_values2$SeasNum[repl_values2$SeasNum==max(repl_values2$SeasNum)] <- max(repl_values2$SeasNum)-1
repl_values2$SeasNum[repl_values2$SeasNum==1|repl_values2$SeasNum==2] <- 3
repl_values2$OffAvg <- predict(model_reploff,newdata=repl_values2)
repl_values2$DefAvg <- predict(model_repldef,newdata=repl_values2)
repl_values2$Repl_offExp3v2 <- repl_values2$Repl_offExp3-repl_values2$OffAvg
repl_values2$Repl_defExp3v2 <- repl_values2$Repl_defExp3-repl_values2$DefAvg
repl_values2$Repl_offv2 <- repl_values2$Repl_off-repl_values2$OffAvg
repl_values2$Repl_defv2 <- repl_values2$Repl_def-repl_values2$DefAvg
repl_values2$Repl_offExp3v2 <- mean(repl_values2$Repl_offExp3v2)
repl_values2$Repl_defExp3v2 <- mean(repl_values2$Repl_defExp3v2)
repl_values2$Repl_offv2 <- mean(repl_values2$Repl_offv2)
repl_values2$Repl_defv2 <- mean(repl_values2$Repl_defv2)
repl_values2$Repl_offExp3v2 <- repl_values2$Repl_offExp3v2+repl_values2$OffAvg
repl_values2$Repl_defExp3v2 <- repl_values2$Repl_defExp3v2+repl_values2$DefAvg
repl_values2$Repl_offv2 <- repl_values2$Repl_offv2+repl_values2$OffAvg
repl_values2$Repl_defv2 <- repl_values2$Repl_defv2+repl_values2$DefAvg
ma <- function(x, n = 17){stats::filter(x, rep(1 / n, n), sides = 1)}
repl_values2$Repl_offExp3v3 <- ma(repl_values2$Repl_offExp3v2,17)
repl_values2$Repl_defExp3v3 <- ma(repl_values2$Repl_defExp3v2,17)
repl_values2$Repl_offv3 <- ma(repl_values2$Repl_offv2,17)
repl_values2$Repl_defv3 <- ma(repl_values2$Repl_defv2,17)

repl_values2$Repl_offExp3 <- repl_values2$Repl_offExp3v3
repl_values2$Repl_defExp3 <- repl_values2$Repl_defExp3v3
repl_values2$Repl_off <- repl_values2$Repl_offv3
repl_values2$Repl_def <- repl_values2$Repl_defv3
repl_values2 <- repl_values2[,c(1:5)]
repl_values2$Repl_offExp3[1:16] <- -0.23346773
repl_values2$Repl_defExp3[1:16] <- 0.16284262
repl_values2$Repl_off[1:16] <- -0.0242653375
repl_values2$Repl_def[1:16] <- 0.315292932
names(repl_values2) <- c("Date","Repl_offExp3Norm","Repl_defExp3Norm","Repl_offNorm","Repl_defNorm")

repl_values <- repl_values %>% left_join(repl_values2,by=c("Date")) %>% ungroup()

save(vellplusmin,file="vellplusmin.RData")
save(repl_values,file="repl_values.RData")

currentranks <- vellplusmin[vellplusmin$Date==datevec&vellplusmin$MIN>100&!is.na(vellplusmin$MIN>100),]
currentranks <- left_join(currentranks,rollingboxALL[,c("PlayerID","Date","MIN.y")]) %>% ungroup()
currentranks <- currentranks[!((currentranks$MIN.y==currentranks$MIN)&(currentranks$MIN.x!=currentranks$MIN.y)),]
currentranksTOT <- currentranks[order(-currentranks$VellosoPlusMinus_total),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_total")]
currentranksOFF <- currentranks[order(-currentranks$VellosoPlusMinus_off),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_off")]
currentranksDEF <- currentranks[order(-currentranks$VellosoPlusMinus_def),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_def")]
currentranksTOT$Rank_tot <- seq.int(nrow((currentranksTOT)))
currentranksOFF$Rank_off <- seq.int(nrow((currentranksOFF)))
currentranksDEF$Rank_def <- seq.int(nrow((currentranksDEF)))
currentranksTOT <- currentranksTOT %>% left_join(currentranksOFF,by=c("PlayerID","PlayersFull","MIN.x","MIN","Teams")) %>% left_join(currentranksDEF,by=c("PlayerID","PlayersFull","MIN.x","MIN","Teams"))
currentranks2 <- vellplusmin[vellplusmin$Date=="2022-06-27"&vellplusmin$MIN>100&!is.na(vellplusmin$MIN>100),]
currentranksTOT2 <- currentranks2[order(-currentranks2$VellosoPlusMinus_total),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_total")]
currentranksOFF2 <- currentranks2[order(-currentranks2$VellosoPlusMinus_off),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_off")]
currentranksDEF2 <- currentranks2[order(-currentranks2$VellosoPlusMinus_def),c("PlayersFull","PlayerID","MIN.x","MIN","Teams","VellosoPlusMinus_def")]
currentranksTOT2$Rank_tot <- seq.int(nrow((currentranksTOT2)))
currentranksOFF2$Rank_off <- seq.int(nrow((currentranksOFF2)))
currentranksDEF2$Rank_def <- seq.int(nrow((currentranksDEF2)))
currentranksTOT2 <- currentranksTOT2 %>% left_join(currentranksOFF2,by=c("PlayerID","PlayersFull","MIN.x","MIN","Teams")) %>% left_join(currentranksDEF2,by=c("PlayerID","PlayersFull","MIN.x","MIN","Teams"))
currentranksTOT2 <- currentranksTOT2[,c("VellosoPlusMinus_total","VellosoPlusMinus_off","VellosoPlusMinus_def","PlayerID")]
currentranksTOT2 <- dplyr::rename(currentranksTOT2,VellPMTot_YearStart=VellosoPlusMinus_total,VellPMOff_YearStart=VellosoPlusMinus_off,VellPMDef_YearStart=VellosoPlusMinus_def)
currentranksTOT <- left_join(currentranksTOT,currentranksTOT2,by=c("PlayerID")) %>% ungroup()
currentranksTOT$VellPMOff_YearStart[is.na(currentranksTOT$VellPMOff_YearStart)] <- -0.4750624
currentranksTOT$VellPMDef_YearStart[is.na(currentranksTOT$VellPMDef_YearStart)] <- -0.637358428
currentranksTOT$VellPMTot_YearStart[is.na(currentranksTOT$VellPMTot_YearStart)] <- currentranksTOT$VellPMOff_YearStart[is.na(currentranksTOT$VellPMTot_YearStart)] + currentranksTOT$VellPMDef_YearStart[is.na(currentranksTOT$VellPMTot_YearStart)]
currentranksTOT$YTDChgTot <- currentranksTOT$VellosoPlusMinus_total-currentranksTOT$VellPMTot_YearStart
currentranksTOT$YTDChgOff <- currentranksTOT$VellosoPlusMinus_off-currentranksTOT$VellPMOff_YearStart
currentranksTOT$YTDChgDef <- currentranksTOT$VellosoPlusMinus_def-currentranksTOT$VellPMDef_YearStart
currentranksTOT <- left_join(currentranksTOT,regdata[regdata$Date==max(regdata$Date),c("PlayersFull","RAPM5_total","RAPM5_off","RAPM5_def")])
save(currentranksTOT,file="currentranksTOT.RData")

regdata <- regdata %>% left_join(vellplusmin[,c("PlayerID","Date","VellosoPlusMinus_off","VellosoPlusMinus_def")],by=c("PlayerID","Date"))
regdata$off_error <- regdata$VellosoPlusMinus_off-regdata$RAPM1_offFWD
regdata$def_error <- regdata$VellosoPlusMinus_def-regdata$RAPM1_defFWD
#analyze model
model_off <- lm(formula = regformulaOFF, data = regdata[regdata$Date<=datevec&regdata$MIN.x>500,],weights=wgt)
model_def <- lm(formula = regformulaDEF, data = regdata[regdata$Date<=datevec&regdata$MIN.x>500,],weights=wgt)
summary(model_off)
summary(model_def)

regdata2$Off <- predict(model_off,regdata2)
regdata2$Def <- predict(model_def,regdata2)
regdata2$Tot <- regdata2$Off + regdata2$Def
altpos <- regdata2[regdata2$Date==max(regdata2$Date),c("PlayersFull","PlayerID","Off","Def","Tot")]
altpos <- rbind(altpos,dplyr::select(dplyr::rename(currentranksTOT,Off=VellosoPlusMinus_off,Def=VellosoPlusMinus_def,Tot=VellosoPlusMinus_total),c("PlayersFull","PlayerID","Off","Def","Tot")))

dfchart <- regdata[,c("PlayerID","PlayersFull","Date","MIN.x","MIN","MINPG_reg")] %>% left_join(vellplusmin[,c("PlayerID","Date","VellosoPlusMinus_total","VellosoPlusMinus_off","VellosoPlusMinus_def")],by=c("PlayerID","Date"))
dfchart <- left_join(dfchart,rollingboxALL[,c("PlayerID","Date","MIN.y")]) %>% ungroup()
dfchart <- dfchart[!((dfchart$MIN.y==dfchart$MIN)&(dfchart$MIN.x!=dfchart$MIN.y)),]
dfchart$MIN[dfchart$PlayerID=="440"&dfchart$Date>"2009-10-01"] <- 0
dfchart <- dfchart %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(maxRAPM = max(VellosoPlusMinus_total[MIN>1000],na.rm=TRUE),
         wasmax = as.numeric(VellosoPlusMinus_total==maxRAPM),
         maxRAPM2 = max(VellosoPlusMinus_off[MIN>1000],na.rm=TRUE),
         wasmax2 = as.numeric(VellosoPlusMinus_off==maxRAPM2),
         maxRAPM3 = max(VellosoPlusMinus_def[MIN>1000],na.rm=TRUE),
         wasmax3 = as.numeric(VellosoPlusMinus_def==maxRAPM3)) %>% 
  ungroup() %>%
  dplyr::group_by(PlayerID) %>%
  dplyr::mutate(playermax = sum(wasmax,na.rm=TRUE),
         playermax2 = sum(wasmax2,na.rm=TRUE),
         playermax3 = sum(wasmax3,na.rm=TRUE))

dfchart <- dfchart[!is.na(dfchart$VellosoPlusMinus_total),]

dfchart <- dfchart %>%
  dplyr::group_by(PlayerID) %>%
  dplyr::mutate(maxforplayer = max(VellosoPlusMinus_total[MIN>1000],na.rm=TRUE),
         ismaxforplayer = as.numeric(VellosoPlusMinus_total==maxforplayer),
          maxforplayer2 = max(Date[MIN>1000],na.rm=TRUE),
            ismaxforplayer2 = as.numeric(Date==maxforplayer2))

dfchart <- dfchart[dfchart$MIN>=250,]

test <- dfchart[dfchart$MIN.x>=5000&dfchart$MINPG_reg>=20,] %>% 
  dplyr::group_by(Date) %>% 
  arrange(VellosoPlusMinus_total) %>%
  dplyr::summarize(VellosoPlusMinus_total1 = VellosoPlusMinus_total[n()],
                   VellosoPlusMinus_total2 = VellosoPlusMinus_total[n()-1],
                   VellosoPlusMinus_total3 = VellosoPlusMinus_total[n()-2],
                   VellosoPlusMinus_total4 = VellosoPlusMinus_total[n()-3],
                   VellosoPlusMinus_total5 = VellosoPlusMinus_total[n()-4])
test <- left_join(test,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_total")],PlayersFull1=PlayersFull,VellosoPlusMinus_total1=VellosoPlusMinus_total),by=c("VellosoPlusMinus_total1")) %>% ungroup()
test <- left_join(test,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_total")],PlayersFull2=PlayersFull,VellosoPlusMinus_total2=VellosoPlusMinus_total),by=c("VellosoPlusMinus_total2")) %>% ungroup()
test <- left_join(test,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_total")],PlayersFull3=PlayersFull,VellosoPlusMinus_total3=VellosoPlusMinus_total),by=c("VellosoPlusMinus_total3")) %>% ungroup()
test <- left_join(test,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_total")],PlayersFull4=PlayersFull,VellosoPlusMinus_total4=VellosoPlusMinus_total),by=c("VellosoPlusMinus_total4")) %>% ungroup()
test <- left_join(test,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_total")],PlayersFull5=PlayersFull,VellosoPlusMinus_total5=VellosoPlusMinus_total),by=c("VellosoPlusMinus_total5")) %>% ungroup()

testOFF <- dfchart[dfchart$MIN.x>=5000&dfchart$MINPG_reg>=20,] %>% 
  dplyr::group_by(Date) %>% 
  arrange(VellosoPlusMinus_off) %>%
  dplyr::summarize(VellosoPlusMinus_off1 = VellosoPlusMinus_off[n()],
                   VellosoPlusMinus_off2 = VellosoPlusMinus_off[n()-1],
                   VellosoPlusMinus_off3 = VellosoPlusMinus_off[n()-2],
                   VellosoPlusMinus_off4 = VellosoPlusMinus_off[n()-3],
                   VellosoPlusMinus_off5 = VellosoPlusMinus_off[n()-4])
testOFF <- left_join(testOFF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_off")],PlayersFull1=PlayersFull,VellosoPlusMinus_off1=VellosoPlusMinus_off),by=c("VellosoPlusMinus_off1")) %>% ungroup()
testOFF <- left_join(testOFF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_off")],PlayersFull2=PlayersFull,VellosoPlusMinus_off2=VellosoPlusMinus_off),by=c("VellosoPlusMinus_off2")) %>% ungroup()
testOFF <- left_join(testOFF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_off")],PlayersFull3=PlayersFull,VellosoPlusMinus_off3=VellosoPlusMinus_off),by=c("VellosoPlusMinus_off3")) %>% ungroup()
testOFF <- left_join(testOFF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_off")],PlayersFull4=PlayersFull,VellosoPlusMinus_off4=VellosoPlusMinus_off),by=c("VellosoPlusMinus_off4")) %>% ungroup()
testOFF <- left_join(testOFF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_off")],PlayersFull5=PlayersFull,VellosoPlusMinus_off5=VellosoPlusMinus_off),by=c("VellosoPlusMinus_off5")) %>% ungroup()

testDEF <- dfchart[dfchart$MIN.x>=5000&dfchart$MINPG_reg>=20,] %>% 
  dplyr::group_by(Date) %>% 
  arrange(VellosoPlusMinus_def) %>%
  dplyr::summarize(VellosoPlusMinus_def1 = VellosoPlusMinus_def[n()],
                   VellosoPlusMinus_def2 = VellosoPlusMinus_def[n()-1],
                   VellosoPlusMinus_def3 = VellosoPlusMinus_def[n()-2],
                   VellosoPlusMinus_def4 = VellosoPlusMinus_def[n()-3],
                   VellosoPlusMinus_def5 = VellosoPlusMinus_def[n()-4])
testDEF <- left_join(testDEF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_def")],PlayersFull1=PlayersFull,VellosoPlusMinus_def1=VellosoPlusMinus_def),by=c("VellosoPlusMinus_def1")) %>% ungroup()
testDEF <- left_join(testDEF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_def")],PlayersFull2=PlayersFull,VellosoPlusMinus_def2=VellosoPlusMinus_def),by=c("VellosoPlusMinus_def2")) %>% ungroup()
testDEF <- left_join(testDEF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_def")],PlayersFull3=PlayersFull,VellosoPlusMinus_def3=VellosoPlusMinus_def),by=c("VellosoPlusMinus_def3")) %>% ungroup()
testDEF <- left_join(testDEF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_def")],PlayersFull4=PlayersFull,VellosoPlusMinus_def4=VellosoPlusMinus_def),by=c("VellosoPlusMinus_def4")) %>% ungroup()
testDEF <- left_join(testDEF,dplyr::rename(dfchart[,c("PlayersFull","VellosoPlusMinus_def")],PlayersFull5=PlayersFull,VellosoPlusMinus_def5=VellosoPlusMinus_def),by=c("VellosoPlusMinus_def5")) %>% ungroup()


currentranksTOT3 <- left_join(currentranksTOT,coefALL[coefALL$Date=="2022-11-14",c("PlayerID","RAPM5_total","RAPM5_off","RAPM5_def")]) %>% ungroup()
currentranksTOT3 <- left_join(currentranksTOT3,regdata[regdata$Date=="2022-11-14",c("PlayerID","Center_reg","Forward_reg","Guard_reg")]) %>% ungroup()
mean(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Guard_reg==1])
mean(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Forward_reg==1])
mean(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Center_reg==1])
mean(currentranksTOT3$RAPM5_def[currentranksTOT3$Guard_reg==1],na.rm=TRUE)
mean(currentranksTOT3$RAPM5_def[currentranksTOT3$Forward_reg==1],na.rm=TRUE)
mean(currentranksTOT3$RAPM5_def[currentranksTOT3$Center_reg==1],na.rm=TRUE)
sum(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Guard_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Guard_reg==1])/sum(currentranksTOT3$MIN.x[currentranksTOT3$Guard_reg==1])
sum(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Forward_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Forward_reg==1])/sum(currentranksTOT3$MIN.x[currentranksTOT3$Forward_reg==1])
sum(currentranksTOT3$VellosoPlusMinus_def[currentranksTOT3$Center_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Center_reg==1])/sum(currentranksTOT3$MIN.x[currentranksTOT3$Center_reg==1])
sum(currentranksTOT3$RAPM5_def[currentranksTOT3$Guard_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Guard_reg==1],na.rm=TRUE)/sum(currentranksTOT3$MIN.x[currentranksTOT3$Guard_reg==1],na.rm=TRUE)
sum(currentranksTOT3$RAPM5_def[currentranksTOT3$Forward_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Forward_reg==1],na.rm=TRUE)/sum(currentranksTOT3$MIN.x[currentranksTOT3$Forward_reg==1],na.rm=TRUE)
sum(currentranksTOT3$RAPM5_def[currentranksTOT3$Center_reg==1]*currentranksTOT3$MIN.x[currentranksTOT3$Center_reg==1],na.rm=TRUE)/sum(currentranksTOT3$MIN.x[currentranksTOT3$Center_reg==1],na.rm=TRUE)

# make plot
dfchart %>%
  ggplot(aes(x = Date,
             y = VellosoPlusMinus_total,
             group = PlayerID,
             color = PlayerID,
            label = paste0(substr(PlayersFull, 1, 1), ". ", word(PlayersFull, -1)))) +
  geom_line(data = . %>% filter(playermax == 0 & MIN>1000) ,
            color = 'gray',
            size =0.1) +
  geom_line(data = . %>% filter((PlayersFull=="LeBron James")) ,
            color = c('gold'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="LeBron James") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='gold',
                  alpha = 1,
                  nudge_y = .005,
                  segment.size = .1
                  ) +
  geom_line(data = . %>% filter((PlayersFull=="Stephen Curry")) ,
            color = c('sky blue'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Stephen Curry") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='sky blue',
                  alpha = 1,
                  nudge_y = .005,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="Nikola Jokic")) ,
            color = c('blue'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Nikola Jokic") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='blue',
                  alpha = 1,
                  nudge_y = .01,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="Kevin Durant")) ,
            color = c('red'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Kevin Durant") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='red',
                  alpha = 1,
                  nudge_y = .5,
                  nudge_x = -.5,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="Giannis Antetokounmpo")) ,
            color = c('dark grey'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Giannis Antetokounmpo") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='dark grey',
                  alpha = 1,
                  nudge_y = -1,
                  nudge_x = .5,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="James Harden")) ,
            color = c('chartreuse2'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="James Harden") & ismaxforplayer==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='chartreuse2',
                  alpha = 1,
                  nudge_y = -1,
                  nudge_x = .15,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="Russell Westbrook")) ,
            color = c('magenta'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Russell Westbrook") & ismaxforplayer2==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='magenta',
                  alpha = 1,
                  nudge_y = -.03,
                  segment.size = .1
  )  +
  geom_line(data = . %>% filter((PlayersFull=="Derrick Rose")) ,
            color = c('cadetblue'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Derrick Rose") & ismaxforplayer2==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='cadetblue',
                  alpha = 1,
                  nudge_y = .1,
                  segment.size = .1
  ) +
  geom_line(data = . %>% filter((PlayersFull=="Joel Embiid")) ,
            color = c('orange'),
            size =1) +
  # add labels for players that meet certain criteria
  geom_text_repel(data = . %>% filter( (PlayersFull=="Joel Embiid") & ismaxforplayer2==1),
                  size = 3,
                  family = "Tahoma",
                  fontface=2,
                  color='orange',
                  alpha = 1,
                  nudge_y = .1,
                  segment.size = .1
  ) +
  theme_bruno() +
  # thematic tweaks
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(size = 10),
        axis.title  = element_text(size = 9)) +
  # add axis titles, plot titles, subtitles, and caption
  labs(y = "Total Bayesian Adjusted Plus Minus",
       x = "Date",
       title = "Leading Adjusted Plus/Minus Scores over Time",
       subtitle = "Minimum 1000 Min. Played per Season, Active NBA MVPs Highlighted",
       caption = "Source: Scraped data from espn.com") +
  scale_x_date(labels = date_format("%m-%y"),
               breaks = scales::breaks_pretty(10))

ggsave("RAPMoverTime.png", w = 12, h = 6, dpi = 75)



