#0. Clean up and erase

rm(list=ls())

#1. Load Relevant Packages and Set WD
library('pdftools')
library('tidyverse')
library('gdata')
library('lubridate')
library('dplyr')
library('plotly')
library('ggplot2')
library('stringr')
library('selectr')
library('xml2')
library('rvest')
library('jsonlite')
library('RSelenium')
library('R.utils')
library('ds4psy')
library('foreign')
library("haven")
library('plm')
library('dplyr')
library('ds4psy')
library("xlsx")
library("writexl")
library("randomForest")
library("party")
library('glmnet')
library('caret')
#library('quantreg')
library("readxl")
library("tictoc")
library(parallel)
library(MASS)
library(fastDummies)
library(fixest)
#setwd(working)

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())


load("allregdata_withlines_FINAL.RData")
load("seasonpreds.RData")
load("modelgammaV2additions5.RData")
seasondata_add5OLD <- seasondata_add5
load("modelgammaV2additions6.RData")
seasondata_add2OLD <- seasondata_add2
load("additions_teammerge.RData")
teammergeOLD <- teammerge[!(teammerge$Season==max(seasondata$Season)),]

start <- length(seasondata_add2$Date[seasondata_add2$Date<"2022-10-10"])
seasonpreds$Team.x <- seasonpreds$Team
seasonpreds$Team.y <- seasonpreds$Team
seasonpreds <- seasonpreds %>%
  dplyr::mutate(predwins.x = predwins,
         prednetrat.x = prednetrat,
         predoffrat.x = predoffrat,
         preddefrat.x = preddefrat,
         TeamQual_pre.x = TeamQual_pre,
         TeamQualORPM_pre.x = TeamQualORPM_pre,
         TeamQualDRPM_pre.x = TeamQualDRPM_pre,
         predwins.y = predwins,
         prednetrat.y = prednetrat,
         predoffrat.y = predoffrat,
         preddefrat.y = preddefrat,
         TeamQual_pre.y = TeamQual_pre,
         TeamQualORPM_pre.y = TeamQualORPM_pre,
         TeamQualDRPM_pre.y = TeamQualDRPM_pre)

seasondata <- seasondata %>% left_join(dplyr::select(seasonpreds,Team.x,Season,predwins.x,prednetrat.x,predoffrat.x,preddefrat.x,TeamQual_pre.x,TeamQualORPM_pre.x,TeamQualDRPM_pre.x),by=c("Team.x","Season"))
seasondata <- seasondata %>% left_join(dplyr::select(seasonpreds,Team.y,Season,predwins.y,prednetrat.y,predoffrat.y,preddefrat.y,TeamQual_pre.y,TeamQualORPM_pre.y,TeamQualDRPM_pre.y),by=c("Team.y","Season"))

seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today


myfunc <- function(i) {
  TeamAvgPts <- with(seasondata,(mean(c(PTS.x[Date>=(Date[i]-365)&Date<Date[i]&RegularSeason==1&!is.na(PTS.x)&(GameCount.x>=GameCountAvg[i]|Season==Season[i])],PTS.y[Date>=(Date[i]-365)&Date<=Date[i]&RegularSeason==1&!is.na(PTS.y)&(GameCount.y>=GameCountAvg[i]|Season==Season[i])]),na.rm=TRUE)))
  TeamSDPts <- with(seasondata,sd(c(PTS.x[Date>=(Date[i]-365)&Date<Date[i]&RegularSeason==1&!is.na(PTS.x)&(GameCount.x>=GameCountAvg[i]|Season==Season[i])],PTS.y[Date>=(Date[i]-365)&Date<=Date[i]&RegularSeason==1&!is.na(PTS.y)&(GameCount.y>=GameCountAvg[i]|Season==Season[i])]),na.rm=TRUE))
  GameID <- with(seasondata,GameID[i])
  Date <- with(seasondata,Date[i])
  value <- data.frame(TeamAvgPts,TeamSDPts,GameID,Date)
  return(value)
}
tic()
numCores <- detectCores()
seasondata_add2 <- mclapply(c(start:length(seasondata$Date)), myfunc,mc.cores = numCores-2)
seasondata_add2 <- dplyr::bind_rows(seasondata_add2, .id = "column_label")
toc()

seasondata_add2OLD <- seasondata_add2OLD[!(seasondata_add2OLD$GameID %in% seasondata_add2$GameID),]
seasondata_add2 <- rbind(seasondata_add2OLD,seasondata_add2)
seasondata_add2 <- distinct(seasondata_add2)
save(seasondata_add2,file="modelgammaV2additions6.RData")
#check
length(seasondata$Date)==length(seasondata_add2$Date)
seasondata <- seasondata %>% left_join(seasondata_add2,by=c("GameID","Date"))

seasondata <- seasondata %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(TeamAvgPts=TeamAvgPts[n()],
         TeamSDPts=TeamSDPts[n()])
test <- seasondata %>% dplyr::group_by(Date) %>% dplyr::summarize(AvgOff2 = mean(TeamAvgPts))

seasondata$predwinsSE <- seasonpreds$predwinsSE[1]
seasondata$prednetratSE <- 5
seasondata$predoffratSE <- 4.5
seasondata$preddefratSE <- 4.5

#Compute alt teamavgpoints
seasondata$TotalPoints[seasondata$Date>=today] <- 0
seasondata <- seasondata %>%
  dplyr::group_by(Season) %>%
  mutate(CumSumTotPts = cumsum(TotalPoints)/cumsum(ones),
         CountTotPts = cumsum(ones))
seasondata <- seasondata %>%
  dplyr::group_by(Season,Date) %>%
  mutate(TotPtsDay = sum(TotalPoints)/sum(ones),
         TotPtsDayCount = sum(ones),
         CumsSumTotPts2 = (CountTotPts[n()]*CumSumTotPts[n()] - TotPtsDay[n()]*TotPtsDayCount[n()])/(CountTotPts[n()]-TotPtsDayCount[n()]))
seasondata$TotalPoints[seasondata$Date>=today] <- NA
seasondata$CumsSumTotPts2[seasondata$Date==today+1] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+2] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+3] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+4] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+5] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+6] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
seasondata$CumsSumTotPts2[seasondata$Date==today+7] <- max(seasondata$CumsSumTotPts2[seasondata$Date==today])
#for TeamAvg ALT see sheet "varsptstotv2.xlsx" for work. Idea is to create a better prior
#by adding the trend growth from year prior to the end of last season's average, and subtract by -1.5 to
# adjust for being early in schedule. Subjectively I create a prior for this year that is 1.5 higher based on
# my expectation for the new rule changes. Also a -1.5 for lockout season in 2011-12. Minus 
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==1] <- 93.61445
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==2] <- 96.470855
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==3] <- 96.01247982
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==4] <- 95.83224795
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==5] <- 97.6873536
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==6] <- 98.93484389
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==7] <- 99.09220861
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==8] <- 99.63245479
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==9] <- 97.60215859 #minus extra 1.5 for lockout shortened season (slow start)
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==10] <- 95.60239792
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==11] <- 97.53729549
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==12] <- 100.5081155
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==13] <- 99.57787962
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==14] <- 102.3418243
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==15] <- 105.3204921
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==16] <- 106.1264463
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==17] <- 111.0759437
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==18] <- 112.8223718
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==19] <- 110.6819585 #minus extra 1.5 for no longer pandemic
seasondata$TeamAvgPts_seasALT[seasondata$SeasNum==20] <- 111.6 #110.6 minus 1 for start plus 2 for take fouls rule

seasondata$TeamAvgPtsALT <- seasondata$TeamAvgPts_seasALT*seasondata$SeasGamesPREVWGT.x+(seasondata$CumsSumTotPts2/2)*seasondata$SeasGamesCURRWGT.x
seasondata$TeamAvgPtsALT[is.na(seasondata$TeamAvgPtsALT)] <- seasondata$TeamAvgPts_seasALT[is.na(seasondata$TeamAvgPtsALT)]

seasondata <- seasondata %>%
  dplyr::group_by(Season,Playoffs) %>%
  dplyr::mutate(TeamAvgPtsALT_temp = TeamAvgPtsALT[n()])
seasondata <- seasondata %>%
  dplyr::group_by(Season) %>%
  dplyr::mutate(TeamAvgPtsALT_temp = TeamAvgPtsALT_temp[1])

seasondata$TeamAvgPtsALT[seasondata$Playoffs==1] <- seasondata$TeamAvgPtsALT_temp[seasondata$Playoffs==1]
seasondata <- seasondata %>%
  dplyr::group_by(Date) %>%
  mutate(TeamAvgPtsALT = mean(TeamAvgPtsALT))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)


###if there are new teams, may need to update team list in line below manually
teammerge <- expand.grid(unique(seasondata$Date[seasondata$Season==max(seasondata$Season)]), unique(seasondata$Team.x[seasondata$Season=="2021-22"]))
teammerge <- dplyr::rename(teammerge, Date=Var1,Team=Var2)
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[!(seasondata$Date %in% unique(teammergeOLD$Date)),c("Date","Team.x","Team.y","PTS.x","PTS.y","predoffrat.x","preddefrat.x","predoffrat.y","preddefrat.y","GameCount.x")],Team=Team.x,OppTeam=Team.y),by=c("Date","Team"))
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[!(seasondata$Date %in% unique(teammergeOLD$Date)),c("Date","Team.y","Team.x","PTS.y","PTS.x","predoffrat.y","preddefrat.y","predoffrat.x","preddefrat.x","GameCount.y")],Team=Team.y,OppTeam=Team.x),by=c("Date","Team"))
teammerge$PTS.x.x[is.na(teammerge$PTS.x.x)] <- 0
teammerge$PTS.y.x[is.na(teammerge$PTS.y.x)] <- 0
teammerge$PTS.y.y[is.na(teammerge$PTS.y.y)] <- 0
teammerge$PTS.x.y[is.na(teammerge$PTS.x.y)] <- 0
teammerge$GameCount <- ifelse(is.na(teammerge$GameCount.x),teammerge$GameCount.y,teammerge$GameCount.x)
teammerge <- teammerge %>% 
  fill(GameCount) %>%
  ungroup()
teammerge$GameCount[is.na(teammerge$GameCount)] <- min(teammerge$GameCount,na.rm=TRUE)

teammerge <- teammerge %>% left_join(distinct(seasondata[!(seasondata$Date %in% unique(teammergeOLD$Date)),],Date,Season),by=c("Date"))
teammerge <- teammerge %>% left_join(distinct(seasondata[!(seasondata$Date %in% unique(teammergeOLD$Date)),],Date,TeamAvgPtsALT,TeamSDPts,predoffratSE,preddefratSE,Playoffs),by=c("Date"))
if(any(names(teammerge) %in% c("Season.x"))) {
  teammerge <- dplyr::rename(teammerge,Season=Season.x)
  teammerge <- dplyr::select(teammerge,-Season.y)
}
teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(TotalPoints = (cumsum(PTS.x.x)+cumsum(PTS.y.y))/((cumsum(PTS.x.x>0)+cumsum(PTS.y.y>0))),
         DefTotalPoints = (cumsum(PTS.y.x)+cumsum(PTS.x.y))/((cumsum(PTS.y.x>0)+cumsum(PTS.x.y>0)))) %>%
  ungroup()
teammerge <- teammerge %>%
  dplyr::mutate(AddOff = PTS.x.x+PTS.y.y,
         AddDef = PTS.y.x+PTS.x.y,
         StartOffRat = ifelse(!is.na(predoffrat.x.x),predoffrat.x.x,ifelse(!is.na(predoffrat.y.y),predoffrat.y.y,NA)),
         StartDefRat = ifelse(!is.na(preddefrat.x.x),preddefrat.x.x,ifelse(!is.na(preddefrat.y.y),preddefrat.y.y,NA)),
         StartOppOffRat = ifelse(!is.na(predoffrat.x.y),predoffrat.x.y,ifelse(!is.na(predoffrat.y.x),predoffrat.y.x,NA)),
         StartOppDefRat = ifelse(!is.na(preddefrat.x.y),preddefrat.x.y,ifelse(!is.na(preddefrat.y.x),preddefrat.y.x,NA)),
         curroffSE = predoffratSE,
         currdefSE = preddefratSE)

teammerge$MinDate <- min(seasondata$Date[seasondata$SeasNum==max(seasondata$SeasNum)])

#ADD BELOW IF DO FULL MERGE
# teammerge <- teammerge %>%
#   dplyr::group_by(Season) %>%
#   mutate(MinDate = min(Date)) %>%
#   ungroup()
# teammerge$GameCount[teammerge$Date==teammerge$MinDate] <- 1

teammerge$AddOff[teammerge$AddOff==0] <- NA
teammerge$AddDef[teammerge$AddDef==0] <- NA
teammerge$OppTeam <- ifelse(is.na(teammerge$OppTeam.x),teammerge$OppTeam.y,teammerge$OppTeam.x)
teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(StartOffRat= mean(StartOffRat,na.rm=TRUE),
          StartDefRat= mean(StartDefRat,na.rm=TRUE),
         curroffSE = mean(curroffSE,na.rm=TRUE),
         currdefSE = mean(currdefSE,na.rm=TRUE))

teammerge <- teammerge %>%
  dplyr::mutate(CurrOff = StartOffRat+TeamAvgPtsALT,
         CurrDef = TeamAvgPtsALT-StartDefRat,
         CurrOppOff = StartOppOffRat+TeamAvgPtsALT,
         CurrOppDef = TeamAvgPtsALT-StartOppDefRat)

if (length(unique(teammerge$Date))!=0) {
  teammerge$NewOff <- NA
    teammerge$NewDef <- NA
    teammerge$NewOffSE <- NA
    teammerge$NewDefSE <- NA
    teammerge$AddOffAdj <- NA
    teammerge$AddDefAdj <- NA
}

# teammergeJOIN <- teammergeOLD[teammergeOLD$Season==max(seasondata$Season),]
# if (length(unique(teammergeJOIN$Date))!=0) {
#   datevector <- unique(c(seasondata$Date[!(seasondata$Date %in% unique(teammergeOLD$Date))],max(teammergeJOIN$Date)))
#   teammergeJOIN <- teammergeJOIN %>%
#     dplyr::group_by(Team,Season) %>%
#     dplyr::mutate(TotalPoints = sapply(1:n(), function(x) ifelse(x<n(),TotalPoints[x+1],NA)),
#            DefTotalPoints = sapply(1:n(), function(x) ifelse(x<n(),DefTotalPoints[x+1],NA)),
#            NewOff = sapply(1:n(), function(x) ifelse(x<n(),NewOff[x+1],NA)),
#            NewDef = sapply(1:n(), function(x) ifelse(x<n(),NewDef[x+1],NA)),
#            NewOff2 = sapply(1:n(), function(x) ifelse(x<n(),NewOff2[x+1],NA)),
#            NewDef2 = sapply(1:n(), function(x) ifelse(x<n(),NewDef2[x+1],NA))) %>%
#     ungroup()
# } else {
#   datevector <- unique(c(seasondata$Date[!(seasondata$Date %in% unique(teammergeOLD$Date))]))
# }
# 
# teammerge <- dplyr::bind_rows(teammerge,teammergeJOIN)
teammerge <- teammerge[order(teammerge$Team,teammerge$Date),]
datevector <- unique(seasondata$Date[seasondata$Season==max(seasondata$Season)])
# datevector <- unique(seasondata$Date)
k <- 1
for (j in datevector) {
  if(j %in% unique(teammerge$MinDate)) {
    teammerge$NewOff[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$CurrOff[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewDef[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$CurrDef[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewOffSE[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$curroffSE[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewDefSE[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$currdefSE[teammerge$Date==j&is.na(teammerge$AddOff)]
    
    teammerge$AddOffAdj[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddOff[teammerge$Date==j&!is.na(teammerge$AddOff)]-teammerge$CurrOppDef[teammerge$Date==j&!is.na(teammerge$AddOff)])+teammerge$TeamAvgPtsALT[teammerge$Date==j&!is.na(teammerge$AddOff)]
    teammerge$AddDefAdj[teammerge$Date==j&!is.na(teammerge$AddOff)] <- teammerge$TeamAvgPtsALT[teammerge$Date==j&!is.na(teammerge$AddOff)]-(teammerge$CurrOppOff[teammerge$Date==j&!is.na(teammerge$AddOff)]-teammerge$AddDef[teammerge$Date==j&!is.na(teammerge$AddOff)])
    teammerge$NewOff[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddOffAdj[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$CurrOff[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)
    teammerge$NewDef[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddDefAdj[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$CurrDef[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)
    teammerge$NewOffSE[teammerge$Date==j&!is.na(teammerge$AddOff)] <- sqrt((teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2))
    teammerge$NewDefSE[teammerge$Date==j&!is.na(teammerge$AddOff)] <- sqrt((teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2))
  } else {
    teammerge$CurrOff[teammerge$Date==j] <- teammerge$NewOff[teammerge$Date==datevector[k-1]]
    teammerge$CurrDef[teammerge$Date==j] <- teammerge$NewDef[teammerge$Date==datevector[k-1]]
    teammerge$CurrOppOff[teammerge$Date==j&!is.na(teammerge$AddOff)] <- as.vector(left_join(teammerge[teammerge$Date==j&!is.na(teammerge$AddOff),c("OppTeam")],dplyr::rename(teammerge[teammerge$Date==datevector[k-1],c("Team","NewOff")],OppTeam=Team),by=c("OppTeam"))[,c(2)]$NewOff)
    teammerge$CurrOppDef[teammerge$Date==j&!is.na(teammerge$AddOff)] <- as.vector(left_join(teammerge[teammerge$Date==j&!is.na(teammerge$AddOff),c("OppTeam")],dplyr::rename(teammerge[teammerge$Date==datevector[k-1],c("Team","NewDef")],OppTeam=Team),by=c("OppTeam"))[,c(2)]$NewDef)
    teammerge$curroffSE[teammerge$Date==j] <- teammerge$NewOffSE[teammerge$Date==datevector[k-1]]
    teammerge$currdefSE[teammerge$Date==j] <- teammerge$NewDefSE[teammerge$Date==datevector[k-1]]
    
    teammerge$NewOff[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$CurrOff[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewDef[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$CurrDef[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewOffSE[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$curroffSE[teammerge$Date==j&is.na(teammerge$AddOff)]
    teammerge$NewDefSE[teammerge$Date==j&is.na(teammerge$AddOff)] <- teammerge$currdefSE[teammerge$Date==j&is.na(teammerge$AddOff)]
    
    
    teammerge$AddOffAdj[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddOff[teammerge$Date==j&!is.na(teammerge$AddOff)]-teammerge$CurrOppDef[teammerge$Date==j&!is.na(teammerge$AddOff)])+teammerge$TeamAvgPtsALT[teammerge$Date==j&!is.na(teammerge$AddOff)]
    teammerge$AddDefAdj[teammerge$Date==j&!is.na(teammerge$AddOff)] <- teammerge$TeamAvgPtsALT[teammerge$Date==j&!is.na(teammerge$AddOff)]-(teammerge$CurrOppOff[teammerge$Date==j&!is.na(teammerge$AddOff)]-teammerge$AddDef[teammerge$Date==j&!is.na(teammerge$AddOff)])
    teammerge$NewOff[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddOffAdj[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$CurrOff[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)
    teammerge$NewDef[teammerge$Date==j&!is.na(teammerge$AddOff)] <- (teammerge$AddDefAdj[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$CurrDef[teammerge$Date==j&!is.na(teammerge$AddOff)]*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)
    teammerge$NewOffSE[teammerge$Date==j&!is.na(teammerge$AddOff)] <- sqrt((teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$curroffSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2))
    teammerge$NewDefSE[teammerge$Date==j&!is.na(teammerge$AddOff)] <- sqrt((teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2*teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2)/(teammerge$currdefSE[teammerge$Date==j&!is.na(teammerge$AddOff)]^2+teammerge$TeamSDPts[teammerge$Date==j&!is.na(teammerge$AddOff)]^2))
  }
  k <- k+1
 print(as.Date(j))
}

# test2 <- teammerge[teammerge$Season=="2021-22"&teammerge$Team=="Golden State Warriors",]
# test2 <- dplyr::select(test2,Date,Team,PTS.x.x,PTS.y.x,PTS.y.y,PTS.x.y,OppTeam,TotalPoints,DefTotalPoints,NewOff,NewDef,everything())
# test2$wgtold <- test2$TeamSDPts^2/(test2$curroffSE^2+test2$TeamSDPts^2)
# test2$wgtold[!is.na(test2$AddOffAdj)]
# prod(test2$wgtold[!is.na(test2$AddOffAdj)])

teammerge$seaswgt <- ifelse(teammerge$GameCount<=82&teammerge$GameCount>62,(82-teammerge$GameCount)/20,1)
teammerge$seaswgt[teammerge$Playoffs==1] <- 0

teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(TotalPoints = (cumsum(PTS.x.x)+cumsum(PTS.y.y))/((cumsum(PTS.x.x>0)+cumsum(PTS.y.y>0))),
         DefTotalPoints = (cumsum(PTS.y.x)+cumsum(PTS.x.y))/((cumsum(PTS.y.x>0)+cumsum(PTS.x.y>0)))) %>%
  ungroup()

teammerge$NewOff2 <- teammerge$seaswgt*teammerge$NewOff + (1-teammerge$seaswgt)*teammerge$TotalPoints
teammerge$NewDef2 <- teammerge$seaswgt*teammerge$NewDef + (1-teammerge$seaswgt)*teammerge$DefTotalPoints
teammerge$NewOff2[is.nan(teammerge$NewOff2)] <- teammerge$NewOff[is.nan(teammerge$NewOff2)]
teammerge$NewDef2[is.nan(teammerge$NewDef2)] <- teammerge$NewDef[is.nan(teammerge$NewDef2)]

if (length(unique(teammerge$Date))!=0) {
teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(TotalPoints = sapply(1:n(), function(x) ifelse(x-1>0,TotalPoints[x-1],0)),
         DefTotalPoints = sapply(1:n(), function(x) ifelse(x-1>0,DefTotalPoints[x-1],0)),
         NewOff = sapply(1:n(), function(x) ifelse(x-1>0,NewOff[x-1],CurrOff[x])),
         NewDef = sapply(1:n(), function(x) ifelse(x-1>0,NewDef[x-1],CurrDef[x])),
         NewOff2 = sapply(1:n(), function(x) ifelse(x-1>0,NewOff2[x-1],CurrOff[x])),
         NewDef2 = sapply(1:n(), function(x) ifelse(x-1>0,NewDef2[x-1],CurrDef[x]))) %>%
  ungroup()
}
teammergeOLD <- teammergeOLD[!(teammergeOLD$Date %in% teammerge$Date),]
teammerge <- rbind(teammergeOLD,teammerge)
teammerge <- distinct(teammerge)
teammerge <- teammerge[order(teammerge$Team,teammerge$Date),]

seasondata <- seasondata %>%
  dplyr::group_by(Season) %>%
  dplyr::mutate(TeamAvgPts_seas = TeamAvgPts[1])

seasondata$StartNewOff.x <- seasondata$predoffrat.x+seasondata$TeamAvgPts_seasALT
seasondata$StartNewOff.y <- seasondata$predoffrat.y+seasondata$TeamAvgPts_seasALT
seasondata$StartNewDef.x <- seasondata$TeamAvgPts_seasALT-seasondata$preddefrat.x
seasondata$StartNewDef.y <- seasondata$TeamAvgPts_seasALT-seasondata$preddefrat.y

seasondata$StartNewOffv2.x <- seasondata$predoffrat.x+seasondata$PrevOppDefPtsTot.x
seasondata$StartNewOffv2.y <- seasondata$predoffrat.y+seasondata$PrevOppDefPtsTot.y
seasondata$StartNewDefv2.x <- seasondata$PrevOppPtsTot.x-seasondata$preddefrat.x
seasondata$StartNewDefv2.y <- seasondata$PrevOppPtsTot.y-seasondata$preddefrat.y

seasondata <- seasondata %>% left_join(dplyr::rename(teammerge[,c("Team","Date","NewOff","NewDef","NewOff2","NewDef2")],Team.x=Team,NewOff.x=NewOff,NewDef.x=NewDef,NewOff2.x=NewOff2,NewDef2.x=NewDef2),by=c("Team.x","Date"))
seasondata <- seasondata %>% left_join(dplyr::rename(teammerge[,c("Team","Date","NewOff","NewDef","NewOff2","NewDef2")],Team.y=Team,NewOff.y=NewOff,NewDef.y=NewDef,NewOff2.y=NewOff2,NewDef2.y=NewDef2),by=c("Team.y","Date"))

fillindate <- min(unique(seasondata$Date[seasondata$Season==max(seasondata$Season)&seasondata$Date>=today]))
seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

myfunc3 <- function(i) {

  opplist.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i]],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i]])
  opplist.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i]],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i]])
  opplist20.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)])
  opplist20.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)])
  opplist10.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)])
  opplist10.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)])
  opplist5.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)])
  opplist5.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)])
  date.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist.x))
  date.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist.y))
  date20.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist20.x))
  date20.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist20.y))
  date10.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist10.x))
  date10.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist10.y))
  date5.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist5.x))
  date5.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>=today,fillindate,seasondata$Date[i])),length(opplist5.y))
  df.x <- data.frame(opplist.x,date.x)
  df.y <- data.frame(opplist.y,date.y)
  df20.x <- data.frame(opplist20.x,date20.x)
  df20.y <- data.frame(opplist20.y,date20.y)
  df10.x <- data.frame(opplist10.x,date10.x)
  df10.y <- data.frame(opplist10.y,date10.y)
  df5.x <- data.frame(opplist5.x,date5.x)
  df5.y <- data.frame(opplist5.y,date5.y)
  df.x <- dplyr::rename(df.x,Date=date.x,Team=opplist.x)
  df.y <- dplyr::rename(df.y,Date=date.y,Team=opplist.y)
  df.x <- df.x %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df.y <- df.y %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppNewOff2.x <- mean(df.x$NewOff2,na.rm = TRUE)
  OppNewOff2.y <- mean(df.y$NewOff2,na.rm = TRUE)
  OppNewDef2.x <- mean(df.x$NewDef2,na.rm = TRUE)
  OppNewDef2.y <- mean(df.y$NewDef2,na.rm = TRUE)
  df20.x <- dplyr::rename(df20.x,Date=date20.x,Team=opplist20.x)
  df20.y <- dplyr::rename(df20.y,Date=date20.y,Team=opplist20.y)
  df20.x <- df20.x %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df20.y <- df20.y %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppNewOff220.x <- mean(df20.x$NewOff2,na.rm = TRUE)
  OppNewOff220.y <- mean(df20.y$NewOff2,na.rm = TRUE)
  OppNewDef220.x <- mean(df20.x$NewDef2,na.rm = TRUE)
  OppNewDef220.y <- mean(df20.y$NewDef2,na.rm = TRUE)
  df10.x <- dplyr::rename(df10.x,Date=date10.x,Team=opplist10.x)
  df10.y <- dplyr::rename(df10.y,Date=date10.y,Team=opplist10.y)
  df10.x <- df10.x %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df10.y <- df10.y %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppNewOff210.x <- mean(df10.x$NewOff2,na.rm = TRUE)
  OppNewOff210.y <- mean(df10.y$NewOff2,na.rm = TRUE)
  OppNewDef210.x <- mean(df10.x$NewDef2,na.rm = TRUE)
  OppNewDef210.y <- mean(df10.y$NewDef2,na.rm = TRUE)
  df5.x <- dplyr::rename(df5.x,Date=date5.x,Team=opplist5.x)
  df5.y <- dplyr::rename(df5.y,Date=date5.y,Team=opplist5.y)
  df5.x <- df5.x %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df5.y <- df5.y %>% left_join(teammerge[,c("NewOff2","NewDef2","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppNewOff25.x <- mean(df5.x$NewOff2,na.rm = TRUE)
  OppNewOff25.y <- mean(df5.y$NewOff2,na.rm = TRUE)
  OppNewDef25.x <- mean(df5.x$NewDef2,na.rm = TRUE)
  OppNewDef25.y <- mean(df5.y$NewDef2,na.rm = TRUE)
  
  GameID <- with(seasondata,GameID[i])
  Date <- with(seasondata,Date[i])
  
  value <- data.frame(GameID,Date,OppNewOff2.x,
                      OppNewOff2.y,
                      OppNewDef2.x,
                      OppNewDef2.y,
                      OppNewOff220.x,
                      OppNewOff220.y,
                      OppNewDef220.x,
                      OppNewDef220.y,
                      OppNewOff210.x,
                      OppNewOff210.y,
                      OppNewDef210.x,
                      OppNewDef210.y,
                      OppNewOff25.x,
                      OppNewOff25.y,
                      OppNewDef25.x,
                      OppNewDef25.y)
  
  return(value)

}

tic()
numCores <- detectCores()
seasondata_add5 <- mclapply(c(start:length(seasondata$Date)), myfunc3,mc.cores = numCores-2)
seasondata_add5 <- dplyr::bind_rows(seasondata_add5, .id = "column_label")
toc()

seasondata_add5OLD <- seasondata_add5OLD[!(seasondata_add5OLD$GameID %in% seasondata_add5$GameID),]
seasondata_add5 <- rbind(seasondata_add5OLD,seasondata_add5)
seasondata_add5 <- distinct(seasondata_add5)
save(seasondata_add5,file="modelgammaV2additions5.RData")
#check
length(seasondata$Date)==length(seasondata_add5$Date)
seasondata <- seasondata %>% left_join(seasondata_add5,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)

seasondata$OppNewOff2.x[is.nan(seasondata$OppNewOff2.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff2.x)]
seasondata$OppNewDef2.x[is.nan(seasondata$OppNewDef2.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef2.x)]
seasondata$OppNewOff2.y[is.nan(seasondata$OppNewOff2.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff2.y)]
seasondata$OppNewDef2.y[is.nan(seasondata$OppNewDef2.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef2.y)]
seasondata$OppNewOff220.x[is.nan(seasondata$OppNewOff220.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff220.x)]
seasondata$OppNewDef220.x[is.nan(seasondata$OppNewDef220.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef220.x)]
seasondata$OppNewOff220.y[is.nan(seasondata$OppNewOff220.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff220.y)]
seasondata$OppNewDef220.y[is.nan(seasondata$OppNewDef220.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef220.y)]
seasondata$OppNewOff210.x[is.nan(seasondata$OppNewOff210.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff210.x)]
seasondata$OppNewDef210.x[is.nan(seasondata$OppNewDef210.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef210.x)]
seasondata$OppNewOff210.y[is.nan(seasondata$OppNewOff210.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff210.y)]
seasondata$OppNewDef210.y[is.nan(seasondata$OppNewDef210.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef210.y)]
seasondata$OppNewOff25.x[is.nan(seasondata$OppNewOff25.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff25.x)]
seasondata$OppNewDef25.x[is.nan(seasondata$OppNewDef25.x)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef25.x)]
seasondata$OppNewOff25.y[is.nan(seasondata$OppNewOff25.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewOff25.y)]
seasondata$OppNewDef25.y[is.nan(seasondata$OppNewDef25.y)] <- seasondata$TeamAvgPts_seasALT[is.nan(seasondata$OppNewDef25.y)]

seasondata$NewOff2.x[is.nan(seasondata$NewOff2.x)] <- seasondata$NewOff.x[is.nan(seasondata$NewOff2.x)]
seasondata$NewDef2.x[is.nan(seasondata$NewDef2.x)] <- seasondata$NewDef.x[is.nan(seasondata$NewDef2.x)]
seasondata$NewOff2.y[is.nan(seasondata$NewOff2.y)] <- seasondata$NewOff.y[is.nan(seasondata$NewOff2.y)]
seasondata$NewDef2.y[is.nan(seasondata$NewDef2.y)] <- seasondata$NewDef.y[is.nan(seasondata$NewDef2.y)]

seasondata <- seasondata %>%
  dplyr::mutate(NetAdjMargv2.x = NetMargin.x + (OppNewOff2.x-OppNewDef2.x),
         NetAdjMargv2.y = NetMargin.y + (OppNewOff2.y-OppNewDef2.y),
         PrevAdjNetMarginv2.x = (StartNewOff.x-StartNewDef.x),
         PrevAdjNetMarginv2.y = (StartNewOff.y-StartNewDef.y),
         OppNetMargin20v2.x = OppNewOff220.x -OppNewDef220.x,
         OppNetMargin10v2.x = OppNewOff210.x -OppNewDef210.x,
         OppNetMargin5v2.x = OppNewOff25.x -OppNewDef25.x,
         NetAdjMarg20v2.x = CapMargin20.x+OppNetMargin20v2.x,
         NetAdjMarg10v2.x = CapMargin10.x+OppNetMargin10v2.x,
         NetAdjMarg5v2.x = CapMargin5.x+OppNetMargin5v2.x,
         OppNetMargin20v2.y = OppNewOff220.y -OppNewDef220.y,
         OppNetMargin10v2.y = OppNewOff210.y -OppNewDef210.y,
         OppNetMargin5v2.y = OppNewOff25.y -OppNewDef25.y,
         NetAdjMarg20v2.y = CapMargin20.y+OppNetMargin20v2.y,
         NetAdjMarg10v2.y = CapMargin10.y+OppNetMargin10v2.y,
         NetAdjMarg5v2.y = CapMargin5.y+OppNetMargin5v2.y)

seasondata$NetAdjMarg3.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg.x
seasondata$NetAdjMarg3.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg3.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg.y
seasondata$NetAdjMarg3.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv3 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^2*((seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)>0)
seasondata$AdjNetMarSqNegv3 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^2*((seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)<=0)
seasondata$AdjNetMarCubv3 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^3

seasondata$NetAdjMarg320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20.x
seasondata$NetAdjMarg320.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20.y
seasondata$NetAdjMarg320.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10.x
seasondata$NetAdjMarg310.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10.y
seasondata$NetAdjMarg310.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg35.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5.x
seasondata$NetAdjMarg35.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg35.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5.y
seasondata$NetAdjMarg35.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal3.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal.x
seasondata$PointsTotal3.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal3.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal.y
seasondata$PointsTotal3.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal20.x
seasondata$PointsTotal320.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal20.y
seasondata$PointsTotal320.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal310.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal310.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal35.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal35.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal35.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal35.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal3.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal.x
seasondata$DefPointsTotal3.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal3.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal.y
seasondata$DefPointsTotal3.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal320.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal320.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal310.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal310.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal35.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal35.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal35.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal35.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints3.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints.x
seasondata$OppTotalPoints3.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints3.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints.y
seasondata$OppTotalPoints3.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints20.x
seasondata$OppTotalPoints320.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints20.y
seasondata$OppTotalPoints320.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints10.x
seasondata$OppTotalPoints310.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints10.y
seasondata$OppTotalPoints310.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints35.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppTotalPoints5.x
seasondata$OppTotalPoints35.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints35.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppTotalPoints5.y
seasondata$OppTotalPoints35.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints3.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints.x
seasondata$OppDefTotalPoints3.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints3.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints.y
seasondata$OppDefTotalPoints3.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints20.x
seasondata$OppDefTotalPoints320.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints20.y
seasondata$OppDefTotalPoints320.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints10.x
seasondata$OppDefTotalPoints310.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints10.y
seasondata$OppDefTotalPoints310.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints35.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppDefTotalPoints5.x
seasondata$OppDefTotalPoints35.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints35.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppDefTotalPoints5.y
seasondata$OppDefTotalPoints35.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]



##version4
seasondata$NetAdjMarg4.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv2.x
seasondata$NetAdjMarg4.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg4.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv2.y
seasondata$NetAdjMarg4.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv4 <- (seasondata$NetAdjMarg4.x-seasondata$NetAdjMarg4.y)^2*((seasondata$NetAdjMarg4.x-seasondata$NetAdjMarg4.y)>0)
seasondata$AdjNetMarSqNegv4 <- (seasondata$NetAdjMarg4.x-seasondata$NetAdjMarg4.y)^2*((seasondata$NetAdjMarg4.x-seasondata$NetAdjMarg4.y)<=0)
seasondata$AdjNetMarCubv4 <- (seasondata$NetAdjMarg4.x-seasondata$NetAdjMarg4.y)^3

seasondata$NetAdjMarg420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v2.x
seasondata$NetAdjMarg420.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v2.y
seasondata$NetAdjMarg420.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v2.x
seasondata$NetAdjMarg410.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v2.y
seasondata$NetAdjMarg410.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg45.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v2.x
seasondata$NetAdjMarg45.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg45.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v2.y
seasondata$NetAdjMarg45.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal4.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal.x
seasondata$PointsTotal4.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal4.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal.y
seasondata$PointsTotal4.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal20.x
seasondata$PointsTotal420.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal20.y
seasondata$PointsTotal420.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal410.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal410.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal45.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal45.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal45.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal45.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal4.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal.x
seasondata$DefPointsTotal4.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal4.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal.y
seasondata$DefPointsTotal4.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal420.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal420.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal410.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal410.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal45.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal45.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal45.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal45.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints4.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewOff2.x
seasondata$OppTotalPoints4.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints4.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewOff2.y
seasondata$OppTotalPoints4.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewOff220.x
seasondata$OppTotalPoints420.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewOff220.y
seasondata$OppTotalPoints420.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewOff210.x
seasondata$OppTotalPoints410.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewOff210.y
seasondata$OppTotalPoints410.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints45.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewOff25.x
seasondata$OppTotalPoints45.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints45.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewOff25.y
seasondata$OppTotalPoints45.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints4.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewDef2.x
seasondata$OppDefTotalPoints4.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints4.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewDef2.y
seasondata$OppDefTotalPoints4.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewDef220.x
seasondata$OppDefTotalPoints420.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewDef220.y
seasondata$OppDefTotalPoints420.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewDef210.x
seasondata$OppDefTotalPoints410.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewDef210.y
seasondata$OppDefTotalPoints410.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints45.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewDef25.x
seasondata$OppDefTotalPoints45.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints45.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewDef25.y
seasondata$OppDefTotalPoints45.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]


#version5
seasondata$NetAdjMarg5.x <- seasondata$NewOff2.x-seasondata$NewDef2.x
seasondata$NetAdjMarg5.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg5.y <- seasondata$NewOff2.y-seasondata$NewDef2.y
seasondata$NetAdjMarg5.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv5 <- (seasondata$NetAdjMarg5.x-seasondata$NetAdjMarg5.y)^2*((seasondata$NetAdjMarg5.x-seasondata$NetAdjMarg5.y)>0)
seasondata$AdjNetMarSqNegv5 <- (seasondata$NetAdjMarg5.x-seasondata$NetAdjMarg5.y)^2*((seasondata$NetAdjMarg5.x-seasondata$NetAdjMarg5.y)<=0)
seasondata$AdjNetMarCubv5 <- (seasondata$NetAdjMarg5.x-seasondata$NetAdjMarg5.y)^3

seasondata$NetAdjMarg520.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v2.x
seasondata$NetAdjMarg520.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg520.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v2.y
seasondata$NetAdjMarg520.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg510.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v2.x
seasondata$NetAdjMarg510.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg510.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v2.y
seasondata$NetAdjMarg510.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg55.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v2.x
seasondata$NetAdjMarg55.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg55.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v2.y
seasondata$NetAdjMarg55.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal5v2.x <- seasondata$NewOff2.x
seasondata$PointsTotal5v2.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal5v2.y <- seasondata$NewOff2.y
seasondata$PointsTotal5v2.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal520.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal20.x
seasondata$PointsTotal520.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal520.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal20.y
seasondata$PointsTotal520.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal510.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal510.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal510.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal510.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal55.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal55.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal55.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal55.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal5v2.x <- seasondata$NewDef2.x
seasondata$DefPointsTotal5v2.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal5v2.y <- seasondata$NewDef2.y
seasondata$DefPointsTotal5v2.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal520.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal520.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal520.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal520.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal510.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal510.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal510.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal510.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal55.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal55.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal55.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal55.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints5v2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewOff2.x
seasondata$OppTotalPoints5v2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints5v2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewOff2.y
seasondata$OppTotalPoints5v2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints520.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewOff220.x
seasondata$OppTotalPoints520.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints520.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewOff220.y
seasondata$OppTotalPoints520.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints510.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewOff210.x
seasondata$OppTotalPoints510.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints510.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewOff210.y
seasondata$OppTotalPoints510.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints55.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewOff25.x
seasondata$OppTotalPoints55.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints55.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewOff25.y
seasondata$OppTotalPoints55.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints5v2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewDef2.x
seasondata$OppDefTotalPoints5v2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints5v2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewDef2.y
seasondata$OppDefTotalPoints5v2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints520.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppNewDef220.x
seasondata$OppDefTotalPoints520.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints520.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppNewDef220.y
seasondata$OppDefTotalPoints520.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints510.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewDef210.x
seasondata$OppDefTotalPoints510.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints510.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewDef210.y
seasondata$OppDefTotalPoints510.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints55.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewDef25.x
seasondata$OppDefTotalPoints55.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints55.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewDef25.y
seasondata$OppDefTotalPoints55.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]


seasondata <- seasondata %>% ungroup()
test2 <- distinct(seasondata,Season,Team.x,.keep_all=FALSE)
test2$Team.y <- test2$Team.x
test2$GameCount.x <- 1
test2$GameCount.y <- 1
test2 <- test2 %>% left_join(seasondata[,c("Season",'Team.x',"GameCount.x","TeamQual_pre.x","TeamQualORPM_pre.x","TeamQualDRPM_pre.x","TeamQualRPM.x","TeamQualORPM.x","TeamQualDRPM.x")],by=c("Season","Team.x","GameCount.x"))
test2 <- test2 %>% left_join(seasondata[,c("Season",'Team.y',"GameCount.y","TeamQual_pre.y","TeamQualORPM_pre.y","TeamQualDRPM_pre.y","TeamQualRPM.y","TeamQualORPM.y","TeamQualDRPM.y")],by=c("Season","Team.y","GameCount.y"))
test2$TeamQual_pre.x[is.na(test2$TeamQual_pre.x)] <- test2$TeamQual_pre.y[is.na(test2$TeamQual_pre.x)]
test2$TeamQual_pre.y[is.na(test2$TeamQual_pre.y)] <- test2$TeamQual_pre.x[is.na(test2$TeamQual_pre.y)]
test2$TeamQualORPM_pre.x[is.na(test2$TeamQualORPM_pre.x)] <- test2$TeamQualORPM_pre.y[is.na(test2$TeamQualORPM_pre.x)]
test2$TeamQualORPM_pre.y[is.na(test2$TeamQualORPM_pre.y)] <- test2$TeamQualORPM_pre.x[is.na(test2$TeamQualORPM_pre.y)]
test2$TeamQualDRPM_pre.x[is.na(test2$TeamQualDRPM_pre.x)] <- test2$TeamQualDRPM_pre.y[is.na(test2$TeamQualDRPM_pre.x)]
test2$TeamQualDRPM_pre.y[is.na(test2$TeamQualDRPM_pre.y)] <- test2$TeamQualDRPM_pre.x[is.na(test2$TeamQualDRPM_pre.y)]
test2$TeamQualRPM.x[is.na(test2$TeamQualRPM.x)] <- test2$TeamQualRPM.y[is.na(test2$TeamQualRPM.x)]
test2$TeamQualRPM.y[is.na(test2$TeamQualRPM.y)] <- test2$TeamQualRPM.x[is.na(test2$TeamQualRPM.y)]
test2$TeamQualORPM.x[is.na(test2$TeamQualORPM.x)] <- test2$TeamQualORPM.y[is.na(test2$TeamQualORPM.x)]
test2$TeamQualORPM.y[is.na(test2$TeamQualORPM.y)] <- test2$TeamQualORPM.x[is.na(test2$TeamQualORPM.y)]
test2$TeamQualDRPM.x[is.na(test2$TeamQualDRPM.x)] <- test2$TeamQualDRPM.y[is.na(test2$TeamQualDRPM.x)]
test2$TeamQualDRPM.y[is.na(test2$TeamQualDRPM.y)] <- test2$TeamQualDRPM.x[is.na(test2$TeamQualDRPM.y)]

test2 <- test2 %>%
  dplyr::group_by(Season) %>%
  mutate(zscore1 = (TeamQual_pre.x-mean(TeamQual_pre.x))/sd(TeamQual_pre.x),
         zscore2 = (TeamQualRPM.x-mean(TeamQualRPM.x))/sd(TeamQualRPM.x),
         TeamQual_preAdj.x = zscore1*sd(TeamQualRPM.x) +mean(TeamQualRPM.x),
         zscore3 = (TeamQualORPM_pre.x-mean(TeamQualORPM_pre.x))/sd(TeamQualORPM_pre.x),
         zscore4 = (TeamQualORPM.x-mean(TeamQualORPM.x))/sd(TeamQualORPM.x),
         TeamQualORPM_preAdj.x = zscore3*sd(TeamQualORPM.x) +mean(TeamQualORPM.x),
         zscore5 = (TeamQualDRPM_pre.x-mean(TeamQualDRPM_pre.x))/sd(TeamQualDRPM_pre.x),
         zscore6 = (TeamQualDRPM.x-mean(TeamQualDRPM.x))/sd(TeamQualDRPM.x),
         TeamQualDRPM_preAdj.x = zscore5*sd(TeamQualDRPM.x) +mean(TeamQualDRPM.x))

test2 <- test2 %>%
  dplyr::group_by(Season) %>%
  mutate(zscore1 = (TeamQual_pre.y-mean(TeamQual_pre.y))/sd(TeamQual_pre.y),
         zscore2 = (TeamQualRPM.y-mean(TeamQualRPM.y))/sd(TeamQualRPM.y),
         TeamQual_preAdj.y = zscore1*sd(TeamQualRPM.y) +mean(TeamQualRPM.y),
         zscore3 = (TeamQualORPM_pre.y-mean(TeamQualORPM_pre.y))/sd(TeamQualORPM_pre.y),
         zscore4 = (TeamQualORPM.y-mean(TeamQualORPM.y))/sd(TeamQualORPM.y),
         TeamQualORPM_preAdj.y = zscore3*sd(TeamQualORPM.y) +mean(TeamQualORPM.y),
         zscore5 = (TeamQualDRPM_pre.y-mean(TeamQualDRPM_pre.y))/sd(TeamQualDRPM_pre.y),
         zscore6 = (TeamQualDRPM.y-mean(TeamQualDRPM.y))/sd(TeamQualDRPM.y),
         TeamQualDRPM_preAdj.y = zscore5*sd(TeamQualDRPM.y) +mean(TeamQualDRPM.y))

test3 <- dplyr::select(test2,Season,Team.x,TeamQual_preAdj.x,TeamQualORPM_preAdj.x,TeamQualDRPM_preAdj.x)
test4 <- dplyr::select(test2,Season,Team.y,TeamQual_preAdj.y,TeamQualORPM_preAdj.y,TeamQualDRPM_preAdj.y)

seasondata <- seasondata %>% left_join(test3,by=c("Team.x","Season"))
seasondata <- seasondata %>% left_join(test4,by=c("Team.y","Season"))

#change for normalzied teamqual
seasondata <- seasondata %>% ungroup()
test2 <- distinct(seasondata,Season,Team.x,.keep_all=FALSE)
test2$Team.y <- test2$Team.x
test2$GameCount.x <- 1
test2$GameCount.y <- 1
test2 <- test2 %>% left_join(seasondata[,c("Season",'Team.x',"GameCount.x","TeamQual_pre.x","TeamQualORPM_pre.x","TeamQualDRPM_pre.x","TeamQualRPMNorm.x","TeamQualORPMNorm.x","TeamQualDRPMNorm.x")],by=c("Season","Team.x","GameCount.x"))
test2 <- test2 %>% left_join(seasondata[,c("Season",'Team.y',"GameCount.y","TeamQual_pre.y","TeamQualORPM_pre.y","TeamQualDRPM_pre.y","TeamQualRPMNorm.y","TeamQualORPMNorm.y","TeamQualDRPMNorm.y")],by=c("Season","Team.y","GameCount.y"))
test2$TeamQual_pre.x[is.na(test2$TeamQual_pre.x)] <- test2$TeamQual_pre.y[is.na(test2$TeamQual_pre.x)]
test2$TeamQual_pre.y[is.na(test2$TeamQual_pre.y)] <- test2$TeamQual_pre.x[is.na(test2$TeamQual_pre.y)]
test2$TeamQualORPM_pre.x[is.na(test2$TeamQualORPM_pre.x)] <- test2$TeamQualORPM_pre.y[is.na(test2$TeamQualORPM_pre.x)]
test2$TeamQualORPM_pre.y[is.na(test2$TeamQualORPM_pre.y)] <- test2$TeamQualORPM_pre.x[is.na(test2$TeamQualORPM_pre.y)]
test2$TeamQualDRPM_pre.x[is.na(test2$TeamQualDRPM_pre.x)] <- test2$TeamQualDRPM_pre.y[is.na(test2$TeamQualDRPM_pre.x)]
test2$TeamQualDRPM_pre.y[is.na(test2$TeamQualDRPM_pre.y)] <- test2$TeamQualDRPM_pre.x[is.na(test2$TeamQualDRPM_pre.y)]
test2$TeamQualRPMNorm.x[is.na(test2$TeamQualRPMNorm.x)] <- test2$TeamQualRPMNorm.y[is.na(test2$TeamQualRPMNorm.x)]
test2$TeamQualRPMNorm.y[is.na(test2$TeamQualRPMNorm.y)] <- test2$TeamQualRPMNorm.x[is.na(test2$TeamQualRPMNorm.y)]
test2$TeamQualORPMNorm.x[is.na(test2$TeamQualORPMNorm.x)] <- test2$TeamQualORPMNorm.y[is.na(test2$TeamQualORPMNorm.x)]
test2$TeamQualORPMNorm.y[is.na(test2$TeamQualORPMNorm.y)] <- test2$TeamQualORPMNorm.x[is.na(test2$TeamQualORPMNorm.y)]
test2$TeamQualDRPMNorm.x[is.na(test2$TeamQualDRPMNorm.x)] <- test2$TeamQualDRPMNorm.y[is.na(test2$TeamQualDRPMNorm.x)]
test2$TeamQualDRPMNorm.y[is.na(test2$TeamQualDRPMNorm.y)] <- test2$TeamQualDRPMNorm.x[is.na(test2$TeamQualDRPMNorm.y)]

test2 <- test2 %>%
  dplyr::group_by(Season) %>%
  mutate(zscore1Norm = (TeamQual_pre.x-mean(TeamQual_pre.x))/sd(TeamQual_pre.x),
         zscore2Norm = (TeamQualRPMNorm.x-mean(TeamQualRPMNorm.x))/sd(TeamQualRPMNorm.x),
         TeamQual_preAdjNorm.x = zscore1Norm*sd(TeamQualRPMNorm.x) +mean(TeamQualRPMNorm.x),
         zscore3Norm = (TeamQualORPM_pre.x-mean(TeamQualORPM_pre.x))/sd(TeamQualORPM_pre.x),
         zscore4Norm = (TeamQualORPMNorm.x-mean(TeamQualORPMNorm.x))/sd(TeamQualORPMNorm.x),
         TeamQualORPM_preAdjNorm.x = zscore3Norm*sd(TeamQualORPMNorm.x) +mean(TeamQualORPMNorm.x),
         zscore5Norm = (TeamQualDRPM_pre.x-mean(TeamQualDRPM_pre.x))/sd(TeamQualDRPM_pre.x),
         zscore6Norm = (TeamQualDRPMNorm.x-mean(TeamQualDRPMNorm.x))/sd(TeamQualDRPMNorm.x),
         TeamQualDRPM_preAdjNorm.x = zscore5Norm*sd(TeamQualDRPMNorm.x) +mean(TeamQualDRPMNorm.x))

test2 <- test2 %>%
  dplyr::group_by(Season) %>%
  mutate(zscore1Norm = (TeamQual_pre.y-mean(TeamQual_pre.y))/sd(TeamQual_pre.y),
         zscore2Norm = (TeamQualRPMNorm.y-mean(TeamQualRPMNorm.y))/sd(TeamQualRPMNorm.y),
         TeamQual_preAdjNorm.y = zscore1Norm*sd(TeamQualRPMNorm.y) +mean(TeamQualRPMNorm.y),
         zscore3Norm = (TeamQualORPM_pre.y-mean(TeamQualORPM_pre.y))/sd(TeamQualORPM_pre.y),
         zscore4Norm = (TeamQualORPMNorm.y-mean(TeamQualORPMNorm.y))/sd(TeamQualORPMNorm.y),
         TeamQualORPM_preAdjNorm.y = zscore3Norm*sd(TeamQualORPMNorm.y) +mean(TeamQualORPMNorm.y),
         zscore5Norm = (TeamQualDRPM_pre.y-mean(TeamQualDRPM_pre.y))/sd(TeamQualDRPM_pre.y),
         zscore6Norm = (TeamQualDRPMNorm.y-mean(TeamQualDRPMNorm.y))/sd(TeamQualDRPMNorm.y),
         TeamQualDRPM_preAdjNorm.y = zscore5Norm*sd(TeamQualDRPMNorm.y) +mean(TeamQualDRPMNorm.y))

test3 <- dplyr::select(test2,Season,Team.x,TeamQual_preAdjNorm.x,TeamQualORPM_preAdjNorm.x,TeamQualDRPM_preAdjNorm.x)
test4 <- dplyr::select(test2,Season,Team.y,TeamQual_preAdjNorm.y,TeamQualORPM_preAdjNorm.y,TeamQualDRPM_preAdjNorm.y)

seasondata <- seasondata %>% left_join(test3,by=c("Team.x","Season"))
seasondata <- seasondata %>% left_join(test4,by=c("Team.y","Season"))

seasondata$Box1TeamAvg_new.x <- -1*seasondata$Box1Adjustment_new.x + seasondata$Box1InjAdj.x
seasondata$Box1TeamAvg_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQual_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box2TeamAvg_new.x <- -1*seasondata$Box2Adjustment_new.x + seasondata$Box2InjAdj.x
seasondata$Box2TeamAvg_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQualORPM_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box3TeamAvg_new.x <- -1*seasondata$Box3Adjustment_new.x + seasondata$Box3InjAdj.x
seasondata$Box3TeamAvg_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQualDRPM_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box1TeamAvg_new.y <- -1*seasondata$Box1Adjustment_new.y + seasondata$Box1InjAdj.y
seasondata$Box1TeamAvg_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQual_preAdj.y[seasondata$GameCount.y==1]
seasondata$Box2TeamAvg_new.y <- -1*seasondata$Box2Adjustment_new.y + seasondata$Box2InjAdj.y
seasondata$Box2TeamAvg_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQualORPM_preAdj.y[seasondata$GameCount.y==1]
seasondata$Box3TeamAvg_new.y <- -1*seasondata$Box3Adjustment_new.y + seasondata$Box3InjAdj.y
seasondata$Box3TeamAvg_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQualDRPM_preAdj.y[seasondata$GameCount.y==1]
seasondata$Box1TeamAvg_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQual_preAdj.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box1TeamAvg_new.x
seasondata$Box2TeamAvg_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQualORPM_preAdj.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box2TeamAvg_new.x
seasondata$Box3TeamAvg_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQualDRPM_preAdj.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box3TeamAvg_new.x
seasondata$Box1TeamAvg_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQual_preAdj.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box1TeamAvg_new.y
seasondata$Box2TeamAvg_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQualORPM_preAdj.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box2TeamAvg_new.y
seasondata$Box3TeamAvg_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQualDRPM_preAdj.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box3TeamAvg_new.y

seasondata$Box1Adjustmentv2.x <- -1*(seasondata$Box1TeamAvg_new.x-seasondata$Box1InjAdj.x)
seasondata$Box2Adjustmentv2.x <- -1*(seasondata$Box2TeamAvg_new.x-seasondata$Box2InjAdj.x)
seasondata$Box3Adjustmentv2.x <- -1*(seasondata$Box3TeamAvg_new.x-seasondata$Box3InjAdj.x)
seasondata$Box1Adjustmentv2.y <- -1*(seasondata$Box1TeamAvg_new.y-seasondata$Box1InjAdj.y)
seasondata$Box2Adjustmentv2.y <- -1*(seasondata$Box2TeamAvg_new.y-seasondata$Box2InjAdj.y)
seasondata$Box3Adjustmentv2.y <- -1*(seasondata$Box3TeamAvg_new.y-seasondata$Box3InjAdj.y)

#adjustment for normalization
seasondata$Box1TeamAvgNorm_new.x <- -1*seasondata$Box1AdjustmentNorm_new.x + seasondata$Box1InjAdjNorm.x
seasondata$Box1TeamAvgNorm_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQual_preAdjNorm.x[seasondata$GameCount.x==1]
seasondata$Box2TeamAvgNorm_new.x <- -1*seasondata$Box2AdjustmentNorm_new.x + seasondata$Box2InjAdjNorm.x
seasondata$Box2TeamAvgNorm_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQualORPM_preAdjNorm.x[seasondata$GameCount.x==1]
seasondata$Box3TeamAvgNorm_new.x <- -1*seasondata$Box3AdjustmentNorm_new.x + seasondata$Box3InjAdjNorm.x
seasondata$Box3TeamAvgNorm_new.x[seasondata$GameCount.x==1] <- seasondata$TeamQualDRPM_preAdjNorm.x[seasondata$GameCount.x==1]
seasondata$Box1TeamAvgNorm_new.y <- -1*seasondata$Box1AdjustmentNorm_new.y + seasondata$Box1InjAdjNorm.y
seasondata$Box1TeamAvgNorm_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQual_preAdjNorm.y[seasondata$GameCount.y==1]
seasondata$Box2TeamAvgNorm_new.y <- -1*seasondata$Box2AdjustmentNorm_new.y + seasondata$Box2InjAdjNorm.y
seasondata$Box2TeamAvgNorm_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQualORPM_preAdjNorm.y[seasondata$GameCount.y==1]
seasondata$Box3TeamAvgNorm_new.y <- -1*seasondata$Box3AdjustmentNorm_new.y + seasondata$Box3InjAdjNorm.y
seasondata$Box3TeamAvgNorm_new.y[seasondata$GameCount.y==1] <- seasondata$TeamQualDRPM_preAdjNorm.y[seasondata$GameCount.y==1]
seasondata$Box1TeamAvgNorm_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQual_preAdjNorm.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box1TeamAvgNorm_new.x
seasondata$Box2TeamAvgNorm_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQualORPM_preAdjNorm.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box2TeamAvgNorm_new.x
seasondata$Box3TeamAvgNorm_new.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamQualDRPM_preAdjNorm.x+seasondata$SeasGamesCURRWGT.x*seasondata$Box3TeamAvgNorm_new.x
seasondata$Box1TeamAvgNorm_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQual_preAdjNorm.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box1TeamAvgNorm_new.y
seasondata$Box2TeamAvgNorm_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQualORPM_preAdjNorm.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box2TeamAvgNorm_new.y
seasondata$Box3TeamAvgNorm_new.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamQualDRPM_preAdjNorm.y+seasondata$SeasGamesCURRWGT.y*seasondata$Box3TeamAvgNorm_new.y

seasondata$Box1AdjustmentNormv2.x <- -1*(seasondata$Box1TeamAvgNorm_new.x-seasondata$Box1InjAdjNorm.x)
seasondata$Box2AdjustmentNormv2.x <- -1*(seasondata$Box2TeamAvgNorm_new.x-seasondata$Box2InjAdjNorm.x)
seasondata$Box3AdjustmentNormv2.x <- -1*(seasondata$Box3TeamAvgNorm_new.x-seasondata$Box3InjAdjNorm.x)
seasondata$Box1AdjustmentNormv2.y <- -1*(seasondata$Box1TeamAvgNorm_new.y-seasondata$Box1InjAdjNorm.y)
seasondata$Box2AdjustmentNormv2.y <- -1*(seasondata$Box2TeamAvgNorm_new.y-seasondata$Box2InjAdjNorm.y)
seasondata$Box3AdjustmentNormv2.y <- -1*(seasondata$Box3TeamAvgNorm_new.y-seasondata$Box3InjAdjNorm.y)

seasondata <- seasondata %>%
  dplyr::group_by(Season) %>%
  dplyr::mutate(Box1SeasonAvg = (cumsum(Box1TeamAvg_new.x)+cumsum(Box1TeamAvg_new.y))/(cumsum(ones)+cumsum(ones)),
                Box2SeasonAvg = (cumsum(Box2TeamAvg_new.x)+cumsum(Box2TeamAvg_new.y))/(cumsum(ones)+cumsum(ones)),
                Box3SeasonAvg = (cumsum(Box3TeamAvg_new.x)+cumsum(Box3TeamAvg_new.y))/(cumsum(ones)+cumsum(ones)),
                OnOffSeasonAvg = (cumsum(OnOffTeamAvg.x)+cumsum(OnOffTeamAvg.y))/(cumsum(ones)+cumsum(ones)),
                Box1SeasonAvgNorm = (cumsum(Box1TeamAvgNorm_new.x)+cumsum(Box1TeamAvgNorm_new.y))/(cumsum(ones)+cumsum(ones)),
                Box2SeasonAvgNorm = (cumsum(Box2TeamAvgNorm_new.x)+cumsum(Box2TeamAvgNorm_new.y))/(cumsum(ones)+cumsum(ones)),
                Box3SeasonAvgNorm = (cumsum(Box3TeamAvgNorm_new.x)+cumsum(Box3TeamAvgNorm_new.y))/(cumsum(ones)+cumsum(ones)))

seasondata$Pace2.x[seasondata$Season=="2022-23"] <- seasondata$Pace2.x[seasondata$Season=="2022-23"]+1*seasondata$SeasGamesPREVWGT.x[seasondata$Season=="2022-23"]
seasondata$Pace2.y[seasondata$Season=="2022-23"] <- seasondata$Pace2.y[seasondata$Season=="2022-23"]+1*seasondata$SeasGamesPREVWGT.y[seasondata$Season=="2022-23"]
seasondata$PaceXPace2 <- seasondata$Pace2.x*seasondata$Pace2.y
seasondata$PaceXSq2 <- seasondata$Pace2.x*seasondata$Pace2.x
seasondata$PaceYSq2 <- seasondata$Pace2.y*seasondata$Pace2.y


#version 6 (only first ten games)
seasondata$NetAdjMarg6.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg.x
seasondata$NetAdjMarg6.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg6.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg.y
seasondata$NetAdjMarg6.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv6 <- (seasondata$NetAdjMarg6.x-seasondata$NetAdjMarg6.y)^2*((seasondata$NetAdjMarg6.x-seasondata$NetAdjMarg6.y)>0)
seasondata$AdjNetMarSqNegv6 <- (seasondata$NetAdjMarg6.x-seasondata$NetAdjMarg6.y)^2*((seasondata$NetAdjMarg6.x-seasondata$NetAdjMarg6.y)<=0)
seasondata$AdjNetMarCubv6 <- (seasondata$NetAdjMarg6.x-seasondata$NetAdjMarg6.y)^3

seasondata$NetAdjMarg620.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg20.x
seasondata$NetAdjMarg620.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg620.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg20.y
seasondata$NetAdjMarg620.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg610.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10.x
seasondata$NetAdjMarg610.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg610.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10.y
seasondata$NetAdjMarg610.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg65.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5.x
seasondata$NetAdjMarg65.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg65.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5.y
seasondata$NetAdjMarg65.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal6.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal.x
seasondata$PointsTotal6.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal6.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal.y
seasondata$PointsTotal6.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal620.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal20.x
seasondata$PointsTotal620.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal620.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal20.y
seasondata$PointsTotal620.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal610.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal610.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal610.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal610.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal65.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal65.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal65.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal65.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal6.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal.x
seasondata$DefPointsTotal6.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal6.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal.y
seasondata$DefPointsTotal6.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal620.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal620.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal620.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal620.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal610.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal610.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal610.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal610.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal65.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal65.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal65.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal65.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints6.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints.x
seasondata$OppTotalPoints6.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints6.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints.y
seasondata$OppTotalPoints6.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints620.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints20.x
seasondata$OppTotalPoints620.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints620.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints20.y
seasondata$OppTotalPoints620.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints610.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints10.x
seasondata$OppTotalPoints610.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints610.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints10.y
seasondata$OppTotalPoints610.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints65.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppTotalPoints5.x
seasondata$OppTotalPoints65.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints65.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppTotalPoints5.y
seasondata$OppTotalPoints65.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints6.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints.x
seasondata$OppDefTotalPoints6.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints6.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints.y
seasondata$OppDefTotalPoints6.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints620.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints20.x
seasondata$OppDefTotalPoints620.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints620.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints20.y
seasondata$OppDefTotalPoints620.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints610.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints10.x
seasondata$OppDefTotalPoints610.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints610.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints10.y
seasondata$OppDefTotalPoints610.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints65.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppDefTotalPoints5.x
seasondata$OppDefTotalPoints65.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints65.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppDefTotalPoints5.y
seasondata$OppDefTotalPoints65.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]

#version7
seasondata$NetAdjMarg7.x <- seasondata$NewOff2.x-seasondata$NewDef2.x
seasondata$NetAdjMarg7.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg7.y <- seasondata$NewOff2.y-seasondata$NewDef2.y
seasondata$NetAdjMarg7.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv7 <- (seasondata$NetAdjMarg7.x-seasondata$NetAdjMarg7.y)^2*((seasondata$NetAdjMarg7.x-seasondata$NetAdjMarg7.y)>0)
seasondata$AdjNetMarSqNegv7 <- (seasondata$NetAdjMarg7.x-seasondata$NetAdjMarg7.y)^2*((seasondata$NetAdjMarg7.x-seasondata$NetAdjMarg7.y)<=0)
seasondata$AdjNetMarCubv7 <- (seasondata$NetAdjMarg7.x-seasondata$NetAdjMarg7.y)^3

seasondata$NetAdjMarg720.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg20v2.x
seasondata$NetAdjMarg720.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg720.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg20v2.y
seasondata$NetAdjMarg720.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg710.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v2.x
seasondata$NetAdjMarg710.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg710.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v2.y
seasondata$NetAdjMarg710.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg75.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v2.x
seasondata$NetAdjMarg75.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg75.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v2.y
seasondata$NetAdjMarg75.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal7v2.x <- seasondata$NewOff2.x
seasondata$PointsTotal7v2.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal7v2.y <- seasondata$NewOff2.y
seasondata$PointsTotal7v2.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal720.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal20.x
seasondata$PointsTotal720.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal720.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal20.y
seasondata$PointsTotal720.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal710.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal710.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal710.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal710.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal75.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal75.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal75.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal75.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal7v2.x <- seasondata$NewDef2.x
seasondata$DefPointsTotal7v2.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal7v2.y <- seasondata$NewDef2.y
seasondata$DefPointsTotal7v2.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal720.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal720.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal720.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal720.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal710.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal710.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal710.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal710.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal75.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal75.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal75.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal75.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints7v2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewOff2.x
seasondata$OppTotalPoints7v2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints7v2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewOff2.y
seasondata$OppTotalPoints7v2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints720.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewOff220.x
seasondata$OppTotalPoints720.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints720.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewOff220.y
seasondata$OppTotalPoints720.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints710.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewOff210.x
seasondata$OppTotalPoints710.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints710.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewOff210.y
seasondata$OppTotalPoints710.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints75.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewOff25.x
seasondata$OppTotalPoints75.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints75.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewOff25.y
seasondata$OppTotalPoints75.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints7v2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewDef2.x
seasondata$OppDefTotalPoints7v2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints7v2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewDef2.y
seasondata$OppDefTotalPoints7v2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints720.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewDef220.x
seasondata$OppDefTotalPoints720.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints720.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewDef220.y
seasondata$OppDefTotalPoints720.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints710.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppNewDef210.x
seasondata$OppDefTotalPoints710.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints710.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppNewDef210.y
seasondata$OppDefTotalPoints710.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints75.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppNewDef25.x
seasondata$OppDefTotalPoints75.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints75.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppNewDef25.y
seasondata$OppDefTotalPoints75.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]


#fix teamqual to 10 games only
seasondata$Box1TeamAvg_newv3.x <- -1*seasondata$Box1Adjustment_new.x + seasondata$Box1InjAdj.x
seasondata$Box1TeamAvg_newv3.x[seasondata$GameCount.x==1] <- seasondata$TeamQual_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box2TeamAvg_newv3.x <- -1*seasondata$Box2Adjustment_new.x + seasondata$Box2InjAdj.x
seasondata$Box2TeamAvg_newv3.x[seasondata$GameCount.x==1] <- seasondata$TeamQualORPM_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box3TeamAvg_newv3.x <- -1*seasondata$Box3Adjustment_new.x + seasondata$Box3InjAdj.x
seasondata$Box3TeamAvg_newv3.x[seasondata$GameCount.x==1] <- seasondata$TeamQualDRPM_preAdj.x[seasondata$GameCount.x==1]
seasondata$Box1TeamAvg_newv3.y <- -1*seasondata$Box1Adjustment_new.y + seasondata$Box1InjAdj.y
seasondata$Box1TeamAvg_newv3.y[seasondata$GameCount.y==1] <- seasondata$TeamQual_preAdj.y[seasondata$GameCount.y==1]
seasondata$Box2TeamAvg_newv3.y <- -1*seasondata$Box2Adjustment_new.y + seasondata$Box2InjAdj.y
seasondata$Box2TeamAvg_newv3.y[seasondata$GameCount.y==1] <- seasondata$TeamQualORPM_preAdj.y[seasondata$GameCount.y==1]
seasondata$Box3TeamAvg_newv3.y <- -1*seasondata$Box3Adjustment_new.y + seasondata$Box3InjAdj.y
seasondata$Box3TeamAvg_newv3.y[seasondata$GameCount.y==1] <- seasondata$TeamQualDRPM_preAdj.y[seasondata$GameCount.y==1]

seasondata$Box1TeamAvg_newv3.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$TeamQual_preAdj.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Box1TeamAvg_newv3.x
seasondata$Box2TeamAvg_newv3.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$TeamQualORPM_preAdj.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Box2TeamAvg_newv3.x
seasondata$Box3TeamAvg_newv3.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$TeamQualDRPM_preAdj.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Box3TeamAvg_newv3.x
seasondata$Box1TeamAvg_newv3.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$TeamQual_preAdj.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Box1TeamAvg_newv3.y
seasondata$Box2TeamAvg_newv3.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$TeamQualORPM_preAdj.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Box2TeamAvg_newv3.y
seasondata$Box3TeamAvg_newv3.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$TeamQualDRPM_preAdj.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Box3TeamAvg_newv3.y

seasondata$Box1Adjustmentv3.x <- -1*(seasondata$Box1TeamAvg_newv3.x-seasondata$Box1InjAdj.x)
seasondata$Box2Adjustmentv3.x <- -1*(seasondata$Box2TeamAvg_newv3.x-seasondata$Box2InjAdj.x)
seasondata$Box3Adjustmentv3.x <- -1*(seasondata$Box3TeamAvg_newv3.x-seasondata$Box3InjAdj.x)
seasondata$Box1Adjustmentv3.y <- -1*(seasondata$Box1TeamAvg_newv3.y-seasondata$Box1InjAdj.y)
seasondata$Box2Adjustmentv3.y <- -1*(seasondata$Box2TeamAvg_newv3.y-seasondata$Box2InjAdj.y)
seasondata$Box3Adjustmentv3.y <- -1*(seasondata$Box3TeamAvg_newv3.y-seasondata$Box3InjAdj.y)

seasondata$GameDummyLess10 <- ifelse(seasondata$GameCountAvg<=10,1,0)
seasondata$WinPctX10.x <- seasondata$WinPct.x*seasondata$GameDummyLess10
seasondata$WinPct20X10.x <- seasondata$WinPct20.x*seasondata$GameDummyLess10
seasondata$WinPct10X10.x <- seasondata$WinPct10.x*seasondata$GameDummyLess10
seasondata$WinPct5X10.x <- seasondata$WinPct5.x*seasondata$GameDummyLess10
seasondata$WinPctX10.y <- seasondata$WinPct.y*seasondata$GameDummyLess10
seasondata$WinPct20X10.y <-seasondata$WinPct20.y*seasondata$GameDummyLess10
seasondata$WinPct10X10.y <- seasondata$WinPct10.y*seasondata$GameDummyLess10
seasondata$WinPct5X10.y <- seasondata$WinPct5.y*seasondata$GameDummyLess10

seasondata$Pace3.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevPace.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Pace.x
seasondata$Pace3.x[seasondata$GameCount.x==1] <- seasondata$PrevPace.x[seasondata$GameCount.x==1]
seasondata$Pace3.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevPace.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Pace.y
seasondata$Pace3.y[seasondata$GameCount.y==1] <- seasondata$PrevPace.y[seasondata$GameCount.y==1]

seasondata$Pace3.x[seasondata$Season=="2022-23"] <- seasondata$Pace3.x[seasondata$Season=="2022-23"]+1.5*seasondata$SeasGamesPREVWGT10.x[seasondata$Season=="2022-23"]
seasondata$Pace3.y[seasondata$Season=="2022-23"] <- seasondata$Pace3.y[seasondata$Season=="2022-23"]+1.5*seasondata$SeasGamesPREVWGT10.y[seasondata$Season=="2022-23"]
seasondata$PaceXPace3 <- seasondata$Pace3.x*seasondata$Pace3.y
seasondata$PaceXSq3 <- seasondata$Pace3.x*seasondata$Pace3.x
seasondata$PaceYSq3 <- seasondata$Pace3.y*seasondata$Pace3.y

#fix teamqual to 10 games only
seasondata$OnOffTeamAvg_newv2.x <- -1*seasondata$OnOffAdjustment_new.x + seasondata$OnOffInjAdj.x
seasondata$OnOffTeamAvg_newv2.x[seasondata$GameCount.x==1] <- seasondata$OnOffTeamAvg.x[seasondata$GameCount.x==1]
seasondata$OnOffTeamAvg_newv2.y <- -1*seasondata$OnOffAdjustment_new.y + seasondata$OnOffInjAdj.y
seasondata$OnOffTeamAvg_newv2.y[seasondata$GameCount.y==1] <- seasondata$OnOffTeamAvg.y[seasondata$GameCount.y==1]

seasondata$OnOffTeamAvg_newv2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$OnOffTeamAvg.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OnOffTeamAvg_newv2.x
seasondata$OnOffTeamAvg_newv2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$OnOffTeamAvg.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OnOffTeamAvg_newv2.y

seasondata$OnOffAdjustmentv2.x <- -1*(seasondata$OnOffTeamAvg_newv2.x-seasondata$OnOffInjAdj.x)
seasondata$OnOffAdjustmentv2.y <- -1*(seasondata$OnOffTeamAvg_newv2.y-seasondata$OnOffInjAdj.y)


#version 8 (same as 3 but with starting point of 5 for the opponents)
seasondata$NetAdjMarg8.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg.x
seasondata$NetAdjMarg8.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg8.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg.y
seasondata$NetAdjMarg8.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv8 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^2*((seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)>0)
seasondata$AdjNetMarSqNegv8 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^2*((seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)<=0)
seasondata$AdjNetMarCubv8 <- (seasondata$NetAdjMarg3.x-seasondata$NetAdjMarg3.y)^3

seasondata$NetAdjMarg820.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20.x
seasondata$NetAdjMarg820.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg820.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20.y
seasondata$NetAdjMarg820.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg810.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10.x
seasondata$NetAdjMarg810.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg810.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10.y
seasondata$NetAdjMarg810.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg85.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5.x
seasondata$NetAdjMarg85.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg85.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5.y
seasondata$NetAdjMarg85.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

seasondata$PointsTotal8.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal.x
seasondata$PointsTotal8.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal8.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal.y
seasondata$PointsTotal8.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal820.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal20.x
seasondata$PointsTotal820.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal820.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal20.y
seasondata$PointsTotal820.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal810.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal810.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal810.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal810.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]
seasondata$PointsTotal85.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewOff.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal85.x[seasondata$GameCount.x==1] <- seasondata$StartNewOff.x[seasondata$GameCount.x==1]
seasondata$PointsTotal85.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewOff.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal85.y[seasondata$GameCount.y==1] <- seasondata$StartNewOff.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal8.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal.x
seasondata$DefPointsTotal8.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal8.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal.y
seasondata$DefPointsTotal8.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal820.x <- seasondata$SeasGamesPREVWGT.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal820.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal820.y <- seasondata$SeasGamesPREVWGT.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal820.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal810.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal810.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal810.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal810.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal85.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$StartNewDef.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal85.x[seasondata$GameCount.x==1] <- seasondata$StartNewDef.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal85.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$StartNewDef.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal85.y[seasondata$GameCount.y==1] <- seasondata$StartNewDef.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints8.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints.x
seasondata$OppTotalPoints8.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppTotalPoints8.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints.y
seasondata$OppTotalPoints8.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppTotalPoints820.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints20.x
seasondata$OppTotalPoints820.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppTotalPoints820.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints20.y
seasondata$OppTotalPoints820.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppTotalPoints810.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints10.x
seasondata$OppTotalPoints810.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppTotalPoints810.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints10.y
seasondata$OppTotalPoints810.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppTotalPoints85.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT5.x*seasondata$OppTotalPoints5.x
seasondata$OppTotalPoints85.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppTotalPoints85.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT5.y*seasondata$OppTotalPoints5.y
seasondata$OppTotalPoints85.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints8.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints.x
seasondata$OppDefTotalPoints8.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints8.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints.y
seasondata$OppDefTotalPoints8.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints820.x <- seasondata$SeasGamesPREVWGT.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints20.x
seasondata$OppDefTotalPoints820.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints820.y <- seasondata$SeasGamesPREVWGT.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints20.y
seasondata$OppDefTotalPoints820.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints810.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints10.x
seasondata$OppDefTotalPoints810.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints810.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints10.y
seasondata$OppDefTotalPoints810.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints85.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT5.x*seasondata$OppDefTotalPoints5.x
seasondata$OppDefTotalPoints85.x[seasondata$GameCount.x==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints85.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$TeamAvgPts_seasALT+seasondata$SeasGamesCURRWGT5.y*seasondata$OppDefTotalPoints5.y
seasondata$OppDefTotalPoints85.y[seasondata$GameCount.y==1] <- seasondata$TeamAvgPts_seasALT[seasondata$GameCount.y==1]

seasondata <- seasondata %>%
  dplyr::mutate(NetAdjMargv3.x = CapMargin.x + OppNetMargin.x,
                NetAdjMargv3.y = CapMargin.y + OppNetMargin.y)


#version 9 (same as 3 but using margins capped at 25)
seasondata$NetAdjMarg9.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv3.x
seasondata$NetAdjMarg9.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg9.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv3.y
seasondata$NetAdjMarg9.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv9 <- (seasondata$NetAdjMarg9.x-seasondata$NetAdjMarg9.y)^2*((seasondata$NetAdjMarg9.x-seasondata$NetAdjMarg9.y)>0)
seasondata$AdjNetMarSqNegv9 <- (seasondata$NetAdjMarg9.x-seasondata$NetAdjMarg9.y)^2*((seasondata$NetAdjMarg9.x-seasondata$NetAdjMarg9.y)<=0)
seasondata$AdjNetMarCubv9 <- (seasondata$NetAdjMarg9.x-seasondata$NetAdjMarg9.y)^3


seasondata$HomeMargNoOut2 <- ifelse(seasondata$HomeMargin>25,25,ifelse(seasondata$HomeMargin<(-25),-25,seasondata$HomeMargin))
seasondata$HomeMargNoOut3 <- ifelse(seasondata$HomeMargin>20,20,ifelse(seasondata$HomeMargin<(-20),-20,seasondata$HomeMargin))


load("MarginConvFactors.RData")

seasondata <- seasondata %>% left_join(my_data,by=c("HomeMargin")) %>% ungroup()

seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

load("modelgammaV2additions10.RData")
seasondata_add10OLD <- seasondata_add10
myfunc1 <- function(y) {
  

  CapMargin2.x <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))
  
  CapMargin2.y <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))
  
  CapMargin220.x <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))
  
  CapMargin220.y <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))
  
  CapMargin210.x <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))
  
  CapMargin210.y <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))
  
  CapMargin25.x <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))
  
  CapMargin25.y <- with(seasondata,(sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) - sum(HomeMargNoOut2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))

  CapMargin3.x <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))
  
  CapMargin3.y <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))
  
  CapMargin320.x <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))
  
  CapMargin320.y <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))
  
  CapMargin310.x <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))
  
  CapMargin310.y <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))
  
  CapMargin35.x <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))
  
  CapMargin35.y <- with(seasondata,(sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) - sum(HomeMargNoOut3[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))
  
  CapMargin4.x <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))
  
  CapMargin4.y <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))
  
  CapMargin420.x <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))
  
  CapMargin420.y <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))
  
  CapMargin410.x <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))
  
  CapMargin410.y <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))
  
  CapMargin45.x <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))
  
  CapMargin45.y <- with(seasondata,(sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) - sum(HomeMargNoOut4[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))
  
  CapMargin5v2.x <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))
  
  CapMargin5v2.y <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))
  
  CapMargin520.x <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))
  
  CapMargin520.y <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))
  
  CapMargin510.x <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))
  
  CapMargin510.y <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))
  
  CapMargin55.x <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))
  
  CapMargin55.y <- with(seasondata,(sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) - sum(HomeMargNoOut5[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))
  
  
  GameID <- with(seasondata,GameID[y])
  Date <- with(seasondata,Date[y])
  
  value <- data.frame(GameID,Date,CapMargin2.x,CapMargin2.y,CapMargin220.x,CapMargin220.y,CapMargin210.x,
                      CapMargin210.y,CapMargin25.x,CapMargin25.y,CapMargin3.x,CapMargin3.y,CapMargin320.x,CapMargin320.y,CapMargin310.x,
                      CapMargin310.y,CapMargin35.x,CapMargin35.y,CapMargin4.x,CapMargin4.y,CapMargin420.x,CapMargin420.y,CapMargin410.x,
                      CapMargin410.y,CapMargin45.x,CapMargin45.y,CapMargin5v2.x,CapMargin5v2.y,CapMargin520.x,CapMargin520.y,CapMargin510.x,
                      CapMargin510.y,CapMargin55.x,CapMargin55.y)
  
  return(value)
  
}
tic()
numCores <- detectCores()
seasondata_add10 <- mclapply(c(start:length(seasondata$Date)), myfunc1,mc.cores = numCores-2)
seasondata_add10 <- dplyr::bind_rows(seasondata_add10, .id = "column_label")
toc()

seasondata_add10OLD <- seasondata_add10OLD[!(seasondata_add10OLD$GameID %in% seasondata_add10$GameID),]
seasondata_add10 <- rbind(seasondata_add10OLD,seasondata_add10)
seasondata_add10 <- distinct(seasondata_add10)
save(seasondata_add10,file="modelgammaV2additions10.RData")
#check
length(seasondata$Date)==length(seasondata_add10$Date)
seasondata <- seasondata %>% left_join(seasondata_add10,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)


seasondata <- seasondata %>%
  dplyr::mutate(NetAdjMargv11.x = CapMargin2.x + OppNetMargin.x,
                NetAdjMargv11.y = CapMargin2.y + OppNetMargin.y,
                NetAdjMargv12.x = CapMargin3.x + OppNetMargin.x,
                NetAdjMargv12.y = CapMargin3.y + OppNetMargin.y,
                NetAdjMargv13.x = CapMargin4.x + OppNetMargin.x,
                NetAdjMargv13.y = CapMargin4.y + OppNetMargin.y,
                NetAdjMargv14.x = CapMargin5v2.x + OppNetMargin.x,
                NetAdjMargv14.y = CapMargin5v2.y + OppNetMargin.y,
                NetAdjMarg20v11.x = CapMargin220.x+OppNetMargin20.x,
                NetAdjMarg10v11.x = CapMargin210.x+OppNetMargin10.x,
                NetAdjMarg5v11.x = CapMargin25.x+OppNetMargin5.x,
                NetAdjMarg20v11.y = CapMargin220.y+OppNetMargin20.y,
                NetAdjMarg10v11.y = CapMargin210.y+OppNetMargin10.y,
                NetAdjMarg5v11.y = CapMargin25.y+OppNetMargin5.y,
                NetAdjMarg20v12.x = CapMargin320.x+OppNetMargin20.x,
                NetAdjMarg10v12.x = CapMargin310.x+OppNetMargin10.x,
                NetAdjMarg5v12.x = CapMargin35.x+OppNetMargin5.x,
                NetAdjMarg20v12.y = CapMargin320.y+OppNetMargin20.y,
                NetAdjMarg10v12.y = CapMargin310.y+OppNetMargin10.y,
                NetAdjMarg5v12.y = CapMargin35.y+OppNetMargin5.y,
                NetAdjMarg20v13.x = CapMargin420.x+OppNetMargin20.x,
                NetAdjMarg10v13.x = CapMargin410.x+OppNetMargin10.x,
                NetAdjMarg5v13.x = CapMargin45.x+OppNetMargin5.x,
                NetAdjMarg20v13.y = CapMargin420.y+OppNetMargin20.y,
                NetAdjMarg10v13.y = CapMargin410.y+OppNetMargin10.y,
                NetAdjMarg5v13.y = CapMargin45.y+OppNetMargin5.y,
                NetAdjMarg20v14.x = CapMargin520.x+OppNetMargin20.x,
                NetAdjMarg10v14.x = CapMargin510.x+OppNetMargin10.x,
                NetAdjMarg5v14.x = CapMargin55.x+OppNetMargin5.x,
                NetAdjMarg20v14.y = CapMargin520.y+OppNetMargin20.y,
                NetAdjMarg10v14.y = CapMargin510.y+OppNetMargin10.y,
                NetAdjMarg5v14.y = CapMargin55.y+OppNetMargin5.y)

#version 11 (skip ten to avoid variable naming confusion)
seasondata$NetAdjMarg11.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv11.x
seasondata$NetAdjMarg11.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg11.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv11.y
seasondata$NetAdjMarg11.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv11 <- (seasondata$NetAdjMarg11.x-seasondata$NetAdjMarg11.y)^2*((seasondata$NetAdjMarg11.x-seasondata$NetAdjMarg11.y)>0)
seasondata$AdjNetMarSqNegv11 <- (seasondata$NetAdjMarg11.x-seasondata$NetAdjMarg11.y)^2*((seasondata$NetAdjMarg11.x-seasondata$NetAdjMarg11.y)<=0)
seasondata$AdjNetMarCubv11 <- (seasondata$NetAdjMarg11.x-seasondata$NetAdjMarg11.y)^3

seasondata$NetAdjMarg1120.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v11.x
seasondata$NetAdjMarg1120.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1120.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v11.y
seasondata$NetAdjMarg1120.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg1110.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v11.x
seasondata$NetAdjMarg1110.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1110.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v11.y
seasondata$NetAdjMarg1110.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg115.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v11.x
seasondata$NetAdjMarg115.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg115.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v11.y
seasondata$NetAdjMarg115.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

#version 12 (skip ten to avoid variable naming confusion)
seasondata$NetAdjMarg12.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv12.x
seasondata$NetAdjMarg12.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg12.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv12.y
seasondata$NetAdjMarg12.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv12 <- (seasondata$NetAdjMarg12.x-seasondata$NetAdjMarg12.y)^2*((seasondata$NetAdjMarg12.x-seasondata$NetAdjMarg12.y)>0)
seasondata$AdjNetMarSqNegv12 <- (seasondata$NetAdjMarg12.x-seasondata$NetAdjMarg12.y)^2*((seasondata$NetAdjMarg12.x-seasondata$NetAdjMarg12.y)<=0)
seasondata$AdjNetMarCubv12 <- (seasondata$NetAdjMarg12.x-seasondata$NetAdjMarg12.y)^3

seasondata$NetAdjMarg1220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v12.x
seasondata$NetAdjMarg1220.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v12.y
seasondata$NetAdjMarg1220.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg1210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v12.x
seasondata$NetAdjMarg1210.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v12.y
seasondata$NetAdjMarg1210.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg125.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v12.x
seasondata$NetAdjMarg125.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg125.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v12.y
seasondata$NetAdjMarg125.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

#version 13 (skip ten to avoid variable naming confusion)
seasondata$NetAdjMarg13.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv13.x
seasondata$NetAdjMarg13.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg13.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv13.y
seasondata$NetAdjMarg13.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv13 <- (seasondata$NetAdjMarg13.x-seasondata$NetAdjMarg13.y)^2*((seasondata$NetAdjMarg13.x-seasondata$NetAdjMarg13.y)>0)
seasondata$AdjNetMarSqNegv13 <- (seasondata$NetAdjMarg13.x-seasondata$NetAdjMarg13.y)^2*((seasondata$NetAdjMarg13.x-seasondata$NetAdjMarg13.y)<=0)
seasondata$AdjNetMarCubv13 <- (seasondata$NetAdjMarg13.x-seasondata$NetAdjMarg13.y)^3

seasondata$NetAdjMarg1320.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v13.x
seasondata$NetAdjMarg1320.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1320.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v13.y
seasondata$NetAdjMarg1320.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg1310.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v13.x
seasondata$NetAdjMarg1310.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1310.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v13.y
seasondata$NetAdjMarg1310.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg135.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v13.x
seasondata$NetAdjMarg135.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg135.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v13.y
seasondata$NetAdjMarg135.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]

#version 14 (skip ten to avoid variable naming confusion)
seasondata$NetAdjMarg14.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMargv14.x
seasondata$NetAdjMarg14.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg14.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMargv14.y
seasondata$NetAdjMarg14.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv14 <- (seasondata$NetAdjMarg14.x-seasondata$NetAdjMarg14.y)^2*((seasondata$NetAdjMarg14.x-seasondata$NetAdjMarg14.y)>0)
seasondata$AdjNetMarSqNegv14 <- (seasondata$NetAdjMarg14.x-seasondata$NetAdjMarg14.y)^2*((seasondata$NetAdjMarg14.x-seasondata$NetAdjMarg14.y)<=0)
seasondata$AdjNetMarCubv14 <- (seasondata$NetAdjMarg14.x-seasondata$NetAdjMarg14.y)^3

seasondata$NetAdjMarg1420.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20v14.x
seasondata$NetAdjMarg1420.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1420.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20v14.y
seasondata$NetAdjMarg1420.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg1410.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10v14.x
seasondata$NetAdjMarg1410.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg1410.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10v14.y
seasondata$NetAdjMarg1410.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]
seasondata$NetAdjMarg145.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMarginv2.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5v14.x
seasondata$NetAdjMarg145.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMarginv2.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg145.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMarginv2.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5v14.y
seasondata$NetAdjMarg145.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMarginv2.y[seasondata$GameCount.y==1]


#create team groups to avoid playoff noise in home/away
goodhomecourt <- c("San Antonio Spurs","Indiana Pacers","Denver Nuggets","Oklahoma City Thunder","Utah Jazz","Memphis Grizzlies","Los Angeles Lakers","Portland Trail Blazers")
badhomecourt <- c("Brooklyn Nets","Chicago Bulls","Cleveland Cavaliers","Milwaukee Bucks","Minnesota Timberwolves","New York Knicks","Orlando Magic","Sacramento Kings")
goodawaycourt <- c("Brooklyn Nets","Chicago Bulls","Dallas Mavericks","LA Clippers","Miami Heat","Boston Celtics","Memphis Grizzlies","Oklahoma City Thunder","San Antonio Spurs")
badawaycourt <- c("Charlotte Hornets","Cleveland Cavaliers","Denver Nuggets","Detroit Pistons","Golden State Warriors","Philadelphia 76ers","Washington Wizards","Portland Trail Blazers")
seasondata$goodhomecourt.x <- ifelse(seasondata$Team.x %in% goodhomecourt,1,0)
seasondata$badhomecourt.x <- ifelse(seasondata$Team.x %in% badhomecourt,1,0)
seasondata$avghomecourt.x <- ifelse(!((seasondata$Team.x %in% goodhomecourt)|(seasondata$Team.x %in% badhomecourt)),1,0)
seasondata$goodhomecourt.y <- ifelse(seasondata$Team.y %in% goodhomecourt,1,0)
seasondata$badhomecourt.y <- ifelse(seasondata$Team.y %in% badhomecourt,1,0)
seasondata$avghomecourt.y <- ifelse(!((seasondata$Team.y %in% goodhomecourt)|(seasondata$Team.y %in% badhomecourt)),1,0)
seasondata$goodawaycourt.x <- ifelse(seasondata$Team.x %in% goodawaycourt,1,0)
seasondata$badawaycourt.x <- ifelse(seasondata$Team.x %in% badawaycourt,1,0)
seasondata$avgawaycourt.x <- ifelse(!((seasondata$Team.x %in% goodawaycourt)|(seasondata$Team.x %in% badawaycourt)),1,0)
seasondata$goodawaycourt.y <- ifelse(seasondata$Team.y %in% goodawaycourt,1,0)
seasondata$badawaycourt.y <- ifelse(seasondata$Team.y %in% badawaycourt,1,0)
seasondata$avgawaycourt.y <- ifelse(!((seasondata$Team.y %in% goodawaycourt)|(seasondata$Team.y %in% badawaycourt)),1,0)

seasondata$goodhomecourtpo.x <- seasondata$goodhomecourt.x*seasondata$Playoffs
seasondata$badhomecourtpo.x <- seasondata$badhomecourt.x*seasondata$Playoffs
seasondata$avghomecourtpo.x <- seasondata$avghomecourt.x*seasondata$Playoffs
seasondata$goodhomecourtpo.y <- seasondata$goodhomecourt.y*seasondata$Playoffs
seasondata$badhomecourtpo.y <- seasondata$badhomecourt.y*seasondata$Playoffs
seasondata$avghomecourtpo.y <- seasondata$avghomecourt.y*seasondata$Playoffs
seasondata$goodawaycourtpo.x <- seasondata$goodawaycourt.x*seasondata$Playoffs
seasondata$badawaycourtpo.x <- seasondata$badawaycourt.x*seasondata$Playoffs
seasondata$avgawaycourtpo.x <- seasondata$avgawaycourt.x*seasondata$Playoffs
seasondata$goodawaycourtpo.y <- seasondata$goodawaycourt.y*seasondata$Playoffs
seasondata$badawaycourtpo.y <- seasondata$badawaycourt.y*seasondata$Playoffs
seasondata$avgawaycourtpo.y <- seasondata$avgawaycourt.y*seasondata$Playoffs

grthomecourt <- c("San Antonio Spurs","Indiana Pacers","Denver Nuggets","Utah Jazz","Portland Trail Blazers")
prgdhomecourt <- c("Oklahoma City Thunder","Memphis Grizzlies","Los Angeles Lakers","Dallas Mavericks")
prbadhomecourt <- c("Chicago Bulls","Cleveland Cavaliers","Milwaukee Bucks","New York Knicks","Sacramento Kings")
rlybadhomecourt <- c("Brooklyn Nets","Minnesota Timberwolves","Orlando Magic")
grtawaycourt <- c("Brooklyn Nets","Dallas Mavericks","LA Clippers","Boston Celtics","Memphis Grizzlies","San Antonio Spurs")
prgdawaycourt <- c("Chicago Bulls","Miami Heat","Oklahoma City Thunder","Toronto Raptors")
prbadawaycourt <- c("Denver Nuggets","Indiana Pacers","Portland Trail Blazers","Utah Jazz","Sacramento Kings","Minnesota Timberwolves")
rlybadawaycourt <- c("Charlotte Hornets","Cleveland Cavaliers","Detroit Pistons","Golden State Warriors","Philadelphia 76ers","Washington Wizards")

seasondata$grthomecourt.x <- ifelse(seasondata$Team.x %in% grthomecourt,1,0)
seasondata$prgdhomecourt.x <- ifelse(seasondata$Team.x %in% prgdhomecourt,1,0)
seasondata$prbadhomecourt.x <- ifelse(seasondata$Team.x %in% prbadhomecourt,1,0)
seasondata$rlybadhomecourt.x <- ifelse(seasondata$Team.x %in% rlybadhomecourt,1,0)
seasondata$avg2homecourt.x <- ifelse(!((seasondata$Team.x %in% grthomecourt)|(seasondata$Team.x %in% prgdhomecourt)|(seasondata$Team.x %in% prbadhomecourt)|(seasondata$Team.x %in% rlybadhomecourt)),1,0)
seasondata$grthomecourt.y <- ifelse(seasondata$Team.y %in% grthomecourt,1,0)
seasondata$prgdhomecourt.y <- ifelse(seasondata$Team.y %in% prgdhomecourt,1,0)
seasondata$prbadhomecourt.y <- ifelse(seasondata$Team.y %in% prbadhomecourt,1,0)
seasondata$rlybadhomecourt.y <- ifelse(seasondata$Team.y %in% rlybadhomecourt,1,0)
seasondata$avg2homecourt.y <- ifelse(!((seasondata$Team.y %in% grthomecourt)|(seasondata$Team.y %in% prgdhomecourt)|(seasondata$Team.y %in% prbadhomecourt)|(seasondata$Team.y %in% rlybadhomecourt)),1,0)

seasondata$grtawaycourt.x <- ifelse(seasondata$Team.x %in% grtawaycourt,1,0)
seasondata$prgdawaycourt.x <- ifelse(seasondata$Team.x %in% prgdawaycourt,1,0)
seasondata$prbadawaycourt.x <- ifelse(seasondata$Team.x %in% prbadawaycourt,1,0)
seasondata$rlybadawaycourt.x <- ifelse(seasondata$Team.x %in% rlybadawaycourt,1,0)
seasondata$avg2awaycourt.x <- ifelse(!((seasondata$Team.x %in% grtawaycourt)|(seasondata$Team.x %in% prgdawaycourt)|(seasondata$Team.x %in% prbadawaycourt)|(seasondata$Team.x %in% rlybadawaycourt)),1,0)
seasondata$grtawaycourt.y <- ifelse(seasondata$Team.y %in% grtawaycourt,1,0)
seasondata$prgdawaycourt.y <- ifelse(seasondata$Team.y %in% prgdawaycourt,1,0)
seasondata$prbadawaycourt.y <- ifelse(seasondata$Team.y %in% prbadawaycourt,1,0)
seasondata$rlybadawaycourt.y <- ifelse(seasondata$Team.y %in% rlybadawaycourt,1,0)
seasondata$avg2awaycourt.y <- ifelse(!((seasondata$Team.y %in% grtawaycourt)|(seasondata$Team.y %in% prgdawaycourt)|(seasondata$Team.y %in% prbadawaycourt)|(seasondata$Team.y %in% rlybadawaycourt)),1,0)

gamemax <- seasondata %>%
  dplyr::group_by(Season) %>%
  dplyr::summarize(TotSeasGames = mean(PrevGameMax.x,na.rm=TRUE))
gamemax <- gamemax %>%
  mutate(TotSeasGames = sapply(1:n(), function(x) ifelse(x<n(),TotSeasGames[x+1],TotSeasGames[x])))
seasondata <- seasondata %>% left_join(gamemax,by=c("Season")) %>% ungroup()

seasondata$GameCountAdj <- (seasondata$GameCountAvg/seasondata$TotSeasGames)*82
seasondata$GameDummyLess5Adj <- ifelse(seasondata$GameCountAdj<=5,1,0)
seasondata$GameDummyLess20Adj <- ifelse(seasondata$GameCountAdj<=20,1,0)
seasondata$GameDummy2040Adj <- ifelse(seasondata$GameCountAdj>20&seasondata$GameCountAdj<=40,1,0)
seasondata$GameDummy4060Adj <- ifelse(seasondata$GameCountAdj>40&seasondata$GameCountAdj<=60,1,0)
seasondata$GameDummy6082Adj <- ifelse(seasondata$GameCountAdj>60&seasondata$GameCountAdj<=82,1,0)
seasondata$GameCountAdjSq <- seasondata$GameCountAdj*seasondata$GameCountAdj

stdev <- seasondata %>%
  dplyr::group_by(SeasNum,Team.x) %>%
  dplyr::summarize(NetAdjMargPREV = mean(PrevPtsTot.x) - mean(PrevDefPtsTot.x)) %>% 
  ungroup()

stdev2 <- stdev %>%
  dplyr::group_by(SeasNum) %>%
  dplyr::summarize(stdevmarg = sd(NetAdjMargPREV,na.rm=TRUE),
                   NetAdjMargPREV = mean(NetAdjMargPREV,na.rm=TRUE)) %>% 
  ungroup()

modelpred <- lm(formula = stdevmarg ~ SeasNum,data=stdev2[stdev2$SeasNum>=5,])  
stdev2$stdevmargpred <- predict(modelpred,newdata=stdev2)

stdev3 <- stdev2[,c("SeasNum","stdevmargpred")]

seasondata <- seasondata %>% left_join(stdev3, by=c("SeasNum")) %>% ungroup()

seasondata$TeamQualORPMNorm2.x <- seasondata$TeamQualORPMNorm.x*seasondata$stdevmargpred/4.714447
seasondata$TeamQualDRPMNorm2.x <- seasondata$TeamQualDRPMNorm.x*seasondata$stdevmargpred/4.714447
seasondata$Box1AdjustmentNormv22.x <- seasondata$Box1AdjustmentNormv2.x*seasondata$stdevmargpred/4.714447
seasondata$Box2AdjustmentNormv22.x <- seasondata$Box2AdjustmentNormv2.x*seasondata$stdevmargpred/4.714447
seasondata$Box3AdjustmentNormv22.x <- seasondata$Box3AdjustmentNormv2.x*seasondata$stdevmargpred/4.714447
seasondata$TeamQualORPMNorm2.y <- seasondata$TeamQualORPMNorm.y*seasondata$stdevmargpred/4.714447
seasondata$TeamQualDRPMNorm2.y <- seasondata$TeamQualDRPMNorm.y*seasondata$stdevmargpred/4.714447
seasondata$Box1AdjustmentNormv22.y <- seasondata$Box1AdjustmentNormv2.y*seasondata$stdevmargpred/4.714447
seasondata$Box2AdjustmentNormv22.y <- seasondata$Box2AdjustmentNormv2.y*seasondata$stdevmargpred/4.714447
seasondata$Box3AdjustmentNormv22.y <- seasondata$Box3AdjustmentNormv2.y*seasondata$stdevmargpred/4.714447

stdev <- seasondata %>%
  dplyr::group_by(SeasNum,Team.x) %>%
  dplyr::summarize(TeamQualRPM.x = mean(TeamQualRPM.x,na.rm=TRUE),
                   TeamQualORPM.x = mean(TeamQualORPM.x,na.rm=TRUE),
                   TeamQualDRPM.x = mean(TeamQualDRPM.x,na.rm=TRUE)) %>% 
  ungroup()

stdev2 <- stdev %>%
  dplyr::group_by(SeasNum) %>%
  dplyr::summarize(stdevmargRPM = sd(TeamQualRPM.x,na.rm=TRUE),
                   stdevmargORPM = sd(TeamQualORPM.x,na.rm=TRUE),
                   stdevmargDRPM = sd(TeamQualDRPM.x,na.rm=TRUE)) %>% 
  ungroup()
# stdev2$stdevmargRPM <- stdev2$stdevmargRPM/stdev2$stdevmargRPM[1]
# stdev2$stdevmargORPM <- stdev2$stdevmargORPM/stdev2$stdevmargORPM[1]
# stdev2$stdevmargDRPM <- stdev2$stdevmargDRPM/stdev2$stdevmargDRPM[1]

seasondata <- seasondata %>% left_join(stdev2, by=c("SeasNum")) %>% ungroup()

seasondata$TeamQualORPMNorm3.x <- seasondata$TeamQualORPM.x/seasondata$stdevmargORPM
seasondata$TeamQualDRPMNorm3.x <- seasondata$TeamQualDRPM.x/seasondata$stdevmargDRPM
seasondata$Box1AdjustmentNormv23.x <- seasondata$Box1Adjustmentv2.x/seasondata$stdevmargRPM
seasondata$Box2AdjustmentNormv23.x <- seasondata$Box2Adjustmentv2.x/seasondata$stdevmargORPM
seasondata$Box3AdjustmentNormv23.x <- seasondata$Box3Adjustmentv2.x/seasondata$stdevmargDRPM
seasondata$TeamQualORPMNorm3.y <- seasondata$TeamQualORPM.y/seasondata$stdevmargORPM
seasondata$TeamQualDRPMNorm3.y <- seasondata$TeamQualDRPM.y/seasondata$stdevmargDRPM
seasondata$Box1AdjustmentNormv23.y <- seasondata$Box1Adjustmentv2.y/seasondata$stdevmargRPM
seasondata$Box2AdjustmentNormv23.y <- seasondata$Box2Adjustmentv2.y/seasondata$stdevmargORPM
seasondata$Box3AdjustmentNormv23.y <- seasondata$Box3Adjustmentv2.y/seasondata$stdevmargDRPM


seasondata$TeamQualDRPMsq.x <- seasondata$TeamQualDRPM.x*seasondata$TeamQualDRPM.x
seasondata$TeamQualORPMsq.x <- seasondata$TeamQualORPM.x*seasondata$TeamQualORPM.x
seasondata$TeamQualRPMsq.x <- seasondata$TeamQualRPM.x*seasondata$TeamQualRPM.x
seasondata$Box1Adjustmentv2sq.x <- seasondata$Box1Adjustmentv2.x*seasondata$Box1Adjustmentv2.x
seasondata$Box2Adjustmentv2sq.x <- seasondata$Box2Adjustmentv2.x*seasondata$Box2Adjustmentv2.x
seasondata$Box3Adjustmentv2sq.x <- seasondata$Box3Adjustmentv2.x*seasondata$Box3Adjustmentv2.x
seasondata$TeamQualDRPMsq.y <- seasondata$TeamQualDRPM.y*seasondata$TeamQualDRPM.y
seasondata$TeamQualORPMsq.y <- seasondata$TeamQualORPM.y*seasondata$TeamQualORPM.y
seasondata$TeamQualRPMsq.y <- seasondata$TeamQualRPM.y*seasondata$TeamQualRPM.y
seasondata$Box1Adjustmentv2sq.y <- seasondata$Box1Adjustmentv2.y*seasondata$Box1Adjustmentv2.y
seasondata$Box2Adjustmentv2sq.y <- seasondata$Box2Adjustmentv2.y*seasondata$Box2Adjustmentv2.y
seasondata$Box3Adjustmentv2sq.y <- seasondata$Box3Adjustmentv2.y*seasondata$Box3Adjustmentv2.y
# box1avg <- seasondata[seasondata$RegularSeason==1,] %>%
#   dplyr::group_by(SeasNum) %>%
#   arrange(Season,Date,GameID) %>%
#   dplyr::summarize(Box1SeasonAvgPREV = Box1SeasonAvg[n()],
#             Box2SeasonAvgPREV = Box2SeasonAvg[n()],
#             Box3SeasonAvgPREV = Box3SeasonAvg[n()],
#             OnOffSeasonAvgPREV = OnOffSeasonAvg[n()])
# box1avg$SeasNum <- box1avg$SeasNum+1
# 
# seasondata <- seasondata %>% left_join(box1avg,by=c("SeasNum"))
# seasondata$Box1SeasonAvg[seasondata$GameCountAvg<=3] <- seasondata$Box1SeasonAvgPREV[seasondata$GameCountAvg<=3]
# seasondata$Box2SeasonAvg[seasondata$GameCountAvg<=3] <- seasondata$Box2SeasonAvgPREV[seasondata$GameCountAvg<=3]
# seasondata$Box3SeasonAvg[seasondata$GameCountAvg<=3] <- seasondata$Box3SeasonAvgPREV[seasondata$GameCountAvg<=3]
# seasondata$OnOffSeasonAvg[seasondata$GameCountAvg<=3] <- seasondata$OnOffSeasonAvgPREV[seasondata$GameCountAvg<=3]

seasondata$onesv2 <- ifelse(seasondata$Date>=today,NA,seasondata$ones)
#for home court advantage add fouls and ref turnovers
load("modelgammaV2additions20.RData")
load("homecourtrefs.RData")
seasondata <- left_join(seasondata,homecourtrefs[,c("GameID","Team.x","RefTOs.x")],by=c("GameID","Team.x")) %>% ungroup()
seasondata <- left_join(seasondata,homecourtrefs[,c("GameID","Team.y","RefTOs.y")],by=c("GameID","Team.y")) %>% ungroup()
seasondata_add20OLD <- seasondata_add20

seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

myfunc20 <- function(y) {

  Fouls.x <- with(seasondata,(sum(PF.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(PF.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE)))

  Fouls.y <- with(seasondata,(sum(PF.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(PF.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE)))

  FoulsOpp.x <- with(seasondata,(sum(PF.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(PF.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE)))

  FoulsOpp.y <- with(seasondata,(sum(PF.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(PF.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE)))

  Fouls10.x <- with(seasondata,(sum(PF.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(PF.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE)))

  Fouls10.y <- with(seasondata,(sum(PF.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(PF.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE)))

  FoulsOpp10.x <- with(seasondata,(sum(PF.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(PF.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE)))

  FoulsOpp10.y <- with(seasondata,(sum(PF.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(PF.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE)))

  RefTOSum.x <- with(seasondata,(sum(RefTOs.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(RefTOs.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]],na.rm=TRUE)))
  
  RefTOSum.y <- with(seasondata,(sum(RefTOs.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(RefTOs.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]],na.rm=TRUE)))
  
  RefTOSum10.x <- with(seasondata,(sum(RefTOs.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(RefTOs.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)],na.rm=TRUE)))
  
  RefTOSum10.y <- with(seasondata,(sum(RefTOs.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(RefTOs.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE))/(sum(onesv2[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)],na.rm=TRUE) + sum(onesv2[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)],na.rm=TRUE)))

  GameID <- with(seasondata,GameID[y])
  Date <- with(seasondata,Date[y])

  value <- data.frame(GameID,Date,Fouls.x,Fouls.y,FoulsOpp.x,FoulsOpp.y,Fouls10.x,Fouls10.y,FoulsOpp10.x,FoulsOpp10.y,
                      RefTOSum.x,RefTOSum.y,RefTOSum10.x,RefTOSum10.y)

  return(value)

}
tic()
numCores <- detectCores()
seasondata_add20 <- mclapply(c(start:length(seasondata$Date)), myfunc20,mc.cores = numCores-2)
seasondata_add20 <- dplyr::bind_rows(seasondata_add20, .id = "column_label")
toc()

seasondata_add20OLD <- seasondata_add20OLD[!(seasondata_add20OLD$GameID %in% seasondata_add20$GameID),]
seasondata_add20 <- rbind(seasondata_add20OLD,seasondata_add20)
seasondata_add20 <- distinct(seasondata_add20)
save(seasondata_add20,file="modelgammaV2additions20.RData")
#check
length(seasondata$Date)==length(seasondata_add20$Date)
seasondata <- seasondata %>% left_join(seasondata_add20,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)


load("modelgammaV2additions30.RData")
seasondata_add30OLD <- seasondata_add30

seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

myfunc30 <- function(y) {
  
  PrevFouls.x <- with(seasondata,ifelse(!is_empty(Fouls.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),Fouls.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],Fouls.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevFouls.y <- with(seasondata,ifelse(!is_empty(Fouls.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),Fouls.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],Fouls.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevFoulsOpp.x <- with(seasondata,ifelse(!is_empty(FoulsOpp.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),FoulsOpp.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],FoulsOpp.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevFoulsOpp.y <- with(seasondata,ifelse(!is_empty(FoulsOpp.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),FoulsOpp.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],FoulsOpp.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevFouls10.x <- with(seasondata,ifelse(!is_empty(Fouls10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),Fouls10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],Fouls10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevFouls10.y <- with(seasondata,ifelse(!is_empty(Fouls10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),Fouls10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],Fouls10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevFoulsOpp10.x <- with(seasondata,ifelse(!is_empty(FoulsOpp10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),FoulsOpp10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],FoulsOpp10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevFoulsOpp10.y <- with(seasondata,ifelse(!is_empty(FoulsOpp10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),FoulsOpp10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],FoulsOpp10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevRefTOSum.x <- with(seasondata,ifelse(!is_empty(RefTOSum.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),RefTOSum.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],RefTOSum.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevRefTOSum.y <- with(seasondata,ifelse(!is_empty(RefTOSum.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),RefTOSum.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],RefTOSum.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevRefTOSum10.x <- with(seasondata,ifelse(!is_empty(RefTOSum10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),RefTOSum10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],RefTOSum10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevRefTOSum10.y <- with(seasondata,ifelse(!is_empty(RefTOSum10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),RefTOSum10.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],RefTOSum10.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  
  GameID <- with(seasondata,GameID[y])
  Date <- with(seasondata,Date[y])
  
  value <- data.frame(GameID,Date,PrevFouls.x,PrevFouls.y,PrevFoulsOpp.x,PrevFoulsOpp.y,PrevFouls10.x,PrevFouls10.y,PrevFoulsOpp10.x,PrevFoulsOpp10.y,
                      PrevRefTOSum.x,PrevRefTOSum.y,PrevRefTOSum10.x,PrevRefTOSum10.y)
  
  return(value)
  
}
tic()
numCores <- detectCores()
seasondata_add30 <- mclapply(c(start:length(seasondata$Date)), myfunc30,mc.cores = numCores-2)
seasondata_add30 <- dplyr::bind_rows(seasondata_add30, .id = "column_label")
toc()

seasondata_add30OLD <- seasondata_add30OLD[!(seasondata_add30OLD$GameID %in% seasondata_add30$GameID),]
seasondata_add30 <- rbind(seasondata_add30OLD,seasondata_add30)
seasondata_add30 <- distinct(seasondata_add30)
save(seasondata_add30,file="modelgammaV2additions30.RData")
#check
length(seasondata$Date)==length(seasondata_add30$Date)
seasondata <- seasondata %>% left_join(seasondata_add30,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)


seasondata$Fouls2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevFouls.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Fouls.x
seasondata$Fouls2.x[seasondata$GameCount.x==1] <- seasondata$PrevFouls.x[seasondata$GameCount.x==1]
seasondata$Fouls102.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevFouls10.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Fouls10.x
seasondata$Fouls102.x[seasondata$GameCount.x==1] <- seasondata$PrevFouls10.x[seasondata$GameCount.x==1]
seasondata$FoulsOpp2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevFoulsOpp.x+seasondata$SeasGamesCURRWGT10.x*seasondata$FoulsOpp.x
seasondata$FoulsOpp2.x[seasondata$GameCount.x==1] <- seasondata$PrevFoulsOpp.x[seasondata$GameCount.x==1]
seasondata$FoulsOpp102.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevFoulsOpp10.x+seasondata$SeasGamesCURRWGT10.x*seasondata$FoulsOpp10.x
seasondata$FoulsOpp102.x[seasondata$GameCount.x==1] <- seasondata$PrevFoulsOpp10.x[seasondata$GameCount.x==1]
seasondata$RefTOSum2.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevRefTOSum.x+seasondata$SeasGamesCURRWGT10.x*seasondata$RefTOSum.x
seasondata$RefTOSum2.x[seasondata$GameCount.x==1] <- seasondata$PrevRefTOSum.x[seasondata$GameCount.x==1]
seasondata$RefTOSum102.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevRefTOSum10.x+seasondata$SeasGamesCURRWGT10.x*seasondata$RefTOSum10.x
seasondata$RefTOSum102.x[seasondata$GameCount.x==1] <- seasondata$PrevRefTOSum10.x[seasondata$GameCount.x==1]

seasondata$Fouls2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevFouls.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Fouls.y
seasondata$Fouls2.y[seasondata$GameCount.y==1] <- seasondata$PrevFouls.y[seasondata$GameCount.y==1]
seasondata$Fouls102.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevFouls10.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Fouls10.y
seasondata$Fouls102.y[seasondata$GameCount.y==1] <- seasondata$PrevFouls10.y[seasondata$GameCount.y==1]
seasondata$FoulsOpp2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevFoulsOpp.y+seasondata$SeasGamesCURRWGT10.y*seasondata$FoulsOpp.y
seasondata$FoulsOpp2.y[seasondata$GameCount.y==1] <- seasondata$PrevFoulsOpp.y[seasondata$GameCount.y==1]
seasondata$FoulsOpp102.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevFoulsOpp10.y+seasondata$SeasGamesCURRWGT10.y*seasondata$FoulsOpp10.y
seasondata$FoulsOpp102.y[seasondata$GameCount.y==1] <- seasondata$PrevFoulsOpp10.y[seasondata$GameCount.y==1]
seasondata$RefTOSum2.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevRefTOSum.y+seasondata$SeasGamesCURRWGT10.y*seasondata$RefTOSum.y
seasondata$RefTOSum2.y[seasondata$GameCount.y==1] <- seasondata$PrevRefTOSum.y[seasondata$GameCount.y==1]
seasondata$RefTOSum102.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevRefTOSum10.y+seasondata$SeasGamesCURRWGT10.y*seasondata$RefTOSum10.y
seasondata$RefTOSum102.y[seasondata$GameCount.y==1] <- seasondata$PrevRefTOSum10.y[seasondata$GameCount.y==1]

seasondata$TotalFouls.x <- seasondata$Fouls2.x + seasondata$FoulsOpp2.x
seasondata$TotalFouls.y <- seasondata$Fouls2.y + seasondata$FoulsOpp2.y
seasondata$TotalFouls10.x <- seasondata$Fouls102.x + seasondata$FoulsOpp102.x
seasondata$TotalFouls10.y <- seasondata$Fouls102.y + seasondata$FoulsOpp102.y

seasondata$TotalFoulsAvg <- (seasondata$TotalFouls.x + seasondata$TotalFouls.y)/2
seasondata$TotalFoulsAvg10 <- (seasondata$TotalFouls10.x + seasondata$TotalFouls10.y)/2
seasondata$RefTOSumAvg <- (seasondata$RefTOSum2.x + seasondata$RefTOSum2.y)/2
seasondata$RefTOSumAvg10 <- (seasondata$RefTOSum102.x + seasondata$RefTOSum102.y)/2


#Create non-linearities in Vegas terms
seasondata$LineOpenDum1 <- ifelse(abs(seasondata$Line_Open)>10,1,0)
seasondata$LineOpenDum2 <- ifelse(abs(seasondata$Line_Open)<3,1,0)
seasondata$LineOpenDum3 <- ifelse(abs(seasondata$Line_Open)>=3&abs(seasondata$Line_Open)<=10,1,0)
seasondata$Line_OpenSq <- seasondata$Line_Open^2


seasondata$OneZeroComb <- ifelse(seasondata$HomeSeriesStatus=="1-0"&seasondata$Playoffs==1,-1,0)
seasondata$OneZeroComb[seasondata$HomeSeriesStatus=="0-1"&seasondata$Playoffs==1] <- 1

teammerge <- teammerge[teammerge$Date<today,]
save(teammerge,file="additions_teammerge.RData")
save(seasondata,file="allregdata_FINALV2.RData")

