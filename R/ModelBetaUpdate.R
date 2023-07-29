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
library('RSelenium')
library(R.utils)
library('ds4psy')
library(foreign)
library("haven")
library(plm)
library(dplyr)
#setwd(working)

load("modelbetaV1_dataformerge.RData")
load("allregdata_modelalphaV4.RData")
load("allregdata_modelbetaV1FORMERGE.RData")
load("modelbetaV1_teamlist.RData")
seasondata <- seasondata[order(seasondata$Date),]
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
startdate <- today
enddate <- today+7
seasondataOLD <- seasondata[seasondata$Date<(startdate-10),]
start <- length(seasondataOLD$Date)
teamlistMERGEAgain <- teamlistMERGE

#Things that will feed into regression
playerdataMERGE$OnOffAdjustment.x  <- playerdataMERGE$OnOffAdjustment
playerdataMERGE$Box1Adjustment.x  <- playerdataMERGE$Box1Adjustment
playerdataMERGE$Box2Adjustment.x  <- playerdataMERGE$Box2Adjustment
playerdataMERGE$Box3Adjustment.x  <- playerdataMERGE$Box3Adjustment
playerdataMERGE$Box1AdjChg.x  <- playerdataMERGE$Box1AdjChg
playerdataMERGE$superadj.x  <- playerdataMERGE$superadj
playerdataMERGE$staradj.x  <- playerdataMERGE$staradj
playerdataMERGE$solidadj.x  <- playerdataMERGE$solidadj
playerdataMERGE$TeamQualRPM.x  <- playerdataMERGE$TeamQualRPM
playerdataMERGE$TeamQualORPM.x  <- playerdataMERGE$TeamQualORPM
playerdataMERGE$TeamQualDRPM.x  <- playerdataMERGE$TeamQualDRPM
playerdataMERGE$PlayoffExpMin.x  <- playerdataMERGE$PlayoffExpMin
playerdataMERGE$PlayoffExpGame.x  <- playerdataMERGE$PlayoffExpGame
playerdataMERGE$PlayoffExpMinPG.x  <- playerdataMERGE$PlayoffExpMinPG
playerdataMERGE$OnOffTeamAvg.x  <- playerdataMERGE$OnOffTeamAvg
playerdataMERGE$Box1TeamAvg.x  <- playerdataMERGE$Box1TeamAvg
playerdataMERGE$Box2TeamAvg.x  <- playerdataMERGE$Box2TeamAvg
playerdataMERGE$Box3TeamAvg.x  <- playerdataMERGE$Box3TeamAvg
playerdataMERGE$OnOffInjAdj.x  <- playerdataMERGE$OnOffInjAdj
playerdataMERGE$Box1InjAdj.x  <- playerdataMERGE$Box1InjAdj
playerdataMERGE$Box2InjAdj.x  <- playerdataMERGE$Box2InjAdj
playerdataMERGE$Box3InjAdj.x  <- playerdataMERGE$Box3InjAdj
playerdataMERGE$OnOffAdjustment_new.x  <- playerdataMERGE$OnOffAdjustment_new
playerdataMERGE$Box1Adjustment_new.x  <- playerdataMERGE$Box1Adjustment_new
playerdataMERGE$Box2Adjustment_new.x  <- playerdataMERGE$Box2Adjustment_new
playerdataMERGE$Box3Adjustment_new.x  <- playerdataMERGE$Box3Adjustment_new
playerdataMERGE$Box1AdjChg_new.x  <- playerdataMERGE$Box1AdjChg_new
#normalization adjustment
playerdataMERGE$Box1AdjustmentNorm.x  <- playerdataMERGE$Box1AdjustmentNorm
playerdataMERGE$Box2AdjustmentNorm.x  <- playerdataMERGE$Box2AdjustmentNorm
playerdataMERGE$Box3AdjustmentNorm.x  <- playerdataMERGE$Box3AdjustmentNorm
playerdataMERGE$Box1AdjChgNorm.x  <- playerdataMERGE$Box1AdjChgNorm
playerdataMERGE$TeamQualRPMNorm.x  <- playerdataMERGE$TeamQualRPMNorm
playerdataMERGE$TeamQualORPMNorm.x  <- playerdataMERGE$TeamQualORPMNorm
playerdataMERGE$TeamQualDRPMNorm.x  <- playerdataMERGE$TeamQualDRPMNorm
playerdataMERGE$Box1TeamAvgNorm.x  <- playerdataMERGE$Box1TeamAvgNorm
playerdataMERGE$Box2TeamAvgNorm.x  <- playerdataMERGE$Box2TeamAvgNorm
playerdataMERGE$Box3TeamAvgNorm.x  <- playerdataMERGE$Box3TeamAvgNorm
playerdataMERGE$Box1InjAdjNorm.x  <- playerdataMERGE$Box1InjAdjNorm
playerdataMERGE$Box2InjAdjNorm.x  <- playerdataMERGE$Box2InjAdjNorm
playerdataMERGE$Box3InjAdjNorm.x  <- playerdataMERGE$Box3InjAdjNorm
playerdataMERGE$Box1AdjustmentNorm_new.x  <- playerdataMERGE$Box1AdjustmentNorm_new
playerdataMERGE$Box2AdjustmentNorm_new.x  <- playerdataMERGE$Box2AdjustmentNorm_new
playerdataMERGE$Box3AdjustmentNorm_new.x  <- playerdataMERGE$Box3AdjustmentNorm_new
playerdataMERGE$Box1AdjChgNorm_new.x  <- playerdataMERGE$Box1AdjChgNorm_new




playerdataMERGE$OnOffAdjustment.y  <- playerdataMERGE$OnOffAdjustment
playerdataMERGE$Box1Adjustment.y  <- playerdataMERGE$Box1Adjustment
playerdataMERGE$Box2Adjustment.y  <- playerdataMERGE$Box2Adjustment
playerdataMERGE$Box3Adjustment.y  <- playerdataMERGE$Box3Adjustment
playerdataMERGE$Box1AdjChg.y  <- playerdataMERGE$Box1AdjChg
playerdataMERGE$superadj.y  <- playerdataMERGE$superadj
playerdataMERGE$staradj.y <- playerdataMERGE$staradj
playerdataMERGE$solidadj.y  <- playerdataMERGE$solidadj
playerdataMERGE$TeamQualRPM.y  <- playerdataMERGE$TeamQualRPM
playerdataMERGE$TeamQualORPM.y  <- playerdataMERGE$TeamQualORPM
playerdataMERGE$TeamQualDRPM.y  <- playerdataMERGE$TeamQualDRPM
playerdataMERGE$PlayoffExpMin.y  <- playerdataMERGE$PlayoffExpMin
playerdataMERGE$PlayoffExpGame.y  <- playerdataMERGE$PlayoffExpGame
playerdataMERGE$PlayoffExpMinPG.y  <- playerdataMERGE$PlayoffExpMinPG
playerdataMERGE$OnOffTeamAvg.y  <- playerdataMERGE$OnOffTeamAvg
playerdataMERGE$Box1TeamAvg.y  <- playerdataMERGE$Box1TeamAvg
playerdataMERGE$Box2TeamAvg.y  <- playerdataMERGE$Box2TeamAvg
playerdataMERGE$Box3TeamAvg.y  <- playerdataMERGE$Box3TeamAvg
playerdataMERGE$OnOffInjAdj.y  <- playerdataMERGE$OnOffInjAdj
playerdataMERGE$Box1InjAdj.y  <- playerdataMERGE$Box1InjAdj
playerdataMERGE$Box2InjAdj.y  <- playerdataMERGE$Box2InjAdj
playerdataMERGE$Box3InjAdj.y  <- playerdataMERGE$Box3InjAdj
playerdataMERGE$OnOffAdjustment_new.y  <- playerdataMERGE$OnOffAdjustment_new
playerdataMERGE$Box1Adjustment_new.y  <- playerdataMERGE$Box1Adjustment_new
playerdataMERGE$Box2Adjustment_new.y  <- playerdataMERGE$Box2Adjustment_new
playerdataMERGE$Box3Adjustment_new.y  <- playerdataMERGE$Box3Adjustment_new
playerdataMERGE$Box1AdjChg_new.y  <- playerdataMERGE$Box1AdjChg_new
#normalization adjustment
playerdataMERGE$Box1AdjustmentNorm.y  <- playerdataMERGE$Box1AdjustmentNorm
playerdataMERGE$Box2AdjustmentNorm.y  <- playerdataMERGE$Box2AdjustmentNorm
playerdataMERGE$Box3AdjustmentNorm.y  <- playerdataMERGE$Box3AdjustmentNorm
playerdataMERGE$Box1AdjChgNorm.y  <- playerdataMERGE$Box1AdjChgNorm
playerdataMERGE$TeamQualRPMNorm.y  <- playerdataMERGE$TeamQualRPMNorm
playerdataMERGE$TeamQualORPMNorm.y  <- playerdataMERGE$TeamQualORPMNorm
playerdataMERGE$TeamQualDRPMNorm.y  <- playerdataMERGE$TeamQualDRPMNorm
playerdataMERGE$Box1TeamAvgNorm.y  <- playerdataMERGE$Box1TeamAvgNorm
playerdataMERGE$Box2TeamAvgNorm.y  <- playerdataMERGE$Box2TeamAvgNorm
playerdataMERGE$Box3TeamAvgNorm.y  <- playerdataMERGE$Box3TeamAvgNorm
playerdataMERGE$Box1InjAdjNorm.y  <- playerdataMERGE$Box1InjAdjNorm
playerdataMERGE$Box2InjAdjNorm.y  <- playerdataMERGE$Box2InjAdjNorm
playerdataMERGE$Box3InjAdjNorm.y  <- playerdataMERGE$Box3InjAdjNorm
playerdataMERGE$Box1AdjustmentNorm_new.y  <- playerdataMERGE$Box1AdjustmentNorm_new
playerdataMERGE$Box2AdjustmentNorm_new.y  <- playerdataMERGE$Box2AdjustmentNorm_new
playerdataMERGE$Box3AdjustmentNorm_new.y  <- playerdataMERGE$Box3AdjustmentNorm_new
playerdataMERGE$Box1AdjChgNorm_new.y  <- playerdataMERGE$Box1AdjChgNorm_new


playerdataMERGE$Team.x  <- playerdataMERGE$Team
playerdataMERGE$Team.y  <- playerdataMERGE$Team

playerdataMERGE.x <- dplyr::select(playerdataMERGE,Team.x,Date,OnOffAdjustment.x,
                            Box1Adjustment.x  ,
                            Box2Adjustment.x  ,
                            Box3Adjustment.x  ,
                            Box1AdjChg.x,staradj.x,superadj.x,solidadj.x,
                            Box1TeamAvg.x,Box2TeamAvg.x,Box3TeamAvg.x,
                            OnOffTeamAvg.x,Box1InjAdj.x,Box2InjAdj.x,Box3InjAdj.x,
                            OnOffInjAdj.x,OnOffAdjustment_new.x,
                            Box1Adjustment_new.x  ,
                            Box2Adjustment_new.x  ,
                            Box3Adjustment_new.x  ,
                            Box1AdjChg_new.x,
                            Box1AdjustmentNorm.x  ,
                            Box2AdjustmentNorm.x  ,
                            Box3AdjustmentNorm.x  ,
                            Box1AdjChgNorm.x,
                            Box1TeamAvgNorm.x,Box2TeamAvgNorm.x,Box3TeamAvgNorm.x,
                            Box1InjAdjNorm.x,Box2InjAdjNorm.x,Box3InjAdjNorm.x,
                            Box1AdjustmentNorm_new.x  ,
                            Box2AdjustmentNorm_new.x  ,
                            Box3AdjustmentNorm_new.x  ,
                            Box1AdjChgNorm_new.x)

playerdataMERGE.y <- dplyr::select(playerdataMERGE,Team.y,Date,OnOffAdjustment.y,
                           Box1Adjustment.y  ,
                            Box2Adjustment.y  ,
                            Box3Adjustment.y  ,
                             Box1AdjChg.y,staradj.y,superadj.y,solidadj.y,
                           Box1TeamAvg.y,Box2TeamAvg.y,Box3TeamAvg.y,
                           OnOffTeamAvg.y,Box1InjAdj.y,Box2InjAdj.y,Box3InjAdj.y,
                           OnOffInjAdj.y,OnOffAdjustment_new.y,
                           Box1Adjustment_new.y  ,
                           Box2Adjustment_new.y  ,
                           Box3Adjustment_new.y  ,
                           Box1AdjChg_new.y,
                           Box1AdjustmentNorm.y  ,
                           Box2AdjustmentNorm.y  ,
                           Box3AdjustmentNorm.y  ,
                           Box1AdjChgNorm.y,
                           Box1TeamAvgNorm.y,Box2TeamAvgNorm.y,Box3TeamAvgNorm.y,
                           Box1InjAdjNorm.y,Box2InjAdjNorm.y,Box3InjAdjNorm.y,
                           Box1AdjustmentNorm_new.y  ,
                           Box2AdjustmentNorm_new.y  ,
                           Box3AdjustmentNorm_new.y  ,
                           Box1AdjChgNorm_new.y)

seasondata$DateOld <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today


seasondata <- seasondata %>% 
  left_join(playerdataMERGE.x,by = c("Team.x","Date")) %>%
  ungroup()
seasondata <- seasondata %>% 
  left_join(playerdataMERGE.y,by = c("Team.y","Date")) %>%
  ungroup()

seasondata$Date <- seasondata$DateOld
seasondata <- dplyr::select(seasondata,-DateOld)

#some additional edits (allow vary by possession, home court adjustment for recent games, try to adjust for injuries in opponent strength)
seasondata$EstPace <- (seasondata$Pace2.x + seasondata$Pace2.y)/2
modelEstPace <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + 
                     + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y + OppPtsPerPossAvg2.x + 
                     + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y + RegularSeason + Playoffs + BtoB.x + BtoB.y + 
                     + Bubble + Pandemic + PartialPandemic + Pace2.x + Pace2.y + Pace2.x*RegularSeason + Pace2.y*RegularSeason + PaceXSq2 + PaceYSq2 + PaceXPace2 + PaceXSq2*RegularSeason + PaceYSq2*RegularSeason + PaceXPace2*RegularSeason, data = seasondata)
seasondata$EstPace <- predict(modelEstPace,newdata=seasondata)
seasondata <- seasondata %>%
  dplyr::mutate(PPXPace1 = PtsPerPossAvg2.x*EstPace , PPXPace2 = PtsPerPossAvg2.y*EstPace , PPXPace3 = DefPtsPerPossAvg2.x*EstPace , PPXPace4 = DefPtsPerPossAvg2.y*EstPace , PPXPace5 =OppPtsPerPossAvg2.x*EstPace , PPXPace6 = OppPtsPerPossAvg2.y*EstPace , PPXPace7 = OppDefPtsPerPossAvg2.x*EstPace , PPXPace8 = OppDefPtsPerPossAvg2.y*EstPace)


seasondata <- seasondata %>% left_join(seasondata2,by = c("GameID","Date")) %>% ungroup()
for (i in start:length(seasondata$Date)) {
  # for (i in 1:length(seasondata$Date)) {
  
  seasondata$NumGameAll.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i],(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])
  seasondata$NumGameAll.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i],(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])
  seasondata$NumHomeAll.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])
  seasondata$NumHomeAll.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])
  
  seasondata$NumGame20.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))
  seasondata$NumGame10.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))
  seasondata$NumGame5.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))
  seasondata$NumHome20.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19))
  seasondata$NumHome10.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9))
  seasondata$NumHome5.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4))
  seasondata$NumGame20.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))
  seasondata$NumGame10.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))
  seasondata$NumGame5.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))
  seasondata$NumHome20.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19))
  seasondata$NumHome10.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9))
  seasondata$NumHome5.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4))
  
}

seasondata$HomeAdjAll.x <- ifelse(seasondata$NumGameAll.x==0,0,(seasondata$NumHomeAll.x-(seasondata$NumGameAll.x-seasondata$NumHomeAll.x))/seasondata$NumGameAll.x)
seasondata$HomeAdjAll.y <- ifelse(seasondata$NumGameAll.y==0,0,(seasondata$NumHomeAll.y-(seasondata$NumGameAll.y-seasondata$NumHomeAll.y))/seasondata$NumGameAll.y)

seasondata$HomeAdj20.x <- ifelse(seasondata$NumGame20.x==0,0,(seasondata$NumHome20.x-(seasondata$NumGame20.x-seasondata$NumHome20.x))/seasondata$NumGame20.x)
seasondata$HomeAdj20.y <- ifelse(seasondata$NumGame20.y==0,0,(seasondata$NumHome20.y-(seasondata$NumGame20.y-seasondata$NumHome20.y))/seasondata$NumGame20.y)
seasondata$HomeAdj10.x <- ifelse(seasondata$NumGame10.x==0,0,(seasondata$NumHome10.x-(seasondata$NumGame10.x-seasondata$NumHome10.x))/seasondata$NumGame10.x)
seasondata$HomeAdj10.y <- ifelse(seasondata$NumGame10.y==0,0,(seasondata$NumHome10.y-(seasondata$NumGame10.y-seasondata$NumHome10.y))/seasondata$NumGame10.y)
seasondata$HomeAdj5.x <- ifelse(seasondata$NumGame5.x==0,0,(seasondata$NumHome5.x-(seasondata$NumGame5.x-seasondata$NumHome5.x))/seasondata$NumGame5.x)
seasondata$HomeAdj5.y <- ifelse(seasondata$NumGame5.y==0,0,(seasondata$NumHome5.y-(seasondata$NumGame5.y-seasondata$NumHome5.y))/seasondata$NumGame5.y)

seasondata2 <- seasondata[,c("GameID","Date","NumGameAll.x","NumGameAll.y","NumHomeAll.x",
                             "NumHomeAll.y","NumGame20.x","NumGame10.x","NumGame5.x",
                             "NumHome20.x","NumHome10.x","NumHome5.x","NumGame20.y",
                             "NumGame10.y","NumGame5.y","NumHome20.y","NumHome10.y",
                             "NumHome5.y","HomeAdjAll.x","HomeAdjAll.y","HomeAdj20.x","HomeAdj20.y",
                             "HomeAdj10.x","HomeAdj10.y","HomeAdj5.x","HomeAdj5.y")]
save(seasondata2, file = paste("allregdata_modelbetaV1FORMERGE.RData", sep=""))

#Things that will feed into regression
teamlistMERGE$OppWinPct.x <- teamlistMERGE$OppWinPct
teamlistMERGE$OppWinPct20.x <- teamlistMERGE$OppWinPct20
teamlistMERGE$OppWinPct10.x <- teamlistMERGE$OppWinPct10
teamlistMERGE$OppWinPct5.x <- teamlistMERGE$OppWinPct5
teamlistMERGE$OppOnOffAdj.x <- teamlistMERGE$OppOnOffAdj
teamlistMERGE$OppOnOffAdj20.x <- teamlistMERGE$OppOnOffAdj20
teamlistMERGE$OppOnOffAdj10.x <- teamlistMERGE$OppOnOffAdj10
teamlistMERGE$OppOnOffAdj5.x <- teamlistMERGE$OppOnOffAdj5
teamlistMERGE$OppBox1Adj.x <- teamlistMERGE$OppBox1Adj
teamlistMERGE$OppBox1Adj20.x <- teamlistMERGE$OppBox1Adj20 
teamlistMERGE$OppBox1Adj10.x <- teamlistMERGE$OppBox1Adj10
teamlistMERGE$OppBox1Adj5.x <- teamlistMERGE$OppBox1Adj5
teamlistMERGE$OppBox2Adj.x <- teamlistMERGE$OppBox2Adj
teamlistMERGE$OppBox2Adj20.x <- teamlistMERGE$OppBox2Adj20 
teamlistMERGE$OppBox2Adj10.x <- teamlistMERGE$OppBox2Adj10
teamlistMERGE$OppBox2Adj5.x <- teamlistMERGE$OppBox2Adj5
teamlistMERGE$OppBox3Adj.x <- teamlistMERGE$OppBox3Adj 
teamlistMERGE$OppBox3Adj20.x <- teamlistMERGE$OppBox3Adj20
teamlistMERGE$OppBox3Adj10.x <- teamlistMERGE$OppBox3Adj10
teamlistMERGE$OppBox3Adj5.x <- teamlistMERGE$OppBox3Adj5
teamlistMERGE$OwnOnOffAdj.x <- teamlistMERGE$OwnOnOffAdj
teamlistMERGE$OwnOnOffAdj20.x <- teamlistMERGE$OwnOnOffAdj20
teamlistMERGE$OwnOnOffAdj10.x <- teamlistMERGE$OwnOnOffAdj10
teamlistMERGE$OwnOnOffAdj5.x <- teamlistMERGE$OwnOnOffAdj5
teamlistMERGE$OwnBox1Adj.x <- teamlistMERGE$OwnBox1Adj
teamlistMERGE$OwnBox1Adj20.x <- teamlistMERGE$OwnBox1Adj20
teamlistMERGE$OwnBox1Adj10.x <- teamlistMERGE$OwnBox1Adj10
teamlistMERGE$OwnBox1Adj5.x <- teamlistMERGE$OwnBox1Adj5
teamlistMERGE$OwnBox2Adj.x <- teamlistMERGE$OwnBox2Adj
teamlistMERGE$OwnBox2Adj20.x <- teamlistMERGE$OwnBox2Adj20
teamlistMERGE$OwnBox2Adj10.x <- teamlistMERGE$OwnBox2Adj10
teamlistMERGE$OwnBox2Adj5.x <- teamlistMERGE$OwnBox2Adj5
teamlistMERGE$OwnBox3Adj.x <- teamlistMERGE$OwnBox3Adj 
teamlistMERGE$OwnBox3Adj20.x <- teamlistMERGE$OwnBox3Adj20
teamlistMERGE$OwnBox3Adj10.x <- teamlistMERGE$OwnBox3Adj10
teamlistMERGE$OwnBox3Adj5.x <- teamlistMERGE$OwnBox3Adj5

teamlistMERGE$OppWinPct.y <- teamlistMERGE$OppWinPct
teamlistMERGE$OppWinPct20.y <- teamlistMERGE$OppWinPct20
teamlistMERGE$OppWinPct10.y <- teamlistMERGE$OppWinPct10
teamlistMERGE$OppWinPct5.y <- teamlistMERGE$OppWinPct5
teamlistMERGE$OppOnOffAdj.y <- teamlistMERGE$OppOnOffAdj
teamlistMERGE$OppOnOffAdj20.y <- teamlistMERGE$OppOnOffAdj20
teamlistMERGE$OppOnOffAdj10.y <- teamlistMERGE$OppOnOffAdj10
teamlistMERGE$OppOnOffAdj5.y <- teamlistMERGE$OppOnOffAdj5
teamlistMERGE$OppBox1Adj.y <- teamlistMERGE$OppBox1Adj
teamlistMERGE$OppBox1Adj20.y <- teamlistMERGE$OppBox1Adj20 
teamlistMERGE$OppBox1Adj10.y <- teamlistMERGE$OppBox1Adj10
teamlistMERGE$OppBox1Adj5.y <- teamlistMERGE$OppBox1Adj5
teamlistMERGE$OppBox2Adj.y <- teamlistMERGE$OppBox2Adj
teamlistMERGE$OppBox2Adj20.y <- teamlistMERGE$OppBox2Adj20 
teamlistMERGE$OppBox2Adj10.y <- teamlistMERGE$OppBox2Adj10
teamlistMERGE$OppBox2Adj5.y <- teamlistMERGE$OppBox2Adj5
teamlistMERGE$OppBox3Adj.y <- teamlistMERGE$OppBox3Adj 
teamlistMERGE$OppBox3Adj20.y <- teamlistMERGE$OppBox3Adj20
teamlistMERGE$OppBox3Adj10.y <- teamlistMERGE$OppBox3Adj10
teamlistMERGE$OppBox3Adj5.y <- teamlistMERGE$OppBox3Adj5
teamlistMERGE$OwnOnOffAdj.y <- teamlistMERGE$OwnOnOffAdj
teamlistMERGE$OwnOnOffAdj20.y <- teamlistMERGE$OwnOnOffAdj20
teamlistMERGE$OwnOnOffAdj10.y <- teamlistMERGE$OwnOnOffAdj10
teamlistMERGE$OwnOnOffAdj5.y <- teamlistMERGE$OwnOnOffAdj5
teamlistMERGE$OwnBox1Adj.y <- teamlistMERGE$OwnBox1Adj
teamlistMERGE$OwnBox1Adj20.y <- teamlistMERGE$OwnBox1Adj20
teamlistMERGE$OwnBox1Adj10.y <- teamlistMERGE$OwnBox1Adj10
teamlistMERGE$OwnBox1Adj5.y <- teamlistMERGE$OwnBox1Adj5
teamlistMERGE$OwnBox2Adj.y <- teamlistMERGE$OwnBox2Adj
teamlistMERGE$OwnBox2Adj20.y <- teamlistMERGE$OwnBox2Adj20
teamlistMERGE$OwnBox2Adj10.y <- teamlistMERGE$OwnBox2Adj10
teamlistMERGE$OwnBox2Adj5.y <- teamlistMERGE$OwnBox2Adj5
teamlistMERGE$OwnBox3Adj.y <- teamlistMERGE$OwnBox3Adj 
teamlistMERGE$OwnBox3Adj20.y <- teamlistMERGE$OwnBox3Adj20
teamlistMERGE$OwnBox3Adj10.y <- teamlistMERGE$OwnBox3Adj10
teamlistMERGE$OwnBox3Adj5.y <- teamlistMERGE$OwnBox3Adj5

teamlistMERGE$Team.x  <- teamlistMERGE$Team
teamlistMERGE$Team.y  <- teamlistMERGE$Team

teamlistMERGE.x <- dplyr::select(teamlistMERGE,Team.x,Date,OppWinPct.x ,
                          OppWinPct20.x,
                          OppWinPct10.x ,
                          OppWinPct5.x,
                          OppOnOffAdj.x ,
                          OppOnOffAdj20.x ,
                          OppOnOffAdj10.x ,
                          OppOnOffAdj5.x ,
                          OppBox1Adj.x ,
                          OppBox1Adj20.x ,
                          OppBox1Adj10.x ,
                          OppBox1Adj5.x ,
                          OppBox2Adj.x ,
                          OppBox2Adj20.x ,
                          OppBox2Adj10.x ,
                          OppBox2Adj5.x ,
                          OppBox3Adj.x , 
                          OppBox3Adj20.x,
                          OppBox3Adj10.x ,
                          OppBox3Adj5.x ,
                          OwnOnOffAdj.x ,
                          OwnOnOffAdj20.x ,
                          OwnOnOffAdj10.x ,
                          OwnOnOffAdj5.x ,
                          OwnBox1Adj.x ,
                          OwnBox1Adj20.x ,
                          OwnBox1Adj10.x ,
                          OwnBox1Adj5.x ,
                          OwnBox2Adj.x ,
                          OwnBox2Adj20.x ,
                          OwnBox2Adj10.x ,
                          OwnBox2Adj5.x ,
                          OwnBox3Adj.x ,
                          OwnBox3Adj20.x ,
                          OwnBox3Adj10.x ,
                          OwnBox3Adj5.x)

teamlistMERGE.y <- dplyr::select(teamlistMERGE,Team.y,Date,OppWinPct.y ,
                          OppWinPct20.y,
                          OppWinPct10.y ,
                          OppWinPct5.y,
                          OppOnOffAdj.y ,
                          OppOnOffAdj20.y ,
                          OppOnOffAdj10.y ,
                          OppOnOffAdj5.y ,
                          OppBox1Adj.y ,
                          OppBox1Adj20.y ,
                          OppBox1Adj10.y ,
                          OppBox1Adj5.y ,
                          OppBox2Adj.y ,
                          OppBox2Adj20.y ,
                          OppBox2Adj10.y ,
                          OppBox2Adj5.y ,
                          OppBox3Adj.y , 
                          OppBox3Adj20.y,
                          OppBox3Adj10.y ,
                          OppBox3Adj5.y ,
                          OwnOnOffAdj.y ,
                          OwnOnOffAdj20.y ,
                          OwnOnOffAdj10.y ,
                          OwnOnOffAdj5.y ,
                          OwnBox1Adj.y ,
                          OwnBox1Adj20.y ,
                          OwnBox1Adj10.y ,
                          OwnBox1Adj5.y ,
                          OwnBox2Adj.y ,
                          OwnBox2Adj20.y ,
                          OwnBox2Adj10.y ,
                          OwnBox2Adj5.y ,
                          OwnBox3Adj.y ,
                          OwnBox3Adj20.y ,
                          OwnBox3Adj10.y ,
                          OwnBox3Adj5.y)

seasondata$DateOld <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

seasondata <- seasondata %>% 
  left_join(teamlistMERGE.x,by = c("Team.x","Date")) %>%
  ungroup()
seasondata <- seasondata %>% 
  left_join(teamlistMERGE.y,by = c("Team.y","Date")) %>%
  ungroup()

seasondata$Date <- seasondata$DateOld
seasondata <- dplyr::select(seasondata,-DateOld)



seasondata$OppWinPct.x[seasondata$GameCount.x==1] <- 0.5
seasondata$OppWinPct20.x[seasondata$GameCount.x==1] <- 0.5
seasondata$OppWinPct10.x[seasondata$GameCount.x==1] <- 0.5
seasondata$OppWinPct5.x[seasondata$GameCount.x==1] <- 0.5
seasondata$OppOnOffAdj.x[seasondata$GameCount.x==1] <- 0
seasondata$OppOnOffAdj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OppOnOffAdj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OppOnOffAdj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox1Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox1Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox1Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox1Adj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox2Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox2Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox2Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox2Adj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox3Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox3Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox3Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OppBox3Adj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnOnOffAdj.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnOnOffAdj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnOnOffAdj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnOnOffAdj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox1Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox1Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox1Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox1Adj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox2Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox2Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox2Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox2Adj5.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox3Adj.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox3Adj20.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox3Adj10.x[seasondata$GameCount.x==1] <- 0
seasondata$OwnBox3Adj5.x[seasondata$GameCount.x==1] <- 0

seasondata$OppWinPct.y[seasondata$GameCount.y==1] <- 0.5
seasondata$OppWinPct20.y[seasondata$GameCount.y==1] <- 0.5
seasondata$OppWinPct10.y[seasondata$GameCount.y==1] <- 0.5
seasondata$OppWinPct5.y[seasondata$GameCount.y==1] <- 0.5
seasondata$OppOnOffAdj.y[seasondata$GameCount.y==1] <- 0
seasondata$OppOnOffAdj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OppOnOffAdj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OppOnOffAdj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox1Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox1Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox1Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox1Adj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox2Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox2Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox2Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox2Adj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox3Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox3Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox3Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OppBox3Adj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnOnOffAdj.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnOnOffAdj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnOnOffAdj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnOnOffAdj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox1Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox1Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox1Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox1Adj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox2Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox2Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox2Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox2Adj5.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox3Adj.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox3Adj20.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox3Adj10.y[seasondata$GameCount.y==1] <- 0
seasondata$OwnBox3Adj5.y[seasondata$GameCount.y==1] <- 0


##second add-ins for player quality
playerdataMERGE2.x <- dplyr::select(playerdataMERGE,Team.x,Date,
                             TeamQualRPM.x,
                             TeamQualORPM.x,
                             TeamQualDRPM.x,
                             TeamQualRPMNorm.x,
                             TeamQualORPMNorm.x,
                             TeamQualDRPMNorm.x)

playerdataMERGE2.y <- dplyr::select(playerdataMERGE,Team.y,Date,
                             TeamQualRPM.y,
                             TeamQualORPM.y,
                             TeamQualDRPM.y,
                             TeamQualRPMNorm.y,
                             TeamQualORPMNorm.y,
                             TeamQualDRPMNorm.y)

seasondata$DateOld <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today



seasondata <- seasondata %>% 
  left_join(playerdataMERGE2.x,by = c("Team.x","Date")) %>%
  ungroup()
seasondata <- seasondata %>% 
  left_join(playerdataMERGE2.y,by = c("Team.y","Date")) %>%
  ungroup()

seasondata$Date <- seasondata$DateOld
seasondata <- dplyr::select(seasondata,-DateOld)


###allow coeeficients to change over season
seasondata <- seasondata %>%
  dplyr::mutate(PtsPerPossAvg2.xXGameCount =  PtsPerPossAvg2.x*GameCountAvg,
         PtsPerPossAvg2.yXGameCount =  PtsPerPossAvg2.y*GameCountAvg,
         DefPtsPerPossAvg2.xXGameCount =  DefPtsPerPossAvg2.x*GameCountAvg,
         DefPtsPerPossAvg2.yXGameCount =  DefPtsPerPossAvg2.y*GameCountAvg,
         OppPtsPerPossAvg2.xXGameCount =  OppPtsPerPossAvg2.x*GameCountAvg,
         OppPtsPerPossAvg2.yXGameCount =  OppPtsPerPossAvg2.y*GameCountAvg,
         OppDefPtsPerPossAvg2.xXGameCount =  OppDefPtsPerPossAvg2.x*GameCountAvg,
         OppDefPtsPerPossAvg2.yXGameCount =  OppDefPtsPerPossAvg2.y*GameCountAvg,
         WinPct2.xXGameCount =  WinPct2.x*GameCountAvg,
         WinPct20.xXGameCount =  WinPct20.x*GameCountAvg,
         WinPct10.xXGameCount =  WinPct10.x*GameCountAvg,
         WinPct5.xXGameCount =  WinPct5.x*GameCountAvg,
         AdjNetRating20.xXGameCount =  AdjNetRating20.x*GameCountAvg,
         AdjNetRating10.xXGameCount =  AdjNetRating10.x*GameCountAvg,
         AdjNetRating5.xXGameCount =  AdjNetRating5.x*GameCountAvg,
         WinPct2.yXGameCount =  WinPct2.y*GameCountAvg,
         WinPct20.yXGameCount =  WinPct20.y*GameCountAvg,
         WinPct10.yXGameCount =  WinPct10.y*GameCountAvg,
         WinPct5.yXGameCount =  WinPct5.y*GameCountAvg,
         AdjNetRating20.yXGameCount =  AdjNetRating20.y*GameCountAvg,
         AdjNetRating10.yXGameCount =  AdjNetRating10.y*GameCountAvg,
         AdjNetRating5.yXGameCount =  AdjNetRating5.y*GameCountAvg,
         TeamQualRPM.xXGameCount = TeamQualRPM.x*GameCountAvg,
         TeamQualORPM.xXGameCount = TeamQualORPM.x*GameCountAvg,
         TeamQualDRPM.xXGameCount = TeamQualDRPM.x*GameCountAvg,
         TeamQualRPM.yXGameCount = TeamQualRPM.y*GameCountAvg,
         TeamQualORPM.yXGameCount = TeamQualORPM.y*GameCountAvg,
         TeamQualDRPM.yXGameCount = TeamQualDRPM.y*GameCountAvg,
         PrevSeasonRating.xXGameCount = PrevSeasonRating.x*GameCountAvg,
         PrevSeasonRating.yXGameCount = PrevSeasonRating.y*GameCountAvg)


seasondata <- seasondata %>%
  dplyr::mutate(PPXPace9 = AdjNetRating20.x*EstPace, PPXPace10 = AdjNetRating20.y*EstPace, PPXPace11 = AdjNetRating10.x*EstPace, PPXPace12 = AdjNetRating10.y*EstPace, PPXPace13 = AdjNetRating5.x*EstPace, PPXPace14 =AdjNetRating5.y*EstPace)

seasondata <- seasondata %>%
  dplyr::mutate(PPXPace15 = PtsPerPossAvg20.x*EstPace, PPXPace16 = PtsPerPossAvg10.x*EstPace,PPXPace17 = PtsPerPossAvg5.x*EstPace,
         PPXPace18 = DefPtsPerPossAvg20.x*EstPace, PPXPace19 = DefPtsPerPossAvg10.x*EstPace, PPXPace20 = DefPtsPerPossAvg5.x*EstPace,
         PPXPace21 = OppPtsPerPossAvg20.x*EstPace, PPXPace22 = OppPtsPerPossAvg10.x*EstPace,PPXPace23 = OppPtsPerPossAvg5.x*EstPace,
         PPXPace24 = OppDefPtsPerPossAvg20.x*EstPace, PPXPace25 = OppDefPtsPerPossAvg10.x*EstPace, PPXPace26 = OppDefPtsPerPossAvg5.x*EstPace,
         PPXPace27 = PtsPerPossAvg20.y*EstPace, PPXPace28 = PtsPerPossAvg10.y*EstPace,PPXPace29 = PtsPerPossAvg5.y*EstPace,
         PPXPace30 = DefPtsPerPossAvg20.y*EstPace, PPXPace31 = DefPtsPerPossAvg10.y*EstPace, PPXPace32 = DefPtsPerPossAvg5.y*EstPace,
         PPXPace33 = OppPtsPerPossAvg20.y*EstPace, PPXPace34 = OppPtsPerPossAvg10.y*EstPace,PPXPace35 = OppPtsPerPossAvg5.y*EstPace,
         PPXPace36 = OppDefPtsPerPossAvg20.y*EstPace, PPXPace37 = OppDefPtsPerPossAvg10.y*EstPace, PPXPace38 = OppDefPtsPerPossAvg5.y*EstPace)

seasondata <- seasondata %>%
  dplyr::mutate(PPXLess2015 = PtsPerPossAvg20.x*GameDummyLess20, PPXLess2016 = PtsPerPossAvg10.x*GameDummyLess20,PPXLess2017 = PtsPerPossAvg5.x*GameDummyLess20,
         PPXLess2018 = DefPtsPerPossAvg20.x*GameDummyLess20, PPXLess2019 = DefPtsPerPossAvg10.x*GameDummyLess20, PPXLess2020 = DefPtsPerPossAvg5.x*GameDummyLess20,
         PPXLess2021 = OppPtsPerPossAvg20.x*GameDummyLess20, PPXLess2022 = OppPtsPerPossAvg10.x*GameDummyLess20,PPXLess2023 = OppPtsPerPossAvg5.x*GameDummyLess20,
         PPXLess2024 = OppDefPtsPerPossAvg20.x*GameDummyLess20, PPXLess2025 = OppDefPtsPerPossAvg10.x*GameDummyLess20, PPXLess2026 = OppDefPtsPerPossAvg5.x*GameDummyLess20,
         PPXLess2027 = PtsPerPossAvg20.y*GameDummyLess20, PPXLess2028 = PtsPerPossAvg10.y*GameDummyLess20,PPXLess2029 = PtsPerPossAvg5.y*GameDummyLess20,
         PPXLess2030 = DefPtsPerPossAvg20.y*GameDummyLess20, PPXLess2031 = DefPtsPerPossAvg10.y*GameDummyLess20, PPXLess2032 = DefPtsPerPossAvg5.y*GameDummyLess20,
         PPXLess2033 = OppPtsPerPossAvg20.y*GameDummyLess20, PPXLess2034 = OppPtsPerPossAvg10.y*GameDummyLess20,PPXLess2035 = OppPtsPerPossAvg5.y*GameDummyLess20,
         PPXLess2036 = OppDefPtsPerPossAvg20.y*GameDummyLess20, PPXLess2037 = OppDefPtsPerPossAvg10.y*GameDummyLess20, PPXLess2038 = OppDefPtsPerPossAvg5.y*GameDummyLess20)



#playoff series
seasondata$Ones <- 1
seasondata$TeamA <- ifelse(seasondata$Team.x<seasondata$Team.y,seasondata$Team.x,seasondata$Team.y)
seasondata$TeamB <- ifelse(seasondata$Team.x>seasondata$Team.y,seasondata$Team.x,seasondata$Team.y)
seasondata$GameDes2 <- paste(seasondata$TeamA, " and ",seasondata$TeamB,sep="")
seasondata$GameDes2[seasondata$Playoffs==0] <- "RegSeason"
seasondata <- seasondata %>% 
  dplyr::group_by(GameDes2, Season) %>%
  dplyr::mutate(SeriesGame = cumsum(Ones)) %>%
  ungroup()
seasondata$SeriesGame[seasondata$RegularSeason==1] <- 0
seasondata$GameOne <- ifelse(seasondata$SeriesGame==1,1,0)
seasondata$GameTwo <- ifelse(seasondata$SeriesGame==2,1,0)
seasondata$GameThree <- ifelse(seasondata$SeriesGame==3,1,0)
seasondata$GameFour <- ifelse(seasondata$SeriesGame==4,1,0)
seasondata$GameFive <- ifelse(seasondata$SeriesGame==5,1,0)
seasondata$GameSix <- ifelse(seasondata$SeriesGame==6,1,0)
seasondata$GameSeven <- ifelse(seasondata$SeriesGame==7,1,0)
seasondata$TeamAHome[seasondata$TeamA==seasondata$Team.x] <- 1
seasondata$TeamAHome[seasondata$TeamA==seasondata$Team.y] <- 0
seasondata$TeamASeriesWin <- ifelse((seasondata$TeamAHome==1&seasondata$HomeWin==1)|(seasondata$TeamAHome==0&seasondata$HomeWin==0),1,0)
seasondata$TeamASeriesWin[seasondata$Date>=today] <- 0
seasondata <- seasondata %>% 
  dplyr::group_by(GameDes2, Season) %>%
  dplyr::mutate(TeamASeriesWinTot = cumsum(TeamASeriesWin)) %>%
  ungroup()
seasondata$TeamBSeriesWinTot <- seasondata$SeriesGame-seasondata$TeamASeriesWinTot
seasondata$TeamBSeriesWinTot <- ifelse(seasondata$TeamASeriesWin==1,seasondata$TeamBSeriesWinTot,seasondata$TeamBSeriesWinTot-1)
seasondata$TeamASeriesWinTot <- ifelse(seasondata$TeamASeriesWin==1,seasondata$TeamASeriesWinTot-1,seasondata$TeamASeriesWinTot)
seasondata$TeamASeriesWinTot[seasondata$TeamASeriesWinTot==-1] <- 0
seasondata$TeamBSeriesWinTot[seasondata$TeamBSeriesWinTot==-1] <- 0
seasondata$HomeSeriesStatus <- ifelse(seasondata$TeamAHome==1,paste0(seasondata$TeamASeriesWinTot,"-",seasondata$TeamBSeriesWinTot,sep=""),paste0(seasondata$TeamBSeriesWinTot,"-",seasondata$TeamASeriesWinTot,sep=""))
seasondata$OneZero <- ifelse(seasondata$HomeSeriesStatus=="1-0"&seasondata$Playoffs==1,1,0)
seasondata$ZeroOne <- ifelse(seasondata$HomeSeriesStatus=="0-1"&seasondata$Playoffs==1,1,0)
seasondata$OneOne <- ifelse(seasondata$HomeSeriesStatus=="1-1"&seasondata$Playoffs==1,1,0)
seasondata$TwoZero <- ifelse(seasondata$HomeSeriesStatus=="2-0"&seasondata$Playoffs==1,1,0)
seasondata$ZeroTwo <- ifelse(seasondata$HomeSeriesStatus=="0-2"&seasondata$Playoffs==1,1,0)
seasondata$ThreeZero <- ifelse(seasondata$HomeSeriesStatus=="3-0"&seasondata$Playoffs==1,1,0)
seasondata$ZeroThree <- ifelse(seasondata$HomeSeriesStatus=="0-3"&seasondata$Playoffs==1,1,0)
seasondata$TwoOne <- ifelse(seasondata$HomeSeriesStatus=="2-1"&seasondata$Playoffs==1,1,0)
seasondata$OneTwo <- ifelse(seasondata$HomeSeriesStatus=="1-2"&seasondata$Playoffs==1,1,0)
seasondata$ThreeOne <- ifelse(seasondata$HomeSeriesStatus=="3-1"&seasondata$Playoffs==1,1,0)
seasondata$TwoTwo <- ifelse(seasondata$HomeSeriesStatus=="2-2"&seasondata$Playoffs==1,1,0)
seasondata$OneThree <- ifelse(seasondata$HomeSeriesStatus=="1-3"&seasondata$Playoffs==1,1,0)
seasondata$ThreeTwo <- ifelse(seasondata$HomeSeriesStatus=="3-2"&seasondata$Playoffs==1,1,0)
seasondata$TwoThree <- ifelse(seasondata$HomeSeriesStatus=="2-3"&seasondata$Playoffs==1,1,0)
seasondata$TwoZeroComb <- ifelse(seasondata$HomeSeriesStatus=="2-0"&seasondata$Playoffs==1,-1,0)
seasondata$TwoZeroComb[seasondata$HomeSeriesStatus=="0-2"] <- 1
seasondata$ThreeZeroComb <- ifelse(seasondata$HomeSeriesStatus=="3-0"&seasondata$Playoffs==1,-1,0)
seasondata$ThreeZeroComb[seasondata$HomeSeriesStatus=="0-3"] <- 1
seasondata$SweepProspect <- ifelse(seasondata$ThreeZero==1|seasondata$ZeroThree==1,1,0)

##Tankathon
teamlistMERGE2 <- dplyr::select(teamlistMERGEAgain,Team,Season,Date,DateMax,Team.x,Team.y,WinPct)
seasondataMERGE <- seasondata[,c("Team.x","Team.y","Date","Wins.x","Losses.x","Wins.y","Losses.y")]
seasondataMERGE <- dplyr::rename(seasondataMERGE,DateMax=Date)
teamlistMERGE2 <- teamlistMERGE2 %>% left_join(seasondataMERGE,by = c("Team.x","DateMax")) %>% ungroup()
teamlistMERGE2 <- dplyr::rename(teamlistMERGE2,Team.y = Team.y.x)
teamlistMERGE2 <- dplyr::select(teamlistMERGE2,Team,Season,Date,DateMax,Team.x,Team.y,WinPct,Wins.x,Losses.x)
teamlistMERGE2 <- teamlistMERGE2 %>% left_join(seasondataMERGE,by = c("Team.y","DateMax")) %>% ungroup()
teamlistMERGE2 <- dplyr::rename(teamlistMERGE2,Team.x = Team.x.x,Wins.x=Wins.x.x,Losses.x=Losses.x.x)
teamlistMERGE2 <- dplyr::select(teamlistMERGE2,Team,Season,Date,DateMax,Team.x,Team.y,WinPct,Wins.x,Losses.x,Wins.y,Losses.y)
teamlistMERGE2$Wins <- ifelse(is.na(teamlistMERGE2$Wins.x),teamlistMERGE2$Wins.y,teamlistMERGE2$Wins.x)
teamlistMERGE2$Losses <- ifelse(is.na(teamlistMERGE2$Wins.x),teamlistMERGE2$Losses.y,teamlistMERGE2$Losses.x)
teamlistMERGE2$WinPct[teamlistMERGE2$Season=="2003-04"] <- teamlistMERGE2$Wins[teamlistMERGE2$Season=="2003-04"]/(teamlistMERGE2$Wins[teamlistMERGE2$Season=="2003-04"]+teamlistMERGE2$Losses[teamlistMERGE2$Season=="2003-04"])
teamlistMERGE2 <- dplyr::select(teamlistMERGE2,Team,Season,Date,DateMax,Team.x,Team.y,WinPct,Wins,Losses)
teamlistMERGE2$East <- ifelse(teamlistMERGE2$Team %in% c("Miami Heat","Cleveland Cavaliers","Orlando Magic","Chicago Bulls","Philadelphia 76ers","Boston Celtics","Milwaukee Bucks","Toronto Raptors","Brooklyn Nets","Charlotte Hornets","Atlanta Hawks","Washington Wizards","New York Knicks","Indiana Pacers","Detroit Pistons"),1,0 )
teamlistMERGE2 <- teamlistMERGE2 %>% 
  dplyr::group_by(Date,East) %>%
  dplyr::mutate(PlaceRanks = order(order(WinPct, decreasing=TRUE))) %>%
  ungroup()
teamlistMERGE2 <- teamlistMERGE2 %>% 
  dplyr::group_by(Date,East) %>%
  dplyr::mutate(CutoffWins8 = Wins[PlaceRanks==8],CutoffWins10 = Wins[PlaceRanks==10],
         CutoffLosses8 = Losses[PlaceRanks==8],CutoffLosses10 = Losses[PlaceRanks==10],
         CutoffWinPct8 = WinPct[PlaceRanks==8],CutoffWinPct10 = WinPct[PlaceRanks==10],) %>%
  ungroup()
teamlistMERGE2$GameCount <- teamlistMERGE2$Wins+teamlistMERGE2$Losses
teamlistMERGE2$GamesLeft <- 82 - teamlistMERGE2$Wins - teamlistMERGE2$Losses
teamlistMERGE2$GamesLeft[teamlistMERGE2$Season=="2020-21"] <- 72 - teamlistMERGE2$Wins[teamlistMERGE2$Season=="2020-21"] - teamlistMERGE2$Losses[teamlistMERGE2$Season=="2020-21"]
teamlistMERGE2$GamesBack8 <- ((teamlistMERGE2$Wins - teamlistMERGE2$CutoffWins8) + (teamlistMERGE2$CutoffLosses8 - teamlistMERGE2$Losses))/2
teamlistMERGE2$GamesBack10 <- ((teamlistMERGE2$Wins - teamlistMERGE2$CutoffWins10) + (teamlistMERGE2$CutoffLosses10 - teamlistMERGE2$Losses))/2
teamlistMERGE2$AmountBehind <- teamlistMERGE2$GamesBack8/teamlistMERGE2$GamesLeft
teamlistMERGE2$AmountBehind[teamlistMERGE2$Season=="2021-22"] <- teamlistMERGE2$GamesBack10[teamlistMERGE2$Season=="2021-22"]/teamlistMERGE2$GamesLeft[teamlistMERGE2$Season=="2021-22"]
teamlistMERGE2$AmountBehind[teamlistMERGE2$Season=="2020-21"] <- teamlistMERGE2$GamesBack10[teamlistMERGE2$Season=="2020-21"]/teamlistMERGE2$GamesLeft[teamlistMERGE2$Season=="2020-21"]
teamlistMERGE2$Tankathon <- ifelse(teamlistMERGE2$AmountBehind<=-.3&teamlistMERGE2$GamesLeft<=25,1,0)
teamlistMERGE2$Tankathon[teamlistMERGE2$Tankathon==1&teamlistMERGE2$GamesLeft>5&teamlistMERGE2$GamesBack8>=-5&!(teamlistMERGE2$Season=="2020-21"|teamlistMERGE2$Season=="2021-22")] <- 0
teamlistMERGE2$Tankathon[teamlistMERGE2$Tankathon==1&teamlistMERGE2$GamesLeft>5&teamlistMERGE2$GamesBack10>=-5&(teamlistMERGE2$Season=="2020-21"|teamlistMERGE2$Season=="2021-22")] <- 0
teamlistMERGE2$Tankathon[teamlistMERGE2$Tankathon==1&teamlistMERGE2$GamesLeft<=5&teamlistMERGE2$AmountBehind>-1] <- 0
teamlistMERGE2 <- dplyr::select(teamlistMERGE2,Team,Date,Tankathon)
teamlistMERGE2 <- dplyr::rename(teamlistMERGE2,Tankathon.x=Tankathon,Team.x=Team)
seasondata <- seasondata %>% left_join(teamlistMERGE2,by = c("Team.x","Date")) %>% ungroup()
teamlistMERGE2 <- dplyr::rename(teamlistMERGE2,Tankathon.y=Tankathon.x,Team.y=Team.x)
seasondata <- seasondata %>% left_join(teamlistMERGE2,by = c("Team.y","Date")) %>% ungroup()

seasondata$Tankathon.x[seasondata$Playoffs==1] <- 0
seasondata$Tankathon.y[seasondata$Playoffs==1] <- 0
seasondata$Tankathon.x[is.na(seasondata$Tankathon.x)] <- 0
seasondata$Tankathon.y[is.na(seasondata$Tankathon.y)] <- 0

#ManualTank Adhustments
seasondata$Tankathon.x[seasondata$Team.x=="Portland Trail Blazers"&seasondata$Season=="2021-22"&seasondata$Date>="2022-02-28"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Portland Trail Blazers"&seasondata$Season=="2021-22"&seasondata$Date>="2022-02-28"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Chicago Bulls"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Chicago Bulls"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="LA Clippers"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="LA Clippers"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Milwaukee Bucks"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Milwaukee Bucks"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Minnesota Timberwolves"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Minnesota Timberwolves"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="New Orleans Pelicans"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="New Orleans Pelicans"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Toronto Raptors"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Toronto Raptors"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Utah Jazz"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Utah Jazz"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Denver Nuggets"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Denver Nuggets"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="San Antonio Spurs"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="San Antonio Spurs"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Miami Heat"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Miami Heat"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Phoenix Suns"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Phoenix Suns"&seasondata$Season=="2021-22"&seasondata$Date=="2022-04-10"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Charlotte Hornets"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Charlotte Hornets"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="San Antonio Spurs"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="San Antonio Spurs"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Houston Rockets"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Houston Rockets"&seasondata$Season=="2022-23"&seasondata$Date>="2023-02-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Orlando Magic"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Orlando Magic"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Portland Trail Blazers"&seasondata$Season=="2022-23"&seasondata$Date>="2023-03-29"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Portland Trail Blazers"&seasondata$Season=="2022-23"&seasondata$Date>="2023-03-29"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Washington Wizards"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Washington Wizards"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Indiana Pacers"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Indiana Pacers"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Utah Jazz"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Utah Hazz"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-01"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Philadelphia 76ers"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Philadelphia 76ers"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Boston Celtics"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Boston Celtics"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 0
seasondata$Tankathon.y[seasondata$Team.y=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 0
seasondata$Tankathon.x[seasondata$Team.x=="Sacramento Kings"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Sacramento Kings"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-07"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Houston Rockets"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 0
seasondata$Tankathon.y[seasondata$Team.y=="Houston Rockets"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 0
seasondata$Tankathon.x[seasondata$Team.x=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 0
seasondata$Tankathon.y[seasondata$Team.y=="Detroit Pistons"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 0
seasondata$Tankathon.x[seasondata$Team.x=="Chicago Bulls"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Chicago Bulls"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Dallas Mavericks"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Dallas Mavericks"&seasondata$Season=="2022-23"&seasondata$Date>="2023-04-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Oklahoma City Thunder"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Oklahoma City Thunder"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1
seasondata$Tankathon.x[seasondata$Team.x=="Phoenix Suns"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1
seasondata$Tankathon.y[seasondata$Team.y=="Phoenix Suns"&seasondata$Season=="2022-23"&seasondata$Date=="2023-04-09"] <- 1


#third add-ons for playoff experience
playerdataMERGE3.x <- dplyr::select(playerdataMERGE,Team.x,Date,
                             PlayoffExpMin.x,
                             PlayoffExpGame.x,
                             PlayoffExpMinPG.x)

playerdataMERGE3.y <- dplyr::select(playerdataMERGE,Team.y,Date,
                             PlayoffExpMin.y,
                             PlayoffExpGame.y,
                             PlayoffExpMinPG.y)

playerdataMERGE3.x$PlayoffExpMin.x[is.infinite(playerdataMERGE3.x$PlayoffExpMin.x)] <- 0
playerdataMERGE3.x$PlayoffExpGame.x[is.infinite(playerdataMERGE3.x$PlayoffExpGame.x)] <- 0
playerdataMERGE3.x$PlayoffExpMinPG.x[is.infinite(playerdataMERGE3.x$PlayoffExpMinPG.x)] <- 0
playerdataMERGE3.y$PlayoffExpMin.y[is.infinite(playerdataMERGE3.y$PlayoffExpMin.y)] <- 0
playerdataMERGE3.y$PlayoffExpGame.y[is.infinite(playerdataMERGE3.y$PlayoffExpGame.y)] <- 0
playerdataMERGE3.y$PlayoffExpMinPG.y[is.infinite(playerdataMERGE3.y$PlayoffExpMinPG.y)] <- 0

seasondata$DateOld <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

seasondata <- seasondata %>% 
  left_join(playerdataMERGE3.x,by = c("Team.x","Date")) %>%
  ungroup()
seasondata <- seasondata %>% 
  left_join(playerdataMERGE3.y,by = c("Team.y","Date")) %>%
  ungroup()

seasondata$Date <- seasondata$DateOld
seasondata <- dplyr::select(seasondata,-DateOld)

seasondata$GameDummy2040 <- ifelse(seasondata$GameCountAvg>20&seasondata$GameCountAvg<40,1,0)
seasondata$GameDummy4060 <- ifelse(seasondata$GameCountAvg>=40&seasondata$GameCountAvg<60,1,0)
seasondata$GameDummy6082 <- ifelse(seasondata$GameCountAvg>=60&seasondata$GameCountAvg<=82,1,0)
seasondata$row_num <- seq.int(nrow(seasondata)) 
seasondata$ones <- 1

seasondata <- seasondata %>% 
  dplyr::group_by(Season) %>% 
  dplyr::mutate(SeasTotalPoints20 = mean(TotalPoints[RegularSeason==1&GameCountAvg<20],na.rm=TRUE)) %>% 
  ungroup()

seasondata$DateOld <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today

myfunc <- function(x){
  with(x, sapply(row_num, function(y) 
    (mean(TotalPoints[Season==Season[y] & Date < Date[y]& Date > Date[y]-30],na.rm = TRUE))))
}
myfunc2 <- function(x){
  with(x, sapply(row_num, function(y) 
    (mean(TotalPoints[Season==Season[y] & Date < Date[y]& Date > Date[y]-60],na.rm = TRUE))))
}
myfunc3 <- function(x){
  with(x, sapply(row_num, function(y) 
    (mean(SeasTotalPoints20[SeasNum==(SeasNum[y]-1)],na.rm = TRUE))))
}
myfunc4 <- function(x){
  with(x, sapply(row_num, function(y) 
    (mean(TotalPoints[Season==Season[y] & Date < Date[y]],na.rm = TRUE))))
}
seasondata <- seasondata %>%
  do(data.frame(., SeasTotPtAvg30 = myfunc(.)))
seasondata <- seasondata %>%
  do(data.frame(., SeasTotPtAvg60 = myfunc2(.)))
seasondata <- seasondata %>%
  do(data.frame(., SeasTot20LAST = myfunc3(.)))
seasondata <- seasondata %>%
  do(data.frame(., SeasTotPtAvgALL = myfunc4(.)))
seasondata$BallChgDummy <- ifelse(seasondata$Season=="2021-22"&seasondata$GameCountAvg<=20,1,0)
seasondata$BallChgDummy2 <- ifelse(seasondata$Season=="2021-22"&seasondata$GameCountAvg>20&seasondata$GameCountAvg<=40,1,0)
seasondata$Season2021Dummy <- ifelse(seasondata$Season=="2021-22",1,0)
seasondata$Season2020Dummy <- ifelse(seasondata$Season=="2020-21",1,0)
seasondata$Season2019Dummy <- ifelse(seasondata$Season=="2019-20",1,0)
seasondata$Season2018Dummy <- ifelse(seasondata$Season=="2018-19",1,0)
seasondata$Season2017Dummy <- ifelse(seasondata$Season=="2017-18",1,0)
seasondata$Season2016Dummy <- ifelse(seasondata$Season=="2016-17",1,0)
seasondata$Season2015Dummy <- ifelse(seasondata$Season=="2015-16",1,0)
seasondata$Season2014Dummy <- ifelse(seasondata$Season=="2014-15",1,0)
seasondata$Season2013Dummy <- ifelse(seasondata$Season=="2013-14",1,0)
seasondata$Season2012Dummy <- ifelse(seasondata$Season=="2012-13",1,0)
seasondata$Season2011Dummy <- ifelse(seasondata$Season=="2011-12",1,0)
seasondata$Season2010Dummy <- ifelse(seasondata$Season=="2010-11",1,0)
seasondata$Season2009Dummy <- ifelse(seasondata$Season=="2009-10",1,0)
seasondata$Season2008Dummy <- ifelse(seasondata$Season=="2008-09",1,0)
seasondata$Season2007Dummy <- ifelse(seasondata$Season=="2007-08",1,0)
seasondata$Season2006Dummy <- ifelse(seasondata$Season=="2006-07",1,0)
seasondata$Season2005Dummy <- ifelse(seasondata$Season=="2005-06",1,0)
seasondata$Season2004Dummy <- ifelse(seasondata$Season=="2004-05",1,0)
seasondata$Season2003Dummy <- ifelse(seasondata$Season=="2003-04",1,0)

seasondata$SeasTotPtAvg30[seasondata$GameCountAvg<2] <- seasondata$SeasTot20LAST[seasondata$GameCountAvg<2]
seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3] <- .2*seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]+.8*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]
seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4] <- .4*seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]+.6*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]
seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5] <- .6*seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]+.4*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]
seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6] <- .8*seasondata$SeasTotPtAvg30[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]+.2*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]

seasondata$SeasTotPtAvg60[seasondata$GameCountAvg<2] <- seasondata$SeasTot20LAST[seasondata$GameCountAvg<2]
seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3] <- .2*seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]+.8*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]
seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4] <- .4*seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]+.6*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]
seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5] <- .6*seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]+.4*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]
seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6] <- .8*seasondata$SeasTotPtAvg60[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]+.2*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]
seasondata$GameCountAvgSq <- seasondata$GameCountAvg*seasondata$GameCountAvg

seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg<2] <- seasondata$SeasTot20LAST[seasondata$GameCountAvg<2]
seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3] <- .2*seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]+.8*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<3]
seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4] <- .4*seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]+.6*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=3&seasondata$GameCountAvg<4]
seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5] <- .6*seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]+.4*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=4&seasondata$GameCountAvg<5]
seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6] <- .8*seasondata$SeasTotPtAvgALL[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]+.2*seasondata$SeasTot20LAST[seasondata$GameCountAvg>=5&seasondata$GameCountAvg<6]

seasondata$SeasTotOverAvg <- seasondata$SeasTotPtAvg30-seasondata$SeasTotPtAvgALL


seasondata$Date <- seasondata$DateOld
seasondata <- dplyr::select(seasondata,-DateOld)
  
seasondataRF <- seasondata
seasondataRF$HomeWin <- as.factor(seasondataRF$HomeWin)
seasondataRF$Team.x <- as.factor(seasondataRF$Team.x)
seasondataRF$Team.y <- as.factor(seasondataRF$Team.y)

save(seasondata, file = paste("allregdata_modelbetaV1.RData", sep=""))
save(seasondataRF, file = paste("allregdata_modelgammaV1.RData", sep=""))
preddata_final <- seasondata[seasondata$Date>=startdate,]
preddata_final <- preddata_final[preddata_final$Date>=today,]
preddata_final <- preddata_final[preddata_final$Team.x!="TBD "&preddata_final$Team.y!="TBD ",]
preddata_final <- preddata_final[preddata_final$Team.x!="TBD"&preddata_final$Team.y!="TBD",]
preddata_finalRF <- seasondataRF[seasondataRF$Date>=startdate,]
preddata_finalRF <- preddata_finalRF[preddata_finalRF$Date>=today,]
preddata_finalRF <- preddata_finalRF[preddata_finalRF$Team.x!="TBD "&preddata_finalRF$Team.y!="TBD ",]
preddata_finalRF <- preddata_finalRF[preddata_finalRF$Team.x!="TBD"&preddata_finalRF$Team.y!="TBD",]
seasondata <- seasondata[seasondata$Date<(startdate),]
seasondataREGRF <- seasondataRF[seasondataRF$Date<(startdate),]
save(preddata_final, file = paste("testdata_modelbetaV1.RData", sep=""))
save(preddata_finalRF, file = paste("testdata_modelgammaV1.RData", sep=""))
save(seasondataREGRF, file = paste("regdata_modelgammaV1.RData", sep=""))





modelbeta1a <- lm(formula = HomeMargin ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
                  + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
                  + RegularSeason + Playoffs + 
                    BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
                  + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
                  + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
                    + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct2.y 
                   + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y 
                  + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPctX20.y 
                  + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x 
                  + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                  + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                  + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
                    + Box1Adjustment.x + Box1AdjChg.x 
                  + OnOffAdjustment.y
                  + Box1Adjustment.y + Box1AdjChg.y 
                  + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
                  + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
                  + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                  + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
                  + OppBox1Adj.x + OppBox1Adj20.x 
                  + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x 
                  + OppWinPct.y + OppBox1Adj.y + OppBox1Adj20.y 
                  + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
                    TeamQualORPM.x + TeamQualDRPM.x + 
                    TeamQualORPM.y + TeamQualDRPM.y +
                  GameOne +
                    #GameTwo + 
                    OneZero + ZeroOne + OneOne + TwoZeroComb  + OneTwo + TwoOne + ThreeZeroComb + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
                    #GameThree + GameFour + GameFive + GameSix 
                  + GameSeven + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y 
                  + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x 
                  + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + PtsPerPossAvg2.x*GameDummyLess20 +
                    PtsPerPossAvg2.y*GameDummyLess20 + DefPtsPerPossAvg2.x*GameDummyLess20 + DefPtsPerPossAvg2.y*GameDummyLess20 + 
                    OppPtsPerPossAvg2.x*GameDummyLess20 + OppPtsPerPossAvg2.y*GameDummyLess20 + OppDefPtsPerPossAvg2.x*GameDummyLess20 
                  + OppDefPtsPerPossAvg2.y*GameDummyLess20, data = seasondata)
summary(modelbeta1a)$coef
modelbeta1b <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
                  + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
                  + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
                  + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
                  + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
                    + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y 
                  + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y 
                  + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y 
                  + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x 
                  + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                  + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                  + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
                  + Box1Adjustment.x + Box3Adjustment.x + Box1AdjChg.x 
                  + OnOffAdjustment.y
                  + Box1Adjustment.y + Box3Adjustment.y + Box1AdjChg.y 
                  + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
                  + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
                    +  PPXPace9 + PPXPace10 + PPXPace11 + PPXPace12 + PPXPace13 + PPXPace14 + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                  + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x + OppWinPct20.x 
                  + OppWinPct10.x + OppWinPct5.x + OppBox1Adj.x + OppBox1Adj20.x 
                  + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x 
                  + OppWinPct.y + OppWinPct20.y + OppWinPct10.y + OppWinPct5.y + OppBox1Adj.y + OppBox1Adj20.y 
                  + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
                    TeamQualORPM.x + TeamQualDRPM.x + 
                    TeamQualORPM.y + TeamQualDRPM.y +
                    TeamQualORPM.x*RegularSeason + TeamQualDRPM.x*RegularSeason +
                    TeamQualORPM.y*RegularSeason + TeamQualDRPM.y*RegularSeason +
                    PtsPerPossAvg2.x*RegularSeason + PtsPerPossAvg2.y*RegularSeason + DefPtsPerPossAvg2.x*RegularSeason + DefPtsPerPossAvg2.y*RegularSeason
                  + PPXPace1*RegularSeason + PPXPace2*RegularSeason + PPXPace3*RegularSeason + PPXPace4*RegularSeason + WinPct20.x*RegularSeason +
                    WinPct10.x*RegularSeason + WinPct20.y*RegularSeason + WinPct10.y*RegularSeason +
                    GameOne +
                    #GameTwo + 
                    OneZero + ZeroOne + OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
                  #GameThree + GameFour + GameFive + GameSix 
                  + GameSeven + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y + PlayoffExpMinPG.x + PlayoffExpMinPG.y
                  + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + PlayoffExpMinPG.x*RegularSeason + PlayoffExpMinPG.y*RegularSeason, data = seasondata)
summary(modelbeta1b)$coef
modelbeta1c <- lm(formula = TotalPoints ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
                  + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
                  + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
                  + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
                  + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 + GameDummy2040 + GameDummy4060 + GameDummy6082
                  + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + PPXPace15 + PPXPace16 + PPXPace17 +
                    PPXPace18 + PPXPace19 + PPXPace20 +
                    PPXPace21 + PPXPace22 + PPXPace23 +
                    PPXPace24 + PPXPace25 + PPXPace26 +
                    PPXPace27 + PPXPace28 + PPXPace29 +
                    PPXPace30 + PPXPace31 + PPXPace32 +
                    PPXPace33 + PPXPace34 + PPXPace35 +
                    PPXPace36 + PPXPace37 + PPXPace38 
                  + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                  + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
                  + Box1Adjustment.x + Box1AdjChg.x 
                  + OnOffAdjustment.y
                  + Box1Adjustment.y + Box1AdjChg.y 
                  + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
                  + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
                    HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                  + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y +
                    TeamQualORPM.x + TeamQualDRPM.x + 
                    TeamQualORPM.y + TeamQualDRPM.y +
                    PtsPerPossAvg2.x*RegularSeason*EstPace + PtsPerPossAvg2.y*RegularSeason*EstPace + DefPtsPerPossAvg2.x*RegularSeason*EstPace + DefPtsPerPossAvg2.y*RegularSeason*EstPace
                  + GameOne +
                    GameTwo +
                    #  OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
                    GameThree + GameFour + GameFive + GameSix
                  + GameSeven + SweepProspect + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
                  + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x 
                  + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + PtsPerPossAvg2.x*GameDummyLess20*EstPace +
                    PtsPerPossAvg2.y*GameDummyLess20*EstPace + DefPtsPerPossAvg2.x*GameDummyLess20*EstPace + DefPtsPerPossAvg2.y*GameDummyLess20*EstPace + 
                    OppPtsPerPossAvg2.x*GameDummyLess20*EstPace + OppPtsPerPossAvg2.y*GameDummyLess20*EstPace + OppDefPtsPerPossAvg2.x*GameDummyLess20*EstPace 
                  + OppDefPtsPerPossAvg2.y*GameDummyLess20*EstPace 
                  + GameCountAvg + GameCountAvgSq+GameCountAvg*RegularSeason+ GameCountAvgSq*RegularSeason
                  +SeasTotPtAvg30+SeasTotPtAvg30*RegularSeason
                  +SeasTotPtAvg60+SeasTotPtAvg60*RegularSeason+SeasTotOverAvg+SeasTotOverAvg*RegularSeason, data = seasondata)
summary(modelbeta1c)$coef
modelbeta1d <- glm(formula = HomeWin ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
                   + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
                   + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
                   + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
                     + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct2.y 
                   + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y 
                   + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPctX20.y 
                   + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x 
                   + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
                   + Box1Adjustment.x + Box1AdjChg.x 
                   + OnOffAdjustment.y
                   + Box1Adjustment.y + Box1AdjChg.y 
                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
                   + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
                     HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
                   + OppWinPct.y +
                     TeamQualORPM.x + TeamQualDRPM.x + 
                     TeamQualORPM.y + TeamQualDRPM.y +
                     GameOne +
                     #GameTwo + 
                     OneZero + ZeroOne + OneOne + TwoZeroComb  + OneTwo + TwoOne + ThreeZeroComb + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
                   #GameThree + GameFour + GameFive + GameSix 
                   + GameSeven + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x 
                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + PtsPerPossAvg2.x*GameDummyLess20 +
                     PtsPerPossAvg2.y*GameDummyLess20 + DefPtsPerPossAvg2.x*GameDummyLess20 + DefPtsPerPossAvg2.y*GameDummyLess20 + 
                     OppPtsPerPossAvg2.x*GameDummyLess20 + OppPtsPerPossAvg2.y*GameDummyLess20 + OppDefPtsPerPossAvg2.x*GameDummyLess20 
                   + OppDefPtsPerPossAvg2.y*GameDummyLess20, data = seasondata, family = binomial(link = "logit"))
summary(modelbeta1d)$coef
# seasondata$TP_resid <- seasondata$TotalPoints-predict(modelbeta1c,newdata=seasondata)
# seasondata$HM_resid <- seasondata$HomeMargin-predict(modelbeta1a,newdata=seasondata)
# ggplot(seasondata,aes(x=GameCountAvg,y=TP_resid)) + geom_point() + stat_smooth(aes(y=TP_resid), method = "lm", formula = y ~ x + I(x^2))

#Now, create data with previewed games and predicted lines for model alphs, version 2
pred_margin <- predict(modelbeta1a,newdata=preddata_final)
pred_poss <- predict(modelbeta1b,newdata=preddata_final)
pred_totalpoints <- predict(modelbeta1c,newdata=preddata_final)
if(length(preddata_final$Date)!=0) {
  pred_homewin <- predict(modelbeta1d,newdata=preddata_final,type = "response")
} else {
  pred_homewin <- pred_margin
}
date <- preddata_final$Date
description <- preddata_final$GameDescription
result_margin <- NA
result_poss <- NA
pred_line <- -1*pred_margin
result_line <- NA
pred_overunder <- pred_totalpoints
if(length(preddata_final$Date)!=0) {
  version <- "BetaV3"
  predictionsBetaV3 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)
  summary(modelbeta1a)
  save(predictionsBetaV3, file = paste("predictionsBetaV3.RData", sep=""))
  
} else {
  message("Error: No Games in Next 3 days -- dont run")
}




modelalpha4a <- lm(formula = HomeMargin ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) + GameDummy5 + GameDummy10 + GameDummy15 + GameDummy20 + GameDummy25 + GameDummy30 + GameDummy35 + GameDummy40 + GameDummy45 + GameDummy50 + GameDummy55 + GameDummy60 + GameDummy65 + GameDummy70 + GameDummy82 + GameDummy5XPrevSeas + GameDummy10XPrevSeas + GameDummy15XPrevSeas + GameDummy20XPrevSeas + GameDummy25XPrevSeas + GameDummy30XPrevSeas + GameDummy35XPrevSeas + GameDummy40XPrevSeas + GameDummy45XPrevSeas + GameDummy50XPrevSeas + GameDummy55XPrevSeas + GameDummy60XPrevSeas + GameDummy65XPrevSeas + GameDummy70XPrevSeas + GameDummy82XPrevSeas + GameDummy5XPrevSeasY + GameDummy10XPrevSeasY + GameDummy15XPrevSeasY + GameDummy20XPrevSeasY + GameDummy25XPrevSeasY + GameDummy30XPrevSeasY + GameDummy35XPrevSeasY + GameDummy40XPrevSeasY + GameDummy45XPrevSeasY + GameDummy50XPrevSeasY + GameDummy55XPrevSeasY + GameDummy60XPrevSeasY + GameDummy65XPrevSeasY + GameDummy70XPrevSeasY + GameDummy82XPrevSeasY  + PaceXSq2 + PaceYSq2 + PaceXPace2 +
                     AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + YearSq + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                   + ThrPercent.x + ThrPAPerPoss.x + ThrPMPerPoss.x + DefThrPercent.y + DefThrPAPerPoss.y  + DefThrPMPerPoss.y
                   + ThrPercent.y + ThrPAPerPoss.y + ThrPMPerPoss.y + DefThrPercent.x + DefThrPAPerPoss.x + DefThrPMPerPoss.x
                   + TwoPercent.x + TwoPAPerPoss.x + TwoPMPerPoss.x + DefTwoPercent.y + DefTwoPAPerPoss.y  + DefTwoPMPerPoss.y
                   + TwoPercent.y + TwoPAPerPoss.y + TwoPMPerPoss.y + DefTwoPercent.x + DefTwoPAPerPoss.x + DefTwoPMPerPoss.x
                   + FTPercent.x + FTAPerPoss.x + FTMPerPoss.x + FTPercentXDefFTs.y + DefFTAPerPoss.y
                   + FTPercent.y + FTAPerPoss.y + FTMPerPoss.y + FTPercentXDefFTs.x + DefFTAPerPoss.x
                   + DREBrt.x + OREBrt.x + DREBrtXFGMiss.x  + OREBrtXFGMiss.x
                   + DREBrt.y + OREBrt.y + DREBrtXFGMiss.y + OREBrtXFGMiss.y
                   + ASTPerPoss.x + ASTPerPoss.y + DefASTPerPoss.x + DefASTPerPoss.y
                   + STLPerPoss.x + STLPerPoss.y + DefSTLPerPoss.x + DefSTLPerPoss.y
                   + BLKPerPoss.x + BLKPerPoss.y + DefBLKPerPoss.x + DefBLKPerPoss.y
                   + TOPerPoss.x + TOPerPoss.y + DefTOPerPoss.x + DefTOPerPoss.y
                   + ThrPercentXX.x + ThrPAPerPossXX.x + ThrPMPerPossXX.x 
                   + TwoPercentXX.x + TwoPAPerPossXX.x + TwoPMPerPossXX.x 
                   + FTAPerPossXX.x + DREBRtXX.x + DREBTotXX.x 
                   + ASTPerPossXX.x + TOPerPossXX.x + ThrPercentXX.y + ThrPAPerPossXX.y 
                   + ThrPMPerPossXX.y + TwoPercentXX.y + TwoPAPerPossXX.y 
                   + TwoPMPerPossXX.y + FTAPerPossXX.y + DREBRtXX.y 
                   + DREBTotXX.y + ASTPerPossXX.y + TOPerPossXX.y + NumGamesWeek.x 
                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + ShrHomeWeek.x 
                   + ShrHomeWeek.y + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                   + DistDummy6 + DistDummy7, data = seasondata)
summary(modelalpha4a)$coef
modelalpha4b <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) + GameDummy5 + GameDummy10 + GameDummy15 + GameDummy20 + GameDummy25 + GameDummy30 + GameDummy35 + GameDummy40 + GameDummy45 + GameDummy50 + GameDummy55 + GameDummy60 + GameDummy65 + GameDummy70 + GameDummy82 + GameDummy5XPrevSeas + GameDummy10XPrevSeas + GameDummy15XPrevSeas + GameDummy20XPrevSeas + GameDummy25XPrevSeas + GameDummy30XPrevSeas + GameDummy35XPrevSeas + GameDummy40XPrevSeas + GameDummy45XPrevSeas + GameDummy50XPrevSeas + GameDummy55XPrevSeas + GameDummy60XPrevSeas + GameDummy65XPrevSeas + GameDummy70XPrevSeas + GameDummy82XPrevSeas + GameDummy5XPrevSeasY + GameDummy10XPrevSeasY + GameDummy15XPrevSeasY + GameDummy20XPrevSeasY + GameDummy25XPrevSeasY + GameDummy30XPrevSeasY + GameDummy35XPrevSeasY + GameDummy40XPrevSeasY + GameDummy45XPrevSeasY + GameDummy50XPrevSeasY + GameDummy55XPrevSeasY + GameDummy60XPrevSeasY + GameDummy65XPrevSeasY + GameDummy70XPrevSeasY + GameDummy82XPrevSeasY  + PaceXSq2 + PaceYSq2 + PaceXPace2 +
                     AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + YearSq + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                   + ThrPercent.x + ThrPAPerPoss.x + ThrPMPerPoss.x + DefThrPercent.y + DefThrPAPerPoss.y  + DefThrPMPerPoss.y
                   + ThrPercent.y + ThrPAPerPoss.y + ThrPMPerPoss.y + DefThrPercent.x + DefThrPAPerPoss.x + DefThrPMPerPoss.x
                   + TwoPercent.x + TwoPAPerPoss.x + TwoPMPerPoss.x + DefTwoPercent.y + DefTwoPAPerPoss.y  + DefTwoPMPerPoss.y
                   + TwoPercent.y + TwoPAPerPoss.y + TwoPMPerPoss.y + DefTwoPercent.x + DefTwoPAPerPoss.x + DefTwoPMPerPoss.x
                   + FTPercent.x + FTAPerPoss.x + FTMPerPoss.x + FTPercentXDefFTs.y + DefFTAPerPoss.y
                   + FTPercent.y + FTAPerPoss.y + FTMPerPoss.y + FTPercentXDefFTs.x + DefFTAPerPoss.x
                   + DREBrt.x + OREBrt.x + DREBrtXFGMiss.x  + OREBrtXFGMiss.x
                   + DREBrt.y + OREBrt.y + DREBrtXFGMiss.y + OREBrtXFGMiss.y
                   + ASTPerPoss.x + ASTPerPoss.y + DefASTPerPoss.x + DefASTPerPoss.y
                   + STLPerPoss.x + STLPerPoss.y + DefSTLPerPoss.x + DefSTLPerPoss.y
                   + BLKPerPoss.x + BLKPerPoss.y + DefBLKPerPoss.x + DefBLKPerPoss.y
                   + TOPerPoss.x + TOPerPoss.y + DefTOPerPoss.x + DefTOPerPoss.y
                   + ThrPercentXX.x + ThrPAPerPossXX.x + ThrPMPerPossXX.x 
                   + TwoPercentXX.x + TwoPAPerPossXX.x + TwoPMPerPossXX.x 
                   + FTAPerPossXX.x + DREBRtXX.x + DREBTotXX.x 
                   + ASTPerPossXX.x + TOPerPossXX.x + ThrPercentXX.y + ThrPAPerPossXX.y 
                   + ThrPMPerPossXX.y + TwoPercentXX.y + TwoPAPerPossXX.y 
                   + TwoPMPerPossXX.y + FTAPerPossXX.y + DREBRtXX.y 
                   + DREBTotXX.y + ASTPerPossXX.y + TOPerPossXX.y + NumGamesWeek.x 
                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + ShrHomeWeek.x 
                   + ShrHomeWeek.y + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                   + DistDummy6 + DistDummy7, data = seasondata)
summary(modelalpha4b)$coef
modelalpha4c <- lm(formula = TotalPoints ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
                   + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
                   + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
                   + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
                     + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y 
                   + WinPct20.y + WinPct10.y + WinPct5.y + PPXPace15 + PtsPerPossAvg20.x + PPXPace16 + PtsPerPossAvg10.x + PPXPace17 + PtsPerPossAvg5.x +
                     PPXPace18 + DefPtsPerPossAvg20.x + PPXPace19 + DefPtsPerPossAvg10.x + PPXPace20 + DefPtsPerPossAvg5.x +
                     PPXPace21 + OppPtsPerPossAvg20.x + PPXPace22 + OppPtsPerPossAvg10.x + PPXPace23 + OppPtsPerPossAvg5.x +
                     PPXPace24 + OppDefPtsPerPossAvg20.x + PPXPace25 + OppDefPtsPerPossAvg10.x + PPXPace26 + OppDefPtsPerPossAvg5.x +
                     PPXPace27 + PtsPerPossAvg20.y + PPXPace28 + PtsPerPossAvg10.y + PPXPace29 + PtsPerPossAvg5.y +
                     PPXPace30 + DefPtsPerPossAvg20.y + PPXPace31 + DefPtsPerPossAvg10.y + PPXPace32 + DefPtsPerPossAvg5.y +
                     PPXPace33 + OppPtsPerPossAvg20.y + PPXPace34 + OppPtsPerPossAvg10.y + PPXPace35 + OppPtsPerPossAvg5.y +
                     PPXPace36 + OppDefPtsPerPossAvg20.y + PPXPace37 + OppDefPtsPerPossAvg10.y + PPXPace38 + OppDefPtsPerPossAvg5.y + 
                     PPXLess2015 + PPXLess2016 + PPXLess2017 + 
                     PPXLess2018 + PPXLess2019 + PPXLess2020 + 
                     PPXLess2021 + PPXLess2022 + PPXLess2023 +
                     PPXLess2024 +  PPXLess2025 + PPXLess2026  +
                     PPXLess2027 + PPXLess2028 +  PPXLess2029  +
                     PPXLess2030 + PPXLess2031 + PPXLess2032 +
                     PPXLess2033 + PPXLess2034 + PPXLess2035 +
                     PPXLess2036  + PPXLess2037 + PPXLess2038 + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y 
                   + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y +
                     + ThrPercent.x + ThrPAPerPoss.x + ThrPMPerPoss.x + DefThrPercent.y + DefThrPAPerPoss.y  + DefThrPMPerPoss.y
                   + ThrPercent.y + ThrPAPerPoss.y + ThrPMPerPoss.y + DefThrPercent.x + DefThrPAPerPoss.x + DefThrPMPerPoss.x
                   + TwoPercent.x + TwoPAPerPoss.x + TwoPMPerPoss.x + DefTwoPercent.y + DefTwoPAPerPoss.y  + DefTwoPMPerPoss.y
                   + TwoPercent.y + TwoPAPerPoss.y + TwoPMPerPoss.y + DefTwoPercent.x + DefTwoPAPerPoss.x + DefTwoPMPerPoss.x
                   + FTPercent.x + FTAPerPoss.x + FTMPerPoss.x + FTPercentXDefFTs.y + DefFTAPerPoss.y
                   + FTPercent.y + FTAPerPoss.y + FTMPerPoss.y + FTPercentXDefFTs.x + DefFTAPerPoss.x
                   + DREBrt.x + OREBrt.x + DREBrtXFGMiss.x  + OREBrtXFGMiss.x
                   + DREBrt.y + OREBrt.y + DREBrtXFGMiss.y + OREBrtXFGMiss.y
                   + ASTPerPoss.x + ASTPerPoss.y + DefASTPerPoss.x + DefASTPerPoss.y
                   + STLPerPoss.x + STLPerPoss.y + DefSTLPerPoss.x + DefSTLPerPoss.y
                   + BLKPerPoss.x + BLKPerPoss.y + DefBLKPerPoss.x + DefBLKPerPoss.y
                   + TOPerPoss.x + TOPerPoss.y + DefTOPerPoss.x + DefTOPerPoss.y
                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                   + DistDummy6 + DistDummy7 
                   + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
                     +  PPXPace9 + PPXPace10 + PPXPace11 + PPXPace12 + PPXPace13 + PPXPace14 + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y , data = seasondata)
summary(modelalpha4c)$coef
modelalpha4d <- glm(formula = HomeWin ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) + GameDummy5 + GameDummy10 + GameDummy15 + GameDummy20 + GameDummy25 + GameDummy30 + GameDummy35 + GameDummy40 + GameDummy45 + GameDummy50 + GameDummy55 + GameDummy60 + GameDummy65 + GameDummy70 + GameDummy82 + GameDummy5XPrevSeas + GameDummy10XPrevSeas + GameDummy15XPrevSeas + GameDummy20XPrevSeas + GameDummy25XPrevSeas + GameDummy30XPrevSeas + GameDummy35XPrevSeas + GameDummy40XPrevSeas + GameDummy45XPrevSeas + GameDummy50XPrevSeas + GameDummy55XPrevSeas + GameDummy60XPrevSeas + GameDummy65XPrevSeas + GameDummy70XPrevSeas + GameDummy82XPrevSeas + GameDummy5XPrevSeasY + GameDummy10XPrevSeasY + GameDummy15XPrevSeasY + GameDummy20XPrevSeasY + GameDummy25XPrevSeasY + GameDummy30XPrevSeasY + GameDummy35XPrevSeasY + GameDummy40XPrevSeasY + GameDummy45XPrevSeasY + GameDummy50XPrevSeasY + GameDummy55XPrevSeasY + GameDummy60XPrevSeasY + GameDummy65XPrevSeasY + GameDummy70XPrevSeasY + GameDummy82XPrevSeasY  + PaceXSq2 + PaceYSq2 + PaceXPace2 +
                      AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + YearSq + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
                    + ThrPercent.x + ThrPAPerPoss.x + ThrPMPerPoss.x + DefThrPercent.y + DefThrPAPerPoss.y  + DefThrPMPerPoss.y
                    + ThrPercent.y + ThrPAPerPoss.y + ThrPMPerPoss.y + DefThrPercent.x + DefThrPAPerPoss.x + DefThrPMPerPoss.x
                    + TwoPercent.x + TwoPAPerPoss.x + TwoPMPerPoss.x + DefTwoPercent.y + DefTwoPAPerPoss.y  + DefTwoPMPerPoss.y
                    + TwoPercent.y + TwoPAPerPoss.y + TwoPMPerPoss.y + DefTwoPercent.x + DefTwoPAPerPoss.x + DefTwoPMPerPoss.x
                    + FTPercent.x + FTAPerPoss.x + FTMPerPoss.x + FTPercentXDefFTs.y + DefFTAPerPoss.y
                    + FTPercent.y + FTAPerPoss.y + FTMPerPoss.y + FTPercentXDefFTs.x + DefFTAPerPoss.x
                    + DREBrt.x + OREBrt.x + DREBrtXFGMiss.x  + OREBrtXFGMiss.x
                    + DREBrt.y + OREBrt.y + DREBrtXFGMiss.y + OREBrtXFGMiss.y
                    + ASTPerPoss.x + ASTPerPoss.y + DefASTPerPoss.x + DefASTPerPoss.y
                    + STLPerPoss.x + STLPerPoss.y + DefSTLPerPoss.x + DefSTLPerPoss.y
                    + BLKPerPoss.x + BLKPerPoss.y + DefBLKPerPoss.x + DefBLKPerPoss.y
                    + TOPerPoss.x + TOPerPoss.y + DefTOPerPoss.x + DefTOPerPoss.y
                    + ThrPercentXX.x + ThrPAPerPossXX.x + ThrPMPerPossXX.x 
                    + TwoPercentXX.x + TwoPAPerPossXX.x + TwoPMPerPossXX.x 
                    + FTAPerPossXX.x + DREBRtXX.x + DREBTotXX.x 
                    + ASTPerPossXX.x + TOPerPossXX.x + ThrPercentXX.y + ThrPAPerPossXX.y 
                    + ThrPMPerPossXX.y + TwoPercentXX.y + TwoPAPerPossXX.y 
                    + TwoPMPerPossXX.y + FTAPerPossXX.y + DREBRtXX.y 
                    + DREBTotXX.y + ASTPerPossXX.y + TOPerPossXX.y + NumGamesWeek.x 
                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + ShrHomeWeek.x 
                    + ShrHomeWeek.y + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
                    + DistDummy6 + DistDummy7, data = seasondata, family = binomial(link = "logit"))
summary(modelalpha4d)$coef

pred_margin <- predict(modelalpha4a,newdata=preddata_final)
pred_poss <- predict(modelalpha4b,newdata=preddata_final)
pred_totalpoints <- predict(modelalpha4c,newdata=preddata_final)
if(length(preddata_final$Date)!=0) {
  pred_homewin <- predict(modelalpha4d,newdata=preddata_final,type = "response")
} else {
  pred_homewin <- pred_margin
}
date <- preddata_final$Date
description <- preddata_final$GameDescription
result_margin <- NA
result_poss <- NA
pred_line <- -1*pred_margin
result_line <- NA
pred_overunder <- pred_totalpoints
if(length(preddata_final$Date)!=0) {
  version <- "AlphaV4"
  predictionsAlphaV4 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)
  summary(modelbeta1a)
  save(predictionsAlphaV4, file = paste("predictionsAlphaV4.RData", sep=""))
  load("predictionsAlphaV4.RData")
  # load("predictionsBetaV1.RData")
  # load("predictionsBetaV2.RData")
  load("predictions.RData")
  
  # predictions <- predictions[,c(1,2,3,4,5,6,7,8)]
  predictions$cutoff <- NA
  predictionsAlphaV4$cutoff <- NA
  # predictionsBetaV1$cutoff <- NA
  # predictionsBetaV2$cutoff <- NA
  predictionsBetaV3$cutoff <- NA
  predictions_final <- rbind(predictions,predictionsAlphaV4,predictionsBetaV3)
  save(predictions_final, file = paste("predictions_final.RData", sep=""))
} else {
  message("Error: No Games in Next 3 days -- dont run")
}





load("todayslines.RData")
if (any(!is.na(lines))&today<"2023-06-20") {
  
predictions_final <-  predictions_final %>% left_join(lines,by = c("description","date")) %>% ungroup()
predictions_final <- dplyr::select(predictions_final,description,date,pred_overunder,pred_homewin,pred_line,homeline,moneylinehome,moneylineaway,overunder,everything())

save(predictions_final, file = paste("predictions_final.RData", sep=""))
} else {
  message("Error: No lines today -- check website/schedule")
}