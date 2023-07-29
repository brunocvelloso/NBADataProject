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
library(tictoc)
#setwd(working)

load("modelbetaV1_dataformerge.RData")
load("seasonsummaries_PlayerData.RData")
load("teamroster.RData")
load("modelbetaV1_PlayerData.RData")
load("allregdata_modelalphaV4.RData")

load("box_nopreseason.RData")
load("startdate.RData")
startdatemaster <- max(box_nopreseason$Date)
startdatemaster <- startdate

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
enddatemaster <- today
rm(box_nopreseason)

load("box_nopreseasonBetaUpdate.RData")
dateloop <- distinct(box_nopreseason,Date,Season)
dateloop <- dateloop %>% dplyr::group_by(Season) %>% dplyr::mutate(DateMax = as.Date(max(Date))+1) %>% ungroup()
Date <- unique(c(dateloop$Date,dateloop$DateMax))
dateloop2 <- data.frame(Date)
dateloop2 <- dateloop2[order(dateloop2$Date),]

playerdata <- dplyr::select(playerdata,MINPGPREV,MINPG2,ProjMINFinal,MinWgt,MinWgtInjAdj,MinEnoughAdjFinal,FinalStatus,InclDummy1,superadj,staradj,solidadj,zscore,superstar,star,solid,everything())

tic()
# for (i in as.list(dateloop2)) {
for (i in startdatemaster:enddatemaster) {
  # startdate <- i
  # enddate <- i
  
  startdate <- as.Date(i,origin)
  enddate <- as.Date(i,origin)
  
  playerdataTEMP <- playerdata[playerdata$Date==startdate,c("Team","Players","PlayerID","OnOffGame","RPMScore","ORPMScore","DRPMScore","OnOffTeamAvg","Box1TeamAvg","Box2TeamAvg","Box3TeamAvg","ImpactPct.pg","Season")]
  seasonvar <- ifelse((month(enddate)>=10&day(enddate)>=12)|month(enddate)>=11,paste(year(enddate),"-",substr(year(enddate)+1,3,4),sep=""),paste(year(enddate)-1,"-",substr(year(enddate),3,4),sep=""))
  playerdataSeasonTemp <- playerdata[playerdata$Season==seasonvar,c("Team","Players","PlayerID","Date","MinWgtInjAdj","InclDummy1")]
  playerdataSeasonTemp <-  playerdataSeasonTemp %>% left_join(playerdataTEMP,by = c("Team","Players","PlayerID")) %>% ungroup()
  playerdataSeasonTemp <- playerdataSeasonTemp[!is.na(playerdataSeasonTemp$OnOffGame),]
  
  playerdataSeasonTemp$MinWgtXOnOffXUsgInjAdj <- playerdataSeasonTemp$MinWgtInjAdj*playerdataSeasonTemp$OnOffGame*playerdataSeasonTemp$ImpactPct.pg
  playerdataSeasonTemp$MinWgtXBox1Adj <- playerdataSeasonTemp$MinWgtInjAdj*playerdataSeasonTemp$RPMScore
  playerdataSeasonTemp$MinWgtXBox2Adj <- playerdataSeasonTemp$MinWgtInjAdj*playerdataSeasonTemp$ORPMScore
   playerdataSeasonTemp$MinWgtXBox3Adj <- playerdataSeasonTemp$MinWgtInjAdj*playerdataSeasonTemp$DRPMScore
  playerdataSeasonTemp <- playerdataSeasonTemp %>% 
    dplyr::group_by(Team, Season, Date) %>%
    dplyr::mutate(OnOffInjAdj = sum(MinWgtXOnOffXUsgInjAdj,na.rm = TRUE), Box1InjAdj = sum(MinWgtXBox1Adj,na.rm = TRUE),
           Box2InjAdj = sum(MinWgtXBox1Adj,na.rm = TRUE),Box3InjAdj = sum(MinWgtXBox3Adj,na.rm = TRUE))
  
  playerdataTempMerge <- playerdataSeasonTemp %>% 
    dplyr::group_by(Team, Season, Date) %>%
    dplyr::summarize(OnOffTeamAvg = max(OnOffTeamAvg,na.rm = TRUE),Box1TeamAvg = max(Box1TeamAvg,na.rm = TRUE),
              Box2TeamAvg = max(Box1TeamAvg,na.rm = TRUE),
              Box3TeamAvg = max(Box3TeamAvg,na.rm = TRUE),Box1InjAdj = max(Box1InjAdj,na.rm = TRUE),
              Box2InjAdj = max(Box1InjAdj,na.rm = TRUE),
              OnOffInjAdj = max(OnOffInjAdj,na.rm = TRUE),Box3InjAdj = max(Box3InjAdj,na.rm = TRUE)) %>%
    ungroup()
  
  playerdataTempMerge$OnOffAdjustment <- -1*(playerdataTempMerge$OnOffTeamAvg - playerdataTempMerge$OnOffInjAdj)
  playerdataTempMerge$Box1Adjustment <- -1*(playerdataTempMerge$Box1TeamAvg - playerdataTempMerge$Box1InjAdj)
  playerdataTempMerge$Box2Adjustment <- -1*(playerdataTempMerge$Box2TeamAvg - playerdataTempMerge$Box2InjAdj)
  playerdataTempMerge$Box3Adjustment <- -1*(playerdataTempMerge$Box3TeamAvg - playerdataTempMerge$Box3InjAdj)
  
  ###NEXT STEPS: CREATE list of teams, adn for each get list of opponents, game counts, ordered by date. Then
  ###merge the adjustments to the opponents, and compute the sum of opponent adjustent for whole season,
  ###last 20 games, 10 games, 5 games and so forth. Compute own adjustment for last 5 games, 10 games, 20 games
  
  teamlist <- distinct(playerdataTEMP,Team,Season)
  teamlist$Date <- startdate
  teamlist$OppWinPct <- NA
  teamlist$OppWinPct20 <- NA
  teamlist$OppWinPct10 <- NA
  teamlist$OppWinPct5 <- NA
  teamlist$OppOnOffAdj <- NA
  teamlist$OppOnOffAdj20 <- NA
  teamlist$OppOnOffAdj10 <- NA
  teamlist$OppOnOffAdj5 <- NA
  teamlist$OppBox1Adj <- NA
  teamlist$OppBox1Adj20 <- NA
  teamlist$OppBox1Adj10 <- NA
  teamlist$OppBox1Adj5 <- NA
  teamlist$OppBox2Adj <- NA
  teamlist$OppBox2Adj20 <- NA
  teamlist$OppBox2Adj10 <- NA
  teamlist$OppBox2Adj5 <- NA
  teamlist$OppBox3Adj <- NA
  teamlist$OppBox3Adj20 <- NA
  teamlist$OppBox3Adj10 <- NA
  teamlist$OppBox3Adj5 <- NA
  teamlist$OwnOnOffAdj <- NA
  teamlist$OwnOnOffAdj20 <- NA
  teamlist$OwnOnOffAdj10 <- NA
  teamlist$OwnOnOffAdj5 <- NA
  teamlist$OwnBox1Adj <- NA
  teamlist$OwnBox1Adj20 <- NA
  teamlist$OwnBox1Adj10 <- NA
  teamlist$OwnBox1Adj5 <- NA
  teamlist$OwnBox2Adj <- NA
  teamlist$OwnBox2Adj20 <- NA
  teamlist$OwnBox2Adj10 <- NA
  teamlist$OwnBox2Adj5 <- NA
  teamlist$OwnBox3Adj <- NA
  teamlist$OwnBox3Adj20 <- NA
  teamlist$OwnBox3Adj10 <- NA
  teamlist$OwnBox3Adj5 <- NA
  seasondataTEMP <- seasondata[seasondata$Season==seasonvar&seasondata$Date<=startdate,]
  teamlistwinpct1 <- seasondataTEMP %>%
    dplyr::group_by(Team.x) %>%
    dplyr::summarize(Date = max(Date)) %>%
    ungroup()
  teamlistwinpct1 <- dplyr::rename(teamlistwinpct1,Team = Team.x)
  teamlistwinpct2 <- seasondataTEMP %>%
    dplyr::group_by(Team.y) %>%
    dplyr::summarize(Date = max(Date)) %>%
    ungroup()
  teamlistwinpct2 <- dplyr::rename(teamlistwinpct2,Team = Team.y)
  teamlistwinpct <- rbind(teamlistwinpct1,teamlistwinpct2)
  teamlistwinpct <- teamlistwinpct %>%
    dplyr::group_by(Team) %>%
    dplyr::summarize(DateMax = max(Date)) %>%
    ungroup()
  teamlist <- teamlist %>% left_join(teamlistwinpct,by = c("Team")) %>% ungroup()
  teamlist$Team.x <- teamlist$Team
  teamlist$Team.y <- teamlist$Team
  teamlist <- teamlist %>% left_join(dplyr::rename(seasondataTEMP[,c("WinPct2.x","Team.x","Date")],DateMax=Date),by = c("Team.x","DateMax")) %>% ungroup() 
  teamlist <- teamlist %>% left_join(dplyr::rename(seasondataTEMP[,c("WinPct2.y","Team.y","Date")],DateMax=Date),by = c("Team.y","DateMax")) %>% ungroup() 
  teamlist$WinPct <- ifelse(!is.na(teamlist$WinPct2.x),teamlist$WinPct2.x,teamlist$WinPct2.y)
  seasondataTEMP <- seasondata[seasondata$Season==seasonvar&seasondata$Date<startdate,]
  for (j in teamlist$Team) {
    seasondataTeams1 <- seasondataTEMP[seasondataTEMP$Team.x==j,c("Team.y","Date","HomeMargin","GameCount.x")]
    seasondataTeams1 <- dplyr::rename(seasondataTeams1,Team = Team.y,GameCount = GameCount.x)
    seasondataTeams2 <- seasondataTEMP[seasondataTEMP$Team.y==j,c("Team.x","Date","HomeMargin","GameCount.y")]
    seasondataTeams2$HomeMargin <- seasondataTeams2$HomeMargin*-1
    seasondataTeams2 <- dplyr::rename(seasondataTeams2,Team = Team.x,GameCount = GameCount.y)
    seasondataTEAM <- rbind(seasondataTeams1,seasondataTeams2)
    seasondataTEAM <- seasondataTEAM[order(seasondataTEAM$Date),]
    seasondataTEAM <- seasondataTEAM %>% left_join(teamlist[,c("WinPct","Team")],by = c("Team")) %>% ungroup() 
    seasondataTEAM <- seasondataTEAM %>% left_join(playerdataTempMerge[,c("Team","Date","OnOffAdjustment","Box1Adjustment","Box2Adjustment","Box3Adjustment")],by = c("Team","Date")) %>% ungroup() 
    teamlist$OppWinPct[teamlist$Team==j] <- mean(seasondataTEAM$WinPct,na.rm = TRUE) 
    teamlist$OppWinPct20[teamlist$Team==j] <- mean(seasondataTEAM$WinPct[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OppWinPct10[teamlist$Team==j] <- mean(seasondataTEAM$WinPct[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OppWinPct5[teamlist$Team==j] <- mean(seasondataTEAM$WinPct[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OppOnOffAdj[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment,na.rm = TRUE) 
    teamlist$OppOnOffAdj20[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OppOnOffAdj10[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OppOnOffAdj5[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OppBox1Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment,na.rm = TRUE) 
    teamlist$OppBox1Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OppBox1Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OppBox1Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OppBox2Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment,na.rm = TRUE) 
    teamlist$OppBox2Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OppBox2Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OppBox2Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OppBox3Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment,na.rm = TRUE) 
    teamlist$OppBox3Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OppBox3Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OppBox3Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    
    
    if (length(seasondataTEAM$Team)!=0) {
      seasondataTEAM$Team <- j
    }
    seasondataTEAM <- seasondataTEAM %>% left_join(playerdataTempMerge[,c("Team","Date","OnOffAdjustment","Box1Adjustment","Box2Adjustment","Box3Adjustment")],by = c("Team","Date")) %>% ungroup() 
    teamlist$OwnOnOffAdj[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment.y,na.rm = TRUE) 
    teamlist$OwnOnOffAdj20[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OwnOnOffAdj10[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OwnOnOffAdj5[teamlist$Team==j] <- mean(seasondataTEAM$OnOffAdjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OwnBox1Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment.y,na.rm = TRUE) 
    teamlist$OwnBox1Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OwnBox1Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OwnBox1Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box1Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OwnBox2Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment.y,na.rm = TRUE) 
    teamlist$OwnBox2Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OwnBox2Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OwnBox2Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box2Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
    teamlist$OwnBox3Adj[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment.y,na.rm = TRUE) 
    teamlist$OwnBox3Adj20[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-18)],na.rm = TRUE)
    teamlist$OwnBox3Adj10[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-8)],na.rm = TRUE)
    teamlist$OwnBox3Adj5[teamlist$Team==j] <- mean(seasondataTEAM$Box3Adjustment.y[seasondataTEAM$GameCount>=(max(seasondataTEAM$GameCount)-3)],na.rm = TRUE)
  }
  
  # if (i==dateloop2[1]) {
  #   teamlistMERGE <- teamlist
  # }
  # if (i!=dateloop2[1]) {
  #   teamlistMERGE <- rbind(teamlistMERGE,teamlist)
  # }
  if (i==startdatemaster) {
    teamlistMERGE <- teamlist
  }
  if (i!=startdatemaster) {
    teamlistMERGE <- rbind(teamlistMERGE,teamlist)
  }
  print(as.Date(i,origin))
}
toc()

teamlistTOADD <- teamlistMERGE
save(teamlistTOADD, file = paste("modelbetaV1_teamlistTOADD.RData", sep=""))
load("modelbetaV1_teamlist.RData")
teamlistMERGE <- teamlistMERGE[teamlistMERGE$Date<startdatemaster,]
teamlistMERGE <- rbind(teamlistMERGE,teamlistTOADD)
teamlistMERGE <- distinct(teamlistMERGE,Team,Date,.keep_all = TRUE)

teamlistMERGE <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(teamlistMERGE,
                                function(x) replace(x, is.infinite(x), NA)))
teamlistMERGE <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(teamlistMERGE,
                                function(x) replace(x, is.nan(x), NA)))

save(teamlistMERGE, file = paste("modelbetaV1_teamlist.RData", sep=""))


