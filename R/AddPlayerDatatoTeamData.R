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
#setwd(working)

load("box_nopreseason.RData")
load("startdate.RData")
startdatemaster <- max(box_nopreseason$Date)
startdatemaster <- startdate

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
enddatemaster <- today

load("data_modelalphaV4.RData")
load("vellplusmin.RData")
load("repl_values.RData")
load("testdata_modelalphaV4.RData") #for new games

#create team rosters and cleaning to make sure team names link up (and no all-stars)
box_nopreseason$Team[box_nopreseason$Team=="NO/Oklahoma City Hornets"] <- "New Orleans Hornets"
box_nopreseason$Team[box_nopreseason$Team=="Seattle SuperSonics"] <- "Oklahoma City Thunder"
box_nopreseason$Team[box_nopreseason$Team=="New Jersey Nets"] <- "Brooklyn Nets"
box_nopreseason$Team[box_nopreseason$Team=="New Orleans Hornets"] <- "New Orleans Pelicans"
box_nopreseason$Team[box_nopreseason$Team=="Charlotte Bobcats"] <- "Charlotte Hornets"
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Western Conf All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Eastern Conf All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Western Conf All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Eastern Conf All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Giannis ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Lebron ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="USA ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="World ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Durant ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Curry ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Steph ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Stephen ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Giannis ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Lebron ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team LeBron ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="USA ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="World ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Durant ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Curry ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Steph ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Team Stephen ",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Western Confe All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Western Confe All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="Eastern Confe All-Stars",]
box_nopreseason <- box_nopreseason[box_nopreseason$Team!="",]

box_nopreseason <- box_nopreseason %>% 
  dplyr::group_by(Season) %>% 
  dplyr::mutate(SeasNum = cur_group_id()) %>%
  ungroup()

teamroster <- distinct(box_nopreseason,PlayerID,Team,Season,.keep_all=TRUE)
teamroster <- teamroster[order(teamroster$Season,teamroster$Team,teamroster$Players),]
teamroster <- teamroster %>%
  dplyr::select(Season,Team, Players, PlayerID, Date, everything())
teamroster$PlayerID[!is.na(teamroster$Players)&teamroster$Players=="TEAM"] <- teamroster$Players[!is.na(teamroster$Players)&teamroster$Players=="TEAM"]
teamroster <- teamroster[order(teamroster$Season,teamroster$Team,teamroster$Players),]
teamroster <- teamroster[!is.na(teamroster$Season),]

box_nopreseason <- merge(box_nopreseason,seasondata,by="GameID",all.x=TRUE,all.y=FALSE)
box_nopreseason <- box_nopreseason[,c(1:100)]
box_nopreseason <- distinct(box_nopreseason,GameID,Players,GameDescription,PlayerID,Date.x,Team,.keep_all = TRUE)
box_nopreseason$ImpactPlays <- box_nopreseason$FGA + .44*box_nopreseason$FTA + box_nopreseason$AST + box_nopreseason$REB + box_nopreseason$BLK + box_nopreseason$STL + box_nopreseason$TO + box_nopreseason$PF
MINdata <- aggregate(box_nopreseason$MIN, by=list(GameID=box_nopreseason$GameID,Team=box_nopreseason$Team), FUN=sum,na.rm = TRUE)
MINdata <- dplyr::rename(MINdata ,MIN.xy=x)
box_nopreseason <- merge(box_nopreseason,MINdata,by=c("GameID","Team"),all.x=TRUE,all.y=FALSE)
box_nopreseason$MIN.xy <- box_nopreseason$MIN.xy/5
box_nopreseason$ImpactPct <- box_nopreseason$ImpactPlays/((box_nopreseason$MIN/box_nopreseason$MIN.xy)*box_nopreseason$Poss)
box_nopreseason$PossPlayed <- ((box_nopreseason$MIN/box_nopreseason$MIN.xy)*box_nopreseason$Poss)
box_nopreseason$ImpactPct[is.infinite(box_nopreseason$ImpactPct)] <- 0
box_nopreseason$ImpactPct[is.nan(box_nopreseason$ImpactPct)] <- NA
box_nopreseason$OnOff <- ifelse(!is.na(box_nopreseason$Team)&!is.na(box_nopreseason$Team.x)&box_nopreseason$Team==box_nopreseason$Team.x,(box_nopreseason$MIN.xy/box_nopreseason$MIN)*box_nopreseason$`+/-` - (box_nopreseason$MIN.xy/(box_nopreseason$MIN.xy-box_nopreseason$MIN))*(box_nopreseason$HomeMargin-box_nopreseason$`+/-`),(box_nopreseason$MIN.xy/box_nopreseason$MIN)*box_nopreseason$`+/-` - (box_nopreseason$MIN.xy/(box_nopreseason$MIN.xy-box_nopreseason$MIN))*(-1*box_nopreseason$HomeMargin-box_nopreseason$`+/-`))
box_nopreseason <- box_nopreseason %>%
  dplyr::select(GameID,Players, MIN, MIN.xy, ImpactPlays, ImpactPct, `+/-`,HomeMargin,OnOff,everything())

box_nopreseason <- dplyr::rename(box_nopreseason,Season = Season.x,Date = Date.x)
box_nopreseason$PlayerID[!is.na(box_nopreseason$Players)&box_nopreseason$Players=="TEAM"] <- box_nopreseason$Players[!is.na(box_nopreseason$Players)&box_nopreseason$Players=="TEAM"]
box_nopreseason <- box_nopreseason[!is.na(box_nopreseason$GameID),]

box_nopreseason$Margin[box_nopreseason$Team==box_nopreseason$Team.x] <- box_nopreseason$HomeMargin[box_nopreseason$Team==box_nopreseason$Team.x]
box_nopreseason$Margin[box_nopreseason$Team==box_nopreseason$Team.y] <- -1*box_nopreseason$HomeMargin[box_nopreseason$Team==box_nopreseason$Team.y]
box_nopreseason$Margin[is.na(box_nopreseason$MIN)&box_nopreseason$Players!="TEAM"] <- 0
box_nopreseason$MIN.xy[is.na(box_nopreseason$MIN)&box_nopreseason$Players!="TEAM"] <- NA
# box_nopreseason <- do.call(data.frame,                      # Replace Inf in data by NA
#                  lapply(box_nopreseason,
#                         function(x) replace(x, is.infinite(x), NA)))
# box_nopreseason <- do.call(data.frame,                      # Replace Inf in data by NA
#                  lapply(box_nopreseason,
#                         function(x) replace(x, is.nan(x), NA)))

box_nopreseason$GameIDFix <- box_nopreseason$GameID
box_nopreseason$GameIDFix[is.na(box_nopreseason$MIN)&box_nopreseason$Players!="TEAM"] <- ""
box_nopreseason$DateFix <- box_nopreseason$Date
box_nopreseason$DateFix[is.na(box_nopreseason$MIN)&box_nopreseason$Players!="TEAM"] <- NA

##Estimate Box Plus/Minus
box_nopreseason$OREB.pm <- ifelse(is.infinite(box_nopreseason$OREB/box_nopreseason$MIN)|is.nan(box_nopreseason$OREB/box_nopreseason$MIN),NA,box_nopreseason$OREB/box_nopreseason$MIN)
box_nopreseason$DREB.pm <- ifelse(is.infinite(box_nopreseason$DREB/box_nopreseason$MIN)|is.nan(box_nopreseason$DREB/box_nopreseason$MIN),NA,box_nopreseason$DREB/box_nopreseason$MIN)
box_nopreseason$AST.pm <- ifelse(is.infinite(box_nopreseason$AST/box_nopreseason$MIN)|is.nan(box_nopreseason$AST/box_nopreseason$MIN),NA,box_nopreseason$AST/box_nopreseason$MIN)
box_nopreseason$STL.pm <- ifelse(is.infinite(box_nopreseason$STL/box_nopreseason$MIN)|is.nan(box_nopreseason$STL/box_nopreseason$MIN),NA,box_nopreseason$STL/box_nopreseason$MIN)
box_nopreseason$PTS.pm <- ifelse(is.infinite(box_nopreseason$PTS/box_nopreseason$MIN)|is.nan(box_nopreseason$PTS/box_nopreseason$MIN),NA,box_nopreseason$PTS/box_nopreseason$MIN)
box_nopreseason$TO.pm <- ifelse(is.infinite(box_nopreseason$TO/box_nopreseason$MIN)|is.nan(box_nopreseason$TO/box_nopreseason$MIN),NA,box_nopreseason$TO/box_nopreseason$MIN)
box_nopreseason$PF.pm <- ifelse(is.infinite(box_nopreseason$PF/box_nopreseason$MIN)|is.nan(box_nopreseason$PF/box_nopreseason$MIN),NA,box_nopreseason$PF/box_nopreseason$MIN)

box_nopreseason$ID_team <- as.character(paste(box_nopreseason$PlayerID,"_",box_nopreseason$Team,sep=""))
# playerdata <- expand.grid(unique(box_nopreseason$Date[!is.na(box_nopreseason$PlayerID)]), unique(box_nopreseason$ID_team[!is.na(box_nopreseason$PlayerID)]))
playerdata <- expand.grid(unique(box_nopreseason$Date[!is.na(box_nopreseason$PlayerID)&box_nopreseason$Date>=startdate]), unique(box_nopreseason$ID_team[!is.na(box_nopreseason$PlayerID)]))
playerdata <- dplyr::rename(playerdata, Date=Var1,ID_team=Var2)
playerdata$ID_team <- as.character(playerdata$ID_team)
seasons <- distinct(box_nopreseason,Date,Season)
playerdata <- playerdata %>% left_join(seasons[,c("Date","Season")],by=c("Date"))
playerdata$PlayerID <- substr(playerdata$ID_team,1,unlist(gregexpr("_",playerdata$ID_team))-1)
playerdata$Team <- substr(playerdata$ID_team,unlist(gregexpr("_",playerdata$ID_team))+1,nchar(playerdata$ID_team))
playerdata <- playerdata %>% left_join(box_nopreseason[,c("Date","PlayerID","Team","MIN","MIN.xy","ImpactPct","+/-","Margin","GameID","Playoffs.x")],by=c("Date","PlayerID","Team"))
playerdataTOADD2 <- playerdata
playerdataTOADD <- playerdata[playerdata$Date==max(playerdata$Date),]
playerdataTOADD$Date <- today 
playerdataTOADD$MIN <- 0
playerdataTOADD$MIN.xy <- 0 
playerdataTOADD$GameID <- "" 
playerdataTOADD$ImpactPct <- 0 
playerdataTOADD$`+/-` <- 0 
playerdataTOADD$Margin <- 0 
if (length(preddata_final$GameID)>0) {
addplayerdata <- function(i) {
  Team <- i
  GameID <- ifelse(is.na(max(preddata_final$GameID[(preddata_final$Team.x==Team|preddata_final$Team.y==Team)&preddata_final$Date==min(preddata_final$Date)],na.rm=TRUE)),
                   "",max(preddata_final$GameID[(preddata_final$Team.x==Team|preddata_final$Team.y==Team)&preddata_final$Date==min(preddata_final$Date)],na.rm=TRUE))
  Playoffs.x <- min(preddata_final$Playoffs,na.rm=TRUE)
  Season <- min(preddata_final$Season,na.rm=TRUE)
  value <- data.frame(Team,GameID,Playoffs.x,Season)
  return(value)
}
GameID_dat <- lapply(unique(playerdataTOADD$Team), addplayerdata) 
GameID_dat <- dplyr::bind_rows(GameID_dat, .id = "column_label")
GameID_dat <- dplyr::select(GameID_dat,-column_label)
playerdataTOADD <- dplyr::select(playerdataTOADD,-GameID,-Playoffs.x,-Season)
playerdataTOADD <- playerdataTOADD %>% left_join(GameID_dat,by=c("Team"))
###if new season
# load("newseasonplayerdata.RData")
}
load("modelbetaV1_PlayerData.RData")
save(playerdata,file="playerdata_backup.RData")
playerdata <- dplyr::select(playerdata,Date,ID_team,Season,PlayerID,Team,MIN,MIN.xy,ImpactPct,`+/-`,Margin,GameID,Playoffs.x)
playerdataOLD <- playerdata[playerdata$Date<startdate,]
playerdata <- rbind(playerdataOLD,playerdataTOADD2,playerdataTOADD)
rm(playerdataOLD)
playerdata$MIN[is.na(playerdata$MIN)] <- 0
playerdata$MIN.xy[is.na(playerdata$MIN.xy)] <- 0
playerdata$MIN.xy[playerdata$MIN==0] <- 0
playerdata$ImpactPct[is.na(playerdata$ImpactPct)] <- 0
playerdata$`+/-`[is.na(playerdata$`+/-`)] <- 0
playerdata$Margin[is.na(playerdata$Margin)] <- 0
playerdata <- playerdata %>% 
  dplyr::group_by(PlayerID,Season) %>%
  dplyr::mutate(Test = sum(as.numeric(MIN>0)),
         Test3 = sum(as.numeric(Date=="2022-10-16")))
playerdata <- playerdata[playerdata$Test>0|playerdata$Test3>0,]
# playerdata <- playerdata[playerdata$Test>0|playerdata$Season=="2022-23",]
playerdata <- playerdata %>% 
  dplyr::group_by(ID_team,Season) %>%
  dplyr::mutate(Test2 = sum(as.numeric(MIN>0)),
         Test4 = sum(as.numeric(Date=="2022-10-16")))
playerdata <- playerdata[playerdata$Test2>0|playerdata$Test4>0,]

# ###FOR EARLY DAYS OF SEASON###
# load("newseasonplayerdata.RData")
# replace1 <- playerdata[playerdata$Date=="2022-10-18",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-18")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)
# load("newseasonplayerdata.RData")
# replace1 <- playerdata[playerdata$Date=="2022-10-19",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-19")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)
# load("newseasonplayerdata.RData")
# replace1 <- playerdata[playerdata$Date=="2022-10-20",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-20")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)
# load("newseasonplayerdata.RData")
# replace1 <- playerdata[playerdata$Date=="2022-10-21",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-21")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)
# load("newseasonplayerdata.RData")
# replace1 <- playerdata[playerdata$Date=="2022-10-22",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-22")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)
# replace1 <- playerdata[playerdata$Date=="2022-10-23",]
# playerdataTOADD <- playerdataTOADD[!(playerdataTOADD$PlayerID %in% replace1$PlayerID),]
# playerdataTOADD$Date <- as.Date("2022-10-23")
# playerdata <- dplyr::bind_rows(playerdata,playerdataTOADD)



# playerdata <- playerdata[playerdata$Test2>0|playerdata$Season=="2022-23",]
playerdata <- dplyr::select(playerdata,-Test3,-Test4)
playerdata <- playerdata %>%
  dplyr::group_by(PlayerID,Team,Season) %>%
  arrange(PlayerID,Team,Season,Date) %>%
  dplyr::mutate(MIN2 = cumsum(MIN),
         Games2 = as.numeric(cumsum(MIN>0)),
         MINPG2 = MIN2/Games2,
         MIN2 = sapply(1:n(), function(x) ifelse(x-1>0,MIN2[x-1],0)),
         Games2 = sapply(1:n(), function(x) ifelse(x-1>0,Games2[x-1],0)),
         MINPG2 = sapply(1:n(), function(x) ifelse(x-1>0,MINPG2[x-1],0))) %>%
  ungroup()
playerdata <- dplyr::select(playerdata,-Test,-Test2)
playerdata <- playerdata %>%
  dplyr::group_by(PlayerID,Season) %>%
  arrange(PlayerID,Season,Date) %>%
  dplyr::mutate(MIN3 = cumsum(MIN),
         Games3 = as.numeric(cumsum(MIN>0)),
         MINPG3 = MIN3/Games3,
         MIN3 = sapply(1:n(), function(x) ifelse(x-1>0,MIN3[x-1],0)),
         Games3 = sapply(1:n(), function(x) ifelse(x-1>0,Games3[x-1],0)),
         MINPG3 = sapply(1:n(), function(x) ifelse(x-1>0,MINPG3[x-1],0))) %>%
  ungroup()
playerdata <- playerdata %>%
  dplyr::group_by(PlayerID,Season) %>%
  arrange(PlayerID,Season,Date) %>%
  dplyr::mutate(ImpactPct.pg = cumsum(ImpactPct),
         PlusMin = cumsum(`+/-`),
         TeamPlusMin = cumsum(Margin),
         MIN.xysum = cumsum(MIN.xy),
         ImpactPct.pg = sapply(1:n(), function(x) ifelse(x-1>0,ImpactPct.pg[x-1],NaN)),
         PlusMin = sapply(1:n(), function(x) ifelse(x-1>0,PlusMin[x-1],0)),
         TeamPlusMin = sapply(1:n(), function(x) ifelse(x-1>0,TeamPlusMin[x-1],0)),
         MIN.xysum = sapply(1:n(), function(x) ifelse(x-1>0,MIN.xysum[x-1],0))) %>%
  ungroup()
playerdata$OnOffGame <- (playerdata$PlusMin/playerdata$MIN3-(playerdata$TeamPlusMin-playerdata$PlusMin)/(playerdata$MIN.xysum-playerdata$MIN3))*36
playerdata$OnOffGame[is.nan(playerdata$OnOffGame)|is.infinite(playerdata$OnOffGame)] <- 0
playerdata$ImpactPct.pg <- playerdata$ImpactPct.pg/playerdata$Games3
playerdata$ImpactPct.pg[is.nan(playerdata$ImpactPct.pg)|is.infinite(playerdata$ImpactPct.pg)] <- 0.39
playerdata <- playerdata[order(playerdata$Season,playerdata$Team,playerdata$Date,playerdata$MIN),]
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(IsGame = as.numeric(sum(MIN)>0)/n())
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(SeasGames = cumsum(IsGame))
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(SeasGames = max(SeasGames))
playerdata$SeasGames[playerdata$Date==today&playerdata$GameID!=""] <- (playerdata$SeasGames[playerdata$Date==today&playerdata$GameID!=""]+1)
playerdata$IsGame[playerdata$Date==today] <- 1

playerdata$SeasGamesPREVWGT <- ifelse(playerdata$SeasGames<=20,(21-playerdata$SeasGames)/20,0)
playerdata$SeasGamesCURRWGT <- ifelse(playerdata$SeasGames<=20,(playerdata$SeasGames-1)/20,1)
playerdata$SeasGamesPREVWGT[playerdata$SeasGames==0] <- 1
playerdata$SeasGamesCURRWGT[playerdata$SeasGames==0] <- 0
playerdata <- playerdata %>% 
  dplyr::group_by(Season) %>% 
  dplyr::mutate(SeasNum = cur_group_id()) %>%
  ungroup()
playerdata$SeasNumLag <- playerdata$SeasNum-1


#fix first day game minutes pg averages
MINPGfixdata <- playerdata %>% 
  dplyr::group_by(Season) %>%
  dplyr::summarize(MaxDate = max(Date,na.rm = TRUE)) %>%
  ungroup()
MINPGfixdata$MaxDate[2:length(MINPGfixdata$MaxDate)] <- MINPGfixdata$MaxDate[1:(length(MINPGfixdata$MaxDate)-1)]
MINPGfixdata$MaxDate[1] <- NA
playerdata <- playerdata %>% 
  left_join(MINPGfixdata,by = c("Season")) %>%
  ungroup()
MINPGfixdata2 <- playerdata %>% 
  dplyr::group_by(Season) %>%
  dplyr::summarize(MaxDate2 = max(Date,na.rm = TRUE)) %>%
  ungroup()
MINPGfixdata2$MaxDate2[3:length(MINPGfixdata2$MaxDate2)] <- MINPGfixdata2$MaxDate2[1:(length(MINPGfixdata2$MaxDate2)-2)]
MINPGfixdata2$MaxDate2[1:2] <- NA
playerdata <- playerdata %>% 
  left_join(MINPGfixdata2,by = c("Season")) %>%
  ungroup()

playerdataTEMP <- dplyr::select(playerdata,PlayerID,Date,Team,MINPG3,ImpactPct.pg,OnOffGame)
playerdataTEMP <- dplyr::rename(playerdataTEMP,MaxDate = Date,MINPGPREV = MINPG3,ImpactPctPREV = ImpactPct.pg,OnOffGamePREV=OnOffGame)
playerdataTEMP <- distinct(playerdataTEMP,MaxDate,PlayerID,.keep_all = TRUE)
playerdataTEMP <- dplyr::select(playerdataTEMP,-Team)
playerdata <- playerdata %>% left_join(playerdataTEMP,by = c("MaxDate","PlayerID")) %>% ungroup()

playerdataTEMP2 <- dplyr::select(playerdata,PlayerID,Date,Team,MINPG3,ImpactPct.pg,OnOffGame)
playerdataTEMP2 <- dplyr::rename(playerdataTEMP2,MaxDate2 = Date,MINPGPREV2 = MINPG3,ImpactPctPREV2 = ImpactPct.pg,OnOffGamePREV2=OnOffGame)
playerdataTEMP2 <- distinct(playerdataTEMP2,MaxDate2,PlayerID,.keep_all = TRUE)
playerdataTEMP2 <- dplyr::select(playerdataTEMP2,-Team)
playerdata <- playerdata %>% left_join(playerdataTEMP2,by = c("MaxDate2","PlayerID")) %>% ungroup()

playerdata$MINPGPREV[is.na(playerdata$MINPGPREV)&playerdata$PlayerID!="TEAM"] <- playerdata$MINPGPREV2[is.na(playerdata$MINPGPREV)&playerdata$PlayerID!="TEAM"]
playerdata$MINPGPREV[is.na(playerdata$MINPGPREV)&playerdata$PlayerID!="TEAM"] <- 7
playerdata$ImpactPctPREV[is.na(playerdata$ImpactPctPREV)&playerdata$PlayerID!="TEAM"] <- playerdata$ImpactPctPREV2[is.na(playerdata$ImpactPctPREV)&playerdata$PlayerID!="TEAM"]
playerdata$ImpactPctPREV[is.na(playerdata$ImpactPctPREV)&playerdata$PlayerID!="TEAM"] <- .39
playerdata$OnOffGamePREV[is.na(playerdata$OnOffGamePREV)&playerdata$PlayerID!="TEAM"] <- playerdata$OnOffGamePREV2[is.na(playerdata$OnOffGamePREV)&playerdata$PlayerID!="TEAM"]
playerdata$OnOffGamePREV[is.na(playerdata$OnOffGamePREV)&playerdata$PlayerID!="TEAM"] <- -1

playerdata$ImpactPct.pg[playerdata$Games3<=0] <- playerdata$ImpactPctPREV[playerdata$Games3<=0]
playerdata$MINPG3[playerdata$Games3==0] <- playerdata$MINPGPREV[playerdata$Games3==0]
playerdata$OnOffGame[playerdata$Games3<=0] <- playerdata$OnOffGamePREV[playerdata$Games3<=0]


playerdata$GamesPREVWGT <- ifelse(playerdata$Games3<=5,(5-playerdata$Games3)/5,0)
playerdata$GamesCURRWGT <- ifelse(playerdata$Games3<=5,(playerdata$Games3)/5,1)
playerdata$GamesPREVWGT[playerdata$Games3==0] <- 1
playerdata$GamesCURRWGT[playerdata$Games3==0] <- 0
playerdata$MINPG3 <- playerdata$GamesPREVWGT*playerdata$MINPGPREV + playerdata$GamesCURRWGT*playerdata$MINPG3
playerdata$ImpactPct.pg <- playerdata$GamesPREVWGT*playerdata$ImpactPctPREV + playerdata$GamesCURRWGT*playerdata$ImpactPct.pg
playerdata$OnOffGame <- playerdata$GamesPREVWGT*playerdata$OnOffGamePREV + playerdata$GamesCURRWGT*playerdata$OnOffGame
playerdata$OnOffGame <- pmin(playerdata$OnOffGame,25) #take away outliers from small amts played
playerdata$OnOffGame <- pmax(playerdata$OnOffGame,-25) #take away outliers from small amts played
playerdata$GamesPREVWGT2 <- ifelse(playerdata$Games2<=5,(5-playerdata$Games2)/5,0)
playerdata$GamesCURRWGT2 <- ifelse(playerdata$Games2<=5,(playerdata$Games2)/5,1)
playerdata$GamesPREVWGT2[playerdata$Games2==0] <- 1
playerdata$GamesCURRWGT2[playerdata$Games2==0] <- 0
playerdata$MINPG2[playerdata$Games2==0] <- playerdata$MINPG3[playerdata$Games2==0]
playerdata$MINPG2[(playerdata$Games2==playerdata$Games3)&playerdata$Games2<=5] <- playerdata$MINPG3[(playerdata$Games2==playerdata$Games3)&playerdata$Games2<=5]
playerdata$MINPG2[(playerdata$Games2!=playerdata$Games3)&playerdata$Games2<=5&playerdata$Games2>0] <- playerdata$GamesPREVWGT2[(playerdata$Games2!=playerdata$Games3)&playerdata$Games2<=5&playerdata$Games2>0]*playerdata$MINPG3[(playerdata$Games2!=playerdata$Games3)&playerdata$Games2<=5&playerdata$Games2>0] + playerdata$GamesCURRWGT2[(playerdata$Games2!=playerdata$Games3)&playerdata$Games2<=5&playerdata$Games2>0]*playerdata$MINPG2[(playerdata$Games2!=playerdata$Games3)&playerdata$Games2<=5&playerdata$Games2>0]


vellplusmin$Date <- vellplusmin$Date+1
vellplusmin <- vellplusmin[,c("PlayerID","Date","VellPMOff_PaceAdj","VellPMDef_PaceAdj","VellPMOff_PaceAdjNorm","VellPMDef_PaceAdjNorm")]
vellplusmin$VellPM_PaceAdj <- vellplusmin$VellPMOff_PaceAdj + vellplusmin$VellPMDef_PaceAdj
vellplusmin$VellPM_PaceAdjNorm <- vellplusmin$VellPMOff_PaceAdjNorm + vellplusmin$VellPMDef_PaceAdjNorm
vellplusmin <- vellplusmin %>% 
  dplyr::group_by(PlayerID) %>%
  complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
  fill(VellPMOff_PaceAdj,VellPMDef_PaceAdj,VellPM_PaceAdj,VellPMDef_PaceAdjNorm,VellPM_PaceAdjNorm) %>%
  ungroup()
vellplusminADD <- vellplusmin[vellplusmin$Date==max(vellplusmin$Date),]
vellplusminADD$Date <- today
vellplusmin <- rbind(vellplusmin,vellplusminADD)
vellplusmin <- vellplusmin %>% 
  dplyr::group_by(PlayerID) %>%
  complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
  fill(VellPMOff_PaceAdj,VellPMDef_PaceAdj,VellPM_PaceAdj,VellPMDef_PaceAdjNorm,VellPM_PaceAdjNorm) %>%
  ungroup()
repl_values$Date <- repl_values$Date + 1
repl_values <- repl_values[,c("Date","Repl_offExp3","Repl_defExp3","Repl_offExp3Norm","Repl_defExp3Norm")]
repl_values <- repl_values %>% 
  complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
  fill(Repl_offExp3,Repl_defExp3,Repl_offExp3Norm,Repl_defExp3Norm)
repl_valuesADD <- repl_values[repl_values$Date==max(repl_values$Date),]
repl_valuesADD$Date <- today
repl_values <- rbind(repl_values,repl_valuesADD)
repl_values <- repl_values %>% 
  complete(Date = seq.Date(min(Date),max(Date),by="day")) %>%
  fill(Repl_offExp3,Repl_defExp3,Repl_offExp3Norm,Repl_defExp3Norm)
vellplusmin <- distinct(vellplusmin)
repl_values <- distinct(repl_values)
playerdata <- playerdata %>% left_join(vellplusmin,by=c("Date","PlayerID"))
playerdata <- playerdata %>% left_join(repl_values,by=c("Date"))
playerdata$VellPMOff_PaceAdj[is.na(playerdata$VellPMOff_PaceAdj)] <- playerdata$Repl_offExp3[is.na(playerdata$VellPMOff_PaceAdj)]
playerdata$VellPMDef_PaceAdj[is.na(playerdata$VellPMDef_PaceAdj)] <- playerdata$Repl_defExp3[is.na(playerdata$VellPMDef_PaceAdj)]
playerdata$VellPM_PaceAdj <- playerdata$VellPMOff_PaceAdj + playerdata$VellPMDef_PaceAdj
playerdata$VellPMOff_PaceAdjNorm[is.na(playerdata$VellPMOff_PaceAdjNorm)] <- playerdata$Repl_offExp3[is.na(playerdata$VellPMOff_PaceAdjNorm)]
playerdata$VellPMDef_PaceAdjNorm[is.na(playerdata$VellPMDef_PaceAdjNorm)] <- playerdata$Repl_defExp3[is.na(playerdata$VellPMDef_PaceAdjNorm)]
playerdata$VellPM_PaceAdjNorm <- playerdata$VellPMOff_PaceAdjNorm + playerdata$VellPMDef_PaceAdjNorm

#calc for playoff minutes
box_nopreseason$PlayoffMin <- 0
box_nopreseason$PlayoffGames <- 0
box_nopreseason$PlayoffMin[box_nopreseason$Playoffs.x==1] <- box_nopreseason$MIN[box_nopreseason$Playoffs.x==1]
box_nopreseason$PlayoffMin[is.na(box_nopreseason$PlayoffMin)] <- 0
box_nopreseason$PlayoffGames[box_nopreseason$Playoffs.x==1] <- 1
box_nopreseason$PlayoffGames[is.na(box_nopreseason$PlayoffMin)] <- 0
box_nopreseason <- box_nopreseason[order(box_nopreseason$PlayerID,box_nopreseason$PlayerID,box_nopreseason$Date),]
box_nopreseason <- box_nopreseason %>% 
  dplyr::group_by(PlayerID) %>%
  dplyr::mutate(PlayoffMin = cumsum(PlayoffMin),PlayoffGames = cumsum(PlayoffGames)) %>%
  ungroup()
box_nopreseason$PlayoffMinAvg <- ifelse(is.finite(box_nopreseason$PlayoffMin/box_nopreseason$PlayoffGames),box_nopreseason$PlayoffMin/box_nopreseason$PlayoffGames,0)
playoffmindata <- dplyr::select(box_nopreseason,PlayerID,Players,Date,PlayoffMin,PlayoffGames,PlayoffMinAvg,SeasNum.x)
playoffmindata <- distinct(playoffmindata,PlayerID,Date,.keep_all = TRUE)

playoffmindata <- playoffmindata %>% 
  dplyr::group_by(PlayerID) %>%
  dplyr::mutate(PlayoffMinMax = max(PlayoffMin,na.rm = TRUE),PlayoffGamesMax = max(PlayoffGames,na.rm = TRUE)) %>%
  ungroup()
playoffmindata2 <- playoffmindata[playoffmindata$SeasNum.x==max(playoffmindata$SeasNum.x),]
playoffmindata2 <- distinct(playoffmindata2,PlayerID,.keep_all = TRUE)
playoffmindata2 <- dplyr::select(playoffmindata2,PlayerID,Players,Date,PlayoffMinMax,PlayoffGamesMax)
playoffmindata2$Date <- today
playoffmindata2 <- dplyr::rename(playoffmindata2,PlayoffMin=PlayoffMinMax,PlayoffGames=PlayoffGamesMax)
playoffmindata2$PlayoffMinAvg <- ifelse(is.finite(playoffmindata2$PlayoffMin/playoffmindata2$PlayoffGames),playoffmindata2$PlayoffMin/playoffmindata2$PlayoffGames,0)
playoffmindata <- dplyr::select(box_nopreseason,PlayerID,Players,Date,PlayoffMin,PlayoffGames,PlayoffMinAvg)
playoffmindata <- rbind(playoffmindata,playoffmindata2)
save(playoffmindata,file="playoffmindata.RData")

playerdata$RPMScore <- playerdata$VellPM_PaceAdj
playerdata$ORPMScore <- playerdata$VellPMOff_PaceAdj
playerdata$DRPMScore <- playerdata$VellPMDef_PaceAdj
playerdata$RPMScoreNorm <- playerdata$VellPM_PaceAdjNorm
playerdata$ORPMScoreNorm <- playerdata$VellPMOff_PaceAdjNorm
playerdata$DRPMScoreNorm <- playerdata$VellPMDef_PaceAdjNorm


load("todaystable.RData")
table$PlayerID <- substr(table$elemplayercards,unlist(gregexpr("/id/",table$elemplayercards))+4,nchar(table$elemplayercards))
table$PlayerID <- substr(table$PlayerID,1,unlist(gregexpr("/",table$PlayerID))-1)
table <- dplyr::select(table,-elemplayercards)

######Manual Adjustments######
table$FinalStatus[table$NAME=="Malcolm Brogdon"&table$DATE=="May 25"] <- 1
table$FinalStatus[table$NAME=="Draymond Green"&table$DATE=="May 13"] <- 1
table$FinalStatus[table$NAME=="Gabe Vincent"&table$DATE=="May 25"] <- 1
# table$FinalStatus[table$NAME=="Nerlens Noel"&table$DATE=="Nov 29"] <- 0.5
# table$FinalStatus[table$NAME=="Nerlens Noel"&table$DATE=="Nov 29"] <- 0.5
# table$FinalStatus[table$NAME=="Nerlens Noel"&table$DATE=="Nov 29"] <- 0.5
# table$FinalStatus[table$NAME=="Nerlens Noel"&table$DATE=="Nov 29"] <- 0.5
# table$FinalStatus[table$NAME=="Scottie Barnes"&table$DATE=="Nov 22"] <- 0
# table$FinalStatus[table$NAME=="Kendall Brown"&table$DATE=="Nov 14"] <- 0
# table$FinalStatus[table$NAME=="Wendell Carter Jr."&table$DATE=="Nov 20"] <- 0.5
# table$FinalStatus[table$NAME=="Jakob Poeltl"&table$DATE=="Nov 20"] <- 1
# table$FinalStatus[table$NAME=="Romeo Langford"&table$DATE=="Nov 20"] <- 1
# table$FinalStatus[table$NAME=="Otto Porter Jr."&table$DATE=="Nov 19"] <- 0.5
# table$FinalStatus[table$NAME=="Chris Boucher"&table$DATE=="Nov 19"] <- 0.5
# table$FinalStatus[table$NAME=="Dalano Banton"&table$DATE=="Nov 19"] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Jimmy Butler"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 30"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "6430"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Mikal Bridges"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3147657"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Royce O'Neale"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "2583632"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Scottie Barnes"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4433134"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Gary Trent Jr."
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4277843"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Jakob Poeltl"
# table$POS[nrow(table)] <- "C"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3134908"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Bobby Portis"
# table$POS[nrow(table)] <- "C"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3064482"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Joe Ingles"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "2968436"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Jae Crowder"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "6581"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Patrick Williams"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4431687"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Ayo Dosunmu"
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4397002"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Coby White"
# table$POS[nrow(table)] <- "PG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4395651"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Josh Okogie"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4065663"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Eric Gordon"
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3431"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Nikola Vucevic"
# table$POS[nrow(table)] <- "C"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "6478"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Alex Caruso"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "2991350"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Nikola Jokic"
# table$POS[nrow(table)] <- "C"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3112335"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Kentavious Caldwell-Pope"
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "2581018"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Aaron Gordon"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3064290"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Jamal Murray"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3936299"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Michael Porter Jr."
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4278104"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Kevin Huerter"
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4066372"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "De'Aaron Fox"
# table$POS[nrow(table)] <- "PG"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4066259"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Damonatas Sabonis"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3155942"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Harrison Barnes"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 8"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "6578"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Cody Martin"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3138161"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "P.J. Washington"
# table$POS[nrow(table)] <- "PF"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4278078"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Dennis Smith Jr."
# table$POS[nrow(table)] <- "PG"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4065697"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 1
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 0
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0.5
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Kelly Oubre Jr."
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3133603"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Gordon Hayward"
# table$POS[nrow(table)] <- "SF"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "4249"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0
# table <- rbind(table,table[nrow(table),])
# table$NAME[nrow(table)] <- "Terry Rozier"
# table$POS[nrow(table)] <- "SG"
# table$DATE[nrow(table)] <- "Apr 3"
# table$STATUS[nrow(table)] <- "Day-To-Day"
# table$PlayerID[nrow(table)] <- "3074752"
# table$COMMENT[nrow(table)] <- NA
# table$questionable[nrow(table)] <- 0
# table$doubtful[nrow(table)] <- 0
# table$probable[nrow(table)] <- 0
# table$out[nrow(table)] <- 1
# table$Date[nrow(table)] <- today
# table$FinalStatus[nrow(table)] <- 0


playerdata <- playerdata %>% 
  left_join(table,by = c("PlayerID","Date")) %>%
  ungroup()

playerdata <- playerdata %>% left_join(distinct(distinct(box_nopreseason,PlayerID,Players),PlayerID,.keep_all=TRUE),by=c("PlayerID"))


playerdata$InclDummy1 <- as.numeric(playerdata$MIN>0)
playerdata$InclDummy1[playerdata$Date==today] <- 1
playerdata$InclDummy1[playerdata$Date==today&playerdata$FinalStatus==0] <- 0
playerdata$InclDummy1[playerdata$Date==today&playerdata$FinalStatus==0.5] <- 0.5

# #further manual adjustments for injuries
# playerdata$InclDummy1[playerdata$Players=="Z. LaVine"&playerdata$Team=="Chicago Bulls"&playerdata$Season=="2022-23"&playerdata$Date>="2022-10-22"] <- 0.5
# playerdata$InclDummy1[playerdata$Players=="V. Oladipo"&playerdata$Team=="Miami Heat"&playerdata$Season=="2021-22"&playerdata$Date>="2022-05-28"] <- 0.5
# playerdata$InclDummy1[playerdata$Players=="D. Robinson"&playerdata$Team=="Miami Heat"&playerdata$Season=="2021-22"&playerdata$Date>="2022-05-28"] <- 0.5


#trade adjustments for current season
playerdata$InclDummy1[playerdata$Players=="K. Nunn"&playerdata$Team=="Los Angeles Lakers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-01-23"] <- 0
playerdata$InclDummy1[playerdata$Players=="R. Hachimura"&playerdata$Team=="Washington Wizards"&playerdata$Season=="2022-23"&playerdata$Date>="2023-01-23"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Irving"&playerdata$Team=="Brooklyn Nets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-06"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Finney-Smith"&playerdata$Team=="Dallas Mavericks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-06"] <- 0
playerdata$InclDummy1[playerdata$Players=="S. Dinwiddie"&playerdata$Team=="Dallas Mavericks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-06"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Morris"&playerdata$Team=="Brooklyn Nets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-06"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Hart"&playerdata$Team=="Portland Trail Blazers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="C. Reddish"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="R. Arcidiacono"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="S. Mykhailiuk"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Russell"&playerdata$Team=="Minnesota Timberwolves"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Beasley"&playerdata$Team=="Utah Jazz"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Vanderbilt"&playerdata$Team=="Utah Jazz"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="R. Westbrook"&playerdata$Team=="Los Angeles Lakers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Conley"&playerdata$Team=="Utah Jazz"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Dedmon"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Durant"&playerdata$Team=="Brooklyn Nets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Bridges"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="C. Johnson"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Crowder"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="T.J. Warren"&playerdata$Team=="Brooklyn Nets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Poeltl"&playerdata$Team=="San Antonio Spurs"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Birch"&playerdata$Team=="Toronto Raptors"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="G. Payton II"&playerdata$Team=="Portland Trail Blazers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Knox II"&playerdata$Team=="Detroit Pistons"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Plumlee"&playerdata$Team=="Charlotte Hornets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="R. Jackson"&playerdata$Team=="LA Clippers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Graham"&playerdata$Team=="New Orleans Pelicans"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Richardson"&playerdata$Team=="San Antonio Spurs"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Wiseman"&playerdata$Team=="Golden State Warriors"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Bamba"&playerdata$Team=="Orlando Magic"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="P. Beverley"&playerdata$Team=="Los Angeles Lakers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="E. Gordon"&playerdata$Team=="Houston Rockets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Green"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="L. Kennard"&playerdata$Team=="LA Clippers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Holiday"&playerdata$Team=="Atlanta Hawks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="F. Kaminsky"&playerdata$Team=="Atlanta Hawks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="G. Mathews"&playerdata$Team=="Houston Rockets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="B. Fernando"&playerdata$Team=="Houston Rockets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="S. Bey"&playerdata$Team=="Detroit Pistons"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="B. Hyland"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="G. Hill"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Thybulle"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. McDaniels"&playerdata$Team=="Charlotte Hornets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Crowder"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="T. Bryant"&playerdata$Team=="Los Angeles Lakers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Reed"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="M. Muscala"&playerdata$Team=="Oklahoma City Thunder"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Bazley"&playerdata$Team=="Oklahoma City Thunder"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="D. Saric"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-09"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Love"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-17"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Nwora"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-17"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Toscano-Anderson"&playerdata$Team=="Los Angeles Lakers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-02-17"] <- 0
playerdata$InclDummy1[playerdata$Players=="G. Dragic"&playerdata$Team=="Chicago Bulls"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="W. Barton"&playerdata$Team=="Washington Wizards"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="R.J. Hampton"&playerdata$Team=="Orlando Magic"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Wall"&playerdata$Team=="LA Clippers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="S. Mamukelashvili"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="I. Joe"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Walker"&playerdata$Team=="Dallas Mavericks"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="K. Bazemore"&playerdata$Team=="Sacramento Kings"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Primo"&playerdata$Team=="San Antonio Spurs"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
#long-term injury that failed to pop up on espn injuries
playerdata$InclDummy1[playerdata$Players=="L. Ball"&playerdata$Team=="Charlotte Hornets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="W. Barton"&playerdata$Team=="Washington Wizards"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-20"] <- 0
playerdata$InclDummy1[playerdata$Players=="C. Martin"&playerdata$Team=="Charlotte Hornets"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Champagnie"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0
playerdata$InclDummy1[playerdata$Players=="J. Champagnie"&playerdata$Team=="Toronto Raptors"&playerdata$Season=="2022-23"&playerdata$Date>="2023-03-05"] <- 0

# playerdata$InclDummy1[playerdata$Players=="E. Bledsoe"&playerdata$Team=="LA Clippers"&playerdata$Season=="2021-22"&playerdata$Date>="2022-02-04"] <- 0
# playerdata$InclDummy1[playerdata$Players=="J. Winslow"&playerdata$Team=="LA Clippers"&playerdata$Season=="2021-22"&playerdata$Date>="2022-02-04"] <- 0

playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season) %>%
  dplyr::mutate(SeasMin = cumsum(MIN)/5)
playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season,Date) %>%
  dplyr::mutate(SeasMin = max(SeasMin)-sum(MIN/5))
playerdata$GameShare <- ifelse(is.nan(playerdata$Games3/playerdata$SeasGames)|is.infinite(playerdata$Games3/playerdata$SeasGames),1,playerdata$Games3/playerdata$SeasGames)
playerdata$GameShare <- ifelse(playerdata$SeasGames<=1,1,playerdata$GameShare)
playerdata$PlaysEnough <- ifelse(playerdata$MINPG2>=8&playerdata$GameShare>=.20,1,0)

playerdata$MINPGAdj <- NA
playerdata$MINPGAdj[((((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7))&playerdata$InclDummy1==1))&!is.na(playerdata$MINPG2)] <- playerdata$MINPG2[((((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7))&playerdata$InclDummy1==1))&!is.na(playerdata$MINPG2)]
playerdata$MINPGAdj[((((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7))&playerdata$InclDummy1==0.5))&!is.na(playerdata$MINPG2)&playerdata$Date==today] <- 0.5*playerdata$MINPG2[((((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7))&playerdata$InclDummy1==0.5))&!is.na(playerdata$MINPG2)&playerdata$Date==today]
playerdata$MINPGAdj[!is.na(playerdata$MINPGAdj)&playerdata$MINPGAdj>=25&!is.na(playerdata$Playoffs.x==1)&playerdata$Playoffs.x==1] <- playerdata$MINPGAdj[!is.na(playerdata$MINPGAdj)&playerdata$MINPGAdj>=25&!is.na(playerdata$Playoffs.x==1)&playerdata$Playoffs.x==1]+3.5
######Manual Adjustments for Minutes###### (eg, if a player was added recently and plays a lot and I want to include)
##MinWgt Manual Adjustment: idea is to make certain players have enoug games share and MPG to make it into calculation below
# playerdata$MINPGAdj[playerdata$Players=="K. Irving"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"] <- 34.5
# playerdata$PlaysEnough[playerdata$Players=="K. Irving"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"] <- 1
# playerdata$GameShare[playerdata$Players=="K. Irving"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"] <- 0.55
# playerdata$MINPGAdj[playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"&playerdata$Date>="2022-02-09"] <- 35
# playerdata$PlaysEnough[playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"&playerdata$Date>="2022-02-09"] <- 1
# playerdata$GameShare[playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"&playerdata$Date>="2022-02-09"] <- 0.55



playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  dplyr::mutate(MinEnough= sum(MINPG2[PlaysEnough==1],na.rm = TRUE),MinOther = sum(MINPG2[PlaysEnough==0],na.rm = TRUE),
         MinTotal = 48*5, MinAll = sum(MINPG2,na.rm = TRUE),MinEnoughAdj = sum(MINPGAdj[PlaysEnough==1],na.rm = TRUE),
         MinTotalRestAdj = sum(MIN2[PlaysEnough==1&InclDummy1>0&!((MINPG2>=20&GameShare>0.5)|(MINPG2>=25&Games3>=15)|(MINPG2>=12&GameShare>0.7))],na.rm = TRUE))

playerdata <- playerdata %>%
  dplyr::mutate(MinRest = max(MinTotal - MinEnoughAdj,0), MinOtherAdj = MinAll - MinEnoughAdj)

playerdata$MINPG2b <- (playerdata$MIN2/playerdata$MinTotalRestAdj)*playerdata$MinRest
playerdata$ID <- NA
playerdata$ID[playerdata$PlaysEnough==1&playerdata$InclDummy1==1&!(((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7)))&!is.na(playerdata$MINPG2)] <- 1
playerdata$MINPGAdj[playerdata$PlaysEnough==1&playerdata$InclDummy1==1&!(((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7)))&!is.na(playerdata$MINPG2)] <- pmin(playerdata$MINPG2b[playerdata$PlaysEnough==1&playerdata$InclDummy1==1&!(((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7)))&!is.na(playerdata$MINPG2)],25)
playerdata$MINPGAdj[playerdata$PlaysEnough==1&playerdata$InclDummy1==0.5&!(((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7)))&!is.na(playerdata$MINPG2)&playerdata$Date==today] <- pmin(0.5*playerdata$MINPG2b[playerdata$PlaysEnough==1&playerdata$InclDummy1==0.5&!(((playerdata$MINPG2>=20&playerdata$GameShare>0.5)|(playerdata$MINPG2>=25&playerdata$Games3>=15)|(playerdata$MINPG2>=12&playerdata$GameShare>0.7)))&!is.na(playerdata$MINPG2)&playerdata$Date==today],25)

playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  dplyr::mutate(MinEnoughAdj2= sum(MINPGAdj,na.rm = TRUE),CountRest= sum(ifelse(InclDummy1>0&PlaysEnough==0,1,0),na.rm = TRUE))

playerdata$AllocateRest <- pmax(240-playerdata$MinEnoughAdj2,0)/playerdata$CountRest

playerdata$MINPGAdj[playerdata$PlaysEnough==0&playerdata$InclDummy1>0&!is.na(playerdata$MINPG2)] <- playerdata$AllocateRest[playerdata$PlaysEnough==0&playerdata$InclDummy1>0&!is.na(playerdata$MINPG2)]
playerdata$MINPGAdj[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]
playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  dplyr::mutate(MinEnoughAdjFinal= sum(MINPGAdj,na.rm = TRUE))
playerdata$MINPGAdj[is.na(playerdata$MINPGAdj)] <- 0

playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(MINPGAdjSum = sum(MINPGAdj),
         MINPGAdjSumMore20 = sum(MINPGAdj[MINPGAdj>=20]),
         MINPGAdjSumLess20 = sum(MINPGAdj[MINPGAdj<20]),
         MINPGAdjDiff = max(240-MINPGAdjSumMore20,0))

playerdata$MINPGAdj[playerdata$MINPGAdj<20] <- (playerdata$MINPGAdj[playerdata$MINPGAdj<20]/playerdata$MINPGAdjSumLess20[playerdata$MINPGAdj<20])*playerdata$MINPGAdjDiff[playerdata$MINPGAdj<20]

playerdata$DummyMin33 <- as.numeric(playerdata$MINPG2>33)
playerdata$DummyMin30 <- as.numeric(playerdata$MINPG2>30)
playerdata$DummyMin25 <- as.numeric(playerdata$MINPG2>25)
playerdata$DummyMin20 <- as.numeric(playerdata$MINPG2>20)
playerdata$DummyMin15 <- as.numeric(playerdata$MINPG2>15)
playerdata$DummyMin10 <- as.numeric(playerdata$MINPG2>10)
playerdata$DummySeasGames10 <- as.numeric(playerdata$SeasGames<=10)
playerdata <- playerdata %>%
  dplyr::group_by(PlayerID,Season,Team) %>%
  arrange(PlayerID,Season,Team,Date) %>%
  dplyr::mutate(SeasGames7 = SeasGames-dplyr::lag(SeasGames,n=7,default=0),
         SeasGames14 = SeasGames-dplyr::lag(SeasGames,n=14,default=0),
         SeasGames30 = SeasGames-dplyr::lag(SeasGames,n=30,default=0),
         MIN7 = MIN3-dplyr::lag(MIN3,n=7,default=0),
         MIN14 = MIN3-dplyr::lag(MIN3,n=14,default=0),
         MIN30 = MIN3-dplyr::lag(MIN3,n=30,default=0),
         SeasMin7 = SeasMin-dplyr::lag(SeasMin,n=7,default=0),
         SeasMin14 = SeasMin-dplyr::lag(SeasMin,n=14,default=0),
         SeasMin30 = SeasMin-dplyr::lag(SeasMin,n=30,default=0),
         RowNum = row_number()) %>%
  ungroup()
playerdata$SeasGames7[playerdata$RowNum<=7] <- playerdata$SeasGames[playerdata$RowNum<=7]
playerdata$SeasGames14[playerdata$RowNum<=14] <- playerdata$SeasGames[playerdata$RowNum<=14]
playerdata$SeasGames30[playerdata$RowNum<=30] <- playerdata$SeasGames[playerdata$RowNum<=30]
playerdata$MIN7[playerdata$RowNum<=7] <- playerdata$MIN3[playerdata$RowNum<=7]
playerdata$MIN14[playerdata$RowNum<=14] <- playerdata$MIN3[playerdata$RowNum<=14]
playerdata$MIN30[playerdata$RowNum<=30] <- playerdata$MIN3[playerdata$RowNum<=30]
playerdata$SeasMin7[playerdata$RowNum<=7] <- playerdata$SeasMin[playerdata$RowNum<=7]
playerdata$SeasMin14[playerdata$RowNum<=14] <- playerdata$SeasMin[playerdata$RowNum<=14]
playerdata$SeasMin30[playerdata$RowNum<=30] <- playerdata$SeasMin[playerdata$RowNum<=30]

playerdata <- playerdata %>%
  dplyr::mutate(MINPG7 = MIN7/SeasGames7,
         MINPG14 = MIN14/SeasGames14,
         MINPG30 = MIN30/SeasGames30,
         SeasShr7 = MIN7/SeasMin7,
         SeasShr14 = MIN14/SeasMin14,
         SeasShr30 = MIN30/SeasMin30)
playerdata$MIN7[is.nan(playerdata$MINPG7)|is.infinite(playerdata$MINPG7)] <- playerdata$MINPG2[is.nan(playerdata$MINPG7)|is.infinite(playerdata$MINPG7)]*3
playerdata$MIN14[is.nan(playerdata$MINPG14)|is.infinite(playerdata$MINPG14)] <- playerdata$MINPG2[is.nan(playerdata$MINPG14)|is.infinite(playerdata$MINPG14)]*6
playerdata$MIN30[is.nan(playerdata$MINPG30)|is.infinite(playerdata$MINPG30)] <- playerdata$MINPG2[is.nan(playerdata$MINPG30)|is.infinite(playerdata$MINPG30)]*12
playerdata$MINPG7[is.nan(playerdata$MINPG7)|is.infinite(playerdata$MINPG7)] <- playerdata$MINPG2[is.nan(playerdata$MINPG7)|is.infinite(playerdata$MINPG7)]
playerdata$MINPG14[is.nan(playerdata$MINPG14)|is.infinite(playerdata$MINPG14)] <- playerdata$MINPG2[is.nan(playerdata$MINPG14)|is.infinite(playerdata$MINPG14)]
playerdata$MINPG30[is.nan(playerdata$MINPG30)|is.infinite(playerdata$MINPG30)] <- playerdata$MINPG2[is.nan(playerdata$MINPG30)|is.infinite(playerdata$MINPG30)]
playerdata$SeasShr <- playerdata$MIN3/playerdata$SeasMin
playerdata$SeasShr[is.nan(playerdata$SeasShr)|is.infinite(playerdata$SeasShr)] <- playerdata$MINPG2[is.nan(playerdata$SeasShr)|is.infinite(playerdata$SeasShr)]/48
playerdata$SeasShr7[is.nan(playerdata$SeasShr7)|is.infinite(playerdata$SeasShr7)] <- playerdata$SeasShr[is.nan(playerdata$SeasShr7)|is.infinite(playerdata$SeasShr7)]
playerdata$SeasShr14[is.nan(playerdata$SeasShr14)|is.infinite(playerdata$SeasShr14)] <- playerdata$SeasShr[is.nan(playerdata$SeasShr14)|is.infinite(playerdata$SeasShr14)]
playerdata$SeasShr30[is.nan(playerdata$SeasShr30)|is.infinite(playerdata$SeasShr30)] <- playerdata$SeasShr[is.nan(playerdata$SeasShr30)|is.infinite(playerdata$SeasShr30)]
playerdata$MIN7[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]*3
playerdata$MIN14[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]*6
playerdata$MIN30[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]*12
playerdata$MINPG7[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]
playerdata$MINPG14[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]
playerdata$MINPG30[playerdata$SeasGames<=1] <- playerdata$MINPG2[playerdata$SeasGames<=1]

# playerdata$SeasGames[playerdata$Date==today] <- 1


model1 <-lm(MIN ~ DummyMin33 + DummyMin30 + DummyMin25 + DummyMin20 + DummyMin15 + DummyMin10 + 
              PlaysEnough + GameShare + Games3 + MINPG2 + DummyMin33*MINPG2 + DummyMin30*MINPG2 + 
              DummyMin25*MINPG2 + DummyMin20*MINPG2 + DummyMin15*MINPG2 + DummyMin10*MINPG2 + 
              PlaysEnough*MINPG2 + GameShare*MINPG2 + SeasGames + GameShare*MINPG2*SeasGames + GameShare*SeasGames + 
              Games3*MINPG2+Playoffs.x+Playoffs.x*MINPG2+
              Playoffs.x*MINPG2*GameShare+Playoffs.x*DummyMin15+Playoffs.x*DummyMin20+
              Playoffs.x*DummyMin25+Playoffs.x*DummyMin30 +Playoffs.x*DummyMin33 +DummySeasGames10 + 
              DummyMin33*DummySeasGames10 + DummyMin30*DummySeasGames10 + DummyMin25*DummySeasGames10 + 
              DummyMin20*DummySeasGames10 + DummyMin15*DummySeasGames10 + DummyMin10*DummySeasGames10 + 
              PlaysEnough*DummySeasGames10 + GameShare*DummySeasGames10 + Games3*DummySeasGames10 + 
              MINPG2*DummySeasGames10 + DummyMin33*MINPG2*DummySeasGames10 + DummyMin30*MINPG2*DummySeasGames10 + 
              DummyMin25*MINPG2*DummySeasGames10 + DummyMin20*MINPG2*DummySeasGames10 + 
              DummyMin15*MINPG2*DummySeasGames10 + DummyMin10*MINPG2*DummySeasGames10 + 
              PlaysEnough*MINPG2*DummySeasGames10 + GameShare*MINPG2*DummySeasGames10 + 
              Games3*MINPG2*DummySeasGames10 +Playoffs.x*DummySeasGames10 +
              Playoffs.x*MINPG2*DummySeasGames10 +Playoffs.x*MINPG2*GameShare*DummySeasGames10 +
              Playoffs.x*DummyMin15*DummySeasGames10 +Playoffs.x*DummyMin20*DummySeasGames10 +
              Playoffs.x*DummyMin25*DummySeasGames10 +
              Playoffs.x*DummyMin30*DummySeasGames10 + Playoffs.x*DummyMin33*DummySeasGames10 +
              DummyMin33*PlaysEnough + DummyMin30*PlaysEnough + DummyMin25*PlaysEnough + 
              DummyMin20*PlaysEnough + DummyMin15*PlaysEnough + DummyMin10*PlaysEnough + 
              PlaysEnough*PlaysEnough + GameShare*PlaysEnough + Games3*PlaysEnough + 
              MINPG2*PlaysEnough + DummyMin33*MINPG2*PlaysEnough + DummyMin30*MINPG2*PlaysEnough + 
              DummyMin25*MINPG2*PlaysEnough + DummyMin20*MINPG2*PlaysEnough + 
              DummyMin15*MINPG2*PlaysEnough + DummyMin10*MINPG2*PlaysEnough + 
              PlaysEnough*MINPG2*PlaysEnough + GameShare*MINPG2*PlaysEnough + 
              Games3*MINPG2*PlaysEnough +Playoffs.x*PlaysEnough +
              Playoffs.x*MINPG2*PlaysEnough +Playoffs.x*MINPG2*GameShare*PlaysEnough +
              Playoffs.x*DummyMin15*PlaysEnough +Playoffs.x*DummyMin20*PlaysEnough +
              Playoffs.x*DummyMin25*PlaysEnough +
              Playoffs.x*DummyMin30*PlaysEnough + 
              Playoffs.x*DummyMin33*PlaysEnough + MINPG7 + MINPG14 + MINPG30 +
              MIN7 + MIN14 + MIN30 + SeasShr7 + SeasShr14 + SeasShr30 +
              SeasShr + MINPG7*MINPG2 + MINPG14*MINPG2 + MINPG30*MINPG2 +
              MIN7*MINPG2 + MIN14*MINPG2 + MIN30*MINPG2 + SeasShr7*MINPG2 + SeasShr14*MINPG2 + SeasShr30*MINPG2 +
              SeasShr*MINPG2,data=playerdata[playerdata$InclDummy1==1,] )
playerdata$ProjMIN <- predict(model1,playerdata)
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Holiday"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Holiday"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="B. Lopez"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(31,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="B. Lopez"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="G. Antetokounmpo"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="G. Antetokounmpo"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Middleton"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(28,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Middleton"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="N. Jokic"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="N. Jokic"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Murray"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Murray"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="A. Gordon"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"] <- pmax(32,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="A. Gordon"&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="M. Porter Jr."&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"] <- pmax(32,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="M. Porter Jr."&playerdata$Team=="Denver Nuggets"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Durant"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Durant"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Booker"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Booker"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Ayton"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"] <- pmax(33,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Ayton"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="C. Paul"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"] <- pmax(33,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="C. Paul"&playerdata$Team=="Phoenix Suns"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Morant"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"] <- pmax(33,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Morant"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Bane"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Bane"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Brooks"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"] <- pmax(33,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Brooks"&playerdata$Team=="Memphis Grizzlies"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Butler"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Butler"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Lowry"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"] <- pmax(25,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="K. Lowry"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="B. Adebayo"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="B. Adebayo"&playerdata$Team=="Miami Heat"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Mitchell"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Mitchell"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Garland"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Garland"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="E. Mobley"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="E. Mobley"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Allen"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Allen"&playerdata$Team=="Cleveland Cavaliers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Brunson"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Brunson"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Randle"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Randle"&playerdata$Team=="New York Knicks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Harden"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Harden"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Embiid"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"] <- pmax(35,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Embiid"&playerdata$Team=="Philadelphia 76ers"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Tatum"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Tatum"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Brown"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="J. Brown"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="A. Horford"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"] <- pmax(31,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="A. Horford"&playerdata$Team=="Boston Celtics"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Fox"&playerdata$Team=="Sacramento Kings"&playerdata$Season=="2022-23"] <- pmax(36,playerdata$ProjMIN[playerdata$Date>="2023-04-10"&playerdata$Players=="D. Fox"&playerdata$Team=="Sacramento Kings"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="B. Portis"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(24,playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="B. Portis"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="P. Connaughton"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(16,playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="P. Connaughton"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="T. Antetokounmpo"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- 0
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="G. Allen"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- pmax(22,playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="G. Allen"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"])
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="L. Wigginton"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- 0
playerdata$ProjMIN[(playerdata$Date=="2023-04-15"|playerdata$Date=="2023-04-16")&playerdata$Players=="M. Beauchamp"&playerdata$Team=="Milwaukee Bucks"&playerdata$Season=="2022-23"] <- 8

playerdata$ProjMIN2 <- playerdata$ProjMIN
playerdata$ProjMIN[playerdata$InclDummy1==0] <- 0
playerdata$ProjMIN_orig <- playerdata$ProjMIN
playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN>=20] <- 0.5*playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN>=20]
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(ProjMINSum = sum(ProjMIN,na.rm=TRUE),
         ProjMINSumMore20 = sum(ProjMIN[ProjMIN_orig>=20],na.rm=TRUE),
         ProjMINSum1520 = sum(ProjMIN_orig[ProjMIN_orig<20&ProjMIN_orig>=15],na.rm=TRUE),
         ProjMINDiff1 = min(max(240-ProjMINSumMore20,0),ProjMINSum1520))
playerdata <- playerdata[playerdata$Date<="2023-06-12",]
playerdata$ProjMIN[playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15] <- (playerdata$ProjMIN[playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15]/playerdata$ProjMINSum1520[playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15])*playerdata$ProjMINDiff1[playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15]
playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15] <- 0.5*playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<20&playerdata$ProjMIN_orig>=15]

playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(ProjMINSum1520v2 = sum(ProjMIN[ProjMIN_orig<20&ProjMIN_orig>=15],na.rm=TRUE),
         ProjMINSum815 = sum(ProjMIN[ProjMIN_orig<15&ProjMIN_orig>=8],na.rm=TRUE),
         ProjMINDiff2 = min(max(240-ProjMINSumMore20-ProjMINSum1520v2,0),ProjMINSum815))
playerdata$ProjMIN[playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8] <- (playerdata$ProjMIN[playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8]/playerdata$ProjMINSum815[playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8])*playerdata$ProjMINDiff2[playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8]
playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8] <- 0.5*playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<15&playerdata$ProjMIN_orig>=8]

playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(ProjMINSum815v2 = sum(ProjMIN[ProjMIN_orig<15&ProjMIN_orig>=8],na.rm=TRUE),
         ProjMINSumLess8 = sum(ProjMIN[ProjMIN_orig<8&ProjMIN_orig>0],na.rm=TRUE),
         ProjMINDiff3 = min(max(240-ProjMINSumMore20-ProjMINSum1520v2-ProjMINSum1520v2-ProjMINSum815v2,0),ProjMINSumLess8))
playerdata$ProjMIN[playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0] <- (playerdata$ProjMIN[playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0]/playerdata$ProjMINSumLess8[playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0])*playerdata$ProjMINDiff3[playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0]
playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0] <- 0.5*playerdata$ProjMIN[playerdata$InclDummy1==0.5&playerdata$ProjMIN_orig<8&playerdata$ProjMIN_orig>0]
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(ProjMinTotal = sum(ProjMIN,na.rm=TRUE))
playerdata$ProjMIN[playerdata$ProjMIN<0] <- 0
playerdata$ProjMINFinal <- (playerdata$ProjMIN/playerdata$ProjMinTotal)*240
playerdata <- playerdata %>%
  dplyr::group_by(Team,Season,Date) %>%
  dplyr::mutate(ProjMinSumFinal = sum(ProjMINFinal,na.rm=TRUE))
playerdataTODAY <- playerdata[playerdata$Date==max(playerdata$Date),]
playerdataTODAY <- dplyr::select(playerdataTODAY,Date,Season,PlayerID,Players,Team,MIN,GameID,MIN2,Games2,MINPG2,MIN3,Games3,MINPG3,ProjMINFinal,ProjMinSumFinal,ProjMIN,Playoffs.x,ProjMIN_orig,RPMScoreNorm,ORPMScoreNorm,DRPMScoreNorm,RPMScore,ORPMScore,DRPMScore,InclDummy1,GameShare,PlaysEnough,SeasMin7,MIN7,MINPG7,SeasShr,SeasShr7,MIN14,MIN30,MINPG14,MINPG30,DummyMin10,DummyMin20,DummyMin33,DummySeasGames10,SeasGames,PlaysEnough)

######FINAL Manual Adjustments for Minutes###### (eg, for first game of season or when things are wrong)
# playerdata$ProjMINFinal[playerdata$Players=="D. Sabonis"&playerdata$Team=="Sacramento Kings"&playerdata$InclDummy1==1&playerdata$Season=="2021-22"&playerdata$Date==today] <- 35


playerdata$MinWgt <- (playerdata$MIN2)/(playerdata$SeasMin)
playerdata$MinWgtInjAdj <- (playerdata$ProjMINFinal)/(playerdata$ProjMinSumFinal/5)
playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season,Date) %>%
  dplyr::mutate(TotalWgt = sum(ifelse(PlayerID!="TEAM",MinWgt,0),na.rm = TRUE),TotalWgtInjAdj = sum(ifelse(PlayerID!="TEAM",MinWgtInjAdj,0),na.rm = TRUE))
playerdata$MinWgt[playerdata$PlayerID=="TEAM"] <- NA
playerdata$MinWgtInjAdj[playerdata$PlayerID=="TEAM"] <- NA


#for all past dates, make the player weights exact
playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season,Date) %>%
  dplyr::mutate(TotalWgt = sum(ifelse(PlayerID!="TEAM",MinWgt,0),na.rm = TRUE),TotalWgtInjAdj = sum(ifelse(PlayerID!="TEAM",MinWgtInjAdj,0),na.rm = TRUE))

playerdata <- distinct(playerdata,PlayerID,Team,Date,.keep_all=TRUE)
#Missing Superstars and starters
playerdata$EnoughMinutes <- ifelse((playerdata$MINPG3*playerdata$Games3)/(playerdata$SeasGames*48)>.25,1,0)
playerdata <- playerdata %>% 
  dplyr::group_by(Season, Date) %>%
  dplyr::mutate(RPMMean = mean(RPMScore[EnoughMinutes==1],na.rm = TRUE), 
         RPMSD = sd(RPMScore[EnoughMinutes==1],na.rm = TRUE),
         RPMMean2 = mean(RPMScore[ProjMINFinal>10],na.rm = TRUE), 
         RPMSD2 = sd(RPMScore[ProjMINFinal>10],na.rm = TRUE)) %>%
  ungroup()
playerdata$zscore <- (playerdata$RPMScore-playerdata$RPMMean)/playerdata$RPMSD
# playerdata$zscore[playerdata$EnoughMinutes==0] <- -0.75
# playerdata$zscore[playerdata$SeasGames==1&playerdata$MINPGAdj>10] <- (playerdata$BoxPlusMin1[playerdata$SeasGames==1&playerdata$MINPGAdj>10]-playerdata$BoxPlusMin1Mean2[playerdata$SeasGames==1&playerdata$MINPGAdj>10])/playerdata$BoxPlusMin1SD2[playerdata$SeasGames==1&playerdata$MINPGAdj>10]
playerdata$superstar <- ifelse(playerdata$zscore>=1.75&playerdata$MINPG3>25,1,0)
playerdata$star <- ifelse(playerdata$zscore>=0.9&playerdata$MINPG3>20,1,0)
playerdata$solid <- ifelse(playerdata$zscore>=(-.25)&playerdata$MINPG3>15,1,0)
#manual adjustments
# playerdata$superstar[playerdata$Players=="L. Doncic"&playerdata$Date>="2022-04-01"] <- 1
# playerdata$star[playerdata$Players=="L. Doncic"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$solid[playerdata$Players=="L. Doncic"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$superstar[playerdata$Players=="J. Tatum"&playerdata$Date>="2022-04-01"&playerdata$Team=="Boston Celtics"] <- 1
# playerdata$star[playerdata$Players=="J. Tatum"&playerdata$Date>="2022-04-01"&playerdata$Team=="Boston Celtics"] <- 0
# playerdata$solid[playerdata$Players=="J. Tatum"&playerdata$Date>="2022-04-01"&playerdata$Team=="Boston Celtics"] <- 0
# playerdata$superstar[playerdata$Players=="J. Embiid"&playerdata$Date>="2022-04-01"] <- 1
# playerdata$star[playerdata$Players=="J. Embiid"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$solid[playerdata$Players=="J. Embiid"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$superstar[playerdata$Players=="K. Durant"&playerdata$Date>="2022-04-01"] <- 1
# playerdata$star[playerdata$Players=="K. Durant"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$solid[playerdata$Players=="K. Durant"&playerdata$Date>="2022-04-01"] <- 0
# playerdata$solid[playerdata$superstar==1&playerdata$MINPG<25] <- 1
# playerdata$solid[playerdata$star==1&playerdata$MINPG<18] <- 1
# playerdata$superstar[playerdata$superstar==1&playerdata$MINPG<25] <- 0
# playerdata$star[playerdata$star==1&playerdata$MINPG<18] <- 0
playerdata$supergames <- playerdata$Games2*playerdata$superstar
playerdata$stargames <- playerdata$Games2*playerdata$star
playerdata$solidgames <-playerdata$Games2*playerdata$solid
playerdata$supernow <- playerdata$InclDummy1*playerdata$superstar
playerdata$starnow <- playerdata$InclDummy1*playerdata$star
playerdata$solidnow <-playerdata$InclDummy1*playerdata$solid
playerdata$superseasgames <- (playerdata$SeasGames-1)*playerdata$superstar
playerdata$starseasgames <- (playerdata$SeasGames-1)*playerdata$star
playerdata$solidseasgames <- (playerdata$SeasGames-1)*playerdata$solid
playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  dplyr::mutate(superplayed = sum(supergames,na.rm = TRUE), supertotal = sum(superseasgames,na.rm = TRUE),
         starplayed = sum(stargames,na.rm = TRUE), startotal = sum(starseasgames,na.rm = TRUE),
         solidplayed = sum(solidgames,na.rm = TRUE), solidtotal = sum(solidseasgames,na.rm = TRUE),
         supernowsum = sum(supernow,na.rm = TRUE), superallsum = sum(superstar,na.rm = TRUE),
         starnowsum = sum(starnow,na.rm = TRUE), starallsum = sum(star,na.rm = TRUE),
         solidnowsum = sum(solidnow,na.rm = TRUE), solidallsum = sum(solid,na.rm = TRUE)) %>%
  ungroup()


playerdata$superadj <- ifelse(is.infinite(playerdata$supernowsum/playerdata$superallsum - playerdata$superplayed/playerdata$supertotal)|is.na(playerdata$supernowsum/playerdata$superallsum - playerdata$superplayed/playerdata$supertotal),0,playerdata$supernowsum/playerdata$superallsum - playerdata$superplayed/playerdata$supertotal)
playerdata$staradj <- ifelse(is.infinite(playerdata$starnowsum/playerdata$starallsum - playerdata$starplayed/playerdata$startotal)|is.na(playerdata$starnowsum/playerdata$starallsum - playerdata$starplayed/playerdata$startotal),0,playerdata$starnowsum/playerdata$starallsum - playerdata$starplayed/playerdata$startotal)
playerdata$solidadj <- ifelse(is.infinite(playerdata$solidnowsum/playerdata$solidallsum - playerdata$solidplayed/playerdata$solidtotal)|is.na(playerdata$solidnowsum/playerdata$solidallsum - playerdata$solidplayed/playerdata$solidtotal),0,playerdata$solidnowsum/playerdata$solidallsum - playerdata$solidplayed/playerdata$solidtotal)


#Now create team weighted averages
playerdata$MinWgtXOnOff <- playerdata$MinWgt*playerdata$OnOffGame
playerdata$MinWgtXOnOffXUsg <- playerdata$MinWgt*playerdata$OnOffGame*playerdata$ImpactPct.pg
playerdata$OnOffXUsg <- playerdata$OnOffGame*playerdata$ImpactPct.pg
playerdata$MinWgtXOnOffInjAdj <- playerdata$MinWgtInjAdj*playerdata$OnOffGame
playerdata$MinWgtXOnOffXUsgInjAdj <- playerdata$MinWgtInjAdj*playerdata$OnOffGame*playerdata$ImpactPct.pg
# playerdata <- playerdata %>% 
#   dplyr::group_by(Team, Season, Date) %>%
#   dplyr::mutate(OnOffTeamAvg = sum(MinWgtXOnOffXUsg,na.rm = TRUE), OnOffInjAdj = sum(MinWgtXOnOffXUsgInjAdj,na.rm = TRUE))
playerdata$RPMXMinWgt <- playerdata$MinWgt*playerdata$RPMScore
playerdata$ORPMXMinWgt <- playerdata$MinWgt*playerdata$ORPMScore
playerdata$DRPMXMinWgt <- playerdata$MinWgt*playerdata$DRPMScore
playerdata$RPMXMinAdj <- playerdata$MinWgtInjAdj*playerdata$RPMScore
playerdata$ORPMXMinAdj <- playerdata$MinWgtInjAdj*playerdata$ORPMScore
playerdata$DRPMXMinAdj <- playerdata$MinWgtInjAdj*playerdata$DRPMScore
playerdata$RPMXMinWgtNorm <- playerdata$MinWgt*playerdata$RPMScoreNorm
playerdata$ORPMXMinWgtNorm <- playerdata$MinWgt*playerdata$ORPMScoreNorm
playerdata$DRPMXMinWgtNorm <- playerdata$MinWgt*playerdata$DRPMScoreNorm
playerdata$RPMXMinAdjNorm <- playerdata$MinWgtInjAdj*playerdata$RPMScoreNorm
playerdata$ORPMXMinAdjNorm <- playerdata$MinWgtInjAdj*playerdata$ORPMScoreNorm
playerdata$DRPMXMinAdjNorm <- playerdata$MinWgtInjAdj*playerdata$DRPMScoreNorm


#add in playoff totals
playerdata <- playerdata %>% left_join(dplyr::select(playoffmindata,-Players),by = c("PlayerID","Date")) %>% ungroup()
playerdata$PlayoffMinXMinAdj <- playerdata$MinWgtInjAdj*playerdata$PlayoffMin
playerdata$PlayoffGameXMinAdj <- playerdata$MinWgtInjAdj*playerdata$PlayoffGames
playerdata$PlayoffAvgXMinAdj <- playerdata$MinWgtInjAdj*playerdata$PlayoffMinAvg

playerdata <- distinct(playerdata,PlayerID,Team,Date,.keep_all=TRUE)
playerdataMERGE <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  arrange(Team,Season,Date) %>%
  dplyr::summarize(SeasGames = mean(SeasGames,na.rm = TRUE),IsGame = mean(IsGame,na.rm = TRUE),
            Box1TeamAvg = sum(RPMXMinWgt,na.rm = TRUE), Box1InjAdj = sum(RPMXMinAdj,na.rm = TRUE),
         Box2TeamAvg = sum(ORPMXMinWgt,na.rm = TRUE), Box2InjAdj = sum(ORPMXMinAdj,na.rm = TRUE),
         Box3TeamAvg = sum(DRPMXMinWgt,na.rm = TRUE), Box3InjAdj = sum(DRPMXMinAdj,na.rm = TRUE),
         TeamQualRPM = sum(RPMXMinAdj,na.rm = TRUE),TeamQualORPM = sum(ORPMXMinAdj,na.rm = TRUE),
         TeamQualDRPM = sum(DRPMXMinAdj,na.rm = TRUE),PlayoffExpMin = sum(PlayoffMinXMinAdj,na.rm = TRUE),
         PlayoffExpGame = sum(PlayoffGameXMinAdj,na.rm = TRUE),PlayoffExpMinPG = sum(PlayoffAvgXMinAdj,na.rm = TRUE),
         OnOffTeamAvg = sum(MinWgtXOnOffXUsg,na.rm = TRUE),
         OnOffInjAdj = sum(MinWgtXOnOffXUsgInjAdj,na.rm = TRUE),superadj=max(superadj,na.rm=TRUE),
         staradj=max(staradj,na.rm=TRUE),solidadj=max(solidadj,na.rm=TRUE),MaxDate=max(MaxDate,na.rm=TRUE),
         Box1TeamAvgNorm = sum(RPMXMinWgtNorm,na.rm = TRUE), Box1InjAdjNorm = sum(RPMXMinAdjNorm,na.rm = TRUE),
         Box2TeamAvgNorm = sum(ORPMXMinWgtNorm,na.rm = TRUE), Box2InjAdjNorm = sum(ORPMXMinAdjNorm,na.rm = TRUE),
         Box3TeamAvgNorm = sum(DRPMXMinWgtNorm,na.rm = TRUE), Box3InjAdjNorm = sum(DRPMXMinAdjNorm,na.rm = TRUE),
         TeamQualRPMNorm = sum(RPMXMinAdjNorm,na.rm = TRUE),TeamQualORPMNorm = sum(ORPMXMinAdjNorm,na.rm = TRUE),
         TeamQualDRPMNorm = sum(DRPMXMinAdjNorm,na.rm = TRUE),)

#Things that will feed into regression
playerdataMERGE$OnOffAdjustment <- -1*(playerdataMERGE$OnOffTeamAvg - playerdataMERGE$OnOffInjAdj)
playerdataMERGE$Box1Adjustment <- -1*(playerdataMERGE$Box1TeamAvg - playerdataMERGE$Box1InjAdj)
playerdataMERGE$Box2Adjustment <- -1*(playerdataMERGE$Box2TeamAvg - playerdataMERGE$Box2InjAdj)
playerdataMERGE$Box3Adjustment <- -1*(playerdataMERGE$Box3TeamAvg - playerdataMERGE$Box3InjAdj)

playerdataMERGE$OnOffAdjustment[playerdataMERGE$SeasGames<=1] <- 0
playerdataMERGE$Box1Adjustment[playerdataMERGE$SeasGames<=1] <- 0
playerdataMERGE$Box2Adjustment[playerdataMERGE$SeasGames<=1] <- 0
playerdataMERGE$Box3Adjustment[playerdataMERGE$SeasGames<=1] <- 0


playerdataMERGE$Box1AdjustmentNorm <- -1*(playerdataMERGE$Box1TeamAvgNorm - playerdataMERGE$Box1InjAdjNorm)
playerdataMERGE$Box2AdjustmentNorm <- -1*(playerdataMERGE$Box2TeamAvgNorm - playerdataMERGE$Box2InjAdjNorm)
playerdataMERGE$Box3AdjustmentNorm <- -1*(playerdataMERGE$Box3TeamAvgNorm - playerdataMERGE$Box3InjAdjNorm)


playerdataMERGE$Box1AdjustmentNorm[playerdataMERGE$SeasGames<=1] <- 0
playerdataMERGE$Box2AdjustmentNorm[playerdataMERGE$SeasGames<=1] <- 0
playerdataMERGE$Box3AdjustmentNorm[playerdataMERGE$SeasGames<=1] <- 0

playerdataMERGE <- playerdataMERGE[playerdataMERGE$IsGame>0,]

#create first difference
playerdataMERGE <- playerdataMERGE %>% 
  dplyr::group_by(Team,Season) %>% 
  dplyr::mutate(Box1AdjChg = Box1Adjustment - dplyr::lag(Box1Adjustment,n = 1,default = 0),
                Box1AdjChgNorm = Box1AdjustmentNorm - dplyr::lag(Box1AdjustmentNorm,n = 1,default = 0)) %>% 
  ungroup()
playerdataMERGE$Box1AdjChg[is.na(playerdataMERGE$Box1AdjChg)&!is.na(playerdataMERGE$Box1Adjustment)] <- 0
playerdataMERGE$Box1AdjChgNorm[is.na(playerdataMERGE$Box1AdjChgNorm)&!is.na(playerdataMERGE$Box1AdjustmentNorm)] <- 0


playerdata <- playerdata %>% 
  dplyr::group_by(Team, Season, Date) %>%
  arrange(Team,Season,Date) %>%
  dplyr::mutate(SeasGames = mean(SeasGames,na.rm = TRUE),IsGame = mean(IsGame,na.rm = TRUE),
         Box1TeamAvg = sum(RPMXMinWgt,na.rm = TRUE), Box1InjAdj = sum(RPMXMinAdj,na.rm = TRUE),
         Box2TeamAvg = sum(ORPMXMinWgt,na.rm = TRUE), Box2InjAdj = sum(ORPMXMinAdj,na.rm = TRUE),
         Box3TeamAvg = sum(DRPMXMinWgt,na.rm = TRUE), Box3InjAdj = sum(DRPMXMinAdj,na.rm = TRUE),
         TeamQualRPM = sum(RPMXMinAdj,na.rm = TRUE),TeamQualORPM = sum(ORPMXMinAdj,na.rm = TRUE),
         TeamQualDRPM = sum(DRPMXMinAdj,na.rm = TRUE),PlayoffExpMin = sum(PlayoffMinXMinAdj,na.rm = TRUE),
         PlayoffExpGame = sum(PlayoffGameXMinAdj,na.rm = TRUE),PlayoffExpMinPG = sum(PlayoffAvgXMinAdj,na.rm = TRUE),
         OnOffTeamAvg = sum(MinWgtXOnOffXUsg,na.rm = TRUE),
         OnOffInjAdj = sum(MinWgtXOnOffXUsgInjAdj,na.rm = TRUE),
         Box1TeamAvgNorm = sum(RPMXMinWgtNorm,na.rm = TRUE), Box1InjAdjNorm = sum(RPMXMinAdjNorm,na.rm = TRUE),
         Box2TeamAvgNorm = sum(ORPMXMinWgtNorm,na.rm = TRUE), Box2InjAdjNorm = sum(ORPMXMinAdjNorm,na.rm = TRUE),
         Box3TeamAvgNorm = sum(DRPMXMinWgtNorm,na.rm = TRUE), Box3InjAdjNorm = sum(DRPMXMinAdjNorm,na.rm = TRUE),
         TeamQualRPMNorm = sum(RPMXMinAdjNorm,na.rm = TRUE),TeamQualORPMNorm = sum(ORPMXMinAdjNorm,na.rm = TRUE),
         TeamQualDRPMNorm = sum(DRPMXMinAdjNorm,na.rm = TRUE))

#Things that will feed into regression
playerdata$OnOffAdjustment <- -1*(playerdata$OnOffTeamAvg - playerdata$OnOffInjAdj)
playerdata$Box1Adjustment <- -1*(playerdata$Box1TeamAvg - playerdata$Box1InjAdj)
playerdata$Box2Adjustment <- -1*(playerdata$Box2TeamAvg - playerdata$Box2InjAdj)
playerdata$Box3Adjustment <- -1*(playerdata$Box3TeamAvg - playerdata$Box3InjAdj)

playerdata$OnOffAdjustment[playerdata$SeasGames<=1] <- 0
playerdata$Box1Adjustment[playerdata$SeasGames<=1] <- 0
playerdata$Box2Adjustment[playerdata$SeasGames<=1] <- 0
playerdata$Box3Adjustment[playerdata$SeasGames<=1] <- 0


playerdata$Box1AdjustmentNorm <- -1*(playerdata$Box1TeamAvgNorm - playerdata$Box1InjAdjNorm)
playerdata$Box2AdjustmentNorm <- -1*(playerdata$Box2TeamAvgNorm - playerdata$Box2InjAdjNorm)
playerdata$Box3AdjustmentNorm <- -1*(playerdata$Box3TeamAvgNorm - playerdata$Box3InjAdjNorm)


playerdata$Box1AdjustmentNorm[playerdata$SeasGames<=1] <- 0
playerdata$Box2AdjustmentNorm[playerdata$SeasGames<=1] <- 0
playerdata$Box3AdjustmentNorm[playerdata$SeasGames<=1] <- 0


##start new code here to celan adjustment data

playerdataMERGE <- playerdataMERGE %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(MaxDate=max(Date,na.rm=TRUE)) %>%
  ungroup()
playerdataMERGE <- playerdataMERGE %>%
  dplyr::group_by(Season) %>% 
  dplyr::mutate(SeasNum = cur_group_id()) %>%
  ungroup()
playerdataMERGE$SeasNumLag <- as.integer(playerdataMERGE$SeasNum-1)
playerdataMERGE <- playerdataMERGE %>% left_join(distinct(dplyr::rename(playerdataMERGE[,c("Team","SeasNum","MaxDate")],SeasNumLag=SeasNum)),by=c("Team","SeasNumLag"))

playerdataMERGE2 <- playerdataMERGE
playerdataMERGE2 <- playerdataMERGE2 %>% 
  rename_with(stringr::str_replace,
              pattern = "TeamAvg",
              replacement = "TeamAvgPREV",
              matches("TeamAvg")) %>%
  ungroup()

playerdataMERGE <- dplyr::rename(playerdataMERGE,OnOffTeamAvg_new=OnOffTeamAvg,Box1TeamAvg_new=Box1TeamAvg,Box2TeamAvg_new=Box2TeamAvg,Box3TeamAvg_new=Box3TeamAvg,
                                          OnOffAdjustment_new=OnOffAdjustment,Box1Adjustment_new=Box1Adjustment,Box2Adjustment_new=Box2Adjustment,Box3Adjustment_new=Box3Adjustment,
                          Box1AdjChg_new=Box1AdjChg,Box1TeamAvgNorm_new=Box1TeamAvgNorm,Box2TeamAvgNorm_new=Box2TeamAvgNorm,Box3TeamAvgNorm_new=Box3TeamAvgNorm,
                          Box1AdjustmentNorm_new=Box1AdjustmentNorm,Box2AdjustmentNorm_new=Box2AdjustmentNorm,Box3AdjustmentNorm_new=Box3AdjustmentNorm,
                          Box1AdjChgNorm_new=Box1AdjChgNorm)

playerdataMERGE2 <- dplyr::rename(playerdataMERGE2,OtherDate = MaxDate.y,MaxDate.y = Date)
playerdataMERGE2 <- dplyr::select(playerdataMERGE2,MaxDate.y,Team,ends_with("TeamAvgPREV"),ends_with("TeamAvgPREVNorm"))
playerdataMERGE <- playerdataMERGE %>% left_join(playerdataMERGE2,by = c("Team","MaxDate.y"))
playerdataMERGE <- dplyr::select(playerdataMERGE,-MaxDate.x)
playerdataMERGE <- dplyr::rename(playerdataMERGE,MaxDate=MaxDate.y)

playerdataMERGE$SeasGamesPREVWGT <- ifelse(playerdataMERGE$SeasGames<=20,(21-playerdataMERGE$SeasGames)/20,0)
playerdataMERGE$SeasGamesCURRWGT <- ifelse(playerdataMERGE$SeasGames<=20,(playerdataMERGE$SeasGames-1)/20,1)
playerdataMERGE$SeasGamesPREVWGT[playerdataMERGE$SeasGames==0] <- 1
playerdataMERGE$SeasGamesCURRWGT[playerdataMERGE$SeasGames==0] <- 0

#Things that will feed into regression
playerdataMERGE$OnOffTeamAvg <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$OnOffTeamAvg_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$OnOffTeamAvgPREV
playerdataMERGE$Box1TeamAvg <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box1TeamAvg_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box1TeamAvgPREV
playerdataMERGE$Box2TeamAvg <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box2TeamAvg_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box2TeamAvgPREV
playerdataMERGE$Box3TeamAvg <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box3TeamAvg_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box3TeamAvgPREV
playerdataMERGE$Box1TeamAvgNorm <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box1TeamAvgNorm_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box1TeamAvgPREVNorm
playerdataMERGE$Box2TeamAvgNorm <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box2TeamAvgNorm_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box2TeamAvgPREVNorm
playerdataMERGE$Box3TeamAvgNorm <- playerdataMERGE$SeasGamesCURRWGT*playerdataMERGE$Box3TeamAvgNorm_new + playerdataMERGE$SeasGamesPREVWGT*playerdataMERGE$Box3TeamAvgPREVNorm

#Things that will feed into regression
playerdataMERGE$OnOffAdjustment <- -1*(playerdataMERGE$OnOffTeamAvg - playerdataMERGE$OnOffInjAdj)
playerdataMERGE$Box1Adjustment <- -1*(playerdataMERGE$Box1TeamAvg - playerdataMERGE$Box1InjAdj)
playerdataMERGE$Box2Adjustment <- -1*(playerdataMERGE$Box2TeamAvg - playerdataMERGE$Box2InjAdj)
playerdataMERGE$Box3Adjustment <- -1*(playerdataMERGE$Box3TeamAvg - playerdataMERGE$Box3InjAdj)
playerdataMERGE$Box1AdjustmentNorm <- -1*(playerdataMERGE$Box1TeamAvgNorm - playerdataMERGE$Box1InjAdjNorm)
playerdataMERGE$Box2AdjustmentNorm <- -1*(playerdataMERGE$Box2TeamAvgNorm - playerdataMERGE$Box2InjAdjNorm)
playerdataMERGE$Box3AdjustmentNorm <- -1*(playerdataMERGE$Box3TeamAvgNorm - playerdataMERGE$Box3InjAdjNorm)


#create first difference
playerdataMERGE <- playerdataMERGE %>% 
  dplyr::group_by(Team,Season) %>% 
  dplyr::mutate(Box1AdjChg = Box1Adjustment - dplyr::lag(Box1Adjustment,n = 1,default = 0),
                Box1AdjChgNorm = Box1AdjustmentNorm - dplyr::lag(Box1AdjustmentNorm,n = 1,default = 0)) %>% 
  ungroup()
playerdataMERGE$Box1AdjChg[is.na(playerdataMERGE$Box1AdjChg)&!is.na(playerdataMERGE$Box1Adjustment)] <- 0
playerdataMERGE$Box1AdjChgNorm[is.na(playerdataMERGE$Box1AdjChgNorm)&!is.na(playerdataMERGE$Box1AdjustmentNorm)] <- 0

table1 <- table
load("injurytable.RData")
table <- table[table$Date<today,]
table <- dplyr::bind_rows(table,table1)
table <- distinct(table)
save(table, file = paste("injurytable.RData", sep=""))
save(playerdata, file = paste("modelbetaV1_PlayerData.RData", sep=""))
save(playerdataMERGE, file = paste("modelbetaV1_dataformerge.RData", sep=""))
playerdataVIEW <- playerdata[playerdata$Date>=max(playerdata$Date),]
playerdataVIEW <- dplyr::select(playerdataVIEW,ORPMScore,DRPMScore,MINPGPREV,MINPG2,MINPGAdj,MinWgt,MinWgtInjAdj,MinEnoughAdjFinal,FinalStatus,InclDummy1,superadj,staradj,solidadj,zscore,superstar,star,solid,everything())
playerdataVIEW <- dplyr::select(playerdataVIEW,RPMScore,ORPMScore,DRPMScore,MIN,MINPGPREV,MINPG2,MINPGAdj,MinWgt,MinWgtInjAdj,MinEnoughAdjFinal,FinalStatus,InclDummy1,superadj,staradj,solidadj,zscore,superstar,star,solid,everything())
save(playerdataVIEW, file = paste("checkteamadjustment.RData", sep=""))
save(playerdataTODAY, file = paste("checkinjuries.RData", sep=""))
load("injuryhistory.RData")
injuryhistory <- rbind(injuryhistory,playerdataVIEW)
injuryhistory <- distinct(injuryhistory)
save(injuryhistory, file = paste("injuryhistory.RData", sep=""))

seasonplayerdata <- playerdata %>% 
  dplyr::group_by(Season) %>%
  dplyr::summarize(MaxDate = max(Date,na.rm = TRUE)) %>% 
  ungroup()

regularseasondates <- c(as.Date("04/09/2023", format = "%m/%d/%Y")+2,as.Date("05/16/2021", format = "%m/%d/%Y")+2,
                        as.Date("04/10/2022", format = "%m/%d/%Y")+2,
                        as.Date("04/11/2018", format = "%m/%d/%Y")+3,as.Date("08/14/2020", format = "%m/%d/%Y")+1,
                        as.Date("04/10/2019", format = "%m/%d/%Y")+3,as.Date("04/12/2017", format = "%m/%d/%Y")+3,
                        as.Date("04/13/2016", format = "%m/%d/%Y")+3,as.Date("04/15/2015", format = "%m/%d/%Y")+3,
                        as.Date("04/16/2014", format = "%m/%d/%Y")+3,as.Date("04/17/2013", format = "%m/%d/%Y")+3,
                        as.Date("04/26/2012", format = "%m/%d/%Y")+2,as.Date("04/13/2011", format = "%m/%d/%Y")+3,
                        as.Date("04/14/2010", format = "%m/%d/%Y")+3,as.Date("04/16/2009", format = "%m/%d/%Y")+2,
                        as.Date("04/16/2008", format = "%m/%d/%Y")+3,as.Date("04/18/2007", format = "%m/%d/%Y")+3,
                        as.Date("04/19/2006", format = "%m/%d/%Y")+3,as.Date("04/14/2004", format = "%m/%d/%Y")+3,
                        as.Date("04/20/2005", format = "%m/%d/%Y")+3,max(playerdata$Date,na.rm = TRUE))

seasonplayerdata <- playerdata[playerdata$Date %in% seasonplayerdata$MaxDate,]
regseasonplayerdata <- playerdata[playerdata$Date %in% regularseasondates,]
save(seasonplayerdata, file = paste("seasonsummaries_PlayerData.RData", sep=""))
save(regseasonplayerdata, file = paste("regseasonsummaries_PlayerData.RData", sep=""))
save(box_nopreseason, file = paste("box_nopreseasonBetaUpdate.RData", sep=""))
save(teamroster, file = paste("teamroster.RData", sep=""))



