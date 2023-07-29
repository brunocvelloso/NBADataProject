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
#setwd("/Users/ricardovelloso/Dropbox/Projects/NBAGambling")

load("nba_teamboxNEW.RData")
load("nba_playerboxNEW.RData")
load("NBA_ESPNgamelinks.RData")
load("NBA_ESPNgamelinksSUB.RData")
load("startdate.RData")
load("enddate.RData")
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())


####1. Player Box Score Data###
#load master cleaned box score data
load("master_boxscores.RData")
boxscoredataMASTER <- boxscoredata

#take new update, clean and then merge back into master data
boxscoredata <- nba_playerboxNEW
boxscoredata$fg[boxscoredata$fg=="-----"|is.na(boxscoredata$fg)] <-""
boxscoredata$fg3[boxscoredata$fg3=="-----"|is.na(boxscoredata$fg3)] <- ""
boxscoredata$ft[boxscoredata$ft=="-----"|is.na(boxscoredata$ft)] <- ""
boxscoredata <- dplyr::rename(boxscoredata,Players=athlete_short_name,Starter=starter,PlayersFull=athlete_display_name)
boxscoredata$Starter <- ifelse(boxscoredata$Starter==TRUE,1,0)
boxscoredata$Position <- boxscoredata$athlete_position_abbreviation
boxscoredata$Home <- ifelse(boxscoredata$Team==boxscoredata$Team.x,1,0)
boxscoredata$DNP <- ifelse(boxscoredata$did_not_play==TRUE,1,0) 
boxscoredata$DNP[boxscoredata$min=="--"&boxscoredata$pts=="--"] <- 1
boxscoredata[, c("min","oreb","dreb","reb","ast","stl","blk","to","pf","plus_minus","pts")] <- sapply(boxscoredata[, c("min","oreb","dreb","reb","ast","stl","blk","to","pf","plus_minus","pts")], as.numeric)
boxscoredata$FGM <- as.numeric(substr(boxscoredata$fg,1,(str_locate(pattern ='-', boxscoredata$fg))-1))
boxscoredata$FGA <- as.numeric(substr(boxscoredata$fg,(str_locate(pattern ='-', boxscoredata$fg))+1,nchar(boxscoredata$fg)))
boxscoredata$'3PM' <- as.numeric(substr(boxscoredata$fg3,1,(str_locate(pattern ='-', boxscoredata$fg3))-1))
boxscoredata$'3PA' <- as.numeric(substr(boxscoredata$fg3,(str_locate(pattern ='-', boxscoredata$fg3))+1,nchar(boxscoredata$fg3)))
boxscoredata$'FTM' <- as.numeric(substr(boxscoredata$ft,1,(str_locate(pattern ='-', boxscoredata$ft))-1))
boxscoredata$'FTA' <- as.numeric(substr(boxscoredata$ft,(str_locate(pattern ='-', boxscoredata$ft))+1,nchar(boxscoredata$ft)))
boxscoredata$'2PM' <- boxscoredata$FGM-boxscoredata$'3PM'
boxscoredata$'2PA' <- boxscoredata$FGA-boxscoredata$'3PA'
boxscoredata$'FG%' <- boxscoredata$'FGM'/boxscoredata$'FGA'
boxscoredata$'FT%' <- boxscoredata$'FTM'/boxscoredata$'FTA'
boxscoredata$'3P%' <- boxscoredata$'3PM'/boxscoredata$'3PA'
boxscoredata$'2P%' <- boxscoredata$'2PM'/boxscoredata$'2PA'
boxscoredata$LastName <- tolower(substr(boxscoredata$Players,str_locate(pattern =' ', boxscoredata$Players)+1,nchar(boxscoredata$Players)))
boxscoredata$FirstLetter <- tolower(substr(boxscoredata$Players,1,1))
boxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', boxscoredata$LastName)[,1]),boxscoredata$LastName,paste(substr(boxscoredata$LastName,1,str_locate(pattern =' ', boxscoredata$LastName)[,1]-1),"-",substr(boxscoredata$LastName,str_locate(pattern =' ', boxscoredata$LastName)[,1]+1,nchar(boxscoredata$LastName)),sep=""))
boxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', boxscoredata$LastName)[,1]),boxscoredata$LastName,paste(substr(boxscoredata$LastName,1,str_locate(pattern =' ', boxscoredata$LastName)[,1]-1),"-",substr(boxscoredata$LastName,str_locate(pattern =' ', boxscoredata$LastName)[,1]+1,nchar(boxscoredata$LastName)),sep=""))
boxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', boxscoredata$LastName)[,1]),boxscoredata$LastName,paste(substr(boxscoredata$LastName,1,str_locate(pattern =' ', boxscoredata$LastName)[,1]-1),"-",substr(boxscoredata$LastName,str_locate(pattern =' ', boxscoredata$LastName)[,1]+1,nchar(boxscoredata$LastName)),sep=""))
boxscoredata$LastName <- ifelse(substr(boxscoredata$LastName,nchar(boxscoredata$LastName)-2,nchar(boxscoredata$LastName))=="jr.",paste(substr(boxscoredata$LastName,1,nchar(boxscoredata$LastName)-3),"jr",sep=""),boxscoredata$LastName)
boxscoredata$LastName <- ifelse(substr(boxscoredata$LastName,nchar(boxscoredata$LastName)-2,nchar(boxscoredata$LastName))=="sr.",paste(substr(boxscoredata$LastName,1,nchar(boxscoredata$LastName)-3),"sr",sep=""),boxscoredata$LastName)
boxscoredata$PlayerID <-  as.character(boxscoredata$athlete_id)
boxscoredata$BoxScore <- paste("https://www.espn.com/nba/boxscore/_/gameId/",boxscoredata$GameID,sep="")
boxscoredata$PlaybyPlay <- paste("https://www.espn.com/nba/playbyplay/_/gameId/",boxscoredata$GameID,sep="")
boxscoredata$elemplayercards <- boxscoredata$PlayerID
boxscoredata <- dplyr::select(boxscoredata,-`+/-`)
boxscoredata <- dplyr::rename(boxscoredata,MIN=min,FG=fg,`3PT`=fg3,FT=ft,OREB=oreb,DREB=dreb,REB=reb,AST=ast,STL=stl,BLK=blk,TO=to,PF=pf,`+/-`=plus_minus,PTS=pts)
boxscoredata$GameID.x <- boxscoredata$GameID
boxscoredata$GameID.y <- boxscoredata$GameID
boxscoredata$GameDescription.x <- boxscoredata$GameDescription
boxscoredata$GameDescription.y <- boxscoredata$GameDescription
boxscoredata$LastName2 <- boxscoredata$LastName
boxscoredata$ApostropheDash <- NA
boxscoredata$ApostropheDash2 <- NA
boxscoredata$Match1 <- paste(boxscoredata$FirstLetter,boxscoredata$LastName,boxscoredata$GameID,sep="")
boxscoredata$Year <- year(boxscoredata$Date)
boxscoredata$Month <- month(boxscoredata$Date)
boxscoredata$Day <- day(boxscoredata$Date)
boxscoredata$Position5 <- boxscoredata$Position
boxscoredata$Position <- substr(boxscoredata$Position5,nchar(boxscoredata$Position5),nchar(boxscoredata$Position5))
boxscoredata$Season <- ifelse((boxscoredata$Month>=10&boxscoredata$Day>=12)|boxscoredata$Month>=11,paste(boxscoredata$Year,"-",substr(boxscoredata$Year+1,3,4),sep=""),paste(boxscoredata$Year-1,"-",substr(boxscoredata$Year,3,4),sep=""))

boxscoredata <- dplyr::select(boxscoredata,GameID,LastName,FirstLetter,Players,MIN,FG,`3PT`,FT,OREB,DREB,REB,AST,STL,BLK,TO,PF,`+/-`,PTS,Team,Starter,Position,Home,DNP,FGM,FGA,`3PM`,
                       `3PA`,FTM,FTA,`2PM`,`2PA`,`FG%`,`FT%`,`3P%`,`2P%`,GameID.x,GameDescription.x,elemplayercards,PlayerID,GameID.y,GameDescription.y,PlayersFull,Position5,ApostropheDash2,Match1,
                       Year,Month,Day,BoxScore,PlaybyPlay,Date,RegularSeason,Playoffs,Finals,Season)

#FIX FOR PFs and TOs for box score data (team data is often 0 or 1 and incorrect) and weird duplicates in box score
boxscoredata <- boxscoredata[order(boxscoredata$Date,boxscoredata$GameID,boxscoredata$Home,-boxscoredata$Starter,boxscoredata$PlayerID),]
boxscoredata$findduplicates <- paste(boxscoredata$PlayersFull,boxscoredata$GameID,boxscoredata$Team,sep="")
boxscoredata$dupl <- duplicated(boxscoredata$findduplicates,fromLAST=TRUE)
test <- boxscoredata[boxscoredata$dupl==1,]
boxscoredata <- boxscoredata[boxscoredata$dupl==0,]
boxscoredata <- dplyr::select(boxscoredata,-findduplicates,-dupl)

box <- boxscoredata %>%
  dplyr::group_by(GameID,Team) %>%
  dplyr::summarize(OREB = sum(OREB,na.rm=TRUE),
            DREB= sum(DREB,na.rm=TRUE),
            REB = sum(REB,na.rm=TRUE),
            TO= sum(TO,na.rm=TRUE),
            PF= sum(PF,na.rm=TRUE),
            AST= sum(AST,na.rm=TRUE),
            STL= sum(STL,na.rm=TRUE),
            BLK= sum(BLK,na.rm=TRUE)) %>%
  ungroup()

boxscoredataUPDATE <- boxscoredata
save(boxscoredataUPDATE, file = paste("NBAboxscoresRECENTUPDATE.RData", sep=""))
boxscoredataMASTER <- boxscoredataMASTER[!(boxscoredataMASTER$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
boxscoredataMASTER <- rbind(boxscoredataMASTER,boxscoredataUPDATE)
boxscoredata <- boxscoredataMASTER
save(boxscoredata, file = paste("master_boxscores.RData", sep=""))

###2. Team Box Score Data###
load("master_teamboxscores.RData")
teamboxscoredataMASTER <- teamboxscoredata

teamboxscoredata <- nba_teamboxNEW
teamboxscoredata$field_goals_made_field_goals_attempted[teamboxscoredata$field_goals_made_field_goals_attempted=="-----"|is.na(teamboxscoredata$field_goals_made_field_goals_attempted)] <-""
teamboxscoredata$three_point_field_goals_made_three_point_field_goals_attempted[teamboxscoredata$three_point_field_goals_made_three_point_field_goals_attempted=="-----"|is.na(teamboxscoredata$three_point_field_goals_made_three_point_field_goals_attempted)] <- ""
teamboxscoredata$free_throws_made_free_throws_attempted[teamboxscoredata$free_throws_made_free_throws_attempted=="-----"|is.na(teamboxscoredata$free_throws_made_free_throws_attempted)] <- ""
teamboxscoredata$Starter <- 0
teamboxscoredata$Players <- "TEAM"
teamboxscoredata$PlayersFull <- "TEAM"
teamboxscoredata$Position <- ""
teamboxscoredata$Home <- ifelse(teamboxscoredata$Team==teamboxscoredata$Team.x,1,0)
teamboxscoredata$DNP <- 0
teamboxscoredata <- dplyr::rename(teamboxscoredata,FG=field_goals_made_field_goals_attempted,`3PT`=three_point_field_goals_made_three_point_field_goals_attempted,
                       FT=free_throws_made_free_throws_attempted,reb=total_rebounds,oreb=offensive_rebounds,
                       dreb=defensive_rebounds,ast=assists,stl=steals,blk=blocks,to=turnovers,pf=fouls)
teamboxscoredata$FGM <- as.numeric(substr(teamboxscoredata$FG,1,(str_locate(pattern ='-', teamboxscoredata$FG))-1))
teamboxscoredata$FGA <- as.numeric(substr(teamboxscoredata$FG,(str_locate(pattern ='-', teamboxscoredata$FG))+1,nchar(teamboxscoredata$FG)))
teamboxscoredata$'3PM' <- as.numeric(substr(teamboxscoredata$`3PT`,1,(str_locate(pattern ='-', teamboxscoredata$`3PT`))-1))
teamboxscoredata$'3PA' <- as.numeric(substr(teamboxscoredata$`3PT`,(str_locate(pattern ='-', teamboxscoredata$`3PT`))+1,nchar(teamboxscoredata$`3PT`)))
teamboxscoredata$'FTM' <- as.numeric(substr(teamboxscoredata$FT,1,(str_locate(pattern ='-', teamboxscoredata$FT))-1))
teamboxscoredata$'FTA' <- as.numeric(substr(teamboxscoredata$FT,(str_locate(pattern ='-', teamboxscoredata$FT))+1,nchar(teamboxscoredata$FT)))
teamboxscoredata$'2PM' <- teamboxscoredata$FGM-teamboxscoredata$'3PM'
teamboxscoredata$'2PA' <- teamboxscoredata$FGA-teamboxscoredata$'3PA'
teamboxscoredata$'FG%' <- teamboxscoredata$'FGM'/teamboxscoredata$'FGA'
teamboxscoredata$'FT%' <- teamboxscoredata$'FTM'/teamboxscoredata$'FTA'
teamboxscoredata$'3P%' <- teamboxscoredata$'3PM'/teamboxscoredata$'3PA'
teamboxscoredata$'2P%' <- teamboxscoredata$'2PM'/teamboxscoredata$'2PA'
teamboxscoredata$pts <- teamboxscoredata$`3PM`*3+teamboxscoredata$`2PM`*2+teamboxscoredata$FTM*1
teamboxscoredata[, c("oreb","dreb","reb","ast","stl","blk","to","pf","pts")] <- sapply(teamboxscoredata[, c("oreb","dreb","reb","ast","stl","blk","to","pf","pts")], as.numeric)
teamboxscoredata$LastName <- tolower(substr(teamboxscoredata$Players,str_locate(pattern =' ', teamboxscoredata$Players)+1,nchar(teamboxscoredata$Players)))
teamboxscoredata$FirstLetter <- tolower(substr(teamboxscoredata$Players,1,1))
teamboxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', teamboxscoredata$LastName)[,1]),teamboxscoredata$LastName,paste(substr(teamboxscoredata$LastName,1,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]-1),"-",substr(teamboxscoredata$LastName,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]+1,nchar(teamboxscoredata$LastName)),sep=""))
teamboxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', teamboxscoredata$LastName)[,1]),teamboxscoredata$LastName,paste(substr(teamboxscoredata$LastName,1,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]-1),"-",substr(teamboxscoredata$LastName,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]+1,nchar(teamboxscoredata$LastName)),sep=""))
teamboxscoredata$LastName <- ifelse(is.na(str_locate(pattern =' ', teamboxscoredata$LastName)[,1]),teamboxscoredata$LastName,paste(substr(teamboxscoredata$LastName,1,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]-1),"-",substr(teamboxscoredata$LastName,str_locate(pattern =' ', teamboxscoredata$LastName)[,1]+1,nchar(teamboxscoredata$LastName)),sep=""))
teamboxscoredata$LastName <- ifelse(substr(teamboxscoredata$LastName,nchar(teamboxscoredata$LastName)-2,nchar(teamboxscoredata$LastName))=="jr.",paste(substr(teamboxscoredata$LastName,1,nchar(teamboxscoredata$LastName)-3),"jr",sep=""),teamboxscoredata$LastName)
teamboxscoredata$LastName <- ifelse(substr(teamboxscoredata$LastName,nchar(teamboxscoredata$LastName)-2,nchar(teamboxscoredata$LastName))=="sr.",paste(substr(teamboxscoredata$LastName,1,nchar(teamboxscoredata$LastName)-3),"sr",sep=""),teamboxscoredata$LastName)
teamboxscoredata$PlayerID <-  NA
teamboxscoredata$BoxScore <- paste("https://www.espn.com/nba/boxscore/_/gameId/",teamboxscoredata$GameID,sep="")
teamboxscoredata$PlaybyPlay <- paste("https://www.espn.com/nba/playbyplay/_/gameId/",teamboxscoredata$GameID,sep="")
teamboxscoredata$elemplayercards <- teamboxscoredata$PlayerID
teamboxscoredata$MIN <- NA
teamboxscoredata$`+/-` <- NA
teamboxscoredata <- dplyr::rename(teamboxscoredata,OREB2=oreb,DREB2=dreb,REB2=reb,AST2=ast,STL2=stl,BLK2=blk,TO2=to,PF2=pf,PTS=pts)
teamboxscoredata$REB2 <- teamboxscoredata$OREB2+teamboxscoredata$DREB2
teamboxscoredata$GameID.x <- teamboxscoredata$GameID
teamboxscoredata$GameID.y <- NA
teamboxscoredata$GameDescription.x <- teamboxscoredata$GameDescription
teamboxscoredata$GameDescription.y <- NA
teamboxscoredata$LastName2 <- teamboxscoredata$LastName
teamboxscoredata$ApostropheDash <- 0
teamboxscoredata$ApostropheDash2 <- 0
teamboxscoredata$Match1 <- paste(teamboxscoredata$FirstLetter,teamboxscoredata$LastName,teamboxscoredata$GameID,sep="")
teamboxscoredata$Year <- year(teamboxscoredata$Date)
teamboxscoredata$Month <- month(teamboxscoredata$Date)
teamboxscoredata$Day <- day(teamboxscoredata$Date)
teamboxscoredata$Position5 <- teamboxscoredata$Position
teamboxscoredata$Position <- substr(teamboxscoredata$Position5,nchar(teamboxscoredata$Position5),nchar(teamboxscoredata$Position5))
teamboxscoredata$Season <- ifelse((teamboxscoredata$Month>=10&teamboxscoredata$Day>=12)|teamboxscoredata$Month>=11,paste(teamboxscoredata$Year,"-",substr(teamboxscoredata$Year+1,3,4),sep=""),paste(teamboxscoredata$Year-1,"-",substr(teamboxscoredata$Year,3,4),sep=""))

teamboxscoredata <- teamboxscoredata %>% left_join(box,by=c("GameID","Team"))

teamboxscoredata <- dplyr::select(teamboxscoredata,GameID,LastName,FirstLetter,Players,MIN,FG,`3PT`,FT,OREB,DREB,REB,AST,STL,BLK,TO,PF,`+/-`,PTS,Team,Starter,Position,Home,DNP,FGM,FGA,`3PM`,
                       `3PA`,FTM,FTA,`2PM`,`2PA`,`FG%`,`FT%`,`3P%`,`2P%`,GameID.x,GameDescription.x,elemplayercards,PlayerID,GameID.y,GameDescription.y,PlayersFull,Position5,ApostropheDash2,Match1,
                       Year,Month,Day,BoxScore,PlaybyPlay,Date,RegularSeason,Playoffs,Finals,Season,OREB2,DREB2,REB2,AST2,STL2,BLK2,TO2,PF2)

teamboxscoredataUPDATE <- teamboxscoredata
save(teamboxscoredataUPDATE, file = paste("NBAteamboxscoresRECENTUPDATE.RData", sep=""))
teamboxscoredataMASTER <- teamboxscoredataMASTER[!(teamboxscoredataMASTER$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
teamboxscoredataMASTER <- rbind(teamboxscoredataMASTER,teamboxscoredataUPDATE)
teamboxscoredata <- teamboxscoredataMASTER
save(teamboxscoredata, file = paste("master_teamboxscores.RData", sep=""))

