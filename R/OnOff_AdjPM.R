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
library(fastDummies)
#setwd(working)

load("master_detailed_teambox.RData")
load("master_adjplusminus.RData")
load("master_detailedboxscore.RData")
load("master_teamboxscores.RData")
load("master_boxscores.RData")
load("coefALL.RData")
load("coef2ALL.RData")
load("rollingboxALL.RData")
load("playerages.RData")
load("playersnames.RData")

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())

#if you need to make playersnames and playersages you can use code below

playersnames2 <- distinct(boxscoredata,PlayerID,.keep_all=TRUE) %>%
  dplyr::select(.,PlayerID,Players,PlayersFull)
playersnames2 <- playersnames2[!(playersnames2$PlayerID %in% playersnames$PlayerID),]
#get players ages and draft info from espn.com
j <- 1
playerages2 <- playersnames2
playerages2$dob <- NA
playerages2$draft <- NA
if (length(playerages2$PlayerID)>0) {
for (i in playerages2$PlayerID) {
      url_date <- paste("https://www.espn.com/nba/player/bio/_/id/",i,sep="")
      webpage <- read_html(url_date)
      name1 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(1) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name2 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(2) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name3 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(3) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name4 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(4) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name5 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(5) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name6 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(6) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name7 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(7) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name8 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(8) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name9 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(9) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()
      name10 <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(10) > div > span.Bio__Label.ttu.mr2.dib.clr-gray-04') %>% html_text()

      dob <- NA
      draft <- NA
      if(length(name1)>0) if(name1=="DOB"|name1=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(1) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name2)>0) if(name2=="DOB"|name2=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(2) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name3)>0) if(name3=="DOB"|name3=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(3) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name4)>0) if(name4=="DOB"|name4=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(4) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name5)>0) if(name5=="DOB"|name5=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(5) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name6)>0) if(name6=="DOB"|name6=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(6) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name7)>0) if(name7=="DOB"|name7=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(7) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name8)>0) if(name8=="DOB"|name8=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(8) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name9)>0) if(name9=="DOB"|name9=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(9) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name10)>0) if(name10=="DOB"|name10=="Birthdate") dob <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(10) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()

      if(length(name1)>0) if(name1=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(1) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name2)>0) if(name2=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(2) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name3)>0) if(name3=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(3) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name4)>0) if(name4=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(4) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name5)>0) if(name5=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(5) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name6)>0) if(name6=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(6) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name7)>0) if(name7=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(7) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name8)>0) if(name8=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(8) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name9)>0) if(name9=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(9) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()
      if(length(name10)>0) if(name10=="Draft Info") draft <- html_nodes(webpage,'#fittPageContainer > div.StickyContainer > div:nth-child(5) > div > div > section.Card.Bio > div > div:nth-child(10) > div > span.dib.flex-uniform.mr3.clr-gray-01') %>% html_text()

      playerages2$dob[j] <- dob
      playerages2$draft[j] <- draft
      print(j)
      j <- j+1


}
  
  playersnames <- rbind(playersnames,playersnames2)
  playerages <- rbind(playerages,playerages2[,c(1:5)])
  save(playerages,file="playerages.RData")
  save(playersnames,file="playersnames.RData")
  
}
playerages$dob2 <- as.Date(playerages$dob,"%m/%d/%Y")
playerages$firstseason <- paste(as.numeric(substr(playerages$draft,1,4)),"-",substr(as.numeric(substr(playerages$draft,1,4))+1,3,4),sep="")
playerages$Round <- as.numeric(substr(playerages$draft,unlist(gregexpr("Rd ",playerages$draft))+2,unlist(gregexpr("Rd ",playerages$draft))+3))
playerages$Pick <- as.numeric(substr(playerages$draft,unlist(gregexpr("Pk ",playerages$draft))+2,unlist(gregexpr("Pk ",playerages$draft))+4))
playerages$PickNum <- playerages$Pick
#create a draft value based on 538 estimation of average 5y value of picks
playerages$draftvalue <- 1
playerages$draftvalue[playerages$PickNum==1] <- 6.5
playerages$draftvalue[playerages$PickNum==2] <- 5.5
playerages$draftvalue[playerages$PickNum==3] <- 4.8
playerages$draftvalue[playerages$PickNum==4] <- 4.4
playerages$draftvalue[playerages$PickNum==5] <- 4
playerages$draftvalue[playerages$PickNum==6] <- 19/5
playerages$draftvalue[playerages$PickNum==7] <- 18/5
playerages$draftvalue[playerages$PickNum==8] <- 17/5
playerages$draftvalue[playerages$PickNum==9] <- 16/5
playerages$draftvalue[playerages$PickNum==10] <- 15/5
playerages$draftvalue[playerages$PickNum==11] <- 14/5
playerages$draftvalue[playerages$PickNum==12] <- 13.5/5
playerages$draftvalue[playerages$PickNum==13] <- 13/5
playerages$draftvalue[playerages$PickNum==14] <- 12.5/5
playerages$draftvalue[playerages$PickNum==15] <- 12/5
playerages$draftvalue[playerages$PickNum==16] <- 12/5-(1/15)
playerages$draftvalue[playerages$PickNum==17] <- 12/5-2*(1/15)
playerages$draftvalue[playerages$PickNum==18] <- 12/5-3*(1/15)
playerages$draftvalue[playerages$PickNum==19] <- 12/5-4*(1/15)
playerages$draftvalue[playerages$PickNum==20] <- 12/5-5*(1/15)
playerages$draftvalue[playerages$PickNum==21] <- 12/5-6*(1/15)
playerages$draftvalue[playerages$PickNum==22] <- 12/5-7*(1/15)
playerages$draftvalue[playerages$PickNum==23] <- 12/5-8*(1/15)
playerages$draftvalue[playerages$PickNum==24] <- 12/5-9*(1/15)
playerages$draftvalue[playerages$PickNum==25] <- 12/5-10*(1/15)
playerages$draftvalue[playerages$PickNum==26] <- 12/5-11*(1/15)
playerages$draftvalue[playerages$PickNum==27] <- 12/5-12*(1/15)
playerages$draftvalue[playerages$PickNum==28] <- 12/5-13*(1/15)
playerages$draftvalue[playerages$PickNum==29] <- 12/5-14*(1/15)
playerages$draftvalue[playerages$PickNum==30] <- 12/5-15*(1/15)
playerages$draftvalue[playerages$PickNum>30&playerages$PickNum<=60] <- 1.1


boxscoredata <- boxscoredata[!is.na(boxscoredata$MIN),]

teamboxscoredata <- teamboxscoredata[order(teamboxscoredata$GameID,teamboxscoredata$Team),]
teambox1 <- distinct(teamboxscoredata,GameID,.keep_all=TRUE)
teamboxscoredata <- teamboxscoredata[order(teamboxscoredata$GameID,-teamboxscoredata$Team),]
teambox2 <- distinct(teamboxscoredata,GameID,.keep_all=TRUE)
teambox1a <- left_join(teambox1,teambox2,by=c("GameID"))
teambox2a <- left_join(teambox2,teambox1,by=c("GameID"))
teambox <- rbind(teambox1a,teambox2a)
teambox$MIN.x <- as.numeric(teambox$MIN.x)
teambox$MIN.y <- as.numeric(teambox$MIN.y)
teambox$MIN.x <- 48
teambox$MIN.y <- 48
boxscoredata <- boxscoredata %>% left_join(dplyr::select(dplyr::rename(teambox,Team=Team.x),GameID,Team,Team.y,MIN.x,OREB.x,DREB.x,REB.x,STL.x,BLK.x,AST.x,TO.x,PF.x,PTS.x,FGA.x,FGM.x,`3PA.x`,`3PM.x`,`2PA.x`,`2PM.x`,FTA.x,FTM.x,MIN.y,OREB.y,DREB.y,REB.y,STL.y,BLK.y,AST.y,TO.y,PF.y,PTS.y,FGA.y,FGM.y,`3PA.y`,`3PM.y`,`2PA.y`,`2PM.y`,FTA.y,FTM.y),by=c("GameID","Team"))       
boxscoredata <- distinct(boxscoredata)
boxscoredata$teamplusmin <- boxscoredata$PTS.x-boxscoredata$PTS.y
datevec <- as.Date(seq(as.Date("2004-11-08",origin="1970-01-01"),as.Date((today-1),origin="1970-01-01"),by=14),origin="1970-01-01")
datevec <- datevec[(month(datevec)!=7&month(datevec)!=8&month(datevec)!=9&month(datevec)!=10)|(month(datevec)==10&day(datevec)==31&year(datevec)==2022)]
datevec_rollbox <- datevec[!datevec %in% unique(rollingboxALL$Date)]
datevec_apm1 <- datevec[!datevec %in% unique(coefALL$Date)]
datevec_apm2 <- datevec[!datevec %in% unique(coef2ALL$Date)]
# datevec_apm1 <- datevec[datevec==max(unique(coefALL$Date))]
# datevec_apm2 <- datevec[datevec==max(unique(coef2ALL$Date))]
# datevec_apm1 <- datevec[datevec %in% unique(coefALL$Date)]
# datevec_apm2 <- datevec[datevec %in% unique(coef2ALL$Date)]

if (length(datevec_rollbox)>0) {
rollingboxALL_master <- rollingboxALL
func_rollingbox <- function(j) {
  rollingbox5 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-(365*5),] %>%
    dplyr::group_by(PlayerID) %>%
    dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
              MIN = sum(MIN,na.rm=TRUE),
              Games = MIN/MINPG,
              Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
              OREB = sum(OREB,na.rm=TRUE),
              DREB = sum(DREB,na.rm=TRUE),
              REB = sum(REB,na.rm=TRUE),
              STL = sum(STL,na.rm=TRUE),
              BLK = sum(BLK,na.rm=TRUE),
              AST = sum(AST,na.rm=TRUE),
              TO = sum(TO,na.rm=TRUE),
              Fouls = sum(PF,na.rm= TRUE),
              PTS = sum(PTS,na.rm=TRUE),
              Teams = paste(unique(Team),collapse = ","),
              Plusminus = sum(`+/-`,na.rm= TRUE),
              FGA = sum(FGA,na.rm=TRUE),
              FGM = sum(FGM,na.rm=TRUE),
              ThreePA = sum(`3PA`,na.rm=TRUE),
              ThreePM = sum(`3PM`,na.rm=TRUE),
              TwoPA = sum(`2PA`,na.rm=TRUE),
              TwoPM = sum(`2PM`,na.rm= TRUE),
              FTA = sum(FTA,na.rm=TRUE),
              FTM = sum(FTM,na.rm=TRUE),
              PG = sum(Position5=="PG",na.rm=TRUE),
              SG = sum(Position5=="SG",na.rm=TRUE),
              SF = sum(Position5=="SF",na.rm=TRUE),
              PF = sum(Position5=="PF",na.rm=TRUE),
              G = sum(Position=="G",na.rm=TRUE),
              F = sum(Position=="F",na.rm=TRUE),
              C = sum(Position=="C",na.rm=TRUE),
              
              MIN_team = sum(MIN.x,na.rm=TRUE),
              OREB_team = sum(OREB.x,na.rm=TRUE),
              DREB_team = sum(DREB.x,na.rm=TRUE),
              REB_team = sum(REB.x,na.rm=TRUE),
              STL_team = sum(STL.x,na.rm=TRUE),
              BLK_team = sum(BLK.x,na.rm=TRUE),
              AST_team = sum(AST.x,na.rm=TRUE),
              TO_team = sum(TO.x,na.rm=TRUE),
              PF_team = sum(PF.x,na.rm= TRUE),
              PTS_team = sum(PTS.x,na.rm=TRUE),
              Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
              FGA_team = sum(FGA.x,na.rm=TRUE),
              FGM_team = sum(FGM.x,na.rm=TRUE),
              ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
              ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
              TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
              TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
              FTA_team = sum(FTA.x,na.rm=TRUE),
              FTM_team = sum(FTM.x,na.rm=TRUE),
              
              MIN_opp = sum(MIN.y,na.rm=TRUE),
              OREB_opp = sum(OREB.y,na.rm=TRUE),
              DREB_opp = sum(DREB.y,na.rm=TRUE),
              REB_opp = sum(REB.y,na.rm=TRUE),
              STL_opp = sum(STL.y,na.rm=TRUE),
              BLK_opp = sum(BLK.y,na.rm=TRUE),
              AST_opp = sum(AST.y,na.rm=TRUE),
              TO_opp = sum(TO.y,na.rm=TRUE),
              PF_opp = sum(PF.y,na.rm = TRUE),
              PTS_opp = sum(PTS.y,na.rm=TRUE),
              FGA_opp = sum(FGA.y,na.rm=TRUE),
              FGM_opp = sum(FGM.y,na.rm=TRUE),
              ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
              ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
              TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
              TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
              FTA_opp = sum(FTA.y,na.rm=TRUE),
              FTM_opp = sum(FTM.y,na.rm=TRUE),
              
              FGApm = (FGA*36)/MIN,
              FGMpm = (FGM*36)/MIN,
              ThreePApm = (ThreePA*36)/MIN,
              ThreePMpm = (ThreePM*36)/MIN,
              TwoPApm = (TwoPA*36)/MIN,
              TwoPMpm = (TwoPM*36)/MIN,
              FTApm = (FTA*36)/MIN,
              FTMpm= (FTM*36)/MIN,
              REBpm= (REB*36)/MIN,
              OREBpm= (OREB*36)/MIN,
              DREBpm= (DREB*36)/MIN,
              STLpm= (STL*36)/MIN,
              BLKpm= (BLK*36)/MIN,
              ASTpm= (AST*36)/MIN,
              TOpm= (TO*36)/MIN,
              PFpm= (Fouls*36)/MIN,
              Plusminuspm= (Plusminus*36)/MIN,
              PTSpm= (PTS*36)/MIN,
              ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + Fouls,
              TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
              AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
              ImpactPct = ImpactPlays/AdjTeamImpPlays,
              TotalImpact = ImpactPlays/TeamImpactPlays,
              ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
              TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
              AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
              ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
              TotalImpact_off = ImpactPlays/TeamImpactPlays,
              ImpactPlays_def = BLK + STL + Fouls + DREB,
              TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
              AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
              ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
              TotalImpact_def = ImpactPlays/TeamImpactPlays,
              ImpactPlayspm= (ImpactPlays*36)/MIN,
              ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
              ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
              FGPct = FGM/FGA,
              ThreePct = ThreePM/ThreePA,
              TwoPct = TwoPM/TwoPA,
              FTPct = FTM/FTA,
              FTMperFG = FTM/FGA,
              FTAperFG = FTA/FGA,
              EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
              TSPct = (PTS/(FGA*2+.44*FTA*2)),
              OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
              DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
              OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
              REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
              TOPct = TO/(FGA+.44*FTA+TO)) %>%
    left_join(playersnames,by=c("PlayerID")) %>%
    dplyr::select(.,PlayerID,Players,PlayersFull,everything())
  
  rollingbox5 <- dplyr::select(rollingbox5,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
                               ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
                               EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct,PFpm)
  rollingbox5$Date <- as.Date(j,origin="1970-01-01")
  
  rollingbox3 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-(365*3),] %>%
    dplyr::group_by(PlayerID) %>%
    dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
              MIN = sum(MIN,na.rm=TRUE),
              Games = MIN/MINPG,
              Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
              OREB = sum(OREB,na.rm=TRUE),
              DREB = sum(DREB,na.rm=TRUE),
              REB = sum(REB,na.rm=TRUE),
              STL = sum(STL,na.rm=TRUE),
              BLK = sum(BLK,na.rm=TRUE),
              AST = sum(AST,na.rm=TRUE),
              TO = sum(TO,na.rm=TRUE),
              Fouls = sum(PF,na.rm= TRUE),
              PTS = sum(PTS,na.rm=TRUE),
              Teams = paste(unique(Team),collapse = ","),
              Plusminus = sum(`+/-`,na.rm= TRUE),
              FGA = sum(FGA,na.rm=TRUE),
              FGM = sum(FGM,na.rm=TRUE),
              ThreePA = sum(`3PA`,na.rm=TRUE),
              ThreePM = sum(`3PM`,na.rm=TRUE),
              TwoPA = sum(`2PA`,na.rm=TRUE),
              TwoPM = sum(`2PM`,na.rm= TRUE),
              FTA = sum(FTA,na.rm=TRUE),
              FTM = sum(FTM,na.rm=TRUE),
              PG = sum(Position5=="PG",na.rm=TRUE),
              SG = sum(Position5=="SG",na.rm=TRUE),
              SF = sum(Position5=="SF",na.rm=TRUE),
              PF = sum(Position5=="PF",na.rm=TRUE),
              G = sum(Position=="G",na.rm=TRUE),
              F = sum(Position=="F",na.rm=TRUE),
              C = sum(Position=="C",na.rm=TRUE),
              
              MIN_team = sum(MIN.x,na.rm=TRUE),
              OREB_team = sum(OREB.x,na.rm=TRUE),
              DREB_team = sum(DREB.x,na.rm=TRUE),
              REB_team = sum(REB.x,na.rm=TRUE),
              STL_team = sum(STL.x,na.rm=TRUE),
              BLK_team = sum(BLK.x,na.rm=TRUE),
              AST_team = sum(AST.x,na.rm=TRUE),
              TO_team = sum(TO.x,na.rm=TRUE),
              PF_team = sum(PF.x,na.rm= TRUE),
              PTS_team = sum(PTS.x,na.rm=TRUE),
              Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
              FGA_team = sum(FGA.x,na.rm=TRUE),
              FGM_team = sum(FGM.x,na.rm=TRUE),
              ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
              ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
              TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
              TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
              FTA_team = sum(FTA.x,na.rm=TRUE),
              FTM_team = sum(FTM.x,na.rm=TRUE),
              
              MIN_opp = sum(MIN.y,na.rm=TRUE),
              OREB_opp = sum(OREB.y,na.rm=TRUE),
              DREB_opp = sum(DREB.y,na.rm=TRUE),
              REB_opp = sum(REB.y,na.rm=TRUE),
              STL_opp = sum(STL.y,na.rm=TRUE),
              BLK_opp = sum(BLK.y,na.rm=TRUE),
              AST_opp = sum(AST.y,na.rm=TRUE),
              TO_opp = sum(TO.y,na.rm=TRUE),
              PF_opp = sum(PF.y,na.rm = TRUE),
              PTS_opp = sum(PTS.y,na.rm=TRUE),
              FGA_opp = sum(FGA.y,na.rm=TRUE),
              FGM_opp = sum(FGM.y,na.rm=TRUE),
              ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
              ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
              TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
              TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
              FTA_opp = sum(FTA.y,na.rm=TRUE),
              FTM_opp = sum(FTM.y,na.rm=TRUE),
              
              FGApm = (FGA*36)/MIN,
              FGMpm = (FGM*36)/MIN,
              ThreePApm = (ThreePA*36)/MIN,
              ThreePMpm = (ThreePM*36)/MIN,
              TwoPApm = (TwoPA*36)/MIN,
              TwoPMpm = (TwoPM*36)/MIN,
              FTApm = (FTA*36)/MIN,
              FTMpm= (FTM*36)/MIN,
              REBpm= (REB*36)/MIN,
              OREBpm= (OREB*36)/MIN,
              DREBpm= (DREB*36)/MIN,
              STLpm= (STL*36)/MIN,
              BLKpm= (BLK*36)/MIN,
              ASTpm= (AST*36)/MIN,
              TOpm= (TO*36)/MIN,
              PFpm= (Fouls*36)/MIN,
              Plusminuspm= (Plusminus*36)/MIN,
              PTSpm= (PTS*36)/MIN,
              ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + Fouls,
              TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
              AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
              ImpactPct = ImpactPlays/AdjTeamImpPlays,
              TotalImpact = ImpactPlays/TeamImpactPlays,
              ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
              TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
              AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
              ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
              TotalImpact_off = ImpactPlays/TeamImpactPlays,
              ImpactPlays_def = BLK + STL + Fouls + DREB,
              TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
              AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
              ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
              TotalImpact_def = ImpactPlays/TeamImpactPlays,
              ImpactPlayspm= (ImpactPlays*36)/MIN,
              ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
              ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
              FGPct = FGM/FGA,
              ThreePct = ThreePM/ThreePA,
              TwoPct = TwoPM/TwoPA,
              FTPct = FTM/FTA,
              FTMperFG = FTM/FGA,
              FTAperFG = FTA/FGA,
              EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
              TSPct = (PTS/(FGA*2+.44*FTA*2)),
              OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
              DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
              OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
              REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
              TOPct = TO/(FGA+.44*FTA+TO)) %>%
    left_join(playersnames,by=c("PlayerID")) %>%
    dplyr::select(.,PlayerID,Players,PlayersFull,everything())
  
  rollingbox3 <- dplyr::select(rollingbox3,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
                               ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
                               EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct,PFpm)
  rollingbox3$Date <- as.Date(j,origin="1970-01-01")
  
  rollingbox1 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-365,] %>%
    dplyr::group_by(PlayerID) %>%
    dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
              MIN = sum(MIN,na.rm=TRUE),
              Games = MIN/MINPG,
              Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
              OREB = sum(OREB,na.rm=TRUE),
              DREB = sum(DREB,na.rm=TRUE),
              REB = sum(REB,na.rm=TRUE),
              STL = sum(STL,na.rm=TRUE),
              BLK = sum(BLK,na.rm=TRUE),
              AST = sum(AST,na.rm=TRUE),
              TO = sum(TO,na.rm=TRUE),
              Fouls = sum(PF,na.rm= TRUE),
              PTS = sum(PTS,na.rm=TRUE),
              Teams = paste(unique(Team),collapse = ","),
              Plusminus = sum(`+/-`,na.rm= TRUE),
              FGA = sum(FGA,na.rm=TRUE),
              FGM = sum(FGM,na.rm=TRUE),
              ThreePA = sum(`3PA`,na.rm=TRUE),
              ThreePM = sum(`3PM`,na.rm=TRUE),
              TwoPA = sum(`2PA`,na.rm=TRUE),
              TwoPM = sum(`2PM`,na.rm= TRUE),
              FTA = sum(FTA,na.rm=TRUE),
              FTM = sum(FTM,na.rm=TRUE),
              PG = sum(Position5=="PG",na.rm=TRUE),
              SG = sum(Position5=="SG",na.rm=TRUE),
              SF = sum(Position5=="SF",na.rm=TRUE),
              PF = sum(Position5=="PF",na.rm=TRUE),
              G = sum(Position=="G",na.rm=TRUE),
              F = sum(Position=="F",na.rm=TRUE),
              C = sum(Position=="C",na.rm=TRUE),
              
              MIN_team = sum(MIN.x,na.rm=TRUE),
              OREB_team = sum(OREB.x,na.rm=TRUE),
              DREB_team = sum(DREB.x,na.rm=TRUE),
              REB_team = sum(REB.x,na.rm=TRUE),
              STL_team = sum(STL.x,na.rm=TRUE),
              BLK_team = sum(BLK.x,na.rm=TRUE),
              AST_team = sum(AST.x,na.rm=TRUE),
              TO_team = sum(TO.x,na.rm=TRUE),
              PF_team = sum(PF.x,na.rm= TRUE),
              PTS_team = sum(PTS.x,na.rm=TRUE),
              Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
              FGA_team = sum(FGA.x,na.rm=TRUE),
              FGM_team = sum(FGM.x,na.rm=TRUE),
              ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
              ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
              TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
              TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
              FTA_team = sum(FTA.x,na.rm=TRUE),
              FTM_team = sum(FTM.x,na.rm=TRUE),
              
              MIN_opp = sum(MIN.y,na.rm=TRUE),
              OREB_opp = sum(OREB.y,na.rm=TRUE),
              DREB_opp = sum(DREB.y,na.rm=TRUE),
              REB_opp = sum(REB.y,na.rm=TRUE),
              STL_opp = sum(STL.y,na.rm=TRUE),
              BLK_opp = sum(BLK.y,na.rm=TRUE),
              AST_opp = sum(AST.y,na.rm=TRUE),
              TO_opp = sum(TO.y,na.rm=TRUE),
              PF_opp = sum(PF.y,na.rm = TRUE),
              PTS_opp = sum(PTS.y,na.rm=TRUE),
              FGA_opp = sum(FGA.y,na.rm=TRUE),
              FGM_opp = sum(FGM.y,na.rm=TRUE),
              ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
              ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
              TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
              TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
              FTA_opp = sum(FTA.y,na.rm=TRUE),
              FTM_opp = sum(FTM.y,na.rm=TRUE),
              
              FGApm = (FGA*36)/MIN,
              FGMpm = (FGM*36)/MIN,
              ThreePApm = (ThreePA*36)/MIN,
              ThreePMpm = (ThreePM*36)/MIN,
              TwoPApm = (TwoPA*36)/MIN,
              TwoPMpm = (TwoPM*36)/MIN,
              FTApm = (FTA*36)/MIN,
              FTMpm= (FTM*36)/MIN,
              REBpm= (REB*36)/MIN,
              OREBpm= (OREB*36)/MIN,
              DREBpm= (DREB*36)/MIN,
              STLpm= (STL*36)/MIN,
              BLKpm= (BLK*36)/MIN,
              ASTpm= (AST*36)/MIN,
              TOpm= (TO*36)/MIN,
              PFpm= (Fouls*36)/MIN,
              Plusminuspm= (Plusminus*36)/MIN,
              PTSpm= (PTS*36)/MIN,
              ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + Fouls,
              TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
              AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
              ImpactPct = ImpactPlays/AdjTeamImpPlays,
              TotalImpact = ImpactPlays/TeamImpactPlays,
              ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
              TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
              AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
              ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
              TotalImpact_off = ImpactPlays/TeamImpactPlays,
              ImpactPlays_def = BLK + STL + Fouls + DREB,
              TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
              AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
              ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
              TotalImpact_def = ImpactPlays/TeamImpactPlays,
              ImpactPlayspm= (ImpactPlays*36)/MIN,
              ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
              ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
              FGPct = FGM/FGA,
              ThreePct = ThreePM/ThreePA,
              TwoPct = TwoPM/TwoPA,
              FTPct = FTM/FTA,
              FTMperFG = FTM/FGA,
              FTAperFG = FTA/FGA,
              EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
              TSPct = (PTS/(FGA*2+.44*FTA*2)),
              OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
              DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
              OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
              REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
              TOPct = TO/(FGA+.44*FTA+TO)) %>%
    left_join(playersnames,by=c("PlayerID")) %>%
    dplyr::select(.,PlayerID,Players,PlayersFull,everything())
  
  rollingbox1 <- dplyr::select(rollingbox1,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
                               ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
                               EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct,PFpm)
  rollingbox1$Date <- as.Date(j,origin="1970-01-01")
  
  rollingbox <- rollingbox5 %>% left_join(rollingbox3,by=c("PlayerID","Players","PlayersFull","Date")) %>% left_join(rollingbox1,by=c("PlayerID","Players","PlayersFull","Date"))
  return(rollingbox)
}
tic()
numCores <- detectCores()
#rollingboxALL <- lapply(as.Date(datevec_rollbox,origin="1970-01-01"), func_rollingbox)
rollingboxALL <- mclapply(as.Date(datevec_rollbox,origin="1970-01-01"), func_rollingbox,mc.cores = max(numCores-4,1))
rollingboxALL <- dplyr::bind_rows(rollingboxALL, .id = "column_label")
toc()


#.x is 5y, .y is 3y and none is 1y
rollingboxALL <- rollingboxALL[order(rollingboxALL$PlayerID,rollingboxALL$Date),]
#fill in 1y data if there is 5y data
rollingboxALL <- data.frame(rollingboxALL)
rollingboxALL[is.na(rollingboxALL$MIN),names(rollingboxALL)[110:161]] <- rollingboxALL[is.na(rollingboxALL$MIN),names(rollingboxALL)[58:109]]
rollingboxALL$Season <- ifelse((month(rollingboxALL$Date)>=10&day(rollingboxALL$Date)>=12)|month(rollingboxALL$Date)>=11,paste(year(rollingboxALL$Date),"-",substr(year(rollingboxALL$Date)+1,3,4),sep=""),paste(year(rollingboxALL$Date)-1,"-",substr(year(rollingboxALL$Date),3,4),sep=""))
rollingboxALL <- rollingboxALL %>% left_join(dplyr::select(playerages,PlayerID,dob2,draftvalue,firstseason),by=c("PlayerID"))
rollingboxALL$firstyear <- as.numeric(substr(rollingboxALL$firstseason,1,4))
rollingboxALL$currentyear <- as.numeric(substr(rollingboxALL$Season,1,4))
rollingboxALL <- rollingboxALL %>%
  dplyr::group_by(PlayerID) %>%
  dplyr::mutate(firstyear2 = min(year(Date)))  %>%
  ungroup()
rollingboxALL$firstyear[is.na(rollingboxALL$firstyear)] <- rollingboxALL$firstyear2[is.na(rollingboxALL$firstyear)]
rollingboxALL$experience <- rollingboxALL$currentyear - rollingboxALL$firstyear
rollingboxALL$age <- as.numeric((rollingboxALL$Date - rollingboxALL$dob2))/365
rollingboxALL$Guard <- ifelse((rollingboxALL$G.x>=rollingboxALL$`F.x`)&(rollingboxALL$G.x>=rollingboxALL$`C.x`),1,0)
rollingboxALL$Forward <- ifelse((rollingboxALL$`F.x`>=rollingboxALL$`C.x`)&(rollingboxALL$`F.x`>rollingboxALL$G.x),1,0)
rollingboxALL$Center <- ifelse((rollingboxALL$C.x>rollingboxALL$`F.x`)&(rollingboxALL$C.x>rollingboxALL$G.x),1,0)

#later we use bayesian updating to fill in percentages here
rollingboxALL$LowFT <- rollingboxALL$FTApm.x*(rollingboxALL$MIN.x/36)<20
rollingboxALL$Low3 <- rollingboxALL$ThreePApm.x*(rollingboxALL$MIN.x/36)<20
rollingboxALL <- rollingboxALL[!(is.na(rollingboxALL$LowFT)|is.na(rollingboxALL$Low3)),]

#create regression variables
rollingboxALL <- rollingboxALL %>%
  dplyr::mutate(MINPG_reg = ((MIN.x-MIN.y)+(MIN.y-MIN)*1.5+(MIN)*2)/((Games.x-Games.y)+(Games.y-Games)*1.5+Games*2),
         Guard_reg = Guard,
         Forward_reg = Forward,
         Center_reg = Center,
         ThreePct_reg = ((ThreePMpm.x*MIN.x-ThreePMpm.y*MIN.y) + (ThreePMpm.y*MIN.y-ThreePMpm*MIN)*1.5 + ThreePMpm*MIN*2)/((ThreePApm.x*MIN.x-ThreePApm.y*MIN.y) + (ThreePApm.y*MIN.y-ThreePApm*MIN)*1.5 + ThreePApm*MIN*2),
         TwoPct_reg = ((TwoPMpm.x*MIN.x-TwoPMpm.y*MIN.y) + (TwoPMpm.y*MIN.y-TwoPMpm*MIN)*1.5 + TwoPMpm*MIN*2)/((TwoPApm.x*MIN.x-TwoPApm.y*MIN.y) + (TwoPApm.y*MIN.y-TwoPApm*MIN)*1.5 + TwoPApm*MIN*2),
         FTPct_reg = ((FTMpm.x*MIN.x-FTMpm.y*MIN.y) + (FTMpm.y*MIN.y-FTMpm*MIN)*1.5 + FTMpm*MIN*2)/((FTApm.x*MIN.x-FTApm.y*MIN.y) + (FTApm.y*MIN.y-FTApm*MIN)*1.5 + FTApm*MIN*2),
         PTSpm_reg = ((PTSpm.x*MIN.x-PTSpm.y*MIN.y) + (PTSpm.y*MIN.y-PTSpm*MIN)*1.5 + PTSpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         OREBpm_reg = ((OREBpm.x*MIN.x-OREBpm.y*MIN.y) + (OREBpm.y*MIN.y-OREBpm*MIN)*1.5 + OREBpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         DREBpm_reg = ((DREBpm.x*MIN.x-DREBpm.y*MIN.y) + (DREBpm.y*MIN.y-DREBpm*MIN)*1.5 + DREBpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         ASTpm_reg = ((ASTpm.x*MIN.x-ASTpm.y*MIN.y) + (ASTpm.y*MIN.y-ASTpm*MIN)*1.5 + ASTpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         STLpm_reg = ((STLpm.x*MIN.x-STLpm.y*MIN.y) + (STLpm.y*MIN.y-STLpm*MIN)*1.5 + STLpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         BLKpm_reg = ((BLKpm.x*MIN.x-BLKpm.y*MIN.y) + (BLKpm.y*MIN.y-BLKpm*MIN)*1.5 + BLKpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         TOpm_reg = ((TOpm.x*MIN.x-TOpm.y*MIN.y) + (TOpm.y*MIN.y-TOpm*MIN)*1.5 + TOpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         PFpm_reg = ((PFpm.x*MIN.x-PFpm.y*MIN.y) + (PFpm.y*MIN.y-PFpm*MIN)*1.5 + PFpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         ImpactPlayspm_reg = ((ImpactPlayspm.x*MIN.x-ImpactPlayspm.y*MIN.y) + (ImpactPlayspm.y*MIN.y-ImpactPlayspm*MIN)*1.5 + ImpactPlayspm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         ThreePMpm_reg = ((ThreePMpm.x*MIN.x-ThreePMpm.y*MIN.y) + (ThreePMpm.y*MIN.y-ThreePMpm*MIN)*1.5 + ThreePMpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         ThreePApm_reg = ((ThreePApm.x*MIN.x-ThreePApm.y*MIN.y) + (ThreePApm.y*MIN.y-ThreePApm*MIN)*1.5 + ThreePApm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         TwoPApm_reg = ((TwoPApm.x*MIN.x-TwoPApm.y*MIN.y) + (TwoPApm.y*MIN.y-TwoPApm*MIN)*1.5 + TwoPApm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         TwoPMpm_reg = ((TwoPMpm.x*MIN.x-TwoPMpm.y*MIN.y) + (TwoPMpm.y*MIN.y-TwoPMpm*MIN)*1.5 + TwoPMpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         FTApm_reg = ((FTApm.x*MIN.x-FTApm.y*MIN.y) + (FTApm.y*MIN.y-FTApm*MIN)*1.5 + FTApm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         FTMpm_reg = ((FTMpm.x*MIN.x-FTMpm.y*MIN.y) + (FTMpm.y*MIN.y-FTMpm*MIN)*1.5 + FTMpm*MIN*2)/((MIN.x-MIN.y) + (MIN.y-MIN)*1.5 + MIN*2),
         FTMperFG_reg = FTMpm_reg/(ThreePApm_reg+TwoPApm_reg),
         TSPct_reg = (3*ThreePMpm_reg+2*TwoPMpm_reg+FTMpm_reg*1)/(2*ThreePApm_reg+2*TwoPApm_reg+.44*2*FTApm_reg),
         Age_reg = age,
         Age24_reg = as.numeric(Age_reg<=24),
         Age30_reg = as.numeric(Age_reg<=30&Age_reg>24),
         AgeXAge24_reg = Age_reg*Age24_reg,
         AgeXAge30_reg = Age_reg*Age30_reg,
         Exp3_reg = as.numeric(experience<=3),
         DraftVal_reg = draftvalue,
         DraftValXExp3_reg = DraftVal_reg*Exp3_reg,
         NonShooter_reg = ThreePApm<.25,
         TotalImpactChg_reg = TotalImpact-TotalImpact.x,
         ImpChgXDraftVal_reg = DraftVal_reg*TotalImpactChg_reg,
         ImpChgXExp3_reg = TotalImpactChg_reg*Exp3_reg,
         ImpChgXExp3XDraftValue_reg = DraftVal_reg*TotalImpactChg_reg*Exp3_reg,
         PlusMinRaw5_reg = Plusminuspm.x,
         PlusMinRaw3_reg = Plusminuspm.y,
         PlusMinRaw1_reg = Plusminuspm)

#bayesian updating. For low FTs: beta (17,7.5). For Low 3s: beta(5.75,19.25)
rollingboxALL$ThreePct_reg[rollingboxALL$Low3==1] <- (5.75+rollingboxALL$ThreePMpm.x[rollingboxALL$Low3==1]*(rollingboxALL$MIN.x[rollingboxALL$Low3==1]/36))/(5.75+19.25+rollingboxALL$ThreePApm.x[rollingboxALL$Low3==1]*(rollingboxALL$MIN.x[rollingboxALL$Low3==1]/36))
rollingboxALL$FTPct_reg[rollingboxALL$LowFT==1] <- (17+rollingboxALL$FTMpm.x[rollingboxALL$LowFT==1]*(rollingboxALL$MIN.x[rollingboxALL$LowFT==1]/36))/(17+7.5+rollingboxALL$FTApm.x[rollingboxALL$LowFT==1]*(rollingboxALL$MIN.x[rollingboxALL$LowFT==1]/36))
rollingboxALL$NonShooterXThreePct_reg <- rollingboxALL$ThreePct_reg*rollingboxALL$NonShooter_reg

#replace all nans and inf with NA
rollingboxALL <- do.call(data.frame,                      # Replace Inf in data by NA
                               lapply(rollingboxALL,
                                      function(x) replace(x, is.infinite(x), NA)))
rollingboxALL <- do.call(data.frame,                      # Replace Inf in data by NA
                               lapply(rollingboxALL,
                                      function(x) replace(x, is.nan(x), NA)))

rollingboxALL$PlayerID2 <- paste(" ",rollingboxALL$PlayerID," ",sep="")
rollingboxALL$RAPM_Mult <- ifelse(!is.na(rollingboxALL$Pace.x),rollingboxALL$Pace.x/200,1)
rollingboxALL_master <- rollingboxALL_master[!(rollingboxALL_master$Date %in% datevec_rollbox),]
rollingboxALL <- dplyr::bind_rows(rollingboxALL_master,rollingboxALL)
}
#calculate Regularized Adjusted Plus Minus
adjplusminus <- adjplusminus %>% left_join(distinct(boxscoredata[,c("GameID","GameDescription.x","Date","Playoffs","Season")]),by=c("GameID"))
adjplusminus$Month <- month(adjplusminus$Date)
adjplusminus$Bubble <- ifelse(adjplusminus$Date>="2020-04-01"&adjplusminus$Date<="2020-10-20",1,0)
adjplusminus$Pandemic <- ifelse(adjplusminus$Date>="2020-12-01"&adjplusminus$Date<"2021-04-01",1,0)
adjplusminus <- dummy_cols(adjplusminus,select_columns=c("Season"))
adjplusminus$Month1 <- ifelse(adjplusminus$Month>=10&adjplusminus$Month<12,1,0)
adjplusminus$Month2 <- ifelse((adjplusminus$Month>=1&adjplusminus$Month<=2)|adjplusminus$Month==12,1,0)
adjplusminus$Month3 <- ifelse(adjplusminus$Month>=3&adjplusminus$Month<=4,1,0)
# adjplusminus$Team.x <- substr(adjplusminus$GameDescription.x,1,unlist(gregexpr(" at ",adjplusminus$GameDescription.x))-1)
# adjplusminus$Team.y <- substr(adjplusminus$GameDescription.x,unlist(gregexpr(" at ",adjplusminus$GameDescription.x))+4,nchar(adjplusminus$GameDescription.x))

func_apm <- function(j) {
  adjpm4_RAPM <- adjplusminus[adjplusminus$Date<=j&adjplusminus$Date>=(j-(365*5)),]
  
  adjpm2 <- adjpm4_RAPM
  adjpm2$PtsPM <- adjpm2$hpp_poss
  adjpm2$HomeAdj <- 1
  adjpm2$upby <- (adjpm2$starthomescore-adjpm2$startawayscore)
  adjpm2$upby10 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>10,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-10),-1,0))
  adjpm2$upby16 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>16,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-16),-1,0))
  adjpm2$upby25 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>25,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-25),-1,0))
  adjpm2$upbyX10 <- abs(adjpm2$upby10)*adjpm2$upby
  adjpm2$upbyX16 <- abs(adjpm2$upby16)*adjpm2$upby
  adjpm2$upbyX25 <- abs(adjpm2$upby25)*adjpm2$upby
  newnames1 <- str_replace_all(paste("OFF",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],sep="")," ","")
  newcols1 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],"')),1,0)",sep=""),collapse=","),")",sep="")))
  newnames2 <- str_replace_all(paste("DEF",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],sep="")," ","")
  newcols2 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],"')),-1,0)",sep=""),collapse=","),")",sep="")))
  adjpm2 <- cbind(adjpm2,newcols1,newcols2)
  names(adjpm2)[(length(names(adjpm2))-ncol(newcols1)-ncol(newcols2)+1):length(names(adjpm2))] <- c(newnames1,newnames2)
  adjpm2$dummyrestOFF <- 0
  adjpm2$dummyrestDEF <- 0
  for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x<=500&rollingboxALL$Date==j]) {
    adjpm2$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,i)),1+adjpm2$dummyrestOFF,adjpm2$dummyrestOFF)
    adjpm2$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,i)),adjpm2$dummyrestDEF-1,adjpm2$dummyrestDEF)
  }
  
  
  adjpm3 <- adjpm4_RAPM
  adjpm3$PtsPM <- adjpm3$app_poss
  adjpm3$HomeAdj <- -1
  adjpm3$upby <- (adjpm3$startawayscore-adjpm3$starthomescore)
  adjpm3$upby10 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>10,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-10),-1,0))
  adjpm3$upby16 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>16,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-16),-1,0))
  adjpm3$upby25 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>25,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-25),-1,0))
  adjpm3$upbyX10 <- abs(adjpm3$upby10)*adjpm3$upby
  adjpm3$upbyX16 <- abs(adjpm3$upby16)*adjpm3$upby
  adjpm3$upbyX25 <- abs(adjpm3$upby25)*adjpm3$upby
  newnames1 <- str_replace_all(paste("DEF",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],sep="")," ","")
  newcols1 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],"')),-1,0)",sep=""),collapse=","),")",sep="")))
  newnames2 <- str_replace_all(paste("OFF",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],sep="")," ","")
  newcols2 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN.x>500&rollingboxALL$Date==j],"')),1,0)",sep=""),collapse=","),")",sep="")))
  adjpm3 <- cbind(adjpm3,newcols1,newcols2)
  names(adjpm3)[(length(names(adjpm3))-ncol(newcols1)-ncol(newcols2)+1):length(names(adjpm3))] <- c(newnames1,newnames2)
  adjpm3$dummyrestOFF <- 0
  adjpm3$dummyrestDEF <- 0
  for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x<=500&rollingboxALL$Date==j]) {
    adjpm3$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,i)),1+adjpm3$dummyrestOFF,adjpm3$dummyrestOFF)
    adjpm3$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,i)),adjpm3$dummyrestDEF-1,adjpm3$dummyrestDEF)
  }
  
  adjpm4 <- dplyr::bind_rows(adjpm2,adjpm3)
  
  names(adjpm4) <- str_replace(names(adjpm4)," ","")
  names(adjpm4) <- str_replace(names(adjpm4)," ","")
  
  adjpm4_RAPM <- adjpm4
  
  wgt1 <- adjpm4_RAPM$poss
  wgt2 <- ifelse(adjpm4_RAPM$Date<j-365*3&adjpm4_RAPM$Date>=(j-(365*5)),1,ifelse(adjpm4_RAPM$Date<j-365*1&adjpm4_RAPM$Date>=(j-(365*3)),1.25,1.5))
  wgt3 <- wgt1*wgt2
  wgt4 <- ifelse(adjpm4_RAPM$Playoffs==1,wgt3*1.3,wgt3)
  wgt5 <- ifelse(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)<=15,1,pmax(1-.075*(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)-15),0.25))
  adjpm4_RAPM$wgt <- wgt4*wgt5
  adjpm4_RAPM <- adjpm4_RAPM[adjpm4_RAPM$poss_home>=1&adjpm4_RAPM$poss_away>=1&abs(adjpm4_RAPM$PtsPM)<=400,]
  adjpm4_RAPM$Playoffs2 <- adjpm4_RAPM$Playoffs
  adjpm4_RAPM <- adjpm4_RAPM[,c(149:length(names(adjpm4_RAPM)))]
  adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(adjpm4_RAPM,
                                function(x) replace(x, is.infinite(x), NA)))
  adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(adjpm4_RAPM,
                                function(x) replace(x, is.nan(x), NA)))

  
  adjpm4_RAPM <- adjpm4_RAPM[!is.na(adjpm4_RAPM$PtsPM),]
  adjpm4_RAPM$HomeXPlayoff <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Playoffs2
  adjpm4_RAPM$HomeXBubble <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Bubble
  adjpm4_RAPM$HomeXPandemic <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Pandemic
  adjpm4_RAPM$upby <- adjpm4_RAPM$upby + 1
  adjpm4_RAPM$upby10 <- adjpm4_RAPM$upby10 + 1
  adjpm4_RAPM$upby16 <- adjpm4_RAPM$upby16 + 1
  adjpm4_RAPM$upby25 <- adjpm4_RAPM$upby25 + 1
  adjpm4_RAPM$upbyX10 <- adjpm4_RAPM$upbyX10 + 1
  adjpm4_RAPM$upbyX16 <- adjpm4_RAPM$upbyX16 + 1
  adjpm4_RAPM$upbyX25 <- adjpm4_RAPM$upbyX25 + 1
  adjpm4_RAPM <- adjpm4_RAPM[,colSums(adjpm4_RAPM,na.rm=TRUE)!=0]
  adjpm4_RAPM$upby <- adjpm4_RAPM$upby - 1
  adjpm4_RAPM$upby10 <- adjpm4_RAPM$upby10 - 1
  adjpm4_RAPM$upby16 <- adjpm4_RAPM$upby16 - 1
  adjpm4_RAPM$upby25 <- adjpm4_RAPM$upby25 - 1
  adjpm4_RAPM$upbyX10 <- adjpm4_RAPM$upbyX10 - 1
  adjpm4_RAPM$upbyX16 <- adjpm4_RAPM$upbyX16 - 1
  adjpm4_RAPM$upbyX25 <- adjpm4_RAPM$upbyX25 - 1
  y <- adjpm4_RAPM$PtsPM
  wgt <- adjpm4_RAPM$wgt
  x <- data.matrix(dplyr::select(adjpm4_RAPM,-PtsPM,-wgt))
  
  # cv_model2_a <- cv.glmnet(x, y, alpha = 0,weight=wgt) #alpha = 0 is ridge regression
  # save_lambda <- cv_model2_a$lambda.min
  # save_lambda
  best_lambda2_a <- 16.97737
  best_model2_a <- glmnet(x, y, alpha = 0, lambda = best_lambda2_a,weight=wgt)
  test <- coef(best_model2_a)
  coef <- data.frame(name = test@Dimnames[[1]][test@i + 1], coefficient = test@x)
  coef_off <- coef[unlist(gregexpr("OFF",coef$name))>0,]
  coef_def <- coef[unlist(gregexpr("DEF",coef$name))>0,]
  coef_off$PlayerID <- substr(coef_off$name,4,nchar(coef_off$name))
  coef_def$PlayerID <- substr(coef_def$name,4,nchar(coef_def$name))
  coef_off <- dplyr::rename(coef_off,RAPM5_off=coefficient)
  coef_def <- dplyr::rename(coef_def,RAPM5_def=coefficient)
  coef_off$Date <- as.Date(j,origin="1970-01-01")
  coef_def$Date <- as.Date(j,origin="1970-01-01")
  coef_off <- dplyr::select(coef_off,-name)
  coef_def <- dplyr::select(coef_def,-name)
  coef <- full_join(coef_off,coef_def,by=c("PlayerID","Date"))
  coef <- dplyr::select(coef,PlayerID,Date,RAPM5_off,RAPM5_def)
  
  coef <- left_join (coef,playersnames,by=c("PlayerID"))
  coef_new <- rbind()
  coef$RAPM5_def[coef$PlayerID=="myrestOFF"] <- coef$RAPM5_def[coef$PlayerID=="myrestDEF"]
  coef$RAPM5_total <- coef$RAPM5_off+coef$RAPM5_def
  coef$PlayerID[coef$PlayerID=="myrestOFF"] <- "Replacement"
  coef <- coef[coef$PlayerID!="myrestDEF",]
  return(coef)
}
if (length(datevec_apm1)>0) {
  coefALL_master <- coefALL
tic()
numCores <- detectCores()
#coefALL <- lapply(as.Date(datevec_apm1,origin="1970-01-01"), func_apm)
coefALL <- mclapply(as.Date(datevec_apm1,origin="1970-01-01"), func_apm,mc.cores = max(numCores-4,1))
coefALL <- dplyr::bind_rows(coefALL, .id = "column_label")
toc()
coefALL_master <- coefALL_master[!(coefALL_master$Date %in% datevec_apm1),]
coefALL <- dplyr::bind_rows(coefALL_master,coefALL)

}

func_apm2 <- function(j) {
  adjpm4_RAPM <- adjplusminus[adjplusminus$Date<=j&adjplusminus$Date>=(j-(365)),]
  
  adjpm2 <- adjpm4_RAPM
  adjpm2$PtsPM <- adjpm2$hpp_poss
  adjpm2$HomeAdj <- 1
  adjpm2$upby <- (adjpm2$starthomescore-adjpm2$startawayscore)
  adjpm2$upby10 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>10,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-10),-1,0))
  adjpm2$upby16 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>16,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-16),-1,0))
  adjpm2$upby25 <- ifelse((adjpm2$starthomescore-adjpm2$startawayscore)>25,1,ifelse((adjpm2$starthomescore-adjpm2$startawayscore)<(-25),-1,0))
  adjpm2$upbyX10 <- abs(adjpm2$upby10)*adjpm2$upby
  adjpm2$upbyX16 <- abs(adjpm2$upby16)*adjpm2$upby
  adjpm2$upbyX25 <- abs(adjpm2$upby25)*adjpm2$upby
  newnames1 <- str_replace_all(paste("OFF",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],sep="")," ","")
  newcols1 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],"')),1,0)",sep=""),collapse=","),")",sep="")))
  newnames2 <- str_replace_all(paste("DEF",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],sep="")," ","")
  newcols2 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],"')),-1,0)",sep=""),collapse=","),")",sep="")))
  adjpm2 <- cbind(adjpm2,newcols1,newcols2)
  names(adjpm2)[(length(names(adjpm2))-ncol(newcols1)-ncol(newcols2)+1):length(names(adjpm2))] <- c(newnames1,newnames2)
  adjpm2$dummyrestOFF <- 0
  adjpm2$dummyrestDEF <- 0
  for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN<=300&rollingboxALL$Date==j]) {
    adjpm2$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,i)),1+adjpm2$dummyrestOFF,adjpm2$dummyrestOFF)
    adjpm2$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,i)),adjpm2$dummyrestDEF-1,adjpm2$dummyrestDEF)
  }
  
  
  adjpm3 <- adjpm4_RAPM
  adjpm3$PtsPM <- adjpm3$app_poss
  adjpm3$HomeAdj <- -1
  adjpm3$upby <- (adjpm3$startawayscore-adjpm3$starthomescore)
  adjpm3$upby10 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>10,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-10),-1,0))
  adjpm3$upby16 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>16,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-16),-1,0))
  adjpm3$upby25 <- ifelse((adjpm3$startawayscore-adjpm3$starthomescore)>25,1,ifelse((adjpm3$startawayscore-adjpm3$starthomescore)<(-25),-1,0))
  adjpm3$upbyX10 <- abs(adjpm3$upby10)*adjpm3$upby
  adjpm3$upbyX16 <- abs(adjpm3$upby16)*adjpm3$upby
  adjpm3$upbyX25 <- abs(adjpm3$upby25)*adjpm3$upby
  newnames1 <- str_replace_all(paste("DEF",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],sep="")," ","")
  newcols1 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],"')),-1,0)",sep=""),collapse=","),")",sep="")))
  newnames2 <- str_replace_all(paste("OFF",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],sep="")," ","")
  newcols2 <- eval(parse(text = paste0("cbind(",paste0(paste0("ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,'",rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j],"')),1,0)",sep=""),collapse=","),")",sep="")))
  adjpm3 <- cbind(adjpm3,newcols1,newcols2)
  names(adjpm3)[(length(names(adjpm3))-ncol(newcols1)-ncol(newcols2)+1):length(names(adjpm3))] <- c(newnames1,newnames2)
  adjpm3$dummyrestOFF <- 0
  adjpm3$dummyrestDEF <- 0
  for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN<=300&rollingboxALL$Date==j]) {
    adjpm3$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,i)),1+adjpm3$dummyrestOFF,adjpm3$dummyrestOFF)
    adjpm3$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,i)),adjpm3$dummyrestDEF-1,adjpm3$dummyrestDEF)
  }
  
  adjpm4 <- dplyr::bind_rows(adjpm2,adjpm3)
  
  names(adjpm4) <- str_replace(names(adjpm4)," ","")
  names(adjpm4) <- str_replace(names(adjpm4)," ","")
  
  adjpm4_RAPM <- adjpm4
  
  wgt1 <- adjpm4_RAPM$poss
  wgt2 <- ifelse(adjpm4_RAPM$Date<j-365*3&adjpm4_RAPM$Date>=(j-(365*5)),1,ifelse(adjpm4_RAPM$Date<j-365*1&adjpm4_RAPM$Date>=(j-(365*3)),1.25,1.5))
  wgt3 <- wgt1*wgt2
  wgt4 <- ifelse(adjpm4_RAPM$Playoffs==1,wgt3*1.3,wgt3)
  wgt5 <- ifelse(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)<=15,1,pmax(1-.075*(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)-15),.25))
  adjpm4_RAPM$wgt <- wgt4*wgt5
  adjpm4_RAPM <- adjpm4_RAPM[adjpm4_RAPM$poss_home>=1&adjpm4_RAPM$poss_away>=1&abs(adjpm4_RAPM$PtsPM)<=400,]
  adjpm4_RAPM$Playoffs2 <- adjpm4_RAPM$Playoffs
  adjpm4_RAPM <- adjpm4_RAPM[,c(149:length(names(adjpm4_RAPM)))]
  adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(adjpm4_RAPM,
                                function(x) replace(x, is.infinite(x), NA)))
  adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
                         lapply(adjpm4_RAPM,
                                function(x) replace(x, is.nan(x), NA)))
  
  
  adjpm4_RAPM <- adjpm4_RAPM[!is.na(adjpm4_RAPM$PtsPM),]
  adjpm4_RAPM$HomeXPlayoff <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Playoffs2
  adjpm4_RAPM$HomeXBubble <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Bubble
  adjpm4_RAPM$HomeXPandemic <- adjpm4_RAPM$HomeAdj*adjpm4_RAPM$Pandemic
  adjpm4_RAPM$upby <- adjpm4_RAPM$upby + 1
  adjpm4_RAPM$upby10 <- adjpm4_RAPM$upby10 + 1
  adjpm4_RAPM$upby16 <- adjpm4_RAPM$upby16 + 1
  adjpm4_RAPM$upby25 <- adjpm4_RAPM$upby25 + 1
  adjpm4_RAPM$upbyX10 <- adjpm4_RAPM$upbyX10 + 1
  adjpm4_RAPM$upbyX16 <- adjpm4_RAPM$upbyX16 + 1
  adjpm4_RAPM$upbyX25 <- adjpm4_RAPM$upbyX25 + 1
  adjpm4_RAPM <- adjpm4_RAPM[,colSums(adjpm4_RAPM,na.rm=TRUE)!=0]
  adjpm4_RAPM$upby <- adjpm4_RAPM$upby - 1
  adjpm4_RAPM$upby10 <- adjpm4_RAPM$upby10 - 1
  adjpm4_RAPM$upby16 <- adjpm4_RAPM$upby16 - 1
  adjpm4_RAPM$upby25 <- adjpm4_RAPM$upby25 - 1
  adjpm4_RAPM$upbyX10 <- adjpm4_RAPM$upbyX10 - 1
  adjpm4_RAPM$upbyX16 <- adjpm4_RAPM$upbyX16 - 1
  adjpm4_RAPM$upbyX25 <- adjpm4_RAPM$upbyX25 - 1
  y <- adjpm4_RAPM$PtsPM
  wgt <- adjpm4_RAPM$wgt
  x <- data.matrix(dplyr::select(adjpm4_RAPM,-PtsPM,-wgt))
  
  # cv_model2_a <- cv.glmnet(x, y, alpha = 0,weight=wgt) #alpha = 0 is ridge regression
  # save_lambda <- cv_model2_a$lambda.min
  best_lambda2_a <- 0
  best_model2_a <- glmnet(x, y, alpha = 0, lambda = best_lambda2_a,weight=wgt)
  test <- coef(best_model2_a)
  coef2 <- data.frame(name = test@Dimnames[[1]][test@i + 1], coefficient = test@x)
  coef2_off <- coef2[unlist(gregexpr("OFF",coef2$name))>0,]
  coef2_def <- coef2[unlist(gregexpr("DEF",coef2$name))>0,]
  coef2_off$PlayerID <- substr(coef2_off$name,4,nchar(coef2_off$name))
  coef2_def$PlayerID <- substr(coef2_def$name,4,nchar(coef2_def$name))
  coef2_off <- dplyr::rename(coef2_off,RAPM1_off=coefficient)
  coef2_def <- dplyr::rename(coef2_def,RAPM1_def=coefficient)
  coef2_off$Date <- as.Date(j,origin="1970-01-01")
  coef2_def$Date <- as.Date(j,origin="1970-01-01")
  coef2_off <- dplyr::select(coef2_off,-name)
  coef2_def <- dplyr::select(coef2_def,-name)
  coef2 <- full_join(coef2_off,coef2_def,by=c("PlayerID","Date"))
  coef2 <- dplyr::select(coef2,PlayerID,Date,RAPM1_off,RAPM1_def)
  
  coef2 <- left_join (coef2,playersnames,by=c("PlayerID"))
  coef2$RAPM1_def[coef2$PlayerID=="myrestOFF"] <- coef2$RAPM1_def[coef2$PlayerID=="myrestDEF"]
  coef2$RAPM1_total <- coef2$RAPM1_off+coef2$RAPM1_def
  coef2$PlayerID[coef2$PlayerID=="myrestOFF"] <- "Replacement"
  coef2 <- coef2[coef2$PlayerID!="myrestDEF",]
  return(coef2)
}

if (length(datevec_apm2)>0) {
  coef2ALL_master <- coef2ALL
tic()
numCores <- detectCores()
#coef2ALL <- lapply(as.Date(datevec_apm2,origin="1970-01-01"), func_apm2)
coef2ALL <- mclapply(as.Date(datevec_apm2,origin="1970-01-01"), func_apm2,mc.cores = max(numCores-4,1))
coef2ALL <- dplyr::bind_rows(coef2ALL, .id = "column_label")
toc()
coef2ALL_master <- coef2ALL_master[!(coef2ALL_master$Date %in% datevec_apm2),]
coef2ALL <- dplyr::bind_rows(coef2ALL_master,coef2ALL)

}

save(coefALL,file="coefALL.RData")
save(coef2ALL,file="coef2ALL.RData")
save(rollingboxALL,file="rollingboxALL.RData")



# for (j in as.Date(datevec,origin="1970-01-01")) {
#   rollingbox5 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-(365*5),] %>%
#     dplyr::group_by(PlayerID) %>%
#     dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
#               MIN = sum(MIN,na.rm=TRUE),
#               Games = MIN/MINPG,
#               Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
#               OREB = sum(OREB,na.rm=TRUE),
#               DREB = sum(DREB,na.rm=TRUE),
#               REB = sum(REB,na.rm=TRUE),
#               STL = sum(STL,na.rm=TRUE),
#               BLK = sum(BLK,na.rm=TRUE),
#               AST = sum(AST,na.rm=TRUE),
#               TO = sum(TO,na.rm=TRUE),
#               PF = sum(PF,na.rm= TRUE),
#               PTS = sum(PTS,na.rm=TRUE),
#               Teams = paste(unique(Team),collapse = ","),
#               Plusminus = sum(`+/-`,na.rm= TRUE),
#               FGA = sum(FGA,na.rm=TRUE),
#               FGM = sum(FGM,na.rm=TRUE),
#               ThreePA = sum(`3PA`,na.rm=TRUE),
#               ThreePM = sum(`3PM`,na.rm=TRUE),
#               TwoPA = sum(`2PA`,na.rm=TRUE),
#               TwoPM = sum(`2PM`,na.rm= TRUE),
#               FTA = sum(FTA,na.rm=TRUE),
#               FTM = sum(FTM,na.rm=TRUE),
#               PG = sum(Position5=="PG",na.rm=TRUE),
#               SG = sum(Position5=="SG",na.rm=TRUE),
#               SF = sum(Position5=="SF",na.rm=TRUE),
#               PF = sum(Position5=="PF",na.rm=TRUE),
#               G = sum(Position=="G",na.rm=TRUE),
#               F = sum(Position=="F",na.rm=TRUE),
#               C = sum(Position=="C",na.rm=TRUE),
#               
#               MIN_team = sum(MIN.x,na.rm=TRUE),
#               OREB_team = sum(OREB.x,na.rm=TRUE),
#               DREB_team = sum(DREB.x,na.rm=TRUE),
#               REB_team = sum(REB.x,na.rm=TRUE),
#               STL_team = sum(STL.x,na.rm=TRUE),
#               BLK_team = sum(BLK.x,na.rm=TRUE),
#               AST_team = sum(AST.x,na.rm=TRUE),
#               TO_team = sum(TO.x,na.rm=TRUE),
#               PF_team = sum(PF.x,na.rm= TRUE),
#               PTS_team = sum(PTS.x,na.rm=TRUE),
#               Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
#               FGA_team = sum(FGA.x,na.rm=TRUE),
#               FGM_team = sum(FGM.x,na.rm=TRUE),
#               ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
#               ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
#               TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
#               TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
#               FTA_team = sum(FTA.x,na.rm=TRUE),
#               FTM_team = sum(FTM.x,na.rm=TRUE),
#               
#               MIN_opp = sum(MIN.y,na.rm=TRUE),
#               OREB_opp = sum(OREB.y,na.rm=TRUE),
#               DREB_opp = sum(DREB.y,na.rm=TRUE),
#               REB_opp = sum(REB.y,na.rm=TRUE),
#               STL_opp = sum(STL.y,na.rm=TRUE),
#               BLK_opp = sum(BLK.y,na.rm=TRUE),
#               AST_opp = sum(AST.y,na.rm=TRUE),
#               TO_opp = sum(TO.y,na.rm=TRUE),
#               PF_opp = sum(PF.y,na.rm = TRUE),
#               PTS_opp = sum(PTS.y,na.rm=TRUE),
#               FGA_opp = sum(FGA.y,na.rm=TRUE),
#               FGM_opp = sum(FGM.y,na.rm=TRUE),
#               ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
#               ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
#               TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
#               TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
#               FTA_opp = sum(FTA.y,na.rm=TRUE),
#               FTM_opp = sum(FTM.y,na.rm=TRUE),
#               
#               FGApm = (FGA*36)/MIN,
#               FGMpm = (FGM*36)/MIN,
#               ThreePApm = (ThreePA*36)/MIN,
#               ThreePMpm = (ThreePM*36)/MIN,
#               TwoPApm = (TwoPA*36)/MIN,
#               TwoPMpm = (TwoPM*36)/MIN,
#               FTApm = (FTA*36)/MIN,
#               FTMpm= (FTM*36)/MIN,
#               REBpm= (REB*36)/MIN,
#               OREBpm= (OREB*36)/MIN,
#               DREBpm= (DREB*36)/MIN,
#               STLpm= (STL*36)/MIN,
#               BLKpm= (BLK*36)/MIN,
#               ASTpm= (AST*36)/MIN,
#               TOpm= (TO*36)/MIN,
#               Plusminuspm= (Plusminus*36)/MIN,
#               PTSpm= (PTS*36)/MIN,
#               ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + PF,
#               TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
#               AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
#               ImpactPct = ImpactPlays/AdjTeamImpPlays,
#               TotalImpact = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
#               TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
#               AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
#               ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
#               TotalImpact_off = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_def = BLK + STL + PF + DREB,
#               TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
#               AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
#               ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
#               TotalImpact_def = ImpactPlays/TeamImpactPlays,
#               ImpactPlayspm= (ImpactPlays*36)/MIN,
#               ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
#               ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
#               FGPct = FGM/FGA,
#               ThreePct = ThreePM/ThreePA,
#               TwoPct = TwoPM/TwoPA,
#               FTPct = FTM/FTA,
#               FTMperFG = FTM/FGA,
#               FTAperFG = FTA/FGA,
#               EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
#               TSPct = (PTS/(FGA*2+.44*FTA*2)),
#               OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
#               DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
#               OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
#               REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
#               TOPct = TO/(FGA+.44*FTA+TO)) %>%
#     left_join(playersnames,by=c("PlayerID")) %>%
#     dplyr::select(.,PlayerID,Players,PlayersFull,everything())
#   
#   rollingbox5 <- dplyr::select(rollingbox5,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
#                        ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
#                        EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct)
#   rollingbox5$Date <- as.Date(j,origin="1970-01-01")
#   
#   rollingbox3 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-(365*3),] %>%
#     dplyr::group_by(PlayerID) %>%
#     dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
#               MIN = sum(MIN,na.rm=TRUE),
#               Games = MIN/MINPG,
#               Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
#               OREB = sum(OREB,na.rm=TRUE),
#               DREB = sum(DREB,na.rm=TRUE),
#               REB = sum(REB,na.rm=TRUE),
#               STL = sum(STL,na.rm=TRUE),
#               BLK = sum(BLK,na.rm=TRUE),
#               AST = sum(AST,na.rm=TRUE),
#               TO = sum(TO,na.rm=TRUE),
#               PF = sum(PF,na.rm= TRUE),
#               PTS = sum(PTS,na.rm=TRUE),
#               Teams = paste(unique(Team),collapse = ","),
#               Plusminus = sum(`+/-`,na.rm= TRUE),
#               FGA = sum(FGA,na.rm=TRUE),
#               FGM = sum(FGM,na.rm=TRUE),
#               ThreePA = sum(`3PA`,na.rm=TRUE),
#               ThreePM = sum(`3PM`,na.rm=TRUE),
#               TwoPA = sum(`2PA`,na.rm=TRUE),
#               TwoPM = sum(`2PM`,na.rm= TRUE),
#               FTA = sum(FTA,na.rm=TRUE),
#               FTM = sum(FTM,na.rm=TRUE),
#               PG = sum(Position5=="PG",na.rm=TRUE),
#               SG = sum(Position5=="SG",na.rm=TRUE),
#               SF = sum(Position5=="SF",na.rm=TRUE),
#               PF = sum(Position5=="PF",na.rm=TRUE),
#               G = sum(Position=="G",na.rm=TRUE),
#               F = sum(Position=="F",na.rm=TRUE),
#               C = sum(Position=="C",na.rm=TRUE),
#               
#               MIN_team = sum(MIN.x,na.rm=TRUE),
#               OREB_team = sum(OREB.x,na.rm=TRUE),
#               DREB_team = sum(DREB.x,na.rm=TRUE),
#               REB_team = sum(REB.x,na.rm=TRUE),
#               STL_team = sum(STL.x,na.rm=TRUE),
#               BLK_team = sum(BLK.x,na.rm=TRUE),
#               AST_team = sum(AST.x,na.rm=TRUE),
#               TO_team = sum(TO.x,na.rm=TRUE),
#               PF_team = sum(PF.x,na.rm= TRUE),
#               PTS_team = sum(PTS.x,na.rm=TRUE),
#               Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
#               FGA_team = sum(FGA.x,na.rm=TRUE),
#               FGM_team = sum(FGM.x,na.rm=TRUE),
#               ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
#               ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
#               TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
#               TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
#               FTA_team = sum(FTA.x,na.rm=TRUE),
#               FTM_team = sum(FTM.x,na.rm=TRUE),
#               
#               MIN_opp = sum(MIN.y,na.rm=TRUE),
#               OREB_opp = sum(OREB.y,na.rm=TRUE),
#               DREB_opp = sum(DREB.y,na.rm=TRUE),
#               REB_opp = sum(REB.y,na.rm=TRUE),
#               STL_opp = sum(STL.y,na.rm=TRUE),
#               BLK_opp = sum(BLK.y,na.rm=TRUE),
#               AST_opp = sum(AST.y,na.rm=TRUE),
#               TO_opp = sum(TO.y,na.rm=TRUE),
#               PF_opp = sum(PF.y,na.rm = TRUE),
#               PTS_opp = sum(PTS.y,na.rm=TRUE),
#               FGA_opp = sum(FGA.y,na.rm=TRUE),
#               FGM_opp = sum(FGM.y,na.rm=TRUE),
#               ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
#               ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
#               TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
#               TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
#               FTA_opp = sum(FTA.y,na.rm=TRUE),
#               FTM_opp = sum(FTM.y,na.rm=TRUE),
#               
#               FGApm = (FGA*36)/MIN,
#               FGMpm = (FGM*36)/MIN,
#               ThreePApm = (ThreePA*36)/MIN,
#               ThreePMpm = (ThreePM*36)/MIN,
#               TwoPApm = (TwoPA*36)/MIN,
#               TwoPMpm = (TwoPM*36)/MIN,
#               FTApm = (FTA*36)/MIN,
#               FTMpm= (FTM*36)/MIN,
#               REBpm= (REB*36)/MIN,
#               OREBpm= (OREB*36)/MIN,
#               DREBpm= (DREB*36)/MIN,
#               STLpm= (STL*36)/MIN,
#               BLKpm= (BLK*36)/MIN,
#               ASTpm= (AST*36)/MIN,
#               TOpm= (TO*36)/MIN,
#               Plusminuspm= (Plusminus*36)/MIN,
#               PTSpm= (PTS*36)/MIN,
#               ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + PF,
#               TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
#               AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
#               ImpactPct = ImpactPlays/AdjTeamImpPlays,
#               TotalImpact = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
#               TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
#               AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
#               ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
#               TotalImpact_off = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_def = BLK + STL + PF + DREB,
#               TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
#               AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
#               ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
#               TotalImpact_def = ImpactPlays/TeamImpactPlays,
#               ImpactPlayspm= (ImpactPlays*36)/MIN,
#               ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
#               ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
#               FGPct = FGM/FGA,
#               ThreePct = ThreePM/ThreePA,
#               TwoPct = TwoPM/TwoPA,
#               FTPct = FTM/FTA,
#               FTMperFG = FTM/FGA,
#               FTAperFG = FTA/FGA,
#               EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
#               TSPct = (PTS/(FGA*2+.44*FTA*2)),
#               OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
#               DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
#               OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
#               REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
#               TOPct = TO/(FGA+.44*FTA+TO)) %>%
#     left_join(playersnames,by=c("PlayerID")) %>%
#     dplyr::select(.,PlayerID,Players,PlayersFull,everything())
#   
#   rollingbox3 <- dplyr::select(rollingbox3,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
#                        ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
#                        EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct)
#   rollingbox3$Date <- as.Date(j,origin="1970-01-01")
#   
#   rollingbox1 <- boxscoredata[boxscoredata$Date<=j&boxscoredata$Date>j-365,] %>%
#     dplyr::group_by(PlayerID) %>%
#     dplyr::summarize(MINPG = mean(MIN,na.rm=TRUE),
#               MIN = sum(MIN,na.rm=TRUE),
#               Games = MIN/MINPG,
#               Pace = mean(FGA.x+FGA.y-OREB.x-OREB.y+TO.x+TO.y+.44*FTA.x+.44*FTA.y,na.rm=TRUE),
#               OREB = sum(OREB,na.rm=TRUE),
#               DREB = sum(DREB,na.rm=TRUE),
#               REB = sum(REB,na.rm=TRUE),
#               STL = sum(STL,na.rm=TRUE),
#               BLK = sum(BLK,na.rm=TRUE),
#               AST = sum(AST,na.rm=TRUE),
#               TO = sum(TO,na.rm=TRUE),
#               PF = sum(PF,na.rm= TRUE),
#               PTS = sum(PTS,na.rm=TRUE),
#               Teams = paste(unique(Team),collapse = ","),
#               Plusminus = sum(`+/-`,na.rm= TRUE),
#               FGA = sum(FGA,na.rm=TRUE),
#               FGM = sum(FGM,na.rm=TRUE),
#               ThreePA = sum(`3PA`,na.rm=TRUE),
#               ThreePM = sum(`3PM`,na.rm=TRUE),
#               TwoPA = sum(`2PA`,na.rm=TRUE),
#               TwoPM = sum(`2PM`,na.rm= TRUE),
#               FTA = sum(FTA,na.rm=TRUE),
#               FTM = sum(FTM,na.rm=TRUE),
#               PG = sum(Position5=="PG",na.rm=TRUE),
#               SG = sum(Position5=="SG",na.rm=TRUE),
#               SF = sum(Position5=="SF",na.rm=TRUE),
#               PF = sum(Position5=="PF",na.rm=TRUE),
#               G = sum(Position=="G",na.rm=TRUE),
#               F = sum(Position=="F",na.rm=TRUE),
#               C = sum(Position=="C",na.rm=TRUE),
#               
#               MIN_team = sum(MIN.x,na.rm=TRUE),
#               OREB_team = sum(OREB.x,na.rm=TRUE),
#               DREB_team = sum(DREB.x,na.rm=TRUE),
#               REB_team = sum(REB.x,na.rm=TRUE),
#               STL_team = sum(STL.x,na.rm=TRUE),
#               BLK_team = sum(BLK.x,na.rm=TRUE),
#               AST_team = sum(AST.x,na.rm=TRUE),
#               TO_team = sum(TO.x,na.rm=TRUE),
#               PF_team = sum(PF.x,na.rm= TRUE),
#               PTS_team = sum(PTS.x,na.rm=TRUE),
#               Plusminus_team = sum(`teamplusmin`,na.rm= TRUE),
#               FGA_team = sum(FGA.x,na.rm=TRUE),
#               FGM_team = sum(FGM.x,na.rm=TRUE),
#               ThreePA_team = sum(`3PA.x`,na.rm=TRUE),
#               ThreePM_team = sum(`3PM.x`,na.rm=TRUE),
#               TwoPA_team = sum(`2PA.x`,na.rm=TRUE),
#               TwoPM_team = sum(`2PM.x`,na.rm= TRUE),
#               FTA_team = sum(FTA.x,na.rm=TRUE),
#               FTM_team = sum(FTM.x,na.rm=TRUE),
#               
#               MIN_opp = sum(MIN.y,na.rm=TRUE),
#               OREB_opp = sum(OREB.y,na.rm=TRUE),
#               DREB_opp = sum(DREB.y,na.rm=TRUE),
#               REB_opp = sum(REB.y,na.rm=TRUE),
#               STL_opp = sum(STL.y,na.rm=TRUE),
#               BLK_opp = sum(BLK.y,na.rm=TRUE),
#               AST_opp = sum(AST.y,na.rm=TRUE),
#               TO_opp = sum(TO.y,na.rm=TRUE),
#               PF_opp = sum(PF.y,na.rm = TRUE),
#               PTS_opp = sum(PTS.y,na.rm=TRUE),
#               FGA_opp = sum(FGA.y,na.rm=TRUE),
#               FGM_opp = sum(FGM.y,na.rm=TRUE),
#               ThreePA_opp = sum(`3PA.y`,na.rm=TRUE),
#               ThreePM_opp = sum(`3PM.y`,na.rm=TRUE),
#               TwoPA_opp = sum(`2PA.y`,na.rm=TRUE),
#               TwoPM_opp = sum(`2PM.y`,na.rm= TRUE),
#               FTA_opp = sum(FTA.y,na.rm=TRUE),
#               FTM_opp = sum(FTM.y,na.rm=TRUE),
#               
#               FGApm = (FGA*36)/MIN,
#               FGMpm = (FGM*36)/MIN,
#               ThreePApm = (ThreePA*36)/MIN,
#               ThreePMpm = (ThreePM*36)/MIN,
#               TwoPApm = (TwoPA*36)/MIN,
#               TwoPMpm = (TwoPM*36)/MIN,
#               FTApm = (FTA*36)/MIN,
#               FTMpm= (FTM*36)/MIN,
#               REBpm= (REB*36)/MIN,
#               OREBpm= (OREB*36)/MIN,
#               DREBpm= (DREB*36)/MIN,
#               STLpm= (STL*36)/MIN,
#               BLKpm= (BLK*36)/MIN,
#               ASTpm= (AST*36)/MIN,
#               TOpm= (TO*36)/MIN,
#               Plusminuspm= (Plusminus*36)/MIN,
#               PTSpm= (PTS*36)/MIN,
#               ImpactPlays = FGA + AST + STL + BLK + REB + .44*FTA + TO + PF,
#               TeamImpactPlays = FGA_team + AST_team + STL_team + BLK_team + REB_team + .44*FTA_team + TO_team + PF_team,
#               AdjTeamImpPlays = TeamImpactPlays*(MIN/MIN_team),
#               ImpactPct = ImpactPlays/AdjTeamImpPlays,
#               TotalImpact = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_off = FGA + AST + OREB + .44*FTA + TO,
#               TeamImpactPlays_off = FGA_team + AST_team + OREB_team + .44*FTA_team + TO_team,
#               AdjTeamImpPlays_off = TeamImpactPlays_off*(MIN/MIN_team),
#               ImpactPct_off = ImpactPlays_off/AdjTeamImpPlays_off,
#               TotalImpact_off = ImpactPlays/TeamImpactPlays,
#               ImpactPlays_def = BLK + STL + PF + DREB,
#               TeamImpactPlays_def = BLK_team + STL_team + PF_team + DREB_team,
#               AdjTeamImpPlays_def = TeamImpactPlays_def*(MIN/MIN_team),
#               ImpactPct_def = ImpactPlays_def/AdjTeamImpPlays_def,
#               TotalImpact_def = ImpactPlays/TeamImpactPlays,
#               ImpactPlayspm= (ImpactPlays*36)/MIN,
#               ImpactPlays_offpm= (ImpactPlays_off*36)/MIN,
#               ImpactPlays_defpm= (ImpactPlays_def*36)/MIN,
#               FGPct = FGM/FGA,
#               ThreePct = ThreePM/ThreePA,
#               TwoPct = TwoPM/TwoPA,
#               FTPct = FTM/FTA,
#               FTMperFG = FTM/FGA,
#               FTAperFG = FTA/FGA,
#               EffFGPct = (3*ThreePM+2*TwoPM)/(ThreePA*2+TwoPA*2),
#               TSPct = (PTS/(FGA*2+.44*FTA*2)),
#               OnOff = (Plusminus*36)/MIN-(Plusminus_team*36)/(MIN_team-MIN),
#               DREBrt = (DREB*MIN_team)/((DREB_team+OREB_opp)*MIN),
#               OREBrt = (OREB*MIN_team)/((OREB_team+DREB_opp)*MIN),
#               REBrt = (REB*MIN_team)/((REB_team+REB_opp)*MIN),
#               TOPct = TO/(FGA+.44*FTA+TO)) %>%
#     left_join(playersnames,by=c("PlayerID")) %>%
#     dplyr::select(.,PlayerID,Players,PlayersFull,everything())
#   
#   rollingbox1 <- dplyr::select(rollingbox1,PlayerID,Players,PlayersFull,Teams,MINPG,MIN,Games,Pace,PG,SG,SF,PF,G,F,C,FGApm,FGMpm,ThreePApm,ThreePMpm,TwoPApm,TwoPMpm,FTApm,FTMpm,REBpm,OREBpm,DREBpm,STLpm,BLKpm,ASTpm,TOpm,Plusminuspm,PTSpm,ImpactPlayspm,ImpactPlays_offpm,ImpactPlays_defpm,
#                        ImpactPct,TotalImpact,ImpactPct_off,TotalImpact_off,ImpactPct_def,TotalImpact_def,FGPct,ThreePct,TwoPct,FTPct,FTMperFG,FTAperFG,
#                        EffFGPct,TSPct,OnOff,DREBrt,OREBrt,REBrt,TOPct)
#   rollingbox1$Date <- as.Date(j,origin="1970-01-01")
#   
#   rollingbox <- rollingbox5 %>% left_join(rollingbox3,by=c("PlayerID","Players","PlayersFull","Date")) %>% left_join(rollingbox1,by=c("PlayerID","Players","PlayersFull","Date"))
#   
#   if(j==datevec[1]) {
#     rollingboxALL <- rollingbox
#   } else {
#     rollingboxALL <- rbind(rollingboxALL,rollingbox)
#   }
#   print(as.Date(j,origin="1970-01-01"))
# }


# tic()
# for (j in as.Date(datevec,origin="1970-01-01")) {
#   adjpm4_RAPM <- adjplusminus[adjplusminus$Date<=j&adjplusminus$Date>=(j-(365*5)),]
#   adjpm2 <- adjpm4_RAPM
#   adjpm2$PtsPM <- adjpm2$hpp_poss
#   adjpm2$HomeAdj <- 1
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x>13000&rollingboxALL$Date==j]) {
#     adjpm2[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(adjpm2,ifelse(!is.na(stri_extract_first_regex(homeplayers,i)),1,0))
#     adjpm2[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(adjpm2,ifelse(!is.na(stri_extract_first_regex(awayplayers,i)),-1,0))
#   }
#   adjpm2$dummyrestOFF <- 0
#   adjpm2$dummyrestDEF <- 0
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x<=5&rollingboxALL$Date==j]) {
#     adjpm2$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,i)),1+adjpm2$dummyrestOFF,adjpm2$dummyrestOFF)
#     adjpm2$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,i)),adjpm2$dummyrestDEF-1,adjpm2$dummyrestDEF)
#   }
#   
#   
#   adjpm3 <- adjpm4_RAPM
#   adjpm3$PtsPM <- adjpm3$app_poss
#   adjpm3$HomeAdj <- -1
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x>13000&rollingboxALL$Date==j]) {
#     adjpm3[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(adjpm3,ifelse(!is.na(stri_extract_first_regex(homeplayers,i)),-1,0))
#     adjpm3[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(adjpm3,ifelse(!is.na(stri_extract_first_regex(awayplayers,i)),1,0))
#   }
#   adjpm3$dummyrestOFF <- 0
#   adjpm3$dummyrestDEF <- 0
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN.x<=5&rollingboxALL$Date==j]) {
#     adjpm3$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,i)),1+adjpm3$dummyrestOFF,adjpm3$dummyrestOFF)
#     adjpm3$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,i)),adjpm3$dummyrestDEF-1,adjpm3$dummyrestDEF)
#   }
#   
#   adjpm4 <- dplyr::bind_rows(adjpm2,adjpm3)
#   
#   names(adjpm4) <- str_replace(names(adjpm4)," ","")
#   names(adjpm4) <- str_replace(names(adjpm4)," ","")
#   
#   adjpm4_RAPM <- adjpm4
#   
#   wgt1 <- adjpm4_RAPM$poss
#   wgt2 <- ifelse(adjpm4_RAPM$Date<j-365*3&adjpm4_RAPM$Date>=(j-(365*5)),1,ifelse(adjpm4_RAPM$Date<j-365*1&adjpm4_RAPM$Date>=(j-(365*3)),1.25,1.5))
#   wgt3 <- wgt1*wgt2
#   wgt4 <- ifelse(adjpm4_RAPM$Playoffs==1,wgt3*1.5,wgt3)
#   wgt5 <- ifelse(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)<=15,1,pmax(1-.1*(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)-15),0))
#   adjpm4_RAPM$wgt <- wgt4*wgt5
#   adjpm4_RAPM <- adjpm4_RAPM[adjpm4_RAPM$poss_home>=1&adjpm4_RAPM$poss_away>=1&abs(adjpm4_RAPM$PtsPM)<=400,]
#   adjpm4_RAPM <- adjpm4_RAPM[,c(147:length(names(adjpm4_RAPM)))]
#   adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
#                            lapply(adjpm4_RAPM,
#                                   function(x) replace(x, is.infinite(x), NA)))
#   adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
#                            lapply(adjpm4_RAPM,
#                                   function(x) replace(x, is.nan(x), NA)))
#   
#   
#   adjpm4_RAPM <- adjpm4_RAPM[!is.na(adjpm4_RAPM$PtsPM),]
#   adjpm4_RAPM <- adjpm4_RAPM[,colSums(adjpm4_RAPM,na.rm=TRUE)!=0]
#   y <- adjpm4_RAPM$PtsPM
#   wgt <- adjpm4_RAPM$wgt
#   x <- data.matrix(dplyr:select(adjpm4_RAPM,-PtsPM,-wgt))
#   
#   # cv_model2_a <- cv.glmnet(x, y, alpha = 0,weight=wgt) #alpha = 0 is ridge regression
#   # save_lambda <- cv_model2_a$lambda.min
#   best_lambda2_a <- 26.96255
#   best_model2_a <- glmnet(x, y, alpha = 0, lambda = best_lambda2_a,weight=wgt)
#   test <- coef(best_model2_a)
#   coef <- data.frame(name = test@Dimnames[[1]][test@i + 1], coefficient = test@x)
#   coef_off <- coef[unlist(gregexpr("OFF",coef$name))>0,]
#   coef_def <- coef[unlist(gregexpr("DEF",coef$name))>0,]
#   coef_off$PlayerID <- substr(coef_off$name,4,nchar(coef_off$name))
#   coef_def$PlayerID <- substr(coef_def$name,4,nchar(coef_def$name))
#   coef_off <- dplyr::rename(coef_off,RAPM5_off=coefficient)
#   coef_def <- dplyr::rename(coef_def,RAPM5_def=coefficient)
#   coef_off$Date <- as.Date(j,origin="1970-01-01")
#   coef_def$Date <- as.Date(j,origin="1970-01-01")
#   coef_off <- dplyr::select(coef_off,-name)
#   coef_def <- dplyr::select(coef_def,-name)
#   coef <- full_join(coef_off,coef_def,by=c("PlayerID","Date"))
#   coef <- dplyr::select(coef,PlayerID,Date,RAPM5_off,RAPM5_def)
#   
#   coef <- left_join (coef,playersnames,by=c("PlayerID"))
#   coef_new <- rbind()
#   coef$RAPM5_def[coef$PlayerID=="myrestOFF"] <- coef$RAPM5_def[coef$PlayerID=="myrestDEF"]
#   coef$RAPM5_total <- coef$RAPM5_off+coef$RAPM5_def
#   coef$PlayerID[coef$PlayerID=="myrestOFF"] <- "Replacement"
#   coef <- coef[coef$PlayerID!="myrestDEF",]
#   
#   if(j==datevec[1]) {
#     coefALL <- coef
#   } else {
#     coefALL <- rbind(coefALL,coef2)
#   }
#   
#   
#   #1y RAPM
#   adjpm4_RAPM <- adjplusminus[adjplusminus$Date<=j&adjplusminus$Date>=(j-(365)),]
#   adjpm2 <- adjpm4_RAPM
#   adjpm2$PtsPM <- adjpm2$hpp_poss
#   adjpm2$HomeAdj <- 1
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j]) {
#     adjpm2[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(adjpm2,ifelse(!is.na(stri_extract_first_regex(homeplayers,i)),1,0))
#     adjpm2[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(adjpm2,ifelse(!is.na(stri_extract_first_regex(awayplayers,i)),-1,0))
#   }
#   adjpm2$dummyrestOFF <- 0
#   adjpm2$dummyrestDEF <- 0
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN<=300&rollingboxALL$Date==j]) {
#     adjpm2$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$homeplayers,i)),1+adjpm2$dummyrestOFF,adjpm2$dummyrestOFF)
#     adjpm2$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm2$awayplayers,i)),adjpm2$dummyrestDEF-1,adjpm2$dummyrestDEF)
#   }
#   
#   
#   adjpm3 <- adjpm4_RAPM
#   adjpm3$PtsPM <- adjpm3$app_poss
#   adjpm3$HomeAdj <- -1
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN>300&rollingboxALL$Date==j]) {
#     adjpm3[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(adjpm3,ifelse(!is.na(stri_extract_first_regex(homeplayers,i)),-1,0))
#     adjpm3[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(adjpm3,ifelse(!is.na(stri_extract_first_regex(awayplayers,i)),1,0))
#   }
#   adjpm3$dummyrestOFF <- 0
#   adjpm3$dummyrestDEF <- 0
#   for (i in rollingboxALL$PlayerID2[rollingboxALL$MIN<=300&rollingboxALL$Date==j]) {
#     adjpm3$dummyrestOFF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$homeplayers,i)),1+adjpm3$dummyrestOFF,adjpm3$dummyrestOFF)
#     adjpm3$dummyrestDEF <- ifelse(!is.na(stri_extract_first_regex(adjpm3$awayplayers,i)),adjpm3$dummyrestDEF-1,adjpm3$dummyrestDEF)
#   }
#   
#   adjpm4 <- dplyr::bind_rows(adjpm2,adjpm3)
#   
#   names(adjpm4) <- str_replace(names(adjpm4)," ","")
#   names(adjpm4) <- str_replace(names(adjpm4)," ","")
#   
#   adjpm4_RAPM <- adjpm4
#   
#   wgt1 <- adjpm4_RAPM$poss
#   wgt2 <- ifelse(adjpm4_RAPM$Date<j-365*3&adjpm4_RAPM$Date>=(j-(365*5)),1,ifelse(adjpm4_RAPM$Date<j-365*1&adjpm4_RAPM$Date>=(j-(365*3)),1.5,2))
#   wgt3 <- (wgt1*wgt2)/2
#   wgt4 <- ifelse(adjpm4_RAPM$Playoffs==1,wgt3*2,wgt3)
#   wgt5 <- ifelse(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)<=15,1,pmax(1-.05*(abs(adjpm4_RAPM$starthomescore-adjpm4_RAPM$startawayscore)-15),.25))
#   adjpm4_RAPM$wgt <- wgt4*wgt5
#   adjpm4_RAPM <- adjpm4_RAPM[adjpm4_RAPM$poss_home>=1&adjpm4_RAPM$poss_away>=1,]
#   adjpm4_RAPM <- adjpm4_RAPM[,c(147:length(names(adjpm4_RAPM)))]
#   adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
#                          lapply(adjpm4_RAPM,
#                                 function(x) replace(x, is.infinite(x), NA)))
#   adjpm4_RAPM <- do.call(data.frame,                      # Replace Inf in data by NA
#                          lapply(adjpm4_RAPM,
#                                 function(x) replace(x, is.nan(x), NA)))
#   
#   
#   adjpm4_RAPM <- adjpm4_RAPM[!is.na(adjpm4_RAPM$PtsPM),]
#   adjpm4_RAPM <- adjpm4_RAPM[,colSums(adjpm4_RAPM,na.rm=TRUE)!=0]
#   y <- adjpm4_RAPM$PtsPM
#   wgt <- adjpm4_RAPM$wgt
#   x <- data.matrix(dplyr::select(adjpm4_RAPM,-PtsPM,-wgt))
#   
#   # cv_model2_a <- cv.glmnet(x, y, alpha = 0,weight=wgt) #alpha = 0 is ridge regression
#   # best_lambda2_a <- cv_model2_a$lambda.min 
#   best_lambda2_a <- 0 #just calculate OLS
#   best_model2_a <- glmnet(x, y, alpha = 0, lambda = best_lambda2_a,weight=wgt)
#   test <- coef(best_model2_a)
#   coef2 <- data.frame(name = test@Dimnames[[1]][test@i + 1], coefficient = test@x)
#   coef2_off <- coef2[unlist(gregexpr("OFF",coef2$name))>0,]
#   coef2_def <- coef2[unlist(gregexpr("DEF",coef2$name))>0,]
#   coef2_off$PlayerID <- substr(coef2_off$name,4,nchar(coef2_off$name))
#   coef2_def$PlayerID <- substr(coef2_def$name,4,nchar(coef2_def$name))
#   coef2_off <- dplyr::rename(coef2_off,RAPM1_off=coefficient)
#   coef2_def <- dplyr::rename(coef2_def,RAPM1_def=coefficient)
#   coef2_off$Date <- as.Date(j,origin="1970-01-01")
#   coef2_def$Date <- as.Date(j,origin="1970-01-01")
#   coef2_off <- dplyr::select(coef2_off,-name)
#   coef2_def <- dplyr::select(coef2_def,-name)
#   coef2 <- full_join(coef2_off,coef2_def,by=c("PlayerID","Date"))
#   coef2 <- dplyr::select(coef2,PlayerID,Date,RAPM1_off,RAPM1_def)
#   
#   coef2 <- left_join (coef2,playersnames,by=c("PlayerID"))
#   coef2$RAPM1_def[coef2$PlayerID=="myrestOFF"] <- coef2$RAPM1_def[coef2$PlayerID=="myrestDEF"]
#   coef2$RAPM1_total <- coef2$RAPM1_off+coef2$RAPM1_def
#   coef2$PlayerID[coef2$PlayerID=="myrestOFF"] <- "Replacement"
#   coef2 <- coef2[coef2$PlayerID!="myrestDEF",]
#   
#   if(j==datevec[1]) {
#     coef2ALL <- coef2
#   } else {
#     coef2ALL <- rbind(coef2ALL,coef2)
#   }
#   print(as.Date(j,origin="1970-01-01"))
#   
# }
# toc()




#old RAPM code (if needed)
# players <- boxscoredata[boxscoredata$Date<=today&boxscoredata$Date>=today-365,] %>%
#   distinct(PlayerID,Team) %>%
#   left_join(distinct(boxscoredata[boxscoredata$Date<=today&boxscoredata$Date>=today-365,c("PlayerID","Players")],PlayerID,.keep_all=TRUE),by=c("PlayerID")) %>%
# players <- players[order(players$Players),]
# players$PlayerID <- paste(" ",players$PlayerID," ",sep="")
# adjplusminus$HomeAdj <- 1
# adjplusminus$dummyrest <- 0
# for (i in players$PlayerID[players$MIN<=300]) {
#   adjplusminus$dummyrest <- ifelse(!is.na(stri_extract_first_regex(adjplusminus$homeplayers,i)),1+adjplusminus$dummyrest,adjplusminus$dummyrest)
#   adjplusminus$dummyrest <- ifelse(!is.na(stri_extract_first_regex(adjplusminus$awayplayers,i)),adjplusminus$dummyrest-1,adjplusminus$dummyrest)
# }
# for (i in players$PlayerID[players$MIN>300]) {
#   adjplusminus[[paste("dummy",i,sep="")]] <- with(adjplusminus,ifelse(!is.na(stri_extract_first_regex(homeplayers,i)),1,ifelse(!is.na(stri_extract_first_regex(awayplayers,i)),-1,0)))
# }
# names(adjplusminus) <- str_replace(names(adjplusminus)," ","")
# names(adjplusminus) <- str_replace(names(adjplusminus)," ","")
# regformula <- paste("pmp_poss ~ ",paste(names(adjplusminus[,c(147:571)]),collapse = " + "),sep="")
# summary(modelalpha4d)$coef
# constant <- summary(modelalpha4d)$coef[1]
# 
# for (i in players$PlayerID[players$MIN>300]) {
#   players[[str_replace(paste("dummy",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,1,0))
# }
# players$HomeAdj <- 0
# players$dummyrest<- -1
# names(players) <- str_replace(names(players)," ","")
# players$adjpm_all <- predict(modelalpha4d,newdata=players)-constant
# players2 <- players[,c("PlayerID","Players","Team","MIN","adjpm_all")]
# players <- players[,c("PlayerID","Players","Team","MIN","adjpm_all")]
# results <- data.frame(summary(modelalpha4d)$coef)
# 
# adjpm4$PtsPM[is.nan(adjpm4$PtsPM)] <- NA
# adjpm4$PtsPM[is.infinite(adjpm4$PtsPM)] <- NA
# names(adjpm4) <- str_replace(names(adjpm4)," ","")
# names(adjpm4) <- str_replace(names(adjpm4)," ","")
# regformula <- paste("PtsPM ~ HomeAdj + ",paste(names(adjpm4[,c(572:1420)]),collapse = " + "),sep="")
# modelalpha4d <- lm(formula = regformula, data = adjpm4[adjpm4$minlen>0&adjpm4$poss>0&adjpm4$poss_home>0&adjpm4$poss_away>0,],weight=poss)
# summary(modelalpha4d)$coef
# constant <- summary(modelalpha4d)$coef[1]
# results <- data.frame(summary(modelalpha4d)$coef)
# 
# 
# for (i in players$PlayerID[players$MIN>300]) {
#   players[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,1,0))
#   players[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,1,0))
# }
# players$HomeAdj <- 0
# players$dummyrestDEF <- -1
# players$dummyrestOFF <- -1
# names(players) <- str_replace(names(players)," ","")
# players$adjpm_all <- predict(modelalpha4d,newdata=players)-constant
# players <- players[,c("PlayerID","Players","MIN","adjpm_all","HomeAdj","dummyrestOFF","dummyrestDEF")]
# for (i in players$PlayerID[players$MIN>300]) {
#   players[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,0,0))
#   players[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,1,0))
# }
# names(players) <- str_replace(names(players)," ","")
# players$adjpm_off <- predict(modelalpha4d,newdata=players)-constant
# players <- players[,c("PlayerID","Players","MIN","adjpm_all","adjpm_off","HomeAdj","dummyrestOFF","dummyrestDEF")]
# for (i in players$PlayerID[players$MIN>300]) {
#   players[[str_replace(paste("DEF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,1,0))
#   players[[str_replace(paste("OFF",i,sep="")," ","")]] <- with(players,ifelse(PlayerID==i,0,0))
# }
# names(players) <- str_replace(names(players)," ","")
# players$adjpm_def <- predict(modelalpha4d,newdata=players)-constant
# players <- players[,c("PlayerID","Players","MIN","adjpm_all","adjpm_off","adjpm_def","HomeAdj","dummyrestOFF","dummyrestDEF")]
# 
# 
# # adjpm4$pred_margin <- predict(modelalpha4d,newdata=adjpm4)
# # adjpm4$pred_margerror <- (adjpm4$PtsPM-adjpm4$pred_margin)^2
# # adjpm4$less5 <- ifelse(adjpm4$minlen<=.5,1,0)
# # adjpm4$less10 <- ifelse(adjpm4$minlen<=1,1,0)
# # modelalpha5d <- lm(formula = pred_margerror ~ minlen + poss + less5, data = adjpm4[adjpm4$minlen>0,])
# # adjpm4$predpred_margerror <- predict(modelalpha5d,newdata=adjpm4)
# # adjpm5 <- adjpm4[adjpm4$minlen>0,c("minlen","poss","PtsPM","pred_margin","pred_margerror","predpred_margerror")]
# # summary(modelalpha5d)$coef
