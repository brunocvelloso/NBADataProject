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
#setwd(working)

load("master_teamboxscores.RData")
load("master_boxscores.RData")
load("NBA_ESPNgamelinks.RData")
load("NBA_ESPNgamelinksSUB.RData")
load("NBAdistance.RData")
load("startdate.RData")
load("enddate.RData")
load("data_modelalphaV4.RData")

seasondata_master <- seasondata
rm(seasondata)
save(seasondata_master, file = paste("data_modelalphaV4OLD.RData", sep="")) #this is in case something goes wrong with update

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())

# if you want to update all see line below
# startdate <- as.Date("2003-10-01",origin="1970-01-01")

#combine team and player box into one
boxscoredata <- rbind(boxscoredata,teamboxscoredata[,c(1:55)])

boxscoredata <- subset(boxscoredata, select = -c(GameID.x,Match1,`3PT`,LastName,FirstLetter,FG,GameID.y,GameDescription.y,PlayersFull,Position5,ApostropheDash2,BoxScore,PlaybyPlay) )
#drop pre-season games
boxscoredata <- boxscoredata[boxscoredata$RegularSeason==1|boxscoredata$Playoffs==1,]
boxscoredata <- boxscoredata[order(boxscoredata$Date,boxscoredata$GameID,boxscoredata$Team,boxscoredata$Starter),]
box_nopreseason <- boxscoredata
save(box_nopreseason, file = paste("box_nopreseason.RData", sep=""))
box_nopreseason <- box_nopreseason[box_nopreseason$Date>=startdate,]

seasondataALL <- box_nopreseason[box_nopreseason$Players=="TEAM",]
seasondataHOME <- box_nopreseason[box_nopreseason$Players=="TEAM"&box_nopreseason$Home==1,]
seasondataAWAY <- box_nopreseason[box_nopreseason$Players=="TEAM"&box_nopreseason$Home==0,]
seasondata <- seasondataHOME
seasondata <- subset(seasondata, select = -c(elemplayercards,PlayerID,Starter,Position,`+/-`,DNP,Home,FT,Year,Month,Day,Players) )
seasondataHOME <- subset(seasondataHOME, select = -c(elemplayercards,PlayerID,Starter,Position,`+/-`,DNP,Home,FT,Year,Month,Day,Players) )
seasondataAWAY <- subset(seasondataAWAY, select = -c(elemplayercards,PlayerID,Starter,Position,`+/-`,DNP,Home,FT,Year,Month,Day,Players) )
seasondata <- seasondata[!is.na(seasondata$GameID),]
seasondataHOME <- seasondataHOME[!is.na(seasondataHOME$GameID),]
seasondataAWAY <- seasondataAWAY[!is.na(seasondataAWAY$GameID),]


seasondata <- merge(seasondata,seasondataAWAY,by="GameID",all.x=TRUE,all.y=FALSE)
seasondata$HomePts <- seasondata$PTS.x
seasondata$AwayPts <- seasondata$PTS.y
seasondata$HomeWin <- ifelse(seasondata$HomePts>seasondata$AwayPts,1,0)
seasondata$HomeMargin <- seasondata$HomePts - seasondata$AwayPts
seasondata <- subset(seasondata, select = -c(GameDescription.x.y,Date.y,RegularSeason.y,Playoffs.y,Finals.y,Season.y) )
seasondata <- dplyr::rename(seasondata,GameDescription=GameDescription.x.x)
seasondata <- dplyr::rename(seasondata,Date=Date.x,RegularSeason=RegularSeason.x,Playoffs=Playoffs.x,Finals=Finals.x,Season=Season.x)
seasondata <- seasondata %>%
  dplyr::select(GameID,Team.x, Team.y, Date, HomePts,AwayPts,HomeWin,HomeMargin,everything())
seasondata$MIN.x <- 240
seasondata$MIN.y <- 240
seasondata$Poss <- ((seasondata$FGA.x+seasondata$FGA.y)+0.44*(seasondata$FTA.x+seasondata$FTA.y)+seasondata$TO.x+seasondata$TO.y-seasondata$OREB.x-seasondata$OREB.y)/2
seasondata$PtsPerPoss.x <- (seasondata$PTS.x/seasondata$Poss)*100
seasondata$PtsPerPoss.y <- (seasondata$PTS.y/seasondata$Poss)*100

#Create variables as NAs in same order
seasondata$GameCount.x <- NA
seasondata$GameCount.y <- NA
seasondata$PtsPerPossAvg.x <- NA
seasondata$PtsPerPossAvg.y <- NA
seasondata$DefPtsPerPossAvg.x <- NA
seasondata$DefPtsPerPossAvg.y <- NA
seasondata$OppPtsPerPossAvg.x <- NA
seasondata$OppDefPtsPerPossAvg.x <- NA
seasondata$OppPtsPerPossAvg.y <- NA
seasondata$OppDefPtsPerPossAvg.y <- NA
seasondata$BtoB.x <- NA
seasondata$BtoB.y <- NA
seasondata$AdjNetRating.x <- NA
seasondata$NetRating.x <- (seasondata$PtsPerPossAvg.x - seasondata$DefPtsPerPossAvg.x)
seasondata$AdjNetRating.y <- (seasondata$PtsPerPossAvg.y - seasondata$DefPtsPerPossAvg.y) + (seasondata$OppPtsPerPossAvg.y - seasondata$OppDefPtsPerPossAvg.y)
seasondata$NetRating.y <- (seasondata$PtsPerPossAvg.y - seasondata$DefPtsPerPossAvg.y)
seasondata <- seasondata %>% 
  dplyr::mutate(GameCountAvg = (GameCount.y + GameCount.x)/2)
seasondata$Bubble<- ifelse(seasondata$Date>="2020-04-01"&seasondata$Date<="2020-10-20",1,0)
seasondata$Pandemic<- ifelse(seasondata$Date>="2020-12-01"&seasondata$Date<"2021-04-01",1,0)
seasondata$PartialPandemic<- ifelse(seasondata$Date>="2021-04-01"&seasondata$Date<="2021-09-01",1,0)
seasondata <- seasondata %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = cur_group_id())
seasondata$PrevSeasonRating.x <- NA
seasondata$PrevSeasonRating.y <- NA
seasondata$Pace.x <- NA
seasondata$Pace.y <- NA
seasondata$GameDummy5 <- ifelse(seasondata$GameCountAvg<=5,1,0)
seasondata$GameDummy10 <- ifelse(seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10,1,0)
seasondata$GameDummy15 <- ifelse(seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15,1,0)
seasondata$GameDummy20 <- ifelse(seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20,1,0)
seasondata$GameDummy25 <- ifelse(seasondata$GameCountAvg>20&seasondata$GameCountAvg<=25,1,0)
seasondata$GameDummy30 <- ifelse(seasondata$GameCountAvg>25&seasondata$GameCountAvg<=30,1,0)
seasondata$GameDummy35 <- ifelse(seasondata$GameCountAvg>30&seasondata$GameCountAvg<=35,1,0)
seasondata$GameDummy40 <- ifelse(seasondata$GameCountAvg>35&seasondata$GameCountAvg<=40,1,0)
seasondata$GameDummy45 <- ifelse(seasondata$GameCountAvg>40&seasondata$GameCountAvg<=45,1,0)
seasondata$GameDummy50 <- ifelse(seasondata$GameCountAvg>45&seasondata$GameCountAvg<=50,1,0)
seasondata$GameDummy55 <- ifelse(seasondata$GameCountAvg>50&seasondata$GameCountAvg<=55,1,0)
seasondata$GameDummy60 <- ifelse(seasondata$GameCountAvg>55&seasondata$GameCountAvg<=60,1,0)
seasondata$GameDummy65 <- ifelse(seasondata$GameCountAvg>60&seasondata$GameCountAvg<=65,1,0)
seasondata$GameDummy70 <- ifelse(seasondata$GameCountAvg>65&seasondata$GameCountAvg<=70,1,0)
seasondata$GameDummy82 <- ifelse(seasondata$GameCountAvg>70&seasondata$GameCountAvg<=82,1,0)
seasondata$GameDummy5XPrevSeas <- seasondata$GameDummy5*seasondata$PrevSeasonRating.x
seasondata$GameDummy10XPrevSeas <- seasondata$GameDummy10*seasondata$PrevSeasonRating.x
seasondata$GameDummy15XPrevSeas <- seasondata$GameDummy15*seasondata$PrevSeasonRating.x
seasondata$GameDummy20XPrevSeas <- seasondata$GameDummy20*seasondata$PrevSeasonRating.x
seasondata$GameDummy25XPrevSeas <- seasondata$GameDummy25*seasondata$PrevSeasonRating.x
seasondata$GameDummy30XPrevSeas <- seasondata$GameDummy30*seasondata$PrevSeasonRating.x
seasondata$GameDummy35XPrevSeas <- seasondata$GameDummy35*seasondata$PrevSeasonRating.x
seasondata$GameDummy40XPrevSeas <- seasondata$GameDummy40*seasondata$PrevSeasonRating.x
seasondata$GameDummy45XPrevSeas <- seasondata$GameDummy45*seasondata$PrevSeasonRating.x
seasondata$GameDummy50XPrevSeas <- seasondata$GameDummy50*seasondata$PrevSeasonRating.x
seasondata$GameDummy55XPrevSeas <- seasondata$GameDummy55*seasondata$PrevSeasonRating.x
seasondata$GameDummy60XPrevSeas <- seasondata$GameDummy60*seasondata$PrevSeasonRating.x
seasondata$GameDummy65XPrevSeas <- seasondata$GameDummy65*seasondata$PrevSeasonRating.x
seasondata$GameDummy70XPrevSeas <- seasondata$GameDummy70*seasondata$PrevSeasonRating.x
seasondata$GameDummy82XPrevSeas <- seasondata$GameDummy82*seasondata$PrevSeasonRating.x
seasondata$GameDummy5XPrevSeasY <- seasondata$GameDummy5*seasondata$PrevSeasonRating.y
seasondata$GameDummy10XPrevSeasY <- seasondata$GameDummy10*seasondata$PrevSeasonRating.y
seasondata$GameDummy15XPrevSeasY <- seasondata$GameDummy15*seasondata$PrevSeasonRating.y
seasondata$GameDummy20XPrevSeasY <- seasondata$GameDummy20*seasondata$PrevSeasonRating.y
seasondata$GameDummy25XPrevSeasY <- seasondata$GameDummy25*seasondata$PrevSeasonRating.y
seasondata$GameDummy30XPrevSeasY <- seasondata$GameDummy30*seasondata$PrevSeasonRating.y
seasondata$GameDummy35XPrevSeasY <- seasondata$GameDummy35*seasondata$PrevSeasonRating.y
seasondata$GameDummy40XPrevSeasY <- seasondata$GameDummy40*seasondata$PrevSeasonRating.y
seasondata$GameDummy45XPrevSeasY <- seasondata$GameDummy45*seasondata$PrevSeasonRating.y
seasondata$GameDummy50XPrevSeasY <- seasondata$GameDummy50*seasondata$PrevSeasonRating.y
seasondata$GameDummy55XPrevSeasY <- seasondata$GameDummy55*seasondata$PrevSeasonRating.y
seasondata$GameDummy60XPrevSeasY <- seasondata$GameDummy60*seasondata$PrevSeasonRating.y
seasondata$GameDummy65XPrevSeasY <- seasondata$GameDummy65*seasondata$PrevSeasonRating.y
seasondata$GameDummy70XPrevSeasY <- seasondata$GameDummy70*seasondata$PrevSeasonRating.y
seasondata$GameDummy82XPrevSeasY <- seasondata$GameDummy82*seasondata$PrevSeasonRating.y
seasondata$PaceXPace <- seasondata$Pace.x*seasondata$Pace.y
seasondata$PaceXSq <- seasondata$Pace.x*seasondata$Pace.x
seasondata$PaceYSq <- seasondata$Pace.y*seasondata$Pace.y
seasondata$HomeMargPerPoss <- seasondata$HomeMargin/(seasondata$Poss/100)
seasondata$AdjNetMarSqPlus <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^2*((seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)>0)
seasondata$AdjNetMarSqNeg <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^2*((seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)<=0)
seasondata$AdjNetMarCub <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^3
seasondata$Year <- year(seasondata$Date)
seasondata$YearSq <- seasondata$Year*seasondata$Year
seasondata$YearCub <- seasondata$Year*seasondata$Year*seasondata$Year
seasondata$GameCount.x <- NA
seasondata$GameCount.y <- NA
seasondata$Wins.x <- NA
seasondata$Losses.x <- NA
seasondata$Wins20.x <- NA
seasondata$Losses20.x <- NA
seasondata$Wins10.x <- NA
seasondata$Losses10.x <- NA
seasondata$Wins5.x <- NA
seasondata$Losses5.x <- NA
seasondata$Wins.y <- NA
seasondata$Losses.y <- NA
seasondata$Wins20.y <- NA
seasondata$Losses20.y <- NA
seasondata$Wins10.y <- NA
seasondata$Losses10.y <- NA
seasondata$Wins5.y <- NA
seasondata$Losses5.y <- NA
seasondata$PtsPerPossAvg.x <- NA
seasondata$PtsPerPossAvg.y <- NA                                                                                                                                                                           
seasondata$DefPtsPerPossAvg.x <- NA
seasondata$DefPtsPerPossAvg.y <- NA
seasondata$PtsPerPossAvg20.x <- NA
seasondata$PtsPerPossAvg20.y <- NA                                                                                                                                                                                                                                         
seasondata$DefPtsPerPossAvg20.x <- NA
seasondata$DefPtsPerPossAvg20.y <- NA
seasondata$PtsPerPossAvg10.x <- NA
seasondata$PtsPerPossAvg10.y <- NA                                                                                                                                                                                                                                        
seasondata$DefPtsPerPossAvg10.x <- NA
seasondata$DefPtsPerPossAvg10.y <- NA
seasondata$PtsPerPossAvg5.x <- NA
seasondata$PtsPerPossAvg5.y <- NA                                                                                                                                                                                                                                       
seasondata$DefPtsPerPossAvg5.x <- NA
seasondata$DefPtsPerPossAvg5.y <- NA                                                                                                                                                                                                                                       
seasondata$OppPtsPerPossAvg.x <- NA
seasondata$OppDefPtsPerPossAvg.x <- NA
seasondata$OppPtsPerPossAvg20.x <- NA
seasondata$OppDefPtsPerPossAvg20.x <- NA
seasondata$OppPtsPerPossAvg10.x <- NA
seasondata$OppDefPtsPerPossAvg10.x <- NA
seasondata$OppPtsPerPossAvg5.x <- NA
seasondata$OppDefPtsPerPossAvg5.x <- NA
seasondata$OppPtsPerPossAvg.y <- NA
seasondata$OppDefPtsPerPossAvg.y <- NA
seasondata$OppPtsPerPossAvg20.y <- NA
seasondata$OppDefPtsPerPossAvg20.y <- NA
seasondata$OppPtsPerPossAvg10.y <- NA
seasondata$OppDefPtsPerPossAvg10.y <- NA
seasondata$OppPtsPerPossAvg5.y <- NA
seasondata$OppDefPtsPerPossAvg5.y <- NA
seasondata$WinPct.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins.x/(seasondata$Wins.x + seasondata$Losses.x))
seasondata$WinPct20.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins20.x/(seasondata$Wins20.x + seasondata$Losses20.x))
seasondata$WinPct10.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins10.x/(seasondata$Wins10.x + seasondata$Losses10.x))
seasondata$WinPct5.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins5.x/(seasondata$Wins5.x + seasondata$Losses5.x))
seasondata$WinPct.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins.y/(seasondata$Wins.y + seasondata$Losses.y))
seasondata$WinPct20.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins20.y/(seasondata$Wins20.y + seasondata$Losses20.y))
seasondata$WinPct10.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins10.y/(seasondata$Wins10.y + seasondata$Losses10.y))
seasondata$WinPct5.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins5.y/(seasondata$Wins5.y + seasondata$Losses5.y))
seasondata$AdjNetRating20.x <- (seasondata$PtsPerPossAvg20.x - seasondata$DefPtsPerPossAvg20.x) + (seasondata$OppPtsPerPossAvg20.x - seasondata$OppDefPtsPerPossAvg20.x)
seasondata$AdjNetRating20.y <- (seasondata$PtsPerPossAvg20.y - seasondata$DefPtsPerPossAvg20.y) + (seasondata$OppPtsPerPossAvg20.y - seasondata$OppDefPtsPerPossAvg20.y)
seasondata$AdjNetRating10.x <- (seasondata$PtsPerPossAvg10.x - seasondata$DefPtsPerPossAvg10.x) + (seasondata$OppPtsPerPossAvg10.x - seasondata$OppDefPtsPerPossAvg10.x)
seasondata$AdjNetRating10.y <- (seasondata$PtsPerPossAvg10.y - seasondata$DefPtsPerPossAvg10.y) + (seasondata$OppPtsPerPossAvg10.y - seasondata$OppDefPtsPerPossAvg10.y)
seasondata$AdjNetRating5.x <- (seasondata$PtsPerPossAvg5.x - seasondata$DefPtsPerPossAvg5.x) + (seasondata$OppPtsPerPossAvg5.x - seasondata$OppDefPtsPerPossAvg5.x)
seasondata$AdjNetRating5.y <- (seasondata$PtsPerPossAvg5.y - seasondata$DefPtsPerPossAvg5.y) + (seasondata$OppPtsPerPossAvg5.y - seasondata$OppDefPtsPerPossAvg5.y)
seasondata$GameDummyLess20 <- ifelse(seasondata$GameCountAvg<=20,1,0)
seasondata$WinPctX20.x <- seasondata$WinPct.x*seasondata$GameDummyLess20
seasondata$WinPct20X20.x <- seasondata$WinPct20.x*seasondata$GameDummyLess20
seasondata$WinPct10X20.x <- seasondata$WinPct10.x*seasondata$GameDummyLess20
seasondata$WinPct5X20.x <- seasondata$WinPct5.x*seasondata$GameDummyLess20
seasondata$WinPctX20.y <- seasondata$WinPct.y*seasondata$GameDummyLess20
seasondata$WinPct20X20.y <-seasondata$WinPct20.y*seasondata$GameDummyLess20
seasondata$WinPct10X20.y <- seasondata$WinPct10.y*seasondata$GameDummyLess20
seasondata$WinPct5X20.y <- seasondata$WinPct5.y*seasondata$GameDummyLess20
seasondata$AdjNetRating20X20.x <- seasondata$AdjNetRating20.x*seasondata$GameDummyLess20
seasondata$AdjNetRating20X20.y <- seasondata$AdjNetRating20.y*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.x <- seasondata$AdjNetRating10.x*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.y <- seasondata$AdjNetRating10.y*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.x <- seasondata$AdjNetRating5.x*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.y <- seasondata$AdjNetRating5.y*seasondata$GameDummyLess20
seasondata$OREBPerPoss.x <- NA
seasondata$OREBPerPoss.y <- NA
seasondata$DefOREBPerPoss.x <- NA
seasondata$DefOREBPerPoss.y <- NA
seasondata$DREBPerPoss.x <- NA
seasondata$DREBPerPoss.y <- NA
seasondata$DefDREBPerPoss.x <- NA
seasondata$DefDREBPerPoss.y <- NA
seasondata$ASTPerPoss.x <- NA
seasondata$ASTPerPoss.y <- NA
seasondata$DefASTPerPoss.x <- NA
seasondata$DefASTPerPoss.y <- NA
seasondata$STLPerPoss.x <- NA
seasondata$STLPerPoss.y <- NA
seasondata$DefSTLPerPoss.x <- NA
seasondata$DefSTLPerPoss.y <- NA
seasondata$BLKPerPoss.x <- NA
seasondata$BLKPerPoss.y <- NA
seasondata$DefBLKPerPoss.x <- NA
seasondata$DefBLKPerPoss.y <- NA
seasondata$TOPerPoss.x <- NA
seasondata$TOPerPoss.y <- NA
seasondata$DefTOPerPoss.x <- NA
seasondata$DefTOPerPoss.y <- NA
seasondata$ThrPMPerPoss.x <- NA
seasondata$ThrPMPerPoss.y <- NA
seasondata$DefThrPMPerPoss.x <- NA
seasondata$DefThrPMPerPoss.y <- NA
seasondata$ThrPAPerPoss.x <- NA
seasondata$ThrPAPerPoss.y <- NA
seasondata$DefThrPAPerPoss.x <- NA
seasondata$DefThrPAPerPoss.y <- NA
seasondata$TwoPMPerPoss.x <- NA
seasondata$TwoPMPerPoss.y <- NA
seasondata$DefTwoPMPerPoss.x <- NA
seasondata$DefTwoPMPerPoss.y <- NA
seasondata$TwoPAPerPoss.x <- NA
seasondata$TwoPAPerPoss.y <- NA
seasondata$DefTwoPAPerPoss.x <- NA
seasondata$DefTwoPAPerPoss.y <- NA
seasondata$FTMPerPoss.x <- NA
seasondata$FTMPerPoss.y <- NA
seasondata$DefFTMPerPoss.x <- NA
seasondata$DefFTMPerPoss.y <- NA
seasondata$FTAPerPoss.x <- NA
seasondata$FTAPerPoss.y <- NA
seasondata$DefFTAPerPoss.x <- NA
seasondata$DefFTAPerPoss.y <- NA
seasondata$FGMPerPoss.x <- NA
seasondata$FGMPerPoss.y <- NA
seasondata$DefFGMPerPoss.x <- NA
seasondata$DefFGMPerPoss.y <- NA
seasondata$FGAPerPoss.x <- NA
seasondata$FGAPerPoss.y <- NA
seasondata$DefFGAPerPoss.x <- NA
seasondata$DefFGAPerPoss.y <- NA
seasondata$DREBrt.x <- seasondata$DREBPerPoss.x/(seasondata$DefFGAPerPoss.x - seasondata$DefFGMPerPoss.x)
seasondata$DREBrt.y <- seasondata$DREBPerPoss.y/(seasondata$DefFGAPerPoss.y - seasondata$DefFGMPerPoss.y)
seasondata$OREBrt.x <- seasondata$OREBPerPoss.x/(seasondata$FGAPerPoss.x - seasondata$FGMPerPoss.x)
seasondata$OREBrt.y <- seasondata$OREBPerPoss.y/(seasondata$FGAPerPoss.y - seasondata$FGMPerPoss.y)
seasondata$FGMissEST <- (((seasondata$DefFGAPerPoss.x + seasondata$FGAPerPoss.x) - (seasondata$DefFGMPerPoss.x + seasondata$FGMPerPoss.x)) + ((seasondata$DefFGAPerPoss.y + seasondata$FGAPerPoss.y) - (seasondata$DefFGMPerPoss.y + seasondata$FGMPerPoss.y)))/2
seasondata$FGMissEST.x <- ((seasondata$DefFGAPerPoss.y - seasondata$DefFGMPerPoss.y) + (seasondata$FGAPerPoss.x - seasondata$FGMPerPoss.x))/2
seasondata$FGMissEST.y <- ((seasondata$DefFGAPerPoss.x - seasondata$DefFGMPerPoss.x) + (seasondata$FGAPerPoss.y - seasondata$FGMPerPoss.y))/2
seasondata$DREBrtXFGMiss.x <- seasondata$DREBrt.x*seasondata$FGMissEST
seasondata$DREBrtXFGMiss.y <- seasondata$DREBrt.y*seasondata$FGMissEST
seasondata$OREBrtXFGMiss.x <- seasondata$OREBrt.x*seasondata$FGMissEST.x
seasondata$OREBrtXFGMiss.y <- seasondata$OREBrt.y*seasondata$FGMissEST.y
seasondata$ThrPercent.x <- seasondata$ThrPMPerPoss.x/seasondata$ThrPAPerPoss.x
seasondata$ThrPercent.y <- seasondata$ThrPMPerPoss.y/seasondata$ThrPAPerPoss.y
seasondata$TwoPercent.x <- seasondata$TwoPMPerPoss.x/seasondata$TwoPAPerPoss.x
seasondata$TwoPercent.y <- seasondata$TwoPMPerPoss.y/seasondata$TwoPAPerPoss.y
seasondata$FTPercent.x <- seasondata$FTMPerPoss.x/seasondata$FTAPerPoss.x
seasondata$FTPercent.y <- seasondata$FTMPerPoss.y/seasondata$FTAPerPoss.y
seasondata$DefThrPercent.x <- seasondata$DefThrPMPerPoss.x/seasondata$DefThrPAPerPoss.x
seasondata$DefThrPercent.y <- seasondata$DefThrPMPerPoss.y/seasondata$DefThrPAPerPoss.y
seasondata$DefTwoPercent.x <- seasondata$DefTwoPMPerPoss.x/seasondata$DefTwoPAPerPoss.x
seasondata$DefTwoPercent.y <- seasondata$DefTwoPMPerPoss.y/seasondata$DefTwoPAPerPoss.y
seasondata$FTPercentXDefFTs.x <- seasondata$FTPercent.y*seasondata$DefFTAPerPoss.x
seasondata$FTPercentXDefFTs.y <- seasondata$FTPercent.x*seasondata$DefFTAPerPoss.y
seasondata$PrevThrPercent.x <- NA
seasondata$PrevThrPercent.y <- NA
seasondata$PrevThrPAPerPoss.x <- NA
seasondata$PrevThrPAPerPoss.y <- NA
seasondata$PrevThrPMPerPoss.x <- NA
seasondata$PrevThrPMPerPoss.y <- NA
seasondata$PrevDefThrPercent.x <- NA
seasondata$PrevDefThrPercent.y <- NA
seasondata$PrevDefThrPAPerPoss.x <- NA
seasondata$PrevDefThrPAPerPoss.y <- NA
seasondata$PrevDefThrPMPerPoss.x <- NA
seasondata$PrevDefThrPMPerPoss.y <- NA
seasondata$PrevTwoPercent.x <- NA
seasondata$PrevTwoPercent.y <- NA
seasondata$PrevTwoPAPerPoss.x <- NA
seasondata$PrevTwoPAPerPoss.y <- NA
seasondata$PrevTwoPMPerPoss.x <- NA
seasondata$PrevTwoPMPerPoss.y <- NA
seasondata$PrevDefTwoPercent.x <- NA
seasondata$PrevDefTwoPercent.y <- NA
seasondata$PrevDefTwoPAPerPoss.x <- NA
seasondata$PrevDefTwoPAPerPoss.y <- NA
seasondata$PrevDefTwoPMPerPoss.x <- NA
seasondata$PrevDefTwoPMPerPoss.y <- NA
seasondata$PrevFTPercent.x <- NA
seasondata$PrevFTPercent.y <- NA
seasondata$PrevFTAPerPoss.x <- NA
seasondata$PrevFTAPerPoss.y <- NA
seasondata$PrevFTMPerPoss.x <- NA
seasondata$PrevFTMPerPoss.y <- NA
seasondata$PrevFTPercentXDefFTs.x <- NA
seasondata$PrevFTPercentXDefFTs.y <- NA
seasondata$PrevDefFTAPerPoss.x <- NA
seasondata$PrevDefFTAPerPoss.y <- NA
seasondata$PrevDREBrt.x <- NA
seasondata$PrevDREBrt.y <- NA
seasondata$PrevOREBrt.x <- NA
seasondata$PrevOREBrt.y <- NA
seasondata$PrevDREBrtXFGMiss.x <- NA
seasondata$PrevDREBrtXFGMiss.y <- NA
seasondata$PrevOREBrtXFGMiss.x <- NA
seasondata$PrevOREBrtXFGMiss.y <- NA
seasondata$PrevASTPerPoss.x <- NA
seasondata$PrevASTPerPoss.y <- NA
seasondata$PrevSTLPerPoss.x <- NA
seasondata$PrevSTLPerPoss.y <- NA
seasondata$PrevBLKPerPoss.x <- NA
seasondata$PrevBLKPerPoss.y <- NA
seasondata$PrevTOPerPoss.x <- NA
seasondata$PrevTOPerPoss.y <- NA
seasondata$PrevDefASTPerPoss.x <- NA
seasondata$PrevDefASTPerPoss.y <- NA
seasondata$PrevDefSTLPerPoss.x <- NA
seasondata$PrevDefSTLPerPoss.y <- NA
seasondata$PrevDefBLKPerPoss.x <- NA
seasondata$PrevDefBLKPerPoss.y <- NA
seasondata$PrevDefTOPerPoss.x <- NA
seasondata$PrevDefTOPerPoss.y <- NA

#V4 update
seasondata$GameDescription <- paste(seasondata$Team.y," at ",seasondata$Team.x,sep="")
seasondata$PrevPtsPerPossAvg.x <- NA
seasondata$PrevPtsPerPossAvg.y <- NA
seasondata$PrevDefPtsPerPossAvg.x <- NA
seasondata$PrevDefPtsPerPossAvg.y <- NA
seasondata$PrevOppPtsPerPossAvg.x <- NA
seasondata$PrevOppPtsPerPossAvg.y <- NA
seasondata$PrevOppDefPtsPerPossAvg.x <- NA
seasondata$PrevOppDefPtsPerPossAvg.y <- NA
seasondata$PrevPace.x <- NA
seasondata$PrevPace.y <- NA
seasondata$PrevWinPct.x <- NA
seasondata$PrevWinPct.y <- NA
seasondata$NumGamesWeek.x <- NA
seasondata$NumHomeGamesWeek.x <- NA
seasondata$NumGamesWeek.y <- NA 
seasondata$NumHomeGamesWeek.y <- NA
seasondata$PtsPerPossAvg2.x  <- 1*seasondata$PtsPerPossAvg.x  + 0*seasondata$PrevPtsPerPossAvg.x 
seasondata$DefPtsPerPossAvg2.x  <- 1*seasondata$DefPtsPerPossAvg.x  + 0*seasondata$PrevDefPtsPerPossAvg.x 
seasondata$OppPtsPerPossAvg2.x  <- 1*seasondata$OppPtsPerPossAvg.x  + 0*seasondata$PrevOppPtsPerPossAvg.x 
seasondata$OppDefPtsPerPossAvg2.x  <- 1*seasondata$OppDefPtsPerPossAvg.x  + 0*seasondata$PrevOppDefPtsPerPossAvg.x 
seasondata$Pace2.x  <- 1*seasondata$Pace.x  + 0*seasondata$PrevPace.x 
seasondata$WinPct2.x  <- 1*seasondata$WinPct.x  + 0*seasondata$PrevWinPct.x 
seasondata$PtsPerPossAvg2.y  <- 1*seasondata$PtsPerPossAvg.y  + 0*seasondata$PrevPtsPerPossAvg.y 
seasondata$DefPtsPerPossAvg2.y  <- 1*seasondata$DefPtsPerPossAvg.y  + 0*seasondata$PrevDefPtsPerPossAvg.y 
seasondata$OppPtsPerPossAvg2.y  <- 1*seasondata$OppPtsPerPossAvg.y  + 0*seasondata$PrevOppPtsPerPossAvg.y 
seasondata$OppDefPtsPerPossAvg2.y  <- 1*seasondata$OppDefPtsPerPossAvg.y  + 0*seasondata$PrevOppDefPtsPerPossAvg.y 
seasondata$Pace2.y  <- 1*seasondata$Pace.y  + 0*seasondata$PrevPace.y 
seasondata$WinPct2.y  <- 1*seasondata$WinPct.y  + 0*seasondata$PrevWinPct.y 
seasondata$AdjNetRating2.x <- (seasondata$PtsPerPossAvg2.x - seasondata$DefPtsPerPossAvg2.x) + (seasondata$OppPtsPerPossAvg2.x - seasondata$OppDefPtsPerPossAvg2.x)
seasondata$NetRating2.x <- (seasondata$PtsPerPossAvg2.x - seasondata$DefPtsPerPossAvg2.x)
seasondata$AdjNetRating2.y <- (seasondata$PtsPerPossAvg2.y - seasondata$DefPtsPerPossAvg2.y) + (seasondata$OppPtsPerPossAvg2.y - seasondata$OppDefPtsPerPossAvg2.y)
seasondata$NetRating2.y <- (seasondata$PtsPerPossAvg2.y - seasondata$DefPtsPerPossAvg2.y)
seasondata$PaceXPace2 <- seasondata$Pace2.x*seasondata$Pace2.y
seasondata$PaceXSq2 <- seasondata$Pace2.x*seasondata$Pace2.x
seasondata$PaceYSq2 <- seasondata$Pace2.y*seasondata$Pace2.y
seasondata$AdjNetMarSqPlus2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^2*((seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)>0)
seasondata$AdjNetMarSqNeg2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^2*((seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)<=0)
seasondata$AdjNetMarCub2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^3
seasondata$ShrHomeWeek.x <- ifelse(seasondata$NumGamesWeek.x==0,.5,seasondata$NumHomeGamesWeek.x/seasondata$NumGamesWeek.x)
seasondata$ShrHomeWeek.y <- ifelse(seasondata$NumGamesWeek.y==0,0.5,seasondata$NumHomeGamesWeek.y/seasondata$NumGamesWeek.y)
seasondata$ThrPercentXX.x <- seasondata$ThrPercent.x*seasondata$DefThrPercent.y 
seasondata$ThrPAPerPossXX.x <- seasondata$ThrPAPerPoss.x*seasondata$DefThrPAPerPoss.y
seasondata$ThrPMPerPossXX.x <- seasondata$ThrPMPerPoss.x*seasondata$DefThrPMPerPoss.y
seasondata$TwoPercentXX.x <- seasondata$TwoPercent.x*seasondata$DefTwoPercent.y 
seasondata$TwoPAPerPossXX.x <- seasondata$TwoPAPerPoss.x*seasondata$DefTwoPAPerPoss.y
seasondata$TwoPMPerPossXX.x <- seasondata$TwoPMPerPoss.x*seasondata$DefTwoPMPerPoss.y
seasondata$FTAPerPossXX.x <- seasondata$FTAPerPoss.x*seasondata$DefFTAPerPoss.y
seasondata$DREBRtXX.x <- seasondata$DREBrt.x*seasondata$OREBrt.y
seasondata$DREBTotXX.x <- seasondata$DREBrtXFGMiss.x*seasondata$OREBrtXFGMiss.y
seasondata$ASTPerPossXX.x <- seasondata$ASTPerPoss.x*seasondata$DefASTPerPoss.y 
seasondata$TOPerPossXX.x <- seasondata$TOPerPoss.x*seasondata$DefTOPerPoss.y 
seasondata$ThrPercentXX.y <- seasondata$ThrPercent.y*seasondata$DefThrPercent.x 
seasondata$ThrPAPerPossXX.y <- seasondata$ThrPAPerPoss.y*seasondata$DefThrPAPerPoss.x
seasondata$ThrPMPerPossXX.y <- seasondata$ThrPMPerPoss.y*seasondata$DefThrPMPerPoss.x
seasondata$TwoPercentXX.y <- seasondata$TwoPercent.y*seasondata$DefTwoPercent.x 
seasondata$TwoPAPerPossXX.y <- seasondata$TwoPAPerPoss.y*seasondata$DefTwoPAPerPoss.x
seasondata$TwoPMPerPossXX.y <- seasondata$TwoPMPerPoss.y*seasondata$DefTwoPMPerPoss.x
seasondata$FTAPerPossXX.y <- seasondata$FTAPerPoss.y*seasondata$DefFTAPerPoss.x
seasondata$DREBRtXX.y <- seasondata$DREBrt.y*seasondata$OREBrt.x
seasondata$DREBTotXX.y <- seasondata$DREBrtXFGMiss.y*seasondata$OREBrtXFGMiss.x
seasondata$ASTPerPossXX.y <- seasondata$ASTPerPoss.y*seasondata$DefASTPerPoss.x 
seasondata$TOPerPossXX.y <- seasondata$TOPerPoss.y*seasondata$DefTOPerPoss.x 
# estimate distance traveled for games
NBAdistance$Description3 <- str_replace(NBAdistance$Description1,"New York Knicks","Brooklyn Nets")
NBAdistance$Description4 <- str_replace(NBAdistance$Description2,"New York Knicks","Brooklyn Nets")
NBAdistance$Description5 <- str_replace(NBAdistance$Description1,"LA Clippers","Los Angeles Lakers")
NBAdistance$Description6 <- str_replace(NBAdistance$Description2,"LA Clippers","Los Angeles Lakers")
NBAdistance$Description7 <- str_replace(NBAdistance$Description1,"Brooklyn Nets","New York Knicks")
NBAdistance$Description8 <- str_replace(NBAdistance$Description2,"Brooklyn Nets","New York Knicks")
NBAdistance$Description9 <- str_replace(NBAdistance$Description1,"Los Angeles Lakers","LA Clippers")
NBAdistance$Description10 <- str_replace(NBAdistance$Description2,"Los Angeles Lakers","LA Clippers")
NBAdistance1 <- NBAdistance[,c(1,2)]
NBAdistance2 <- NBAdistance[,c(1,3)]
NBAdistance3 <- NBAdistance[,c(1,4)]
NBAdistance4 <- NBAdistance[,c(1,5)]
NBAdistance5 <- NBAdistance[,c(1,6)]
NBAdistance6 <- NBAdistance[,c(1,7)]
NBAdistance7 <- NBAdistance[,c(1,8)]
NBAdistance8 <- NBAdistance[,c(1,9)]
NBAdistance9 <- NBAdistance[,c(1,10)]
NBAdistance10 <- NBAdistance[,c(1,11)]
NBAdistance1 <- dplyr::rename(NBAdistance1, GameDescription = Description1)
NBAdistance2 <- dplyr::rename(NBAdistance2, GameDescription = Description2)
NBAdistance3 <- dplyr::rename(NBAdistance3, GameDescription = Description3)
NBAdistance4 <- dplyr::rename(NBAdistance4, GameDescription = Description4)
NBAdistance5 <- dplyr::rename(NBAdistance5, GameDescription = Description5)
NBAdistance6 <- dplyr::rename(NBAdistance6, GameDescription = Description6)
NBAdistance7 <- dplyr::rename(NBAdistance7, GameDescription = Description7)
NBAdistance8 <- dplyr::rename(NBAdistance8, GameDescription = Description8)
NBAdistance9 <- dplyr::rename(NBAdistance9, GameDescription = Description9)
NBAdistance10 <- dplyr::rename(NBAdistance10, GameDescription = Description10)
NBAdistanceMERGE <- rbind(NBAdistance1,NBAdistance2,NBAdistance3,NBAdistance4,NBAdistance5,NBAdistance6,NBAdistance7,NBAdistance8,NBAdistance9,NBAdistance10)
NBAdistanceMERGE <- distinct(NBAdistanceMERGE)
seasondata <- merge(seasondata,NBAdistanceMERGE, by=c("GameDescription"),all.x=TRUE,all.y=FALSE)
seasondata$mi_to_place[seasondata$GameDescription=="Brooklyn Nets at New York Knicks"] <- 0
seasondata$mi_to_place[seasondata$GameDescription=="New York Knicks at Brooklyn Nets"] <- 0
seasondata$mi_to_place[seasondata$GameDescription=="Los Angeles Lakers at LA Clippers"] <- 0
seasondata$mi_to_place[seasondata$GameDescription=="LA Clippers at Los Angeles Lakers"] <- 0
seasondata$mi_to_place[seasondata$GameDescription=="Los Angeles Lakers at New York Knicks"] <- 2458.96962
seasondata$mi_to_place[seasondata$GameDescription=="New York Knicks at Los Angeles Lakers"] <- 2458.96962
seasondata$DistDummy1 <- ifelse(seasondata$mi_to_place==0,1,0)
seasondata$DistDummy2 <- ifelse(seasondata$mi_to_place>0&seasondata$mi_to_place<=200,1,0)
seasondata$DistDummy3 <- ifelse(seasondata$mi_to_place>200&seasondata$mi_to_place<=500,1,0)
seasondata$DistDummy4 <- ifelse(seasondata$mi_to_place>500&seasondata$mi_to_place<=1000,1,0)
seasondata$DistDummy5 <- ifelse(seasondata$mi_to_place>1000&seasondata$mi_to_place<=1500,1,0)
seasondata$DistDummy6 <- ifelse(seasondata$mi_to_place>1500&seasondata$mi_to_place<=2000,1,0)
seasondata$DistDummy7 <- ifelse(seasondata$mi_to_place>2000,1,0)
seasondata$TotalPoints <- seasondata$PTS.x + seasondata$PTS.y
seasondata$TotalPointsPerPoss <- seasondata$PtsPerPoss.x + seasondata$PtsPerPoss.y
#####################

seasondataTEMP <- seasondata
seasondata <- seasondata_master
rm(seasondata_master)
seasondata <- rbind(data.frame(seasondata),data.frame(seasondataTEMP))
seasondata <- dplyr::rename(seasondata,`3PM.x`=`X3PM.x`)
seasondata <- dplyr::rename(seasondata,`3PA.x`=`X3PA.x`,`3PA.x`=`X3PA.x`,`2PA.x`=`X2PA.x`,`2PM.x`=`X2PM.x`,`FG%.x`=`FG..x`)
seasondata <- dplyr::rename(seasondata,`FT%.x`=`FT..x`,`2P%.x`=`X2P..x`,`3P%.x`=`X3P..x`)
seasondata <- dplyr::rename(seasondata,`3PM.y`=`X3PM.y`)
seasondata <- dplyr::rename(seasondata,`3PA.y`=`X3PA.y`,`3PA.y`=`X3PA.y`,`2PA.y`=`X2PA.y`,`2PM.y`=`X2PM.y`,`FG%.y`=`FG..y`)
seasondata <- dplyr::rename(seasondata,`FT%.y`=`FT..y`,`2P%.y`=`X2P..y`,`3P%.y`=`X3P..y`)
seasondata <- seasondata[order(seasondata$GameID,seasondata$Date,seasondata$PTS.x),]
seasondata <- distinct(seasondata,GameID, Date, GameDescription,.keep_all = TRUE)
seasondata <- seasondata[order(seasondata$Date,seasondata$GameID),]
seasondataOLD <- seasondata[seasondata$Date<startdate,]
start <- length(seasondataOLD$Date)
if (start==0) {start <- 1}
#for (i in 23360:length(seasondata$Date)) {

func_seasondata0 <- function(i) {
  
  GameCount.x <- as.numeric(length(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<=seasondata$Date[i]&seasondata$Season==seasondata$Season[i]]))
  GameCount.y <- as.numeric(length(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<=seasondata$Date[i]&seasondata$Season==seasondata$Season[i]]))
  value <- data.frame(GameCount.x,GameCount.y)                                                                                                                                                                                                                                
  return(value)
  
}
tic()
numCores <- detectCores()
seasondata_add <- mclapply(c(start:length(seasondata$Date)), func_seasondata0,mc.cores = numCores)
seasondata_add <- dplyr::bind_rows(seasondata_add, .id = "column_label")
toc()
names(seasondata[start:length(seasondata$Date),c("GameCount.x","GameCount.y")])==names(dplyr::select(seasondata_add,-c("column_label")))
seasondata[start:length(seasondata$Date),c("GameCount.x","GameCount.y")] <- dplyr::select(seasondata_add,-c("column_label"))


func_seasondata1 <- function(i) {
  
  Wins.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0)
  Losses.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0)
  Wins20.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))
  Losses20.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))
  Wins10.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))
  Losses10.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))
  Wins5.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))
  Losses5.x <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))
  
  Wins.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0)
  Losses.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0)
  Wins20.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))
  Losses20.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))
  Wins10.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))
  Losses10.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))
  Wins5.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))
  Losses5.y <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))
  
  
  PtsPerPossAvg.x <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]) 
                                         + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]) 
                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))])))
  
  PtsPerPossAvg.y <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]) 
                                         + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))]) 
                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&!is.na(seasondata$PTS.x[i]))])))
  DefPtsPerPossAvg.x <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]) 
                                            + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]) 
                                                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])])))
  
  DefPtsPerPossAvg.y <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]) 
                                            + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])]) 
                                                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&!is.na(seasondata$PTS.x[i])])))
  
  #last 20
  PtsPerPossAvg20.x <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19))]) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19))]) 
                                                                                                                                                                                                                                                  + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))])))
  
  PtsPerPossAvg20.y <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19))]) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19))]) 
                                                                                                                                                                                                                                                  + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))])))
  DefPtsPerPossAvg20.x <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)]) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)]) 
                                                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)])))
  
  DefPtsPerPossAvg20.y <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)]) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)]) 
                                                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)])))
  
  #last 10
  PtsPerPossAvg10.x <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9))]) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9))]) 
                                                                                                                                                                                                                                                 + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))])))
  
  PtsPerPossAvg10.y <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9))]) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9))]) 
                                                                                                                                                                                                                                                 + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))])))
  DefPtsPerPossAvg10.x <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)]) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)]) 
                                                                                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)])))
  
  DefPtsPerPossAvg10.y <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)]) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)]) 
                                                                                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)])))
  #last 5
  PtsPerPossAvg5.x <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4))]) 
                                          + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4))]) 
                                                                                                                                                                                                                                                + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))])))
  
  PtsPerPossAvg5.y <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4))]) 
                                          + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4))]) 
                                                                                                                                                                                                                                                + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))])))
  DefPtsPerPossAvg5.x <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)]) 
                                             + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)]) 
                                                                                                                                                                                                                                                   + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)])))
  
  DefPtsPerPossAvg5.y <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)]) 
                               + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)]) 
                                                                                                                                                                                                                                                   + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)]))) 
  
  
  BtoB.x <- ifelse(is_empty(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&(seasondata$Date==(seasondata$Date[i]-1))]),0,1)
  BtoB.y <- ifelse(is_empty(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&(seasondata$Date==(seasondata$Date[i]-1))]),0,1)
  
  
  value <- data.frame(Wins.x,Losses.x,Wins20.x,Losses20.x,Wins10.x,
                      Losses10.x,Wins5.x,Losses5.x,Wins.y,Losses.y,Wins20.y,Losses20.y,Wins10.y,
                      Losses10.y,Wins5.y,Losses5.y,PtsPerPossAvg.x,PtsPerPossAvg.y,
                      DefPtsPerPossAvg.x,DefPtsPerPossAvg.y,PtsPerPossAvg20.x,PtsPerPossAvg20.y,
                      DefPtsPerPossAvg20.x,DefPtsPerPossAvg20.y,PtsPerPossAvg10.x,PtsPerPossAvg10.y,
                      DefPtsPerPossAvg10.x,DefPtsPerPossAvg10.y,PtsPerPossAvg5.x,PtsPerPossAvg5.y,
                      DefPtsPerPossAvg5.x,DefPtsPerPossAvg5.y,BtoB.x,BtoB.y)                                                                                                                                                                                                                                
  return(value)
}
tic()
numCores <- detectCores()
seasondata_add <- mclapply(c(start:length(seasondata$Date)), func_seasondata1,mc.cores = numCores)
seasondata_add <- dplyr::bind_rows(seasondata_add, .id = "column_label")
toc()
names(seasondata[start:length(seasondata$Date),c("Wins.x","Losses.x",
                                           "Wins20.x","Losses20.x","Wins10.x","Losses10.x",
                                           "Wins5.x","Losses5.x","Wins.y","Losses.y","Wins20.y",
                                           "Losses20.y","Wins10.y","Losses10.y","Wins5.y","Losses5.y",
                                           "PtsPerPossAvg.x","PtsPerPossAvg.y","DefPtsPerPossAvg.x",
                                           "DefPtsPerPossAvg.y","PtsPerPossAvg20.x","PtsPerPossAvg20.y",
                                           "DefPtsPerPossAvg20.x","DefPtsPerPossAvg20.y","PtsPerPossAvg10.x",
                                           "PtsPerPossAvg10.y","DefPtsPerPossAvg10.x","DefPtsPerPossAvg10.y",
                                           "PtsPerPossAvg5.x","PtsPerPossAvg5.y","DefPtsPerPossAvg5.x",
                                           "DefPtsPerPossAvg5.y","BtoB.x","BtoB.y")])==names(dplyr::select(seasondata_add,-c("column_label")))
seasondata[start:length(seasondata$Date),c("Wins.x","Losses.x",
                                           "Wins20.x","Losses20.x","Wins10.x","Losses10.x",
                                           "Wins5.x","Losses5.x","Wins.y","Losses.y","Wins20.y",
                                           "Losses20.y","Wins10.y","Losses10.y","Wins5.y","Losses5.y",
                                           "PtsPerPossAvg.x","PtsPerPossAvg.y","DefPtsPerPossAvg.x",
                                           "DefPtsPerPossAvg.y","PtsPerPossAvg20.x","PtsPerPossAvg20.y",
                                           "DefPtsPerPossAvg20.x","DefPtsPerPossAvg20.y","PtsPerPossAvg10.x",
                                           "PtsPerPossAvg10.y","DefPtsPerPossAvg10.x","DefPtsPerPossAvg10.y",
                                           "PtsPerPossAvg5.x","PtsPerPossAvg5.y","DefPtsPerPossAvg5.x",
                                           "DefPtsPerPossAvg5.y","BtoB.x","BtoB.y")] <- dplyr::select(seasondata_add,-c("column_label"))

teammerge <- expand.grid(unique(seasondata$Date), unique(seasondata$Team.x))
teammerge <- dplyr::rename(teammerge, Date=Var1,Team=Var2)
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[,c("Date","Team.x","PtsPerPossAvg.x","DefPtsPerPossAvg.x")],Team=Team.x),by=c("Date","Team"))
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[,c("Date","Team.y","PtsPerPossAvg.y","DefPtsPerPossAvg.y")],Team=Team.y),by=c("Date","Team"))
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[,c("Date","Team.x","PTS.x","PTS.y","Poss")],Team=Team.x),by=c("Date","Team"))
teammerge <- teammerge %>% left_join(dplyr::rename(seasondata[,c("Date","Team.y","PTS.y","PTS.x","Poss")],Team=Team.y),by=c("Date","Team"))
teammerge$PTS.x.x[is.na(teammerge$PTS.x.x)] <- 0
teammerge$PTS.y.x[is.na(teammerge$PTS.y.x)] <- 0
teammerge$PTS.y.y[is.na(teammerge$PTS.y.y)] <- 0
teammerge$PTS.x.y[is.na(teammerge$PTS.x.y)] <- 0
teammerge$Poss.x[is.na(teammerge$Poss.x)] <- 0
teammerge$Poss.y[is.na(teammerge$Poss.y)] <- 0
teammerge <- teammerge %>% left_join(distinct(seasondata,Date,Season),by=c("Date"))
teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(PtsPerPossAvg2 = 100*((cumsum(PTS.x.x)+cumsum(PTS.y.y))/((cumsum(Poss.x)+cumsum(Poss.y)))),
         TotalPoints = (cumsum(PTS.x.x)+cumsum(PTS.y.y))/((cumsum(Poss.x>0)+cumsum(Poss.y>0))),
         DefPtsPerPossAvg2 = 100*((cumsum(PTS.y.x)+cumsum(PTS.x.y))/((cumsum(Poss.x)+cumsum(Poss.y)))),
         DefTotalPoints = (cumsum(PTS.y.x)+cumsum(PTS.x.y))/((cumsum(Poss.x>0)+cumsum(Poss.y>0)))) %>%
  ungroup()
teammerge <- teammerge %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(PtsPerPossAvg2 = sapply(1:n(), function(x) ifelse(x-1>0,PtsPerPossAvg2[x-1],0)),
         TotalPoints = sapply(1:n(), function(x) ifelse(x-1>0,TotalPoints[x-1],0)),
         DefPtsPerPossAvg2 = sapply(1:n(), function(x) ifelse(x-1>0,DefPtsPerPossAvg2[x-1],0)),
         DefTotalPoints = sapply(1:n(), function(x) ifelse(x-1>0,DefTotalPoints[x-1],0))) %>%
  ungroup()
teammerge$PtsPerPossAvg2[teammerge$PtsPerPossAvg2==0] <- NaN
teammerge$TotalPoints[teammerge$TotalPoints==0] <- NaN
teammerge$DefPtsPerPossAvg2[teammerge$DefPtsPerPossAvg2==0] <- NaN
teammerge$DefTotalPoints[teammerge$DefTotalPoints==0] <- NaN
teammerge <- distinct(teammerge)
teammerge <- dplyr::rename(teammerge[,c("Date","Team","PtsPerPossAvg2","DefPtsPerPossAvg2","TotalPoints","DefTotalPoints")],PtsPerPossAvg=PtsPerPossAvg2,DefPtsPerPossAvg=DefPtsPerPossAvg2)

seasondata$GameCount.x <- as.numeric(seasondata$GameCount.x)
seasondata$GameCount.y <- as.numeric(seasondata$GameCount.y)

func_seasondata2 <- function(i) {
  opplist.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i]],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i]])
  opplist.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i]],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i]])
  opplist20.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)])
  opplist20.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)])
  opplist10.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)])
  opplist10.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)])
  opplist5.x <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.x[i] & seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.x[i] & seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)])
  opplist5.y <- c(seasondata$Team.y[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.x==seasondata$Team.y[i] & seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)],seasondata$Team.x[seasondata$Season==seasondata$Season[i] & seasondata$Date < seasondata$Date[i] & seasondata$Team.y==seasondata$Team.y[i] & seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)])
  date.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist.x))
  date.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist.y))
  date20.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist20.x))
  date20.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist20.y))
  date10.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist10.x))
  date10.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist10.y))
  date5.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist5.x))
  date5.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i]),origin="1970-01-01"),length(opplist5.y))
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
  df.x <- df.x %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df.y <- df.y %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppPtsPerPossAvg.x <- mean(df.x$PtsPerPossAvg,na.rm = TRUE)
  OppPtsPerPossAvg.y <- mean(df.y$PtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg.x <- mean(df.x$DefPtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg.y <- mean(df.y$DefPtsPerPossAvg,na.rm = TRUE)
  df20.x <- dplyr::rename(df20.x,Date=date20.x,Team=opplist20.x)
  df20.y <- dplyr::rename(df20.y,Date=date20.y,Team=opplist20.y)
  df20.x <- df20.x %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df20.y <- df20.y %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppPtsPerPossAvg20.x <- mean(df20.x$PtsPerPossAvg,na.rm = TRUE)
  OppPtsPerPossAvg20.y <- mean(df20.y$PtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg20.x <- mean(df20.x$DefPtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg20.y <- mean(df20.y$DefPtsPerPossAvg,na.rm = TRUE)
  df10.x <- dplyr::rename(df10.x,Date=date10.x,Team=opplist10.x)
  df10.y <- dplyr::rename(df10.y,Date=date10.y,Team=opplist10.y)
  df10.x <- df10.x %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df10.y <- df10.y %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppPtsPerPossAvg10.x <- mean(df10.x$PtsPerPossAvg,na.rm = TRUE)
  OppPtsPerPossAvg10.y <- mean(df10.y$PtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg10.x <- mean(df10.x$DefPtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg10.y <- mean(df10.y$DefPtsPerPossAvg,na.rm = TRUE)
  df5.x <- dplyr::rename(df5.x,Date=date5.x,Team=opplist5.x)
  df5.y <- dplyr::rename(df5.y,Date=date5.y,Team=opplist5.y)
  df5.x <- df5.x %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df5.y <- df5.y %>% left_join(teammerge[,c("PtsPerPossAvg","DefPtsPerPossAvg","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppPtsPerPossAvg5.x <- mean(df5.x$PtsPerPossAvg,na.rm = TRUE)
  OppPtsPerPossAvg5.y <- mean(df5.y$PtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg5.x <- mean(df5.x$DefPtsPerPossAvg,na.rm = TRUE)
  OppDefPtsPerPossAvg5.y <- mean(df5.y$DefPtsPerPossAvg,na.rm = TRUE)
  
  value <- data.frame(OppPtsPerPossAvg.x,OppDefPtsPerPossAvg.x,OppPtsPerPossAvg20.x,OppDefPtsPerPossAvg20.x,OppPtsPerPossAvg10.x,OppDefPtsPerPossAvg10.x ,OppPtsPerPossAvg5.x,OppDefPtsPerPossAvg5.x,OppPtsPerPossAvg.y,OppDefPtsPerPossAvg.y,OppPtsPerPossAvg20.y,OppDefPtsPerPossAvg20.y,OppPtsPerPossAvg10.y,OppDefPtsPerPossAvg10.y,OppDefPtsPerPossAvg5.y,OppPtsPerPossAvg5.y)                                                                                                                                                                                                                          
  return(value)
}
tic()
numCores <- detectCores()
seasondata_add <- mclapply(c(start:length(seasondata$Date)), func_seasondata2,mc.cores = numCores)
seasondata_add <- dplyr::bind_rows(seasondata_add, .id = "column_label")
toc()
names(seasondata[start:length(seasondata$Date),c("OppPtsPerPossAvg.x","OppDefPtsPerPossAvg.x","OppPtsPerPossAvg20.x",
                                                 "OppDefPtsPerPossAvg20.x","OppPtsPerPossAvg10.x","OppDefPtsPerPossAvg10.x"
                                                 ,"OppPtsPerPossAvg5.x","OppDefPtsPerPossAvg5.x","OppPtsPerPossAvg.y",
                                                 "OppDefPtsPerPossAvg.y","OppPtsPerPossAvg20.y","OppDefPtsPerPossAvg20.y",
                                                 "OppPtsPerPossAvg10.y","OppDefPtsPerPossAvg10.y","OppDefPtsPerPossAvg5.y",
                                                 "OppPtsPerPossAvg5.y")])==names(dplyr::select(seasondata_add,-c("column_label")))
seasondata[start:length(seasondata$Date),c("OppPtsPerPossAvg.x","OppDefPtsPerPossAvg.x","OppPtsPerPossAvg20.x",
                                           "OppDefPtsPerPossAvg20.x","OppPtsPerPossAvg10.x","OppDefPtsPerPossAvg10.x"
                                           ,"OppPtsPerPossAvg5.x","OppDefPtsPerPossAvg5.x","OppPtsPerPossAvg.y",
                                           "OppDefPtsPerPossAvg.y","OppPtsPerPossAvg20.y","OppDefPtsPerPossAvg20.y",
                                           "OppPtsPerPossAvg10.y","OppDefPtsPerPossAvg10.y","OppDefPtsPerPossAvg5.y",
                                           "OppPtsPerPossAvg5.y")] <- dplyr::select(seasondata_add,-c("column_label"))

seasondata$AdjNetRating.x <- (seasondata$PtsPerPossAvg.x - seasondata$DefPtsPerPossAvg.x) + (seasondata$OppPtsPerPossAvg.x - seasondata$OppDefPtsPerPossAvg.x)
seasondata$NetRating.x <- (seasondata$PtsPerPossAvg.x - seasondata$DefPtsPerPossAvg.x)
seasondata$AdjNetRating.y <- (seasondata$PtsPerPossAvg.y - seasondata$DefPtsPerPossAvg.y) + (seasondata$OppPtsPerPossAvg.y - seasondata$OppDefPtsPerPossAvg.y)
seasondata$NetRating.y <- (seasondata$PtsPerPossAvg.y - seasondata$DefPtsPerPossAvg.y)

seasondata <- seasondata %>% 
  dplyr::mutate(GameCountAvg = (GameCount.y + GameCount.x)/2)
seasondata$Bubble<- ifelse(seasondata$Date>="2020-04-01"&seasondata$Date<="2020-10-20",1,0)
seasondata$Pandemic<- ifelse(seasondata$Date>="2020-12-01"&seasondata$Date<"2021-04-01",1,0)
seasondata$PartialPandemic<- ifelse(seasondata$Date>="2021-04-01"&seasondata$Date<="2021-09-01",1,0)

#create season number
seasondata <- seasondata %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = cur_group_id())

for (i in start:length(seasondata$Date)) {
  seasondata$PrevSeasonRating.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$AdjNetRating.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$AdjNetRating.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevSeasonRating.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$AdjNetRating.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$AdjNetRating.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
}


for (i in start:length(seasondata$Date)) {
  seasondata$Pace.x[i] <- mean(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]],na.rm=TRUE)
  seasondata$Pace.y[i] <- mean(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]],na.rm=TRUE)
  
}

seasondata$GameDummy5 <- ifelse(seasondata$GameCountAvg<=5,1,0)
seasondata$GameDummy10 <- ifelse(seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10,1,0)
seasondata$GameDummy15 <- ifelse(seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15,1,0)
seasondata$GameDummy20 <- ifelse(seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20,1,0)
seasondata$GameDummy25 <- ifelse(seasondata$GameCountAvg>20&seasondata$GameCountAvg<=25,1,0)
seasondata$GameDummy30 <- ifelse(seasondata$GameCountAvg>25&seasondata$GameCountAvg<=30,1,0)
seasondata$GameDummy35 <- ifelse(seasondata$GameCountAvg>30&seasondata$GameCountAvg<=35,1,0)
seasondata$GameDummy40 <- ifelse(seasondata$GameCountAvg>35&seasondata$GameCountAvg<=40,1,0)
seasondata$GameDummy45 <- ifelse(seasondata$GameCountAvg>40&seasondata$GameCountAvg<=45,1,0)
seasondata$GameDummy50 <- ifelse(seasondata$GameCountAvg>45&seasondata$GameCountAvg<=50,1,0)
seasondata$GameDummy55 <- ifelse(seasondata$GameCountAvg>50&seasondata$GameCountAvg<=55,1,0)
seasondata$GameDummy60 <- ifelse(seasondata$GameCountAvg>55&seasondata$GameCountAvg<=60,1,0)
seasondata$GameDummy65 <- ifelse(seasondata$GameCountAvg>60&seasondata$GameCountAvg<=65,1,0)
seasondata$GameDummy70 <- ifelse(seasondata$GameCountAvg>65&seasondata$GameCountAvg<=70,1,0)
seasondata$GameDummy82 <- ifelse(seasondata$GameCountAvg>70&seasondata$GameCountAvg<=82,1,0)


seasondata$GameDummy5XPrevSeas <- seasondata$GameDummy5*seasondata$PrevSeasonRating.x
seasondata$GameDummy10XPrevSeas <- seasondata$GameDummy10*seasondata$PrevSeasonRating.x
seasondata$GameDummy15XPrevSeas <- seasondata$GameDummy15*seasondata$PrevSeasonRating.x
seasondata$GameDummy20XPrevSeas <- seasondata$GameDummy20*seasondata$PrevSeasonRating.x
seasondata$GameDummy25XPrevSeas <- seasondata$GameDummy25*seasondata$PrevSeasonRating.x
seasondata$GameDummy30XPrevSeas <- seasondata$GameDummy30*seasondata$PrevSeasonRating.x
seasondata$GameDummy35XPrevSeas <- seasondata$GameDummy35*seasondata$PrevSeasonRating.x
seasondata$GameDummy40XPrevSeas <- seasondata$GameDummy40*seasondata$PrevSeasonRating.x
seasondata$GameDummy45XPrevSeas <- seasondata$GameDummy45*seasondata$PrevSeasonRating.x
seasondata$GameDummy50XPrevSeas <- seasondata$GameDummy50*seasondata$PrevSeasonRating.x
seasondata$GameDummy55XPrevSeas <- seasondata$GameDummy55*seasondata$PrevSeasonRating.x
seasondata$GameDummy60XPrevSeas <- seasondata$GameDummy60*seasondata$PrevSeasonRating.x
seasondata$GameDummy65XPrevSeas <- seasondata$GameDummy65*seasondata$PrevSeasonRating.x
seasondata$GameDummy70XPrevSeas <- seasondata$GameDummy70*seasondata$PrevSeasonRating.x
seasondata$GameDummy82XPrevSeas <- seasondata$GameDummy82*seasondata$PrevSeasonRating.x

seasondata$GameDummy5XPrevSeasY <- seasondata$GameDummy5*seasondata$PrevSeasonRating.y
seasondata$GameDummy10XPrevSeasY <- seasondata$GameDummy10*seasondata$PrevSeasonRating.y
seasondata$GameDummy15XPrevSeasY <- seasondata$GameDummy15*seasondata$PrevSeasonRating.y
seasondata$GameDummy20XPrevSeasY <- seasondata$GameDummy20*seasondata$PrevSeasonRating.y
seasondata$GameDummy25XPrevSeasY <- seasondata$GameDummy25*seasondata$PrevSeasonRating.y
seasondata$GameDummy30XPrevSeasY <- seasondata$GameDummy30*seasondata$PrevSeasonRating.y
seasondata$GameDummy35XPrevSeasY <- seasondata$GameDummy35*seasondata$PrevSeasonRating.y
seasondata$GameDummy40XPrevSeasY <- seasondata$GameDummy40*seasondata$PrevSeasonRating.y
seasondata$GameDummy45XPrevSeasY <- seasondata$GameDummy45*seasondata$PrevSeasonRating.y
seasondata$GameDummy50XPrevSeasY <- seasondata$GameDummy50*seasondata$PrevSeasonRating.y
seasondata$GameDummy55XPrevSeasY <- seasondata$GameDummy55*seasondata$PrevSeasonRating.y
seasondata$GameDummy60XPrevSeasY <- seasondata$GameDummy60*seasondata$PrevSeasonRating.y
seasondata$GameDummy65XPrevSeasY <- seasondata$GameDummy65*seasondata$PrevSeasonRating.y
seasondata$GameDummy70XPrevSeasY <- seasondata$GameDummy70*seasondata$PrevSeasonRating.y
seasondata$GameDummy82XPrevSeasY <- seasondata$GameDummy82*seasondata$PrevSeasonRating.y

seasondata$PaceXPace <- seasondata$Pace.x*seasondata$Pace.y
seasondata$PaceXSq <- seasondata$Pace.x*seasondata$Pace.x
seasondata$PaceYSq <- seasondata$Pace.y*seasondata$Pace.y

seasondata$HomeMargPerPoss <- seasondata$HomeMargin/(seasondata$Poss/100)


####VERSION 2 UPDATES
#add non-linearities in margin
seasondata$AdjNetMarSqPlus <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^2*((seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)>0)
seasondata$AdjNetMarSqNeg <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^2*((seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)<=0)
seasondata$AdjNetMarCub <- (seasondata$AdjNetRating.x-seasondata$AdjNetRating.y)^3

#allow for time trend in homefield advantage over time
seasondata$Year <- year(seasondata$Date)-2002
seasondata$YearSq <- seasondata$Year*seasondata$Year
seasondata$YearCub <- seasondata$Year*seasondata$Year*seasondata$Year

seasondata$WinPct.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins.x/(seasondata$Wins.x + seasondata$Losses.x))
seasondata$WinPct20.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins20.x/(seasondata$Wins20.x + seasondata$Losses20.x))
seasondata$WinPct10.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins10.x/(seasondata$Wins10.x + seasondata$Losses10.x))
seasondata$WinPct5.x <- ifelse((seasondata$Wins.x + seasondata$Losses.x)==0,.5,seasondata$Wins5.x/(seasondata$Wins5.x + seasondata$Losses5.x))
seasondata$WinPct.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins.y/(seasondata$Wins.y + seasondata$Losses.y))
seasondata$WinPct20.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins20.y/(seasondata$Wins20.y + seasondata$Losses20.y))
seasondata$WinPct10.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins10.y/(seasondata$Wins10.y + seasondata$Losses10.y))
seasondata$WinPct5.y <- ifelse((seasondata$Wins.y + seasondata$Losses.y)==0,.5,seasondata$Wins5.y/(seasondata$Wins5.y + seasondata$Losses5.y))

seasondata$AdjNetRating20.x <- (seasondata$PtsPerPossAvg20.x - seasondata$DefPtsPerPossAvg20.x) + (seasondata$OppPtsPerPossAvg20.x - seasondata$OppDefPtsPerPossAvg20.x)
seasondata$AdjNetRating20.y <- (seasondata$PtsPerPossAvg20.y - seasondata$DefPtsPerPossAvg20.y) + (seasondata$OppPtsPerPossAvg20.y - seasondata$OppDefPtsPerPossAvg20.y)
seasondata$AdjNetRating10.x <- (seasondata$PtsPerPossAvg10.x - seasondata$DefPtsPerPossAvg10.x) + (seasondata$OppPtsPerPossAvg10.x - seasondata$OppDefPtsPerPossAvg10.x)
seasondata$AdjNetRating10.y <- (seasondata$PtsPerPossAvg10.y - seasondata$DefPtsPerPossAvg10.y) + (seasondata$OppPtsPerPossAvg10.y - seasondata$OppDefPtsPerPossAvg10.y)
seasondata$AdjNetRating5.x <- (seasondata$PtsPerPossAvg5.x - seasondata$DefPtsPerPossAvg5.x) + (seasondata$OppPtsPerPossAvg5.x - seasondata$OppDefPtsPerPossAvg5.x)
seasondata$AdjNetRating5.y <- (seasondata$PtsPerPossAvg5.y - seasondata$DefPtsPerPossAvg5.y) + (seasondata$OppPtsPerPossAvg5.y - seasondata$OppDefPtsPerPossAvg5.y)

seasondata$GameDummyLess20 <- ifelse(seasondata$GameCountAvg<=20,1,0)

#interaction terms for recent performance
seasondata$WinPctX20.x <- seasondata$WinPct.x*seasondata$GameDummyLess20
seasondata$WinPct20X20.x <- seasondata$WinPct20.x*seasondata$GameDummyLess20
seasondata$WinPct10X20.x <- seasondata$WinPct10.x*seasondata$GameDummyLess20
seasondata$WinPct5X20.x <- seasondata$WinPct5.x*seasondata$GameDummyLess20
seasondata$WinPctX20.y <- seasondata$WinPct.y*seasondata$GameDummyLess20
seasondata$WinPct20X20.y <-seasondata$WinPct20.y*seasondata$GameDummyLess20
seasondata$WinPct10X20.y <- seasondata$WinPct10.y*seasondata$GameDummyLess20
seasondata$WinPct5X20.y <- seasondata$WinPct5.y*seasondata$GameDummyLess20

seasondata$AdjNetRating20X20.x <- seasondata$AdjNetRating20.x*seasondata$GameDummyLess20
seasondata$AdjNetRating20X20.y <- seasondata$AdjNetRating20.y*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.x <- seasondata$AdjNetRating10.x*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.y <- seasondata$AdjNetRating10.y*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.x <- seasondata$AdjNetRating5.x*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.y <- seasondata$AdjNetRating5.y*seasondata$GameDummyLess20

for (i in start:length(seasondata$Date)) {
  
  seasondata$OREBPerPoss.x[i] <- 100*((sum(seasondata$OREB.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                       + sum(seasondata$OREB.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$OREBPerPoss.y[i] <- 100*((sum(seasondata$OREB.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                       + sum(seasondata$OREB.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefOREBPerPoss.x[i] <- 100*((sum(seasondata$OREB.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                          + sum(seasondata$OREB.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefOREBPerPoss.y[i] <- 100*((sum(seasondata$OREB.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                          + sum(seasondata$OREB.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DREBPerPoss.x[i] <- 100*((sum(seasondata$DREB.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                       + sum(seasondata$DREB.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DREBPerPoss.y[i] <- 100*((sum(seasondata$DREB.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                       + sum(seasondata$DREB.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefDREBPerPoss.x[i] <- 100*((sum(seasondata$DREB.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                          + sum(seasondata$DREB.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefDREBPerPoss.y[i] <- 100*((sum(seasondata$DREB.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                          + sum(seasondata$DREB.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$ASTPerPoss.x[i] <- 100*((sum(seasondata$AST.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$AST.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$ASTPerPoss.y[i] <- 100*((sum(seasondata$AST.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$AST.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefASTPerPoss.x[i] <- 100*((sum(seasondata$AST.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$AST.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefASTPerPoss.y[i] <- 100*((sum(seasondata$AST.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$AST.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$STLPerPoss.x[i] <- 100*((sum(seasondata$STL.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$STL.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$STLPerPoss.y[i] <- 100*((sum(seasondata$STL.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$STL.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefSTLPerPoss.x[i] <- 100*((sum(seasondata$STL.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$STL.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefSTLPerPoss.y[i] <- 100*((sum(seasondata$STL.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$STL.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$BLKPerPoss.x[i] <- 100*((sum(seasondata$BLK.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$BLK.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$BLKPerPoss.y[i] <- 100*((sum(seasondata$BLK.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$BLK.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefBLKPerPoss.x[i] <- 100*((sum(seasondata$BLK.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$BLK.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefBLKPerPoss.y[i] <- 100*((sum(seasondata$BLK.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$BLK.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$TOPerPoss.x[i] <- 100*((sum(seasondata$TO.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                     + sum(seasondata$TO.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$TOPerPoss.y[i] <- 100*((sum(seasondata$TO.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                     + sum(seasondata$TO.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefTOPerPoss.x[i] <- 100*((sum(seasondata$TO.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$TO.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                       + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefTOPerPoss.y[i] <- 100*((sum(seasondata$TO.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$TO.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                       + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$ThrPMPerPoss.x[i] <- 100*((sum(seasondata$`3PM.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`3PM.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$ThrPMPerPoss.y[i] <- 100*((sum(seasondata$`3PM.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`3PM.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefThrPMPerPoss.x[i] <- 100*((sum(seasondata$`3PM.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`3PM.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefThrPMPerPoss.y[i] <- 100*((sum(seasondata$`3PM.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`3PM.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$ThrPAPerPoss.x[i] <- 100*((sum(seasondata$`3PA.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`3PA.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$ThrPAPerPoss.y[i] <- 100*((sum(seasondata$`3PA.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`3PA.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefThrPAPerPoss.x[i] <- 100*((sum(seasondata$`3PA.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`3PA.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefThrPAPerPoss.y[i] <- 100*((sum(seasondata$`3PA.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`3PA.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$TwoPMPerPoss.x[i] <- 100*((sum(seasondata$`2PM.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`2PM.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$TwoPMPerPoss.y[i] <- 100*((sum(seasondata$`2PM.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`2PM.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefTwoPMPerPoss.x[i] <- 100*((sum(seasondata$`2PM.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`2PM.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefTwoPMPerPoss.y[i] <- 100*((sum(seasondata$`2PM.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`2PM.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$TwoPAPerPoss.x[i] <- 100*((sum(seasondata$`2PA.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`2PA.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$TwoPAPerPoss.y[i] <- 100*((sum(seasondata$`2PA.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                        + sum(seasondata$`2PA.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefTwoPAPerPoss.x[i] <- 100*((sum(seasondata$`2PA.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`2PA.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefTwoPAPerPoss.y[i] <- 100*((sum(seasondata$`2PA.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                           + sum(seasondata$`2PA.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$FTMPerPoss.x[i] <- 100*((sum(seasondata$FTM.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FTM.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$FTMPerPoss.y[i] <- 100*((sum(seasondata$FTM.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FTM.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefFTMPerPoss.x[i] <- 100*((sum(seasondata$FTM.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FTM.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefFTMPerPoss.y[i] <- 100*((sum(seasondata$FTM.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FTM.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$FTAPerPoss.x[i] <- 100*((sum(seasondata$FTA.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FTA.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$FTAPerPoss.y[i] <- 100*((sum(seasondata$FTA.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FTA.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefFTAPerPoss.x[i] <- 100*((sum(seasondata$FTA.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FTA.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefFTAPerPoss.y[i] <- 100*((sum(seasondata$FTA.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FTA.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$FGMPerPoss.x[i] <- 100*((sum(seasondata$FGM.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FGM.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$FGMPerPoss.y[i] <- 100*((sum(seasondata$FGM.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FGM.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefFGMPerPoss.x[i] <- 100*((sum(seasondata$FGM.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FGM.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefFGMPerPoss.y[i] <- 100*((sum(seasondata$FGM.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FGM.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  seasondata$FGAPerPoss.x[i] <- 100*((sum(seasondata$FGA.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FGA.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$FGAPerPoss.y[i] <- 100*((sum(seasondata$FGA.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                      + sum(seasondata$FGA.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  seasondata$DefFGAPerPoss.x[i] <- 100*((sum(seasondata$FGA.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FGA.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  seasondata$DefFGAPerPoss.y[i] <- 100*((sum(seasondata$FGA.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                         + sum(seasondata$FGA.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
  
  
  
}

seasondata$DREBrt.x <- seasondata$DREBPerPoss.x/(seasondata$DefFGAPerPoss.x - seasondata$DefFGMPerPoss.x)
seasondata$DREBrt.y <- seasondata$DREBPerPoss.y/(seasondata$DefFGAPerPoss.y - seasondata$DefFGMPerPoss.y)
seasondata$OREBrt.x <- seasondata$OREBPerPoss.x/(seasondata$FGAPerPoss.x - seasondata$FGMPerPoss.x)
seasondata$OREBrt.y <- seasondata$OREBPerPoss.y/(seasondata$FGAPerPoss.y - seasondata$FGMPerPoss.y)
seasondata$FGMissEST <- (((seasondata$DefFGAPerPoss.x + seasondata$FGAPerPoss.x) - (seasondata$DefFGMPerPoss.x + seasondata$FGMPerPoss.x)) + ((seasondata$DefFGAPerPoss.y + seasondata$FGAPerPoss.y) - (seasondata$DefFGMPerPoss.y + seasondata$FGMPerPoss.y)))/2
seasondata$FGMissEST.x <- ((seasondata$DefFGAPerPoss.y - seasondata$DefFGMPerPoss.y) + (seasondata$FGAPerPoss.x - seasondata$FGMPerPoss.x))/2
seasondata$FGMissEST.y <- ((seasondata$DefFGAPerPoss.x - seasondata$DefFGMPerPoss.x) + (seasondata$FGAPerPoss.y - seasondata$FGMPerPoss.y))/2
seasondata$DREBrtXFGMiss.x <- seasondata$DREBrt.x*seasondata$FGMissEST
seasondata$DREBrtXFGMiss.y <- seasondata$DREBrt.y*seasondata$FGMissEST
seasondata$OREBrtXFGMiss.x <- seasondata$OREBrt.x*seasondata$FGMissEST.x
seasondata$OREBrtXFGMiss.y <- seasondata$OREBrt.y*seasondata$FGMissEST.y


seasondata$ThrPercent.x <- seasondata$ThrPMPerPoss.x/seasondata$ThrPAPerPoss.x
seasondata$ThrPercent.y <- seasondata$ThrPMPerPoss.y/seasondata$ThrPAPerPoss.y
seasondata$TwoPercent.x <- seasondata$TwoPMPerPoss.x/seasondata$TwoPAPerPoss.x
seasondata$TwoPercent.y <- seasondata$TwoPMPerPoss.y/seasondata$TwoPAPerPoss.y
seasondata$FTPercent.x <- seasondata$FTMPerPoss.x/seasondata$FTAPerPoss.x
seasondata$FTPercent.y <- seasondata$FTMPerPoss.y/seasondata$FTAPerPoss.y

seasondata$DefThrPercent.x <- seasondata$DefThrPMPerPoss.x/seasondata$DefThrPAPerPoss.x
seasondata$DefThrPercent.y <- seasondata$DefThrPMPerPoss.y/seasondata$DefThrPAPerPoss.y
seasondata$DefTwoPercent.x <- seasondata$DefTwoPMPerPoss.x/seasondata$DefTwoPAPerPoss.x
seasondata$DefTwoPercent.y <- seasondata$DefTwoPMPerPoss.y/seasondata$DefTwoPAPerPoss.y

#instead of including seasondata$DefFTMPerPoss, since FT % unaffected by defense
seasondata$FTPercentXDefFTs.x <- seasondata$FTPercent.y*seasondata$DefFTAPerPoss.x
seasondata$FTPercentXDefFTs.y <- seasondata$FTPercent.x*seasondata$DefFTAPerPoss.y


#get previous season rating, so that in beginning we mesh average with previous season
for (i in start:length(seasondata$Date)) {
  seasondata$PrevThrPercent.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$ThrPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevThrPercent.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$ThrPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevThrPAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$ThrPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevThrPAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$ThrPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevThrPMPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$ThrPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevThrPMPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$ThrPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ThrPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPercent.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefThrPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPercent.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefThrPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefThrPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefThrPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPMPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefThrPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefThrPMPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefThrPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefThrPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPercent.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$TwoPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPercent.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$TwoPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$TwoPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$TwoPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPMPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$TwoPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTwoPMPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$TwoPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TwoPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPercent.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefTwoPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPercent.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefTwoPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefTwoPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefTwoPAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPMPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefTwoPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTwoPMPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefTwoPMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTwoPMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  
  seasondata$PrevFTPercent.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$FTPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTPercent.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$FTPercent.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTPercent.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$FTAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$FTAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTMPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$FTMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTMPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$FTMPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTMPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTPercentXDefFTs.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$FTPercentXDefFTs.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTPercentXDefFTs.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevFTPercentXDefFTs.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$FTPercentXDefFTs.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$FTPercentXDefFTs.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefFTAPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefFTAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefFTAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefFTAPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefFTAPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefFTAPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  
  seasondata$PrevDREBrt.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DREBrt.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DREBrt.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDREBrt.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DREBrt.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DREBrt.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOREBrt.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$OREBrt.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OREBrt.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOREBrt.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$OREBrt.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OREBrt.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDREBrtXFGMiss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DREBrtXFGMiss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DREBrtXFGMiss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDREBrtXFGMiss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DREBrtXFGMiss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DREBrtXFGMiss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOREBrtXFGMiss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$OREBrtXFGMiss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OREBrtXFGMiss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOREBrtXFGMiss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$OREBrtXFGMiss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OREBrtXFGMiss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevASTPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$ASTPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ASTPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevASTPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$ASTPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$ASTPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevSTLPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$STLPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$STLPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevSTLPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$STLPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$STLPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevBLKPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$BLKPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$BLKPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevBLKPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$BLKPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$BLKPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTOPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$TOPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TOPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevTOPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$TOPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$TOPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefASTPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefASTPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefASTPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefASTPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefASTPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefASTPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefSTLPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefSTLPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefSTLPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefSTLPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefSTLPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefSTLPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefBLKPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefBLKPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefBLKPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefBLKPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefBLKPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefBLKPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTOPerPoss.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefTOPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTOPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefTOPerPoss.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefTOPerPoss.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefTOPerPoss.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  
}

#replace with last season's if game 1
seasondata$ThrPercent.x[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevThrPercent.x[seasondata$GameCountAvg<2] 
seasondata$ThrPercent.y[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevThrPercent.y[seasondata$GameCountAvg<2] 
seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevThrPAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevThrPAPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevThrPMPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevThrPMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefThrPercent.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPercent.x[seasondata$GameCountAvg<2] 
seasondata$DefThrPercent.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPercent.y[seasondata$GameCountAvg<2] 
seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPAPerPoss.y[seasondata$GameCountAvg<2]  
seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$TwoPercent.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPercent.x[seasondata$GameCountAvg<2] 
seasondata$TwoPercent.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPercent.y[seasondata$GameCountAvg<2] 
seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPAPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPMPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTwoPMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefTwoPercent.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPercent.x[seasondata$GameCountAvg<2] 
seasondata$DefTwoPercent.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPercent.y[seasondata$GameCountAvg<2] 
seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPAPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPMPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTwoPMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$FTPercent.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTPercent.x[seasondata$GameCountAvg<2] 
seasondata$FTPercent.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTPercent.y[seasondata$GameCountAvg<2] 
seasondata$FTAPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$FTAPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTAPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$FTMPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTMPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$FTMPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTMPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTPercentXDefFTs.x[seasondata$GameCountAvg<2] 
seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevFTPercentXDefFTs.y[seasondata$GameCountAvg<2] 
seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevDefFTAPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevDefFTAPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DREBrt.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDREBrt.x[seasondata$GameCountAvg<2] 
seasondata$DREBrt.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDREBrt.y[seasondata$GameCountAvg<2] 
seasondata$OREBrt.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevOREBrt.x[seasondata$GameCountAvg<2] 
seasondata$OREBrt.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevOREBrt.y[seasondata$GameCountAvg<2] 
seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevDREBrtXFGMiss.x[seasondata$GameCountAvg<2] 
seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevDREBrtXFGMiss.y[seasondata$GameCountAvg<2] 
seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevOREBrtXFGMiss.x[seasondata$GameCountAvg<2] 
seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevOREBrtXFGMiss.y[seasondata$GameCountAvg<2] 
seasondata$ASTPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevASTPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$ASTPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevASTPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$STLPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevSTLPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$STLPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevSTLPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$BLKPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevBLKPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$BLKPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevBLKPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$TOPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTOPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$TOPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevTOPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefASTPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefASTPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefASTPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefASTPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefSTLPerPoss.x[seasondata$GameCountAvg<2]  
seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg<2]  <- 1*seasondata$PrevDefSTLPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefBLKPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefBLKPerPoss.y[seasondata$GameCountAvg<2] 
seasondata$DefTOPerPoss.x[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTOPerPoss.x[seasondata$GameCountAvg<2] 
seasondata$DefTOPerPoss.y[seasondata$GameCountAvg<2]  <-  1*seasondata$PrevDefTOPerPoss.y[seasondata$GameCountAvg<2] 

#replace with 75% with last season if game 5 or less
seasondata$ThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  
seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTwoPMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTPercent.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTPercent.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTMPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTMPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTPercentXDefFTs.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevFTPercentXDefFTs.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefFTAPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefFTAPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$OREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$OREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevOREBrt.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$OREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$OREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevOREBrt.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevOREBrtXFGMiss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevOREBrtXFGMiss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$ASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$ASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$STLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$STLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevSTLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$STLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$STLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevSTLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$BLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$BLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevBLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$BLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$BLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevBLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$TOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$TOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevTOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefASTPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefASTPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefSTLPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  
seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefSTLPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefBLKPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefBLKPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTOPerPoss.x[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 
seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  <- .25*seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5]  + .75*seasondata$PrevDefTOPerPoss.y[seasondata$GameCountAvg>=2&seasondata$GameCountAvg<=5] 


#replace with 50% with last season if game 5 to Game 10
seasondata$ThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  
seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTwoPMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTPercent.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTPercent.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTMPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTMPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTPercentXDefFTs.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevFTPercentXDefFTs.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefFTAPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefFTAPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$OREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$OREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevOREBrt.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$OREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$OREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevOREBrt.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevOREBrtXFGMiss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevOREBrtXFGMiss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$ASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$ASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$STLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$STLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevSTLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$STLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$STLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevSTLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$BLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$BLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevBLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$BLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$BLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevBLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$TOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$TOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevTOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefASTPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefASTPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefSTLPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  
seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefSTLPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefBLKPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefBLKPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTOPerPoss.x[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 
seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  <- .5*seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10]  + .5*seasondata$PrevDefTOPerPoss.y[seasondata$GameCountAvg>5&seasondata$GameCountAvg<=10] 

#replace with 33% with last season if game 10 to 15
seasondata$ThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  
seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTwoPMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTPercent.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTPercent.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTMPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTMPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTPercentXDefFTs.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevFTPercentXDefFTs.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefFTAPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefFTAPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$OREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$OREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevOREBrt.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$OREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$OREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevOREBrt.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevOREBrtXFGMiss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevOREBrtXFGMiss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$ASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$ASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$STLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$STLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevSTLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$STLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$STLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevSTLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$BLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$BLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevBLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$BLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$BLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevBLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$TOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$TOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevTOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefASTPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefASTPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefSTLPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  
seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefSTLPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefBLKPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefBLKPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTOPerPoss.x[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 
seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  <- .67*seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15]  + .33*seasondata$PrevDefTOPerPoss.y[seasondata$GameCountAvg>10&seasondata$GameCountAvg<=15] 


#replace with 20% with last season if game 15 to 20
seasondata$ThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  
seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefThrPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTwoPMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTPercent.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTPercent.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTMPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTMPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTPercentXDefFTs.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTPercentXDefFTs.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$FTPercentXDefFTs.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevFTPercentXDefFTs.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefFTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefFTAPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefFTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefFTAPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$OREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$OREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevOREBrt.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$OREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$OREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevOREBrt.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$OREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevOREBrtXFGMiss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$OREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevOREBrtXFGMiss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$ASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$ASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$STLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$STLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevSTLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$STLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$STLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevSTLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$BLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$BLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevBLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$BLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$BLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevBLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$TOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$TOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevTOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefASTPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefASTPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefSTLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefSTLPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  
seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefSTLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefSTLPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefBLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefBLKPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefBLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefBLKPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTOPerPoss.x[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 
seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  <- .8*seasondata$DefTOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20]  + .2*seasondata$PrevDefTOPerPoss.y[seasondata$GameCountAvg>15&seasondata$GameCountAvg<=20] 


#Start V4 Updates
for (i in start:length(seasondata$Date)) {
  seasondata$PrevPtsPerPossAvg.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$PtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$PtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevPtsPerPossAvg.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$PtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$PtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefPtsPerPossAvg.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$DefPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevDefPtsPerPossAvg.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$DefPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$DefPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOppPtsPerPossAvg.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$OppPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OppPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOppPtsPerPossAvg.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$OppPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OppPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOppDefPtsPerPossAvg.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$OppDefPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OppDefPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevOppDefPtsPerPossAvg.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$OppDefPtsPerPossAvg.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$OppDefPtsPerPossAvg.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevPace.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$Pace.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$Pace.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevPace.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$Pace.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$Pace.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevWinPct.x[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.x[i],seasondata$WinPct.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$WinPct.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  seasondata$PrevWinPct.y[i] <- ifelse(seasondata$Team.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])]==seasondata$Team.y[i],seasondata$WinPct.x[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])],seasondata$WinPct.y[seasondata$GameID==max(seasondata$GameID[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$SeasNum==(seasondata$SeasNum[i]-1)])])
  
  seasondata$NumGamesWeek.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7))
  seasondata$NumHomeGamesWeek.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7))
  seasondata$NumGamesWeek.y[i] <- sum((seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7))
  seasondata$NumHomeGamesWeek.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7))
  
  
}

#replace with last season's if game >20
seasondata$PtsPerPossAvg2.x  <- 1*seasondata$PtsPerPossAvg.x  + 0*seasondata$PrevPtsPerPossAvg.x 
seasondata$DefPtsPerPossAvg2.x  <- 1*seasondata$DefPtsPerPossAvg.x  + 0*seasondata$PrevDefPtsPerPossAvg.x 
seasondata$OppPtsPerPossAvg2.x  <- 1*seasondata$OppPtsPerPossAvg.x  + 0*seasondata$PrevOppPtsPerPossAvg.x 
seasondata$OppDefPtsPerPossAvg2.x  <- 1*seasondata$OppDefPtsPerPossAvg.x  + 0*seasondata$PrevOppDefPtsPerPossAvg.x 
seasondata$Pace2.x  <- 1*seasondata$Pace.x  + 0*seasondata$PrevPace.x 
seasondata$WinPct2.x  <- 1*seasondata$WinPct.x  + 0*seasondata$PrevWinPct.x 
seasondata$PtsPerPossAvg2.y  <- 1*seasondata$PtsPerPossAvg.y  + 0*seasondata$PrevPtsPerPossAvg.y 
seasondata$DefPtsPerPossAvg2.y  <- 1*seasondata$DefPtsPerPossAvg.y  + 0*seasondata$PrevDefPtsPerPossAvg.y 
seasondata$OppPtsPerPossAvg2.y  <- 1*seasondata$OppPtsPerPossAvg.y  + 0*seasondata$PrevOppPtsPerPossAvg.y 
seasondata$OppDefPtsPerPossAvg2.y  <- 1*seasondata$OppDefPtsPerPossAvg.y  + 0*seasondata$PrevOppDefPtsPerPossAvg.y 
seasondata$Pace2.y  <- 1*seasondata$Pace.y  + 0*seasondata$PrevPace.y 
seasondata$WinPct2.y  <- 1*seasondata$WinPct.y  + 0*seasondata$PrevWinPct.y 


#replace with last season's if game 1
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==1] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==1] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==1] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==1] 
seasondata$Pace2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevPace.x[seasondata$GameCount.x==1] 
seasondata$WinPct2.x[seasondata$GameCount.x==1]  <- 1*seasondata$PrevWinPct.x[seasondata$GameCount.x==1] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==1] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==1] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==1] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==1] 
seasondata$Pace2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevPace.y[seasondata$GameCount.y==1] 
seasondata$WinPct2.y[seasondata$GameCount.y==1]  <- 1*seasondata$PrevWinPct.y[seasondata$GameCount.y==1] 

#replace with last season's if game 2
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==2] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==2] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==2] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==2] 
seasondata$Pace2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$Pace.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevPace.x[seasondata$GameCount.x==2] 
seasondata$WinPct2.x[seasondata$GameCount.x==2]  <- 0.05*seasondata$WinPct.x[seasondata$GameCount.x==2]  + .95*seasondata$PrevWinPct.x[seasondata$GameCount.x==2] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==2] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==2] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==2] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==2] 
seasondata$Pace2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$Pace.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevPace.y[seasondata$GameCount.y==2] 
seasondata$WinPct2.y[seasondata$GameCount.y==2]  <- 0.05*seasondata$WinPct.y[seasondata$GameCount.y==2]  + .95*seasondata$PrevWinPct.y[seasondata$GameCount.y==2] 

#replace with last season's if game 3
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==3] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==3] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==3] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==3] 
seasondata$Pace2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$Pace.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevPace.x[seasondata$GameCount.x==3] 
seasondata$WinPct2.x[seasondata$GameCount.x==3]  <- 0.1*seasondata$WinPct.x[seasondata$GameCount.x==3]  + .9*seasondata$PrevWinPct.x[seasondata$GameCount.x==3] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==3] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==3] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==3] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==3] 
seasondata$Pace2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$Pace.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevPace.y[seasondata$GameCount.y==3] 
seasondata$WinPct2.y[seasondata$GameCount.y==3]  <- 0.1*seasondata$WinPct.y[seasondata$GameCount.y==3]  + .9*seasondata$PrevWinPct.y[seasondata$GameCount.y==3] 

#replace with last season's if game 4
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==4] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==4] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==4] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==4] 
seasondata$Pace2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$Pace.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevPace.x[seasondata$GameCount.x==4] 
seasondata$WinPct2.x[seasondata$GameCount.x==4]  <- 0.15*seasondata$WinPct.x[seasondata$GameCount.x==4]  + .85*seasondata$PrevWinPct.x[seasondata$GameCount.x==4] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==4] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==4] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==4] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==4] 
seasondata$Pace2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$Pace.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevPace.y[seasondata$GameCount.y==4] 
seasondata$WinPct2.y[seasondata$GameCount.y==4]  <- 0.15*seasondata$WinPct.y[seasondata$GameCount.y==4]  + .85*seasondata$PrevWinPct.y[seasondata$GameCount.y==4] 

#replace with last season's if game 5
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==5] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==5] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==5] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==5] 
seasondata$Pace2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$Pace.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevPace.x[seasondata$GameCount.x==5] 
seasondata$WinPct2.x[seasondata$GameCount.x==5]  <- 0.2*seasondata$WinPct.x[seasondata$GameCount.x==5]  + .8*seasondata$PrevWinPct.x[seasondata$GameCount.x==5] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==5] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==5] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==5] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==5] 
seasondata$Pace2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$Pace.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevPace.y[seasondata$GameCount.y==5] 
seasondata$WinPct2.y[seasondata$GameCount.y==5]  <- 0.2*seasondata$WinPct.y[seasondata$GameCount.y==5]  + .8*seasondata$PrevWinPct.y[seasondata$GameCount.y==5] 

#replace with last season's if game 6
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==6] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==6] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==6] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==6] 
seasondata$Pace2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$Pace.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevPace.x[seasondata$GameCount.x==6] 
seasondata$WinPct2.x[seasondata$GameCount.x==6]  <- 0.25*seasondata$WinPct.x[seasondata$GameCount.x==6]  + .75*seasondata$PrevWinPct.x[seasondata$GameCount.x==6] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==6] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==6] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==6] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==6] 
seasondata$Pace2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$Pace.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevPace.y[seasondata$GameCount.y==6] 
seasondata$WinPct2.y[seasondata$GameCount.y==6]  <- 0.25*seasondata$WinPct.y[seasondata$GameCount.y==6]  + .75*seasondata$PrevWinPct.y[seasondata$GameCount.y==6] 

#replace with last season's if game 7
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==7] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==7] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==7] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==7] 
seasondata$Pace2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$Pace.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevPace.x[seasondata$GameCount.x==7] 
seasondata$WinPct2.x[seasondata$GameCount.x==7]  <- 0.3*seasondata$WinPct.x[seasondata$GameCount.x==7]  + .7*seasondata$PrevWinPct.x[seasondata$GameCount.x==7] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==7] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==7] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==7] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==7] 
seasondata$Pace2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$Pace.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevPace.y[seasondata$GameCount.y==7] 
seasondata$WinPct2.y[seasondata$GameCount.y==7]  <- 0.3*seasondata$WinPct.y[seasondata$GameCount.y==7]  + .7*seasondata$PrevWinPct.y[seasondata$GameCount.y==7] 

#replace with last season's if game 8
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==8] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==8] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==8] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==8] 
seasondata$Pace2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$Pace.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevPace.x[seasondata$GameCount.x==8] 
seasondata$WinPct2.x[seasondata$GameCount.x==8]  <- 0.35*seasondata$WinPct.x[seasondata$GameCount.x==8]  + .65*seasondata$PrevWinPct.x[seasondata$GameCount.x==8] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==8] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==8] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==8] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==8] 
seasondata$Pace2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$Pace.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevPace.y[seasondata$GameCount.y==8] 
seasondata$WinPct2.y[seasondata$GameCount.y==8]  <- 0.35*seasondata$WinPct.y[seasondata$GameCount.y==8]  + .65*seasondata$PrevWinPct.y[seasondata$GameCount.y==8] 

#replace with last season's if game 9
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==9] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==9] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==9] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==9] 
seasondata$Pace2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$Pace.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevPace.x[seasondata$GameCount.x==9] 
seasondata$WinPct2.x[seasondata$GameCount.x==9]  <- 0.4*seasondata$WinPct.x[seasondata$GameCount.x==9]  + .6*seasondata$PrevWinPct.x[seasondata$GameCount.x==9] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==9] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==9] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==9] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==9] 
seasondata$Pace2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$Pace.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevPace.y[seasondata$GameCount.y==9] 
seasondata$WinPct2.y[seasondata$GameCount.y==9]  <- 0.4*seasondata$WinPct.y[seasondata$GameCount.y==9]  + .6*seasondata$PrevWinPct.y[seasondata$GameCount.y==9] 

#replace with last season's if game 10
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==10] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==10] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==10] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==10] 
seasondata$Pace2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$Pace.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevPace.x[seasondata$GameCount.x==10] 
seasondata$WinPct2.x[seasondata$GameCount.x==10]  <- 0.45*seasondata$WinPct.x[seasondata$GameCount.x==10]  + .55*seasondata$PrevWinPct.x[seasondata$GameCount.x==10] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==10] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==10] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==10] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==10] 
seasondata$Pace2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$Pace.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevPace.y[seasondata$GameCount.y==10] 
seasondata$WinPct2.y[seasondata$GameCount.y==10]  <- 0.45*seasondata$WinPct.y[seasondata$GameCount.y==10]  + .55*seasondata$PrevWinPct.y[seasondata$GameCount.y==10] 

#replace with last season's if game 11
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==11] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==11] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==11] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==11] 
seasondata$Pace2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$Pace.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevPace.x[seasondata$GameCount.x==11] 
seasondata$WinPct2.x[seasondata$GameCount.x==11]  <- 0.5*seasondata$WinPct.x[seasondata$GameCount.x==11]  + .5*seasondata$PrevWinPct.x[seasondata$GameCount.x==11] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==11] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==11] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==11] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==11] 
seasondata$Pace2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$Pace.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevPace.y[seasondata$GameCount.y==11] 
seasondata$WinPct2.y[seasondata$GameCount.y==11]  <- 0.5*seasondata$WinPct.y[seasondata$GameCount.y==11]  + .5*seasondata$PrevWinPct.y[seasondata$GameCount.y==11] 

#replace with last season's if game 12
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==12] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==12] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==12] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==12] 
seasondata$Pace2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$Pace.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevPace.x[seasondata$GameCount.x==12] 
seasondata$WinPct2.x[seasondata$GameCount.x==12]  <- 0.55*seasondata$WinPct.x[seasondata$GameCount.x==12]  + .45*seasondata$PrevWinPct.x[seasondata$GameCount.x==12] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==12] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==12] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==12] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==12] 
seasondata$Pace2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$Pace.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevPace.y[seasondata$GameCount.y==12] 
seasondata$WinPct2.y[seasondata$GameCount.y==12]  <- 0.55*seasondata$WinPct.y[seasondata$GameCount.y==12]  + .45*seasondata$PrevWinPct.y[seasondata$GameCount.y==12] 

#replace with last season's if game 13
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==13] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==13] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==13] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==13] 
seasondata$Pace2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$Pace.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevPace.x[seasondata$GameCount.x==13] 
seasondata$WinPct2.x[seasondata$GameCount.x==13]  <- 0.6*seasondata$WinPct.x[seasondata$GameCount.x==13]  + .4*seasondata$PrevWinPct.x[seasondata$GameCount.x==13] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==13] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==13] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==13] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==13] 
seasondata$Pace2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$Pace.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevPace.y[seasondata$GameCount.y==13] 
seasondata$WinPct2.y[seasondata$GameCount.y==13]  <- 0.6*seasondata$WinPct.y[seasondata$GameCount.y==13]  + .4*seasondata$PrevWinPct.y[seasondata$GameCount.y==13] 

#replace with last season's if game 14
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==14] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==14] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==14] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==14] 
seasondata$Pace2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$Pace.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevPace.x[seasondata$GameCount.x==14] 
seasondata$WinPct2.x[seasondata$GameCount.x==14]  <- 0.65*seasondata$WinPct.x[seasondata$GameCount.x==14]  + .35*seasondata$PrevWinPct.x[seasondata$GameCount.x==14] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==14] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==14] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==14] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==14] 
seasondata$Pace2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$Pace.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevPace.y[seasondata$GameCount.y==14] 
seasondata$WinPct2.y[seasondata$GameCount.y==14]  <- 0.65*seasondata$WinPct.y[seasondata$GameCount.y==14]  + .35*seasondata$PrevWinPct.y[seasondata$GameCount.y==14] 

#replace with last season's if game 15
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==15] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==15] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==15] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==15] 
seasondata$Pace2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$Pace.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevPace.x[seasondata$GameCount.x==15] 
seasondata$WinPct2.x[seasondata$GameCount.x==15]  <- 0.7*seasondata$WinPct.x[seasondata$GameCount.x==15]  + .3*seasondata$PrevWinPct.x[seasondata$GameCount.x==15] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==15] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==15] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==15] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==15] 
seasondata$Pace2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$Pace.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevPace.y[seasondata$GameCount.y==15] 
seasondata$WinPct2.y[seasondata$GameCount.y==15]  <- 0.7*seasondata$WinPct.y[seasondata$GameCount.y==15]  + .3*seasondata$PrevWinPct.y[seasondata$GameCount.y==15] 

#replace with last season's if game 16
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==16] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==16] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==16] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==16] 
seasondata$Pace2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$Pace.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevPace.x[seasondata$GameCount.x==16] 
seasondata$WinPct2.x[seasondata$GameCount.x==16]  <- 0.75*seasondata$WinPct.x[seasondata$GameCount.x==16]  + .25*seasondata$PrevWinPct.x[seasondata$GameCount.x==16] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==16] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==16] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==16] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==16] 
seasondata$Pace2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$Pace.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevPace.y[seasondata$GameCount.y==16] 
seasondata$WinPct2.y[seasondata$GameCount.y==16]  <- 0.75*seasondata$WinPct.y[seasondata$GameCount.y==16]  + .25*seasondata$PrevWinPct.y[seasondata$GameCount.y==16] 

#replace with last season's if game 17
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==17] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==17] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==17] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==17] 
seasondata$Pace2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$Pace.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevPace.x[seasondata$GameCount.x==17] 
seasondata$WinPct2.x[seasondata$GameCount.x==17]  <- 0.8*seasondata$WinPct.x[seasondata$GameCount.x==17]  + .2*seasondata$PrevWinPct.x[seasondata$GameCount.x==17] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==17] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==17] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==17] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==17] 
seasondata$Pace2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$Pace.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevPace.y[seasondata$GameCount.y==17] 
seasondata$WinPct2.y[seasondata$GameCount.y==17]  <- 0.8*seasondata$WinPct.y[seasondata$GameCount.y==17]  + .2*seasondata$PrevWinPct.y[seasondata$GameCount.y==17] 

#replace with last season's if game 18
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==18] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==18] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==18] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==18] 
seasondata$Pace2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$Pace.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevPace.x[seasondata$GameCount.x==18] 
seasondata$WinPct2.x[seasondata$GameCount.x==18]  <- 0.85*seasondata$WinPct.x[seasondata$GameCount.x==18]  + .15*seasondata$PrevWinPct.x[seasondata$GameCount.x==18] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==18] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==18] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==18] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==18] 
seasondata$Pace2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$Pace.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevPace.y[seasondata$GameCount.y==18] 
seasondata$WinPct2.y[seasondata$GameCount.y==18]  <- 0.85*seasondata$WinPct.y[seasondata$GameCount.y==18]  + .15*seasondata$PrevWinPct.y[seasondata$GameCount.y==18] 

#replace with last season's if game 19
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==19] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==19] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==19] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==19] 
seasondata$Pace2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$Pace.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevPace.x[seasondata$GameCount.x==19] 
seasondata$WinPct2.x[seasondata$GameCount.x==19]  <- 0.9*seasondata$WinPct.x[seasondata$GameCount.x==19]  + .1*seasondata$PrevWinPct.x[seasondata$GameCount.x==19] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==19] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==19] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==19] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==19] 
seasondata$Pace2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$Pace.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevPace.y[seasondata$GameCount.y==19] 
seasondata$WinPct2.y[seasondata$GameCount.y==19]  <- 0.9*seasondata$WinPct.y[seasondata$GameCount.y==19]  + .1*seasondata$PrevWinPct.y[seasondata$GameCount.y==19] 

#replace with last season's if game 20
seasondata$PtsPerPossAvg2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$PtsPerPossAvg.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevPtsPerPossAvg.x[seasondata$GameCount.x==20] 
seasondata$DefPtsPerPossAvg2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$DefPtsPerPossAvg.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevDefPtsPerPossAvg.x[seasondata$GameCount.x==20] 
seasondata$OppPtsPerPossAvg2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$OppPtsPerPossAvg.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevOppPtsPerPossAvg.x[seasondata$GameCount.x==20] 
seasondata$OppDefPtsPerPossAvg2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$OppDefPtsPerPossAvg.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevOppDefPtsPerPossAvg.x[seasondata$GameCount.x==20] 
seasondata$Pace2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$Pace.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevPace.x[seasondata$GameCount.x==20] 
seasondata$WinPct2.x[seasondata$GameCount.x==20]  <- 0.95*seasondata$WinPct.x[seasondata$GameCount.x==20]  + .05*seasondata$PrevWinPct.x[seasondata$GameCount.x==20] 
seasondata$PtsPerPossAvg2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$PtsPerPossAvg.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevPtsPerPossAvg.y[seasondata$GameCount.y==20] 
seasondata$DefPtsPerPossAvg2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$DefPtsPerPossAvg.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevDefPtsPerPossAvg.y[seasondata$GameCount.y==20] 
seasondata$OppPtsPerPossAvg2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$OppPtsPerPossAvg.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevOppPtsPerPossAvg.y[seasondata$GameCount.y==20] 
seasondata$OppDefPtsPerPossAvg2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$OppDefPtsPerPossAvg.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevOppDefPtsPerPossAvg.y[seasondata$GameCount.y==20] 
seasondata$Pace2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$Pace.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevPace.y[seasondata$GameCount.y==20] 
seasondata$WinPct2.y[seasondata$GameCount.y==20]  <- 0.95*seasondata$WinPct.y[seasondata$GameCount.y==20]  + .05*seasondata$PrevWinPct.y[seasondata$GameCount.y==20] 

seasondata$AdjNetRating2.x <- (seasondata$PtsPerPossAvg2.x - seasondata$DefPtsPerPossAvg2.x) + (seasondata$OppPtsPerPossAvg2.x - seasondata$OppDefPtsPerPossAvg2.x)
seasondata$NetRating2.x <- (seasondata$PtsPerPossAvg2.x - seasondata$DefPtsPerPossAvg2.x)
seasondata$AdjNetRating2.y <- (seasondata$PtsPerPossAvg2.y - seasondata$DefPtsPerPossAvg2.y) + (seasondata$OppPtsPerPossAvg2.y - seasondata$OppDefPtsPerPossAvg2.y)
seasondata$NetRating2.y <- (seasondata$PtsPerPossAvg2.y - seasondata$DefPtsPerPossAvg2.y)

#fix adjrating in past 20, 10 and 5 for game 1
seasondata$AdjNetRating20.x[seasondata$GameCount.x==1]  <- seasondata$AdjNetRating2.x[seasondata$GameCount.x==1]
seasondata$AdjNetRating10.x[seasondata$GameCount.x==1]  <- seasondata$AdjNetRating2.x[seasondata$GameCount.x==1]
seasondata$AdjNetRating5.x[seasondata$GameCount.x==1]  <- seasondata$AdjNetRating2.x[seasondata$GameCount.x==1]
seasondata$AdjNetRating20.y[seasondata$GameCount.y==1]  <- seasondata$AdjNetRating2.y[seasondata$GameCount.y==1]
seasondata$AdjNetRating10.y[seasondata$GameCount.y==1]  <- seasondata$AdjNetRating2.y[seasondata$GameCount.y==1]
seasondata$AdjNetRating5.y[seasondata$GameCount.y==1]  <- seasondata$AdjNetRating2.y[seasondata$GameCount.y==1]


seasondata$PaceXPace2 <- seasondata$Pace2.x*seasondata$Pace2.y
seasondata$PaceXSq2 <- seasondata$Pace2.x*seasondata$Pace2.x
seasondata$PaceYSq2 <- seasondata$Pace2.y*seasondata$Pace2.y
seasondata$AdjNetMarSqPlus2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^2*((seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)>0)
seasondata$AdjNetMarSqNeg2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^2*((seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)<=0)
seasondata$AdjNetMarCub2 <- (seasondata$AdjNetRating2.x-seasondata$AdjNetRating2.y)^3

seasondata$ShrHomeWeek.x <- ifelse(seasondata$NumGamesWeek.x==0,.5,seasondata$NumHomeGamesWeek.x/seasondata$NumGamesWeek.x)
seasondata$ShrHomeWeek.y <- ifelse(seasondata$NumGamesWeek.y==0,0.5,seasondata$NumHomeGamesWeek.y/seasondata$NumGamesWeek.y)


seasondata$ThrPercentXX.x <- seasondata$ThrPercent.x*seasondata$DefThrPercent.y 
seasondata$ThrPAPerPossXX.x <- seasondata$ThrPAPerPoss.x*seasondata$DefThrPAPerPoss.y
seasondata$ThrPMPerPossXX.x <- seasondata$ThrPMPerPoss.x*seasondata$DefThrPMPerPoss.y
seasondata$TwoPercentXX.x <- seasondata$TwoPercent.x*seasondata$DefTwoPercent.y 
seasondata$TwoPAPerPossXX.x <- seasondata$TwoPAPerPoss.x*seasondata$DefTwoPAPerPoss.y
seasondata$TwoPMPerPossXX.x <- seasondata$TwoPMPerPoss.x*seasondata$DefTwoPMPerPoss.y
seasondata$FTAPerPossXX.x <- seasondata$FTAPerPoss.x*seasondata$DefFTAPerPoss.y
seasondata$DREBRtXX.x <- seasondata$DREBrt.x*seasondata$OREBrt.y
seasondata$DREBTotXX.x <- seasondata$DREBrtXFGMiss.x*seasondata$OREBrtXFGMiss.y
seasondata$ASTPerPossXX.x <- seasondata$ASTPerPoss.x*seasondata$DefASTPerPoss.y 
seasondata$TOPerPossXX.x <- seasondata$TOPerPoss.x*seasondata$DefTOPerPoss.y 

seasondata$ThrPercentXX.y <- seasondata$ThrPercent.y*seasondata$DefThrPercent.x 
seasondata$ThrPAPerPossXX.y <- seasondata$ThrPAPerPoss.y*seasondata$DefThrPAPerPoss.x
seasondata$ThrPMPerPossXX.y <- seasondata$ThrPMPerPoss.y*seasondata$DefThrPMPerPoss.x
seasondata$TwoPercentXX.y <- seasondata$TwoPercent.y*seasondata$DefTwoPercent.x 
seasondata$TwoPAPerPossXX.y <- seasondata$TwoPAPerPoss.y*seasondata$DefTwoPAPerPoss.x
seasondata$TwoPMPerPossXX.y <- seasondata$TwoPMPerPoss.y*seasondata$DefTwoPMPerPoss.x
seasondata$FTAPerPossXX.y <- seasondata$FTAPerPoss.y*seasondata$DefFTAPerPoss.x
seasondata$DREBRtXX.y <- seasondata$DREBrt.y*seasondata$OREBrt.x
seasondata$DREBTotXX.y <- seasondata$DREBrtXFGMiss.y*seasondata$OREBrtXFGMiss.x
seasondata$ASTPerPossXX.y <- seasondata$ASTPerPoss.y*seasondata$DefASTPerPoss.x 
seasondata$TOPerPossXX.y <- seasondata$TOPerPoss.y*seasondata$DefTOPerPoss.x 

# NBAdistance <- read_dta("/Users/ricardovelloso/Dropbox/Data/NBAGambling/NBAdistancewithTEAMS.dta")
# NBAdistance$Description3 <- str_replace(NBAdistance$Description1,"New York Knicks","Brooklyn Nets")
# NBAdistance$Description4 <- str_replace(NBAdistance$Description2,"New York Knicks","Brooklyn Nets")
# NBAdistance$Description5 <- str_replace(NBAdistance$Description1,"LA Clippers","Los Angeles Lakers")
# NBAdistance$Description6 <- str_replace(NBAdistance$Description2,"LA Clippers","Los Angeles Lakers")
# NBAdistance$Description7 <- str_replace(NBAdistance$Description1,"Brooklyn Nets","New York Knicks")
# NBAdistance$Description8 <- str_replace(NBAdistance$Description2,"Brooklyn Nets","New York Knicks")
# NBAdistance$Description9 <- str_replace(NBAdistance$Description1,"Los Angeles Lakers","LA Clippers")
# NBAdistance$Description10 <- str_replace(NBAdistance$Description2,"Los Angeles Lakers","LA Clippers")
# NBAdistance1 <- NBAdistance[,c(1,2)]
# NBAdistance2 <- NBAdistance[,c(1,3)]
# NBAdistance3 <- NBAdistance[,c(1,4)]
# NBAdistance4 <- NBAdistance[,c(1,5)]
# NBAdistance5 <- NBAdistance[,c(1,6)]
# NBAdistance6 <- NBAdistance[,c(1,7)]
# NBAdistance7 <- NBAdistance[,c(1,8)]
# NBAdistance8 <- NBAdistance[,c(1,9)]
# NBAdistance9 <- NBAdistance[,c(1,10)]
# NBAdistance10 <- NBAdistance[,c(1,11)]
# NBAdistance1 <- dplyr::rename(NBAdistance1, GameDescription = Description1)
# NBAdistance2 <- dplyr::rename(NBAdistance2, GameDescription = Description2)
# NBAdistance3 <- dplyr::rename(NBAdistance3, GameDescription = Description3)
# NBAdistance4 <- dplyr::rename(NBAdistance4, GameDescription = Description4)
# NBAdistance5 <- dplyr::rename(NBAdistance5, GameDescription = Description5)
# NBAdistance6 <- dplyr::rename(NBAdistance6, GameDescription = Description6)
# NBAdistance7 <- dplyr::rename(NBAdistance7, GameDescription = Description7)
# NBAdistance8 <- dplyr::rename(NBAdistance8, GameDescription = Description8)
# NBAdistance9 <- dplyr::rename(NBAdistance9, GameDescription = Description9)
# NBAdistance10 <- dplyr::rename(NBAdistance10, GameDescription = Description10)
# NBAdistanceMERGE <- rbind(NBAdistance1,NBAdistance2,NBAdistance3,NBAdistance4,NBAdistance5,NBAdistance6,NBAdistance7,NBAdistance8,NBAdistance9,NBAdistance10)
# NBAdistanceMERGE <- distinct(NBAdistanceMERGE)
# seasondata <- merge(seasondata,NBAdistanceMERGE, by=c("GameDescription"),all.x=TRUE,all.y=FALSE)
# seasondata$mi_to_place[seasondata$GameDescription=="Brooklyn Nets at New York Knicks"] <- 0
# seasondata$mi_to_place[seasondata$GameDescription=="New York Knicks at Brooklyn Nets"] <- 0
# seasondata$mi_to_place[seasondata$GameDescription=="Los Angeles Lakers at LA Clippers"] <- 0
# seasondata$mi_to_place[seasondata$GameDescription=="LA Clippers at Los Angeles Lakers"] <- 0
# seasondata$mi_to_place[seasondata$GameDescription=="Los Angeles Lakers at New York Knicks"] <- 2458.96962
# seasondata$mi_to_place[seasondata$GameDescription=="New York Knicks at Los Angeles Lakers"] <- 2458.96962

seasondata$DistDummy1 <- ifelse(seasondata$mi_to_place==0,1,0)
seasondata$DistDummy2 <- ifelse(seasondata$mi_to_place>0&seasondata$mi_to_place<=200,1,0)
seasondata$DistDummy3 <- ifelse(seasondata$mi_to_place>200&seasondata$mi_to_place<=500,1,0)
seasondata$DistDummy4 <- ifelse(seasondata$mi_to_place>500&seasondata$mi_to_place<=1000,1,0)
seasondata$DistDummy5 <- ifelse(seasondata$mi_to_place>1000&seasondata$mi_to_place<=1500,1,0)
seasondata$DistDummy6 <- ifelse(seasondata$mi_to_place>1500&seasondata$mi_to_place<=2000,1,0)
seasondata$DistDummy7 <- ifelse(seasondata$mi_to_place>2000,1,0)

#One tweak so that game 1s appear in dataset after V4 update
seasondata$AdjNetRating20X20.x <- seasondata$AdjNetRating20.x*seasondata$GameDummyLess20
seasondata$AdjNetRating20X20.y <- seasondata$AdjNetRating20.y*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.x <- seasondata$AdjNetRating10.x*seasondata$GameDummyLess20
seasondata$AdjNetRating10X20.y <- seasondata$AdjNetRating10.y*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.x <- seasondata$AdjNetRating5.x*seasondata$GameDummyLess20
seasondata$AdjNetRating5X20.y <- seasondata$AdjNetRating5.y*seasondata$GameDummyLess20

#TotalPoints
seasondata$TotalPoints <- seasondata$PTS.x + seasondata$PTS.y
seasondata$TotalPointsPerPoss <- seasondata$PtsPerPoss.x + seasondata$PtsPerPoss.y

#remove duplicates
seasondata <- seasondata[order(seasondata$GameID,seasondata$Date,seasondata$PTS.x),]
seasondata <- distinct(seasondata,GameID, Date, GameDescription,.keep_all = TRUE)

save(seasondata, file = paste("data_modelalphaV4.RData", sep=""))








