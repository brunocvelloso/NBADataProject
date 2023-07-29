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
library(parallel)
library(MASS)
library(tidyverse)
library(jsonlite)
library(janitor)
library(extrafont)
library(ggrepel)
library(scales)
#setwd(working)


load("modelbetaV1_dataformerge.RData")
load("allregdata_modelalphaV4.RData")
load("modelgammaV2additions1.RData")
seasondata_add1OLD <- seasondata_add1
load("modelgammaV2additions2.RData")
seasondata_add2OLD <- seasondata_add2
load("modelgammaV2additions3.RData")
seasondata_add3OLD <- seasondata_add3
load("modelgammaV2additions4.RData")
seasondata_add4OLD <- seasondata_add4
load("allregdata_modelbetaV1.RData")
load("modelbetaV1_teamlist.RData")

seasondata$LateSeriesTie <- ifelse(seasondata$RegularSeason==1,0,0)
seasondata$LateSeriesTie[seasondata$GameSeven==1|seasondata$TwoTwo==1] <- 1
seasondata$DownTwo <- 0
seasondata$DownTwo[seasondata$ZeroTwo==1|seasondata$OneThree==1] <- 1
seasondata$NormPlayoffAdv <- 0
seasondata$NormPlayoffAdv[seasondata$GameOne==1|seasondata$ThreeOne==1|seasondata$ZeroTwo==1|seasondata$ThreeTwo] <- 1
seasondata$UpOneEarly <- 0
seasondata$UpOneEarly[seasondata$OneZero==1|seasondata$TwoOne==1] <- 1
seasondata$DownOneEarly <- 0
seasondata$DownOneEarly[seasondata$ZeroOne==1|seasondata$OneTwo==1] <- 1
seasondata$AwayCloseGame6 <- 0
seasondata$AwayCloseGame6[seasondata$TwoThree==1] <- 1
seasondata$HomeTeamSeriesAdv <- ifelse(seasondata$GameOne==1|seasondata$GameTwo==1|seasondata$GameFive==1|seasondata$GameSeven==1,1,0)
seasondata$AwayTeamSeriesAdv <- ifelse(seasondata$GameThree==1|seasondata$GameFour==1|seasondata$GameSix==1,1,0)
seasondata$EarlyHomeGames <- ifelse(seasondata$GameOne==1|seasondata$GameTwo==1,1,0)
seasondata$MidSeriesGames <- ifelse(seasondata$GameThree==1|seasondata$GameFour==1|seasondata$GameFive,1,0)

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
startdate <- today

seasondata$row_num2 <- seq.int(nrow(seasondata))
seasondata$HomeMargNoOut <- ifelse(seasondata$HomeMargin>30,30,ifelse(seasondata$HomeMargin<(-30),-30,seasondata$HomeMargin))

#start <- 1 
start <- length(seasondata_add1$Date[seasondata_add1$Date<"2022-10-10"])
seasondata$Date2 <- seasondata$Date
seasondata$Date[seasondata$Date>today] <- today
myfunc1 <- function(y) {

PointsTotal.x <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))

PointsTotal.y <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))

PointsTotal20.x <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))

PointsTotal20.y <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))

PointsTotal10.x <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))

PointsTotal10.y <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))

PointsTotal5.x <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))

PointsTotal5.y <- with(seasondata,(sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))

Pace20.x <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))

Pace20.y <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))

Pace10.x <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))

Pace10.y <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))

Pace5.x <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))

Pace5.y <- with(seasondata,(sum(Poss[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(Poss[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))

PrevGameMax.x <- with(seasondata,max(max(GameCount.x[(SeasNum==(SeasNum[y]-1) & RegularSeason==1 & Team.x==Team.x[y])]),max(GameCount.y[(SeasNum==(SeasNum[y]-1) & RegularSeason==1 & Team.y==Team.x[y])])))

PrevGameMax.y <- with(seasondata,max(max(GameCount.x[(SeasNum==(SeasNum[y]-1) & RegularSeason==1 & Team.x==Team.y[y])]),max(GameCount.y[(SeasNum==(SeasNum[y]-1) & RegularSeason==1 & Team.y==Team.y[y])])))

DefPointsTotal.x <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))

DefPointsTotal.y <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))

DefPointsTotal20.x <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))

DefPointsTotal20.y <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))

DefPointsTotal10.x <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))

DefPointsTotal10.y <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))

DefPointsTotal5.x <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))

DefPointsTotal5.y <- with(seasondata,(sum(PTS.y[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(PTS.x[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))

CapMargin.x <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y]])))

CapMargin.y <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y]]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y]])))

CapMargin20.x <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-19)])))

CapMargin20.y <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-19)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-19)])))

CapMargin10.x <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-9)])))

CapMargin10.y <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-9)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-9)])))

CapMargin5.x <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.x[y] & GameCount.x>=(GameCount.x[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.x[y] & GameCount.y>=(GameCount.x[y]-4)])))

CapMargin5.y <- with(seasondata,(sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) - sum(HomeMargNoOut[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)]))/(sum(ones[Season==Season[y] & Date < Date[y] & Team.x==Team.y[y] & GameCount.x>=(GameCount.y[y]-4)]) + sum(ones[Season==Season[y] & Date < Date[y] & Team.y==Team.y[y] & GameCount.y>=(GameCount.y[y]-4)])))
GameID <- with(seasondata,GameID[y])
Date <- with(seasondata,Date[y])

value <- data.frame(GameID,Date,PointsTotal.x,PointsTotal.y,PointsTotal20.x,PointsTotal20.y,PointsTotal10.x,PointsTotal10.y,PointsTotal5.x,PointsTotal5.y,Pace20.x,Pace20.y,
  Pace10.x,Pace10.y,Pace5.x,Pace5.y,PrevGameMax.x,PrevGameMax.y,DefPointsTotal.x,DefPointsTotal.y,DefPointsTotal20.x,DefPointsTotal20.y,
  DefPointsTotal10.x,DefPointsTotal10.y,DefPointsTotal5.x,DefPointsTotal5.y,CapMargin.x,CapMargin.y,CapMargin20.x,CapMargin20.y,CapMargin10.x,
  CapMargin10.y,CapMargin5.x,CapMargin5.y)
  
  return(value)

}
tic()
numCores <- detectCores()
seasondata_add1 <- mclapply(c(start:length(seasondata$Date)), myfunc1,mc.cores = numCores)
seasondata_add1 <- dplyr::bind_rows(seasondata_add1, .id = "column_label")
toc()

seasondata_add1OLD <- seasondata_add1OLD[!(seasondata_add1OLD$GameID %in% seasondata_add1$GameID),]
seasondata_add1 <- rbind(seasondata_add1OLD,seasondata_add1)
seasondata_add1 <- distinct(seasondata_add1)
save(seasondata_add1,file="modelgammaV2additions1.RData")
#check
length(seasondata$Date)==length(seasondata_add1$Date)
seasondata <- seasondata %>% left_join(seasondata_add1,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)

myfunc2 <- function(y) {
  
  PrevPtsTot.x <- with(seasondata,ifelse(!is_empty(PointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),PointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],PointsTotal.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevPtsTot.y <- with(seasondata,ifelse(!is_empty(PointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),PointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],PointsTotal.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevPace.x <- with(seasondata,ifelse(!is_empty(Pace.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),Pace.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],Pace.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevPace.y <- with(seasondata,ifelse(!is_empty(Pace.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),Pace.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],Pace.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  PrevDefPtsTot.x <- with(seasondata,ifelse(!is_empty(DefPointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),DefPointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],DefPointsTotal.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  
  PrevDefPtsTot.y <- with(seasondata,ifelse(!is_empty(DefPointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),DefPointsTotal.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],DefPointsTotal.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  
  GameID <- with(seasondata,GameID[y])
  Date <- with(seasondata,Date[y])
  
  value <- data.frame(GameID,Date,PrevPtsTot.x,PrevPtsTot.y,PrevPace.x,PrevPace.y,PrevDefPtsTot.x,PrevDefPtsTot.y)
  
  return(value)
}
tic()
numCores <- detectCores()
seasondata_add2 <- mclapply(c(start:length(seasondata$Date)), myfunc2,mc.cores = numCores)
seasondata_add2 <- dplyr::bind_rows(seasondata_add2, .id = "column_label")
toc()

seasondata_add2OLD <- seasondata_add2OLD[!(seasondata_add2OLD$GameID %in% seasondata_add2$GameID),]
seasondata_add2 <- rbind(seasondata_add2OLD,seasondata_add2)
seasondata_add2 <- distinct(seasondata_add2)
save(seasondata_add2,file="modelgammaV2additions2.RData")
#check
length(seasondata$Date)==length(seasondata_add2$Date)
seasondata <- seasondata %>% left_join(seasondata_add2,by=c("GameID","Date"))

teamlistMERGE <- teamlistMERGE %>% left_join(seasondata[,c("PTS.x","Team.x","Date")],by = c("Team.x","Date")) %>% ungroup() 
teamlistMERGE <- teamlistMERGE %>% left_join(seasondata[,c("PTS.y","Team.y","Date")],by = c("Team.y","Date")) %>% ungroup() 
teamlistMERGE$OffPts <- ifelse(is.na(teamlistMERGE$PTS.x),teamlistMERGE$PTS.y,teamlistMERGE$PTS.x)
teamlistMERGE <- teamlistMERGE %>% left_join(seasondata[,c("PTS.y","Team.x","Date")],by = c("Team.x","Date")) %>% ungroup() 
teamlistMERGE <- teamlistMERGE %>% left_join(seasondata[,c("PTS.x","Team.y","Date")],by = c("Team.y","Date")) %>% ungroup() 
teamlistMERGE$DefPts <- ifelse(is.na(teamlistMERGE$PTS.x.y),teamlistMERGE$PTS.y.y,teamlistMERGE$PTS.x.y)
teamlistMERGE$OffPts[is.na(teamlistMERGE$OffPts)] <- 0
teamlistMERGE$DefPts[is.na(teamlistMERGE$DefPts)] <- 0
teamlistMERGE <- teamlistMERGE %>%
  dplyr::group_by(Team,Season) %>%
  dplyr::mutate(PointsTotal = (cumsum(OffPts))/(cumsum(OffPts>0)),
                DefPointsTotal = (cumsum(DefPts))/(cumsum(DefPts>0))) %>%
  ungroup()
teamlistMERGE <- teamlistMERGE %>%
    dplyr::group_by(Team,Season) %>%
    dplyr::mutate(PointsTotal = sapply(1:n(), function(x) ifelse(x-1>0,PointsTotal[x-1],NA)),
                  DefPointsTotal = sapply(1:n(), function(x) ifelse(x-1>0,DefPointsTotal[x-1],NA))) %>%
    ungroup()

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
  date.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist.x))
  date.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist.y))
  date20.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist20.x))
  date20.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist20.y))
  date10.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist10.x))
  date10.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist10.y))
  date5.x <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist5.x))
  date5.y <- rep_len(as.Date(ifelse(seasondata$Date[i]>today,today,seasondata$Date[i])),length(opplist5.y))
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
  df.x <- df.x %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df.y <- df.y %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppTotalPoints.x <- mean(df.x$PointsTotal,na.rm = TRUE)
  OppTotalPoints.y <- mean(df.y$PointsTotal,na.rm = TRUE)
  OppDefTotalPoints.x <- mean(df.x$DefPointsTotal,na.rm = TRUE)
  OppDefTotalPoints.y <- mean(df.y$DefPointsTotal,na.rm = TRUE)
  df20.x <- dplyr::rename(df20.x,Date=date20.x,Team=opplist20.x)
  df20.y <- dplyr::rename(df20.y,Date=date20.y,Team=opplist20.y)
  df20.x <- df20.x %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df20.y <- df20.y %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppTotalPoints20.x <- mean(df20.x$PointsTotal,na.rm = TRUE)
  OppTotalPoints20.y <- mean(df20.y$PointsTotal,na.rm = TRUE)
  OppDefTotalPoints20.x <- mean(df20.x$DefPointsTotal,na.rm = TRUE)
  OppDefTotalPoints20.y <- mean(df20.y$DefPointsTotal,na.rm = TRUE)
  df10.x <- dplyr::rename(df10.x,Date=date10.x,Team=opplist10.x)
  df10.y <- dplyr::rename(df10.y,Date=date10.y,Team=opplist10.y)
  df10.x <- df10.x %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df10.y <- df10.y %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppTotalPoints10.x <- mean(df10.x$PointsTotal,na.rm = TRUE)
  OppTotalPoints10.y <- mean(df10.y$PointsTotal,na.rm = TRUE)
  OppDefTotalPoints10.x <- mean(df10.x$DefPointsTotal,na.rm = TRUE)
  OppDefTotalPoints10.y <- mean(df10.y$DefPointsTotal,na.rm = TRUE)
  df5.x <- dplyr::rename(df5.x,Date=date5.x,Team=opplist5.x)
  df5.y <- dplyr::rename(df5.y,Date=date5.y,Team=opplist5.y)
  df5.x <- df5.x %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  df5.y <- df5.y %>% left_join(teamlistMERGE[,c("PointsTotal","DefPointsTotal","Team","Date")],by = c("Team","Date")) %>% ungroup() 
  OppTotalPoints5.x <- mean(df5.x$PointsTotal,na.rm = TRUE)
  OppTotalPoints5.y <- mean(df5.y$PointsTotal,na.rm = TRUE)
  OppDefTotalPoints5.x <- mean(df5.x$DefPointsTotal,na.rm = TRUE)
  OppDefTotalPoints5.y <- mean(df5.y$DefPointsTotal,na.rm = TRUE)
 
  GameID <- with(seasondata,GameID[i])
  Date <- with(seasondata,Date[i])
  
  value <- data.frame(GameID,Date,OppTotalPoints.x,
  OppTotalPoints.y,
  OppDefTotalPoints.x,
  OppDefTotalPoints.y,
  OppTotalPoints20.x,
  OppTotalPoints20.y,
  OppDefTotalPoints20.x,
  OppDefTotalPoints20.y,
  OppTotalPoints10.x,
  OppTotalPoints10.y,
  OppDefTotalPoints10.x,
  OppDefTotalPoints10.y,
  OppTotalPoints5.x,
  OppTotalPoints5.y,
  OppDefTotalPoints5.x,
  OppDefTotalPoints5.y)
  
  return(value)
  
  
  
}
tic()
numCores <- detectCores()
seasondata_add3 <- mclapply(c(start:length(seasondata$Date)), myfunc3,mc.cores = numCores)
seasondata_add3 <- dplyr::bind_rows(seasondata_add3, .id = "column_label")
toc()

seasondata_add3OLD <- seasondata_add3OLD[!(seasondata_add3OLD$GameID %in% seasondata_add3$GameID),]
seasondata_add3 <- rbind(seasondata_add3OLD,seasondata_add3)
seasondata_add3 <- distinct(seasondata_add3)
save(seasondata_add3,file="modelgammaV2additions3.RData")
#check
length(seasondata$Date)==length(seasondata_add3$Date)
seasondata <- seasondata %>% left_join(seasondata_add3,by=c("GameID","Date"))

seasondata$Date <- seasondata$Date2
seasondata <- dplyr::select(seasondata,-Date2)


myfunc4 <- function(y) {
  
  PrevOppPtsTot.x <- with(seasondata,ifelse(!is_empty(OppTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),OppTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],OppTotalPoints.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  PrevOppPtsTot.y <- with(seasondata,ifelse(!is_empty(OppTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),OppTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],OppTotalPoints.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  PrevOppDefPtsTot.x <- with(seasondata,ifelse(!is_empty(OppDefTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])]),OppDefTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.x[y] & Team.x==Team.x[y])],OppDefTotalPoints.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.x[y] & Team.y==Team.x[y])]))
  PrevOppDefPtsTot.y <- with(seasondata,ifelse(!is_empty(OppDefTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])]),OppDefTotalPoints.x[(SeasNum==(SeasNum[y]-1) & GameCount.x==PrevGameMax.y[y] & Team.x==Team.y[y])],OppDefTotalPoints.y[(SeasNum==(SeasNum[y]-1) & GameCount.y==PrevGameMax.y[y] & Team.y==Team.y[y])]))
  GameID <- with(seasondata,GameID[y])
  Date <- with(seasondata,Date[y])
  value <- data.frame(GameID,Date,PrevOppPtsTot.x,PrevOppPtsTot.y,PrevOppDefPtsTot.x,PrevOppDefPtsTot.y)
  
  return(value)
}
tic()
numCores <- detectCores()
seasondata_add4 <- mclapply(c(start:length(seasondata$Date)), myfunc4,mc.cores = numCores)
seasondata_add4 <- dplyr::bind_rows(seasondata_add4, .id = "column_label")
toc()

seasondata_add4OLD <- seasondata_add4OLD[!(seasondata_add4OLD$GameID %in% seasondata_add4$GameID),]
seasondata_add4 <- rbind(seasondata_add4OLD,seasondata_add4)
seasondata_add4 <- distinct(seasondata_add4)
save(seasondata_add4,file="modelgammaV2additions4.RData")
#check
length(seasondata$Date)==length(seasondata_add4$Date)
seasondata <- seasondata %>% left_join(seasondata_add4,by=c("GameID","Date"))


#fill in first game so it is not NA, then make it a blended weight of the previous
seasondata$SeasGamesPREVWGT.x <- ifelse(seasondata$GameCount.x<=20,(21-seasondata$GameCount.x)/20,0)
seasondata$SeasGamesCURRWGT.x <- ifelse(seasondata$GameCount.x<=20,(seasondata$GameCount.x-1)/20,1)
seasondata$SeasGamesPREVWGT.y <- ifelse(seasondata$GameCount.y<=20,(21-seasondata$GameCount.y)/20,0)
seasondata$SeasGamesCURRWGT.y <- ifelse(seasondata$GameCount.y<=20,(seasondata$GameCount.y-1)/20,1)
seasondata$SeasGamesPREVWGT10.x <- ifelse(seasondata$GameCount.x<=10,(11-seasondata$GameCount.x)/10,0)
seasondata$SeasGamesCURRWGT10.x <- ifelse(seasondata$GameCount.x<=10,(seasondata$GameCount.x-1)/10,1)
seasondata$SeasGamesPREVWGT10.y <- ifelse(seasondata$GameCount.y<=10,(11-seasondata$GameCount.y)/10,0)
seasondata$SeasGamesCURRWGT10.y <- ifelse(seasondata$GameCount.y<=10,(seasondata$GameCount.y-1)/10,1)
seasondata$SeasGamesPREVWGT5.x <- ifelse(seasondata$GameCount.x<=5,(6-seasondata$GameCount.x)/5,0)
seasondata$SeasGamesCURRWGT5.x <- ifelse(seasondata$GameCount.x<=5,(seasondata$GameCount.x-1)/5,1)
seasondata$SeasGamesPREVWGT5.y <- ifelse(seasondata$GameCount.y<=5,(6-seasondata$GameCount.y)/5,0)
seasondata$SeasGamesCURRWGT5.y <- ifelse(seasondata$GameCount.y<=5,(seasondata$GameCount.y-1)/5,1)

seasondata <- seasondata %>%
  dplyr::mutate(NetMargin.x = PointsTotal.x - DefPointsTotal.x,
         OppNetMargin.x = OppTotalPoints.x -OppDefTotalPoints.x,
         NetAdjMarg.x = NetMargin.x + OppNetMargin.x,
         NetMargin.y = PointsTotal.y - DefPointsTotal.y,
         OppNetMargin.y = OppTotalPoints.y -OppDefTotalPoints.y,
         NetAdjMarg.y = NetMargin.y + OppNetMargin.y,
         PrevNetMargin.x = PrevPtsTot.x-PrevDefPtsTot.x,
         PrevNetMargin.y = PrevPtsTot.y-PrevDefPtsTot.y,
         PrevOppNetMargin.x = PrevOppPtsTot.x-PrevOppDefPtsTot.x,
         PrevOppNetMargin.y = PrevOppPtsTot.y-PrevOppDefPtsTot.y,
         PrevAdjNetMargin.x = PrevNetMargin.x+PrevOppNetMargin.x,
         PrevAdjNetMargin.y = PrevNetMargin.y+PrevOppNetMargin.y,
         OppNetMargin20.x = OppTotalPoints20.x -OppDefTotalPoints20.x,
         OppNetMargin10.x = OppTotalPoints10.x -OppDefTotalPoints10.x,
         OppNetMargin5.x = OppTotalPoints5.x -OppDefTotalPoints5.x,
         NetAdjMarg20.x = CapMargin20.x+OppNetMargin20.x,
         NetAdjMarg10.x = CapMargin10.x+OppNetMargin10.x,
         NetAdjMarg5.x = CapMargin5.x+OppNetMargin5.x,
         OppNetMargin20.y = OppTotalPoints20.y -OppDefTotalPoints20.y,
         OppNetMargin10.y = OppTotalPoints10.y -OppDefTotalPoints10.y,
         OppNetMargin5.y = OppTotalPoints5.y -OppDefTotalPoints5.y,
         NetAdjMarg20.y = CapMargin20.y+OppNetMargin20.y,
         NetAdjMarg10.y = CapMargin10.y+OppNetMargin10.y,
         NetAdjMarg5.y = CapMargin5.y+OppNetMargin5.y,
  )

seasondata$NetAdjMarg2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMargin.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg.x
seasondata$NetAdjMarg2.x[seasondata$GameCount.x==1] <- seasondata$PrevAdjNetMargin.x[seasondata$GameCount.x==1]
seasondata$NetAdjMarg2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMargin.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg.y
seasondata$NetAdjMarg2.y[seasondata$GameCount.y==1] <- seasondata$PrevAdjNetMargin.y[seasondata$GameCount.y==1]
seasondata$AdjNetMarSqPlusv2 <- (seasondata$NetAdjMarg2.x-seasondata$NetAdjMarg2.y)^2*((seasondata$NetAdjMarg2.x-seasondata$NetAdjMarg2.y)>0)
seasondata$AdjNetMarSqNegv2 <- (seasondata$NetAdjMarg2.x-seasondata$NetAdjMarg2.y)^2*((seasondata$NetAdjMarg2.x-seasondata$NetAdjMarg2.y)<=0)
seasondata$AdjNetMarCubv2 <- (seasondata$NetAdjMarg2.x-seasondata$NetAdjMarg2.y)^3

seasondata$NetAdjMarg220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevAdjNetMargin.x+seasondata$SeasGamesCURRWGT.x*seasondata$NetAdjMarg20.x
seasondata$NetAdjMarg220.x[seasondata$GameCount.x==1] <- 0
seasondata$NetAdjMarg220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevAdjNetMargin.y+seasondata$SeasGamesCURRWGT.y*seasondata$NetAdjMarg20.y
seasondata$NetAdjMarg220.y[seasondata$GameCount.y==1] <- 0
seasondata$NetAdjMarg210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevAdjNetMargin.x+seasondata$SeasGamesCURRWGT10.x*seasondata$NetAdjMarg10.x
seasondata$NetAdjMarg210.x[seasondata$GameCount.x==1] <- 0
seasondata$NetAdjMarg210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevAdjNetMargin.y+seasondata$SeasGamesCURRWGT10.y*seasondata$NetAdjMarg10.y
seasondata$NetAdjMarg210.y[seasondata$GameCount.y==1] <- 0
seasondata$NetAdjMarg25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevAdjNetMargin.x+seasondata$SeasGamesCURRWGT5.x*seasondata$NetAdjMarg5.x
seasondata$NetAdjMarg25.x[seasondata$GameCount.x==1] <- 0
seasondata$NetAdjMarg25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevAdjNetMargin.y+seasondata$SeasGamesCURRWGT5.y*seasondata$NetAdjMarg5.y
seasondata$NetAdjMarg25.y[seasondata$GameCount.y==1] <- 0

seasondata$PointsTotal2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal.x
seasondata$PointsTotal2.x[seasondata$GameCount.x==1] <- seasondata$PrevPtsTot.x[seasondata$GameCount.x==1]
seasondata$PointsTotal2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal.y
seasondata$PointsTotal2.y[seasondata$GameCount.y==1] <- seasondata$PrevPtsTot.y[seasondata$GameCount.y==1]
seasondata$PointsTotal220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$PointsTotal20.x
seasondata$PointsTotal220.x[seasondata$GameCount.x==1] <- seasondata$PrevPtsTot.x[seasondata$GameCount.x==1]
seasondata$PointsTotal220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$PointsTotal20.y
seasondata$PointsTotal220.y[seasondata$GameCount.y==1] <- seasondata$PrevPtsTot.y[seasondata$GameCount.y==1]
seasondata$PointsTotal210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$PointsTotal10.x
seasondata$PointsTotal210.x[seasondata$GameCount.x==1] <- seasondata$PrevPtsTot.x[seasondata$GameCount.x==1]
seasondata$PointsTotal210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$PointsTotal10.y
seasondata$PointsTotal210.y[seasondata$GameCount.y==1] <- seasondata$PrevPtsTot.y[seasondata$GameCount.y==1]
seasondata$PointsTotal25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$PointsTotal5.x
seasondata$PointsTotal25.x[seasondata$GameCount.x==1] <- seasondata$PrevPtsTot.x[seasondata$GameCount.x==1]
seasondata$PointsTotal25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$PointsTotal5.y
seasondata$PointsTotal25.y[seasondata$GameCount.y==1] <- seasondata$PrevPtsTot.y[seasondata$GameCount.y==1]

seasondata$DefPointsTotal2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal.x
seasondata$DefPointsTotal2.x[seasondata$GameCount.x==1] <- seasondata$PrevDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal.y
seasondata$DefPointsTotal2.y[seasondata$GameCount.y==1] <- seasondata$PrevDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$DefPointsTotal20.x
seasondata$DefPointsTotal220.x[seasondata$GameCount.x==1] <- seasondata$PrevDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$DefPointsTotal20.y
seasondata$DefPointsTotal220.y[seasondata$GameCount.y==1] <- seasondata$PrevDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$DefPointsTotal10.x
seasondata$DefPointsTotal210.x[seasondata$GameCount.x==1] <- seasondata$PrevDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$DefPointsTotal10.y
seasondata$DefPointsTotal210.y[seasondata$GameCount.y==1] <- seasondata$PrevDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$DefPointsTotal25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$DefPointsTotal5.x
seasondata$DefPointsTotal25.x[seasondata$GameCount.x==1] <- seasondata$PrevDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$DefPointsTotal25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$DefPointsTotal5.y
seasondata$DefPointsTotal25.y[seasondata$GameCount.y==1] <- seasondata$PrevDefPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppTotalPoints2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints.x
seasondata$OppTotalPoints2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints.y
seasondata$OppTotalPoints2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppTotalPoints20.x
seasondata$OppTotalPoints220.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppTotalPoints20.y
seasondata$OppTotalPoints220.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppTotalPoints10.x
seasondata$OppTotalPoints210.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppTotalPoints10.y
seasondata$OppTotalPoints210.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppTotalPoints25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppTotalPoints5.x
seasondata$OppTotalPoints25.x[seasondata$GameCount.x==1] <- seasondata$PrevOppPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppTotalPoints25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppTotalPoints5.y
seasondata$OppTotalPoints25.y[seasondata$GameCount.y==1] <- seasondata$PrevOppPtsTot.y[seasondata$GameCount.y==1]

seasondata$OppDefTotalPoints2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints.x
seasondata$OppDefTotalPoints2.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints.y
seasondata$OppDefTotalPoints2.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT.x*seasondata$OppDefTotalPoints20.x
seasondata$OppDefTotalPoints220.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT.y*seasondata$OppDefTotalPoints20.y
seasondata$OppDefTotalPoints220.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT10.x*seasondata$OppDefTotalPoints10.x
seasondata$OppDefTotalPoints210.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT10.y*seasondata$OppDefTotalPoints10.y
seasondata$OppDefTotalPoints210.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata$OppDefTotalPoints25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevOppDefPtsTot.x+seasondata$SeasGamesCURRWGT5.x*seasondata$OppDefTotalPoints5.x
seasondata$OppDefTotalPoints25.x[seasondata$GameCount.x==1] <- seasondata$PrevOppDefPtsTot.x[seasondata$GameCount.x==1]
seasondata$OppDefTotalPoints25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevOppDefPtsTot.y+seasondata$SeasGamesCURRWGT5.y*seasondata$OppDefTotalPoints5.y
seasondata$OppDefTotalPoints25.y[seasondata$GameCount.y==1] <- seasondata$PrevOppDefPtsTot.y[seasondata$GameCount.y==1]
seasondata <- dplyr::rename(seasondata,PrevPace.x=PrevPace.x.y,PrevPace.y=PrevPace.y.y)
seasondata$Pace2.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevPace.x+seasondata$SeasGamesCURRWGT.x*seasondata$Pace.x
seasondata$Pace2.x[seasondata$GameCount.x==1] <- seasondata$PrevPace.x[seasondata$GameCount.x==1]
seasondata$Pace2.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevPace.y+seasondata$SeasGamesCURRWGT.y*seasondata$Pace.y
seasondata$Pace2.y[seasondata$GameCount.y==1] <- seasondata$PrevPace.y[seasondata$GameCount.y==1]
seasondata$Pace220.x <- seasondata$SeasGamesPREVWGT.x*seasondata$PrevPace.x+seasondata$SeasGamesCURRWGT.x*seasondata$Pace20.x
seasondata$Pace220.x[seasondata$GameCount.x==1] <- seasondata$PrevPace.x[seasondata$GameCount.x==1]
seasondata$Pace220.y <- seasondata$SeasGamesPREVWGT.y*seasondata$PrevPace.y+seasondata$SeasGamesCURRWGT.y*seasondata$Pace20.y
seasondata$Pace220.y[seasondata$GameCount.y==1] <- seasondata$PrevPace.y[seasondata$GameCount.y==1]
seasondata$Pace210.x <- seasondata$SeasGamesPREVWGT10.x*seasondata$PrevPace.x+seasondata$SeasGamesCURRWGT10.x*seasondata$Pace10.x
seasondata$Pace210.x[seasondata$GameCount.x==1] <- seasondata$PrevPace.x[seasondata$GameCount.x==1]
seasondata$Pace210.y <- seasondata$SeasGamesPREVWGT10.y*seasondata$PrevPace.y+seasondata$SeasGamesCURRWGT10.y*seasondata$Pace10.y
seasondata$Pace210.y[seasondata$GameCount.y==1] <- seasondata$PrevPace.y[seasondata$GameCount.y==1]
seasondata$Pace25.x <- seasondata$SeasGamesPREVWGT5.x*seasondata$PrevPace.x+seasondata$SeasGamesCURRWGT5.x*seasondata$Pace5.x
seasondata$Pace25.x[seasondata$GameCount.x==1] <- seasondata$PrevPace.x[seasondata$GameCount.x==1]
seasondata$Pace25.y <- seasondata$SeasGamesPREVWGT5.y*seasondata$PrevPace.y+seasondata$SeasGamesCURRWGT5.y*seasondata$Pace5.y
seasondata$Pace25.y[seasondata$GameCount.y==1] <- seasondata$PrevPace.y[seasondata$GameCount.y==1]

seasondata$LateSeriesTie <- ifelse(seasondata$RegularSeason==1,0,0)
seasondata$LateSeriesTie[seasondata$GameSeven==1|seasondata$TwoTwo==1] <- 1
seasondata$DownTwo <- 0
seasondata$DownTwo[seasondata$ZeroTwo==1|seasondata$OneThree==1] <- 1
seasondata$NormPlayoffAdv <- 0
seasondata$NormPlayoffAdv[seasondata$GameOne==1|seasondata$ThreeOne==1|seasondata$ZeroTwo==1|seasondata$ThreeTwo] <- 1
seasondata$UpOneEarly <- 0
seasondata$UpOneEarly[seasondata$OneZero==1|seasondata$TwoOne==1] <- 1
seasondata$DownOneEarly <- 0
seasondata$DownOneEarly[seasondata$ZeroOne==1|seasondata$OneTwo==1] <- 1
seasondata$AwayCloseGame6 <- 0
seasondata$AwayCloseGame6[seasondata$TwoThree==1] <- 1

seasondata$HomeTeamSeriesAdv <- ifelse(seasondata$GameOne==1|seasondata$GameTwo==1|seasondata$GameFive==1|seasondata$GameSeven==1,1,0)
seasondata$AwayTeamSeriesAdv <- ifelse(seasondata$GameThree==1|seasondata$GameFour==1|seasondata$GameSix==1,1,0)
seasondata$EarlyHomeGames <- ifelse(seasondata$GameOne==1|seasondata$GameTwo==1,1,0)
seasondata$MidSeriesGames <- ifelse(seasondata$GameThree==1|seasondata$GameFour==1|seasondata$GameFive,1,0)


seasondata <- dplyr::rename(seasondata,`3PM.x`=`X3PM.x`)
seasondata <- dplyr::rename(seasondata,`3PA.x`=`X3PA.x`,`3PA.x`=`X3PA.x`,`2PA.x`=`X2PA.x`,`2PM.x`=`X2PM.x`,`FG%.x`=`FG..x`)
seasondata <- dplyr::rename(seasondata,`FT%.x`=`FT..x`,`2P%.x`=`X2P..x`,`3P%.x`=`X3P..x`)
seasondata <- dplyr::rename(seasondata,`3PM.y`=`X3PM.y`)
seasondata <- dplyr::rename(seasondata,`3PA.y`=`X3PA.y`,`3PA.y`=`X3PA.y`,`2PA.y`=`X2PA.y`,`2PM.y`=`X2PM.y`,`FG%.y`=`FG..y`)
seasondata <- dplyr::rename(seasondata,`FT%.y`=`FT..y`,`2P%.y`=`X2P..y`,`3P%.y`=`X3P..y`)
seasondata[sapply(seasondata, is.infinite)] <- NA
seasondata[sapply(seasondata, is.nan)] <- NA

seasondata <-   dplyr::select(seasondata,-row_num2)

save(seasondata, file = paste("allregdata_modelgammaV2.RData", sep=""))
#remove duplicates
# seasondata <- seasondata[order(seasondata$GameID,seasondata$Date,seasondata$PTS.x),]
# seasondata <- distinct(seasondata,GameID, Date, GameDescription,.keep_all = TRUE)
