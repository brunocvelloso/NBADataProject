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
library("haven")
#setwd(working)

#first load data and calculate regression
load("data_modelalphaV4.RData")
save(seasondata, file = paste("data_modelalphaV4Orig.RData", sep="")) #precaution in case a mistake is made

#Model Alpha v4. Just takes team-level data (i.e. no data on player quality).
#think of it as a "base" model
#1a tries to predict home team margine of victory (negative if loss)
#1b tries to predict the number of possessions in the game
#1c tries to predict the total combined points scored by both teams (per possession)
#1d predicts the probability that the home team wins (logistic regression)
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
modelalpha4c <- lm(formula = TotalPointsPerPoss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) + GameDummy5 + GameDummy10 + GameDummy15 + GameDummy20 + GameDummy25 + GameDummy30 + GameDummy35 + GameDummy40 + GameDummy45 + GameDummy50 + GameDummy55 + GameDummy60 + GameDummy65 + GameDummy70 + GameDummy82 + GameDummy5XPrevSeas + GameDummy10XPrevSeas + GameDummy15XPrevSeas + GameDummy20XPrevSeas + GameDummy25XPrevSeas + GameDummy30XPrevSeas + GameDummy35XPrevSeas + GameDummy40XPrevSeas + GameDummy45XPrevSeas + GameDummy50XPrevSeas + GameDummy55XPrevSeas + GameDummy60XPrevSeas + GameDummy65XPrevSeas + GameDummy70XPrevSeas + GameDummy82XPrevSeas + GameDummy5XPrevSeasY + GameDummy10XPrevSeasY + GameDummy15XPrevSeasY + GameDummy20XPrevSeasY + GameDummy25XPrevSeasY + GameDummy30XPrevSeasY + GameDummy35XPrevSeasY + GameDummy40XPrevSeasY + GameDummy45XPrevSeasY + GameDummy50XPrevSeasY + GameDummy55XPrevSeasY + GameDummy60XPrevSeasY + GameDummy65XPrevSeasY + GameDummy70XPrevSeasY + GameDummy82XPrevSeasY  + PaceXSq2 + PaceYSq2 + PaceXPace2 +
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


##### Now we want to predict "future" games, so this part scrapes ESPN's website for future games
#from this we can come up with a prediction based on our regression models above
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
startdate <- today
enddate <- today+7

#define some date variables
year_start <- year(startdate)
year_end <- year(enddate)
month_start <- month(startdate)
month_end <- month(enddate)
day_start <- day(startdate)
day_end <- day(enddate)

#create empty vectors that will later create data table
gameID <- c()
boxlinks <- c()
pbyplinks<- c()
yearvec<- c()
monthvec<- c()
dayvec<- c()


#loop from startdate to enddate, one day at a time
for(year in year_start:year_end)
{
  for(month in 1:12)
  {
    if (month<10) {
      mo_str <- paste("0",month,sep = "")
    }
    if (month>=10) {
      mo_str <- paste(month,sep = "")
    }
    for(day in 1:31)
    {
      if (day<10) {
        day_str <- paste("0",day,sep = "")
      }
      if (day>=10) {
        day_str <- paste(day,sep = "")
      }
      url_date <- paste ("https://www.espn.com/nba/scoreboard/_/date/",year,mo_str,day_str, sep = "", collapse = NULL)
      url_date
      #skip if before date range
      if ((year==year_start&month<month_start)|(year==year_start&month==month_start&day<day_start)) {
        next
      }
      #skip if after date range
      if ((year==year_end&month>month_end)|(year==year_end&month==month_end&day>day_end)) {
        next
      }
      
      # remDr$navigate(url_date)
      # #  withTimeout({remDr$navigate(url_date); cat(month, day_str, "try: 0", "\n")},
      # #             timeout = 1,onTimeout="silent")
      # # # 
      # 
      # counter <- 0
      # while (remDr$getTitle()[[1]][1]!="NBA Basketball Scores - NBA Scoreboard - ESPN")
      # {
      #   counter <- counter+1
      #   remDr$navigate(url_date)
      #   # withTimeout({remDr$refresh(); cat(month, day_str, "try: ", counter, "\n")},
      #   #                 timeout = 10, onTimeout = "silent")
      #   
      # }
      # elem <- remDr$findElements(using = "name", value = "&lpos=nba:scoreboard:gamecast")
      
      webpage <- read_html(url_date)
      elem <- html_nodes(webpage,paste0('a')) %>% html_attr("href")
      elem <- elem[which(regexpr('/game/_/gameId/', elem) >= 1)]
      elem <- unique(elem)
      elem <- paste0("https://www.espn.com",elem,sep="")
      
      elembox <- c(1:length(elem))
      elempbyp <- c(1:length(elem))
      elemgameID <- c(1:length(elem))
      yeartemp <- c(1:length(elem))
      monthtemp <- c(1:length(elem))
      daytemp <- c(1:length(elem))
      if (length(elem)>0) {
        for (i in 1:length(elem)){
          #elem[[i]]$highlightElement() 
          elembox[i] <- elem[i]
          elempbyp[i] <- elembox[i]
          elemgameID[i] <- substr(elembox[i],nchar(elembox[i])-8,nchar(elembox[i]))
          start <- str_locate_all(pattern ='/game/',elembox[i])[[1]][1]
          end <- str_locate_all(pattern ='/game/',elembox[i])[[1]][2]
          str_sub(elempbyp[i],start,end) <- "/playbyplay/"
          str_sub(elembox[i],start,end) <- "/boxscore/"
          yeartemp[i] <- year
          monthtemp[i] <- month
          daytemp[i] <- day
        }
      }
      boxlinks <- c(boxlinks,elembox)
      pbyplinks <- c(pbyplinks,elempbyp)
      gameID <- c(gameID,elemgameID)
      yearvec <- c(yearvec,yeartemp)
      monthvec <- c(monthvec,monthtemp)
      dayvec <- c(dayvec,daytemp)
      
      #create dataframe
      Year <- c(yearvec)
      Month <- c(monthvec)
      Day <- c(dayvec)
      BoxScore <- c(boxlinks)
      PlaybyPlay <- c(pbyplinks)
      GameID <- c(gameID)
      
      NBA_ESPNgamelinksFORPRED <- data.frame(Year, Month, Day, BoxScore, PlaybyPlay, GameID)
    }
  }
}      
     
NBA_ESPNgamelinksFORPRED <- NBA_ESPNgamelinksFORPRED[NBA_ESPNgamelinksFORPRED$GameID!=".espn.com",]   
save(NBA_ESPNgamelinksFORPRED, file = paste("NBA_ESPNgamelinksFORPRED.RData", sep=""))  

NBA_ESPNgamelinks <- NBA_ESPNgamelinksFORPRED
NBA_ESPNgamelinks$GameID <- as.character(NBA_ESPNgamelinks$GameID)
GameID <- c()
Team.x <- c()
Team.y <- c()
Date <- c(today)
HomePts <- c()
AwayPts <- c()
HomeWin <- c()
HomeMargin <- c()
MIN.x <- c()
OREB.x <- c()
DREB.x <- c()
REB.x <- c()
AST.x <- c()
STL.x <- c()
BLK.x <- c()
TO.x <- c()
PF.x <- c()
PTS.x <- c()
FGM.x <- c()
FGA.x <- c()
`3PM.x` <- c()
`3PA.x` <- c()
FTM.x <- c()
FTA.x <- c()
`2PM.x` <- c()
`2PA.x` <- c()
`FG%.x` <- c()
`FT%.x` <- c()
`3P%.x` <- c()
`2P%.x` <- c()
GameDescription <- c()
RegularSeason <- c()
Playoffs <- c()
Finals <- c()
Season <- c()
MIN.y <- c()
OREB.y <- c()
DREB.y <- c()
REB.y <- c()
AST.y <- c()
STL.y <- c()
BLK.y <- c()
TO.y <- c()
PF.y <- c()
PTS.y <- c()
FGM.y <- c()
FGA.y <- c()
`3PM.y` <- c()
`3PA.y` <- c()
FTM.y <- c()
FTA.y <- c()
`2PM.y` <- c()
`2PA.y` <- c()
`FG%.y` <- c()
`FT%.y` <- c()
`3P%.y` <- c()
`2P%.y` <- c()

NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==11&NBA_ESPNgamelinks$Day==31),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==9&NBA_ESPNgamelinks$Day==31),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==6&NBA_ESPNgamelinks$Day==31),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==4&NBA_ESPNgamelinks$Day==31),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==31),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==30),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==30),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2021),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2022),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2023),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2031),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2025),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2026),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2027),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2033),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2029),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day==29&NBA_ESPNgamelinks$Year==2030),]
if (length(NBA_ESPNgamelinks$GameID)!=0) { 
for (i in 1:length(NBA_ESPNgamelinks$GameID)) {
        NBA_ESPNgamelinks[,c(4)] <- sapply(NBA_ESPNgamelinks[, c(4)], as.character)
        NBA_ESPNgamelinks[,c(5)] <- sapply(NBA_ESPNgamelinks[, c(5)], as.character)  
        date <- as.Date(paste0(NBA_ESPNgamelinks$Year[i],"-",NBA_ESPNgamelinks$Month[i],"-",NBA_ESPNgamelinks$Day[i],sep=""))
        url_boxscore <- NBA_ESPNgamelinks$BoxScore[NBA_ESPNgamelinks$GameID==NBA_ESPNgamelinks$GameID[i]]
        gameID <- substr(url_boxscore,nchar(url_boxscore)-8,nchar(url_boxscore))
        webpage <- read_html(url_boxscore)
        #awayteamCity_html <- html_nodes(webpage,'#fittPageContainer > div:nth-child(3) > div > div:nth-child(1) > div > div.Gamestrip__Competitors.relative.flex > div.Gamestrip__Team.relative.flex.w-100.items-center.Gamestrip__Team--away > div.Gamestrip__TeamContent.flex.tc.w-100 > div > div.Gamestrip__Info.flex.flex-column > div.Gamestrip__InfoWrapper.items-center.flex > div > a > div') %>% html_text()
        awayteamCity_html <- html_nodes(webpage,'.ScoreCell__TeamName.ScoreCell__TeamName--displayName.truncate.db') %>% html_text()
        #awayteamMascot_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.away > div > div.team-container > div.team-info > div.team-info-wrapper > a > span.short-name') %>% html_text()
        hometeamCity_html <- awayteamCity_html[2] 
        awayteamCity_html <- awayteamCity_html[1] 
        #hometeamCity_html <- html_nodes(webpage,'#fittPageContainer > div:nth-child(3) > div > div:nth-child(1) > div > div.Gamestrip__Competitors.relative.flex > div.Gamestrip__Team.relative.flex.w-100.items-center.Gamestrip__Team--home > div.Gamestrip__TeamContent.flex.tc.w-100 > div > div.Gamestrip__Info.flex.flex-column > div.Gamestrip__InfoWrapper.items-center.flex > div > a > div') %>% html_text()
        #hometeamMascot_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.home > div > div.team-container > div.team-info > div.team-info-wrapper > a > span.short-name') %>% html_text()
        # if (length(awayteamCity_html)==0) {
        #   awayteamCity_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.away > div > div.team-container > div.team-info > div.team-info-wrapper > div > span.long-name') %>% html_text()
        #   awayteamMascot_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.away > div > div.team-container > div.team-info > div.team-info-wrapper > div > span.short-name') %>% html_text()
        # }
        # if (length(hometeamCity_html)==0) {
        #   hometeamCity_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.home > div > div.team-container > div.team-info > div.team-info-wrapper > div > span.long-name') %>% html_text()
        #   hometeamMascot_html <- html_nodes(webpage,'#gamepackage-matchup-wrap > header > div > div.team.home > div > div.team-container > div.team-info > div.team-info-wrapper > div > span.short-name') %>% html_text()
        # }
        # print(i)
        GameID[i] <- gameID
        Team.x[i] <- paste0(hometeamCity_html,sep="")
        Team.y[i] <- paste0(awayteamCity_html,sep="")
        Date[i] <- date
        HomePts[i] <- NA
        AwayPts[i] <- NA
        HomeWin[i] <- NA
        HomeMargin[i] <- NA
        MIN.x[i] <- NA
        OREB.x[i] <- NA
        DREB.x[i] <- NA
        REB.x[i] <- NA
        AST.x[i] <- NA
        STL.x[i] <- NA
        BLK.x[i] <- NA
        TO.x[i] <- NA
        PF.x[i] <- NA
        PTS.x[i] <- NA
        FGM.x[i] <- NA
        FGA.x[i] <- NA
        `3PM.x`[i] <- NA
        `3PA.x`[i] <- NA
        FTM.x[i] <- NA
        FTA.x[i] <- NA
        `2PM.x`[i] <- NA
        `2PA.x`[i] <- NA
        `FG%.x`[i] <- NA
        `FT%.x`[i] <- NA
        `3P%.x`[i] <- NA
        `2P%.x`[i] <- NA
        #HAVE TO UPDATE THE NEXT 5 STRINGS BY SEASON
        GameDescription[i] <- paste0(awayteamCity_html," at ",hometeamCity_html,sep="")
        RegularSeason[i] <- ifelse(date >= as.Date("10/18/2022", format = "%m/%d/%Y") & date <= as.Date("04/09/2023", format = "%m/%d/%Y"),1,0)
        Playoffs[i] <- ifelse(date >= as.Date("04/11/2023", format = "%m/%d/%Y") & date <= as.Date("06/18/2023", format = "%m/%d/%Y"),1,0)
        Finals[i] <- ifelse(date >= as.Date("06/01/2023", format = "%m/%d/%Y") & date <= as.Date("06/18/2023", format = "%m/%d/%Y"),1,0)
        Season[i] <- "2022-23"
        MIN.y[i] <- NA
        OREB.y[i] <- NA
        DREB.y[i] <- NA
        REB.y[i] <- NA
        AST.y[i] <- NA
        STL.y[i] <- NA
        BLK.y[i] <- NA
        TO.y[i] <- NA
        PF.y[i] <- NA
        PTS.y[i] <- NA
        FGM.y[i] <- NA
        FGA.y[i] <- NA
        `3PM.y`[i] <- NA
        `3PA.y`[i] <- NA
        FTM.y[i] <- NA
        FTA.y[i] <- NA
        `2PM.y`[i] <- NA
        `2PA.y`[i] <- NA
        `FG%.y`[i] <- NA
        `FT%.y`[i] <- NA
        `3P%.y`[i] <- NA
        `2P%.y`[i] <- NA
}
      

preddata <- data.frame(GameID,Team.x,Team.y,Date,HomePts,AwayPts,HomeWin,HomeMargin,MIN.x,OREB.x,DREB.x,REB.x,AST.x,STL.x,BLK.x,TO.x,PF.x,PTS.x,FGM.x,FGA.x,`3PM.x`,`3PA.x`,FTM.x,FTA.x,`2PM.x`,`2PA.x`,`FG%.x`,`FT%.x`,`3P%.x`,`2P%.x`,GameDescription,RegularSeason,Playoffs,Finals,Season,MIN.y,OREB.y,DREB.y,REB.y,AST.y,STL.y,BLK.y,TO.y,PF.y,PTS.y,FGM.y,FGA.y,`3PM.y`,`3PA.y`,FTM.y,FTA.y,`2PM.y`,`2PA.y`,`FG%.y`,`FT%.y`,`3P%.y`,`2P%.y`)
preddata <- dplyr::rename(preddata,`3PM.x`=`X3PM.x`)
preddata <- dplyr::rename(preddata,`3PA.x`=`X3PA.x`,`3PA.x`=`X3PA.x`,`2PA.x`=`X2PA.x`,`2PM.x`=`X2PM.x`,`FG%.x`=`FG..x`)
preddata <- dplyr::rename(preddata,`FT%.x`=`FT..x`,`2P%.x`=`X2P..x`,`3P%.x`=`X3P..x`)
preddata <- dplyr::rename(preddata,`3PM.y`=`X3PM.y`)
preddata <- dplyr::rename(preddata,`3PA.y`=`X3PA.y`,`3PA.y`=`X3PA.y`,`2PA.y`=`X2PA.y`,`2PM.y`=`X2PM.y`,`FG%.y`=`FG..y`)
preddata <- dplyr::rename(preddata,`FT%.y`=`FT..y`,`2P%.y`=`X2P..y`,`3P%.y`=`X3P..y`)
preddata$Poss <- ((preddata$FGA.x+preddata$FGA.y)+0.44*(preddata$FTA.x+preddata$FTA.y)+preddata$TO.x+preddata$TO.y-preddata$OREB.x-preddata$OREB.y)/2
preddata$PtsPerPoss.x <- (preddata$PTS.x/preddata$Poss)*100
preddata$PtsPerPoss.y <- (preddata$PTS.y/preddata$Poss)*100
preddata$GameCount.x <- NA
preddata$GameCount.y <- NA
preddata$PtsPerPossAvg.x <- NA
preddata$PtsPerPossAvg.y <- NA
preddata$DefPtsPerPossAvg.x <- NA
preddata$DefPtsPerPossAvg.y <- NA
preddata$OppPtsPerPossAvg.x <- NA
preddata$OppDefPtsPerPossAvg.x <- NA
preddata$OppPtsPerPossAvg.y <- NA
preddata$OppDefPtsPerPossAvg.y <- NA
preddata$BtoB.x <- NA
preddata$BtoB.y <- NA
preddata$AdjNetRating.x <- NA
preddata$NetRating.x <- (preddata$PtsPerPossAvg.x - preddata$DefPtsPerPossAvg.x)
preddata$AdjNetRating.y <- (preddata$PtsPerPossAvg.y - preddata$DefPtsPerPossAvg.y) + (preddata$OppPtsPerPossAvg.y - preddata$OppDefPtsPerPossAvg.y)
preddata$NetRating.y <- (preddata$PtsPerPossAvg.y - preddata$DefPtsPerPossAvg.y)
preddata <- preddata %>% 
  dplyr::mutate(GameCountAvg = (GameCount.y + GameCount.x)/2)
preddata$Bubble<- ifelse(preddata$Date>="2020-04-01"&preddata$Date<="2020-10-20",1,0)
preddata$Pandemic<- ifelse(preddata$Date>="2020-12-01"&preddata$Date<"2021-04-01",1,0)
preddata$PartialPandemic<- ifelse(preddata$Date>="2021-04-01"&preddata$Date<="2021-09-01",1,0)
preddata <- preddata %>% dplyr::group_by(Season) %>% dplyr::mutate(SeasNum = cur_group_id())
preddata$PrevSeasonRating.x <- NA
preddata$PrevSeasonRating.y <- NA
preddata$Pace.x <- NA
preddata$Pace.y <- NA
preddata$GameDummy5 <- ifelse(preddata$GameCountAvg<=5,1,0)
preddata$GameDummy10 <- ifelse(preddata$GameCountAvg>5&preddata$GameCountAvg<=10,1,0)
preddata$GameDummy15 <- ifelse(preddata$GameCountAvg>10&preddata$GameCountAvg<=15,1,0)
preddata$GameDummy20 <- ifelse(preddata$GameCountAvg>15&preddata$GameCountAvg<=20,1,0)
preddata$GameDummy25 <- ifelse(preddata$GameCountAvg>20&preddata$GameCountAvg<=25,1,0)
preddata$GameDummy30 <- ifelse(preddata$GameCountAvg>25&preddata$GameCountAvg<=30,1,0)
preddata$GameDummy35 <- ifelse(preddata$GameCountAvg>30&preddata$GameCountAvg<=35,1,0)
preddata$GameDummy40 <- ifelse(preddata$GameCountAvg>35&preddata$GameCountAvg<=40,1,0)
preddata$GameDummy45 <- ifelse(preddata$GameCountAvg>40&preddata$GameCountAvg<=45,1,0)
preddata$GameDummy50 <- ifelse(preddata$GameCountAvg>45&preddata$GameCountAvg<=50,1,0)
preddata$GameDummy55 <- ifelse(preddata$GameCountAvg>50&preddata$GameCountAvg<=55,1,0)
preddata$GameDummy60 <- ifelse(preddata$GameCountAvg>55&preddata$GameCountAvg<=60,1,0)
preddata$GameDummy65 <- ifelse(preddata$GameCountAvg>60&preddata$GameCountAvg<=65,1,0)
preddata$GameDummy70 <- ifelse(preddata$GameCountAvg>65&preddata$GameCountAvg<=70,1,0)
preddata$GameDummy82 <- ifelse(preddata$GameCountAvg>70&preddata$GameCountAvg<=82,1,0)
preddata$GameDummy5XPrevSeas <- preddata$GameDummy5*preddata$PrevSeasonRating.x
preddata$GameDummy10XPrevSeas <- preddata$GameDummy10*preddata$PrevSeasonRating.x
preddata$GameDummy15XPrevSeas <- preddata$GameDummy15*preddata$PrevSeasonRating.x
preddata$GameDummy20XPrevSeas <- preddata$GameDummy20*preddata$PrevSeasonRating.x
preddata$GameDummy25XPrevSeas <- preddata$GameDummy25*preddata$PrevSeasonRating.x
preddata$GameDummy30XPrevSeas <- preddata$GameDummy30*preddata$PrevSeasonRating.x
preddata$GameDummy35XPrevSeas <- preddata$GameDummy35*preddata$PrevSeasonRating.x
preddata$GameDummy40XPrevSeas <- preddata$GameDummy40*preddata$PrevSeasonRating.x
preddata$GameDummy45XPrevSeas <- preddata$GameDummy45*preddata$PrevSeasonRating.x
preddata$GameDummy50XPrevSeas <- preddata$GameDummy50*preddata$PrevSeasonRating.x
preddata$GameDummy55XPrevSeas <- preddata$GameDummy55*preddata$PrevSeasonRating.x
preddata$GameDummy60XPrevSeas <- preddata$GameDummy60*preddata$PrevSeasonRating.x
preddata$GameDummy65XPrevSeas <- preddata$GameDummy65*preddata$PrevSeasonRating.x
preddata$GameDummy70XPrevSeas <- preddata$GameDummy70*preddata$PrevSeasonRating.x
preddata$GameDummy82XPrevSeas <- preddata$GameDummy82*preddata$PrevSeasonRating.x
preddata$GameDummy5XPrevSeasY <- preddata$GameDummy5*preddata$PrevSeasonRating.y
preddata$GameDummy10XPrevSeasY <- preddata$GameDummy10*preddata$PrevSeasonRating.y
preddata$GameDummy15XPrevSeasY <- preddata$GameDummy15*preddata$PrevSeasonRating.y
preddata$GameDummy20XPrevSeasY <- preddata$GameDummy20*preddata$PrevSeasonRating.y
preddata$GameDummy25XPrevSeasY <- preddata$GameDummy25*preddata$PrevSeasonRating.y
preddata$GameDummy30XPrevSeasY <- preddata$GameDummy30*preddata$PrevSeasonRating.y
preddata$GameDummy35XPrevSeasY <- preddata$GameDummy35*preddata$PrevSeasonRating.y
preddata$GameDummy40XPrevSeasY <- preddata$GameDummy40*preddata$PrevSeasonRating.y
preddata$GameDummy45XPrevSeasY <- preddata$GameDummy45*preddata$PrevSeasonRating.y
preddata$GameDummy50XPrevSeasY <- preddata$GameDummy50*preddata$PrevSeasonRating.y
preddata$GameDummy55XPrevSeasY <- preddata$GameDummy55*preddata$PrevSeasonRating.y
preddata$GameDummy60XPrevSeasY <- preddata$GameDummy60*preddata$PrevSeasonRating.y
preddata$GameDummy65XPrevSeasY <- preddata$GameDummy65*preddata$PrevSeasonRating.y
preddata$GameDummy70XPrevSeasY <- preddata$GameDummy70*preddata$PrevSeasonRating.y
preddata$GameDummy82XPrevSeasY <- preddata$GameDummy82*preddata$PrevSeasonRating.y
preddata$PaceXPace <- preddata$Pace.x*preddata$Pace.y
preddata$PaceXSq <- preddata$Pace.x*preddata$Pace.x
preddata$PaceYSq <- preddata$Pace.y*preddata$Pace.y
preddata$HomeMargPerPoss <- preddata$HomeMargin/(preddata$Poss/100)
preddata$AdjNetMarSqPlus <- (preddata$AdjNetRating.x-preddata$AdjNetRating.y)^2*((preddata$AdjNetRating.x-preddata$AdjNetRating.y)>0)
preddata$AdjNetMarSqNeg <- (preddata$AdjNetRating.x-preddata$AdjNetRating.y)^2*((preddata$AdjNetRating.x-preddata$AdjNetRating.y)<=0)
preddata$AdjNetMarCub <- (preddata$AdjNetRating.x-preddata$AdjNetRating.y)^3
preddata$Year <- year(preddata$Date)
preddata$YearSq <- preddata$Year*preddata$Year
preddata$YearCub <- preddata$Year*preddata$Year*preddata$Year
preddata$GameCount.x <- NA
preddata$GameCount.y <- NA
preddata$Wins.x <- NA
preddata$Losses.x <- NA
preddata$Wins20.x <- NA
preddata$Losses20.x <- NA
preddata$Wins10.x <- NA
preddata$Losses10.x <- NA
preddata$Wins5.x <- NA
preddata$Losses5.x <- NA
preddata$Wins.y <- NA
preddata$Losses.y <- NA
preddata$Wins20.y <- NA
preddata$Losses20.y <- NA
preddata$Wins10.y <- NA
preddata$Losses10.y <- NA
preddata$Wins5.y <- NA
preddata$Losses5.y <- NA
preddata$PtsPerPossAvg.x <- NA
preddata$PtsPerPossAvg.y <- NA                                                                                                                                                                           
preddata$DefPtsPerPossAvg.x <- NA
preddata$DefPtsPerPossAvg.y <- NA
preddata$PtsPerPossAvg20.x <- NA
preddata$PtsPerPossAvg20.y <- NA                                                                                                                                                                                                                                         
preddata$DefPtsPerPossAvg20.x <- NA
preddata$DefPtsPerPossAvg20.y <- NA
preddata$PtsPerPossAvg10.x <- NA
preddata$PtsPerPossAvg10.y <- NA                                                                                                                                                                                                                                        
preddata$DefPtsPerPossAvg10.x <- NA
preddata$DefPtsPerPossAvg10.y <- NA
preddata$PtsPerPossAvg5.x <- NA
preddata$PtsPerPossAvg5.y <- NA                                                                                                                                                                                                                                       
preddata$DefPtsPerPossAvg5.x <- NA
preddata$DefPtsPerPossAvg5.y <- NA                                                                                                                                                                                                                                       
preddata$OppPtsPerPossAvg.x <- NA
preddata$OppDefPtsPerPossAvg.x <- NA
preddata$OppPtsPerPossAvg20.x <- NA
preddata$OppDefPtsPerPossAvg20.x <- NA
preddata$OppPtsPerPossAvg10.x <- NA
preddata$OppDefPtsPerPossAvg10.x <- NA
preddata$OppPtsPerPossAvg5.x <- NA
preddata$OppDefPtsPerPossAvg5.x <- NA
preddata$OppPtsPerPossAvg.y <- NA
preddata$OppDefPtsPerPossAvg.y <- NA
preddata$OppPtsPerPossAvg20.y <- NA
preddata$OppDefPtsPerPossAvg20.y <- NA
preddata$OppPtsPerPossAvg10.y <- NA
preddata$OppDefPtsPerPossAvg10.y <- NA
preddata$OppPtsPerPossAvg5.y <- NA
preddata$OppDefPtsPerPossAvg5.y <- NA
preddata$WinPct.x <- ifelse((preddata$Wins.x + preddata$Losses.x)==0,.5,preddata$Wins.x/(preddata$Wins.x + preddata$Losses.x))
preddata$WinPct20.x <- ifelse((preddata$Wins.x + preddata$Losses.x)==0,.5,preddata$Wins20.x/(preddata$Wins20.x + preddata$Losses20.x))
preddata$WinPct10.x <- ifelse((preddata$Wins.x + preddata$Losses.x)==0,.5,preddata$Wins10.x/(preddata$Wins10.x + preddata$Losses10.x))
preddata$WinPct5.x <- ifelse((preddata$Wins.x + preddata$Losses.x)==0,.5,preddata$Wins5.x/(preddata$Wins5.x + preddata$Losses5.x))
preddata$WinPct.y <- ifelse((preddata$Wins.y + preddata$Losses.y)==0,.5,preddata$Wins.y/(preddata$Wins.y + preddata$Losses.y))
preddata$WinPct20.y <- ifelse((preddata$Wins.y + preddata$Losses.y)==0,.5,preddata$Wins20.y/(preddata$Wins20.y + preddata$Losses20.y))
preddata$WinPct10.y <- ifelse((preddata$Wins.y + preddata$Losses.y)==0,.5,preddata$Wins10.y/(preddata$Wins10.y + preddata$Losses10.y))
preddata$WinPct5.y <- ifelse((preddata$Wins.y + preddata$Losses.y)==0,.5,preddata$Wins5.y/(preddata$Wins5.y + preddata$Losses5.y))
preddata$AdjNetRating20.x <- (preddata$PtsPerPossAvg20.x - preddata$DefPtsPerPossAvg20.x) + (preddata$OppPtsPerPossAvg20.x - preddata$OppDefPtsPerPossAvg20.x)
preddata$AdjNetRating20.y <- (preddata$PtsPerPossAvg20.y - preddata$DefPtsPerPossAvg20.y) + (preddata$OppPtsPerPossAvg20.y - preddata$OppDefPtsPerPossAvg20.y)
preddata$AdjNetRating10.x <- (preddata$PtsPerPossAvg10.x - preddata$DefPtsPerPossAvg10.x) + (preddata$OppPtsPerPossAvg10.x - preddata$OppDefPtsPerPossAvg10.x)
preddata$AdjNetRating10.y <- (preddata$PtsPerPossAvg10.y - preddata$DefPtsPerPossAvg10.y) + (preddata$OppPtsPerPossAvg10.y - preddata$OppDefPtsPerPossAvg10.y)
preddata$AdjNetRating5.x <- (preddata$PtsPerPossAvg5.x - preddata$DefPtsPerPossAvg5.x) + (preddata$OppPtsPerPossAvg5.x - preddata$OppDefPtsPerPossAvg5.x)
preddata$AdjNetRating5.y <- (preddata$PtsPerPossAvg5.y - preddata$DefPtsPerPossAvg5.y) + (preddata$OppPtsPerPossAvg5.y - preddata$OppDefPtsPerPossAvg5.y)
preddata$GameDummyLess20 <- ifelse(preddata$GameCountAvg<=20,1,0)
preddata$WinPctX20.x <- preddata$WinPct.x*preddata$GameDummyLess20
preddata$WinPct20X20.x <- preddata$WinPct20.x*preddata$GameDummyLess20
preddata$WinPct10X20.x <- preddata$WinPct10.x*preddata$GameDummyLess20
preddata$WinPct5X20.x <- preddata$WinPct5.x*preddata$GameDummyLess20
preddata$WinPctX20.y <- preddata$WinPct.y*preddata$GameDummyLess20
preddata$WinPct20X20.y <-preddata$WinPct20.y*preddata$GameDummyLess20
preddata$WinPct10X20.y <- preddata$WinPct10.y*preddata$GameDummyLess20
preddata$WinPct5X20.y <- preddata$WinPct5.y*preddata$GameDummyLess20
preddata$AdjNetRating20X20.x <- preddata$AdjNetRating20.x*preddata$GameDummyLess20
preddata$AdjNetRating20X20.y <- preddata$AdjNetRating20.y*preddata$GameDummyLess20
preddata$AdjNetRating10X20.x <- preddata$AdjNetRating10.x*preddata$GameDummyLess20
preddata$AdjNetRating10X20.y <- preddata$AdjNetRating10.y*preddata$GameDummyLess20
preddata$AdjNetRating5X20.x <- preddata$AdjNetRating5.x*preddata$GameDummyLess20
preddata$AdjNetRating5X20.y <- preddata$AdjNetRating5.y*preddata$GameDummyLess20
preddata$OREBPerPoss.x <- NA
preddata$OREBPerPoss.y <- NA
preddata$DefOREBPerPoss.x <- NA
preddata$DefOREBPerPoss.y <- NA
preddata$DREBPerPoss.x <- NA
preddata$DREBPerPoss.y <- NA
preddata$DefDREBPerPoss.x <- NA
preddata$DefDREBPerPoss.y <- NA
preddata$ASTPerPoss.x <- NA
preddata$ASTPerPoss.y <- NA
preddata$DefASTPerPoss.x <- NA
preddata$DefASTPerPoss.y <- NA
preddata$STLPerPoss.x <- NA
preddata$STLPerPoss.y <- NA
preddata$DefSTLPerPoss.x <- NA
preddata$DefSTLPerPoss.y <- NA
preddata$BLKPerPoss.x <- NA
preddata$BLKPerPoss.y <- NA
preddata$DefBLKPerPoss.x <- NA
preddata$DefBLKPerPoss.y <- NA
preddata$TOPerPoss.x <- NA
preddata$TOPerPoss.y <- NA
preddata$DefTOPerPoss.x <- NA
preddata$DefTOPerPoss.y <- NA
preddata$ThrPMPerPoss.x <- NA
preddata$ThrPMPerPoss.y <- NA
preddata$DefThrPMPerPoss.x <- NA
preddata$DefThrPMPerPoss.y <- NA
preddata$ThrPAPerPoss.x <- NA
preddata$ThrPAPerPoss.y <- NA
preddata$DefThrPAPerPoss.x <- NA
preddata$DefThrPAPerPoss.y <- NA
preddata$TwoPMPerPoss.x <- NA
preddata$TwoPMPerPoss.y <- NA
preddata$DefTwoPMPerPoss.x <- NA
preddata$DefTwoPMPerPoss.y <- NA
preddata$TwoPAPerPoss.x <- NA
preddata$TwoPAPerPoss.y <- NA
preddata$DefTwoPAPerPoss.x <- NA
preddata$DefTwoPAPerPoss.y <- NA
preddata$FTMPerPoss.x <- NA
preddata$FTMPerPoss.y <- NA
preddata$DefFTMPerPoss.x <- NA
preddata$DefFTMPerPoss.y <- NA
preddata$FTAPerPoss.x <- NA
preddata$FTAPerPoss.y <- NA
preddata$DefFTAPerPoss.x <- NA
preddata$DefFTAPerPoss.y <- NA
preddata$FGMPerPoss.x <- NA
preddata$FGMPerPoss.y <- NA
preddata$DefFGMPerPoss.x <- NA
preddata$DefFGMPerPoss.y <- NA
preddata$FGAPerPoss.x <- NA
preddata$FGAPerPoss.y <- NA
preddata$DefFGAPerPoss.x <- NA
preddata$DefFGAPerPoss.y <- NA
preddata$DREBrt.x <- preddata$DREBPerPoss.x/(preddata$DefFGAPerPoss.x - preddata$DefFGMPerPoss.x)
preddata$DREBrt.y <- preddata$DREBPerPoss.y/(preddata$DefFGAPerPoss.y - preddata$DefFGMPerPoss.y)
preddata$OREBrt.x <- preddata$OREBPerPoss.x/(preddata$FGAPerPoss.x - preddata$FGMPerPoss.x)
preddata$OREBrt.y <- preddata$OREBPerPoss.y/(preddata$FGAPerPoss.y - preddata$FGMPerPoss.y)
preddata$FGMissEST <- (((preddata$DefFGAPerPoss.x + preddata$FGAPerPoss.x) - (preddata$DefFGMPerPoss.x + preddata$FGMPerPoss.x)) + ((preddata$DefFGAPerPoss.y + preddata$FGAPerPoss.y) - (preddata$DefFGMPerPoss.y + preddata$FGMPerPoss.y)))/2
preddata$FGMissEST.x <- ((preddata$DefFGAPerPoss.y - preddata$DefFGMPerPoss.y) + (preddata$FGAPerPoss.x - preddata$FGMPerPoss.x))/2
preddata$FGMissEST.y <- ((preddata$DefFGAPerPoss.x - preddata$DefFGMPerPoss.x) + (preddata$FGAPerPoss.y - preddata$FGMPerPoss.y))/2
preddata$DREBrtXFGMiss.x <- preddata$DREBrt.x*preddata$FGMissEST
preddata$DREBrtXFGMiss.y <- preddata$DREBrt.y*preddata$FGMissEST
preddata$OREBrtXFGMiss.x <- preddata$OREBrt.x*preddata$FGMissEST.x
preddata$OREBrtXFGMiss.y <- preddata$OREBrt.y*preddata$FGMissEST.y
preddata$ThrPercent.x <- preddata$ThrPMPerPoss.x/preddata$ThrPAPerPoss.x
preddata$ThrPercent.y <- preddata$ThrPMPerPoss.y/preddata$ThrPAPerPoss.y
preddata$TwoPercent.x <- preddata$TwoPMPerPoss.x/preddata$TwoPAPerPoss.x
preddata$TwoPercent.y <- preddata$TwoPMPerPoss.y/preddata$TwoPAPerPoss.y
preddata$FTPercent.x <- preddata$FTMPerPoss.x/preddata$FTAPerPoss.x
preddata$FTPercent.y <- preddata$FTMPerPoss.y/preddata$FTAPerPoss.y
preddata$DefThrPercent.x <- preddata$DefThrPMPerPoss.x/preddata$DefThrPAPerPoss.x
preddata$DefThrPercent.y <- preddata$DefThrPMPerPoss.y/preddata$DefThrPAPerPoss.y
preddata$DefTwoPercent.x <- preddata$DefTwoPMPerPoss.x/preddata$DefTwoPAPerPoss.x
preddata$DefTwoPercent.y <- preddata$DefTwoPMPerPoss.y/preddata$DefTwoPAPerPoss.y
preddata$FTPercentXDefFTs.x <- preddata$FTPercent.y*preddata$DefFTAPerPoss.x
preddata$FTPercentXDefFTs.y <- preddata$FTPercent.x*preddata$DefFTAPerPoss.y
preddata$PrevThrPercent.x <- NA
preddata$PrevThrPercent.y <- NA
preddata$PrevThrPAPerPoss.x <- NA
preddata$PrevThrPAPerPoss.y <- NA
preddata$PrevThrPMPerPoss.x <- NA
preddata$PrevThrPMPerPoss.y <- NA
preddata$PrevDefThrPercent.x <- NA
preddata$PrevDefThrPercent.y <- NA
preddata$PrevDefThrPAPerPoss.x <- NA
preddata$PrevDefThrPAPerPoss.y <- NA
preddata$PrevDefThrPMPerPoss.x <- NA
preddata$PrevDefThrPMPerPoss.y <- NA
preddata$PrevTwoPercent.x <- NA
preddata$PrevTwoPercent.y <- NA
preddata$PrevTwoPAPerPoss.x <- NA
preddata$PrevTwoPAPerPoss.y <- NA
preddata$PrevTwoPMPerPoss.x <- NA
preddata$PrevTwoPMPerPoss.y <- NA
preddata$PrevDefTwoPercent.x <- NA
preddata$PrevDefTwoPercent.y <- NA
preddata$PrevDefTwoPAPerPoss.x <- NA
preddata$PrevDefTwoPAPerPoss.y <- NA
preddata$PrevDefTwoPMPerPoss.x <- NA
preddata$PrevDefTwoPMPerPoss.y <- NA
preddata$PrevFTPercent.x <- NA
preddata$PrevFTPercent.y <- NA
preddata$PrevFTAPerPoss.x <- NA
preddata$PrevFTAPerPoss.y <- NA
preddata$PrevFTMPerPoss.x <- NA
preddata$PrevFTMPerPoss.y <- NA
preddata$PrevFTPercentXDefFTs.x <- NA
preddata$PrevFTPercentXDefFTs.y <- NA
preddata$PrevDefFTAPerPoss.x <- NA
preddata$PrevDefFTAPerPoss.y <- NA
preddata$PrevDREBrt.x <- NA
preddata$PrevDREBrt.y <- NA
preddata$PrevOREBrt.x <- NA
preddata$PrevOREBrt.y <- NA
preddata$PrevDREBrtXFGMiss.x <- NA
preddata$PrevDREBrtXFGMiss.y <- NA
preddata$PrevOREBrtXFGMiss.x <- NA
preddata$PrevOREBrtXFGMiss.y <- NA
preddata$PrevASTPerPoss.x <- NA
preddata$PrevASTPerPoss.y <- NA
preddata$PrevSTLPerPoss.x <- NA
preddata$PrevSTLPerPoss.y <- NA
preddata$PrevBLKPerPoss.x <- NA
preddata$PrevBLKPerPoss.y <- NA
preddata$PrevTOPerPoss.x <- NA
preddata$PrevTOPerPoss.y <- NA
preddata$PrevDefASTPerPoss.x <- NA
preddata$PrevDefASTPerPoss.y <- NA
preddata$PrevDefSTLPerPoss.x <- NA
preddata$PrevDefSTLPerPoss.y <- NA
preddata$PrevDefBLKPerPoss.x <- NA
preddata$PrevDefBLKPerPoss.y <- NA
preddata$PrevDefTOPerPoss.x <- NA
preddata$PrevDefTOPerPoss.y <- NA

#Add V4 stuff
preddata$GameDescription <- paste(preddata$Team.y," at ",preddata$Team.x,sep="")
preddata$PrevPtsPerPossAvg.x <- NA
preddata$PrevPtsPerPossAvg.y <- NA
preddata$PrevDefPtsPerPossAvg.x <- NA
preddata$PrevDefPtsPerPossAvg.y <- NA
preddata$PrevOppPtsPerPossAvg.x <- NA
preddata$PrevOppPtsPerPossAvg.y <- NA
preddata$PrevOppDefPtsPerPossAvg.x <- NA
preddata$PrevOppDefPtsPerPossAvg.y <- NA
preddata$PrevPace.x <- NA
preddata$PrevPace.y <- NA
preddata$PrevWinPct.x <- NA
preddata$PrevWinPct.y <- NA
preddata$NumGamesWeek.x <- NA
preddata$NumHomeGamesWeek.x <- NA
preddata$NumGamesWeek.y <- NA 
preddata$NumHomeGamesWeek.y <- NA
preddata$PtsPerPossAvg2.x  <- 1*preddata$PtsPerPossAvg.x  + 0*preddata$PrevPtsPerPossAvg.x 
preddata$DefPtsPerPossAvg2.x  <- 1*preddata$DefPtsPerPossAvg.x  + 0*preddata$PrevDefPtsPerPossAvg.x 
preddata$OppPtsPerPossAvg2.x  <- 1*preddata$OppPtsPerPossAvg.x  + 0*preddata$PrevOppPtsPerPossAvg.x 
preddata$OppDefPtsPerPossAvg2.x  <- 1*preddata$OppDefPtsPerPossAvg.x  + 0*preddata$PrevOppDefPtsPerPossAvg.x 
preddata$Pace2.x  <- 1*preddata$Pace.x  + 0*preddata$PrevPace.x 
preddata$WinPct2.x  <- 1*preddata$WinPct.x  + 0*preddata$PrevWinPct.x 
preddata$PtsPerPossAvg2.y  <- 1*preddata$PtsPerPossAvg.y  + 0*preddata$PrevPtsPerPossAvg.y 
preddata$DefPtsPerPossAvg2.y  <- 1*preddata$DefPtsPerPossAvg.y  + 0*preddata$PrevDefPtsPerPossAvg.y 
preddata$OppPtsPerPossAvg2.y  <- 1*preddata$OppPtsPerPossAvg.y  + 0*preddata$PrevOppPtsPerPossAvg.y 
preddata$OppDefPtsPerPossAvg2.y  <- 1*preddata$OppDefPtsPerPossAvg.y  + 0*preddata$PrevOppDefPtsPerPossAvg.y 
preddata$Pace2.y  <- 1*preddata$Pace.y  + 0*preddata$PrevPace.y 
preddata$WinPct2.y  <- 1*preddata$WinPct.y  + 0*preddata$PrevWinPct.y 
preddata$AdjNetRating2.x <- (preddata$PtsPerPossAvg2.x - preddata$DefPtsPerPossAvg2.x) + (preddata$OppPtsPerPossAvg2.x - preddata$OppDefPtsPerPossAvg2.x)
preddata$NetRating2.x <- (preddata$PtsPerPossAvg2.x - preddata$DefPtsPerPossAvg2.x)
preddata$AdjNetRating2.y <- (preddata$PtsPerPossAvg2.y - preddata$DefPtsPerPossAvg2.y) + (preddata$OppPtsPerPossAvg2.y - preddata$OppDefPtsPerPossAvg2.y)
preddata$NetRating2.y <- (preddata$PtsPerPossAvg2.y - preddata$DefPtsPerPossAvg2.y)
preddata$PaceXPace2 <- preddata$Pace2.x*preddata$Pace2.y
preddata$PaceXSq2 <- preddata$Pace2.x*preddata$Pace2.x
preddata$PaceYSq2 <- preddata$Pace2.y*preddata$Pace2.y
preddata$AdjNetMarSqPlus2 <- (preddata$AdjNetRating2.x-preddata$AdjNetRating2.y)^2*((preddata$AdjNetRating2.x-preddata$AdjNetRating2.y)>0)
preddata$AdjNetMarSqNeg2 <- (preddata$AdjNetRating2.x-preddata$AdjNetRating2.y)^2*((preddata$AdjNetRating2.x-preddata$AdjNetRating2.y)<=0)
preddata$AdjNetMarCub2 <- (preddata$AdjNetRating2.x-preddata$AdjNetRating2.y)^3
preddata$ShrHomeWeek.x <- ifelse(preddata$NumGamesWeek.x==0,.5,preddata$NumHomeGamesWeek.x/preddata$NumGamesWeek.x)
preddata$ShrHomeWeek.y <- ifelse(preddata$NumGamesWeek.y==0,0.5,preddata$NumHomeGamesWeek.y/preddata$NumGamesWeek.y)
preddata$ThrPercentXX.x <- preddata$ThrPercent.x*preddata$DefThrPercent.y 
preddata$ThrPAPerPossXX.x <- preddata$ThrPAPerPoss.x*preddata$DefThrPAPerPoss.y
preddata$ThrPMPerPossXX.x <- preddata$ThrPMPerPoss.x*preddata$DefThrPMPerPoss.y
preddata$TwoPercentXX.x <- preddata$TwoPercent.x*preddata$DefTwoPercent.y 
preddata$TwoPAPerPossXX.x <- preddata$TwoPAPerPoss.x*preddata$DefTwoPAPerPoss.y
preddata$TwoPMPerPossXX.x <- preddata$TwoPMPerPoss.x*preddata$DefTwoPMPerPoss.y
preddata$FTAPerPossXX.x <- preddata$FTAPerPoss.x*preddata$DefFTAPerPoss.y
preddata$DREBRtXX.x <- preddata$DREBrt.x*preddata$OREBrt.y
preddata$DREBTotXX.x <- preddata$DREBrtXFGMiss.x*preddata$OREBrtXFGMiss.y
preddata$ASTPerPossXX.x <- preddata$ASTPerPoss.x*preddata$DefASTPerPoss.y 
preddata$TOPerPossXX.x <- preddata$TOPerPoss.x*preddata$DefTOPerPoss.y 
preddata$ThrPercentXX.y <- preddata$ThrPercent.y*preddata$DefThrPercent.x 
preddata$ThrPAPerPossXX.y <- preddata$ThrPAPerPoss.y*preddata$DefThrPAPerPoss.x
preddata$ThrPMPerPossXX.y <- preddata$ThrPMPerPoss.y*preddata$DefThrPMPerPoss.x
preddata$TwoPercentXX.y <- preddata$TwoPercent.y*preddata$DefTwoPercent.x 
preddata$TwoPAPerPossXX.y <- preddata$TwoPAPerPoss.y*preddata$DefTwoPAPerPoss.x
preddata$TwoPMPerPossXX.y <- preddata$TwoPMPerPoss.y*preddata$DefTwoPMPerPoss.x
preddata$FTAPerPossXX.y <- preddata$FTAPerPoss.y*preddata$DefFTAPerPoss.x
preddata$DREBRtXX.y <- preddata$DREBrt.y*preddata$OREBrt.x
preddata$DREBTotXX.y <- preddata$DREBrtXFGMiss.y*preddata$OREBrtXFGMiss.x
preddata$ASTPerPossXX.y <- preddata$ASTPerPoss.y*preddata$DefASTPerPoss.x 
preddata$TOPerPossXX.y <- preddata$TOPerPoss.y*preddata$DefTOPerPoss.x 
NBAdistance <- read_dta("/Users/ricardovelloso/Dropbox/Data/NBAGambling/NBAdistancewithTEAMS.dta")
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
preddata <- merge(preddata,NBAdistanceMERGE, by=c("GameDescription"),all.x=TRUE,all.y=FALSE)
preddata$mi_to_place[preddata$GameDescription=="Brooklyn Nets at New York Knicks"] <- 0
preddata$mi_to_place[preddata$GameDescription=="New York Knicks at Brooklyn Nets"] <- 0
preddata$mi_to_place[preddata$GameDescription=="Los Angeles Lakers at LA Clippers"] <- 0
preddata$mi_to_place[preddata$GameDescription=="LA Clippers at Los Angeles Lakers"] <- 0
preddata$mi_to_place[preddata$GameDescription=="Los Angeles Lakers at New York Knicks"] <- 2458.96962
preddata$mi_to_place[preddata$GameDescription=="New York Knicks at Los Angeles Lakers"] <- 2458.96962
preddata$DistDummy1 <- ifelse(preddata$mi_to_place==0,1,0)
preddata$DistDummy2 <- ifelse(preddata$mi_to_place>0&preddata$mi_to_place<=200,1,0)
preddata$DistDummy3 <- ifelse(preddata$mi_to_place>200&preddata$mi_to_place<=500,1,0)
preddata$DistDummy4 <- ifelse(preddata$mi_to_place>500&preddata$mi_to_place<=1000,1,0)
preddata$DistDummy5 <- ifelse(preddata$mi_to_place>1000&preddata$mi_to_place<=1500,1,0)
preddata$DistDummy6 <- ifelse(preddata$mi_to_place>1500&preddata$mi_to_place<=2000,1,0)
preddata$DistDummy7 <- ifelse(preddata$mi_to_place>2000,1,0)
preddata$TotalPoints <- preddata$PTS.x + preddata$PTS.y
preddata$TotalPointsPerPoss <- preddata$PtsPerPoss.x + preddata$PtsPerPoss.y
preddata <- preddata[preddata$Team.x!="TBD"&preddata$Team.x!="TBD "&preddata$Team.y!="TBD"&preddata$Team.y!="TBD ",]
for (i in unique(seasondata$Team.x[seasondata$Season=="2021-22"])) {
  if (!(i %in% preddata$Team.x)&!((i %in% preddata$Team.y))) {
    preddata[nrow(preddata)+1,] <- preddata[nrow(preddata),]
    preddata$GameDescription[nrow(preddata)] <- paste(i," at ",i,sep="")
    preddata$Team.x[nrow(preddata)] <- paste(i,sep="")
    preddata$Team.y[nrow(preddata)] <- paste(i,sep="")
    preddata$Date[nrow(preddata)] <- max(preddata$Date)
    preddata$GameID[nrow(preddata)] <- paste("999",i,sep="")
  }
}

#same as "UpdateTeamVariables" but just for future games now
seasondataTEMP <- preddata
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
}
seasondataOLD <- seasondata[seasondata$Date<startdate,]
start <- length(seasondataOLD$Date)
#for (i in 1:300) {
for (i in start:length(seasondata$Date)) {
  seasondata$GameCount.x[i] <- length(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<=seasondata$Date[i]&seasondata$Season==seasondata$Season[i]])
  seasondata$GameCount.y[i] <- length(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<=seasondata$Date[i]&seasondata$Season==seasondata$Season[i]])
  
  seasondata$Wins.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,na.rm = TRUE)
  seasondata$Losses.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,na.rm = TRUE)
  seasondata$Wins20.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19),na.rm = TRUE)
  seasondata$Losses20.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19),na.rm = TRUE)
  seasondata$Wins10.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9),na.rm = TRUE)
  seasondata$Losses10.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9),na.rm = TRUE)
  seasondata$Wins5.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4),na.rm = TRUE)
  seasondata$Losses5.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4),na.rm = TRUE)
  
  seasondata$Wins.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,na.rm = TRUE)
  seasondata$Losses.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0,(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0,na.rm = TRUE)
  seasondata$Wins20.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19),na.rm = TRUE)
  seasondata$Losses20.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19),na.rm = TRUE)
  seasondata$Wins10.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9),na.rm = TRUE)
  seasondata$Losses10.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9),na.rm = TRUE)
  seasondata$Wins5.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4),na.rm = TRUE)
  seasondata$Losses5.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin<0&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$HomeMargin>0&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4),na.rm = TRUE)

    
  seasondata$PtsPerPossAvg.x[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                         + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE)))
  
  seasondata$PtsPerPossAvg.y[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                         + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                                                                                                                                                                                                     + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE)))
  seasondata$DefPtsPerPossAvg.x[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                            + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE)))
  
  seasondata$DefPtsPerPossAvg.y[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                            + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE) 
                                                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm = TRUE)))
  
  #last 20
  seasondata$PtsPerPossAvg20.x[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19))],na.rm=TRUE) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                              + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19))],na.rm=TRUE)))
  
  seasondata$PtsPerPossAvg20.y[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19))],na.rm=TRUE) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                              + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19))],na.rm=TRUE)))
  seasondata$DefPtsPerPossAvg20.x[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)],na.rm=TRUE) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-19)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                                 + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-19)],na.rm=TRUE)))
  
  seasondata$DefPtsPerPossAvg20.y[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)],na.rm=TRUE) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-19)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                                 + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-19)],na.rm=TRUE)))
  
  #last 10
  seasondata$PtsPerPossAvg10.x[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9))],na.rm=TRUE) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9))],na.rm=TRUE)))
  
  seasondata$PtsPerPossAvg10.y[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9))],na.rm=TRUE) 
                                           + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9))],na.rm=TRUE)))
  seasondata$DefPtsPerPossAvg10.x[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)],na.rm=TRUE) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-9)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                                + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-9)],na.rm=TRUE)))
  
  seasondata$DefPtsPerPossAvg10.y[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)],na.rm=TRUE) 
                                              + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-9)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                                + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-9)],na.rm=TRUE)))
  #last 5
  seasondata$PtsPerPossAvg5.x[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4))],na.rm=TRUE) 
                                          + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                            + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4))],na.rm=TRUE)))
  
  seasondata$PtsPerPossAvg5.y[i] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4))],na.rm=TRUE) 
                                          + sum(seasondata$PTS.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4))],na.rm=TRUE) 
                                                                                                                                                                                                                                                                            + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4))],na.rm=TRUE)))
  seasondata$DefPtsPerPossAvg5.x[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)],na.rm=TRUE) 
                                             + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.x[i]-4)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                               + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.x[i]-4)],na.rm=TRUE)))
  
  seasondata$DefPtsPerPossAvg5.y[i] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)],na.rm=TRUE) 
                                             + sum(seasondata$PTS.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.x>=(seasondata$GameCount.y[i]-4)],na.rm=TRUE) 
                                                                                                                                                                                                                                                                               + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])&seasondata$GameCount.y>=(seasondata$GameCount.y[i]-4)],na.rm=TRUE)))
  
  #get oppoent strengths
  oppvec.x <- c(seasondata$Team.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],seasondata$Team.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])
  oppvec.y <- c(seasondata$Team.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],seasondata$Team.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])
  gmct.x <- c(seasondata$GameCount.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],seasondata$GameCount.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])
  gmct.y <- c(seasondata$GameCount.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],seasondata$GameCount.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])
  mA <- data.frame(oppvec.x,gmct.x)
  mA <- mA[order(gmct.x),]
  oppvec.x <- mA$oppvec.x
  mB <- data.frame(oppvec.y,gmct.y)
  mB <- mB[order(gmct.y),]
  oppvec.y <- mB$oppvec.y
  ptsvec.x <- c(0)
  defptsvec.x <- c(0)
  for (j in 1:length(oppvec.x)) {
    ptsvec.x[j] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                           sum(seasondata$PTS.y[(seasondata$Team.y==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                                                                                                                                                                  sum(seasondata$Poss[(seasondata$Team.y==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
    defptsvec.x[j] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                              sum(seasondata$PTS.x[(seasondata$Team.y==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                                                                                                                                                                     sum(seasondata$Poss[(seasondata$Team.y==oppvec.x[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
    
  }
  
  seasondata$OppPtsPerPossAvg.x[i] <- mean(ptsvec.x,na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg.x[i] <- mean(defptsvec.x,na.rm = TRUE)
  seasondata$OppPtsPerPossAvg20.x[i] <- mean(ptsvec.x[max((length(ptsvec.x)-19),1):length(ptsvec.x)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg20.x[i] <- mean(defptsvec.x[max((length(ptsvec.x)-19),1):length(ptsvec.x)],na.rm = TRUE)
  seasondata$OppPtsPerPossAvg10.x[i] <- mean(ptsvec.x[max((length(ptsvec.x)-9),1):length(ptsvec.x)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg10.x[i] <- mean(defptsvec.x[max((length(ptsvec.x)-9),1):length(ptsvec.x)],na.rm = TRUE)
  seasondata$OppPtsPerPossAvg5.x[i] <- mean(ptsvec.x[max((length(ptsvec.x)-4),1):length(ptsvec.x)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg5.x[i] <- mean(defptsvec.x[max((length(ptsvec.x)-4),1):length(ptsvec.x)],na.rm = TRUE)
  
  ptsvec.y <- c(0)
  defptsvec.y <- c(0)
  for (j in 1:length(oppvec.y)) {
    ptsvec.y[j] <- 100*((sum(seasondata$PTS.x[(seasondata$Team.x==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                           sum(seasondata$PTS.y[(seasondata$Team.y==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                                                                                                                                                                  sum(seasondata$Poss[(seasondata$Team.y==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
    defptsvec.y[j] <- 100*((sum(seasondata$PTS.y[(seasondata$Team.x==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                              sum(seasondata$PTS.x[(seasondata$Team.y==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]))/(sum(seasondata$Poss[(seasondata$Team.x==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])]) +
                                                                                                                                                                     sum(seasondata$Poss[(seasondata$Team.y==oppvec.y[j]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])])))
    
  }
  
  seasondata$OppPtsPerPossAvg.y[i] <- mean(ptsvec.y,na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg.y[i] <- mean(defptsvec.y,na.rm = TRUE)
  seasondata$OppPtsPerPossAvg20.y[i] <- mean(ptsvec.y[max((length(ptsvec.y)-19),1):length(ptsvec.y)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg20.y[i] <- mean(defptsvec.y[max((length(ptsvec.y)-19),1):length(ptsvec.y)],na.rm = TRUE)
  seasondata$OppPtsPerPossAvg10.y[i] <- mean(ptsvec.y[max((length(ptsvec.y)-9),1):length(ptsvec.y)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg10.y[i] <- mean(defptsvec.y[max((length(ptsvec.y)-9),1):length(ptsvec.y)],na.rm = TRUE)
  seasondata$OppPtsPerPossAvg5.y[i] <- mean(ptsvec.y[max((length(ptsvec.y)-4),1):length(ptsvec.y)],na.rm = TRUE)
  seasondata$OppDefPtsPerPossAvg5.y[i] <- mean(defptsvec.y[max((length(ptsvec.y)-4),1):length(ptsvec.y)],na.rm = TRUE)
  
  seasondata$BtoB.x[i] <- ifelse(is_empty(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.x[i]|seasondata$Team.y==seasondata$Team.x[i])&(seasondata$Date==(seasondata$Date[i]-1))]),0,1)
  seasondata$BtoB.y[i] <- ifelse(is_empty(seasondata$Team.x[(seasondata$Team.x==seasondata$Team.y[i]|seasondata$Team.y==seasondata$Team.y[i])&(seasondata$Date==(seasondata$Date[i]-1))]),0,1)
  
}


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
  
  seasondata$OREBPerPoss.x[i] <- 100*((sum(seasondata$OREB.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                       + sum(seasondata$OREB.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$OREBPerPoss.y[i] <- 100*((sum(seasondata$OREB.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                       + sum(seasondata$OREB.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefOREBPerPoss.x[i] <- 100*((sum(seasondata$OREB.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                          + sum(seasondata$OREB.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefOREBPerPoss.y[i] <- 100*((sum(seasondata$OREB.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                          + sum(seasondata$OREB.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DREBPerPoss.x[i] <- 100*((sum(seasondata$DREB.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                       + sum(seasondata$DREB.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DREBPerPoss.y[i] <- 100*((sum(seasondata$DREB.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                       + sum(seasondata$DREB.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                        + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefDREBPerPoss.x[i] <- 100*((sum(seasondata$DREB.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                          + sum(seasondata$DREB.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefDREBPerPoss.y[i] <- 100*((sum(seasondata$DREB.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                          + sum(seasondata$DREB.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                           + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$ASTPerPoss.x[i] <- 100*((sum(seasondata$AST.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$AST.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$ASTPerPoss.y[i] <- 100*((sum(seasondata$AST.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$AST.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefASTPerPoss.x[i] <- 100*((sum(seasondata$AST.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$AST.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefASTPerPoss.y[i] <- 100*((sum(seasondata$AST.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$AST.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$STLPerPoss.x[i] <- 100*((sum(seasondata$STL.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$STL.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$STLPerPoss.y[i] <- 100*((sum(seasondata$STL.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$STL.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefSTLPerPoss.x[i] <- 100*((sum(seasondata$STL.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$STL.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefSTLPerPoss.y[i] <- 100*((sum(seasondata$STL.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$STL.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$BLKPerPoss.x[i] <- 100*((sum(seasondata$BLK.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$BLK.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$BLKPerPoss.y[i] <- 100*((sum(seasondata$BLK.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$BLK.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefBLKPerPoss.x[i] <- 100*((sum(seasondata$BLK.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$BLK.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefBLKPerPoss.y[i] <- 100*((sum(seasondata$BLK.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$BLK.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$TOPerPoss.x[i] <- 100*((sum(seasondata$TO.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                     + sum(seasondata$TO.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$TOPerPoss.y[i] <- 100*((sum(seasondata$TO.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                     + sum(seasondata$TO.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                    + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefTOPerPoss.x[i] <- 100*((sum(seasondata$TO.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$TO.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                       + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefTOPerPoss.y[i] <- 100*((sum(seasondata$TO.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$TO.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                       + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$ThrPMPerPoss.x[i] <- 100*((sum(seasondata$`3PM.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`3PM.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$ThrPMPerPoss.y[i] <- 100*((sum(seasondata$`3PM.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`3PM.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefThrPMPerPoss.x[i] <- 100*((sum(seasondata$`3PM.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`3PM.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefThrPMPerPoss.y[i] <- 100*((sum(seasondata$`3PM.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`3PM.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$ThrPAPerPoss.x[i] <- 100*((sum(seasondata$`3PA.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`3PA.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$ThrPAPerPoss.y[i] <- 100*((sum(seasondata$`3PA.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`3PA.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefThrPAPerPoss.x[i] <- 100*((sum(seasondata$`3PA.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`3PA.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefThrPAPerPoss.y[i] <- 100*((sum(seasondata$`3PA.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`3PA.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$TwoPMPerPoss.x[i] <- 100*((sum(seasondata$`2PM.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`2PM.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$TwoPMPerPoss.y[i] <- 100*((sum(seasondata$`2PM.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`2PM.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefTwoPMPerPoss.x[i] <- 100*((sum(seasondata$`2PM.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`2PM.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefTwoPMPerPoss.y[i] <- 100*((sum(seasondata$`2PM.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`2PM.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$TwoPAPerPoss.x[i] <- 100*((sum(seasondata$`2PA.x`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`2PA.y`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$TwoPAPerPoss.y[i] <- 100*((sum(seasondata$`2PA.x`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                        + sum(seasondata$`2PA.y`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                          + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefTwoPAPerPoss.x[i] <- 100*((sum(seasondata$`2PA.y`[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`2PA.x`[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefTwoPAPerPoss.y[i] <- 100*((sum(seasondata$`2PA.y`[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                           + sum(seasondata$`2PA.x`[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                             + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$FTMPerPoss.x[i] <- 100*((sum(seasondata$FTM.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FTM.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$FTMPerPoss.y[i] <- 100*((sum(seasondata$FTM.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FTM.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefFTMPerPoss.x[i] <- 100*((sum(seasondata$FTM.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FTM.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefFTMPerPoss.y[i] <- 100*((sum(seasondata$FTM.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FTM.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$FTAPerPoss.x[i] <- 100*((sum(seasondata$FTA.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FTA.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$FTAPerPoss.y[i] <- 100*((sum(seasondata$FTA.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FTA.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefFTAPerPoss.x[i] <- 100*((sum(seasondata$FTA.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FTA.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefFTAPerPoss.y[i] <- 100*((sum(seasondata$FTA.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FTA.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$FGMPerPoss.x[i] <- 100*((sum(seasondata$FGM.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FGM.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$FGMPerPoss.y[i] <- 100*((sum(seasondata$FGM.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FGM.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefFGMPerPoss.x[i] <- 100*((sum(seasondata$FGM.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FGM.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefFGMPerPoss.y[i] <- 100*((sum(seasondata$FGM.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FGM.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  seasondata$FGAPerPoss.x[i] <- 100*((sum(seasondata$FGA.x[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FGA.y[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$FGAPerPoss.y[i] <- 100*((sum(seasondata$FGA.x[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                      + sum(seasondata$FGA.y[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                      + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  seasondata$DefFGAPerPoss.x[i] <- 100*((sum(seasondata$FGA.y[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FGA.x[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.x[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  seasondata$DefFGAPerPoss.y[i] <- 100*((sum(seasondata$FGA.y[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                         + sum(seasondata$FGA.x[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE))/(sum(seasondata$Poss[(seasondata$Team.x==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE) 
                                                                                                                                                                                         + sum(seasondata$Poss[(seasondata$Team.y==seasondata$Team.y[i]&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i])],na.rm=TRUE)))
  
  
  
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
  
  seasondata$NumGamesWeek.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),(seasondata$Team.y==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),na.rm=TRUE)
  seasondata$NumHomeGamesWeek.x[i] <- sum((seasondata$Team.x==seasondata$Team.x[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),na.rm=TRUE)
  seasondata$NumGamesWeek.y[i] <- sum((seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),(seasondata$Team.y==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),na.rm=TRUE)
  seasondata$NumHomeGamesWeek.y[i] <- sum((seasondata$Team.x==seasondata$Team.y[i])&seasondata$Date<seasondata$Date[i]&seasondata$Season==seasondata$Season[i]&seasondata$Date>=(seasondata$Date[i]-7),na.rm=TRUE)
  
  
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

#fix for all-star stuff
seasondata <- seasondata[seasondata$Team.x!="Team LeBron"&seasondata$Team.x!="Team Giannis",]

save(seasondata, file = paste("allregdata_modelalphaV4.RData", sep="")) #includes future games now
preddata_final <- seasondata[seasondata$Date>=startdate,]
preddata_final <- preddata_final[preddata_final$Date>=today,]
preddata_final <- preddata_final[preddata_final$Team.x!="TBD "&preddata_final$Team.y!="TBD ",]
preddata_final <- preddata_final[preddata_final$Team.x!="TBD"&preddata_final$Team.y!="TBD",]
save(preddata_final, file = paste("testdata_modelalphaV4.RData", sep=""))

#Now, create data with previewed games and predicted lines
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
pred_overunder <- pred_totalpoints*(pred_poss/100)
version <- "AlphaV4"
if(length(preddata_final$Date)!=0) {
  predictionsV4 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)
  predictions <- predictionsV4
  save(predictions, file = paste("predictions.RData", sep=""))
} else {
  print("No Games in Next 3 days -- dont run")
}


