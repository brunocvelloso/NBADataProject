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
#setwd(working)


load("modelbetaV1_dataformerge.RData")
load("allregdata_modelgammaV2.RData")
load("modelbetaV1_teamlist.RData")

ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
startdate <- today

seasondata <- seasondata %>%
  dplyr::group_by(Season) %>%
  dplyr::mutate(Box1SeasonAvg = (cumsum(Box1TeamAvg.x)+cumsum(Box1TeamAvg.y))/(cumsum(ones)+cumsum(ones)),
         Box2SeasonAvg = (cumsum(Box2TeamAvg.x)+cumsum(Box2TeamAvg.y))/(cumsum(ones)+cumsum(ones)),
         Box3SeasonAvg = (cumsum(Box3TeamAvg.x)+cumsum(Box3TeamAvg.y))/(cumsum(ones)+cumsum(ones)),
         OnOffSeasonAvg = (cumsum(OnOffTeamAvg.x)+cumsum(OnOffTeamAvg.y))/(cumsum(ones)+cumsum(ones)))


seasondata <- seasondata[seasondata$Team.x!="TBD "&seasondata$Team.y!="TBD ",]
seasondata <- seasondata[seasondata$Team.x!="TBD"&seasondata$Team.y!="TBD",]
preddata_final <- seasondata[seasondata$Date>=today,]
seasondata <- seasondata[seasondata$Date<(today),]
  
# modelbeta1a <- lm(formula = HomeMargin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                   + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                     PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                   + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                     PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                   + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                   + RegularSeason + Playoffs +
#                     BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                   + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                     + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                   + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                   + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                   + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                     NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                   + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                   + Box1Adjustment.x + Box1AdjChg.x
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box1AdjChg.y
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
#                   + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                   + OppBox1Adj.x + OppBox1Adj20.x
#                   + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x
#                   + OppWinPct.y + OppBox1Adj.y + OppBox1Adj20.y
#                   + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
#                     TeamQualORPM.x + TeamQualDRPM.x +
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                     TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y, data = seasondata)
# summary(modelbeta1a)$coef
# modelbeta1b <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
#                   + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
#                   + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
#                   + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                     + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y 
#                   + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y 
#                   + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y 
#                   + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x 
#                   + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
#                   + Box1Adjustment.x + Box3Adjustment.x + Box1AdjChg.x 
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box3Adjustment.y + Box1AdjChg.y 
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
#                   + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
#                     +  PPXPace9 + PPXPace10 + PPXPace11 + PPXPace12 + PPXPace13 + PPXPace14 + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x + OppWinPct20.x 
#                   + OppWinPct10.x + OppWinPct5.x + OppBox1Adj.x + OppBox1Adj20.x 
#                   + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x 
#                   + OppWinPct.y + OppWinPct20.y + OppWinPct10.y + OppWinPct5.y + OppBox1Adj.y + OppBox1Adj20.y 
#                   + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
#                     TeamQualORPM.x + TeamQualDRPM.x + 
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*RegularSeason + TeamQualDRPM.x*RegularSeason +
#                     TeamQualORPM.y*RegularSeason + TeamQualDRPM.y*RegularSeason +
#                     PtsPerPossAvg2.x*RegularSeason + PtsPerPossAvg2.y*RegularSeason + DefPtsPerPossAvg2.x*RegularSeason + DefPtsPerPossAvg2.y*RegularSeason
#                   + PPXPace1*RegularSeason + PPXPace2*RegularSeason + PPXPace3*RegularSeason + PPXPace4*RegularSeason + WinPct20.x*RegularSeason +
#                     WinPct10.x*RegularSeason + WinPct20.y*RegularSeason + WinPct10.y*RegularSeason +
#                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y + PlayoffExpMinPG.x + PlayoffExpMinPG.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + PlayoffExpMinPG.x*RegularSeason + PlayoffExpMinPG.y*RegularSeason, data = seasondata)
# summary(modelbeta1b)$coef
# modelbeta1c <- lm(formula = TotalPoints ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                   + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                     PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                   + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20
#                   + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                   + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 + GameDummy2040 + GameDummy4060 + GameDummy6082
#                   + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + PointsTotal220.x + PointsTotal220.y + DefPointsTotal220.x + DefPointsTotal220.y + OppTotalPoints220.x
#                   + OppTotalPoints220.y + OppDefTotalPoints220.x + OppDefTotalPoints220.y +
#                     PointsTotal210.x + PointsTotal210.y + DefPointsTotal210.x + DefPointsTotal210.y + OppTotalPoints210.x
#                   + OppTotalPoints210.y + OppDefTotalPoints210.x + OppDefTotalPoints210.y +
#                     PointsTotal25.x + PointsTotal25.y + DefPointsTotal25.x + DefPointsTotal25.y + OppTotalPoints25.x
#                   + OppTotalPoints25.y + OppDefTotalPoints25.x + OppDefTotalPoints25.y
#                   + PointsTotal220.x*GameDummyLess20 + PointsTotal220.y*GameDummyLess20 + DefPointsTotal220.x*GameDummyLess20 + DefPointsTotal220.y*GameDummyLess20 + OppTotalPoints220.x*GameDummyLess20
#                   + OppTotalPoints220.y*GameDummyLess20 + OppDefTotalPoints220.x*GameDummyLess20 + OppDefTotalPoints220.y*GameDummyLess20 +
#                     PointsTotal210.x*GameDummyLess20 + PointsTotal210.y*GameDummyLess20 + DefPointsTotal210.x*GameDummyLess20 + DefPointsTotal210.y*GameDummyLess20 + OppTotalPoints210.x*GameDummyLess20
#                   + OppTotalPoints210.y*GameDummyLess20 + OppDefTotalPoints210.x*GameDummyLess20 + OppDefTotalPoints210.y*GameDummyLess20 +
#                     PointsTotal25.x*GameDummyLess20 + PointsTotal25.y*GameDummyLess20 + DefPointsTotal25.x*GameDummyLess20 + DefPointsTotal25.y*GameDummyLess20 + OppTotalPoints25.x*GameDummyLess20
#                   + OppTotalPoints25.y*GameDummyLess20 + OppDefTotalPoints25.x*GameDummyLess20 + OppDefTotalPoints25.y*GameDummyLess20
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                   + Box1Adjustment.x + Box1AdjChg.x
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box1AdjChg.y
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y +
#                     HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y +
#                     TeamQualORPM.x + TeamQualDRPM.x +
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                     TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                   EarlyHomeGames
#                   #  OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
#                   + MidSeriesGames + GameSix
#                   + GameSeven + SweepProspect + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y
#                   + GameCountAvg + GameCountAvgSq+GameCountAvg*RegularSeason+ GameCountAvgSq*RegularSeason
#                   +SeasTotPtAvg30
#                   +SeasTotPtAvg60+SeasTotOverAvg
#                   # +SeasTotPtAvg30*RegularSeason
#                   # +SeasTotPtAvg60*RegularSeason+SeasTotOverAvg*RegularSeason+PointsTotal2.x*RegularSeason + PointsTotal2.y*RegularSeason + DefPointsTotal2.x*RegularSeason + DefPointsTotal2.y*RegularSeason + OppTotalPoints2.x*RegularSeason
#                   # + OppTotalPoints2.y*RegularSeason + OppDefTotalPoints2.x*RegularSeason + OppDefTotalPoints2.y*RegularSeason
#                   , data = seasondata)
# summary(modelbeta1c)$coef
# modelbeta1d <- glm(formula = HomeWin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                    + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                      PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                    + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                      PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                    + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                    + RegularSeason + Playoffs +
#                      BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                    + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                    + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                      + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                    + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                    + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                    + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                      NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                    + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                    + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                    + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                    + Box1Adjustment.x + Box1AdjChg.x
#                    + OnOffAdjustment.y
#                    + Box1Adjustment.y + Box1AdjChg.y
#                    + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
#                    + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                    + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                    + OppWinPct.y +
#                      TeamQualORPM.x + TeamQualDRPM.x +
#                      TeamQualORPM.y + TeamQualDRPM.y +
#                      TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                      TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                      HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                    + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                    + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y, data = seasondata, family = binomial(link = "logit"))
# summary(modelbeta1d)$coef
# 
# #Now, create data with previewed games and predicted lines for model alphs, version 2
# pred_margin <- predict(modelbeta1a,newdata=preddata_final)
# pred_poss <- predict(modelbeta1b,newdata=preddata_final)
# pred_totalpoints <- predict(modelbeta1c,newdata=preddata_final)
# pred_homewin <- predict(modelbeta1d,newdata=preddata_final,type = "response")
# date <- preddata_final$Date
# description <- preddata_final$GameDescription
# result_margin <- NA
# result_poss <- NA
# pred_line <- -1*pred_margin
# result_line <- NA
# pred_overunder <- pred_totalpoints
# version <- "BetaV4"
# predictionsBetaV4 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)


# load("predictionsAlphaV4.RData")
# load("predictionsBetaV3.RData")
# load("predictions.RData")
# 
# # predictions <- predictions[,c(1,2,3,4,5,6,7,8)]
# predictions$cutoff <- NA
# predictionsAlphaV4$cutoff <- NA
# predictionsBetaV3$cutoff <- NA
# predictionsBetaV4$cutoff <- NA
# 
# predictions_final <- rbind(predictions,predictionsAlphaV4,predictionsBetaV3,predictionsBetaV4)
# 
# save(predictions_final, file = paste("predictions_final.RData", sep=""))

load("OldLinesForGammaV2.RData")
load("todayslines.RData")
load("pointsbetlines.RData")

dfdata2 <- dfdata2[dfdata2$Date<today,]

###manually edit lines if necessary
# lines$overunder[9] <- 216 #philly at tor
# lines$overunder[10] <- 227 #charlotte at new york
# save(lines,file="todayslines.RData")
# lines$date[1] <- as.Date("2022-06-16")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Golden State Warriors at Dallas Mavericks"
# lines$awayteam[nrow(lines)] <- "Golden State Warriors"
# lines$hometeam[nrow(lines)] <- "Dallas Mavericks"
# lines$homeline[nrow(lines)] <- -1
# lines$moneylinehome[nrow(lines)] <- -112
# lines$moneylineaway[nrow(lines)] <- -104
# lines$overunder[nrow(lines)] <- 215.5
# lines$date[nrow(lines)] <- as.Date("2022-05-24")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Boston Celtics at Miami Heat"
# lines$awayteam[nrow(lines)] <- "Boston Celtics"
# lines$hometeam[nrow(lines)] <- "Miami Heat"
# lines$homeline[nrow(lines)] <- 1
# lines$moneylinehome[nrow(lines)] <- -104
# lines$moneylineaway[nrow(lines)] <- -116
# lines$overunder[nrow(lines)] <- 203.5
# lines$date[nrow(lines)] <- as.Date("2022-05-25")
# lines <- lines[2:3,]
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Memphis Grizzlies at Minnesota Timberwolves"
# lines$awayteam[nrow(lines)] <- "Memphis Grizzlies"
# lines$hometeam[nrow(lines)] <- "Minnesota Timberwolves"
# lines$homeline[nrow(lines)] <- 2
# lines$moneylinehome[nrow(lines)] <- 110
# lines$moneylineaway[nrow(lines)] <- -130
# lines$overunder[nrow(lines)] <- 228
# lines$date[nrow(lines)] <- as.Date("2022-04-29")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Phoenix Suns at Dallas Mavericks"
# lines$awayteam[nrow(lines)] <- "Phoenix Suns"
# lines$hometeam[nrow(lines)] <- "Dallas Mavericks"
# lines$homeline[nrow(lines)] <- 1
# lines$moneylinehome[nrow(lines)] <- -102
# lines$moneylineaway[nrow(lines)] <- -116
# lines$overunder[nrow(lines)] <- 219.5
# lines$date[nrow(lines)] <- as.Date("2022-05-06")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Memphis Grizzlies at Golden State Warriors"
# lines$awayteam[nrow(lines)] <- "Memphis Grizzlies"
# lines$hometeam[nrow(lines)] <- "Golden State Warriors"
# lines$homeline[nrow(lines)] <- -9
# lines$moneylinehome[nrow(lines)] <- -400
# lines$moneylineaway[nrow(lines)] <- 315
# lines$overunder[nrow(lines)] <- 228
# lines$date[nrow(lines)] <- as.Date("2022-05-09")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Phoenix Suns at New Orleans Pelicans"
# lines$awayteam[nrow(lines)] <- "Phoenix Suns"
# lines$hometeam[nrow(lines)] <- "New Orleans Pelicans"
# lines$homeline[nrow(lines)] <- 3
# lines$moneylinehome[nrow(lines)] <- 126
# lines$moneylineaway[nrow(lines)] <- -148
# lines$overunder[nrow(lines)] <- 214.5
# lines$date[nrow(lines)] <- as.Date("2022-04-28")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Philadelphia 76ers at Toronto Raptors"
# lines$awayteam[nrow(lines)] <- "Philadelphia 76ers"
# lines$hometeam[nrow(lines)] <- "Toronto Raptors"
# lines$homeline[nrow(lines)] <- 1.5
# lines$moneylinehome[nrow(lines)] <- 102
# lines$moneylineaway[nrow(lines)] <- -120
# lines$overunder[nrow(lines)] <- 209.5
# lines$date[nrow(lines)] <- as.Date("2022-04-28")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Chicago Bulls at Milwaukee Bucks"
# lines$awayteam[nrow(lines)] <- "Chicago Bulls"
# lines$hometeam[nrow(lines)] <- "Milwaukee Bucks"
# lines$homeline[nrow(lines)] <- -10
# lines$moneylinehome[nrow(lines)] <- -480
# lines$moneylineaway[nrow(lines)] <- 370
# lines$overunder[nrow(lines)] <- 218.5
# lines$date[nrow(lines)] <- as.Date("2022-04-27")
# lines$date[5] <- as.Date("2022-04-19")
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Washington Wizards at Charlotte Hornets"
# lines$awayteam[nrow(lines)] <- "Washington Wizards"
# lines$hometeam[nrow(lines)] <- "Charlotte Hornets"
# lines$homeline[nrow(lines)] <- -11
# lines$moneylinehome[nrow(lines)] <- -560
# lines$moneylineaway[nrow(lines)] <- 420
# lines$overunder[nrow(lines)] <- 231
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Toronto Raptors at New York Knicks"
# lines$awayteam[nrow(lines)] <- "Toronto Raptors"
# lines$hometeam[nrow(lines)] <- "New York Knicks"
# lines$homeline[nrow(lines)] <- 5
# lines$moneylinehome[nrow(lines)] <- 172
# lines$moneylineaway[nrow(lines)] <- -205
# lines$overunder[nrow(lines)] <- 227
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Detroit Pistons at Philadelphia 76ers"
# lines$awayteam[nrow(lines)] <- "Detroit Pistons"
# lines$hometeam[nrow(lines)] <- "Philadelphia 76ers"
# lines$homeline[nrow(lines)] <- -15
# lines$moneylinehome[nrow(lines)] <- -1350
# lines$moneylineaway[nrow(lines)] <- 810
# lines$overunder[nrow(lines)] <- 232.5
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Chicago Bulls at Minnesota Timberwolves"
# lines$awayteam[nrow(lines)] <- "Chicago Bulls"
# lines$hometeam[nrow(lines)] <- "Minnesota Timberwolves"
# lines$homeline[nrow(lines)] <- -6.5
# lines$moneylinehome[nrow(lines)] <- -270
# lines$moneylineaway[nrow(lines)] <- 220
# lines$overunder[nrow(lines)] <- 234.5
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Oklahoma City Thunder at LA Clippers"
# lines$awayteam[nrow(lines)] <- "Oklahoma City Thunder"
# lines$hometeam[nrow(lines)] <- "LA Clippers"
# lines$homeline[nrow(lines)] <- -10
# lines$moneylinehome[nrow(lines)] <- -500
# lines$moneylineaway[nrow(lines)] <- 385
# lines$overunder[nrow(lines)] <- 221
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Sacramento Kings at Phoenix Suns"
# lines$awayteam[nrow(lines)] <- "Sacramento Kings"
# lines$hometeam[nrow(lines)] <- "Phoenix Suns"
# lines$homeline[nrow(lines)] <- -13
# lines$moneylinehome[nrow(lines)] <- -850
# lines$moneylineaway[nrow(lines)] <- 590
# lines$overunder[nrow(lines)] <- 235
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "San Antonio Spurs at Dallas Mavericks"
# lines$awayteam[nrow(lines)] <- "San Antonio Spurs"
# lines$hometeam[nrow(lines)] <- "Dallas Mavericks"
# lines$homeline[nrow(lines)] <- -9.5
# lines$moneylinehome[nrow(lines)] <- -450
# lines$moneylineaway[nrow(lines)] <- 350
# lines$overunder[nrow(lines)] <- 226
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Utah Jazz at Portland Trail Blazers"
# lines$awayteam[nrow(lines)] <- "Utah Jazz"
# lines$hometeam[nrow(lines)] <- "Portland Trail Blazers"
# lines$homeline[nrow(lines)] <- 19.5
# lines$moneylinehome[nrow(lines)] <- 1500
# lines$moneylineaway[nrow(lines)] <- -4000
# lines$overunder[nrow(lines)] <- 232
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Los Angeles Lakers at Denver Nuggets"
# lines$awayteam[nrow(lines)] <- "Los Angeles Lakers"
# lines$hometeam[nrow(lines)] <- "Denver Nuggets"
# lines$homeline[nrow(lines)] <- -7.5
# lines$moneylinehome[nrow(lines)] <- -320
# lines$moneylineaway[nrow(lines)] <- 260
# lines$overunder[nrow(lines)] <- 232
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Golden State Warriors at New Orleans Pelicans"
# lines$awayteam[nrow(lines)] <- "Golden State Warriors"
# lines$hometeam[nrow(lines)] <- "New Orleans Pelicans"
# lines$homeline[nrow(lines)] <- 3.5
# lines$moneylinehome[nrow(lines)] <- 136
# lines$moneylineaway[nrow(lines)] <- -162
# lines$overunder[nrow(lines)] <- 224
# lines <- rbind(lines,lines[nrow(lines),])
# lines$description[nrow(lines)] <- "Boston Celtics at Memphis Grizzlies"
# lines$awayteam[nrow(lines)] <- "Boston Celtics"
# lines$hometeam[nrow(lines)] <- "Memphis Grizzlies"
# lines$homeline[nrow(lines)] <- 1.5
# lines$moneylinehome[nrow(lines)] <- 100
# lines$moneylineaway[nrow(lines)] <- -118
# lines$overunder[nrow(lines)] <- 227.5

lines <- lines[,c(1:8)]
lines_all <- lines_all[lines_all$date<today,]
lines_all <- rbind(lines_all,lines)
lines_all <- lines_all[!is.na(lines_all$description),]
lines_all <- distinct(lines_all,description,date,.keep_all=TRUE)
save(lines_all,file="pointsbetlines.RData")
lines_all <- dplyr::rename(lines_all,ML.x=moneylinehome,ML.y=moneylineaway)
lines_all$Fav.x <- ifelse(lines_all$ML.x<(-100),1,0)
lines_all$Fav.y <- ifelse(lines_all$ML.y<(-100),1,0)
lines_all$HomeWinVegas.x  <- (lines_all$Fav.x*(-1)*(lines_all$ML.x/(-1*lines_all$ML.x+100))+(1-lines_all$Fav.x)*(100/(lines_all$ML.x+100)))
lines_all$HomeWinVegas.y <- (1-(lines_all$Fav.y*(-1)*(lines_all$ML.y/(-1*lines_all$ML.y+100))+(1-lines_all$Fav.y)*(100/(lines_all$ML.y+100))))
lines_all$HomeWinVegas.x[is.nan(lines_all$HomeWinVegas.x)]  <- 0.5
lines_all$HomeWinVegas.y[is.nan(lines_all$HomeWinVegas.y)]  <- 0.5
lines_all$HomeWinVegas <- (lines_all$HomeWinVegas.x + lines_all$HomeWinVegas.y)/2
lines_all$Line_Close <- lines_all$homeline
lines_all$OverUnder_Close <- NA
lines_all <- dplyr::select(lines_all,-c(awayteam,hometeam,Fav.x,Fav.y))
lines_all <- dplyr::rename(lines_all,GameDescription=description,Date=date,Line_Open=homeline,OverUnder_Open=overunder)
dfdata2 <- rbind(dfdata2,lines_all)
dfdata2$GameDescription <- gsub("\u00A0", " ", dfdata2$GameDescription, fixed = TRUE)
dfdata2 <- distinct(dfdata2,GameDescription,Date,.keep_all=TRUE)
save(dfdata2,file="OldLinesForGammaV2.RData")

# predictions_final <-  predictions_final %>% left_join(lines,by = c("description","date")) %>% ungroup()
# predictions_final <- dplyr::select(predictions_final,description,date,pred_overunder,pred_homewin,pred_line,homeline,moneylinehome,moneylineaway,overunder,everything())
# 
# save(predictions_final, file = paste("predictions_final.RData", sep=""))

dfdata2$GameDescription <- gsub("\u00A0", " ", dfdata2$GameDescription, fixed = TRUE)
seasondata <- seasondata %>% left_join(dfdata2,by = c("GameDescription","Date")) %>% ungroup()
preddata_final <- preddata_final %>% left_join(dfdata2,by = c("GameDescription","Date")) %>% ungroup()

preddata_final$HomeWinVegas.x[preddata_final$ML.x==-100&!is.na(preddata_final$ML.x)] <- .5
preddata_final$HomeWinVegas.y[preddata_final$ML.y==-100&!is.na(preddata_final$ML.y)] <- .5
seasondata$HomeWinVegas.x[seasondata$ML.x==-100&!is.na(seasondata$ML.x)] <- .5
seasondata$HomeWinVegas.y[seasondata$ML.y==-100&!is.na(seasondata$ML.y)] <- .5
preddata_final$HomeWinVegas[preddata_final$ML.x==-100&!is.na(preddata_final$ML.x)] <- (preddata_final$HomeWinVegas.x[preddata_final$ML.x==-100&!is.na(preddata_final$ML.x)] + preddata_final$HomeWinVegas.y[preddata_final$ML.x==-100&!is.na(preddata_final$ML.x)])/2
preddata_final$HomeWinVegas[preddata_final$ML.y==-100&!is.na(preddata_final$ML.y)] <- (preddata_final$HomeWinVegas.x[preddata_final$ML.y==-100&!is.na(preddata_final$ML.y)] + preddata_final$HomeWinVegas.y[preddata_final$ML.y==-100&!is.na(preddata_final$ML.y)])/2
seasondata$HomeWinVegas <- ifelse(seasondata$ML.x==-100|seasondata$ML.y==-100,(seasondata$HomeWinVegas.x + seasondata$HomeWinVegas.y)/2,seasondata$HomeWinVegas)

seasondata <- rbind(seasondata,preddata_final)
seasondata$LineFact <- as.factor(paste0(seasondata$Line_Close,seasondata$Season,sep=""))
model_impute <- lm(formula = HomeWinVegas ~ factor(LineFact), data = seasondata)
seasondata$VegasImpute[!is.na(seasondata$Line_Close)] <- predict(model_impute,seasondata[!is.na(seasondata$Line_Close),])
seasondata$test1 <- abs(seasondata$VegasImpute-seasondata$HomeWinVegas)
seasondata$MLError <- ifelse(seasondata$test1>.1,1,0)
seasondata$HomeWinVegas[seasondata$MLError==1] <- NA
model_impute <- lm(formula = HomeWinVegas ~ factor(LineFact), data = seasondata)
seasondata$VegasImpute[!is.na(seasondata$Line_Close)] <- predict(model_impute,seasondata[!is.na(seasondata$Line_Close),])
seasondata$LineFact2 <- seasondata$LineFact
seasondata$LineFact <- as.factor(paste0(seasondata$Line_Open,seasondata$Season,sep=""))
id <- which(!(seasondata$LineFact %in% levels(seasondata$LineFact2)))
seasondata$LineFact[id] <- NA
seasondata$VegasImputeOpen[!is.na(seasondata$Line_Open)] <- predict(model_impute,seasondata[!is.na(seasondata$Line_Open),])
seasondata$HomeWinVegas_Open[seasondata$Date<"2021-12-07"] <- ifelse(seasondata$Line_Close[seasondata$Date<"2021-12-07"]==seasondata$Line_Open[seasondata$Date<"2021-12-07"],seasondata$HomeWinVegas[seasondata$Date<"2021-12-07"],seasondata$VegasImputeOpen[seasondata$Date<"2021-12-07"])
seasondata$HomeWinVegas_Open[seasondata$Date>="2021-12-07"] <- seasondata$HomeWinVegas[seasondata$Date>="2021-12-07"]

preddata_final$HomeWinVegas_Open <- preddata_final$HomeWinVegas


#below is for when i do alphav5 with the lines incorporated in model

# modelalpha5a <- lm(formula = HomeMargin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                    + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                      PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                    + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                      PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                    + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                    + RegularSeason + Playoffs +
#                      BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                    + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                    + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                      + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                    + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                    + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                    + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                      NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                    + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                    + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                    + DistDummy6 + DistDummy7
#                    + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                    + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                    + OppWinPct.y +
#                      HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                    + Tankathon.x + Tankathon.y + NumGamesWeek.x
#                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y + Line_Open, data = seasondata)
# summary(modelalpha5a)$coef
# modelalpha5b <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y
#                   + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y
#                   + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                   + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                     + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y
#                   + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y
#                   + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y
#                   + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x
#                   + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                   + Box1Adjustment.x + Box3Adjustment.x + Box1AdjChg.x
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box3Adjustment.y + Box1AdjChg.y
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
#                   + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 +
#                     +  PPXPace9 + PPXPace10 + PPXPace11 + PPXPace12 + PPXPace13 + PPXPace14 + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x + OppWinPct20.x
#                   + OppWinPct10.x + OppWinPct5.x + OppBox1Adj.x + OppBox1Adj20.x
#                   + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x
#                   + OppWinPct.y + OppWinPct20.y + OppWinPct10.y + OppWinPct5.y + OppBox1Adj.y + OppBox1Adj20.y
#                   + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
#                     TeamQualORPM.x + TeamQualDRPM.x +
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*RegularSeason + TeamQualDRPM.x*RegularSeason +
#                     TeamQualORPM.y*RegularSeason + TeamQualDRPM.y*RegularSeason +
#                     PtsPerPossAvg2.x*RegularSeason + PtsPerPossAvg2.y*RegularSeason + DefPtsPerPossAvg2.x*RegularSeason + DefPtsPerPossAvg2.y*RegularSeason
#                   + PPXPace1*RegularSeason + PPXPace2*RegularSeason + PPXPace3*RegularSeason + PPXPace4*RegularSeason + WinPct20.x*RegularSeason +
#                     WinPct10.x*RegularSeason + WinPct20.y*RegularSeason + WinPct10.y*RegularSeason +
#                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y + PlayoffExpMinPG.x + PlayoffExpMinPG.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + PlayoffExpMinPG.x*RegularSeason + PlayoffExpMinPG.y*RegularSeason, data = seasondata)
# summary(modelalpha5b)$coef
# modelalpha5c <- lm(formula = TotalPoints ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                    + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                      PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                    + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20
#                    + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                    + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                    + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 + GameDummy2040 + GameDummy4060 + GameDummy6082
#                    + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + PointsTotal220.x + PointsTotal220.y + DefPointsTotal220.x + DefPointsTotal220.y + OppTotalPoints220.x
#                    + OppTotalPoints220.y + OppDefTotalPoints220.x + OppDefTotalPoints220.y +
#                      PointsTotal210.x + PointsTotal210.y + DefPointsTotal210.x + DefPointsTotal210.y + OppTotalPoints210.x
#                    + OppTotalPoints210.y + OppDefTotalPoints210.x + OppDefTotalPoints210.y +
#                      PointsTotal25.x + PointsTotal25.y + DefPointsTotal25.x + DefPointsTotal25.y + OppTotalPoints25.x
#                    + OppTotalPoints25.y + OppDefTotalPoints25.x + OppDefTotalPoints25.y
#                    + PointsTotal220.x*GameDummyLess20 + PointsTotal220.y*GameDummyLess20 + DefPointsTotal220.x*GameDummyLess20 + DefPointsTotal220.y*GameDummyLess20 + OppTotalPoints220.x*GameDummyLess20
#                    + OppTotalPoints220.y*GameDummyLess20 + OppDefTotalPoints220.x*GameDummyLess20 + OppDefTotalPoints220.y*GameDummyLess20 +
#                      PointsTotal210.x*GameDummyLess20 + PointsTotal210.y*GameDummyLess20 + DefPointsTotal210.x*GameDummyLess20 + DefPointsTotal210.y*GameDummyLess20 + OppTotalPoints210.x*GameDummyLess20
#                    + OppTotalPoints210.y*GameDummyLess20 + OppDefTotalPoints210.x*GameDummyLess20 + OppDefTotalPoints210.y*GameDummyLess20 +
#                      PointsTotal25.x*GameDummyLess20 + PointsTotal25.y*GameDummyLess20 + DefPointsTotal25.x*GameDummyLess20 + DefPointsTotal25.y*GameDummyLess20 + OppTotalPoints25.x*GameDummyLess20
#                    + OppTotalPoints25.y*GameDummyLess20 + OppDefTotalPoints25.x*GameDummyLess20 + OppDefTotalPoints25.y*GameDummyLess20
#                    + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                    + DistDummy6 + DistDummy7 
#                    + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                    + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y +
#                     EarlyHomeGames
#                    #  OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
#                    + MidSeriesGames + GameSix
#                    + GameSeven + SweepProspect + Tankathon.x + Tankathon.y + NumGamesWeek.x
#                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y
#                    + GameCountAvg + GameCountAvgSq+GameCountAvg*RegularSeason+ GameCountAvgSq*RegularSeason
#                    +SeasTotPtAvg30
#                    +SeasTotPtAvg60+SeasTotOverAvg
#                    # +SeasTotPtAvg30*RegularSeason
#                    # +SeasTotPtAvg60*RegularSeason+SeasTotOverAvg*RegularSeason+PointsTotal2.x*RegularSeason + PointsTotal2.y*RegularSeason + DefPointsTotal2.x*RegularSeason + DefPointsTotal2.y*RegularSeason + OppTotalPoints2.x*RegularSeason
#                    # + OppTotalPoints2.y*RegularSeason + OppDefTotalPoints2.x*RegularSeason + OppDefTotalPoints2.y*RegularSeason 
#                    +OverUnder_Open, data = seasondata)
# summary(modelalpha5c)$coef
# modelalpha5d <- glm(formula = HomeWin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                    + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                      PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                    + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                      PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                    + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                    + RegularSeason + Playoffs +
#                      BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                    + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                    + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                      + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                    + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                    + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                    + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                      NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                    + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                    + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                    + DistDummy6 + DistDummy7
#                    + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                    + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                    + OppWinPct.y +
#                      HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                    + Tankathon.x + Tankathon.y + NumGamesWeek.x
#                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y+HomeWinVegas_Open+Line_Open, data = seasondata, family = binomial(link = "logit"))
# summary(modelalpha5d)$coef
# 
# #Now, create data with previewed games and predicted lines for model alpha, version 2
# pred_margin <- predict(modelalpha5a,newdata=preddata_final)
# pred_poss <- predict(modelalpha5b,newdata=preddata_final)
# pred_totalpoints <- predict(modelalpha5c,newdata=preddata_final)
# pred_homewin <- predict(modelalpha5d,newdata=preddata_final,type = "response")
# date <- preddata_final$Date
# description <- preddata_final$GameDescription
# result_margin <- NA
# result_poss <- NA
# pred_line <- -1*pred_margin
# result_line <- NA
# pred_overunder <- pred_totalpoints
# version <- "AlphaV5"
# predictionsAlphaV5 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)
# 
# modelbeta1a <- lm(formula = HomeMargin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                   + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                     PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                   + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                     PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                   + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                   + RegularSeason + Playoffs +
#                     BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                   + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                     + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                   + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                   + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                   + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                     NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                   + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                   + Box1Adjustment.x + Box1AdjChg.x
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box1AdjChg.y
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
#                   + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                   + OppBox1Adj.x + OppBox1Adj20.x
#                   + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x
#                   + OppWinPct.y + OppBox1Adj.y + OppBox1Adj20.y
#                   + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
#                     TeamQualORPM.x + TeamQualDRPM.x +
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                     TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y+Line_Open, data = seasondata)
# summary(modelbeta1a)$coef
# modelbeta1b <- lm(formula = Poss ~ PtsPerPossAvg2.x + PtsPerPossAvg2.y + DefPtsPerPossAvg2.x + DefPtsPerPossAvg2.y 
#                   + OppPtsPerPossAvg2.x + OppPtsPerPossAvg2.y + OppDefPtsPerPossAvg2.x + OppDefPtsPerPossAvg2.y 
#                   + GameCountAvg + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic 
#                   + PrevSeasonRating.x + PrevSeasonRating.y + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y) 
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                     + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + WinPct2.x + WinPct20.x + WinPct10.x + WinPct5.x + WinPct2.y 
#                   + WinPct20.y + WinPct10.y + WinPct5.y + AdjNetRating20.x + AdjNetRating20.y + AdjNetRating10.x + AdjNetRating10.y 
#                   + AdjNetRating5.x + AdjNetRating5.y + WinPctX20.x + WinPct20X20.x + WinPct10X20.x + WinPct5X20.x + WinPctX20.y 
#                   + WinPct20X20.y + WinPct10X20.y + WinPct5X20.y + AdjNetRating20X20.x + AdjNetRating20X20.y + AdjNetRating10X20.x 
#                   + AdjNetRating10X20.y + AdjNetRating5X20.x + AdjNetRating5X20.y
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5 
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x 
#                   + Box1Adjustment.x + Box3Adjustment.x + Box1AdjChg.x 
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box3Adjustment.y + Box1AdjChg.y 
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y 
#                   + PPXPace1 + PPXPace2 + PPXPace3 + PPXPace4 + PPXPace5 + PPXPace6 + PPXPace7 + PPXPace8 + 
#                     +  PPXPace9 + PPXPace10 + PPXPace11 + PPXPace12 + PPXPace13 + PPXPace14 + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x + OppWinPct20.x 
#                   + OppWinPct10.x + OppWinPct5.x + OppBox1Adj.x + OppBox1Adj20.x 
#                   + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x 
#                   + OppWinPct.y + OppWinPct20.y + OppWinPct10.y + OppWinPct5.y + OppBox1Adj.y + OppBox1Adj20.y 
#                   + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
#                     TeamQualORPM.x + TeamQualDRPM.x + 
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*RegularSeason + TeamQualDRPM.x*RegularSeason +
#                     TeamQualORPM.y*RegularSeason + TeamQualDRPM.y*RegularSeason +
#                     PtsPerPossAvg2.x*RegularSeason + PtsPerPossAvg2.y*RegularSeason + DefPtsPerPossAvg2.x*RegularSeason + DefPtsPerPossAvg2.y*RegularSeason
#                   + PPXPace1*RegularSeason + PPXPace2*RegularSeason + PPXPace3*RegularSeason + PPXPace4*RegularSeason + WinPct20.x*RegularSeason +
#                     WinPct10.x*RegularSeason + WinPct20.y*RegularSeason + WinPct10.y*RegularSeason +
#                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y + PlayoffExpMinPG.x + PlayoffExpMinPG.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + PlayoffExpMinPG.x*RegularSeason + PlayoffExpMinPG.y*RegularSeason, data = seasondata)
# summary(modelbeta1b)$coef
# modelbeta1c <- lm(formula = TotalPoints ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                   + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                     PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                   + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20
#                   + RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                   + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 + GameDummy2040 + GameDummy4060 + GameDummy6082
#                   + AdjNetMarSqPlus2 + AdjNetMarSqNeg2 + Year + PointsTotal220.x + PointsTotal220.y + DefPointsTotal220.x + DefPointsTotal220.y + OppTotalPoints220.x
#                   + OppTotalPoints220.y + OppDefTotalPoints220.x + OppDefTotalPoints220.y +
#                     PointsTotal210.x + PointsTotal210.y + DefPointsTotal210.x + DefPointsTotal210.y + OppTotalPoints210.x
#                   + OppTotalPoints210.y + OppDefTotalPoints210.x + OppDefTotalPoints210.y +
#                     PointsTotal25.x + PointsTotal25.y + DefPointsTotal25.x + DefPointsTotal25.y + OppTotalPoints25.x
#                   + OppTotalPoints25.y + OppDefTotalPoints25.x + OppDefTotalPoints25.y
#                   + PointsTotal220.x*GameDummyLess20 + PointsTotal220.y*GameDummyLess20 + DefPointsTotal220.x*GameDummyLess20 + DefPointsTotal220.y*GameDummyLess20 + OppTotalPoints220.x*GameDummyLess20
#                   + OppTotalPoints220.y*GameDummyLess20 + OppDefTotalPoints220.x*GameDummyLess20 + OppDefTotalPoints220.y*GameDummyLess20 +
#                     PointsTotal210.x*GameDummyLess20 + PointsTotal210.y*GameDummyLess20 + DefPointsTotal210.x*GameDummyLess20 + DefPointsTotal210.y*GameDummyLess20 + OppTotalPoints210.x*GameDummyLess20
#                   + OppTotalPoints210.y*GameDummyLess20 + OppDefTotalPoints210.x*GameDummyLess20 + OppDefTotalPoints210.y*GameDummyLess20 +
#                     PointsTotal25.x*GameDummyLess20 + PointsTotal25.y*GameDummyLess20 + DefPointsTotal25.x*GameDummyLess20 + DefPointsTotal25.y*GameDummyLess20 + OppTotalPoints25.x*GameDummyLess20
#                   + OppTotalPoints25.y*GameDummyLess20 + OppDefTotalPoints25.x*GameDummyLess20 + OppDefTotalPoints25.y*GameDummyLess20
#                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                   + Box1Adjustment.x + Box1AdjChg.x
#                   + OnOffAdjustment.y
#                   + Box1Adjustment.y + Box1AdjChg.y
#                   + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y +
#                     HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                   + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y +
#                     TeamQualORPM.x + TeamQualDRPM.x +
#                     TeamQualORPM.y + TeamQualDRPM.y +
#                     TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                     TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                   EarlyHomeGames
#                   #  OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
#                   + MidSeriesGames + GameSix
#                   + GameSeven + SweepProspect + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y
#                   + GameCountAvg + GameCountAvgSq+GameCountAvg*RegularSeason+ GameCountAvgSq*RegularSeason
#                   +SeasTotPtAvg30
#                   +SeasTotPtAvg60+SeasTotOverAvg
#                   # +SeasTotPtAvg30*RegularSeason
#                   # +SeasTotPtAvg60*RegularSeason+SeasTotOverAvg*RegularSeason+PointsTotal2.x*RegularSeason + PointsTotal2.y*RegularSeason + DefPointsTotal2.x*RegularSeason + DefPointsTotal2.y*RegularSeason + OppTotalPoints2.x*RegularSeason
#                   # + OppTotalPoints2.y*RegularSeason + OppDefTotalPoints2.x*RegularSeason + OppDefTotalPoints2.y*RegularSeason
#                   , data = seasondata)
# summary(modelbeta1c)$coef
# modelbeta1d <- glm(formula = HomeWin ~ PointsTotal2.x + PointsTotal2.y + DefPointsTotal2.x + DefPointsTotal2.y + OppTotalPoints2.x
#                    + OppTotalPoints2.y + OppDefTotalPoints2.x + OppDefTotalPoints2.y +
#                      PointsTotal2.x*GameDummyLess20 + PointsTotal2.y*GameDummyLess20 + DefPointsTotal2.x*GameDummyLess20 + DefPointsTotal2.y*GameDummyLess20 + OppTotalPoints2.x*GameDummyLess20
#                    + OppTotalPoints2.y*GameDummyLess20 + OppDefTotalPoints2.x*GameDummyLess20 + OppDefTotalPoints2.y*GameDummyLess20 +
#                      PrevAdjNetMargin.x + PrevAdjNetMargin.y + PrevAdjNetMargin.x*GameDummyLess20 + PrevAdjNetMargin.y*GameDummyLess20
#                    + PrevAdjNetMargin.x*RegularSeason + PrevAdjNetMargin.y*RegularSeason
#                    + RegularSeason + Playoffs +
#                      BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
#                    + Pace2.x + Pace2.y + factor(Team.x) + factor(Team.y)
#                    + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
#                      + AdjNetMarSqPlusv2 + AdjNetMarSqNegv2 + Year + WinPct2.x + WinPct2.y
#                    + NetAdjMarg220.x + NetAdjMarg220.y + NetAdjMarg210.x + NetAdjMarg210.y
#                    + NetAdjMarg25.x + NetAdjMarg25.y + WinPctX20.x + WinPctX20.y
#                    + NetAdjMarg220.x*GameDummyLess20 + NetAdjMarg220.y*GameDummyLess20 +
#                      NetAdjMarg210.x*GameDummyLess20 + NetAdjMarg210.y*GameDummyLess20
#                    + NetAdjMarg25.x*GameDummyLess20 + NetAdjMarg25.y*GameDummyLess20
#                    + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
#                    + DistDummy6 + DistDummy7 + OnOffAdjustment.x
#                    + Box1Adjustment.x + Box1AdjChg.x
#                    + OnOffAdjustment.y
#                    + Box1Adjustment.y + Box1AdjChg.y
#                    + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
#                    + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
#                    + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
#                    + OppWinPct.y +
#                      TeamQualORPM.x + TeamQualDRPM.x +
#                      TeamQualORPM.y + TeamQualDRPM.y +
#                      TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
#                      TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
#                      HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
#                    + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
#                    + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
#                    + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y+Line_Open +HomeWinVegas_Open, data = seasondata, family = binomial(link = "logit"))
# summary(modelbeta1d)$coef
# 
# #Now, create data with previewed games and predicted lines for model alphs, version 2
# pred_margin <- predict(modelbeta1a,newdata=preddata_final)
# pred_poss <- predict(modelbeta1b,newdata=preddata_final)
# pred_totalpoints <- predict(modelbeta1c,newdata=preddata_final)
# pred_homewin <- predict(modelbeta1d,newdata=preddata_final,type = "response")
# date <- preddata_final$Date
# description <- preddata_final$GameDescription
# result_margin <- NA
# result_poss <- NA
# pred_line <- -1*pred_margin
# result_line <- NA
# pred_overunder <- pred_totalpoints
# version <- "BetaV5"
# predictionsBetaV5 <- data.frame(description,date,pred_overunder,pred_homewin,pred_line,result_line,version)
# 
# predictionsAlphaV5$cutoff <- NA
# predictionsBetaV5$cutoff <- NA
# predictions_final <- rbind(predictions,predictionsAlphaV4,predictionsAlphaV5,predictionsBetaV3,predictionsBetaV4,predictionsBetaV5)
# 
# predictions_final <-  predictions_final %>% left_join(lines,by = c("description","date")) %>% ungroup()
# predictions_final <- dplyr::select(predictions_final,description,date,pred_overunder,pred_homewin,pred_line,homeline,moneylinehome,moneylineaway,overunder,everything())
# 
# save(predictions_final, file = paste("predictions_final.RData", sep=""))



# predictionsBetaV42 <- dplyr::rename(predictionsBetaV4,GameDescription=description,Date=date,pred_totalpointsOLD=pred_overunder,pred_marginOLD=pred_line,pred_homewinOLD=pred_homewin)
# predictionsBetaV42 <- dplyr::select(predictionsBetaV42,GameDescription,Date,pred_totalpointsOLD,pred_marginOLD,pred_homewinOLD)
# predictionsBetaV42$pred_marginOLD <- -1*predictionsBetaV42$pred_marginOLD
# results$GameDescription <- gsub("\u00A0", " ", results$GameDescription, fixed = TRUE)
# results <- distinct(results,GameDescription,Date,.keep_all=TRUE)
# results <- rbind(results,predictionsBetaV42)
# save(results, file = paste("oldpredictions.RData", sep=""))
# 
# predictionsAlphaV42 <- dplyr::rename(predictionsAlphaV4,GameDescription=description,Date=date,pred_totalpointsOLDalpha=pred_overunder,pred_marginOLDalpha=pred_line,pred_homewinOLDalpha=pred_homewin)
# predictionsAlphaV42 <- dplyr::select(predictionsAlphaV42,GameDescription,Date,pred_totalpointsOLDalpha,pred_marginOLDalpha,pred_homewinOLDalpha)
# predictionsAlphaV42$pred_marginOLDalpha <- -1*predictionsAlphaV42$pred_marginOLDalpha
# resultsALPHA$GameDescription <- gsub("\u00A0", " ", resultsALPHA$GameDescription, fixed = TRUE)
# resultsALPHA <- distinct(resultsALPHA,GameDescription,Date,.keep_all=TRUE)
# resultsALPHA <- rbind(resultsALPHA,predictionsAlphaV42)
# save(resultsALPHA, file = paste("oldpredictionsALPHA.RData", sep=""))



#fix line errors
seasondata$Line_Open[seasondata$GameDescription=="LA Clippers at Indiana Pacers"&seasondata$Date=="2019-12-09"] <- 1.5
seasondata$Line_Open[seasondata$GameDescription=="Oklahoma City Thunder at Utah Jazz"&seasondata$Date=="2019-12-09"] <- (-9)
seasondata$OverUnder_Open[seasondata$GameDescription=="Chicago Bulls at Utah Jazz"&seasondata$Date=="2016-11-17"] <- 194.5
seasondata$OverUnder_Open[seasondata$GameDescription=="Oklahoma City Thunder at Utah Jazz"&seasondata$Date=="2019-12-09"] <- 211.5
seasondata$OverUnder_Open[seasondata$GameDescription=="LA Clippers at Indiana Pacers"&seasondata$Date=="2019-12-09"] <- 220
seasondata$OverUnder_Open[seasondata$GameDescription=="Dallas Mavericks at Chicago Bulls"&seasondata$Date=="2012-04-21"] <- round((95.8+84.8+96.3+88.2)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Washington Wizards at Miami Heat"&seasondata$Date=="2012-04-21"] <- round((98.5+92.5+93.6+98.4)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Brooklyn Nets at Milwaukee Bucks"&seasondata$Date=="2012-04-21"] <- round((93.1+99.1+99+98.7)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Portland Trail Blazers at Memphis Grizzlies"&seasondata$Date=="2012-04-21"] <- round((97.5+97.5+95+93)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Phoenix Suns at Utah Jazz"&seasondata$Date=="2012-04-24"] <- round((98.5+98.5+99.7+99)/2)
seasondata$Line_Open[seasondata$GameDescription=="Dallas Mavericks at Chicago Bulls"&seasondata$Date=="2012-04-21"] <- -6
seasondata$Line_Open[seasondata$GameDescription=="Washington Wizards at Miami Heat"&seasondata$Date=="2012-04-21"] <- -5.5
seasondata$Line_Open[seasondata$GameDescription=="Brooklyn Nets at Milwaukee Bucks"&seasondata$Date=="2012-04-21"] <- -7.5
seasondata$Line_Open[seasondata$GameDescription=="Portland Trail Blazers at Memphis Grizzlies"&seasondata$Date=="2012-04-21"] <- -9.5
seasondata$Line_Open[seasondata$GameDescription=="Phoenix Suns at Utah Jazz"&seasondata$Date=="2012-04-24"] <- -6
seasondata$OverUnder_Open[seasondata$GameDescription=="Milwaukee Bucks at Indiana Pacers"&seasondata$Date=="2021-10-25"] <- 230
seasondata$OverUnder_Open[seasondata$GameDescription=="Philadelphia 76ers at San Antonio Spurs"&seasondata$Date=="2021-05-02"] <- 221
seasondata$OverUnder_Open[seasondata$GameDescription=="Atlanta Hawks at Utah Jazz"&seasondata$Date=="2021-11-09"] <- 222.5
seasondata$OverUnder_Open[seasondata$GameDescription=="Washington Wizards at Sacramento Kings"&seasondata$Date=="2021-04-14"] <- 240.5
seasondata$OverUnder_Open[seasondata$GameDescription=="Minnesota Timberwolves at Los Angeles Lakers"&seasondata$Date=="2021-03-16"] <- 223
seasondata$OverUnder_Open[seasondata$GameDescription=="Sacramento Kings at Boston Celtics"&seasondata$Date=="2021-03-19"] <- 233
seasondata$OverUnder_Open[seasondata$GameDescription=="Detroit Pistons at Washington Wizards"&seasondata$Date=="2013-02-27"] <- round((94.9+98.8+93.2+95.8)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Atlanta Hawks at Brooklyn Nets"&seasondata$Date=="2010-12-19"] <- round((95+95+94+100)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Indiana Pacers at Boston Celtics"&seasondata$Date=="2010-12-19"] <- round((100+100+96+91)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="Los Angeles Lakers at Toronto Raptors"&seasondata$Date=="2010-12-19"] <- round((99+105+101+95)/2)
seasondata$OverUnder_Open[seasondata$GameDescription=="LA Clippers at Dallas Mavericks"&seasondata$Date=="2015-02-09"] <- round((103+103.5+105.5+103)/2)

save(seasondata, file = paste("allregdata_withlines_FINAL.RData", sep=""))

load("checkinjuries.RData")
playerdataTODAY <- playerdataTODAY[order(playerdataTODAY$Team),]
