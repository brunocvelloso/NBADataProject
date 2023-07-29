#0. Clean up and erase

rm(list=ls())

#1. Load Relevant Packages and Set WD

library('tidyverse')
library('lubridate')
library('ggplot2')
library('stringr')
library('selectr')
library('ds4psy')
library('dplyr')
library("xlsx")

load("allregdata_FINALV2.RData")
load("combdata.RData")
load("combdataplayoffs.RData")
#get todays date (after 9pm returns tomorrows date) 
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
startdate <- today

#additional cleaning
seasondata <- seasondata[seasondata$Team.x!="TBD "&seasondata$Team.y!="TBD ",]
seasondata <- seasondata[seasondata$Team.x!="TBD"&seasondata$Team.y!="TBD",]
preddata_final <- seasondata[seasondata$Date>=today,]
seasondata <- seasondata[seasondata$Date<(today),]

predictline <- lm(formula = HomeMargin ~ NetAdjMarg3.x + NetAdjMarg3.y +
                    #  PrevAdjNetMarginv2.x + PrevAdjNetMarginv2.y + PrevAdjNetMarginv2.x*GameDummyLess20 + PrevAdjNetMarginv2.y*GameDummyLess20
                    # + PrevAdjNetMarginv2.x*RegularSeason + PrevAdjNetMarginv2.y*RegularSeason
                    + RegularSeason + Playoffs +
                    BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
                  + Pace2.x + Pace2.y 
                  + grthomecourt.x + prgdhomecourt.x + prbadhomecourt.x + rlybadhomecourt.x + avg2homecourt.x
                  + grtawaycourt.y + prgdawaycourt.y + prbadawaycourt.y + rlybadawaycourt.y + avg2awaycourt.y 
                  + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20
                  + AdjNetMarSqPlusv3 + AdjNetMarSqNegv3 + Year + YearSq + WinPct2.x + WinPct2.y
                  + NetAdjMarg320.x + NetAdjMarg320.y + NetAdjMarg310.x + NetAdjMarg310.y
                  + NetAdjMarg35.x + NetAdjMarg35.y + WinPctX20.x + WinPctX20.y
                  + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
                  + DistDummy6 + DistDummy7 + OnOffAdjustment.x
                  + Box1Adjustmentv2.x + Box1Adjustmentv2sq.x # + Box1AdjChg.x 
                  + OnOffAdjustment.y
                  + Box1Adjustmentv2.y + Box1Adjustmentv2sq.y # + Box1AdjChg.y
                  # + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
                  + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                  + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y # + OppWinPct.x
                  # + OppBox1Adj.x + OppBox1Adj20.x
                  # + OppBox1Adj10.x + OppBox1Adj5.x + OwnBox1Adj.x + OwnBox1Adj20.x + OwnBox1Adj10.x + OwnBox1Adj5.x
                  # + OppWinPct.y # + OppBox1Adj.y + OppBox1Adj20.y
                  # + OppBox1Adj10.y + OppBox1Adj5.y + OwnBox1Adj.y + OwnBox1Adj20.y + OwnBox1Adj10.y + OwnBox1Adj5.y +
                  + TeamQualORPM.x + TeamQualDRPM.x + TeamQualORPMsq.x + TeamQualDRPMsq.x +
                    TeamQualORPM.y + TeamQualDRPM.y + TeamQualORPMsq.y + TeamQualDRPMsq.y +
                    TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
                    TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
                    HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
                  + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
                  + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
                  + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y # + Box1SeasonAvg + Box2SeasonAvg
                  # + Box3SeasonAvg + OnOffSeasonAvg
                  + TotalFoulsAvg + RefTOSumAvg + TotalFoulsAvg10 + RefTOSumAvg10 + OneZeroComb, data = seasondata[seasondata$Season>="2007-08",])

predictwin <- glm(formula = HomeWin ~ NetAdjMarg11.x + NetAdjMarg11.y +
                     # PrevAdjNetMarginv2.x + PrevAdjNetMarginv2.y + PrevAdjNetMarginv2.x*GameDummyLess20 + PrevAdjNetMarginv2.y*GameDummyLess20
                     # + PrevAdjNetMarginv2.x*RegularSeason + PrevAdjNetMarginv2.y*RegularSeason
                     + RegularSeason + Playoffs +
                     BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
                   + Pace2.x + Pace2.y                     
                   + grthomecourt.x + prgdhomecourt.x + prbadhomecourt.x + rlybadhomecourt.x + avg2homecourt.x
                   + grtawaycourt.y + prgdawaycourt.y + prbadawaycourt.y + rlybadawaycourt.y + avg2awaycourt.y 
                   + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 +
                     + AdjNetMarSqPlusv11 + AdjNetMarSqNegv11 + Year + YearSq + WinPct2.x + WinPct2.y
                   + NetAdjMarg1120.x + NetAdjMarg1120.y 
                   + NetAdjMarg1110.x + NetAdjMarg1110.y
                   + NetAdjMarg115.x + NetAdjMarg115.y  + WinPctX20.x + WinPctX20.y
                   + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
                   + DistDummy6 + DistDummy7 + OnOffAdjustment.x
                   + Box1Adjustmentv2.x + Box1Adjustmentv2sq.x # + Box1AdjChg.x
                   + OnOffAdjustment.y
                   + Box1Adjustmentv2.y + Box1Adjustmentv2sq.y # + Box1AdjChg.y
                   # + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y
                   + HomeAdj20.x 
                   + HomeAdj10.x + HomeAdj5.x
                   + HomeAdj20.y 
                   + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y + OppWinPct.x
                   
                   
                   + OppWinPct.y 
                   +
                     TeamQualORPM.x + TeamQualDRPM.x + TeamQualORPMsq.x + TeamQualDRPMsq.x +
                     TeamQualORPM.y + TeamQualDRPM.y + TeamQualORPMsq.y + TeamQualDRPMsq.y +
                     TeamQualORPM.x*GameDummyLess20 + TeamQualDRPM.x*GameDummyLess20 +
                     TeamQualORPM.y*GameDummyLess20 + TeamQualDRPM.y*GameDummyLess20 +
                     HomeTeamSeriesAdv + AwayTeamSeriesAdv + TwoThree + ThreeZeroComb + DownTwo
                   + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
                   + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
                   + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y 
                   # + Box1SeasonAvg + Box2SeasonAvg
                   # + Box3SeasonAvg + OnOffSeasonAvg
                   + TotalFoulsAvg + TotalFoulsAvg*RegularSeason + OneZeroComb, data = seasondata[seasondata$Season>="2007-08",], family = binomial(link = "logit"))

save(predictline,file="predictwin.RData")
save(predictwin,file="predictline.RData")

test <- combdata[1:30,c("Team.y","NetAdjMarg3.y","TeamQualORPM.y","TeamQualDRPM.y")]
test$TeamQualRPM <- test$TeamQualORPM.y + test$TeamQualDRPM.y
test <- dplyr::rename(test,TeamQualORPM=TeamQualORPM.y,TeamQualDRPM=TeamQualDRPM.y,NetAdjMarg3=NetAdjMarg3.y,Team=Team.y)
powerrankings <- data.frame(combdata$Team.x,combdata$Team.y,predict(predictline,combdata),predict(predictline,combdataplayoffs))
powerrankings$Team <- ifelse(powerrankings$combdata.Team.y=="Atlanta Hawks",powerrankings$combdata.Team.x,powerrankings$combdata.Team.y)
powerrankings$predict.predictline..combdata. <- ifelse(powerrankings$combdata.Team.y=="Atlanta Hawks",powerrankings$predict.predictline..combdata.,-1*powerrankings$predict.predictline..combdata.)
powerrankings$predict.predictline..combdata.[powerrankings$combdata.Team.x=="Atlanta Hawks"&powerrankings$combdata.Team.y=="Atlanta Hawks"] <- c(-1,1)*powerrankings$predict.predictline..combdata.[powerrankings$combdata.Team.x=="Atlanta Hawks"&powerrankings$combdata.Team.y=="Atlanta Hawks"]
powerrankings$predict.predictline..combdataplayoffs. <- ifelse(powerrankings$combdata.Team.y=="Atlanta Hawks",powerrankings$predict.predictline..combdataplayoffs.,-1*powerrankings$predict.predictline..combdataplayoffs.)
powerrankings$predict.predictline..combdataplayoffs.[powerrankings$combdata.Team.x=="Atlanta Hawks"&powerrankings$combdata.Team.y=="Atlanta Hawks"] <- c(-1,1)*powerrankings$predict.predictline..combdataplayoffs.[powerrankings$combdata.Team.x=="Atlanta Hawks"&powerrankings$combdata.Team.y=="Atlanta Hawks"]
powerrankings <- powerrankings %>%
  dplyr::group_by(Team) %>%
  dplyr::summarize(Rating = mean(predict.predictline..combdata.),
                   PlayoffRating = mean(predict.predictline..combdataplayoffs.))
powerrankings$Rating <- powerrankings$Rating-mean(powerrankings$Rating)
powerrankings$PlayoffRating <- powerrankings$PlayoffRating-mean(powerrankings$PlayoffRating)
powerrankings <- powerrankings %>% left_join(test,by=c('Team')) %>% ungroup()
powerrankings$TeamQualDRPM <- powerrankings$TeamQualDRPM-mean(powerrankings$TeamQualDRPM)
powerrankings$TeamQualORPM <- powerrankings$TeamQualORPM-mean(powerrankings$TeamQualORPM)
powerrankings$TeamQualRPM <- powerrankings$TeamQualRPM-mean(powerrankings$TeamQualRPM)
powerrankings[,c("Rating","NetAdjMarg3","TeamQualRPM")] <- round(powerrankings[,c("Rating","NetAdjMarg3","TeamQualRPM")],digits=1)
powerrankings <- powerrankings[,c("Team","Rating","NetAdjMarg3","TeamQualRPM")]
powerrankings <- rename(powerrankings,ActualAdjMargin=NetAdjMarg3,PlayerQuality=TeamQualRPM)
powerrankings <- powerrankings[order(-powerrankings$Rating),]
print(powerrankings,n=30)

