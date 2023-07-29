#0. Clean up and erase

rm(list=ls())

#1. Load Relevant Packages and Set WD
library("stats4")
library("bbmle")
library("caret")
#setwd(working)

load("allregdata_FINALV2.RData")

modelbeta4a <- lm(formula = HomeMargin ~ NetAdjMarg3.x + NetAdjMarg3.y +
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
                  + TotalFoulsAvg + RefTOSumAvg + TotalFoulsAvg10 + RefTOSumAvg10, data = seasondata[seasondata$Season>="2007-08",])

modelbeta4c <- lm(formula = TotalPoints ~ PointsTotal5v2.x + PointsTotal5v2.y + DefPointsTotal5v2.x + DefPointsTotal5v2.y + OppTotalPoints5v2.x
                  + OppTotalPoints5v2.y + OppDefTotalPoints5v2.x + OppDefTotalPoints5v2.y +
                    RegularSeason + Playoffs + BtoB.x + BtoB.y + Bubble + Pandemic + PartialPandemic
                  + Pace2.x + Pace2.y + GameDummy5
                  + PaceXSq2 + PaceYSq2 + PaceXPace2  + GameDummyLess20 + GameDummy2040 + GameDummy4060 + GameDummy6082
                  + Year + YearSq + PointsTotal520.x + PointsTotal520.y + DefPointsTotal520.x + DefPointsTotal520.y + OppTotalPoints520.x
                  + OppTotalPoints520.y + OppDefTotalPoints520.x + OppDefTotalPoints520.y +
                    PointsTotal510.x + PointsTotal510.y + DefPointsTotal510.x + DefPointsTotal510.y + OppTotalPoints510.x
                  + OppTotalPoints510.y + OppDefTotalPoints510.x + OppDefTotalPoints510.y +
                    PointsTotal55.x + PointsTotal55.y + DefPointsTotal55.x + DefPointsTotal55.y + OppTotalPoints55.x
                  + OppTotalPoints55.y + OppDefTotalPoints55.x + OppDefTotalPoints55.y
                  + DistDummy2 + DistDummy3 + DistDummy4 + DistDummy5
                  + DistDummy6 + DistDummy7 + OnOffAdjustment.x
                  # + Box1AdjChg.x 
                  + Box2Adjustmentv2.x + Box3Adjustmentv2.x
                  + OnOffAdjustment.y
                  # + Box1AdjChg.y 
                  + Box2Adjustmentv2.y + Box3Adjustmentv2.y
                  # + staradj.x + superadj.x + solidadj.x + staradj.y + superadj.y + solidadj.y +
                  # + HomeAdj20.x + HomeAdj10.x + HomeAdj5.x
                  # + HomeAdj20.y + HomeAdj10.y + HomeAdj5.y + HomeAdjAll.x + HomeAdjAll.y +
                  + TeamQualORPM.x + TeamQualDRPM.x +
                    TeamQualORPM.y + TeamQualDRPM.y +
                    EarlyHomeGames
                  #  OneOne + TwoZero + ZeroTwo  + OneTwo + TwoOne + ThreeZero + ZeroThree + ThreeOne + TwoTwo + OneThree + TwoThree + ThreeTwo
                  + MidSeriesGames + GameSix
                  + GameSeven + SweepProspect + Tankathon.x + Tankathon.y + PlayoffExpMin.x + PlayoffExpMin.y
                  + PlayoffExpMin.x*RegularSeason + PlayoffExpMin.y*RegularSeason + NumGamesWeek.x
                  + NumHomeGamesWeek.x + NumGamesWeek.y + NumHomeGamesWeek.y
                  + GameCountAvg + GameCountAvgSq+GameCountAvg*RegularSeason+ GameCountAvgSq*RegularSeason
                  +SeasTotPtAvg30
                  +SeasTotPtAvg60+SeasTotOverAvg + Box1SeasonAvg + Box2SeasonAvg
                  + Box3SeasonAvg + OnOffSeasonAvg
                  +SeasTotPtAvg30*RegularSeason
                  +SeasTotPtAvg60*RegularSeason+SeasTotOverAvg*RegularSeason, data = seasondata[seasondata$Season>="2007-08",])

seasondata$PredMarg[seasondata$Season>="2007-08"] <- predict(modelbeta4a,seasondata[seasondata$Season>="2007-08",])
seasondata$PredTotPts[seasondata$Season>="2007-08"] <- predict(modelbeta4c,seasondata[seasondata$Season>="2007-08",])

testdata <- seasondata[seasondata$Season>="2007-08",c("HomeMargin","PredMarg","PredTotPts")]
testdata2 <- seasondata[seasondata$Season>="2007-08",c("HomeMargin","PredMarg","PredTotPts")]
testdata2$HomeMargin <- -1*testdata2$HomeMargin
testdata2$PredMarg <- -1*testdata2$PredMarg


testdata <- rbind(testdata,testdata2)

overalldist <- ggplot(testdata, aes(HomeMargin)) +
  geom_histogram(aes(y = ..density..), fill='lightgray', col='black',binwidth=1) +
  stat_function(fun = dnorm, args = list(mean=mean(testdata$HomeMargin,na.rm=TRUE), sd=sd(testdata$HomeMargin,na.rm=TRUE)))


sd_avg <- sd(testdata$HomeMargin,na.rm=TRUE)
sd_avg2 <- sd(testdata$HomeMargin[testdata$PredMarg<=1&testdata$PredMarg>=-1],na.rm=TRUE)

testdata <- testdata[!is.na(testdata$HomeMargin)&!is.na(testdata$PredMarg)&!is.na(testdata$PredTotPts),]
testdata$count <- 1
testdata$count <- cumsum(testdata$count)
# sd <- a + b*sd_avg2
# mean_shift <- c + b*mean + c&mean^2
testdata$sd_avg2 <- sd_avg2
set.seed(200)
idx <- createDataPartition(testdata$count, p=1,list=FALSE)
# x <- 0
# distr <- dnorm(0,0,sd_avg)*(x>=0.5&x<=(-0.5))/(1-pnorm(-0.5,0,sd_avg)-pnorm(0.5,0,sd_avg))


vec1 <- c()
vec2 <- c()
vec3 <- c()
vec4 <- c()
count <- 1
for (i in 180:240) {
  for (k in -25:25) {
    vec1[count] <- i
    vec2[count] <- sd(testdata$HomeMargin[testdata$PredTotPts>=(i-2)&testdata$PredTotPts<=(i+2)&testdata$PredMarg>=(k-1)&testdata$PredMarg<=(k+1)],na.rm=TRUE)
    vec3[count] <- k
    vec4[count] <- sum(testdata$HomeMargin[testdata$PredTotPts>=(i-2)&testdata$PredTotPts<=(i+2)&testdata$PredMarg>=(k-1)&testdata$PredMarg<=(k+1)],na.rm=TRUE)/mean(testdata$HomeMargin[testdata$PredTotPts>=(i-2)&testdata$PredTotPts<=(i+2)&testdata$PredMarg>=(k-1)&testdata$PredMarg<=(k+1)],na.rm=TRUE)
    count <- count+1
  }
  
}

test3 <- data.frame(vec1,vec2,vec3,vec4)
model <- lm(vec2 ~ abs(vec3)*(abs(vec3)<3)+abs(vec3)*(abs(vec3)<7)+abs(vec3)*(abs(vec3)<10)+vec1,test3[test3$vec4>200,])
summary(model)

# model$coefficients['abs(vec3)']*abs(mean)+model$coefficients['vec1']*predtotpts+model$coefficients['(Intercept)']
# model$coefficients['abs(vec3)']*10+model$coefficients['vec1']*220+model$coefficients['(Intercept)']

func <- function(beta1,beta2,beta3,beta4,beta5,beta6,beta7) {
  mean <- testdata$PredMarg[idx]
  predtotpts <- testdata$PredTotPts[idx]
  predsd <- model$coefficients['(Intercept)'] + model$coefficients['vec1']*predtotpts + model$coefficients['abs(vec3)']*abs(mean) + model$coefficients['abs(vec3) < 3TRUE']*(abs(mean)<3) + model$coefficients['abs(vec3) < 7TRUE']*(abs(mean)<7) + model$coefficients['abs(vec3):abs(vec3) < 3TRUE']*abs(mean)*(abs(mean)<3) +model$coefficients['abs(vec3):abs(vec3) < 7TRUE']*abs(mean)*(abs(mean)<7) 
  x <- testdata$HomeMargin[idx]
  likelihood <- ((pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=0.5&x>=(-0.5))*0 + 
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=1.5&x>=(0.5))*beta1 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-0.5)&x>=(-1.5))*beta1 +
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=2.5&x>=(1.5))*beta2 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-1.5)&x>=(-2.5))*beta2 +
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=4.5&x>=(2.5))*beta3 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-2.5)&x>=(-4.5))*beta3 +
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=4.5&x<=(9.5))*beta4 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-4.5)&x>=(-9.5))*beta4 +
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=9.5&x<=(15.5))*beta5 +  (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-9.5)&x>=(-15.5))*beta5 +
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=15.5&x<=(35.5))*beta6 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-15.5)&x>=(-35.5))*beta6 + 
                   (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=35.5)*beta7 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-35.5))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                                                                                                                                                    (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                                                                                                                                                    (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                                                                                                                                                    (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                                                                                                                                                    (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                                                                                                                                                    (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                                                                                                                                                    (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
  
  -1*sum(log(likelihood))
  
}


# est <- stats4::mle(minuslog = func,start=list(a=0,b=0.06000096,beta1=.67,beta2=.85,beta3=1,beta4=1.2,beta5=1,beta6=0.8,beta7=1.2),method="Nelder-Mead",skip.hessian)
est2 <- mle2(minuslog = func,start=list(beta1=1,beta2=1,beta3=1,beta4=1,beta5=1,beta6=1,beta7=1),method="Nelder-Mead")
summary(est2)

# a=coef(est2)['a']
# b=coef(est2)['b']
beta1=coef(est2)['beta1']
beta2=coef(est2)['beta2']
beta3=coef(est2)['beta3']
beta4=coef(est2)['beta4']
beta5=coef(est2)['beta5']
beta6=coef(est2)['beta6']
beta7=coef(est2)['beta7']

# testdata$likelihood <- func(12.5,0.1,.7,1,1,1,1,1,1)
func(1,1,1,1,1,1,1)
func(beta1,beta2,beta3,beta4,beta5,beta6,beta7)
func(0.67,0.9,1,1.15,1,0.9,2)
func(1,1,1,1,1,1,1)




simulatedist <- function(mean,predtotpts) {
  

mean <- mean
predtotpts <- predtotpts
rangedown <- predtotpts-10
rangeup <- predtotpts+10
predsd <- model$coefficients['(Intercept)'] + model$coefficients['vec1']*predtotpts + model$coefficients['abs(vec3)']*abs(mean) + model$coefficients['abs(vec3) < 3TRUE']*(abs(mean)<3) + model$coefficients['abs(vec3) < 7TRUE']*(abs(mean)<7) + model$coefficients['abs(vec3):abs(vec3) < 3TRUE']*abs(mean)*(abs(mean)<3) +model$coefficients['abs(vec3):abs(vec3) < 7TRUE']*abs(mean)*(abs(mean)<7) 
dist <- c()
dist2 <- c()
dist3 <- c()
for (x in -40:40) {
  
  if(x==(-40)) {
    dist[x+41] <-  
      ((pnorm(x-0.5,mean,predsd))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                                                                                                                                          (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                                                                                                                                          (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                                                                                                                                          (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                                                                                                                                          (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                                                                                                                                          (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                                                                                                                                          (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
    
    
  } else {
    if (x==40) {
      dist[x+41] <- ((1-pnorm(x-0.5,mean,predsd))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                            (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                            (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                            (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                            (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                            (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                            (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
      
      
    } else {
      dist[x+41] <- ((pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=0.5&x>=(-0.5))*0 + 
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=1.5&x>=(0.5))*beta1 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-0.5)&x>=(-1.5))*beta1 +
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=2.5&x>=(1.5))*beta2 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-1.5)&x>=(-2.5))*beta2 +
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=4.5&x>=(2.5))*beta3 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-2.5)&x>=(-4.5))*beta3 +
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=4.5&x<=(9.5))*beta4 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-4.5)&x>=(-9.5))*beta4 +
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=9.5&x<=(15.5))*beta5 +  (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-9.5)&x>=(-15.5))*beta5 +
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=15.5&x<=(35.5))*beta6 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-15.5)&x>=(-35.5))*beta6 + 
                       (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=35.5)*beta7 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-35.5))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                                                                                                                                                        (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                                                                                                                                                        (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                                                                                                                                                        (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                                                                                                                                                        (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                                                                                                                                                        (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                                                                                                                                                        (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
      
      
    }
  }
  
 }

test4 <- data.frame(x=c(-40:40),dist)
                   

for (i in test4$x) {
  if (i==40) {
    test4$EmpDist[i+41] <- sum((testdata$HomeMargin>(i-0.5))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
  } else {
    if (i==(-40)) {
      test4$EmpDist[i+41] <- sum(testdata$HomeMargin<(i+0.5)&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
    } else {
      test4$EmpDist[i+41] <- sum((testdata$HomeMargin>(i-0.5)&testdata$HomeMargin<(i+0.5))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
    }
  }
  
}
                                                                                                                                                                 
return(ggplot(test4, aes(x=x)) +geom_line(aes(y=dist,color="Est Dist"))+geom_line(aes(y=EmpDist,color="Emp Dist"))+labs(color="Legend text"))

}

simulatemedian <- function(mean,predtotpts) {
  
  
  mean <- mean
  predtotpts <- predtotpts
  rangedown <- predtotpts-10
  rangeup <- predtotpts+10
  predsd <- model$coefficients['(Intercept)'] + model$coefficients['vec1']*predtotpts + model$coefficients['abs(vec3)']*abs(mean) + model$coefficients['abs(vec3) < 3TRUE']*(abs(mean)<3) + model$coefficients['abs(vec3) < 7TRUE']*(abs(mean)<7) + model$coefficients['abs(vec3):abs(vec3) < 3TRUE']*abs(mean)*(abs(mean)<3) +model$coefficients['abs(vec3):abs(vec3) < 7TRUE']*abs(mean)*(abs(mean)<7) 
  dist <- c()
  dist2 <- c()
  dist3 <- c()
  for (x in -40:40) {
    
    if(x==(-40)) {
      dist[x+41] <-  
        ((pnorm(x-0.5,mean,predsd))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                              (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                              (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                              (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                              (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                              (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                              (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
      
      
    } else {
      if (x==40) {
        dist[x+41] <- ((1-pnorm(x-0.5,mean,predsd))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                                              (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                                              (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                                              (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                                              (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                                              (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                                              (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
        
        
      } else {
        dist[x+41] <- ((pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=0.5&x>=(-0.5))*0 + 
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=1.5&x>=(0.5))*beta1 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-0.5)&x>=(-1.5))*beta1 +
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=2.5&x>=(1.5))*beta2 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-1.5)&x>=(-2.5))*beta2 +
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=4.5&x>=(2.5))*beta3 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-2.5)&x>=(-4.5))*beta3 +
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=4.5&x<=(9.5))*beta4 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-4.5)&x>=(-9.5))*beta4 +
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=9.5&x<=(15.5))*beta5 +  (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-9.5)&x>=(-15.5))*beta5 +
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=15.5&x<=(35.5))*beta6 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-15.5)&x>=(-35.5))*beta6 + 
                         (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x>=35.5)*beta7 + (pnorm(x+0.5,mean,predsd)-pnorm(x-0.5,mean,predsd))*(x<=(-35.5))*beta7)/((pnorm(1.5,mean,predsd)-pnorm(0.5,mean,predsd))*(beta1) + (pnorm(-0.5,mean,predsd)-pnorm(-1.5,mean,predsd))*(beta1) +
                                                                                                                                                                          (pnorm(2.5,mean,predsd)-pnorm(1.5,mean,predsd))*(beta2) + (pnorm(-1.5,mean,predsd)-pnorm(-2.5,mean,predsd))*(beta2) +
                                                                                                                                                                          (pnorm(4.5,mean,predsd)-pnorm(2.5,mean,predsd))*(beta3) + (pnorm(-2.5,mean,predsd)-pnorm(-4.5,mean,predsd))*(beta3) +
                                                                                                                                                                          (pnorm(9.5,mean,predsd)-pnorm(4.5,mean,predsd))*(beta4) + (pnorm(-4.5,mean,predsd)-pnorm(-9.5,mean,predsd))*(beta4) +
                                                                                                                                                                          (pnorm(15.5,mean,predsd)-pnorm(9.5,mean,predsd))*(beta5) +  (pnorm(-9.5,mean,predsd)-pnorm(-15.5,mean,predsd))*(beta5) +
                                                                                                                                                                          (pnorm(35.5,mean,predsd)-pnorm(15.5,mean,predsd))*(beta6) + (pnorm(-15.5,mean,predsd)-pnorm(-35.5,mean,predsd))*(beta6) + 
                                                                                                                                                                          (1-pnorm(35.5,mean,predsd))*(beta7) + (pnorm(-35.5,mean,predsd))*(beta7))
        
        
      }
    }
    
  }
  
  test4 <- data.frame(x=c(-40:40),dist)
  
  
  for (i in test4$x) {
    if (i==40) {
      test4$EmpDist[i+41] <- sum((testdata$HomeMargin>(i-0.5))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
    } else {
      if (i==(-40)) {
        test4$EmpDist[i+41] <- sum(testdata$HomeMargin<(i+0.5)&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
      } else {
        test4$EmpDist[i+41] <- sum((testdata$HomeMargin>(i-0.5)&testdata$HomeMargin<(i+0.5))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)/sum((testdata$HomeMargin>(-100000))&testdata$PredMarg>(mean-2)&testdata$PredMarg<(mean+2)&testdata$PredTotPts>rangedown&testdata$PredTotPts<rangeup)
      }
    }
    
  }
  test4$cumsum <- cumsum(test4$dist)/max(cumsum(test4$dist))
  medianest <- (((min(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))>0.5])-(.5)))/(min(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))>0.5])-(max(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))<0.5]))))*min(test4$x[cumsum(test4$dist)/max(cumsum(test4$dist))>0.5])+((-1*(max(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))<0.5])-(.5)))/(min(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))>0.5])-(max(test4$cumsum[cumsum(test4$dist)/max(cumsum(test4$dist))<0.5]))))*max(test4$x[cumsum(test4$dist)/max(cumsum(test4$dist))<0.5])
  return(medianest)
  
}



#simulatedist(13,210)
#simulatemedian(13,210)

 


