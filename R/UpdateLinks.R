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
#setwd(working)

#2. Load Relevant Input Data
load("nba_teamboxCOMPLETE.RData")
load("nba_playerboxCOMPLETE.RData")
load("nba_pbpCOMPLETE.RData")
load("NBA_ESPNgamelinks.RData")
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
enddate <- today-1
startdate <- max(nba_teamboxCOMPLETE$Date)


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
      
      webpage <- read_html(url_date)
      elem <- html_nodes(webpage,paste0('a')) %>% html_attr("href")
      elem <- elem[which(regexpr('/boxscore/_/gameId/', elem) >= 1)]
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
          start <- str_locate_all(pattern ='boxscore',elembox[i])[[1]][1]
          end <- str_locate_all(pattern ='boxscore',elembox[i])[[1]][2]
          str_sub(elempbyp[i],start,end) <- "playbyplay"
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
      Date<- NA
      RegularSeason <- NA
      Playoffs <- NA
      Finals <- NA
      GameDescription <- NA
      Team.x <- NA
      Team.y <- NA
      Season <- NA
      season <- NA
      
      NBA_ESPNgamelinksLASTUPDATE <- data.frame(Year, Month, Day, BoxScore, PlaybyPlay, GameID,
                                                Date,RegularSeason,Playoffs,Finals,GameDescription,
                                                Team.x,Team.y,Season,season)
      NBA_ESPNgamelinksLASTUPDATE$error <- 0
      
      save(NBA_ESPNgamelinksLASTUPDATE, file = paste("NBA_ESPNgamelinksLASTUPDATE.RData", sep=""))
      load("NBA_ESPNgamelinks.RData")
      NBA_ESPNgamelinks <- rbind(NBA_ESPNgamelinks,NBA_ESPNgamelinksLASTUPDATE)
      NBA_ESPNgamelinks <- distinct(NBA_ESPNgamelinks)
      #remove dates that aren't really dates
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==11&NBA_ESPNgamelinks[ , c("Day")]==31),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==9&NBA_ESPNgamelinks[ , c("Day")]==31),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==6&NBA_ESPNgamelinks[ , c("Day")]==31),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==4&NBA_ESPNgamelinks[ , c("Day")]==31),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==2&NBA_ESPNgamelinks[ , c("Day")]==31),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks[ , c("Month")]==2&NBA_ESPNgamelinks[ , c("Day")]==30),]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(!is_wholenumber(NBA_ESPNgamelinks[ , c("Year")]/4)&NBA_ESPNgamelinks[ , c("Month")]==2&NBA_ESPNgamelinks[ , c("Day")]==29),]
      #remove any rows that aren't actual years
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[NBA_ESPNgamelinks$Year>1900,]
      NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$Year==2022&NBA_ESPNgamelinks$Month==2&NBA_ESPNgamelinks$Day>=18&NBA_ESPNgamelinks$Day<=23),]
      save(NBA_ESPNgamelinks, file = paste("NBA_ESPNgamelinks.RData", sep=""))
      print(elembox)
      print(url_date)
    }
  }
}


#NBA Season Dates and Playoff Dates
load("NBA_ESPNgamelinks.RData")
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!NBA_ESPNgamelinks$GameID==".espn.com",]
#Create vairable to indicate Playoffs or Regular Season
NBA_ESPNgamelinks$Date <- as.Date(with(NBA_ESPNgamelinks, paste(Year, Month, Day, sep="-")),"%Y-%m-%d")
NBA_ESPNgamelinks$RegularSeason <- ifelse(NBA_ESPNgamelinks$Date >= as.Date("12/22/2020", format = "%m/%d/%Y") &
                                            NBA_ESPNgamelinks$Date <= as.Date("05/16/2021", format = "%m/%d/%Y"), 1, 0)
NBA_ESPNgamelinks$Playoffs <- ifelse(NBA_ESPNgamelinks$Date >= as.Date("05/18/2021", format = "%m/%d/%Y") &
                                       NBA_ESPNgamelinks$Date <= as.Date("07/20/2021", format = "%m/%d/%Y"), 1, 0)
NBA_ESPNgamelinks$Finals <- ifelse(NBA_ESPNgamelinks$Date >= as.Date("07/06/2021", format = "%m/%d/%Y") &
                                     NBA_ESPNgamelinks$Date <= as.Date("07/20/2021", format = "%m/%d/%Y"), 1, 0)
NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/19/2021", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/10/2022", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/12/2022", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/19/2022", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/02/2022", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/19/2022", format = "%m/%d/%Y")] <- 1
#2023 season
NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/18/2022", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/9/2023", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/11/2023", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/18/2023", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/01/2023", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/18/2023", format = "%m/%d/%Y")] <- 1


NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/22/2019", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("08/14/2020", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("08/15/2020", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("10/11/2020", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("09/30/2020", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("10/11/2020", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/16/2018", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/10/2019", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/13/2019", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/13/2019", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("05/30/2019", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/13/2019", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/17/2017", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/11/2018", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/14/2018", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/08/2018", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("05/31/2018", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/08/2018", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/25/2016", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/12/2017", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/15/2017", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/12/2017", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/01/2017", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/12/2017", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/27/2015", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/13/2016", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/16/2016", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/19/2016", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/02/2016", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/19/2016", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/28/2014", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/15/2015", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/18/2015", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/16/2015", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/04/2015", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/16/2015", format = "%m/%d/%Y")] <- 1


NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/29/2013", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/16/2014", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/19/2014", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/15/2014", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/05/2014", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/15/2014", format = "%m/%d/%Y")] <- 1


NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/30/2012", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/17/2013", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/20/2013", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/20/2013", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/06/2013", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/20/2013", format = "%m/%d/%Y")] <- 1


NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("12/25/2011", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/26/2012", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/28/2012", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/21/2012", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/12/2012", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/21/2012", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/26/2010", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/13/2011", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/16/2011", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/12/2011", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("05/31/2011", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/12/2011", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/27/2009", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/14/2010", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/17/2010", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/17/2010", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/03/2010", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/17/2010", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/28/2008", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/16/2009", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/18/2009", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/14/2009", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/04/2009", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/14/2009", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/30/2007", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/16/2008", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/19/2008", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/17/2008", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/05/2008", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/17/2008", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/31/2006", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/18/2007", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/21/2007", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/14/2007", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/07/2007", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/14/2007", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("11/01/2005", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/19/2006", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/22/2006", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/20/2006", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/08/2006", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/20/2006", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("11/02/2004", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/20/2005", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/23/2005", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/06/2005", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/09/2005", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/23/2005", format = "%m/%d/%Y")] <- 1

NBA_ESPNgamelinks$RegularSeason[NBA_ESPNgamelinks$Date >= as.Date("10/28/2003", format = "%m/%d/%Y") &
                                  NBA_ESPNgamelinks$Date <= as.Date("04/14/2004", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Playoffs[NBA_ESPNgamelinks$Date >= as.Date("04/17/2004", format = "%m/%d/%Y") &
                             NBA_ESPNgamelinks$Date <= as.Date("06/15/2004", format = "%m/%d/%Y")] <- 1
NBA_ESPNgamelinks$Finals[NBA_ESPNgamelinks$Date >= as.Date("06/06/2004", format = "%m/%d/%Y") &
                           NBA_ESPNgamelinks$Date <= as.Date("06/15/2004", format = "%m/%d/%Y")] <- 1

# NBA_ESPNgamelinks$Season[is.na(NBA_ESPNgamelinks$Season)] <- ifelse(NBA_ESPNgamelinks$Month[is.na(NBA_ESPNgamelinks$Season)]>=10,paste(NBA_ESPNgamelinks$Year[is.na(NBA_ESPNgamelinks$Season)],"-",substr(NBA_ESPNgamelinks$Year[is.na(NBA_ESPNgamelinks$Season)]+1,3,4),sep=""),paste(NBA_ESPNgamelinks$Year[is.na(NBA_ESPNgamelinks$Season)]-1,"-",substr(NBA_ESPNgamelinks$Year[is.na(NBA_ESPNgamelinks$Season)],3,4),sep=""))
NBA_ESPNgamelinks$Season <- ifelse((NBA_ESPNgamelinks$Month>=10&NBA_ESPNgamelinks$Day>=12)|NBA_ESPNgamelinks$Month>=11,paste(NBA_ESPNgamelinks$Year,"-",substr(NBA_ESPNgamelinks$Year+1,3,4),sep=""),paste(NBA_ESPNgamelinks$Year-1,"-",substr(NBA_ESPNgamelinks$Year,3,4),sep=""))

NBA_ESPNgamelinks$season <- as.numeric(paste("20",substr(NBA_ESPNgamelinks$Season,nchar(NBA_ESPNgamelinks$Season)-1,nchar(NBA_ESPNgamelinks$Season)),sep=""))
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[(NBA_ESPNgamelinks$RegularSeason==1|NBA_ESPNgamelinks$Playoffs==1)&!is.na(NBA_ESPNgamelinks$RegularSeason),]
save(NBA_ESPNgamelinks, file = paste("NBA_ESPNgamelinks.RData", sep=""))

NBA_ESPNgamelinks$GameID <- as.character(NBA_ESPNgamelinks$GameID)
NBA_ESPNgamelinksSUB <- NBA_ESPNgamelinks[NBA_ESPNgamelinks$Date>=startdate&NBA_ESPNgamelinks$Date<=enddate,]
NBA_ESPNgamelinksSUB <- NBA_ESPNgamelinksSUB[!duplicated(NBA_ESPNgamelinksSUB$GameID),]


##check ESPN if data is available
nba_playerboxNEW <- nba_playerboxCOMPLETE[1:1,]
nba_teamboxNEW <- nba_teamboxCOMPLETE[1:1,]
nba_pbpNEW <- nba_pbpCOMPLETE[1:1,]

#fill in case of error
NBA_ESPNgamelinksSUB$error <- 0
#for new links, lets get it from the API the JSON File of NBA Data
for (i in 1:length(NBA_ESPNgamelinksSUB$GameID)) {
  
  tryCatch(
    {
      game_id <- as.numeric(NBA_ESPNgamelinksSUB$GameID[i])
      print(i)
      play_base_url <- "http://cdn.espn.com/core/nba/playbyplay?render=false&userab=1&xhr=1&"
      
      ## Inputs
      ## game_id
      full_url <- paste0(play_base_url,
                         "gameId=", game_id)
      
      res <- httr::RETRY("GET", full_url)
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      raw_play_df <- jsonlite::fromJSON(resp)[["gamepackageJSON"]]
      raw_play_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df),flatten=TRUE)
      plays <- raw_play_df[["plays"]] %>%
        tidyr::unnest_wider(.data$participants)
      aths <- plays %>%
        dplyr::group_by(.data$id) %>%
        dplyr::select(.data$id, .data$athlete.id) %>%
        tidyr::unnest_wider(.data$athlete.id, names_sep = "_")
      if (length(names(aths))<4) {
        aths$athlete.id_3 <- NA
      }
      names(aths)<-c("play.id","athlete.id.1","athlete.id.2","athlete.id.3")
      plays_df <- dplyr::bind_cols(plays, aths) %>%
        dplyr::select(-.data$athlete.id)
      
      raw_play_df <- jsonlite::fromJSON(resp)[["gamepackageJSON"]]
      players_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["players"]]), flatten=TRUE) %>%
        tidyr::unnest(.data$statistics) %>%
        tidyr::unnest(.data$athletes)
      
      stat_cols <- players_df$names[[1]]
      players_df$stats[lengths(players_df$stats)==0] <- "DNP"
      stats <- players_df$stats
      stats_df <- as.data.frame(do.call(rbind,stats))
      colnames(stats_df) <- stat_cols
      players_df <- players_df %>%
        dplyr::select(.data$starter,.data$ejected, .data$didNotPlay,.data$active,
                      .data$athlete.displayName,.data$athlete.jersey,
                      .data$athlete.id,.data$athlete.shortName,
                      .data$athlete.headshot.href,.data$athlete.position.name,
                      .data$athlete.position.abbreviation,.data$team.shortDisplayName,
                      .data$team.name,.data$team.logo,.data$team.id,.data$team.abbreviation,
                      .data$team.color,.data$team.alternateColor,.data$team.location)
      
      raw_play_df <- jsonlite::fromJSON(resp)[["gamepackageJSON"]]
      season <- raw_play_df[['header']][['season']][['year']]
      season_type <- raw_play_df[['header']][['season']][['type']]
      homeAwayTeam1 = toupper(raw_play_df[['header']][['competitions']][['competitors']][[1]][['homeAway']][1])
      homeAwayTeam2 = toupper(raw_play_df[['header']][['competitions']][['competitors']][[1]][['homeAway']][2])
      homeTeamId = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][1]
      awayTeamId = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['id']][2]
      homeTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][1]
      awayTeamMascot = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['name']][2]
      homeTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][1]
      awayTeamName = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['location']][2]
      
      homeTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][1]
      awayTeamAbbrev = raw_play_df[['header']][['competitions']][['competitors']][[1]][['team']][['abbreviation']][2]
      game_date = as.Date(substr(raw_play_df[['header']][['competitions']][['date']],0,10))
      teams_box_score_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["teams"]]),flatten=TRUE)
      
      teams_box_score_df_2 <- teams_box_score_df[[1]][[2]] %>%
        dplyr::select(.data$displayValue, .data$name) %>%
        dplyr::rename(Home = .data$displayValue)
      teams_box_score_df_1 <- teams_box_score_df[[1]][[1]] %>%
        dplyr::select(.data$displayValue, .data$name) %>%
        dplyr::rename(Away = .data$displayValue)
      teams2 <- data.frame(t(teams_box_score_df_2$Home))
      colnames(teams2) <- t(teams_box_score_df_2$name)
      teams2$homeAway <- homeAwayTeam1
      teams2$OpponentId <- as.integer(awayTeamId)
      teams2$OpponentName <- awayTeamName
      teams2$OpponentMascot <- awayTeamMascot
      teams2$OpponentAbbrev <- awayTeamAbbrev
      
      teams1 <- data.frame(t(teams_box_score_df_1$Away))
      colnames(teams1) <- t(teams_box_score_df_1$name)
      teams1$homeAway <- homeAwayTeam2
      teams1$OpponentId <- as.integer(homeTeamId)
      teams1$OpponentName <- homeTeamName
      teams1$OpponentMascot <- homeTeamMascot
      teams1$OpponentAbbrev <- homeTeamAbbrev
      teams <- dplyr::bind_rows(teams1,teams2)
      
      team_box_score <- teams_box_score_df %>%
        dplyr::select(-.data$statistics) %>%
        dplyr::bind_cols(teams)
      
      team_box_score <- team_box_score %>%
        dplyr::mutate(
          game_id = game_id,
          season = season,
          season_type = season_type,
          game_date = game_date
        ) %>%
        janitor::clean_names() %>%
        dplyr::select(
          .data$game_id,
          .data$season,
          .data$season_type,
          .data$game_date,
          tidyr::everything())
      
      player_box <- dplyr::bind_cols(stats_df,players_df) %>%
        dplyr::select(.data$athlete.displayName,.data$team.shortDisplayName, tidyr::everything())
      nba_pbp <- plays_df %>%
        janitor::clean_names()
      team_box_score <- team_box_score %>%
        janitor::clean_names()
      player_box <- player_box %>%
        janitor::clean_names()
      
      game_id <- NBA_ESPNgamelinksSUB$GameID[i]
      season <-  as.numeric(paste("20",substr(NBA_ESPNgamelinksSUB$Season[i],nchar(NBA_ESPNgamelinksSUB$Season[i])-1,nchar(NBA_ESPNgamelinksSUB$Season[i])),sep=""))
      season_type <- team_box_score$season_type[1]
      game_date <- NBA_ESPNgamelinksSUB$Date[i]
      
      player_box <- dplyr::rename(player_box,fg3=x3pt,plus_minus=x)
      player_box$game_id <- as.numeric(game_id)
      player_box$GameID <- game_id
      player_box$season <- season
      player_box$season_type <- season_type
      player_box$game_date <- game_date
      
      team_box_score$GameID <- game_id
      
      nba_pbp <- dplyr::rename(nba_pbp,participants_0_athlete_id=athlete_id_1,participants_1_athlete_id=athlete_id_2,participants_2_athlete_id=athlete_id_3)
      nba_pbp$season <- season
      nba_pbp$season_type <- season_type
      nba_pbp$id <- as.numeric(nba_pbp$id)
      nba_pbp$GameID <- substr(as.character(nba_pbp$id),1,9)
      
      nba_pbp$Team.x <- paste(team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      nba_pbp$Team.y <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2],sep="")
      nba_pbp$GameDescription <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2]," at ",team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      nba_pbp$RegularSeason <- NBA_ESPNgamelinksSUB$RegularSeason[i]
      nba_pbp$Finals <- NBA_ESPNgamelinksSUB$Finals[i]
      nba_pbp$Playoffs <- NBA_ESPNgamelinksSUB$Playoffs[i]
      nba_pbp$Date <- NBA_ESPNgamelinksSUB$Date[i]
        
      team_box_score$Team.x <- paste(team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      team_box_score$Team.y <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2],sep="")
      team_box_score$GameDescription <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2]," at ",team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      team_box_score$RegularSeason <- NBA_ESPNgamelinksSUB$RegularSeason[i]
      team_box_score$Finals <- NBA_ESPNgamelinksSUB$Finals[i]
      team_box_score$Playoffs <- NBA_ESPNgamelinksSUB$Playoffs[i]
      team_box_score$Date <- NBA_ESPNgamelinksSUB$Date[i]
      team_box_score$Team <- team_box_score$team_display_name
        
      player_box$Team.x <- paste(team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      player_box$Team.y <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2],sep="")
      player_box$GameDescription <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2]," at ",team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      player_box$RegularSeason <- NBA_ESPNgamelinksSUB$RegularSeason[i]
      player_box$Finals <- NBA_ESPNgamelinksSUB$Finals[i]
      player_box$Playoffs <- NBA_ESPNgamelinksSUB$Playoffs[i]
      player_box$Date <- NBA_ESPNgamelinksSUB$Date[i]
      player_box$Team <- paste(player_box$team_location," ",player_box$team_name,sep="")
      
      player_box <- dplyr::select(player_box,-team_alternate_color)
      player_box <- dplyr::select(player_box,-team_location)
      
      NBA_ESPNgamelinksSUB$Team.x[i] <- paste(team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      NBA_ESPNgamelinksSUB$Team.y[i] <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2],sep="")
      NBA_ESPNgamelinksSUB$GameDescription[i] <- paste(team_box_score$opponent_name[2]," ",team_box_score$opponent_mascot[2]," at ",team_box_score$opponent_name[1]," ",team_box_score$opponent_mascot[1],sep="")
      
      nba_pbpNEW$GameID <- substr(as.character(nba_pbpNEW$id),1,9)
      nba_playerboxNEW$GameID <- substr(as.character(nba_playerboxNEW$game_id),1,9)
      nba_teamboxNEW$GameID <- substr(as.character(nba_teamboxNEW$game_id),1,9)
    
      nba_playerboxNEW <- dplyr::bind_rows(nba_playerboxNEW,player_box)
      nba_teamboxNEW <- dplyr::bind_rows(nba_teamboxNEW,team_box_score)
      nba_pbpNEW <- dplyr::bind_rows(nba_pbpNEW, nba_pbp)
      
    },
    error = function(e){
      NBA_ESPNgamelinksSUB$error[i] <- 1
      cat("ERROR :",conditionMessage(e), "\n")
    }
  )
  
}

nba_playerboxNEW <- nba_playerboxNEW[2:length(nba_playerboxNEW$min),]
nba_teamboxNEW <- nba_teamboxNEW[2:length(nba_teamboxNEW$team_color),]
nba_pbpNEW <- nba_pbpNEW[2:length(nba_pbpNEW$shooting_play),]

nba_playerboxNEW <- distinct(nba_playerboxNEW)
nba_teamboxNEW <- distinct(nba_teamboxNEW)
nba_pbpNEW <- distinct(nba_pbpNEW)
NBA_ESPNgamelinksSUB <- distinct(NBA_ESPNgamelinksSUB)

nba_playerboxCOMPLETE <- nba_playerboxCOMPLETE[!(nba_playerboxCOMPLETE$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
nba_teamboxCOMPLETE <- nba_teamboxCOMPLETE[!(nba_teamboxCOMPLETE$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
nba_pbpCOMPLETE <- nba_pbpCOMPLETE[!(nba_pbpCOMPLETE$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
NBA_ESPNgamelinks <- NBA_ESPNgamelinks[!(NBA_ESPNgamelinks$GameID %in% NBA_ESPNgamelinksSUB$GameID),]

nba_playerboxCOMPLETE <- dplyr::bind_rows(nba_playerboxCOMPLETE,nba_playerboxNEW)
nba_teamboxCOMPLETE <- dplyr::bind_rows(nba_teamboxCOMPLETE,nba_teamboxNEW)
nba_pbpCOMPLETE <- dplyr::bind_rows(nba_pbpCOMPLETE,nba_pbpNEW)
NBA_ESPNgamelinks <- dplyr::bind_rows(NBA_ESPNgamelinks,NBA_ESPNgamelinksSUB)

save(nba_pbpCOMPLETE, file = paste("nba_pbpCOMPLETE.RData", sep=""))
save(nba_playerboxCOMPLETE, file = paste("nba_playerboxCOMPLETE.RData", sep=""))
save(nba_teamboxCOMPLETE, file = paste("nba_teamboxCOMPLETE.RData", sep=""))
save(NBA_ESPNgamelinks, file = paste("NBA_ESPNgamelinks.RData", sep=""))
save(nba_pbpNEW, file = paste("nba_pbpNEW.RData", sep=""))
save(nba_playerboxNEW, file = paste("nba_playerboxNEW.RData", sep=""))
save(nba_teamboxNEW, file = paste("nba_teamboxNEW.RData", sep=""))
save(NBA_ESPNgamelinksSUB, file = paste("NBA_ESPNgamelinksSUB.RData", sep=""))
save(startdate, file = paste("startdate.RData", sep=""))
save(enddate, file = paste("enddate.RData", sep=""))

nba_teamboxCOMPLETE$home <- ifelse(nba_teamboxCOMPLETE$Team==nba_teamboxCOMPLETE$Team.x,1,0)
nba_pbpCOMPLETE <- nba_pbpCOMPLETE %>% left_join(nba_teamboxCOMPLETE[,c("GameID","team_id","home")],by=c("GameID","team_id"))
homecourtrefs <- nba_pbpCOMPLETE %>% 
  dplyr::group_by(GameID,Team.x,Team.y,season) %>%
  dplyr::summarize(RefTOs.x = sum(type_text=="Palming Turnover"|type_text=="Disc Dribble Turnover"|type_text=="Double Dribble Turnover"|type_text=="Traveling"&home==1,na.rm=TRUE),
                   RefTOs.y = sum(type_text=="Palming Turnover"|type_text=="Disc Dribble Turnover"|type_text=="Double Dribble Turnover"|type_text=="Traveling"&home==0,na.rm=TRUE)) %>% ungroup()
homecourtrefs <- homecourtrefs[!is.na(homecourtrefs$GameID),]
save(homecourtrefs, file = paste("homecourtrefs.RData", sep=""))

