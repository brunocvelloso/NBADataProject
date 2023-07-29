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
library(parallel)
library(MASS)
#setwd(working)

load("nba_teamboxNEW.RData")
load("nba_playerboxNEW.RData")
load("nba_pbpNEW.RData")
load("NBA_ESPNgamelinks.RData")
load("NBA_ESPNgamelinksSUB.RData")
load("startdate.RData")
load("enddate.RData")
ifelse(as.numeric(substr(Sys.time(),12,13))>=20&as.numeric(substr(Sys.time(),12,13))<24,today <- (today()+1),today <- today())
nba_pbpNEW$id <- format(nba_pbpNEW$id,scientific=FALSE)

#just for test
# nba_playerboxNEW <- nba_playerboxCOMPLETE[nba_playerboxCOMPLETE$Date>="2003-10-01"&nba_playerboxCOMPLETE$Date<"2015-10-01",] #
# rm(nba_playerboxCOMPLETE)
# nba_pbpNEW <- nba_pbpCOMPLETE[nba_pbpCOMPLETE$Date>="2003-10-01"&nba_pbpCOMPLETE$Date<"2015-10-01",] #
# rm(nba_pbpCOMPLETE)
# nba_pbpNEW <- nba_pbpNEW[!is.na(nba_pbpNEW$GameID),]
# nba_teamboxNEW <- nba_teamboxCOMPLETE[nba_teamboxCOMPLETE$Date>="2003-10-01"&nba_teamboxCOMPLETE$Date<"2015-10-01",] #
# rm(nba_teamboxCOMPLETE)
# NBA_ESPNgamelinksSUB <- NBA_ESPNgamelinks[NBA_ESPNgamelinks$Date>="2003-10-01"&NBA_ESPNgamelinks$Date<"2015-10-01",] #
# rm(NBA_ESPNgamelinks)
###end test stuff
tic()
load("master_adjplusminus.RData")
load("master_detailed_teambox.RData")
load("master_detailedboxscore.RData")
detailedboxscoreMASTER <- detailedboxscore
detailed_teamboxMASTER <- detailed_teambox
adjplusminusMASTER <- adjplusminus

nba_teamboxNEW$home <- ifelse(nba_teamboxNEW$Team==nba_teamboxNEW$Team.x,1,0)
nba_playerboxNEW$home <- ifelse(nba_playerboxNEW$Team==nba_playerboxNEW$Team.x,1,0)

starters <- nba_playerboxNEW %>%
  dplyr::group_by(GameID) %>%
  dplyr::summarize(homestarters = paste(" ",paste(athlete_id[starter==TRUE&home==1],collapse=" ")," ",sep=""),
            awaystarters = paste(" ",paste(athlete_id[starter==TRUE&home==0],collapse=" ")," ",sep=""))

nba_pbp <- nba_pbpNEW
nba_pbp <- nba_pbp %>% left_join(nba_teamboxNEW[,c("GameID","team_id","home")],by=c("GameID","team_id"))

print("possessions")
nba_pbp$textlagFORPOSS <- sapply(1:nrow(nba_pbp), function(x) ifelse(x>1,nba_pbp$text[x-1],""))
#identify possessions
nba_pbp$endposs <- 0
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 2 of 2"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 2 of 2"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 2 of 2"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 2 of 2"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive rebound"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Rebound"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$type_text,"Turnover"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$type_text,"turnover"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 3 of 3"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 3 of 3"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 3 of 3"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 3 of 3"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive team rebound"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Team Rebound"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Traveling"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"traveling"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"double dribble"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Double dribble"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Double Dribble"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"palming"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Palming"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"discontinue"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"disc dribble"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"Disc Dribble"))] <- 1
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$endposs[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0

nba_pbp$homeoffense <- 0
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 2 of 2"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 2 of 2"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 2 of 2"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 2 of 2"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive rebound"))&nba_pbp$home==0] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Rebound"))&nba_pbp$home==0] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$type_text,"Turnover"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$type_text,"turnover"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 3 of 3"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 3 of 3"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 3 of 3"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 3 of 3"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive team rebound"))&nba_pbp$home==0] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Team Rebound"))&nba_pbp$home==0] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Traveling"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"traveling"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"double dribble"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Double dribble"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Double Dribble"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"palming"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Palming"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"discontinue"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"disc dribble"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"Disc Dribble"))&nba_pbp$home==1] <- 1
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homeoffense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0

nba_pbp$homedefense <- 0
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 2 of 2"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 2 of 2"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 2 of 2"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 2 of 2"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive rebound"))&nba_pbp$home==1] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Rebound"))&nba_pbp$home==1] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$type_text,"Turnover"))&nba_pbp$home==0&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$type_text,"turnover"))&nba_pbp$home==0&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 3 of 3"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 3 of 3"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 3 of 3"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 3 of 3"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive team rebound"))&nba_pbp$home==1] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Team Rebound"))&nba_pbp$home==1] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Traveling"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"traveling"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"double dribble"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Double dribble"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Double Dribble"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"palming"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Palming"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"discontinue"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"disc dribble"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"Disc Dribble"))&nba_pbp$home==0] <- 1
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made free throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"makes Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0
nba_pbp$homedefense[!is.na(stri_extract_first_regex(nba_pbp$text,"made Free Throw 1 of 1"))&!is.na(stri_extract_first_regex(nba_pbp$textlagFORPOSS,"transition take foul"))] <- 0

nba_pbp <- dplyr::select(nba_pbp,-textlagFORPOSS)

nba_pbp$diff <- nba_pbp$homeoffense-nba_pbp$homedefense

nba_pbp$difflag1 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-1>0,nba_pbp$diff[x-1],0))
nba_pbp$difflag2 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-2>0,nba_pbp$diff[x-2],0))
nba_pbp$difflag3 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-3>0,nba_pbp$diff[x-3],0))
nba_pbp$difflag4 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-4>0,nba_pbp$diff[x-4],0))
nba_pbp$difflag5 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-5>0,nba_pbp$diff[x-5],0))
nba_pbp$difflag6 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-6>0,nba_pbp$diff[x-6],0))
nba_pbp$difflag7 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-7>0,nba_pbp$diff[x-7],0))
nba_pbp$difflag8 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-8>0,nba_pbp$diff[x-8],0))
nba_pbp$difflag9 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-9>0,nba_pbp$diff[x-9],0))
nba_pbp$difflag10 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-10>0,nba_pbp$diff[x-10],0))
nba_pbp$difflag11 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-11>0,nba_pbp$diff[x-11],0))
nba_pbp$difflag12 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-12>0,nba_pbp$diff[x-12],0))
nba_pbp$difflag13 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-13>0,nba_pbp$diff[x-13],0))
nba_pbp$difflag14 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-14>0,nba_pbp$diff[x-14],0))
nba_pbp$difflag15 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-15>0,nba_pbp$diff[x-15],0))
nba_pbp$difflag16 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-16>0,nba_pbp$diff[x-16],0))
nba_pbp$difflag17 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-17>0,nba_pbp$diff[x-17],0))
nba_pbp$difflag18 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-18>0,nba_pbp$diff[x-18],0))
nba_pbp$difflag19 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-19>0,nba_pbp$diff[x-19],0))
nba_pbp$difflag20 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x-20>0,nba_pbp$diff[x-20],0))

nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag1==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag2==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag3==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag4==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag5==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag6==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag7==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag8==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag9==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag10==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag11==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag12==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag13==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag14==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag15==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag16==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag17==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag18==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag19==1] <- 1
nba_pbp$homedefense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))&nba_pbp$difflag20==1] <- 1
  
nba_pbp$homeoffense[nba_pbp$homedefense==0&!is.na(stri_extract_first_regex(nba_pbp$text,"End of"))] <- 1

nba_pbp <- dplyr::select(nba_pbp,-c("diff","difflag1","difflag2","difflag3","difflag4","difflag5","difflag6","difflag7","difflag8","difflag9","difflag10","difflag11","difflag12","difflag13","difflag14","difflag15","difflag16","difflag17","difflag18","difflag19","difflag20"))

print("minutes")
nba_pbp$min <- as.numeric(substr(nba_pbp$clock_display_value,1,unlist(gregexpr(':', nba_pbp$clock_display_value))-1))+(as.numeric(substr(nba_pbp$clock_display_value,unlist(gregexpr(':', nba_pbp$clock_display_value))+1,nchar(nba_pbp$clock_display_value))))/60
nba_pbp$min[unlist(gregexpr(':', nba_pbp$clock_display_value))==-1] <- as.numeric(substr(nba_pbp$clock_display_value[unlist(gregexpr(':', nba_pbp$clock_display_value))==-1],unlist(gregexpr(':', nba_pbp$clock_display_value[unlist(gregexpr(':', nba_pbp$clock_display_value))==-1]))+1,nchar(nba_pbp$clock_display_value[unlist(gregexpr(':', nba_pbp$clock_display_value))==-1])))/60
nba_pbp$min[nba_pbp$period_number==1] <- -1*(nba_pbp$min[nba_pbp$period_number==1]+36)+48
nba_pbp$min[nba_pbp$period_number==2] <- -1*(nba_pbp$min[nba_pbp$period_number==2]+24)+48
nba_pbp$min[nba_pbp$period_number==3] <- -1*(nba_pbp$min[nba_pbp$period_number==3]+12)+48
nba_pbp$min[nba_pbp$period_number==4] <- -1*(nba_pbp$min[nba_pbp$period_number==4]+0)+48
nba_pbp$min[nba_pbp$period_number==5] <- -1*(nba_pbp$min[nba_pbp$period_number==5])+48+5
nba_pbp$min[nba_pbp$period_number==6] <- -1*(nba_pbp$min[nba_pbp$period_number==6])+48+10
nba_pbp$min[nba_pbp$period_number==7] <- -1*(nba_pbp$min[nba_pbp$period_number==7])+48+15
nba_pbp$min[nba_pbp$period_number==8] <- -1*(nba_pbp$min[nba_pbp$period_number==8])+48+20
nba_pbp$min[nba_pbp$period_number==9] <- -1*(nba_pbp$min[nba_pbp$period_number==9])+48+25
nba_pbp$min[nba_pbp$period_number==10] <- -1*(nba_pbp$min[nba_pbp$period_number==10])+48+30

print("field goals")
#Field Goal Attempts
nba_pbp$FGA <- 0
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGA[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1


nba_pbp$FGM <- 0
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"shot"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Shot"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"jumper"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three pointer"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Jumper"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Pointer"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"dunk"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Dunk"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"layup"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Layup"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"floater"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1
nba_pbp$FGM[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Floater"))] <- 1


#view(nba_pbp2[nba_pbp2$coordinate_x==25|nba_pbp2$coordinate_x==25,])
#3pt Field Goal Attempts
nba_pbp$FGA_Three <- 0
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$score_value==3&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$score_value==2&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 0
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"misses"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three

nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$score_value==3&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$score_value==2&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 0
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"missed"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three

nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$score_value==3&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$score_value==2&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 0
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three

nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=46&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left pocket
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=4&nba_pbp$coordinate_y>=11&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=45&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=5&nba_pbp$coordinate_y>=13&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=44&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=6&nba_pbp$coordinate_y>=14&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=43&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=7&nba_pbp$coordinate_y>=16&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=42&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=8&nba_pbp$coordinate_y>=17&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=41&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=9&nba_pbp$coordinate_y>=18&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=40&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=10&nba_pbp$coordinate_y>=19&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=39&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=11&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=38&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=12&nba_pbp$coordinate_y>=20&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=37&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=13&nba_pbp$coordinate_y>=21&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=36&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=14&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=35&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=15&nba_pbp$coordinate_y>=22&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=34&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=16&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=33&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=17&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=32&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=18&nba_pbp$coordinate_y>=23&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=31&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=19&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=30&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=20&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=29&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=21&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=28&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=22&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=27&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=23&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x>=26&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=24&nba_pbp$coordinate_y>=24&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x==25&nba_pbp$coordinate_y>=25&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #right corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$score_value==3&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$score_value==2&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 0
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"three point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&!is.na(stri_extract_first_regex(nba_pbp$text,"Three Point"))] <- 1
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three
nba_pbp$FGA_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12&is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))&is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1 #left corner three


nba_pbp$FGM_Three <- 0
nba_pbp$FGM_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))&nba_pbp$FGA_Three==1] <- 1
nba_pbp$FGM_Three[!is.na(stri_extract_first_regex(nba_pbp$text,"made"))&nba_pbp$FGA_Three==1] <- 1

nba_pbp$FGM_Two <- 0
nba_pbp$FGA_Two <- 0
nba_pbp$FGM_Two[nba_pbp$FGM_Three==0&nba_pbp$FGM==1] <- 1
nba_pbp$FGA_Two[nba_pbp$FGA_Three==0&nba_pbp$FGA==1] <- 1

#checks
# sum(nba_pbp$FGM_Three)
# sum(nba_pbp$FGA_Three)
# sum(nba_pbp$FGM_Two)
# sum(nba_pbp$FGA_Two)
# sum(nba_pbp$FGM)
# sum(nba_pbp$FGA)

print("box score stats")

nba_pbp$ScoreAtt <- 0
nba_pbp$ScoreAtt[!is.na(stri_extract_first_regex(nba_pbp$text,"free throw 2 of 2"))] <- 1
nba_pbp$ScoreAtt[!is.na(stri_extract_first_regex(nba_pbp$text,"free throw 3 of 3"))] <- 1
nba_pbp$ScoreAtt[!is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw 2 of 2"))] <- 1
nba_pbp$ScoreAtt[!is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw 3 of 3"))] <- 1
nba_pbp$ScoreAtt[nba_pbp$FGA==1] <- 1

nba_pbp$TO <- 0
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$type_text,"Turnover"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Turnover"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$type_text,"turnover"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"turnover"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$type_text,"double dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"traveling"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$type_text,"Double Dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Traveling"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Traveling"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"traveling"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"double dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Double dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Double Dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"palming"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Palming"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"discontinue"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"disc dribble"))] <- 1
nba_pbp$TO[!is.na(stri_extract_first_regex(nba_pbp$text,"Disc Dribble"))] <- 1

nba_pbp$PF <- 0
nba_pbp$PF[!is.na(stri_extract_first_regex(nba_pbp$text,"Foul"))] <- 1
nba_pbp$PF[!is.na(stri_extract_first_regex(nba_pbp$text,"foul"))] <- 1
nba_pbp$STL <- 0
nba_pbp$STL[!is.na(stri_extract_first_regex(nba_pbp$text,"steal"))] <- 1
nba_pbp$STL[!is.na(stri_extract_first_regex(nba_pbp$text,"stolen"))] <- 1
nba_pbp$STL[!is.na(stri_extract_first_regex(nba_pbp$text,"Steal"))] <- 1
nba_pbp$STL[!is.na(stri_extract_first_regex(nba_pbp$text,"Stolen"))] <- 1
nba_pbp$BLK <- 0
nba_pbp$BLK[!is.na(stri_extract_first_regex(nba_pbp$text,"block"))] <- 1
nba_pbp$BLK[!is.na(stri_extract_first_regex(nba_pbp$text,"Block"))] <- 1
nba_pbp$DREB <- 0
nba_pbp$DREB[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive rebound"))] <- 1
nba_pbp$DREB[!is.na(stri_extract_first_regex(nba_pbp$text,"defensive team rebound"))] <- 1
nba_pbp$DREB[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Rebound"))] <- 1
nba_pbp$DREB[!is.na(stri_extract_first_regex(nba_pbp$text,"Defensive Team Rebound"))] <- 1
nba_pbp$OREB <- 0
nba_pbp$OREB[!is.na(stri_extract_first_regex(nba_pbp$text,"offensive rebound"))] <- 1
nba_pbp$OREB[!is.na(stri_extract_first_regex(nba_pbp$text,"offensive team rebound"))] <- 1
nba_pbp$OREB[!is.na(stri_extract_first_regex(nba_pbp$text,"Offensive Rebound"))] <- 1
nba_pbp$OREB[!is.na(stri_extract_first_regex(nba_pbp$text,"Offensive Team Rebound"))] <- 1
nba_pbp$REB <- ifelse(nba_pbp$DREB==1|nba_pbp$OREB==1,1,0)
nba_pbp$AST <- 0
nba_pbp$AST[!is.na(stri_extract_first_regex(nba_pbp$text,"assist"))] <- 1
nba_pbp$AST[!is.na(stri_extract_first_regex(nba_pbp$text,"Assist"))] <- 1

nba_pbp$FTA <- 0
nba_pbp$FTA[!is.na(stri_extract_first_regex(nba_pbp$text,"Free Throw"))] <- 1
nba_pbp$FTA[!is.na(stri_extract_first_regex(nba_pbp$text,"free throw"))] <- 1
nba_pbp$FTM <- 0
nba_pbp$FTM[nba_pbp$FTA==1&!is.na(stri_extract_first_regex(nba_pbp$text,"makes"))] <- 1
nba_pbp$FTM[nba_pbp$FTA==1&!is.na(stri_extract_first_regex(nba_pbp$text,"made"))] <- 1



nba_pbp$stint <- 0
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$text))>0] <- 1
nba_pbp$textlag <- sapply(1:nrow(nba_pbp), function(x) ifelse(x>1,nba_pbp$text[x-1],""))
nba_pbp$textlag2 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x>2,nba_pbp$text[x-2],""))
nba_pbp$textlag3 <- sapply(1:nrow(nba_pbp), function(x) ifelse(x>3,nba_pbp$text[x-3],""))
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag))>0&unlist(gregexpr("free throw", nba_pbp$text))>0] <- 1
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag2))>0&(unlist(gregexpr("free throw", nba_pbp$textlag))>0)&unlist(gregexpr("free throw", nba_pbp$text))>0] <- 1
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag3))>0&(unlist(gregexpr("free throw", nba_pbp$textlag2))>0)&(unlist(gregexpr("free throw", nba_pbp$textlag))>0)&unlist(gregexpr("free throw", nba_pbp$text))>0] <- 1
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag))>0&unlist(gregexpr("Free Throw", nba_pbp$text))>0] <- 1
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag2))>0&(unlist(gregexpr("Free Throw", nba_pbp$textlag))>0)&unlist(gregexpr("Free Throw", nba_pbp$text))>0] <- 1
nba_pbp$stint[unlist(gregexpr("enters the game", nba_pbp$textlag3))>0&(unlist(gregexpr("Free Throw", nba_pbp$textlag2))>0)&(unlist(gregexpr("Free Throw", nba_pbp$textlag))>0)&unlist(gregexpr("Free Throw", nba_pbp$text))>0] <- 1
nba_pbp$stintfwd <- sapply(1:nrow(nba_pbp), function(x) ifelse(x+1<=length(nba_pbp$stint),nba_pbp$stint[x+1],0))
nba_pbp$stint[nba_pbp$stintfwd==1] <- 0
nba_pbp$stint <- sapply(1:nrow(nba_pbp), function(x) ifelse(x>1,nba_pbp$stint[x-1],0))


nba_pbp <- nba_pbp %>%
  dplyr::group_by(GameID) %>%
  dplyr::mutate(stint_num = cumsum(stint)+1) %>%
  ungroup()
nba_pbp$stint_id <- paste(nba_pbp$GameID,"_",nba_pbp$stint_num,sep="")

nba_pbp <- nba_pbp %>% left_join(starters,by=c("GameID"))
# nba_pbp$homeplayers[nba_pbp$stint_num==1] <- paste(nba_pbp$homestarters[nba_pbp$stint_num==1],"_",nba_pbp$GameID[nba_pbp$stint_num==1],sep="")
# nba_pbp$awayplayers[nba_pbp$stint_num==1] <- paste(nba_pbp$awaystarters[nba_pbp$stint_num==1],"_",nba_pbp$GameID[nba_pbp$stint_num==1],sep="")

# for (i in 2:max(nba_pbp$stint_num)) {
#   oldhome <- unique(nba_pbp$homeplayers[nba_pbp$stint_num==i-1])
#   oldaway <- unique(nba_pbp$awayplayers[nba_pbp$stint_num==i-1])
#   newhome <- unique(nba_pbp$homeplayers[nba_pbp$stint_num==i-1])
#   newaway <- unique(nba_pbp$awayplayers[nba_pbp$stint_num==i-1])
#   vec1 <-  nba_pbp$participants_0_athlete_id[!is.na(stri_extract_first_regex(nba_pbp$text,"enters the game"))&(nba_pbp$stint_num==(i-1))] 
#   vec2 <- nba_pbp$participants_1_athlete_id[!is.na(stri_extract_first_regex(nba_pbp$text,"enters the game"))&(nba_pbp$stint_num==(i-1))] 
#   for (j in 1:length(nba_pbp$participants_0_athlete_id[!is.na(stri_extract_first_regex(nba_pbp$text,"enters the game"))&(nba_pbp$stint_num==(i-1))])) {
#     newhome <- str_replace(newhome,vec2[j],vec1[j])
#     newaway <- str_replace(newaway,vec2[j],vec1[j])
#   }
#   
#   nba_pbp$homeplayers[nba_pbp$stint_num==i] <- newhome
#   nba_pbp$awayplayers[nba_pbp$stint_num==i] <- newaway
# }

#shot charts
nba_pbp$cornerthree <- 0
nba_pbp$cornerthree[nba_pbp$FGA_Three==1&nba_pbp$coordinate_x>=47&nba_pbp$coordinate_y<12] <- 1
nba_pbp$cornerthree[nba_pbp$FGA_Three==1&nba_pbp$coordinate_x<=3&nba_pbp$coordinate_y<12] <- 1

nba_pbp$atbthree <- 0
nba_pbp$atbthree[nba_pbp$FGA_Three==1&nba_pbp$cornerthree==0] <- 1

nba_pbp$topkeytwo <- 0
nba_pbp$topkeytwo[nba_pbp$FGA_Three==0&nba_pbp$coordinate_y>=15&nba_pbp$FGA==1] <- 1

nba_pbp$wingtwo <- 0
nba_pbp$wingtwo[nba_pbp$FGA_Three==0&(nba_pbp$coordinate_x>34|nba_pbp$coordinate_x<16)&nba_pbp$coordinate_y<15&nba_pbp$FGA==1] <- 1

nba_pbp$midctrtwo <- 0
nba_pbp$midctrtwo[nba_pbp$FGA_Three==0&nba_pbp$coordinate_x<=34&nba_pbp$coordinate_x>=16&nba_pbp$coordinate_y<15&nba_pbp$coordinate_y>=6&nba_pbp$FGA==1] <- 1

nba_pbp$restrareatwo <- 0
nba_pbp$restrareatwo[nba_pbp$FGA_Three==0&nba_pbp$coordinate_x<=34&nba_pbp$coordinate_x>=16&nba_pbp$coordinate_y<6&nba_pbp$FGA==1] <- 1

sum(nba_pbp$cornerthree)
sum(nba_pbp$atbthree)
sum(nba_pbp$topkeytwo)
sum(nba_pbp$wingtwo)
sum(nba_pbp$midctrtwo)
sum(nba_pbp$restrareatwo)
sum(nba_pbp$FGA)

nba_pbp$text[is.na(nba_pbp$text)] <- nba_pbp$type_text[is.na(nba_pbp$text)]
nba_pbp$distance <- NA
nba_pbp$distance[unlist(gregexpr("-foot", nba_pbp$text))>0] <- substr(nba_pbp$text[unlist(gregexpr("-foot", nba_pbp$text))>0],unlist(gregexpr("-foot", nba_pbp$text[unlist(gregexpr("-foot", nba_pbp$text))>0]))-2,unlist(gregexpr("-foot", nba_pbp$text[unlist(gregexpr("-foot", nba_pbp$text))>0]))-1)
nba_pbp$distance[unlist(gregexpr(" ft", nba_pbp$text))>0] <- substr(nba_pbp$text[unlist(gregexpr(" ft", nba_pbp$text))>0],unlist(gregexpr(" ft", nba_pbp$text[unlist(gregexpr(" ft", nba_pbp$text))>0]))-2,unlist(gregexpr(" ft", nba_pbp$text[unlist(gregexpr(" ft", nba_pbp$text))>0]))-1)
nba_pbp$distance <- as.numeric(nba_pbp$distance)
# mean(nba_pbp$distance[nba_pbp$restrareatwo==1],na.rm=TRUE) #5.5
# mean(nba_pbp$distance[nba_pbp$midctrtwo==1],na.rm=TRUE) #11
# mean(nba_pbp$distance[nba_pbp$wingtwo==1],na.rm=TRUE) #16
# mean(nba_pbp$distance[nba_pbp$topkeytwo==1],na.rm=TRUE) #18
# mean(nba_pbp$distance[nba_pbp$cornerthree==1],na.rm=TRUE) #24
# mean(nba_pbp$distance[nba_pbp$atbthree==1],na.rm=TRUE) #26
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$restrareatwo==1] <- 5.5
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$midctrtwo==1] <- 11
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$wingtwo==1] <- 16
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$topkeytwo==1] <- 18
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$cornerthree==1] <- 24
nba_pbp$distance[is.na(nba_pbp$distance)&nba_pbp$FGA==1&nba_pbp$atbthree==1] <- 26.5

nba_pbp$longtwo <- 0
nba_pbp$longtwo[nba_pbp$FGA_Three==0&nba_pbp$FGA==1&nba_pbp$distance>=16] <- 1

nba_pbp$longtwoM <- 0
nba_pbp$longtwoM[nba_pbp$longtwo==1&nba_pbp$FGM==1] <- 1
nba_pbp$restrareatwoM <- 0
nba_pbp$restrareatwoM[nba_pbp$restrareatwo==1&nba_pbp$FGM==1] <- 1
nba_pbp$wingtwoM <- 0
nba_pbp$wingtwoM[nba_pbp$wingtwo==1&nba_pbp$FGM==1] <- 1
nba_pbp$topkeytwoM <- 0
nba_pbp$topkeytwoM[nba_pbp$topkeytwo==1&nba_pbp$FGM==1] <- 1
nba_pbp$midctrtwoM <- 0
nba_pbp$midctrtwoM[nba_pbp$midctrtwo==1&nba_pbp$FGM==1] <- 1
nba_pbp$cornerthreeM <- 0
nba_pbp$cornerthreeM[nba_pbp$cornerthree==1&nba_pbp$FGM==1] <- 1
nba_pbp$atbthreeM <- 0
nba_pbp$atbthreeM[nba_pbp$atbthree==1&nba_pbp$FGM==1] <- 1

print("adj plus minus")
adjplusminus <- nba_pbp %>%
  dplyr::group_by(GameID,stint_num,stint_id) %>%
  dplyr::summarize(subin = paste(" ",paste(participants_0_athlete_id[!is.na(stri_extract_first_regex(text,"enters the game"))],collapse = " ")," ",sep=""),
            subout = paste(" ",paste(participants_1_athlete_id[!is.na(stri_extract_first_regex(text,"enters the game"))],collapse = " ")," ",sep=""),
            homestarters = max(homestarters),
            awaystarters = max(awaystarters),
            TO_home = sum(TO[home==1],na.rm = TRUE),
            PF_home = sum(PF[home==1],na.rm = TRUE),
            STL_home = sum(STL[home==0],na.rm = TRUE),
            REB_home = sum(REB[home==1],na.rm = TRUE),
            OREB_home = sum(OREB[home==1],na.rm = TRUE),
            DREB_home = sum(DREB[home==1],na.rm = TRUE),
            BLK_home = sum(BLK[home==0],na.rm = TRUE),
            AST_home = sum(AST[home==1],na.rm = TRUE),
            FTA_home = sum(FTA[home==1],na.rm = TRUE),
            FTM_home = sum(FTM[home==1],na.rm = TRUE),
            TO_away = sum(TO[home==0],na.rm = TRUE),
            PF_away = sum(PF[home==0],na.rm = TRUE),
            STL_away = sum(STL[home==1],na.rm = TRUE),
            REB_away = sum(REB[home==0],na.rm = TRUE),
            OREB_away = sum(OREB[home==0],na.rm = TRUE),
            DREB_away = sum(DREB[home==0],na.rm = TRUE),
            BLK_away = sum(BLK[home==1],na.rm = TRUE),
            AST_away = sum(AST[home==0],na.rm = TRUE),
            FTA_away = sum(FTA[home==0],na.rm = TRUE),
            FTM_away = sum(FTM[home==0],na.rm = TRUE),
            TO = sum(TO),
            PF = sum(PF),
            STL = sum(STL),
            REB = sum(REB),
            OREB = sum(OREB),
            DREB = sum(DREB),
            BLK = sum(BLK),
            AST = sum(AST),
            FTA = sum(FTA),
            FTM = sum(FTM),
            startmin = min(min),
            endmin = max(min),
            minlen = endmin-startmin,
            starthomescore = min(home_score),
            endhomescore = max(home_score),
            homepoints = endhomescore-starthomescore,
            startawayscore = min(away_score),
            endawayscore = max(away_score),
            awaypoints = endawayscore-startawayscore,
            poss = sum(endposs),
            poss_home = sum(homeoffense,na.rm = TRUE),
            poss_away = sum(homedefense,na.rm = TRUE),
            poss= sum(endposs,na.rm = TRUE),
            FGA_home = sum(FGA[home==1],na.rm = TRUE),
            FGM_home = sum(FGM[home==1],na.rm = TRUE),
            FGA_home = sum(FGA[home==1],na.rm = TRUE),
            FGM_home = sum(FGM[home==1],na.rm = TRUE),
            FGA_three_home = sum(FGA_Three[home==1],na.rm = TRUE),
            FGM_three_home = sum(FGM_Three[home==1],na.rm = TRUE),
            FGA_two_home = sum(FGA_Two[home==1],na.rm = TRUE),
            FGM_two_home = sum(FGM_Two[home==1],na.rm = TRUE),
            scoreatt_home = sum(ScoreAtt[home==1],na.rm = TRUE),
            FGA_away= sum(FGA[home==0],na.rm = TRUE),
            FGM_away= sum(FGM[home==0],na.rm = TRUE),
            FGA_three_away= sum(FGA_Three[home==0],na.rm = TRUE),
            FGM_three_away= sum(FGM_Three[home==0],na.rm = TRUE),
            FGA_two_away= sum(FGA_Two[home==0],na.rm = TRUE),
            FGM_two_away= sum(FGM_Two[home==0],na.rm = TRUE),
            scoreatt_away= sum(ScoreAtt[home==0],na.rm = TRUE),
            FGA = sum(FGA),
            FGM = sum(FGM),
            FGA_three = sum(FGA_Three),
            FGM_three = sum(FGM_Three),
            FGA_two = sum(FGA_Two),
            FGM_two = sum(FGM_Two),
            scoreatt = sum(ScoreAtt),
            atbthree_home = sum(atbthree[home==1],na.rm = TRUE),
            atbthreeM_home = sum(atbthreeM[home==1],na.rm = TRUE),
            atbthree_away= sum(atbthree[home==0],na.rm = TRUE),
            atbthreeM_away= sum(atbthreeM[home==0],na.rm = TRUE),
            atbthree = sum(atbthree),
            atbthreeM = sum(atbthreeM),
            cornerthree_home = sum(cornerthree[home==1],na.rm = TRUE),
            cornerthreeM_home = sum(cornerthreeM[home==1],na.rm = TRUE),
            cornerthree_away= sum(cornerthree[home==0],na.rm = TRUE),
            cornerthreeM_away= sum(cornerthreeM[home==0],na.rm = TRUE),
            cornerthree = sum(cornerthree),
            cornerthreeM = sum(cornerthreeM),
            longtwo_home = sum(longtwo[home==1],na.rm = TRUE),
            longtwoM_home = sum(longtwoM[home==1],na.rm = TRUE),
            longtwo_away= sum(longtwo[home==0],na.rm = TRUE),
            longtwoM_away= sum(longtwoM[home==0],na.rm = TRUE),
            longtwo = sum(longtwo),
            longtwoM = sum(longtwoM),
            wingtwo_home = sum(wingtwo[home==1],na.rm = TRUE),
            wingtwoM_home = sum(wingtwoM[home==1],na.rm = TRUE),
            wingtwo_away= sum(wingtwo[home==0],na.rm = TRUE),
            wingtwoM_away= sum(wingtwoM[home==0],na.rm = TRUE),
            wingtwo = sum(wingtwo),
            wingtwoM = sum(wingtwoM),
            midctrtwo_home = sum(midctrtwo[home==1],na.rm = TRUE),
            midctrtwoM_home = sum(midctrtwoM[home==1],na.rm = TRUE),
            midctrtwo_away= sum(midctrtwo[home==0],na.rm = TRUE),
            midctrtwoM_away= sum(midctrtwoM[home==0],na.rm = TRUE),
            midctrtwo = sum(midctrtwo),
            midctrtwoM = sum(midctrtwoM),
            restrareatwo_home = sum(restrareatwo[home==1],na.rm = TRUE),
            restrareatwoM_home = sum(restrareatwoM[home==1],na.rm = TRUE),
            restrareatwo_away= sum(restrareatwo[home==0],na.rm = TRUE),
            restrareatwoM_away= sum(restrareatwoM[home==0],na.rm = TRUE),
            restrareatwo = sum(restrareatwo),
            restrareatwoM = sum(restrareatwoM),
            topkeytwo_home = sum(topkeytwo[home==1],na.rm = TRUE),
            topkeytwoM_home = sum(topkeytwoM[home==1],na.rm = TRUE),
            topkeytwo_away= sum(topkeytwo[home==0],na.rm = TRUE),
            topkeytwoM_away= sum(topkeytwoM[home==0],na.rm = TRUE),
            topkeytwo = sum(topkeytwo),
            topkeytwoM = sum(topkeytwoM)
  ) %>% 
  dplyr::ungroup()



adjplusminus <- adjplusminus %>%
  dplyr::group_by(GameID) %>%
  dplyr::mutate(startmin = sapply(1:n(), function(x) ifelse(x>1,endmin[x-1],0)),
         starthomescore = sapply(1:n(), function(x) ifelse(x>1,endhomescore[x-1],0)),
         startawayscore = sapply(1:n(), function(x) ifelse(x>1,endawayscore[x-1],0)),
         minlen = endmin-startmin,
         homepoints = endhomescore-starthomescore,
         awaypoints = endawayscore-startawayscore,
         subtoutlag = sapply(1:n(), function(x) ifelse(x>1,subout[x-1],"")),
         subtinlag = sapply(1:n(), function(x) ifelse(x>1,subin[x-1],"")),
         subout1 = ifelse(is.na(word(subtoutlag,2)),"",paste(" ",word(subtoutlag,2)," ",sep="")),
         subout2 = ifelse(is.na(word(subtoutlag,3)),"",paste(" ",word(subtoutlag,3)," ",sep="")),
         subout3 = ifelse(is.na(word(subtoutlag,4)),"",paste(" ",word(subtoutlag,4)," ",sep="")),
         subout4 = ifelse(is.na(word(subtoutlag,5)),"",paste(" ",word(subtoutlag,5)," ",sep="")),
         subout5 = ifelse(is.na(word(subtoutlag,6)),"",paste(" ",word(subtoutlag,6)," ",sep="")),
         subout6 = ifelse(is.na(word(subtoutlag,7)),"",paste(" ",word(subtoutlag,7)," ",sep="")),
         subout7 = ifelse(is.na(word(subtoutlag,8)),"",paste(" ",word(subtoutlag,8)," ",sep="")),
         subout8 = ifelse(is.na(word(subtoutlag,9)),"",paste(" ",word(subtoutlag,9)," ",sep="")),
         subout9 = ifelse(is.na(word(subtoutlag,10)),"",paste(" ",word(subtoutlag,10)," ",sep="")),
         subout10 = ifelse(is.na(word(subtoutlag,11)),"",paste(" ",word(subtoutlag,11)," "," ",sep="")),
         subin1 = ifelse(is.na(word(subtinlag,2)),"",paste(" ",word(subtinlag,2)," "," ",sep="")),
         subin2 = ifelse(is.na(word(subtinlag,3)),"",paste(" ",word(subtinlag,3)," ",sep="")),
         subin3 = ifelse(is.na(word(subtinlag,4)),"",paste(" ",word(subtinlag,4)," ",sep="")),
         subin4 = ifelse(is.na(word(subtinlag,5)),"",paste(" ",word(subtinlag,5)," ",sep="")),
         subin5 = ifelse(is.na(word(subtinlag,6)),"",paste(" ",word(subtinlag,6)," ",sep="")),
         subin6 = ifelse(is.na(word(subtinlag,7)),"",paste(" ",word(subtinlag,7)," ",sep="")),
         subin7 = ifelse(is.na(word(subtinlag,8)),"",paste(" ",word(subtinlag,8)," ",sep="")),
         subin8 = ifelse(is.na(word(subtinlag,9)),"",paste(" ",word(subtinlag,9)," ",sep="")),
         subin9 = ifelse(is.na(word(subtinlag,10)),"",paste(" ",word(subtinlag,10)," ",sep="")),
         subin10 = ifelse(is.na(word(subtinlag,11)),"",paste(" ",word(subtinlag,11)," "," ",sep="")),
         homeplayers = homestarters,
         homeplayers = sapply(1:n(), function(x) {
           if (x==1) a <<- homeplayers[x]
           if (x>1) { 
           a <<- ifelse(subout1[x]!="",str_replace(a,subout1[x],subin1[x]),a)
           a <<- ifelse(subout2[x]!="",str_replace(a,subout2[x],subin2[x]),a)
           a <<- ifelse(subout3[x]!="",str_replace(a,subout3[x],subin3[x]),a)
           a <<- ifelse(subout4[x]!="",str_replace(a,subout4[x],subin4[x]),a)
           a <<- ifelse(subout5[x]!="",str_replace(a,subout5[x],subin5[x]),a)
           a <<- ifelse(subout6[x]!="",str_replace(a,subout6[x],subin6[x]),a)
           a <<- ifelse(subout7[x]!="",str_replace(a,subout7[x],subin7[x]),a)
           a <<- ifelse(subout8[x]!="",str_replace(a,subout8[x],subin8[x]),a)
           a <<- ifelse(subout9[x]!="",str_replace(a,subout9[x],subin9[x]),a)
           a <<- ifelse(subout10[x]!="",str_replace(a,subout10[x],subin10[x]),a)
           }
           a
           }),
         awayplayers = awaystarters,
         awayplayers = sapply(1:n(), function(x) {
           if (x==1) a <<- awayplayers[x]
           if (x>1) { 
             a <<- ifelse(subout1[x]!="",str_replace(a,subout1[x],subin1[x]),a)
             a <<- ifelse(subout2[x]!="",str_replace(a,subout2[x],subin2[x]),a)
             a <<- ifelse(subout3[x]!="",str_replace(a,subout3[x],subin3[x]),a)
             a <<- ifelse(subout4[x]!="",str_replace(a,subout4[x],subin4[x]),a)
             a <<- ifelse(subout5[x]!="",str_replace(a,subout5[x],subin5[x]),a)
             a <<- ifelse(subout6[x]!="",str_replace(a,subout6[x],subin6[x]),a)
             a <<- ifelse(subout7[x]!="",str_replace(a,subout7[x],subin7[x]),a)
             a <<- ifelse(subout8[x]!="",str_replace(a,subout8[x],subin8[x]),a)
             a <<- ifelse(subout9[x]!="",str_replace(a,subout9[x],subin9[x]),a)
             a <<- ifelse(subout10[x]!="",str_replace(a,subout10[x],subin10[x]),a)
           }
           a
         })) %>%
  ungroup()
print("detailed box")
detailedboxscore1 <- nba_pbp %>%
  dplyr::group_by(GameID,participants_0_athlete_id) %>%
  dplyr::summarize(TO = sum(TO),
                   PF = sum(PF),
                   # STL = sum(STL),
                   REB = sum(REB),
                   OREB = sum(OREB),
                   DREB = sum(DREB),
                   # BLK = sum(BLK),
                   # AST = sum(AST),
                   FTA = sum(FTA),
                   FTM = sum(FTM),
                   FGA = sum(FGA),
                   FGM = sum(FGM),
                   FGA_three = sum(FGA_Three),
                   FGM_three = sum(FGM_Three),
                   FGA_two = sum(FGA_Two),
                   FGM_two = sum(FGM_Two),
                   scoreatt = sum(ScoreAtt),
                   atbthree = sum(atbthree),
                   atbthreeM = sum(atbthreeM),
                   cornerthree = sum(cornerthree),
                   cornerthreeM = sum(cornerthreeM),
                   longtwo = sum(longtwo),
                   longtwoM = sum(longtwoM),
                   wingtwo = sum(wingtwo),
                   wingtwoM = sum(wingtwoM),
                   midctrtwo = sum(midctrtwo),
                   midctrtwoM = sum(midctrtwoM),
                   restrareatwo = sum(restrareatwo),
                   restrareatwoM = sum(restrareatwoM),
                   topkeytwo = sum(topkeytwo),
                   topkeytwoM = sum(topkeytwoM)
  ) %>% 
  dplyr::ungroup()

detailedboxscore1 <- detailedboxscore1[!is.na(detailedboxscore1$participants_0_athlete_id),]
detailedboxscore1 <- dplyr::rename(detailedboxscore1,athlete_id=participants_0_athlete_id)

detailedboxscore2 <- nba_pbp %>%
  dplyr::group_by(GameID,participants_1_athlete_id) %>%
  dplyr::summarize(AST = sum(AST),
                   STL = sum(STL),
                   BLK = sum(BLK),
                   
  ) %>% 
  dplyr::ungroup()
detailedboxscore2 <- detailedboxscore2[!is.na(detailedboxscore2$participants_1_athlete_id),]
detailedboxscore2 <- dplyr::rename(detailedboxscore2,athlete_id=participants_1_athlete_id)


detailedboxscore3 <- nba_pbp %>%
  dplyr::group_by(GameID,participants_0_athlete_id) %>%
  dplyr::summarize(home = mean(home[unlist(gregexpr("enters the game", text))>0])) %>% 
  dplyr::ungroup()
detailedboxscore3 <- detailedboxscore3[!is.na(detailedboxscore3$participants_0_athlete_id),]
detailedboxscore3 <- dplyr::rename(detailedboxscore3,athlete_id=participants_0_athlete_id)

detailedboxscore4 <- nba_pbp %>%
  dplyr::group_by(GameID,participants_0_athlete_id,home) %>%
  dplyr::summarize(TO = sum(TO),
                   REB = sum(REB),
                   OREB = sum(OREB),
                   DREB = sum(DREB),
                   Team.x = max(Team.x,na.rm=TRUE),
                   Team.y = max(Team.y,na.rm=TRUE)) %>% 
  dplyr::ungroup()
detailedboxscore4$newid <- paste(detailedboxscore4$participants_0_athlete_id,detailedboxscore4$home,sep="")
detailedboxscore4 <- detailedboxscore4[detailedboxscore4$newid=="NA1"|detailedboxscore4$newid=="NA0",]
detailedboxscore4$participants_0_athlete_id[detailedboxscore4$newid=="NA1"] <- detailedboxscore4$Team.x[detailedboxscore4$newid=="NA1"]
detailedboxscore4$participants_0_athlete_id[detailedboxscore4$newid=="NA0"] <- detailedboxscore4$Team.y[detailedboxscore4$newid=="NA0"]
detailedboxscore4 <- dplyr::select(detailedboxscore4,GameID,participants_0_athlete_id,TO,REB,OREB,DREB,home)
detailedboxscore4 <- dplyr::rename(detailedboxscore4,athlete_id=participants_0_athlete_id)

detailedboxscore <- full_join(detailedboxscore1,detailedboxscore2,by=c("GameID","athlete_id")) %>% 
  full_join(detailedboxscore3,by=c("GameID","athlete_id")) %>%
  dplyr::bind_rows(detailedboxscore4)
detailedboxscore <- detailedboxscore[order(detailedboxscore$GameID,detailedboxscore$home,detailedboxscore$athlete_id),]

detailed_teambox <- detailedboxscore %>%
  dplyr::group_by(GameID,home) %>%
  dplyr::summarize(across(-matches('athlete_id'), funs(sum(., na.rm = TRUE))))
names(detailed_teambox) <- str_replace(names(detailed_teambox),"_sum","")

#add pts
detailedboxscore$PTS <- detailedboxscore$FGM_three*3+detailedboxscore$FGM_two*2+detailedboxscore$FTM*1
detailed_teambox$PTS <- detailed_teambox$FGM_three*3+detailed_teambox$FGM_two*2+detailed_teambox$FTM*1


adjplusminus <- adjplusminus %>%
  dplyr::mutate(minlen = endmin - startmin,
         homepoints = endhomescore - starthomescore,
         awaypoints = endawayscore - startawayscore,
          plusmin = homepoints-awaypoints,
         pmpm = (plusmin/minlen)*48,
         hppm = (homepoints/minlen)*48,
         appm = (awaypoints/minlen)*48,
         pmp_poss = (plusmin/(poss/2))*100,
         hpp_poss = (homepoints/poss_home)*100,
         app_poss = (awaypoints/poss_away)*100
  )

#final cleaning
detailed_teambox <- detailed_teambox[detailed_teambox$home!="NaN",]
detailedboxscore <- detailedboxscore[detailedboxscore$home!="NaN",]
detailed_teambox <- detailed_teambox[!is.na(detailed_teambox$GameID),]
detailedboxscore <- detailedboxscore[!is.na(detailedboxscore$GameID),]
toc()



adjplusminusUPDATE <- adjplusminus
save(adjplusminusUPDATE, file = paste("adjplusminusUPDATE.RData", sep=""))
adjplusminusMASTER <- adjplusminusMASTER[!(adjplusminusMASTER$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
adjplusminusMASTER <- rbind(adjplusminusMASTER,adjplusminusUPDATE)
adjplusminus <- adjplusminusMASTER
save(adjplusminus, file = paste("master_adjplusminus.RData", sep=""))

detailedboxscoreUPDATE <- detailedboxscore
save(detailedboxscoreUPDATE, file = paste("detailedboxscoreUPDATE.RData", sep=""))
detailedboxscoreMASTER <- detailedboxscoreMASTER[!(detailedboxscoreMASTER$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
detailedboxscoreMASTER <- rbind(detailedboxscoreMASTER,detailedboxscoreUPDATE)
detailedboxscore <- detailedboxscoreMASTER
save(detailedboxscore, file = paste("master_detailedboxscore.RData", sep=""))

detailed_teamboxUPDATE <- detailed_teambox
save(detailed_teamboxUPDATE, file = paste("detailed_teamboxUPDATE.RData", sep=""))
detailed_teamboxMASTER <- detailed_teamboxMASTER[!(detailed_teamboxMASTER$GameID %in% NBA_ESPNgamelinksSUB$GameID),]
detailed_teamboxMASTER <- rbind(detailed_teamboxMASTER,detailed_teamboxUPDATE)
detailed_teambox <- detailed_teamboxMASTER
save(detailed_teambox, file = paste("master_detailed_teambox.RData", sep=""))





