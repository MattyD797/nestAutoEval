# Script to load the data and inspect it

library(tidyverse)

# godwit data
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/Black-tailed Godwit/GPS/gpsOnGps_tracks.Rda")
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/Black-tailed Godwit/GPS/gpsOnArgos_tracks.Rda")
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/Black-tailed Godwit/Argos/argosOnArgos_tracks.Rda")
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/Black-tailed Godwit/Argos/argosOnGps_tracks.Rda")




# lapwing data
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/lapwing/lapwingOnLapwing_tracks.Rda")
load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/lapwing/allOnLapwing_tracks.Rda")

# Lapwing renesting infomation
vava_renest <- data.frame("id" = c("2021-5308","2021-5303","2021-5268","2021-5277","2021-5270"), "year" = as.character(rep(2021,5)),"second_nest_start" = as.POSIXct(c("2021-04-21 00:00","2021-05-07 00:00","2021-05-02 00:00","2021-04-29 00:00","2021-05-18 00:00")))
str(vava_renest)


btgoArgos_renest <- data.frame("id" = as.character(c("123424-2017","1438803-2016","144433-2016","144434-2017","144439-2016","144441-2017","144443-2017","157536-2018")),"second_nest_start" = as.POSIXct(c("2017-05-05 00:00","2016-05-18 00:00","2016-05-07 00:00","2017-05-15 00:00","2016-05-24 00:00","2017-05-15 00:00","2017-05-21 00:00","2018-05-04 00:00")))


#this is a problem.... dunno why?
RenestInfo$id %in% unique(tracks$id)

DelineateNestAttempts <- function(tracks = lapwingpred, RenestInfo = vava_renest, DaysBeforeRenestPoss = 5){
#   
# tracks = gpsOnArgos_tracks
# RenestInfo = btgoArgos_renest
# DaysBeforeRenestPoss = 5

renesters <- tracks %>% filter(id %in% RenestInfo$id) %>% arrange(id)
unique(renesters$id)

single <- tracks %>% filter(!id %in% unique(renesters$id))


lst <- list(length(unique(renesters$id)),NA)
for(i in 1:length(unique(renesters$id))){
  x <- as.vector(unique(renesters$id)[i])
  renestdata <- RenestInfo %>% arrange(id) %>%  filter(id == x)
  data <- tracks %>% filter(format(t,"%Y") == renestdata$year)
  tmp <- data %>% filter(id == x)
  tmp1 <- tmp %>% filter(t < renestdata$second_nest_start - DaysBeforeRenestPoss)
  tmp1$id <- paste(tmp1$id,"_1",sep="")
  tmp2 <- tmp %>% filter(t >= renestdata$second_nest_start - DaysBeforeRenestPoss)
  tmp2$id <- paste(tmp2$id,"_2",sep="")
  dat <- rbind(tmp1,tmp2)
   lst[[i]] <- dat
 }

lapwing_re <- as.data.frame(do.call(rbind, lst))
lapwingdata_fin <- rbind(lapwing_re,single)
tracks <<- lapwingdata_fin %>% arrange(id)
}


DelineateNestAttempts(tracks = gpsOnArgos_tracks, RenestInfo = btgoArgos_renest,DaysBeforeRenestPoss = 5)


unique(tracks$id)








