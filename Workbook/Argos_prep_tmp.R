# 1. set#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R") 
source("Functions/format_functions_v2.1.R")
source("Constants/file_locations.R")

#set up packages and environment ---------------------------------------
# library(remotes)
# remotes::install_github("picardis/nestR", build_vignettes = F)

#load in packages
setUp(c("randomForest", 
        "m2b", 
        "moveHMM", 
        "momentuHMM", 
        "dplyr",
        "tidyverse", 
        "caret",
        "mlbench", 
        "nestR", 
        "coda", 
        "jagsUI", 
        "R2jags", 
        "runjags", 
        "rjags", 
        "tidymodels", "ggplot2", "lme4"))

#ID	date_time	latitude	longitude	b	t

#### 1. Bring in the data ####
#bring in the Argos data
BTGO_spdata <- read.csv("C:/Users/14064/Dropbox/BTGO Movmement Study/Data/Data from Mo 2020/Haanmeer_Argos_MattLukeNathan.csv")
BTGO_tag <- read.csv("C:/Users/14064/Dropbox/BTGO Movmement Study/Data/Data from Mo 2020/Haanmer_Argos_NestIndividual_joined_completioncheck.csv")

#### 1. Clean spatial data ####
head(BTGO_spdata)

#scrub for locations deemed erroneous by algorithm
BTGO_spdata <- BTGO_spdata[which(is.na(BTGO_spdata$algorithm.marked.outlier)),]
BTGO_spdata <- BTGO_spdata[which(!is.na(BTGO_spdata$argos.valid.location.algorithm)),]
#remove locations with error class B or Z
BTGO_spdata <- BTGO_spdata[which(BTGO_spdata$argos.lc != c("B", "Z")),]

#reformat timestamp
BTGO_spdata$timestamp <- as.POSIXct(BTGO_spdata$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "CET")

#break down to minimal dataframe
BTGO_spdata <- BTGO_spdata[,c(names(BTGO_spdata) %in% c("event.id", "location.long", "location.lat", "timestamp", "individual.local.identifier", "tag.local.identifier"))]
head(BTGO_spdata)

#remove locations without time
BTGO_spdata <- BTGO_spdata[which(!is.na(BTGO_spdata$timestamp)),]
#remove duplicates
BTGO_spdata <- BTGO_spdata[!duplicated(BTGO_spdata[c(2:3)]),]

names(BTGO_spdata) <- c("event.id", "date_time", "longitude", "latitude", "TransmitterNr", "tag.local.identifier")

#### 3. prep for labelling and RF ####
BTGO_final <- BTGO_spdata[,-c(1,6)]

head(BTGO_final)

BTGO_final$b <- -1

BTGO_final$id <- paste(BTGO_final$TransmitterNr, format(BTGO_final$date_time, format="%Y"), sep="-")
BTGO_final <- BTGO_final[,which(!names(BTGO_final) %in% ("TransmitterNr"))]
head(BTGO_final)

#
lst <- list(NA,length(unique(BTGO_final$id)))

for(i in 1:length(unique(BTGO_final$id))){
        list_id <- as.data.frame(unique(BTGO_final$id))
        lst[[i]] <- as.data.frame(BTGO_final %>% filter(id == as.character(list_id[i,])) )
        write.csv(lst[[i]], paste("C:/Users/14064/Dropbox/BTGO Movmement Study/nestAutoEval/Argos_nestData/",as.character(list_id[i,]), ".csv", sep=""))
        
        }#i



for(i in 1:length(BTGO_tag[which(BTGO_tag$Complete == "TRUE"),"id"])){
        list_id <- as.data.frame(BTGO_tag[which(BTGO_tag$Complete == "TRUE"),"id"])
        lst[[i]] <- as.data.frame(BTGO_final %>% filter(id == as.character(list_id[i,])) )
        write.csv(lst[[i]], paste("C:/Users/14064/Dropbox/BTGO Movmement Study/nestAutoEval/Argos_nestData/LabelReady/",as.character(list_id[i,]), ".csv", sep=""))
        
}#i


