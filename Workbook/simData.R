library(tidyverse)
library(knitr)
library(kableExtra)

#load constants and functions
source("Functions/standard_functions.R")
source("Constants/file_locations.R")

# Main --------------------------------------------------------------------

#Define file path
argosTracks <- read_csv("Argos/Argos.csv")




# Filter Argos ------------------------------------------------------------

argosFilt <- argosTracks[,-c(1:2,6:12,14:26,28,29)] %>% 
  add_column(format(as.POSIXct(.$timestamp), format = "%Y")) %>%
  rename(Year = `format(as.POSIXct(.$timestamp), format = "%Y")`,
         ID = `tag-local-identifier`, 
         lon = `location-long`, 
         lat = `location-lat`) %>% 
  mutate(ID = paste0(ID,
                     "-",
                     Year))%>% 
  defNestArea(minLat,minLon) %>% 
  dplyr::select(-last_col())




# Distribution of Points --------------------------------------------------

hour <- as.numeric(format(as.POSIXct(argosFilt$timestamp), format = "%H"))

argosInd <- cbind(argosFilt, hour) %>% group_split(ID)

argosFiltered <- list()
i <- 1
for(val in 1:length(argosInd)){
  if(nrow(argosInd[[val]]) > 200){
    argosFiltered[[i]] <- argosInd[[val]]
    i <- i + 1
  }
}

findDist <- function(x){
  Dist <- vector()
  for(val in 0:23){
    Dist <- c(Dist,nrow(x[x$hour==val,])/nrow(x))
  }
  
  combDist <- c(sum(Dist[c(1:3, 22:24)]), 
                sum(Dist[c(4:9)]), 
                sum(Dist[c(10:15)]),
                sum(Dist[c(16:21)]))
  
  
  return(combDist)
}

hourDist <- lapply(argosFiltered, findDist)
hourDist <- do.call("rbind", hourDist)
timeSD <- rbind(apply(hourDist, 2, sd), apply(hourDist, 2, summary))
rownames(timeSD)[1]<-"SD";timeSD

#Find the number of pings for each ind
numberPings <- vector()
for(val in 1:length(argosFiltered)){
  numberPings <- c(numberPings, nrow(argosFiltered[[val]]))
}
summary(numberPings)   

#How many points on avg to grab for each individual
meanPoint <- ceiling(summary(numberPings)[4]*timeSD[5,])
#variation in the number of points per individual
sdPoint <- ceiling(summary(numberPings)[4]*timeSD[1,])


# GPS track simulation ----------------------------------------------------

#define file path 
GPS <- readTracks(predicted_tracks)%>% 
  defNestArea(minLat, minLon)

#Group GPS by hour
hour <- as.numeric(format(as.POSIXct(GPS$time), format = "%H"))
GPSBHour <- cbind(GPS, hour) 

GPS <- GPSBHour %>% group_split(ID)

GPSFiltered <- list()
i <- 1
for(val in 1:length(GPS)){
  if(nrow(GPS[[val]]) > 800){
    GPSFiltered[[i]] <- GPS[[val]]
    i <- i + 1
  }
}


# Low Resolution GPS ------------------------------------------------------

lowRes <- function(x, meanPoint){
  #Split data frame by hour groups
  first <- x[c(which(x$hour== 0), 
               which(x$hour== 1),
               which(x$hour== 2),
               which(x$hour== 21),
               which(x$hour== 22),
               which(x$hour== 23)),]
  sec <- x[c(which(x$hour== 3), 
             which(x$hour== 4),
             which(x$hour== 5),
             which(x$hour== 6),
             which(x$hour== 7),
             which(x$hour== 8)),]
  third <- x[c(which(x$hour== 9), 
               which(x$hour== 10),
               which(x$hour== 11),
               which(x$hour== 12),
               which(x$hour== 13),
               which(x$hour== 14)),]
  fourth <- x[c(which(x$hour== 15), 
                which(x$hour== 16),
                which(x$hour== 17),
                which(x$hour== 18),
                which(x$hour== 19),
                which(x$hour== 20)),]
  
  
  numPoints1 <- floor(rnorm(1, meanPoint[1], sdPoint[1]))
  numPoints2 <- floor(rnorm(1, meanPoint[2], sdPoint[2]))
  numPoints3 <- floor(rnorm(1, meanPoint[3], sdPoint[3]))
  numPoints4 <- floor(rnorm(1, meanPoint[4], sdPoint[4]))
  
  #randomely select number of points
  rows1 <- sample(1:nrow(first), numPoints1)
  rows2 <- sample(1:nrow(sec), numPoints2)
  rows3 <- sample(1:nrow(third), numPoints3)
  rows4 <- sample(1:nrow(fourth), numPoints4, )
  
  lowResGPS <- rbind(first[rows1,],
                     sec[rows2,], 
                     third[rows3, ], 
                     fourth[rows4,])
  
  return(lowResGPS[order(lowResGPS$time),])
}

set.seed(1234)
lowResGPS <- lapply(GPSFiltered, lowRes, meanPoint)


# Add Error to lat and lon ------------------------------------------------
hour <- as.numeric(format(as.POSIXct(argosFilt$timestamp), format = "%H"))
hargosFilt <- cbind(argosFilt,hour) %>% 
  group_split(hour) 

dist<-data.frame()
for(val in 1:length(hargosFilt)){
  temp <- vector()
  labcol <- names(table(hargosFilt[[2]]$`argos:lc`))
  j <- 1
  for (i in 1:length(labcol)){
    temp[j] <- length(which(hargosFilt[[val]]$`argos:lc`==labcol[i]))
    j <- j+1
  }
  dist <- rbind(dist, temp)
}

colnames(dist) <- names(table(hargosFilt[[2]]$`argos:lc`));dist

combineRows<-function(x){
  combDist <- c(sum(x[c(1:3, 22:24)]), 
                sum(x[c(4:9)]), 
                sum(x[c(10:15)]),
                sum(x[c(16:21)]))
  return(combDist)
}

summedRows <- apply(dist,2, combineRows)

combineRowsPercent <-function(x){
  percents <- vector()
  for(val in 1:length(x)){
    percents[val] <- x[val]/sum(x)
  }
  
  return(percents)
}

summedRowsPercent <- t(apply(summedRows, 1, combineRowsPercent))
colnames(summedRowsPercent) <- names(table(hargosFilt[[2]]$`argos:lc`))
summedRowsPercent


sampleRows <- function(x, percents, i){
  temp <- data.frame()
  lengthX <- nrow(x)
  for(val in c("0", "1", "2", "3", "A")){
    #Pick rows
    errorRow <- sample(1:nrow(x), lengthX*percents[i, val])
    
    #Place label on chosen rows
    temp <- rbind(temp, cbind(x[errorRow,], val))
    
    #remove rows, so they cannot be chosen again
    x <- x[-errorRow,]
  }
  #make all remaining rows the worst error
  last <- cbind(x, "B")
  colnames(last)[ncol(last)] <- colnames(temp)[ncol(temp)]
  temp <- rbind(temp, last)
  
  return(temp)
}


labelError <- function(x, percents){
  
  #Split data frame by hour groups
  first <- x[c(which(x$hour== 0), 
               which(x$hour== 1),
               which(x$hour== 2),
               which(x$hour== 21),
               which(x$hour== 22),
               which(x$hour== 23)),]
  sec <- x[c(which(x$hour== 3), 
             which(x$hour== 4),
             which(x$hour== 5),
             which(x$hour== 6),
             which(x$hour== 7),
             which(x$hour== 8)),]
  third <- x[c(which(x$hour== 9), 
               which(x$hour== 10),
               which(x$hour== 11),
               which(x$hour== 12),
               which(x$hour== 13),
               which(x$hour== 14)),]
  fourth <- x[c(which(x$hour== 15), 
                which(x$hour== 16),
                which(x$hour== 17),
                which(x$hour== 18),
                which(x$hour== 19),
                which(x$hour== 20)),]
  
  
  #label all rows by error
  labFirst <- sampleRows(first, percents, 1)
  labSec <- sampleRows(sec, percents, 2)
  labThird <- sampleRows(third, percents, 3)
  labFourth <- sampleRows(fourth, percents, 4)
  
  #Combine all labeled data
  labComplete <- rbind(labFirst, labSec, labThird, labFourth)
  
  return(labComplete[order(labComplete$ID, labComplete$time),])
}

lowResGPSComb <- do.call("rbind",lowResGPS) %>% 
  labelError(summedRowsPercent)

source("functions/standard_functions.R")


llGPSComb <-ll2utm(lowResGPSComb[,c(1:2,4,3,5:7)])

llGPSComb <- llGPSComb[!(llGPSComb$val=="A" | llGPSComb$val=="B"),]

for(i in 1:nrow(llGPSComb)){
  set.seed(1234)
  angle <- sample(1:360,1)
  if(llGPSComb$val[i] == "3"){
    llGPSComb$x[i] <- llGPSComb$x[i] + 250*cos(angle)
    llGPSComb$y[i] <- llGPSComb$y[i] + 250*cos(angle)
  } else if(llGPSComb$val[i] == "2"){
    llGPSComb$x[i] <- llGPSComb$x[i] + 500*cos(angle)
    llGPSComb$y[i] <- llGPSComb$y[i] + 500*cos(angle)
  } else if(llGPSComb$val[i] == "1"){
    llGPSComb$x[i] <- llGPSComb$x[i] + 1500*cos(angle)
    llGPSComb$y[i] <- llGPSComb$y[i] + 1500*cos(angle)
  } else if(llGPSComb$val[i] == "0"){
    llGPSComb$x[i] <- llGPSComb$x[i] + 5000*cos(angle)
    llGPSComb$y[i] <- llGPSComb$y[i] + 5000*cos(angle)
  } else{
    print(llGPSComb$val[i])
  }
  
}

llcoord <- SpatialPoints(llGPSComb[,3:4],proj4string=CRS("+proj=utm +zone=31 +datum=WGS84"))

utmcoord <- spTransform(llcoord,CRS(paste0("+proj=longlat  +datum=WGS84")))
llGPSComb[,3] <- attr(utmcoord,"coords")[,1]
llGPSComb[,4] <- attr(utmcoord,"coords")[,2]

llGPSComb <- llGPSComb %>% 
  rename(ID = 1, 
         time = 2, 
         lon = 3, 
         lat = 4,
         b = 5)
argosTracks <- llGPSComb[,c(1:2,4,3,5)]

save(argosTracks, file = "Argos/simulatedArgos.Rda")
