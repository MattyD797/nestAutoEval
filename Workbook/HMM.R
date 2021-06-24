#1. setup packages and environment

  #clean environment
  rm(list=ls(all=TRUE))

  #load constants and functions
  source("Functions/standard_functions.R")
  source("Constants/file_locations.R")
  
  #load in packages
  setUp(c("moveHMM", 
          "momentuHMM",
          "crawl",
          "dplyr",
          "tidyverse", 
          "rgdal", 
          "parallel"))
      
#2. Load in Data
  
  tracks <- readTracks(predicted_tracks)

#3. Create a xytb object
  
  #convert to UTM coordinates
  tracks_utm <- ll2utm(tracks, zone = 31)

  #create temporally-regular data
  crawlWrap(obsData = tracks_utm, 
            timeStep = "15 min", 
            ncores= (detectCores()-1), 
            retryParallel = TRUE,
            retryFits = 100,
            method = "Nelder-Mead",
            fixPar = c(NA, NA), 
            theta = c(0,0))
  
  