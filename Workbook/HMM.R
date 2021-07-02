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
  
#3. Define nesting tracks
  
  nestTracks <- defNestArea(tracks, minLat = 50, minLon = 3)
  
#4. Create a momentuHMM data object
  
  #convert to UTM coordinates
  tracks_utm <- ll2utm(nestTracks, zone = 31)

  #create temporally-regular data
  crwOut <- crawlWrap(obsData = tracks_utm, 
                      timeStep = "15 min", 
                      ncores= detectCores()/2,
                      retryFits = 10,
                      method = "Nelder-Mead",
                      fixPar = c(NA, NA), 
                      theta = c(0,0))
  
  #finally create momentuHMM data
  birdData <- prepData(data=crwOut)
  
  # add cosinor covariate based on hour of day
  birdData$hour <- as.integer(strftime(birdData$time, format = "%H", tz="GMT"))
  
#5. Find starting parameters
  #a. step length (mean and standard deviation)
    plot(birdData)
  
#6. Fitting Model
  
    #a. label state names
      stateNames <- c("incubating", "foraging", "chicktending", "migrating")
    
    #b. define distributions
      dist <- list(step = "gamma", angle = "vm")
      
    #c. define intial parameters
      Par0_m1 <- list(step=c(.1, 1, 5, 10, .5, 2, 5, 1000), angle = c(1, 0.2,0.2, 3))
      
    #d. fit model
      m1 <- fitHMM(data = birdData, nSims = 100,  
                     nbStates = 4, dist = dist, Par0 = Par0_m1,retryFits = 5, 
                   estAngleMean = list(angle=FALSE), stateNames = stateNames)

#7.Find predicted states
      states <- viterbi(m1)
      
      table(states)/nrow(birdData)
      
      
      
      
      
  