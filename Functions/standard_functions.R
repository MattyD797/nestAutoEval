#Standard setup of the workbook. Should be used in every script to clean environment
##Input: Packages to be installed and loaded into environment
  setUp <- function(x){
    #create function to load and install (missing) packages
    
    #install.packages("m2b")
    foo <- function(x){
      for( i in x ){
        #  require returns TRUE invisibly if it was able to load package
        if( ! require( i , character.only = TRUE ) ){
          #  If package was not able to be loaded then re-install
          install.packages( i , dependencies = TRUE )
          #  Load package after installing
          require( i , character.only = TRUE )
        }
      }
    }
    
    foo(x)
    
    options(max.print = 99)
    as.POSIXct(Sys.time(), origin = "1970-01-01")
  }

#Concatenates all tracks into one data frame to be easily interpreted by processing scripts
##Input: directory to files containing predicted tracks
  readTracks<-function(df){
    #locate all files in the directory
    files <- list.files(df)
    #Load all files as dfs into a list
    myfiles <- lapply(paste0(predicted_tracks,files), get.csv) %>% 
      bind_rows() %>% 
      rename("ID" = 1,
             "time" = 2, 
             "lat" = 3, 
             "lon" = 4, 
             "temp" = 5) %>% 
      na.omit()
    
    return(myfiles)
  }
  
  #Read individual track and modify to prepare for binding with rest of data
  ##Input: csv of individual bird track
    get.csv <- function(df){
      read.csv(df) %>%
        dplyr::select(-last_col()) %>%
        mutate(
          ID = as.character(ID),
          date_time = as.POSIXct(date_time, format="%m/%d/%Y %H:%M"),
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude),
          b = as.numeric(b)
        ) %>% 
        suppressWarnings()
    }
    
  #Take in all tracks and subset by lat and lon or utm of the nesting area
  ##Input: dataframe of tracks
    defNestArea<- function(x, minLat, minLon){
      x <- x %>% 
            filter(lat > minLat, 
                    lon > minLon)
      return(x)
    }
  
#Convert longlat coordinates to utm or visversa
##Input: xytb object
  ll2utm <- function(df, zone = 31, ellps = "WGS84", proj1 = "longlat", proj2 = "utm"){
    
    llcoord <- SpatialPoints(df[,3:4],
                             proj4string=CRS(paste0("+proj=", 
                                                    proj1, 
                                                    " +datum=", 
                                                    ellps)))
    
    utmcoord <- spTransform(llcoord,CRS(paste0("+proj=", 
                                                proj2, 
                                                " +zone=", 
                                                zone, 
                                                " +datum=", 
                                                ellps)))
    df[,3] <- attr(utmcoord,"coords")[,1]
    df[,4] <- attr(utmcoord,"coords")[,2]
    
    df <- df %>% 
      rename(x = 3, 
             y = 4)
    
    return(df)
  }



  
  
