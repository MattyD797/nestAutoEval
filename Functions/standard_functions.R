
# setUp -------------------------------------------------------------------

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
    
    options(max.print = 999)
    as.POSIXct(Sys.time(), origin = "1970-01-01")
    
   #
    
  }

# readTracks --------------------------------------------------------------

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
             "b" = 5) %>% 
      na.omit()
    
    return(myfiles)
  }
  
  #Read individual track and modify to prepare for binding with rest of data
  ##Input: csv of individual bird track
    get.csv <- function(df){
      track <- read.csv(df) %>%
        dplyr::select(-last_col()) %>%
        mutate(
          ID = as.character(ID),
          date_time = as.POSIXct(date_time, format="%m/%d/%Y %H:%M"),
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude),
          b = as.numeric(b)
        ) %>% 
        suppressWarnings()
      
      return(track[!duplicated(track$date_time),])
    }
    
  #Take in all tracks and subset by lat and lon or utm of the nesting area
  ##Input: dataframe of tracks
    defNestArea<- function(x, minLat, minLon){
      x <- x %>% 
            filter(lat > minLat, 
                    lon > minLon)
      return(x)
    }

# ll2utm ------------------------------------------------------------------

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

# tracks2xytb -------------------------------------------------------------

#Create a xytb object from the imported tracks
#Input from read tracks with proper labels(id, t, x, y, b)
  tracks2xytb <- function(tracks, desc, winsize, idquant, move){
    tracks <- tracks %>% 
                rename(id = ID, 
                        x = lon,
                        y= lat, 
                        t = time) %>% 
                mutate(id = as.character(id),
                       x = as.numeric(x),
                       y = as.numeric(y),
                       b = as.character(b),
                       t = as.POSIXct(t)) %>% 
      xytb(desc, winsize, idquant, move)
    return(tracks)
  }
  

# getPredictions ----------------------------------------------------------
  
#Extract predicitons from trained RF xytb object
##Input: a trained xytb object
  getPredictions <- function(x){
    return(x@predb)
  }
  

# bindXytbs ---------------------------------------------------------------

#Bind together a list of xytb objects
##Input: a list of xytb objects
  bindXytbs <- function(x, origTracks){
    xyt <- data.frame()
    for(val in 1:length(x)){
      xyt<-rbind(xyt, x[[val]]@xyt)
    }
    
    b <- data.frame()
    for(val in 1:length(x)){
      b <-rbind(b, x[[val]]@b)
    }
    
    dxyt <- data.frame()
    for(val in 1:length(x)){
      dxyt <-rbind(dxyt, x[[val]]@dxyt)
    }
    
    befdxyt <- data.frame()
    for(val in 1:length(x)){
      befdxyt <-rbind(befdxyt, x[[val]]@befdxyt)
    }
    
    xytb <- tracks2xytb(origTracks, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))
    
    xytb@xyt <- xyt
    xytb@b <- b
    xytb@dxyt <- dxyt
    xytb@befdxyt <- befdxyt
    
    return(xytb)
  }


# balanceBeh & dropBeh ----------------------------------------------------

#Balance known behaviors
##Input: track data with predicted behaviors
  balanceBeh <- function(x){
    behTable <- table(x$b)
    minVal <- behTable %>% min()
    set.seed(1234)
    behTable
    for(val in 2:length(behTable)){
      indexes <- sample(which(x$b == names(behTable)[val]), (behTable[val]-minVal), replace = FALSE)
      x$b[indexes] <- -1
    }
    return(x)
  }
  
  #Drop out known behaviors
  ##Input: track data with predicted behaviors
  dropBeh <- function(x, prop = c(0.2, 0.2, 0.2, 0.2)){
    behTable <- table(x$b)
    set.seed(1234)
    behTable
    for(val in 2:length(behTable)){
      indexes <- sample(which(x$b == names(behTable)[val]), behTable[val]*prop[val-1], replace = FALSE)
      x$b[indexes] <- -1
    }
    return(x)
  }


# xytb2RF -----------------------------------------------------------------

#Create xytb object into dataframe interpretable for the tidymodels workflow
##Input: xytb object, nob: no behavior observation factor
  xytb2RF <- function(xytb, nob){
    #take labeled behaviors
    dt_tracks <- cbind(xytb@b$b, xytb@befdxyt)
    colnames(dt_tracks)[1] <- "actual"
    
    #create dataframe for RF training
    dt_clean <- dt_tracks[which(dt_tracks$actual != nob),] %>% 
      drop_na() 
    dt_list <- list(dt_tracks, dt_clean)
    return(dt_list)
  }
  

# tidymodels --------------------------------------------------------------

#Split data into training and test sets
##Input: cleaned data 
  splitData <- function(dt_clean, prop, strata = NULL, breaks = 4){
    
    #Set random split
    set.seed(1234)
    
    #split the data prop train:test
    dt_split <- initial_split(dt_clean, 
                              prop = prop,
                              strata = all_of(strata), 
                              breaks = breaks)
    
    return(dt_split)
  }
