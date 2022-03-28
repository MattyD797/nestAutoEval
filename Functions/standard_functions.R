
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
  readTracks<-function(df, minLat, minLon){
    #locate all files in the directory
    files <- list.files(df)
    #Load all files as dfs into a list
    myfiles <- lapply(paste0(df,files), get.csv) %>% 
      dplyr::bind_rows() %>% 
      rename("ID" = 1,
             "time" = 2, 
             "lat" = 3, 
             "lon" = 4, 
             "b" = 5) %>%
      filter(lat > minLat, 
             lon > minLon) %>% 
      na.omit()
    
    return(myfiles)
  }
  
  #Read individual track and modify to prepare for binding with rest of data
  ##Input: csv of individual bird track
    get.csv <- function(df){
      track <- read_csv(df) 
      
      if("ID" %in% colnames(track)){
        colnames(track)[which(colnames(track)=="ID")] <- "id"
      }
      if("timestamp" %in% colnames(track)){
        colnames(track)[which(colnames(track)=="timestamp")] <- "date_time"
      }
      
      track <- track %>% mutate(
        ID = as.character(id),
        date_time = as.POSIXct(date_time, format="%m/%d/%Y %H:%M"),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        b = as.numeric(b)
      ) %>% dplyr::select(c(ID,date_time,latitude, longitude,b))  %>% 
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
                              strata = actual, 
                              breaks = breaks)
    
    return(dt_split)
  }
  
  
#createForest----------------------------------------------------------------
  createForest<- function(pred_tracks,
                          minLat = 50,
                          minLon = 3,
                          drops = c(0,0,0,0),
                          desc = "BTGO Birds",
                          winsize = seq(3,15,2), 
                          idquant = seq(0,1,.25),
                          move = c(5,10,15),
                          prop = 8/10, 
                          breaks = 4,
                          nob = -1,
                          ntrees = 1000,
                          mtry = c(50, 100, 200, 250)
  ){
    #packages and setup--------------------------------------------------------
    
    #load in packages
    setUp(c("randomForest", 
            "m2b", 
            "moveHMM", 
            "momentuHMM", 
            "dplyr",
            "tidyverse", 
            "caret",
            "mlbench", 
            "nestr", 
            "coda", 
            "jagsUI", 
            "R2jags", 
            "runjags", 
            "rjags", 
            "tidymodels"))
    
    # Load Data----------------------------------------------------------------------------
    tracks <- data.frame()
    
    for(val in pred_tracks){
      tracks <- rbind(tracks,readTracks(val, minLat, minLon))
    }
    
    
    # Drop Preperation -------------------------------------------------------------
    tracks_split <- dropBeh(tracks, drops) %>% 
      group_split(ID)
    
    j<- 1
    tracks_split2 <- list()
    for (i in tracks_split){
      if (nrow(i)>move[3]){
        tracks_split2[[j]] <- i
        j <- j+1
      }
    }
    
    #xytb-------------------------------------------------------------------------
    xytbs <- lapply(tracks_split2, 
                    tracks2xytb, 
                    desc=desc, 
                    winsize=winsize, 
                    idquant=idquant,
                    move=move)
    xytb <- bindXytbs(xytbs, tracks)
    
    #Training Frame--------------------------------------------------------------
    dt_list <- xytb2RF(xytb, nob)
    
    dt_tracks <- dt_list[[1]]
    dt_clean <- dt_list[[2]]
    
    #Tidymodels workflow-------------------------------------------------------------
    # split data #
    
    dt_split <- splitData(dt_clean, prop, "actual", breaks = breaks)
    
    # define train and test set #
    
    dt_train <- training(dt_split)
    dt_test <- testing(dt_split)
    
    # specify cross validation data #
    
    dt_cv <- vfold_cv(dt_train)
    
    # define recipe #
    
    dt_recipe <- 
      recipe(dt_clean) %>%
      update_role(everything()) %>% 
      update_role(actual, new_role = "outcome") %>% 
      step_corr(all_predictors()) %>% 
      step_center(all_predictors(), -all_outcomes()) %>% 
      step_scale(all_predictors(), -all_outcomes())%>%
      prep()
    
    # define model #
    
    rf_model <- 
      # specify that the model is a random forest
      rand_forest() %>%
      # specify that the `mtry` parameter needs to be tuned
      set_args(mtry = tune(), ntrees = ntrees) %>%
      # select the engine/package that underlies the model
      set_engine("randomForest", importance = TRUE) %>%
      # choose either the continuous regression or classification
      set_mode("classification")
    
    # setup work flow ~ recipe and model#
    
    rf_workflow <- workflow() %>%
      # add the recipe
      add_recipe(dt_recipe) %>%
      # add the model
      add_model(rf_model)
    
    # create CV search grid #
    #~86 for GPS and 45 for Argos
    rf_grid <- expand.grid(mtry = mtry)
    
    
    print("Training model (may take time)...")
    
    # tune mtry #
    rf_tune_results <- rf_workflow %>%
      tune_grid(resamples = dt_cv, #CV object
                grid = rf_grid, # grid of values to try
                metrics = metric_set(yardstick::f_meas))
    
    # collect metrics #
    
    mtry_vals <- rf_tune_results %>%
      collect_metrics()
    
    mtry_vals
    
    # pick the best #
    param_final <- rf_tune_results %>%
      select_best(metric = "f_meas")
    
    # finalize workflow #
    
    rf_workflow <- rf_workflow %>%
      finalize_workflow(param_final)
    
    #Evaluate model---------------------------------------------------------------
    rf_fit <- rf_workflow %>%
      # fit on the training set and evaluate on test set
      last_fit(dt_split)
    
    # collect metrics and confusion chart #
    test_metrics <- rf_fit %>% collect_metrics()
    test_metrics
    test_predictions <- rf_fit %>% collect_predictions()
    f1 <- f_meas(test_predictions, actual, .pred_class)
    acc <- accuracy(test_predictions, actual, .pred_class)
    prec <- precision(test_predictions, actual, .pred_class)
    rec <- recall(test_predictions, actual, .pred_class)
    confm <- conf_mat(test_predictions, actual, .pred_class)
    
    all_metrics <- list(f1, acc, prec, rec, confm)
    
    #Final Model------------------------------------------------------------------
    final_model <- fit(rf_workflow, dt_clean)
    
    #Predict Tracks---------------------------------------------------------------
    rowsNA <- which(is.na(dt_tracks), arr.ind=TRUE)[,1] %>%  unique()
    predictionsRF <- predict(final_model, dt_tracks %>% na.omit())
    predictions <- cbind(xytb@b[-rowsNA, c("id", "t")], predictionsRF) %>% 
      rename(b = .pred_class)
    
    return(list(final_model, predictions, all_metrics))
  }
  
#GraphBeh--------------------------------------------------------------- 
  graphBeh <- function(track, ids=tracks[1,1]){
    test_track <- track[track$id==ids,]
    test_xts <- xts(test_track, order.by = test_track$t, tzone = )
    graph <- dygraph(test_xts, xlab = "time", ylab = "behavior") %>% dyOptions(stepPlot = TRUE)
    return(graph)
  }

#makePredictions---------------------------------------------------------------
  makePredictions <- function(model, dir, minLat = 50, minLon = 3, desc = "BTGO Birds",
                              winsize = seq(3,15,2), 
                              idquant = seq(0,1,.25),
                              move = c(5,10,15), 
                              nob = -1){
    
    #split tracks by ID for xytb object transformation
    splitTracks <- readTracks(dir, minLat, minLon) %>% 
      group_split(ID)
    
    #remove tracks with less than window size
    #advisable to be the same size used to train model
    j<- 1
    test2 <- list()
    for (i in splitTracks){
      if(nrow(i)>move[3]){
        test2[[j]] <- i
        j <- j+1
      }
    }
    
    #create xytb objects
    xytbs <- lapply(test2, 
                    tracks2xytb, 
                    desc=desc, 
                    winsize=winsize, 
                    idquant=idquant,
                    move=move)
    
    #bind into one xytb object
    xytb <- bindXytbs(xytbs, tracks)
    
    #filter out parameters
    dt_tracks <- cbind(xytb@b$b, xytb@befdxyt)
    colnames(dt_tracks)[1] <- "actual"
    
    #make predictions
    predictionsT <- predict(model, dt_tracks %>% na.omit())
    
    rowsNA <- which(is.na(dt_tracks), arr.ind=TRUE)[,1] %>%  unique()
    
    predictions <- cbind(xytb@b[-rowsNA, c("id", "t")], predictionsT) %>% 
      rename(b = .pred_class)
    
    return(predictions)
    
  }
  
