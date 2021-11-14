# Author: Luke Wilde
# Script to define functions for building matrices: first, check format, and second, build them
#

#!!! Error for chick-tending happening bc those with beh (n=19) and those with fixes (25) are different - fails to rename the rows !!!#
# rm(list=ls(all=TRUE))
# 
# #on my local machine
# load("C:/Users/14064/Desktop/Local Loc/nestAutoEval/predictions/RF.Rda")
# names(predictions)[3] <- "b"
# 
# predictions$b <- as.numeric(predictions$b)
# 
# 
# # RF_prediction = predictions; season.begin = "01-01"; season.end = "12-31"; period_length = 27; behavior_signal= "4"
# load("C:/Users/14064/Desktop/Local Loc/nestAutoEval/predictions/RF.Rda")
# names(predictions)[3] <- "b"
# 
# predictions$b <- as.numeric(predictions$b)
# 
# RF_prediction=predictions
# season.begin = "04-25"
# season.end = "09-20"
# period_length = 22
# behavior_signal= "4"
# min.occ = 2
# avg.fix.rate=.25

#### Function to construct the survival and observation matrices ####
build_matrices <- function(RF_prediction, season.begin = "01-01", season.end = "12-31", period_length = 27, behavior_signal= "1", min.occ = 2, avg.fix.rate=.25){ 
  
  # check input data
  # Check that all the fields are there
  if (any(!c(exists("id", where = RF_prediction),
             exists("t", where = RF_prediction),
             exists("b", where = RF_prediction)))) {
    
    stop("Either data does not include required fields or column names are different.
         Check that gps_data includes id, t, and b.")   }
    
  #check classes  
  if (!(class(RF_prediction$id) == "character")) stop("id column needs to be character")
  if (!(class(RF_prediction$b) == "numeric")) stop("b column needs to be numeric")
  if (!(inherits(RF_prediction$t, "POSIXct"))) stop("t needs to be 'POSIXct' format")
  if (sum(is.na(RF_prediction$b)) > 0) stop("please exclude rows where date is NA")
  



    #extract Julian day
    Julian <- as.numeric(format(RF_prediction$t, "%j"))
    RF_prediction <- cbind(RF_prediction, Julian)
    
    #count the number of times each behavior occurs each day
    beh <- RF_prediction %>% group_by(id, Julian) %>% count(b)
    
    #find the days that the behavior of interest occurs
    beh <- beh[beh$b==as.numeric(behavior_signal),]
    max(beh$n) #DIAGNOSTIC: this cannot be above 96 for a 15 min fix rate, or 144 for a 10 min
    
    beh[(beh$n > (.90*(24/avg.fix.rate))),"n"] <- 0 # put this in bc multiple indiv had impossibly high values for the observed behavior (101, 150 per day); happened bc of irregular fix rates and all behaviors those days being classified as the behavior of interest.
    
    #create empty fate matrix
    mat_beh <- matrix(NA, nrow = length(unique(beh$id)), ncol = 365)
    
    #loop through individuals
    for(i in 1:length(beh$id)){
      mat_beh[match(beh[i,1], pull(unique(beh[,1]))),
              as.numeric(beh[i, 2])] <- as.numeric(beh[i,4])
    }
    
    #replace NA's, which are days where no occurences of behavior were observed, with 0s
    mat_beh[is.na(mat_beh)] <- 0
    
    mat_beh[which(mat_beh < min.occ)] <- 0
    
    for(i in 1:nrow(mat_beh)){ tmp <-mat_beh[i,max(which(mat_beh[i,] > as.numeric(min.occ)))]}

    tmp

    #apply function reads through the matrix containing positive values, and locates the column that has the maximum value

    apply(mat_beh>0,1,which.max) #DIAGNOSTIC: reports where the max occurs for each row
    apply(mat_beh>0,2,which.max) #DIAGNOSTIC: reports where the max occurs for each column

    max.col(t(mat_beh >0), "last") #DIAGNOSTIC
    
    
    
    #GPS fixes
    
    fixes <- RF_prediction %>% group_by(id) %>% count(Julian)
    
    fixes <- fixes[(fixes$id %in% beh$id),]
    
    #create blank matrix to fill
    mat_fix <- matrix(NA, nrow = length(unique(fixes$id)), ncol = 365)
    
    
    for(i in 1:length(fixes$id)){
      mat_fix[match(fixes[i,1], pull(unique(fixes[,1]))),
              as.numeric(fixes[i, 2])] <- as.numeric(fixes[i,3])
    }
    
    #GPS_fix_matrix
    mat_fix[is.na(mat_fix)] <- 0
    
    
    #
    nrow(mat_fix); nrow(mat_beh)
    
    colnames(mat_fix) <- colnames(mat_beh) <- NULL
    
    rownames(mat_fix) <- rownames(mat_beh) <- c(unique(beh$id))
    
    
    #convert to POSIXct
    season.begin_fmt <- as.POSIXct(season.begin, format = "%m-%d")
    season.end_fmt <- as.POSIXct(season.end, format = "%m-%d")
    
    
    ## subset matrices to season length ##
    
    mat_beh <- mat_beh[,as.numeric(format(season.begin_fmt, "%j")):as.numeric(format(season.end_fmt, "%j"))]
    
    mat_fix <- mat_fix[,as.numeric(format(season.begin_fmt, "%j")):as.numeric(format(season.end_fmt, "%j"))]
    
    
    #identify first non-zero value in each row
    tmp_start <- as.vector(apply(mat_beh, 1, function(x) which(x!=0, arr.ind=T)))
    
    #convert to list, then vector
    lst <- list(NA,nrow(mat_beh))
    
    for(i in 1:nrow(mat_beh)){
      lst[i] <- min(tmp_start[[i]])
    }
    
    tmp_start <- as.vector(do.call(rbind, lst)) 
    
    tmp_start[which(!is.finite(tmp_start))] <- as.numeric(format(season.begin_fmt, "%j"))
    
    tmp_end <- tmp_start + period_length
    
    tmp_start
    tmp_end
    
    lst <- list(NA,nrow(mat_beh))
    #use each to subset the matrices
    for(i in 1:nrow(mat_beh)){
      lst[[i]] <- mat_beh[i,c(tmp_start[i]:tmp_end[i])]
    }
    
    mat_beh_final <- as.matrix(do.call(rbind, lst))
    
    lst <- list(NA,nrow(mat_beh))
    for(i in 1:nrow(mat_beh)){
      lst[[i]] <- mat_fix[i,c(tmp_start[i]:tmp_end[i])] 
    }
    
    mat_fix_final <- as.matrix(do.call(rbind, lst))
    
    #set rownames back
    rownames(mat_fix_final) <- rownames(mat_beh_final) <- rownames(mat_beh)
    
    #rename for ease
    mat_fix <- mat_fix_final
    mat_beh <- mat_beh_final
    matrices <<- list(mat_fix, mat_beh)
    names(matrices) <<- c("mat_fix", "mat_beh")
    
  
 
  
}



#
estimate_outcomes_LRW <- function(fixes,
                              visits,
                              model = "null", jags_file_folder = "C:/Users/14064/Dropbox/BTGO Movmement Study/nestR-master/nestR-master/inst/jags",
                              mcmc_params = list(burn_in = 1000,
                                                 n_chain = 2,
                                                 thin = 5,
                                                 n_adapt = 1000,
                                                 n_iter = 10000)){
  
  # Select the correct JAGS file for the model
  model_txt <- dplyr::case_when(
    model == "null" ~ "nest_outcome_null.txt",
    model == "phi_time" ~ "nest_outcome_phi_time.txt",
    model == "p_time" ~ "nest_outcome_p_time.txt",
    model == "phi_time_p_time" ~ "nest_outcome_phi_time_p_time.txt",
    model == "null_indiv" ~ "nest_outcome_null_RandEffIndiv.txt",
    model == "phi_time_indiv" ~ "nest_outcome_phi_time_RandEffIndiv.txt",
    model == "p_time_indiv" ~ "nest_outcome_p_time_RandEffIndiv.txt",
    model == "phi_time_p_time_indiv" ~ "nest_outcome_phi_time_p_time_RandEffIndiv.txt",
    model == "null_indiv_year" ~ "nest_outcome_null_RandEffIndiv_RandEffYear.txt",
    model == "phi_time_indiv_year" ~ "nest_outcome_phi_time_RandEffIndiv_RandEffYear.txt",
    model == "p_time_indiv_year" ~ "nest_outcome_p_time_RandEffIndiv_RandEffYear.txt",
    model == "phi_time_p_time_indiv_year" ~ "nest_outcome_phi_time_p_time_RandEffIndiv_RandEffYear.txt"
  )
  
  # Path to the JAGS file
  jags_file <- file.path(jags_file_folder,  model_txt)
  
  # Starting values for survival status
  s1 <- initialize_z_LRW(ch = visits)
  
  # Define JAGS model
  jags <- rjags::jags.model(file = jags_file,
                            data = list("nests" = nrow(visits),
                                        "days" = ncol(visits),
                                        "gps_fixes" = fixes,
                                        "y" = visits),
                            inits = list("z" = s1),
                            n.chain = mcmc_params$n_chain,
                            n.adapt = mcmc_params$n_adapt)
  
  #Run the burn-in
  rjags:::update.jags(object = jags, n.iter = mcmc_params$burn_in)
  
  #Generate posterior samples
  post <- rjags::jags.samples(model = jags,
                              variable.names = c("phi.b0", "phi.b1", "phi", "p.b0", "p.b1", "p", "z", "sigma2"),
                              n.iter = mcmc_params$n_iter,
                              thin = mcmc_params$thin)
  
  # Add the names to the list 'post'
  post$names <- row.names(fixes)
  
  # Add the model type
  post$model <- model
  
  return(post)
  
}



#build latent variable - a partially observed variable representing the state
# ch = matrices$mat_beh_full

initialize_z_LRW <- function(ch = visits) {
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch #
  
  # Loop through each nest
  for (i in 1:nrow(ch)) {
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1
    
    # The last sighting is the last time the animal was observed at the nest
    n2 <- if( sum(ch[i,]) > 0) {print(max(which(ch[i,] > 0)))} else {print(2)}
    
    # Set all states between first and last to 1
    state[i, n1:n2] <- 1
    
    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }
  
  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state == 0] <- NA
  # 

  # 
  # Return
  return(state)
}



# tmp <- which(fixes == 0, arr.ind=TRUE); colnames(tmp) <- NULL; rownames(tmp) <- NULL
# 
# for(i in 1:nrow(tmp)){
#   
#   state[tmp[i,1],tmp[i,2]] <<- NA
#   
# }


inferred_surv <- function(mcmc_object, animals = matrices$mat_fix_full, ci = 0.95){
  
  # Initialize list for output
  out <- list()  
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  ### Population-level survival
  
  # If the model had time-varying phi, report the slope and intercept
  if (grepl("phi_time", mcmc_object$model)){
    
    # Note that these parameters are on logit scale
    out$phi <- data.frame(b0_lwr = apply(mcmc_object$phi.b0, 1, quantile, lwr),
                          b0_mean = apply(mcmc_object$phi.b0, 1, mean),
                          b0_upr = apply(mcmc_object$phi.b0, 1, quantile, upr),
                          b1_lwr = apply(mcmc_object$phi.b1, 1, quantile, lwr),
                          b1_mean = apply(mcmc_object$phi.b1, 1, mean),
                          b1_upr = apply(mcmc_object$phi.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$phi <- data.frame(lwr = quantile(mcmc_object$phi, lwr),
                          mean = mean(mcmc_object$phi),
                          upr = quantile(mcmc_object$phi, upr))
    row.names(out$phi) <- NULL
    
  }
  
  ### Population-level detection
  
  if (grepl("p_time", mcmc_object$model)){
    
    # Note that these parameters are on logit scale
    out$p <- data.frame(b0_lwr = apply(mcmc_object$p.b0, 1, quantile, lwr),
                        b0_mean = apply(mcmc_object$p.b0, 1, mean),
                        b0_upr = apply(mcmc_object$p.b0, 1, quantile, upr),
                        b1_lwr = apply(mcmc_object$p.b1, 1, quantile, lwr),
                        b1_mean = apply(mcmc_object$p.b1, 1, mean),
                        b1_upr = apply(mcmc_object$p.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$p <- data.frame(lwr = quantile(mcmc_object$p, lwr),
                        mean = mean(mcmc_object$p),
                        upr = quantile(mcmc_object$p, upr))
    row.names(out$p) <- NULL
    
  }
  
  
  ### Individual-level survival
  
  indiv <- data.frame(animal = as.factor(dput(as.character(rownames(animals)))),
                      pr_succ_lwr = NA,
                      pr_succ_mean = NA,
                      pr_succ_upr = NA,
                      last_day_lwr = NA,
                      last_day_mean = NA,
                      last_day_upr = NA)
  
  # Probability burst was a successful nest (survived to last day)
  # Get the last day for each burst + iteration + chain
  last_day <- apply(mcmc_object$z, c(1,3,4), getElement, ncol(mcmc_object$z))
  
  #get values
  indiv$pr_succ_lwr <- apply(last_day, 1, quantile, lwr)
  indiv$pr_succ_mean <- apply(last_day, 1, mean)
  indiv$pr_succ_upr <- apply(last_day, 1, quantile, upr)
  
  # Latest day that a nest survived to
  latest_day <- apply(mcmc_object$z, c(1, 3, 4), sum)
  
  # get values 
  indiv$last_day_lwr <- apply(latest_day, 1, quantile, lwr)
  indiv$last_day_mean <- apply(latest_day, 1, mean)
  indiv$last_day_upr <- apply(latest_day, 1, quantile, upr)
  
  # Add to output list
  out$outcomes <- indiv
  
  return(out)
  
}
