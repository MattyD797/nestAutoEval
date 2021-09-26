# 1. set#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R") 
source("Functions/format_functions_v1.R")
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
        "tidymodels"))

#RF generated predictions
#predictions <- load("./predictions/RF.Rda")

#on my local machine
load("C:/Users/14064/Desktop/Local Loc/nestAutoEval/predictions/RF.Rda")
names(predictions)[3] <- "b"

predictions$b <- as.numeric(predictions$b)


#### 8. run the function to create matrices ####
source("Functions/format_functions_v1.R"); build_matrices(RF_prediction=predictions, season.begin = "03-25", season.end = "08-20", period_length = 23, behavior_signal= "1")

matrices$mat_beh
matrices$mat_fix

# matrices$mat_beh[matrices$mat_beh < 10] <- 0



# tmp <- which(matrices$mat_fix == 0, arr.ind=TRUE); colnames(tmp) <- NULL; rownames(tmp) <- NULL
# 
# for(i in 1:nrow(tmp)){
#   
#   matrices$mat_beh[tmp[i,1],tmp[i,2]] <- NA
#   
# } ; matrices$mat_beh

#subset to those with complete incubation cycles
mat_keep_rows <- c("2015-2014", "2016-2013", "2018-2014", "2002-2014", "2002-2015")

matrices$mat_beh_full <-  matrices$mat_beh[rownames(matrices$mat_beh) %in% mat_keep_rows, ] 
matrices$mat_fix_full <-  matrices$mat_fix[rownames(matrices$mat_fix) %in% mat_keep_rows, ] 

matrices$mat_beh_full
matrices$mat_fix_full

initialize_z_LRW <- function(ch = visits) {
  # Initialize state using the "capture history" (in CMR parlance)
  state <- ch #

  # Loop through each nest
  for (i in 1:nrow(ch)) {
    # The earliest "sighting" will always be the first day of the attempt
    n1 <- 1

    # The last sighting is the last time the animal was observed at the nest
    n2 <- max(which(ch[i,] > 0))

    # Set all states between first and last to 1
    state[i, n1:n2] <- 1

    # Reset first to NA (because always see them on first day by definition)
    state[i, n1] <- NA
  }

  # Now set any states remaining as 0 to NA so that JAGS will estimate them
  state[state == 0] <- NA

  # tmp <- which(ch == 0, arr.ind=TRUE)
  # 
  # for(i in 1:nrow(tmp)){
  # 
  #   state[tmp[i,1],tmp[i,2]] <- NA
  # 
  # }
  #
  # Return
  return(state)
}

initialize_z_LRW(ch = matrices$mat_beh_full)

#matrices$mat_fix_full[4,c(19:24)] <- c(15,34,62,244,24,48)

#### 9. predict survival from states ####
btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, visits = matrices$mat_beh_full, model = "phi_time_p_time", mcmc_params = list(burn_in = 1000, n_chain = 3, thin = 5, n_adapt = 1000, n_iter = 5000)) ; inferred_surv(btgo_outcomes)



btgo_outcomes$z



#population levels
plot_survival(btgo_outcomes)
plot_detection(btgo_outcomes)


print(plot_nest_surv(btgo_outcomes, who = 1))

#print all the plots of survival 
for( i in c(1:5)){plot(plot_nest_surv(btgo_outcomes, who = i))}

# diagnostics
btgo_pb0_coda <- coda::as.mcmc.list(btgo_outcomes$p.b0)
btgo_pb1_coda <- coda::as.mcmc.list(btgo_outcomes$p.b1)

#looking for fuzzy caterpillars (good mixing of the 3 mcmc chains) and a simetrical distribution that fits the observed data (black dashes)
plot(btgo_pb0_coda); plot(btgo_pb1_coda)


#### get outcome estimate ####

inferred_surv(btgo_outcomes)

















