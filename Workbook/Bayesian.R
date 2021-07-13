# 1. setup packages and environment ---------------------------------------

#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R")
source("Functions/format_functions.R")
source("Constants/file_locations.R")

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
build_matrices(RF_prediction = predictions, season.begin = "03-25", season.end = "08-20", period_length = 24, behavior_signal= "1")

matrices$mat_beh


#subset to those with complete incubation cycles
mat_keep_rows <- c("2015-2014", "2016-2013", "2018-2014", "2002-2014", "2002-2015")

matrices$mat_beh_full <-  matrices$mat_beh[rownames(matrices$mat_beh) %in% mat_keep_rows, ] 
matrices$mat_fix_full <-  matrices$mat_fix[rownames(matrices$mat_fix) %in% mat_keep_rows, ] 

matrices$mat_beh_full
matrices$mat_fix_full

#### 9. predict survival from states ####
btgo_outcomes <- nestR::estimate_outcomes(matrices$mat_fix, matrices$mat_beh, model = "phi_time_p_time", mcmc_params = list(burn_in = 1000, n_chain = 3, thin = 5, n_adapt = 1000, n_iter = 5000))

btgo_outcomes$z


#population levels
plot_survival(btgo_outcomes)
plot_detection(btgo_outcomes)


print(plot_nest_surv(btgo_outcomes, who = 5))

#print all the plots of survival 
for( i in c(1:5)){plot(plot_nest_surv(btgo_outcomes, who = i))}

# diagnostics
btgo_pb0_coda <- coda::as.mcmc.list(btgo_outcomes$p.b0)
btgo_pb1_coda <- coda::as.mcmc.list(btgo_outcomes$p.b1)

#looking for fuzzy caterpillars (good mixing of the 3 mcmc chains) and a simetrical distribution that fits the observed data (black dashes)
plot(btgo_pb0_coda); plot(btgo_pb1_coda)


#### get outcome estimate ####

inferred_surv <- function(mcmc_object, ci = 0.95){

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

indiv <- data.frame(animal = as.factor(dput(as.character(unique(predictions$id)))),
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


inferred_surv(btgo_outcomes)





#### CHICK TENDING #########

#### extract encounter and GPS matrices from predicted behaviors ####

# summarize predictions df
str(predicted_full)


#extract julian day
Julian <- as.numeric(format(predicted_full$t, "%j"))
predicted_full <- cbind(predicted_full, Julian)

unique(predicted_full$id)
#define bounds of the 
#Nesting - between 70 and 213
#chick tending - between 80 and 244

#fate
chick.beh <- predicted_full %>% group_by(id, Julian) %>% count(b)
chick.beh <- chick.beh[chick.beh$b==4,]


chick.beh.final <- matrix(nrow = length(unique(chick.beh$id)), ncol = 365)
rownames(chick.beh.final) <- c(unique(chick.beh$id))

for(i in 1:length(chick.beh$id)){
  chick.beh.final[match(chick.beh[i,1], pull(unique(chick.beh[,1]))),
                 as.numeric(chick.beh[i, 2])] <- as.numeric(chick.beh[i,4])
}
#fate matrix
chick.beh.final[is.na(chick.beh.final)] <- 0


#GPS fixes
gps.fixes <- predicted_full %>% group_by(id) %>% count(Julian)

gps.fixes.final <- matrix(nrow = length(unique(gps.fixes$id)), ncol = 365)
rownames(gps.fixes.final) <- c(unique(gps.fixes$id))



for(i in 1:length(gps.fixes$id)){
  gps.fixes.final[match(gps.fixes[i,1], pull(unique(gps.fixes[,1]))),
                  as.numeric(gps.fixes[i, 2])] <- as.numeric(gps.fixes[i,3])
}

#GPS_fix_matrix
gps.fixes.final[is.na(gps.fixes.final)] <- 0




# gps.fixes.df <- as.data.frame(gps.fixes)
# head(gps.fixes.df)


#


colnames(chick.beh.final) <- NULL
rownames(chick.beh.final) <- NULL
colnames(gps.fixes.final) <- NULL
rownames(gps.fixes.final) <- NULL

#trim down to the field season (for some reason, these need to be > 70 days long)
chick.beh.final.trim <- chick.beh.final[,90:230]
gps.fixes.final.trim <- gps.fixes.final[,90:230]






#### predict survival from states ####


btgo_outcomes <- estimate_outcomes(gps.fixes.final.trim, chick.beh.final.trim, model = "phi_time")


#population levels
plot_survival(btgo_outcomes)
plot_detection(btgo_outcomes)


btgo_outcomes$z

#print all the plots of survival 
for( i in 1:nrow(gps.fixes.final.trim)){plot(plot_nest_surv(btgo_outcomes, who = i))}

# diagnostics
btgo_pb0_coda <- coda::as.mcmc.list(btgo_outcomes$p.b0) #checking intercept of observation process
btgo_pb1_coda <- coda::as.mcmc.list(btgo_outcomes$p.b1) #checking slope of observation process

#looking for fuzzy caterpillars (good mixing of the 3 mcmc chains) and a simetrical distribution that fits the observed data (black dashes)
plot(btgo_pb0_coda); plot(btgo_pb1_coda)


#### get outcome estimate ####

inferred_chick_surv <- function(mcmc_object, ci = 0.95){
  
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
  
  indiv <- data.frame(animal = as.factor(dput(as.character(unique(predicted_full$id)))),
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


inferred_chick_surv(btgo_outcomes)








