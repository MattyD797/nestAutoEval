# 1. setup packages and environment ---------------------------------------

#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R")
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


# 2. Load in Data for training -----------------------------------------------

tracks <- readTracks(predicted_tracks)

#xytb thinks birds are connected. Ran independently and combined later
tracks_split <- tracks %>% 
                  group_split(ID)

# 3. Create a xytb object for each track ------------------------------

xytbs <- lapply(tracks_split, tracks2xytb, desc="BTGO Birds", winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))


# 4. Combine xytb objects -------------------------------------------------

xytb <- bindXytbs(xytbs, tracks)

# 4. Train Model ----------------------------------------------------------

chick_rf <- modelRF(xytb, type = "actual", rfcv = TRUE, nob = "-1", colin = FALSE) 

resRF(chick_rf) #to view the out of bag error limit
resRF(chick_rf, "importance") # importance of individual variables
resRF(chick_rf,"confusion") # view the confusion matrix and the statistics by each class

# 5. Extract predictions --------------------------------------------------

predictions <- getPredictions(chick_rf)

# 6. Get the Julian Data -----------------------------------------------------

predictions <- predictions %>% 
                add_column(as.numeric(format(predictions$t, "%j"))) %>% 
                rename(Julian = 4)




#define bounds of the 
#Nesting - between 70 and 213
#chick tending - between 80 and 244

#fate
nest.beh <- predictions %>% group_by(id, Julian) %>% count(b)
nest.beh <- nest.beh[nest.beh$b==1,]


nest.beh.final <- matrix(nrow = length(unique(nest.beh$id)), ncol = 365)
rownames(nest.beh.final) <- c(unique(nest.beh$id))

for(i in 1:length(nest.beh$id)){
  nest.beh.final[match(nest.beh[i,1], pull(unique(nest.beh[,1]))),
                  as.numeric(nest.beh[i, 2])] <- as.numeric(nest.beh[i,4])
}
#fate matrix
nest.beh.final[is.na(nest.beh.final)] <- 0


#GPS fixes
gps.fixes <- predictions %>% group_by(id) %>% count(Julian)

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


colnames(nest.beh.final) <- NULL
rownames(nest.beh.final) <- NULL
colnames(gps.fixes.final) <- NULL
rownames(gps.fixes.final) <- NULL

#trim down to the field season (for some reason, these need to be > 70 days long)
nest.beh.final.trim <- nest.beh.final[,90:165]
gps.fixes.final.trim <- gps.fixes.final[,90:165]






#### predict survival from states ####


btgo_outcomes <- estimate_outcomes(gps.fixes.final.trim, nest.beh.final.trim, model = "phi_time_p_time")


#population levels
plot_survival(btgo_outcomes)
plot_detection(btgo_outcomes)


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








