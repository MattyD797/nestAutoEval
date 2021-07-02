#1. setup packages and environment

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
        "rjags"))


#2. Load in Data

tracks <- readTracks(predicted_tracks) 

#3. Create a xytb object from loaded tracks
xytb <- tracks2xytb(tracks, desc="BTGO Birds", winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))

#4. model with random forest
chick_rf <- modelRF(xytb, type = "actual", nob = "-1", ntree = 3201, mtry = 40) 



resRF(chick_rf) #to view the out of bag error limit
resRF(chick_rf, "importance") # importance of individual variables
resRF(chick_rf,"confusion") # view the confusion matrix and the statistics by each class

#behavioral states as predicted, vs the one observed - compared in time
#resB(chick_rf, nob="-1") 

# and then viewed in space
#resB(chick_rf, nob="-1", "space") 

# finally density - isnt working
#resB(chick_rf,"density", nob="-1") 

# need to export and predict from trained model (above) on new data for which we have annotations. Testing the model.
predicted_full <- chick_rf@predb




# to extract the model and make predictions without the need to merge birds together


#call test data, using similar to above
test.data <- do.call('rbind',  myfiles[c(7,8,17,18,21)]) #birds with entire tracks

#process as before, convert to xytb object
str(test.data)
test.data_1 <- test.data[,c(4,3,6,5,1)]; str(test.data_1); head(test.data_1)

names(test.data_1)[1] <- "x"
names(test.data_1)[2] <- "y"
names(test.data_1)[3] <- "t"
names(test.data_1)[4] <- "b"
names(test.data_1)[5] <- "id"

str(test.data_1)
head(test.data_1);tail(test.data_1)

test.data_1$x <- as.numeric(test.data_1$x)
test.data_1$y <- -1*as.numeric(test.data_1$y)
test.data_1$b <- -1
test.data_1$b <- as.character(test.data_1$b)
test.data_1$id <- as.character(test.data_1$id)
test.data_1$t <- as.POSIXct(test.data_1$t, format = "%m/%d/%Y %H:%M", origin = "1970-01-01") #for some reason, the t column lost the seconds

str(test.data_1); head(test.data_1); tail(test.data_1)
unique(test.data_1$id)

test.data_1 <- test.data_1[complete.cases(test.data_1),]

test.data_1 <- test.data_1[!duplicated(test.data_1$t),] 


xytb.test <- xytb(test.data_1, desc="BTGO Birds",winsize=seq(3,15,2), idquant=seq(0,1,.25),move=c(5,10,15))

xytb.test@befdxyt

xytb.test.pred <- cbind(xytb.test@b,xytb.test@dxyt,stringsAsFactors=F)
xytb.test.pred$t

chick_modRF <- extractRF(chick_rf)

xytb.test.pred <- na.omit(xytb.test.pred)

predictions_exported <- predict(chick_modRF, newdata = xytb.test.pred)

predictions_exported_df <- as.data.frame(predictions_exported)
time <- as.data.frame(xytb.test.pred$t) 
animal <- as.data.frame(xytb.test.pred$id)

predictions <- cbind("id"=animal,"t"=time,"b"=predictions_exported_df, stringsAsFactors=F)
predictions$`xytb.test.pred$t`

predictions <- data.frame("id" = predictions$`xytb.test.pred$id`, "t"= predictions$`xytb.test.pred$t`,"b" = predictions$predictions_exported)

#class(xytb.test@dxyt)


### TO DO ###
#2. need to look at what is going on with the last_day math
#3. calculate days between first and last? i.e. incubation period or fledging period
#4. calculate probability of survival since start - in nesting 22–24 days, in chick tending 28-34 days (Cite: https://doi-org.pallas2.tcl.sc.edu/10.2173/bow.bktgod.01)
### -- -- ###






#### extract encounter and GPS matrices from predicted behaviors ####

# summarize predictions df
str(predictions)


#extract julian day
Julian <- as.numeric(format(predictions$t, "%j"))
predictions <- cbind(predictions, Julian)

unique(predictions$id)
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








