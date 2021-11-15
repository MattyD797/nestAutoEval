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

#RF generated predictions
load("predictions/RF.Rda")


# Persistence Check -------------------------------------------------------

#add continous behaviors in predictions
pred <- predictions %>% 
          group_split(id) %>% 
          lapply(addPersist)

ggplot(pred[[2]], aes(x= t, y = repeated, fill = .pred_class))+geom_bar(stat="identity")


unique(pred[[2]]$repeated[pred[[2]]$.pred_class  == 1])











