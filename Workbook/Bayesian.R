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
        "tidymodels", "ggplot2", "lme4"))

#RF generated predictions
#predictions <- load("./predictions/RF.Rda")

#on my local machine
load("C:/Users/14064/Desktop/Local Loc/nestAutoEval/predictions/RF.Rda")
names(predictions)[3] <- "b"

predictions$b <- as.numeric(predictions$b)



#### 8. run the function to create matrices ####

# SETTINGS DESCRIPTION!! FOLLOW THESE GUIDELINES #
#Nest - best settings season.begin = "03-25", season.end = "08-20", period_length = 26
#Chick - best settings season.begin = "03-25", season.end = "09-20", period_length = 21

source("Functions/format_functions_v1.R"); build_matrices(RF_prediction=predictions, season.begin = "03-25", season.end = "09-20", period_length = 21, behavior_signal= "4")

matrices$mat_beh[c(1:5),c(1:10)]
matrices$mat_fix[c(1:5),c(1:10)]


#subset to those with complete incubation cycles
 mat_keep_rows <- c("2015-2014", "2016-2013", "2018-2014", "2002-2014", "2002-2015") #only for nest model

 # for nest
matrices$mat_beh_full <-  matrices$mat_beh[rownames(matrices$mat_beh) %in% mat_keep_rows, ] 
matrices$mat_fix_full <-  matrices$mat_fix[rownames(matrices$mat_fix) %in% mat_keep_rows, ] 

# for chick-tending
matrices$mat_beh_full <-  matrices$mat_beh 
matrices$mat_fix_full <-  matrices$mat_fix 


#### 9. predict survival from states ####
btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, visits = matrices$mat_beh_full, model = "phi_time_p_time", mcmc_params = list(burn_in = 1000, n_chain = 3, thin = 5, n_adapt = 1000, n_iter = 5000)) ; inferred_surv(btgo_outcomes)

# 
# 
# btgo_outcomes$z
# 
# 
# 
# #population levels
# plot_survival(btgo_outcomes)
# plot_detection(btgo_outcomes)
# 
# 
# print(plot_nest_surv(btgo_outcomes, who = 1))
# 
# #print all the plots of survival 
# for( i in c(1:5)){plot(plot_nest_surv(btgo_outcomes, who = i))}
# 
# # diagnostics
# btgo_pb0_coda <- coda::as.mcmc.list(btgo_outcomes$p.b0)
# btgo_pb1_coda <- coda::as.mcmc.list(btgo_outcomes$p.b1)
# 
# #looking for fuzzy caterpillars (good mixing of the 3 mcmc chains) and a simetrical distribution that fits the observed data (black dashes)
# plot(btgo_pb0_coda); plot(btgo_pb1_coda)


#### get outcome estimate - nesting ####

#process data
surv <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,3] #this is for the boxplot, dichotemy plot



ggplot() + geom_boxplot(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), y=surv), colour =  "grey40", outlier.alpha = 0.001) + geom_point(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), y=surv), na.rm=TRUE, position=position_jitter(width=.152, height = 0), colour = "purple3") + theme_classic()  + labs(x = "True Fate", y = "Pr(Survival|movement)")  + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12))  





#### get outcome estimate - chick tending ####

#process data
surv <- inferred_surv(btgo_outcomes, ci = .80)$outcomes #this is for the boxplot, dichotemy plot
out <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,c(5:7)] #for the scatterplot and line of best fit

Nest_Fates <- read_csv("fieldNotes/Nest_Fates.csv")

Nest_Fates <- Nest_Fates[ which(Nest_Fates$id %in% inferred_surv(btgo_outcomes, ci = .80)$outcomes[,1]),]

out_final <- cbind(Nest_Fates, out); out_final <- out_final[ which(out_final$LastDaySeenChickGuiding != "NA"),]

#find relationship
summary(lm(last_day_mean ~ DaysBroodAlive, data = out_final))

ggplot(surv) + geom_boxplot(aes(x = c("Unknown", "Fledged", "Fledged", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown","Unknown","Fledged","Unknown","Fledged","Unknown","Unknown","Fledged","Unknown","Fledged","Fledged"), y=pr_succ_mean), colour =  "grey40" , outlier.alpha = 0.001) + geom_point(aes(x = c("Unknown", "Fledged", "Fledged", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown","Unknown","Fledged","Unknown","Fledged","Unknown","Unknown","Fledged","Unknown","Fledged","Fledged"), y=pr_succ_mean), na.rm=TRUE, position=position_jitter(width=.12, height = 0), colour = "forestgreen") + theme_classic()  + labs(x = "True Fate", y = "Pr(Survival|movement)")  + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12))  

ggplot(out_final) + geom_pointrange(aes(x = DaysBroodAlive, y=last_day_mean, ymax = last_day_upr, ymin = last_day_lwr), na.rm=TRUE, position=position_jitter(width=.2, height = 0), colour = "forestgreen") + theme_classic() + labs(x = "Age at Last Field Observation", y = "Median Day Predicted Alive")  + coord_cartesian(ylim=c(0,32), xlim = c(0,36)) + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12)) + scale_x_continuous(expand = c(0,0), limits = c(-10,36), breaks = seq(0,36, by = 2)) + scale_y_continuous(expand = c(0,0), limits = c(-10,36), breaks = seq(-10,36, by = 2)) + geom_smooth(aes(x = DaysBroodAlive, y=last_day_mean), se=F, method = "lm", colour = "forestgreen")






