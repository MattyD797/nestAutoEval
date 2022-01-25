# 1. set#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R") 
source("Functions/format_functions_v2.1.R")
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
load("./predictions/RF.Rda")
names(predictions)[3] <- "b"

predictions$b <- as.numeric(predictions$b)
# Aug 26, tag 2015-2013 = 101 (looked at it, every location was classified as chick tending for the entire day); April 29, 2015-2014 = 150 (this one had the same issue, plus irregular fix rates between 2-15 min, mostly 5)


#### 8. run the function to create matrices ####

# SETTINGS DESCRIPTION!! FOLLOW THESE GUIDELINES #
#
#Chick - best settings season.begin = "05-10", season.end = "10-20", period_length = 26, behavior_signal= "3", min.occ = 16

unique(predictions$b)

build_matrices(RF_prediction=predictions, 
               season.begin = "05-10", 
               season.end = "10-20", 
               period_length = 26, 
               behavior_signal= "3", 
               min.occ = 16)

# for chick-tending
matrices$mat_beh_full <-  matrices$mat_beh 
matrices$mat_fix_full <-  matrices$mat_fix 

matrices$mat_beh_full[which(rownames(matrices$mat_beh_full) %in% 
                                    c("2004-2013","2020-2013","2041-2013",
                                      "2012-2013","2023-2013","1008-2013",
                                      "1009-2013","2017-2013","2019-2013")),]

matrices$mat_beh_full[which(!rownames(matrices$mat_beh_full) %in% 
                                    c("2004-2013","2020-2013","2041-2013",
                                      "2012-2013","2023-2013","1008-2013",
                                      "1009-2013","2017-2013","2019-2013")),]


btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, 
                                       visits = matrices$mat_beh_full, 
                                       model = "phi_time_p_time_indiv", 
                                       mcmc_params = list(burn_in = 1000,
                                                          n_chain = 3, 
                                                          thin = 5, 
                                                          n_adapt = 1000,
                                                          n_iter = 3000))

#inferred_surv(btgo_outcomes)

Nest_Fates <- read_csv("fieldNotes/Nest_Fates_1.csv")

Nest_Fates <- Nest_Fates[ which(Nest_Fates$id %in% 
                                        inferred_surv(btgo_outcomes, 
                                                                 ci = .80)$outcomes[,1]),]

Nest_Fates$FledgingSuccess
Nest_Fates$id

fate <- Nest_Fates[ order(Nest_Fates$id),]

check <- cbind(fate$FledgingSuccess, inferred_surv(btgo_outcomes)$outcomes)



ggplot(check) + 
        geom_boxplot(aes(x=factor(fate$FledgingSuccess), 
                                 y = pr_succ_mean), 
                     colour =  "grey40" , 
                     outlier.alpha = 0.001) + 
        geom_point(aes(x=factor(fate$FledgingSuccess), 
                       y = pr_succ_mean), 
                   na.rm=TRUE, 
                   position=position_jitter(width=.12, 
                                            height = 0), 
                   colour = "forestgreen") + 
        theme_classic()  + 
        labs(x = "True Fate", 
             y = "Pr(Survival)")  + 
        theme(axis.text.x = element_text(colour = "grey30", 
                                         size = 10),  
              axis.text.y = element_text(colour = "grey30", 
                                         size = 10), 
              axis.title.x = element_text(colour = "grey30", 
                                          size = 12), 
              axis.title.y = element_text(colour = "grey30", 
                                          size = 12)) 



#NEST
#Nest - best settings season.begin = "03-25", season.end = "08-20", period_length = 26, behavior_signal= "1", min.occ = 16

build_matrices(RF_prediction=predictions, 
               season.begin = "03-25", 
               season.end = "08-20", 
               period_length = 26, 
               behavior_signal= "1", 
               min.occ = 8)


matrices$mat_beh[c(1:5),c(1:10)]
matrices$mat_fix[c(1:5),c(1:10)]


#subset to those with complete incubation cycles
mat_keep_rows <- c("2015-2014", "2016-2013", "2018-2014", "2002-2014", "2002-2015") #only for nest model

# for nest

matrices$mat_beh_full <-  matrices$mat_beh[rownames(matrices$mat_beh) %in% 
                                                   mat_keep_rows, ] 
matrices$mat_fix_full <-  matrices$mat_fix[rownames(matrices$mat_fix) %in% 
                                                   mat_keep_rows, ] 


=======
# for chick-tending
matrices$mat_beh_full <-  matrices$mat_beh 
matrices$mat_fix_full <-  matrices$mat_fix 

matrices$mat_beh_full <-  matrices$mat_beh[rownames(matrices$mat_beh) %in% 
                                                   mat_keep_rows, ] 
matrices$mat_fix_full <-  matrices$mat_fix[rownames(matrices$mat_fix) %in% 
                                                   mat_keep_rows, ] 

# for chick-tending
matrices$mat_beh_full <-  matrices$mat_beh 
matrices$mat_fix_full <-  matrices$mat_fix 




#### 9. predict survival from states ####
btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, 
                                       visits = matrices$mat_beh_full, 
                                       model = "phi_time_p_time_indiv", 
                                       mcmc_params = list(burn_in = 1000, 
                                                          n_chain = 3, 
                                                          thin = 5, 
                                                          n_adapt = 1000, 
                                                          n_iter = 5000)) #; inferred_surv(btgo_outcomes)

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

# 
# #### get outcome estimate - nesting ####
# 
# #process data
surv <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,3] #this is for the boxplot, dichotemy plot
# 
# 
fate <- c(24,24,24,20,24)
pred <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,6]; pred
# 
print(summary(lm(fate ~ pred)))

# 
ggplot() + geom_boxplot(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), 
                            y=surv), colour =  "grey40", outlier.alpha = 0.001) + 
        geom_point(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), 
                       y=surv), 
                   na.rm=TRUE, 
                   position=position_jitter(width=.152, height = 0), 
                   colour = "purple3") + 
        theme_classic()  + labs(x = "True Fate", y = "Pr(Survival|movement)")  + 
        theme(axis.text.x = element_text(colour = "grey30", size = 10),  
              axis.text.y = element_text(colour = "grey30", size = 10), 
              axis.title.x = element_text(colour = "grey30", size = 12), 
              axis.title.y = element_text(colour = "grey30", size = 12))   # 
# 
# 
# 
# 





