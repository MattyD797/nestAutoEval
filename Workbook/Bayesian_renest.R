# 1. set#clean environment
rm(list=ls(all=TRUE))

#load constants and functions
source("Functions/standard_functions.R") 
source("Functions/format_functions_v2.2.R")
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
#load("C:/Users/14064/Dropbox/BTGO Movmement Study/nestAutoEval/predictions/RF_FrieslandArgos.Rda")
predictions <- read.csv("./argos.csv")
predictions <- as.data.frame(predictions %>% filter(id == c("123424-2017-att1","144433-2016-att1","144434-2017-att1","144439-2016-att1","144441-2017-att1","144443-2017-att1", "157536-2018-att1","123424-2017-att2","144433-2016-att2","144434-2017-att2","144439-2016-att2","144441-2017-att2","144443-2017-att2", "157536-2018-att2")))

predictions <- predictions[,-1]
names(predictions)[3] <- "b"

predictions$b <- as.numeric(predictions$b)
predictions$t <- as.POSIXct(predictions$t , format="%m/%d/%Y %H:%M")
 # Aug 26, tag 2015-2013 = 101 (looked at it, every location was classified as chick tending for the entire day); April 29, 2015-2014 = 150 (this one had the same issue, plus irregular fix rates between 2-15 min, mostly 5)

head(predictions)
str(predictions)

unique(predictions$id)

# 
# memory.limit(size = 35000) 
# 
# renest <- data.frame()
# 
# for(i in c("123424-2017","144433-2016","144434-2017","144439-2016","144441-2017","144443-2017", "157536-2018")){
#   tmp1 <- predictions %>% filter(id == as.character(i))
#   for(f in 1:nrow(tmp)){ tmp <- tmp1[f,]
#   } #f
#   for(j in c("2017-04-29 23:00:00", "2016-04-28 23:00:00", "2017-05-03 23:00:00", "2016-05-04 23:00:00", "2017-04-21 23:00:00", "2017-05-02 23:00:00", "2018-04-18 23:00:00")){
#     
#     tmp$id <- if(tmp$t > as.POSIXct(j, format = "%Y-%m-%d %H:%M:%S", tz=attr(predictions$t, "tzone"))) {paste(as.character(i), "att2", sep="-")} else {
#   paste(as.character(i), "att1", sep="-")}
#   
#     tmp2<- rbind(renest, tmp)
#     renest <- rbind(renest, tmp2)
#     }#j
#   
#   }#i
# 
# renest
# 
# unique(renest$id)
# 
# warnings()
# 
# d <- as.data.frame(predictions %>% group_by(id) %>% summarise(min = min(t), max = max(t)))
# d2 <- as.data.frame(renest %>% group_by(id) %>% summarise(min = min(t), max = max(t)))

#### 8. run the function to create matrices ####

# SETTINGS DESCRIPTION!! FOLLOW THESE GUIDELINES #
#
#Chick - best settings season.begin = "05-10", season.end = "10-20", period_length = 26, behavior_signal= "3", min.occ = 16

unique(predictions$b)

build_matrices(RF_prediction=predictions, season.begin = "05-01", season.end = "11-20", period_length = 26, behavior_signal= "3", min.occ = 2, avg.fix.rate=1)

# for chick-tending
matrices$mat_beh_full <-  matrices$mat_beh 
matrices$mat_fix_full <-  matrices$mat_fix 
# 
# matrices$mat_beh_full[which(rownames(matrices$mat_beh_full) %in% c("2004-2013","2020-2013","2041-2013","2012-2013","2023-2013","1008-2013","1009-2013","2017-2013","2019-2013")),]

matrices$mat_beh_full[which(!rownames(matrices$mat_beh_full) %in% c("2004-2013","2020-2013","2041-2013","2012-2013","2023-2013","1008-2013","1009-2013","2017-2013","2019-2013")),]


btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, visits = matrices$mat_beh_full, model = "phi_time_p_time_indiv", mcmc_params = list(burn_in = 1000, n_chain = 3, thin = 5, n_adapt = 1000, n_iter = 3000))

fixes = matrices$mat_fix_full; visits = matrices$mat_beh_full

inferred_surv(btgo_outcomes)

Nest_Fates <- read_csv("fieldNotes/Nest_Fates_1.csv")

Nest_Fates <- Nest_Fates[ which(Nest_Fates$id %in% inferred_surv(btgo_outcomes, ci = .80)$outcomes[,1]),]

Nest_Fates$FledgingSuccess
Nest_Fates$id

fate <- Nest_Fates[ order(Nest_Fates$id),]

check <- cbind(fate$FledgingSuccess, inferred_surv(btgo_outcomes)$outcomes)



ggplot(check) + geom_boxplot(aes(x=factor(fate$FledgingSuccess), y = pr_succ_mean), colour =  "grey40" , outlier.alpha = 0.001) + geom_point(aes(x=factor(fate$FledgingSuccess), y = pr_succ_mean), na.rm=TRUE, position=position_jitter(width=.12, height = 0), colour = "forestgreen") + theme_classic()  + labs(x = "True Fate", y = "Pr(Survival)")  + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12)) 

#Argos birds only
ggplot(inferred_surv(btgo_outcomes)$outcomes) + geom_boxplot(aes(x = factor(0), y = pr_succ_mean), colour =  "grey40" , outlier.alpha = 0.001) + geom_point(aes(x = factor(0), y = pr_succ_mean), na.rm=TRUE, position=position_jitter(width=.12, height = 0), colour = "forestgreen") + theme_classic()  + labs(y = "Pr(Survival)")  + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12)) 


#NEST
#GSP - best settings season.begin = "03-25", season.end = "08-20", period_length = 26, behavior_signal= "1", min.occ = 16
#Argos - season.begin = "04-01", season.end = "08-20", period_length = 26, behavior_signal= "1", min.occ = 2

predictions <- as.data.frame(predictions %>% filter(id != c("123424-2017","144433-2016","144434-2017","144439-2016","144441-2017","144443-2017", "157536-2018")))

build_matrices(RF_prediction=predictions, season.begin = "04-01", season.end = "08-20", period_length = 26, behavior_signal= "1", min.occ = 2)
# 
# 
#  matrices$mat_beh[c(1:5),c(1:10)]
#  matrices$mat_fix[c(1:5),c(1:10)]
# 
# 
# #subset to those with complete incubation cycles
#  mat_keep_rows <- c("2015-2014", "2016-2013", "2018-2014", "2002-2014", "2002-2015") #only for nest model
# 
#  # for nest

  matrices$mat_beh_full <-  matrices$mat_beh#[rownames(matrices$mat_beh) %in% mat_keep_rows, ] 
  matrices$mat_fix_full <-  matrices$mat_fix#[rownames(matrices$mat_fix) %in% mat_keep_rows, ] 

fixes = matrices$mat_fix_full; visits = matrices$mat_beh_full

#### 9. predict survival from states ####
btgo_outcomes <- estimate_outcomes_LRW(fixes = matrices$mat_fix_full, visits = matrices$mat_beh_full, model = "phi_time_p_time_indiv", mcmc_params = list(burn_in = 1000, n_chain = 3, thin = 5, n_adapt = 1000, n_iter = 5000)) #; inferred_surv(btgo_outcomes)

inferred_surv(btgo_outcomes)  
  
  
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
  
 true_fate<- read.csv("./fieldNotes/Nest_LocFate_Argos.csv")
  
  true_fate <- true_fate[,c("id", "NestSuccess")]
  
  head(true_fate)
  
  names(true_fate)[1] <- "animal"
  
 surv <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,3] #this is for the boxplot, dichotemy plot
# 
# 
 
 plot <- right_join(true_fate,inferred_surv(btgo_outcomes, ci = .80)$outcomes, by="animal")
 
 head(plot)
 
 ch <- as.data.frame(plot %>% group_by(animal) %>% count())
 
 plot[plot$animal %in% c(as.vector(ch[which(ch$n == 1),1])),]
 
#  
# fate <- c(24,24,24,20,24)
# pred <- inferred_surv(btgo_outcomes, ci = .80)$outcomes[,6]; pred
# # 
# print(summary(lm(fate ~ pred)))

# 

 #argos only
ggplot() + geom_boxplot(aes(x = NestSuccess, y = pr_succ_mean), data = plot) + geom_point(aes(x = NestSuccess, y = pr_succ_mean), data = plot, na.rm=TRUE, position=position_jitter(width=.152, height = 0), colour = "purple3") + theme_classic()

#gps only
 ggplot() + geom_boxplot(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), y=surv), colour =  "grey40", outlier.alpha = 0.001) + geom_point(aes(x = c("Hatched", "Hatched", "Hatched", "Failed", "Hatched"), y=surv), na.rm=TRUE, position=position_jitter(width=.152, height = 0), colour = "purple3") + theme_classic()  + labs(x = "True Fate", y = "Pr(Survival|movement)")  + theme(axis.text.x = element_text(colour = "grey30", size = 10),  axis.text.y = element_text(colour = "grey30", size = 10), axis.title.x = element_text(colour = "grey30", size = 12), axis.title.y = element_text(colour = "grey30", size = 12))   # 
# 
# 
# 
# 





