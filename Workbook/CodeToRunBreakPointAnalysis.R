# script to perform breakpoint analyses on renesting Argos birds
#Luke Wilde
#01/28/22

#install.packages("bcp")

library(dplyr)
library(parsedate)
library(bcp) 

#load the predicted data
load('C:/Users/14064/Downloads/argosRF.Rda')
ArgosDt <- predictions; head(ArgosDt)


#load the metadata 
ArgosMt <- read.csv("C:/Users/14064/Dropbox/BTGO Movmement Study/nestAutoEval-main_2/nestAutoEval-main/fieldNotes/Nest_LocFate_Argos.csv"); head(ArgosMt); names(ArgosMt)

ArgosMt <- ArgosMt %>% mutate(across(c(TagDeployedDate,StartKnownActive,EndKnownActive), parse_date))

ArgosMt$DaysMonitored <- as.vector((ArgosMt$EndKnownActive - ArgosMt$TagDeployedDate)/(3600*24)) #or else its in seconds


#remove individuals that do not have complete 

#identify those with multiple nesting attempts
renesters <- ArgosMt %>% group_by(id) %>% summarise(Nests=length(unique(EndKnownActive))) %>% filter(Nests > 1)


renesters2 <- ArgosMt %>% filter(id %in% unique(renesters$id)) %>% filter(DaysMonitored > 1)

data <- ArgosDt %>% filter(id %in% unique(renesters$id))

unique(data$id)
unique(ArgosDt$id); unique(renesters$id)

data$b

#try the change point

beh_interest <- 5 #the behavior of interest
minimum_confidence <- 0.7  #the minimum value of Bayesian posterior probability that this is a cp

data <- data %>% mutate(b = ifelse(b != beh_interest, 9, b))

bcp_134747 <- bcp(y = data$b, return.mcmc = T, burnin = 100, mcmc = 500)
plot(bcp_134747)
legacyplot(bcp_134747)

data[which(bcp_134747$posterior.prob > minimum_confidence),"t"]
