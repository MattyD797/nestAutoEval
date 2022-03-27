# code to run a changepoint analysis for choosing the dates where a new nest attempt occured

#load in librarys
library(tidyverse)
library(mcp) #install.packages("mcp")
library(bcp) #install.packages("bcp")
#remotes::install_github("lindeloev/mcp")

#load in datasets

load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/Black-tailed Godwit/GPS/gpsOnArgos_tracks.Rda")

load("H:/.shortcut-targets-by-id/1p1ONghQ9l1JHzgdbP84k_291oeql3a5I/AutoNestEval/Predictions/lapwing/lapwingOnLapwing_tracks.Rda")

model=list( y ~ 1,
            ~ 1 
            )

data <- gpsOnArgos_tracks %>% filter(t >= as.POSIXct("2016-03-01 00:00") & t < as.POSIXct("2016-09-01 00:00"))
pred <- data %>% filter(id == unique(data$id[3]))
testdat <- data.frame("x"=pred$t, "y"=pred$b)

testdat$y <- ifelse(testdat$y != 2, 0, 1)
#fit_mcp <- mcp(model = model, data=pred,par_x="time")

fit_bcp <- bcp(as.numeric(testdat$y), return.mcmc=T,p0=0.005)
plot(fit_bcp)

bcp_sum <- as.data.frame(summary(fit_bcp))
bcp_sum$id <- 1:length(testdat)

(sel <- bcp_sum[which(fit_bcp$posterior.prob > 0.6), ])

data