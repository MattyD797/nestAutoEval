# Using movement tracks to infer successful breeding of a precocial bird with machine learning

## Summary 

We are training ML algorithms to infer the breeding success of shorebirds that were not observed on the breeding grounds (i.e., Nate’s spain birds). Additionally, we want to create 1) a manuscript, 2) a model that other researchers can use on their own study organisms, and 3) auser-friendly R package.

## Possible Manuscript

1.We present this tool, and using inferred breeding success, make some estimates for what affected the hatching/nesting probability of Nate’s Spain birds as a test-piece.

2.We present the tool, and using the inferred breeding success on Nate’s Spain birds and several other species, make a broad conclusion about the factors that affect hatching/fledging success in whatever taxonomic group we use (e.g., shorebirds, precocial birds, godwits and similar)

3.We compare Random Forest to HMM throughout and compare their performance. (This is least generalizable, as it only pertains to groups who know true states and have movement data – naturally, that is a rare situation!)

## Objectives

1. Create machine learning algorithm to predict the behavioral state from tracking data alone
Steps:
   - Train a randomForest and Hidden Markov model
   - Use trained models to predict behavioral states through all GPS tracks
   - Calculate performance metrics (options)
     - Out of bag (default for rf, done in model stage)
     - Leave-one-out cross validation (done on predictions)
     - Proportion agreement cross validation (done on predictions)
2.Run Bayesian Cormack-Jolly-Seber models to estimate probability of hatching and probability of fledging, separately (however, nest failure = chick tending failure by default)
Steps:
   - Build state and space matrices
     - state – whether focal behavior was observed for each day
       - either NA/1 for not incubating/incubating  
         - need a persistence check (i.e., if x>4 hrs, then 1, if x<4 then NA) (binary)
       - or # of hours that a bird showed incubating behavior (scalar)
       - in either case, requires birds with full nesting season observed in the data
     - space – whether the tag collected data on a given dayi.the number of GPS fixes per day (scalar)
   - Process matrices with # days in incubation cyclea.need a function 
   - Run model
     - user must choose among constant and time-varying survival, detection, or survival+detection.
     - requires mcmc_params – to set the n.chains, n.iter, burnin, and adapt. values
   - Assess Markov Chain Monte Carlo diagnostics (coda package)
   - Plot both survival and detection process
     - here we want a probability of successful hatching or fledging (as %)
   - Calculate performance metrics from reproductive outcome
     - Accuracy, Recall, Sensitivity, Specificity, F1
  
## 
  
