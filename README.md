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
  1.Train a randomForest and Hidden Markov model
  2.Use trained models to predict behavioral states through all GPS tracks
  3.Calculate performance metrics (options)
    a.Out of bag (default for rf, done in model stage)
    b.Leave-one-out cross validation (done on predictions)
    c.Proportion agreement cross validation (done on predictions)
2.Run Bayesian Cormack-Jolly-Seber models to estimate probability of hatching and probability of fledging, separately (however, nest failure = chick tending failure by default)
Steps:
  1.Build state and space matrices
    a.state – whether focal behavior was observed for each day
      i.either NA/1 for not incubating/incubating  
        1.need a persistence check (i.e., if x>4 hrs, then 1, if x<4 then NA) (binary)
      ii.or # of hours that a bird showed incubating behavior (scalar)
      iii.in either case, requires birds with full nesting season observed in the data
    b.space – whether the tag collected data on a given dayi.the number of GPS fixes per day (scalar)
  2.Process matrices with # days in incubation cyclea.need a function 
  3.Run model
    a.user must choose among constant and time-varying survival, detection, or survival+detection.
    b.requires mcmc_params – to set the n.chains, n.iter, burnin, and adapt. values
  4.Assess Markov Chain Monte Carlo diagnostics (coda package)
  5.Plot both survival and detection process
    a.here we want a probability of successful hatching or fledging (as %)
  6.Calculate performance metrics from reproductive outcome
    a.Accuracy, Recall, Sensitivity, Specificity, F1
  
  
