# Using movement tracks to infer successful breeding of a precocial bird with machine learning
<center> Authors: Luke Wilde and Matthew Duggan </center>


## Introduction

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; We are training hidden Markov models (HMMs) and random forests (RFs) to infer the breeding success of a threatened shorebird, *Limosa limosa* (hereby, known as godwits), with geospatial data. The challenges with geospatial data include a variable error rate in predicted location, limited covariate data to "teach" a model, and other commmon adversities when analyzing large data sets, including missing and inconsistent data. In addition, a limited amount of research has been conducted in automating the evaluation of precocial nesting behavior. Precocial means the hatchlings are almost fully independent once hatched where they forage for food themselves under the protection of the parents. <br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The goal of this repository is to build an autonomous framework and methodology in evaluating shorebird nesting behavior. The benefits of such a study would open the opportunity to evaluate survival probability of chicks and fitness of individuals. We hope this study leads to more focused conservation management and increases the reach of future shorebird studies. 

## Methods

### Create Random Forest Model
1. [X] Make Random Forest 
2. [X] Calculate Accuracy
3. [X] Calculate Out of Bag Error

### Create Hidden Markov Model
1. [ ] Make HMM
2. [ ] Calculate Accuracy
3. [ ] Calculate Out of Bag Error

### Run a Bayesian Cormack-Jolly-Seber model 
This is to estimate probability of hatching and probability of fledging. Note: nest failure = chick tending failure by default.
1. [ ] Build state and space matrices
   - State: the number of hours that a bird showed incubating behavior (LOOKING AT THESE, I THINK ITS JUST INSTANCES IN A DAY, BUT I DONT THINK THAT MATTERS MUCH ON THE RESULTS)
   - Space: the number of GPS fixes per day
   - requires full nesting behavior or depredated = 4 complete, 1 failed for 5 total (GPS).
2. [ ] Process matrices with number of days in incubation
   - need a function
3. [ ] Run model
4. [X] Assess Markov Chain Monte Carlo diagnostics
   - coda package
5. [X] Plot survival and detection process
   - probability of successful hatching 
6. [ ] Calculate performance metrics from reproductive outcome
   - Accuracy, Recall, Sensitivity, Specificity, F1

### Penthouse matrices to improve upon the above method
This achieves the same end, but should perform better as it will allow the MCMC sampling to estimate Pr(survival) at a specified elapsed-time: for nesting 24 d and for chick tending 28 days. 
1. [ ] Identify the season range for each model
   - use the matrices you built in the last section
   - have user specify the start and end date (POSIXct) to be considered. Build part of the function
   - trim the 365 day matrices down to what is specified 
2. [ ] Within the specified window, find first instance of consistent behavior (incubation or chick tending)
   - need a persistence check: that is, where did a bird first exhibit 4 days in a row with some instances of a given behavior
   - user needs to be able to specify the number of days
3. [ ] Define the histories from the first entry from the first consistent behavior period until specified number of days 
   - user needs to be able to specify number of days, again: in BTGO its 22-24 d of incubation and 28-34 d of chick tending.
   - this way, each row (individual history) of the nest.beh.final should start with the first day a bird was observed incubating and proceed for 24 days
4. [ ] Ensure that the observation matrix (i.e., nest.beh.final) samples the same columns and thus has EXACTLY the same dimensions as the state matrix (i.e., nest.beh.final)
5. I envisino the function looking like: state_matrix <- function(full_matrix, start_season_date, end_season_date, num_days_incub){} where the full_matrix is what we already have (nest.beh.final), the start_season_date, end_season_date terms filter the columns down so that only the breeding season is considered, and then the num_days_incub specifies how many columns from the first the matrix must be wide.
6. In the end, the matrix will have each row for an indiv., each column for the number of days in an incubation att or chick-tending period, the first cell cannot be 0, and each preceeding cell in a row is the sum of predicted incubating states or GPS fixes on a given day (can be 0 or >=1) 


### Argos Birds
1. [ ] Label birds using Tableau
   - Focus on birds with complte incubation
2. [ ] Predict Behaviors
   - Calculate LOOCV to estimate error
   - [ ] If good:
    - Proceed to estimate survival
   - [ ] If bad:
    - Subset GPS data to resemble Argos
    - Train model on Argos data (all birds)
3. [ ] Predict Survival (Refer to Bayesian model)
   - Includes birds tagged by NRS in Spain

### Expand Study to other Shorebird Species
1. [ ] Scope the range of species
   - *Scolopacids* (godwits, whimbrel)
   - *Charidriformes* (all shorebirds)
   - All Precocial Birds (geese, ducks, turkey)
2. [ ] Pick the number of species from each group or choose example species
3. [ ] Minimum sample size per species ?
4. [ ] BIG QUESTION: How to measure accuracy?
5. [ ] Feeler in Arctic Shorebird Demographic Network
   - Movement Tracks
   - Nest Fate
   - Fledgling Fate

### Create R package
1. [ ] Create a R and Description Folder
2. [ ] Break code into useable functions
3. [ ] Add documentary to each function using roxygen2
4. [ ] Construct a source package
5. [ ] Submit to CRAN


## Results & Figures



## References

1. McClintock, B.T., Michelot, T. (2018) momentuHMM: R package for generalized hidden Markov models of animal movement. Methods in Ecology and Evolution, 9(6), 1518-1530.

2. Michelot, T., Langrock, R., Patterson, T.A. (2016). moveHMM: An R package for analysing animal movement data using hidden Markov models. Methods in Ecology and Evolution. 7(11), 1308-1315.
