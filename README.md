# Using movement tracks to infer successful breeding of a precocial bird with machine learning
<center> Authors: Luke Wilde and Matthew Duggan </center>


## Introduction

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; We are training hidden Markov models (HMMs) and random forests (RFs) to infer the breeding success of a threatened shorebird, *Limosa limosa* (hereby, known as godwits), with geospatial data. The challenges with geospatial data include a variable error rate in predicted location, limited covariate data to "teach" a model, and other commmon adversities when analyzing large data sets, including missing and inconsistent data. In addition, a limited amount of research has been conducted in automating the evaluation of precocial nesting behavior. Precocial means the hatchlings are almost fully independent once hatched where they forage for food themselves under the protection of the parents. <br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The goal of this repository is to build an autonomous framework and methodology in evaluating shorebird nesting behavior. The benefits of such a study would open the opportunity to evaluate survival probability of chicks and fitness of individuals. We hope this study leads to more focused conservation management and increases the reach of future shorebird studies. 

## Methods

### Create Random Forest Model
1. [ ] Make Random Forest 
2. [ ] Calculate Accuracy
3. [ ] Calculate Out of Bag Error

### Create Hidden Markov Model
1. [ ] Make HMM
2. [ ] Calculate Accuracy
3. [ ] Calculate Out of Bag Error

### Run a Bayesian Cormack-Jolly-Seber model 
This is to estimate probability of hatching and probability of fledging. Note: nest failure = chick tending failure by default.
1. [ ] Build state and space matrices
   - State: the number of hours that a bird showed incubating behavior
   - Space: the number of GPS fixes per day
   - requires full nesting behavior = 4 birds
2. [ ] Process matrices with number of days in incubation
   - need a function
3. [ ] Run model
4. [ ] Assess Markov Chain Monte Carlo diagnostics
   - coda package
5. [ ] Plot survival and detection process
   - probability of successful hatching 
6. [ ] Calculate performance metrics from reproductive outcome
   - Accuracy, Recall, Sensitivity, Specificity, F1

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
