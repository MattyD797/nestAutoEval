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


# 2. Load in Data for training -----------------------------------------------

tracks <- readTracks(predicted_tracks)

# 3. random drop and split training data ------------------------------------------------
tracks_split <- dropBeh(tracks, c(0,0,0,0)) %>% 
  group_split(ID)

# 4. Create a xytb object for each track ------------------------------

xytbs <- lapply(tracks_split, 
                tracks2xytb, 
                desc="BTGO Birds", 
                winsize=seq(3,15,2), 
                idquant=seq(0,1,.25),
                move=c(5,10,15))

# 5. Combine xytb objects -------------------------------------------------

xytb <- bindXytbs(xytbs, tracks)


# 6. Create RF training dataframe --------------------------------------------

dt_clean <- xytb2RF(xytb, -1)


# 7. tidymodels workflow -----------------------------------------------------

# split data #

dt_split <- splitData(dt_clean, 8/10, actual)

# define train and test set #

dt_train <- training(dt_split)
dt_test <- testing(dt_split)

# specify cross validation data #

dt_cv <- vfold_cv(dt_train)

# define recipe #

dt_recipe <- 
  recipe(dt_clean) %>%
  update_role(everything()) %>% 
  update_role(actual, new_role = "outcome") %>% 
  step_corr(all_predictors()) %>% 
  step_center(all_predictors(), -all_outcomes()) %>% 
  step_scale(all_predictors(), -all_outcomes())%>%
  prep()

# define model #

rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune(), ntrees = 1000) %>%
  # select the engine/package that underlies the model
  set_engine("randomForest", importance = TRUE) %>%
  # choose either the continuous regression or classification
  set_mode("classification")

# setup work flow ~ recipe and model#

rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(dt_recipe) %>%
  # add the model
  add_model(rf_model)

# create CV search grid #

rf_grid <- expand.grid(mtry = c(85:90))

# tune mtry #

rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = dt_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(yardstick::f_meas))

# collect metrics #

mtry_vals <- rf_tune_results %>%
  collect_metrics()

# pick the best #
param_final <- rf_tune_results %>%
  select_best(metric = "f_meas")

# finalize workflow #

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

# 8. Evaluate models performance ------------------------------------------

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(dt_split)

# collect metrics and confusion chart #
test_metrics <- rf_fit %>% collect_metrics()
test_metrics
test_predictions <- rf_fit %>% collect_predictions()
f_meas(test_predictions, actual, .pred_class)
precision(test_predictions, actual, .pred_class)
recall(test_predictions, actual, .pred_class)
conf_mat(test_predictions, actual, .pred_class)


# 9. Final model ----------------------------------------------------------
final_model <- fit(rf_workflow, dt_clean)


# 10. Predictions on tracks ---------------------------------------------------
rowsNA <- which(is.na(dt_tracks), arr.ind=TRUE)[,1] %>%  unique()
predictionsRF <- predict(final_model, dt_tracks %>% na.omit())
predictions <- cbind(xytb@b[-rowsNA, c("id", "t")], predictionsRF)

save(predictions, file= "predictions/RF.Rda")
