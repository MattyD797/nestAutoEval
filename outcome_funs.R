#adapted from Picardi et al. 2019 - nestR

#### get outcome estimate ####
inferred_surv <- function(mcmc_object, ci = 0.95){
  
  # Initialize list for output
  out <- list()  
  
  # Calculate the quantiles for the bounds of the credible interval
  lwr <- 0 + (1 - ci)/2
  upr <- 1 - (1 - ci)/2
  
  ### Population-level survival
  
  # If the model had time-varying phi, report the slope and intercept
  if (grepl("phi_time", mcmc_object$model)){
    
    # Note that these parameters are on logit scale
    out$phi <- data.frame(b0_lwr = apply(mcmc_object$phi.b0, 1, quantile, lwr),
                          b0_mean = apply(mcmc_object$phi.b0, 1, mean),
                          b0_upr = apply(mcmc_object$phi.b0, 1, quantile, upr),
                          b1_lwr = apply(mcmc_object$phi.b1, 1, quantile, lwr),
                          b1_mean = apply(mcmc_object$phi.b1, 1, mean),
                          b1_upr = apply(mcmc_object$phi.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$phi <- data.frame(lwr = quantile(mcmc_object$phi, lwr),
                          mean = mean(mcmc_object$phi),
                          upr = quantile(mcmc_object$phi, upr))
    row.names(out$phi) <- NULL
    
  }
  
  ### Population-level detection
  
  if (grepl("p_time", mcmc_object$model)){
    
    # Note that these parameters are on logit scale
    out$p <- data.frame(b0_lwr = apply(mcmc_object$p.b0, 1, quantile, lwr),
                        b0_mean = apply(mcmc_object$p.b0, 1, mean),
                        b0_upr = apply(mcmc_object$p.b0, 1, quantile, upr),
                        b1_lwr = apply(mcmc_object$p.b1, 1, quantile, lwr),
                        b1_mean = apply(mcmc_object$p.b1, 1, mean),
                        b1_upr = apply(mcmc_object$p.b1, 1, quantile, upr))
  } else {
    
    # Note that these estimates are not on logit scale
    out$p <- data.frame(lwr = quantile(mcmc_object$p, lwr),
                        mean = mean(mcmc_object$p),
                        upr = quantile(mcmc_object$p, upr))
    row.names(out$p) <- NULL
    
  }
  
  ### Individual-level survival
  
  indiv <- data.frame(animal = as.factor(dput(as.character(unique(predictions$id)))),
                      pr_succ_lwr = NA,
                      pr_succ_mean = NA,
                      pr_succ_upr = NA,
                      last_day_lwr = NA,
                      last_day_mean = NA,
                      last_day_upr = NA)
  
  # Probability burst was a successful nest (survived to last day)
  # Get the last day for each burst + iteration + chain
  last_day <- apply(mcmc_object$z, c(1,3,4), getElement, ncol(mcmc_object$z))
  
  #get values
  indiv$pr_succ_lwr <- apply(last_day, 1, quantile, lwr)
  indiv$pr_succ_mean <- apply(last_day, 1, mean)
  indiv$pr_succ_upr <- apply(last_day, 1, quantile, upr)
  
  # Latest day that a nest survived to
  latest_day <- apply(mcmc_object$z, c(1, 3, 4), sum)
  
  # get values 
  indiv$last_day_lwr <- apply(latest_day, 1, quantile, lwr)
  indiv$last_day_mean <- apply(latest_day, 1, mean)
  indiv$last_day_upr <- apply(latest_day, 1, quantile, upr)
  
  # Add to output list
  out$outcomes <- indiv
  
  return(out)
  
}


