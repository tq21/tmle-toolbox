## Density ratio estimation as a binary classification problem

library(data.table)
library(sl3)

density_ratio_est <- function(W, A, A_star, sl_lib, ...) {
  DT <- cbind(W, A)
  DT_star <- cbind(W, A_star)
  DT_star[, A := A_star]
  DT_star[, A_star := NULL]
  
  DELTA <- c(rep(0, nrow(DT)), rep(1, nrow(DT)))
  DT_dup <- rbind(DT, DT_star)
  DT_dup[, DELTA := DELTA]
  
  # Predicting DELTA using super learner
  task <- make_sl3_Task(DT_dup, 
                        covariates = names(DT)[!names(DT) %in% c("A", "DELTA")],
                        outcome = "DELTA",
                        outcome_type = "binomial")
  lrnr_sl <- Lrnr_sl$new(learners = sl_lib)
  sl_fit <- lrnr_sl$train(task)
  
  task_pred <- make_sl3_Task(DT, covariates = names(DT)[names(DT) != "A"])
  pred <- sl_fit$predict(task_pred)
  
  return(pred / (1 - pred))
}
