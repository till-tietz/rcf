#'Estimates conditional average treatment effects based on a trained causal forest and test data
#'
#'@param cf output of the rcf causal_forest function
#'@param test a data frame of test data
#'@return result list consisting of: 1 data frame with predictors, treatment assignment and cate estimates, 2 list of vectors with cate estimates for each observation
#'@export


predict_causal_forest <- function(cf,test){
  cf <- cf[["cate_estimates"]]
  #get observations (by row name in test df) that fall into each leaf
  get_obs <- function(x){
    obs <- as.numeric(rownames(subset(test, eval(parse(text = cf[x, "filter"])))))
  }
  #data structure is a list of numeric vectors (each corresponding to one leaf in cf)
  #with indices (from test) of observations that fall into a leaf
  obs <- furrr::future_map(1:nrow(cf), ~get_obs(.x))
  #find out into which nodes every specific observation in test falls
  #loop over leafs and observations
  leaf_bool <- function(x){
    obs_i <- x
    bool <- function(x){
      out <- any(obs[[x]] == obs_i)
    }
    bool_out <- furrr::future_map_lgl(1:length(obs), ~bool(.x))
  }
  #outputs a logical vector of length cf for each observation in test
  #indicating which leafs it falls into with TRUE and which leafs
  #it does not fall into with FALSE
  leaf_bool_out <- furrr::future_map(.x = as.numeric(rownames(test)), ~leaf_bool(.x))
  #subset cf by leaf_bool_out logical vector to get cate estimates for all leafs
  #any specific observation in test falls into >> compute cate estimate
  #for each observation in test as average of leaf cate it falls into
  cate_est <- function(x){
    cate_pred <- mean(cf$cate[leaf_bool_out[[x]]],na.rm = TRUE)
  }
  #compute mean cate
  cate_est_out <- furrr::future_map_dbl(1:length(leaf_bool_out), ~cate_est(.x))
  #return all cate estimates for an observation
  cate_pred <- function(x){
    cate_pred <- cf$cate[leaf_bool_out[[x]]]
  }
  cate_pred_out <- furrr::future_map(1:length(leaf_bool_out), ~cate_pred(.x))
  #append cate estimates to test
  test$cate <- cate_est_out
  #return list containing data frame of 1: observations, covariates and mean cate estimates
  #and 2: all cate predictions per observation
  return(list(mean_cate_df = test, cate_pred_by_obs = cate_pred_out))
}
