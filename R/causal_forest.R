#'Trains a causal forest that can be used to estimate conditional average treatment effects.
#'
#' @param n_trees number of trees to grow as numeric vector.
#' @param data a data frame with predictor, treatment assignment and outcome variables.
#' @param outcome name of the outcome variable as character vector.
#' @param covariates names of the predictors as character vector.
#' @param treat name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 = treatment, 0 = control with no NA).
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector.
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects.
#' @param feature_fraction fraction of total number of predictors to use in fitting each tree as numeric vector between 0-1.
#' @param honest_split enables honest splitting. Default TRUE.
#' @param honesty_fraction fraction of data to be used for honest estimation as numeric vector between 0-1. Default 0.5.
#' @return list of causal trees.
#' @export

causal_forest <- function(n_trees, data, outcome, covariates, treat, minsize, alpha, feature_fraction, honest_split = TRUE, honesty_fraction = 0.5){
  trees <- furrr::future_map(1:n_trees, ~causal_tree(data = data,
                                                     outcome = outcome,
                                                     covariates = covariates,
                                                     treat = treat,
                                                     minsize = minsize,
                                                     alpha = alpha,
                                                     feature_fraction = feature_fraction,
                                                     honest_split = honest_split,
                                                     honesty_fraction = honesty_fraction),
                             .progress = TRUE)
  return(trees)
}
