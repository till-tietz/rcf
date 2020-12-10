#'Trains a causal forest that can be used to estimate conditional average treatment effects.
#'
#' @param data a data frame with predictor, treatment assignment and outcome variables
#' @param target name of the outcome variable as character vector
#' @param covariates names of the predictors as character vector
#' @param treatment name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA)
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects.
#' @param n_trees number of trees to grow as numeric vector
#' @param feature_frac fraction of total number of predictors to use in fitting each tree as numeric vector between 0-1
#' @param honesty.fraction fraction of data to be used for honest estimation as numeric vector between 0-1
#' @return trained causal forest (list of two data frames with splitting information)
#' @export


causal_forest <- function(data,target,covariates,treatment,minsize,alpha,n_trees,feature_frac,honesty.fraction){
  #define tree growing function
  grow_tree <- function(data,target,covariates,treatment,minsize,alpha,honesty.fraction,feature_frac){
    #bag data >> sample data with replacement to create a data set of the same size as the input data
    train <- data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
    #sample covariates (random sample based on feature_frac)
    #(each tree is only fitted with a subsample of the specified covariates)
    features_sample <- sample(covariates, size = ceiling(length(covariates) * feature_frac),replace = FALSE)
    #prepare causal tree inputs
    targ <- target
    treat <- treatment
    ms <- minsize
    a <- alpha
    h.frac <- honesty.fraction
    #define causal tree function
    tree <- causal_tree(data = train, target = targ, covariates = features_sample,
                        treatment = treat, minsize = ms, alpha = a, honesty.fraction = h.frac)
    #return cate estimates
    return(list(tree$fit, tree$variable_importance_info))
  }#close tree growing function

  #execute tree growing function n_trees times (build n_trees causal trees)
  trees <- furrr::future_map(1:n_trees,
                             ~grow_tree(data = data, target = target, covariates = covariates,
                                        treatment = treatment, minsize = minsize, alpha = alpha,
                                        feature_frac = feature_frac, honesty.fraction = honesty.fraction),
                             .progress = TRUE)
  #bind terminal leaf info + cate estimates for all n_tree trees
  extract_cate <- function(x){
    out <- trees[[x]][[1]]
    return(out)
  }
  cate_out <- purrr::map_dfr(1:length(trees), ~extract_cate(.x))

  extract_varimp <- function(x){
    out <- trees[[x]][[2]]
    return(out)
  }
  varimp_out <- purrr::map_dfr(1:length(trees), ~extract_varimp(.x))
  varimp_out <- dplyr::filter(varimp_out, depth > 0)

  #prep cate estimates
  return(list(cate_estimates = cate_out, split_vars = varimp_out))
}
