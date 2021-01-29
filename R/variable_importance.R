#' Computes variable importance measures for a causal forest
#'
#' @param cf output of the rcf causal_forest function
#' @param covariates names of predictors used in training the causal forest as character vector
#' @param n maximum tree depth of splits to be considered for variable importance computation
#' @param k decay parameter controlling weighting of splits at different depths for variable importance computation
#' @return a data frame of variables and their importance measures
#' @export


variable_importance <- function(cf, covariates, n, k){
  #extract data on which variables were split at which level from cf object
  data <- cf[["split_vars"]]
  #consider only those variables of "covariates" that were split in the cf
  vars <- covariates[!(covariates %in% setdiff(covariates, data$var))]
  #set up loop over each variable
  var_eval <- function(x){
    variable <- x
    #set up loop over each depth level
    depth_eval <- function(x){
      #number of total splits at depth x
      n_total_dk <- nrow(subset(data, data[["depth"]] == x))
      #number of splits of var x at depth x
      n_var_dk <- nrow(subset(data, data[["depth"]] == x & data[["var"]] == variable))
      div_w <- (n_var_dk / n_total_dk)* x^-n
      return(div_w)
    }
    #execute loop over all depths
    #compute numerator of importance formula
    numerator <- purrr::map_dbl(1:n, ~depth_eval(.x))
    numerator <- sum(numerator, na.rm = TRUE)
    #compute denominator of importance formula
    denominator <- c(1:n)^-k
    denominator <- sum(denominator, na.rm = TRUE)
    #compute importance score
    var_imp <- numerator/denominator
    out <- data.frame(variable = variable,
                      importance = var_imp,
                      stringsAsFactors = FALSE)
    return(out)
  }
  #if there are variables that were not split in cf execute loop over vars
  #set importance of non split vars to 0
  if(length(setdiff(covariates, data$var)) > 0){
    variable_importance <- purrr::map_dfr(.x = covariates, ~var_eval(.x))
    variable_importance <- dplyr::arrange(variable_importance, dplyr::desc(variable_importance[["importance"]]))
    rest <- data.frame(variable = setdiff(covariates, data$var),
                       importance = rep(0, length(setdiff(covariates, data$var))))
    variable_importance <- rbind(variable_importance, rest)
    #if all variables were split in cf execute loop over vars
  } else {
    variable_importance <- purrr::map_dfr(.x = covariates, ~var_eval(.x))
    variable_importance <- dplyr::arrange(variable_importance, dplyr::desc(variable_importance[["importance"]]))
  }
  return(variable_importance)
}
