#'sample splitting function
#'
#' @param data list of predictor, treatment assignment and outcome variables
#' @param vars names of the predictors as character vector.
#' @param outcome name of the outcome variable as character vector.
#' @param treat name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA).
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector.
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects.
#' @return list containing (split = splitting information, subset = logical vector indicating left + right observations, data = observations used in split, var = variable that was split)
#' @export

split <- function(data, vars, outcome, treat, minsize, alpha){
  split_points <- lapply(data[vars], function(i) unique(i))
  var_names <- rep(names(split_points), each = length(split_points[[1]]))
  points <- unlist(split_points)

  split_points <- mapply(function(var, sp) lapply(sp, function(i) var <= i), data[vars], split_points, SIMPLIFY = TRUE)
  split_cost <- sapply(split_points, cost_function, outcome = data[[outcome]], treat = data[[treat]], minsize = minsize, alpha = alpha)

  if(all(is.na(split_cost))){
    split <- NULL
    subset <- NULL
  } else {
    split_index <- which.max(split_cost)

    var <- var_names[split_index]
    val <- points[split_index]

    left <- paste(var, "<=", val)
    right <- paste(var, ">", val)

    split <- c(left, right)
    subset <- split_points[[split_index]]
  }
  return(list("split" = split, "subset" = subset, "data" = data, "var" = var))
}
