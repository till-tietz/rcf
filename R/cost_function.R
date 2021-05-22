#'Causal Tree cost function
#'
#' @param splits logical subsetting vector for observations to use.
#' @param outcome name of the outcome variable as character vector.
#' @param treat name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA).
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector.
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects.
#' @return numeric vector with split cost
#' @export


cost_function <- function(splits, outcome, treat, minsize, alpha){

  left <- outcome[splits]
  right <- outcome[!splits]

  treat_left <- treat[splits]
  treat_right <- treat[!splits]

  n <- length(outcome)

  try_split <- all(c(sum(treat_right) > minsize,
                     sum(treat_left) > minsize,
                     n - sum(treat_right) > minsize,
                     n - sum(treat_left) > minsize))

  if(try_split){

    t_left <- left[treat_left == 1]
    c_left <- left[treat_left == 0]

    t_right <- right[treat_right == 1]
    c_right <- right[treat_right == 0]


    ate_right <- mean(t_right) - mean(c_right)
    ate_left <- mean(t_left) - mean(c_left)

    var_right <- var(t_right) + var(c_right)
    var_left <- var(t_left) + var(c_left)

    split_crit <- ((alpha*((ate_left - ate_right)^2)) - ((1-alpha)*((var_left + var_right)/n)))
  } else {
    split_crit <- NA
  }
  return(split_crit)
}
