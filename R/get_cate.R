#'estimate cate in terminal leafs
#'
#' @param tree data.frame with tree splitting structure.
#' @param data data.frame of data to estimate cate with.
#' @param treat name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA).
#' @param outcome name of the outcome variable as character vector.
#' @return data.frame of terminal leafs with cate estimates.
#' @export

get_cate <- function(tree, data, treat, outcome){
  leafs <- tree[tree$state == "leaf", "filter"]
  leaf_data <- lapply(leafs, function(j) subset(data, eval(parse(text = j))))
  outcome_t <- lapply(leaf_data, function(j) j[j[[treat]] == 1, outcome])
  outcome_c <- lapply(leaf_data, function(j) j[j[[treat]] == 0, outcome])

  leafs <- list("filter" = leafs, "outcome_t" = outcome_t, "outcome_c" = outcome_c)
  return(leafs)
}
