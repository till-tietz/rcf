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
  cate <- sapply(leaf_data, function(j) mean(j[j[[treat]] == 1, outcome]) - mean(j[j[[treat]] == 0, outcome]))
  leafs <- data.frame(leaf = leafs,
                      cate = cate)
  return(leafs)
}
