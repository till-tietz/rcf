#' Computes variable importance measures for a causal forest
#'
#' @param cf output of the rcf causal_forest function
#' @param covariates names of predictors used in training the causal forest as character vector
#' @param n maximum tree depth of splits to be considered for variable importance computation
#' @param d decay parameter controlling weighting of splits at different depths for variable importance computation
#' @return a data frame of variables and their importance measures
#' @export


variable_importance <- function(cf, covariates, n, d){
  imp_data <- lapply(cf, function(i) i[["var_importance"]])
  imp_data <- do.call(rbind, imp_data)
  imp_data <- imp_data[-which(imp_data$depth == 0),]

  total_splits <- as.data.frame(table(imp_data$depth))
  colnames(total_splits) <- c("depth", "total_splits")

  var_grouped <- base::split(imp_data, imp_data$var)
  imp <- lapply(var_grouped, function(i) as.data.frame(table(i$depth)))
  imp <- lapply(imp, setNames, c("depth", "n_splits"))
  imp <- lapply(imp, function(i) merge(i, total_splits, by = "depth"))
  imp <- lapply(imp, function(i) i[as.numeric(i$depth) <= n,])
  imp <- lapply(imp, function(i) sum((i$n_splits / i$total_splits) * as.numeric(i$depth)^-d)/ sum(c(1:n)^-d))
  imp <- cbind(names(imp), do.call(c, imp))
  colnames(imp) <- c("variable", "importance")
  rownames(imp) <- NULL
  imp <- as.data.frame(imp)

  var_not_split <- setdiff(vars, imp$variable)
  var_not_split <- data.frame(variable = var_not_split,
                              importance = rep(0, length(var_not_split)))

  imp <- rbind(imp, var_not_split)
  imp <- dplyr::arrange(imp, desc(importance))
  return(imp)
}
