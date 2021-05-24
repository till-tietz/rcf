#'Trains a single causal tree
#'
#' @param data a data frame with predictor, treatment assignment, outcome variables and no NA.
#' @param outcome name of the outcome variable as character vector.
#' @param covariates names of the predictors as character vector.
#' @param treat name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA).
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector.
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects.
#' @param feature_fraction fraction of total number of predictors to use in fitting each tree as numeric vector between 0-1.
#' @param sample_fraction fraction of observations to fit tree with.
#' @param honest_split enables honest splitting. Default TRUE.
#' @param honesty_fraction fraction of data to be used for honest estimation as numeric vector between 0-1.
#' @return trained causal tree (splitting structure, cate estimates, oob samples, data for variable importance computation)
#' @export


causal_tree <- function(data, outcome, covariates, treat, minsize, alpha, feature_fraction, sample_fraction, honest_split, honesty_fraction){

  covariates <- sample(covariates, size = ceiling(length(covariates) * feature_fraction), replace = FALSE)
  sample_index <- sample(1:nrow(data), size = ceiling(sample_fraction * nrow(data)), replace = FALSE)
  oob <- setdiff(c(1:nrow(data)), sample_index)
  data <- data[sample_index, c(covariates, outcome, treat)]

  if(honest_split){
    honest_index <- sample(1:nrow(data), size = nrow(data)*(1 - honesty_fraction), replace = FALSE)
    data_fit <- data[honest_index,]
    data_honest <- data[-honest_index,]
    data <- as.list(data_fit)
  } else {
    data <- as.list(data)
  }

  tree_info <- data.frame(node = 1,
                          filter = NA,
                          state = "split",
                          stringsAsFactors = FALSE)
  tree_info$data <- list(data)
  tree_info$subset <- list(rep(TRUE, length(data[[1]])))

  var_importance <- data.frame(depth = 0,
                               var = NA,
                               stringsAsFactors = FALSE)

  do_splits <- TRUE

  while(do_splits){

    calculate <- which(tree_info$state == "split")

    for(i in calculate){

      comp_data <- lapply(tree_info[i,"data"][[1]], function(j) j[tree_info[i,"subset"][[1]]])

      split <- split(data = comp_data, vars = covariates, outcome = outcome,
                     treat = treat, minsize = minsize, alpha = alpha)

      if(is.null(split[["split"]])){

        tree_info[i, "state"] <- "leaf"

      }else{

        mn <- max(tree_info$node)
        mdepth <- max(var_importance$depth)

        split_here  <- !sapply(split[["split"]], function(x,y) any(grepl(x, x = y)), y = tree_info$filter)

        if(!is.na(tree_info[i,"filter"])){

          tmp_filt <- paste(tree_info[i, "filter"], split[["split"]], sep = " & ")

        } else {

          tmp_filt <- split[["split"]]
        }

        node_1 <- data.frame(node = mn + 1,
                             filter = tmp_filt[1],
                             state = "split",
                             row.names = NULL)

        node_1$data <- list(split[["data"]])
        node_1$subset <- list(split[["subset"]])

        node_2 <- data.frame(node = mn + 2,
                             filter = tmp_filt[2],
                             state = "split",
                             row.names = NULL)

        node_2$data <- list(split[["data"]])
        node_2$subset <- list(!split[["subset"]])

        nodes_new <- rbind(node_1, node_2)
        rm(node_1,node_2)

        tree_info[i, "state"] <- ifelse(all(!split_here), "leaf", "parent")

        var_imp <- data.frame(depth = mdepth + 1,
                              var = split[["var"]])

        tree_info <- rbind(tree_info, nodes_new)
        var_importance <- rbind(var_importance, var_imp)
      }
      do_splits <- !all(tree_info$state != "split")
    }
  }
  tree_info <- tree_info[,c(1:3)]

  if(honest_split){
    cate <- get_cate(tree = tree_info, data = data_honest, treat = treat, outcome = outcome)
  } else {
    cate <- get_cate(tree = tree_info, data = as.data.frame(data), treat = treat, outcome = outcome)
  }

  return(list("tree" = tree_info, "cate_estimate" = cate, "oob" = oob, "var_importance" = var_importance))
}
