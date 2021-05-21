#'Estimates conditional average treatment effects based on a trained causal forest and test data
#'
#'@param data a data frame with observations to predict and no NA
#'@param cf output of the rcf causal_forest function
#'@param predict_oob set to TRUE if you want to predict cates on the observations you used to fit the causal forest (uses out of bag observations); set to FALSE if you wish to use a test data set. Default TRUE.
#'@return a data.frame with a cate estimate for each observation.
#'@export


predict_causal_forest <- function(data, cf, predict_obb = TRUE){

  each_tree <- function(x){
    tree_i <- cf[[x]]

    if(predict_obb){
      oob_data <- data[tree_i[["oob"]],]
    } else {
      oob_data <- data
    }

    cate <- function(x){
      leaf <- tree_i[["cate_estimate"]][x,]
      obs <- as.numeric(rownames(subset(oob_data, eval(parse(text = leaf[["leaf"]])))))
      cate <- rep(leaf[["cate"]], length(obs))
      obs <- cbind(obs,cate)
    }
    obs_cate <- furrr::future_map(1:nrow(tree_i[["cate_estimate"]]), ~cate(.x))
    obs_cate <- do.call(rbind,obs_cate)
    return(obs_cate)
  }
  cates <- furrr::future_map(1:length(cf), ~each_tree(.x), .progress = TRUE)
  cates <- as.data.frame(do.call(rbind, cates))
  cates <- stats::aggregate(cates, list(obs_id = cates$obs), mean, na.rm = TRUE)
  cates <- cates[,c("obs_id","cate")]
  return(cates)
}
