#'Estimates conditional average treatment effects based on a trained causal forest and test data
#'
#'@param data a data frame with observations to predict and no NA
#'@param cf output of the rcf causal_forest function
#'@param predict_oob set to TRUE if you want to predict cates on the observations you used to fit the causal forest (uses out of bag observations); set to FALSE if you wish to use a test data set. Default TRUE.
#'@return a data.frame with a cate estimate for each observation.
#'@export


predict_causal_forest <- function(data, cf, predict_oob = TRUE){

  each_tree <- function(x){
    tree_i <- cf[[x]]

    if(predict_oob){
      oob_data <- data[tree_i[["oob"]],]
    } else {
      oob_data <- data
    }

    cate <- function(x){
      leaf <- tree_i[["cate_estimate"]][["filter"]][x]
      obs <- as.numeric(rownames(subset(oob_data, eval(parse(text = leaf)))))

      if(length(obs) == 0){
        obs <- data.frame("obs" = NA)
        obs[["val_t"]] <- list(NA)
        obs[["val_c"]] <- list(NA)
      } else {
        obs <- data.frame("obs" = obs)
        obs[["val_t"]] <- list(tree_i[["cate_estimate"]][["outcome_t"]][[x]])
        obs[["val_c"]] <- list(tree_i[["cate_estimate"]][["outcome_c"]][[x]])
      }
      return(obs)
    }
    obs_cate <- purrr::map(1:length(tree_i[["cate_estimate"]][["filter"]]), ~cate(.x))
    obs_cate <- do.call(rbind,obs_cate)
    return(obs_cate)
  }
  cates <- purrr::map(1:length(cf), ~each_tree(.x))
  cates <- as.data.frame(do.call(rbind, cates))
  cates <- stats::na.omit(cates)
  cates <- dplyr::group_split(dplyr::group_by(cates, obs))

  each_obs <- function(x){
    obs_i <- cates[[x]]
    cate_i <- mean(unlist(obs_i[["val_t"]])) - mean(unlist(obs_i[["val_c"]]))
    obs_i <- obs_i[1,"obs"]
    out <- data.frame("obs" = obs_i,
                      "cate" = cate_i)
    return(out)
  }

  cates_out <- purrr::map(1:length(cates), ~each_obs(.x))
  cates_out <- do.call(rbind, cates_out)
  return(cates_out)
}


