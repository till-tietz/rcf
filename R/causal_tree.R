#'Trains a single causal tree
#'
#' @param data a data frame with predictor, treatment assignment and outcome variables
#' @param target name of the outcome variable as character vector
#' @param covariates names of the predictors as character vector
#' @param treatment name of the treatment indicator variable as character vector (treatment indicator must be binary numeric 1 == treatment, 0 == control with no NA)
#' @param minsize minimum number of treatment and control observations that must be present in each split as numeric vector
#' @param alpha weighting of cost function numeric vector between 0-1. weights closer to 1 put more emphasis on maximizing heterogeneity. weights closer to 0 put more weight on precisely estimating treatment effects
#' @param honesty.fraction fraction of data to be used for honest estimation as numeric vector between 0-1
#' @return trained causal tree (list of three data frames with splitting information)
#' @export


causal_tree <- function(data,target,covariates,treatment,minsize,alpha,honesty.fraction){
  #split data into subsample for creating and repopulating the tree
  #after building tree with data treatment effects in terminal leafs
  #are computed with data_honest
  data_init <- as.data.frame(data)
  index <- sample(1:nrow(data_init), size = nrow(data_init)*(1 - honesty.fraction), replace = FALSE)
  data <- data_init[index,]
  data_honest <- data_init[-index,]
  #initialize while loop (keep splitting while condition is TRUE)
  do_splits <- TRUE
  #split state control + splitting info
  #node = node number (tree depth)
  #state takes on values "split"(nodes to be split),"parent"(nodes already split but not terminal),
  #and leaf (terminal node)
  tree_info <- data.frame(node = as.numeric(1),
                          filter = NA,
                          state = "split",
                          stringsAsFactors = FALSE)

  #variable importance info >> record of which variable was split at which depth
  var_importance_info <- data.frame(depth = as.numeric(0),
                                    var = NA,
                                    stringsAsFactors = FALSE)
  #open while loop
  while(do_splits){
    #nodes to be assessed for splitting (split nodes withe state == "split")
    calculate <- which(tree_info$state == "split")
    #opening for loop >> function keeps placing splits until while condition is no longer satisfied
    #i.e. as long as there nodes in state "split" the for loop executes
    for(i in calculate){
      #subsetting data according to completed splits (arrive at subsample we wish to split further)
      #if we are at depth > 1 subset according to completed splits)
      if(!is.na(tree_info[i, "filter"])){
        comp_data <- subset(data, eval(parse(text = tree_info[i, "filter"])))
      } else {
        #if this is the first node do not subset
        comp_data <- data
      }

      depth <- (stringr::str_count(tree_info[i, "filter"], pattern = "&")) + 2
      if(!is.na(depth)){
        depth <- depth
      } else {
        depth[1] <- c(1)
      }
      #compute split criterion
      #variance of ate across leaves minus uncertainty about these estimates (variance of outcomes
      #in treatment and control group observations of both leafs) >> select max value
      #alpha weights the relative importance of both quantities in splitting
      #loop over every variable and every unique value of it
      split_rule <- function(x){
        var <- x
        sp_var <- unique(comp_data[[var]])
        sp_var <- sp_var[!is.na(sp_var)]
        eval_split_rule <- function(x){
          #observations in left leaf
          left <- subset(comp_data, comp_data[[var]] <= sp_var[x])
          #observations in right leaf
          right <- subset(comp_data, comp_data[[var]] > sp_var[x])
          #left treatment and control subsets
          left_t <- subset(left, left[[treatment]] == 1)[[target]]
          left_c <- subset(left, left[[treatment]] == 0)[[target]]
          #right treatment and control subsets
          right_t <- subset(right, right[[treatment]] == 1)[[target]]
          right_c <- subset(right, right[[treatment]] == 0)[[target]]

          #assess minsize criterion
          minsize_left <- (length(left_t) + length(left_c)) >= (2 * minsize)
          minsize_right <- (length(right_t) + length(right_c)) >= (2 * minsize)

          if(all(minsize_left, minsize_right)){
            #compute ates
            ate_left <- mean(left_t, na.rm = TRUE) - mean(left_c, na.rm = TRUE)
            ate_right <- mean(right_t, na.rm = TRUE) - mean(right_c, na.rm = TRUE)
            #compute variance in outcomes of treatment and control in leafs
            var_left <- var(left_t, na.rm = TRUE) + var(left_c, na.rm = TRUE)
            var_right <- var(right_t, na.rm = TRUE) + var(right_c, na.rm = TRUE)
            #compute split criterion
            msete <- (alpha*((ate_left - ate_right)^2)) - ((1-alpha)*((var_left + var_right)/nrow(comp_data)))
          } else {
            msete <- NA
          }
          return(msete)
        }
        #execute loop over each split point
        msete <- purrr::map_dbl(1:length(sp_var), ~eval_split_rule(.x))

        if(length(msete[!is.na(msete)]) > 0){
          max_split_msete <- sp_var[which.max(msete)]
          max_msete <- max(msete, na.rm = TRUE)
        } else {
          max_split_msete <- NA
          max_msete <- NA
        }

        split_rule_out <- data.frame("var" = var,
                                     "sc" = max_msete,
                                     "sp" = max_split_msete)
        return(split_rule_out)
      }
      #execute loop over each covariate
      split_criteria <- purrr::map_dfr(.x = covariates, ~split_rule(.x))
      best_split <- split_criteria[which.max(split_criteria[["sc"]]),c("var","sp")]

      if(dim(best_split)[1] == 0){
        tree_info[i, "state"] <- "leaf"
      }else{
        #find the current depth of the tree (max node)
        mn <- max(tree_info$node)

        #write filter rule
        tmp_filter <- c(paste(best_split[["var"]], "<=", best_split[["sp"]]),
                        paste(best_split[["var"]], ">", best_split[["sp"]]))

        #check if split has already been made
        split_here  <- !sapply(tmp_filter,
                               FUN = function(x,y) any(grepl(x, x = y)),
                               y = tree_info$filter)

        # append the splitting rules (this serves as subsetting instructions to retrieve all units in leafs created by the current split)
        if(!is.na(tree_info[i,"filter"])){
          tmp_filter  <- paste(tree_info[i, "filter"],
                               tmp_filter, sep = " & ")
        }
        #data frame with all information about the leaf created by the split
        nodes_new <- data.frame(node = c(mn+1, mn+2),
                                filter = tmp_filter,
                                state = rep("split", 2),
                                row.names = NULL)[split_here,]

        #overwrite state of current node
        #indicator of whether the current node is a terminal leaf
        tree_info[i, "state"] <- ifelse(all(!split_here), "leaf", "parent")

        #bind leaf info to tree info
        tree_info <- rbind(tree_info, nodes_new)

        #create and update var importance info data frame
        var_importance_new <- data.frame(depth = depth,
                                         var = best_split[["var"]],
                                         stringsAsFactors = FALSE)
        var_importance_info <- rbind(var_importance_info, var_importance_new)
      }
      #update while loop condition (checks if there are open splits)
      do_splits <- !all(tree_info$state != "split")
    }#end for loop
  }#end while loop
  #extract all terminal leafs from tree_info data
  leafs <- tree_info[tree_info$state == "leaf", ]["filter"]
  #function to estimate cates in terminal leafs using the honest sample
  est_honest_fun <- function(x){
    #subset data according to filter rules of terminal leafs (get observations in terminal leafs)
    dat_hon <- subset(data_honest, eval(parse(text = leafs[x, "filter"])))
    #compute cate in each terminal leaf
    cate_hon <- mean(subset(dat_hon, dat_hon[[treatment]] == 1)[[target]]) - mean(subset(dat_hon, dat_hon[[treatment]] == 0)[[target]])
    return(cate_hon)
  }
  #execute loop over terminal leafs
  cate_hon <- purrr::map_dbl(1:nrow(leafs), ~est_honest_fun(.x))
  leafs$cate <- cate_hon
  #return tree info and cate estimates
  return(list(tree = tree_info, fit = leafs, variable_importance_info = var_importance_info))
}
