# require(e1071)
require(naivebayes)

#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to BGLR(...)
#' 
#' @return the object created by BGLR

peppuR_nb <- function(X, data, pair_cname, outcome_cname, sample_cname, ...) {
  
  #Remove sample_cname columns from X
  X <- X[,-which(colnames(X)==sample_cname)]
  
  if(!is.null(pair_cname)){
    if (length(data[[pair_cname]]) > 0) {
        # If pair information was provided, add it to the data matrix warning('There is probably a better way to include pair information
        # into an SVM')
        X <- cbind(X, pair = as.factor(data[[pair_cname]]))
    }
    # set.seed(42) nb_fit <- naiveBayes(x=X, y=data$y,...)
    ## BS Comment 5/29: did you mean "pair_cname" here or "outcome_cname"?
    nb_fit <- naive_bayes(x = X, y = as.factor(data[[pair_cname]]))
  }else{
    # BS comment 5/29: very rough fix for now to drop "ID" column
    nb_fit <- naive_bayes(x = X, y = data[[outcome_cname]])
  }
    return(nb_fit)
}

