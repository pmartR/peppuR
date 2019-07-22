require(MASS)

#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to BGLR(...)
#' 
#' @return the object created by BGLR

peppuR_lda <- function(X, data, outcome_cname, ...) {
    # set.seed(42)
    lda_fit <- lda(x = X, grouping = as.factor(data[[outcome_cname]]), ...)
    return(lda_fit)
}


