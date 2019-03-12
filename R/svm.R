require(kernlab)

#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param oneclass - if pair information is provided, should a one-class SVM be built?
#' @param ... - additional arguements passed to BGLR(...)
#' 
#' @return the object created by BGLR

peppuR_svm <- function(X, data, outcome_cname, ...) {
    # print(...)
    
    if (all(as.numeric(data[[outcome_cname]]) >= 0)) {
        # Translate Y=0/1 to Y=-1/1
        data[outcome_cname] <- 2 * as.numeric(data[[outcome_cname]]) - 1
    }
    
    return(ksvm(x = data.matrix(X), y = data[outcome_cname], prob.model = TRUE, verbose = FALSE, ...))
}



