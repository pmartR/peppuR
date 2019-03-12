require(caret)

#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to BGLR(...)
#' 
#' @return the object created by BGLR

peppuR_knn <- function(X_train, X_test, train_class, train_partition, test_partition, ...) {
    knn_fit = knn3Train(X_train, X_test, cl = train_class, ...)
    return(knn_fit)
}
