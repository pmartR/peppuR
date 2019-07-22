# require(randomForest)
require(ranger)
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to BGLR(...)
#' 
#' @return the object created by BGLR

peppuR_rf <- function(X, data, outcome_cname, ...) {
    
    # Can't add pair information into a random forest like this if(length(data$pair)>0){ If pair information was provided, add it to
    # the data matrix warning('There is probably a better way to include pair information into a random forest') pair_mat <-
    # build_x_mat(data.frame(pair=as.factor(data$pair))) X <- cbind(X,pair_mat) } set.seed(42) rf_fit <- randomForest(x=X,
    # y=as.factor(data$y)) If you try to use the formula 'y~.' you will likely get the error: 'Error: protect(): protection stack
    # overflow' so I switched to the 'dependent.variable.name='y'' interface as suggested here:
    # https://github.com/imbs-hl/ranger/issues/103
    rf_fit <- ranger::ranger(data = data.frame(y = data[outcome_cname], X),
                     dependent.variable.name = outcome_cname,
                     importance = "impurity",
                     probability = TRUE)
    return(rf_fit)
}
