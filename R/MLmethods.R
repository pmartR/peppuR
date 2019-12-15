# require(randomForest)
require(ranger)
#' Random Forest method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to ranger(...)
#' 
#' @return the object created by ranger::randomforest
#' @rdname peppuR_rf
#' @export

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

require(kernlab)

#' SVM method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to svm(...)
#' @return the object created by kernlab::svm
#' @rdname peppuR_svm
#' @export

peppuR_svm <- function(X, data, outcome_cname, ...) {
  # print(...)
  
  if (all(as.numeric(data[[outcome_cname]]) >= 0)) {
    # Translate Y=0/1 to Y=-1/1
    data[outcome_cname] <- 2 * as.numeric(data[[outcome_cname]]) - 1
  }
  
  return(kernlab::ksvm(x = data.matrix(X), y = data[outcome_cname], prob.model = TRUE, verbose = FALSE, ...))
}

require(MASS)
#' LDA method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#'        pair - vector of pair identifiers
#'        time - n vector of time point identifiers
#' @param ... - additional arguements passed to lda(...)
#' 
#' @return the object created by MASS::lda
#' @rdname peppuR_lda
#' @export

peppuR_lda <- function(X, data, outcome_cname, ...) {
  # set.seed(42)
  lda_fit <- MASS::lda(x = X, grouping = as.factor(data[[outcome_cname]]), ...)
  return(lda_fit)
}

#' LR method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#' @param ... - additional arguements passed to glm(...)
#' 
#' @return the object created by stats::glm
#' @rdname peppuR_lr
#' @export

peppuR_lr <- function(X, data, outcome_cname, ...) {

  lr_formula <- (paste(outcome_cname, "~", sep = ""))
  bound_data <- cbind(data[,which(colnames(data) %in% outcome_cname), drop=FALSE], X)
  colnames(bound_data)[1] <- outcome_cname
  
  ##----------------------##
  ## Deal with covariates ##
  ##----------------------##
  #clogit is expecting a formula so take the colnames of X and build the formula
  if(is.null(colnames(X))){
    colnames(X) <- paste0("Col",1:ncol(X))
  }
  Xcnames <- colnames(X)
  lr_formula <- paste(lr_formula,Xcnames[1])
  for(i in 2:length(Xcnames)){
    lr_formula <- paste0(lr_formula," + ",Xcnames[i])
  }
  lr_formula <- as.formula(lr_formula)
  # these should already be in the same order
  lr_fit  <- glm(formula = lr_formula, data = bound_data, family=binomial)
  
  return(lr_fit)
}


require(caret)

#' KNN method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#' @param ... - additional arguements passed to knn3Train(...)
#' 
#' @return the object created by caret::knn3Train
#' @rdname peppuR_knn
#' @export

peppuR_knn <- function(X_train, X_test, train_class, train_partition, test_partition, ...) {
  knn_fit = knn3Train(X_train, X_test, cl = train_class, ...)
  return(knn_fit)
}

require(naivebayes)

#' Naive Bayes method
#' @param X - n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y
#' @param data - a data.frame with columns:
#'           y - n vector of class identifiers for each for of X
#'          ID - n vector of patient IDs
#' @param ... - additional arguements passed to naive_bayes(...)
#' @return the object created by naivebayes::naive_bayes
#' @rdname peppuR_nb
#' @export

peppuR_nb <- function(X, data, outcome_cname, sample_cname, ...) {
  
  #Remove sample_cname columns from X
  #X <- X[,-which(colnames(X)==sample_cname)]
  # if(!is.null(pair_cname)){
  #   if (length(data[[pair_cname]]) > 0) {
  #     # If pair information was provided, add it to the data matrix 
  #     #X <- cbind(X, pair = as.factor(data[[pair_cname]]))
  #   }
    # set.seed(42) nb_fit <- naiveBayes(x=X, y=data$y,...)
    ## BS Comment 5/29: did you mean "pair_cname" here or "outcome_cname"?
    #nb_fit <- naivebayes::naive_bayes(x = X, y = as.factor(data[[outcome_cname]]))
  #}else{
    # BS comment 5/29: very rough fix for now to drop "ID" column
    # BS comment 8/20: nb this version of nb requires factor response
    nb_fit <- naivebayes::naive_bayes(x = X, y = as.factor(data[[outcome_cname]]))
  #nb_fit <- e1071::naiveBayes(x = X, y = as.factor(data[[outcome_cname]]))
  #}
  return(nb_fit)
}



