require(kernlab)
require(naivebayes)
require(ranger)
require(caret)
require(MASS)

#' Applies ML algorithms to partitioned data
#'
#' This function applies ML algorithms to 'MLinput' object
#'
#' @param data_object object of class 'MLinput' a list of two data frames
#'   containing train and test rows to subset in x_data and y_data
#' @param methods a vector of strings specifying which ML algorithm you want to
#'   implement? options: 'bglr', 'clogit'. The length of this vector must be
#'   equal to the number of data sources in data_object
#' @param single_source is a character string identifying which data source
#'   (when there is more than 1 data source in data_object) to apply ML to.
#'   Defaults to NULL
#'
#' @export
MLwrapper = function(data_object, methods, scale_and_center = FALSE, single_source = NULL) {
  # match 'method' argument with available options
  optns <- c("svm", "rf", "nb", "knn", "lda")
  
  if (!(all(methods %in% optns))) 
    stop(paste("Provided 'method' argument is invalid, please select from", c(optns), "."))
  
  # check that partition_info attr is present in data_object
  if (is.null(attr(data_object, "partition_info"))) 
    stop("dataPartitioning has not been applied")
  
  # extract number of sources
  n_sources = attr(data_object, "n_sources")
  
  # check if single_source is provided then data_object must have more than one data source
  # if (!is.null(single_source) & n_sources == 1) 
  #   stop("'single_source' can only be provided when 'data_object' has more than one data source")
  # 
  # case where n_sources is 1
  #if (n_sources == 1) {
    # extract parts from data_object
    # x_data = data_object$X
    # y_data = data_object$Y
    # 
    # part_info = attr(data_object, "partition_info")
    # 
    # # extract cnames
    # sample_cname = attr(data_object, "cnames")$sample_cname
    # outcome_cname = attr(data_object, "cnames")$outcome_cname
    # pair_cname = attr(data_object, "cnames")$pair_cname
    # 
    # # sample_cname column index
    # samp_ind = which(names(x_data) %in% sample_cname)
    # 
    # # loop over the list of methods and apply them with the MLwrapper_helper
    # ml_rez = lapply(methods, MLwrapper_helper, x_data, y_data, part_info, scale_and_center, outcome_cname, pair_cname, sample_cname)
    # names(ml_rez) = methods
    # 
    # attr(data_object, "ML_results") = ml_rez
    # attr(data_object, "ML_method") = methods
    # 
 # }
   # else if (n_sources > 1) {
    # check that the length of 'method' vector is equal to n_sources, note this does not apply when 'single_source' argument is
    # provided
    if (is.null(single_source)) {
      if (length(methods) != n_sources) 
        stop("the number of methods provided in 'method' argument is not equal to the number of sources in 'data_object'")
    
    
    # extract parts from data_object
    x_data = data_object$X
    y_data = data_object$Y
    
    part_info = attr(data_object, "partition_info")
    
    # extract cnames
    sample_cname = attr(data_object, "cnames")$sample_cname
    outcome_cname = attr(data_object, "cnames")$outcome_cname
    pair_cname = attr(data_object, "cnames")$pair_cname
    
    # sample_cname column index
    #samp_ind = which(names(x_data[[1]]) %in% sample_cname)
    ml_rez_list = mapply(FUN = MLwrapper_helper, method = methods, X = x_data, MoreArgs = list(y_data, part_info, scale_and_center, 
                                                                                               outcome_cname, pair_cname, sample_cname))
    ml_rez_list = as.data.frame(ml_rez_list)
    ml_rez_list = lapply(ml_rez_list, as.data.frame, stringsAsFactors = FALSE)
    
    names(ml_rez_list) = paste(names(x_data), names(ml_rez_list), sep = ".")
    
    attr(data_object, "ML_results") = ml_rez_list
    attr(data_object, "ML_method") = methods
    }
    # case where single_source is provided
    else if (!is.null(single_source)) {
      
      # extract parts from data_object
      x_data = data_object$X
      y_data = data_object$Y
      part_info = attr(data_object, "partition_info")
      
      # check that single_source is one of the data sources in data_object
      if (!(single_source %in% names(x_data))) 
        stop(paste(single_source, " is not one of the data source names of 'data_object'", sep = ""))
      
      # select the correct data source
      x_data = x_data[which(names(x_data) %in% single_source)][[1]]
      #names(x_data) = NULL
      #x_data = as.data.frame(x_data)
      
      # extract cnames
      sample_cname = attr(data_object, "cnames")$sample_cname
      outcome_cname = attr(data_object, "cnames")$outcome_cname
      pair_cname = attr(data_object, "cnames")$pair_cname
      
      # sample_cname column index
      #samp_ind = which(names(x_data) %in% sample_cname)
      
      ml_rez = lapply(methods, MLwrapper_helper, x_data, y_data, part_info, scale_and_center, outcome_cname, pair_cname, sample_cname)
      names(ml_rez) = methods
      
      # add attributes to data_object
      attr(data_object, "ML_results") = ml_rez
      attr(data_object, "ML_method") = methods
      attr(data_object, "single_source") = single_source
      
    }
  #}
  
  return(data_object)
}




MLwrapper_helper = function(method, X, data, partition_info, scale_and_center, outcome_cname, pair_cname, sample_cname) {
  # extract partition_info
  
  train_partitions = partition_info$train
  test_partitions = partition_info$test
  if(sample_cname %in% colnames(X)){
    X <- X[,-which(colnames(X) %in% sample_cname), drop=FALSE]
  }
  
  # switch statement
  ml_method <- switch(method, svm = peppuR_svm, rf = peppuR_rf, nb = peppuR_nb, knn = peppuR_knn, lda = peppuR_lda)
  
  # if knn, the model is memoryless and can't be stored
  pred_labels <- mapply(function(train, test) {
    train_partition = train$Train
    test_partition = test$Test
    
    
    
    if (scale_and_center) {
      X_train <- scale(X[train_partition, , drop = FALSE], scale = TRUE, center = TRUE)
      X_test <- scale(X[test_partition, , drop = FALSE], center = attr(X_train, "scaled:center"), scale = attr(X_train, 
                                                                                                               "scaled:scale"))
    }
    start <- Sys.time()
    # If the model is knn, treat it differently
    if(tolower(method) == "knn") { 
      model <- "memoryless"
      # figure out the best way to scale and center the data
      X_train <- scale(X[train_partition, ], scale = TRUE, center = TRUE)
      X_test <- (X[test_partition, ] - attr(X_train, "scaled:center"))/attr(X_train, "scaled:scale")
      start <- Sys.time()
      pred_prob <- attr(ml_method(X_train, X_test, train_class = as.factor(data[train_partition, outcome_cname]), prob = TRUE), 
                        "prob")
      train_time <- Sys.time() - start
      # 
      # pred_label <- as.numeric(apply(pred_prob, 1, function(x) names(x)[which.max(x)]))
      # truth <- data[test_partition, outcome_cname, drop = FALSE]
      # ID <- data[test_partition, sample_cname, drop = FALSE]
      # res <- data.frame(PredictedProbs = pred_prob, PredictedLabel = pred_label, Truth = truth, SampleID = ID)
      # attributes(res)$train_time <- train_time
    } else {
      # figure out the best way to scale and center the data
      #X_train <- scale(X[train_partition, ], scale = TRUE, center = TRUE)
      #X_test <- (X[test_partition, ] - attr(X_train, "scaled:center"))/attr(X_train, "scaled:scale")
      start <- Sys.time()
      # model <- try(attr(ml_method(X_train, X_test, train_class = as.factor(data[train_partition, outcome_cname]), prob = TRUE), 
      #                   "prob"))
      # train_time <- Sys.time() - start
      # }
      # 
      # pred_label <- as.numeric(apply(pred_prob, 1, function(x) names(x)[which.max(x)]))
      # truth <- data[test_partition, outcome_cname, drop = FALSE]
      # ID <- data[test_partition, sample_cname, drop = FALSE]
      # res <- data.frame(PredictedProbs = pred_prob, PredictedLabel = pred_label, Truth = truth, SampleID = ID)
      # attributes(res)$train_time <- train_time
      model <- try(ml_method(X = X[train_partition, , drop = FALSE], data = data[train_partition, , drop = FALSE], 
                             outcome_cname = outcome_cname, pair_cname = pair_cname))
      train_time <- Sys.time() - start
      
      if (class(model) == "try-error") {
        res <- data.frame(PredictedProbs.0 = rep(1, length(test_partition)), PredictedProbs.1 = rep(1, length(test_partition)), 
                          PredictedLabel = rep(NA, length(test_partition)), Truth = data[test_partition, outcome_cname, drop = FALSE])
        return(res)
      }
      
      # lda uses a different syntax to return probabilities
      
      if (method == "lda") {
        pred_prob <- predict(model, newdata = X[test_partition, , drop = FALSE])$posterior
      } else if (method == "rf") {
        pred_prob <- predict(model, data = data.frame(X[test_partition, , drop = FALSE]))$predictions
        #pred_prob <- data.frame(1 - as.numeric(pred_prob), pred_prob)
        colnames(pred_prob) <- c("0", "1")
      } else if (method == "svm") {
        pred_prob <- predict(model, newdata = X[test_partition, , drop = FALSE])
        pred_prob <- 1/(1 + exp(-pred_prob))
        pred_prob <- data.frame(1 - pred_prob, pred_prob)
        colnames(pred_prob) <- c("0", "1")
      } else if(method=="nb"){
        pred_prob <- predict(model, newdata = X[test_partition, , drop = FALSE],type = "prob")
      }else{
        pred_prob <- predict(model, newdata = X[test_partition, , drop = FALSE])
      }
    }
    
    #
    pred_label <- as.numeric(apply(pred_prob, 1, function(x) names(x)[which.max(x)]))
    truth <- data[test_partition, outcome_cname, drop = FALSE]
    ID <- data[test_partition, sample_cname, drop = FALSE]
    res <- data.frame(PredictedProbs = pred_prob, PredictedLabel = pred_label, Truth = truth, SampleID = ID, Time = rep(train_time, length(pred_label)))
    attributes(res)$train_time <- train_time
    return(res)
    
  }, train_partitions, test_partitions)
  
  
  
  pred_labels = as.data.frame(pred_labels)
  pred_labels = lapply(pred_labels, as.data.frame, stringsAsFactors = FALSE)
  # organize data
  part_names = names(pred_labels)
  
  
  result = mapply(function(item, name) {
    rez = cbind(item, rep(name, nrow(item)))
    names(rez)[4] = "Truth"
    names(rez)[6] = "Time"
    names(rez)[7] = "Partition_info"
    
    return(rez)
    
  }, pred_labels, part_names)
  
  result = as.data.frame(result)
  result = lapply(result, as.data.frame, stringsAsFactors = FALSE)
  result = do.call(rbind, result)
  
  return(result)
}

