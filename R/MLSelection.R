#' Applies the MLWrapper to all data sources with all algorithms and returns
#' performance and timing metrics for each source/algorithm combination
#'
#' importFrom magrittr "%>%"
#' @param data_object argument is the output produced by as.MLinput, which
#'   contains a single x data frame or a list of x data frames, a y data frames
#'   and attributes
#' @param partition_style one of 'random' or 'paired' (character string)
#'   indicating type of partition
#' @param folds integer of k for k-fold cross validation
#' @param repeats integer of number of iterations to repeat cross validation
#' @param algorithms either 'all', or one of 'rf', 'svm', 'nb', 'knn'. May also
#'   include a vector combination of the latter (e.g. c('rf','svm'))
#'
#' @export
#' 
MLSelection <- function(data_object, partition_style = 'random', folds = 3, repeats = 10, algorithms = 'all', data_sources = 'all', single_source = NULL) {
  #setup partition information
  if(!is.null(attr(data_object, "Partition_info"))){
    existing_partitions <- attr(data_object, "Partition_info")
    attr(data_object, "Partition_info") <- NULL
  }
  data_object <- dataPartitioning(data_object, partition_style = partition_style, folds = folds, repeats = repeats)
  #setup algorithm information
  if(algorithms == 'all') {
    used_methods <- c("knn", "rf", "nb", "svm", "lda")
  } else {
    used_methods <- algorithms
  }
  # try all algorithms on all data sources
  if(data_sources == 'all'){# & attr(data_object,"n_sources") > 1){
    results <- vector("list", attr(data_object,"n_sources"))
    for(sources in 1:attr(data_object,"n_sources")){
      temp_results <- MLwrapper(data_object = data_object, methods = used_methods, single_source = names(data_object$X)[sources])
      output_probabilities <- attr(temp_results, "ML_results")
      #browser()
      results[[sources]] <- lapply(output_probabilities, function(x){
        AUCs <- try(AUC::roc(predictions = x$PredictedProbs.1, labels = factor(x$Truth)),silent = TRUE )
        attributes(AUCs)$Time <- try(mean(x$Time))
        return(AUCs)
    })
      #------------ This wont work with the current data structure. Catch this later ----------#
      if (any(class(results[[sources]]) == "try-error")){
        results[[sources]] <- NA
      }
    }
    names(results) <- names(data_object$X)
  }
  #assign class 'mlSelect' to results
  class(results) = c("mlSelect", "list")
  return(results)
}