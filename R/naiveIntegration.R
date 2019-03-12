#'Combine probabilities from multiple data sources.
#'
#'@param probabilities_by_source a list of data.frames with one column for each
#'  predicted class corresponding to the probability of that class label given
#'  the data source. Each data frame corresponds to a data source. All
#'  probabilities in each row must sum to 1. The rows are generally cross
#'  validated test sets.
#'
#'@details This function combines class probabilities from learning algorithms
#'  on multiple or single data sources over cross validation partitions. The
#'  mechanism for combining the probabilities is a posterior probability where
#'  \deqn{P(c_{j} | x_{i}) = /prod{P(c_{j} | x_{i}^{s})}_{s=1}^{ns}} where
#'  \eqn{c} is one of \eqn{j} class labels, \eqn{x} is observed data for one of
#'  \eqn{ns} data sources.
#'@export
#'
naiveIntegration <- function(probabilities_by_source){
  nobs <- lapply(probabilities_by_source, nrow)
  if (sum(unlist(nobs)) != unlist(nobs[1])*length(nobs)) { # check that all dfs have the same number of rows
    stop("The same number of observations is needed for all sources")
  }
  # strip away columns that are not probabilities
  # by design these lists are in the same order
  meta_info <- probabilities_by_source[[1]] %>%
    dplyr::select(Truth, Partition_info) #rename and correct partition info later

  probabilities_by_source <- lapply(probabilities_by_source, function(d_source){
    return(d_source[, grepl("Prob", colnames(d_source))])
  })
  #---- multiply dataframes together elementwise------#
  # (e.g. the first row/column in each dataframe is multiplied together)
  integrated_probs <- Reduce("*", probabilities_by_source)
  integrated_probs <- integrated_probs/apply(integrated_probs, 1, sum)
  PredictedLabel <- apply(integrated_probs, 1, function(x) names(x)[which.max(x)])
  PredictedLabel <- gsub(pattern = "PredictedProbs.",replacement = "", x = PredictedLabel)
  
  meta_info <- cbind(PredictedLabel, meta_info)
  #--- return meta information back to the probabilities
  results <- cbind(integrated_probs, meta_info)
  return(results)
}
