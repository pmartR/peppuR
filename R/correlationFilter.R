#' Removes features that are clustered by correlation, replacing each cluster
#' with a single feature at a minimum within cluster correlation threshold
#'
#' This function removes features for a single data frame or a list of data
#' frames
#'
#' @param data_object argument is the output produced by as.ML function, which
#'   contains a single x data frame or a list of x data frames, a y data frames
#'   and attributes
#' @details
#'
#' @export
correlationFilter <- function(data_object){
  x = data_object$X
  sample_cname = attr(data_object, "cnames")$sample_cname
  n_sources = attr(data_object, "n_sources")
  categorical_cols = attr(data_object, "categorical_columns")$categorical_cols
  
  # cases where n_sources == 1 and n_sources > 1
  
  # if (n_sources == 1) {
  #   covariates <- data_object$X[,-which(colnames(data_object$X) == sample_cname)]
  #   cluster_obj <- klaR::corclust(covariates)
  # } else if (n_sources > 1) {
  
    covariates = lapply(data_object$X, function(data_source) data_source[,-which(colnames(data_source) == sample_cname)])
    cluster_obj <- lapply(covariates, function(source_cov) klaR::corclust(source_cov))
#  }
  #assign class 'uniFiltRes' to x_result
  return(cluster_obj)
  # attr(data_object, "correlationFiltering") = TRUE
  # 
  # return(data_object)
}

