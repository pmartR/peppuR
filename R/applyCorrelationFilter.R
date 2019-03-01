#' Removes features that are clustered by correlation, replacing each cluster
#' with a single feature at a minimum within cluster correlation threshold
#'
#' This function removes features for a single data frame or a list of data
#' frames
#'
#' @param data_object argument is the output produced by as.ML function, which
#'   contains a single x data frame or a list of x data frames, a y data frames
#'   and attributes
#' @param corFilt_object TODO:write
#' @param threshold Maximum accepted average within cluster correlation for
#'   selection of a features
#' @details
#'
#' @export
applyCorrelationFilter <- function(data_object, corFilt_object, threshold){
  # extract x and y data frames from data_object, as well as cnames
  sample_cname = attr(data_object, "cnames")$sample_cname
  n_sources = attr(data_object, "n_sources")
  x = data_object$X
  
  # first check that the threshold is appropriate
  if (any(threshold > 1 | threshold < 0)) {
    stop("correlation threshold must be between 0 and 1")
  }
  if (length(threshold) != n_sources) {
    stop("the number of correlation threshold values must match the number of sources")
  }
  # cases where n_sources == 1 and n_sources > 1
  if (n_sources == 1) {
    new_x = applyFilt_helper(x_mat = x, corFilt_obj = corFilt_object, sample_cname = sample_cname, thresh=threshold)
    newx_att = attr(new_x, "features_removed")
    data_object$X = new_x
    attr(data_object, "correlation_features_rm") = newx_att
    attr(data_object, "correlationFiltering") = TRUE
  } else if (n_sources > 1) {
    new_x_list = mapply(applyFilt_helper, x, corFilt_object, threshold, MoreArgs = list(sample_cname), USE.NAMES = TRUE)
    newx_list_att = lapply(new_x_list, function(item) {
      attr(item, "features_removed")
    })
    data_object$X = new_x_list
    attr(data_object,  "correlation_features_rm") = newx_list_att
    attr(data_object, "correlationFiltering") = TRUE
  }
#  plot(ccres, mincor = 0.7)
  return(data_object)
}

applyFilt_helper = function(x_mat, corFilt_obj, thresh=threshold, sample_cname) {
  samps <- x_mat[,sample_cname]
  x_mat <- x_mat[,-which(colnames(x_mat) == sample_cname)]
  cvtres <- klaR::cvtree(corFilt_obj, mincor = thresh) #minimum within cluster correlation
  # remove these features from x data frame
  newdata <- klaR::xtractvars(cvtres, x_mat, thres = thresh) # maximum accepted average within cluster correlation for selection of a variable
  newdata <- cbind(newdata, samps)
  names(newdata)[ncol(newdata)] <- sample_cname
  features_rm <- colnames(x_mat)[!colnames(x_mat) %in% colnames(newdata)]
  attr(newdata, "features_removed") = features_rm
  return(newdata)
}
