#' Plots an object of class MLInput
#'
#' @param MLinput_object an object of the class 'MLInput', usually created by \code{\link{as.MLInput}}.
#' @return plots ggplot2 object
#' @export
#' 
plot.MLinput <- function(MLinput_object, order_plot = TRUE, ...) {
  require(ggplot2)
  require(gridExtra)
  list(.plot.roc(MLinput_object, ...), .plot.data(MLinput_object, ...))
}

.plot.roc <- function(MLinput_object, order_plot = TRUE, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
  # check for a corRes object #
  if(!inherits(MLinput_object, "MLinput")) stop("object must be of class MLinput'")
  # check title and colorbar options #
  if(!is.null(plot_title)) {
    if(!is.character(plot_title)) stop("plot_title must be a character vector")
  }
  if(is.null(attr(MLinput_object, "ML_results"))) {
    roc_plot <- NULL
  } else {
    output_probabilities <- attr(MLinput_object, "ML_results")
    results <- naiveIntegration(output_probabilities)  
    metrics <- AUC::roc(predictions = results$PredictedProbs.1, labels = factor(results$Truth))  
    roc_plot <- ggplot(data.frame(fpr = metrics$fpr, tpr = metrics$tpr), aes(x = fpr, y = tpr))+
      geom_line(color = "blue")+
      theme_bw()+
      ggtitle(plot_title)  
  }
  return(roc_plot)

}

.plot.data <- function(MLinput_object, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
  warning("Data plotting not yet implemented")
}