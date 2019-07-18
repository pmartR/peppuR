#' Plots an object of class mlSelect
#' 
#' For plotting an S3 object of type 'uniFiltRes'
#'
#' @param uniFiltRes an object of the class 'uniFiltRes', usually created by \code{\link{univariate_feature_selection}}.
#' @param pval_threshold is the cutoff value for pvalues, can be a single value or a vector of distinct pvalues
#' 
#' @details 
#' @return plots ggplot2 object
#'
#' @examples
#' 

plot.mlSelect <- function(results_object, roc_curves = TRUE, time_chart = FALSE, ...) {
  require(ggplot2)
  require(gridExtra)
  .plot.mlSelect(results_object, roc_curves = TRUE, time_chart = FALSE, ...)
}

.plot.mlSelect <- function(results_object, roc_curves = TRUE, time_chart = FALSE, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
  # check for a corRes object #
  if(!inherits(results_object, "mlSelect")) stop("object must be of class 'mlSelect'")
  # check title and colorbar options #
  if(!is.null(plot_title)) {
    if(!is.character(plot_title)) stop("plot_title must be a character vector")
  }
  if(roc_curves){
    plots <- lapply(seq_along(results_object), function(i,algs) {
      fprs <- unlist(lapply(algs[[i]], function(metrics) metrics$fpr))
      tprs <- unlist(lapply(algs[[i]], function(metrics) metrics$tpr))
      Algorithm <- unlist(gsub("[0-9]","", names(fprs)))
      plot_frame <- data.frame(fprs, tprs, Algorithm)
      ggplot(plot_frame, aes(x = fprs, y = tprs, color = Algorithm))+
        geom_line()+
        ggtitle(paste(names(algs[i])))+theme(legend.position = 'none') 
    }, algs = results_object)
    plots[[length(plots)]] <- plots[[length(plots)]]+theme(legend.position = 'bottom') 
    roc_plot <- do.call("grid.arrange", list(grobs = plots, top = grid::textGrob(plot_title, gp=grid::gpar(fontsize=20))))
  } else {
    roc_plot <- NULL
  }
  if(time_chart){
    time_chart <- NULL
  } else {
    time_chart <- NULL
  }
  return(roc_plot)
}

