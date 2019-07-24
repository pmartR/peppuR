#' Plots an object of class mlSelect
#' 
#' For plotting an S3 object of type 'uniFiltRes'
#'
#' @param mlSelect_object an object of the class 'mlSelect', usually created by \code{\link{MLSelection}}.
#' @param roc_curves logical indicator for plotting ROC curves
#' @param time_chart logical indicator for plotting method run times
#' 
#' @return plots ggplot2 object
#' @export
#' 

plot.mlSelect <- function(mlSelect_object, roc_curves = TRUE, time_chart = TRUE, ...) {
  require(ggplot2)
  require(gridExtra)
  .plot.mlSelect(mlSelect_object, roc_curves = TRUE, time_chart = TRUE, ...)
}

.plot.mlSelect <- function(mlSelect_object, roc_curves = TRUE, time_chart = TRUE, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
  # check for a corRes object #
  if(!inherits(mlSelect_object, "mlSelect")) stop("object must be of class 'mlSelect'")
  # check title and colorbar options #
  if(!is.null(plot_title)) {
    if(!is.character(plot_title)) stop("plot_title must be a character vector")
  }
  if(roc_curves){
    plots <- lapply(seq_along(mlSelect_object), function(i,dsource) {
      fprs <- unlist(lapply(dsource[[i]], function(metrics) metrics$fpr))
      tprs <- unlist(lapply(dsource[[i]], function(metrics) metrics$tpr))
      Algorithm <- unlist(gsub("[0-9]","", names(fprs)))
      plot_frame <- data.frame(fprs, tprs, Algorithm)
      ggplot(plot_frame, aes(x = fprs, y = tprs, color = Algorithm))+
        geom_line()+
        ggtitle(paste(names(dsource[i])))+theme(legend.position = 'none') 
    }, dsource = mlSelect_object)
    plots[[length(plots)]] <- plots[[length(plots)]]+theme(legend.position = 'bottom') 
    roc_plot <- do.call("grid.arrange", list(grobs = plots, top = grid::textGrob(plot_title, gp=grid::gpar(fontsize=20))))
  } else {
    roc_plot <- NULL
  }
  if(time_chart){
    plot_data <- lapply(seq_along(mlSelect_object), function(i,dsource) {
      Algorithm <- names(dsource[[i]])
      Time <- unlist(lapply(dsource[[i]], function(algs) attributes(algs)$Time))
      AUC <- unlist(lapply(dsource[[i]], function(algs) AUC::auc(algs)))
      Source <- names(dsource)[i]
      plot_frame <- data.frame(Source, Algorithm, Time, AUC)
    }, dsource = mlSelect_object)
    plot_frame <- do.call("rbind", plot_data)
    time_plot <- ggplot(plot_frame, aes(x= Time, y = AUC, color = Algorithm))+geom_point()+
      facet_grid(rows = vars(Source))
  } else {
    time_plot <- NULL
  }
  return(list(roc_plot, time_plot))
}

