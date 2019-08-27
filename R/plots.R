#' plots MLinput, featSelect objects
#' @details
#' Various further arguments can be specified depending on the class of the object being plotted.
#'
#' @name plot-peppuR
#'
NULL

#' Plots an object of class MLInput
#'
#' @param MLinput_object an object of the class 'MLInput', usually created by \code{\link{as.MLInput}}.
#' @return plots ggplot2 object
#' @rdname plot-peppuR-MLinput
#' @export
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
    if(attr(MLinput_object, "n_sources") == 1){
      results <- output_probabilities[[1]]
    } else {
      results <- naiveIntegration(output_probabilities) 
    }
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

#' plot.featSelect
#' Plots an object of class featSelect
#' 
#' For plotting an S3 object of type 'featSelect'
#' @rdname plot-peppuR-featSelect
#' @param featSelect_object an object of the class 'featSelect'
#' @param order_plot logical indicator for plotting order plots
#' @return plots ggplot2 object
#' @export
plot.featSelect <- function(featSelect_object, order_plot = TRUE, ...) {
  require(ggplot2)
  require(gridExtra)
  .plot.rofi(featSelect_object, order_plot = TRUE, ...)
}

.plot.featSelect <- function(featSelect_object, order_plot = TRUE, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
  # check for a corRes object #
  if(!inherits(featSelect_object, "featSelect")) stop("object must be of class featSelect'")
  # check title and colorbar options #
  if(!is.null(plot_title)) {
    if(!is.character(plot_title)) stop("plot_title must be a character vector")
  }
  plot <- ggplot2::qplot(reorder(Feature, -importance_metric),importance_metric,data=featSelect_object[[1]],colour=Source)+
    xlab("Variable Number")+ylab("Probability in Final Feature Set")+theme(axis.text.x = element_text(angle = 90))+theme_bw()
  return(plot)
}

#' Plots an object of class mlSelect
#' 
#' For plotting an S3 object of type 'mlSelect'
#'
#' @param mlSelect_object an object of the class 'mlSelect', usually created by 
#' @param roc_curves logical indicator for plotting ROC curves
#' @param time_chart logical indicator for plotting method run times
#' 
#' @return plots ggplot2 object
#' @rdname plot-peppuR-mlSelect
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
