#' Plots an object of class featSelect
#' 
#' For plotting an S3 object of type 'featSelect'
#'
#' @param featSelect_object an object of the class 'featSelect', currently created by \code{\link{rofi}}.
#' @param order_plot logical indicator for plotting order plots
#' 
#' @return plots ggplot2 object
#' @export

plot.rofi <- function(featSelect_object, order_plot = TRUE, ...) {
  require(ggplot2)
  require(gridExtra)
  .plot.rofi(featSelect_object, order_plot = TRUE, ...)
}

.plot.rofi <- function(featSelect_object, order_plot = TRUE, x_lab = NULL, y_lab = NULL, plot_title = NULL, title_size = 14, x_lab_size = 11, y_lab_size = 11, bw_theme = FALSE) {
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