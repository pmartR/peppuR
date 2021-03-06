% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataPartitioning.R
\name{dataPartitioning}
\alias{dataPartitioning}
\title{Returns information on how to partition y_data}
\usage{
dataPartitioning(data_object, partition_style = "random", folds = 4,
  repeats = 100, holdout_perc = 0.25)
}
\arguments{
\item{data_object}{argument is the output produced by as.MLinput, which contains a single x data frame or a list of x data frames, a y data frames and attributes}

\item{partition_style}{one of 'random' or 'paired' (character string) indicating type of partition}

\item{folds}{integer of k for k-fold cross validation}

\item{repeats}{integer of number of iterations to repeat cross validation}

\item{holdout_perc}{numeric between 0 and 1 indicating the percentage of data to withold for the holdout validation set}
}
\description{
This function returns information on how to partition (randomly or paired) y_data
importFrom magrittr "%>%"
}
