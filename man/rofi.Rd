% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rofi.R
\name{rofi}
\alias{rofi}
\title{Repeated Optimized Feature Integration}
\usage{
rofi(MLinput, source_alg_pairs, nn = 1, f_prob = 0.1, nu = 1/100,
  max_iter = 2 * sum(attr(MLinput, "data_info")$number_of_features),
  conv_check = (sum(attr(MLinput, "data_info")$number_of_features) + 1),
  epsilon = 0.01, after_conv_checks = 100)
}
\arguments{
\item{MLinput}{an as.MLinput object which contains a single X data frame or a
list of X data frames, a Y data frame and attributes}

\item{source_alg_pairs}{a named vector of algorithms (one of "knn", "nb",
"svm", or "rf") with names as the corresponding data source}

\item{nn}{integer. The number of times to repeat the optimization in its
entirety}

\item{f_prob}{numeric greater than 0 and leq 1. The proportion of the full
feature set to initialize the optimization routine}

\item{nu}{numeric greater than 0 and leq 1. The scale value for feature
acceptance criteria of a difference in AUC values}

\item{max_iter}{int. Maximum number of iterations to allow in nn iterations}

\item{conv_check}{int. Number of iterations at which to perform a
convergence check. Typically set to the total number of features}

\item{epsilon}{numeric greater than 0 and leq 1. AUC convergence threshold,
typically small (< 0.1).}

\item{after_conv_checks}{int. After the initial convergence check, the
Interval of iterations at which to perform a convergence check}
}
\description{
Repeated Optimized Feature Integration
}
