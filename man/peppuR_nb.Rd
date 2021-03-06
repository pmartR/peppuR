% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLmethods.R
\name{peppuR_nb}
\alias{peppuR_nb}
\title{Naive Bayes method}
\usage{
peppuR_nb(X, data, pair_cname, outcome_cname, sample_cname, ...)
}
\arguments{
\item{X}{- n-by-p data.frame containing all of the 'covariates' that will be used to predict class of y}

\item{data}{- a data.frame with columns:
   y - n vector of class identifiers for each for of X
  ID - n vector of patient IDs
pair - vector of pair identifiers
time - n vector of time point identifiers}

\item{...}{- additional arguements passed to BGLR(...)}
}
\value{
the object created by naivebayes::naive_bayes
}
\description{
Naive Bayes method
}
