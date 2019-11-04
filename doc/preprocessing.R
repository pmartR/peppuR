## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(peppuR)
data("single_source")
single_source_peppuRobj <- univariate_feature_selection(single_source)

