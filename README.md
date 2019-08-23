<img width="274" alt="peppuR" src="https://user-images.githubusercontent.com/8594663/54779914-a3d0d400-4bd5-11e9-9ea0-192196c0ef49.png"> 

# peppuR: Pipieline for the Evaluation and Predictive analysis of Paired and Unpaired Research designs

  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/lmbramer/peppuR/branch/master/graph/badge.svg)](https://codecov.io/gh/lmbramer/peppuR?branch=master)
  <!-- badges: end -->

This R package provides functionality for pre-processing of paired and unpaired data in preparation for the application machine learning algorithms. The analysis pipeline carefully documents steps so that the end data product is fully reproducible. This package was designed to seamlessly handle single or multiple data types with an ultimate end goal of integrated and flexible machine learning and feature selection over fundamentally different data sources. 

## Installation:
```r
devtools::install_github("pmartR/peppuR")
```

## Usage
Get started by following along with the intro vignette:
```r
vignette("single-source-pipeline", package = "peppuR")
```
