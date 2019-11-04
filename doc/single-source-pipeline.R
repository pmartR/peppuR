## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", tidy.opts = list(width.cutoff = 70))
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(peppuR)
library(ggplot2)
library(dplyr)
library(kableExtra)
set.seed(1014)

## ----data----------------------------------------------------------------
library(MASS)
dim(birthwt)
head(birthwt)

## ----process-------------------------------------------------------------
# Add subject names to the data
birthweight_data <- birthwt
birthweight_data$ID <- paste("ID",1:nrow(birthweight_data), sep = "_")
birthweight_data$low <- as.factor(birthweight_data$low)


# Make categorical columns factors
birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")] <- lapply(birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")], function(x) as.factor(x))

# Create an organized data object
single_source_peppuRobj <- as.MLinput(X = birthweight_data, Y = NULL, 
                                      meta_colnames = c("ID", "low"),
                                      categorical_features = TRUE,
                                      sample_cname = "ID", outcome_cname = "low")

## ----view_data-----------------------------------------------------------
attributes(single_source_peppuRobj)

## ----partition-----------------------------------------------------------
single_source_peppuRobj <- dataPartitioning(single_source_peppuRobj, folds = 5, repeats = 10)

## ----leearn--------------------------------------------------------------
rf_results <- MLwrapper(data_object = single_source_peppuRobj, methods = "rf")
#----Take a look at the results ------#
output_probabilities <- attr(rf_results, "ML_results")

## ----view_results, echo=FALSE, tidy.opts=list(width.cutoff=50)-----------
temp <- data.frame(output_probabilities)[1:10,1:5]
colnames(temp) <- gsub(pattern= '1\\.rf', replacement = '1\nrf', x = colnames(temp))
knitr::kable(temp, row.names = FALSE, align = 'l', booktabs = TRUE) %>%
  kableExtra::column_spec(column = c(1:5), width = '7cm')

## ----plot_results--------------------------------------------------------
library(ggplot2)
plot(rf_results)[[1]]+ggtitle("Random Forest ROC")


## ----rofi, cache=TRUE----------------------------------------------------
#-----ROFI------#
source_alg_pairs <- "rf"
names(source_alg_pairs) <- "DS_1"
rofi_results <- rofi(single_source_peppuRobj, source_alg_pairs, nn = 5, f_prob=0.4,
                    nu=1/100, max_iter = 35)

## ----plot_rofi-----------------------------------------------------------
plot(rofi_results)

