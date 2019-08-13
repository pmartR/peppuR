## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(peppuR)
library(ggplot2)
set.seed(1014)

## ------------------------------------------------------------------------
library(MASS)
dim(birthwt)
head(birthwt)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
attributes(single_source_peppuRobj)

## ------------------------------------------------------------------------
single_source_peppuRobj <- dataPartitioning(single_source_peppuRobj, folds = 5, repeats = 10)

## ------------------------------------------------------------------------
rf_results <- MLwrapper(data_object = single_source_peppuRobj, methods = "rf")
#----Take a look at the results ------#
output_probabilities <- attr(rf_results, "ML_results")

## ---- echo=FALSE---------------------------------------------------------

knitr::kable(data.frame(output_probabilities)[1:10,], row.names = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
#plot(rf_results, roc = TRUE)
# 
# temp <- AUC::roc(output_probabilities$source1.rf$PredictedProbs.1, labels = output_probabilities$source1.rf$Truth)
# 
# ggplot(data.frame(fpr = temp$fpr, tpr = temp$tpr), aes(x = fpr, y = tpr))+
#   geom_line(color = "blue")+
#   theme_bw()+
#   ggtitle("Random Forest ROC")

plot(rf_results)[[1]]+ggtitle("Random Forest ROC")


## ---- cache=TRUE---------------------------------------------------------
#-----ROFI------#
source_alg_pairs <- "rf"
names(source_alg_pairs) <- "DS_1"
rofi_results <- rofi(single_source_peppuRobj, source_alg_pairs, nn = 5, f_prob=0.4 ,
                     nu=1/100, conv_check = 40,max_iter=50,epsilon = 0.01, after_conv_checks = 5)

## ------------------------------------------------------------------------
#source("../R/plot_rofi.R")
#plot.rofi(rofi_results)
peppuR::plot.rofi(rofi_results)

