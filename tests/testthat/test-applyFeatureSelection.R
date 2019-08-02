context("applyFeatureSelection")
library(peppuR)
library(caret)
library(MASS)
library(dplyr)

#------ Single Source -------#
# Add subject names to the data
birthweight_data <- MASS::birthwt
birthweight_data$ID <- paste("ID",1:nrow(birthweight_data), sep = "_")
birthweight_data$low <- as.factor(birthweight_data$low)

# Make categorical columns factors
birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")] <- lapply(birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")], function(x) as.factor(x))


sample_cname <- "ID"
outcome_cname <- "low"
pair_cname <- NULL

# create as.MLimput
result = as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                    categorical_features = T , sample_cname = sample_cname,
                    outcome_cname = outcome_cname, pair_cname = pair_cname)

ufs_result = univariate_feature_selection(result)

#using applyFeatureSelection function
apply_fs = applyFeatureSelection(result, ufs_result, pval_threshold = .05)

#------ Multi Source -------#
data("multi_source")

x_multi = multi_source$X
y_multi = multi_source$Y

sample_cname = "ID"
outcome_cname = "Group"
pair_cname = "paircol"

multi_results <- suppressMessages(as.MLinput(X=x_multi, Y = y_multi, categorical_features = TRUE,
                                       sample_cname = sample_cname, outcome_cname = outcome_cname,
                                       pair_cname = pair_cname))

multi_ufs_result = univariate_feature_selection(multi_results)

#using applyFeatureSelection function
apply_multi_fs = applyFeatureSelection(multi_results, multi_ufs_result, pval_threshold = rep(.2, length(multi_results$X)))

test_that("output tests for single source applyFeatureSelection", {
  expect_that(apply_fs, is_a("list"))
  expect_that(length(apply_fs), equals(2))
  expect_equal( sum(ufs_result$source1$p_value > 0.05), (sum(attributes(result)$data_info$number_of_features) - (ncol(apply_fs$X$source1)-1)) )#subtract 1 for ID column
  expect_error(applyFeatureSelection(result,ufs_result, pval_threshold = 1.2))
  expect_error(applyFeatureSelection(result, ufs_result, pval_threshold = c(0.2, 0.2)))
  
  })

test_that("output tests for multi source applyFeatureSelection", {
  expect_that(apply_multi_fs, is_a("list"))
  expect_that(length(apply_multi_fs), equals(2))
  expect_equal(sum(unlist(lapply(multi_ufs_result, function(x) x$p_value > 0.2))), sum(attributes(multi_results)$data_info$number_of_features)-sum(unlist(lapply(apply_multi_fs$X, function(x) ncol(x)-1))) )
  expect_error(applyFeatureSelection(multi_results, multi_ufs_result, pval_threshold = 0.2))
  })
