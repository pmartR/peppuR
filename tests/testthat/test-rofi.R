context("rofi")
library(peppuR)
data("multi_source")

x_multi = multi_source$X
y_multi = multi_source$Y

sample_cname = "ID"
outcome_cname = "Group"
pair_cname = "paircol"

data_obj <- suppressMessages(as.MLinput(X=x_multi, Y = y_multi, categorical_features = TRUE,
                                       sample_cname = sample_cname, outcome_cname = outcome_cname,
                                       pair_cname = pair_cname))

#Impute, nzv, etc...
data_obj <- impute_missing(data_obj)
data_obj <- varianceFilter(data_obj)
data_obj <- applyvarianceFilter(data_object = data_obj)
cfilter <- correlationFilter(data_obj)
data_obj <- applyCorrelationFilter(data_object = data_obj, corFilt_object = cfilter, threshold = c(0.9, 0.9, 0.9,0.9,0.9))

#----- ROFI ------#
source_alg_pairs <- c("rf", "rf", "svm", "rf", "rf")
names(source_alg_pairs) <- names(data_obj$X)

test_that("errors if conditions aren't met", {
  expect_error(rofi(data_obj, source_alg_pairs, nn = 3, f_prob=0.10 ,
                    nu=1/100, max_iter=100,
                    conv_check=100, epsilon = 0.01, after_conv_checks = 10))
})

data_obj <- dataPartitioning(data_obj)
rofi_results <- rofi(data_obj, source_alg_pairs, nn = 1, f_prob=0.70 ,
                     nu=1/100, max_iter=110,
                     conv_check=100, epsilon = 0.01, after_conv_checks = 10)

test_that("rofi works", {
  expect_s3_class(rofi_results, "featSelect")
})

