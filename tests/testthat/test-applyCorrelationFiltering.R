context("applyCorrelationFiltering")
library(peppuR)

#------ Single Source -------#
# Add subject names to the data
birthweight_data <- MASS::birthwt
birthweight_data$ID <- paste("ID",1:nrow(birthweight_data), sep = "_")
birthweight_data$low <- as.factor(birthweight_data$low)

# Make categorical columns factors
birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")] <- lapply(birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")], function(x) as.factor(x))
birthweight_data$correlated_age <- birthweight_data$age + 1

sample_cname <- "ID"
outcome_cname <- "low"
pair_cname <- NULL

# create as.MLimput
result = as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                    categorical_features = T , sample_cname = sample_cname,
                    outcome_cname = outcome_cname, pair_cname = pair_cname)

corObj <- correlationFilter(result)
new_result <- applyCorrelationFilter(data_object = result, corFilt_object = corObj,threshold = 0.4)

#------ Multi Source -------#
data("multi_source")

x_multi = multi_source$X
y_multi = multi_source$Y

sample_cname = "ID"
outcome_cname = "Group"
pair_cname = "paircol"

multi_result <- suppressMessages(as.MLinput(X=x_multi, Y = y_multi, categorical_features = TRUE,
                                             sample_cname = sample_cname, outcome_cname = outcome_cname,
                                             pair_cname = pair_cname))

multiCorObj <- correlationFilter(multi_result)
new_result <- applyCorrelationFilter(data_object = multi_result, corFilt_object = multiCorObj,threshold = rep(0.7, length(multi_result$X)))
test_that("correlated feature is removed", {
  expect_equal(attributes(new_result)$correlation_features_rm, "correlated_age")
})

