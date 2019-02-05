context("as.MLinput multisource")
library(peppuR)
library(caret)

data("multi_source")

x_multi = multi_source$X
y_multi = multi_source$Y

sample_cname = "ID"
outcome_cname = "Group"
pair_cname = "paircol"

results <- as.MLinput(X=x_multi, Y = y_multi, categorical_features = TRUE,
                      sample_cname = sample_cname, outcome_cname = outcome_cname,
                      pair_cname = pair_cname)

#arguments to test as input arguments of as.MLinput function
vec <- c(1,2,3)

test_that("Categorical checks mutli source as.MLinput", {
  expect_error(as.MLinput(X=x_multi, Y = y_multi, categorical_features = FALSE,
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = pair_cname))
  expect_equal(attr(results, "categorical_columns")$categorical_cols$Demographics,
               "Sex")
 })