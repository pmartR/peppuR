context("as.MLinput Multisource")
#library(peppuR)
data("multi_source")

x_multi = multi_source$X
y_multi = multi_source$Y

sample_cname = "ID"
outcome_cname = "Group"
pair_cname = "paircol"

results <- suppressMessages(as.MLinput(X=x_multi, Y = y_multi, categorical_features = TRUE,
                      sample_cname = sample_cname, outcome_cname = outcome_cname,
                      pair_cname = pair_cname))

smashed_X <- x_multi
smashed_X$MRM <- dplyr::left_join(y_multi, smashed_X$MRM, by = "ID")
smashed_results <- suppressMessages(as.MLinput(X=smashed_X, Y=NULL, meta_colnames = c("ID", "Group", "paircol"),
                                               sample_cname = sample_cname, outcome_cname = outcome_cname,
                                               pair_cname = pair_cname, categorical_features = TRUE))

#arguments to test as input arguments of as.MLinput function
vec <- c(1,2,3)

test_that("Categorical checks mutli source as.MLinput", {
  expect_error(as.MLinput(X=x_multi, Y = y_multi, categorical_features = FALSE,
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = pair_cname))
  expect_error(as.MLinput(X=unname(x_multi[1:3]), Y = y_multi, categorical_features = TRUE,
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = pair_cname))
  expect_equal(attr(results, "categorical_columns")$categorical_cols$Demographics,
               "Sex.Male")
 })

test_that("Y columns are extracted", {
  expect_equal(smashed_results, results)
  })

test_that("Columns in sources", {
  x_multi$Cytokines <-  x_multi$Cytokines[,-which(colnames(x_multi$Cytokines) == sample_cname)]
  expect_error(as.MLinput(X=x_multi, Y = y_multi, categorical_features = FALSE,
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = pair_cname))
  expect_error(as.MLinput(X=x_multi, Y = NULL, meta_colnames = c("ID", "Group", "paircol"), categorical_features = FALSE,
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = pair_cname))
})