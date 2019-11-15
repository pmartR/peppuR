context("MLWrapperSingleSource")
#library(peppuR)
library(MASS)

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

single_source_peppuRobj <- dataPartitioning(single_source_peppuRobj, folds = 5, repeats = 10)

rf_results <- MLwrapper(data_object = single_source_peppuRobj, methods = "rf")
#other_results <- MLSelection(single_source_peppuRobj)
#plot_results <- plot(other_results)
test_that("Random Forest completed", {
  expect_true(grepl("rf", names(attr(rf_results, "ML_results"))))
  #expect_equal(class(other_results), c("mlSelect", "list"))
 # expect_length(plot_results, 2)
})