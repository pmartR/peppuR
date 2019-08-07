context("MLSelectionSingleSource")
library(peppuR)

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
data_obj = as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                    categorical_features = T , sample_cname = sample_cname,
                    outcome_cname = outcome_cname, pair_cname = pair_cname)


#------- Machine Learning --------#
testa <- MLSelection(data_object = data_obj) #each data source returns a ROC obj

test_that("selection works", {
  expect_s3_class(testa, "mlSelect")
})

p <- plot(testa, roc_curves = TRUE, time_chart = TRUE)

test_that("plotting works", {
  expect_true(is(p[[1]], "gtable"))
  expect_true(is(p[[2]], "ggplot"))
})
