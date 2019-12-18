context("as.MLinput")
#library(peppuR)
library(caret)
library(MASS)

# Add subject names to the data
birthweight_data <- MASS::birthwt
birthweight_data$ID <- paste("ID",1:nrow(birthweight_data), sep = "_")
birthweight_data$low <- as.factor(birthweight_data$low)

# Make categorical columns factors
birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")] <- lapply(birthweight_data[, colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")], function(x) as.factor(x))


sample_cname <- "ID"
outcome_cname <- "low"
pair_cname <- NULL

#arguments to test as input arguments of as.MLinput function
result = as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                    categorical_features = T , sample_cname = sample_cname,
                    outcome_cname = outcome_cname, pair_cname = pair_cname)

test_that("Categorical checks single source as.MLinput", {
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                          categorical_features = FALSE , sample_cname = sample_cname,
                          outcome_cname = outcome_cname, pair_cname = pair_cname))
  expect_equal(attr(result, "categorical_columns")$categorical_cols$source1,
               c("race.2","race.3", "smoke.1", "ht.1", "ui.1"))
  expect_equal(attr(as.MLinput(X = birthweight_data[, !colnames(birthweight_data) %in% c("race", "smoke", "ht", "ui")],
               Y = NULL, meta_colnames = c("low", "ID"),
               categorical_features = FALSE , sample_cname = sample_cname,
               outcome_cname = outcome_cname, pair_cname = pair_cname), "categorical_columns")$categorical_cols, "none")
})



test_that("Missing data checks error for single source as.MLinput", {
  birthweight_data$ID[10] <- NA
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                          categorical_features = TRUE , sample_cname = sample_cname,
                          outcome_cname = outcome_cname, pair_cname = pair_cname), "Missing sample names in X")
  birthweight_data$low[1] <- NA
  expect_error(as.MLinput(X = birthweight_data[-10,], Y = NULL, meta_colnames = c("low", "ID"),
                          categorical_features = TRUE , sample_cname = sample_cname,
                          outcome_cname = outcome_cname, pair_cname = pair_cname), "Missing outcomes in Y")
  y_df <- birthweight_data[,c("low","ID")]
  y_df$ID[80] <- NA
  expect_error(as.MLinput(X = birthweight_data[80:90,-(which(colnames(birthweight_data) == "low"))],
             Y = y_df[80:90,], meta_colnames = NULL,
             categorical_features = TRUE , sample_cname = sample_cname,
             outcome_cname = outcome_cname, pair_cname = pair_cname), "Missing sample names in Y")
  
  
})

test_that("Output for as.MLinput is properly formatted", {
  expect_that(result, is_a("list"))
  expect_that(length(result), equals(2))
  expect_that(names(result), equals(c("X", "Y")))
  expect_that(nrow(result$Y), equals(nrow(result$X$source1)))
  expect_that(result$Y[,sample_cname], equals(rownames(result$X$source1)))
  expect_that(ncol(result$Y), equals(length(unique(c(sample_cname, c("ID", "low"))))))
  expect_equal(result, as.MLinput(X = birthweight_data[,-(which(colnames(birthweight_data) == "low"))],
                                  Y = birthweight_data[,c("low","ID")], meta_colnames = NULL,
                                  categorical_features = TRUE , sample_cname = sample_cname,
                                  outcome_cname = outcome_cname, pair_cname = pair_cname))
})

test_that("Invalid input data throw errors",{     
  expect_error(as.MLinput(X = c(1,2,3), Y = birthweight_data[,c("ID", "low")],
                          categorical_features = T, sample_cname,
                          outcome_cname, pair_cname), "X must be of class 'data.frame' or 'list'")
  expect_error(as.MLinput(X = birthweight_data[,-(which(colnames(birthweight_data) == "low"))], Y = birthweight_data[-1,c("ID", "low")],
                          categorical_features = T, sample_cname,
                          outcome_cname, pair_cname), "X and Y do not have the same number of rows")
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = NULL,
                          categorical_features = T, sample_cname,
                          outcome_cname, pair_cname), "Both Y and meta_colnames are NULL. Please specify either Y or meta_colnames")
  #incorrect input for arguments
  #expect_that(as.MLinput(x = x_multi, y = y_multi, categorical_features = T, sample_cname, outcome_cname, pair_cname), shows_message()) 
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"), categorical_features = T, sample_cname = "made_up")) 
  expect_error(as.MLinput(X = birthweight_data, Y = "string", categorical_features = T, sample_cname, outcome_cname))
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                          sample_cname = sample_cname, outcome_cname = "when"))
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                          sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = "nonesense"))
  
})

test_that("Pairs are identified when approriate",{
  expect_error(as.MLinput(X = birthweight_data, Y = NULL, meta_colnames = c("low", "ID"),
                          sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = "ID"),
               "there needs to be at least two observations for each unique ID")
  birthweight_data$Pair <- rep(c(1,2,3), nrow(birthweight_data)/3)
  paired_result <- as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                              meta_colnames = c("low", "ID", "Pair"),
                              sample_cname = sample_cname, outcome_cname = outcome_cname,
                              pair_cname = "Pair")
  expect_equal(attr(paired_result, "data_info")$paired, TRUE)
  # check single pairs
  birthweight_data$Pair[1] <- 1000
  expect_error(as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                          meta_colnames = c("low", "ID", "Pair"),
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = "Pair"),"there needs to be at least two observations for each unique Pair")
  birthweight_data$Pair <- 1000
  expect_error(as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                          meta_colnames = c("low", "ID", "Pair"),
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = "Pair"),"Pair column should contain at least 2 unique items")
  birthweight_data$Pair[1] <- NA
  expect_error(as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                          meta_colnames = c("low", "ID", "Pair"),
                          sample_cname = sample_cname, outcome_cname = outcome_cname,
                          pair_cname = "Pair"),"Missing pair information")
  
  
  
  })

test_that("Missing rows are removed", {
  x_single_narows <- birthweight_data
  x_single_narows[c(4,6,8,10), 2:(ncol(x_single_narows)-1)] <- NA
  result_less_one <- as.MLinput(X = x_single_narows, Y = NULL, meta_colnames = c("low", "ID"), categorical_features = T, sample_cname, outcome_cname)
  expect_that(nrow(result_less_one$X$source1), equals(nrow(result$X$source1)-4))
  birthweight_data$Pair <- rep(c(1,2,3), nrow(birthweight_data)/3)
  birthweight_data[1,2:(ncol(x_single_narows)-1)] <- NA
  paired_result <- as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                              meta_colnames = c("low", "ID", "Pair"),
                              sample_cname = sample_cname, outcome_cname = outcome_cname,
                              pair_cname = "Pair")
  expect_equal(nrow(paired_result$X$source1), nrow(result$X$source1)-1)
  birthweight_data$Pair[1:2] <- 50
  paired_result2 <- as.MLinput(X=birthweight_data, Y = NULL, categorical_features = TRUE,
                              meta_colnames = c("low", "ID", "Pair"),
                              sample_cname = sample_cname, outcome_cname = outcome_cname,
                              pair_cname = "Pair")
  expect_equal(nrow(paired_result2$X$source1), nrow(result$X$source1)-2)
  
})

# test_that("more input arguments throwing errors",{     
#   #lets change a few rows of x_single to be all NA values
#   x_single_na = x_single
#   x_single_na[c(3,5,7), -1] = NA
#   rez = as.MLinput(x = x_single_na, y = y_single, categorical_features = T, sample_cname, outcome_cname, pair_cname)
#   expect_that(nrow(x_single_na) > nrow(rez$x), is_true()) 
#   
#   #lets remove a row from x_single
#   x_single_new = x_single
#   x_single_new = x_single_new[-5,]
#   expect_that(as.MLinput(x = x_single_new, y = y_single, categorical_features = T, sample_cname, outcome_cname, pair_cname), throws_error()) 
#   
#   #lets make paircol in y_single be the same value
#   y_single_new = y_single
#   y_single_new$paircol = 1
#   
#   #remove pair_col from y_single
#   y_single_new2 = y_single[, -3]
#   
#   expect_that(as.MLinput(x = x_single, y = y_single_new, categorical_features = T, sample_cname, outcome_cname, pair_cname), throws_error())
#   expect_that(as.MLinput(x = x_single_new, y = y_single, categorical_features = T, sample_cname, outcome_cname, pair_cname = NULL), throws_error())
#   
#   #lets rename a column of x_single to be outcome_cname
#   x_single_rename = x_single
#   names(x_single_rename)[4] = "Group"
#   
#   expect_that(as.MLinput(x = x_single_rename, y = y_single, categorical_features = F, sample_cname, outcome_cname, pair_cname = NULL), throws_error())
#   
#   #lets change outcome_cname in y_single to be a numeric vector
#   y_single_rename = y_single
#   y_single_rename$Group = as.numeric(y_single_rename$Group)
#   expect_that(as.MLinput(x = x_single, y = y_single_rename, categorical_features = F, sample_cname, outcome_cname, pair_cname = NULL), throws_error())
#   
#   
# })

