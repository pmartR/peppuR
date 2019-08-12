#' Paired feature correction
#'
#' apply correction to paired features (need a better explanation here)
#'
#' @param data_object argument is the output produced by as.MLinput function, which contains a single X data frame or a list of X data frames, a y data frames and attributes
#' @details
#' 
#' @examples 
#' #'dontrun{
#'library(peppuR)
#'library(missForest)
#'library(mice)
#'
#'data('single_source')
#'data('multi_source')
#'
#'x_multi = multi_source$X
#'y_multi = multi_source$Y
#'
#'x_single = single_source$X
#'y_single = single_source$Y
#'
#'sample_cname = 'ID'
#'outcome_cname = 'Group'
#'pair_cname = 'paircol'
#'
#'result = as.MLinput(X = x_single, y = y_single, categorical_features = T , sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'result2 = as.MLinput(X = x_multi, y = y_multi, categorical_features = T, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'
#'imputed_res = impute_missing(result, method = 'randomforest')
#'imputed_res2 = impute_missing(result2, method = 'randomforest')
#'
#'ufs_result = univariate_feature_selection(imputed_res)
#'ufs_result2 = univariate_feature_selection(imputed_res2)
#'
#'apply_fs = applyFeatureSelection(imputed_res, ufs_result, pval_threshold = .05)
#'apply_fs2 = applyFeatureSelection(imputed_res2, ufs_result2, pval_threshold = c(.5,.1,.2,.3,.5))
#'
#'final_result = correlationFiltering(data_object = apply_fs)
#'final_result2 = correlationFiltering(data_object = apply_fs2)
#'
#'pc_result = pairCorrection(final_result)
#'pc_result2 = pairCorrection(final_result2)
#'
#'}
#' 
#' @export

pairCorrection = function(data_object) {
    
    # extract X and y data frames from data_object, as well as cnames
    X = data_object$X
    y = data_object$Y
    sample_cname = attr(data_object, "cnames")$sample_cname
    pair_cname = attr(data_object, "cnames")$pair_cname
    n_sources = attr(data_object, "n_sources")
    categorical_cols = attr(data_object, "categorical_columns")
    
    # cases where n_sources == 1 and n_sources > 1
    
    # if (n_sources == 1) {
    #     new_x = projectPairs(Xdata = X, Ydata = y, sample_cname = sample_cname, pair_cname = pair_cname)
    #     data_object$X = new_x
    # } else if (n_sources > 1) {
        new_x_list = lapply(X, projectPairs, Ydata = y, sample_cname = sample_cname, pair_cname = pair_cname)
        data_object$X = new_x_list
   # }
    
    attr(data_object, "pairCorrection") = TRUE
    
    return(data_object)
}

# Function to create an X matrix based on a covariate data frame
build_x_mat <- function(cov_df) {
    
    # If all covariates are numeric, simply return the same matrix back
    if (is.numeric(cov_df)) {
        return(data.matrix(cov_df))
    }
    
    # If the covariates are a mix of numeric, factors and characters, return matrix of group identifiers
    Xmatrix <- NULL
    for (i in 1:ncol(cov_df)) {
        
        if (is.numeric(cov_df[, i])) {
            # If column i is numeric, append it to X
            Xmatrix <- cbind(Xmatrix, cov_df[, i])
        } else {
            coli_levels <- unique(cov_df[, i])
            n_levels <- length(coli_levels)
            if (n_levels != length(cov_df[, i])) {
                Xcoli <- matrix(0, nrow(cov_df), n_levels)
                
                for (j in 1:n_levels) {
                  Xcoli[cov_df[, i] == coli_levels[j], j] <- 1
                }
                Xmatrix <- cbind(Xmatrix, Xcoli)
            }
        }
    }
    return(Xmatrix)
}


projectPairs <- function(Xdata, Ydata, sample_cname, pair_cname) {
    x_data = Xdata[, -which(names(Xdata) %in% sample_cname)]
    y_data = Ydata[, -which(names(Ydata) %in% sample_cname)]
    
    pairX <- build_x_mat(data.frame(pair = as.factor(y_data[[pair_cname]])))
    pair_proj <- pairX %*% MASS::ginv(t(pairX) %*% pairX) %*% t(pairX)
    pair_proj <- diag(nrow(pair_proj)) - pair_proj
    x_data <- pair_proj %*% data.matrix(x_data)
    
    x_data = as.data.frame(x_data)
    
    result = cbind(Xdata[[sample_cname]], x_data)
    names(result)[1] = sample_cname
    
    return(result)
}
