#' Removes features that are above pvalue threshold
#'
#' This function removes features that are above pvalue threshold for a single data frame or a list of data frames
#'
#' @param data_object argument is the output produced by as.ML function, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' @param ufs_result is a single data frame or a list of data frames of feature names and corresponding p values
#' @param pval_threshold is the cutoff value for pvalues, can be a single value or a vector of distinct pvalues
#' @details 
#' 
#' @examples 
#'dontrun{
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
#'result = as.MLinput(X = x_single, Y = y_single, categorical_features = T , sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'result2 = as.MLinput(X = x_multi, Y = y_multi, categorical_features = T, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
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
#'}
#' @export

applyFeatureSelection = function(data_object, ufs_result, pval_threshold) {
    
    # extract x and y data frames from data_object, as well as cnames
    x = data_object$X
    n_sources = attr(data_object, "n_sources")
    
    # cases where n_sources == 1 and n_sources > 1
    
    if (n_sources == 1) {
        if (length(pval_threshold) != n_sources) {
            stop("the number of pval_threshold values must match the number of sources")
        }
        if (pval_threshold > 1 | pval_threshold < 0) {
            stop("pval_threshold must be between 0 and 1")
        }
        
        new_x = applyFS_helper(x_mat = x, ufs_result = ufs_result, pval_thresh = pval_threshold)
        newx_att = attr(new_x, "features_removed")
        
        data_object$X = new_x
        attr(data_object, "ufs_features_rm") = newx_att
    } else if (n_sources > 1) {
        if (length(pval_threshold) != n_sources) {
            stop("the number of pval_threshold values must match the number of sources")
        }
        if (any(pval_threshold > 1) | any(pval_threshold < 0)) {
            stop("pval_threshold must be between 0 and 1")
        }
        
        new_x_list = mapply(applyFS_helper, x, ufs_result, pval_threshold, USE.NAMES = T)
        newx_list_att = lapply(new_x_list, function(item) {
            attr(item, "features_removed")
        })
        
        data_object$X = new_x_list
        attr(data_object, "ufs_features_removed") = newx_list_att
    }
    return(data_object)
}

applyFS_helper = function(x_mat, ufs_result_df, pval_thresh) {
    features_rm = ufs_result_df$feature_name[which(ufs_result_df$p_value > pval_thresh)]
    
    # remove these features from x data frame
    xfeat_rm = which(names(x_mat) %in% features_rm)
    
    if (length(xfeat_rm) == 0) {
        x_mat = x_mat
    } else {
        x_mat = x_mat[, -xfeat_rm]
    }
    
    attr(x_mat, "features_removed") = features_rm
    
    return(x_mat)
}
