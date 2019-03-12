#'Imputes missing values
#'
#'This function imputes missing values in a single data frame or a list of data frames
#'
#' @param data_object argument is the output produced by as.MLinput, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' @param method argument specifies which imputation package to use, missForest, mice, amelia
#' @param threshold argument is a percentage, if a column in x data frame has less than threshold percent of missing values then data will be imputed. But if a column has more missing values than the percent threshold, these columns will be dropped from x data frame
#'
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
#'result = as.MLinput(x = x_single, y = y_single, categorical_features = T , sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'result2 = as.MLinput(x = x_multi, y = y_multi, categorical_features = T, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'
#'imputed_res = impute_missing(result, method = 'randomforest')
#'imputed_res2 = impute_missing(result2, method = 'randomforest')
#'
#'}
#'
#' @export
#' 

impute_missing = function(data_object, method = "randomforest", threshold = 0.1) {
    # extract cnames
    sample_cname = attr(data_object, "cnames")$sample_cname
    
    # check that data inherits from the class list
    if (!inherits(data_object, "list")) 
        stop("data_object argument must be of class list")
    
    # check that method is one of the missing data imputation packages
    if (!(method %in% c("randomforest", "mice"))) 
        stop("method must be one of, randomforest or mice")
    
    # check that threshold is a percentage between zero and one
    if (threshold > 1 | threshold < 0) 
        stop("threshold must be a percentage between 0 and 1")
    
    # load required imputation packages
    suppressMessages(suppressPackageStartupMessages({
        library(missForest)
        library(mice)
    }))
    
    # first lets check if data has single x data frame or a list of x data frames
    if (attr(data_object, "n_sources") == 1) {
        # lets extract missing_data data frame from data_object and single x data frame
        na_df = attr(data_object, "missing_data")
        x_single = data_object$X
        
        # which columns of na_df have more missing values then the threshold, we will drop these columns
        inds = which(na_df > threshold)
        
        # dropping those columns from x data frame
        if (length(inds) == 0) {
            x_new = x_single
            x_new[, which(names(x_new) == sample_cname)] = as.factor(x_new[, which(names(x_new) == sample_cname)])
        } else {
            x_new = x_single[, -inds]
            x_new[, which(names(x_new) == sample_cname)] = as.factor(x_new[, which(names(x_new) == sample_cname)])
        }
        
        # apply specified imputation method
        if (method == "randomforest") {
            sample_col = x_new[, which(names(x_new) == sample_cname)]
            x_imp = missForest(x_new[, -which(names(x_new) == sample_cname)])
            result = x_imp$ximp
            result = cbind(sample_col, result)
            names(result)[1] = sample_cname
            
            # replace data_object$X with result
            data_object$X = result
            
            # set post imputation attributes of original data_object
            num_features = length(which(colSums(is.na(x_new)) > 0))
            filtered_features = setdiff(names(x_single), names(x_new))
            attr(data_object, "imputation_info") = list(method = method, threshold = threshold, num_features = num_features, filtered_features = filtered_features)
        } else if (method == "mice") {
            sample_col = x_new[, which(names(x_new) == sample_cname)]
            x_imp = mice(x_new[, -which(names(x_new) == sample_cname)])
            x_imp = complete(x_imp, 1)
            result = cbind(sample_col, x_imp)
            names(result)[1] = sample_cname
            
            # replace data_object$X with result
            data_object$X = result
            
            # set post imputation attributes of original data_object object
            num_features = length(which(colSums(is.na(x_new)) > 0))
            filtered_features = setdiff(names(x_single), names(x_new))
            attr(data_object, "imputation_info") = list(method = method, threshold = threshold, num_features = num_features, filtered_features = filtered_features)
        }
        
    } else if (attr(data_object, "n_sources") > 1) {
        # first lets pull missing_data attr from data_object
        na_df_list = attr(data_object, "missing_data")
        x_list = data_object$X
        
        # look at which features have less missing values then threshold and keep those features, for every data frame in na_df_list
        x_new_list = mapply(function(na_df, x_df) {
            inds = which(na_df > threshold)
            if (length(inds) == 0) {
                x_new = x_df
                na_df_new = na_df
            } else {
                x_new = x_df[, -inds]
                na_df_new = as.data.frame(na_df[-inds, ])
                names(na_df_new) = "NA percent per column"
            }
            attr(x_new, "na_new") = na_df_new
            
            return(x_new)
        }, na_df_list, x_list)
        
        # apply specified imputation method to each data frame
        if (method == "randomforest") {
            x_imp_list = lapply(x_new_list, function(x) {
                inds = which(attr(x, "na_new") > 0)
                if (length(inds) == 0) {
                  result = x
                } else {
                  sample_col = x[, which(names(x) == sample_cname)]
                  result = missForest(x[, -which(names(x) == sample_cname)])$ximp
                  result = cbind(sample_col, result)
                  names(result)[1] = sample_cname
                  attr(result, "na_new") = attr(x, "na_new")
                }
                
                return(result)
            })
            # replace data_object$X with x_imp_list
            data_object$X = x_imp_list
            
            # set post imputation attributes of original data object
            num_features = lapply(x_new_list, function(x) {
                length(which(colSums(is.na(x)) > 0))
            })
            filtered_features = mapply(function(x_df, x_filtered) {
                setdiff(names(x_df), names(x_filtered))
            }, x_list, x_new_list)
            attr(data_object, "imputation_info") = list(method = method, threshold = threshold, num_features = num_features, filtered_features = filtered_features)
            
        } else if (method == "mice") {
            x_imp_list = lapply(x_new_list, function(x) {
                inds = which(attr(x, "na_new") > 0)
                if (length(inds) == 0) {
                  result = x
                } else {
                  sample_col = x[, which(names(x) == sample_cname)]
                  result = mice(x[, -which(names(x) == sample_cname)])
                  result = complete(result, 1)
                  result = cbind(sample_col, result)
                  names(result)[1] = sample_cname
                  attr(result, "na_new") = attr(x, "na_new")
                }
                
                return(result)
            })
            # replace data_object$X with x_imp_list
            data_object$X = x_imp_list
            
            # set post imputation attributes of returned result
            num_features = lapply(x_new_list, function(x) {
                length(which(colSums(is.na(x)) > 0))
            })
            filtered_features = mapply(function(x_df, x_filtered) {
                setdiff(names(x_df), names(x_filtered))
            }, x_list, x_new_list)
            attr(data_object, "imputation_info") = list(method = method, threshold = threshold, num_features = num_features, filtered_features = filtered_features)
            
        }
        
    }
    
    return(data_object)
}

