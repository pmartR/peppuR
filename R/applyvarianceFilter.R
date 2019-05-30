#' Removes features that have near zero variance
#'
#' This function removes features that are above a percentage threshold of features with near zero variance  
#'
#' @param data_object argument is the output produced by as.ML function, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' @param threshold is the cutoff value for percentage of partitions containing features with near zero variance
#' @details 
#' 
#' @examples 
#'dontrun{
#'}
#'
#'
#' @export

applyvarianceFilter = function(data_object, threshold = 0.1) {
    # check that data_object inherits from 'as.MLinput'
    
    # check that 'varianceFilt_info' attribute is not NULL in data_object
    if (is.null(attr(data_object, "varianceFilt_info"))) 
        stop("'varianceFilter' function has not been applied to data_object")
    
    
    # first we'll extract attributes from 'data_object'
    x_data = data_object$X
    n_sources = attr(data_object, "n_sources")
    partition_info = attr(data_object, "partition_info")
    varFilt_info = attr(data_object, "varianceFilt_info")
    
    # case where n_sources is 1
    if (n_sources == 1) {
        # case where dataPartitioning has not been applied
        if (is.null(attr(data_object, "partition_info"))) {
            
            # removing features with near zero variance from x_data
            rm_inds = which(names(x_data) %in% varFilt_info)
            
            if (length(rm_inds) > 0) {
                new_xdata = x_data[, -rm_inds]
                attr(new_xdata, "near_zero_var_removed") = varFilt_info
                data_object$X = new_xdata
            }
            
        } else {
            # case where dataPartitioning has been applied
            
            # check that there are indeed features with near zero variance
            if (inherits(varFilt_info, "table")) {
                extra_partitions = attr(data_object, "extra_partitions")
                folds = attr(data_object, "foldrep")$folds
                reps = attr(data_object, "foldrep")$repeats
                
                # apply replace_partitions
                rp_result = replace_partitions(x_data = x_data, varFilt_info = varFilt_info, extra_partitions = extra_partitions, 
                  folds = folds, reps = reps, thresh = threshold)
                
                # update data_object
                if (!is.null(rp_result$partitions_to_replace)) {
                  data_part_train = partition_info$train
                  data_part_test = partition_info$test
                  data_part_extra_train = extra_partitions$train
                  data_part_extra_test = extra_partitions$test
                  partitions_to_replace = rp_result$partitions_to_replace
                  
                  data_part_train_new = replace(data_part_train, partitions_to_replace, data_part_extra_train[1:length(partitions_to_replace)])
                  data_part_test_new = replace(data_part_test, partitions_to_replace, data_part_extra_test[1:length(partitions_to_replace)])
                  rez = list(train = data_part_train_new, test = data_part_test_new)
                  
                  attr(data_object, "partition_info") = rez
                }
                
                if (!is.null(rp_result$feat_to_remove)) {
                  data_object$X = x_data[, -which(names(x_data) %in% rp_result$feat_to_remove)]
                }
                
                # reset varFilt_info attr
                attr(data_object, "varianceFilt_info") = NULL
                
                # apply varianceFilter once again
                data_object = varianceFilter(data_object)
                varFilt_info = attr(data_object, "varianceFilt_info")
                
                # check if there are any near zero var features and repeat process
                if (inherits(varFilt_info, "table")) {
                  x_data = data_object$X
                  varFilt_info = attr(data_object, "varianceFilt_info")
                  partition_info = attr(data_object, "partition_info")
                  extra_partitions = attr(data_object, "extra_partitions")
                  
                  rp_result2 = replace_partitions(x_data = x_data, varFilt_info = varFilt_info, extra_partitions = extra_partitions, 
                    folds = folds, reps = reps, thresh = threshold)
                  
                  # update data_object
                  if (!is.null(rp_result2$partitions_to_replace)) {
                    data_part_train = partition_info$train
                    data_part_test = partition_info$test
                    data_part_extra_train = extra_partitions$train
                    data_part_extra_test = extra_partitions$test
                    partitions_to_replace = rp_result2$partitions_to_replace
                    
                    data_part_train_new = replace(data_part_train, partitions_to_replace, data_part_extra_train[1:length(partitions_to_replace)])
                    data_part_test_new = replace(data_part_test, partitions_to_replace, data_part_extra_test[1:length(partitions_to_replace)])
                    rez = list(train = data_part_train_new, test = data_part_test_new)
                    
                    attr(data_object, "partition_info") = rez
                  }
                  
                  if (!is.null(rp_result2$feat_to_remove)) {
                    data_object$X = x_data[, -which(names(x_data) %in% rp_result2$feat_to_remove)]
                  }
                  
                  # reset varFilt_info attr
                  attr(data_object, "varianceFilt_info") = NULL
                  
                  # apply varianceFilter once again
                  data_object = varianceFilter(data_object)
                  varFilt_info = attr(data_object, "varianceFilt_info")
                  
                  if (inherits(varFilt_info, "table")) {
                    stop("threshold is too high, cannot generate sef of partitions where near zero variance criteria is met")
                  }
                }
            }
        }
    } else if (n_sources > 1) {
        # case where dataPartitioning has not been applied
        if (is.null(partition_info)) {
            # remove near zero variance features from each data frame
            new_xdata = mapply(function(x_df, vf) {
                if (length(vf) > 0) {
                  inds_rm = which(names(x_df) %in% vf)
                  x_df = x_df[, -inds_rm]
                } else {
                  x_df
                }
                attr(x_df, "near_zero_var_removed") = vf
                return(x_df)
                
            }, x_data, varFilt_info)
            
            # update x_data in data_object
            data_object$X = new_xdata
            
        } else {
            # case where dataPartitioning has been applied
            
            # check that there are indeed x_data frames with near zero var features
            if (any(varFilt_info != "none")) {
                extra_partitions = attr(data_object, "extra_partitions")
                folds = attr(data_object, "foldrep")$folds
                reps = attr(data_object, "foldrep")$repeats
                
                # apply replace_partitions
                rp_result_list = mapply(replace_partitions, x_data, varFilt_info, MoreArgs = list(extra_partitions, folds, reps, threshold))
                
                # lets update x_data with rp_result_list
                new_xdata = mapply(function(x_df, rp_res) {
                  if (is.null(rp_res)) {
                    result = x_df
                  } else {
                    if (!is.null(rp_res$feat_to_remove)) {
                      result = x_df[, -which(names(x_df) %in% rp_res$feat_to_remove)]
                      attr(result, "feats_below_rm") = rp_res$feat_to_remove
                    } else {
                      result = x_df
                    }
                  }
                  return(result)
                }, x_data, rp_result_list)
                
                # now update x_data in data_object
                data_object$X = new_xdata
                
                # now lets gather the partitions to remove
                all_partitions_to_replace = unique(unlist(lapply(rp_result_list, function(item) {
                  return(item$partitions_to_replace)
                })))
                
                if (!is.null(all_partitions_to_replace)) {
                  data_part_train = partition_info$train
                  data_part_test = partition_info$test
                  data_part_extra_train = extra_partitions$train
                  data_part_extra_test = extra_partitions$test
                  partitions_to_replace = all_partitions_to_replace
                  
                  data_part_train_new = replace(data_part_train, partitions_to_replace, data_part_extra_train[1:length(partitions_to_replace)])
                  data_part_test_new = replace(data_part_test, partitions_to_replace, data_part_extra_test[1:length(partitions_to_replace)])
                  rez = list(train = data_part_train_new, test = data_part_test_new)
                  
                  attr(data_object, "partition_info") = rez
                }
                
                # reset varFilt_info attr
                attr(data_object, "varianceFilt_info") = NULL
                
                # apply varianceFilter once again
                data_object = varianceFilter(data_object)
                varFilt_info = attr(data_object, "varianceFilt_info")
                
                # check if there are any near zero var features and repeat process
                if (any(varFilt_info != "none")) {
                  x_data = data_object$X
                  varFilt_info = attr(data_object, "varianceFilt_info")
                  partition_info = attr(data_object, "partition_info")
                  extra_partitions = attr(data_object, "extra_partitions")
                  
                  # apply replace_partitions
                  rp_result_list2 = mapply(replace_partitions, x_data, varFilt_info, MoreArgs = list(extra_partitions, folds, reps, 
                    threshold))
                  
                  # lets update x_data with rp_result_list2
                  new_xdata2 = mapply(function(x_df, rp_res) {
                    if (is.null(rp_res)) {
                      result = x_df
                      
                    } else {
                      if (!is.null(rp_res$feat_to_remove)) {
                        result = x_df[, -which(names(x_df) %in% rp_res$feat_to_remove)]
                        attr(result, "feats_below_rm") = rp_res$feat_to_remove
                      } else {
                        result = x_df
                      }
                      
                    }
                    return(result)
                  }, x_data, rp_result_list2)
                  
                  # now update x_data in data_object
                  data_object$X = new_xdata2
                  
                  # now lets gather the partitions to remove
                  all_partitions_to_replace = unique(unlist(lapply(rp_result_list2, function(item) {
                    return(item$partitions_to_replace)
                  })))
                  
                  if (!is.null(all_partitions_to_replace)) {
                    data_part_train = partition_info$train
                    data_part_test = partition_info$test
                    data_part_extra_train = extra_partitions$train
                    data_part_extra_test = extra_partitions$test
                    partitions_to_replace = all_partitions_to_replace
                    
                    data_part_train_new = replace(data_part_train, partitions_to_replace, data_part_extra_train[1:length(partitions_to_replace)])
                    data_part_test_new = replace(data_part_test, partitions_to_replace, data_part_extra_test[1:length(partitions_to_replace)])
                    rez = list(train = data_part_train_new, test = data_part_test_new)
                    
                    attr(data_object, "partition_info") = rez
                  }
                  
                  # reset varFilt_info attr
                  attr(data_object, "varianceFilt_info") = NULL
                  
                  # apply varianceFilter once again
                  data_object = varianceFilter(data_object)
                  varFilt_info = attr(data_object, "varianceFilt_info")
                  
                  if (any(varFilt_info != "none")) {
                    stop("threshold is too high, cannot generate sef of partitions where near zero variance criteria is met")
                  }
                }
            }
        }
    }
    
    # create applyvarianceFilter attr and set to TRUE
    attr(data_object, "applyvarianceFilter") = TRUE
    return(data_object)
}


# helper function that replaces near zero var features below 'threshold' argument

replace_partitions = function(x_data, varFilt_info, extra_partitions, folds, reps, thresh) {
    
    if (!inherits(varFilt_info, "table")) {
        final_result = NULL
    } else {
        col_sums = colSums(varFilt_info)
        perc = sapply(col_sums, function(x) {
            x/(folds * reps)
        })
        
        feat_above = names(perc)[which(perc > thresh)]
        feat_below = names(perc)[which(perc < thresh)]
        
        # remove near zero var features above threshold
        if (length(feat_above) > 0) {
            feat_to_remove = names(x_data)[which(names(x_data) %in% feat_above)]
        } else {
            feat_to_remove = NULL
        }
        
        if (length(feat_below) > 0) {
            # extra partition info
            data_part_extra_train = extra_partitions$train
            
            # find all the partitions that contain the feature below the threshold
            if (length(feat_below) == 1) {
                partitions_to_replace = rownames(varFilt_info)[which(varFilt_info[, which(colnames(varFilt_info) %in% feat_below)] > 
                  0)]
            } else {
                partitions_to_replace = rownames(varFilt_info)[which(rowSums(varFilt_info[, which(colnames(varFilt_info) %in% feat_below)]) > 
                  0)]
            }
            
            if (length(partitions_to_replace) > length(data_part_extra_train)) 
                stop("threshold is too high, there are more partitions to replace than replacements")
        } else {
            partitions_to_replace = NULL
        }
        
        final_result = list(partitions_to_replace = partitions_to_replace, feat_to_remove = feat_to_remove)
    }
    
    return(final_result)
}
