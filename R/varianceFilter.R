#' Applies near Zero Variance function to data or data partitions
#' 
#' This function Applies near Zero Variance function to data or data partitions
#' 
#' @param data_object argument is the output produced by as.MLinput, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' 
#' @details 
#' 
#' @examples 
#' @export
varianceFilter = function(data_object, ...) {
    .varianceFilter(data_object, ...)
}

.varianceFilter = function(data_object) {
    # first we'll extract attributes from 'data_object'
    partitions_list = attr(data_object, "partition_info")
    
    x_data = data_object$X
    n_sources = attr(data_object, "n_sources")
    sample_cname = attr(data_object, "cname")$sample_cname
    
    # case where 'partitions_list' is NULL
    if (is.null(partitions_list)) {
        # check the case where x_data is a single data frame
        if (n_sources == 1) {
            x_df = x_data[, -which(names(x_data) %in% sample_cname)]
            cols = caret::nearZeroVar(x_df)
            cols_names = names(x_df)[cols]
            
            final_result = cols_names
        } else if (n_sources > 1) {
            x_df_list = lapply(x_data, function(item) {
                x_df = item[, -which(names(item) %in% sample_cname)]
                cols = caret::nearZeroVar(x_df)
                cols_names = names(x_df)[cols]
                return(cols_names)
            })
            final_result = x_df_list
        }
        # case where 'partitions_list' is provided
    } else if (!is.null(partitions_list)) {
        partitions_list_train = partitions_list$train
        
        # check case where x_data is a single data frame
        if (n_sources == 1) {
            # first subset x_data and y_data, then apply nearZeroVar()
            near_zero_train = lapply(partitions_list_train, function(item) {
                inds = item$Train
                x_subset = x_data[inds, -which(names(x_data) %in% sample_cname)]
                cols = caret::nearZeroVar(x_subset)
                cols_names = names(x_subset)[cols]
                return(cols_names)
            })
            
            inds = which(lengths(near_zero_train) > 0)
            
            if (length(inds) == 0) {
                final_result = "none"
            } else {
                near_zero_train = near_zero_train[inds]
                partitions = names(near_zero_train)
                len = lengths(near_zero_train)
                
                temp_res = mapply(rep, partitions, len)
                
                new_df = data.frame(partitions = unlist(temp_res), features = unlist(near_zero_train))
                new_df = table(new_df)
                final_result = new_df
            }
        } else if (n_sources > 1) {
            near_zero_train_list = lapply(x_data, function(x_df) {
                nz_result = lapply(partitions_list_train, function(item) {
                  inds = item$Train
                  x_subset = x_df[inds, -which(names(x_df) %in% sample_cname)]
                  cols = caret::nearZeroVar(x_subset)
                  cols_names = names(x_subset)[cols]
                  return(cols_names)
                })
                return(nz_result)
            })
            
            new_list = lapply(near_zero_train_list, function(item) {
                inds = which(lengths(item) > 0)
                if (length(inds) == 0) {
                  res = "none"
                } else {
                  item = item[inds]
                  partitions = names(item)
                  len = lengths(item)
                  
                  temp_res = mapply(rep, partitions, len)
                  res = data.frame(partitions = unlist(temp_res), features = unlist(item))
                  res = table(res)
                }
                
                return(res)
            })
            
            final_result = new_list
        }
    }
    
    attr(data_object, "varianceFilt_info") = final_result
    
    return(data_object)
}
