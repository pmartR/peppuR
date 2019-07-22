#' Plots an object of class uniFiltRes
#' 
#' For plotting an S3 object of type 'uniFiltRes'
#'
#' @param uniFiltRes an object of the class 'uniFiltRes', usually created by \code{\link{univariate_feature_selection}}.
#' @param pval_threshold is the cutoff value for pvalues, can be a single value or a vector of distinct pvalues
#' 
#' @return plots ggplot2 object
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
#'ufs_result = univariate_feature_selection(imputed_res)
#'ufs_result2 = univariate_feature_selection(imputed_res2)
#'
#'plot(ufs_result)
#'plot(ufs_result2)
#'
#'}
#'
#' @export

plot.uniFiltRes <- function(uniFilt_results, pval_threshold = NULL) {
    # extract n_sources attribute from uniFilt_results
    n_sources = attr(uniFilt_results, "n_sources")
    
    # check that pval_threshold is appropriate
    if (!is.null(pval_threshold)) {
        if (length(pval_threshold) == 1) {
            if (length(pval_threshold) != n_sources) {
                stop("the number of pval_threshold values must match the number of sources")
            }
            if (pval_threshold > 1 | pval_threshold < 0) {
                stop("pval_threshold must be between 0 and 1")
            }
        } else if (length(pval_threshold) > 1) {
            if (length(pval_threshold) != n_sources) {
                stop("the number of pval_threshold values must match the number of sources")
            }
            if (any(pval_threshold > 1) | any(pval_threshold < 0)) {
                stop("pval_threshold must be between 0 and 1")
            }
        }
    }
    
    if (!is.null(pval_threshold)) {
        
        if (n_sources == 1) {
            pvals = uniFilt_results$p_value
            
            # lets check which features are below pval_threshold and which are over
            features_below = length(which(pvals < pval_threshold))
            features_above = length(which(pvals > pval_threshold))
            
            # create plot data
            feature_type = c(rep("kept", features_below), rep("filtered", features_above))
            plot_data = as.data.frame(table(feature_type))
            
            p = ggplot2::ggplot(data = plot_data, ggplot2::aes(x = feature_type, y = Freq, fill = feature_type)) + ggplot2::geom_bar(stat = "identity", 
                position = "dodge", width = 0.5) + ggplot2::ggtitle(paste("Features Filtered (", pval_threshold, " p-value threshold)", 
                sep = "")) + ggplot2::xlab("data source") + ggplot2::ylab("Number of Features") + ggplot2::geom_text(aes(label = Freq), 
                vjust = -0.3, color = "black", size = 3.5)
        } else if (n_sources > 1) {
            # extract and reorganize data from uniFilt_results
            data = mapply(function(x, pval_thresh) {
                features_below = length(which(x$p_value < pval_thresh))
                features_above = length(which(x$p_value > pval_thresh))
                return(data.frame(kept = features_below, filtered = features_above, pval_thresh = pval_thresh))
            }, uniFilt_results, pval_threshold)
            
            data = as.data.frame(data)
            data = lapply(data, as.data.frame, stringsAsFactors = F)
            
            source_name = names(data)
            
            filtered = unlist(lapply(data, function(x) {
                return(x$filtered)
            }))
            kept = unlist(lapply(data, function(x) {
                return(x$kept)
            }))
            thresh = unlist(lapply(data, function(x) {
                return(x$pval_thresh)
            }))
            
            plot_data = data.frame(data_source = source_name, filtered = filtered, kept = kept, pval_thresh = thresh)
            
            # melting plot data
            melt_data = melt(plot_data, id.vars = c("data_source", "pval_thresh"))
            
            p = ggplot2::ggplot(data = melt_data, ggplot2::aes(x = data_source, y = value, fill = variable)) + ggplot2::geom_bar(stat = "identity", 
                position = "dodge", width = 0.5) + ggplot2::ggtitle("Features Filtered") + ggplot2::xlab("data source") + ggplot2::ylab("Number of Features") + 
                ggplot2::geom_text(aes(label = value), position = position_dodge(width = 0.5), vjust = -0.3, color = "black", size = 3.5) + 
                ggplot2::scale_x_discrete(labels = paste(source_name, pval_threshold, sep = "\n"))
            
        }
    } else if (is.null(pval_threshold)) {
        if (n_sources == 1) {
            data = as.data.frame(uniFilt_results)
            
            p = ggplot2::ggplot(data, aes(x = p_value)) + ggplot2::geom_histogram(color = "black", fill = "orange") + theme_bw() + 
                ggplot2::ggtitle("P value Distribution")
        } else if (n_sources > 1) {
            source_names = names(uniFilt_results)
            data = lapply(uniFilt_results, function(x) {
                p_value = x$p_value
                return(p_value)
            })
            
            plot_data = mapply(function(pvals, names) {
                df = list(p_value = pvals, data_source = rep(names, length(pvals)))
                return(df)
            }, data, source_names)
            
            plot_data = as.data.frame(plot_data)
            plot_data = lapply(plot_data, as.data.frame, stringsAsFactors = F)
            df = do.call("rbind", plot_data)
            
            p = ggplot2::ggplot(df, aes(x = p_value)) + ggplot2::geom_histogram(color = "black", fill = "orange") + ggplot2::facet_wrap(~data_source) + 
                ggplot2::theme_bw() + ggplot2::ggtitle("P value Distribution (by data source)")
            
        }
    }
    
    return(p)
}
