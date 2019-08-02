#' Computes features p-values
#'
#' This function computes p-values for features of a single data frame or a list of data frames using, glm, lmer and glmer functions
#'
#' @param data_object argument is the output produced by as.MLinput function, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' @details 
#' 
#' @examples 
#'dontrun{
#'library(peppuR)
#'library(missForest)
#'library(mice)
#'
#'data("single_source")
#'data("multi_source")
#'
#'x_multi = multi_source$X
#'y_multi = multi_source$Y
#'
#'x_single = single_source$X
#'y_single = single_source$Y
#'
#'sample_cname = "ID"
#'outcome_cname = "Group"
#'pair_cname = "paircol"
#'
#'result = as.MLinput(X = x_single, Y = y_single, categorical_features = T , sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'result2 = as.MLinput(X = x_multi, Y = y_multi, categorical_features = T, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
#'
#'imputed_res = impute_missing(result, method = "randomforest")
#'imputed_res2 = impute_missing(result2, method = "randomforest")
#'
#'ufs_result = univariate_feature_selection(imputed_res)
#'ufs_result2 = univariate_feature_selection(imputed_res2)
#'
#'}
#' 
#' @export

univariate_feature_selection = function(data_object){
  #extract x and y data frames from data_object, as well as cnames
  x = data_object$X
  y = data_object$Y
  outcome_cname = attr(data_object, "cnames")$outcome_cname
  pair_cname = attr(data_object, "cnames")$pair_cname
  sample_cname = attr(data_object, "cnames")$sample_cname
  n_sources = attr(data_object, "n_sources")
  categorical_cols = attr(data_object, "categorical_columns")$categorical_cols
  
  #case where x is a single data frame
  # if(n_sources == 1){
  #     x_result = test_helper(x_mat = x, categorical_cols = categorical_cols, y_mat = y, sample_cname = sample_cname, pair_cname = pair_cname, outcome_cname = outcome_cname)
  # }
  #case where x is a list of data frames
  #else if(n_sources > 1){
      x_result = mapply(test_helper, x, categorical_cols,
                        MoreArgs = list(y, sample_cname, pair_cname,
                                        outcome_cname), USE.NAMES = T)
      x_result = as.data.frame(x_result)
      x_result = lapply(x_result, as.data.frame, stringsAsFactors = F)
#  }
  
  #add number of sources attribute to x_result
  attr(x_result, "n_sources") = n_sources
  
  #assign class 'uniFiltRes' to x_result
  class(x_result) = c("uniFiltRes", "list")
return(x_result)  
}



#we need a helper function that applies test to all the columns of the x data frame
test_helper = function(x_mat, categorical_cols, y_mat, sample_cname, pair_cname, outcome_cname){
  #create bound data from x and y data frames
  bound_data = merge(x_mat, y_mat, by = sample_cname)
  #first, check if pair_cname is NULL or non-NULL
  if(is.null(pair_cname)){
    
    if(length(categorical_cols) == 0){
      #remove sample_cname col form bound_data
      samp_ind = which(names(bound_data) == sample_cname)
      bound_data = bound_data[, -samp_ind] 
      
      col_names = names(bound_data)
      col_names = setdiff(col_names, outcome_cname)
      
      pvals = lapply(col_names, function(x) applyglm(bound_data, feature = x, outcome_cname = outcome_cname))
      
      result = data.frame(feature_name = col_names, p_value = unlist(pvals), stringsAsFactors = F)
    }else{
      samp_ind = which(names(bound_data) == sample_cname)
      bound_data = bound_data[, -samp_ind] 
      
      catcol_inds = which(names(bound_data) %in% unlist(categorical_cols))
      
      #here we apply glm binomial function to categorical columns of bound_data
      c_pvals = lapply(categorical_cols, function(x) applyglm2(bound_data, feature = x, outcome_cname = outcome_cname))
      c_pvals = unlist(c_pvals)
      names(c_pvals) = categorical_cols
      
      #now we apply glm gaussian function to numeric columns of bound_data
      remaining_data = bound_data[, -catcol_inds]
      
      col_names = names(remaining_data)
      col_names = setdiff(col_names, outcome_cname)
      
      pvals = lapply(col_names, function(x) applyglm(remaining_data, feature = x, outcome_cname = outcome_cname))
      pvals = unlist(pvals)
      names(pvals) = col_names
      
      all_pvals = c(c_pvals, pvals)
      all_names = names(all_pvals)
      
      result = data.frame(feature_name = all_names, p_value = unlist(all_pvals), stringsAsFactors = F)
    }
    ### this code should've been removed? 9/26 ###
    #remove sample_cname col form bound_data
    #samp_ind = which(names(bound_data) == sample_cname)
    #bound_data = bound_data[, -samp_ind]
    
    #applying glm function to bound_data
    #col_names = names(bound_data[, -which(names(bound_data) == outcome_cname)])
    #pvals = lapply(col_names, function(x) applylm(bound_data, feature = x, outcome_cname = outcome_cname))
    
    #result = data.frame(feature_name = col_names, p_value = unlist(pvals), stringsAsFactors = F)
  }else{
    #case where pair is non NULL
    
    if(length(categorical_cols) == 0){
      #remove sample_cname col form bound_data
      samp_ind = which(names(bound_data) == sample_cname)
      bound_data = bound_data[, -samp_ind] 
      
      col_names = names(bound_data)
      col_names = setdiff(col_names, (c(pair_cname, outcome_cname)))
      
      pvals = lapply(col_names, function(x) applylmer(bound_data, feature = x, outcome_cname = outcome_cname, pair_cname = pair_cname))
      
      result = data.frame(feature_name = col_names, p_value = unlist(pvals), stringsAsFactors = F)
    }else{
      samp_ind = which(names(bound_data) == sample_cname)
      bound_data = bound_data[, -samp_ind] 
      
      catcol_inds = which(names(bound_data) %in% categorical_cols)

      #here we apply glmer function to categorical columns of bound_data
      c_pvals = lapply(categorical_cols, function(x) applyglmer(bound_data, feature = x, outcome_cname = outcome_cname, pair_cname = pair_cname))
      c_pvals = unlist(c_pvals)
      names(c_pvals) = categorical_cols
      
      #now we apply lmer function to numeric columns of bound_data
      remaining_data = bound_data[, -catcol_inds]
      
      col_names = names(remaining_data)
      col_names = setdiff(col_names, (c(pair_cname, outcome_cname)))
      
      pvals = lapply(col_names, function(x) applylmer(remaining_data, feature = x, outcome_cname = outcome_cname, pair_cname = pair_cname))
      pvals = unlist(pvals)
      names(pvals) = col_names
      
      all_pvals = c(c_pvals, pvals)
      all_names = names(all_pvals)
      
      result = data.frame(feature_name = all_names, p_value = unlist(all_pvals), stringsAsFactors = F)
    }
    

      }

  return(result)
}


applyglm <- function(bound_data, feature, outcome_cname){
  test <- glm(as.formula(paste(feature, "~", outcome_cname, sep = "")), data = bound_data, family = gaussian())
  return(coef(summary(test))[, 'Pr(>|t|)'][2])
}

applyglm2 <- function(bound_data, feature, outcome_cname){
  test <- glm(as.formula(paste(feature, "~", outcome_cname, sep = "")), data = bound_data, family = binomial())
  return(coef(summary(test))[, 'Pr(>|z|)'][2])
}

applylmer <- function(bound_data, feature, outcome_cname, pair_cname){
  test <- lme4::lmer(as.formula(paste(feature, "~", outcome_cname, "+", "(1|", pair_cname, ")", sep = "")), data = bound_data)
  
  coefs = data.frame(coef(summary(test)))
  coefs$p.z = 2 *(1 - pnorm(abs(coefs$t.value)))
  return(coefs$p.z[2])
}

applyglmer <- function(bound_data, feature, outcome_cname, pair_cname){
  test <- lme4::glmer(as.formula(paste(feature, "~", outcome_cname, "+", "(1|", pair_cname, ")", sep = "")), data = bound_data, family = binomial)
  
  return(coef(summary(test))[, 'Pr(>|z|)'][2])
}
