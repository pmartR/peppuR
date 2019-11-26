#'Creates input for ML packages
#'
#'This function creates an object that is ready to be used with peppuR package
#'functions
#'
#'@importFrom magrittr "%>%"
#'
#'@param X data.frame or a list of data.frames all with n rows, f+1 columns, where one of the columns is a unique
#'  sample identifier
#'@param Y (optional) data.frame, one column designating sample id and the other
#'  columns give extra info, e.g. outcome, pair. If NULL, Y will be created from
#'  X using the meta_colnames argument
#'@param meta_colnames chr, defaults to NULL, otherwise a character vector of
#'  column names in \code{X} that designate extra info e.g. outcome, pair as
#'  well as sample id
#'@param categorical_features logical, defaults to FALSE, specifies whether X
#'  contains categorical features
#'@param sample_cname chr, indicates which column contains sample ids
#'@param outcome_cname chr, indicates which column contains the response
#'  variable or classification outcome
#'@param pair_cname chr, (optional) indicates which column contains pairing
#'  information if the data are under a paired design
#'
#'@details
#'
#' @examples
#'dontrun{
#'library(peppuR)
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
#'}
#'@export
#'

as.MLinput <- function(X, Y, meta_colnames = NULL, categorical_features = FALSE, sample_cname, outcome_cname, pair_cname = NULL) {
  # check that X and Y are of appropriate class #
  if (!inherits(X, c("data.frame", "list"))) 
    stop("X must be of class 'data.frame' or 'list'")
  if (!inherits(Y, "data.frame")) {
    if(!is.null(Y)) {
      stop("Y must either be a 'data.frame' or NULL") 
    }
  } 
  
  #------- make Y from X if Y is NULL ------#
  if(is.null(Y)) {
    if(is.null(meta_colnames)){
      stop("Both Y and meta_colnames are NULL. Please specify either Y or meta_colnames")
    } else {
      # if sample_cname is part of meta_colnmaes, separate them
      if(sample_cname %in% meta_colnames) {
        meta_colnames <- meta_colnames[which(meta_colnames != sample_cname)]
      }
      # If X is a data frame, simply extract the Y columns
      if (inherits(X, "data.frame")) {
        Y <- X[, which(colnames(X) %in% unique(c(meta_colnames, sample_cname)))]
        X <- X[, -which(colnames(X) %in% unique(meta_colnames))]
      } else if (inherits(X, "list")) { 
        # If X is a list of data frames, loop over the list and extract the columns belonging to Y
        raw_Y <- vector("list", length(X))
        for (data_source in 1:length(X)){
          # data_source must have "sample_cname"
          x_data <- X[[data_source]]
          if (!sample_cname %in% colnames(x_data)){
            stop("All X must have sample names to ensure the same ordering in Y")
          } else {
            # extract the Y columns
            raw_Y[[data_source]] <- x_data[, which(colnames(x_data) %in% unique(c(meta_colnames, sample_cname))), drop = FALSE] #keep df structure
            # remove the Y columns from X aside from sample name
            if(any(colnames(x_data) %in% unique(meta_colnames))){
              X[[data_source]] <- x_data[, -which(colnames(x_data) %in% unique(meta_colnames))]
            }
          }
        }
        
        # make Y a data frame arranged by sample name
        Y <- raw_Y %>% 
          purrr::reduce(dplyr::left_join)
        # X <- lapply(X, function(d_source){
        #   subX <- d_source[which(d_source[,sample_cname] %in% Y[,sample_cname]), ]
        #   newX <- build_x_mat(subX)
        #   return(newX)
        # } )
        
      }
    }
  }
  #----- if X is a data frame, turn it to a list --------#
  if (inherits(X, "data.frame")) {
    temp <- vector("list")
    temp$source1 <- X
    X <- temp
  }
  if (inherits(X, "list")) {
    # X should be a named list, if not assign generic names to the list
    if (is.null(names(X))) {
      names(X) <- paste(rep("source", length(X)), 1:length(X), sep = "")
    }
    # apply checks across all X
    passed_checks <- lapply(X, MLinput_helper, y_df = Y, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
    # format categorical columns
    if (categorical_features) {
      # first make an attempt at converting type from characters
      #X <- type.convert(X)
    }
    # find all columns that appear as factors
    categorical_list <- lapply(X, function(df) {
      lapply(df[, -which(names(df) == sample_cname)], is.factor) # finds factor columns
    })
    # count the number of factors in each source
    categorical_sum <- unlist(lapply(categorical_list, function(x) {
      sum(unlist(x))
    }))
    # if there are no categorical features...
    if(all(categorical_sum == 0)){ #...and there should be, error out
      cat_cols <- "none"
      if (categorical_features) stop("No categorical features detected. Change categorical_features to FALSE or change categorical columns to factors")
      # if there are categorical features...
    } else if (any(categorical_sum >= 1)) {
      cat_cols <- lapply(categorical_list, function(x) {
        ind <- which(unlist(x))
        return(names(ind)) # return categorical feature locations 
      })
      df_with_cat <- names(which(categorical_sum >= 1)) # note which sources have categorical features
      if (!categorical_features){ #...and there shouldn't be, error out
        stop(paste(df_with_cat, " has categorical features detected, change these features to numeric or set 'categorical_features' to TRUE", 
                   sep = ""))
      }
      # Change categorical features to dummy variables
      cat_cols <- vector("list", length(X))
      for(i in 1:length(X)){
        dsource <- X[[i]]
        if(any(unlist(lapply(dsource, is.factor)))){
          temp <- dummy_var_fun(dsource, sample_cname)
          X[[i]] <- temp[[1]]
          cat_cols[[i]] <- temp[[2]]
        } 
      }
      names(cat_cols) <- names(X)
      # X <- lapply(X, function(dsource, sample_cname){
      #    else {
      #     result <- dsource
      #   }
      #   return(result)
      #   #ifelse(any(unlist(lapply(dsource, is.factor))), yes = dummy_var_fun(dsource, sample_cname), no = dsource)
      # }, sample_cname = sample_cname)
    }
    
    # here we apply the allna_row_helper function to X and Y to remove all NA rows
    allna_row_results <- lapply(X, allna_row_helper, y = Y, sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
    allna_rows <- unlist(allna_row_results)
    
    if (length(allna_rows) >= 1) {
      # lets remove allna_rows from X data frames and also Y data frame
      X <- lapply(X, function(df) {
        inds <- which(df[[sample_cname]] %in% allna_rows)
        return(df[-inds, ])
      })
      y_inds = which(Y[[sample_cname]] %in% allna_rows)
      Y <- Y[-y_inds, ]
    }
    
    # key X by sample id and remove sample id from X
    X <- lapply(X, function(df){
      rownames(df) <- df[, sample_cname] #key
      # df <- df %>%
      #   dplyr::select(-sample_cname) #remove sample id
      return(df)
    })
    
    # key Y by sample id
    rownames(Y) <- as.character(Y[, sample_cname])
    
    # create output list
    output_list <- list(X = X, Y = Y)
    
    # set number of sources attribute
    attr(output_list, "n_sources") <- length(X)
    
    # check for NA values in the columns of data frames stored in 'X' list
    # set missing_data attribute
    na_df <- lapply(X, function(x) {
      temp <- as.data.frame(colSums(is.na(x))/nrow(x))
      names(temp) <- "NA percent per column"
      return(temp)
    })
    attr(output_list, "missing_data") = na_df
    
    # set has_na attribute
    has_na <- lapply(na_df, function(df){
      any(df[, "NA percent per column"] > 0)
    })
    attr(output_list, "has_na") <- has_na
    
    # calculating data_info for each data frame in 'X' list. e.g. we need a named list of 'number_of_features' counts
    number_of_samples <- nrow(Y)
    number_of_features <- as.data.frame(lapply(X, function(d_source){
      return(sum(!colnames(d_source) %in% sample_cname))
    }))
    #row.names(number_of_features) <- "n_features"
    
    # setting list attributes
    attr(output_list, "cnames") <- list(sample_cname = sample_cname, outcome_cname = outcome_cname, pair_cname = pair_cname)
    attr(output_list, "data_info") <- list(number_of_samples = number_of_samples, number_of_features = unlist(number_of_features), 
                                           paired = !is.null(pair_cname))
    attr(output_list, "categorical_columns") = list(categorical_cols = cat_cols)
    #----- if X is a single data frame --------#
  } else {stop("X must be a list or a data.frame")}
  
  # Finally convert X to a matrix for learning
  X <- lapply(X, function(d_source){
    subX <- d_source[which(d_source[,sample_cname] %in% Y[,sample_cname]), ]
    newX <- build_x_mat(subX)
    return(newX)
  } )
  
  # adding 'MLinput' class to output_list
  class(output_list) = c("MLinput", "list")
  return(output_list)
}


# helper function to apply to a single X data frame, applies all checks
MLinput_helper = function(x_df, y_df, sample_cname, outcome_cname, pair_cname) {
  # added a check to make sure x_df and y_df have the same number of rows
  if (nrow(x_df) != nrow(y_df)) stop("X and Y do not have the same number of rows")
  
  # check that sample_cname is present in x_df and y_df
  if (!(sample_cname %in% names(x_df)) | !(sample_cname %in% names(y_df))) {
    stop(paste(sample_cname, " is not a column of both X and Y", sep = ""))
  }
  
  # check for missing values in sample ids and outcomes (not allowed)
  if (any(is.na(x_df[, sample_cname]))) stop("Missing sample names in X")
  if (any(is.na(y_df[, sample_cname]))) stop("Missing sample names in Y")
  if (any(is.na(y_df[, outcome_cname]))) stop("Missing outcomes in Y")
  
  # first need to check that the rows of x_df and the rows of y_df are in the same order
  indices = match(y_df[, sample_cname], x_df[, sample_cname])
  
  # reorder Y to match row order of X
  x_df = x_df[indices, ]
  
  # check that pair_cname is in both X and Y and that there is more than one unique thing in pair_cname column
  if (!is.null(pair_cname)) {
    if (!(pair_cname %in% names(y_df))) {
      stop(paste(pair_cname, "is not a column of Y", sep = ""))
    }
    # check for missing values in pairs
    if (any(is.na(y_df[, pair_cname]))) stop("Missing pair information")
    
    # check that pair_cname column has at least 2 unique items
    if (length(unique(y_df[, which(names(y_df) %in% pair_cname)])) < 2) 
      stop(paste(pair_cname, "column should contain at least 2 unique items", sep = " "))
    
    # check that there are atleast 2 observations with each unique pair_id
    if (any(table(y_df[, which(names(y_df) == pair_cname)]) < 2)) 
      stop(paste("there needs to be at least two observations for each unique", pair_cname, sep = " "))
  }
  
  # check the number of items in outcome column, should be equal to number of rows in X
  if (length(y_df[, which(names(y_df) == outcome_cname)]) != nrow(x_df)) 
    stop(paste(outcome_cname, "column does not have the same number of items as the number of rows in X"))
  
  # check that outcome_cname is NOT in X
  if (outcome_cname %in% names(x_df)) 
    stop(paste(outcome_cname, "is a column in X", sep = ""))
  
  # check that outcome_cname col is a character vector why?
  # if (!is.factor(y_df[[outcome_cname]])) 
  #     stop(paste(outcome_cname, " column is not a character vector", sep = ""))
  
  return(T)
}

# helper function to apply to a single X data frame and Y data frame, checks for and removes all NA rows in X and Y
allna_row_helper = function(x, y, sample_cname, outcome_cname, pair_cname = NULL) {
  # check that there are no all NA rows in x
  all_na_rows = which(rowSums(is.na(x[, -which(names(x) == sample_cname)])) == (ncol(x) - 1))
  allna_names = x[all_na_rows, sample_cname]
  
  if (length(all_na_rows) > 0) {
    message("there are rows with all NA values, these samples will be removed from all data sources")
    
    # check that there is more than one unique outcome in the pair value that the all NA row belonged to
    if (!is.null(pair_cname)) {
      pair_rm = lapply(all_na_rows, function(z) {
        pair_id = y[z, pair_cname]
        y_sub = y[which(y[[pair_cname]] == pair_id), ]
        if (length(unique(y_sub[[outcome_cname]])) < 2) {
          pair_remove = pair_id
        } else pair_remove = NULL
        return(pair_remove)
      })
      
      # remove pair from y if needed which pair_cnames to remove
      pair_rm = unlist(pair_rm)
      
      # which rows have pair_cnames we want to remove, remove these rows from y data frame
      if (!is.null(pair_rm)) {
        inds = lapply(pair_rm, function(z) {
          inds = which(y[[pair_cname]] == z)
          return(inds)
        })
        
        inds = unlist(inds)
        inds_names = y[[sample_cname]][inds]
        
        allna_names = c(allna_names, inds_names)
      }
    }
  }
  return(allna_names)
}

build_x_mat <- function(cov_df){
  
  if(is.null(ncol(cov_df))){
    cov_df <- matrix(cov_df,ncol=1)
  }
  
  #If all covariates are numeric, simply return the same matrix back
  if(is.numeric(cov_df)){
    return(data.matrix(cov_df))
  }
  
  #If the covariates are a mix of numeric, factors and characters, return matrix of group identifiers
  Xmatrix <- NULL
  for(i in 1:ncol(cov_df)){
    
    if(is.numeric(cov_df[,i])){
      #If column i is numeric, append it to X
      Xmatrix <- cbind(Xmatrix,cov_df[,i])
    }else{
      coli_levels <- unique(cov_df[,i])
      n_levels <- length(coli_levels)
      if(n_levels!=length(cov_df[,i])){
        Xcoli <- matrix(0,nrow(cov_df),n_levels)
        
        for(j in 1:n_levels){
          Xcoli[cov_df[,i]==coli_levels[j],j] <- 1
        }
        Xmatrix <- cbind(Xmatrix,Xcoli)
      }
    }
  }
  return(Xmatrix)
}

dummy_var_fun <- function(X, sample_cname){
  dmy <- caret::dummyVars(formula = paste("`",sample_cname,"`", "~ .", sep = ""), data = X, fullRank = TRUE)
  newX <- predict(dmy, newdata = X)
  newX <- as.data.frame(newX)
  X <- X %>%
    dplyr::select(sample_cname) %>%
    cbind(newX)
  new_categorical_names <- names(newX)[!names(newX) %in% dmy$vars]
  return(list(X, new_categorical_names))
  
  
  
  
}