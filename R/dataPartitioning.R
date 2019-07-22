#' Returns information on how to partition y_data
#' 
#' This function returns information on how to partition (randomly or paired) y_data
#' importFrom magrittr "%>%"
#' @param data_object argument is the output produced by as.MLinput, which contains a single x data frame or a list of x data frames, a y data frames and attributes
#' @param partition_style one of 'random' or 'paired' (character string) indicating type of partition
#' @param folds integer of k for k-fold cross validation
#' @param repeats integer of number of iterations to repeat cross validation
#' @param holdout_perc numeric between 0 and 1 indicating the percentage of data to withold for the holdout validation set
#' @export

dataPartitioning = function(data_object, partition_style = "random", folds = 4, repeats = 100, holdout_perc = 0.25) {
    # extract y_data from data_object and cnames
    y_data = data_object$Y
    group_identifier = attr(data_object, "cnames")$outcome_cname
    pair_identifier = attr(data_object, "cnames")$pair_cname
    
    # initial checks
    if (!(partition_style %in% c("random", "paired"))) 
        stop("'partition_style' must be one of, 'random' or 'paired'")
    if (!inherits(y_data, "data.frame")) 
        stop("'y_data' must be of class 'data.frame'")
    
    if (partition_style == "paired" & is.null(pair_identifier)) 
        stop("'pair_identifier' is required for a 'paired' partition_style")
    
    repeats_new = repeats * 1.5
    repeats_extra = repeats_new - repeats
    
    if (partition_style == "random") {
        partition_result = pureRandomTestingTraining(data = y_data, group_identifier = group_identifier, folds = folds, repeats = repeats, 
            holdout_perc = holdout_perc)
        
        # rearrrange partition_result data into test data and training data
        train_list = lapply(partition_result, function(item) {
            item$train
        })
        test_list = lapply(partition_result, function(item) {
            item$test
        })
        
        # form fold and repeat lists
        fld = rep(1:folds, repeats)
        fld = as.list(fld)
        
        reps = as.list(1:repeats)
        reps = lapply(reps, rep, folds)
        reps = unlist(reps)
        reps = as.list(reps)
        
        # form folds repeats data frames
        train_df = mapply(fold_rep_train, train_list, fld, reps)
        train_df = as.data.frame(train_df)
        train_df = lapply(train_df, as.data.frame, stringsAsFactors = F)
        train_df = do.call(rbind, train_df)
        
        test_df = mapply(fold_rep_test, test_list, fld, reps)
        test_df = as.data.frame(test_df)
        test_df = lapply(test_df, as.data.frame, stringsAsFactors = F)
        test_df = do.call(rbind, test_df)
        
        final_result = list(train = train_df, test = test_df)
        final_result = lapply(final_result, function(x) {
            split(data.table::as.data.table(x), by = c("Fold", "Rep"))
        })
        
        ########## partition data for repeats_extra########## ##########
        partition_result_extra = pureRandomTestingTraining(data = y_data, group_identifier = group_identifier, folds = folds, repeats = repeats_extra, 
            holdout_perc = holdout_perc)
        
        # rearrrange partition_result data into test data and training data
        train_list = lapply(partition_result_extra, function(item) {
            item$train
        })
        test_list = lapply(partition_result_extra, function(item) {
            item$test
        })
        
        # form fold and repeat lists
        fld = rep(1:folds, repeats_extra)
        fld = as.list(fld)
        
        reps = as.list(1:repeats_extra)
        reps = lapply(reps, rep, folds)
        reps = unlist(reps)
        reps = as.list(reps)
        
        # form folds repeats data frames
        train_df = mapply(fold_rep_train, train_list, fld, reps)
        train_df = as.data.frame(train_df)
        train_df = lapply(train_df, as.data.frame, stringsAsFactors = F)
        train_df = do.call(rbind, train_df)
        
        test_df = mapply(fold_rep_test, test_list, fld, reps)
        test_df = as.data.frame(test_df)
        test_df = lapply(test_df, as.data.frame, stringsAsFactors = F)
        test_df = do.call(rbind, test_df)
        
        final_result_extra = list(train = train_df, test = test_df)
        final_result_extra = lapply(final_result_extra, function(x) {
            split(data.table::as.data.table(x), by = c("Fold", "Rep"))
        })
        
    } else if (partition_style == "paired") {
        partition_result = pairedCaseControlTestingTraining(data = y_data, group_identifier = group_identifier, pair_identifier = pair_identifier, 
            folds = folds, repeats = repeats)
        
        # rearrrange partition_result data into two tibble data frames, one for test data one for train data
        train_list = lapply(partition_result, function(item) {
            item$train
        })
        test_list = lapply(partition_result, function(item) {
            item$test
        })
        
        # form fold and repeat lists
        fld = rep(1:folds, repeats)
        fld = as.list(fld)
        
        reps = as.list(1:repeats)
        reps = lapply(reps, rep, folds)
        reps = unlist(reps)
        reps = as.list(reps)
        
        # form folds repeats data frames
        train_df = mapply(fold_rep_train, train_list, fld, reps)
        train_df = as.data.frame(train_df)
        train_df = lapply(train_df, as.data.frame, stringsAsFactors = F)
        train_df = do.call(rbind, train_df)
        
        test_df = mapply(fold_rep_test, test_list, fld, reps)
        test_df = as.data.frame(test_df)
        test_df = lapply(test_df, as.data.frame, stringsAsFactors = F)
        test_df = do.call(rbind, test_df)
        
        final_result = list(train = train_df, test = test_df)
        final_result = lapply(final_result, function(x) {
            split(data.table::as.data.table(x), by = c("Fold", "Rep"))
        })
        
        ########## partition data for repeats_extra########## ##########
        partition_result_extra = pairedCaseControlTestingTraining(data = y_data, group_identifier = group_identifier, pair_identifier = pair_identifier, 
            folds = folds, repeats = repeats)
        
        # rearrrange partition_result data into two tibble data frames, one for test data one for train data
        train_list = lapply(partition_result_extra, function(item) {
            item$train
        })
        test_list = lapply(partition_result_extra, function(item) {
            item$test
        })
        
        # form fold and repeat lists
        fld = rep(1:folds, repeats_extra)
        fld = as.list(fld)
        
        reps = as.list(1:repeats_extra)
        reps = lapply(reps, rep, folds)
        reps = unlist(reps)
        reps = as.list(reps)
        
        # form folds repeats data frames
        train_df = mapply(fold_rep_train, train_list, fld, reps)
        train_df = as.data.frame(train_df)
        train_df = lapply(train_df, as.data.frame, stringsAsFactors = F)
        train_df = do.call(rbind, train_df)
        
        test_df = mapply(fold_rep_test, test_list, fld, reps)
        test_df = as.data.frame(test_df)
        test_df = lapply(test_df, as.data.frame, stringsAsFactors = F)
        test_df = do.call(rbind, test_df)
        
        final_result_extra = list(train = train_df, test = test_df)
        final_result_extra = lapply(final_result_extra, function(x) {
            split(data.table::as.data.table(x), by = c("Fold", "Rep"))
        })
    }
    
    attr(data_object, "partition_info") = final_result
    attr(data_object, "extra_partitions") = final_result_extra
    attr(data_object, "foldrep") = list(folds = folds, repeats = repeats)
    
    return(data_object)
}


pureRandomTestingTraining <- function(data, group_identifier, folds = 4, repeats = 100, holdout_perc = 0.25) {
    
    groups <- data[, group_identifier]
    seed <- 42
    
    #------ determine training indeces ----------#
    # returns a k x repeats size list of training indeces set seed for reproducability purposes
    set.seed(42)
    training_ind_list <- caret::createMultiFolds(y = groups, k = folds, times = repeats)
    
    #------ diff data and train to return testing indeces -----#
    # returns a k x repeats size list of grouped testing and training indeces
    ind_list <- lapply(training_ind_list, function(training_inds) {
        partition_list <- list()
        partition_list$train <- training_inds
        partition_list$test <- setdiff(1:nrow(data), training_inds)
        return(partition_list)
    })
    return(ind_list)
}



pairedCaseControlTestingTraining <- function(data, group_identifier, pair_identifier, folds = 4, repeats = 100, holdout_set = NULL) {
    
    tuning_set <- FALSE
    seed <- 42
    #------- create a map to keep orignal indexing in tact -----#
    data$old_index <- 1:nrow(data)
    #------- remove the holdout set if there is one ---------#
    if (length(holdout_set) > 0) {
        data <- data[-which(data[, pair_identifier] %in% holdout_set), ]
    }
    #------- use map to find unique pairs -----#
    singled_pair_subset <- data %>% dplyr::arrange_(group_identifier) %>% dplyr::distinct_(pair_identifier, .keep_all = TRUE)  #keep only unique identifiers for training subset
    singled_pair_subset[1:nrow(singled_pair_subset)/2, group_identifier] <- 1  # convert half the labes to zeros for caret's balancing act  
    index_map <- data.frame(old = singled_pair_subset$old_index, new = 1:nrow(singled_pair_subset), pair = singled_pair_subset[, pair_identifier])
    rownames(index_map) <- index_map$new
    
    #------ vector of group labels to split ------#
    group <- singled_pair_subset[, group_identifier]
    
    #------ determine training indeces ----------#
    # returns a k x repeats size list of training indeces
    training_ind_list <- caret::createMultiFolds(y = group, k = folds, times = repeats)
    
    #------- match case-control pairs --------#
    ind_list <- lapply(training_ind_list, function(x) {
        pairs <- index_map[x, "pair"]
        partition_list <- list()
        partition_list$train <- data[which(data[, pair_identifier] %in% pairs), "old_index"]
        partition_list$test <- data[which(!data[, pair_identifier] %in% pairs), "old_index"]
        return(partition_list)
    })
    #------ diff data and train to return testing indeces -----#
    # returns a k x repeats size list of grouped testing and training indeces ind_list <- lapply(training_ind_list_mapped,
    # function(training_inds){ pairs <- index_map[x, 'pair'] partition_list <- list() partition_list$train <- training_inds
    # partition_list$test <- data[which(!data[,pair_identifier] %in% pairs), 'old_index'] return(partition_list) })
    
}

# folds repeats helper functions for training data and test data
fold_rep_train = function(train_vec, fold, repp) {
    result = data.frame(Fold = rep(fold, length(train_vec)), Rep = rep(repp, length(train_vec)), Train = train_vec)
    return(result)
}

fold_rep_test = function(train_vec, fold, repp) {
    result = data.frame(Fold = rep(fold, length(train_vec)), Rep = rep(repp, length(train_vec)), Test = train_vec)
    return(result)
}
