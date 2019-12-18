require(dplyr)
#' Repeated Optimized Feature Integration
#' @importFrom magrittr "%>%"
#' @param MLinput an as.MLinput object which contains a single X data frame or a
#'   list of X data frames, a Y data frame and attributes
#' @param source_alg_pairs a named vector of algorithms (one of "knn", "nb",
#'   "svm", or "rf") with names as the corresponding data source
#' @param nn integer. The number of times to repeat the optimization in its
#'   entirety
#' @param f_prob numeric greater than 0 and leq 1. The proportion of the full
#'   feature set to initialize the optimization routine
#' @param nu numeric greater than 0 and leq 1. The scale value for feature
#'   acceptance criteria of a difference in AUC values
#' @param max_iter int. Maximum number of iterations to allow in nn iterations
#' @param conv_check int. Number of iterations at which to perform a
#'   convergence check. Typically set to the total number of features
#' @param epsilon numeric greater than 0 and leq 1. AUC convergence threshold,
#'   typically small (< 0.1).
#' @param after_conv_checks int. After the initial convergence check, the
#'   Interval of iterations at which to perform a convergence check
#' @export
rofi <- function(MLinput, source_alg_pairs, nn = 1, f_prob=0.1 , nu=1/100,
                 max_iter=2*sum(attr(MLinput, "data_info")$number_of_features), 
                 conv_check=(sum(attr(MLinput, "data_info")$number_of_features)+1),
                 epsilon = 0.01, after_conv_checks = 100, verbose=FALSE){
  
  #--- Initialization ----#
  sample_cname <- attr(MLinput, "cnames")$sample_cname
  n_sources <- attr(MLinput, "n_sources")
  total_features <- sum(unlist(lapply(MLinput$X, function(d_source) sum(!colnames(d_source) %in% sample_cname))))
  feature_inds <- seq(from = 1, to = total_features, by = 1)
  
  #---- List all features and their sources -----#
  feature_list <- vector("list", n_sources)
  source_list <- vector("list", n_sources)
  for (d_source in 1:n_sources){
      feature_list[[d_source]] <- colnames(MLinput$X[[d_source]])[which(!colnames(MLinput$X[[d_source]]) %in% sample_cname)]
      source_list[[d_source]] <- rep(names(MLinput$X)[d_source], length(feature_list[[d_source]]))
  }
  #---- Create a map between features, their indices, and source names -----#
  feature_map <- data.frame(Feature = unlist(feature_list), Position = feature_inds, Source = unlist(source_list), stringsAsFactors = FALSE)
  
  sources <- names(source_alg_pairs) #Named data sources
  if (is.null(sources)) {
    stop("source_alg_pairs must be a named vector. The vector is a list of algorithms and the names are the corresponding data source")
  }
  
  # How often the AUC is recorded for convergence purposes
  if (after_conv_checks > conv_check) {
    stop("after_conv_checks should be smaller than conv_checks")
  }
  benchmark_auc <- after_conv_checks
  
  # Results list
  results <- vector("list", nn)
  
  for(i in 1:nn){
    aucchecks <- NULL

    #Create elements in results list
    results[[i]] <- vector("list",4)
    names(results[[i]]) <- c("AllAUC","BestAUC","f_Vectors","Changed")
    results[[i]]$BestAUC <- rep(NA,max_iter)
    results[[i]]$AllAUC <- rep(NA,max_iter)
    results[[i]]$f_Vectors <- matrix(NA,max_iter,total_features)
    results[[i]]$Changed <- rep(NA,max_iter)
    
    #------1. Select a ranodm number of 100*f_prob% features to include in model  
    fveci <- rbinom(total_features,1,f_prob)
    
    #------2. Do cross validation using randomly chosen features
    included_features <- feature_map$Feature[which(fveci==1)]
    # subset data to those features
    subsetted_data <- MLinput
      subsetted_data$X <- lapply(subsetted_data$X, function(d_source){
        d_source <- d_source[ ,which(colnames(d_source) %in% c(included_features, sample_cname)), drop=FALSE] # TODO:Features could be named the same across sources, account for this later
        return(d_source)
      })

    #----- Learn on all sources and retrieve initial AUC------#
    all_res <- fvecLearning(subsetted_data, source_alg_pairs) # TODO: Test edge cases of single/no features
    if (n_sources == 1) {
      all_res_probs <- all_res[[1]]
    } else {
      all_res_probs <- naiveIntegration(all_res)
    }
    # calculate AUC
    all_res_roc <- AUC::roc(predictions = all_res_probs$PredictedProbs.1, labels = as.factor(all_res_probs$Truth))
    all_AUC <- AUC::auc(all_res_roc)
    
    iter <- 1
    results[[i]]$BestAUC[iter] <- results[[i]]$AllAUC[iter] <- auci <- all_AUC
    results[[i]]$f_Vectors[iter,] <- fveci

    #------3. Repeat until AUC_diff is below some threshold
    auc_diff <- 1
    
    # Create the vector that defines the order in which each features is turned on/off
    # change later to avoid creating a vector of length maxIter...
    if((max_iter-1)<=total_features){
      flip_order <- sample(total_features,max_iter-1,replace = FALSE)
    }else{
      flip_order <- rep(NA,max_iter-1)
      
      for(lazy in 1:round((max_iter-1)/total_features,0)){
        flip_order[(1:total_features)+total_features*(lazy-1)] <- sample(total_features)
      }
      flip_order[-c(1:(total_features*lazy))] <- sample(total_features,(max_iter-1)%%total_features,replace=FALSE)
    }
    # prepend a 0 for the first change
    flip_order <- c(0,flip_order)
    results[[i]]$Changed <- flip_order
    
    while(iter<max_iter){
      iter <- iter+1
      
      #Step a
      fvecip1 <- fveci
      
      #Step b - flip random feature on or off
      ##Randomly select a source to perturb
      #rsource <- sample(nsources,1)
      ##Ranomly select a feature within that source
      #toflip <- sample(source_cols[[rsource]],1)
      ##Each feature gets same probability of inclusion
      #toflip <- base::sample(length(fvecip1),size=1,replace = FALSE)
      ##Use flip_order to decide which is turned on/off
      toflip <- flip_order[iter]
      
      #results[[i]]$Changed[iter] <- toflip
      #print(toflip)
      #cat("\n")
      fvecip1[toflip] <- 1-fvecip1[toflip]
      flipped <- rep(FALSE,length(source_alg_pairs))
      flipped_name <- feature_map$Feature[toflip]
      # Find which data source the flipped feature belongs to
      #print(toflip)
      #print(feature_map$Source[toflip])
      #print(which(feature_map$Source[toflip] %in% sources))
      #flipped[which(feature_map$Source[toflip] %in% sources)] <- TRUE
      # for(j in 1:length(sources)){
      flipped[which(sources %in% feature_map$Source[toflip])] <- TRUE
      # for(j in 1:length(sources)){
      #   if(length(grep(sources[j],flipped_name))>0){
      #     flipped[j] <- TRUE
      #   }
      # }
      # 
      #Step c - compute AUC with perturbed feature vector
      #Xdata_i <- Xdata[,which(fvecip1==1)] #Only keep the randomly selected features (columns of Xdata)
      included_features <- feature_map$Feature[which(fvecip1==1)]
      subsetted_data <- MLinput
      
      subsetted_data$X <- lapply(subsetted_data$X, function(d_source){
        d_source <- d_source[ ,which(colnames(d_source) %in% c(included_features, sample_cname)), drop=FALSE] # TODO:Features could be named the same across sources, account for this later
        return(d_source)
      })
      # all_res <- run_all(Xdf = Xdata_i, Ydf = Ydata, parts = partitions, sa_pairs = source_alg_pairs, previous_run = all_res$allOut, to_update=flipped)
      # aucip1 <- all_res$AUC
      # 
      all_res <- fvecLearning(subsetted_data, source_alg_pairs, previous_run = all_res, to_update = flipped) # TODO: Test edge cases of single/no features
      if (n_sources == 1) {
        all_res_probs <- all_res[[1]]
      } else {
        all_res_probs <- naiveIntegration(all_res)
      }
      all_res_roc <- AUC::roc(predictions = all_res_probs$PredictedProbs.1, labels = as.factor(all_res_probs$Truth))
      aucip1 <- AUC::auc(all_res_roc)
      
      
      #Step d - compute acceptance prob and flip coin
      # i - acceptance prob
      theta <- min(1,exp((aucip1-auci)/nu))
      if(verbose){
        cat("Iter: ",i,"-",iter,"\n")
        cat("AUC diff: ",aucip1-auci,"\n")
        cat("Current AUC: ",aucip1,"\n")
        cat("Acceptance prob: ",theta,"\n")
        cat("-----------------\n")
      }
      results[[i]]$AllAUC[iter] <- aucip1 #Save current AUC to AllAUC
      
      # ii - accept/reject decision decision
      if(theta>runif(1)){
        #Accept - fvec i become fvec i+1
        fveci <- fvecip1
        auci <- aucip1
      }
      
      results[[i]]$BestAUC[iter] <- auci #Only save accepted AUC as BestAUC
      results[[i]]$f_Vectors[iter,] <- fveci
      
      #Step 4 - Check convergence every conv_check step after the first conv_check
      if(iter <= conv_check) {
        dynamic_check <- conv_check
      } else {
        dynamic_check <- after_conv_checks
      }
      #Record the AUC every "benchmark_auc" iterations
      if(iter%%benchmark_auc==0){
       # print("Yes!")
        aucchecks <- c(aucchecks,auci)
        #print(aucchecks)
      }
      if(iter%%dynamic_check==0){
        all_auc_diff <- diff(aucchecks)
        auc_diff <- abs(all_auc_diff[length(all_auc_diff)])
        if(auc_diff < epsilon){
          #If converged, force iter to kick out of step 3
          stop <- iter
          iter <- Inf
        }
      }
    }
    
    #Remove trailing NAs from AllAUC, f_Vectors
    if(!is.finite(iter)){
      results[[i]]$BestAUC <- results[[i]]$BestAUC[1:stop]
      results[[i]]$AllAUC <- results[[i]]$AllAUC[1:stop]
      results[[i]]$f_Vectors <- results[[i]]$f_Vectors[1:stop,]
      results[[i]]$Changed <- results[[i]]$Changed[1:stop]
    }
    
  }
  
  #---- Get Feature Importance and Info -----#
  final_fvecs <- lapply(results, function(nn){
    nn$f_Vectors[nrow(nn$f_Vectors), ]
  })
  
  importance <- data.frame(t(do.call("rbind", final_fvecs)))
  colnames(importance) <- paste("Iteration",1:ncol(importance))
  importance_metric <- rowSums(importance)/ncol(importance)
  importance <- cbind(importance_metric, importance)
  importance <- cbind(feature_map, importance) %>% dplyr::select(-Position)
  
  #---- Get Performance Metrics and Info -----#
  aucs <- vector("list", nn)
  for(i in 1:nn){
    aucs[[i]] <- data.frame(Iteration = i, do.call("cbind", results[[i]]))
  }
  aucs <- do.call("rbind", aucs)
  colnames(aucs)[4:(length(feature_map$Feature)+3)] <- feature_map$Feature 
  results_obj <- list(importance, aucs)
  class(results_obj) <- "featSelect"
  return(results_obj)
}

fvecLearning <- function(featurizedMLinput, source_alg_pairs, previous_run = NULL, to_update = NULL, supervised = FALSE){
  nsources <- attr(featurizedMLinput, "n_sources")
  s_names <- names(featurizedMLinput$X)
  sample_cname <- attr(featurizedMLinput, "cnames")$sample_cname
  outcome_cname <- attr(featurizedMLinput, "cnames")$outcome_cname
  parts <- attr(featurizedMLinput, "partition_info")
  
  #----- initialize if needed -----#
  if(is.null(previous_run)){
    #If no previous run was provided, do them all
    results <- vector("list",nsources)
    names(results) <- s_names
    to_update <- rep(TRUE,nsources)
  }else{
    results <- previous_run
  }
  #--- Loop through sources and learn ----#
  # if (nsources == 1) {
  #   i <- 1
  #   if(all(colnames(featurizedMLinput$X) %in% sample_cname)){
  #     results[[i]] <- lapply(parts, function(x) data.frame(PredicetedProbs.0 = rep(1, length(x$test)),PredicetedProbs.1 = rep(1, length(x$test)),
  #                                                          PredictedLabel = rep(NA, length(x$test)), Truth = featurizedMLinput$Y[x$test, outcome_cname]))
  #   } else {
  #     results[[i]] <- attr(MLwrapper(data_object = featurizedMLinput, methods = source_alg_pairs[[i]]), "ML_results")[[source_alg_pairs[[i]]]]
  #   }
  # } else {
    for(i in 1:nsources){
      if(to_update[i]){
        if(all(colnames(featurizedMLinput$X[[i]]) %in% sample_cname) | (ncol(featurizedMLinput$X[[i]]) <=1 )){
          results[[i]] <- lapply(parts, function(x) data.frame(PredicetedProbs.0 = rep(1, length(x$test)),PredicetedProbs.1 = rep(1, length(x$test)),
                                                               PredictedLabel = rep(NA, length(x$test)), Truth = featurizedMLinput$Y[x$test, outcome_cname]))
        }else{
          results[[i]] <- MLwrapper(data_object = featurizedMLinput, methods = source_alg_pairs[[i]], single_source = unname(s_names[i]))[[source_alg_pairs[[i]]]]
        }
      }
    }

  return(results)
}

