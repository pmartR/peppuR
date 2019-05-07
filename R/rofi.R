#' Repeated Optimized Feature Integration
#' @param MLinput
#' @param source_alg_pairs
#' @param nn
#' @export
rofi <- function(MLinput, source_alg_pairs, nn = 1, f_prob=0.1 , nu=1/100, maxIter=2*sum(attr(MLinput, "data_info")$number_of_features), conv_check=(sum(attr(MLinput, "data_info")$number_of_features)+1), epsilon = 0.01,verbose=FALSE){
  #Xdata: n-by-p data.frame of feature vectors
  #Ydata: n-vector of 0/1 class labels
  #partitions: the partitions for test/training
  #source_alg_pairs: list of sources and the methods to use with each
  #nn: number of times to do outer loop
  #nu: scale value in the difference in AUC values
  #maxIter: maximum number of iterations allowed within one "nn" iteration
  #conv_check: how often should we check for convergence in step 3?
  #epsilon: AUC convergence threshold
  
  #--- Index features ----#
  sample_cname <- attr(MLinput, "cnames")$sample_cname
  n_sources <- attr(MLinput, "n_sources")
  #p <- sum(attr(MLinput, "data_info")$number_of_features) this needs to be fixed use this bandaid for now
  if (n_sources > 1){
    p <- sum(unlist(lapply(MLinput$X, function(d_source) sum(!colnames(d_source) %in% sample_cname))))
  } else {
    p <- sum(!colnames(MLinput$X) %in% sample_cname)
  }
  
  feature_inds <- seq(from = 1, to = p, by = 1)
  
  # if (length(source_alg_pairs) > 1){
  #   names(feature_inds) <- unlist(lapply(MLinput$X, function(d_source){
  #     feats <- colnames(d_source)[which(!colnames(d_source) %in% sample_cname)] 
  #   })
  # } else {
  #   samp_ind <- which(names(MLinput$X) %in% sample_cname)
  #   names(feature_inds) <- names(MLinput$X)
  # }
  # Not my favorite way to index features, but works for now
  feature_list <- vector("list", n_sources)
  source_list <- vector("list", n_sources)
  
  for (d_source in 1:n_sources){
    if (n_sources == 1) {
      feature_list[[d_source]] <- colnames(MLinput$X)[which(!colnames(MLinput$X) %in% sample_cname)]
      source_list[[d_source]] <- rep("single source", length(feature_list[[d_source]]))
    } else {
      feature_list[[d_source]] <- colnames(MLinput$X[[d_source]])[which(!colnames(MLinput$X[[d_source]]) %in% sample_cname)]
      source_list[[d_source]] <- rep(names(MLinput$X)[d_source], length(feature_list[[d_source]]))
    }
  }
  
  #p <- ncol(Xdata) #number of features
  sources <- names(source_alg_pairs) #Named data sources
  if (is.null(sources)) {
    stop("source_alg_pairs must be a named vector. The vector is a list of algorithms and the names are the corresponding data source")
  }
  feature_map <- data.frame(Feature = unlist(feature_list), Position = feature_inds, Source = unlist(source_list), stringsAsFactors = FALSE)
  
  # How often the AUC is recorded for convergence purposes
  benchmark_auc <- p
  
  # Results list
  results <- vector("list", nn)
  
  #Create list of column numbers for each source
  # source_cols <- source_alg_pairs
  # for(jk in 1:nsources){
  #   source_cols[[jk]] <- grep(names(source_alg_pairs)[jk],colnames(Xdata))
  # }
  
  for(i in 1:nn){
    aucchecks <- NULL
    
    
    #cat("You're out of the loop!\n")
    #Create elements in results list
    results[[i]] <- vector("list",4)
    names(results[[i]]) <- c("AllAUC","BestAUC","f_Vectors","Changed")
    results[[i]]$BestAUC <- rep(NA,maxIter)
    results[[i]]$AllAUC <- rep(NA,maxIter)
    results[[i]]$f_Vectors <- matrix(NA,maxIter,p)
    results[[i]]$Changed <- rep(NA,maxIter)
    
    #1. Select a ranodm number of features to include in model  
    #Select random 100*f_prob% of the features
    fveci <- rbinom(p,1,f_prob)
    
    #2. Do cross validation using randomly chosen features
    included_features <- feature_map$Feature[which(fveci==1)]
    subsetted_data <- MLinput
    if (n_sources == 1) {
      subsetted_data$X <- subsetted_data$X[ ,which(colnames(subsetted_data$X) %in% c(included_features, sample_cname)), drop=FALSE]
    } else {
      subsetted_data$X <- lapply(subsetted_data$X, function(d_source){
        d_source <- d_source[ ,which(colnames(d_source) %in% c(included_features, sample_cname)), drop=FALSE] # TODO:Features could be named the same across sources, account for this later
        return(d_source)
      })
    }

    
    #Xdata_i <- Xdata[,which(fveci==1)] #Only keep the randomly selected features (columns of Xdata)
    #----- Learn on all sources and retrieve initial AUC------#
    all_res <- fvecLearning(subsetted_data, source_alg_pairs) # TODO: Test edge cases of single/no features
    if (n_sources == 1) {
      all_res_probs <- all_res[[1]]
    } else {
      all_res_probs <- naiveIntegration(all_res)
    }
    all_res_roc <- AUC::roc(predictions = all_res_probs$PredictedProbs.1, labels = as.factor(all_res_probs$Truth))
    all_AUC <- AUC::auc(all_res_roc)
    
    iter <- 1
    results[[i]]$BestAUC[iter] <- results[[i]]$AllAUC[iter] <- auci <- all_AUC
    results[[i]]$f_Vectors[iter,] <- fveci
    #results[[i]]$Changed[iter] <- 0
    
    #3. Repeat until AUC_diff is below some threshold
    auc_diff <- 1
    
    #Create the vector that defines the order in which each features is turned on/off
    #don't really love creating a vector of length maxIter but it's easy
    if((maxIter-1)<=p){
      flip_order <- sample(p,maxIter-1,replace = FALSE)
    }else{
      flip_order <- rep(NA,maxIter-1)
      for(lazy in 1:round((maxIter-1)/p,0)){
        flip_order[(1:p)+p*(lazy-1)] <- sample(p)
      }
      flip_order[-c(1:(p*lazy))] <- sample(p,(maxIter-1)%%p,replace=FALSE)
      
    }
    flip_order <- c(0,flip_order)
    results[[i]]$Changed <- flip_order
    
    while(iter<maxIter){
      #cat("You're in the loop!\n")
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
      if (n_sources == 1) {
        subsetted_data$X <- subsetted_data$X[ ,which(colnames(subsetted_data$X) %in% c(included_features, sample_cname)), drop=FALSE]
      } else {
        subsetted_data$X <- lapply(subsetted_data$X, function(d_source){
          d_source <- d_source[ ,which(colnames(d_source) %in% c(included_features, sample_cname)), drop=FALSE] # TODO:Features could be named the same across sources, account for this later
          return(d_source)
        })
      }
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
      #if(verbose){
      #  cat("Iter: ",i,"-",iter,"\n")
      #  cat("AUC diff: ",aucip1-auci,"\n")
      #  cat("Current AUC: ",aucip1,"\n")
      #  cat("Acceptance prob: ",theta,"\n")
      #  cat("-----------------\n")
      #}
      results[[i]]$AllAUC[iter] <- aucip1 #Save current AUC to AllAUC
      
      # ii - accept/reject decision decision
      if(theta>runif(1)){
        #Accept - fvec i become fvec i+1
        fveci <- fvecip1
        auci <- aucip1
      }
      
      results[[i]]$BestAUC[iter] <- auci #Only save accepted AUC as BestAUC
      results[[i]]$f_Vectors[iter,] <- fveci
      
      #Step 4 - Check convergence every 100th step after the first conv_check
      if(iter <= conv_check) {
        dynamic_check <- conv_check
      } else {
        dynamic_check <- p
      }
      #Record the AUC every "benchmark_auc" iterations
      if(iter%%benchmark_auc==0){
        #print("Yes!")
        aucchecks <- c(aucchecks,auci)
        #print(aucchecks)
      }
      
      if(iter%%dynamic_check==0){
        cat("Checking Convergence")
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
  
  return(results)
}

fvecLearning <- function(featurizedMLinput, source_alg_pairs, previous_run = NULL, to_update = NULL, supervised = FALSE){
  nsources <- attr(featurizedMLinput, "n_sources")
  if (nsources == 1){
    s_names <- "Single Source"
  } else {
    s_names <- names(featurizedMLinput$X)
  }
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
  if (nsources == 1) {
    i <- 1
    if(all(colnames(featurizedMLinput$X) %in% sample_cname)){
      results[[i]] <- lapply(parts, function(x) data.frame(PredicetedProbs.0 = rep(1, length(x$test)),PredicetedProbs.1 = rep(1, length(x$test)),
                                                           PredictedLabel = rep(NA, length(x$test)), Truth = featurizedMLinput$Y[x$test, outcome_cname]))
    } else {
      results[[i]] <- attr(MLwrapper(data_object = featurizedMLinput, methods = source_alg_pairs[[i]]), "ML_results")[[source_alg_pairs[[i]]]]
    }
  } else {
    for(i in 1:nsources){
      if(to_update[i]){
        if(all(colnames(featurizedMLinput$X[[i]]) %in% sample_cname) | (ncol(featurizedMLinput$X[[i]]) <=1 )){
          results[[i]] <- lapply(parts, function(x) data.frame(PredicetedProbs.0 = rep(1, length(x$test)),PredicetedProbs.1 = rep(1, length(x$test)),
                                                               PredictedLabel = rep(NA, length(x$test)), Truth = featurizedMLinput$Y[x$test, outcome_cname]))
        }else{
          results[[i]] <- attr(MLwrapper(data_object = featurizedMLinput, methods = source_alg_pairs[[i]], single_source = unname(s_names[i])), "ML_results")[[source_alg_pairs[[i]]]]
        }
      }
    }
  }

  return(results)
}

