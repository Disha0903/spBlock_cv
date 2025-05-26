##This script contains all fucntions which are used in script code.R

## If you would like to save the best models you can uncomment saveRDS() function.

##GBM
################################################################
###Spatial
sp_gbm_retrain <- function(block_sizes, model_names, hyperparams, mydata, pa_data, cropped_env, mydata_past) {
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    # Initialize variables to store the best model and its AUC
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    
    # Modify your code to use the current block_size and model_name
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, 
                     size = block_size, 
                     selection = "random", 
                     iteration = 50, 
                     biomod2 = TRUE) 
    
    folds <- sb$folds_list
    
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      
      for (fold in 1:5) {
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        
        gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                         data = train, n.trees = hyperparams$n.trees[j],
                         interaction.depth = hyperparams$interaction.depth[j],
                         shrinkage = hyperparams$shrinkage[j],
                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                         verbose = FALSE)
        
        # Make predictions on the testing set
        prob_predictions <- predict(gbm_model, newdata = test, type = "response")
        
        # Calculate ROC AUC
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_AUC_valid) {
          best_AUC_valid <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams[j, ]
          best_model <- gbm_model
        }
      }
      
      # Store results for the current hyperparameter combination
      results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                           interaction.depth = hyperparams$interaction.depth[j],
                                           shrinkage = hyperparams$shrinkage[j],
                                           n.minobsinnode = hyperparams$n.minobsinnode[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                       data = mydata, n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Check if current model has a higher AUC on the validation set
      if (ROC_AUC_valid > best_AUC_valid) {
        best_AUC_valid <- ROC_AUC_valid
        best_model_valid <- best_model
      }
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                     interaction.depth = hyperparams$interaction.depth[j],
                                                     shrinkage = hyperparams$shrinkage[j],
                                                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }
    
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/", model_name, "_best_model_", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid.rds"))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}






sp_gbm_lastfold <- function(block_sizes, model_names, hyperparams, mydata, pa_data, cropped_env, mydata_past) {
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    # Initialize variables to store the best model and its AUC
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    # Modify your code to use the current block_size and model_name
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, 
                     size = block_size, 
                     selection = "random", 
                     iteration = 50, 
                     biomod2 = TRUE) 
    
    folds <- sb$folds_list
    
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams)) {
      print(paste0("Iteration: ", j))
      
      # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
      fold_ROC_AUC <- c()
      
      # Loop through all folds
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        
        gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                         data = train, n.trees = hyperparams$n.trees[j],
                         interaction.depth = hyperparams$interaction.depth[j],
                         shrinkage = hyperparams$shrinkage[j],
                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                         verbose = FALSE)
        
        # Make predictions on the testing set
        prob_predictions <- predict(gbm_model, newdata = test, type = "response")
        
        # Calculate ROC AUC
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        # Append ROC AUC to fold_ROC_AUC vector
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_AUC_valid) {
          best_AUC_valid <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams[j, ]
          best_model <- gbm_model
        }
        
        
        # Predict on the validation set and calculate ROC AUC
        valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        
        if (ROC_AUC_valid > best_AUC_valid) {
          best_AUC_valid <- ROC_AUC_valid
          best_model_valid <- best_model
        }
        
      }
      
      # Store results for the current hyperparameter combination
      results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                           interaction.depth = hyperparams$interaction.depth[j],
                                           shrinkage = hyperparams$shrinkage[j],
                                           n.minobsinnode = hyperparams$n.minobsinnode[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
    
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                     interaction.depth = hyperparams$interaction.depth[j],
                                                     shrinkage = hyperparams$shrinkage[j],
                                                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }
    
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/", model_name, "_best_model_lf", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid_lf.rds"))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}



#Random
random_gbm_retrain <- function(hyperparams, mydata, pa_data, cropped_env, mydata_past){
  
  results <- data.frame()
  results_past <- data.frame()
  
  # Generate random folds for 5-fold cross-validation
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = 5, list = TRUE)
  
  # Initialize variables to store the best model and its AUC
  best_model <- NULL
  best_AUC_valid <- 0
  best_model_valid <- NULL
  best_mean_AUC <- 0
  best_hyperparams <- NULL
  
  # Loop through all hyperparameter combinations
  for (j in 1:nrow(hyperparams)) {
    print(paste0("Iteration: ", j))
    
    fold_ROC_AUC <- c()
    
    # Loop through all folds
    for (fold in 1:5) {
      # Get training and testing indices for the current fold
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      # Split data into training and testing sets
      train <- mydata[trainSet, ]
      test <- mydata[testSet, ]
      valid <- mydata_past
      
      # Setup gbm model
      gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                       data = train, n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      # Make predictions on the testing set
      prob_predictions <- predict(gbm_model, newdata = test, type = "response")
      
      # Calculate ROC AUC
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      
      # Append ROC AUC to fold_ROC_AUC vector
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (mean(fold_ROC_AUC) > best_mean_AUC) {
        best_mean_AUC <- mean(fold_ROC_AUC)
        best_hyperparams <- hyperparams[j, ]
        best_model <- gbm_model
      }
    }
    
    results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                         interaction.depth = hyperparams$interaction.depth[j],
                                         shrinkage = hyperparams$shrinkage[j],
                                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                                         mean_ROC_AUC = mean(fold_ROC_AUC),
                                         fold_ROC_AUC = toString(fold_ROC_AUC)))
    
    ##add gbm model to train on whole data for validation
    gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                     data = mydata, n.trees = hyperparams$n.trees[j],
                     interaction.depth = hyperparams$interaction.depth[j],
                     shrinkage = hyperparams$shrinkage[j],
                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                     verbose = FALSE)
    
    # Predict on the validation set and calculate ROC AUC
    valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
    
    # Check if current model has a higher AUC on the validation set
    if (ROC_AUC_valid > best_AUC_valid) {
      best_AUC_valid <- ROC_AUC_valid
      best_model_valid <- best_model
    }
    
    results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                   interaction.depth = hyperparams$interaction.depth[j],
                                                   shrinkage = hyperparams$shrinkage[j],
                                                   n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                   ROC_AUC_valid = ROC_AUC_valid))
    
    # Save the best model for the current hyperparameter combination
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/gbm_random_best_model.rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/gbm_random_best_model_valid.rds"))
    
    write.csv(results, "/app/proj_cv/results/gbm_random_results.csv")
    write.csv(results_past, "/app/proj_cv/results/gbm_random_results_past.csv")
  }
}




random_gbm_lastfold <- function(hyperparams, mydata, pa_data, cropped_env, mydata_past){
  
  results <- data.frame()
  results_past <- data.frame()
  
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = 5, list = TRUE)
  
  # Initialize variables to store the best model and its AUC
  best_model <- NULL
  best_AUC_valid <- 0
  best_model_valid <- NULL
  best_mean_AUC <- 0
  best_hyperparams <- NULL
  
  # Loop through all hyperparameter combinations
  for (j in 1:nrow(hyperparams)) {
    print(paste0("Iteration: ", j))
    
    fold_ROC_AUC <- c()
    
    # Loop through all folds
    for (fold in 1:5) {
      # Get training and testing indices for the current fold
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      # Split data into training and testing sets
      train <- mydata[trainSet, ]
      test <- mydata[testSet, ]
      valid <- mydata_past
      
      # Setup gbm model
      gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                       data = train, n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      # Make predictions on the testing set
      prob_predictions <- predict(gbm_model, newdata = test, type = "response")
      
      # Calculate ROC AUC
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      
      # Append ROC AUC to fold_ROC_AUC vector
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (mean(fold_ROC_AUC) > best_mean_AUC) {
        best_mean_AUC <- mean(fold_ROC_AUC)
        best_hyperparams <- hyperparams[j, ]
        best_model <- gbm_model
      }
      
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Check if current model has a higher AUC on the validation set
      if (ROC_AUC_valid > best_AUC_valid) {
        best_AUC_valid <- ROC_AUC_valid
        best_model_valid <- best_model
      }
    }
    
    results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                         interaction.depth = hyperparams$interaction.depth[j],
                                         shrinkage = hyperparams$shrinkage[j],
                                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                                         mean_ROC_AUC = mean(fold_ROC_AUC),
                                         fold_ROC_AUC = toString(fold_ROC_AUC)))
    
    results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                   interaction.depth = hyperparams$interaction.depth[j],
                                                   shrinkage = hyperparams$shrinkage[j],
                                                   n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                   ROC_AUC_valid = ROC_AUC_valid))
    
    # Save the best model for the current hyperparameter combination
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/gbm_random_best_model_lf.rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/gbm_random_best_model_valid_lf.rds"))
    
    write.csv(results, "/app/proj_cv/results/gbm_random_results_lf.csv")
    write.csv(results_past, "/app/proj_cv/results/gbm_random_results_past_lf.csv")
  }
}




##Temporal
temp_gbm_retrain <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams, mydata, mydata_past) {
  results_list <- list() 
  results_past_list <- list()
  
  # Loop through cropping datasets
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      # Modify your code to use the current block_size and model_name
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5, # number of folds
                       size = block_size, # size of the blocks in meters
                       selection = "random", # random blocks-to-fold
                       iteration = 50, # find evenly dispersed folds
                       biomod2 = TRUE) # also create folds for biomod2
      
      folds <- sb$folds_list
      
      # Loop through all hyperparameter combinations
      for (j in 1:nrow(hyperparams)) {
        print(paste0("Iteration: ", j))
        
        # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
        fold_ROC_AUC <- c()
        
        # Loop through all folds
        for (fold in 1:5) {
          # Get training and testing indices for the current fold
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          # Split data into training and testing sets
          train <- interval_data[trainSet, ]  
          test <- interval_data[testSet, ]
          
          # Setup gbm model
          gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                           data = train, n.trees = hyperparams$n.trees[j],
                           interaction.depth = hyperparams$interaction.depth[j],
                           shrinkage = hyperparams$shrinkage[j],
                           n.minobsinnode = hyperparams$n.minobsinnode[j],
                           verbose = FALSE)
          
          # Make predictions on the testing set
          prob_predictions <- predict(gbm_model, newdata = test, type = "response")
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
        }
        
      
        results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                             interaction.depth = hyperparams$interaction.depth[j],
                                             shrinkage = hyperparams$shrinkage[j],
                                             n.minobsinnode = hyperparams$n.minobsinnode[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                         data = mydata, n.trees = hyperparams$n.trees[j],
                         interaction.depth = hyperparams$interaction.depth[j],
                         shrinkage = hyperparams$shrinkage[j],
                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                         verbose = FALSE)
        
        valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                       interaction.depth = hyperparams$interaction.depth[j],
                                                       shrinkage = hyperparams$shrinkage[j],
                                                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    
    results <- aggregate(. ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    ##for past
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/", model_name, "_best_model_", j, ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid.rds"))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}





temp_gbm_lastfold <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams, mydata_past) {
  results_list <- list()
  results_past_list <- list()
  
  # Loop through cropping datasets
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      # Modify your code to use the current block_size and model_name
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5, # number of folds
                       size = block_size, # size of the blocks in meters
                       selection = "random", # random blocks-to-fold
                       iteration = 50, # find evenly dispersed folds
                       biomod2 = TRUE) # also create folds for biomod2
      
      folds <- sb$folds_list
      
      # Loop through all hyperparameter combinations
      for (j in 1:nrow(hyperparams)) {
        print(paste0("Iteration: ", j))
        
        # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
        fold_ROC_AUC <- c()
        
        # Loop through all folds
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          # Split data into training and testing sets
          train <- interval_data[trainSet, ]  # Use interval_data for the current interval
          test <- interval_data[testSet, ]
          
          # Setup gbm model
          gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                           data = train, n.trees = hyperparams$n.trees[j],
                           interaction.depth = hyperparams$interaction.depth[j],
                           shrinkage = hyperparams$shrinkage[j],
                           n.minobsinnode = hyperparams$n.minobsinnode[j],
                           verbose = FALSE)
          
          # Make predictions on the testing set
          prob_predictions <- predict(gbm_model, newdata = test, type = "response")
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          # Predict on the validation set and calculate ROC AUC
          valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
          ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
          
          
        }
        
        # Store results for the current hyperparameter combination
        results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                             interaction.depth = hyperparams$interaction.depth[j],
                                             shrinkage = hyperparams$shrinkage[j],
                                             n.minobsinnode = hyperparams$n.minobsinnode[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        
        results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                       interaction.depth = hyperparams$interaction.depth[j],
                                                       shrinkage = hyperparams$shrinkage[j],
                                                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    # Calculate the mean ROC AUC for each hyperparameter set
    results <- aggregate(. ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    ##for past
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/", model_name, "_best_model_", j, ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid.rds"))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}






##Random Forest
###########################################################################################################

#spatial
sp_rf_retrain <- function(block_sizes, model_names, hyperparams_rf, mydata, pa_data, cropped_env, mydata_past){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, # number of folds
                     size = block_size, # size of the blocks in metres
                     selection = "random", # random blocks-to-fold
                     iteration = 50, # find evenly dispersed folds
                     biomod2 = TRUE) # also create folds for biomod2
    
    folds <- sb$folds_list
    
    # Initialize variables to store the best model and its AUC
    best_model <- NULL
    best_AUC_valid <- 0
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_rf)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        mydata$occurrenceStatus <- factor(mydata$occurrenceStatus)
        train$occurrenceStatus <- factor(train$occurrenceStatus)
        test$occurrenceStatus <- factor(test$occurrenceStatus)
        valid$occurrenceStatus <- factor(valid$occurrenceStatus)
        
        # Setup Random Forest model
        
        rf_model <- randomForest(occurrenceStatus ~ .,
                                 data = train,
                                 ntree = hyperparams_rf$n.trees[j],
                                 max_depth = hyperparams_rf$max_depth[j],
                                 min_samples_split = hyperparams_rf$min_samples_split[j])
        
        # Make predictions on the testing set
        prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        # Append ROC AUC to fold_ROC_AUC vector
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_mean_AUC) {
          best_mean_AUC <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams_rf[j,]
          best_model <- rf_model
        }
      }
      results <- rbind(results, data.frame(n.trees = hyperparams_rf$n.trees[j],  
                                           max_depth = hyperparams_rf$max_depth[j],
                                           min_samples_split = hyperparams_rf$min_samples_split[j], 
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      # Train a Random Forest model on the full dataset
      rf_model <- randomForest(occurrenceStatus ~ .,
                               data = mydata,
                               ntree = hyperparams_rf$n.trees[j],
                               max_depth = hyperparams_rf$max_depth[j],
                               min_samples_split = hyperparams_rf$min_samples_split[j])
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[,2]
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Check if the current model has a higher AUC on the validation set
      if (ROC_AUC_valid > best_AUC_valid) {
        best_AUC_valid <- ROC_AUC_valid
        best_model_valid <- rf_model
      }
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(n.trees = hyperparams_rf$n.trees[j],  
                                                     max_depth = hyperparams_rf$max_depth[j],
                                                     min_samples_split = hyperparams_rf$min_samples_split[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
      
    }
    
    # Save the best model for the current hyperparameter combination
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/",model_name, "_best_model_", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/",model_name, '_best_model_valid', '.rds'))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}






sp_rf_lastfold <- function(block_sizes, model_names, hyperparams_rf, mydata, pa_data, cropped_env, mydata_past){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, # number of folds
                     size = block_size, # size of the blocks in metres
                     selection = "random", # random blocks-to-fold
                     iteration = 50, # find evenly dispersed folds
                     biomod2 = TRUE) # also create folds for biomod2
    
    folds <- sb$folds_list
    
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_rf)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        mydata$occurrenceStatus <- factor(mydata$occurrenceStatus)
        train$occurrenceStatus <- factor(train$occurrenceStatus)
        test$occurrenceStatus <- factor(test$occurrenceStatus)
        valid$occurrenceStatus <- factor(valid$occurrenceStatus)
        
        # Setup Random Forest model
        
        rf_model <- randomForest(occurrenceStatus ~ .,
                                 data = train,
                                 ntree = hyperparams_rf$n.trees[j],
                                 max_depth = hyperparams_rf$max_depth[j],
                                 min_samples_split = hyperparams_rf$min_samples_split[j])
        
        # Make predictions on the testing set
        prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
        
        # Calculate ROC AUC
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        # Append ROC AUC to fold_ROC_AUC vector
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        # Predict on the validation set and calculate ROC AUC
        valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[,2]
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        
      }
      results <- rbind(results, data.frame(n.trees = hyperparams_rf$n.trees[j],  
                                           max_depth = hyperparams_rf$max_depth[j],
                                           min_samples_split = hyperparams_rf$min_samples_split[j], 
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(n.trees = hyperparams_rf$n.trees[j],  
                                                     max_depth = hyperparams_rf$max_depth[j],
                                                     min_samples_split = hyperparams_rf$min_samples_split[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
      
    }
    
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/",model_name, "_best_model_lf", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/",model_name, '_best_model_valid_lf', '.rds'))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}




##temporal
temp_rf_retrain <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_rf, mydata, mydata_past) {
  
  results_list <- list()
  results_past_list <- list()
  
  # Loop through cropping datasets
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      # Modify your code to use the current block_size and model_name
      #set.seed(1) #for 600km
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5, # number of folds
                       size = block_size, # size of the blocks in meters
                       selection = "random", # random blocks-to-fold
                       iteration = 50, # find evenly dispersed folds
                       biomod2 = TRUE) # also create folds for biomod2
      
      folds <- sb$folds_list
      
      # Loop through all hyperparameter combinations
      for (j in 1:nrow(hyperparams_rf)) {
        print(paste0("Iteration: ", j))
        
        # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
        fold_ROC_AUC <- c()
        
        # Loop through all folds
        for (fold in 1:5) {
          # Get training and testing indices for the current fold
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          # Split data into training and testing sets
          train <- interval_data[trainSet, ]  # Use interval_data for the current interval
          test <- interval_data[testSet, ]
          
          train$occurrenceStatus <- factor(train$occurrenceStatus)
          test$occurrenceStatus <- factor(test$occurrenceStatus)
          mydata$occurrenceStatus <- factor(mydata$occurrenceStatus)
          valid$occurrenceStatus <- factor(valid$occurrenceStatus)
          
          # Setup gbm model
          rf_model <- randomForest(occurrenceStatus ~ .,
                                   data = train,
                                   ntree = hyperparams_rf$n.trees[j],
                                   max_depth = hyperparams_rf$max_depth[j],
                                   min_samples_split = hyperparams_rf$min_samples_split[j])
          
          # Make predictions on the testing set
          prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (mean(fold_ROC_AUC) > best_mean_AUC) {
            best_mean_AUC <- mean(fold_ROC_AUC)
            best_hyperparams <- hyperparams_rf[j, ]
            best_model <- rf_model
          }
        }
        
        # Store results for the current hyperparameter combination
        results <- rbind(results, data.frame(n.trees = hyperparams_rf$n.trees[j],
                                             max_depth = hyperparams_rf$max_depth[j],
                                             min_samples_split = hyperparams_rf$min_samples_split[j], # Access the maxnodes value
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        rf_model <- randomForest(occurrenceStatus ~ .,
                                 data = mydata,
                                 ntree = hyperparams_rf$n.trees[j],
                                 max_depth = hyperparams_rf$max_depth[j],
                                 min_samples_split = hyperparams_rf$min_samples_split[j])
        
        
        # Predict on the validation set and calculate ROC AUC
        valid_predictions <- predict(rf_model, newdata = mydata_past, type = "prob")[, 2]
        ROC_AUC_valid <- pROC::auc(mydata_past$occurrenceStatus, valid_predictions)
        
        results_past <- rbind(results_past, data.frame(n.trees = hyperparams_rf$n.trees[j],
                                                       max_depth = hyperparams_rf$max_depth[j],
                                                       min_samples_split = hyperparams_rf$min_samples_split[j], # Access the maxnodes value
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    results <- aggregate(. ~ n.trees +max_depth + min_samples_split,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ n.trees +max_depth + min_samples_split,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}




temp_rf_lastfold <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_rf, mydata_past) {
  
  results_list <- list()  # Initialize a list to store results
  results_past_list <- list()
  
  # Loop through cropping datasets
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0 
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      # Modify your code to use the current block_size and model_name
      #set.seed(1) #for 600km
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5, # number of folds
                       size = block_size, # size of the blocks in meters
                       selection = "random", # random blocks-to-fold
                       iteration = 50, # find evenly dispersed folds
                       biomod2 = TRUE) # also create folds for biomod2
      
      folds <- sb$folds_list
      
      # Loop through all hyperparameter combinations
      for (j in 1:nrow(hyperparams_rf)) {
        print(paste0("Iteration: ", j))
        
        # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
        fold_ROC_AUC <- c()
        
        # Loop through all folds
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          # Split data into training and testing sets
          train <- interval_data[trainSet, ]  # Use interval_data for the current interval
          test <- interval_data[testSet, ]
          
          train$occurrenceStatus <- factor(train$occurrenceStatus)
          test$occurrenceStatus <- factor(test$occurrenceStatus)
    
          valid$occurrenceStatus <- factor(valid$occurrenceStatus)
          
          # Setup gbm model
          rf_model <- randomForest(occurrenceStatus ~ .,
                                   data = train,
                                   ntree = hyperparams_rf$n.trees[j],
                                   max_depth = hyperparams_rf$max_depth[j],
                                   min_samples_split = hyperparams_rf$min_samples_split[j])
          
          # Make predictions on the testing set
          prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          # Predict on the validation set and calculate ROC AUC
          valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[,2]
          ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        }
        
        # Store results for the current hyperparameter combination
        results <- rbind(results, data.frame(n.trees = hyperparams_rf$n.trees[j],
                                             max_depth = hyperparams_rf$max_depth[j],
                                             min_samples_split = hyperparams_rf$min_samples_split[j], # Access the maxnodes value
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        results_past <- rbind(results_past, data.frame(n.trees = hyperparams_rf$n.trees[j],
                                                       max_depth = hyperparams_rf$max_depth[j],
                                                       min_samples_split = hyperparams_rf$min_samples_split[j], # Access the maxnodes value
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    
    # Calculate the mean ROC AUC for each hyperparameter set
    results <- aggregate(. ~ n.trees +max_depth + min_samples_split,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ n.trees +max_depth + min_samples_split,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}



##XGBOOST
#######################################################################################


#spatial
sp_xgb_retrain <- function(block_sizes, model_names, hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    # Modify your code to use the current block_size and model_name
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, # number of folds
                     size = block_size, # size of the blocks in meters
                     selection = "random", # random blocks-to-fold
                     iteration = 50, # find evenly dispersed folds
                     biomod2 = TRUE) # also create folds for biomod2
    
    folds <- sb$folds_list
    
    # Initialize variables to store the best model and its AUC
    best_model <- NULL
    best_AUC_valid <- 0
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_xgb)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      # Loop through all folds
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        
        # Setup XGBoost model
        xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                             label = train$occurrenceStatus,
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc")
        
        # Make predictions on the testing set
        prob_predictions <- predict(xgb_model, 
                                    newdata = as.matrix(test[, -ncol(test)]), 
                                    type='response')
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        # Append ROC AUC to fold_ROC_AUC vector
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_mean_AUC) {
          best_mean_AUC <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams_xgb[j, ]
          best_model <- xgb_model
        }
      }
      
      # Store results for the current hyperparameter combination
      results <- rbind(results, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                           max_depth = hyperparams_xgb$max_depth[j],
                                           eta = hyperparams_xgb$eta[j],
                                           subsample = hyperparams_xgb$subsample[j],
                                           min_child_weight = hyperparams_xgb$min_child_weight[j],
                                           gamma = hyperparams_xgb$gamma[j],
                                           colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      # Setup XGBoost model on the entire dataset
      xgb_model <- xgboost(data = as.matrix(mydata[, -ncol(mydata)]), # Convert to matrix
                           label = as.numeric(mydata$occurrenceStatus),
                           nrounds = hyperparams_xgb$nrounds[j],
                           max_depth = hyperparams_xgb$max_depth[j],
                           eta = hyperparams_xgb$eta[j],
                           subsample = hyperparams_xgb$subsample[j],
                           min_child_weight = hyperparams_xgb$min_child_weight[j],
                           gamma = hyperparams_xgb$gamma[j],
                           colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                           objective = "binary:logistic",
                           eval_metric = "auc")
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]), type='response')
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Check if current model has a higher AUC on the validation set
      if (ROC_AUC_valid > best_AUC_valid) {
        best_AUC_valid <- ROC_AUC_valid
        best_model_valid <- best_model
      }
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                     max_depth = hyperparams_xgb$max_depth[j],
                                                     eta = hyperparams_xgb$eta[j],
                                                     subsample = hyperparams_xgb$subsample[j],
                                                     min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                     gamma = hyperparams_xgb$gamma[j],
                                                     colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }
    
    # Save the best model for the current hyperparameter combination
    #saveRDS(best_model,paste0("/app/proj_cv/results/models/", model_name, "_best_model_", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid.rds"))
    
    # Save the results and results_past
    write.csv(results, file = paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, file = paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}





sp_xgb_lastfold <- function(block_sizes, model_names, hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past) {
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, 
                     size = block_size, 
                     selection = "random", 
                     iteration = 50, 
                     biomod2 = TRUE) 
    
    folds <- sb$folds_list
    
    # Initialize variables to store the best model and its AUC
    best_model <- NULL
    best_AUC_valid <- 0
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_xgb)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      # Loop through all folds
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        # Setup XGBoost model
        xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                             label = train$occurrenceStatus,
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc")
        
        # Make predictions on the testing set
        prob_predictions <- predict(xgb_model, 
                                    newdata = as.matrix(test[, -ncol(test)]), 
                                    type='response')
        
        # Calculate ROC AUC
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        # Append ROC AUC to fold_ROC_AUC vector
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        # Predict on the validation set and calculate ROC AUC
        valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]), type='response')
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        
      }
      
      # Store results for the current hyperparameter combination
      results <- rbind(results, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                           max_depth = hyperparams_xgb$max_depth[j],
                                           eta = hyperparams_xgb$eta[j],
                                           subsample = hyperparams_xgb$subsample[j],
                                           min_child_weight = hyperparams_xgb$min_child_weight[j],
                                           gamma = hyperparams_xgb$gamma[j],
                                           colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                     max_depth = hyperparams_xgb$max_depth[j],
                                                     eta = hyperparams_xgb$eta[j],
                                                     subsample = hyperparams_xgb$subsample[j],
                                                     min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                     gamma = hyperparams_xgb$gamma[j],
                                                     colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }


    #saveRDS(best_model,paste0("/app/proj_cv/results/models/", model_name, "_best_model_lf", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid_lf.rds"))
    
    # Save the results and results_past
    write.csv(results, file = paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, file = paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}




#temporal
temp_xgb_retrain <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_xgb, mydata, mydata_past) {
  results_list <- list()  
  results_past_list <- list()
  
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    valid <- mydata_past
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      set.seed(21)  # Set the seed
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_xgb)) {
        print(paste0("Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                               label = train$occurrenceStatus,
                               nrounds = hyperparams_xgb$nrounds[j],
                               max_depth = hyperparams_xgb$max_depth[j],
                               eta = hyperparams_xgb$eta[j],
                               subsample = hyperparams_xgb$subsample[j],
                               min_child_weight = hyperparams_xgb$min_child_weight[j],
                               gamma = hyperparams_xgb$gamma[j],
                               colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                               objective = "binary:logistic",
                               eval_metric = "auc")
          
          prob_predictions <- predict(xgb_model, as.matrix(test[, -ncol(test)]))
          
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (mean(fold_ROC_AUC) > best_mean_AUC) {
            best_mean_AUC <- mean(fold_ROC_AUC)
            best_hyperparams <- hyperparams_xgb[j, ]
            best_model <- xgb_model
          }
        }
        
        results <- rbind(results, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                             max_depth = hyperparams_xgb$max_depth[j],
                                             eta = hyperparams_xgb$eta[j],
                                             subsample = hyperparams_xgb$subsample[j],
                                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                                             gamma = hyperparams_xgb$gamma[j],
                                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        xgb_model <- xgboost(data = as.matrix(mydata[, -ncol(mydata)]), # Convert to matrix
                             label = as.numeric(mydata$occurrenceStatus),
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc")
        
        valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]), type='response')
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        
        results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                       max_depth = hyperparams_xgb$max_depth[j],
                                                       eta = hyperparams_xgb$eta[j],
                                                       subsample = hyperparams_xgb$subsample[j],
                                                       min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                       gamma = hyperparams_xgb$gamma[j],
                                                       colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    results <- aggregate(. ~ nrounds + max_depth + eta + subsample + min_child_weight +
                           gamma + colsample_bylevel,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                gamma + colsample_bylevel,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}






temp_xgb_lastfold <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_xgb, mydata_past) {
  results_list <- list()  
  results_past_list <- list()
  
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      set.seed(21)  # Set the seed
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_xgb)) {
        print(paste0("Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                               label = train$occurrenceStatus,
                               nrounds = hyperparams_xgb$nrounds[j],
                               max_depth = hyperparams_xgb$max_depth[j],
                               eta = hyperparams_xgb$eta[j],
                               subsample = hyperparams_xgb$subsample[j],
                               min_child_weight = hyperparams_xgb$min_child_weight[j],
                               gamma = hyperparams_xgb$gamma[j],
                               colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                               objective = "binary:logistic",
                               eval_metric = "auc")
          
          prob_predictions <- predict(xgb_model, as.matrix(test[, -ncol(test)]))
          
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          # Predict on the validation set and calculate ROC AUC
          valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]), type = "response")
          ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
          
          
        }
        
        results <- rbind(results, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                             max_depth = hyperparams_xgb$max_depth[j],
                                             eta = hyperparams_xgb$eta[j],
                                             subsample = hyperparams_xgb$subsample[j],
                                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                                             gamma = hyperparams_xgb$gamma[j],
                                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                       max_depth = hyperparams_xgb$max_depth[j],
                                                       eta = hyperparams_xgb$eta[j],
                                                       subsample = hyperparams_xgb$subsample[j],
                                                       min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                       gamma = hyperparams_xgb$gamma[j],
                                                       colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    results <- aggregate(. ~ nrounds + max_depth + eta + subsample + min_child_weight +
                           gamma + colsample_bylevel,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                gamma + colsample_bylevel,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}








#LightGBM
#####################################################################################

sp_lgb_retrain <- function(block_sizes, model_names, hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    valid <- mydata_past
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, # number of folds
                     size = block_size, # size of the blocks in meters
                     selection = "random", # random blocks-to-fold
                     iteration = 50, # find evenly dispersed folds
                     biomod2 = TRUE) # also create folds for biomod2
    
    folds <- sb$folds_list
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_lgbm)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      # Loop through all folds
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        
        y_train <- train$occurrenceStatus
        y_test <- test$occurrenceStatus
        
        lgb_train <- lgb.Dataset(as.matrix(train[,1:25]),label=y_train)
        lgb_test <- lgb.Dataset.create.valid(lgb_train, test[,1:25],label=y_test)
        
        # Set LightGBM parameters
        lgb_params <- list(objective = "binary",
                           metric = "auc",
                           boosting_type = "gbdt",
                           num_iterations = hyperparams_lgbm$num_iterations[j],
                           num_leaves = hyperparams_lgbm$num_leaves[j],
                           learning_rate = hyperparams_lgbm$learning_rate[j],
                           subsample = hyperparams_lgbm$subsample[j],
                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                           verbose = -1)
        
        # Train the LightGBM model
        lgb_model <- lgb.train(params = lgb_params,data = lgb_train)
        
        prob_predictions <- predict(lgb_model,as.matrix(test[,1:25]))
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_mean_AUC) {
          best_mean_AUC <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams_lgbm[j, ]
          best_model <- lgb_model
        }
      }
      
      results <- rbind(results, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                           num_leaves = hyperparams_lgbm$num_leaves[j],
                                           learning_rate = hyperparams_lgbm$learning_rate[j],
                                           subsample = hyperparams_lgbm$subsample[j],
                                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      
      y_mydata <- mydata$occurrenceStatus
      y_valid <- valid$occurrenceStatus
      lgb_mydata <- lgb.Dataset(as.matrix(mydata[,1:25]),label=y_mydata)
      lgb_valid <- lgb.Dataset.create.valid(lgb_mydata, valid[,1:25], label=y_valid)
      
      lgb_params <- list(objective = "binary",
                         metric = "auc",
                         boosting_type = "gbdt",
                         num_iterations = hyperparams_lgbm$num_iterations[j],
                         num_leaves = hyperparams_lgbm$num_leaves[j],
                         learning_rate = hyperparams_lgbm$learning_rate[j],
                         subsample = hyperparams_lgbm$subsample[j],
                         colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                         verbose = -1)
      
      lgb_model <- lgb.train(params = lgb_params,
                             data = lgb_mydata)
      
      valid_predictions <- predict(lgb_model, as.matrix(valid[,1:25]))
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      results_past <- rbind(results_past, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                     num_leaves = hyperparams_lgbm$num_leaves[j],
                                                     learning_rate = hyperparams_lgbm$learning_rate[j],
                                                     subsample = hyperparams_lgbm$subsample[j],
                                                     colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }
    

    #saveRDS(best_model, paste0("/app/proj_cv/results//models/", model_name, "_best_model_", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid.rds"))
    
    write.csv(results, file = paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, file = paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
  }
}








sp_lgb_lastfold <- function(block_sizes, model_names, hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    best_mean_AUC <- 0
    best_hyperparams <- NULL
    best_model <- NULL
    best_model_valid <- NULL
    best_AUC_valid <- 0
    best_mean_AUC <- 0
    
    results <- data.frame()
    results_past <- data.frame()
    
    valid <- mydata_past
    
    # Modify your code to use the current block_size and model_name
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = 5, # number of folds
                     size = block_size, # size of the blocks in meters
                     selection = "random", # random blocks-to-fold
                     iteration = 50, # find evenly dispersed folds
                     biomod2 = TRUE) # also create folds for biomod2
    
    folds <- sb$folds_list
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams_lgbm)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      # Loop through all folds
      for (fold in 1:5) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        train <- mydata[trainSet, ]
        test <- mydata[testSet, ]
        valid <- mydata_past
        
        y_train <- train$occurrenceStatus
        y_test <- test$occurrenceStatus
        y_mydata <- mydata$occurrenceStatus
        y_valid <- valid$occurrenceStatus
        
        lgb_train <- lgb.Dataset(as.matrix(train[,1:25]),label=y_train)
        lgb_test <- lgb.Dataset.create.valid(lgb_train, test[,1:25],label=y_test)
        
        lgb_mydata <- lgb.Dataset(as.matrix(mydata[,1:25]),label=y_mydata)
        lgb_valid <- lgb.Dataset.create.valid(lgb_mydata, valid[,1:25], label=y_valid)
        
        # Set LightGBM parameters
        lgb_params <- list(objective = "binary",
                           metric = "auc",
                           boosting_type = "gbdt",
                           num_iterations = hyperparams_lgbm$num_iterations[j],
                           num_leaves = hyperparams_lgbm$num_leaves[j],
                           learning_rate = hyperparams_lgbm$learning_rate[j],
                           subsample = hyperparams_lgbm$subsample[j],
                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                           verbose = -1)
        
        # Train the LightGBM model
        lgb_model <- lgb.train(params = lgb_params,data = lgb_train)
        
        prob_predictions <- predict(lgb_model,as.matrix(test[,1:25]))
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (mean(fold_ROC_AUC) > best_mean_AUC) {
          best_mean_AUC <- mean(fold_ROC_AUC)
          best_hyperparams <- hyperparams_lgbm[j, ]
          best_model <- lgb_model
        }
      }
      
      results <- rbind(results, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                           num_leaves = hyperparams_lgbm$num_leaves[j],
                                           learning_rate = hyperparams_lgbm$learning_rate[j],
                                           subsample = hyperparams_lgbm$subsample[j],
                                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      
      
      
      lgb_model <- lgb.train(params = lgb_params,
                             data = lgb_mydata)
      
      valid_predictions <- predict(lgb_model, as.matrix(valid[,1:25]))
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Check if current model has a higher AUC on the validation set
      if (ROC_AUC_valid > best_AUC_valid) {
        best_AUC_valid <- ROC_AUC_valid
        best_model_valid <- best_model
      }
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                     num_leaves = hyperparams_lgbm$num_leaves[j],
                                                     learning_rate = hyperparams_lgbm$learning_rate[j],
                                                     subsample = hyperparams_lgbm$subsample[j],
                                                     colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
    }
    
    # Save the best model for the current hyperparameter combination
    #saveRDS(best_model, paste0("/app/proj_cv/results/models/", model_name, "_best_model_lf", ".rds"))
    #saveRDS(best_model_valid, paste0("/app/proj_cv/results/models/", model_name, "_best_model_valid_lf.rds"))
    
    # Save the results and results_past
    write.csv(results, file = paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, file = paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}








#Temporal
temp_lgb_retrain <- function(time_cropped, time_pa_data, time_intervals_data,block_sizes, model_names, hyperparams_lgbm, mydata, mydata_past) {
  
  results_list <- list()  
  results_past_list <- list()
  
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      set.seed(21)  # Set the seed
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_lgbm)) {
        print(paste0("Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          y_train <- train$occurrenceStatus
          y_test <- test$occurrenceStatus
          
          lgb_train <- lgb.Dataset(as.matrix(train[,1:25]),label=y_train)
          lgb_test <- lgb.Dataset.create.valid(lgb_train, test[,1:25],label=y_test)
          
          # Set LightGBM parameters
          lgb_params <- list(objective = "binary",
                             metric = "auc",
                             boosting_type = "gbdt",
                             num_iterations = hyperparams_lgbm$num_iterations[j],
                             num_leaves = hyperparams_lgbm$num_leaves[j],
                             learning_rate = hyperparams_lgbm$learning_rate[j],
                             subsample = hyperparams_lgbm$subsample[j],
                             colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                             verbose = -1)
          
          
          # Train the LightGBM model
          lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
          prob_predictions <- predict(lgb_model, as.matrix(test[,1:25]))
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (mean(fold_ROC_AUC) > best_mean_AUC) {
            best_mean_AUC <- mean(fold_ROC_AUC)
            best_hyperparams <- hyperparams_lgbm[j, ]
            best_model <- lgb_model
          }
        }
        
        # Store results for the current hyperparameter combination
        results <- rbind(results, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                             num_leaves = hyperparams_lgbm$num_leaves[j],
                                             learning_rate = hyperparams_lgbm$learning_rate[j],
                                             subsample = hyperparams_lgbm$subsample[j],
                                             colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        
        
        y_mydata <- mydata$occurrenceStatus
        y_valid <- valid$occurrenceStatus
        lgb_mydata <- lgb.Dataset(as.matrix(mydata[,1:25]),label=y_mydata)
        lgb_valid <- lgb.Dataset.create.valid(lgb_mydata, valid[,1:25], label=y_valid)
        
        lgb_params <- list(objective = "binary",
                           metric = "auc",
                           boosting_type = "gbdt",
                           num_iterations = hyperparams_lgbm$num_iterations[j],
                           num_leaves = hyperparams_lgbm$num_leaves[j],
                           learning_rate = hyperparams_lgbm$learning_rate[j],
                           subsample = hyperparams_lgbm$subsample[j],
                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                           verbose = -1)
        
        lgb_model <- lgb.train(params = lgb_params,
                               data = lgb_mydata)
        
        valid_predictions <- predict(lgb_model, as.matrix(valid[,1:25]))
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past <- rbind(results_past, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                       num_leaves = hyperparams_lgbm$num_leaves[j],
                                                       learning_rate = hyperparams_lgbm$learning_rate[j],
                                                       subsample = hyperparams_lgbm$subsample[j],
                                                       colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
        
        
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    results <- aggregate(. ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ num_iterations + max_depth + eta + subsample + min_child_weight +
                                gamma + colsample_bylevel,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past.csv"))
    
  }
}





temp_lgb_lastfold <- function(time_cropped, time_pa_data, time_intervals_data,block_sizes, model_names, hyperparams_lgbm, mydata_past) {
  
  results_list <- list()
  results_past_list <- list()
  
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    results <- data.frame()
    results_past <- data.frame()
    
    best_model <- NULL
    best_mean_AUC <- 0  
    
    valid <- mydata_past
    
    for (k in seq_along(block_sizes)) {
      block_size <- block_sizes[k]
      model_name <- model_names[k]
      
      set.seed(21)  # Set the seed
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = 5,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_lgbm)) {
        print(paste0("Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:5) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          y_train <- train$occurrenceStatus
          y_test <- test$occurrenceStatus
          
          lgb_train <- lgb.Dataset(as.matrix(train[,1:25]),label=y_train)
          lgb_test <- lgb.Dataset.create.valid(lgb_train, test[,1:25],label=y_test)
          
          # Set LightGBM parameters
          lgb_params <- list(objective = "binary",
                             metric = "auc",
                             boosting_type = "gbdt",
                             num_iterations = hyperparams_lgbm$num_iterations[j],
                             num_leaves = hyperparams_lgbm$num_leaves[j],
                             learning_rate = hyperparams_lgbm$learning_rate[j],
                             subsample = hyperparams_lgbm$subsample[j],
                             colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                             verbose = -1)
          
          
          # Train the LightGBM model
          lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
          prob_predictions <- predict(lgb_model, as.matrix(test[,1:25]))
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          # Append ROC AUC to fold_ROC_AUC vector
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          valid_predictions <- predict(lgb_model,as.matrix(valid[,1:25]))
          ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
          
        }
        
        # Store results for the current hyperparameter combination
        results <- rbind(results, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                             num_leaves = hyperparams_lgbm$num_leaves[j],
                                             learning_rate = hyperparams_lgbm$learning_rate[j],
                                             subsample = hyperparams_lgbm$subsample[j],
                                             colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                             mean_ROC_AUC = mean(fold_ROC_AUC),
                                             fold_ROC_AUC = toString(fold_ROC_AUC)))
        
        results_past <- rbind(results_past, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                       num_leaves = hyperparams_lgbm$num_leaves[j],
                                                       learning_rate = hyperparams_lgbm$learning_rate[j],
                                                       subsample = hyperparams_lgbm$subsample[j],
                                                       colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                       ROC_AUC_valid = ROC_AUC_valid))
      }
    }
    
    results_list[[length(results_list) + 1]] <- results
    combined_results <- do.call(rbind, results_list)
    results <- aggregate(. ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                         data = combined_results,
                         FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    
    
    results_past_list[[length(results_past_list) + 1]] <- results_past
    combined_results_past <- do.call(rbind, results_past_list)
    results_past <- aggregate(. ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                              data = combined_results_past,
                              FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
    
    write.csv(results, paste0("/app/proj_cv/results/", model_name, "_results_lf.csv"))
    write.csv(results_past, paste0("/app/proj_cv/results/", model_name, "_results_past_lf.csv"))
  }
}





## Functions for preprocessing
##Temporal
spatiotemp_occ_data <- function(cropped_elev, cropped_soil) {
  temp1 <- read.csv('/app/proj_cv/spThin/gen_thin_0.5_spatiotemp_1.csv')
  temp2 <- read.csv('/app/proj_cv/spThin/gen_thin_0.5_spatiotemp_2.csv')
  temp3 <- read.csv('/app/proj_cv/spThin/gen_thin_0.5_spatiotemp_3.csv')
  temp4 <- read.csv('/app/proj_cv/spThin/gen_thin_0.5_spatiotemp_4.csv')
  
  temp1$occurrenceStatus <- 1
  temp2$occurrenceStatus <- 1
  temp3$occurrenceStatus <- 1
  temp4$occurrenceStatus <- 1
  
  abs <- gen_cam[gen_cam$occurrenceStatus == 0,]
  
  abs1 <- abs[abs$time == 1,]
  abs2 <- abs[abs$time == 2,]
  abs3 <- abs[abs$time == 3,]
  abs4 <- abs[abs$time == 4,]
  
  # List of temp data frames
  temp_list <- list(temp1, temp2, temp3, temp4)
  abs_list <- list(abs1, abs2, abs3, abs4)
  
  # Combine temp and abs data frames and select specific columns
  combined_data_list <- list()
  for (i in 1:4) {
    temp_list[[i]] <- temp_list[[i]][, 2:4]
    abs_list[[i]] <- abs_list[[i]][, 1:3]
    combined_data_list[[i]] <- rbind(temp_list[[i]], abs_list[[i]])
  }
  return(combined_data_list)
}





get_mydata <- function(df, cropped){
  mydata <- raster::extract(cropped, df, df = TRUE)
  mydata$occurrenceStatus <- as.numeric(df$occurrenceStatus)
  mydata <- mydata %>% 
    mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
  mydata <- mydata[-1]
  mydata <- na.omit(mydata)
  return(mydata)
}

create_pa_data <- function(temp_data, cropped_temp, elevation_data, soil_data) {
  stacked_data <- stack(c(cropped_temp, elevation_data, soil_data))
  pa_data_temp <- st_as_sf(temp_data, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(stacked_data))
  return( pa_data_temp)
}

# Define a function to load and process spatiotemporal environmental data
spatiotemp_env_data <- function(cropped_elev,cropped_soil,
                                temp1,temp2,temp3,temp4,env_data) {
  
  
  setwd('/app/proj_cv/spatiotemp/temp1')
  temp1_rasters <- list.files(path = "/app/proj_cv/spatiotemp/temp1", pattern='.asc', 
                              all.files=TRUE, full.names=FALSE)
  cropped_temp1 <- env_data(temp1_rasters)
  
  setwd('/app/proj_cv/spatiotemp/temp2')
  temp2_rasters <- list.files(path = "/app/proj_cv/spatiotemp/temp2", pattern='.asc', 
                              all.files=TRUE, full.names=FALSE)
  cropped_temp2 <- env_data(temp2_rasters)
  
  setwd('/app/proj_cv/spatiotemp/temp3')
  temp3_rasters <- list.files(path = "/app/proj_cv/spatiotemp/temp3", pattern='.asc', 
                              all.files=TRUE, full.names=FALSE)
  cropped_temp3 <- env_data(temp3_rasters)
  
  setwd('/app/proj_cv/spatiotemp/temp4')
  temp4_rasters <- list.files(path = "/app/proj_cv/spatiotemp/temp4", pattern='.asc', 
                              all.files=TRUE, full.names=FALSE)
  cropped_temp4 <- env_data(temp4_rasters)
  
  cropped_temp1 <- stack(c(cropped_temp1, cropped_elev,cropped_soil))
  cropped_temp2 <- stack(c(cropped_temp2, cropped_elev,cropped_soil))
  cropped_temp3 <- stack(c(cropped_temp3, cropped_elev,cropped_soil))
  cropped_temp4 <- stack(c(cropped_temp4, cropped_elev,cropped_soil))
  
  
  lst <- list(cropped_temp1,cropped_temp2,cropped_temp3,cropped_temp4)
  return(lst)
}


