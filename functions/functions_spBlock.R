# RETRAIN strategy: Train the model on the full training set (in_time_data) and evaluate on past data
# LAST FOLD strategy: Use the model trained on the last fold of the CV process to evaluate on past data


#' Set this to your local output directory 
your_dir <- "path/to/your/results/"


########################################################## --- SPATIAL BLOCKING FUNCTIONS --- #########################################################################

sp_gbm <- function(block_sizes, model_names, hyperparams, in_time_data, pa_data, cropped_env, out_of_time_data, k=5) {
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    results <- data.frame()
    results_past <- data.frame()
    results_past_lf <- data.frame()
    
    
    # Modify your code to use the current block_size and model_name
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = k, 
                     size = block_size, 
                     selection = "random", 
                     iteration = 50, 
                     biomod2 = TRUE) 
    
    folds <- sb$folds_list
    
    
    # Loop through all hyperparameter combinations
    for (j in 1:nrow(hyperparams)) {
      print(paste0("Iteration: ", j))
      
      fold_ROC_AUC <- c()
      
      
      for (fold in 1:k) {
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- in_time_data[trainSet, ]
        test <- in_time_data[testSet, ]
        valid <- out_of_time_data
        
        
        gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                         data = train, n.trees = hyperparams$n.trees[j],
                         interaction.depth = hyperparams$interaction.depth[j],
                         shrinkage = hyperparams$shrinkage[j],
                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                         verbose = FALSE)
        
        # Make predictions on the testing set
        prob_predictions <- predict(gbm_model, newdata = test, type = "response")
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == k) {
          last_fold_model <- gbm_model
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
                       data = in_time_data, n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                     interaction.depth = hyperparams$interaction.depth[j],
                                                     shrinkage = hyperparams$shrinkage[j],
                                                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
      valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "response")
      ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
      
      results_past_lf <- rbind(results_past_lf, data.frame(n.trees = hyperparams$n.trees[j],
                                                     interaction.depth = hyperparams$interaction.depth[j],
                                                     shrinkage = hyperparams$shrinkage[j],
                                                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                     ROC_AUC_valid_lf = ROC_AUC_valid_lf))
      
    }
    
    
    write.csv(results, paste0(your_dir, model_name, "_results.csv"))
    write.csv(results_past, paste0(your_dir, model_name, "_results_past.csv"))
    write.csv(results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"))
  }
}



sp_rf <- function(block_sizes, model_names, hyperparams_rf, in_time_data, pa_data, cropped_env, out_of_time_data, k=5){
  
  in_time_data$occurrenceStatus <- factor(in_time_data$occurrenceStatus)
  out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus)
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    
    results <- data.frame()
    results_past <- data.frame()
    results_past_lf <- data.frame()
    
    
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = k, # number of folds
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
      
      
      for (fold in 1:k) {
        # Get training and testing indices for the current fold
        trainSet <- unlist(folds[[fold]][1])
        testSet <- unlist(folds[[fold]][2])
        
        # Split data into training and testing sets
        train <- in_time_data[trainSet, ]
        test <- in_time_data[testSet, ]
        valid <- out_of_time_data
        
        # Setup Random Forest model
        
        cat("Retrain: Number of rows in training set = ", nrow(in_time_data), "\n")
        cat("Row indices used for training (Retrain):\n")
        print(rownames(in_time_data))
        
        
        rf_model <- randomForest(occurrenceStatus ~ .,
                                 data      = train,
                                 ntree     = hyperparams_rf$n.trees[j],
                                 nodesize  = hyperparams_rf$nodesize[j],
                                 maxnodes  = hyperparams_rf$maxnodes[j])
        

        prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == k) {
          last_fold_model <- rf_model
        }
      }
      results <- rbind(results, data.frame(ntree     = hyperparams_rf$n.trees[j],
                                           nodesize  = hyperparams_rf$nodesize[j],
                                           maxnodes  = hyperparams_rf$maxnodes[j], 
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      # Train a Random Forest model on the full dataset
      rf_model <- randomForest(occurrenceStatus ~ .,
                               data = in_time_data,
                               ntree     = hyperparams_rf$n.trees[j],
                               nodesize  = hyperparams_rf$nodesize[j],
                               maxnodes  = hyperparams_rf$maxnodes[j])
      
      # Predict on the validation set and calculate ROC AUC
      valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[,2]
      ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
      
      # Store results for the validation predictions
      results_past <- rbind(results_past, data.frame(ntree     = hyperparams_rf$n.trees[j],
                                                     nodesize  = hyperparams_rf$nodesize[j],
                                                     maxnodes  = hyperparams_rf$maxnodes[j],
                                                     ROC_AUC_valid = ROC_AUC_valid))
      
      
      valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "prob")[,2]
      ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
      
      
      results_past_lf <- rbind(results_past_lf, data.frame(ntree     = hyperparams_rf$n.trees[j],
                                                           nodesize  = hyperparams_rf$nodesize[j],
                                                           maxnodes  = hyperparams_rf$maxnodes[j],
                                                     ROC_AUC_valid_lf = ROC_AUC_valid_lf))
      
    }
    
    write.csv(results, paste0(your_dir, model_name, "_results.csv"))
    write.csv(results_past, paste0(your_dir, model_name, "_results_past.csv"))
    write.csv(results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"))
  }
}



sp_lgb <- function(block_sizes, model_names, hyperparams_lgbm, in_time_data, pa_data, cropped_env, out_of_time_data, k = 5) {
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    results <- data.frame()
    results_past <- data.frame()
    results_past_lf <- data.frame()
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = k,
                     size = block_size,
                     selection = "random",
                     iteration = 50,
                     biomod2 = TRUE)
    
    folds <- sb$folds_list
    
    for (j in 1:nrow(hyperparams_lgbm)) {
      cat("Hyperparameter set: ", j, "\n")
      fold_ROC_AUC <- c()
      last_fold_model <- NULL
      
      for (fold in 1:k) {
        trainSet <- unlist(folds[[fold]][[1]])
        testSet <- unlist(folds[[fold]][[2]])
        
        train <- in_time_data[trainSet, ]
        test <- in_time_data[testSet, ]
        
        features <- setdiff(names(train), "occurrenceStatus")
        y_train <- train$occurrenceStatus
        y_test <- test$occurrenceStatus
        
        lgb_train <- lgb.Dataset(as.matrix(train[, features]), label = y_train)
        
        lgb_params <- list(objective = "binary",
                           metric = "auc",
                           boosting_type = "gbdt",
                           num_iterations = hyperparams_lgbm$num_iterations[j],
                           num_leaves = hyperparams_lgbm$num_leaves[j],
                           learning_rate = hyperparams_lgbm$learning_rate[j],
                           subsample = hyperparams_lgbm$subsample[j],
                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                           verbose = -1)
        
        lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
        
        prob_predictions <- predict(lgb_model, as.matrix(test[, features]))
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == k) {
          last_fold_model <- lgb_model
        }
      }
      
      results <- rbind(results, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                           num_leaves = hyperparams_lgbm$num_leaves[j],
                                           learning_rate = hyperparams_lgbm$learning_rate[j],
                                           subsample = hyperparams_lgbm$subsample[j],
                                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      # Retrain on all data
      features <- setdiff(names(in_time_data), "occurrenceStatus")
      y_in_time_data <- in_time_data$occurrenceStatus
      y_valid <- out_of_time_data$occurrenceStatus
      
      lgb_in_time_data <- lgb.Dataset(as.matrix(in_time_data[, features]), label = y_in_time_data)
      
      lgb_model_full <- lgb.train(params = lgb_params, data = lgb_in_time_data)
      
      valid_predictions_full <- predict(lgb_model_full, as.matrix(out_of_time_data[, features]))
      ROC_AUC_valid_full <- pROC::auc(y_valid, valid_predictions_full)
      
      results_past <- rbind(results_past, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                     num_leaves = hyperparams_lgbm$num_leaves[j],
                                                     learning_rate = hyperparams_lgbm$learning_rate[j],
                                                     subsample = hyperparams_lgbm$subsample[j],
                                                     colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                     ROC_AUC_valid = ROC_AUC_valid_full))
      
      # Evaluate last fold model on validation set
      valid_predictions_lf <- predict(last_fold_model, as.matrix(out_of_time_data[, features]))
      ROC_AUC_valid_lf <- pROC::auc(y_valid, valid_predictions_lf)
      
      results_past_lf <- rbind(results_past_lf, data.frame(num_iterations = hyperparams_lgbm$num_iterations[j],
                                                           num_leaves = hyperparams_lgbm$num_leaves[j],
                                                           learning_rate = hyperparams_lgbm$learning_rate[j],
                                                           subsample = hyperparams_lgbm$subsample[j],
                                                           colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                                                           ROC_AUC_valid_lf = ROC_AUC_valid_lf))
    }
    
    write.csv(results, paste0(your_dir, model_name, "_results.csv"))
    write.csv(results_past, paste0(your_dir, model_name, "_results_past.csv"))
    write.csv(results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"))
  }
}





sp_xgb <- function(block_sizes, model_names, hyperparams_xgb, in_time_data, pa_data, cropped_env, out_of_time_data, k=5){
  
  for (i in seq_along(block_sizes)) {
    block_size <- block_sizes[i]
    model_name <- model_names[i]
    
    results <- data.frame()
    results_past <- data.frame()
    results_past_lf <- data.frame()
    
    set.seed(21)
    sb <- cv_spatial(x = pa_data,
                     column = "occurrenceStatus",
                     r = cropped_env,
                     k = k,
                     size = block_size,
                     selection = "random",
                     iteration = 50,
                     biomod2 = TRUE)
    
    folds <- sb$folds_list
    
    for (j in 1:nrow(hyperparams_xgb)) {
      cat("Hyperparameter set: ", j, "\n")
      fold_ROC_AUC <- c()
      last_fold_model <- NULL
      
      for (fold in 1:k) {
        trainSet <- unlist(folds[[fold]][[1]])
        testSet <- unlist(folds[[fold]][[2]])
        
        train <- in_time_data[trainSet, ]
        test <- in_time_data[testSet, ]
        
        xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                             label = as.numeric(train$occurrenceStatus),
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc",
                             verbose = 0)
        
        prob_predictions <- predict(xgb_model, newdata = as.matrix(test[, -ncol(test)]))
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == k) {
          last_fold_model <- xgb_model
        }
      }
      
      # Store CV results
      results <- rbind(results, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                           max_depth = hyperparams_xgb$max_depth[j],
                                           eta = hyperparams_xgb$eta[j],
                                           subsample = hyperparams_xgb$subsample[j],
                                           min_child_weight = hyperparams_xgb$min_child_weight[j],
                                           gamma = hyperparams_xgb$gamma[j],
                                           colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                           mean_ROC_AUC = mean(fold_ROC_AUC),
                                           fold_ROC_AUC = toString(fold_ROC_AUC)))
      
      # Retrain on all data
      xgb_model_full <- xgboost(data = as.matrix(in_time_data[, -ncol(in_time_data)]),
                                label = as.numeric(in_time_data$occurrenceStatus),
                                nrounds = hyperparams_xgb$nrounds[j],
                                max_depth = hyperparams_xgb$max_depth[j],
                                eta = hyperparams_xgb$eta[j],
                                subsample = hyperparams_xgb$subsample[j],
                                min_child_weight = hyperparams_xgb$min_child_weight[j],
                                gamma = hyperparams_xgb$gamma[j],
                                colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                objective = "binary:logistic",
                                eval_metric = "auc",
                                verbose = 0)
      
      # Evaluate retrained model
      valid_predictions_full <- predict(xgb_model_full, newdata = as.matrix(out_of_time_data[, -ncol(out_of_time_data)]))
      ROC_AUC_valid_full <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_full)
      
      results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                     max_depth = hyperparams_xgb$max_depth[j],
                                                     eta = hyperparams_xgb$eta[j],
                                                     subsample = hyperparams_xgb$subsample[j],
                                                     min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                     gamma = hyperparams_xgb$gamma[j],
                                                     colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                     ROC_AUC_valid = ROC_AUC_valid_full))
      
      # Evaluate last fold model
      valid_predictions_lf <- predict(last_fold_model, newdata = as.matrix(out_of_time_data[, -ncol(out_of_time_data)]))
      ROC_AUC_valid_lf <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_lf)
      
      results_past_lf <- rbind(results_past_lf, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                           max_depth = hyperparams_xgb$max_depth[j],
                                                           eta = hyperparams_xgb$eta[j],
                                                           subsample = hyperparams_xgb$subsample[j],
                                                           min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                           gamma = hyperparams_xgb$gamma[j],
                                                           colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                           ROC_AUC_valid_lf = ROC_AUC_valid_lf))
    }
    
    write.csv(results, file = paste0(your_dir, model_name, "_results.csv"))
    write.csv(results_past, file = paste0(your_dir, model_name, "_results_past.csv"))
    write.csv(results_past_lf, file = paste0(your_dir, model_name, "_results_past_lf.csv"))
  }
}




################################################################## --- RANDOM CV BLOCKING ---#############################################################################

random_gbm <- function(hyperparams, in_time_data, pa_data, out_of_time_data, model_name, k=5){
  
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()
  
  # Generate random folds for 5-fold cross-validation
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = k, list = TRUE)
  
  # Loop through all hyperparameter combinations
  for (j in 1:nrow(hyperparams)) {
    print(paste0("Iteration: ", j))
    
    fold_ROC_AUC <- c()
    
    # Loop through all folds
    for (fold in 1:k) {
      # Get training and testing indices for the current fold
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      # Split data into training and testing sets
      train <- in_time_data[trainSet, ]
      test <- in_time_data[testSet, ]
      valid <- out_of_time_data
      
      train <- train[complete.cases(train), ]
      test <- test[complete.cases(test), ]
      valid <- valid[complete.cases(valid), ]
      
      # Setup gbm model
      gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                       data = train, n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      prob_predictions <- predict(gbm_model, newdata = test, type = "response")
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (fold == k) {
        last_fold_model <- gbm_model}
    }
    
    results <- rbind(results, data.frame(n.trees = hyperparams$n.trees[j],
                                         interaction.depth = hyperparams$interaction.depth[j],
                                         shrinkage = hyperparams$shrinkage[j],
                                         n.minobsinnode = hyperparams$n.minobsinnode[j],
                                         mean_ROC_AUC = mean(fold_ROC_AUC),
                                         fold_ROC_AUC = toString(fold_ROC_AUC)))
    
    # Full model retraining on in_time_data
    gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                     data = in_time_data, n.trees = hyperparams$n.trees[j],
                     interaction.depth = hyperparams$interaction.depth[j],
                     shrinkage = hyperparams$shrinkage[j],
                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                     verbose = FALSE)
    
    # Predict on the validation set and calculate ROC AUC
    valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
    
    results_past <- rbind(results_past, data.frame(n.trees = hyperparams$n.trees[j],
                                                   interaction.depth = hyperparams$interaction.depth[j],
                                                   shrinkage = hyperparams$shrinkage[j],
                                                   n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                   ROC_AUC_valid = ROC_AUC_valid))
    
    
    valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "response")
    ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(n.trees = hyperparams$n.trees[j],
                                                   interaction.depth = hyperparams$interaction.depth[j],
                                                   shrinkage = hyperparams$shrinkage[j],
                                                   n.minobsinnode = hyperparams$n.minobsinnode[j],
                                                   ROC_AUC_valid_lf = ROC_AUC_valid_lf))
    
  }
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_random_results.csv'), row.names = FALSE)
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_random_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_random_results_past_lf.csv'), row.names = FALSE)
}




random_rf <- function(hyperparams_rf, in_time_data, pa_data, out_of_time_data, model_name, k=5){
  
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = k, list = TRUE)
  
  results <- data.frame() 
  results_past <- data.frame()  
  results_past_lf <- data.frame()
  
  # Loop through all hyperparameter combinations
  for (j in 1:nrow(hyperparams_rf)) {
    print(paste0("Iteration: ", j))
    
    # Initialize variable to store ROC AUC across all folds for the current hyperparameter combination
    fold_ROC_AUC <- c()
    
    # Loop through all folds
    for (fold in 1:k) {
      # Get training and testing indices for the current fold
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      # Split data into training and testing sets
      in_time_data$occurrenceStatus <- factor(in_time_data$occurrenceStatus)
      out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus)
      train <- in_time_data[trainSet, ]
      test <- in_time_data[testSet, ]
      valid <- out_of_time_data
      
      
      train <- train[complete.cases(train), ]
      test <- test[complete.cases(test), ]
      valid <- valid[complete.cases(valid), ]
      
      # Setup model
      rf_model <- randomForest(occurrenceStatus ~ .,
                               data = train,
                               tree     = hyperparams_rf$n.trees[j],
                               nodesize  = hyperparams_rf$nodesize[j],
                               maxnodes  = hyperparams_rf$maxnodes[j])
      

      prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (fold == k) {
        last_fold_model <- rf_model
        }
    }
    
    rf_model <- randomForest(occurrenceStatus ~ .,
                             data = in_time_data,
                             tree     = hyperparams_rf$n.trees[j],
                             nodesize  = hyperparams_rf$nodesize[j],
                             maxnodes  = hyperparams_rf$maxnodes[j])
    

    results <- rbind(results, data.frame(tree     = hyperparams_rf$n.trees[j],
                                         nodesize  = hyperparams_rf$nodesize[j],
                                         maxnodes  = hyperparams_rf$maxnodes[j],
                                         mean_ROC_AUC = mean(fold_ROC_AUC),
                                         fold_ROC_AUC = toString(fold_ROC_AUC)))
    
    # Predict on the validation set and calculate ROC AUC
    valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[, 2]
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
    
    
    results_past <- rbind(results_past, data.frame(tree = hyperparams_rf$n.trees[j],
                                                   nodesize  = hyperparams_rf$nodesize[j],
                                                   maxnodes  = hyperparams_rf$maxnodes[j],
                                                   ROC_AUC_valid = ROC_AUC_valid))
    
    valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "prob")[, 2]
    ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(tree = hyperparams_rf$n.trees[j],
                                                         nodesize  = hyperparams_rf$nodesize[j],
                                                         maxnodes  = hyperparams_rf$maxnodes[j],
                                                         ROC_AUC_valid_lf = ROC_AUC_valid_lf))
    
  }
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_random_results.csv'), row.names = FALSE)
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_random_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_random_results_past_lf.csv'), row.names = FALSE)
}



random_xgb <- function(hyperparams_xgb, in_time_data, pa_data, out_of_time_data, model_name, k=5){
  
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = k, list = TRUE)
  
  results <- data.frame()  
  results_past <- data.frame()  
  results_past_lf <- data.frame()
  
  for (j in 1:nrow(hyperparams_xgb)) {
    cat("Hyperparameter set: ", j, "\n")
    fold_ROC_AUC <- c()
    last_fold_model <- NULL
    
    for (fold in 1:k) {
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      train <- in_time_data[trainSet, , drop = FALSE]
      test <- in_time_data[testSet, , drop = FALSE]
      valid <- out_of_time_data
      
      train <- train[complete.cases(train), ]
      test <- test[complete.cases(test), ]
      valid <- valid[complete.cases(valid), ]
      
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
                           eval_metric = "auc",
                           verbose = 0)
      
      prob_predictions <- predict(xgb_model, newdata = as.matrix(test[, -ncol(test)]))
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (fold == k) {
        last_fold_model <- xgb_model
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
    
    xgb_model_full <- xgboost(data = as.matrix(in_time_data[, -ncol(in_time_data)]),
                              label = as.numeric(in_time_data$occurrenceStatus),
                              nrounds = hyperparams_xgb$nrounds[j],
                              max_depth = hyperparams_xgb$max_depth[j],
                              eta = hyperparams_xgb$eta[j],
                              subsample = hyperparams_xgb$subsample[j],
                              min_child_weight = hyperparams_xgb$min_child_weight[j],
                              gamma = hyperparams_xgb$gamma[j],
                              colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                              objective = "binary:logistic",
                              eval_metric = "auc",
                              verbose = 0)
    
    valid_predictions_full <- predict(xgb_model_full, newdata = as.matrix(out_of_time_data[, -ncol(out_of_time_data)]))
    ROC_AUC_valid_full <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_full)
    
    results_past <- rbind(results_past, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                   max_depth = hyperparams_xgb$max_depth[j],
                                                   eta = hyperparams_xgb$eta[j],
                                                   subsample = hyperparams_xgb$subsample[j],
                                                   min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                   gamma = hyperparams_xgb$gamma[j],
                                                   colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                   ROC_AUC_valid = ROC_AUC_valid_full))
    
    valid_predictions_lf <- predict(last_fold_model, newdata = as.matrix(out_of_time_data[, -ncol(out_of_time_data)]))
    ROC_AUC_valid_lf <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(nrounds = hyperparams_xgb$nrounds[j],
                                                         max_depth = hyperparams_xgb$max_depth[j],
                                                         eta = hyperparams_xgb$eta[j],
                                                         subsample = hyperparams_xgb$subsample[j],
                                                         min_child_weight = hyperparams_xgb$min_child_weight[j],
                                                         gamma = hyperparams_xgb$gamma[j],
                                                         colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                                                         ROC_AUC_valid_lf = ROC_AUC_valid_lf))
  }
  
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, 'xgb_random_results.csv'))
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, 'xgb_random_results_past.csv'))
  write.csv(results_past_lf, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, 'xgb_random_results_past_lf.csv'))
}




random_lgb <- function(hyperparams_lgbm, in_time_data, pa_data, out_of_time_data, model_name, k = 5) {
  
  set.seed(21)
  folds <- createFolds(pa_data$occurrenceStatus, k = k, list = TRUE)
  
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()
  
  features <- setdiff(names(in_time_data), "occurrenceStatus")
  
  for (j in 1:nrow(hyperparams_lgbm)) {
    cat("Hyperparameter set: ", j, "\n")
    fold_ROC_AUC <- c()
    last_fold_model <- NULL
    
    for (fold in 1:k) {
      trainSet <- unlist(folds[-fold])
      testSet <- unlist(folds[fold])
      
      train <- in_time_data[trainSet, ]
      test <- in_time_data[testSet, ]
      valid <- in_time_data_past
      
      train <- train[complete.cases(train), ]
      test <- test[complete.cases(test), ]
      valid <- valid[complete.cases(valid), ]
      
      y_train <- train$occurrenceStatus
      
      lgb_train <- lgb.Dataset(data = as.matrix(train[, features]), label = y_train)
      
      lgb_params <- list(
        objective = "binary",
        metric = "auc",
        boosting_type = "gbdt",
        num_iterations = hyperparams_lgbm$num_iterations[j],
        num_leaves = hyperparams_lgbm$num_leaves[j],
        learning_rate = hyperparams_lgbm$learning_rate[j],
        subsample = hyperparams_lgbm$subsample[j],
        colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
        verbose = -1
      )
      
      lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
      
      prob_predictions <- predict(lgb_model, as.matrix(test[, features]))
      ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
      
      if (fold == k) {
        last_fold_model <- lgb_model
      }
    }
    
    results <- rbind(results, data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(fold_ROC_AUC)
    ))
    
    # Full model retraining on in_time_data
    lgb_full <- lgb.Dataset(data = as.matrix(in_time_data[, features]), label = in_time_data$occurrenceStatus)
    lgb_model_full <- lgb.train(params = lgb_params, data = lgb_full)
    
    valid_predictions <- predict(lgb_model_full, as.matrix(out_of_time_data[, features]))
    ROC_AUC_valid <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions)
    
    results_past <- rbind(results_past, data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_valid = ROC_AUC_valid
    ))
    
    valid_predictions_lf <- predict(last_fold_model, as.matrix(out_of_time_data[, features]))
    ROC_AUC_valid_lf <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_valid_lf = ROC_AUC_valid_lf
    ))
  }
  
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_lgb_random_results.csv'), row.names = FALSE)
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_lgb_random_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_lgb_random_results_past_lf.csv'), row.names = FALSE)
}







######################################################################## --- ENVIRONMENTAL BLOCKING --- ##################################################################

env_gbm <- function(hyperparams, cluster_data, cluster_count, in_time_data, out_of_time_data, model_name){
  
  features <- setdiff(names(in_time_data), "occurrenceStatus")
  valid <- out_of_time_data[, c(features, "occurrenceStatus"), drop = FALSE]
  # Initialize an empty data frame to store results
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()

  # Loop through each unique set of hyperparameters
  for (j in 1:nrow(hyperparams)) {
    print(paste0("Iteration: ", j))
    
    # Initialize variable to store ROC AUC for the current hyperparameter combination
    fold_ROC_AUC <- c()
    
    # Loop through each cluster as the test set
    for (test_cluster in 1:cluster_count) {
      # Get the indices for training clusters
      train_clusters <- setdiff(1:cluster_count, test_cluster)
      
      # Initialize variable to store ROC AUC for the current test set
      test_set_ROC_AUC_current <- c()
      
      # Loop through all training clusters
      for (fold in train_clusters) {
        
        train <- cluster_data[[fold]]
        test <- cluster_data[[test_cluster]]

        gbm_model <- gbm(
          formula = occurrenceStatus ~ ., distribution = 'bernoulli',
          data = train,
          n.trees = hyperparams$n.trees[j],
          interaction.depth = hyperparams$interaction.depth[j],
          shrinkage = hyperparams$shrinkage[j],
          n.minobsinnode = hyperparams$n.minobsinnode[j],
          verbose = FALSE
        )
        
        # Make predictions on the testing set
        prob_predictions <- predict(gbm_model, newdata = test, type = "response")
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == tail(train_clusters, 1)) {
          last_fold_model <- gbm_model
        }
      }
    }
    
    result_row <- data.frame(
      iteration = j,
      n.trees = hyperparams$n.trees[j],  # Include hyperparameter information
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(fold_ROC_AUC)
    )

    results <- rbind(results, result_row)
    
    
    gbm_model <- gbm(formula = occurrenceStatus ~ ., distribution = 'bernoulli',
                     data = in_time_data, n.trees = hyperparams$n.trees[j],
                     interaction.depth = hyperparams$interaction.depth[j],
                     shrinkage = hyperparams$shrinkage[j],
                     n.minobsinnode = hyperparams$n.minobsinnode[j],
                     verbose = FALSE)
    
    valid_predictions <- predict(gbm_model, newdata = valid, type = "response")
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)

    
    result_past_row <- data.frame(
      iteration = j,
      n.trees = hyperparams$n.trees[j],  # Include hyperparameter information
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      ROC_AUC_valid = ROC_AUC_valid
    )
    results_past <- rbind(results_past, result_past_row)
    
    valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "response")
    ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
    
    result_past_row_lf <- data.frame(
      iteration = j,
      n.trees = hyperparams$n.trees[j],  # Include hyperparameter information
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      ROC_AUC_valid_lf = ROC_AUC_valid_lf
    )
    results_past_lf <- rbind(results_past_lf, result_past_row_lf)
  }
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_env_results.csv'), row.names = FALSE)
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_env_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_gbm_env_results_past_lf.csv'), row.names = FALSE)
}



env_rf <- function(hyperparams_rf, cluster_data, cluster_count, in_time_data, out_of_time_data, model_name) {
  
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()
  
  in_time_data$occurrenceStatus <- factor(in_time_data$occurrenceStatus)
  out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus)
  valid <- out_of_time_data
  
  for (j in 1:nrow(hyperparams_rf)) {
    print(paste0("Iteration: ", j))
    
    fold_ROC_AUC <- c()
    
    for (test_cluster in 1:cluster_count) {
      train_clusters <- setdiff(1:cluster_count, test_cluster)
      
      for (fold in train_clusters) {
        train <- cluster_data[[fold]]
        test  <- cluster_data[[test_cluster]]
        
        train$occurrenceStatus <- factor(train$occurrenceStatus)
        test$occurrenceStatus <- factor(test$occurrenceStatus)
        
        maxnodes_val <- if (is.na(hyperparams_rf$maxnodes[j])) NULL else hyperparams_rf$maxnodes[j]
        
        rf_model <- randomForest(
          occurrenceStatus ~ .,
          data     = train,
          ntree    = hyperparams_rf$n.trees[j],
          nodesize = hyperparams_rf$nodesize[j],
          maxnodes = maxnodes_val
        )
        
        prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == tail(train_clusters, 1)) {
          last_fold_model <- rf_model
        }
      }
    }
    
    results <- rbind(results, data.frame(
      ntree     = hyperparams_rf$n.trees[j],
      nodesize  = hyperparams_rf$nodesize[j],
      maxnodes  = hyperparams_rf$maxnodes[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(fold_ROC_AUC)
    ))
    
    # Retrain on full data
    maxnodes_val <- if (is.na(hyperparams_rf$maxnodes[j])) NULL else hyperparams_rf$maxnodes[j]
    
    rf_model <- randomForest(
      occurrenceStatus ~ .,
      data     = in_time_data,
      ntree    = hyperparams_rf$n.trees[j],
      nodesize = hyperparams_rf$nodesize[j],
      maxnodes = maxnodes_val
    )
    
    valid_predictions <- predict(rf_model, newdata = valid, type = "prob")[, 2]
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
    
    results_past <- rbind(results_past, data.frame(
      ntree     = hyperparams_rf$n.trees[j],
      nodesize  = hyperparams_rf$nodesize[j],
      maxnodes  = hyperparams_rf$maxnodes[j],
      ROC_AUC_valid = ROC_AUC_valid
    ))
    
    valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "prob")[, 2]
    ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(
      ntree     = hyperparams_rf$n.trees[j],
      nodesize  = hyperparams_rf$nodesize[j],
      maxnodes  = hyperparams_rf$maxnodes[j],
      ROC_AUC_valid_lf = ROC_AUC_valid_lf
    ))
  }
  
  write.csv(results, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_env_results.csv'), row.names = FALSE)
  write.csv(results_past, paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_env_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf,paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_rf_env_results_past_lf.csv'), row.names = FALSE)
}



env_xgb <- function(hyperparams_xgb, cluster_data, cluster_count, in_time_data, out_of_time_data, model_name) {
  
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()
  
  feature_names <- colnames(in_time_data)[-ncol(in_time_data)]
  
  for (j in 1:nrow(hyperparams_xgb)) {
    print(paste0("Iteration: ", j))
    fold_ROC_AUC <- c()
    
    for (test_cluster in 1:cluster_count) {
      train_clusters <- setdiff(1:cluster_count, test_cluster)
      
      for (fold in train_clusters) {
        train <- cluster_data[[fold]]
        test  <- cluster_data[[test_cluster]]
        
        X_train <- as.matrix(train[, -ncol(train)])
        colnames(X_train) <- feature_names
        
        xgb_model <- xgboost(
          data = X_train,
          label = train$occurrenceStatus,
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          objective = "binary:logistic",
          eval_metric = "auc",
          verbose = 0
        )
        
        X_test <- as.matrix(test[, -ncol(test)])
        colnames(X_test) <- feature_names
        
        prob_predictions <- predict(xgb_model, newdata = X_test)
        ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == tail(train_clusters, 1)) {
          last_fold_model <- xgb_model
        }
      }
    }
    
    # Save cross-validation results
    results <- rbind(results, data.frame(
      iteration = j,
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    ))
    
    # Retrain on all in_time_data
    X_full_train <- as.matrix(in_time_data[, -ncol(in_time_data)])
    colnames(X_full_train) <- feature_names
    
    xgb_model_full <- xgboost(
      data = X_full_train,
      label = in_time_data$occurrenceStatus,
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      objective = "binary:logistic",
      eval_metric = "auc",
      verbose = 0
    )
    
    X_valid <- as.matrix(out_of_time_data[, -ncol(out_of_time_data)])
    colnames(X_valid) <- feature_names
    
    valid_predictions <- predict(xgb_model_full, newdata = X_valid)
    ROC_AUC_valid <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions)
    
    results_past <- rbind(results_past, data.frame(
      iteration = j,
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      ROC_AUC_valid = ROC_AUC_valid
    ))
    
    valid_predictions_lf <- predict(last_fold_model, newdata = X_valid)
    ROC_AUC_valid_lf <- pROC::auc(out_of_time_data$occurrenceStatus, valid_predictions_lf)
    
    results_past_lf <- rbind(results_past_lf, data.frame(
      iteration = j,
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      ROC_AUC_valid_lf = ROC_AUC_valid_lf  
    ))
  }
  
  write.csv(results,        paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_xgb_env_results.csv'), row.names = FALSE)
  write.csv(results_past,   paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_xgb_env_results_past.csv'), row.names = FALSE)
  write.csv(results_past_lf,paste0('C:/Users/User/Downloads/Downloads/phd_project/results/hpm/', model_name, '_xgb_env_results_past_lf.csv'), row.names = FALSE)
}





env_lgb <- function(hyperparams_lgbm, cluster_data, cluster_count, in_time_data, out_of_time_data, model_name) {
  
  results <- data.frame()
  results_past <- data.frame()
  results_past_lf <- data.frame()
  
  features <- setdiff(names(in_time_data), "occurrenceStatus")
  valid <- out_of_time_data
  
  for (j in 1:nrow(hyperparams_lgbm)) {
    print(paste0("Iteration: ", j))
    
    fold_ROC_AUC <- c()
    
    for (test_cluster in 1:cluster_count) {
      train_clusters <- setdiff(1:cluster_count, test_cluster)
      
      for (fold in train_clusters) {
        train <- cluster_data[[fold]]
        test  <- cluster_data[[test_cluster]]
        
        y_train <- train$occurrenceStatus
        y_test <- test$occurrenceStatus
        
        lgb_train <- lgb.Dataset(data = as.matrix(train[, features]), label = y_train)
        
        lgb_params <- list(
          objective = "binary",
          metric = "auc",
          boosting_type = "gbdt",
          num_iterations = hyperparams_lgbm$num_iterations[j],
          num_leaves = hyperparams_lgbm$num_leaves[j],
          learning_rate = hyperparams_lgbm$learning_rate[j],
          subsample = hyperparams_lgbm$subsample[j],
          colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
          verbose = -1
        )
        
        lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
        
        prob_predictions <- predict(lgb_model, as.matrix(test[, features]))
        ROC_AUC <- pROC::auc(y_test, prob_predictions)
        fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
        
        if (fold == tail(train_clusters, 1)) {
          last_fold_model <- lgb_model
        }
      }
    }
    
    result_row <- data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    )
    results <- rbind(results, result_row)
    
    lgb_in_time_data <- lgb.Dataset(data = as.matrix(in_time_data[, features]), label = in_time_data$occurrenceStatus)
    lgb_model_final <- lgb.train(params = lgb_params, data = lgb_in_time_data)
    
    valid_predictions <- predict(lgb_model_final, as.matrix(valid[, features]))
    ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
    
    result_past_row <- data.frame(
      iteration = j,
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_valid = ROC_AUC_valid
    )
    results_past <- rbind(results_past, result_past_row)
    
    valid_predictions_lf <- predict(last_fold_model, as.matrix(valid[, features]))
    ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
    
    result_past_row_lf <- data.frame(
      iteration = j,
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_valid_lf = ROC_AUC_valid_lf
    )
    results_past_lf <- rbind(results_past_lf, result_past_row_lf)
  }
  write.csv(results,        paste0(your_dir, model_name, "_lgb_env_results.csv"), row.names = FALSE)
  write.csv(results_past,   paste0(your_dir, model_name, "_lgb_env_results_past.csv"), row.names = FALSE)
  write.csv(results_past_lf,paste0(your_dir, model_name, "_lgb_env_results_past_lf.csv"), row.names = FALSE)
}


##############################################################333 --- SPATIO-TEMPORAL BLOCKING --- ######################################################################

spt_gbm <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams, in_time_data, out_of_time_data, k=5) {
  
  results_list <- list() 
  results_past_list <- list()
  results_past_list_lf <- list()
  
  for (i in 1:length(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    valid <- out_of_time_data
    
    for (block_idx in seq_along(block_sizes)) {
      block_size <- block_sizes[block_idx]
      model_name <- model_names[block_idx]
      
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = k,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams)) {
        print(paste0("Crop ", i, " | Block ", block_size, " | Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:k) {
          trainSet <- unlist(folds[[fold]][1])
          testSet  <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]  
          test  <- interval_data[testSet, ]
          
          gbm_model <- gbm(occurrenceStatus ~ ., distribution = 'bernoulli',
                           data = train,
                           n.trees = hyperparams$n.trees[j],
                           interaction.depth = hyperparams$interaction.depth[j],
                           shrinkage = hyperparams$shrinkage[j],
                           n.minobsinnode = hyperparams$n.minobsinnode[j],
                           verbose = FALSE)
          
          prob_predictions <- predict(gbm_model, newdata = test, type = "response")
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (fold == k) last_fold_model <- gbm_model
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          n.trees = hyperparams$n.trees[j],
          interaction.depth = hyperparams$interaction.depth[j],
          shrinkage = hyperparams$shrinkage[j],
          n.minobsinnode = hyperparams$n.minobsinnode[j],
          mean_ROC_AUC = mean(fold_ROC_AUC)
          # Optionally include fold_ROC_AUC as string, but don't aggregate it later
        )
        
        # Full retrain
        gbm_model_full <- gbm(occurrenceStatus ~ ., distribution = 'bernoulli',
                              data = in_time_data,
                              n.trees = hyperparams$n.trees[j],
                              interaction.depth = hyperparams$interaction.depth[j],
                              shrinkage = hyperparams$shrinkage[j],
                              n.minobsinnode = hyperparams$n.minobsinnode[j],
                              verbose = FALSE)
        
        valid_predictions <- predict(gbm_model_full, newdata = valid, type = "response")
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past_list[[length(results_past_list) + 1]] <- data.frame(
          n.trees = hyperparams$n.trees[j],
          interaction.depth = hyperparams$interaction.depth[j],
          shrinkage = hyperparams$shrinkage[j],
          n.minobsinnode = hyperparams$n.minobsinnode[j],
          ROC_AUC_valid = ROC_AUC_valid
        )
        
        valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "response")
        ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
        
        results_past_list_lf[[length(results_past_list_lf) + 1]] <- data.frame(
          n.trees = hyperparams$n.trees[j],
          interaction.depth = hyperparams$interaction.depth[j],
          shrinkage = hyperparams$shrinkage[j],
          n.minobsinnode = hyperparams$n.minobsinnode[j],
          ROC_AUC_valid_lf = ROC_AUC_valid_lf
        )
      }
    }
  }
  
  # Aggregate across all crops and block sizes
  results <- do.call(rbind, results_list)
  results_past <- do.call(rbind, results_past_list)
  results_past_lf <- do.call(rbind, results_past_list_lf)
  
  agg_results <- aggregate(mean_ROC_AUC ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                           data = results, mean)
  
  agg_results_past <- aggregate(ROC_AUC_valid ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                                data = results_past, mean)
  
  agg_results_past_lf <- aggregate(ROC_AUC_valid_lf ~ n.trees + interaction.depth + shrinkage + n.minobsinnode,
                                   data = results_past_lf, mean)
  
  write.csv(agg_results, paste0(your_dir, model_name, "_gbm_spt_results.csv"), row.names = FALSE)
  write.csv(agg_results_past, paste0(your_dir, model_name, "_gbm_spt_results_past.csv"), row.names = FALSE)
  write.csv(agg_results_past_lf, paste0(your_dir, model_name, "_gbm_spt_results_past_lf.csv"), row.names = FALSE)
}




spt_rf <- function(time_cropped, time_pa_data, time_intervals_data,block_sizes, model_names, hyperparams_rf,
                            in_time_data, out_of_time_data, k = 5) {
  
  results_list <- list()
  results_past_list <- list()
  results_past_list_lf <- list()
  
  in_time_data$occurrenceStatus <- factor(in_time_data$occurrenceStatus)
  out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus)
  
  for (i in seq_along(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    valid <- out_of_time_data
    
    for (block_idx in seq_along(block_sizes)) {
      block_size <- block_sizes[block_idx]
      model_name <- model_names[block_idx]
      
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = k,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_rf)) {
        print(paste0("Crop ", i, " | Block ", block_size, " | Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:k) {
          trainSet <- unlist(folds[[fold]][1])
          testSet  <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test  <- interval_data[testSet, ]
          
          train$occurrenceStatus <- factor(train$occurrenceStatus)
          test$occurrenceStatus  <- factor(test$occurrenceStatus)
          
          rf_model <- randomForest(occurrenceStatus ~ .,
                                   data = train,
                                   ntree = hyperparams_rf$n.trees[j],
                                   nodesize = hyperparams_rf$nodesize[j],
                                   maxnodes = hyperparams_rf$maxnodes[j])
          
          prob_predictions <- predict(rf_model, newdata = test, type = "prob")[, 2]
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (fold == k) {
            last_fold_model <- rf_model
          }
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          n.trees = hyperparams_rf$n.trees[j],
          nodesize = hyperparams_rf$nodesize[j],
          maxnodes = hyperparams_rf$maxnodes[j],
          mean_ROC_AUC = mean(fold_ROC_AUC)
        )
        
        rf_model_full <- randomForest(occurrenceStatus ~ .,
                                      data = in_time_data,
                                      ntree = hyperparams_rf$n.trees[j],
                                      nodesize = hyperparams_rf$nodesize[j],
                                      maxnodes = hyperparams_rf$maxnodes[j])
        
        valid_predictions <- predict(rf_model_full, newdata = valid, type = "prob")[, 2]
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past_list[[length(results_past_list) + 1]] <- data.frame(
          n.trees = hyperparams_rf$n.trees[j],
          nodesize = hyperparams_rf$nodesize[j],
          maxnodes = hyperparams_rf$maxnodes[j],
          ROC_AUC_valid = ROC_AUC_valid
        )
        
        # Last fold model on valid
        valid_predictions_lf <- predict(last_fold_model, newdata = valid, type = "prob")[, 2]
        ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
        
        results_past_list_lf[[length(results_past_list_lf) + 1]] <- data.frame(
          n.trees = hyperparams_rf$n.trees[j],
          nodesize = hyperparams_rf$nodesize[j],
          maxnodes = hyperparams_rf$maxnodes[j],
          ROC_AUC_valid_lf = ROC_AUC_valid_lf
        )
      }
    }
  }
  
  # Combine and aggregate
  results <- do.call(rbind, results_list)
  results_past <- do.call(rbind, results_past_list)
  results_past_lf <- do.call(rbind, results_past_list_lf)
  
  agg_results <- aggregate(mean_ROC_AUC ~ n.trees + nodesize + maxnodes,
                           data = results, FUN = mean)
  
  agg_results_past <- aggregate(ROC_AUC_valid ~ n.trees + nodesize + maxnodes,
                                data = results_past, FUN = mean)
  
  agg_results_past_lf <- aggregate(ROC_AUC_valid_lf ~ n.trees + nodesize + maxnodes,
                                   data = results_past_lf, FUN = mean)
  
  write.csv(agg_results, paste0(your_dir, model_name, "_results.csv"), row.names = FALSE)
  write.csv(agg_results_past, paste0(your_dir, model_name, "_results_past.csv"), row.names = FALSE)
  write.csv(agg_results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"), row.names = FALSE)
}




spt_xgb <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_xgb, in_time_data, out_of_time_data, k = 5) {
  
  results_list <- list()
  results_past_list <- list()
  results_past_list_lf <- list()
  
  for (i in seq_along(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    valid <- out_of_time_data
    
    for (block_idx in seq_along(block_sizes)) {
      block_size <- block_sizes[block_idx]
      model_name <- model_names[block_idx]
      
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = k,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_xgb)) {
        cat("Crop", i, "| Block", block_size, "| Iteration:", j, "\n")
        fold_ROC_AUC <- c()
        
        for (fold in 1:k) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                               label = as.numeric(train$occurrenceStatus),
                               nrounds = hyperparams_xgb$nrounds[j],
                               max_depth = hyperparams_xgb$max_depth[j],
                               eta = hyperparams_xgb$eta[j],
                               subsample = hyperparams_xgb$subsample[j],
                               min_child_weight = hyperparams_xgb$min_child_weight[j],
                               gamma = hyperparams_xgb$gamma[j],
                               colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                               objective = "binary:logistic",
                               eval_metric = "auc",
                               verbose = 0)
          
          prob_predictions <- predict(xgb_model, as.matrix(test[, -ncol(test)]))
          ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (fold == k) {
            last_fold_model <- xgb_model
          }
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          mean_ROC_AUC = mean(fold_ROC_AUC)
        )
        
        # Full retrain on in_time_data
        xgb_model <- xgboost(data = as.matrix(in_time_data[, -ncol(in_time_data)]),
                             label = as.numeric(in_time_data$occurrenceStatus),
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc",
                             verbose = 0)
        
        valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]))
        ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past_list[[length(results_past_list) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          ROC_AUC_valid = ROC_AUC_valid
        )
        
        valid_predictions_lf <- predict(last_fold_model, newdata = as.matrix(valid[, -ncol(valid)]))
        ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
        
        results_past_list_lf[[length(results_past_list_lf) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          ROC_AUC_valid_lf = ROC_AUC_valid_lf
        )
      }
    }
  }
  

  agg_results <- aggregate(mean_ROC_AUC ~ nrounds + max_depth + eta + subsample + min_child_weight +
                             gamma + colsample_bylevel, data = do.call(rbind, results_list), mean)
  
  agg_results_past <- aggregate(ROC_AUC_valid ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                  gamma + colsample_bylevel, data = do.call(rbind, results_past_list), mean)
  
  agg_results_past_lf <- aggregate(ROC_AUC_valid_lf ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                     gamma + colsample_bylevel, data = do.call(rbind, results_past_list_lf), mean)
  
  write.csv(agg_results, paste0(your_dir, model_name, "_results.csv"), row.names = FALSE)
  write.csv(agg_results_past, paste0(your_dir, model_name, "_results_past.csv"), row.names = FALSE)
  write.csv(agg_results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"), row.names = FALSE)
}





spt_xgb <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_xgb, in_time_data, out_of_time_data, k = 5) {
  
  results_list <- list()
  results_past_list <- list()
  results_past_list_lf <- list()
  
  for (i in seq_along(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    valid <- out_of_time_data
    
    for (block_idx in seq_along(block_sizes)) {
      block_size <- block_sizes[block_idx]
      model_name <- model_names[block_idx]
      
      set.seed(1)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = k,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_xgb)) {
        cat("Crop", i, "| Block", block_size, "| Iteration:", j, "\n")
        fold_ROC_AUC <- c()
        
        for (fold in 1:k) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          xgb_model <- xgboost(data = as.matrix(train[, -ncol(train)]),
                               label = as.numeric(train$occurrenceStatus),
                               nrounds = hyperparams_xgb$nrounds[j],
                               max_depth = hyperparams_xgb$max_depth[j],
                               eta = hyperparams_xgb$eta[j],
                               subsample = hyperparams_xgb$subsample[j],
                               min_child_weight = hyperparams_xgb$min_child_weight[j],
                               gamma = hyperparams_xgb$gamma[j],
                               colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                               objective = "binary:logistic",
                               eval_metric = "auc",
                               verbose = 0)
          
          prob_predictions <- predict(xgb_model, as.matrix(test[, -ncol(test)]))
          #ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
          
          if (length(unique(test$occurrenceStatus)) == 2) {
            ROC_AUC <- pROC::auc(test$occurrenceStatus, prob_predictions)
            fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          } else {
            cat("Skipping fold due to single-class test set\n")
          }
          
          
          #fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (fold == k) {
            last_fold_model <- xgb_model
          }
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          mean_ROC_AUC = mean(fold_ROC_AUC)
        )
        
        # Full retrain on in_time_data
        xgb_model <- xgboost(data = as.matrix(in_time_data[, -ncol(in_time_data)]),
                             label = as.numeric(in_time_data$occurrenceStatus),
                             nrounds = hyperparams_xgb$nrounds[j],
                             max_depth = hyperparams_xgb$max_depth[j],
                             eta = hyperparams_xgb$eta[j],
                             subsample = hyperparams_xgb$subsample[j],
                             min_child_weight = hyperparams_xgb$min_child_weight[j],
                             gamma = hyperparams_xgb$gamma[j],
                             colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
                             objective = "binary:logistic",
                             eval_metric = "auc",
                             verbose = 0)
        
        
        valid_predictions <- predict(xgb_model, newdata = as.matrix(valid[, -ncol(valid)]))
        
        if (length(unique(valid$occurrenceStatus)) == 2) {
          ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        } else {
          ROC_AUC_valid <- NA
          cat("Skipping validation AUC (retrain) due to single-class\n")
        }
        
        #ROC_AUC_valid <- pROC::auc(valid$occurrenceStatus, valid_predictions)
        
        results_past_list[[length(results_past_list) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          ROC_AUC_valid = ROC_AUC_valid
        )
        
        valid_predictions_lf <- predict(last_fold_model, newdata = as.matrix(valid[, -ncol(valid)]))
        #ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
        
        if (length(unique(valid$occurrenceStatus)) == 2) {
          ROC_AUC_valid_lf <- pROC::auc(valid$occurrenceStatus, valid_predictions_lf)
        } else {
          ROC_AUC_valid_lf <- NA
          cat("Skipping validation AUC (last fold) due to single-class\n")
        }
        
        
        results_past_list_lf[[length(results_past_list_lf) + 1]] <- data.frame(
          nrounds = hyperparams_xgb$nrounds[j],
          max_depth = hyperparams_xgb$max_depth[j],
          eta = hyperparams_xgb$eta[j],
          subsample = hyperparams_xgb$subsample[j],
          min_child_weight = hyperparams_xgb$min_child_weight[j],
          gamma = hyperparams_xgb$gamma[j],
          colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
          ROC_AUC_valid_lf = ROC_AUC_valid_lf
        )
      }
    }
  }
  
  
  agg_results <- aggregate(mean_ROC_AUC ~ nrounds + max_depth + eta + subsample + min_child_weight +
                             gamma + colsample_bylevel, data = do.call(rbind, results_list), mean)
  
  agg_results_past <- aggregate(ROC_AUC_valid ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                  gamma + colsample_bylevel, data = do.call(rbind, results_past_list), mean)
  
  agg_results_past_lf <- aggregate(ROC_AUC_valid_lf ~ nrounds + max_depth + eta + subsample + min_child_weight +
                                     gamma + colsample_bylevel, data = do.call(rbind, results_past_list_lf), mean)
  
  write.csv(agg_results, paste0(your_dir, model_name, "_results.csv"), row.names = FALSE)
  write.csv(agg_results_past, paste0(your_dir, model_name, "_results_past.csv"), row.names = FALSE)
  write.csv(agg_results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"), row.names = FALSE)
}





spt_lgb <- function(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams_lgbm, in_time_data, out_of_time_data, k=5) {
  
  results_list <- list()
  results_past_list <- list()
  results_past_list_lf <- list()
  
  features <- setdiff(names(in_time_data), "occurrenceStatus")
  
  # Ensure labels are numeric (LightGBM requires 0/1)
  in_time_data$occurrenceStatus <- as.numeric(as.character(in_time_data$occurrenceStatus))
  out_of_time_data$occurrenceStatus <- as.numeric(as.character(out_of_time_data$occurrenceStatus))
  
  for (i in seq_along(time_cropped)) {
    crop <- time_cropped[[i]]
    pa <- time_pa_data[[i]]
    interval_data <- time_intervals_data[[i]]
    
    for (block_idx in seq_along(block_sizes)) {
      block_size <- block_sizes[block_idx]
      model_name <- model_names[block_idx]
      
      set.seed(21)
      sb <- cv_spatial(x = pa,
                       column = "occurrenceStatus",
                       r = crop,
                       k = k,
                       size = block_size,
                       selection = "random",
                       iteration = 50,
                       biomod2 = TRUE)
      
      folds <- sb$folds_list
      
      for (j in 1:nrow(hyperparams_lgbm)) {
        print(paste0("Iteration: ", j))
        
        fold_ROC_AUC <- c()
        
        for (fold in 1:k) {
          trainSet <- unlist(folds[[fold]][1])
          testSet <- unlist(folds[[fold]][2])
          
          train <- interval_data[trainSet, ]
          test <- interval_data[testSet, ]
          
          # Convert labels to numeric
          y_train <- as.numeric(as.character(train$occurrenceStatus))
          y_test <- as.numeric(as.character(test$occurrenceStatus))
          
          # Convert features to numeric matrix
          train[, features] <- lapply(train[, features], function(x) as.numeric(as.character(x)))
          test[, features] <- lapply(test[, features], function(x) as.numeric(as.character(x)))
          
          lgb_train <- lgb.Dataset(as.matrix(train[, features]), label = y_train)
          
          lgb_params <- list(objective = "binary",
                             metric = "auc",
                             boosting_type = "gbdt",
                             num_iterations = hyperparams_lgbm$num_iterations[j],
                             num_leaves = hyperparams_lgbm$num_leaves[j],
                             learning_rate = hyperparams_lgbm$learning_rate[j],
                             subsample = hyperparams_lgbm$subsample[j],
                             colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
                             verbose = -1)
          
          lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
          prob_predictions <- predict(lgb_model, as.matrix(test[, features]))
          
          ROC_AUC <- pROC::auc(y_test, prob_predictions)
          fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
          
          if (fold == k) {
            last_fold_model <- lgb_model
          }
        }
        
        results_list[[length(results_list) + 1]] <- data.frame(
          num_iterations = hyperparams_lgbm$num_iterations[j],
          num_leaves = hyperparams_lgbm$num_leaves[j],
          learning_rate = hyperparams_lgbm$learning_rate[j],
          subsample = hyperparams_lgbm$subsample[j],
          colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
          mean_ROC_AUC = mean(fold_ROC_AUC)
        )
        
        # Full model training
        in_time_data[, features] <- lapply(in_time_data[, features], function(x) as.numeric(as.character(x)))
        valid <- out_of_time_data
        valid[, features] <- lapply(valid[, features], function(x) as.numeric(as.character(x)))
        y_valid <- as.numeric(as.character(valid$occurrenceStatus))
        y_in_time_data <- as.numeric(as.character(in_time_data$occurrenceStatus))
        
        lgb_in_time_data <- lgb.Dataset(as.matrix(in_time_data[, features]), label = y_in_time_data)
        lgb_model_full <- lgb.train(params = lgb_params, data = lgb_in_time_data)
        
        valid_predictions <- predict(lgb_model_full, as.matrix(valid[, features]))
        ROC_AUC_valid <- pROC::auc(y_valid, valid_predictions)
        
        results_past_list[[length(results_past_list) + 1]] <- data.frame(
          num_iterations = hyperparams_lgbm$num_iterations[j],
          num_leaves = hyperparams_lgbm$num_leaves[j],
          learning_rate = hyperparams_lgbm$learning_rate[j],
          subsample = hyperparams_lgbm$subsample[j],
          colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
          ROC_AUC_valid = ROC_AUC_valid
        )
        
        valid_predictions_lf <- predict(last_fold_model, as.matrix(valid[, features]))
        ROC_AUC_valid_lf <- pROC::auc(y_valid, valid_predictions_lf)
        
        results_past_list_lf[[length(results_past_list_lf) + 1]] <- data.frame(
          num_iterations = hyperparams_lgbm$num_iterations[j],
          num_leaves = hyperparams_lgbm$num_leaves[j],
          learning_rate = hyperparams_lgbm$learning_rate[j],
          subsample = hyperparams_lgbm$subsample[j],
          colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
          ROC_AUC_valid_lf = ROC_AUC_valid_lf
        )
      }
    }
  }
  
  # Combine and aggregate results
  agg_results <- aggregate(mean_ROC_AUC ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                           data = do.call(rbind, results_list), FUN = mean)
  
  agg_results_past <- aggregate(ROC_AUC_valid ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                                data = do.call(rbind, results_past_list), FUN = mean)
  
  agg_results_past_lf <- aggregate(ROC_AUC_valid_lf ~ num_iterations + num_leaves + learning_rate + subsample + colsample_bytree,
                                   data = do.call(rbind, results_past_list_lf), FUN = mean)
  
  write.csv(agg_results, paste0(your_dir, model_name, "_results.csv"), row.names = FALSE)
  write.csv(agg_results_past, paste0(your_dir, model_name, "_results_past.csv"), row.names = FALSE)
  write.csv(agg_results_past_lf, paste0(your_dir, model_name, "_results_past_lf.csv"), row.names = FALSE)
}

