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









                                    

########################################################## --- FORWARD CHAINING CV FUNCTIONS --- #####################################################################
forward_chaining_gbm <- function(time_intervals_data, hyperparams, out_of_time_data) {
  results_list <- list()
  results_past <- data.frame()
  
  # Loop over all hyperparameter configurations
  for (j in 1:nrow(hyperparams)) {
    fold_ROC_AUC <- c()  # Initialize vector to collect AUCs over folds
    
    # Forward-chaining folds
    folds <- list(
      list(train = 1,    test = 2),
      list(train = 1:2,  test = 3),
      list(train = 1:3,  test = 4)
    )
    
    for (fold_idx in seq_along(folds)) {
      fold <- folds[[fold_idx]]
      train_data <- do.call(rbind, time_intervals_data[fold$train])
      test_data  <- time_intervals_data[[fold$test]]
      
      # Train model
      gbm_model <- gbm(occurrenceStatus ~ ., distribution = "bernoulli",
                       data = train_data,
                       n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
      
      # Predict and calculate AUC
      preds <- predict(gbm_model, newdata = test_data, type = "response")
      auc_val <- pROC::auc(test_data$occurrenceStatus, preds)
      
      fold_ROC_AUC <- c(fold_ROC_AUC, auc_val)
    }
    
    # Save result row with mean and per-fold AUCs
    result_row <- data.frame(
      n.trees = hyperparams$n.trees[j],
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      mean_ROC_AUC = mean(fold_ROC_AUC, na.rm = TRUE),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    )
    
    results_list[[length(results_list) + 1]] <- result_row
  }
  
  # Final training on all 2003–2018 → test on out_of_time_data (1984–2002)
  final_train <- do.call(rbind, time_intervals_data)
  
  for (j in 1:nrow(hyperparams)) {
    final_model <- gbm(occurrenceStatus ~ ., distribution = "bernoulli",
                       data = final_train,
                       n.trees = hyperparams$n.trees[j],
                       interaction.depth = hyperparams$interaction.depth[j],
                       shrinkage = hyperparams$shrinkage[j],
                       n.minobsinnode = hyperparams$n.minobsinnode[j],
                       verbose = FALSE)
    
    final_preds <- predict(final_model, newdata = out_of_time_data, type = "response")
    auc_final <- pROC::auc(out_of_time_data$occurrenceStatus, final_preds)
    
    results_past <- rbind(results_past, data.frame(
      n.trees = hyperparams$n.trees[j],
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      ROC_AUC_valid = auc_final
    ))
  }
  
  # Save results
  results_df <- do.call(rbind, results_list)
  
  write.csv(results_df,
            "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/gbm_forw_chain_results.csv", row.names = FALSE)
  
  write.csv(results_past,
            "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/gbm_forw_chain_results_past.csv", row.names = FALSE)
}






forward_chaining_gbm_lf <- function(time_intervals_data, hyperparams, out_of_time_data, seed = 42) {
  stopifnot(is.list(time_intervals_data), length(time_intervals_data) >= 4)
  set.seed(seed)
  
  # Forward-chaining folds (train grows; validate on the next block)
  folds <- list(
    list(train = 1,    val = 2),
    list(train = 1:2,  val = 3),
    list(train = 1:3,  val = 4)
  )
  
  results_list <- vector("list", nrow(hyperparams))
  
  # 1) CV over the grid
  for (j in seq_len(nrow(hyperparams))) {
    fold_auc <- numeric(length(folds))
    
    for (k in seq_along(folds)) {
      tr_idx <- folds[[k]]$train
      va_idx <- folds[[k]]$val
      
      train_data <- do.call(rbind, time_intervals_data[tr_idx])
      val_data   <- time_intervals_data[[va_idx]]
      
      m <- gbm::gbm(
        occurrenceStatus ~ ., distribution = "bernoulli",
        data = train_data,
        n.trees = hyperparams$n.trees[j],
        interaction.depth = hyperparams$interaction.depth[j],
        shrinkage = hyperparams$shrinkage[j],
        n.minobsinnode = hyperparams$n.minobsinnode[j],
        verbose = FALSE
      )
      
      p <- predict(m, newdata = val_data, type = "response")
      fold_auc[k] <- as.numeric(pROC::auc(val_data$occurrenceStatus, p))
    }
    
    results_list[[j]] <- data.frame(
      n.trees = hyperparams$n.trees[j],
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      mean_ROC_AUC = mean(fold_auc, na.rm = TRUE),
      fold_ROC_AUC = paste(round(fold_auc, 4), collapse = ",")
    )
  }
  
  cv_results <- do.call(rbind, results_list)
  
  # 2) LAST-FOLD final training window (ONLY last TSS training window)
  last_tr_idx <- folds[[length(folds)]]$train
  final_train <- do.call(rbind, time_intervals_data[last_tr_idx])
  
  # 3) Evaluate on OOD test for ALL hyperparameter rows
  results_past_list <- vector("list", nrow(hyperparams))
  for (j in seq_len(nrow(hyperparams))) {
    final_model_j <- gbm::gbm(
      occurrenceStatus ~ ., distribution = "bernoulli",
      data = final_train,
      n.trees = hyperparams$n.trees[j],
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      verbose = FALSE
    )
    
    final_preds_j <- predict(final_model_j, newdata = out_of_time_data, type = "response")
    auc_final_j <- as.numeric(pROC::auc(out_of_time_data$occurrenceStatus, final_preds_j))
    
    results_past_list[[j]] <- data.frame(
      n.trees = hyperparams$n.trees[j],
      interaction.depth = hyperparams$interaction.depth[j],
      shrinkage = hyperparams$shrinkage[j],
      n.minobsinnode = hyperparams$n.minobsinnode[j],
      ROC_AUC_test = auc_final_j
    )
  }
  
  results_past <- do.call(rbind, results_past_list)
  
  # 4) Save
  utils::write.csv(
    cv_results,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/gbm_forw_chain_results_lf.csv",
    row.names = FALSE
  )
  
  utils::write.csv(
    results_past,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/gbm_forw_chain_results_past_lf.csv",
    row.names = FALSE
  )
  
  invisible(list(cv_results = cv_results, test_all = results_past))
}



forward_chaining_rf <- function(time_intervals_data, hyperparams_rf, out_of_time_data) {
  results_list <- list()
  results_past <- data.frame()
  
  # Define folds: forward-chaining setup
  folds <- list(
    list(train = 1,    test = 2),  # 2003–2006 → 2007–2010
    list(train = 1:2,  test = 3),  # 2003–2010 → 2011–2014
    list(train = 1:3,  test = 4)   # 2003–2014 → 2015–2018
  )
  
  for (j in 1:nrow(hyperparams_rf)) {
    fold_ROC_AUC <- c()
    
    for (fold_idx in seq_along(folds)) {
      fold <- folds[[fold_idx]]
      train_data <- do.call(rbind, time_intervals_data[fold$train])
      test_data  <- time_intervals_data[[fold$test]]
      
      # Convert to factors
      train_data$occurrenceStatus <- factor(train_data$occurrenceStatus)
      test_data$occurrenceStatus  <- factor(test_data$occurrenceStatus)
      
      rf_model <- randomForest(occurrenceStatus ~ .,
                               data = train_data,
                               ntree = hyperparams_rf$n.trees[j],
                               nodesize = hyperparams_rf$nodesize[j],
                               maxnodes = hyperparams_rf$maxnodes[j])
      
      # Predict and compute AUC
      prob_predictions <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
      auc_val <- pROC::auc(test_data$occurrenceStatus, prob_predictions)
      
      fold_ROC_AUC <- c(fold_ROC_AUC, auc_val)
    }
    
    # Store results per hyperparameter set
    result_row <- data.frame(
      n.trees = hyperparams_rf$n.trees[j],
      nodesize = hyperparams_rf$nodesize[j],
      maxnodes = hyperparams_rf$maxnodes[j],
      mean_ROC_AUC = mean(fold_ROC_AUC, na.rm = TRUE),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    )
    
    
    results_list[[length(results_list) + 1]] <- result_row
  }
  
  # Final training on full data (2003–2018) → test on past (1984–2002)
  final_train <- do.call(rbind, time_intervals_data)
  final_train$occurrenceStatus <- factor(final_train$occurrenceStatus)
  out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus)
  
  for (j in 1:nrow(hyperparams_rf)) {
    rf_model_final <- randomForest(occurrenceStatus ~ .,
                                   data = final_train,
                                   ntree = hyperparams_rf$n.trees[j],
                                   maxnodes = hyperparams_rf$max_depth[j],
                                   nodesize = hyperparams_rf$min_samples_split[j])
    
    final_probs <- predict(rf_model_final, newdata = out_of_time_data, type = "prob")[, 2]
    auc_final <- pROC::auc(out_of_time_data$occurrenceStatus, final_probs)
    
    results_past <- rbind(results_past, data.frame(
      n.trees = hyperparams_rf$n.trees[j],
      nodesize = hyperparams_rf$nodesize[j],
      maxnodes = hyperparams_rf$maxnodes[j],
      ROC_AUC_valid = auc_final
    ))
  }
  
  # Save CSVs
  results_df <- do.call(rbind, results_list)
  
  write.csv(results_df,
            "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/rf_forw_chain_results.csv", row.names = FALSE)
  
  write.csv(results_past,
            "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/rf_forw_chain_results_past.csv", row.names = FALSE)
}








forward_chaining_rf_lf <- function(time_intervals_data, hyperparams_rf, out_of_time_data, seed = 42) {
  stopifnot(is.list(time_intervals_data), length(time_intervals_data) >= 4)
  set.seed(seed)
  
  tids <- lapply(time_intervals_data, function(df) {
    df$occurrenceStatus <- factor(df$occurrenceStatus, levels = c(0, 1))
    df
  })
  out_of_time_data$occurrenceStatus <- factor(out_of_time_data$occurrenceStatus, levels = c(0, 1))
  
  folds <- list(
    list(train = 1,    val = 2),
    list(train = 1:2,  val = 3),
    list(train = 1:3,  val = 4)
  )
  
  results_list <- vector("list", nrow(hyperparams_rf))
  for (j in seq_len(nrow(hyperparams_rf))) {
    fold_auc <- numeric(length(folds))
    for (k in seq_along(folds)) {
      tr_idx <- folds[[k]]$train
      va_idx <- folds[[k]]$val
      
      train_data <- do.call(rbind, tids[tr_idx])
      val_data   <- tids[[va_idx]]
      
      rf_model <- randomForest::randomForest(
        occurrenceStatus ~ .,
        data     = train_data,
        ntree    = hyperparams_rf$n.trees[j],
        nodesize = hyperparams_rf$nodesize[j],
        maxnodes = hyperparams_rf$maxnodes[j]
      )
      
      prob_val <- predict(rf_model, newdata = val_data, type = "prob")[, 2]
      fold_auc[k] <- as.numeric(pROC::auc(val_data$occurrenceStatus, prob_val))
    }
    
    results_list[[j]] <- data.frame(
      n.trees          = hyperparams_rf$n.trees[j],
      nodesize         = hyperparams_rf$nodesize[j],
      maxnodes         = hyperparams_rf$maxnodes[j],
      ROC_AUC_CV_mean  = mean(fold_auc, na.rm = TRUE),
      ROC_AUC_CV_folds = paste(round(fold_auc, 4), collapse = ",")
    )
  }
  
  cv_results <- do.call(rbind, results_list)
  
  last_tr_idx <- folds[[length(folds)]]$train
  final_train <- do.call(rbind, tids[last_tr_idx])
  
  results_past_list <- vector("list", nrow(hyperparams_rf))
  for (j in seq_len(nrow(hyperparams_rf))) {
    rf_final_j <- randomForest::randomForest(
      occurrenceStatus ~ .,
      data     = final_train,
      ntree    = hyperparams_rf$n.trees[j],
      nodesize = hyperparams_rf$nodesize[j],
      maxnodes = hyperparams_rf$maxnodes[j]
    )
    prob_test_j <- predict(rf_final_j, newdata = out_of_time_data, type = "prob")[, 2]
    auc_test_j  <- as.numeric(pROC::auc(out_of_time_data$occurrenceStatus, prob_test_j))
    
    results_past_list[[j]] <- data.frame(
      n.trees      = hyperparams_rf$n.trees[j],
      nodesize     = hyperparams_rf$nodesize[j],
      maxnodes     = hyperparams_rf$maxnodes[j],
      ROC_AUC_test = auc_test_j
    )
  }
  
  results_past <- do.call(rbind, results_past_list)
  
  utils::write.csv(
    cv_results,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/rf_forw_chain_results_lf.csv",
    row.names = FALSE
  )
  
  utils::write.csv(
    results_past,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/rf_forw_chain_results_past_lf.csv",
    row.names = FALSE
  )
  
  invisible(list(cv_results = cv_results, test_all = results_past))
}




forward_chaining_xgb <- function(time_intervals_data, hyperparams_xgb, out_of_time_data) {
  results_list <- list()
  results_past <- data.frame()
  
  # Automatically detect features (assumes all folds have the same structure)
  features <- setdiff(colnames(time_intervals_data[[1]]), "occurrenceStatus")
  
  # Define forward-chaining folds
  folds <- list(
    list(train = 1,    test = 2),  # 2003–2006 → 2007–2010
    list(train = 1:2,  test = 3),  # 2003–2010 → 2011–2014
    list(train = 1:3,  test = 4)   # 2003–2014 → 2015–2018
  )
  
  for (j in 1:nrow(hyperparams_xgb)) {
    fold_ROC_AUC <- c()
    
    for (fold_idx in seq_along(folds)) {
      fold <- folds[[fold_idx]]
      train_data <- do.call(rbind, time_intervals_data[fold$train])
      test_data  <- time_intervals_data[[fold$test]]
      
      X_train <- as.matrix(train_data[, features])
      y_train <- train_data$occurrenceStatus
      X_test  <- as.matrix(test_data[, features])
      y_test  <- test_data$occurrenceStatus
      
      # Train XGBoost
      xgb_model <- xgboost(
        data = X_train,
        label = y_train,
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
      
      # Predict
      prob_predictions <- predict(xgb_model, newdata = X_test)
      auc_val <- pROC::auc(y_test, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, auc_val)
    }
    
    results_list[[length(results_list) + 1]] <- data.frame(
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      mean_ROC_AUC = mean(fold_ROC_AUC, na.rm = TRUE),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    )
  }
  
  # Final training: all (2003–2018) → test on past (1984–2002)
  final_train <- do.call(rbind, time_intervals_data)
  X_final <- as.matrix(final_train[, features])
  y_final <- final_train$occurrenceStatus
  X_past  <- as.matrix(out_of_time_data[, features])
  y_past  <- out_of_time_data$occurrenceStatus
  
  for (j in 1:nrow(hyperparams_xgb)) {
    final_model <- xgboost(
      data = X_final,
      label = y_final,
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
    
    past_probs <- predict(final_model, newdata = X_past)
    auc_final <- pROC::auc(y_past, past_probs)
    
    results_past <- rbind(results_past, data.frame(
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      ROC_AUC_valid = auc_final
    ))
  }
  
  # Save results
  results_df <- do.call(rbind, results_list)
  
  write.csv(results_df, "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/xgb_forw_chain_results.csv", row.names = FALSE)
  write.csv(results_past, "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/xgb_forw_chain_results_past.csv", row.names = FALSE)
}







forward_chaining_xgb_lf <- function(time_intervals_data, hyperparams_xgb, out_of_time_data, seed = 42) {
  set.seed(seed)
  stopifnot(is.list(time_intervals_data), length(time_intervals_data) >= 4)
  
  features <- setdiff(colnames(time_intervals_data[[1]]), "occurrenceStatus")
  
  folds <- list(
    list(train = 1,    val = 2),
    list(train = 1:2,  val = 3),
    list(train = 1:3,  val = 4)
  )
  
  # 1) CV over hyperparameter grid
  results_list <- vector("list", nrow(hyperparams_xgb))
  for (j in seq_len(nrow(hyperparams_xgb))) {
    fold_auc <- numeric(length(folds))
    for (k in seq_along(folds)) {
      tr_idx <- folds[[k]]$train
      va_idx <- folds[[k]]$val
      
      train_df <- do.call(rbind, time_intervals_data[tr_idx])
      val_df   <- time_intervals_data[[va_idx]]
      
      y_train <- as.numeric(train_df$occurrenceStatus)
      y_val   <- as.numeric(val_df$occurrenceStatus)
      X_train <- as.matrix(train_df[, features, drop = FALSE])
      X_val   <- as.matrix(val_df[, features,   drop = FALSE])
      
      xgb_model <- xgboost::xgboost(
        data = X_train,
        label = y_train,
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
      
      prob_val <- predict(xgb_model, newdata = X_val)
      fold_auc[k] <- as.numeric(pROC::auc(y_val, prob_val))
    }
    results_list[[j]] <- data.frame(
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      ROC_AUC_CV_mean = mean(fold_auc, na.rm = TRUE),
      ROC_AUC_CV_folds = paste(round(fold_auc, 4), collapse = ",")
    )
  }
  cv_results <- do.call(rbind, results_list)
  
  # 2) LAST-FOLD final training window (evaluate ALL configs)
  last_tr_idx <- folds[[length(folds)]]$train
  final_df <- do.call(rbind, time_intervals_data[last_tr_idx])
  X_final <- as.matrix(final_df[, features, drop = FALSE])
  y_final <- as.numeric(final_df$occurrenceStatus)
  
  X_past <- as.matrix(out_of_time_data[, features, drop = FALSE])
  y_past <- as.numeric(out_of_time_data$occurrenceStatus)
  
  results_past_list <- vector("list", nrow(hyperparams_xgb))
  for (j in seq_len(nrow(hyperparams_xgb))) {
    mdl_j <- xgboost::xgboost(
      data = X_final,
      label = y_final,
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
    prob_test <- predict(mdl_j, newdata = X_past)
    auc_test  <- as.numeric(pROC::auc(y_past, prob_test))
    
    results_past_list[[j]] <- data.frame(
      nrounds = hyperparams_xgb$nrounds[j],
      max_depth = hyperparams_xgb$max_depth[j],
      eta = hyperparams_xgb$eta[j],
      subsample = hyperparams_xgb$subsample[j],
      min_child_weight = hyperparams_xgb$min_child_weight[j],
      gamma = hyperparams_xgb$gamma[j],
      colsample_bylevel = hyperparams_xgb$colsample_bylevel[j],
      ROC_AUC_test = auc_test
    )
  }
  results_past <- do.call(rbind, results_past_list)
  
  # 3) Save
  utils::write.csv(
    cv_results,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/xgb_forw_chain_results_lf.csv",
    row.names = FALSE
  )
  utils::write.csv(
    results_past,
    "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/xgb_forw_chain_results_past_lf.csv",
    row.names = FALSE
  )
  
  invisible(list(cv_results = cv_results, test_all = results_past))
}






forward_chaining_lgb <- function(time_intervals_data, hyperparams_lgbm, out_of_time_data) {
  
  results_list <- list()
  results_past_list <- list()
  
  features <- setdiff(colnames(time_intervals_data[[1]]), "occurrenceStatus")
  
  # Define forward-chaining folds
  folds <- list(
    list(train = 1,    test = 2),
    list(train = 1:2,  test = 3),
    list(train = 1:3,  test = 4)
  )
  
  results <- data.frame()
  results_past <- data.frame()
  
  for (j in 1:nrow(hyperparams_lgbm)) {
    fold_ROC_AUC <- c()
    
    for (fold_idx in seq_along(folds)) {
      fold <- folds[[fold_idx]]
      train_data <- do.call(rbind, time_intervals_data[fold$train])
      test_data  <- time_intervals_data[[fold$test]]
      
      y_train <- train_data$occurrenceStatus
      y_test  <- test_data$occurrenceStatus
      
      lgb_train <- lgb.Dataset(as.matrix(train_data[, features]), label = y_train)
      
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
      
      prob_predictions <- predict(lgb_model, as.matrix(test_data[, features]))
      ROC_AUC <- pROC::auc(y_test, prob_predictions)
      fold_ROC_AUC <- c(fold_ROC_AUC, ROC_AUC)
    }
    
    results <- rbind(results, data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      mean_ROC_AUC = mean(fold_ROC_AUC),
      fold_ROC_AUC = toString(round(fold_ROC_AUC, 4))
    ))
    
    # Final model on full train (2003–2018) → test on past (1984–2002)
    final_train <- do.call(rbind, time_intervals_data)
    y_final <- final_train$occurrenceStatus
    y_valid <- out_of_time_data$occurrenceStatus
    
    lgb_final <- lgb.Dataset(as.matrix(final_train[, features]), label = y_final)
    
    lgb_model <- lgb.train(params = lgb_params, data = lgb_final)
    
    valid_predictions <- predict(lgb_model, as.matrix(out_of_time_data[, features]))
    ROC_AUC_valid <- pROC::auc(y_valid, valid_predictions)
    
    results_past <- rbind(results_past, data.frame(
      num_iterations = hyperparams_lgbm$num_iterations[j],
      num_leaves = hyperparams_lgbm$num_leaves[j],
      learning_rate = hyperparams_lgbm$learning_rate[j],
      subsample = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_valid = ROC_AUC_valid
    ))
  }
  
  # Save results
  write.csv(results, paste0("C:/Users/User/Downloads/Downloads/phd_project/results/hpm/", "lgb_forw_chain_results.csv"), row.names = FALSE)
  write.csv(results_past, paste0("C:/Users/User/Downloads/Downloads/phd_project/results/hpm/", "lgb_forw_chain_results_past.csv"), row.names = FALSE)
} 






forward_chaining_lgb_lf <- function(time_intervals_data, hyperparams_lgbm, out_of_time_data, seed = 42) {
  set.seed(seed)
  stopifnot(is.list(time_intervals_data), length(time_intervals_data) >= 4)
  
  features <- setdiff(colnames(time_intervals_data[[1]]), "occurrenceStatus")
  
  folds <- list(
    list(train = 1,    val = 2),
    list(train = 1:2,  val = 3),
    list(train = 1:3,  val = 4)
  )
  
  # --- 1) CV over all hyperparameters ---
  results_list <- vector("list", nrow(hyperparams_lgbm))
  
  for (j in seq_len(nrow(hyperparams_lgbm))) {
    fold_auc <- numeric(length(folds))
    
    for (k in seq_along(folds)) {
      tr_idx <- folds[[k]]$train
      va_idx <- folds[[k]]$val
      
      train_df <- do.call(rbind, time_intervals_data[tr_idx])
      val_df   <- time_intervals_data[[va_idx]]
      
      y_train <- as.numeric(train_df$occurrenceStatus)
      y_val   <- as.numeric(val_df$occurrenceStatus)
      
      lgb_train <- lgb.Dataset(data = as.matrix(train_df[, features, drop = FALSE]),
                               label = y_train)
      
      lgb_params <- list(
        objective        = "binary",
        metric           = "auc",
        boosting_type    = "gbdt",
        num_iterations   = hyperparams_lgbm$num_iterations[j],
        num_leaves       = hyperparams_lgbm$num_leaves[j],
        learning_rate    = hyperparams_lgbm$learning_rate[j],
        bagging_fraction = hyperparams_lgbm$subsample[j],
        feature_fraction = hyperparams_lgbm$colsample_bytree[j],
        verbose          = -1
      )
      
      lgb_model <- lgb.train(params = lgb_params, data = lgb_train)
      prob_val  <- predict(lgb_model, as.matrix(val_df[, features, drop = FALSE]))
      fold_auc[k] <- as.numeric(pROC::auc(y_val, prob_val))
    }
    
    results_list[[j]] <- data.frame(
      num_iterations   = hyperparams_lgbm$num_iterations[j],
      num_leaves       = hyperparams_lgbm$num_leaves[j],
      learning_rate    = hyperparams_lgbm$learning_rate[j],
      subsample        = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_CV_mean  = mean(fold_auc, na.rm = TRUE),
      ROC_AUC_CV_folds = paste(round(fold_auc, 4), collapse = ",")
    )
  }
  
  cv_results <- do.call(rbind, results_list)
  
  # --- 2) LAST-FOLD training for ALL configs ---
  last_tr_idx <- folds[[length(folds)]]$train
  final_df <- do.call(rbind, time_intervals_data[last_tr_idx])
  y_final  <- as.numeric(final_df$occurrenceStatus)
  
  X_past <- as.matrix(out_of_time_data[, features, drop = FALSE])
  y_past <- as.numeric(out_of_time_data$occurrenceStatus)
  
  results_past_list <- vector("list", nrow(hyperparams_lgbm))
  
  for (j in seq_len(nrow(hyperparams_lgbm))) {
    lgb_final <- lgb.Dataset(data = as.matrix(final_df[, features, drop = FALSE]),
                             label = y_final)
    
    lgb_params <- list(
      objective        = "binary",
      metric           = "auc",
      boosting_type    = "gbdt",
      num_iterations   = hyperparams_lgbm$num_iterations[j],
      num_leaves       = hyperparams_lgbm$num_leaves[j],
      learning_rate    = hyperparams_lgbm$learning_rate[j],
      bagging_fraction = hyperparams_lgbm$subsample[j],
      feature_fraction = hyperparams_lgbm$colsample_bytree[j],
      verbose          = -1
    )
    
    lgb_model <- lgb.train(params = lgb_params, data = lgb_final)
    prob_test <- predict(lgb_model, X_past)
    auc_test  <- as.numeric(pROC::auc(y_past, prob_test))
    
    results_past_list[[j]] <- data.frame(
      num_iterations   = hyperparams_lgbm$num_iterations[j],
      num_leaves       = hyperparams_lgbm$num_leaves[j],
      learning_rate    = hyperparams_lgbm$learning_rate[j],
      subsample        = hyperparams_lgbm$subsample[j],
      colsample_bytree = hyperparams_lgbm$colsample_bytree[j],
      ROC_AUC_test     = auc_test
    )
  }
  
  results_past <- do.call(rbind, results_past_list)
  
  # --- 3) Save ---
  utils::write.csv(cv_results,
                   "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/lgb_forw_chain_results_lf.csv",
                   row.names = FALSE
  )
  
  utils::write.csv(results_past,
                   "C:/Users/User/Downloads/Downloads/phd_project/results/hpm/lgb_forw_chain_results_past_lf.csv",
                   row.names = FALSE
  )
  
  invisible(list(cv_results = cv_results, past_results = results_past))
}



