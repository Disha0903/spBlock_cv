# ------------------ GBM ------------------
n.trees <- c(50, 100, 250, 500, 750, 1000)
interaction.depth <- c(3, 4, 5, 6) 
shrinkage <- c(0.005, 0.01) 
n.minobsinnode <- c(5, 10, 15, 20)

hyperparams <- expand.grid(
  n.trees = n.trees, 
  interaction.depth = interaction.depth,
  shrinkage = shrinkage, 
  n.minobsinnode = n.minobsinnode
)

set.seed(123)
hyperparams <- unique(hyperparams) %>%
  sample_n(100)
hyperparams_gbm <- hyperparams 

# ------------------ Random Forest (RF) ------------------
hyperparams_rf <- expand.grid(
  n.trees   = c(50, 100, 250, 500, 750, 900, 1000),
  nodesize  = c(1, 5, 10, 15, 20),
  maxnodes  = c(NA, 10, 50, 100)
)

set.seed(123)
hyperparams_rf <- unique(hyperparams_rf) %>%
  sample_n(100)


# ------------------ XGBoost ------------------
nrounds <- c(50, 100, 500, 1000)
max_depth <- c(3, 5)
eta <- c(0.01, 0.05, 0.1)
subsample <- c(0.6, 0.7, 0.8)
min_child_weight <- c(1, 5, 10)
gamma <- c(0, 0.1, 1)
colsample_bylevel <- c(0.6, 0.7, 0.8)
tree_method <- c("auto", "exact", "approx", "hist")

xgb_hyperparams <- expand.grid(
  nrounds = nrounds,
  max_depth = max_depth,
  eta = eta,
  subsample = subsample,
  min_child_weight = min_child_weight,
  gamma = gamma,
  colsample_bylevel = colsample_bylevel,
  tree_method = tree_method
)

set.seed(123)
hyperparams_xgb <- xgb_hyperparams %>%
  sample_n(100)


# ------------------ LightGBM ------------------
num_iterations <- c(50, 100, 500, 1000)
num_leaves <- c(10, 20, 30, 40)
learning_rate <- c(0.01, 0.05, 0.1)
subsample <- c(0.6, 0.7, 0.8)
colsample_bytree <- c(0.6, 0.7, 0.8)

lgbm_hyperparams <- expand.grid(
  num_iterations = num_iterations,
  num_leaves = num_leaves,
  learning_rate = learning_rate,
  subsample = subsample,
  colsample_bytree = colsample_bytree
)

set.seed(123)
hyperparams_lgbm <- lgbm_hyperparams %>%
  sample_n(100)
