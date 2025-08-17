# ======================================================================
# Minimal MaxEnt (maxnet) demo: RETRAIN vs LAST-FOLD
# ======================================================================
# What this script shows
# ----------------------
# 1) How to run a tiny hyperparameter sweep for MaxEnt (via {maxnet}).
# 2) How to compute CV AUC (5-fold, stratified) for each configuration.
# 3) Two deployment strategies against an *independent* out-of-time set:
#      - RETRAIN  : fit on ALL in-time data, then evaluate on OOT.
#      - LAST-FOLD: use the model from the FINAL CV fold, evaluate on OOT.
# 4) Writes three CSVs with results and prints short summaries.
#
# Why we use synthetic data here
# -----------------------------
# To avoid GIS/CRS dependencies and keep focus on the learning protocol,
# we generate a simple presence/absence dataset with known signal and a
# *slight* distribution shift for the out-of-time (OOT) split.
#
# Requirements
# ------------
# install.packages(c("maxnet", "pROC"))
#
# Outputs (in working directory)
# ------------------------------
# - maxent_demo_results.csv         (CV mean AUC + per-fold AUCs)
# - maxent_demo_results_past.csv    (RETRAIN → OOT AUC)
# - maxent_demo_results_past_lf.csv (LAST-FOLD → OOT AUC)
#
# Notes & pitfalls addressed
# --------------------------
# - maxnet.formula() expects a *single string* of feature codes (e.g. "lqh"),
#   not a character vector (c("l","q","h")). We collapse codes before calling it.
# - AUC is undefined if a fold/test has only one class; safe_auc() returns NA.
# - CV uses *stratified* folds to reduce single-class folds in tests.
# ======================================================================

suppressPackageStartupMessages({
  library(maxnet)
  library(pROC)
})

set.seed(123)

# ----------------------------------------------------------------------
# 1) Create synthetic in-time and out-of-time datasets (tabular)
# ----------------------------------------------------------------------
make_synthetic <- function(n, shift = list(x1 = 0, x1_sd = 1, x2_min = -2, x2_max = 2, x3_p = 0.4),
                           noise_sd = 0.3) {
  x1 <- rnorm(n, mean = shift$x1, sd = shift$x1_sd)
  x2 <- runif(n, min = shift$x2_min, max = shift$x2_max)
  x3 <- rbinom(n, 1, prob = shift$x3_p)
  # logistic DGP with nonlinearity on x2
  lin <- 1.2 * x1 - 1.5 * (x2^2) + 0.8 * x3 + rnorm(n, sd = noise_sd)
  p   <- 1 / (1 + exp(-lin))
  y   <- rbinom(n, 1, p)
  data.frame(x1 = x1, x2 = x2, x3 = x3, occurrenceStatus = y)
}

# In-time data (training + CV)
in_time <- make_synthetic(
  n = 1500,
  shift = list(x1 = 0, x1_sd = 1, x2_min = -2, x2_max = 2, x3_p = 0.40),
  noise_sd = 0.30
)

# Out-of-time data (independent test, slight shift)
out_time <- make_synthetic(
  n = 1200,
  shift = list(x1 = 0.2, x1_sd = 1.1, x2_min = -2.2, x2_max = 1.8, x3_p = 0.45),
  noise_sd = 0.35
)

cat("\nIn-time class balance:\n"); print(table(in_time$occurrenceStatus))
cat("Out-of-time class balance:\n"); print(table(out_time$occurrenceStatus))

# ----------------------------------------------------------------------
# 2) Stratified K-fold indices (keeps class balance per fold)
# ----------------------------------------------------------------------
make_stratified_folds <- function(y, k = 5, seed = 42) {
  set.seed(seed)
  pos <- which(y == 1)
  neg <- which(y == 0)
  pos_splits <- split(sample(pos), rep(1:k, length.out = length(pos)))
  neg_splits <- split(sample(neg), rep(1:k, length.out = length(neg)))
  lapply(seq_len(k), function(i) {
    test_idx  <- c(pos_splits[[i]], neg_splits[[i]])
    train_idx <- setdiff(seq_along(y), test_idx)
    list(train = train_idx, test = test_idx)
  })
}

k <- 5
folds <- make_stratified_folds(in_time$occurrenceStatus, k = k, seed = 7)

# ----------------------------------------------------------------------
# 3) Small utilities
# ----------------------------------------------------------------------
# AUC that returns NA if test has only one class or any failure occurs
safe_auc <- function(y, p) {
  y <- as.numeric(y)
  if (length(unique(y)) < 2L) return(NA_real_)
  as.numeric(pROC::auc(y, p))
}

# Parse feature classes; accepts "lq", "lqh", "l,q,h", "linear,quadratic,hinge", etc.
parse_classes <- function(s) {
  s <- tolower(gsub("\\s+", "", s))
  # compact code (e.g. "lqph")
  if (grepl("^[lqpth]+$", s)) return(strsplit(s, "")[[1]])
  # verbose -> compact mapping
  mp <- c(linear = "l", quadratic = "q", product = "p", hinge = "h", threshold = "t",
          l = "l", q = "q", p = "p", h = "h", t = "t")
  out <- unname(mp[strsplit(s, "[,\\s]+")[[1]]])
  out[!is.na(out)]
}

# Fit a maxnet model; IMPORTANT: collapse feature codes to a single string
fit_maxnet <- function(df, regmult, classes_vec) {
  xcols       <- setdiff(names(df), "occurrenceStatus")
  classes_str <- paste0(classes_vec, collapse = "")  # "lqh", not c("l","q","h")
  maxnet(
    p = df$occurrenceStatus,
    data = df[, xcols, drop = FALSE],
    regmult = regmult,
    f = maxnet.formula(
      p = df$occurrenceStatus,
      data = df[, xcols, drop = FALSE],
      classes = classes_str
    )
  )
}

# ----------------------------------------------------------------------
# 4) Tiny hyperparameter grid (keep small for a demo)
# ----------------------------------------------------------------------
hyperparams <- data.frame(
  regmult  = c(0.5, 1.0, 2.0),
  features = c("lq", "lqh", "lqph"),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------
# 5) Run CV + RETRAIN + LAST-FOLD and collect results
# ----------------------------------------------------------------------
xcols <- setdiff(names(in_time), "occurrenceStatus")

cv_rows       <- vector("list", nrow(hyperparams))
retrain_rows  <- vector("list", nrow(hyperparams))
lastfold_rows <- vector("list", nrow(hyperparams))

for (j in seq_len(nrow(hyperparams))) {
  regmult <- hyperparams$regmult[j]
  classes <- parse_classes(hyperparams$features[j])

  # ----- Cross-validation (mean AUC over k folds)
  fold_auc  <- numeric(k)
  last_model <- NULL

  for (i in seq_len(k)) {
    tr <- folds[[i]]$train
    te <- folds[[i]]$test

    m  <- fit_maxnet(in_time[tr, ], regmult, classes)
    pr <- predict(m, in_time[te, xcols, drop = FALSE], type = "logistic")
    fold_auc[i] <- safe_auc(in_time$occurrenceStatus[te], pr)

    if (i == k) last_model <- m  # keep the *final* CV model (LAST-FOLD)
  }

  cv_rows[[j]] <- data.frame(
    regmult       = regmult,
    features      = hyperparams$features[j],
    mean_ROC_AUC  = mean(fold_auc, na.rm = TRUE),
    fold_ROC_AUC  = paste(round(fold_auc, 4), collapse = ", "),
    stringsAsFactors = FALSE
  )

  # ----- RETRAIN: fit on ALL in-time, test on OOT
  m_full  <- fit_maxnet(in_time, regmult, classes)
  pr_full <- predict(m_full, out_time[, xcols, drop = FALSE], type = "logistic")
  retrain_rows[[j]] <- data.frame(
    regmult       = regmult,
    features      = hyperparams$features[j],
    ROC_AUC_valid = safe_auc(out_time$occurrenceStatus, pr_full),
    stringsAsFactors = FALSE
  )

  # ----- LAST-FOLD: use the final CV model directly on OOT
  pr_lf <- predict(last_model, out_time[, xcols, drop = FALSE], type = "logistic")
  lastfold_rows[[j]] <- data.frame(
    regmult          = regmult,
    features         = hyperparams$features[j],
    ROC_AUC_valid_lf = safe_auc(out_time$occurrenceStatus, pr_lf),
    last_fold_id     = k,
    stringsAsFactors = FALSE
  )
}

cv_df       <- do.call(rbind, cv_rows)
retrain_df  <- do.call(rbind, retrain_rows)
lastfold_df <- do.call(rbind, lastfold_rows)

# ----------------------------------------------------------------------
# 6) Save CSVs + print a compact summary
# ----------------------------------------------------------------------
write.csv(cv_df,       "maxent_demo_results.csv",         row.names = FALSE)
write.csv(retrain_df,  "maxent_demo_results_past.csv",    row.names = FALSE)
write.csv(lastfold_df, "maxent_demo_results_past_lf.csv", row.names = FALSE)

cat("\n=== CV summary (mean ROC AUC over folds) ===\n")
print(cv_df)

cat("\n=== OOT AUC (RETRAIN) ===\n")
print(retrain_df)

cat("\n=== OOT AUC (LAST-FOLD) ===\n")
print(lastfold_df)

cat("\nDone. Wrote:\n",
    "- maxent_demo_results.csv\n",
    "- maxent_demo_results_past.csv\n",
    "- maxent_demo_results_past_lf.csv\n")
