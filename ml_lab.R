# ============================================================
# Fantasmagoria — ML Lab
# Predict pace (and total time) for a target race distance
# ============================================================
# Sections:
#   1. Setup & data reading
#   2. Feature engineering
#   3. Cross-validation framework
#   4. Naive baseline models
#   5. Linear models
#   6. Random forest
#   7. XGBoost
#   8. Neural network
#   9. Metrics comparison & residual plots
#  10. Results & recommendation
# ============================================================


# ============================================================
# 1. SETUP & DATA READING
# ============================================================
{
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(jsonlite)
  library(purrr)
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(slider)       # install.packages("slider")
  library(zoo)          # install.packages("zoo")
  library(randomForest) # install.packages("randomForest")
  library(xgboost)      # install.packages("xgboost")
  library(torch)        # install.packages("torch") + torch::install_torch()
  library(MASS)         # stepwise AIC — ships with base R
  
  source("fantasmagoria_functions.R")
  
  # ---- 1.1 Summary data --------------------------------------
  {
    cat("Reading summary data...\n")
    df_raw <- load_summaries() %>%
      add_scores() %>%
      add_dynamic_scores() %>%
      arrange(start_time) %>%
      filter(start_time > ymd('2024-01-01'))
    
    cat(sprintf("  Loaded %d runs spanning %.1f years\n",
                nrow(df_raw),
                as.numeric(difftime(max(df_raw$start_time),
                                    min(df_raw$start_time), units = "days")) / 365))
    cat(sprintf("  Columns: %s\n\n", paste(names(df_raw), collapse = ", ")))
  }
  
  # ---- 1.2 Sample of detail JSONs (for future feature work) --
  {
    cat("Reading sample of detail JSONs...\n")
    detail_files <- list.files(DETAIL_DIR, pattern = "\\.json$", full.names = TRUE)
    set.seed(42)
    sample_files  <- sample(detail_files, min(20, length(detail_files)))
    detail_sample <- map(sample_files, \(f) fromJSON(f, simplifyVector = TRUE))
    
    # Quick inventory of what's available
    has_gps      <- map_lgl(detail_sample, \(d) !is.null(d$polylines) && length(d$polylines) > 0)
    has_splits   <- map_lgl(detail_sample, \(d) is.data.frame(d$moments) && nrow(d$moments) > 0)
    has_hr       <- map_lgl(detail_sample, \(d) {
      m <- d$metrics
      !is.null(m) && any(m$type == "heart_rate")
    })
    
    cat(sprintf("  Detail sample: %d files\n", length(detail_sample)))
    cat(sprintf("  GPS available:    %d / %d\n", sum(has_gps),    length(detail_sample)))
    cat(sprintf("  Splits available: %d / %d\n", sum(has_splits), length(detail_sample)))
    cat(sprintf("  Heart rate:       %d / %d\n\n", sum(has_hr),   length(detail_sample)))
  }
}


# ============================================================
# 2. FEATURE ENGINEERING
# ============================================================
{
  cat("Engineering features...\n")
  
  # ---- 2.1 Elevation categorical features --------------------
  # Summary CSV has ascent_total, descent_total, distance_total
  {
    df <- df_raw %>%
      mutate(
        # Per-km rates (more comparable across distances)
        ascent_per_km  = ascent_total  / distance_total,
        descent_per_km = descent_total / distance_total,
        net_elev_per_km = (ascent_total - descent_total) / distance_total,
        
        # Net elevation category (per km)
        net_elev_cat = case_when(
          net_elev_per_km <= -100 ~ "big_loss",
          net_elev_per_km <=  -10 ~ "small_loss",
          net_elev_per_km <    10 ~ "neutral",
          net_elev_per_km <    50 ~ "small_gain",
          TRUE                    ~ "big_gain"
        ) %>% factor(levels = c("big_loss", "small_loss", "neutral",
                                "small_gain", "big_gain")),
        
        # Max elevation gain category (ascent per km as proxy)
        ascent_cat = case_when(
          ascent_per_km <  20 ~ "none",
          ascent_per_km <  50 ~ "medium",
          TRUE                ~ "large"
        ) %>% factor(levels = c("none", "medium", "large"))
      )
    
    cat("  Elevation categories:\n")
    cat("    net_elev_cat:\n"); print(table(df$net_elev_cat))
    cat("    ascent_cat:\n");   print(table(df$ascent_cat))
    cat("\n")
  }
  
  # ---- 2.2 Temporal features ---------------------------------
  {
    df <- df %>%
      mutate(
        date      = as.Date(start_time, tz = TZ),
        hour      = lubridate::hour(start_time),
        weekday   = lubridate::wday(start_time, label = TRUE, abbr = TRUE),
        month_num = lubridate::month(start_time),
        year      = lubridate::year(start_time),
        
        # Time of day
        time_of_day = case_when(
          hour <  7  ~ "early_morning",
          hour < 11  ~ "morning",
          hour < 14  ~ "midday",
          hour < 18  ~ "afternoon",
          TRUE       ~ "evening"
        ) %>% factor(levels = c("early_morning", "morning", "midday",
                                "afternoon", "evening")),
        
        # Season (CDMX: dry/cool Dec-Feb, dry/hot Mar-May,
        #                rainy Jun-Sep, transition Oct-Nov)
        season = case_when(
          month_num %in% c(12, 1, 2) ~ "dry_cool",
          month_num %in% c(3, 4, 5)  ~ "dry_hot",
          month_num %in% c(6, 7, 8, 9) ~ "rainy",
          TRUE                        ~ "transition"
        ) %>% factor(levels = c("dry_cool", "dry_hot", "rainy", "transition"))
      )
    
    cat("  Temporal features:\n")
    cat("    time_of_day:\n"); print(table(df$time_of_day))
    cat("    season:\n");      print(table(df$season))
    cat("\n")
  }
  
  # ---- 2.3 Rolling window features ---------------------------
  # All windows are computed looking BACKWARD only (no data leakage)
  # Each row's features describe the state BEFORE that race was run
  {
    df <- df %>% arrange(start_time)
    
    # Helper: lag-based rolling mean over last N races (excluding current)
    roll_mean_races <- function(x, n) {
      lag(slide_dbl(x, mean, .before = n, .after = 0, .complete = FALSE), 1)
    }
    
    # Helper: lag-based rolling sum over last N races (excluding current)
    roll_sum_races <- function(x, n) {
      lag(slide_dbl(x, sum, .before = n, .after = 0, .complete = FALSE), 1)
    }
    
    # Helper: calendar-based rolling sum over last N days (excluding today)
    roll_sum_days <- function(dates, values, n_days) {
      map_dbl(seq_along(dates), function(i) {
        cutoff <- dates[i] - n_days
        idx    <- which(dates < dates[i] & dates >= cutoff)
        if (length(idx) == 0) return(NA_real_)
        sum(values[idx], na.rm = TRUE)
      })
    }
    
    roll_mean_days <- function(dates, values, n_days) {
      map_dbl(seq_along(dates), function(i) {
        cutoff <- dates[i] - n_days
        idx    <- which(dates < dates[i] & dates >= cutoff)
        if (length(idx) == 0) return(NA_real_)
        mean(values[idx], na.rm = TRUE)
      })
    }
    
    roll_count_days <- function(dates, n_days) {
      map_int(seq_along(dates), function(i) {
        cutoff <- dates[i] - n_days
        sum(dates < dates[i] & dates >= cutoff)
      })
    }
    
    df <- df %>%
      mutate(
        # ---- Race-count rolling windows ----------------------
        # Average pace over last 3 / 5 / 10 races
        avg_pace_last3  = roll_mean_races(pace_mean,       3),
        avg_pace_last5  = roll_mean_races(pace_mean,       5),
        avg_pace_last10 = roll_mean_races(pace_mean,      10),
        
        # Average distance over last 3 / 5 races
        avg_dist_last3  = roll_mean_races(distance_total,  3),
        avg_dist_last5  = roll_mean_races(distance_total,  5),
        
        # Total km over last 3 / 5 / 10 races
        km_last3        = roll_sum_races(distance_total,   3),
        km_last5        = roll_sum_races(distance_total,   5),
        km_last10       = roll_sum_races(distance_total,  10),
        
        # ---- Calendar rolling windows ------------------------
        # Total km in last 7 / 14 / 30 days (fatigue / load)
        km_last7d       = roll_sum_days(date, distance_total,  7),
        km_last14d      = roll_sum_days(date, distance_total, 14),
        km_last30d      = roll_sum_days(date, distance_total, 30),
        
        # Average pace in last 7 / 14 days (recent form)
        avg_pace_last7d  = roll_mean_days(date, pace_mean,  7),
        avg_pace_last14d = roll_mean_days(date, pace_mean, 14),
        
        # Number of races in last 7 / 14 days (back-to-back fatigue)
        n_races_last7d  = roll_count_days(date,  7),
        n_races_last14d = roll_count_days(date, 14),
        
        # Days since last race (freshness)
        days_since_last = as.numeric(date - lag(date), units = "days")
      )
    
    cat("  Rolling window features summary:\n")
    roll_cols <- c("avg_pace_last3", "avg_pace_last7d", "km_last7d",
                   "km_last14d", "n_races_last7d", "days_since_last")
    print(summary(df[, roll_cols]))
    cat("\n")
  }
  
  # ---- 2.4 Build final modelling dataset ---------------------
  {
    # Target: pace_mean (min/km)
    # Elevation: continuous only — categoricals dropped (no variance in this dataset)
    # Pace filter: remove outliers beyond 3 SD (walks, warmups, GPS glitches)
    FEATURE_COLS <- c(
      # Race characteristics
      "distance_total",
      "ascent_per_km", "descent_per_km", "net_elev_per_km",
      # Temporal
      "time_of_day", "season", "weekday",
      # Race-count rolling
      "avg_pace_last3", "avg_pace_last5", "avg_pace_last10",
      "avg_dist_last3", "avg_dist_last5",
      "km_last3", "km_last5", "km_last10",
      # Calendar rolling
      "km_last7d", "km_last14d", "km_last30d",
      "avg_pace_last7d", "avg_pace_last14d",
      "n_races_last7d", "n_races_last14d",
      "days_since_last"
    )
    
    TARGET_COL <- "pace_mean"
    
    # Compute pace cutoff from data (3 SD above mean)
    pace_mean_val <- mean(df$pace_mean, na.rm = TRUE)
    pace_sd_val   <- sd(df$pace_mean,   na.rm = TRUE)
    PACE_CUTOFF   <- pace_mean_val + 3 * pace_sd_val
    cat(sprintf("  Pace cutoff (mean + 3SD): %.2f min/km (%s)\n",
                PACE_CUTOFF, pace_dec_to_str(PACE_CUTOFF)))
    
    ml_df <- df %>%
      select(id, start_time, date, all_of(TARGET_COL), all_of(FEATURE_COLS)) %>%
      filter(
        pace_mean <= PACE_CUTOFF,                  # remove outlier paces
        !is.na(avg_pace_last10),                   # rolling windows filled
        !is.na(km_last30d),
        !is.na(days_since_last)
      )
    
    cat(sprintf("  Rows after pace filter:   %d (removed %d outliers)\n",
                nrow(ml_df), nrow(df) - nrow(ml_df)))
    cat(sprintf("  Features: %d\n", length(FEATURE_COLS)))
    cat(sprintf("  Target range: %.2f — %.2f min/km\n\n",
                min(ml_df$pace_mean), max(ml_df$pace_mean)))
    
    # UI terrain proxy — map category to median ascent_per_km from data
    terrain_lookup <- ml_df %>%
      mutate(terrain = case_when(
        ascent_per_km <  10 ~ "flat",
        ascent_per_km <  30 ~ "small_hills",
        TRUE                ~ "hilly"
      )) %>%
      group_by(terrain) %>%
      summarise(
        median_ascent_per_km  = median(ascent_per_km,  na.rm = TRUE),
        median_descent_per_km = median(descent_per_km, na.rm = TRUE),
        n = n(), .groups = "drop"
      )
    cat("  UI terrain proxy lookup (used in Shiny input):\n")
    print(terrain_lookup)
    cat("\n")
    
    # Correlation of numeric features with target
    cat("  Pearson correlation with pace_mean (top 10):\n")
    num_cols <- ml_df %>%
      select(where(is.numeric), -pace_mean) %>%
      names()
    cors <- sapply(num_cols, function(col) {
      cor(ml_df[[col]], ml_df$pace_mean, use = "complete.obs")
    })
    print(sort(abs(cors), decreasing = TRUE)[1:10])
    cat("\n")
  }
}



cat("=== Sections 1-2 complete. Ready for modelling. ===\n")
cat(sprintf("ml_df: %d rows, %d features, target range %.2f-%.2f min/km\n",
            nrow(ml_df), length(FEATURE_COLS),
            min(ml_df$pace_mean), max(ml_df$pace_mean)))


# ============================================================
# 3. CROSS-VALIDATION FRAMEWORK
# ============================================================
# Time-series expanding window CV — train on past, validate on future
# Never lets future data leak into training
# ============================================================
{
  # ---- 3.1 Define folds --------------------------------------
  {
    n         <- nrow(ml_df)
    # 3 folds: each adds ~17% more training data
    # Minimum training size: 60% of data
    fold_cuts <- floor(c(0.60, 0.73, 0.86) * n)
    
    folds <- map(fold_cuts, function(cut) {
      list(
        train_idx = 1:cut,
        val_idx   = (cut + 1):min(cut + floor(0.13 * n), n)
      )
    })
    
    cat("CV folds (expanding window):\n")
    walk(seq_along(folds), function(i) {
      f <- folds[[i]]
      cat(sprintf("  Fold %d: train=%d rows (%s – %s)  val=%d rows (%s – %s)\n",
                  i,
                  length(f$train_idx),
                  format(ml_df$date[min(f$train_idx)], "%Y-%m-%d"),
                  format(ml_df$date[max(f$train_idx)], "%Y-%m-%d"),
                  length(f$val_idx),
                  format(ml_df$date[min(f$val_idx)], "%Y-%m-%d"),
                  format(ml_df$date[max(f$val_idx)], "%Y-%m-%d")))
    })
    cat("\n")
  }
  
  # ---- 3.2 Metric helpers ------------------------------------
  {
    rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm=TRUE))
    mae  <- function(actual, predicted) mean(abs(actual - predicted), na.rm=TRUE)
    mape <- function(actual, predicted) mean(abs((actual - predicted) / actual) * 100, na.rm=TRUE)
    r2   <- function(actual, predicted) {
      ss_res <- sum((actual - predicted)^2, na.rm=TRUE)
      ss_tot <- sum((actual - mean(actual, na.rm=TRUE))^2, na.rm=TRUE)
      1 - ss_res / ss_tot
    }
    
    eval_model <- function(actual, predicted, model_name, fold) {
      tibble(
        model = model_name,
        fold  = fold,
        rmse  = rmse(actual, predicted),
        mae   = mae(actual, predicted),
        mape  = mape(actual, predicted),
        r2    = r2(actual, predicted)
      )
    }
    
    # Storage for all results
    cv_results  <- list()
    all_preds   <- list()   # for residual plots
  }
  
  # ---- 3.3 Prepare model matrices ----------------------------
  {
    # One-hot encode factors for xgboost / neural net
    # Keep original ml_df for R formula-based models
    num_features <- FEATURE_COLS[!FEATURE_COLS %in% c("time_of_day", "season", "weekday")]
    cat_features <- c("time_of_day", "season", "weekday")
    
    ml_matrix <- ml_df %>%
      select(all_of(FEATURE_COLS)) %>%
      mutate(across(all_of(cat_features), as.factor)) %>%
      model.matrix(~ . - 1, data = .) %>%
      as.data.frame()
    
    cat(sprintf("Model matrix: %d rows x %d columns (after one-hot encoding)\n\n",
                nrow(ml_matrix), ncol(ml_matrix)))
  }
}


# ============================================================
# 4. NAIVE BASELINE MODELS
# ============================================================
# These are the floor — any real model must beat them
# ============================================================
{
  cat("--- Section 4: Naive baselines ---\n")
  
  naive_cv <- map_dfr(seq_along(folds), function(fold_i) {
    f        <- folds[[fold_i]]
    train_df <- ml_df[f$train_idx, ]
    val_df   <- ml_df[f$val_idx,   ]
    
    # Baseline 1: global mean pace of training set
    pred_mean <- rep(mean(train_df$pace_mean), nrow(val_df))
    
    # Baseline 2: last 3 races average (already computed as feature)
    pred_last3 <- val_df$avg_pace_last3
    
    # Baseline 3: last 7 days average
    pred_last7d <- val_df$avg_pace_last7d
    
    bind_rows(
      eval_model(val_df$pace_mean, pred_mean,   "naive_global_mean", fold_i),
      eval_model(val_df$pace_mean, pred_last3,  "naive_last3_races", fold_i),
      eval_model(val_df$pace_mean, pred_last7d, "naive_last7d",      fold_i)
    )
  })
  
  cv_results[["naive"]] <- naive_cv
  
  cat("Naive model CV results:\n")
  print(naive_cv %>%
          group_by(model) %>%
          summarise(across(c(rmse, mae, mape, r2), mean), .groups="drop") %>%
          arrange(rmse))
  cat("\n")
}


# ============================================================
# 5. LINEAR MODELS
# ============================================================
{
  cat("--- Section 5: Linear models ---\n")
  
  linear_cv <- map_dfr(seq_along(folds), function(fold_i) {
    f        <- folds[[fold_i]]
    train_df <- ml_df[f$train_idx, ]
    val_df   <- ml_df[f$val_idx,   ]
    
    # 5.1 Full linear model (all features)
    {
      lm_full <- lm(pace_mean ~ ., data = train_df %>%
                      select(pace_mean, all_of(FEATURE_COLS)))
      pred_full <- predict(lm_full, newdata = val_df)
    }
    
    # 5.2 Stepwise AIC selection
    {
      lm_step <- stepAIC(lm_full, direction = "both", trace = FALSE)
      pred_step <- predict(lm_step, newdata = val_df)
    }
    
    # 5.3 Pace-only model (just rolling pace features — strong baseline)
    {
      pace_only_features <- c("avg_pace_last3", "avg_pace_last7d",
                              "avg_pace_last14d", "avg_pace_last10")
      lm_pace <- lm(pace_mean ~ ., data = train_df %>%
                      select(pace_mean, all_of(pace_only_features)))
      pred_pace <- predict(lm_pace, newdata = val_df)
    }
    
    bind_rows(
      eval_model(val_df$pace_mean, pred_full, "lm_full",      fold_i),
      eval_model(val_df$pace_mean, pred_step, "lm_stepwise",  fold_i),
      eval_model(val_df$pace_mean, pred_pace, "lm_pace_only", fold_i)
    )
  })
  
  cv_results[["linear"]] <- linear_cv
  
  cat("Linear model CV results:\n")
  print(linear_cv %>%
          group_by(model) %>%
          summarise(across(c(rmse, mae, mape, r2), mean), .groups="drop") %>%
          arrange(rmse))
  
  # Save final stepwise model (trained on all data) for inspection
  lm_final <- lm(pace_mean ~ ., data = ml_df %>%
                   select(pace_mean, all_of(FEATURE_COLS)))
  lm_step_final <- stepAIC(lm_final, direction = "both", trace = FALSE)
  
  cat("\nStepwise AIC selected features:\n")
  print(names(coef(lm_step_final)))
  cat("\n")
}


# ============================================================
# 6. RANDOM FOREST
# ============================================================
# Handles non-linearity and interactions automatically
# Feature importance shows which variables matter most
# ============================================================
{
  cat("--- Section 6: Random Forest ---\n")
  
  rf_cv <- map_dfr(seq_along(folds), function(fold_i) {
    f          <- folds[[fold_i]]
    train_mat  <- ml_matrix[f$train_idx, ]
    val_mat    <- ml_matrix[f$val_idx,   ]
    train_y    <- ml_df$pace_mean[f$train_idx]
    val_y      <- ml_df$pace_mean[f$val_idx]
    
    rf_fit <- randomForest(
      x        = train_mat,
      y        = train_y,
      ntree    = 300,
      mtry     = floor(sqrt(ncol(train_mat))),
      importance = TRUE
    )
    
    pred_rf <- predict(rf_fit, newdata = val_mat)
    eval_model(val_y, pred_rf, "random_forest", fold_i)
  })
  
  cv_results[["rf"]] <- rf_cv
  
  cat("Random Forest CV results:\n")
  print(rf_cv %>%
          group_by(model) %>%
          summarise(across(c(rmse, mae, mape, r2), mean), .groups="drop"))
  
  # Train final RF on all data for feature importance
  rf_final <- randomForest(
    x          = ml_matrix,
    y          = ml_df$pace_mean,
    ntree      = 500,
    mtry       = floor(sqrt(ncol(ml_matrix))),
    importance = TRUE
  )
  
  cat("\nRandom Forest — top 15 features by %IncMSE:\n")
  imp <- importance(rf_final, type = 1)
  print(sort(imp[,1], decreasing = TRUE)[1:15])
  cat("\n")
}

# ============================================================
# 7. XGBOOST
# ============================================================
{
  cat("--- Section 7: XGBoost ---\n")
  
  xgb_cv <- map_dfr(seq_along(folds), function(fold_i) {
    f         <- folds[[fold_i]]
    train_mat <- as.matrix(ml_matrix[f$train_idx, ])
    val_mat   <- as.matrix(ml_matrix[f$val_idx,   ])
    train_y   <- ml_df$pace_mean[f$train_idx]
    val_y     <- ml_df$pace_mean[f$val_idx]
    
    dtrain <- xgb.DMatrix(data = train_mat, label = train_y)
    dval   <- xgb.DMatrix(data = val_mat,   label = val_y)
    
    xgb_fit <- xgb.train(
      params  = list(
        objective        = "reg:squarederror",
        eta              = 0.05,
        max_depth        = 4,
        subsample        = 0.8,
        colsample_bytree = 0.8,
        min_child_weight = 5
      ),
      data       = dtrain,
      nrounds    = 500,
      watchlist  = list(val = dval),
      early_stopping_rounds = 30,
      verbose    = 0
    )
    
    pred_xgb <- predict(xgb_fit, dval)
    eval_model(val_y, pred_xgb, "xgboost", fold_i)
  })
  
  cv_results[["xgb"]] <- xgb_cv
  
  cat("XGBoost CV results:\n")
  print(xgb_cv %>%
          group_by(model) %>%
          summarise(across(c(rmse, mae, mape, r2), mean), .groups="drop"))
  
  # Train final XGB on all data for feature importance
  dtrain_full <- xgb.DMatrix(
    data  = as.matrix(ml_matrix),
    label = ml_df$pace_mean
  )
  xgb_final <- xgb.train(
    params  = list(objective = "reg:squarederror", eta = 0.05,
                   max_depth = 4, subsample = 0.8,
                   colsample_bytree = 0.8, min_child_weight = 5),
    data    = dtrain_full,
    nrounds = 500,
    verbose = 0
  )
  
  cat("\nXGBoost — top 15 features by gain:\n")
  xgb_imp <- xgb.importance(model = xgb_final)
  print(head(xgb_imp[order(-xgb_imp$Gain), c("Feature","Gain")], 15))
  cat("\n")
}


# ============================================================
# 8. NEURAL NETWORK
# ============================================================
# Simple 3-layer MLP using torch
# Lightweight — no GPU needed
# ============================================================
{
  cat("--- Section 8: Neural Network (MLP) ---\n")
  
  # ---- 8.1 Normalise features (critical for NNs) -------------
  {
    # Compute mean/sd on full dataset — each fold will re-normalise on train only
    mat_full   <- as.matrix(ml_matrix)
    feat_means <- colMeans(mat_full, na.rm = TRUE)
    feat_sds   <- apply(mat_full, 2, sd, na.rm = TRUE)
    feat_sds[feat_sds == 0] <- 1   # avoid div by zero for constant cols
    
    target_mean <- mean(ml_df$pace_mean)
    target_sd   <- sd(ml_df$pace_mean)
  }
  
  # ---- 8.2 Define MLP architecture ---------------------------
  {
    n_inputs <- ncol(ml_matrix)
    
    MLP <- nn_module(
      initialize = function(n_in) {
        self$fc1 <- nn_linear(n_in, 64)
        self$fc2 <- nn_linear(64, 32)
        self$fc3 <- nn_linear(32, 1)
        self$drop <- nn_dropout(0.2)
      },
      forward = function(x) {
        x %>%
          self$fc1() %>% nnf_relu() %>% self$drop() %>%
          self$fc2() %>% nnf_relu() %>%
          self$fc3()
      }
    )
  }
  
  # ---- 8.3 CV loop -------------------------------------------
  {
    nn_cv <- map_dfr(seq_along(folds), function(fold_i) {
      f <- folds[[fold_i]]
      
      # Normalise using train statistics only
      train_mat <- mat_full[f$train_idx, ]
      val_mat   <- mat_full[f$val_idx,   ]
      tr_means  <- colMeans(train_mat)
      tr_sds    <- apply(train_mat, 2, sd); tr_sds[tr_sds == 0] <- 1
      
      train_x <- torch_tensor(scale(train_mat, center = tr_means, scale = tr_sds),
                              dtype = torch_float())
      val_x   <- torch_tensor(
        t((t(val_mat) - tr_means) / tr_sds),
        dtype = torch_float()
      )
      
      train_y_raw <- ml_df$pace_mean[f$train_idx]
      ty_mean     <- mean(train_y_raw); ty_sd <- sd(train_y_raw)
      train_y     <- torch_tensor((train_y_raw - ty_mean) / ty_sd,
                                  dtype = torch_float())$unsqueeze(2)
      val_y_raw   <- ml_df$pace_mean[f$val_idx]
      
      model     <- MLP(n_inputs)
      optimizer <- optim_adam(model$parameters, lr = 1e-3)
      
      # Mini-batch training
      n_train    <- length(f$train_idx)
      batch_size <- 32
      n_epochs   <- 100
      
      for (epoch in seq_len(n_epochs)) {
        model$train()
        idx_shuffled <- sample(n_train)
        for (start in seq(1, n_train, by = batch_size)) {
          end     <- min(start + batch_size - 1, n_train)
          batch_i <- idx_shuffled[start:end]
          xb      <- train_x[batch_i, ]
          yb      <- train_y[batch_i, ]
          optimizer$zero_grad()
          loss <- nnf_mse_loss(model(xb)$squeeze(), yb$squeeze())
          loss$backward()
          optimizer$step()
        }
      }
      
      model$eval()
      with_no_grad({
        pred_norm <- model(val_x)$squeeze()$to(dtype = torch_float())
        pred_raw  <- as.numeric(pred_norm) * ty_sd + ty_mean
      })
      
      eval_model(val_y_raw, pred_raw, "neural_net", fold_i)
    })
    
    cv_results[["nn"]] <- nn_cv
    
    cat("Neural Network CV results:\n")
    print(nn_cv %>%
            group_by(model) %>%
            summarise(across(c(rmse, mae, mape, r2), mean), .groups="drop"))
    cat("\n")
  }
}


# ============================================================
# 9. METRICS COMPARISON & DIAGNOSTIC PLOTS
# ============================================================
{
  cat("--- Section 9: Metrics & plots ---\n")
  
  # ---- 9.1 Summary table -------------------------------------
  {
    all_cv <- bind_rows(cv_results)
    
    summary_table <- all_cv %>%
      group_by(model) %>%
      summarise(
        RMSE = round(mean(rmse), 4),
        MAE  = round(mean(mae),  4),
        MAPE = round(mean(mape), 2),
        R2   = round(mean(r2),   4),
        .groups = "drop"
      ) %>%
      arrange(RMSE)
    
    cat("\n=== CV METRICS SUMMARY (mean across folds) ===\n")
    print(summary_table)
    cat("\n")
    
    # RMSE in pace string format for interpretability
    cat("RMSE in min:sec (how far off on average):\n")
    walk(seq_len(nrow(summary_table)), function(i) {
      cat(sprintf("  %-22s  ±%s min/km\n",
                  summary_table$model[i],
                  pace_dec_to_str(summary_table$RMSE[i])))
    })
    cat("\n")
  }
  
  # ---- 9.2 Retrain best models on all data for diagnostics ---
  {
    # Retrain linear stepwise for residuals
    lm_diag <- lm_step_final
    
    # Retrain RF
    rf_diag <- rf_final
    
    # Predictions on full dataset (in-sample — for residual shape inspection)
    pred_lm  <- predict(lm_diag)
    pred_rf  <- predict(rf_diag, newdata = ml_matrix)
    pred_xgb <- predict(xgb_final, xgb.DMatrix(as.matrix(ml_matrix)))
    
    diag_df <- tibble(
      actual   = ml_df$pace_mean,
      date     = ml_df$date,
      pred_lm  = pred_lm,
      pred_rf  = pred_rf,
      pred_xgb = pred_xgb,
      res_lm   = actual - pred_lm,
      res_rf   = actual - pred_rf,
      res_xgb  = actual - pred_xgb
    )
  }
  
  # ---- 9.3 Plots ---------------------------------------------
  {
    # Plot 1: RMSE comparison across models
    p_rmse <- summary_table %>%
      mutate(model = reorder(model, RMSE)) %>%
      ggplot(aes(x = model, y = RMSE, fill = model)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(RMSE, 3)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(title = "CV RMSE by model (lower is better)",
           x = NULL, y = "RMSE (min/km)") +
      theme_minimal()
    print(p_rmse)
    
    # Plot 2: Actual vs predicted — linear model
    p_lm_fit <- ggplot(diag_df, aes(x = actual, y = pred_lm)) +
      geom_point(alpha = 0.3, size = 1.5, color = "#00d4ff") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Linear model: actual vs predicted",
           x = "Actual pace (min/km)", y = "Predicted pace (min/km)") +
      theme_minimal()
    print(p_lm_fit)
    
    # Plot 3: Linear residuals vs fitted (heteroscedasticity check)
    p_lm_resid <- ggplot(diag_df, aes(x = pred_lm, y = res_lm)) +
      geom_point(alpha = 0.3, size = 1.5, color = "#00d4ff") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE, color = "#ff6b6b", linewidth = 0.8) +
      labs(title = "Linear residuals vs fitted (heteroscedasticity check)",
           x = "Fitted values", y = "Residuals") +
      theme_minimal()
    print(p_lm_resid)
    
    # Plot 4: QQ plot — are residuals normal?
    p_qq <- ggplot(diag_df, aes(sample = res_lm)) +
      stat_qq(alpha = 0.4, color = "#00d4ff") +
      stat_qq_line(color = "red") +
      labs(title = "QQ plot — linear model residuals",
           x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_minimal()
    print(p_qq)
    
    # Plot 5: Residual distribution
    p_res_dist <- ggplot(diag_df) +
      geom_density(aes(x = res_lm),  fill = "#00d4ff", alpha = 0.4) +
      geom_density(aes(x = res_rf),  fill = "#00cc44", alpha = 0.4) +
      geom_density(aes(x = res_xgb), fill = "#ffaa00", alpha = 0.4) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residual distributions (blue=LM, green=RF, orange=XGB)",
           x = "Residual (min/km)", y = "Density") +
      theme_minimal()
    print(p_res_dist)
    
    # Plot 6: RF feature importance
    imp_df <- as.data.frame(importance(rf_final, type = 1)) %>%
      tibble::rownames_to_column("feature") %>%
      rename(inc_mse = `%IncMSE`) %>%
      arrange(desc(inc_mse)) %>%
      head(15)
    
    p_imp <- ggplot(imp_df, aes(x = reorder(feature, inc_mse), y = inc_mse)) +
      geom_col(fill = "#00cc44") +
      coord_flip() +
      labs(title = "Random Forest feature importance (%IncMSE)",
           x = NULL, y = "%IncMSE") +
      theme_minimal()
    print(p_imp)
    
    # Plot 7: Residuals over time (is the model getting worse/better?)
    p_time <- ggplot(diag_df, aes(x = date)) +
      geom_line(aes(y = res_lm),  color = "#00d4ff", alpha = 0.5) +
      geom_line(aes(y = res_rf),  color = "#00cc44", alpha = 0.5) +
      geom_line(aes(y = res_xgb), color = "#ffaa00", alpha = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals over time (blue=LM, green=RF, orange=XGB)",
           x = "Date", y = "Residual (min/km)") +
      theme_minimal()
    print(p_time)
  }
}


# ============================================================
# 10. RESULTS & RECOMMENDATION
# ============================================================
{
  cat("--- Section 10: Results & recommendation ---\n\n")
  
  cat("=== FINAL SUMMARY ===\n")
  print(summary_table)
  
  cat("\n=== INTERPRETATION ===\n")
  
  best_model <- summary_table$model[1]
  best_rmse  <- summary_table$RMSE[1]
  naive_rmse <- summary_table %>% filter(str_detect(model, "naive")) %>%
    slice_min(RMSE, n=1) %>% pull(RMSE)
  improvement_pct <- round((1 - best_rmse / naive_rmse) * 100, 1)
  
  cat(sprintf("  Best model:         %s\n", best_model))
  cat(sprintf("  Best RMSE:          ±%s min/km\n", pace_dec_to_str(best_rmse)))
  cat(sprintf("  Best naive RMSE:    ±%s min/km\n", pace_dec_to_str(naive_rmse)))
  cat(sprintf("  Improvement:        %.1f%% over best naive baseline\n\n", improvement_pct))
  
  cat("=== OVERFITTING CHECK ===\n")
  # Compare in-sample RMSE vs CV RMSE for each model
  insample_lm  <- rmse(ml_df$pace_mean, predict(lm_step_final))
  insample_rf  <- rmse(ml_df$pace_mean, predict(rf_final, newdata = ml_matrix))
  insample_xgb <- rmse(ml_df$pace_mean, predict(xgb_final,
                                                xgb.DMatrix(as.matrix(ml_matrix))))
  
  cv_lm  <- summary_table %>% filter(model == "lm_stepwise")  %>% pull(RMSE)
  cv_rf  <- summary_table %>% filter(model == "random_forest") %>% pull(RMSE)
  cv_xgb <- summary_table %>% filter(model == "xgboost")       %>% pull(RMSE)
  
  overfit_df <- tibble(
    model       = c("lm_stepwise", "random_forest", "xgboost"),
    in_sample   = round(c(insample_lm, insample_rf, insample_xgb), 4),
    cv_rmse     = round(c(cv_lm, cv_rf, cv_xgb), 4),
    overfit_gap = round(cv_rmse - in_sample, 4)
  )
  print(overfit_df)
  
  cat("\n=== NOTES FOR SHINY INTEGRATION ===\n")
  cat("  Input:  distance_km (numeric) + terrain (flat/small_hills/hilly)\n")
  cat("  Terrain maps to ascent_per_km / descent_per_km via terrain_lookup\n")
  cat("  Rolling features computed from latest N races using today()\n")
  cat("  Output: predicted pace per model + confidence interval\n")
  cat("  CI:     use residual SD from CV as ±1.96 * sd for 95% interval\n\n")
  
  # Save terrain_lookup for Shiny use
  saveRDS(terrain_lookup, file.path(BASE_DIR, "terrain_lookup.rds"))
  
  # Save final models for Shiny use
  saveRDS(lm_step_final, file.path(BASE_DIR, "model_lm.rds"))
  
  # Save pace only model, good results
  lm_pace_only_final <- lm(pace_mean ~ avg_pace_last3 + avg_pace_last5 +
                             avg_pace_last10 + avg_pace_last14d,
                           data = ml_df)
  saveRDS(lm_pace_only_final, file.path(BASE_DIR, "model_lm.rds"))
  
  saveRDS(rf_final,      file.path(BASE_DIR, "model_rf.rds"))
  saveRDS(xgb_final,     file.path(BASE_DIR, "model_xgb.rds"))
  saveRDS(FEATURE_COLS,  file.path(BASE_DIR, "feature_cols.rds"))
  saveRDS(feat_means,    file.path(BASE_DIR, "feat_means.rds"))
  saveRDS(feat_sds,      file.path(BASE_DIR, "feat_sds.rds"))
  
  cat("  Models saved to nike_data/ for Shiny use.\n")
  cat("\n=== ML Lab complete ===\n")
  
  saveRDS(ml_df,     file.path(BASE_DIR, "ml_df.rds"))
  saveRDS(ml_matrix, file.path(BASE_DIR, "ml_matrix.rds"))
}
