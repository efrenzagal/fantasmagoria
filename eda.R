# ============================================================
# Nike Run Club — Exploratory Data Analysis (EDA)
# ============================================================
# Project folder: ~/Fantasmagoria 2.0/
# Data folder:    ~/Fantasmagoria 2.0/nike_data/
# ============================================================

# Libraries & config
{
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  
  BASE_DIR   <- "~/Fantasmagoria 2.0/nike_data"
  DETAIL_DIR <- file.path(BASE_DIR, "activity_details")
  N_SAMPLE   <- 10   # number of detail JSONs to sample for detailed EDA
}


# ============================================================
# PART 1: SUMMARY-LEVEL EDA
# ============================================================
{
  cat("=======================================================\n")
  cat("PART 1: SUMMARY-LEVEL EDA\n")
  cat("=======================================================\n\n")
  
  # Load CSV
  {
    csv_path <- file.path(BASE_DIR, "activity_summaries.csv")
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$start_time <- as.POSIXct(df$start_time, tz = "UTC")
    df$end_time   <- as.POSIXct(df$end_time,   tz = "UTC")
  }
  
  # Shape
  {
    cat("── Shape ───────────────────────────────────────────────\n")
    cat(sprintf("Rows: %d | Columns: %d\n\n", nrow(df), ncol(df)))
  }
  
  # Column names & types
  {
    cat("── Column names ────────────────────────────────────────\n")
    print(names(df))
    cat("\n")
    
    cat("── Column types ────────────────────────────────────────\n")
    print(sapply(df, class))
    cat("\n")
  }
  
  # First 3 rows
  {
    cat("── First 3 rows ────────────────────────────────────────\n")
    print(head(df, 3))
    cat("\n")
  }
  
  # Missing values
  {
    cat("── Missing values per column ───────────────────────────\n")
    missing <- sort(colSums(is.na(df)), decreasing = TRUE)
    missing <- missing[missing > 0]
    if (length(missing) == 0) {
      cat("No missing values!\n")
    } else {
      print(missing)
    }
    cat("\n")
  }
  
  # Numeric summary
  {
    cat("── Numeric summary ─────────────────────────────────────\n")
    numeric_cols <- df |> select(where(is.numeric))
    print(summary(numeric_cols))
    cat("\n")
  }
  
  # Activity types & status
  {
    cat("── Activity types ──────────────────────────────────────\n")
    print(table(df$type))
    cat("\n")
    
    cat("── Status breakdown ────────────────────────────────────\n")
    print(table(df$status))
    cat("\n")
  }
  
  # Date range
  {
    cat("── Date range ──────────────────────────────────────────\n")
    cat(sprintf("  Earliest run: %s\n", min(df$start_time, na.rm = TRUE)))
    cat(sprintf("  Latest run:   %s\n", max(df$start_time, na.rm = TRUE)))
    cat(sprintf("  Span:         %.1f years\n",
                as.numeric(difftime(max(df$start_time, na.rm = TRUE),
                                    min(df$start_time, na.rm = TRUE),
                                    units = "days")) / 365))
    cat("\n")
  }
  
  # Distance distribution
  {
    if ("distance_total" %in% names(df)) {
      cat("── Distance (km) distribution ──────────────────────────\n")
      d <- df$distance_total   # meters to km
      cat(sprintf("  Min:    %.2f km\n", min(d, na.rm = TRUE)))
      cat(sprintf("  Median: %.2f km\n", median(d, na.rm = TRUE)))
      cat(sprintf("  Mean:   %.2f km\n", mean(d, na.rm = TRUE)))
      cat(sprintf("  Max:    %.2f km\n", max(d, na.rm = TRUE)))
      cat("\n")
    }
  }
  
  # Duration distribution
  {
    if ("duration_min" %in% names(df)) {
      cat("── Duration (min) distribution ─────────────────────────\n")
      cat(sprintf("  Min:    %.1f min\n", min(df$duration_min, na.rm = TRUE)))
      cat(sprintf("  Median: %.1f min\n", median(df$duration_min, na.rm = TRUE)))
      cat(sprintf("  Mean:   %.1f min\n", mean(df$duration_min, na.rm = TRUE)))
      cat(sprintf("  Max:    %.1f min\n", max(df$duration_min, na.rm = TRUE)))
      cat("\n")
    }
  }
  
  # Pace distribution
  {
    if ("pace_mean" %in% names(df)) {
      cat("── Pace (min/km) distribution ──────────────────────────\n")
      cat(sprintf("  Min:    %.2f min/km\n", min(df$pace_mean, na.rm = TRUE)))
      cat(sprintf("  Median: %.2f min/km\n", median(df$pace_mean, na.rm = TRUE)))
      cat(sprintf("  Mean:   %.2f min/km\n", mean(df$pace_mean, na.rm = TRUE)))
      cat(sprintf("  Max:    %.2f min/km\n", max(df$pace_mean, na.rm = TRUE)))
      cat("\n")
    }
  }
  
  # Calories distribution
  {
    if ("calories_total" %in% names(df)) {
      cat("── Calories burned distribution ────────────────────────\n")
      cat(sprintf("  Min:    %.0f kcal\n", min(df$calories_total, na.rm = TRUE)))
      cat(sprintf("  Median: %.0f kcal\n", median(df$calories_total, na.rm = TRUE)))
      cat(sprintf("  Mean:   %.0f kcal\n", mean(df$calories_total, na.rm = TRUE)))
      cat(sprintf("  Max:    %.0f kcal\n", max(df$calories_total, na.rm = TRUE)))
      cat("\n")
    }
  }
}


# ============================================================
# PART 2: DETAILED-LEVEL EDA (sample of N_SAMPLE JSONs)
# ============================================================
{
  cat("=======================================================\n")
  cat(sprintf("PART 2: DETAILED-LEVEL EDA (sample of %d activities)\n", N_SAMPLE))
  cat("=======================================================\n\n")
  
  # Load sample of detail JSONs
  {
    detail_files <- list.files(DETAIL_DIR, pattern = "\\.json$", full.names = TRUE)
    cat(sprintf("Total detail files available: %d\n\n", length(detail_files)))
    
    set.seed(42)
    sampled_files <- sample(detail_files, min(N_SAMPLE, length(detail_files)))
    details <- map(sampled_files, \(f) fromJSON(f, simplifyVector = TRUE))
  }
  
  # Top-level keys
  {
    cat("── Top-level keys in a detail JSON ─────────────────────\n")
    print(names(details[[1]]))
    cat("\n")
  }
  
  # Metric types available
  {
    cat("── Metric types available (from metric_types field) ────\n")
    all_metric_types <- map(details, \(d) {
      mt <- d$metric_types
      if (is.list(mt)) unlist(mt) else mt
    }) |> unlist() |> unique() |> sort()
    print(all_metric_types)
    cat("\n")
  }
  
  # Tags / metadata fields
  {
    cat("── Tags fields available ───────────────────────────────\n")
    all_tag_keys <- map(details, \(d) {
      if (is.data.frame(d$tags)) names(d$tags)
      else if (is.list(d$tags)) names(d$tags)
      else character(0)
    }) |> unlist() |> unique() |> sort()
    print(all_tag_keys)
    cat("\n")
  }
  
  # Moments (splits)
  {
    cat("── Moments (splits) keys available ─────────────────────\n")
    sample_moments <- map(details, \(d) {
      if (is.data.frame(d$moments)) names(d$moments) else character(0)
    }) |> unlist() |> unique() |> sort()
    print(sample_moments)
    cat("\n")
    
    cat("── Example moments (splits) for one activity ───────────\n")
    for (d in details) {
      if (is.data.frame(d$moments) && nrow(d$moments) > 0) {
        print(head(d$moments, 5))
        break
      }
    }
    cat("\n")
  }
  
  # GPS / polylines
  {
    cat("── Polylines (GPS route) availability ──────────────────\n")
    has_polyline <- map_lgl(details, \(d) {
      !is.null(d$polylines) && length(d$polylines) > 0
    })
    cat(sprintf("  Activities with GPS polyline: %d / %d sampled\n",
                sum(has_polyline), length(details)))
    cat("\n")
  }
  
  # Raw time-series metrics
  {
    cat("── Raw metrics (time-series) availability ───────────────\n")
    has_metrics <- map_lgl(details, \(d) {
      m <- d$metrics
      !is.null(m) && length(m) > 0 && !(is.list(m) && length(m[[1]]) == 0)
    })
    cat(sprintf("  Activities with raw time-series metrics: %d / %d sampled\n",
                sum(has_metrics), length(details)))
    cat("\n")
  }
}


# ============================================================
# SUMMARY: What's available for analysis
# ============================================================
{
  cat("=======================================================\n")
  cat("SUMMARY: What's available for analysis\n")
  cat("=======================================================\n")
  cat(sprintf("  Total runs:              %d\n", nrow(df)))
  cat(sprintf("  Date range:              %.1f years\n",
              as.numeric(difftime(max(df$start_time, na.rm = TRUE),
                                  min(df$start_time, na.rm = TRUE),
                                  units = "days")) / 365))
  cat(sprintf("  Summary CSV columns:     %d\n", ncol(df)))
  cat(sprintf("  Detail metric types:     %d\n", length(all_metric_types)))
  cat(sprintf("  Detail tag fields:       %d\n", length(all_tag_keys)))
  cat(sprintf("  GPS routes available:    %s\n",
              ifelse(any(has_polyline), "Yes", "No")))
  cat(sprintf("  Time-series metrics:     %s\n",
              ifelse(any(has_metrics), "Yes", "No")))
  cat("\nAll metric types found:\n")
  cat(paste(" ", all_metric_types, collapse = "\n"), "\n")
  cat("\nAll tag fields found:\n")
  cat(paste(" ", all_tag_keys, collapse = "\n"), "\n")
}
