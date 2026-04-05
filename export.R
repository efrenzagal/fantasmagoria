# ============================================================
# Nike Run Club Data Exporter
# ============================================================
# HOW TO GET YOUR TOKEN (do this right before running!):
#   1. Open Chrome/Firefox and go to https://www.nike.com/member/profile
#   2. Open DevTools (F12) -> Network tab
#   3. Check "Preserve log" / "Guardar el registro"
#   4. Filter by: api.nike.com
#   5. Click any request with status 200
#   6. Go to Headers -> Request Headers
#   7. Copy the value after "authorization: Bearer "
#   8. Paste it below
#
# ⚠️  Token expires quickly — grab a fresh one and run immediately!
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- CONFIG ------------------------------------------------

ACCESS_TOKEN <- ""          # paste your Bearer token here
OUTPUT_DIR   <- "nike_data" # folder to save files

# ---- SETUP -------------------------------------------------

dir.create(OUTPUT_DIR, showWarnings = FALSE)

nike_get <- function(url) {
  resp <- request(url) |>
    req_headers(Authorization = paste("Bearer", ACCESS_TOKEN)) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  
  if (resp_status(resp) != 200) {
    stop(paste("Status:", resp_status(resp),
               "- grab a fresh token from the Network tab and try again."))
  }
  
  resp |> resp_body_json(simplifyVector = TRUE)
}

# ---- STEP 1: Paginate and collect all activities -----------

cat("Fetching activity summaries...\n")

all_activities <- data.frame()
next_url <- paste0(
  "https://api.nike.com/plus/v3/activities/before_id/v3/*",
  "?limit=30&types=run,jogging&include_deleted=false"
)
page <- 1

repeat {
  cat(sprintf("  Page %d (fetched %d so far)...\n", page, nrow(all_activities)))
  
  data <- nike_get(next_url)
  
  activities <- data$activities
  if (is.null(activities) || nrow(activities) == 0) break
  
  all_activities <- bind_rows(all_activities, activities)
  
  before_id <- data$paging$before_id
  if (is.null(before_id) || before_id == "") break
  
  next_url <- paste0(
    "https://api.nike.com/plus/v3/activities/before_id/v3/", before_id,
    "?limit=30&types=run,jogging&include_deleted=false"
  )
  page <- page + 1
  Sys.sleep(0.3)
}

cat(sprintf("Found %d activities total.\n", nrow(all_activities)))

# Save raw JSON before any transformation
json_path <- file.path(OUTPUT_DIR, "activity_summaries.json")
write_json(all_activities, json_path, pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Saved raw JSON -> %s\n", json_path))

# ---- STEP 2: Flatten metrics into wide format --------------

cat("Flattening metrics...\n")

extract_metrics <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble())
  if (!all(c("metric", "summary", "value") %in% names(df))) return(tibble())
  
  df |>
    mutate(col = paste0(metric, "_", summary)) |>
    select(col, value) |>
    pivot_wider(names_from = col, values_from = value, values_fn = first)
}

metrics_df <- bind_rows(lapply(all_activities$summaries, extract_metrics))

# Build clean summary table
summary_df <- all_activities |>
  select(id, type, start_epoch_ms, end_epoch_ms, active_duration_ms, status) |>
  bind_cols(metrics_df) |>
  mutate(
    start_time   = as.POSIXct(start_epoch_ms / 1000, origin = "1970-01-01", tz = "UTC"),
    end_time     = as.POSIXct(end_epoch_ms   / 1000, origin = "1970-01-01", tz = "UTC"),
    duration_min = active_duration_ms / 60000
  )

# Save CSV
csv_path <- file.path(OUTPUT_DIR, "activity_summaries.csv")
write.csv(summary_df, csv_path, row.names = FALSE)
cat(sprintf("Saved CSV -> %s\n", csv_path))

# ---- STEP 3: Download full detail per activity -------------

cat("\nDownloading full activity details (GPS, splits, etc.)...\n")
detail_dir <- file.path(OUTPUT_DIR, "activity_details")
dir.create(detail_dir, showWarnings = FALSE)

ids <- all_activities$id

# Accumulate shoe_id as we go — no second pass needed
shoe_index_rows <- vector("list", length(ids))

for (i in seq_along(ids)) {
  id       <- ids[[i]]
  out_file <- file.path(detail_dir, paste0(id, ".json"))
  
  if (file.exists(out_file)) {
    cat(sprintf("  [%d/%d] Already downloaded, skipping: %s\n", i, length(ids), id))
    
    # Still need shoe_id from the cached file
    tryCatch({
      detail  <- fromJSON(out_file, simplifyVector = TRUE)
      tags    <- detail$tags
      shoe_id <- if (is.data.frame(tags)) tags[["shoe_id"]] %||% NA_character_
      else                     tags[["shoe_id"]] %||% NA_character_
      shoe_index_rows[[i]] <- data.frame(id = id, shoe_id = shoe_id,
                                         stringsAsFactors = FALSE)
    }, error = function(e) {
      shoe_index_rows[[i]] <<- data.frame(id = id, shoe_id = NA_character_,
                                          stringsAsFactors = FALSE)
    })
    next
  }
  
  cat(sprintf("  [%d/%d] Fetching: %s\n", i, length(ids), id))
  
  tryCatch({
    detail_url <- paste0("https://api.nike.com/sport/v3/me/activity/", id, "?metrics=ALL")
    detail     <- nike_get(detail_url)
    write_json(detail, out_file, pretty = TRUE, auto_unbox = TRUE)
    
    # Extract shoe_id while the JSON is already in memory
    tags    <- detail$tags
    shoe_id <- if (is.data.frame(tags)) tags[["shoe_id"]] %||% NA_character_
    else                     tags[["shoe_id"]] %||% NA_character_
    shoe_index_rows[[i]] <- data.frame(id = id, shoe_id = shoe_id,
                                       stringsAsFactors = FALSE)
  }, error = function(e) {
    cat(sprintf("    WARNING: failed for %s: %s\n", id, e$message))
    shoe_index_rows[[i]] <- data.frame(id = id, shoe_id = NA_character_,
                                       stringsAsFactors = FALSE)
  })
  
  Sys.sleep(0.3)
}

# ---- STEP 4: Save shoe_index.csv ---------------------------

cat("\nBuilding shoe_index.csv...\n")
shoe_index <- bind_rows(shoe_index_rows)
shoe_index_path <- file.path(OUTPUT_DIR, "shoe_index.csv")
write.csv(shoe_index, shoe_index_path, row.names = FALSE)
cat(sprintf("Saved shoe_index.csv -> %s (%d rows, %d unique shoe IDs)\n",
            shoe_index_path,
            nrow(shoe_index),
            length(unique(shoe_index$shoe_id[!is.na(shoe_index$shoe_id)]))))

# ---- DONE --------------------------------------------------

cat("\n✅ Done! Files saved to:", OUTPUT_DIR, "\n")
cat("  activity_summaries.json    raw API response\n")
cat("  activity_summaries.csv     flat table (distance, pace, calories, etc.)\n")
cat("  activity_details/          one JSON per activity (GPS, splits, etc.)\n")
cat("  shoe_index.csv             id + shoe_id lookup (used by dashboard)\n")
cat(sprintf("\nTotal activities exported: %d\n", nrow(summary_df)))
cat("Columns in CSV:\n")
print(names(summary_df))
