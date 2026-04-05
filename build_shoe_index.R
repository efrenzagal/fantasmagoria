# ============================================================
# BUILD shoe_index.csv — run once, then never again
# ============================================================
# Reads all detail JSONs and extracts just id + shoe_id.
# Result: nike_data/shoe_index.csv (~855 rows, loads in <0.01s)
#
# Re-run only if you need to rebuild from scratch.
# Normal updates are handled incrementally by fetch_new_activities().
# ============================================================

source("fantasmagoria_functions.R")
library(jsonlite)
library(dplyr)

cat(sprintf("Reading %d detail JSONs...\n",
            length(list.files(DETAIL_DIR, pattern = "\\.json$"))))

json_files <- list.files(DETAIL_DIR, pattern = "\\.json$", full.names = TRUE)
t0 <- proc.time()

shoe_index <- lapply(json_files, function(f) {
  tryCatch({
    result <- fromJSON(f, simplifyVector = TRUE)
    tags   <- result$tags
    data.frame(
      id      = result$id %||% NA_character_,
      shoe_id = if (is.data.frame(tags)) tags[["shoe_id"]] %||% NA_character_
      else                     tags[["shoe_id"]] %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("  WARNING: failed for", basename(f), ":", e$message, "\n")
    NULL
  })
}) %>% bind_rows()

elapsed <- (proc.time() - t0)[["elapsed"]]

out_path <- file.path(BASE_DIR, "shoe_index.csv")
write.csv(shoe_index, out_path, row.names = FALSE)

cat(sprintf("Done in %.1f sec\n", elapsed))
cat(sprintf("Saved %d rows -> %s\n", nrow(shoe_index), out_path))
cat(sprintf("Unique shoe IDs: %d\n",
            length(unique(shoe_index$shoe_id[!is.na(shoe_index$shoe_id)]))))