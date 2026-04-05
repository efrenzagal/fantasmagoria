# ============================================================
# Fantasmagoria 3.0 — Functions
# ============================================================

# Libraries
{
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(httr2)
}

# ============================================================
# CONFIG
# ============================================================
{
  BASE_DIR   <- "~/Fantasmagoria 3.0/nike_data"
  DETAIL_DIR <- file.path(BASE_DIR, "activity_details")
  LAMBDA     <- -2   # Box-Cox lambda for pace transformation
  TZ         <- "America/Mexico_City"
  
  # Dark theme colors
  COL_BG       <- "#1a1a1a"
  COL_BOX      <- "#2b2b2b"
  COL_TEXT     <- "#f0f0f0"
  COL_GRID     <- "#444444"
  COL_ACCENT   <- "#00d4ff"   # cyan — dots, lines
  COL_TREND    <- "#ff6b6b"   # red — trend line / avg pace reference
  COL_ORANGE   <- "#ffaa00"   # orange — elevation
}


# ============================================================
# DARK THEME HELPERS
# ============================================================
{
  # ggplot2 dark theme to apply to all plots
  theme_fantasmagoria <- function() {
    theme_minimal() +
      theme(
        plot.background   = element_rect(fill = COL_BG,  color = NA),
        panel.background  = element_rect(fill = COL_BOX, color = NA),
        panel.grid.major  = element_line(color = COL_GRID),
        panel.grid.minor  = element_line(color = COL_GRID),
        axis.text         = element_text(color = COL_TEXT),
        axis.title        = element_text(color = COL_TEXT),
        legend.background = element_rect(fill = COL_BOX, color = NA),
        legend.text       = element_text(color = COL_TEXT),
        legend.title      = element_text(color = COL_TEXT),
        plot.title        = element_text(color = COL_TEXT)
      )
  }
  
  # Apply dark layout to any plotly object
  apply_dark_theme <- function(p) {
    p %>% layout(
      paper_bgcolor = COL_BOX,
      plot_bgcolor  = COL_BG,
      font          = list(color = COL_TEXT),
      xaxis = list(
        color     = COL_TEXT,
        gridcolor = COL_GRID,
        linecolor = COL_TEXT,
        tickcolor = COL_TEXT,
        zerolinecolor = COL_GRID
      ),
      yaxis = list(
        color     = COL_TEXT,
        gridcolor = COL_GRID,
        linecolor = COL_TEXT,
        tickcolor = COL_TEXT,
        zerolinecolor = COL_GRID
      ),
      legend = list(font = list(color = COL_TEXT))
    )
  }
}


# ============================================================
# DATA LOADING
# ============================================================
{
  # Load and clean summary CSV
  load_summaries <- function() {
    {
      path <- file.path(BASE_DIR, "activity_summaries.csv")
      if (!file.exists(path)) stop(paste("CSV not found at:", path))
      df <- fread(path) %>% as.data.frame()
    }
    
    # Parse datetimes
    {
      df$start_time <- as.POSIXct(df$start_time, tz = "UTC")
      df$end_time   <- as.POSIXct(df$end_time,   tz = "UTC")
      df$start_time <- with_tz(df$start_time, TZ)
      df$end_time   <- with_tz(df$end_time,   TZ)
    }
    
    # Clean & derive basic columns
    {
      df <- df %>%
        mutate(
          duration_min      = active_duration_ms / 60000,
          time_minutes      = pace_mean * distance_total,
          ascent_per_km     = ascent_total / distance_total,
          net_ascent_per_km = (ascent_total - descent_total) / distance_total,
          temperature       = as.numeric(NA),
          weekday           = lubridate::wday(start_time, label = TRUE),
          hour              = lubridate::hour(start_time),
          year              = lubridate::year(start_time),
          month             = lubridate::month(start_time, label = TRUE)
        )
    }
    
    return(df)
  }
  
  # Load a single detail JSON by activity id
  load_detail <- function(activity_id) {
    {
      path <- file.path(DETAIL_DIR, paste0(activity_id, ".json"))
      if (!file.exists(path)) stop(paste("Detail file not found for id:", activity_id))
      result <- fromJSON(path, simplifyVector = TRUE)
    }
    return(result)
  }
  
  # Extract time-series metrics from a detail JSON
  extract_detail_timeseries <- function(activity_id) {
    {
      result      <- load_detail(activity_id)
      metrics_raw <- result$metrics
      if (is.null(metrics_raw) || length(metrics_raw) == 0) return(NULL)
    }
    
    {
      vars      <- c("distance", "pace", "elevation", "latitude", "longitude",
                     "speed", "steps", "heart_rate", "ascent", "descent")
      list_vars <- vector("list", length(vars))
      names(list_vars) <- vars
      
      for (var in vars) {
        rows <- metrics_raw %>% filter(type == var)
        if (nrow(rows) == 0 || is.null(rows$values)) next
        vals <- rows$values[[1]]
        if (is.null(vals) || nrow(vals) == 0) next
        list_vars[[var]] <- vals %>% mutate(variable = var, id = activity_id)
      }
      
      all_data <- bind_rows(list_vars) %>%
        select(id, variable, value, start_epoch_ms, end_epoch_ms) %>%
        mutate(
          start_time = as.POSIXct(start_epoch_ms / 1000, origin = "1970-01-01", tz = "UTC") %>%
            with_tz(TZ)
        )
    }
    
    return(all_data)
  }
  
  # Extract tags from a detail JSON
  extract_detail_tags <- function(activity_id) {
    {
      result <- load_detail(activity_id)
      tags   <- result$tags
      if (is.null(tags)) return(data.frame(id = activity_id))
    }
    
    {
      tag_df <- data.frame(
        id          = activity_id,
        run_name    = tags[["com.nike.name"]]              %||% NA_character_,
        weather     = tags[["com.nike.weather"]]           %||% NA_character_,
        temperature = tags[["com.nike.temperature"]]       %||% NA_character_,
        location    = tags[["location"]]                   %||% NA_character_,
        shoe_id     = tags[["shoe_id"]]                    %||% NA_character_,
        goal_type   = tags[["com.nike.running.goaltype"]]  %||% NA_character_,
        stringsAsFactors = FALSE
      )
      tag_df$temperature <- as.numeric(tag_df$temperature)
    }
    
    return(tag_df)
  }
  
  # Extract km splits (moments) from a detail JSON
  # Uses speed time-series to subtract pause time (traffic lights, shoe tying)
  extract_detail_splits <- function(activity_id) {
    # Load moments and time-series data
    {
      result  <- load_detail(activity_id)
      moments <- result$moments
      if (is.null(moments) || !is.data.frame(moments) || nrow(moments) == 0) return(NULL)
    }
    
    # Build splits with active-only pace (excluding pauses)
    {
      splits <- moments %>%
        filter(key == "split_km") %>%
        mutate(
          id        = activity_id,
          km        = as.numeric(value),
          timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC") %>%
            with_tz(TZ)
        ) %>%
        arrange(km)
      
      run_start <- as.POSIXct(result$start_epoch_ms / 1000,
                              origin = "1970-01-01", tz = "UTC") %>% with_tz(TZ)
      
      # Try to get speed time-series to detect pauses
      ts <- tryCatch(extract_detail_timeseries(activity_id), error = function(e) NULL)
      speed_ts <- NULL
      if (!is.null(ts)) {
        speed_ts <- ts %>%
          filter(variable == "speed") %>%
          arrange(start_epoch_ms) %>%
          select(start_epoch_ms, end_epoch_ms, speed = value)
      }
      
      splits$prev_time <- c(run_start, splits$timestamp[-nrow(splits)])
      
      if (!is.null(speed_ts) && nrow(speed_ts) > 2) {
        # For each split, sum only the time segments where speed > 0
        PAUSE_THRESHOLD <- 0.5  # m/s — below this counts as paused
        active_durations <- numeric(nrow(splits))
        
        for (i in seq_len(nrow(splits))) {
          split_start_ms <- as.numeric(splits$prev_time[i]) * 1000
          split_end_ms   <- as.numeric(splits$timestamp[i]) * 1000
          
          segs <- speed_ts %>%
            filter(start_epoch_ms >= split_start_ms,
                   start_epoch_ms <= split_end_ms)
          
          if (nrow(segs) > 0) {
            # Each segment duration where runner was moving
            segs <- segs %>%
              mutate(seg_duration = (end_epoch_ms - start_epoch_ms) / 1000) %>%
              filter(speed >= PAUSE_THRESHOLD)
            active_durations[i] <- sum(segs$seg_duration, na.rm = TRUE)
          } else {
            # Fallback to wall-clock time
            active_durations[i] <- as.numeric(difftime(splits$timestamp[i],
                                                       splits$prev_time[i], units = "secs"))
          }
        }
        
        splits$split_duration_s <- active_durations
      } else {
        # Fallback: use wall-clock time if no speed data
        splits$split_duration_s <- as.numeric(difftime(splits$timestamp,
                                                       splits$prev_time, units = "secs"))
      }
      
      splits$split_pace <- splits$split_duration_s / 60
      splits <- splits %>% select(id, km, timestamp, split_duration_s, split_pace)
    }
    
    return(splits)
  }
}


# ============================================================
# SCORING
# ============================================================
{
  bc_transform_distance <- function(x) log(x)
  bc_transform_pace     <- function(x, lambda = LAMBDA) (x ^ lambda - 1) / lambda
  
  add_scores <- function(df, lambda = LAMBDA) {
    {
      df <- df %>%
        mutate(
          t_distance = bc_transform_distance(distance_total),
          t_pace     = bc_transform_pace(pace_mean, lambda),
          s_distance = as.numeric(scale(t_distance)),
          s_pace     = as.numeric(scale(-t_pace)),
          composite  = (s_distance + s_pace) / 2,
          score      = 100 * ecdf(composite)(composite)
        )
    }
    return(df)
  }
  
  add_dynamic_scores <- function(df) {
    {
      df         <- df %>% arrange(start_time)
      n          <- nrow(df)
      dyn_scores <- rep(NA_real_, n)
    }
    
    {
      for (i in 3:n) {
        past_dist    <- df$distance_total[1:(i - 1)]
        past_pace    <- df$pace_mean[1:(i - 1)]
        pct_dist     <- ecdf(past_dist)(df$distance_total[i])
        pct_pace     <- 1 - ecdf(past_pace)(df$pace_mean[i])
        dyn_scores[i] <- 100 * (pct_dist + pct_pace) / 2
      }
      df$dynamic_score <- dyn_scores
    }
    
    return(df)
  }
  
  score_new_race <- function(distance, pace_dec, all_races_df, lambda = LAMBDA) {
    {
      t_dist_new    <- bc_transform_distance(distance)
      t_pace_new    <- bc_transform_pace(pace_dec, lambda)
      s_dist_new    <- (t_dist_new - mean(all_races_df$t_distance, na.rm = TRUE)) /
        sd(all_races_df$t_distance, na.rm = TRUE)
      s_pace_new    <- (-t_pace_new - mean(-all_races_df$t_pace, na.rm = TRUE)) /
        sd(-all_races_df$t_pace, na.rm = TRUE)
      composite_new <- (s_dist_new + s_pace_new) / 2
      score         <- 100 * ecdf(all_races_df$composite)(composite_new)
    }
    return(round(score, 1))
  }
}


# ============================================================
# UTILITIES
# ============================================================
{
  `%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
  
  pace_str_to_dec <- function(x) {
    parts <- str_split(x, "'")[[1]] %>% as.numeric()
    parts[1] + parts[2] / 60
  }
  
  pace_dec_to_str <- function(x) {
    mins <- floor(x)
    secs <- round((x - mins) * 60)
    sprintf("%d'%02d", mins, secs)
  }
  
  # Compute efficiency frontier: best pace at each distance level (Pareto optimal)
  compute_efficiency_frontier <- function(df) {
    {
      frontier <- df %>%
        arrange(distance_total) %>%
        mutate(running_min_pace = cummin(pace_mean)) %>%
        filter(pace_mean == running_min_pace) %>%
        select(distance_total, pace_mean, score, start_time)
    }
    return(frontier)
  }
  
  find_similar_race <- function(distance, pace_dec, net_ascent_per_km, all_races_df, k = 1) {
    {
      train        <- all_races_df %>%
        select(distance_total, pace_mean, net_ascent_per_km) %>%
        as.matrix()
      test         <- matrix(c(distance, pace_dec, net_ascent_per_km), nrow = 1)
      col_means    <- colMeans(train, na.rm = TRUE)
      col_sds      <- apply(train, 2, sd, na.rm = TRUE)
      train_scaled <- scale(train, center = col_means, scale = col_sds)
      test_scaled  <- (test - col_means) / col_sds
      dists        <- apply(train_scaled, 1, function(row) sqrt(sum((row - test_scaled)^2)))
      idx          <- order(dists)[1:k]
    }
    return(all_races_df[idx, ])
  }
}


# ============================================================
# TREND PLOT
# ============================================================
{
  create_trend_plot <- function(df, start_date, end_date, period,
                                agg_fn_name, metric, min_races = 1,
                                show_trend = TRUE) {
    {
      agg_fn <- switch(agg_fn_name, mean = mean, sum = sum, median = median)
      
      y_label <- paste(
        switch(agg_fn_name, mean = "Average", sum = "Total", median = "Median"),
        switch(metric,
               distance_total = "Distance (km)",
               pace_mean      = "Pace (min/km)",
               duration_min   = "Duration (min)",
               ascent_total   = "Ascent (m)",
               cadence_mean   = "Cadence (spm)",
               score          = "Score",
               dynamic_score  = "Dynamic Score",
               calories_total = "Calories",
               metric)
      )
    }
    
    {
      plot_df <- df %>%
        filter(start_time >= as.POSIXct(start_date, tz = TZ),
               start_time <= as.POSIXct(end_date,   tz = TZ)) %>%
        mutate(period = floor_date(start_time, period)) %>%
        group_by(period) %>%
        summarise(
          value   = agg_fn(!!sym(metric), na.rm = TRUE),
          n_races = n(),
          .groups = "drop"
        ) %>%
        filter(n_races >= min_races)
    }
    
    {
      p <- plot_df %>%
        ggplot(aes(x = period, y = value,
                   text = paste0("Period: ", period,
                                 "<br>Value: ", round(value, 2),
                                 "<br>Runs: ", n_races))) +
        geom_point(color = "#00d4ff", size = 2.5) +
        xlab("Date") +
        ylab(y_label) +
        theme_fantasmagoria()
      
      if (show_trend && nrow(plot_df) >= 4) {
        loess_fit     <- loess(value ~ as.numeric(period), data = plot_df)
        plot_df$trend <- predict(loess_fit)
        p <- p + geom_line(data = plot_df, aes(x = period, y = trend),
                           color = COL_TREND, linewidth = 1, inherit.aes = FALSE)
      }
    }
    
    return(p)
  }
}


# ============================================================
# DEEP DIVE PLOTS
# ============================================================
{
  plot_pace_by_km <- function(activity_id, avg_pace = NULL) {
    {
      splits <- extract_detail_splits(activity_id)
      if (is.null(splits) || nrow(splits) == 0) {
        return(ggplot() +
                 labs(title = "No split data available") +
                 theme_fantasmagoria())
      }
    }
    
    {
      p <- splits %>%
        filter(!is.na(split_pace), split_pace > 0, split_pace < 20) %>%
        ggplot(aes(x = km, y = split_pace,
                   text = paste0("Km: ", km,
                                 "<br>Pace: ", sapply(split_pace, pace_dec_to_str)))) +
        geom_line(color  = "#00d4ff", linewidth = 1) +
        geom_point(color = "#00d4ff", size = 2.5) +
        xlab("Km") +
        ylab("Pace (min/km)") +
        theme_fantasmagoria()
      
      if (!is.null(avg_pace)) {
        p <- p + geom_hline(yintercept = avg_pace, color = "#ff6b6b",
                            linetype = "dashed", linewidth = 0.8)
      }
    }
    
    return(p)
  }
  
  plot_pace_continuous <- function(activity_id, avg_pace = NULL) {
    # Data preparation
    {
      ts <- extract_detail_timeseries(activity_id)
      if (is.null(ts)) {
        return(plot_ly() %>%
                 layout(title = "No time-series data available") %>%
                 apply_dark_theme())
      }
      
      pace_ts <- ts %>% filter(variable == "pace") %>%
        arrange(start_epoch_ms) %>%
        select(start_epoch_ms, pace = value)
      
      dist_ts <- ts %>% filter(variable == "distance") %>%
        arrange(start_epoch_ms) %>%
        mutate(cum_distance = cumsum(value)) %>%
        select(start_epoch_ms, cum_distance)
      
      if (nrow(pace_ts) == 0 || nrow(dist_ts) == 0) {
        return(plot_ly() %>%
                 layout(title = "No continuous pace data available") %>%
                 apply_dark_theme())
      }
      
      # Interpolate cumulative distance at each pace timestamp
      dist_lookup <- approxfun(
        x      = dist_ts$start_epoch_ms,
        y      = dist_ts$cum_distance,
        method = "linear",
        rule   = 2
      )
      
      combined <- pace_ts %>%
        mutate(cum_distance = dist_lookup(start_epoch_ms)) %>%
        filter(!is.na(cum_distance), cum_distance >= 0,
               !is.na(pace), pace > 0, pace < 20) %>%
        arrange(cum_distance)
      
      # Remove outliers: keep within 3 SD
      pace_mean_val <- mean(combined$pace, na.rm = TRUE)
      pace_sd_val   <- sd(combined$pace,   na.rm = TRUE)
      combined <- combined %>%
        filter(pace >= pace_mean_val - 3 * pace_sd_val,
               pace <= pace_mean_val + 3 * pace_sd_val)
    }
    
    # Build plotly directly (avoids ggplotly converting geom_line to scatter)
    {
      if (nrow(combined) == 0) {
        return(plot_ly() %>%
                 layout(title = "No continuous pace data available") %>%
                 apply_dark_theme())
      }
      
      # Downsample to ~500 points max for performance
      if (nrow(combined) > 500) {
        idx <- round(seq(1, nrow(combined), length.out = 500))
        combined <- combined[idx, ]
      }
      
      combined$pace_str <- sapply(combined$pace, pace_dec_to_str)
      
      p <- plot_ly(combined, x = ~cum_distance, y = ~pace,
                   type = "scatter", mode = "lines",
                   line = list(color = COL_ACCENT, width = 2),
                   hoverinfo = "text",
                   text = ~paste0("Km: ", round(cum_distance, 2),
                                  "<br>Pace: ", pace_str)) %>%
        layout(xaxis = list(title = "Distance (km)"),
               yaxis = list(title = "Pace (min/km)"))
      
      if (!is.null(avg_pace)) {
        p <- p %>% add_trace(
          x = range(combined$cum_distance), y = c(avg_pace, avg_pace),
          type = "scatter", mode = "lines",
          line = list(color = COL_TREND, width = 2, dash = "dash"),
          hoverinfo = "text",
          text = paste0("Avg: ", pace_dec_to_str(avg_pace)),
          showlegend = FALSE
        )
      }
    }
    
    return(p)
  }
  
  plot_elevation <- function(activity_id) {
    {
      ts <- extract_detail_timeseries(activity_id)
      if (is.null(ts)) {
        return(ggplot() +
                 labs(title = "No time-series data available") +
                 theme_fantasmagoria())
      }
      
      elev <- ts %>% filter(variable == "elevation", value > 1) %>%
        arrange(start_epoch_ms) %>%
        select(start_epoch_ms, elevation = value)
      
      dist <- ts %>% filter(variable == "distance") %>%
        arrange(start_epoch_ms) %>%
        mutate(cum_distance = cumsum(value)) %>%
        select(start_epoch_ms, cum_distance)
      
      if (nrow(elev) == 0 || nrow(dist) == 0) {
        return(ggplot() +
                 labs(title = "No elevation data available") +
                 theme_fantasmagoria())
      }
      
      # Interpolate cum_distance at each elevation timestamp
      dist_lookup <- approxfun(
        x      = dist$start_epoch_ms,
        y      = dist$cum_distance,
        method = "linear",
        rule   = 2
      )
      
      combined <- elev %>%
        mutate(cum_distance = dist_lookup(start_epoch_ms)) %>%
        filter(!is.na(cum_distance), cum_distance >= 0) %>%
        arrange(cum_distance)
      
      # Remove outliers: keep within 3 SD of mean elevation
      elev_mean <- mean(combined$elevation, na.rm = TRUE)
      elev_sd   <- sd(combined$elevation,   na.rm = TRUE)
      combined  <- combined %>%
        filter(elevation >= elev_mean - 3 * elev_sd,
               elevation <= elev_mean + 3 * elev_sd)
    }
    
    {
      if (nrow(combined) == 0) {
        return(ggplot() +
                 labs(title = "No elevation data available") +
                 theme_fantasmagoria())
      }
      
      # Downsample to ~500 points max to avoid C stack overflow in ggplotly
      if (nrow(combined) > 500) {
        idx <- round(seq(1, nrow(combined), length.out = 500))
        combined <- combined[idx, ]
      }
      
      p <- ggplot(combined, aes(x = cum_distance, y = elevation)) +
        geom_line(color = "orange", linewidth = 0.8) +
        xlab("Distance (km)") +
        ylab("Elevation (m)") +
        theme_fantasmagoria()
    }
    
    return(p)
  }
  
  get_gps_data <- function(activity_id) {
    # Extract lat, lon, pace, and cumulative distance for enriched hover
    {
      ts <- extract_detail_timeseries(activity_id)
      if (is.null(ts)) return(NULL)
      
      lat <- ts %>% filter(variable == "latitude")  %>%
        select(start_epoch_ms, value) %>% rename(latitude  = value)
      lon <- ts %>% filter(variable == "longitude") %>%
        select(start_epoch_ms, value) %>% rename(longitude = value)
      
      if (nrow(lat) == 0 || nrow(lon) == 0) return(NULL)
      
      gps <- inner_join(lat, lon, by = "start_epoch_ms") %>%
        mutate(
          start_time = as.POSIXct(start_epoch_ms / 1000, origin = "1970-01-01", tz = "UTC") %>%
            with_tz(TZ),
          minute = as.numeric(difftime(start_time, min(start_time), units = "mins"))
        )
    }
    
    # Interpolate pace and cumulative distance at each GPS timestamp
    {
      pace_ts <- ts %>% filter(variable == "pace") %>%
        arrange(start_epoch_ms) %>%
        select(start_epoch_ms, value)
      
      dist_ts <- ts %>% filter(variable == "distance") %>%
        arrange(start_epoch_ms) %>%
        mutate(cum_distance = cumsum(value)) %>%
        select(start_epoch_ms, cum_distance)
      
      if (nrow(pace_ts) > 1) {
        pace_fn <- approxfun(pace_ts$start_epoch_ms, pace_ts$value,
                             method = "linear", rule = 2)
        gps$pace <- pace_fn(gps$start_epoch_ms)
        gps$pace_str <- sapply(gps$pace, function(x) {
          if (is.na(x) || x <= 0 || x > 20) return("--")
          pace_dec_to_str(x)
        })
      } else {
        gps$pace     <- NA_real_
        gps$pace_str <- "--"
      }
      
      if (nrow(dist_ts) > 1) {
        dist_fn <- approxfun(dist_ts$start_epoch_ms, dist_ts$cum_distance,
                             method = "linear", rule = 2)
        gps$cum_distance <- dist_fn(gps$start_epoch_ms)
      } else {
        gps$cum_distance <- NA_real_
      }
    }
    
    return(gps)
  }
}


# ============================================================
# NIKE API (for Update Data)
# ============================================================
{
  # HTTP helper — makes authorized requests to Nike API
  nike_api_get <- function(url, token) {
    {
      resp <- request(url) |>
        req_headers(Authorization = paste("Bearer", token)) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform()
      
      if (resp_status(resp) != 200) {
        stop(paste("Nike API error:", resp_status(resp),
                   "— grab a fresh token and try again."))
      }
      
      resp |> resp_body_json(simplifyVector = TRUE)
    }
  }
  
  # Flatten nested metric summaries to wide format
  flatten_activity_metrics <- function(df) {
    {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble())
      if (!all(c("metric", "summary", "value") %in% names(df))) return(tibble())
      
      df |>
        mutate(col = paste0(metric, "_", summary)) |>
        select(col, value) |>
        pivot_wider(names_from = col, values_from = value, values_fn = first)
    }
  }
  
  # Fetch new activities incrementally (only after last_known_id)
  fetch_new_activities <- function(token, output_dir, existing_ids = character(0),
                                   log_fn = message) {
    {
      dir.create(output_dir, showWarnings = FALSE)
      all_new <- data.frame()
      next_url <- paste0(
        "https://api.nike.com/plus/v3/activities/before_id/v3/*",
        "?limit=30&types=run,jogging&include_deleted=false"
      )
      page <- 1
    }
    
    # Paginate until we hit already-known activities
    {
      repeat {
        log_fn(paste0("Fetching page ", page, "..."))
        data <- nike_api_get(next_url, token)
        
        activities <- data$activities
        if (is.null(activities) || nrow(activities) == 0) break
        
        # Check which are genuinely new
        new_acts <- activities %>% filter(!id %in% existing_ids)
        if (nrow(new_acts) == 0) break
        
        all_new <- bind_rows(all_new, new_acts)
        
        # If some were already known, we've reached the overlap
        if (nrow(new_acts) < nrow(activities)) break
        
        before_id <- data$paging$before_id
        if (is.null(before_id) || before_id == "") break
        
        next_url <- paste0(
          "https://api.nike.com/plus/v3/activities/before_id/v3/", before_id,
          "?limit=30&types=run,jogging&include_deleted=false"
        )
        page <- page + 1
        Sys.sleep(0.3)
      }
    }
    
    # Flatten and save
    {
      if (nrow(all_new) == 0) {
        log_fn("No new activities found.")
        return(invisible(NULL))
      }
      
      log_fn(paste0("Found ", nrow(all_new), " new activities. Flattening metrics..."))
      metrics_df <- bind_rows(lapply(all_new$summaries, flatten_activity_metrics))
      
      new_summary <- all_new |>
        select(id, type, start_epoch_ms, end_epoch_ms, active_duration_ms, status) |>
        bind_cols(metrics_df) |>
        mutate(
          start_time   = as.POSIXct(start_epoch_ms / 1000, origin = "1970-01-01", tz = "UTC"),
          end_time     = as.POSIXct(end_epoch_ms   / 1000, origin = "1970-01-01", tz = "UTC"),
          duration_min = active_duration_ms / 60000
        )
      
      # Append to existing CSV
      csv_path <- file.path(output_dir, "activity_summaries.csv")
      if (file.exists(csv_path)) {
        existing <- fread(csv_path) %>% as.data.frame()
        combined <- bind_rows(existing, new_summary)
      } else {
        combined <- new_summary
      }
      write.csv(combined, csv_path, row.names = FALSE)
      log_fn(paste0("Updated CSV: ", csv_path, " (", nrow(combined), " total)"))
    }
    
    # Download detail JSONs for new activities
    {
      detail_dir <- file.path(output_dir, "activity_details")
      dir.create(detail_dir, showWarnings = FALSE)
      
      for (i in seq_along(all_new$id)) {
        aid      <- all_new$id[i]
        out_file <- file.path(detail_dir, paste0(aid, ".json"))
        if (file.exists(out_file)) next
        
        log_fn(paste0("  Downloading detail [", i, "/", length(all_new$id), "] ", aid))
        tryCatch({
          detail_url <- paste0("https://api.nike.com/sport/v3/me/activity/", aid, "?metrics=ALL")
          detail     <- nike_api_get(detail_url, token)
          write_json(detail, out_file, pretty = TRUE, auto_unbox = TRUE)
        }, error = function(e) {
          log_fn(paste0("    WARNING: failed for ", aid, ": ", e$message))
        })
        Sys.sleep(0.3)
      }
    }
    
    log_fn(paste0("Done! ", nrow(all_new), " new activities added."))
    return(invisible(nrow(all_new)))
  }
}


# ============================================================
# SHOES ANALYSIS
# ============================================================
{
  # Load shoe_id for all activities from detail JSONs
  load_shoe_data <- function() {
    {
      json_files <- list.files(DETAIL_DIR, pattern = "\\.json$", full.names = TRUE)
      if (length(json_files) == 0) return(data.frame(id = character(0), shoe_id = character(0)))
    }
    
    {
      shoe_list <- lapply(json_files, function(f) {
        tryCatch({
          result <- fromJSON(f, simplifyVector = TRUE)
          tags   <- result$tags
          data.frame(
            id      = result$id %||% NA_character_,
            shoe_id = tags[["shoe_id"]] %||% NA_character_,
            stringsAsFactors = FALSE
          )
        }, error = function(e) NULL)
      })
      shoe_df <- bind_rows(shoe_list)
    }
    
    return(shoe_df)
  }
  
  # Compute shoe summary statistics
  compute_shoe_stats <- function(summaries_df, shoe_df) {
    {
      df <- summaries_df %>%
        left_join(shoe_df, by = "id") %>%
        mutate(shoe_id = ifelse(is.na(shoe_id) | shoe_id == "", "Unassigned", shoe_id))
    }
    
    {
      stats <- df %>%
        group_by(shoe_id) %>%
        summarise(
          n_races        = n(),
          total_km       = round(sum(distance_total, na.rm = TRUE), 1),
          avg_pace       = round(mean(pace_mean, na.rm = TRUE), 2),
          best_pace      = round(min(pace_mean, na.rm = TRUE), 2),
          worst_pace     = round(max(pace_mean, na.rm = TRUE), 2),
          avg_distance   = round(mean(distance_total, na.rm = TRUE), 1),
          min_distance   = round(min(distance_total, na.rm = TRUE), 1),
          max_distance   = round(max(distance_total, na.rm = TRUE), 1),
          first_used     = min(start_time, na.rm = TRUE),
          last_used      = max(start_time, na.rm = TRUE),
          .groups        = "drop"
        ) %>%
        arrange(desc(total_km))
    }
    
    # Add percentiles vs all runs
    {
      all_dist_ecdf <- ecdf(summaries_df$distance_total)
      all_pace_ecdf <- ecdf(summaries_df$pace_mean)
      
      stats <- stats %>%
        mutate(
          dist_pct = round(100 * all_dist_ecdf(avg_distance), 0),
          pace_pct = round(100 * (1 - all_pace_ecdf(avg_pace)), 0)
        )
    }
    
    return(stats)
  }
}
