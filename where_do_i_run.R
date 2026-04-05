# ============================================================
# Fantasmagoria — Where Do I Run?
# GPS Hexagon heatmap + two-pass zone detection
# ============================================================
# Dependencies:
#   install.packages(c("h3jsr", "sf", "dbscan", "leaflet",
#                      "leaflet.extras", "dplyr", "purrr",
#                      "jsonlite", "lubridate", "htmltools"))
#
# Steps:
#   1. Sample N detail JSONs, extract lat/lng incrementally
#   2. Aggregate GPS points into H3 hexagons (h3jsr >= 1.3.1)
#      Two hex tables:
#        hex_stats_all  — all hexes, used for background map layer
#        hex_stats      — filtered (MIN_HEX_RACES), used for clustering
#   3. Zone detection:
#        Pass 1 — DBSCAN with low MINPTS to catch other cities
#        Pass 2 — k-means on large zones to split into neighbourhoods
#   4. Render Leaflet map:
#        Layer 1a — faint background: all hexes (shows other cities)
#        Layer 1b — bright foreground: filtered hexes (main routes)
#        Layer 2  — zone convex hull polygons
#        Layer 3  — permanent zone name labels
#   5. Save outputs + print manual tagging snippet
# ============================================================


# ============================================================
# LIBRARIES & CONFIG
# ============================================================
{
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(jsonlite)
  library(lubridate)
  library(h3jsr)
  library(sf)
  library(dbscan)
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  
  source("fantasmagoria_functions.R")
  
  # ---- Config -----------------------------------------------
  N_SAMPLE   <- NULL   # NULL = all races; set to e.g. 50 for prototype
  
  H3_RES     <- 9      # hex width ~150m
  
  # Pass 1 — DBSCAN: city/region separation
  # Low MINPTS so small other-city clusters are not dropped as noise
  DBSCAN_EPS    <- 0.012
  DBSCAN_MINPTS <- 2
  
  # Pass 2 — k-means: split large zones into neighbourhoods
  KMEANS_K            <- 8    # sub-zones per large zone
  SPLIT_HEX_THRESHOLD <- 50   # zones above this get split
  
  # Hex filter for clustering only (not for display)
  # hex_stats_all (all hexes) is kept separately for the background layer
  MIN_HEX_RACES <- 2
  
  OUT_DIR    <- BASE_DIR
  DARK_TILE  <- "CartoDB.DarkMatter"
  PACE_MIN   <- 4.0
  PACE_MAX   <- 7.5
}


# ============================================================
# STEP 1: SAMPLE RACES & EXTRACT GPS INCREMENTALLY
# ============================================================
{
  cat("=======================================================\n")
  cat("STEP 1: Sampling races and extracting GPS\n")
  cat("=======================================================\n\n")
  
  # Load summaries
  {
    summaries <- load_summaries() %>%
      add_scores() %>%
      select(id, start_time, distance_total, pace_mean, ascent_total)
    
    cat(sprintf("Total races available: %d\n", nrow(summaries)))
  }
  
  # Sample — stratified by year if N_SAMPLE set, else use all
  {
    set.seed(42)
    
    if (is.null(N_SAMPLE)) {
      sampled_ids <- summaries$id
      cat(sprintf("Using all %d races\n\n", length(sampled_ids)))
    } else {
      n_years  <- n_distinct(year(summaries$start_time))
      per_year <- ceiling(N_SAMPLE / n_years)
      
      sampled_ids <- summaries %>%
        mutate(year = year(start_time)) %>%
        group_by(year) %>%
        slice_sample(n = per_year, replace = FALSE) %>%
        ungroup() %>%
        slice_sample(n = N_SAMPLE) %>%
        pull(id)
      
      cat(sprintf("Sampled %d races across %d years\n\n",
                  length(sampled_ids), n_years))
    }
  }
  
  # GPS extraction helper
  {
    extract_gps_minimal <- function(activity_id, summaries_df) {
      pace <- summaries_df$pace_mean[summaries_df$id == activity_id]
      dist <- summaries_df$distance_total[summaries_df$id == activity_id]
      
      ts <- tryCatch(
        extract_detail_timeseries(activity_id),
        error = function(e) NULL
      )
      if (is.null(ts)) return(NULL)
      
      lat_ts <- ts %>% filter(variable == "latitude")  %>%
        select(start_epoch_ms, lat = value)
      lon_ts <- ts %>% filter(variable == "longitude") %>%
        select(start_epoch_ms, lon = value)
      
      if (nrow(lat_ts) == 0 || nrow(lon_ts) == 0) return(NULL)
      
      inner_join(lat_ts, lon_ts, by = "start_epoch_ms") %>%
        slice(seq(1, n(), by = 5)) %>%
        mutate(id = activity_id, pace = pace, dist = dist) %>%
        select(id, lat, lon, pace, dist)
    }
  }
  
  # Iterate one file at a time
  {
    cat("Extracting GPS points (one race at a time)...\n")
    
    gps_list <- vector("list", length(sampled_ids))
    
    for (i in seq_along(sampled_ids)) {
      aid <- sampled_ids[i]
      cat(sprintf("  [%d/%d] %s\n", i, length(sampled_ids), aid))
      gps_list[[i]] <- extract_gps_minimal(aid, summaries)
    }
    
    gps_all <- bind_rows(gps_list) %>%
      filter(!is.na(lat), !is.na(lon))
    
    cat(sprintf("\nTotal GPS points extracted: %s\n\n",
                format(nrow(gps_all), big.mark = ",")))
  }
}


# ============================================================
# STEP 2: HEX AGGREGATION
# ============================================================
# Two tables:
#   hex_stats_all — every hex, used for background display layer
#   hex_stats     — filtered by MIN_HEX_RACES, used for clustering
#   hex_polys_all — sf polygons for all hexes (background layer)
#   hex_polys_sf  — sf polygons for filtered hexes (main layer)
# ============================================================
{
  cat("=======================================================\n")
  cat("STEP 2: Hex aggregation (H3 resolution", H3_RES, ")\n")
  cat("=======================================================\n\n")
  
  # Assign hex IDs
  {
    gps_sf  <- st_as_sf(gps_all, coords = c("lon", "lat"), crs = 4326)
    gps_all <- gps_all %>%
      mutate(hex_id = point_to_cell(input = gps_sf, res = H3_RES))
  }
  
  # Aggregate per hex — full table first
  {
    hex_stats_all <- gps_all %>%
      group_by(hex_id) %>%
      summarise(
        n_points = n(),
        n_races  = n_distinct(id),
        avg_pace = mean(pace, na.rm = TRUE),
        med_pace = median(pace, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        centroid = cell_to_point(hex_id),
        lon      = st_coordinates(centroid)[, 1],
        lat      = st_coordinates(centroid)[, 2]
      ) %>%
      select(-centroid) %>%
      arrange(desc(n_points))
    
    # Filtered table for clustering
    hex_stats <- hex_stats_all %>%
      filter(n_races >= MIN_HEX_RACES)
    
    cat(sprintf("All hexes:              %d\n", nrow(hex_stats_all)))
    cat(sprintf("Filtered (n_races>=%d): %d\n", MIN_HEX_RACES, nrow(hex_stats)))
    cat(sprintf("Most visited hex: %d points, %d races\n\n",
                max(hex_stats_all$n_points), max(hex_stats_all$n_races)))
  }
  
  # Build sf polygon layers
  {
    # Background layer — all hexes
    hex_polys_all <- cell_to_polygon(hex_stats_all$hex_id, simple = FALSE) %>%
      rename(hex_id = h3_address) %>%
      left_join(hex_stats_all, by = "hex_id")
    
    # Main layer — filtered hexes
    hex_polys_sf <- cell_to_polygon(hex_stats$hex_id, simple = FALSE) %>%
      rename(hex_id = h3_address) %>%
      left_join(hex_stats, by = "hex_id")
    
    cat(sprintf("Background hex layer (all):      %d polygons\n",
                nrow(hex_polys_all)))
    cat(sprintf("Main hex layer (filtered):       %d polygons\n\n",
                nrow(hex_polys_sf)))
  }
}


# ============================================================
# STEP 3: ZONE DETECTION
# ============================================================
# Pass 1 — DBSCAN on hex_stats_all (not filtered) with low
#   MINPTS=2 so isolated other-city clusters form zones
#   Noise (zone_id=0) = truly isolated single hexes
#
# Pass 2 — k-means splits any zone above SPLIT_HEX_THRESHOLD
#   (the large CDMX zone) into neighbourhoods
# ============================================================
{
  cat("=======================================================\n")
  cat("STEP 3: Zone detection (DBSCAN + k-means split)\n")
  cat("=======================================================\n\n")
  
  # ---- Pass 1: DBSCAN on ALL hexes -------------------------
  # Using hex_stats_all so single-race city runs are included
  {
    coords_mat <- hex_stats_all %>%
      select(lon, lat) %>%
      as.matrix()
    
    db1 <- dbscan::dbscan(coords_mat,
                          eps    = DBSCAN_EPS,
                          minPts = DBSCAN_MINPTS)
    
    hex_stats_all$zone_id <- db1$cluster
    # Propagate zone_id to filtered hex_stats via hex_id join
    hex_stats <- hex_stats %>%
      left_join(hex_stats_all %>% select(hex_id, zone_id), by = "hex_id")
    
    n_pass1 <- sum(unique(hex_stats_all$zone_id) > 0)
    cat(sprintf("Pass 1 (DBSCAN, MINPTS=%d): %d zones\n", DBSCAN_MINPTS, n_pass1))
    
    pass1_summary <- hex_stats_all %>%
      filter(zone_id > 0) %>%
      group_by(zone_id) %>%
      summarise(
        n_hexes  = n(),
        n_points = sum(n_points),
        n_races  = max(n_races),
        .groups  = "drop"
      ) %>%
      arrange(desc(n_hexes))
    
    cat("Pass 1 zone sizes:\n")
    print(pass1_summary)
    cat("\n")
  }
  
  # ---- Pass 2: k-means split on large zones ----------------
  {
    large_zones <- pass1_summary %>%
      filter(n_hexes >= SPLIT_HEX_THRESHOLD) %>%
      pull(zone_id)
    
    cat(sprintf(
      "Zones to split (>= %d hexes): %s\n\n",
      SPLIT_HEX_THRESHOLD,
      ifelse(length(large_zones) == 0, "none",
             paste(large_zones, collapse = ", "))
    ))
    
    for (z in large_zones) {
      # Split on hex_stats_all so all city hexes inform the k-means
      zone_idx     <- which(hex_stats_all$zone_id == z)
      coords_zone  <- hex_stats_all[zone_idx, ] %>%
        select(lon, lat) %>%
        as.matrix()
      distinct_pts <- nrow(unique(coords_zone))
      k_actual     <- min(KMEANS_K, distinct_pts)
      
      cat(sprintf("  Zone %d (%d hexes) -> %d sub-zones\n",
                  z, length(zone_idx), k_actual))
      
      set.seed(42)
      km <- kmeans(
        jitter(coords_zone, amount = 1e-6),
        centers  = k_actual,
        nstart   = 25,
        iter.max = 100
      )
      
      next_id <- max(hex_stats_all$zone_id) + 1
      hex_stats_all$zone_id[zone_idx] <- km$cluster + next_id - 1
    }
    
    # Re-propagate updated zone_ids to filtered hex_stats
    hex_stats <- hex_stats %>%
      select(-zone_id) %>%
      left_join(hex_stats_all %>% select(hex_id, zone_id), by = "hex_id")
    
    n_final <- sum(unique(hex_stats_all$zone_id) > 0)
    cat(sprintf("\nTotal zones after k-means: %d\n\n", n_final))
    
    final_summary <- hex_stats_all %>%
      filter(zone_id > 0) %>%
      group_by(zone_id) %>%
      summarise(
        n_hexes  = n(),
        n_points = sum(n_points),
        n_races  = max(n_races),
        avg_pace = round(mean(avg_pace, na.rm = TRUE), 2),
        .groups  = "drop"
      ) %>%
      arrange(desc(n_points))
    
    cat("Final zone summary:\n")
    print(final_summary)
    cat("\n")
  }
  
  # ---- Build convex hull per final zone ---------------------
  # Hulls built from hex_stats_all so other-city zones
  # get accurate polygons even if they have only 1 race
  {
    build_hull <- function(zone_df, zone_id_val) {
      pts <- st_as_sf(zone_df, coords = c("lon", "lat"), crs = 4326)
      if (nrow(pts) < 3) {
        # For very small zones (1-2 hexes) use a buffered point
        pts_union <- st_union(pts)
        hull      <- st_buffer(pts_union, dist = 0.005)
      } else {
        hull <- st_convex_hull(st_union(pts))
      }
      
      stats <- zone_df %>%
        summarise(
          n_hexes  = n(),
          n_points = sum(n_points),
          n_races  = max(n_races),
          avg_pace = round(mean(avg_pace, na.rm = TRUE), 2),
          .groups  = "drop"
        )
      
      tibble(
        zone_id  = zone_id_val,
        name     = paste0("Zone ", zone_id_val),
        n_hexes  = stats$n_hexes,
        n_points = stats$n_points,
        n_races  = stats$n_races,
        avg_pace = stats$avg_pace,
        geometry = hull
      )
    }
    
    zone_polygons <- hex_stats_all %>%
      filter(zone_id > 0) %>%
      group_by(zone_id) %>%
      group_map(function(df, key) build_hull(df, key$zone_id)) %>%
      bind_rows() %>%
      st_as_sf(crs = 4326)
    
    cat(sprintf("Built %d zone polygons\n\n", nrow(zone_polygons)))
    cat("Zone polygons (tag names in STEP 5):\n")
    print(zone_polygons %>% st_drop_geometry() %>%
            select(zone_id, name, n_hexes, n_points, n_races, avg_pace))
    cat("\n")
  }
}


# ============================================================
# STEP 4: LEAFLET MAP
# ============================================================
# Layer 1a — background: ALL hexes, faint, shows other cities
# Layer 1b — foreground: filtered hexes, bright, main routes
# Layer 2  — zone convex hull polygons (dashed outline)
# Layer 3  — permanent zone name labels
# ============================================================
{
  cat("=======================================================\n")
  cat("STEP 4: Building Leaflet map\n")
  cat("=======================================================\n\n")
  
  zone_polygons <- readRDS('nike_data/zone_polygons.rds')
  # Color palettes
  {
    # Background layer uses same palette but lower opacity
    pal_all <- colorNumeric(
      palette  = c("#1a1a2e", "#0f3460", "#00d4ff"),
      domain   = log1p(hex_polys_all$n_points),
      na.color = "transparent"
    )
    
    pal_visits <- colorNumeric(
      palette  = c("#1a1a2e", "#16213e", "#0f3460", "#00d4ff", "#ffffff"),
      domain   = log1p(hex_polys_sf$n_points),
      na.color = "transparent"
    )
    
    n_zones_actual <- nrow(zone_polygons)
    base_colors    <- c("#ff6b6b", "#ffaa00", "#00cc44", "#00d4ff",
                        "#aa44ff", "#ff44aa", "#44ffaa", "#ffff44",
                        "#ff8800", "#00ffcc", "#ff0088", "#88ff00",
                        "#0088ff", "#ffcc00", "#cc00ff", "#00ff88")
    zone_colors <- colorFactor(
      palette = rep(base_colors, length.out = n_zones_actual),
      domain  = zone_polygons$zone_id
    )
  }
  
  # Hover labels
  {
    hex_all_labels <- sprintf(
      "<b>Visits:</b> %d points, %d races<br><b>Avg pace:</b> %s min/km",
      hex_polys_all$n_points,
      hex_polys_all$n_races,
      sapply(hex_polys_all$avg_pace, pace_dec_to_str)
    ) %>% lapply(HTML)
    
    hex_labels <- sprintf(
      "<b>Visits:</b> %d points, %d races<br><b>Avg pace:</b> %s min/km",
      hex_polys_sf$n_points,
      hex_polys_sf$n_races,
      sapply(hex_polys_sf$avg_pace, pace_dec_to_str)
    ) %>% lapply(HTML)
    
    zone_labels <- sprintf(
      "<b>%s</b><br>%d hexes | %d races<br>Avg pace: %s min/km",
      zone_polygons$name,
      zone_polygons$n_hexes,
      zone_polygons$n_races,
      sapply(zone_polygons$avg_pace, pace_dec_to_str)
    ) %>% lapply(HTML)
  }
  
  # Zone centroids for label placement
  {
    zone_centroids <- zone_polygons %>%
      mutate(
        centroid = st_centroid(geometry),
        lon      = st_coordinates(centroid)[, 1],
        lat      = st_coordinates(centroid)[, 2]
      ) %>%
      st_drop_geometry()
  }
  
  # Build map
  {
    map <- leaflet() %>%
      addProviderTiles(providers[[DARK_TILE]]) %>%
      
      # Layer 1a: background — all hexes, faint
      # Shows other-city runs even if below MIN_HEX_RACES filter
      addPolygons(
        data        = hex_polys_all,
        fillColor   = ~pal_all(log1p(n_points)),
        fillOpacity = 0.3,
        color       = "transparent",
        weight      = 0,
        label       = hex_all_labels,
        group       = "All hexes (background)"
      ) %>%
      
      # Layer 1b: foreground — filtered hexes, full brightness
      addPolygons(
        data        = hex_polys_sf,
        fillColor   = ~pal_visits(log1p(n_points)),
        fillOpacity = 0.75,
        color       = "transparent",
        weight      = 0,
        label       = hex_labels,
        group       = "Main hexes"
      ) %>%
      
      # Layer 2: zone convex hull outlines
      addPolygons(
        data        = zone_polygons,
        fillColor   = ~zone_colors(zone_id),
        fillOpacity = 0.12,
        color       = ~zone_colors(zone_id),
        weight      = 2,
        opacity     = 0.8,
        dashArray   = "5,5",
        label       = zone_labels,
        group       = "Zones"
      ) %>%
      
      # Layer 3: permanent zone labels
      # addCircleMarkers(radius=0) avoids .b64EncodeFile bug
      addCircleMarkers(
        data         = zone_centroids,
        lng          = ~lon,
        lat          = ~lat,
        radius       = 0,
        stroke       = FALSE,
        fillOpacity  = 0,
        label        = ~name,
        labelOptions = labelOptions(
          noHide    = TRUE,
          direction = "center",
          textOnly  = TRUE,
          style     = list(
            "color"       = "white",
            "font-weight" = "bold",
            "font-size"   = "13px",
            "text-shadow" = "1px 1px 3px black"
          )
        ),
        group = "Zone labels"
      ) %>%
      
      addLegend(
        position  = "bottomright",
        pal       = pal_visits,
        values    = log1p(hex_polys_sf$n_points),
        title     = "Visit intensity (log)",
        opacity   = 0.8,
        labFormat = labelFormat(transform = function(x) round(expm1(x)))
      ) %>%
      
      addLayersControl(
        overlayGroups = c("All hexes (background)", "Main hexes",
                          "Zones", "Zone labels"),
        options       = layersControlOptions(collapsed = FALSE)
      )
    
    print(map)
    cat("Map rendered.\n\n")
  }
}


# ============================================================
# STEP 5: SAVE OUTPUTS
# ============================================================
{
  cat("=======================================================\n")
  cat("STEP 5: Saving outputs\n")
  cat("=======================================================\n\n")
  
  saveRDS(hex_stats,     file.path(OUT_DIR, "hex_stats.rds"))
  saveRDS(hex_stats_all, file.path(OUT_DIR, "hex_stats_all.rds"))
  saveRDS(hex_polys_sf,  file.path(OUT_DIR, "hex_polys_sf.rds"))
  saveRDS(hex_polys_all, file.path(OUT_DIR, "hex_polys_all.rds"))
  saveRDS(zone_polygons, file.path(OUT_DIR, "zone_polygons.rds"))
  
  cat(sprintf("  hex_stats.rds     -> %d hexes (filtered)\n",  nrow(hex_stats)))
  cat(sprintf("  hex_stats_all.rds -> %d hexes (all)\n",       nrow(hex_stats_all)))
  cat(sprintf("  hex_polys_sf.rds  -> %d polygons\n",          nrow(hex_polys_sf)))
  cat(sprintf("  hex_polys_all.rds -> %d polygons\n",          nrow(hex_polys_all)))
  cat(sprintf("  zone_polygons.rds -> %d zones\n",             nrow(zone_polygons)))
  
  cat("\n--- TAG YOUR ZONES ---\n")
  cat("Run after inspecting the map:\n\n")
  cat("  zones <- readRDS('nike_data/zone_polygons.rds')\n")
  cat("  zones$name <- c(\n")
  # for (i in seq_len(nrow(zone_polygons))) {
  #   cat(sprintf("    'Place name here'%s   # zone %d | %d races | avg %s min/km\n",
  #               ifelse(i < nrow(zone_polygons), ",", ""),
  #               zone_polygons$zone_id[i],
  #               zone_polygons$n_races[i],
  #               pace_dec_to_str(zone_polygons$avg_pace[i])))
  # }
  cat("  )\n")
  cat("  saveRDS(zones, 'nike_data/zone_polygons.rds')\n\n")
  
  cat("=======================================================\n")
  cat("Tag zones, then wire load_where_map() into\n")
  cat("fantasmagoria_functions.R and add the tab to app.R\n")
  cat("=======================================================\n")
}

#Tagging
{
  zones <- readRDS('nike_data/zone_polygons.rds')
  
  # Check zone_id and stats to identify each one on the map
  print(zones %>% st_drop_geometry() %>% select(zone_id, name, n_races, avg_pace))
  
  zones$name <- c(
    'Satélite',       
    'Monterrey (Cumbres)',
    'Huatulco',         
    'Rio de Janeiro',     
    'Guaruja',
    'CDMX - Insurgentes Sur/Nápoles',
    'CDMX - Norte (San Rafael/Granada/Anáhuac)',
    'CDMX - Norte (Calzada de Guadalupe/Basílica)',
    'CDMX - Reforma/Roma/Condesa',
    'CDMX - Polanco/Lomas de Chapultepec',
    'CDMX - Chapultepec/Parque Lira',
    'CDMX - Ciudad Universitaria',
    'CDMX - Reforma/Centro/Tlatelolco'
  )
  
  saveRDS(zones, 'nike_data/zone_polygons.rds')
  cat("Done! Reload the map to see names.\n")
  
}


# ============================================================
# SHINY HELPERS
# Paste into fantasmagoria_functions.R when ready
# ============================================================

# load_where_map <- function() {
#   list(
#     hex_all = readRDS(file.path(BASE_DIR, "hex_polys_all.rds")),
#     hex     = readRDS(file.path(BASE_DIR, "hex_polys_sf.rds")),
#     zones   = readRDS(file.path(BASE_DIR, "zone_polygons.rds"))
#   )
# }
#
# render_where_map <- function(map_data, color_by = "visits") {
#   hex_all <- map_data$hex_all
#   hex     <- map_data$hex
#   zones   <- map_data$zones
#
#   pal_all <- colorNumeric(c("#1a1a2e", "#0f3460", "#00d4ff"),
#                           domain = log1p(hex_all$n_points))
#   pal <- if (color_by == "visits") {
#     colorNumeric(c("#1a1a2e", "#00d4ff", "#ffffff"),
#                  domain = log1p(hex$n_points))
#   } else {
#     colorNumeric(c("#00cc44", "#ffaa00", "#ff4444"),
#                  domain = c(PACE_MIN, PACE_MAX))
#   }
#   fill_vals <- if (color_by == "visits") log1p(hex$n_points) else hex$avg_pace
#
#   zone_centroids <- zones %>%
#     mutate(
#       lon = st_coordinates(st_centroid(geometry))[, 1],
#       lat = st_coordinates(st_centroid(geometry))[, 2]
#     ) %>%
#     st_drop_geometry()
#
#   leaflet() %>%
#     addProviderTiles(providers$CartoDB.DarkMatter) %>%
#     addPolygons(data = hex_all, fillColor = ~pal_all(log1p(n_points)),
#                 fillOpacity = 0.3, color = "transparent", weight = 0,
#                 group = "All hexes") %>%
#     addPolygons(data = hex, fillColor = ~pal(fill_vals),
#                 fillOpacity = 0.75, color = "transparent", weight = 0,
#                 group = "Main hexes") %>%
#     addPolygons(data = zones, fillOpacity = 0.12,
#                 color = "white", weight = 2, dashArray = "5,5",
#                 label = ~name, group = "Zones") %>%
#     addCircleMarkers(
#       data = zone_centroids, lng = ~lon, lat = ~lat,
#       radius = 0, stroke = FALSE, fillOpacity = 0,
#       label = ~name,
#       labelOptions = labelOptions(
#         noHide = TRUE, textOnly = TRUE,
#         style  = list("color" = "white", "font-weight" = "bold",
#                       "font-size" = "13px")
#       )
#     ) %>%
#     addLegend("bottomright", pal = pal, values = fill_vals,
#               title = ifelse(color_by == "visits", "Visits", "Avg pace")) %>%
#     addLayersControl(
#       overlayGroups = c("All hexes", "Main hexes", "Zones"),
#       options = layersControlOptions(collapsed = FALSE)
#     )
# }