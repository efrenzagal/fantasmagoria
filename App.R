# ============================================================
# Fantasmagoria 3.0 — Shiny App
# ============================================================
# File structure:
#   ~/Fantasmagoria 3.0/
#     app.R
#     fantasmagoria_functions.R
#     nike_data/
#       activity_summaries.csv
#       activity_details/
# ============================================================

# Libraries & source
{
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(leaflet)
  library(dplyr)
  library(lubridate)
  library(DT)
  
  source("fantasmagoria_functions.R")
}

# Load & prepare data helper
{
  reload_summaries <- function() {
    load_summaries() %>%
      add_scores() %>%
      add_dynamic_scores() %>%
      arrange(desc(start_time))
  }
  
  # Initial load (used for UI dropdowns that need data at build time)
  summaries_df_init <- reload_summaries()
}


# Helper: single stat row for deep dive summary card
dd_stat <- function(label, value, color = "#f0f0f0") {
  tags$div(
    style = "display:flex; justify-content:space-between; align-items:center;
             background:#1a1a1a; padding:6px 12px; border-radius:4px;",
    tags$span(style = "color:#aaa; font-size:11px; text-transform:uppercase;", label),
    tags$span(style = paste0("color:", color, "; font-weight:bold; font-size:14px;"), value)
  )
}

# ============================================================
# UI
# ============================================================
ui <- {
  dashboardPage(
    skin = "black",
    
    # ---- Header --------------------------------------------
    dashboardHeader(title = "Fantasmagoria 3.0"),
    
    # ---- Sidebar -------------------------------------------
    dashboardSidebar(
      sidebarMenu(
        menuItem("Trend Plot",  tabName = "trend",     icon = icon("chart-line")),
        menuItem("New Race",    tabName = "new_race",  icon = icon("person-running")),
        menuItem("Deep Dive",   tabName = "deep_dive", icon = icon("magnifying-glass")),
        menuItem("Shoes",       tabName = "shoes",      icon = icon("shoe-prints")),
        menuItem("Update Data", tabName = "update",    icon = icon("rotate"))
      )
    ),
    
    # ---- Body ----------------------------------------------
    dashboardBody(
      
      # Custom dark styling
      tags$head(tags$style(HTML("
        body, .content-wrapper, .main-sidebar, .sidebar {
          background-color: #1a1a1a !important;
          color: #f0f0f0 !important;
        }
        .box {
          background-color: #2b2b2b !important;
          border-top-color: #444 !important;
          color: #f0f0f0 !important;
        }
        .box-header {
          background-color: #2b2b2b !important;
          color: #f0f0f0 !important;
        }
        .box-title { color: #f0f0f0 !important; }
        .skin-black .main-header .logo {
          background-color: #111 !important;
          color: #f0f0f0 !important;
        }
        .skin-black .main-header .navbar { background-color: #111 !important; }
        .skin-black .main-sidebar         { background-color: #111 !important; }
        label, .control-label, h4, p      { color: #f0f0f0 !important; }
        .dataTables_wrapper, table.dataTable {
          color: #f0f0f0 !important;
          background-color: #2b2b2b !important;
        }
        table.dataTable thead th {
          background-color: #333 !important;
          color: #f0f0f0 !important;
        }
        .info-box { background-color: #2b2b2b !important; color: #f0f0f0 !important; }
        .info-box-number, .info-box-text  { color: #f0f0f0 !important; }
        .shiny-output-error               { color: #ff6b6b !important; }
      "))),
      
      tabItems(
        
        # ====================================================
        # TAB 1: TREND PLOT
        # ====================================================
        tabItem(tabName = "trend",
                fluidRow(
                  
                  # Controls
                  box(title = "Settings", width = 3, solidHeader = TRUE,
                      dateInput("trend_start",      "Start date:",         value = as.Date("2020-01-01")),
                      dateInput("trend_end",        "End date:",           value = Sys.Date()),
                      selectInput("trend_period",   "Group by:",
                                  choices  = c("Day" = "day", "Week" = "week", "Month" = "month", "Year" = "year"),
                                  selected = "month"),
                      selectInput("trend_agg",      "Aggregation:",
                                  choices  = c("Total" = "sum", "Average" = "mean", "Median" = "median"),
                                  selected = "mean"),
                      selectInput("trend_metric",   "Metric:",
                                  choices = c(
                                    "Distance (km)"  = "distance_total",
                                    "Pace (min/km)"  = "pace_mean",
                                    "Duration (min)" = "duration_min",
                                    "Ascent (m)"     = "ascent_total",
                                    "Cadence (spm)"  = "cadence_mean",
                                    "Calories"       = "calories_total",
                                    "Score"          = "score",
                                    "Dynamic Score"  = "dynamic_score"
                                  ),
                                  selected = "distance_total"),
                      numericInput("trend_min_races", "Min runs per period:", value = 1, min = 1),
                      checkboxInput("trend_smooth",   "Show trend line",      value = TRUE)
                  ),
                  
                  # Plot
                  box(title = "Trend", width = 9, solidHeader = TRUE,
                      plotlyOutput("trend_plot", height = "500px")
                  )
                )
        ),
        
        # ====================================================
        # TAB 2: NEW RACE
        # ====================================================
        tabItem(tabName = "new_race",
                
                # Row 1: inputs + score info boxes
                fluidRow(
                  box(title = "New race info", width = 3, solidHeader = TRUE,
                      numericInput("nr_distance", "Distance (km):", value = 10,   min = 0.1, step = 0.5),
                      textInput(   "nr_pace",     "Pace (M'SS):",   value = "5'00"),
                      numericInput("nr_ascent",   "Ascent (m):",    value = 50,   min = 0),
                      numericInput("nr_descent",  "Descent (m):",   value = 50,   min = 0),
                      actionButton("nr_go", "Calculate", icon = icon("calculator"),
                                   style = "color:#fff; background-color:#333; border-color:#555; width:100%")
                  ),
                  box(title = "Score", width = 9, solidHeader = TRUE,
                      fluidRow(
                        infoBoxOutput("nr_score_box",    width = 4),
                        infoBoxOutput("nr_pct_dist_box", width = 4),
                        infoBoxOutput("nr_pct_pace_box", width = 4)
                      )
                  )
                ),
                
                # Row 2: most similar race card (full width, styled)
                fluidRow(
                  box(title = "Most similar past race", width = 12, solidHeader = TRUE,
                      uiOutput("nr_similar_card")
                  )
                ),
                
                # Row 3: violin plots side by side
                fluidRow(
                  box(title = "vs Distance history", width = 6, solidHeader = TRUE,
                      plotlyOutput("nr_dist_plot", height = "280px")),
                  box(title = "vs Pace history",     width = 6, solidHeader = TRUE,
                      plotlyOutput("nr_pace_plot", height = "280px"))
                ),
                
                # Row 4: full width scatterplot
                fluidRow(
                  box(title = "Score scatterplot — your history", width = 12, solidHeader = TRUE,
                      plotlyOutput("nr_scatter_plot", height = "450px"))
                )
        ),
        
        
        # ====================================================
        # TAB 3: DEEP DIVE
        # ====================================================
        tabItem(tabName = "deep_dive",
                fluidRow(
                  
                  # Race selector
                  box(title = "Select race", width = 3, solidHeader = TRUE,
                      selectInput("dd_id", "Race:",
                                  choices  = setNames(
                                    summaries_df_init$id,
                                    paste(format(summaries_df_init$start_time, "%Y-%m-%d"),
                                          round(summaries_df_init$distance_total, 1), "km |",
                                          sapply(summaries_df_init$pace_mean, pace_dec_to_str), "min/km")
                                  ),
                                  selected = summaries_df_init$id[1]),
                      uiOutput("dd_summary_table")
                  ),
                  
                  # Plots
                  box(title = "Pace & Elevation", width = 9, solidHeader = TRUE,
                      radioButtons("dd_pace_view", NULL,
                                   choices  = c("Per km split" = "split", "Continuous" = "continuous"),
                                   selected = "split", inline = TRUE),
                      plotlyOutput("dd_pace_plot",      height = "250px"),
                      plotlyOutput("dd_elevation_plot", height = "250px")
                  )
                ),
                fluidRow(
                  box(title = "GPS Route", width = 12, solidHeader = TRUE,
                      leafletOutput("dd_map", height = "400px")
                  )
                )
        ),
        
        # ====================================================
        # TAB 5: SHOES ANALYSIS
        # ====================================================
        tabItem(tabName = "shoes",
                fluidRow(
                  box(title = "Shoe Dictionary", width = 12, solidHeader = TRUE,
                      p("Edit shoe names below. Shoe IDs from Nike are mapped to friendly names."),
                      uiOutput("shoes_dict_ui")
                  )
                ),
                fluidRow(
                  box(title = "Mileage by Shoe", width = 12, solidHeader = TRUE,
                      plotlyOutput("shoes_bar_plot", height = "300px")
                  )
                ),
                fluidRow(
                  box(title = "Shoe Scorecards", width = 12, solidHeader = TRUE,
                      uiOutput("shoes_scorecards")
                  )
                )
        ),
        
        # ====================================================
        # TAB 6: UPDATE DATA
        # ====================================================
        tabItem(tabName = "update",
                fluidRow(
                  box(title = "Token", width = 6, solidHeader = TRUE,
                      p("Get a fresh token from the Nike website:"),
                      tags$ol(
                        tags$li("Go to ", tags$a("nike.com/member/profile",
                                                 href = "https://www.nike.com/member/profile", target = "_blank")),
                        tags$li("Open DevTools → Network tab"),
                        tags$li("Check 'Guardar el registro' (Preserve log)"),
                        tags$li("Filter by: api.nike.com"),
                        tags$li("Click any 200 request → Headers → copy Authorization Bearer value")
                      ),
                      passwordInput("update_token", "Access token:", placeholder = "eyJhbGci..."),
                      actionButton("update_go", "Run export script", icon = icon("download"),
                                   style = "color:#fff; background-color:#333; border-color:#555;"),
                      br(), br(),
                      verbatimTextOutput("update_log")
                  ),
                  box(title = "Current data summary", width = 6, solidHeader = TRUE,
                      infoBoxOutput("update_total_runs",   width = 12),
                      infoBoxOutput("update_latest_run",   width = 12),
                      infoBoxOutput("update_earliest_run", width = 12)
                  )
                )
        )
        
      ) # end tabItems
    ) # end dashboardBody
  ) # end dashboardPage
}


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Reactive data store — updated when new data is fetched
  summaries_df <- reactiveVal(summaries_df_init)
  
  # ---- TREND PLOT ------------------------------------------
  {
    output$trend_plot <- renderPlotly({
      p <- create_trend_plot(
        df          = summaries_df(),
        start_date  = input$trend_start,
        end_date    = input$trend_end,
        period      = input$trend_period,
        agg_fn_name = input$trend_agg,
        metric      = input$trend_metric,
        min_races   = input$trend_min_races,
        show_trend  = input$trend_smooth
      )
      ggplotly(p, tooltip = "text") %>% apply_dark_theme()
    })
  }
  
  # ---- NEW RACE --------------------------------------------
  {
    new_race <- eventReactive(input$nr_go, {
      req(input$nr_distance, input$nr_pace)
      pace_dec <- pace_str_to_dec(input$nr_pace)
      list(
        distance          = input$nr_distance,
        pace_dec          = pace_dec,
        ascent            = input$nr_ascent,
        descent           = input$nr_descent,
        net_ascent_per_km = (input$nr_ascent - input$nr_descent) / input$nr_distance,
        score             = score_new_race(input$nr_distance, pace_dec, summaries_df())
      )
    })
    
    output$nr_score_box <- renderInfoBox({
      nr <- new_race()
      infoBox("Score", paste0(nr$score, " / 100"),
              icon = icon("star"), color = "yellow", fill = TRUE)
    })
    
    output$nr_pct_dist_box <- renderInfoBox({
      nr  <- new_race()
      pct <- round(100 * ecdf(summaries_df()$distance_total)(nr$distance), 1)
      infoBox("Distance percentile", paste0(pct, "th"),
              icon = icon("ruler"), color = "blue", fill = TRUE)
    })
    
    output$nr_pct_pace_box <- renderInfoBox({
      nr  <- new_race()
      pct <- round(100 * (1 - ecdf(summaries_df()$pace_mean)(nr$pace_dec)), 1)
      infoBox("Pace percentile", paste0(pct, "th"),
              icon = icon("bolt"), color = "green", fill = TRUE)
    })
    
    # Similar race styled card
    output$nr_similar_card <- renderUI({
      nr      <- new_race()
      similar <- find_similar_race(nr$distance, nr$pace_dec, nr$net_ascent_per_km, summaries_df())
      s       <- similar[1, ]
      
      score_color <- if (s$score >= 75) "#00cc44"
      else if (s$score >= 50) "#ffdd00"
      else "#ff4444"
      
      tags$div(
        style = "display:flex; gap:16px; flex-wrap:wrap; padding: 8px 0;",
        tags$div(style = paste0("background:#1a1a1a; border-left: 4px solid ", score_color,
                                "; padding:12px 20px; border-radius:6px; min-width:140px;"),
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Date"),
                 tags$div(style = "color:#f0f0f0; font-size:18px; font-weight:bold;",
                          format(s$start_time, "%Y-%m-%d"))
        ),
        tags$div(style = "background:#1a1a1a; border-left:4px solid #00d4ff; padding:12px 20px; border-radius:6px; min-width:120px;",
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Distance"),
                 tags$div(style = "color:#00d4ff; font-size:18px; font-weight:bold;",
                          paste0(round(s$distance_total, 2), " km"))
        ),
        tags$div(style = "background:#1a1a1a; border-left:4px solid #00d4ff; padding:12px 20px; border-radius:6px; min-width:120px;",
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Pace"),
                 tags$div(style = "color:#00d4ff; font-size:18px; font-weight:bold;",
                          pace_dec_to_str(s$pace_mean))
        ),
        tags$div(style = "background:#1a1a1a; border-left:4px solid #ffaa00; padding:12px 20px; border-radius:6px; min-width:120px;",
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Ascent"),
                 tags$div(style = "color:#ffaa00; font-size:18px; font-weight:bold;",
                          paste0(round(s$ascent_total, 0), " m"))
        ),
        tags$div(style = "background:#1a1a1a; border-left:4px solid #ffaa00; padding:12px 20px; border-radius:6px; min-width:120px;",
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Descent"),
                 tags$div(style = "color:#ffaa00; font-size:18px; font-weight:bold;",
                          paste0(round(s$descent_total, 0), " m"))
        ),
        tags$div(style = paste0("background:#1a1a1a; border-left:4px solid ", score_color,
                                "; padding:12px 20px; border-radius:6px; min-width:120px;"),
                 tags$div(style = "color:#aaa; font-size:11px; text-transform:uppercase;", "Score"),
                 tags$div(style = paste0("color:", score_color, "; font-size:18px; font-weight:bold;"),
                          paste0(round(s$score, 1), " / 100"))
        )
      )
    })
    
    # Violin: distance
    output$nr_dist_plot <- renderPlotly({
      nr <- new_race()
      plot_ly() %>%
        add_trace(
          type = "violin", y = summaries_df()$distance_total,
          name = "History",
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          fillcolor = "rgba(0,212,255,0.2)",
          line = list(color = "#00d4ff"),
          points = FALSE
        ) %>%
        add_trace(
          type = "scatter", mode = "markers",
          x = 0, y = nr$distance,
          name = "New race",
          marker = list(color = "#ff6b6b", size = 14, symbol = "diamond")
        ) %>%
        layout(
          xaxis = list(showticklabels = FALSE, title = ""),
          yaxis = list(title = "Distance (km)"),
          showlegend = FALSE
        ) %>%
        apply_dark_theme()
    })
    
    # Violin: pace
    output$nr_pace_plot <- renderPlotly({
      nr <- new_race()
      plot_ly() %>%
        add_trace(
          type = "violin", y = summaries_df()$pace_mean,
          name = "History",
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          fillcolor = "rgba(0,212,255,0.2)",
          line = list(color = "#00d4ff"),
          points = FALSE
        ) %>%
        add_trace(
          type = "scatter", mode = "markers",
          x = 0, y = nr$pace_dec,
          name = "New race",
          marker = list(color = "#ff6b6b", size = 14, symbol = "diamond")
        ) %>%
        layout(
          xaxis = list(showticklabels = FALSE, title = ""),
          yaxis = list(title = "Pace (min/km)"),
          showlegend = FALSE
        ) %>%
        apply_dark_theme()
    })
    
    # Scatterplot with efficiency frontier
    output$nr_scatter_plot <- renderPlotly({
      nr       <- new_race()
      frontier <- compute_efficiency_frontier(summaries_df())
      
      p <- summaries_df() %>%
        ggplot(aes(x = distance_total, y = pace_mean, color = score,
                   text = paste0(format(start_time, "%Y-%m-%d"),
                                 "<br>", round(distance_total, 1), " km | ",
                                 sapply(pace_mean, pace_dec_to_str),
                                 "<br>Score: ", round(score, 1)))) +
        geom_point(alpha = 0.8, size = 2.5) +
        geom_line(data = frontier, aes(x = distance_total, y = pace_mean),
                  color = "#00cc44", linewidth = 0.8, linetype = "dashed",
                  alpha = 0.7, inherit.aes = FALSE) +
        geom_point(data = frontier, aes(x = distance_total, y = pace_mean),
                   color = "#00cc44", size = 2, shape = 16,
                   alpha = 0.5, inherit.aes = FALSE) +
        geom_point(aes(x = nr$distance, y = nr$pace_dec),
                   color = "#ff6b6b", size = 8, shape = 18, inherit.aes = FALSE) +
        scale_color_gradient2(low = "#ff4444", mid = "#ffdd00",
                              high = "#00cc44", midpoint = 50) +
        xlab("Distance (km)") + ylab("Pace (min/km)") +
        theme_fantasmagoria()
      ggplotly(p, tooltip = "text") %>% apply_dark_theme()
    })
  }
  
  # ---- DEEP DIVE -------------------------------------------
  {
    # Summary as a styled card (no table — avoids white row issue)
    output$dd_summary_table <- renderUI({
      df <- summaries_df() %>% filter(id == input$dd_id)
      if (nrow(df) == 0) return(NULL)
      
      score_color <- if (df$score[1] >= 75) "#00cc44"
      else if (df$score[1] >= 50) "#ffdd00"
      else "#ff4444"
      
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; padding:4px 0;",
        dd_stat("Date",     format(df$start_time[1], "%Y-%m-%d"),       "#f0f0f0"),
        dd_stat("Distance", paste0(round(df$distance_total[1], 2), " km"), "#00d4ff"),
        dd_stat("Pace",     pace_dec_to_str(df$pace_mean[1]),             "#00d4ff"),
        dd_stat("Duration", paste0(round(df$duration_min[1], 1), " min"), "#f0f0f0"),
        dd_stat("Ascent",   paste0(round(df$ascent_total[1], 0), " m"),   "#ffaa00"),
        dd_stat("Descent",  paste0(round(df$descent_total[1], 0), " m"),  "#ffaa00"),
        dd_stat("Score",    paste0(round(df$score[1], 1), " / 100"),      score_color)
      )
    })
    
    output$dd_pace_plot <- renderPlotly({
      avg_pace <- summaries_df() %>% filter(id == input$dd_id) %>% pull(pace_mean)
      if (input$dd_pace_view == "continuous") {
        # plot_pace_continuous returns native plotly — just apply theme
        plot_pace_continuous(input$dd_id, avg_pace = avg_pace) %>%
          apply_dark_theme()
      } else {
        p <- plot_pace_by_km(input$dd_id, avg_pace = avg_pace)
        ggplotly(p, tooltip = "text") %>% apply_dark_theme()
      }
    })
    
    output$dd_elevation_plot <- renderPlotly({
      p <- plot_elevation(input$dd_id)
      ggplotly(p) %>% apply_dark_theme()
    })
    
    output$dd_map <- renderLeaflet({
      gps <- get_gps_data(input$dd_id)
      
      if (is.null(gps) || nrow(gps) == 0) {
        return(leaflet::leaflet() %>%
                 leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
                 leaflet::addPopups(lng = -99.1332, lat = 19.4326,
                                    popup = "No GPS data available for this run"))
      }
      
      pal <- leaflet::colorNumeric("RdYlGn", domain = gps$minute, reverse = TRUE)
      
      # Build hover labels with km, pace, and elapsed time
      hover_labels <- paste0(
        "<b>Km:</b> ", round(gps$cum_distance, 2),
        " | <b>Pace:</b> ", gps$pace_str,
        " | <b>Min:</b> ", round(gps$minute, 1)
      )
      
      leaflet::leaflet(gps) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::addCircles(
          lng         = ~longitude, lat = ~latitude,
          radius      = 8, color = ~pal(minute),
          stroke      = FALSE, fillOpacity = 0.9,
          label       = lapply(hover_labels, shiny::HTML),
          popup       = lapply(hover_labels, shiny::HTML)
        ) %>%
        leaflet::addPolylines(
          lng     = ~longitude, lat = ~latitude,
          color   = "#00d4ff", weight = 2, opacity = 0.6
        ) %>%
        leaflet::addLegend(
          position = "bottomright", pal = pal, values = ~minute,
          title    = "Elapsed time",
          labFormat = leaflet::labelFormat(suffix = " min"),
          opacity  = 0.9
        )
    })
  }
  
  # ---- UPDATE DATA -----------------------------------------
  {
    output$update_total_runs <- renderInfoBox({
      infoBox("Total runs", nrow(summaries_df()),
              icon = icon("person-running"), color = "blue", fill = TRUE)
    })
    
    output$update_latest_run <- renderInfoBox({
      infoBox("Latest run", format(max(summaries_df()$start_time), "%Y-%m-%d"),
              icon = icon("calendar-check"), color = "green", fill = TRUE)
    })
    
    output$update_earliest_run <- renderInfoBox({
      infoBox("First run", format(min(summaries_df()$start_time), "%Y-%m-%d"),
              icon = icon("calendar"), color = "yellow", fill = TRUE)
    })
    
    observeEvent(input$update_go, {
      req(input$update_token)
      
      # Collect log messages
      log_messages <- reactiveVal("")
      log_fn <- function(msg) {
        log_messages(paste0(log_messages(), msg, "\n"))
      }
      
      tryCatch({
        log_fn("Starting incremental update...")
        n_new <- fetch_new_activities(
          token        = input$update_token,
          output_dir   = BASE_DIR,
          existing_ids = summaries_df()$id,
          log_fn       = log_fn
        )
        
        if (!is.null(n_new) && n_new > 0) {
          log_fn("Reloading data...")
          summaries_df(reload_summaries())
          
          # Update the deep dive dropdown
          new_df <- summaries_df()
          updateSelectInput(session, "dd_id",
                            choices  = setNames(
                              new_df$id,
                              paste(format(new_df$start_time, "%Y-%m-%d"),
                                    round(new_df$distance_total, 1), "km |",
                                    sapply(new_df$pace_mean, pace_dec_to_str), "min/km")
                            ),
                            selected = new_df$id[1]
          )
          log_fn("Dashboard updated successfully!")
        }
      }, error = function(e) {
        log_fn(paste0("ERROR: ", e$message))
      })
      
      output$update_log <- renderText({ log_messages() })
    })
  }
  
  # ---- SHOES ANALYSIS ---------------------------------------
  {
    # Load shoe data once (cached)
    shoe_data <- reactive({
      summaries_df()  # trigger reactivity on data refresh
      shoe_df <- load_shoe_data()
      compute_shoe_stats(summaries_df(), shoe_df)
    })
    
    # Shoe dictionary display
    output$shoes_dict_ui <- renderUI({
      stats <- shoe_data()
      if (nrow(stats) == 0) return(p("No shoe data found."))
      
      shoe_ids <- stats$shoe_id[stats$shoe_id != "Unassigned"]
      if (length(shoe_ids) == 0) return(p("No shoes with IDs found in your data."))
      
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:12px;",
        lapply(shoe_ids, function(sid) {
          tags$div(
            style = "background:#1a1a1a; padding:8px 14px; border-radius:6px; border-left:3px solid #00d4ff;",
            tags$span(style = "color:#aaa; font-size:10px;", "ID: "),
            tags$span(style = "color:#f0f0f0; font-size:12px; font-family:monospace;",
                      substr(sid, 1, 12)),
            tags$span(style = "color:#aaa; font-size:10px;", "...")
          )
        })
      )
    })
    
    # Mileage bar chart
    output$shoes_bar_plot <- renderPlotly({
      stats <- shoe_data()
      if (nrow(stats) == 0) return(plot_ly() %>% apply_dark_theme())
      
      stats <- stats %>% arrange(total_km)
      stats$shoe_label <- ifelse(
        stats$shoe_id == "Unassigned", "Unassigned",
        paste0("Shoe ", seq_len(nrow(stats)))
      )
      stats$shoe_label <- factor(stats$shoe_label, levels = stats$shoe_label)
      
      plot_ly(stats, x = ~total_km, y = ~shoe_label, type = "bar",
              orientation = "h",
              marker = list(color = COL_ACCENT),
              hoverinfo = "text",
              text = ~paste0(shoe_label, "<br>", total_km, " km<br>",
                             n_races, " races")) %>%
        layout(
          xaxis = list(title = "Total Kilometers"),
          yaxis = list(title = ""),
          margin = list(l = 100)
        ) %>%
        apply_dark_theme()
    })
    
    # Scorecards for each shoe
    output$shoes_scorecards <- renderUI({
      stats <- shoe_data()
      if (nrow(stats) == 0) return(p("No shoe data available."))
      
      cards <- lapply(seq_len(nrow(stats)), function(i) {
        s <- stats[i, ]
        shoe_label <- if (s$shoe_id == "Unassigned") "Unassigned" else paste0("Shoe ", i)
        
        tags$div(
          style = "background:#2b2b2b; border-radius:8px; padding:16px; margin-bottom:16px;
                   border-left:4px solid #00d4ff;",
          tags$h4(style = "color:#00d4ff; margin-top:0;", shoe_label),
          tags$div(
            style = "display:flex; flex-wrap:wrap; gap:12px;",
            tags$div(style = "display:flex; flex-wrap:wrap; gap:8px; width:100%;",
                     dd_stat("Total km",      paste0(s$total_km, " km"),              "#00d4ff"),
                     dd_stat("Races",         s$n_races,                              "#f0f0f0"),
                     dd_stat("Avg pace",      pace_dec_to_str(s$avg_pace),            "#00d4ff"),
                     dd_stat("Best pace",     pace_dec_to_str(s$best_pace),           "#00cc44"),
                     dd_stat("Worst pace",    pace_dec_to_str(s$worst_pace),          "#ff4444"),
                     dd_stat("Avg distance",  paste0(s$avg_distance, " km"),          "#f0f0f0"),
                     dd_stat("Min distance",  paste0(s$min_distance, " km"),          "#f0f0f0"),
                     dd_stat("Max distance",  paste0(s$max_distance, " km"),          "#f0f0f0"),
                     dd_stat("Dist %ile",     paste0(s$dist_pct, "th"),               "#ffaa00"),
                     dd_stat("Pace %ile",     paste0(s$pace_pct, "th"),               "#ffaa00"),
                     dd_stat("First used",    format(s$first_used, "%Y-%m-%d"),       "#aaa"),
                     dd_stat("Last used",     format(s$last_used, "%Y-%m-%d"),        "#aaa")
            )
          )
        )
      })
      
      do.call(tagList, cards)
    })
  }
  
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui, server)
