#' Little Falls Safety Dashboard
#' 
#' @description
#' An interactive Shiny dashboard for visualizing and analyzing kayaking safety 
#' conditions at Little Falls on the Potomac River. This dashboard provides real-time 
#' data integration with USGS water services, safety scoring, and 7-day forecasting.
#'
#' @details
#' This dashboard provides:
#' 1. Real-time safety assessment with color-coded indicators
#' 2. Detailed breakdown of safety components
#' 3. 7-day flow and safety forecasts
#' 4. Data visualization tools for trend analysis
#' 5. Methodology explanation for safety scoring
#'
#' @author Bubulka Analytics
#' @export

# Required packages
#' @importFrom shiny shinyApp fluidRow wellPanel h4 h6 div br actionButton
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#'   sidebarMenu menuItem tabItems tabItem valueBoxOutput box
#' @importFrom DT dataTableOutput renderDataTable datatable formatStyle styleEqual
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom dataRetrieval readNWISuv readNWISdv
#' @importFrom tidyverse %>% mutate filter select
#' @importFrom lubridate days month Sys.Date
#' @importFrom ggplot2 ggplot aes geom_col geom_text geom_line geom_point
#'   coord_flip labs theme_minimal theme
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dataRetrieval)
library(tidyverse)
library(lubridate)

# Source the enhanced predictor
source("enhanced_potomac_predictor_refactored.R")

#' Define UI for Little Falls Safety Dashboard
#' 
#' @description
#' Creates the user interface for the Little Falls Safety Dashboard
#'
#' @return A dashboardPage object containing the UI definition
#' @noRd
ui <- dashboardPage(
  dashboardHeader(title = "Little Falls (Potomac) Safety Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Methodology", tabName = "methodology", icon = icon("microscope")),
      br(),
      wellPanel(
        h4("Current Status", style = "text-align: center; color: #2c3e50;"),
        div(id = "status_display", style = "padding: 5px;"),
        br(),
        actionButton("refresh_data", "Update Analysis", 
                    class = "btn-info", width = "100%"),
        br(), br(),
        h6("Little Falls Conditions (USGS 01646500)", 
           style = "text-align: center; color: #7f8c8d;"),
        div(textOutput("update_time"), style = "text-align: center; color: #95a5a6;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar { background-color: #34495e !important; }
        .skin-blue .main-header .logo { background-color: #34495e !important; }
        .content-wrapper { background-color: #ecf0f1 !important; }
        .box { border-top: 3px solid #3498db !important; }
      "))
    ),
    
    tabItems(
      # Analysis Dashboard
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("flow_rate"),
          valueBoxOutput("safety_index"), 
          valueBoxOutput("data_quality")
        ),
        
        fluidRow(
          box(
            title = "Safety Assessment Breakdown", status = "primary", 
            solidHeader = TRUE, width = 6, height = 400,
            plotlyOutput("safety_components")
          ),
          
          box(
            title = "7-Day Flow Prediction", status = "success", 
            solidHeader = TRUE, width = 6, height = 400,
            plotlyOutput("forecast_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Analysis Results", status = "info", 
            solidHeader = TRUE, width = 12,
            DT::dataTableOutput("analysis_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Safety Scoring Explanation", status = "info", 
            solidHeader = TRUE, width = 12,
            wellPanel(
              h4("How the Little Falls Safety Score is Calculated (Total: 100 points)"),
              
              tags$div(
                tags$h5("Flow Safety (40 points max)", 
                       style = "color: #2980b9; font-weight: bold;"),
                tags$p("Based on actual Little Falls water levels:"),
                tags$ul(
                  tags$li(tags$strong("< 800 cfs (< 2 feet):"), 
                         " Too low - rocks exposed (0 points)"),
                  tags$li(tags$strong("800-1,500 cfs (2-3 feet):"), 
                         " Good for beginners (30 points)"),
                  tags$li(tags$strong("1,500-2,500 cfs (3-4 feet):"), 
                         " Optimal intermediate (40 points)"),
                  tags$li(tags$strong("2,500-4,000 cfs (4+ feet):"), 
                         " Advanced - fast water (25 points)"),
                  tags$li(tags$strong("> 4,000 cfs (> 5 feet):"), 
                         " High water - very pushy (15-5 points)")
                )
              ),
              
              tags$div(
                tags$h5("Trend Stability (30 points max)", 
                       style = "color: #27ae60; font-weight: bold;"),
                tags$p("How quickly water levels are changing:"),
                tags$ul(
                  tags$li(tags$strong("Stable (< 500 cfs change):"), 
                         " Predictable conditions (30 points)"),
                  tags$li(tags$strong("Moderate change (500-1,000 cfs):"), 
                         " Some variability (20 points)"),
                  tags$li(tags$strong("Rapid change (> 1,000 cfs):"), 
                         " Unpredictable (10-0 points)")
                )
              ),
              
              tags$div(
                tags$h5("Seasonal Factor (20 points max)", 
                       style = "color: #e67e22; font-weight: bold;"),
                tags$p("Time of year affects safety:"),
                tags$ul(
                  tags$li(tags$strong("Summer/Early Fall:"), " Warm, stable (20 points)"),
                  tags$li(tags$strong("Late Fall/Winter:"), 
                         " Cold water risk (10-15 points)"),
                  tags$li(tags$strong("Spring:"), " Unpredictable snowmelt (5 points)")
                )
              ),
              
              tags$div(
                tags$h5("Experience Bonus (10 points max)", 
                       style = "color: #8e44ad; font-weight: bold;"),
                tags$p("Bonus for ideal learning conditions:"),
                tags$ul(
                  tags$li(tags$strong("Perfect conditions:"), 
                         " 2,000-4,000 cfs + stable (10 points)"),
                  tags$li(tags$strong("Good conditions:"), 
                         " 1,500-6,000 cfs + moderate (5 points)")
                )
              )
            )
          )
        )
      ),
      
      # Data Visualization
      tabItem(tabName = "visualization",
        fluidRow(
          box(
            title = "Flow Rate Time Series", status = "primary", 
            solidHeader = TRUE, width = 12, height = 500,
            plotlyOutput("timeseries_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Safety Score Distribution", status = "success", 
            solidHeader = TRUE, width = 6,
            plotlyOutput("score_distribution")
          ),
          
          box(
            title = "Trend Analysis", status = "info", 
            solidHeader = TRUE, width = 6,
            plotlyOutput("trend_analysis")
          )
        )
      ),
      
      # Methodology
      tabItem(tabName = "methodology",
        fluidRow(
          box(
            title = "Analysis Methodology", status = "primary", 
            solidHeader = TRUE, width = 12,
            wellPanel(
              h3("Little Falls Kayaking Safety Analysis"),
              p("This system provides real-time safety assessment for kayaking at 
                Little Falls on the Potomac River."),
              
              h4("Data Sources:"),
              tags$ul(
                tags$li("USGS Station 01646500 - Real-time discharge data"),
                tags$li("Site-specific Little Falls calibration"),
                tags$li("Seasonal and trend analysis")
              ),
              
              h4("Applications:"),
              tags$ul(
                tags$li("Real-time data integration and API usage"),
                tags$li("Multi-factor risk assessment algorithm development"),
                tags$li("Site-specific model calibration techniques"),
                tags$li("Interactive dashboard development with R Shiny")
              )
            )
          )
        )
      )
    )
  )
)

#' Define Server Logic for Little Falls Safety Dashboard
#' 
#' @description
#' Creates the server logic for the Little Falls Safety Dashboard
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return A function that defines the server logic
#' @noRd
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    analysis_data = NULL,
    last_update = Sys.time(),
    system_status = "Initializing..."
  )
  
  #' Update analysis data
  #' 
  #' @description
  #' Fetches new data and updates the analysis
  #' 
  #' @return NULL (updates reactive values as a side effect)
  #' @noRd
  update_analysis <- function() {
    tryCatch({
      values$system_status <- "Fetching data..."
      result <- predict_kayaking_safety(alert_threshold = 50, detailed = TRUE)
      values$analysis_data <- result
      values$last_update <- Sys.time()
      values$system_status <- "Analysis complete"
      
    }, error = function(e) {
      values$system_status <- paste("Error:", e$message)
      warning(paste("Dashboard Error:", e$message))
    })
    
    invisible(NULL)
  }
  
  # Initialize data
  observe({
    update_analysis()
  })
  
  # Refresh button
  observeEvent(input$refresh_data, {
    withProgress(message = "Updating analysis...", value = 0, {
      incProgress(0.5)
      update_analysis()
      incProgress(1)
    })
  })
  
  # Value boxes
  output$flow_rate <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Flow Rate (cfs)", icon = icon("water"), color = "light-blue")
    } else {
      flow <- round(values$analysis_data$current_conditions$flow_cfs, 0)
      valueBox(paste0(flow, " cfs"), "Current Flow Rate", 
               icon = icon("water"), color = "blue")
    }
  })
  
  output$safety_index <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Safety Index", icon = icon("shield-alt"), color = "light-blue")
    } else {
      tryCatch({
        score <- values$analysis_data$current_conditions$safety_score
        if (is.null(score) || is.na(score)) {
          valueBox("No Score", "Safety Index", icon = icon("shield-alt"), 
                  color = "red")
        } else {
          color <- if (score >= 85) {
            "green"
          } else if (score >= 70) {
            "yellow"
          } else if (score >= 50) {
            "orange"
          } else {
            "red"
          }
          valueBox(paste0(score, "/100"), "Safety Index", 
                   icon = icon("shield-alt"), color = color)
        }
      }, error = function(e) {
        valueBox("Error", "Safety Index", icon = icon("shield-alt"), color = "red")
      })
    }
  })
  
  output$data_quality <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Data Quality", icon = icon("check-circle"), 
              color = "light-blue")
    } else {
      source_type <- values$analysis_data$data_source
      if (source_type == "USGS_realtime") {
        quality <- "Real-time"
        color <- "green"
      } else {
        quality <- "Synthetic"
        color <- "yellow"
      }
      valueBox(quality, "Data Source", icon = icon("database"), color = color)
    }
  })
  
  # Analysis table
  output$analysis_table <- DT::renderDataTable({
    if (is.null(values$analysis_data) || is.null(values$analysis_data$forecast)) {
      return(NULL)
    }
    
    tryCatch({
      forecast_df <- values$analysis_data$forecast %>%
        mutate(
          Date = date,
          `Flow (cfs)` = predicted_flow,
          `Safety Score` = safety_score,
          `Risk Level` = risk_level,
          Recommendation = recommendation
        ) %>%
        select(Date, `Flow (cfs)`, `Safety Score`, `Risk Level`, Recommendation)
      
      DT::datatable(forecast_df, 
                    options = list(pageLength = 7, dom = 'ft'),
                    rownames = FALSE) %>%
        DT::formatStyle("Risk Level",
          backgroundColor = DT::styleEqual(
            c("LOW", "MODERATE", "HIGH", "EXTREME"),
            c("#27ae60", "#f39c12", "#e74c3c", "#c0392b")
          ),
          color = "white"
        )
    }, error = function(e) {
      warning(paste("Error rendering analysis table:", e$message))
      return(NULL)
    })
  })
  
  # Update time
  output$update_time <- renderText({
    paste("Updated:", format(values$last_update, "%H:%M:%S"))
  })
  
  # Safety components plot
  output$safety_components <- renderPlotly({
    if (is.null(values$analysis_data)) return(NULL)
    
    tryCatch({
      scoring_data <- data.frame(
        Component = c("Flow Safety", "Trend Stability", 
                     "Seasonal Factor", "Experience Bonus"),
        Points = c(values$analysis_data$detailed_scoring$flow_score,
                   values$analysis_data$detailed_scoring$trend_score,
                   values$analysis_data$detailed_scoring$seasonal_score,
                   values$analysis_data$detailed_scoring$experience_score),
        Max_Points = c(40, 30, 20, 10)
      )
      
      p <- ggplot(scoring_data, 
                 aes(x = reorder(Component, Points), y = Points, fill = Component)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = paste0(Points, "/", Max_Points)), hjust = -0.1) +
        coord_flip() +
        labs(title = "Safety Assessment Breakdown", x = "Component", y = "Points") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p)
      
    }, error = function(e) {
      warning(paste("Error rendering safety components plot:", e$message))
      return(NULL)
    })
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    if (is.null(values$analysis_data)) return(NULL)
    
    tryCatch({
      forecast_data <- values$analysis_data$forecast
      
      p <- ggplot(forecast_data, aes(x = day, y = predicted_flow)) +
        geom_line(color = "#2E86AB", size = 1.2) +
        geom_point(color = "#2E86AB", size = 3) +
        labs(title = "7-Day Flow Forecast", x = "Days", y = "Flow (cfs)") +
        theme_minimal()
      
      ggplotly(p)
      
    }, error = function(e) {
      warning(paste("Error rendering forecast plot:", e$message))
      return(NULL)
    })
  })
  
  # Additional plots for the visualization tab could be implemented here
  # For example:
  # - Time series plot of historical data
  # - Safety score distribution
  # - Trend analysis
}

# Launch message
message("LAUNCHING LITTLE FALLS SAFETY ANALYSIS")
message("=========================================")
message("Real-time safety assessment for Little Falls kayaking") 
message("Site-specific calibration with local expertise")
message("Access dashboard at: http://localhost:3838")

# Run the application
shinyApp(ui = ui, server = server)