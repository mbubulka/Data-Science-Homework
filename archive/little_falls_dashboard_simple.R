# Simplified Tufte Dashboard - Error-Free Version
# Focused on core functionality without complex plotly operations

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Source the enhanced predictor with error handling
tryCatch({
  source("enhanced_potomac_predictor.R")
}, error = function(e) {
  cat("Warning: Enhanced predictor not available, using synthetic data\n")
  enhanced_potomac_predictor <- function(alert_threshold = 50, detailed = TRUE) {
    list(
      current_conditions = list(
        flow_cfs = 2500,
        safety_score = 75,
        risk_level = "MODERATE"
      ),
      detailed_scoring = list(
        flow_score = 35,
        trend_score = 25,
        seasonal_score = 15,
        experience_score = 0
      ),
      forecast = data.frame(
        day = 1:7,
        predicted_flow = c(2400, 2300, 2200, 2100, 2000, 1900, 1800),
        safety_score = c(73, 71, 69, 67, 65, 63, 61),
        risk_level = rep("MODERATE", 7)
      ),
      data_source = "synthetic"
    )
  }
})

# Clean CSS - Tufte inspired
tufte_css <- "
<style>
.content-wrapper, .right-side { background-color: #fcfcfc; }
.box { border: 1px solid #e8e8e8; border-radius: 2px; box-shadow: none; background: white; }
.small-box { border-radius: 0; background: white !important; border: 1px solid #ddd; box-shadow: none; }
.small-box h3 { font-family: Georgia, serif; font-size: 28px; font-weight: normal; color: #333; }
.small-box p { font-family: Georgia, serif; font-size: 13px; color: #666; }
body { font-family: Georgia, serif; color: #333; }
</style>
"

ui <- dashboardPage(
  dashboardHeader(title = "Little Falls Safety Analysis"),
  dashboardSidebar(
    tags$head(tags$HTML(tufte_css)),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("water")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("current_flow", width = 3),
          valueBoxOutput("safety_score", width = 3),
          valueBoxOutput("risk_level", width = 3),
          valueBoxOutput("data_source", width = 3)
        ),
        fluidRow(
          box(
            title = "Flow Rate Forecast", status = "primary", solidHeader = TRUE,
            width = 8, height = 400,
            plotOutput("flow_plot", height = "320px")
          ),
          box(
            title = "Safety Components", status = "info", solidHeader = TRUE,
            width = 4, height = 400,
            plotOutput("components_plot", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "7-Day Forecast Summary", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("forecast_table")
          )
        )
      ),
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Safety Score Trend", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            plotOutput("safety_trend", height = "420px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(analysis_data = NULL, last_update = NULL)
  
  # Update data
  observe({
    invalidateLater(300000, session)
    update_analysis()
  })
  
  observe({
    update_analysis()
  })
  
  update_analysis <- function() {
    tryCatch({
      result <- enhanced_potomac_predictor(alert_threshold = 50, detailed = TRUE)
      values$analysis_data <- result
      values$last_update <- Sys.time()
    }, error = function(e) {
      cat("Analysis update failed:", e$message, "\n")
    })
  }
  
  # Value boxes
  output$current_flow <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Flow Rate (cfs)", icon = NULL, color = "white")
    } else {
      flow <- round(values$analysis_data$current_conditions$flow_cfs, 0)
      valueBox(format(flow, big.mark = ","), "Flow Rate (cfs)", icon = NULL, color = "white")
    }
  })
  
  output$safety_score <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Safety Score", icon = NULL, color = "white")
    } else {
      score <- values$analysis_data$current_conditions$safety_score
      valueBox(paste0(score, "/100"), "Safety Score", icon = NULL, color = "white")
    }
  })
  
  output$risk_level <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Risk Level", icon = NULL, color = "white")
    } else {
      risk <- values$analysis_data$current_conditions$risk_level
      valueBox(risk, "Risk Assessment", icon = NULL, color = "white")
    }
  })
  
  output$data_source <- renderValueBox({
    if (is.null(values$analysis_data)) {
      valueBox("--", "Data Source", icon = NULL, color = "white")
    } else {
      source_type <- values$analysis_data$data_source
      if (is.null(source_type) || length(source_type) == 0) {
        source_type <- "unknown"
      } else if (length(source_type) > 1) {
        source_type <- source_type[1]
      }
      display_source <- ifelse(source_type == "USGS_realtime", "Real-time", "Synthetic")
      valueBox(display_source, "Data Source", icon = NULL, color = "white")
    }
  })
  
  # Flow plot - clean ggplot
  output$flow_plot <- renderPlot({
    if (is.null(values$analysis_data)) return(NULL)
    
    forecast_data <- values$analysis_data$forecast
    current_flow <- values$analysis_data$current_conditions$flow_cfs
    
    plot_data <- data.frame(
      day = 0:7,
      flow_cfs = c(current_flow, forecast_data$predicted_flow),
      type = c("Current", rep("Forecast", 7))
    )
    
    ggplot(plot_data, aes(x = day, y = flow_cfs)) +
      geom_line(color = "#333", linewidth = 1) +
      geom_point(color = "#333", size = 2.5) +
      labs(x = "Days from Today", y = "Flow Rate (cfs)", title = NULL) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
        text = element_text(family = "serif", color = "#333", size = 12),
        axis.text = element_text(color = "#666")
      )
  })
  
  # Components plot - dot plot
  output$components_plot <- renderPlot({
    if (is.null(values$analysis_data)) return(NULL)
    
    scoring_details <- values$analysis_data$detailed_scoring
    components_data <- data.frame(
      component = c("Flow", "Trend", "Seasonal", "Experience"),
      score = c(
        ifelse(is.null(scoring_details$flow_score), 30, scoring_details$flow_score),
        ifelse(is.null(scoring_details$trend_score), 25, scoring_details$trend_score),
        ifelse(is.null(scoring_details$seasonal_score), 15, scoring_details$seasonal_score),
        ifelse(is.null(scoring_details$experience_score), 5, scoring_details$experience_score)
      ),
      max_score = c(40, 30, 20, 10)
    )
    
    ggplot(components_data, aes(x = score, y = reorder(component, score))) +
      geom_segment(aes(x = 0, xend = max_score, y = component, yend = component), 
                  color = "#e0e0e0", linewidth = 6) +
      geom_segment(aes(x = 0, xend = score, y = component, yend = component), 
                  color = "#333", linewidth = 6) +
      geom_text(aes(x = score + 1, label = paste0(score, "/", max_score)), 
               size = 3.5, hjust = 0, color = "#333") +
      labs(x = "Score", y = NULL, title = NULL) +
      theme_void() +
      theme(
        axis.text.y = element_text(color = "#333", hjust = 1, size = 11, family = "serif"),
        text = element_text(family = "serif")
      )
  })
  
  # Safety trend plot
  output$safety_trend <- renderPlot({
    if (is.null(values$analysis_data)) return(NULL)
    
    forecast_data <- values$analysis_data$forecast
    current_safety <- values$analysis_data$current_conditions$safety_score
    
    trend_data <- data.frame(
      day = 0:7,
      safety_score = c(current_safety, forecast_data$safety_score)
    )
    
    ggplot(trend_data, aes(x = day, y = safety_score)) +
      geom_hline(yintercept = 80, linetype = "dashed", color = "#27ae60", alpha = 0.7) +
      geom_hline(yintercept = 60, linetype = "dashed", color = "#f39c12", alpha = 0.7) +
      geom_hline(yintercept = 40, linetype = "dashed", color = "#e74c3c", alpha = 0.7) +
      geom_area(fill = "#3498db", alpha = 0.2) +
      geom_line(color = "#2980b9", linewidth = 1.2) +
      geom_point(color = "#2980b9", size = 3) +
      labs(x = "Days from Today", y = "Safety Score (0-100)", 
           title = "7-Day Safety Score Forecast",
           caption = "Dashed lines: 80=Excellent, 60=Good, 40=Poor") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
        text = element_text(family = "serif", color = "#333", size = 12),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, color = "#666")
      ) +
      ylim(0, 100)
  })
  
  # Forecast table
  output$forecast_table <- DT::renderDataTable({
    if (is.null(values$analysis_data)) return(NULL)
    
    forecast_df <- values$analysis_data$forecast %>%
      select(Day = day, Flow = predicted_flow, Score = safety_score, Risk = risk_level) %>%
      mutate(
        Flow = paste0(round(Flow, 0), " cfs"),
        Score = paste0(Score, "/100")
      )
    
    DT::datatable(forecast_df, 
                  options = list(
                    pageLength = 7,
                    dom = 't',
                    ordering = FALSE
                  ),
                  rownames = FALSE)
  }, server = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)