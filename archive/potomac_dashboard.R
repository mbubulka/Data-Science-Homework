# Potomac River Kayaking Safety Dashboard
# Interactive Shiny application for real-time monitoring

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dataRetrieval)
library(tidyverse)
library(lubridate)

# Source the enhanced predictor
source("enhanced_potomac_predictor.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🏊 Potomac River Kayaking Safety Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("📊 Historical Data", tabName = "historical", icon = icon("chart-line")),
      menuItem("⚙️ Settings", tabName = "settings", icon = icon("cog")),
      br(),
      h4("Quick Status", style = "color: white; text-align: center;"),
      div(id = "quick_status", style = "padding: 10px;"),
      br(),
      actionButton("refresh", "🔄 Refresh Data", 
                  class = "btn-primary", width = "90%"),
      br(), br(),
      h5("Last Updated:", style = "color: white; text-align: center;"),
      textOutput("last_updated", container = function(x) h6(x, style = "color: #aaa; text-align: center;"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-header .logo {
          background-color: #2c3e50 !important;  
        }
        .excellent { background-color: #27ae60 !important; color: white !important; }
        .good { background-color: #f39c12 !important; color: white !important; }
        .fair { background-color: #e67e22 !important; color: white !important; }
        .poor { background-color: #e74c3c !important; color: white !important; }
        .dangerous { background-color: #c0392b !important; color: white !important; }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          # Current Conditions
          valueBoxOutput("current_flow"),
          valueBoxOutput("safety_score"), 
          valueBoxOutput("risk_level")
        ),
        
        fluidRow(
          box(
            title = "📈 Flow Trend (7 Days)", status = "primary", solidHeader = TRUE,
            width = 8, height = 400,
            plotlyOutput("flow_trend_plot")
          ),
          
          box(
            title = "🎯 Safety Breakdown", status = "info", solidHeader = TRUE,
            width = 4, height = 400,
            plotlyOutput("safety_breakdown_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "📅 7-Day Forecast", status = "success", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("forecast_table")
          )
        ),
        
        fluidRow(
          box(
            title = "🚨 Safety Alerts", status = "warning", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("alert_messages")
          ),
          
          box(
            title = "📱 Quick Actions", status = "info", solidHeader = TRUE,
            width = 6,
            wellPanel(
              h4("🚀 Deployment Options"),
              checkboxInput("email_alerts", "📧 Email Alerts (Score < 50)", value = FALSE),
              checkboxInput("sms_alerts", "📱 SMS Notifications", value = FALSE), 
              checkboxInput("social_media", "📢 Social Media Posts", value = FALSE),
              br(),
              downloadButton("download_report", "📄 Download Safety Report", 
                           class = "btn-success"),
              br(), br(),
              actionButton("test_alert", "🧪 Test Alert System", 
                          class = "btn-warning")
            )
          )
        )
      ),
      
      # Historical Data Tab  
      tabItem(tabName = "historical",
        fluidRow(
          box(
            title = "📊 Historical Flow Patterns", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            plotlyOutput("historical_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Monthly Statistics", status = "success", solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("monthly_stats")
          ),
          
          box(
            title = "🏆 Safety Records", status = "info", solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("safety_records")
          )
        )
      ),
      
      # Settings Tab
      tabItem(tabName = "settings",
        fluidRow(
          box(
            title = "⚙️ Alert Settings", status = "primary", solidHeader = TRUE,
            width = 6,
            wellPanel(
              numericInput("alert_threshold", "Safety Score Alert Threshold:", 
                          value = 50, min = 0, max = 100, step = 5),
              textInput("email_address", "Email for Alerts:", 
                       placeholder = "your.email@example.com"),
              textInput("phone_number", "Phone for SMS:", 
                       placeholder = "+1-555-123-4567"),
              checkboxInput("detailed_reports", "Include Detailed Scoring", value = TRUE),
              br(),
              actionButton("save_settings", "💾 Save Settings", class = "btn-success")
            )
          ),
          
          box(
            title = "🔗 Data Sources", status = "info", solidHeader = TRUE,
            width = 6,
            wellPanel(
              h4("📡 USGS Station: 01646500"),
              p("Location: Potomac River near Washington, DC"),
              p("Parameter: Discharge (cubic feet per second)"),
              p("Update Frequency: Every 15 minutes"),
              br(),
              h4("🔄 Refresh Settings"),
              numericInput("auto_refresh", "Auto-refresh interval (minutes):", 
                          value = 15, min = 5, max = 60, step = 5),
              checkboxInput("realtime_mode", "Real-time Mode", value = TRUE)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📋 System Status", status = "warning", solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("system_status")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    prediction_data = NULL,
    last_update = Sys.time(),
    alerts = character(0)
  )
  
  # Enhanced predictor function
  predictor <- enhanced_potomac_predictor
  
  # Update data function
  update_data <- function() {
    tryCatch({
      values$prediction_data <- predictor(alert_threshold = input$alert_threshold %||% 50, 
                                        detailed = TRUE)
      values$last_update <- Sys.time()
      
      # Check for alerts
      if(!is.null(values$prediction_data)) {
        current_score <- values$prediction_data$current_conditions$safety_score
        if(current_score < (input$alert_threshold %||% 50)) {
          alert_msg <- paste0("[", format(Sys.time(), "%H:%M"), "] ⚠️ Safety Alert: Score ", 
                             current_score, "/100 - ", 
                             values$prediction_data$current_conditions$recommendation)
          values$alerts <- c(values$alerts, alert_msg)
          if(length(values$alerts) > 10) values$alerts <- tail(values$alerts, 10)
        }
      }
    }, error = function(e) {
      values$alerts <- c(values$alerts, paste0("[", format(Sys.time(), "%H:%M"), "] ❌ Error: ", e$message))
    })
  }
  
  # Initial data load
  observe({
    update_data()
  })
  
  # Refresh button
  observeEvent(input$refresh, {
    withProgress(message = "Refreshing data...", value = 0, {
      incProgress(0.5)
      update_data() 
      incProgress(1)
    })
  })
  
  # Auto-refresh
  observe({
    if(input$realtime_mode %||% TRUE) {
      invalidateLater((input$auto_refresh %||% 15) * 60 * 1000)
      update_data()
    }
  })
  
  # Value boxes
  output$current_flow <- renderValueBox({
    if(is.null(values$prediction_data)) {
      valueBox("--", "Current Flow (cfs)", icon = icon("water"), color = "light-blue")
    } else {
      flow <- round(values$prediction_data$current_conditions$flow_cfs, 0)
      valueBox(paste0(flow, " cfs"), "Current Flow", icon = icon("water"), color = "blue")
    }
  })
  
  output$safety_score <- renderValueBox({
    if(is.null(values$prediction_data)) {
      valueBox("--", "Safety Score", icon = icon("shield-alt"), color = "light-blue")
    } else {
      score <- values$prediction_data$current_conditions$safety_score
      color <- case_when(
        score >= 85 ~ "green",
        score >= 70 ~ "yellow", 
        score >= 50 ~ "orange",
        score >= 30 ~ "red",
        TRUE ~ "maroon"
      )
      valueBox(paste0(score, "/100"), "Safety Score", icon = icon("shield-alt"), color = color)
    }
  })
  
  output$risk_level <- renderValueBox({
    if(is.null(values$prediction_data)) {
      valueBox("--", "Risk Level", icon = icon("exclamation-triangle"), color = "light-blue")
    } else {
      risk <- values$prediction_data$current_conditions$risk_level
      color <- case_when(
        risk == "LOW" ~ "green",
        risk == "MODERATE" ~ "yellow",
        risk == "HIGH" ~ "red",
        TRUE ~ "maroon"
      )
      valueBox(risk, "Risk Level", icon = icon("exclamation-triangle"), color = color)
    }
  })
  
  # Forecast table
  output$forecast_table <- DT::renderDataTable({
    if(is.null(values$prediction_data)) return(NULL)
    
    forecast_df <- values$prediction_data$forecast %>%
      mutate(
        `📅 Date` = date,
        `🌊 Flow (cfs)` = predicted_flow,
        `🎯 Score` = paste0(safety_score, "/100"),
        `⚠️ Risk` = risk_level,
        `📋 Recommendation` = recommendation
      ) %>%
      select(`📅 Date`, `🌊 Flow (cfs)`, `🎯 Score`, `⚠️ Risk`, `📋 Recommendation`)
    
    DT::datatable(forecast_df, 
                  options = list(pageLength = 7, dom = 't'),
                  rownames = FALSE) %>%
      DT::formatStyle("⚠️ Risk",
        backgroundColor = DT::styleEqual(
          c("LOW", "MODERATE", "HIGH", "EXTREME"),
          c("#27ae60", "#f39c12", "#e74c3c", "#c0392b")
        ),
        color = "white"
      )
  })
  
  # Alert messages
  output$alert_messages <- renderText({
    if(length(values$alerts) == 0) {
      "✅ No current alerts\n\nSystem monitoring normally..."
    } else {
      paste(rev(values$alerts), collapse = "\n")
    }
  })
  
  # Last updated
  output$last_updated <- renderText({
    format(values$last_update, "%H:%M:%S")
  })
  
  # Test alert
  observeEvent(input$test_alert, {
    values$alerts <- c(values$alerts, paste0("[", format(Sys.time(), "%H:%M"), "] 🧪 Test Alert: System operational"))
    showNotification("Test alert sent!", type = "message")
  })
  
  # System status
  output$system_status <- renderText({
    status <- paste0(
      "🟢 Dashboard Status: OPERATIONAL\n",
      "📡 Data Source: ", ifelse(is.null(values$prediction_data), "Initializing...", 
                                values$prediction_data$data_source), "\n",
      "🔄 Last Update: ", format(values$last_update, "%Y-%m-%d %H:%M:%S"), "\n",
      "📊 Records Available: ", ifelse(is.null(values$prediction_data), "0", 
                                     nrow(values$prediction_data$forecast)), "\n",
      "⚠️ Active Alerts: ", length(values$alerts), "\n",
      "💾 Auto-refresh: ", ifelse(input$realtime_mode %||% TRUE, 
                                  paste0("Every ", input$auto_refresh %||% 15, " minutes"), "Disabled")
    )
    status
  })
}

# Run the application
cat("🚀 Starting Potomac River Safety Dashboard...\n")
cat("📱 Access at: http://localhost:3838\n")
cat("🔄 Real-time monitoring enabled\n\n")

shinyApp(ui = ui, server = server)