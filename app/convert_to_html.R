# Convert Shiny Dashboard to Static HTML
# This script creates a static HTML version of your Shiny dashboard

library(htmlwidgets)
library(plotly)
library(DT)
library(htmltools)

# Source your enhanced predictor
if (file.exists("enhanced_potomac_predictor.R")) {
  source("enhanced_potomac_predictor.R")
} else {
  # Fallback predictor function
  enhanced_potomac_predictor <- function(alert_threshold = 50, detailed = TRUE) {
    list(
      current_conditions = list(
        flow_cfs = 2500,
        safety_score = 75,
        risk_level = "MODERATE"
      ),
      forecast = data.frame(
        day = 1:7,
        predicted_flow = c(2500, 2600, 2400, 2300, 2200, 2100, 2000),
        safety_score = c(75, 70, 80, 85, 88, 90, 92),
        risk_level = c("MODERATE", "MODERATE", "LOW", "LOW", "LOW", "LOW", "LOW")
      )
    )
  }
}

# Generate data
prediction_data <- enhanced_potomac_predictor(detailed = TRUE)

# Create HTML content
html_content <- tags$html(
  tags$head(
    tags$title("Potomac River Safety Dashboard"),
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; margin: 0; padding: 20px; background: #f8f9fa; }
      .header { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; text-align: center; }
      .metrics { display: flex; gap: 20px; margin-bottom: 20px; flex-wrap: wrap; }
      .metric-card { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 10px; padding: 20px; flex: 1; min-width: 200px; text-align: center; }
      .metric-value { font-size: 2.5rem; font-weight: bold; margin-bottom: 5px; }
      .metric-label { font-size: 0.9rem; opacity: 0.9; }
      .chart-container { background: white; border-radius: 10px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 15px rgba(0,0,0,0.1); }
      .chart-title { font-size: 1.2rem; font-weight: 600; margin-bottom: 15px; color: #2c3e50; }
    "))
  ),
  tags$body(
    tags$div(class = "header",
      tags$h1("🌊 Potomac River Safety Dashboard"),
      tags$p("Real-time river safety analytics with predictive flow forecasting")
    ),
    
    tags$div(class = "metrics",
      tags$div(class = "metric-card",
        tags$div(class = "metric-value", prediction_data$current_conditions$flow_cfs),
        tags$div(class = "metric-label", "Current Flow (cfs)")
      ),
      tags$div(class = "metric-card",
        tags$div(class = "metric-value", prediction_data$current_conditions$safety_score),
        tags$div(class = "metric-label", "Safety Score")
      ),
      tags$div(class = "metric-card",
        tags$div(class = "metric-value", prediction_data$current_conditions$risk_level),
        tags$div(class = "metric-label", "Risk Level")
      )
    ),
    
    tags$div(class = "chart-container",
      tags$div(class = "chart-title", "7-Day Flow Forecast"),
      tags$div(id = "forecast-chart")
    ),
    
    tags$div(class = "chart-container",
      tags$div(class = "chart-title", "Forecast Data Table"),
      tags$div(id = "forecast-table")
    )
  )
)

# Create plotly chart
forecast_plot <- plot_ly(
  data = prediction_data$forecast,
  x = ~day,
  y = ~predicted_flow,
  type = 'scatter',
  mode = 'lines+markers',
  name = 'Predicted Flow',
  line = list(color = '#3498db', width = 3),
  marker = list(size = 8, color = '#3498db')
) %>%
  add_trace(
    y = ~safety_score * 30,  # Scale for secondary axis
    name = 'Safety Score',
    line = list(color = '#e74c3c', width = 3),
    marker = list(size = 8, color = '#e74c3c'),
    yaxis = 'y2'
  ) %>%
  layout(
    title = '',
    xaxis = list(title = 'Day'),
    yaxis = list(title = 'Flow (cfs)'),
    yaxis2 = list(
      title = 'Safety Score',
      overlaying = 'y',
      side = 'right'
    ),
    showlegend = TRUE,
    plot_bgcolor = 'rgba(0,0,0,0)',
    paper_bgcolor = 'rgba(0,0,0,0)'
  )

# Create data table
forecast_table <- DT::datatable(
  prediction_data$forecast,
  options = list(
    pageLength = 10,
    dom = 't',
    scrollX = TRUE
  ),
  colnames = c('Day', 'Predicted Flow (cfs)', 'Safety Score', 'Risk Level'),
  class = 'cell-border stripe'
)

# Save as HTML
html_with_widgets <- tagList(
  html_content,
  tags$script(HTML("
    document.getElementById('forecast-chart').innerHTML = '';
    document.getElementById('forecast-table').innerHTML = '';
  ")),
  forecast_plot,
  forecast_table
)

# Export to HTML
htmltools::save_html(html_with_widgets, "potomac_dashboard_static.html")

cat("Static HTML dashboard created: potomac_dashboard_static.html\n")