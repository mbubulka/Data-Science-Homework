# Documentation Recommendations

## Overview

This document provides recommendations for improving the documentation of the Potomac River Kayaking Safety Analysis project. Good documentation is essential for users, contributors, and maintainers to understand how to use, modify, and deploy the project.

## README.md Improvements

The current README.md provides a good overview of the project but could be enhanced with the following additions:

### Suggested README.md Structure

```markdown
# Potomac River Kayaking Safety Analysis & Predictive Dashboard

## Overview

[Keep existing overview section - it's well written]

## Installation

### Prerequisites
- R version 4.0.0 or higher
- Required R packages (see below)

### Required Packages
```r
# Install required packages
install.packages(c(
  "dataRetrieval",  # USGS data access
  "tidyverse",      # Data manipulation and visualization
  "lubridate",      # Date handling
  "shiny",          # Interactive web applications
  "shinydashboard", # Dashboard framework
  "DT",             # Interactive tables
  "plotly",         # Interactive plots
  "forecast"        # Time series forecasting
))
```

### Quick Start
1. Clone this repository
2. Open R or RStudio
3. Set your working directory to the project folder
4. Run one of the following:

```r
# Launch the Little Falls dashboard
source("R/dashboard.R")

# Get a quick safety prediction
source("R/predictor.R")
prediction <- predict_kayaking_safety()
```

## Usage

[Keep existing usage section with the following additions]

### API Reference

#### Core Functions

##### `predict_kayaking_safety(alert_threshold = 50, detailed = TRUE)`
Generates a complete safety assessment and 7-day forecast.

**Parameters:**
- `alert_threshold`: Numeric threshold (0-100) for safety alerts
- `detailed`: Whether to include detailed scoring breakdown

**Returns:** A list containing:
- `timestamp`: Time of the analysis
- `current_conditions`: Current flow, safety score, risk level, and recommendation
- `detailed_scoring`: Detailed breakdown of safety components
- `forecast`: 7-day forecast data frame
- `data_source`: Source of the data (USGS_realtime or synthetic)

##### `calculate_safety_metrics(flow_cfs, trend_cfs = 0)`
Calculates safety metrics based on flow rate and trend.

**Parameters:**
- `flow_cfs`: Current flow rate in cubic feet per second
- `trend_cfs`: Weekly trend in flow rate

**Returns:** A list containing safety scores and recommendations

[Add similar documentation for other key functions]

## Contributing

Guidelines for contributing to the project:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests and ensure code quality
5. Commit your changes (`git commit -m 'feat: add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## License

[Specify the license under which the project is released]

## Acknowledgments

- USGS for providing real-time water data
- [Any other acknowledgments]
```

## Missing Documentation

The following documentation should be created:

### 1. Function Documentation

While the refactored code now includes Roxygen2 documentation, consider creating a separate documentation file for each major function with more detailed examples:

#### Example: `predict_kayaking_safety.md`

```markdown
# predict_kayaking_safety

## Description

Main function that retrieves current data, calculates safety metrics,
and generates a 7-day forecast for kayaking conditions.

## Usage

```r
prediction <- predict_kayaking_safety(alert_threshold = 50, detailed = TRUE)
```

## Arguments

- `alert_threshold`: Numeric threshold for safety alerts (0-100). Default is 50.
- `detailed`: Logical indicating whether to display detailed scoring. Default is TRUE.

## Value

A list containing:
- `timestamp`: Time of the analysis
- `current_conditions`: Current flow, safety score, risk level, and recommendation
- `detailed_scoring`: Detailed breakdown of safety components
- `forecast`: 7-day forecast data frame
- `data_source`: Source of the data (USGS_realtime or synthetic)

## Examples

### Basic Usage

```r
# Get basic prediction
prediction <- predict_kayaking_safety()
print(prediction$current_conditions$safety_score)
```

### Custom Alert Threshold

```r
# Get prediction with custom alert threshold
prediction <- predict_kayaking_safety(alert_threshold = 60)
```

### Accessing Forecast Data

```r
# Get the 7-day forecast and plot it
prediction <- predict_kayaking_safety()
forecast <- prediction$forecast

library(ggplot2)
ggplot(forecast, aes(x = day, y = predicted_flow)) +
  geom_line() +
  geom_point() +
  labs(title = "7-Day Flow Forecast", x = "Days", y = "Flow (cfs)")
```
```

### 2. Technical Documentation

Create a technical documentation file explaining the methodology:

#### Example: `methodology.md`

```markdown
# Potomac River Kayaking Safety Analysis Methodology

## Data Sources

### USGS Water Data
- **Source**: U.S. Geological Survey National Water Information System
- **Station**: 01646500 (Little Falls)
- **Parameter**: Daily mean discharge (cubic feet per second)
- **API**: dataRetrieval R package

### Weather Integration
- **Method**: Synthetic weather network with 5 stations
- **Parameters**: Precipitation, temperature
- **Correlation**: 14-day precipitation lag (r = 0.65)

## Safety Scoring Algorithm

### Flow Safety (40 points)
- **< 800 cfs**: Too low - rocks exposed (0 points)
- **800-1,500 cfs**: Good for beginners (30 points)
- **1,500-2,500 cfs**: Optimal intermediate (40 points)
- **2,500-4,000 cfs**: Advanced conditions (25 points)
- **4,000-6,000 cfs**: High water (15 points)
- **> 6,000 cfs**: Flood stage (5 points)

[Continue with other scoring components]

## Forecasting Methodology

### ARIMA Modeling
- **Training Data**: 21 years of historical data
- **Validation**: 6 upstream gauge stations
- **Improvement**: AIC reduction of 29.88 with weather integration

[Additional technical details]
```

### 3. User Guide

Create a user guide for the dashboard:

#### Example: `dashboard_user_guide.md`

```markdown
# Little Falls Dashboard User Guide

## Getting Started

1. Launch the dashboard by running `source("R/dashboard.R")` in R
2. The dashboard will open in your default web browser
3. The main page shows the current safety assessment

## Dashboard Tabs

### Analysis Dashboard
- **Current Conditions**: Top row shows flow rate, safety index, and data quality
- **Safety Assessment Breakdown**: Bar chart showing the components of the safety score
- **7-Day Flow Prediction**: Line chart showing the forecasted flow
- **Detailed Analysis Results**: Table with day-by-day forecast
- **Safety Scoring Explanation**: Details on how the safety score is calculated

### Data Visualization
- **Flow Rate Time Series**: Historical flow data
- **Safety Score Distribution**: Distribution of safety scores
- **Trend Analysis**: Analysis of flow trends

### Methodology
- Information about the data sources and analysis methods

## Interpreting Results

### Safety Index
- **85-100**: Excellent conditions for all skill levels
- **70-84**: Good conditions for intermediate+ paddlers
- **50-69**: Fair conditions for advanced paddlers
- **30-49**: Poor conditions, experts only
- **0-29**: Dangerous conditions, not recommended

[Additional usage information]
```

## Usage Examples

Create a file with comprehensive usage examples:

### Example: `usage_examples.R`

```r
# Potomac River Kayaking Safety Analysis - Usage Examples

# Load the required packages
library(tidyverse)
library(lubridate)

# Source the predictor functions
source("R/predictor.R")

# Example 1: Basic Safety Assessment
# ----------------------------------
# Get the current safety assessment
prediction <- predict_kayaking_safety()

# Print the current conditions
cat("Current Flow:", prediction$current_conditions$flow_cfs, "cfs\n")
cat("Safety Score:", prediction$current_conditions$safety_score, "/100\n")
cat("Risk Level:", prediction$current_conditions$risk_level, "\n")
cat("Recommendation:", prediction$current_conditions$recommendation, "\n")

# Example 2: Custom Alert Threshold
# --------------------------------
# Set a higher threshold for safety alerts
high_alert <- predict_kayaking_safety(alert_threshold = 70)

# Example 3: Analyzing the Forecast
# --------------------------------
# Extract the forecast data
forecast_data <- prediction$forecast

# Calculate average safety score over the next week
avg_safety <- mean(forecast_data$safety_score)
cat("Average 7-day safety score:", round(avg_safety, 1), "/100\n")

# Find the safest day to paddle
safest_day <- forecast_data %>%
  arrange(desc(safety_score)) %>%
  slice(1)

cat("Safest day to paddle:", safest_day$date, 
    "with a safety score of", safest_day$safety_score, "/100\n")

# Example 4: Visualizing the Forecast
# ----------------------------------
# Create a plot of the forecasted flow
ggplot(forecast_data, aes(x = date, y = predicted_flow, color = risk_level)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "LOW" = "#27ae60",
    "MODERATE" = "#f39c12",
    "HIGH" = "#e74c3c",
    "EXTREME" = "#c0392b"
  )) +
  labs(
    title = "7-Day Flow Forecast for Little Falls",
    subtitle = "With risk level indicators",
    x = "Date",
    y = "Flow (cfs)",
    color = "Risk Level"
  ) +
  theme_minimal()

# Example 5: Safety Metrics for a Specific Flow
# --------------------------------------------
# Calculate safety metrics for a specific flow rate
custom_safety <- calculate_safety_metrics(flow_cfs = 3500, trend_cfs = 200)
print(custom_safety)

# Example 6: Batch Processing Multiple Sites
# -----------------------------------------
# Define a list of USGS site IDs
sites <- c(
  "01646500", # Little Falls
  "01638500", # Point of Rocks
  "01613000"  # Hancock
)

# Process each site
site_results <- lapply(sites, function(site_id) {
  tryCatch({
    # Get data for this site
    site_data <- fetch_realtime_data(site_id = site_id, days_back = 10)
    
    # Calculate safety metrics
    flow <- site_data$discharge_cfs[1]
    safety <- calculate_safety_metrics(flow)
    
    # Return site info
    list(
      site_id = site_id,
      flow = flow,
      safety_score = safety$total_score,
      risk_level = safety$risk_level
    )
  }, error = function(e) {
    list(
      site_id = site_id,
      error = e$message
    )
  })
})

# Convert to a data frame
site_summary <- bind_rows(site_results)
print(site_summary)
```

## Additional Documentation Needs

1. **API Documentation**: If the project is to be used as an API or package, create formal API documentation.

2. **Deployment Guide**: Create a guide for deploying the dashboard to shinyapps.io or a Shiny Server.

3. **Troubleshooting Guide**: Create a guide for common issues and their solutions.

4. **Change Log**: Maintain a change log documenting version changes and updates.

5. **Data Dictionary**: Create a data dictionary explaining all data fields and their meanings.

## Documentation Tools

Consider using the following tools to generate and maintain documentation:

1. **pkgdown**: For creating a documentation website if the project is structured as an R package.

2. **roxygen2**: Continue using for function documentation.

3. **bookdown** or **Quarto**: For creating comprehensive user guides and technical documentation.

4. **GitHub Wiki**: For collaborative documentation that can be updated by contributors.