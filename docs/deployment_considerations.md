# Deployment Considerations

## Overview

This document provides recommendations for deploying the Potomac River Kayaking Safety Analysis project to production environments. Proper deployment ensures that the application is reliable, secure, and accessible to users.

## Development-Only Code to Remove

Before deploying to production, the following development-only code should be removed or modified:

### 1. Debug Messages and Print Statements

In `enhanced_potomac_predictor.R`:
- Lines 254-255: Demo execution code at the end of the file
  ```r
  # Run a demo
  demo_result <- enhanced_potomac_predictor(detailed = TRUE)
  ```

- Lines 146-148, 194-207, 209-218: Debug print statements using `cat()`
  - These have been replaced with proper `message()` calls in the refactored version

In `little_falls_dashboard.R`:
- Lines 369-373: Debug launch messages
  ```r
  cat("🎓 LAUNCHING LITTLE FALLS SAFETY ANALYSIS - CLASS PROJECT\n")
  cat("=========================================================\n")
  cat("📊 Real-time safety assessment for Little Falls kayaking\n") 
  cat("🔬 Site-specific calibration with local expertise\n")
  cat("💻 Access dashboard at: http://localhost:3838\n\n")
  ```

### 2. Class Project References

All references to "class project" should be removed for a professional deployment:

- In `enhanced_potomac_predictor.R`:
  - Line 122: `# Class project safety assessment (no external alerts)`
  - Lines 136-140: Class project notes about integration
  - Lines 220-225: Class project applications section

- In `little_falls_dashboard.R`:
  - Line 1: `# Little Falls Safety Dashboard - Class Project Version`
  - Line 2: `# Academic demonstration of real-time hydrological data analysis`
  - Line 17: Title containing "🎓 Little Falls (Potomac) Safety Analysis - Class Project"

### 3. Commented-Out Code

In `potomac_river_analysis.Rmd`:
- Line 39: `# library(kableExtra)  # Removed for Word output compatibility`
  - Either remove this comment or uncomment and use the package if needed

### 4. Hardcoded Configuration

Move hardcoded configuration to environment variables or configuration files:

- USGS site IDs
- Alert thresholds
- Safety scoring parameters
- Dashboard styling

## Deployment Strategy

### 1. Shiny Dashboard Deployment

#### Option A: shinyapps.io

For quick deployment with minimal setup:

1. Create an account on [shinyapps.io](https://www.shinyapps.io/)
2. Install the `rsconnect` package:
   ```r
   install.packages("rsconnect")
   ```
3. Configure your account:
   ```r
   rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
   ```
4. Create a single app.R file combining ui and server:
   ```r
   # app.R
   source("enhanced_potomac_predictor_refactored.R")
   
   # UI and server code from little_falls_dashboard_refactored.R
   
   shinyApp(ui = ui, server = server)
   ```
5. Deploy the application:
   ```r
   rsconnect::deployApp(appName = "potomac-kayaking-safety")
   ```

#### Option B: Shiny Server

For self-hosted deployment with more control:

1. Set up a server with [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/)
2. Create an app directory structure:
   ```
   /srv/shiny-server/potomac-kayaking/
   ├── app.R
   ├── enhanced_potomac_predictor.R
   ├── www/
   │   ├── custom.css
   │   └── images/
   └── data/
   ```
3. Configure Shiny Server in `/etc/shiny-server/shiny-server.conf`
4. Set up proper permissions and restart the server

#### Option C: RStudio Connect

For enterprise deployment with authentication and scheduling:

1. Set up [RStudio Connect](https://www.rstudio.com/products/connect/)
2. Package the application as an R package
3. Use the RStudio IDE to publish directly to Connect
4. Configure authentication and access controls
5. Set up scheduled execution for data updates

### 2. R Package Deployment

For users who want to run the analysis locally:

1. Structure the project as an R package:
   ```
   potomac/
   ├── DESCRIPTION
   ├── NAMESPACE
   ├── R/
   │   ├── predictor.R
   │   ├── dashboard.R
   │   └── utils.R
   ├── man/
   ├── inst/
   │   └── shiny/
   └── vignettes/
   ```

2. Create a DESCRIPTION file:
   ```
   Package: potomac
   Title: Potomac River Kayaking Safety Analysis
   Version: 0.1.0
   Authors@R: person("Your", "Name", email = "your.email@example.com", role = c("aut", "cre"))
   Description: Predictive analytics for kayaking safety on the Potomac River.
   License: MIT + file LICENSE
   Encoding: UTF-8
   LazyData: true
   Roxygen: list(markdown = TRUE)
   RoxygenNote: 7.1.1
   Imports:
       dataRetrieval,
       tidyverse,
       lubridate,
       shiny,
       shinydashboard,
       DT,
       plotly
   Suggests:
       testthat,
       knitr,
       rmarkdown
   ```

3. Build and install the package:
   ```r
   devtools::build()
   devtools::install()
   ```

4. Users can then install from GitHub:
   ```r
   devtools::install_github("username/potomac")
   ```

### 3. API Deployment

To provide programmatic access to the safety predictions:

1. Create a Plumber API:
   ```r
   # plumber.R
   library(plumber)
   source("enhanced_potomac_predictor_refactored.R")
   
   #* @apiTitle Potomac Kayaking Safety API
   #* @apiDescription API for predicting kayaking safety on the Potomac River
   
   #* Get current safety assessment
   #* @get /safety
   function() {
     predict_kayaking_safety(detailed = FALSE)
   }
   
   #* Get safety metrics for a specific flow
   #* @param flow_cfs:numeric Flow rate in cubic feet per second
   #* @param trend_cfs:numeric Weekly trend in flow rate
   #* @get /metrics
   function(flow_cfs, trend_cfs = 0) {
     flow_cfs <- as.numeric(flow_cfs)
     trend_cfs <- as.numeric(trend_cfs)
     calculate_safety_metrics(flow_cfs, trend_cfs)
   }
   ```

2. Deploy the API:
   ```r
   library(plumber)
   pr <- plumb("plumber.R")
   pr$run(port = 8000)
   ```

3. For production, deploy using Docker:
   ```dockerfile
   FROM rocker/r-ver:4.1.0
   
   RUN apt-get update && apt-get install -y \
       libcurl4-openssl-dev \
       libssl-dev
   
   RUN R -e "install.packages(c('plumber', 'dataRetrieval', 'tidyverse', 'lubridate'))"
   
   COPY enhanced_potomac_predictor_refactored.R /app/
   COPY plumber.R /app/
   
   WORKDIR /app
   
   EXPOSE 8000
   
   CMD ["R", "-e", "library(plumber); pr <- plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
   ```

## Dependencies for Deployment

### 1. R Package Dependencies

Ensure all required packages are installed on the deployment server:

```r
install.packages(c(
  "dataRetrieval",  # USGS data access
  "tidyverse",      # Data manipulation and visualization
  "lubridate",      # Date handling
  "shiny",          # Interactive web applications
  "shinydashboard", # Dashboard framework
  "DT",             # Interactive tables
  "plotly",         # Interactive plots
  "forecast",       # Time series forecasting
  "rsconnect"       # For shinyapps.io deployment
))
```

### 2. System Dependencies

Some R packages require system libraries. On Ubuntu/Debian:

```bash
sudo apt-get update
sudo apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgdal-dev \
  libproj-dev \
  libudunits2-dev
```

### 3. Environment Configuration

Create a `.Renviron` file for environment-specific configuration:

```
# .Renviron
USGS_SITE_ID=01646500
ALERT_THRESHOLD=50
CACHE_DIR=./cache
```

Then access these in R:

```r
site_id <- Sys.getenv("USGS_SITE_ID", "01646500")  # Default as fallback
```

### 4. Caching Strategy

Implement caching to reduce API calls to USGS:

```r
get_cached_data <- function(site_id, days_back) {
  cache_file <- file.path("cache", paste0(site_id, "_", Sys.Date(), ".rds"))
  
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  data <- fetch_realtime_data(site_id, days_back)
  
  # Create cache directory if it doesn't exist
  if (!dir.exists("cache")) {
    dir.create("cache", recursive = TRUE)
  }
  
  # Save to cache
  saveRDS(data, cache_file)
  
  return(data)
}
```

## Performance Considerations

### 1. Data Retrieval Optimization

- Implement caching for USGS data to reduce API calls
- Use daily values instead of instant values when historical data is sufficient
- Batch process multiple site requests when possible

### 2. Dashboard Performance

- Use reactive values efficiently in Shiny
- Implement data preprocessing outside of reactive contexts
- Consider using `bindCache()` for expensive operations
- Use `debounce()` or `throttle()` for user inputs that trigger heavy computations

### 3. Scaling Considerations

- For high traffic, consider load balancing with multiple Shiny instances
- Implement a separate API server for data processing
- Use a database for storing historical data instead of API calls

## Monitoring and Maintenance

### 1. Logging

Implement proper logging:

```r
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " [", level, "] ", message)
  
  # Write to log file
  log_file <- file.path("logs", paste0("app_", format(Sys.Date(), "%Y-%m-%d"), ".log"))
  
  # Create logs directory if it doesn't exist
  if (!dir.exists("logs")) {
    dir.create("logs", recursive = TRUE)
  }
  
  write(log_entry, log_file, append = TRUE)
  
  # Also output to console in development
  if (Sys.getenv("R_ENV") == "development") {
    cat(log_entry, "\n")
  }
}
```

### 2. Error Tracking

Implement error tracking and reporting:

```r
# In server.R
options(shiny.error = function() {
  error_details <- geterrmessage()
  log_message(error_details, "ERROR")
  
  # Could also send to an error tracking service
  # send_to_error_service(error_details)
  
  # Show user-friendly message
  showNotification("An error occurred. The development team has been notified.",
                  type = "error", duration = NULL)
})
```

### 3. Usage Analytics

Add Google Analytics or similar:

```r
# In UI
tags$head(
  tags$script(HTML("
    // Google Analytics code
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-XXXXX-Y', 'auto');
    ga('send', 'pageview');
  "))
)
```

## Security Considerations

### 1. Input Validation

Ensure all user inputs are validated:

```r
validate_numeric_input <- function(input, min_value = NULL, max_value = NULL, default = NULL) {
  # Check if input is numeric
  if (!is.numeric(input)) {
    if (is.character(input)) {
      input <- suppressWarnings(as.numeric(input))
    }
    
    if (is.na(input) && !is.null(default)) {
      input <- default
    } else if (is.na(input)) {
      stop("Input must be a numeric value")
    }
  }
  
  # Check min/max constraints
  if (!is.null(min_value) && input < min_value) {
    warning(paste("Input below minimum value of", min_value, "- using minimum"))
    input <- min_value
  }
  
  if (!is.null(max_value) && input > max_value) {
    warning(paste("Input above maximum value of", max_value, "- using maximum"))
    input <- max_value
  }
  
  return(input)
}
```

### 2. Rate Limiting

Implement rate limiting for API endpoints:

```r
# Simple in-memory rate limiter
rate_limiter <- local({
  requests <- list()
  
  function(ip_address, limit = 100, window = 3600) {
    current_time <- as.numeric(Sys.time())
    cutoff_time <- current_time - window
    
    # Clean old requests
    requests <<- Filter(function(x) x > cutoff_time, requests)
    
    # Count requests for this IP
    ip_requests <- sum(names(requests) == ip_address)
    
    if (ip_requests >= limit) {
      return(FALSE)  # Rate limit exceeded
    }
    
    # Add this request
    requests[[length(requests) + 1]] <<- current_time
    names(requests)[length(requests)] <<- ip_address
    
    return(TRUE)  # Request allowed
  }
})
```

### 3. HTTPS

Ensure the application is served over HTTPS:

- For shinyapps.io: This is handled automatically
- For Shiny Server: Configure with a reverse proxy like Nginx + Let's Encrypt
- For RStudio Connect: Configure SSL in the admin settings

## Deployment Checklist

Before deploying to production, ensure:

1. ✅ All development-only code is removed
2. ✅ Error handling is robust and user-friendly
3. ✅ Input validation is implemented for all user inputs
4. ✅ Dependencies are properly documented and installed
5. ✅ Environment-specific configuration is externalized
6. ✅ Performance optimizations are implemented
7. ✅ Logging and monitoring are in place
8. ✅ Security considerations are addressed
9. ✅ Documentation is complete and up-to-date
10. ✅ Testing has been performed in a staging environment