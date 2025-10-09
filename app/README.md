# Potomac River Safety Analysis Application

This directory contains the core application files for the Little Falls kayaking safety analysis system.

## Files

### Main Application
- `little_falls_dashboard_professional.R` - Production-ready Shiny dashboard with Tufte-inspired design
- `enhanced_potomac_predictor.R` - Core prediction engine with USGS data integration
- `test_final_dashboard.R` - Testing and validation script

## Running the Application

### Prerequisites
Ensure you have the required R packages installed:
```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "DT", "dataRetrieval"))
```

### Launch Dashboard
```r
# From the project root directory:
source("app/little_falls_dashboard_professional.R")
```

### Test Application
```r
# Run validation tests:
source("app/test_final_dashboard.R")
```

## Features

### Dashboard Capabilities
- **Real-time Data**: Live USGS water data integration
- **7-Day Forecasting**: Predictive flow and safety analysis
- **Professional Design**: Tufte-inspired visual principles
- **Interactive Analysis**: Dynamic charts and data tables

### Prediction Engine
- **Multi-factor Scoring**: Flow, trend, seasonal, and experience factors
- **Site-specific Calibration**: Optimized for Little Falls conditions
- **Error Handling**: Robust fallback systems for data availability
- **Academic Standards**: Documented methodology and validation

## Technical Architecture

The application follows a modular design:
1. **Data Layer**: USGS API integration with caching
2. **Analysis Layer**: Statistical modeling and forecasting
3. **Presentation Layer**: Interactive dashboard with clean UX
4. **Validation Layer**: Testing and quality assurance

This structure enables both standalone operation and integration into larger systems.