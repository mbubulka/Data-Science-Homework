# Potomac River Kayaking Safety Analysis & Predictive Dashboard

[![R-CMD-check](https://github.com/username/potomac/workflows/R-CMD-check/badge.svg)](https://github.com/username/potomac/actions)
[![test-coverage](https://github.com/username/potomac/workflows/test-coverage/badge.svg)](https://github.com/username/potomac/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This project provides **predictive analytics for kayaking safety** on the Potomac River, specifically calibrated for Little Falls conditions. Unlike existing solutions that only show current conditions, this system offers **7-day forecasting** to help kayakers plan safe trips in advance.

## Key Features

### 🔮 Predictive Analytics
- **7-day flow forecasting** using ARIMA modeling
- **Weather-integrated predictions** combining flow and atmospheric conditions
- **Proactive safety planning** vs. reactive current-condition monitoring

### 🎯 Site-Specific Calibration
- **Little Falls-optimized** safety thresholds based on local paddling expertise
- **Multi-factor risk assessment** considering flow, temperature, and seasonal patterns
- **Experience-level recommendations** for beginners through advanced paddlers

### 📊 Interactive Dashboard
- **Real-time monitoring** with live USGS data integration
- **Visual safety indicators** with color-coded risk levels
- **Historical trend analysis** for pattern recognition
- **Tufte-inspired design** maximizing data-ink ratio and visual clarity

## Unique Value Proposition

**Current Market Gap:** USGS and other services only provide current river conditions, requiring kayakers to make same-day decisions based on present data.

**Our Solution:** Predictive intelligence that enables advance trip planning with 7-day safety forecasts, reducing cancellations and improving safety outcomes.

## Project Structure

This project is organized as a professional R package with clear separation of concerns:

```
potomac-river-analysis/
├── 📱 app/                    # Main application files
│   ├── little_falls_dashboard_professional.R
│   ├── enhanced_potomac_predictor.R
│   └── test_final_dashboard.R
├── 📊 analysis/               # Research and analysis
│   └── potomac_river_analysis.Rmd
├── 📁 archive/                # Development history
├── 🏗️  R/                     # Package functions
├── 📚 docs/                   # Documentation
├── 🧪 tests/                  # Unit tests
├── 📈 figures/                # Generated plots
├── 💾 data/                   # Package data
├── 🔧 inst/                   # Installed files
├── 👥 man/                    # Function documentation
└── ⚙️  .github/               # CI/CD workflows
```

### R Functions
- `R/predictor.R` - Core prediction functions
- `R/dashboard.R` - Dashboard UI and server logic
- `R/potomac-package.R` - Package documentation

### Core Application
- `app/little_falls_dashboard_professional.R` - Production-ready Tufte-inspired dashboard
- `app/enhanced_potomac_predictor.R` - Core prediction engine with USGS data integration
- `app/test_final_dashboard.R` - Testing script for dashboard validation

### Analysis & Research
- `analysis/potomac_river_analysis.Rmd` - Comprehensive watershed analysis and modeling

### Documentation
- `docs/` - Project documentation and design analysis
- `man/` - Function documentation (generated from roxygen comments)
- `inst/` - Installed files (including APA7 citation formatting and bibliography)
- `README.md` - Project overview and usage instructions
- `CONTRIBUTING.md` - Guidelines for contributing to the project
- `LICENSE` - MIT license

### Testing and CI
- `tests/` - Unit tests
- `.github/workflows/` - Continuous integration workflows

### Data
- `data/` - Package data
- `figures/` - Generated visualizations and analysis plots

### Development
- `.gitignore` - Files to exclude from version control
- `DESCRIPTION` - Package metadata
- `NAMESPACE` - Package exports and imports
- `renv/` - Dependency management
- `renv.lock` - Locked dependencies
- `archive/` - Previous dashboard versions and development iterations

## Technical Implementation

### Data Sources
- **USGS Water Services API** - Real-time and historical flow data
- **National Weather Service** - Meteorological forecasting
- **Local Calibration Data** - Site-specific safety thresholds

### Technologies
- **R Markdown** - Comprehensive analysis framework
- **Shiny Dashboard** - Interactive web application
- **ggplot2** - Statistical graphics following grammar of graphics principles
- **ARIMA Modeling** - Time series forecasting
- **tidyverse** - Data manipulation and visualization

### Design Philosophy
The dashboard follows **Edward Tufte's principles** of excellent statistical graphics:
- **Maximized data-ink ratio** - Every visual element represents data
- **Eliminated chartjunk** - No unnecessary decorative elements
- **Clear visual hierarchy** - Primary, secondary, and tertiary information levels
- **Accurate representation** - Visual elements proportional to data values
- **Professional typography** - Clean, readable font choices and spacing

### Safety Algorithm
The system uses a multi-factor scoring approach:
- **Flow Rate Analysis** (40% weight) - Site-specific optimal ranges
- **Temperature Considerations** (25% weight) - Hypothermia risk assessment
- **Seasonal Patterns** (20% weight) - Historical trend analysis
- **Weather Integration** (15% weight) - Precipitation and wind factors

## Little Falls Calibration

Based on local paddling expertise, the system has been calibrated specifically for Little Falls conditions:

- **Optimal Range:** 800-1,500 cfs (Good conditions for most skill levels)
- **Beginner Friendly:** 1,000-1,200 cfs (Stable, forgiving conditions)
- **Advanced Conditions:** 1,500-2,500 cfs (Technical challenges, experienced paddlers)
- **High Risk:** >2,500 cfs or <600 cfs (Dangerous conditions)

## Installation

### From GitHub
```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package from GitHub
devtools::install_github("username/potomac")
```

### From Source
```r
# Clone the repository
git clone https://github.com/username/potomac.git
cd potomac

# Install dependencies using renv
install.packages("renv")
renv::restore()

# Build and install the package
devtools::install()
```

## Usage

### Loading the Package
```r
# Load the package
library(potomac)
```

### Running the Analysis
```r
# Render the full analysis
rmarkdown::render(system.file("analysis/potomac_river_analysis.Rmd", package = "potomac"))
```

### Launching the Dashboard
```r
# Run the professional Tufte-inspired dashboard
source("app/little_falls_dashboard_professional.R")

# Or use the package function (if installed)
run_dashboard()
```

### Using Prediction Functions
```r
# Get current conditions and 7-day forecast
forecast_data <- predict_kayaking_safety()

# Calculate safety metrics for a specific flow
safety_metrics <- calculate_safety_metrics(flow_cfs = 2000, trend_cfs = 0)
```

## Development

### Setting Up Development Environment
```r
# Clone the repository
git clone https://github.com/username/potomac.git
cd potomac

# Install dependencies
install.packages("renv")
renv::restore()

# Load all functions for development
devtools::load_all()
```

### Running Tests
```r
# Run tests
devtools::test()

# Check package
devtools::check()
```

## Research & Validation

This analysis incorporates:
- **15+ years of historical USGS data** for pattern recognition
- **Peer-reviewed research** on river safety and hydrology
- **Local paddling expertise** for site-specific calibration
- **APA7 academic standards** for methodology and citation

## Future Enhancements

- **Mobile app integration** for field use
- **Multiple site expansion** beyond Little Falls
- **Machine learning** for improved prediction accuracy
- **Community feedback integration** for crowd-sourced validation

## Contributing

We welcome contributions to improve the Potomac River Kayaking Safety Analysis project! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines on how to contribute.

### Quick Start for Contributors

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes following the [tidyverse style guide](https://style.tidyverse.org/)
4. Add tests for your changes
5. Run tests and checks (`devtools::test()` and `devtools::check()`)
6. Commit your changes (`git commit -m "feat: add amazing feature"`)
7. Push to your branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

### Code of Conduct

This project is committed to providing a welcoming and harassment-free experience for everyone. We expect all participants to adhere to our code of conduct.

## Version Control

This project uses Git for version control with the following branching strategy:

- `main` - Stable production code
- `develop` - Integration branch for features
- Feature branches - `feature/feature-name`
- Bugfix branches - `bugfix/issue-description`

### Commit Message Format

We follow a structured commit message format:

```
<type>(<scope>): <subject>
```

Where:
- **type**: feat, fix, docs, style, refactor, test, chore
- **scope**: predictor, dashboard, analysis, etc.
- **subject**: Short description of the change

Examples:
- `feat(predictor): add water temperature to safety calculation`
- `fix(dashboard): correct error in trend calculation`
- `docs(readme): update installation instructions`

## References

### Data Sources & Methodology
U.S. Geological Survey. (n.d.). *National Water Information System: Web interface*. https://waterdata.usgs.gov/nwis

Hirsch, R. M., & De Cicco, L. A. (2015). *User guide to the EGRET R package*. U.S. Geological Survey. https://pubs.usgs.gov/tm/04/a10/

De Cicco, L. A., Hirsch, R. M., Lorenz, D., & Watkins, W. D. (2023). *dataRetrieval: R packages for discovering and retrieving water data*. https://cran.r-project.org/package=dataRetrieval

### Visualization Design Principles
Tufte, E. R. (2001). *The visual display of quantitative information* (2nd ed.). Graphics Press.

Tufte, E. R. (1990). *Envisioning information*. Graphics Press.

Tufte, E. R. (1997). *Visual explanations: Images and quantities, evidence and narrative*. Graphics Press.

Few, S. (2012). *Show me the numbers: Designing tables and graphs to enlighten* (2nd ed.). Analytics Press.

Wickham, H. (2016). *ggplot2: Elegant graphics for data analysis*. Springer-Verlag.

## Contact & Integration

This project was developed as part of advanced analytics coursework and is available for integration into bubulkaanalytics.com portfolio.

**Academic Context:** Graduate-level statistical analysis and predictive modeling  
**Commercial Application:** River recreation safety and trip planning services  
**Technical Demonstration:** R-based predictive analytics and dashboard development

---

*Developed by Bubulka Analytics - October 2025*  
*Licensed under the MIT License*