# Version Control Recommendations

## Overview

This document provides recommendations for version control best practices for the Potomac River Kayaking Safety Analysis project. Following these guidelines will help ensure that the repository is clean, secure, and properly structured for collaboration and deployment.

## Hardcoded Credentials and Sensitive Data

The following instances of potentially sensitive data were identified:

1. **No API keys or credentials found** - The project currently uses public USGS data that doesn't require authentication.

2. **Fallback site information in `potomac_river_analysis.Rmd`** - While not sensitive, the hardcoded fallback site information should be moved to a configuration file:

```r
# Fallback site information
data.frame(
  station_nm = "POTOMAC RIVER NEAR WASH, DC LITTLE FALLS PUMP STA",
  dec_lat_va = 38.9498,
  dec_long_va = -77.1276,
  drain_area_va = 11560,
  stringsAsFactors = FALSE
)
```

## Recommended .gitignore File

Create a `.gitignore` file in the project root with the following contents:

```
# R specific
.Rproj.user/
.Rhistory
.RData
.Ruserdata
*.Rproj
.Renviron
.Rprofile

# Package development files
*.tar.gz
*.Rcheck/

# Output files
*.html
*.pdf
*.docx
*.pptx
*.tex
*.log
*.aux
*.out
*.toc

# Data files (if large)
*.csv
*.rds
*.rda
*.rdata
*.feather
*.parquet
*.sqlite
*.db

# Temporary files
*.tmp
*~
.DS_Store
Thumbs.db

# Shiny deployment
rsconnect/

# Environment configuration
config.yml
.env

# Logs
logs/
*.log

# Cached data
cache/
```

## Code That Shouldn't Be Committed

The following code elements should be removed or modified before committing:

1. **Development-only debugging statements** in `enhanced_potomac_predictor.R`:
   - Remove or comment out the demo execution at the end of the file (lines 254-255)
   - These have been removed in the refactored version

2. **Class project references** in both R files:
   - References to "class project" in comments and UI elements
   - These have been removed in the refactored versions

3. **Commented-out code** in `potomac_river_analysis.Rmd`:
   - Line 39: `# library(kableExtra)  # Removed for Word output compatibility`
   - Consider removing or explaining why it's commented out

## Directory Structure Improvements

Consider reorganizing the project with the following structure:

```
potomac-kayaking-safety/
├── R/                      # R functions
│   ├── predictor.R         # Core prediction functions
│   ├── dashboard.R         # Dashboard UI and server logic
│   └── utils.R             # Utility functions
├── analysis/               # Analysis documents
│   └── potomac_river_analysis.Rmd
├── data/                   # Data directory (if needed)
│   └── .gitkeep
├── inst/                   # Installed files
│   └── shiny/              # Shiny app files
├── man/                    # Documentation
├── tests/                  # Unit tests
├── .gitignore              # Git ignore file
├── DESCRIPTION             # Package description
├── LICENSE                 # License file
├── README.md               # Project readme
└── potomac-kayaking.Rproj  # RStudio project file
```

## Commit Message Guidelines

Use structured commit messages following this format:

```
<type>(<scope>): <subject>

<body>

<footer>
```

Where:
- **type**: feat, fix, docs, style, refactor, test, chore
- **scope**: predictor, dashboard, analysis, etc.
- **subject**: Short description of the change
- **body**: Detailed description
- **footer**: Breaking changes, references to issues

Examples:
- `feat(predictor): add water temperature to safety calculation`
- `fix(dashboard): correct error in trend calculation`
- `docs(readme): update installation instructions`

## Branching Strategy

Implement a simple branching strategy:

1. `main` - Stable production code
2. `develop` - Integration branch for features
3. Feature branches - `feature/feature-name`
4. Bugfix branches - `bugfix/issue-description`

## Pre-commit Checks

Before committing, ensure:

1. All R code passes R CMD check without warnings
2. No debugging print statements remain
3. No hardcoded credentials are included
4. All documentation is up to date
5. Code is properly formatted according to the style guide