# Knit Potomac Presentation Script
# Run this in R Console to generate the HTML presentation

# Load required packages
if (!require("xaringan")) install.packages("xaringan")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("scales")) install.packages("scales")
if (!require("lubridate")) install.packages("lubridate")

library(xaringan)
library(rmarkdown)

# Set working directory
setwd("d:/R projects/week 8")

# Render the presentation
rmarkdown::render("potomac_presentation_slides.Rmd")

# Print success message
cat("✓ Presentation rendered successfully!\n")
cat("✓ Open 'potomac_presentation_slides.html' in your browser\n")
cat("✓ Use arrow keys or space bar to navigate slides\n")
cat("✓ Press 'f' for fullscreen mode\n")
cat("✓ Press 'p' for presenter mode\n")