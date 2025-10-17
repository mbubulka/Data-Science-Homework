# Check and prepare presentation for rendering
# This script will help identify any issues and install required packages

# Install required packages if not already installed
required_packages <- c("rmarkdown", "knitr", "ggplot2", "dplyr", "gridExtra", "grid", "lubridate", "scales")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Check if pandoc is available
cat("Checking Pandoc availability...\n")
if(rmarkdown::pandoc_available()) {
  cat("Pandoc is available: version", rmarkdown::pandoc_version(), "\n")
} else {
  cat("Pandoc is NOT available. You need to install Pandoc to render to PowerPoint.\n")
  cat("You can:\n")
  cat("1. Install RStudio (includes Pandoc)\n")
  cat("2. Install Pandoc separately from https://pandoc.org/installing.html\n")
  cat("3. Use the installr package: installr::install.pandoc()\n")
}

# Check R Markdown file for syntax issues
cat("\nChecking R Markdown file...\n")
file_path <- "potomac_presentation_slides.Rmd"

if(file.exists(file_path)) {
  # Read the file and check for common issues
  content <- readLines(file_path)
  
  # Check for balanced code chunks
  chunk_starts <- grep("^```\\{r", content)
  chunk_ends <- grep("^```$", content)
  
  cat("Found", length(chunk_starts), "R code chunk starts\n")
  cat("Found", length(chunk_ends), "code chunk ends\n")
  
  if(length(chunk_starts) != length(chunk_ends)) {
    cat("WARNING: Unbalanced code chunks detected!\n")
  } else {
    cat("Code chunks appear balanced.\n")
  }
  
  # Check for YAML header
  if(content[1] == "---") {
    yaml_end <- which(content == "---")[2]
    if(!is.na(yaml_end)) {
      cat("YAML header found (lines 1-", yaml_end, ")\n")
    }
  }
  
  cat("File appears to be properly formatted.\n")
  
} else {
  cat("ERROR: File", file_path, "not found!\n")
}

cat("\nTo render the presentation, you need:\n")
cat("1. RStudio installed (recommended) OR\n")
cat("2. Pandoc installed separately\n")
cat("3. All required R packages installed\n")
cat("\nOnce you have these, you can render with:\n")
cat("rmarkdown::render('potomac_presentation_slides.Rmd')\n")