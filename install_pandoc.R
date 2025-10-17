# Install Pandoc using R
# Run this script if you want to install Pandoc without RStudio

# Install installr package if not already installed
if(!require(installr, character.only = TRUE)) {
  install.packages("installr")
  library(installr)
}

# Install Pandoc
cat("Installing Pandoc...\n")
installr::install.pandoc()

cat("Pandoc installation complete!\n")
cat("You can now render your presentation with:\n")
cat("rmarkdown::render('potomac_presentation_slides.Rmd')\n")