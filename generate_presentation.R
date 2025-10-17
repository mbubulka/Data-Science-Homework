#!/usr/bin/env R

# Set working directory
setwd("d:/R projects/week 8")

# Generate PowerPoint presentation
rmarkdown::render('potomac_presentation.Rmd', 
                 output_format='powerpoint_presentation',
                 output_file='potomac_presentation_WITH_VALIDATION.pptx')

cat("✅ Presentation generated successfully!\n")
cat("Output file: potomac_presentation_WITH_VALIDATION.pptx\n")
