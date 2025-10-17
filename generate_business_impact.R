# Generate Business Impact Chart
# Clear environment
rm(list = ls())

# Load required libraries
library(ggplot2)
library(scales)

# Create Business Impact Data step by step
print("Creating business impact data...")

Category <- c("Safety Incidents Prevented", "Emergency Response Costs Saved", 
              "Insurance Premium Reduction", "Liability Risk Mitigation")
Annual_Savings <- c(75000, 125000, 45000, 200000)

business_impact <- data.frame(
  Category = Category,
  Annual_Savings = Annual_Savings
)

# Check if data frame was created successfully
print("Business Impact Data created:")
print(business_impact)
print(paste("Number of rows:", nrow(business_impact)))
print(paste("Number of columns:", ncol(business_impact)))

# Create the business impact chart
print("Creating business impact chart...")

tryCatch({
  p_business <- ggplot(business_impact, aes(x = reorder(Category, Annual_Savings), y = Annual_Savings)) +
    geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0("$", format(Annual_Savings/1000, nsmall = 0), "K")), 
              hjust = -0.1, size = 4, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K"), 
                       limits = c(0, max(business_impact$Annual_Savings) * 1.15),
                       expand = c(0, 0)) +
    labs(title = "Figure 9: Annual Business Impact Analysis",
         subtitle = "Projected cost savings and risk mitigation benefits",
         x = "",
         y = "Annual Savings (USD)",
         caption = "Data represents projected annual benefits from safety monitoring implementation") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 9, color = "grey50", hjust = 0),
      plot.margin = margin(20, 40, 20, 20)
    )
  
  print("Chart created successfully!")
  
}, error = function(e) {
  print(paste("Error creating chart:", e$message))
  stop(e)
})

# Save the charts
print("Saving charts...")

tryCatch({
  # Save as high-resolution PNG
  ggsave("business_impact_chart.png", plot = p_business, 
         width = 12, height = 6, dpi = 300, bg = "white")
  
  # Save as PDF for quality
  ggsave("business_impact_chart.pdf", plot = p_business, 
         width = 12, height = 6, bg = "white")
  
  print("Business Impact chart saved successfully:")
  print("- business_impact_chart.png (300 DPI)")
  print("- business_impact_chart.pdf")
  
}, error = function(e) {
  print(paste("Error saving charts:", e$message))
  stop(e)
})