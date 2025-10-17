# Improved Safety Score Gauge - Much More Readable
# For Potomac River Safety Analysis Presentation

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Create improved safety gauge
create_improved_safety_gauge <- function(score = 85, width = 14, height = 8) {
  
  # Create gauge data with better spacing
  gauge_data <- data.frame(
    category = c("Poor\n(0-40)", "Marginal\n(41-60)", "Good\n(61-80)", "Excellent\n(81-100)"),
    value = c(40, 20, 20, 20),
    start_angle = c(0, 40, 60, 80),
    end_angle = c(40, 60, 80, 100),
    color = c("#d62728", "#ff7f0e", "#2ca02c", "#1f77b4"),
    text_color = c("white", "black", "white", "white")
  )
  
  # Determine current zone
  current_zone <- case_when(
    score <= 40 ~ "Poor",
    score <= 60 ~ "Marginal", 
    score <= 80 ~ "Good",
    TRUE ~ "Excellent"
  )
  
  current_color <- case_when(
    score <= 40 ~ "#d62728",
    score <= 60 ~ "#ff7f0e",
    score <= 80 ~ "#2ca02c", 
    TRUE ~ "#1f77b4"
  )
  
  # Create the main gauge plot
  gauge_plot <- ggplot(gauge_data) +
    # Create gauge segments
    geom_rect(aes(xmin = 0, xmax = 1, ymin = start_angle, ymax = end_angle, fill = color),
              color = "white", size = 2) +
    scale_fill_identity() +
    coord_polar(theta = "y", start = 0, direction = 1) +
    xlim(-0.5, 1) +
    
    # Add category labels
    geom_text(aes(x = 0.5, y = start_angle + value/2, label = category),
              size = 6, fontface = "bold", color = gauge_data$text_color) +
    
    # Add score in center
    annotate("text", x = -0.3, y = 50, label = as.character(score), 
            size = 24, fontface = "bold", color = current_color) +
    
    # Add "Safety Score" label
    annotate("text", x = -0.1, y = 50, label = "Safety\nScore", 
            size = 8, fontface = "bold", color = "black") +
    
    # Add current status indicator
    annotate("text", x = -0.45, y = 20, label = paste("Current Status:", current_zone), 
            size = 7, fontface = "bold", color = current_color) +
    
    labs(title = "Potomac River Safety Assessment",
         subtitle = "Real-time conditions at Little Falls, MD") +
    
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray50", margin = margin(b = 20)),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Create information panel
  info_text <- paste(
    "SAFETY FACTORS MONITORED:\n",
    "• Water flow rate (CFS)\n",
    "• Water temperature\n", 
    "• Weather conditions\n",
    "• Historical incident data\n",
    "• Seasonal risk patterns\n\n",
    "SCORE RANGES:\n",
    "• 81-100: Excellent conditions\n",
    "• 61-80: Good for most users\n",
    "• 41-60: Marginal, use caution\n",
    "• 0-40: Poor, avoid if possible\n\n",
    "Updated every 15 minutes\n",
    "Data from USGS Station 01646500"
  )
  
  info_panel <- textGrob(
    info_text,
    x = 0.05, y = 0.95, hjust = 0, vjust = 1,
    gp = gpar(fontsize = 12, fontface = "bold", lineheight = 1.2)
  )
  
  # Combine gauge and info panel
  combined_plot <- grid.arrange(
    gauge_plot, info_panel, 
    ncol = 2, widths = c(2, 1)
  )
  
  return(combined_plot)
}

# Generate the improved gauge
improved_gauge <- create_improved_safety_gauge(score = 85, width = 14, height = 8)

# Save as high-resolution PNG
ggsave("improved_safety_gauge.png", improved_gauge, 
       width = 14, height = 8, dpi = 300, bg = "white")

# Save as PDF for vector graphics
ggsave("improved_safety_gauge.pdf", improved_gauge, 
       width = 14, height = 8, device = "pdf")

print("✓ Improved safety gauge saved as:")
print("  - improved_safety_gauge.png (high-res PNG)")
print("  - improved_safety_gauge.pdf (vector PDF)")