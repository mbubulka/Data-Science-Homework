# Improved Safety Score Gauge - Simple Version
# Copy and paste this into R Console

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Current safety score
score <- 85

# Create gauge data
gauge_data <- data.frame(
  category = c("Poor\n(0-40)", "Marginal\n(41-60)", "Good\n(61-80)", "Excellent\n(81-100)"),
  value = c(40, 20, 20, 20),
  start_angle = c(0, 40, 60, 80),
  end_angle = c(40, 60, 80, 100),
  color = c("#d62728", "#ff7f0e", "#2ca02c", "#1f77b4"),
  text_color = c("white", "black", "white", "white")
)

# Determine current zone and color
current_zone <- ifelse(score <= 40, "Poor",
                ifelse(score <= 60, "Marginal", 
                ifelse(score <= 80, "Good", "Excellent")))

current_color <- ifelse(score <= 40, "#d62728",
                 ifelse(score <= 60, "#ff7f0e",
                 ifelse(score <= 80, "#2ca02c", "#1f77b4")))

# Create the gauge
gauge_plot <- ggplot(gauge_data) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = start_angle, ymax = end_angle, fill = color),
            color = "white", size = 3) +
  scale_fill_identity() +
  coord_polar(theta = "y", start = 0, direction = 1) +
  xlim(-0.8, 1) +
  
  # Add category labels with better positioning
  geom_text(aes(x = 0.6, y = start_angle + value/2, label = category),
            size = 5, fontface = "bold", color = "black") +
  
  # Large score in center
  annotate("text", x = -0.4, y = 50, label = as.character(score), 
          size = 28, fontface = "bold", color = current_color) +
  
  # "Safety Score" label
  annotate("text", x = -0.1, y = 50, label = "Safety\nScore", 
          size = 6, fontface = "bold", color = "black") +
  
  # Current status
  annotate("text", x = -0.6, y = 15, label = paste("Status:", current_zone), 
          size = 6, fontface = "bold", color = current_color) +
  
  labs(title = "Potomac River Safety Assessment - Little Falls, MD",
       subtitle = "Real-time conditions | Updated every 15 minutes") +
  
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50", margin = margin(b = 15)),
    plot.margin = margin(30, 30, 30, 30)
  )

# Create information panel
info_text <- paste(
  "MONITORED FACTORS:\n",
  "• Water flow rate (CFS)\n",
  "• Water temperature\n", 
  "• Weather conditions\n",
  "• Historical incidents\n",
  "• Seasonal patterns\n\n",
  "SAFETY RANGES:\n",
  "• 81-100: Excellent\n",
  "• 61-80: Good conditions\n",
  "• 41-60: Use caution\n",
  "• 0-40: High risk\n\n",
  "Data: USGS Station 01646500"
)

info_panel <- textGrob(
  info_text,
  x = 0.05, y = 0.95, hjust = 0, vjust = 1,
  gp = gpar(fontsize = 11, fontface = "bold", lineheight = 1.3)
)

# Combine plots
combined_plot <- arrangeGrob(gauge_plot, info_panel, ncol = 2, widths = c(2.2, 1))

# Display the plot
grid.arrange(combined_plot)

# Save the files
ggsave("improved_safety_gauge.png", combined_plot, 
       width = 14, height = 7, dpi = 300, bg = "white")

ggsave("improved_safety_gauge.pdf", combined_plot, 
       width = 14, height = 7, device = "pdf")

cat("✓ Improved safety gauge saved as:\n")
cat("  - improved_safety_gauge.png (high-res PNG)\n")
cat("  - improved_safety_gauge.pdf (vector PDF)\n")