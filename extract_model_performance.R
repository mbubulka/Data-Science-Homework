# Extract Model Performance Chart from Presentation
library(ggplot2)

# Model Performance Chart - Extracted from working presentation code
accuracy_data <- data.frame(
  horizon = c("24h", "48h", "72h", "5 days", "7 days"),
  accuracy = c(95, 92, 88, 85, 82),
  horizon_order = 1:5
)

p_model <- ggplot(accuracy_data, aes(x = reorder(horizon, horizon_order), y = accuracy)) +
  geom_col(fill = "#1f77b4", width = 0.6) +
  geom_text(aes(label = paste0(accuracy, "%")), vjust = -0.5, size = 7, fontface = "bold") +
  geom_hline(yintercept = 80, linetype = "dashed", color = "#d62728", size = 1) +
  annotate("text", x = 4.5, y = 82, label = "Target: 80%", color = "#d62728", size = 5) +
  labs(title = "Figure 10: Forecast Accuracy by Time Horizon",
       subtitle = "ARIMA model performance | Exceeds target across all horizons",
       x = "Forecast Horizon", y = "Directional Accuracy (%)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12))

# Save with larger dimensions to prevent cutoff
ggsave("model_performance_extracted.png", plot = p_model, 
       width = 14, height = 8, dpi = 300, bg = "white")
ggsave("model_performance_extracted.pdf", plot = p_model, 
       width = 14, height = 8, bg = "white")

print("Model Performance chart saved:")
print("- model_performance_extracted.png")
print("- model_performance_extracted.pdf")