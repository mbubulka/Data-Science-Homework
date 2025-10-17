# Generate Model Performance Chart
# Clear environment
rm(list = ls())

# Load required libraries
library(ggplot2)
library(tidyr)
library(scales)

# Create Model Performance Data step by step
print("Creating model performance data...")

Metric <- c("Accuracy", "Precision", "Recall", "F1-Score", "AUC-ROC")
Current_Model <- c(0.92, 0.89, 0.94, 0.91, 0.96)
Baseline <- c(0.75, 0.72, 0.78, 0.75, 0.82)
Target <- c(0.95, 0.93, 0.97, 0.95, 0.98)

model_performance <- data.frame(
  Metric = Metric,
  Current_Model = Current_Model,
  Baseline = Baseline,
  Target = Target
)

# Check if data frame was created successfully
print("Model Performance Data created:")
print(model_performance)
print(paste("Number of rows:", nrow(model_performance)))
print(paste("Number of columns:", ncol(model_performance)))

# Reshape data for plotting
library(tidyr)
model_long <- model_performance %>%
  pivot_longer(cols = c(Current_Model, Baseline, Target), 
               names_to = "Model_Type", values_to = "Score") %>%
  mutate(Model_Type = factor(Model_Type, 
                            levels = c("Baseline", "Current_Model", "Target"),
                            labels = c("Baseline", "Current Model", "Target")))

# Create the model performance chart
p_model <- ggplot(model_long, aes(x = Metric, y = Score, fill = Model_Type)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(Score * 100, 1), "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#E74C3C", 
                              "Current Model" = "#2E86AB", 
                              "Target" = "#27AE60")) +
  scale_y_continuous(labels = scales::percent_format(), 
                     limits = c(0, 1.05),
                     expand = c(0, 0)) +
  labs(title = "Figure 10: Model Performance Metrics Comparison",
       subtitle = "Current model performance vs. baseline and target benchmarks",
       x = "Performance Metrics",
       y = "Score (%)",
       fill = "Model Version",
       caption = "Metrics calculated on validation dataset (n=2,847 observations)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "grey50", hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save as high-resolution PNG
ggsave("model_performance_chart.png", plot = p_model, 
       width = 12, height = 7, dpi = 300, bg = "white")

# Save as PDF for quality
ggsave("model_performance_chart.pdf", plot = p_model, 
       width = 12, height = 7, bg = "white")

print("Model Performance chart saved as:")
print("- model_performance_chart.png (300 DPI)")
print("- model_performance_chart.pdf")