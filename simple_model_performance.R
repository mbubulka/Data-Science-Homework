# Simple Model Performance Chart Generator
library(ggplot2)
library(tidyr)

# Create and reshape data directly
model_data <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC-ROC"),
  Current_Model = c(0.92, 0.89, 0.94, 0.91, 0.96),
  Baseline = c(0.75, 0.72, 0.78, 0.75, 0.82),
  Target = c(0.95, 0.93, 0.97, 0.95, 0.98)
)

model_long <- pivot_longer(model_data, 
                          cols = c(Current_Model, Baseline, Target), 
                          names_to = "Model_Type", 
                          values_to = "Score")

model_long$Model_Type <- factor(model_long$Model_Type, 
                               levels = c("Baseline", "Current_Model", "Target"),
                               labels = c("Baseline", "Current Model", "Target"))

# Create the plot
ggplot(model_long, aes(x = Metric, y = Score, fill = Model_Type)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(Score * 100, 1), "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#E74C3C", 
                              "Current Model" = "#2E86AB", 
                              "Target" = "#27AE60")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), 
                     limits = c(0, 1.05),
                     expand = c(0, 0)) +
  labs(title = "Figure 10: Model Performance Metrics Comparison",
       subtitle = "Current model performance vs. baseline and target benchmarks",
       x = "Performance Metrics",
       y = "Score (%)",
       fill = "Model Version") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("model_performance_chart.png", width = 12, height = 7, dpi = 300, bg = "white")
ggsave("model_performance_chart.pdf", width = 12, height = 7, bg = "white")

print("Model Performance chart saved successfully!")
print("Files created:")
print("- model_performance_chart.png")
print("- model_performance_chart.pdf")