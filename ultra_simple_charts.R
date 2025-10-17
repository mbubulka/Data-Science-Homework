# Ultra Simple Chart Generator - No intermediate variables
library(ggplot2)

# Business Impact Chart
print("Creating Business Impact Chart...")

p1 <- ggplot() +
  geom_col(data = NULL, 
           aes(x = c("Safety Incidents\nPrevented", "Emergency Response\nCosts Saved", 
                     "Insurance Premium\nReduction", "Liability Risk\nMitigation"),
               y = c(75000, 125000, 45000, 200000)),
           fill = "#2E86AB", alpha = 0.8, width = 0.7) +
  geom_text(data = NULL,
            aes(x = c("Safety Incidents\nPrevented", "Emergency Response\nCosts Saved", 
                      "Insurance Premium\nReduction", "Liability Risk\nMitigation"),
                y = c(75000, 125000, 45000, 200000),
                label = c("$75K", "$125K", "$45K", "$200K")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(title = "Figure 9: Annual Business Impact Analysis",
       x = "", y = "Annual Savings (USD)") +
  theme_minimal()

print("Saving Business Impact Chart...")
ggsave("business_impact_simple.png", plot = p1, width = 12, height = 6, dpi = 300)

print("Creating Model Performance Chart...")

# Model Performance Chart  
p2 <- ggplot() +
  geom_col(data = NULL,
           aes(x = rep(c("Accuracy", "Precision", "Recall", "F1-Score", "AUC-ROC"), 3),
               y = c(0.92, 0.89, 0.94, 0.91, 0.96,  # Current
                     0.75, 0.72, 0.78, 0.75, 0.82,  # Baseline
                     0.95, 0.93, 0.97, 0.95, 0.98), # Target
               fill = rep(c("Current Model", "Baseline", "Target"), each = 5)),
           position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Baseline" = "#E74C3C", 
                              "Current Model" = "#2E86AB", 
                              "Target" = "#27AE60")) +
  labs(title = "Figure 10: Model Performance Metrics Comparison",
       x = "Metrics", y = "Score", fill = "Model") +
  theme_minimal()

print("Saving Model Performance Chart...")
ggsave("model_performance_simple.png", plot = p2, width = 12, height = 7, dpi = 300)

print("Done! Files created:")
print("- business_impact_simple.png")
print("- model_performance_simple.png")