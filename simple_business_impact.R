# Simple Business Impact Chart Generator
library(ggplot2)

# Create data directly in ggplot
ggplot(data = data.frame(
  Category = c("Safety Incidents Prevented", "Emergency Response Costs Saved", 
               "Insurance Premium Reduction", "Liability Risk Mitigation"),
  Annual_Savings = c(75000, 125000, 45000, 200000)
), aes(x = reorder(Category, Annual_Savings), y = Annual_Savings)) +
  geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0("$", round(Annual_Savings/1000, 0), "K")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0("$", x/1000, "K"), 
                     limits = c(0, 230000),
                     expand = c(0, 0)) +
  labs(title = "Figure 9: Annual Business Impact Analysis",
       subtitle = "Projected cost savings and risk mitigation benefits",
       x = "",
       y = "Annual Savings (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("business_impact_chart.png", width = 12, height = 6, dpi = 300, bg = "white")
ggsave("business_impact_chart.pdf", width = 12, height = 6, bg = "white")

print("Business Impact chart saved successfully!")
print("Files created:")
print("- business_impact_chart.png")
print("- business_impact_chart.pdf")