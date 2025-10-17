# Extract Business Impact Chart from Presentation
library(ggplot2)
library(dplyr)
library(tidyr)

# Business Impact Chart - Extracted from working presentation code
impact <- data.frame(
  metric = c("Rescue Operations", "Tourism Confidence (%)", "Emergency Response (min)"),
  before = c(58, 65, 45),
  after = c(44, 85, 30)
)

impact_long <- impact %>%
  pivot_longer(cols = c(before, after), names_to = "period", values_to = "value")

p_business <- ggplot(impact_long, aes(x = metric, y = value, fill = period)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("after" = "#2ca02c", "before" = "gray60"),
                    labels = c("With Forecast", "Before System")) +
  scale_y_continuous(limits = c(0, 95), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Figure 9: Projected Impact",
       subtitle = "Lower is better for Rescues & Response Time | Higher is better for Confidence",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 15, hjust = 1),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 5, b = 5, l = 5))

# Save with larger dimensions to prevent cutoff
ggsave("business_impact_extracted.png", plot = p_business, 
       width = 14, height = 8, dpi = 300, bg = "white")
ggsave("business_impact_extracted.pdf", plot = p_business, 
       width = 14, height = 8, bg = "white")

print("Business Impact chart saved:")
print("- business_impact_extracted.png")
print("- business_impact_extracted.pdf")