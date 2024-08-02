library(readxl)
library(ggplot2)
library(dplyr)

# Load data
data <- read_excel("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/poverty_table.xlsx")

# Convert pov_prop to percentage and round to 1 decimal place
data <- data %>%
  group_by(community) %>%
  summarize(pov_prop_percentage = round(mean(pov_prop) * 100, 1))

# Reorder communities by pov_prop_percentage in descending order
data <- data %>%
  arrange(desc(pov_prop_percentage)) %>%
  mutate(community = factor(community, levels = community))

# Custom palette
my_palette <- "#8c0000"

# Create bar plot
soralo_poverty <- ggplot(data, aes(x = community, y = pov_prop_percentage)) +
  geom_bar(stat = "identity", width = 0.85, fill = my_palette, color = "black") +
  labs(title = "Percentage of People in Poverty", x = "Communities", y = "Percentage") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

soralo_poverty

# Save the plot as a PNG file
ggsave("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/SORALO_poverty.png", soralo_poverty, width = 10, height = 6, dpi = 300)
