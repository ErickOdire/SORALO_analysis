library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# Load data
data <- read_excel("E:/Work/DASCOT/PROJECTS/SORALO/Tourism/lodges_table.xlsx")

# Round population values to the nearest whole number and summarize data
data <- data %>%
  group_by(community) %>%
  summarize(no_of_lodges = round(sum(no_of_lodges)))

# Reorder communities by population in descending order
data <- data %>%
  arrange(desc(no_of_lodges)) %>%
  mutate(community = factor(community, levels = community))

# Custom palette
my_palette <- "#8c0000"

# Create bar plot
soralo_lodges <- ggplot(data, aes(x = community, y = no_of_lodges)) +
  geom_bar(stat = "identity", width = 0.85, fill = my_palette, color = "black") +
  labs(title = "SORALO Tourist Facilities", x = "Communities", y = "Total Tourist Facilities") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3), labels = scales::number_format(accuracy = 1))

soralo_lodges

# Save the plot as a PNG file
ggsave("E:/Work/DASCOT/PROJECTS/SORALO/Tourism/SORALO_tourist_facilities.png", soralo_lodges, width = 10, height = 6, dpi = 300)
