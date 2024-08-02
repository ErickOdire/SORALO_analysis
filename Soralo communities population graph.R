library(readxl)
library(ggplot2)
library(dplyr)

# Load data
data <- read_excel("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/pop_table.xlsx")

# Round population values to the nearest whole number and summarize data
data <- data %>%
  group_by(community) %>%
  summarize(population = round(sum(population)))

# Reorder communities by population in descending order
data <- data %>%
  arrange(desc(population)) %>%
  mutate(community = factor(community, levels = community))

# Custom palette
my_palette <- "#8c0000"

# Create bar plot
soralo_pop <- ggplot(data, aes(x = community, y = population)) +
  geom_bar(stat = "identity", width = 0.85, fill = my_palette, color = "black") +
  labs(title = "SORALO Communities Population", x = "Communities", y = "Total Population") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

soralo_pop

# Save the plot as a PNG file
ggsave("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/SORALO_population.png", soralo_pop, width = 10, height = 6, dpi = 300)
