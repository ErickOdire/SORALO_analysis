library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load population data
pop_data <- read_excel("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/pop_table.xlsx")

# Load poverty data
poverty_data <- read_excel("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/poverty_table.xlsx")

# Summarize population data
pop_data <- pop_data %>%
  group_by(community) %>%
  summarize(population = round(sum(population)))

# Summarize poverty data
poverty_data <- poverty_data %>%
  group_by(community) %>%
  summarize(pov_prop_percentage = round(mean(pov_prop) * 100, 1))

# Merge datasets
data <- merge(pop_data, poverty_data, by = "community")

# Calculate the number of people in and out of poverty
data <- data %>%
  mutate(in_poverty = round(population * (pov_prop_percentage / 100)),
         not_in_poverty = population - in_poverty)

# Melt the data for plotting
data_melt <- data %>%
  select(community, in_poverty, not_in_poverty) %>%
  pivot_longer(cols = c(in_poverty, not_in_poverty), names_to = "Status", values_to = "count")

# Reorder communities by total population in descending order
data_melt <- data_melt %>%
  mutate(community = factor(community, levels = data$community[order(-data$population)]))

# Custom palette
my_palette <- c("in_poverty" = "#8c0000", "not_in_poverty" = "#e4cea5")

# Create stacked bar plot
soralo_pop_poverty <- ggplot(data_melt, aes(x = community, y = count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.85, color = "black") +
  scale_fill_manual(values = my_palette, labels = c("In Poverty", "Not in Poverty")) +
  labs(title = "SORALO Communities Population and Poverty Status", x = "Communities", y = "Total Population") +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

soralo_pop_poverty

# Save the plot as a PNG file
ggsave("E:/Work/DASCOT/PROJECTS/SORALO/Poverty_Population/SORALO_population_poverty.png", soralo_pop_poverty, width = 10, height = 6, dpi = 300)
