# Weather Data Visualization Script (Sample Version)
# This script loads, samples, and visualizes weather data from a CSV file

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(lubridate)  # For date handling
library(readr)      # For reading CSV files

# ============================================================================
# 1. LOAD DATA
# ============================================================================

cat("Loading weather data...\n")
weather_data <- read_csv("Rproject/weather-project/data/weather.csv")

cat("Original dataset dimensions:", dim(weather_data), "\n")
cat("Column names:", names(weather_data), "\n\n")

# ============================================================================
# 2. SAMPLE AND CLEAN DATA
# ============================================================================

# Sample 10% of the data for faster processing
set.seed(123)  # For reproducibility
weather_sample <- weather_data %>%
  sample_frac(0.1)

cat("Sampled dataset dimensions:", dim(weather_sample), "\n\n")

# Clean the data
weather_clean <- weather_sample %>%
  # Create date column from Year, Month, Day
  mutate(date = make_date(Year, Month, Day)) %>%

  # Rename AvgTemperature to temperature for consistency
  rename(temperature = AvgTemperature) %>%

  # Handle missing values in temperature column
  filter(!is.na(temperature)) %>%

  # Extract month from date for monthly aggregation
  mutate(month_name = month(date, label = TRUE, abbr = FALSE),
         month_num = Month) %>%

  # Sort by date
  arrange(date)

# Display summary of cleaned data
cat("Summary of cleaned data:\n")
summary(weather_clean)
cat("\nMissing values per column:\n")
print(colSums(is.na(weather_clean)))

# ============================================================================
# 3. CREATE VISUALIZATIONS
# ============================================================================

# Ensure the outputs directory exists
if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
  cat("\nCreated 'outputs/' directory\n")
}

# --- Plot 1: Line chart showing temperature over time (sample) ---

cat("\nGenerating temperature over time plot (sample)...\n")

# Further sample for cleaner line plot
weather_plot_sample <- weather_clean %>%
  sample_n(min(10000, nrow(weather_clean)))

temp_plot <- ggplot(weather_plot_sample, aes(x = date, y = temperature)) +
  geom_point(color = "#E74C3C", size = 0.5, alpha = 0.4) +
  geom_smooth(method = "loess", color = "#C0392B", size = 1, se = TRUE) +
  labs(
    title = "Temperature Over Time",
    subtitle = paste0("Sample of ", nrow(weather_plot_sample), " observations"),
    x = "Date",
    y = "Temperature (°F)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Save the temperature plot
ggsave(
  filename = "outputs/temperature_over_time.png",
  plot = temp_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Temperature plot saved to: outputs/temperature_over_time.png\n")

# --- Plot 2: Bar chart showing average temperature per month ---

cat("Generating average temperature per month plot...\n")

# Calculate average temperature per month
temp_monthly <- weather_clean %>%
  group_by(month_name, month_num) %>%
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_num)

# Create bar chart
temp_monthly_plot <- ggplot(temp_monthly, aes(x = month_name, y = avg_temp)) +
  geom_col(fill = "#E67E22", alpha = 0.8) +
  geom_text(
    aes(label = round(avg_temp, 1)),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Average Temperature per Month",
    subtitle = "Monthly temperature averages across all locations",
    x = "Month",
    y = "Average Temperature (°F)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the monthly temperature plot
ggsave(
  filename = "outputs/temperature_per_month.png",
  plot = temp_monthly_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Monthly temperature plot saved to: outputs/temperature_per_month.png\n")

# --- Plot 3: Top 10 Hottest and Coldest Cities ---

cat("Generating top 10 hottest/coldest cities plot...\n")

# Calculate average temperature per city
city_temps <- weather_clean %>%
  filter(!is.na(City), City != "") %>%
  group_by(City, Country) %>%
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 100)  # At least 100 observations for reliability

# Get top 10 hottest and coldest
top_hottest <- city_temps %>%
  arrange(desc(avg_temp)) %>%
  head(10) %>%
  mutate(category = "Hottest",
         city_country = paste0(City, ", ", Country))

top_coldest <- city_temps %>%
  arrange(avg_temp) %>%
  head(10) %>%
  mutate(category = "Coldest",
         city_country = paste0(City, ", ", Country))

# Combine
top_cities <- bind_rows(top_hottest, top_coldest)

# Create plot
cities_plot <- ggplot(top_cities, aes(x = reorder(city_country, avg_temp),
                                       y = avg_temp,
                                       fill = category)) +
  geom_col() +
  geom_text(aes(label = round(avg_temp, 1)),
            hjust = ifelse(top_cities$category == "Coldest", 1.2, -0.2),
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Hottest" = "#E74C3C", "Coldest" = "#3498DB")) +
  labs(
    title = "Top 10 Hottest and Coldest Cities",
    subtitle = "Based on average temperature across all observations",
    x = "City",
    y = "Average Temperature (°F)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = "outputs/top_cities.png",
  plot = cities_plot,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Top cities plot saved to: outputs/top_cities.png\n")

# --- Plot 4: Temperature by Region Comparison ---

cat("Generating temperature by region comparison plot...\n")

# Calculate average temperature per region
region_temps <- weather_clean %>%
  filter(!is.na(Region), Region != "") %>%
  group_by(Region, month_name, month_num) %>%
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_num)

# Create faceted line plot by region
region_plot <- ggplot(region_temps, aes(x = month_name, y = avg_temp,
                                         group = Region, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Temperature Patterns by Region",
    subtitle = "Average monthly temperatures across different world regions",
    x = "Month",
    y = "Average Temperature (°F)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  ) +
  scale_color_brewer(palette = "Set2")

ggsave(
  filename = "outputs/region_comparison.png",
  plot = region_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Region comparison plot saved to: outputs/region_comparison.png\n")

# --- Plot 5: Temperature Trends Over Years ---

cat("Generating temperature trends over years plot...\n")

# Calculate average temperature per year
yearly_temps <- weather_clean %>%
  group_by(Year) %>%
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    min_temp = min(temperature, na.rm = TRUE),
    max_temp = max(temperature, na.rm = TRUE),
    .groups = "drop"
  )

# Create trend plot with confidence interval
trend_plot <- ggplot(yearly_temps, aes(x = Year, y = avg_temp)) +
  geom_ribbon(aes(ymin = min_temp, ymax = max_temp),
              fill = "#E74C3C", alpha = 0.2) +
  geom_line(color = "#E74C3C", size = 1.2) +
  geom_point(color = "#C0392B", size = 2) +
  geom_smooth(method = "lm", color = "#2C3E50", linetype = "dashed",
              se = TRUE, alpha = 0.2) +
  labs(
    title = "Global Temperature Trends Over Time",
    subtitle = "Average yearly temperatures with min/max range and linear trend",
    x = "Year",
    y = "Average Temperature (°F)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/yearly_trends.png",
  plot = trend_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Yearly trends plot saved to: outputs/yearly_trends.png\n")

# Calculate trend statistics
trend_model <- lm(avg_temp ~ Year, data = yearly_temps)
temp_change <- coef(trend_model)[2] * (max(yearly_temps$Year) - min(yearly_temps$Year))

cat("\n--- Temperature Trend Analysis ---\n")
cat(sprintf("Temperature change over period: %.2f°F\n", temp_change))
cat(sprintf("Average change per year: %.3f°F/year\n", coef(trend_model)[2]))
cat("-------------------------------------\n")

# ============================================================================
# 4. COMPLETION MESSAGE
# ============================================================================

cat("\n========================================\n")
cat("Weather data analysis completed!\n")
cat("========================================\n")
cat("Plots saved in the 'outputs/' directory:\n")
cat("  - temperature_over_time.png\n")
cat("  - temperature_per_month.png\n")
cat("  - top_cities.png\n")
cat("  - region_comparison.png\n")
cat("  - yearly_trends.png\n")
cat("========================================\n")
