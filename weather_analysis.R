# Weather Data Visualization Script
# This script loads, cleans, and visualizes weather data from a CSV file

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(lubridate)  # For date handling
library(readr)      # For reading CSV files

# ============================================================================
# 1. LOAD DATA
# ============================================================================

# Read the weather data from CSV file
weather_data <- read_csv("Rproject/weather-project/data/weather.csv")

# Display basic information about the dataset
cat("Dataset dimensions:", dim(weather_data), "\n")
cat("Column names:", names(weather_data), "\n\n")

# ============================================================================
# 2. CLEAN AND PREPROCESS DATA
# ============================================================================

# Clean the data
weather_clean <- weather_data %>%
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

# --- Plot 1: Line chart showing temperature over time ---

cat("\nGenerating temperature over time plot...\n")

temp_plot <- ggplot(weather_clean, aes(x = date, y = temperature)) +
  geom_line(color = "#E74C3C", size = 1) +  # Red line for temperature
  geom_point(color = "#C0392B", size = 1.5, alpha = 0.6) +  # Add points
  labs(
    title = "Temperature Over Time",
    subtitle = "Daily temperature measurements",
    x = "Date",
    y = "Temperature (°C)"
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
  geom_col(fill = "#E67E22", alpha = 0.8) +  # Orange bars
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

# ============================================================================
# 4. COMPLETION MESSAGE
# ============================================================================

cat("\n========================================\n")
cat("Weather data analysis completed!\n")
cat("========================================\n")
cat("Plots saved in the 'outputs/' directory:\n")
cat("  - temperature_over_time.png\n")
cat("  - temperature_per_month.png\n")
cat("========================================\n")
