# Load necessary packages
library(tidyverse)
library(xts)
library(dygraphs)
library(openmeteo)

# 1. Get daily weather data for Buffalo
# Define the geographic coordinates of Buffalo
buffalo_coords <- c(43.00923265935055, -78.78494250958327)

# Get daily weather data from January 1, 2023 to today
weather_data <- weather_history(
  location = buffalo_coords,
  start = "2023-01-01",
  end = Sys.Date(),
  daily = c("temperature_2m_max", "temperature_2m_min", "precipitation_sum")
)

# Calculate the daily average temperature
weather_data <- weather_data %>%
  mutate(daily_temperature_2m_mean = (daily_temperature_2m_max + daily_temperature_2m_min) / 2)

# 2. Convert time series data to xts format
temp_xts <- xts(
  weather_data %>% select(daily_temperature_2m_max, daily_temperature_2m_mean),
  order.by = as.Date(weather_data$date)
)
precip_xts <- xts(
  weather_data %>% select(daily_precipitation_sum),
  order.by = as.Date(weather_data$date)
)

# 3. Create a Dygraph chart
# Temperature chart
temp_dygraph <- dygraph(temp_xts, main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dySeries("daily_temperature_2m_max", label = "Max Temp (°C)") %>%
  dySeries("daily_temperature_2m_mean", label = "Mean Temp (°C)") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", as.character(Sys.Date())))

# Precipitation chart
precip_dygraph <- dygraph(precip_xts, main = "Daily Precipitation in Buffalo, NY") %>%
  dySeries("daily_precipitation_sum", label = "Precipitation (mm)") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", as.character(Sys.Date())))

# 4. Save as HTML file
library(htmlwidgets)
saveWidget(temp_dygraph, "buffalo_temperature.html")
saveWidget(precip_dygraph, "buffalo_precipitation.html")

# Display the chart
print(temp_dygraph)
print(precip_dygraph)
