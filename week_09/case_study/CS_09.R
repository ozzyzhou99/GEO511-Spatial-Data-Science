# Load required libraries
library(sf)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(spData)
library(ggplot2)

# Download CSV file and read
url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"
storm_data <- read_csv(url)

# Extract the year column and filter data after 1950
storm_data <- storm_data %>% 
  mutate(year = year(ISO_TIME)) %>%  # Extract the year from ISO_TIME using lubridate
  filter(year >= 1950)                # Filter out data from 1950 and later

# Convert -999.0 values to NA to indicate missing values
storm_data <- storm_data %>% 
  mutate_if(is.numeric, ~ ifelse(. == -999.0, NA, .))

# Add a year column for faceted plotting
storm_data <- storm_data %>% 
  mutate(decade = floor(year / 10) * 10)

storm_sf <- st_as_sf(storm_data, coords = c("LON", "LAT"), crs = 4326)

# Define the boundaries of the region
region <- st_bbox(storm_sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot a hurricane track map containing panels for each decade
X <- ggplot(data = world) +
  geom_sf(fill = "gray80", color = "white") +
  facet_wrap(~decade) +
  geom_bin2d(data = storm_sf, aes(x = st_coordinates(storm_sf)[, 1], y = st_coordinates(storm_sf)[, 2]), bins = 100) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log", direction = 1, breaks = c(1, 10, 100, 1000)) +
  coord_sf(xlim = c(-160, 60), ylim = c(0, 80)) +
  labs(fill = "Count") +
  scale_x_continuous(breaks = seq(-100, 0, by = 50)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

print(X)

# Load US state data
data(us_states)

# Convert the state data to the same coordinate system as the hurricane data
us_states <- st_transform(us_states, st_crs(storm_sf))

# Change column names to avoid conflicts
us_states <- us_states %>% 
  select(state = NAME)

# Do a spatial join to count the number of hurricanes that hit each state
storm_states <- st_join(storm_sf, us_states, join = st_intersects, left = FALSE)

# Group by state and count the number of hurricanes that hit each state
state_storm_count <- storm_states %>%
  group_by(state) %>%
  summarize(storms = length(unique(USA_SSHS))) %>%
  arrange(desc(storms)) %>%
  slice(1:5)

# Print the five states with the most attacks
print(state_storm_count)
