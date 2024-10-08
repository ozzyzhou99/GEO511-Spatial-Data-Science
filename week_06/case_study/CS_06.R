# Load the required libraries
library(terra)
library(spData)
library(tidyverse)
library(sf)
library(leaflet)

# Download and load data
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc", "crudata.nc", method = "curl")
tmean <- rast("crudata.nc")

# View raster data information
print(tmean)

# Calculate the maximum temperature of each pixel
tmean_max <- max(tmean)

# Load the world dataset
data(world)  

# Extract the maximum temperature value for each country
world_clim <- terra::extract(tmean_max, world, fun=max, na.rm=T, small=T)

# Merge the extracted maximum temperatures with the country dataset
world_clim <- bind_cols(world, as.data.frame(world_clim))


# Plot the world's maximum temperature map
plot <-ggplot(world_clim) +
  geom_sf(aes(fill = max)) +  
  scale_fill_viridis_c(name = "Maximum\nTemperature (C)", limits = c(-5, 40)) +  
  theme_minimal() +  
  theme(legend.position = "bottom")  

print(plot)

# Find the hottest country on each continent
hottest_continents <- world_clim %>%
  group_by(continent) %>%
  top_n(1, max) %>%  # Find the largest value in the max column for each continent
  select(name_long, continent, max) %>%  
  arrange(desc(max)) %>%  
  st_set_geometry(NULL)  

# View output table
print(hottest_continents)

# Save as a CSV file
write.csv(hottest_continents, "hottest_continents.csv", row.names = FALSE)

# Convert date to sf
world_clim_sf <- st_as_sf(world_clim)

# Create a Leaflet map
map <- leaflet(world_clim_sf) %>%
  addTiles() %>%  
  addPolygons(
    fillColor = ~colorNumeric("YlOrRd", max)(max),  
    weight = 0,  
    color = NULL,  
    fillOpacity = 0.7  
  ) %>%
  addLegend(
    pal = colorNumeric("YlOrRd", domain = world_clim_sf$max),
    values = ~max,
    opacity = 0.7,
    title = "Maximum Temperature (C)",
    position = "bottomright"
  )

# Print map
print(map)

