library(spData)
library(sf)
library(tidyverse)
library(ggplot2)
library(units) #this one is optional, but can help with unit conversions.

#load 'world' data from spData package
data(world)  
# load 'states' boundaries from spData package
data(us_states)

albers_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
canada <- world[world$name_long == "Canada", ] %>% st_transform(crs = albers_proj)
new_york <- us_states[us_states$NAME == "New York", ] %>% st_transform(crs = albers_proj)

canada_buffer <- st_buffer(canada, dist = 10000)  # Buffer by 10km
border_area <- st_intersection(canada_buffer, new_york)

print(border_area)

area_sqm <- st_area(border_area)  # Area in square meters
area_sqkm <- set_units(st_area(border_area), "km^2")
print(area_sqkm)

ggplot() +
  geom_sf(data = border_area, fill = "red", color = "black") +   # Red fill for the border area
  coord_sf(xlim = c(-80, -71), ylim = c(40.5, 46.5)) +           # Adjust the limits to match the example
  theme_minimal() +                                              # Use a minimal theme
  labs(title = "New York Land within 10km")                      # Add a title
