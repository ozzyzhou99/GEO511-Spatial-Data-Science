library(spData)
library(sf)
library(tidyverse)
# library(units) #this one is optional, but can help with unit conversions.

#load 'world' data from spData package
data(world)  
# load 'states' boundaries from spData package
data(us_states)

albers_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
canada <- world[world$name_long == "Canada", ] %>% st_transform(crs = albers_proj)
new_york <- us_states[us_states$NAME == "New York", ] %>% st_transform(crs = albers_proj)

canada_buffer <- st_buffer(canada, dist = 10000)  # Buffer by 10km
border_area <- st_intersection(canada_buffer, new_york)

area_sqm <- st_area(border_area)  # Area in square meters
area_sqkm <- area_sqm / 1e6  # Convert to square kilometers
print(area_sqkm)

ggplot() +
  geom_sf(data = border_area) +
  theme_minimal()
