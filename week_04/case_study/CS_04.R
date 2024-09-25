library(tidyverse)
library(nycflights13)
library(maps)

# View the tables
head(flights)
head(airports)

# Find the farthest airport code
farthest_flight <- flights %>%
  arrange(desc(distance)) %>%   # Arrange flights by distance
  slice(1)

# Print the airport code
farthest_code <- farthest_flight %>% 
  pull(dest)
#print(farthest_code)

# Join the table with the one that has the full airport names
farthest_airport <- farthest_flight %>%
  left_join(airports, by = c("dest" = "faa")) %>%  # Join flights by airport code
  select(name) %>%              # Select full name
  as.character()                # Convert the result

#print(farthest_airport)

airports %>%
  distinct(lon,lat) %>%
  ggplot(aes(lon, lat)) +
  borders("world") +
  geom_point(col="red") +
  coord_quickmap()

# Calculate average delay time
mean_delay <- flights %>%
  group_by(dest) %>%
  summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports, by = c("dest" = "faa"))  # obtain latitude and longitude

# Plot the map
ggplot(mean_delay, aes(lon, lat)) +
  borders("state") +  
  geom_point(aes(color = mean_delay), size = 3) +  
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Mean Delay\n(minutes)") +  
  coord_fixed(ratio = 1.3, xlim = c(-125, -65), ylim = c(25, 50))
  