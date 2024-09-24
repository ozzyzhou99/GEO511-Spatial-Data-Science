library(tidyverse)
library(nycflights13)

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
print(farthest_code)

# Join the table with the one that has the full airport names
farthest_airport <- farthest_flight %>%
  left_join(airports, by = c("dest" = "faa")) %>%  # Join flights by airport code
  select(name) %>%              # Select full name
  as.character()                # Convert the result

print(farthest_airport)