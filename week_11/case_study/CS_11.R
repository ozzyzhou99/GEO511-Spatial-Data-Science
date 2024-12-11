# Load required packages
library(tidyverse)
library(spData)
library(sf)
library(foreach)
library(doParallel)
library(tidycensus)
library(leaflet)
library(mapview)

# Register and check parallel cores
registerDoParallel(4)
getDoParWorkers()

# Set Census API Key
census_api_key("71ee368dd6a9d472f0ba814e6f90b8566a1a7192")

# Define race variables
race_vars <- c(
  "Total Population" = "P1_001N",
  "White alone" = "P1_003N",
  "Black or African American alone" = "P1_004N",
  "American Indian and Alaska Native alone" = "P1_005N",
  "Asian alone" = "P1_006N",
  "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
  "Some Other Race alone" = "P1_008N",
  "Two or More Races" = "P1_009N"
)

# 获取2020年人口普查种族数据
options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = race_vars, year=2020,
                      state = "NY", county = "Erie County", geometry = TRUE,
                      sumfile = "pl", cache_table=T) 

# Crop the data to reduce the amount of calculation
erie <- st_crop(erie, xmin = -78.9, xmax = -78.85, ymin = 42.888, ymax = 42.92)

# Make sure the variable column is a factor type
erie$variable <- as.factor(erie$variable)

# 创建并行集群
numCores <- detectCores() - 1 
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Pass the sf object and other required variables to each parallel worker node
clusterExport(cl, c("erie"))
clusterEvalQ(cl, library(sf)) 

# Use a foreach loop to process each race
erie_dots <- foreach(race = levels(erie$variable), .combine = rbind, .packages = c("sf", "dplyr")) %dopar% {
  race_data <- erie %>% filter(variable == race)
  points <- race_data %>%
    st_sample(size = .$value) %>%
    st_as_sf() %>%
    mutate(variable = race) 
  points
}

stopCluster(cl)

# Draw the map using mapview and set zcol to ethnic identity
print(mapview(erie_dots, zcol = "variable", cex = 1, legend = TRUE))
