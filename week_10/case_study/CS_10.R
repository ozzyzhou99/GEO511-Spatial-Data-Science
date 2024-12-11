# Load necessary packages
library(terra)
library(rasterVis)
library(ggplot2)
library(tidyverse)
library(sf)
library(ncdf4)
library(zoo)

# Create a data directory
dir.create("data", showWarnings = FALSE)

# Define data URL
lulc_url <- "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url <- "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# Download data
download.file(lulc_url, destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url, destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

# Set the land cover type
Land_Cover_Type_1 <- c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

# Create a lookup table
lcd <- data.frame(
  ID = Land_Cover_Type_1,
  landcover = names(Land_Cover_Type_1),
  col = c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
          "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
          "#808080", "#000000", "#000000"),
  stringsAsFactors = FALSE)

# Part 1: Time Series Extraction
extract_point_timeseries <- function(lst) {
  # Create point locations
  lw <- data.frame(x= -78.791547, y=43.007211) %>% 
    st_as_sf(coords=c("x","y"), crs=4326)
  
  lw_proj <- st_transform(lw, crs(lst))
  
  lst_values <- terra::extract(lst, lw_proj, buffer=1000, fun=mean, na.rm=TRUE)
  dates <- time(lst)
  
  # Create a data frame
  ts_df <- data.frame(
    date = as.Date(dates),
    temp = as.numeric(lst_values[,-1])
  ) %>%
    filter(!is.na(temp))
  
  # Create smoothed data
  smooth_fit <- smooth.spline(as.numeric(ts_df$date), ts_df$temp, spar=0.1)
  smooth_data <- data.frame(
    date = ts_df$date,
    temp = predict(smooth_fit, as.numeric(ts_df$date))$y
  )
  
  # Create a time series graph
  p <- ggplot() +
    theme_grey() +
    geom_point(data = ts_df, 
               mapping = aes(x = date, y = temp),
               color = "black", 
               alpha = 0.5,
               size = 0.8) +
    geom_line(data = smooth_data,
              mapping = aes(x = date, y = temp),
              color = "blue",
              linewidth = 0.3) +
    scale_y_continuous(limits = c(-10, 40),
                       breaks = seq(0, 40, by = 20)) +
    labs(x = "date",
         y = "Monthly Mean Land Surface Temperature") +
    theme(panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"))
  
  return(list(data = ts_df, plot = p))
}

# Part 2: Calculation of monthly climate data
calculate_monthly_climatology <- function(lst) {
  # Calculate monthly average
  lst_month <- tapp(lst, index="month", fun=mean, na.rm=TRUE)
  
  # Set the month order
  month_order <- c("February", "March", "April", "May",
                   "June", "July", "August", "September",
                   "October", "November", "December", "January")
  
  # Rename the layer
  names(lst_month) <- month.name
  lst_month <- lst_month[[month_order]]
  
  # Convert to data frame
  plot_data <- as.data.frame(lst_month, xy=TRUE) %>%
    gather(key="Month", value="Temperature", -x, -y) %>%
    mutate(Month = factor(Month, levels=month_order))
  
  # Create a monthly climate map
  p <- ggplot(plot_data) +
    geom_raster(aes(x=x, y=y, fill=Temperature)) +
    facet_wrap(~Month, ncol=4) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 15,
      limits = c(-10, 30),
      name = "value",
      na.value = "grey85"
    ) +
    coord_equal() +
    theme_minimal() +
    theme(
      strip.text = element_text(size=10),
      strip.background = element_rect(fill="grey85", color=NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.background = element_rect(fill="grey85", color=NA),
      legend.position = "right",
      panel.border = element_blank()
    )
  
  return(list(plot = p, raster = lst_month))
}

# Part 3: Land cover type analysis
analyze_lst_by_landcover <- function(lst_file, lulc_file) {
  lst <- rast(lst_file, subds="LST_Day_1km") 
  scoff(lst) <- cbind(0.02, -273.15)
  
  lulc <- rast(lulc_file)
  
  lulc2 <- resample(lulc, lst, method="near")
  
  values_lst <- values(lst_month)
  values_lulc <- values(lulc2)
  na_mask <- is.na(values_lst)
  
  df <- data.frame(
    Land_Cover_Type = as.factor(values_lulc[!na_mask]),
    LST = values_lst[!na_mask]
  )
  
  df <- df %>%
    mutate(Land_Cover_ID = as.numeric(Land_Cover_Type))
  
  df_filtered <- df %>%
    filter(Land_Cover_ID %in% c(13, 4))
  
  summary_stats <- df_filtered %>%
    group_by(Land_Cover_Type) %>%
    summarize(
      mean_lst = mean(LST),
      sd_lst = sd(LST)
    )
  
  p <- ggplot(summary_stats, aes(x = Land_Cover_Type, y = mean_lst)) +
    geom_bar(stat = "identity", fill = "#4c72b0") +
    geom_errorbar(aes(ymin = mean_lst - sd_lst, ymax = mean_lst + sd_lst), width = 0.4) +
    labs(
      title = "Land Surface Temperature by Land Cover Type",
      x = "Land Cover Type",
      y = "Mean Land Surface Temperature (°C)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey90")
    )
  
  return(list(plot = p, data = summary_stats))
}


# Run a full analysis
lst_file <- "data/MOD11A2.006_aid0001.nc"
lulc_file <- "data/MCD12Q1.051_aid0001.nc"

# Read and convert LST data
lst <- rast(lst_file, subds="LST_Day_1km")
scoff(lst) <- cbind(0.02, -273.15)

# Run the three-part analysis
ts_results <- extract_point_timeseries(lst)
climate_results <- calculate_monthly_climatology(lst)
#landcover_results <- analyze_lst_by_landcover(lst_file, lulc_file)

print("Part 1: Time Series Analysis")
print(ts_results$plot)

print("Part 2: Monthly Climatology")
print(climate_results$plot)

print("Part 3: Land Cover Analysis")
print(landcover_results$plot)