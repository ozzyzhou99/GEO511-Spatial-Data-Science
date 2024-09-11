library(tidyverse)

# define the link to the data - you can try this in your browser too.  Note that the URL ends in .txt.
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"

#the next line tells the NASA site to create the temporary file
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

# the next lines download the data
temp=read_csv(dataurl, 
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))

# Plot the JJA data over years
temp_plot <- ggplot(temp, aes(YEAR, JJA)) + 
  geom_line(color = "black") +                  # Plot the annual mean JJA temperature
  #geom_smooth(method = "lm", color = "blue") +  # Add linear regression line
  geom_smooth(method = "loess", color = "red") + # Add the smooth line
  xlab("Year") +                                
  ylab("Mean Summer Temperatures (C)") +                  
  ggtitle("Mean Summer Temperatures in Buffalo, NY\nSummer includes June, July, and August\nData from the Global Historical Climate Network\nRed line is a LOESS smooth")


# Save the plots as a PNG file
ggsave("mean_summer_temperatures_buffalo.png", plot = temp_plot, width = 10, height = 6)

# Show the plots
print(temp_plot)