reprex({
  library(tidyverse)
  library(reprex)
  library(sf)
  
  library(spData)
  data(world)
  
  y <- ggplot(world,aes(x=gdpPercap, y=continent, color=continent))+
    geom_density(alpha=0.5,color=F)
  
  print(y)
}, venue = "gh")