# 加载所需的包
library(spData)
library(sf)
library(tidyverse)
library(units)

library(leaflet)

# 加载世界数据和美国州数据
data(world)
data(us_states)

# 将世界数据转换为等面积投影 (Albers equal area projection)
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
world_albers <- st_transform(world, crs = albers)

# 过滤出加拿大的多边形install.packages("leaflet")
canada <- world_albers %>% filter(name_long == "Canada")

# 缓冲加拿大边界10公里 (10000米)
canada_buffer <- st_buffer(canada, dist = 10000)

# 将美国州数据也转换为Albers投影
us_states_albers <- st_transform(us_states, crs = albers)

# 过滤出纽约州
new_york <- us_states_albers %>% filter(NAME == "New York")

# 计算纽约州与加拿大10公里缓冲区的交集
ny_border_area <- st_intersection(new_york, canada_buffer)

# 计算交集区域的面积，单位为平方公里
ny_border_area_km2 <- st_area(ny_border_area) %>% set_units(km^2)

# 打印面积
print(ny_border_area_km2)

# 绘制结果
plot <- ggplot() +
  geom_sf(data = ny_border_area, fill = "red") +
  geom_sf(data = new_york, fill = NA, color = "black") +
  labs(title = "New York Land within 10km of Canadian Border")

# 显式调用 print() 来输出图像
print(plot)


# 将边界对象转换为WGS84投影，因为leaflet使用的坐标系为WGS84
ny_border_area_wgs84 <- st_transform(ny_border_area, crs = 4326)

# 使用 leaflet 创建地图并显式打印
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = ny_border_area_wgs84, 
              color = "red", 
              weight = 2, 
              fillOpacity = 0.5) %>%
  setView(lng = -75, lat = 44, zoom = 7)

# 打印地图
print(map)

