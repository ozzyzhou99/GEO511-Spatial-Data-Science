# 加载必要的包
library(osmdata)
library(ggplot2)
library(sf)

# 定义布法罗地区的边界框 (longitude 和 latitude 范围)
bbox <- c(-78.9, 42.8, -78.7, 43.1)

# 从 OpenStreetMap 提取布法罗的街道 (highways)
buffalo_streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# 提取街道线数据
streets <- buffalo_streets$osm_lines

# 从 OpenStreetMap 提取布法罗的建筑物 (building)
buffalo_buildings <- opq(bbox = bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

# 提取建筑物多边形数据
buildings <- buffalo_buildings$osm_polygons

# 从 OpenStreetMap 提取布法罗的水体 (natural = water)
buffalo_water <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

# 提取水体多边形数据
water <- buffalo_water$osm_polygons

# 可视化布法罗的绿地与 OSM 底图
x <- ggplot() +
  # 底图部分
  geom_sf(data = streets, aes(geometry = geometry), color = "gray80", size = 0.3) +  # OSM 街道
  geom_sf(data = buildings, aes(geometry = geometry), fill = "gray70", color = "gray50", alpha = 0.6) +  # OSM 建筑物
  geom_sf(data = water, aes(geometry = geometry), fill = "lightblue", color = "blue", alpha = 0.5) +  # OSM 水体
  
  # 叠加绿地数据
  geom_sf(data = green_space, aes(geometry = geometry), fill = "green", color = "darkgreen", alpha = 0.6) +  # OSM 绿地
  
  # 调整地图显示范围
  coord_sf(xlim = c(-78.9, -78.7), ylim = c(42.8, 43.1), expand = FALSE) +
  
  # 添加标题和主题
  labs(title = "Green Spaces in Buffalo, NY", subtitle = "With OpenStreetMap Elements",
       caption = "Data Source: OpenStreetMap") +
  theme_minimal() 

# 打印地图
print(x)
