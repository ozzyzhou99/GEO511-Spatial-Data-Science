# 加载所需的库
library(terra)
library(spData)
library(tidyverse)
library(sf)

# 下载并加载CRU数据
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc", "crudata.nc", method = "curl")
tmean <- rast("crudata.nc")

# 查看栅格数据信息
print(tmean)

# 计算每个像素的最大温度
tmean_max <- max(tmean)

# 加载世界数据集
data(world)  

# 提取每个国家的最大温度值
world_clim <- terra::extract(tmean_max, world, fun=max, na.rm=T, small=T)

# 将提取的最大温度与国家数据集合并
world_clim <- bind_cols(world, as.data.frame(world_clim))


# 可视化：绘制世界最大温度图
ggplot(world_clim) +
  geom_sf(aes(fill = max)) +  # 使用 'max' 列表示最大温度
  scale_fill_viridis_c(name = "Maximum\nTemperature (C)", limits = c(0, 30)) +  # 设置颜色渐变和温度范围
  theme_minimal() +  # 使用简单的主题
  theme(legend.position = "bottom")  # 将图例放在底部

