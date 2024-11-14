# 加载所需包
library(tidyverse)
library(spData)
library(sf)
library(foreach)
library(doParallel)
library(tidycensus)
library(leaflet)
library(mapview)

# 注册并检查并行核
registerDoParallel(4)
getDoParWorkers()

# 设置Census API Key
census_api_key("71ee368dd6a9d472f0ba814e6f90b8566a1a7192")

# 定义种族变量
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

# 裁剪数据以减少计算量
erie <- st_crop(erie, xmin = -78.9, xmax = -78.85, ymin = 42.888, ymax = 42.92)

# 确保 variable 列是因子类型
erie$variable <- as.factor(erie$variable)

# 创建并行集群
numCores <- detectCores() - 1 # 留一个核心以保证系统稳定
cl <- makeCluster(numCores)
registerDoParallel(cl)

# 将 sf 对象和其他所需的变量传递给每个并行工作节点
clusterExport(cl, c("erie"))
clusterEvalQ(cl, library(sf)) # 确保每个节点加载 sf 包

# 使用 foreach 循环处理每个种族
erie_dots <- foreach(race = levels(erie$variable), .combine = rbind, .packages = c("sf", "dplyr")) %dopar% {
  # 过滤数据，仅保留当前种族
  race_data <- erie %>% filter(variable == race)
  
  # 对每个人生成随机点，按每个多边形的人口数生成点
  points <- race_data %>%
    st_sample(size = .$value) %>%
    st_as_sf() %>%
    mutate(variable = race) # 添加 variable 列
  
  # 返回生成的点
  points
}

# 停止集群
stopCluster(cl)

# 使用 mapview 绘制地图，并将 zcol 设置为种族身份
print(mapview(erie_dots, zcol = "variable", cex = 1, legend = TRUE))
