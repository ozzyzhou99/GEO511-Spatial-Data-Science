library(ggplot2)
library(gapminder)
library(dplyr)

# Fliter the kuwait data
gapminder_filtered <- gapminder %>% 
  filter(country != "Kuwait")

# create plot 1
plot1 <- ggplot(gapminder_filtered, aes(x = lifeExp, y = gdpPercap, color = continent, size = pop / 100000)) +
  geom_point(alpha = 0.7) + 
  scale_y_continuous(trans = "sqrt", limits = c(200, 50000)) + 
  facet_wrap(~year, nrow = 1) + 
  theme_bw() +  
  labs(
    x = "Life Expectancy",
    y = "GDP per capita",
    color = "Continent",
    size = "Population (100k)"
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10)
  )

# print and save plot 1
print(plot1)
ggsave("plots/wealth_life_expectancy_plot1.png", plot = plot1, width = 15, height = 8)

# calculate plot 2 data
gapminder_continent <- gapminder_filtered %>% 
  group_by(continent, year) %>% 
  summarize(
    gdpPercapweighted = weighted.mean(gdpPercap, w = pop),  # 加权平均GDP
    pop = sum(pop),
    .groups = 'drop' 
  )

# create plot 2
plot2 <- ggplot(gapminder_filtered, aes(x = year, y = gdpPercap, group = country, color = continent)) +
  geom_line(alpha = 0.4) +  
  geom_point(alpha = 0.4, aes(size = pop / 100000)) + 
  geom_line(data = gapminder_continent, aes(x = year, y = gdpPercapweighted, group = continent), color = "black", size = 1) +  
  geom_point(data = gapminder_continent, aes(x = year, y = gdpPercapweighted, group = continent, size = pop / 100000), color = "black", shape = 16) +  
  facet_wrap(~continent, nrow = 1) + 
  theme_bw() +
  labs(
    x = "Year",
    y = "GDP per capita",
    color = "Continent",
    size = "Population (100k)"
  ) +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10)
  )

# print and save plot 1
print(plot2)
ggsave("plots/wealth_life_expectancy_plot2.png", plot = plot2, width = 15, height = 8)
