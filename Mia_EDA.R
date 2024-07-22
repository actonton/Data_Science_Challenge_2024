life_expectancy = read.csv("data/life-expectancy.csv")
colnames(life_expectancy)[4] <- "Life_Expectancy"
life_expectancy$Entity[life_expectancy$Entity == "United States"] <- "USA"
life_expectancy$Entity[life_expectancy$Entity == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
life_expectancy$Entity[life_expectancy$Entity == "Congo"] <- "Republic of Congo"
life_expectancy_2020 = subset(life_expectancy, Year=='2020')

library(tidyverse)
library(magick)

world_map = subset(map_data("world"), region != "Antarctica")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey", alpha = 0.3) +
  geom_map(map = world_map, data = life_expectancy_2020, aes(map_id=Entity, fill=Life_Expectancy)) +
  scale_fill_gradient(low = "#cc4c02", high = "#fff7bc", name = "Life Expectancy") + theme_void() + coord_fixed(1.5)
