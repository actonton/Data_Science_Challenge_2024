life_expectancy = read.csv("data/life-expectancy.csv")
colnames(life_expectancy)[4] <- "Life_Expectancy"

library(tidyverse)
library(magick)

world_map = subset(map_data("world"), region != "Antarctica")
world_map$region[world_map$region == "USA"] <- "United States"
world_map$region[world_map$region == "USA"] <- "United Kingdom"
world_map$region[world_map$region == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
world_map$region[world_map$region == "Republic of Congo"] <- "Congo"
world_map$region[world_map$region == "Ivory Coast"] <- "Cote d'Ivoire"
world_map$region[world_map$region == "Czech Republic"] <- "Czechia"
world_map$region[world_map$region == "Trinidad"] <- "Trinidad and Tobago"

i = 1980
while (i <= max(as.numeric(life_expectancy$Year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(life_expectancy, Year==toString(i)), aes(map_id=Entity, fill=Life_Expectancy)) +
    scale_fill_gradient(low = "#ee4c02", high = "#fcf7ec", name = "Life Expectancy",
                        limits = c(min(life_expectancy$Life_Expectancy), max(life_expectancy$Life_Expectancy))) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) + theme(plot.title=element_text(hjust=0.5))
  filename = paste("plotsLE/",toString(i),".png",sep="")
  ggsave(filename,p)
  i = i+1
}

png_files <- list.files("plotsLE",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 2) %>% # animates
  image_write("LE.gif")

'ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey", alpha = 0.3) +
  geom_map(map = world_map, data = life_expectancy_2020, aes(map_id=Entity, fill=Life_Expectancy)) +
  scale_fill_gradient(low = "#cc4c02", high = "#fff7bc", name = "Life Expectancy") + theme_void() + coord_fixed(1.2)
'