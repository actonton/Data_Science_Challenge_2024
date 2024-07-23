library(tidyverse)
library(plotly)


# Life Expectancy Dataset

le_df <- read_csv("data/life-expectancy.csv")
glimpse(le_df)

## Cleaning

le_clean <- janitor::clean_names(le_df)
glimpse(le_clean)

## Entity

le_clean %>% pull(entity) %>% unique()

## Missing values

#visdat::vis_miss(le_clean)

le_clean %>% 
  filter(is.na(code)) %>% 
  select(entity) %>% 
  distinct()

## World Life Expectancy by Year

p <- le_clean %>% 
  filter(entity == "World") %>% 
  ggplot(aes(x = year, y = period_life_expectancy_at_birth_sex_all_age_0)) +
  geom_line() +
  labs(title = "World Life Expectancy by Year",
       x = "Year",
       y = "Life Expectancy at Birth (years)")
ggplotly(p)

## Split by development status

development_status <- stringr::str_subset(le_clean$entity, "devel") %>% unique()

p <- le_clean %>%
  filter(entity %in% development_status) %>%
  ggplot(aes(x = year, 
             y = period_life_expectancy_at_birth_sex_all_age_0,
             color = entity)) +
  geom_line() + 
  labs(title = "Life Expectancy by Year",
       x = "Year",
       y = "Life Expectancy at Birth (years)")
#ggplotly(p)

le_clean %>% drop_na(code) %>%
  filter(year < 1950) %>% 
  pull(entity) %>% 
  unique() %>% 
  length()

le_clean %>% drop_na(code) %>%
  filter(year >= 1950) %>% 
  group_by(entity) %>% 
  count() %>% 
  filter(n == 72) %>%
  pull(entity) %>%
  unique() %>%
  length()

le_clean %>% drop_na(code) %>%
  pull(entity) %>%
  unique() %>% 
  length()

# Difference in Life Expectancy

#Create a new column that calculates the difference in life expectancy from the previous year.

le_clean <- le_clean %>% 
  group_by(entity) %>% 
  mutate(diff = c(NA, diff(period_life_expectancy_at_birth_sex_all_age_0))) %>%
  ungroup()

p <- le_clean %>%
  filter(entity %in% development_status) %>%
  drop_na(diff) %>%
  ggplot(aes(x = year, 
             y = diff,
             color = entity)) +
  geom_line() + 
  geom_abline(intercept = 0.243, slope = 0, color = "black", linetype = "dotted") +
  labs(title = "Life Expectancy Diff by Year",
       x = "Year",
       y = "Life Expectancy at Birth (years)")
#ggplotly(p)

# Global Vaccination Coverage

vaccination_df <- read_csv("data/global-vaccination-coverage.csv")
vaccination_df %>% glimpse()
vaccination_clean <- janitor::clean_names(vaccination_df)

vaccination_long <- vaccination_clean %>% 
  pivot_longer(cols = -c(entity, code, year), 
               names_to = "vaccine", 
               values_to = "coverage")

visdat::vis_miss(vaccination_long)

WHO_regions <- stringr::str_subset(vaccination_df$Entity, "WHO") %>% unique()
p_vac <- vaccination_long %>% 
  filter(entity %in% WHO_regions) %>%
  ggplot(aes(x = year, y = coverage, color=vaccine)) +
  geom_line() + geom_point() +
  facet_wrap(~entity) +
  labs(title = "Global Vaccination Coverage by Year",
       x = "Year",
       y = "Coverage (%)")
ggplotly(p_vac)

library(gganimate)
suppressWarnings(animate(p_vac+transition_time(year)))

library(magick)

world_map = subset(map_data("world"), region != "Antarctica")

i = 1950
while (i <= max(as.numeric(vaccination_clean$Year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=bcg_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 Year Olds Immunised with BCG (Against TB)") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
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
  image_animate(fps = 5) %>% # animates
  image_write("All_plots.gif")
