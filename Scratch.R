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
#ggplotly(p)

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
colnames(vaccination_clean) = c(
  "entity",
  "code",
  "year",
  "BCG (TB)",
  "Hep B",
  "H. Inf. B",
  "Salk (Polio)",
  "Meningococcal",
  "Pneumococcal",
  "Polio",
  "Rubella",
  "Rotavirus",
  "Yellow Fever",
  "Diptheria, Pertussis, Tetanus"
)

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
animation = suppressMessages(suppressWarnings(animate(p_vac+transition_time(year))))
anim_save("Bouncing.gif",animation)

library(magick)

world_map = subset(map_data("world"), region != "Antarctica")
#standardise world map country names with OWID names
world_map$region[world_map$region == "USA"] <- "United States"
world_map$region[world_map$region == "USA"] <- "United Kingdom"
world_map$region[world_map$region == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
world_map$region[world_map$region == "Republic of Congo"] <- "Congo"
world_map$region[world_map$region == "Ivory Coast"] <- "Cote d'Ivoire"
world_map$region[world_map$region == "Czech Republic"] <- "Czechia"
world_map$region[world_map$region == "Trinidad"] <- "Trinidad and Tobago"

#BCG

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=bcg_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised with BCG (Against TB)") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxBCG/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxBCG",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("BCG.gif")

#HEP

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=hep_b3_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Hepatitis B3") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxHEP/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxHEP",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("HEP.gif")

#HIB

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=hib3_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised with Haemophilus Influenza B") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxHIB/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxHIB",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("HIB.gif")

#IPV

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=ipv1_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised with Salk (against Polio)") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxIPV/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxIPV",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("IPV.gif")

#MCV

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=mcv1_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised with against Meningococcal") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxMCV/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxMCV",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("MCV.gif")

#PCV

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=pcv3_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Pneumococcal") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxPCV/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxPCV",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("PCV.gif")

#POL

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=pol3_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Polio") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxPOL/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxPOL",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("POL.gif")

#RCV

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=rcv1_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised with against Rubella") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxRCV/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxRCV",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("RCV.gif")

#ROTA

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=rota_c_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Rotavirus") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxROTA/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxROTA",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("ROTA.gif")

#YTV

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=yfv_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Yellow Fever") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxYFV/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxYFV",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("YFV.gif")

#DTP

i = 1980
while (i <= max(as.numeric(vaccination_clean$year))) {
  p = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey", alpha = 0.3) +
    geom_map(map = world_map, data = subset(vaccination_clean, year==toString(i)), aes(map_id=entity, fill=dtp3_percent_of_one_year_olds_immunized)) +
    scale_fill_gradient(low = "#fe7c7c", high = "#bbeebb", name = "% Immunised",
                        limits = c(0, 100)) +
    theme_void() + coord_fixed(1.2) + ggtitle(toString(i)) +
    labs(subtitle="% 1 yos Immunised against Diphtheria, Pertussis, Tetanus") +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  filename = paste("plotsVaxDTP/",toString(i),".png",sep="")
  ggsave(filename,p,dpi=150,height=450,width=800,units="px")
  i = i+1
}
png_files <- list.files("plotsVaxDTP",
                        pattern = "\\.png$",
                        recursive = FALSE,
                        all.files = FALSE,
                        full.names = TRUE)
png_files %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write("DTP.gif")



ggplot() +
  geom_line(data=subset(le_clean, entity=="Afghanistan"), aes(x=year, y=life_expectancy)) +
  geom_line(data=subset(vaccination_clean, entity=="Afghanistan"), aes(x=year, y=coverage))


merged_datasets = merge(vaccination_clean, le_clean, by=c("entity","year"))
names(merged_datasets)[names(merged_datasets) == 'period_life_expectancy_at_birth_sex_all_age_0'] <- 'life_expectancy'

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=bcg_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=hep_b3_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=hib3_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=ipv1_percent_of_one_year_olds_immunized, y=life_expectancy, color=year)) +
  theme_bw() +
  labs(title="Salk (Polio) 1st Dose") + xlab("% 1yos vaccinated") + ylab("Life Expectancy")

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=mcv1_percent_of_one_year_olds_immunized, y=life_expectancy, color=year)) +
  theme_bw() +
  labs(title="Meningococcal 1st Dose") + xlab("% 1yos vaccinated") + ylab("Life Expectancy")

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=pcv3_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=pol3_percent_of_one_year_olds_immunized, y=life_expectancy, color=year)) +
  theme_bw() +
  labs(title="Polio 3rd Dose") + xlab("% 1yos vaccinated") + ylab("Life Expectancy")

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=rcv1_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=rota_c_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=yfv_percent_of_one_year_olds_immunized, y=life_expectancy))

ggplot(data=merged_datasets) +
  geom_point(aes(
    x=dtp3_percent_of_one_year_olds_immunized, y=life_expectancy, color=year)) +
  theme_bw() +
  labs(title="Diphtheria 3rd Dose") + xlab("% 1yos vaccinated") + ylab("Life Expectancy")

cor.test(merged_datasets$life_expectancy, merged_datasets$bcg_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$hep_b3_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$hib3_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$ipv1_percent_of_one_year_olds_immunized) #yes
cor.test(merged_datasets$life_expectancy, merged_datasets$mcv1_percent_of_one_year_olds_immunized) #yes
cor.test(merged_datasets$life_expectancy, merged_datasets$pcv3_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$pol3_percent_of_one_year_olds_immunized) #yes
cor.test(merged_datasets$life_expectancy, merged_datasets$rcv1_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$rota_c_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$yfv_percent_of_one_year_olds_immunized) #no
cor.test(merged_datasets$life_expectancy, merged_datasets$dtp3_percent_of_one_year_olds_immunized) #yes

linear_model = lm(formula =
                    life_expectancy ~ ipv1_percent_of_one_year_olds_immunized +
                    mcv1_percent_of_one_year_olds_immunized +
                    pol3_percent_of_one_year_olds_immunized, data=merged_datasets)

