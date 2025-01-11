library(tidyverse)
library(readxl)
library(httr)
library(ggpubr)
library(xml2)
rm(list = ls())

get_coordinates <- function(location) {
  url <- paste0("https://nominatim.openstreetmap.org/search?q=",
                URLencode(location),
                "&format=xml&limit=1") 
  res <- GET(url, user_agent("R script"))
  
  xml_content <- content(res, as = "text", encoding = "UTF-8")
  xml_parsed <- read_xml(xml_content)
  place_node <- xml_find_first(xml_parsed, "//place")
  lat <- xml_attr(place_node, "lat")
  lon <- xml_attr(place_node, "lon")
  return(c(lat = as.numeric(lat), lon = as.numeric(lon)))
}

remove_outliers <- function(data, column) {
  q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  data %>%
    filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}



data <- read_excel("water_temp_from_IMGW-PIB.xlsx")

data <- data %>%
  mutate(
    temperature_C = ifelse(is.na(temperature_C) & temperature_C == "brak danych", NA, temperature_C),
    temperature_C = as.numeric(gsub(",", ".", as.character(temperature_C))))

data <- data %>%
  rowwise() %>%
  mutate(
    coords = list(get_coordinates(paste(station, region)))) %>%
  unnest_wider(coords, names_sep = "_") %>%
  rename(latitude = coords_lat, longitude = coords_lon)

poland_map <- map_data("world") %>%
  filter(region == "Poland")




p1 <- ggplot() +
  geom_polygon(
    data = poland_map,
    aes(x = long, y = lat, group = group),
    fill = "white", color = "black") +
  geom_point(
    data = data,
    aes(x = longitude, y = latitude, color = temperature_C),
    size = 3) +
  coord_map()+
  scale_color_viridis_c(
    name = "Temperature (°C)",
    na.value = "gray") +
  labs(
    title = "Temperatures of water bodies in Poland") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))




data_clean <- remove_outliers(data, "temperature_C")
lat_lon_temp <- gather(data_clean, key="key", value="degree", c("latitude", "longitude"))


p2 <- ggplot(lat_lon_temp, aes(x = degree, y = temperature_C)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  
  geom_smooth(method = "lm", se = TRUE, color = "red") +  
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x, color = "black", label.x.npc = 0.75, label.y.npc = 1) +
  stat_cor(aes(label = ..rr.label..), color = "black", label.x.npc = 0.75, label.y.npc = 0.95) +
  facet_wrap(~key, ncol = 2, scales = "free_x", strip.position = "top") +
  labs(
    title = "Latitudinal and Longitudinal Analysis of Water Body Temperatures in Poland",
    x = "degree (°)",
    y = "Temperature (°C)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "lightgray", color = "black"))


ggsave("lake_temperatures_poland.jpg", plot = p1, width = 10, height = 8)
ggsave("lon_lat_temp_outliers_removed.jpg", plot = p2, width = 10, height = 8)

