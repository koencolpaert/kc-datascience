# Load ggplot2 from github, newer version gives issues in plot
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

# Set working directory
setwd("~/Dev/subway-brussels")

# Load data subway stations and select columns
subways <- read.csv("./input/subway-brussels.csv", header=TRUE, sep=";")
df_bxlsubs <- subways[ ,(1:5)]  

# Created date frame with location and datetime columns
df_bxlsubs_timeloc = df_bxlsubs %>%
  unite(geo_location, c(station, location), sep = ";", remove = FALSE) %>%
  separate(opened, into = c("opened_month", "opened_day", "opened_year"), sep = "/") %>%
  mutate_geocode(geo_location, source = "google")

# Write geolocated df to CSV for further analysis
write.csv(df_bxlsubs_timeloc, file = "./output/subways_bxl.csv")
# Manual conversion from yy to yyyy format
df_subways_bxl <- read.csv("./output/subways_bxl.csv", header=TRUE, sep=",")

# Subways opened by year
ggplot(data = df_subways_bxl, aes(x = opened_year)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Jaar van opening") + ylab("Aantal stations") + ggtitle("Geopende stations per jaar") +
  scale_fill_gradient(low = "lightgray", high = "black")

# Subways opened by Line
ggplot(data = df_subways_bxl, aes(x = line)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Metrolijn") + ylab("Aantal stations") + ggtitle("Geopende stations per Lijn") +
  scale_fill_gradient(low = "lightgray", high = "black")

# Get city map from Google
brussels_map = get_googlemap(center = "Brussels", maptype = "roadmap", zoom = 11, size = c(640, 420), color = "bw")

# Plot subway stations on map
city_plot = function(city, city_map){
  ggmap(city_map, extent = "device") +
    geom_point(data = subset(df_subways_bxl, city == city), aes(x = lon, y = lat),
               color = "#0571b0", size = 3)
}

brussels.plot = city_plot("Brussels", brussels_map)
brussels.plot

# Packages for calculating center
library(purrr)
library(deldir)

# Calculate the centre of the map
data_deldir = df_subways_bxl %>%
  nest(-city, .key = location_info) %>%
  mutate(deldir = map(location_info, function(df) deldir(df$lon, df$lat))) %>%
  mutate(del.area = map(deldir, "del.area")) %>%
  mutate(delsgs = map(deldir, "delsgs")) %>%
  mutate(summary = map(deldir, "summary"))
data_deldir

# Prepare lines between stations
data_deldir_delsgs = data_deldir %>%
  select(city, delsgs) %>%
  unnest()
head(data_deldir_delsgs)

# Calculate the centroid
data_deldir_cent = data_deldir %>%
  select(city, summary) %>%
  unnest() %>%
  group_by(city) %>%
  summarise(cent_x = sum(x * del.wts),
            cent_y = sum(y * del.wts)) %>%
  ungroup()
data_deldir_cent

# Plot everything on map
del_plot = function(city_name, city_map){
  ggmap(city_map, extent = "device") +
    geom_segment(data = subset(data_deldir_delsgs, city == city_name), aes(x = x1, y = y1, xend = x2, yend = y2),
                 size = 1, color= "#92c5de") +
    geom_point(data = subset(df_subways_bxl, city == city_name), aes(x = lon, y = lat),
               color = "#0571b0", size = 3) +
    geom_point(data = subset(data_deldir_cent, city == city_name),
               aes(x = cent_x, y = cent_y),
               size = 6, color= "#ca0020")
}

brussels_del.plot = del_plot("Brussels", brussels_map)
brussels_del.plot

# Packages for multiple timepoints
install.packages("cowplot")
library(cowplot)

time_deldir_delsgs = data.frame()

time_deldir_sum = data.frame()

for(c in c("Brussels")) {
  data_city = filter(data, city == c)
  for(year in min(data_city$opened_year):2015) {
    data_year = filter(data_city, opened_year <= year)
    
    # Add condition to skip if number of stops less than 3
    if(dim(data_year)[1] < 3) next
    
    year_deldir = deldir(data_year$lon, data_year$lat)
    
    year_deldir_delsgs = year_deldir$delsgs %>%
      mutate(city = c) %>%
      mutate(opened_year = year)
    
    time_deldir_delsgs = bind_rows(time_deldir_delsgs, year_deldir_delsgs)
    
    year_deldir_sum = year_deldir$summary %>%
      mutate(city = c) %>%
      mutate(opened_year = year)
    
    time_deldir_sum = bind_rows(time_deldir_sum, year_deldir_sum)
  }
}