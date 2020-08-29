#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)
library(plyr)

#===========================
# SCRAPE DATA FROM WIKIPEDIA
#===========================
## Search for appropriate Wikipedia page
## scrape data with readr
## Look fpr the table containing data, her it is .[[1]]

html.population <- read_html('https://nl.wikipedia.org/wiki/Lijst_van_steden_in_Belgi%C3%AB')

df.be_cities <- html.population %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)

# inspect
df.be_cities %>% head()
df.be_cities %>% names()
View(df.be_cities)

#============================
# REMOVE UNWANTED COLUMNS
#============================
## Only keeping columns stad and inwoners 
## Rename column inwoners

df.be_cities <- df.be_cities[,c(1,5)]
df.be_cities <- rename(df.be_cities,c("Inwoners (2013)[1]"="Inwoners"))


## Get geocoding
data.geo <- geocode(df.be_cities$Stad)

df.be_all_cities <- cbind(df.be_cities, data.geo)

map.be <- get_map(location = "Belgium", zoom = 8, maptype = "roadmap", color = "bw")
map.be %>% ggmap()

#============================================================================
# PLOT CITIES ON MAP
#============================================================================

# FIRST ITERATION

ggmap(map.be) +
  geom_point(data = df.be_all_cities, aes(x = lon, y = lat, size = "Inwoners"), color = "red", alpha = .3) +
  geom_point(data = df.be_all_cities, aes(x = lon, y = lat, size = "Inwoners"), color = "red", shape = 1) 

# FINAL ITERATION
## Can't get the scale to plot aarrrgggghhhh!!!

ggmap(map.be) +
  geom_point(data = df.be_all_cities, aes(x = lon, y = lat, size = "Inwoners"), color = "red", alpha = .1) +
  geom_point(data = df.be_all_cities, aes(x = lon, y = lat, size = "Inwoners"), color = "red", shape = 1) +
  labs(x = NULL, y = NULL) +
  labs(size = 'Inwoners (x1000)') +
  labs(title = "Grootste steden in België", subtitle = "Bron: https://nl.wikipedia.org/wiki/Lijst_van_steden_in_Belgi%C3%AB") +
  scale_size_continuous(range = c(.6,18), labels = scales::comma_format(), breaks = c(1500000, 10000000, 20000000)) +
  theme(text = element_text(color = "#4A4A4A", family = "Gill Sans")) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(plot.title = element_text(size = 32)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "white"))


