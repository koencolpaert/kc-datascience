#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(stringr)
library(ggmap)

#=======
# SCRAPE
#=======
html.be_cities <- read_html("https://nl.wikipedia.org/wiki/Tabel_van_Belgische_gemeenten")


df.be_cities <- html.be_cities %>%
  html_nodes("table") %>%
  .[[1]] %>% 
  html_table()


# inspect
df.be_cities %>% head()

#====================
# CHANGE COLUMN NAMES
#====================

# inspect initial column names
colnames(df.be_cities)

#----------------
# PARSE AS NUMBER
#----------------

df.be_cities <- mutate(df.be_cities, "2017 Inwoners" = parse_number("2017 Inwoners"))


#=========================================
# GEOCODE
# - here, we're getting the lat/long data
#=========================================

data.geo <- geocode(df.be_cities$Gemeente)

#inspect

data.geo %>% head()
data.geo

#========================================
# RECOMBINE: merge geo data to data frame
#========================================

df.be_cities <- cbind(df.be_cities, data.geo)
df.be_cities

# select columns needed
df.be_cities2017 <- df.be_cities[c(2,7,17,18)]

#===========
# GET BE MAP
#===========

map.be <- ggmap::get_map(location = "Belgium", zoom = 7, maptype = "roadmap", color = "bw")

#===========
# FIRST PLOT
#===========

df.be_cities50k <- df.be_cities2017[df.be_cities2017$`2017
Inwoners`>50.000,]

# There is a fault in the data since Herstappe only has 89 inhabitants

ggmap(map.be) +
  geom_point(data = df.be_cities50k,
             aes(x = lon, y = lat)) +
  ggtitle("Steden met +50k inwoners") +
  theme(plot.title = element_text(lineheight=1))
ggsave("./figures/50K.png")