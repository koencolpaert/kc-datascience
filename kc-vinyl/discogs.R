#----------------------------------------
# Environment
# - set working direcory
# - load libraries
#----------------------------------------
setwd("~/Dev/kc-vinyl")

library(rvest)
library(ggplot2)

#----------------------------------------
# Get data from discogs.com
#----------------------------------------
## Trying another method
html.discogs <- read_html('https://www.discogs.com/user/knclprt/collection?page=1&limit=250&header=1')

df.scrape_discogs <- html.discogs %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)

df.scrape_discogs <- df.scrape_discogs[c(3:6)]

ggplot(dplyr::filter(df.scrape_discogs, Year != 0), aes(x = Year)) + 
  geom_bar(stat = "count", fill = "#B79477") + 
  xlab("Jaar") +
  ylab("Aantal") +
  ggtitle("Jaar van release via scrape")