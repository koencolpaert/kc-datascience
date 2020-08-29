#----------------------------------------
# Game analysis
# - Gathering of data for games per season
# - Cleaning of data
#----------------------------------------

setwd("C:/Users/colpaeko/OneDrive/Vault/Vlaamse Overheid/Projecten/R Projects/kc-rsca")

library("rvest")
library("tidyr")
library("ggplot2")

# Building a set of data for game results 

#===========================
# SCRAPE DATA FROM WIKIPEDIA
#===========================

html.games1415 <- read_html('https://nl.wikipedia.org/wiki/RSC_Anderlecht_in_het_seizoen_2014/15')

df.scrape_games1415 <- html.games1415 %>%
  html_nodes("table") %>%
  .[[14]] %>%
  html_table(fill = TRUE)
