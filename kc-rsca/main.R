#----------------------------------------
# Environment
# - set working direcory
# - load libraries
#----------------------------------------
setwd("C:/Users/colpaeko/OneDrive/Vault/Vlaamse Overheid/Projecten/R Projects/kc-rsca")

library("rvest")
library("tidyr")
library("ggplot2")

#===========================
# SCRAPE DATA FROM WIKIPEDIA
#===========================
html.eurogames <- read_html('https://nl.wikipedia.org/wiki/Lijst_van_Europese_wedstrijden_van_RSC_Anderlecht')
html.players <- read_html('https://nl.wikipedia.org/wiki/Lijst_van_spelers_van_RSC_Anderlecht')
html.trainers <- read_html('https://nl.wikipedia.org/wiki/Lijst_van_trainers_van_RSC_Anderlecht')

df.scrape_eurogames <- html.eurogames %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)

df.scrape_players <- html.players %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)
write.csv(df.scrape_players, file = "./output/players.csv")

df.scrape_trainers <- html.trainers %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)
write.csv(df.scrape_trainers, file = "./output/trainers.csv")

#===========================
# DATA FRAME MANIPULATIONS
#===========================

## We have a problem in obs 172
## Creating csv and cleaning manual
write.csv(df.scrape_eurogames, file = "./output/eurogames_legs.csv")
## Change value in obs 172
df_eurogames_legs <- read.csv("./output/eurogames_legs.csv")

## Created date frame with separate columns for European Games
df_eurogames_legs = df_eurogames_legs %>%
  separate(Score, into = c("Leg 1", "Leg 2", "Testmatch"), sep = ",")
write.csv(df_eurogames_legs, file = "./output/eurogames_legs.csv")

## Keep working with new csv
df_eurogames_legs <- read.csv("./output/eurogames_legs.csv")

## Remove duplicate sequencing
df_eurogames_legs <- df_eurogames_legs[c(3:11)]

## Open this thing in Excel to get rid of the nesting




