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
# MAKING SOME BASE PLOTS
#===========================
## Players per country
ggplot(data = df.scrape_players, aes(x = Land)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Land") + ylab("Aantal") + ggtitle("Spelers per land") +
  scale_fill_gradient(low = "#d6b4e7", high = "#793698") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./figures/players_country.png")

## Non-Belgian players
df.scrape_players_foreign <- df.scrape_players[df.scrape_players$Land>"België",]

ggplot(data = df.scrape_players_foreign, aes(x = Land)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Land") + ylab("Aantal") + ggtitle("Buitenlandse spelers per land") +
  scale_fill_gradient(low = "#d6b4e7", high = "#793698") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./figures/players_country_foreign.png")