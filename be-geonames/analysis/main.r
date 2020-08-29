# Load needed packages
library("dplyr")
library("tidyr")
library("readr")
library("kableExtra")
library("ggplot2")
library("ggmap")

# Load initial data and create persistent dataset
steden <- read_tsv("data/BE.txt", col_names = FALSE)[, 1:6]
write.csv(steden, file = "./output/steden.csv")

# Start pattern search
# Names of rivers
aan_rivier <- steden %>%
  mutate(zee = grepl("aan-Zee", X2))  %>%
  mutate(schelde = grepl("aan-de-Schelde", X2))  %>%
  mutate(Leie = grepl("an-de-Leie", X2))  %>%
  mutate(rijn = grepl("aan-de-Rijn", X2)) %>%
  mutate(rijn = grepl("op-de-Rijn", X2)) %>%
  mutate(ijzer = grepl("aan-de-ijzer", X2)) %>%
  mutate(rhin = grepl("sur-Rhin", X2)) %>%
  mutate(sambre = grepl("sur-Sambre", X2)) %>%
  mutate(meuse = grepl("sur-Meuse", X2)) %>%
  gather("name", "yes", zee:meuse) %>%
  filter(yes == TRUE) %>%
  select(- yes)
knitr::kable(head(aan_rivier))

ggplot(data = aan_rivier, aes(x = name)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Naam rivier") + ylab("Aantal gemeenten") + ggtitle("Gemeenten 'aan-de-rivier'") +
  scale_fill_gradient(low = "lightgray", high = "black")

naam_rivier <- steden %>%
  mutate(Schelde = grepl("Schelde", X2))  %>%
  mutate(Leie = grepl("Leie", X2))  %>%
  mutate(Rijn = grepl("Rijn", X2)) %>%
  mutate(Ijzer = grepl("ijzer", X2)) %>%
  mutate(Rhin = grepl("Rhin", X2)) %>%
  mutate(Sambre = grepl("Sambre", X2)) %>%
  mutate(Meuse = grepl("Meuse", X2)) %>%
  gather("name", "yes", Schelde:Meuse) %>%
  filter(yes == TRUE) %>%
  select(- yes)
knitr::kable(head(naam_rivier))

ggplot(data = naam_rivier, aes(x = name)) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Naam rivier") + ylab("Aantal gemeenten") + ggtitle("Gemeenten met rivier in naam") +
  scale_fill_gradient(low = "lightgray", high = "black")

# Names of towns by river prefix
pref_schelde <- steden[grep("^Schelde.*?", steden$X2),]
pref_sambre <- steden[grep("^Sambre.*?", steden$X2),]
pref_meuse <- steden[grep("^Meuse.*?", steden$X2),]

# Merge all datasets with a prefix by river (there must be an elegant way with regex...)
pref_rivier <- rbind(pref_schelde, pref_sambre, pref_meuse)

# Get base map
map <- ggmap::get_map(location = "Belgium", zoom = 7, maptype = "roadmap", color = "bw")

# Names with prefix by river
ggmap(map) +
  geom_point(data = pref_schelde,
             aes(x = X6, y = X5)) +
  ggtitle("Plaatsnamen met Schelde") +
  theme(plot.title = element_text(lineheight=1, face="bold"))

# Names of towns by Saints
pref_sint <- steden[grep("^Sint.*?", steden$X2),]
pref_saint <- steden[grep("^Saint.*?", steden$X2),]
pref_halleluja <- rbind(pref_sint, pref_saint)
# or in 1 run
halleluja <- steden %>%
  mutate(saint = grepl("Saint-", X2))  %>%
  mutate(sainte = grepl("Sainte-", X2))  %>%
  mutate(sint = grepl("Sint-", X2))  %>%
  gather("name", "yes", saint:sint) %>%
  filter(yes == TRUE) %>%
  select(- yes)
knitr::kable(head(halleluja))

ggmap(map) +
  geom_point(data = halleluja,
             aes(x = X6, y = X5)) +
  ggtitle("Plaatsnamen met Sint/Saint/Sainte") +
  theme(plot.title = element_text(lineheight=1))

# In French there is a difference between female (Sainte) and male (Saint)
# How about plotting that huh?

ggmap(map) +
  geom_point(data = halleluja,
             aes(x = X6, y = X5)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=12),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Sint, Saint of Sainte?") +
  theme(plot.title = element_text(lineheight=1)) +
  facet_grid(. ~ name)

# Start geographical analysis on toponym
# write function to subset data
# for (i in seq_along(df)) {
#    uw grep code
#  }
# topo = readLines("./data/toponym.txt")
toponym <- steden %>%
  mutate(akker = grepl(".?akker", X2))  %>%
  mutate(aarde = grepl(".?aarde", X2))  %>%
  mutate(bake = grepl(".?bake", X2))  %>%
  mutate(bakel = grepl(".?bakel", X2))  %>%
  mutate(baken = grepl(".?baken", X2))  %>%
  mutate(balderij = grepl(".?balderij", X2))  %>%
  mutate(beek = grepl(".?beek", X2))  %>%
  mutate(beke = grepl(".?beke", X2))  %>%
  mutate(beemd = grepl(".?beemd", X2))  %>%
  mutate(berg = grepl(".?berg", X2))  %>%
  mutate(bergen = grepl(".?bergen", X2))  %>%
  mutate(bos = grepl(".?bos", X2))  %>%
  mutate(braak = grepl(".?braak", X2))  %>%
  mutate(brand = grepl(".?brand", X2))  %>%
  mutate(broek = grepl(".?broek", X2))  %>%
  mutate(brug = grepl(".?brug", X2))  %>%
  mutate(bruul = grepl(".?bruul", X2))  %>%
  mutate(burg = grepl(".?burg", X2))  %>%
  mutate(chem = grepl(".?chem", X2))  %>%
  mutate(daal = grepl(".?daal", X2))  %>%
  mutate(dael = grepl(".?dael", X2))  %>%
  mutate(del = grepl(".?del", X2))  %>%
  mutate(delle = grepl(".?delle", X2))  %>%
  mutate(dam = grepl(".?dam", X2))  %>%
  mutate(deel = grepl(".?deel", X2))  %>%
  mutate(dijk = grepl(".?dijk", X2))  %>%
  mutate(donk = grepl(".?donk", X2))  %>%
  mutate(doorn = grepl(".?doorn", X2))  %>%
  mutate(dorp = grepl(".?dorp", X2))  %>%
  mutate(drecht = grepl(".?drecht", X2))  %>%
  mutate(dries = grepl(".?dries", X2))  %>%
  mutate(gem = grepl(".?gem", X2))  %>%
  mutate(hestel = grepl(".?hestel", X2))  %>%
  mutate(hem = grepl(".?hem", X2))  %>%
  mutate(heem = grepl(".?heem", X2))  %>%
  mutate(hoek = grepl(".?hoek", X2))  %>%
  mutate(holt = grepl(".?holt", X2))  %>%
  mutate(hout = grepl(".?hout", X2))  %>%
  mutate(horn = grepl(".?horn", X2))  %>%
  mutate(horst = grepl(".?horst", X2))  %>%
  mutate(hoven = grepl(".?hoven", X2))  %>%
  mutate(hof = grepl(".?hof", X2))  %>%
  mutate(hoof = grepl(".?hoof", X2))  %>%
  mutate(hurk = grepl(".?hurk", X2))  %>%
  mutate(acum = grepl(".?acum", X2))  %>%  
  mutate(iacum = grepl(".?iacum", X2))  %>%
  mutate(ingen = grepl(".?ingen", X2))  %>%
  mutate(kapel = grepl(".?kapel", X2))  %>%
  mutate(kapelle = grepl(".?kapelle", X2))  %>%  
  mutate(capel = grepl(".?capel", X2))  %>%
  mutate(capelle = grepl(".?capelle", X2))  %>%
  mutate(kappel = grepl(".?kappel", X2))  %>%
  mutate(karspel = grepl(".?karspel", X2))  %>%
  mutate(kerspel = grepl(".?kerspel", X2))  %>%
  mutate(kerk = grepl(".?kerk", X2))  %>%
  mutate(kerke = grepl(".?kerke", X2))  %>%
  mutate(kerken = grepl(".?kerken", X2))  %>%
  mutate(klooster = grepl(".?klooster", X2))  %>%
  mutate(koog = grepl(".?koog", X2))  %>%
  mutate(laar = grepl(".?laar", X2))  %>%
  mutate(lies = grepl(".?lies", X2))  %>%
  mutate(lis = grepl(".?lis", X2))  %>%
  mutate(luisse = grepl(".?luisse", X2))  %>%
  mutate(luissel = grepl(".?luissel", X2))  %>%
  mutate(lo = grepl(".?lo", X2))  %>%
  mutate(loo = grepl(".?loo", X2))  %>%
  mutate(loon = grepl(".?loon", X2))  %>%
  mutate(made = grepl(".?made", X2))  %>%
  mutate(mede = grepl(".?mede", X2))  %>%
  mutate(maat = grepl(".?maat", X2))  %>%
  mutate(meet = grepl(".?meet", X2))  %>%
  mutate(mark = grepl(".?mark", X2))  %>%
  mutate(meer = grepl(".?meer", X2))  %>%
  mutate(molen = grepl(".?molen", X2))  %>%
  mutate(muiden = grepl(".?muiden", X2))  %>%
  mutate(munster = grepl(".?munster", X2))  %>%
  mutate(monster = grepl(".?monster", X2))  %>%
  mutate(nij = grepl(".?nij", X2))  %>%
  mutate(nie = grepl(".?nie", X2))  %>%
  mutate(fen = grepl(".?fen", X2))  %>%
  mutate(phen = grepl(".?phen", X2))  %>%
  mutate(pomp = grepl(".?pomp", X2))  %>%
  mutate(poort = grepl(".?poort", X2))  %>%
  mutate(rode = grepl(".?rode", X2))  %>%
  mutate(sel = grepl(".?sel", X2))  %>%
  mutate(selle = grepl(".?selle", X2))  %>%
  mutate(zele = grepl(".?zele", X2))  %>%
  mutate(sele = grepl(".?sele", X2))  %>%
  mutate(schot = grepl(".?schot", X2))  %>%
  mutate(schotte = grepl(".?schotte", X2))  %>%
  mutate(schoot = grepl(".?schoot", X2))  %>%
  mutate(speet = grepl(".?speet", X2))  %>%
  mutate(stad = grepl(".?stad", X2))  %>%
  mutate(terp = grepl(".?terp", X2))  %>%
  mutate(til = grepl(".?til", X2))  %>%
  mutate(um = grepl(".?um", X2))  %>%
  mutate(em = grepl(".?em", X2))  %>%
  mutate(trecht = grepl(".?trecht", X2))  %>%
  mutate(tricht = grepl(".?tricht", X2))  %>%
  mutate(vliet = grepl(".?vliet", X2))  %>%
  mutate(voort = grepl(".?voort", X2))  %>%
  mutate(voorde = grepl(".?voorde", X2))  %>%
  mutate(voord = grepl(".?voord", X2))  %>%
  mutate(vorst = grepl(".?vorst", X2))  %>%
  mutate(vroen = grepl(".?vroen", X2))  %>%
  mutate(vroon = grepl(".?vroon", X2))  %>%
  mutate(waard = grepl(".?waard", X2))  %>%
  mutate(wal = grepl(".?wal", X2))  %>%
  mutate(wals = grepl(".?wals", X2))  %>%
  mutate(wassen = grepl(".?wassen", X2))  %>%
  mutate(waes = grepl(".?waes", X2))  %>%
  mutate(werd = grepl(".?werd", X2))  %>%
  mutate(wierde = grepl(".?wierde", X2))  %>%
  mutate(werd = grepl(".?werd", X2))  %>%
  mutate(warden = grepl(".?warden", X2))  %>%
  mutate(wik = grepl(".?wik", X2))  %>%
  mutate(winkel = grepl(".?winkel", X2))  %>%
  mutate(wolde = grepl(".?wolde", X2))  %>%
  mutate(woud = grepl(".?woud", X2))  %>%
  mutate(woude = grepl(".?woude", X2))  %>%
  mutate(wolfs = grepl(".?wolfs", X2))  %>%
  mutate(zand = grepl(".?zand", X2))  %>%
  mutate(zande = grepl(".?zande", X2))  %>%
  mutate(zeel = grepl(".?zeel", X2))  %>%
  mutate(zeil = grepl(".?zeil", X2))  %>%
  mutate(zijl = grepl(".?zijl", X2))  %>%
  mutate(ziel = grepl(".?ziel", X2))  %>%
  mutate(zwet = grepl(".?zwet", X2))  %>%
  gather("toponym", "yes", akker:zwet) %>%
  filter(yes == TRUE) %>%
  select(- yes)
knitr::kable(head(toponym))

# Create a facet viz based on type of toponym
ggmap(map) +
  geom_point(data = toponym,
             aes(x = X6, y = X5)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=12),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Toponiemen in Vlaanderen") +
  theme(plot.title = element_text(lineheight=1)) +
  facet_wrap(~ toponym, ncol = 15)