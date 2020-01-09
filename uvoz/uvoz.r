# 2. faza: Uvoz podatkov
library(readr)
library(dplyr)
library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(xml2)


#sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#uvozimo podatke iz csv datotek:
player.data <- read.csv("nba_data\\player_data.csv")
players <- read.csv("nba_data\\Players.csv", na="")
seasons.stats <- read.csv("nba_data\\Seasons_Stats.csv")
players$X <- NULL
seasons.stats$X <- NULL
players.rodovniski <- players %>% select(Player, height, weight, collage,
                                         birth_city)
colnames(players.rodovniski) <- c("Player", "Height", "Weight", "College", "Birth_State")

players.rodovniski <- players.rodovniski[!is.na(players.rodovniski$Birth_State), ]

#Uredimo tabelo s statističnimi podatki igralcev
annual.totals <- seasons.stats %>% 
  select(Year, Player, Age, G, MP, FG, FGA, X3P, X3PA, X2P, 
         X2PA, FT, FTA, ORB, DRB, TRB, AST, BLK, TOV, STL, FG., X3P., X2P., FT.) %>%
  na.omit()

colnames(annual.totals) <- c("Year", "Player", "Age", "GP", "MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT",
                             "FTA", "ORB", "DRB", "TRB", "AST", "BLK", "TOV", "STL", "FG%", "3P%", "2P%", "FT%")

annual.totals <- annual.totals %>% group_by(Player, Year) %>% filter(row_number() == 1) #Odstranimo odvečne vnose

#uvozimo iz spletne strani
per.36.stats <- NULL
for (i in seq(1980, 2017, 1)) {
  link <- sprintf("https://www.basketball-reference.com/leagues/NBA_%d_per_minute.html", i)
  stran <- read_html(link)
  tabelaena <- stran %>% html_node("table") %>% html_table(fill = TRUE)
  tabelaena$Year <- i
  per.36.stats <- rbind(per.36.stats, tabelaena) 
}
per.36.stats$GS <- NULL
per.36.stats$Tm <- NULL
na.omit(per.36.stats)

per.36.stats <- per.36.stats %>% group_by(Player, Year) %>% filter(row_number() == 1)

per.36.stats <- subset(per.36.stats, grepl('^\\d+$', per.36.stats$G))

# V tabelo s statistikami igralcev dodamo njihove osebne podatke
annual.totals <- merge(annual.totals, players.rodovniski, by="Player")
per.36.stats <- merge(per.36.stats, players.rodovniski, by="Player")




