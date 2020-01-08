# 2. faza: Uvoz podatkov
library(readr)
library(dplyr)
library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(xml2)
library(proto)

#sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#uvozimo podatke iz csv datotek:
player.data <- read.csv("nba_data\\player_data.csv")
players <- read.csv("nba_data\\Players.csv", na="")
seasons.stats <- read.csv("nba_data\\Seasons_Stats.csv")
players$X = NULL
seasons.stats$X = NULL

players.rodovniski <- na.omit(players, invert=FALSE)
annual.totals <- seasons.stats %>% 
  select(Year, Player, Age, G, MP, FG, FGA, X3P, X3PA, X2P, 
         X2PA, FT, FTA, ORB, DRB, TRB, AST, BLK, TOV, STL) %>%
  na.omit()

#uvozimo iz spletne strani
per.36.stats <- NULL
for (i in seq(1980, 2017, 1)) {
  link <- sprintf("https://www.basketball-reference.com/leagues/NBA_%d_per_minute.html", i)
  stran <- read_html(link)
  tabelaena <- stran %>% html_node("table") %>% html_table(fill = TRUE)
  tabelaena$Year <- i
  per.36.stats <- rbind(per.36.stats, tabelaena) 
}
  
# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.