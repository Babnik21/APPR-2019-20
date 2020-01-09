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

players.rodovniski <- na.omit(players, invert=FALSE)
annual.totals <- seasons.stats %>% 
  select(Year, Player, Age, G, MP, FG, FGA, X3P, X3PA, X2P, 
         X2PA, FT, FTA, ORB, DRB, TRB, AST, BLK, TOV, STL, FG., X3P., X2P., FT.) %>%
  na.omit()
colnames(annual.totals) <- c("Year", "Player", "Age", "GP", "MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT",
                             "FTA", "ORB", "DRB", "TRB", "AST", "BLK", "TOV", "STL", "FG%", "3P%", "2P%", "FT%")

annual.totals <- annual.totals %>% group_by(Player, Year) %>% filter(row_number() == 1)




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




# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.