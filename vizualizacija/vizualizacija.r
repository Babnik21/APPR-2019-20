# 3. faza: Vizualizacija podatkov

#starejši manj izgubljenih žog
izgubljene_36 <- per.36.stats[,c(3, 24)] 
izgubljene_36$TOV <- round(izgubljene_36$TOV)

izgubljene_annual <- annual.totals[,c(3, 4, 19)]
izgubljene_annual$TOV <- round(izgubljene_annual$TOV/izgubljene_annual$G)
izgubljene_annual <- izgubljene_annual[,c(1, 3)]

izgubljene_36 <- ddply(izgubljene_36, .(Age, TOV), nrow)
izgubljene_annual <- ddply(izgubljene_annual, .(Age, TOV), nrow)

graf_izgubljene_36 <- ggplot(izgubljene_36 %>% filter(TOV < 20), aes(x = Age, y = TOV, fill=V1, color=V1, size = V1)) + 
  geom_point() + ggtitle("Na 36 minut") +
  labs(fill = "Število košarkarjev") +
  scale_color_gradient(low="green", high="purple") +
  scale_fill_gradient(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE)

graf_izgubljene_annual <- ggplot(izgubljene_annual, aes(x = Age, y = TOV, color = V1, fill=V1, size = V1)) + geom_point() +
  ggtitle("Na tekmo")+ 
  scale_color_gradient(low="green", high="purple") +
  scale_fill_gradient(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE, fill=FALSE)


graf_izgubljene <- ggarrange(graf_izgubljene_36, graf_izgubljene_annual,
                             nrow = 1, common.legend = TRUE, legend="bottom")



#starejši manj skokov
skoki_36 <- per.36.stats[,c(3, 20)]
skoki_36$TRB <- round(skoki_36$TRB)

skok_annual <- annual.totals[,c(3, 4, 16)]
skok_annual$TRB <- round(skok_annual$TRB/skok_annual$G)
skok_annual <- skok_annual[,c(1, 3)]

skoki_36 <- ddply(skoki_36, .(Age, TRB), nrow)
skok_annual <- ddply(skok_annual, .(Age, TRB), nrow)

graf_skoki_36 <- ggplot(skoki_36 %>% filter(TRB < 30), aes(x=Age, y=TRB)) + geom_point(aes(size = V1,fill=V1 ,color=V1)) + 
  ggtitle("Na 36 minut") + labs(fill = "Število košarkarjev") + 
  scale_color_gradient(low="green", high="purple") +
  scale_fill_continuous(low="green",high="purple", breaks = c(50, 150, 250)) + 
  guides(color = FALSE, size = FALSE)

graf_skoki_annual <- ggplot(skok_annual, aes(x=Age, y=TRB)) + geom_point(aes(size = V1, fill=V1 ,color=V1)) + 
  ggtitle("Na tekmo") + 
  scale_color_gradient(low="green", high="purple") +
  scale_fill_continuous(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE, fill=FALSE)

graf_skoki <- ggarrange(graf_skoki_36, graf_skoki_annual, nrow = 1, 
                        common.legend = TRUE, legend="bottom")




#višji igralci več skokov, blokad
graf_visina_skoki <- ggplot(per.36.stats %>% filter(TRB < 30), aes(x = Height, y=TRB)) + 
  geom_point() + geom_smooth(method = 'loess', color="green") +
  ggtitle("Skoki")

graf_visina_blokade <- ggplot(per.36.stats %>% filter(BLK < 30) , aes(x = Height, y = BLK)) +
  geom_point() + geom_smooth(method = 'loess', color="green") +
  ggtitle("Blokade")


graf_visina <- ggarrange(graf_visina_skoki, graf_visina_blokade, nrow =1)



#nižji več asistenc
nizji_ast_36 <- aggregate(AST~Height, per.36.stats[,c(28, 21)], mean)

nizji_ast_annual <- annual.totals %>%
  select(AST, Height, G)
nizji_ast_annual_ast <- aggregate(AST~Height, nizji_ast_annual[,c(1, 2)], sum)
nizji_ast_annual_game <- aggregate(G~Height, nizji_ast_annual[,c(2, 3)], sum)
nizji_ast_annual <- left_join(nizji_ast_annual_ast, nizji_ast_annual_game)
nizji_ast_annual$AST <- round(nizji_ast_annual$AST/nizji_ast_annual$G, 2)

graf_nizji_ast_36 <- ggplot(nizji_ast_36, aes(x=Height, y=AST)) + 
  geom_line() + 
  ggtitle("Na 36 minut")

graf_nizji_ast_annual <- ggplot(nizji_ast_annual, aes(x=Height, y=AST)) + 
  geom_line() + ggtitle("Na tekmo")


graf_nizji_ast <- ggarrange(graf_nizji_ast_36, graf_nizji_ast_annual, nrow = 1)



#nižji boljši odtotki prostih metov in metov iz igre
nizji_ft_fg_36 <- per.36.stats[,c(28, 7, 8, 16, 17)]
nizji_ft_fg_annual <- annual.totals[,c(25, 24, 13, 21, 7)]


graf_nizji_ft_36 <- ggplot(nizji_ft_fg_36 %>% filter(FTA >= 2), aes(x=Height, y =FT_procent)) +
  geom_point() + ggtitle("Uspešnost pri FT na 36 min") +
  geom_smooth(color="purple")

graf_nizji_ft_annual <- ggplot(nizji_ft_fg_annual %>% filter(FTA >= 2), aes(x=Height, y =FT_procent)) +
  geom_point() + ggtitle("Uspešnost pri FT na tekmo") +
  geom_smooth(color="purple")

graf_nizji_fg_36 <- ggplot(nizji_ft_fg_36 %>% filter(FGA >= 2), aes(x=Height, y =FG_procent)) +
  geom_point() + ggtitle("Uspešnost pri FG na 36 min") +
  geom_smooth(color="green")

graf_nizji_fg_annual <- ggplot(nizji_ft_fg_annual %>% filter(FGA >= 2), aes(x=Height, y =FG_procent)) +
  geom_point() + ggtitle("Uspešnost pri FG na tekmo") +
  geom_smooth(color="green")


#zemljevid
zemljevid <- map_data("world")
annual.totals$Birth_State <- gsub("District of Columbia", "Maryland", annual.totals$Birth_State)
igralci_drzave <- annual.totals[!duplicated(annual.totals[,c('Player')]),][,c(1, 28)]
drzave <- data.frame(table(igralci_drzave$Birth_State))
vsota_ameriskih <- 0
for (state in drzave$Var1) {
  if (state %in% state.name) {
  vsota_ameriskih <- vsota_ameriskih + drzave$Freq[drzave$Var1 == state]
  }
}

'%ni%' <- Negate('%in%')
USA <- data.frame("USA", vsota_ameriskih)
names(USA) <- c("Var1", "Freq")
drzave <- rbind(drzave, USA)
drzave <- drzave %>% filter(Var1  %ni% state.name)
names(drzave)[1] <- "region"
zemljevid <- full_join(drzave, zemljevid, by= "region")


 

map_svet <- ggplot(zemljevid, aes(x = long, y = lat, group=group)) +
  geom_polygon(data = zemljevid %>% filter(region != "USA"), aes(fill=Freq)) +
  scale_fill_gradientn(colors=c("blue", "yellow", "green")) +
  geom_polygon(data = zemljevid %>% filter(region == "USA"), fill = "red")+
  theme_void()+ theme(legend.position="bottom") +labs(fill="Število košarkarjev")


#zemljevid - ZDA

amerika_states <- data.frame(table(igralci_drzave$Birth_State)) %>%
  filter(Var1 %in% state.name)
names(amerika_states)[1] <- "region"
amerika_states$region <- tolower(amerika_states$region)
amerika_states <- full_join(amerika_states, map_data("state"), by="region")

zemljevid_zda <- ggplot(amerika_states, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Freq)) + theme_void() +
  scale_fill_gradientn(colors=c("blue", "yellow", "green")) +
  labs(fill="Število košarkarjev")


#AST TOV ratio
drzave_ast_tov <- annual.totals[c("Birth_State", "TOV", "AST")] %>% 
  group_by(Birth_State) %>% summarise_each(funs(sum))
vsota_ast <- 0
vsota_tov <- 0
for (state in drzave_ast_tov$Birth_State) {
  if (state %in% state.name) {
    vsota_ast <- vsota_ast + drzave_ast_tov$AST[drzave_ast_tov$Birth_State == state]
    vsota_tov <- vsota_tov + drzave_ast_tov$TOV[drzave_ast_tov$Birth_State == state]
    }
}

USA_ast_tov <- data.frame("USA", vsota_tov, vsota_ast)
names(USA_ast_tov) <- c("Birth_State", "TOV", "AST")
drzave_ast_tov <- rbind(drzave_ast_tov, USA_ast_tov)
drzave_ast_tov <- drzave_ast_tov %>% filter(Birth_State  %ni% state.name)
names(drzave_ast_tov)[1] <- "region"
drzave_ast_tov$ast_tov <- round(drzave_ast_tov$AST/drzave_ast_tov$TOV, 2)

zemljevid_ast_tov <- full_join(drzave_ast_tov, zemljevid, by="region")

map_ast_tov <- ggplot(zemljevid_ast_tov, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = ast_tov)) + theme_void() +
  scale_fill_gradientn(colors=c("blue", "yellow", "green")) + 
  labs(fill ="AST/TOV")



