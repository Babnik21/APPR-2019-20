# 3. faza: Vizualizacija podatkov

#starejši manj izgubljenih žog
izgubljene_36 <- per.36.stats[,c(3, 24)] 
izgubljene_36$TOV <- round(izgubljene_36$TOV)

izgubljene_annual <- annual.totals[,c(3, 4, 19)]
izgubljene_annual$TOV <- round(izgubljene_annual$TOV/izgubljene_annual$G)
izgubljene_annual <- izgubljene_annual[,c(1, 3)]

izgubljene_36 <- ddply(izgubljene_36, .(Age, TOV), nrow)
izgubljene_annual <- ddply(izgubljene_annual, .(Age, TOV), nrow)

graf_izgubljene_36 <- ggplot(izgubljene_36, aes(x = Age, y = TOV, fill=V1, color=V1, size = V1)) + 
  geom_point() + ggtitle("Povprečje izgubljenih žog na 36 min") +
  scale_x_discrete(breaks = c(20, 25, 30, 35, 40)) +  
  labs(fill = "Number of players") + scale_y_log10() +
  scale_color_gradient(low="green", high="purple") +
  scale_fill_gradient(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE)

graf_izgubljene_annual <- ggplot(izgubljene_annual, aes(x = Age, y = TOV, color = V1, fill=V1, size = V1)) + geom_point() +
  ggtitle("Povprečje izgubljenih žog na tekmo") + labs(size = "Number of players")+ 
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

graf_skoki_36 <- ggplot(skoki_36, aes(x=Age, y=TRB, size = V1,fill=V1 ,color=V1)) + geom_point() + 
  ggtitle("Povprečje skokov na 36 min") + labs(fill = "Number of players") + guides(color=FALSE) + 
  scale_color_gradient(low="green", high="purple") +
  scale_fill_gradient(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE) + scale_y_log10()

graf_skoki_annual <- ggplot(skok_annual, aes(x=Age, y=TRB)) + geom_point(aes(size = V1, fill=V1 ,color=V1)) + 
  ggtitle("Povprečje skokov na tekmo") + labs(fill = "Number of players") + 
  scale_color_gradient(low="green", high="purple") +
  scale_fill_gradient(low="green",high="purple") + 
  guides(color = FALSE, size = FALSE, fill=FALSE)

graf_skoki <- ggarrange(graf_skoki_36, graf_skoki_annual, nrow = 1, 
                        common.legend = TRUE, legend="bottom")




#višji igralci več skokov, blokad
graf_visina_skoki <- ggplot(per.36.stats %>% filter(TRB < 40), aes(x = Height, y=TRB)) + 
  geom_point() + geom_smooth(method = 'loess', color="green")

graf_visina_blokade <- ggplot(per.36.stats %>% filter(BLK < 30) , aes(x = Height, y = BLK)) +
  geom_point() + geom_smooth(method = 'loess', color="green")


graf_visina <- ggarrange(graf_visina_skoki, graf_visina_blokade, nrow =1)



#nižji več asistenc
nizji_ast_36 <- aggregate(AST~Height, per.36.stats[,c(28, 21)], mean)
nizji_ast_annual <- annual.totals %>%
  select(AST, Height, G)
nizji_ast_annual1 <- aggregate(AST~Height, nizji_ast_annual[,c(1, 2)], sum)
nizji_ast_annual2 <- aggregate(G~Height, nizji_ast_annual[,c(2, 3)], sum)
nizji_ast_annual <- left_join(nizji_ast_annual1, nizji_ast_annual2)
nizji_ast_annual$AST <- nizji_ast_annual$AST/nizji_ast_annual$G

graf_nizji_ast_36 <- ggplot(nizji_ast_36, aes(x=Height, y=AST)) + 
  geom_line() + 
  ggtitle("Povprečje asistenc igralcev z isto višino na 36 min")
graf_nizji_ast_annual <- ggplot(nizji_ast_annual, aes(x=Height, y=AST)) + 
  geom_line() + ggtitle("na tekmo")


graf_nizji_ast <- ggarrange(graf_nizji_ast_36, graf_nizji_ast_annual, nrow = 1)




#nižji boljši odtotki prostih metov in metov iz igre
nizji_ft_fg_36 <- per.36.stats[,c(28, 7, 8, 16, 17)]
nizji_ft_fg_annual <- annual.totals[,c(25, 24, 13, 21, 7)]


graf_nizji_ft_36 <- ggplot(nizji_ft_fg_36 %>% filter(FTA >= 2), aes(x=Height, y =FT_procent)) +
  geom_point() + ggtitle("Zadeti prosti meti na 36 min") +
  geom_smooth(color="purple")
graf_nizji_ft_annual <- ggplot(nizji_ft_fg_annual %>% filter(FTA >= 2), aes(x=Height, y =FT_procent)) +
  geom_point() + ggtitle("Zadeti prosti meti na tekmo") +
  geom_smooth(color="purple")
graf_nizji_fg_36 <- ggplot(nizji_ft_fg_36 %>% filter(FGA >= 2), aes(x=Height, y =FG_procent)) +
  geom_point() + ggtitle("Zadeti meti iz igre na 36 min") +
  geom_smooth(color="green")
graf_nizji_fg_annual <- ggplot(nizji_ft_fg_annual %>% filter(FGA >= 2), aes(x=Height, y =FG_procent)) +
  geom_point() + ggtitle("Zadeti meti iz igre na tekmo") +
  geom_smooth(color="green")





#zemljevid
zemljevid <- map_data("world")
igralci_drzave <- annual.totals[!duplicated(annual.totals[,c('Player')]),][,c(1, 28)]
drzave <- data.frame(table(igralci_drzave$Birth_State))
amerika <- c(intersect(drzave$Var1, state.name))
vsota_ameriskih <- 0
for (state in amerika) {
  vsota_ameriskih <- vsota_ameriskih + drzave$Freq[drzave$Var1 == state]
}

'%ni%' <- Negate('%in%')
USA <- data.frame("USA", vsota_ameriskih)
names(USA) <- c("Var1", "Freq")
drzave <- rbind(drzave, USA)
drzave <- drzave %>% filter(Var1  %ni% amerika)
names(drzave)[1] <- "region"
zemljevid <- full_join(drzave, zemljevid, by= "region")


 

map_svet <- ggplot(zemljevid, aes(x = long, y = lat, group=group)) +
  geom_polygon(data = zemljevid %>% filter(region != "USA"), aes(fill=Freq)) +
  scale_fill_gradientn(colors=c("blue", "yellow", "green")) +
  geom_polygon(data = zemljevid %>% filter(region == "USA"), fill = "red")+
  theme_void()+ theme(legend.position="right") +labs(fill="Število košarkašev")



#zemljevid - ZDA

amerika_states <- data.frame(table(igralci_drzave$Birth_State)) %>%
  filter(Var1 %in% state.name)
names(amerika_states)[1] <- "region"
amerika_states$region <- tolower(amerika_states$region)
amerika_states <- full_join(amerika_states, map_data("state"), by="region")

zemljevid_zda <- ggplot(amerika_states, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Freq)) + theme_void() +
  scale_fill_gradientn(colors=c("blue", "yellow", "green")) +
  labs(fill="Število košarkašev")

#AST TOV ratio
ast_tov <- annual.totals[c("Height", "AST", "TOV")]
ast_tov$ast.tov <- round(ast_tov$AST/ast_tov$TOV, 2)
ast_tov <- ast_tov[c(1, 4)]

graf_ast_tov <- ggplot(ast_tov, aes(x=Height, y=ast.tov)) + geom_point() +
  geom_smooth(color="purple")

