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

print(graf_izgubljene)


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

print(graf_skoki)


