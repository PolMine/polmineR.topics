# 
# ggplot(
#   data=foo,
#   aes(x=year, y=count, group=topic, colour=topic)) +
#   geom_area(aes(colour=topic, fill=topic)) 
# +
#   scale_color_manual(values=rep("grey", times=count))
# 
# # aufgrund Bündelung erforderliche Neuberechnung
# 
# 
# colnames(yearTopicLong2) <- c("year", "topic", "nDoc")
# meanNDoc <- apply(as.matrix(ftable(xtabs(nDoc~year+topic, yearTopicLong2))), 2, mean)
# meanNDoc <- meanNDoc[order(meanNDoc, decreasing=T)]
# yearTopicLong2$topic <- ordered(yearTopicLong2$topic, levels=names(meanNDoc))
# meanNDoc <- meanNDoc[which(meanNDoc > 0)]
# yearTopicLong2min <- subset(yearTopicLong2, topic %in% names(meanNDoc))
# yearTopicLong2min$topic <- droplevels(yearTopicLong2min$topic)
# # yearTopicLong2min$topic <- ordered(yearTopicLong2min$topic, levels=names(meanNDoc))
# # reorder(yearTopicLong2$year, yearTopicLong2$topic)
# ```
# 
# ```{r}
# d <- ggplot(data=yearTopicLong2min, aes(x=year, y=nDoc, group=topic, colour=topic, fill=topic))
# d <- d + geom_area(aes(x=year, y=nDoc, order=topic), stat="identity")
# d <- d + scale_color_manual(values=rep("grey", times=nTopics))
# d <- d + scale_x_discrete(breaks=seq(1945, 2015, 5))
# d <- d + ylab("Anteil der Dokumente pro Topic-Klassifikation")
# d <- d + xlab("Jahr")
# d
# ```
# 
# ** Trendverläufe der Themen **
# 
# ```{r}
# plotList <- lapply(
#   levels(yearTopicLong2min$topic),
#   function(selectTopic){
#     oneTopicData <- subset(yearTopicLong2min, topic == selectTopic)
#     d <- ggplot(oneTopicData, aes(x=year, y=nDoc, colour=topic, group=topic))
#     d <- d + geom_line(size=1.2)
#     d <- d + scale_x_discrete(breaks=seq(from=1950, to=2010, by=10))
#     d <- d + scale_y_continuous(breaks=seq(0, 1, by=0.1), limits=c(0,0.4))
#     d <- d + xlab(NULL)
#     d <- d + ylab(NULL)
#     d <- d + theme(legend.position="none")
#     d <- d + labs(title=selectTopic)
#     d
# })
# do.call(grid.arrange, plotList)
# ```
# 
# 
# ** Trendverlauf - Intensität der Ausprägung **
# 
# ```{r}
# theta2 <- as.data.frame(theta)
# rownames(theta2) <- dimnames(dtm)[["Docs"]]
# year <- as.factor(gsub("^(\\d+)_.*$", "\\1", rownames(theta2)))
# maxY <- 0.3
# 
# plotList <- lapply(
#   c(1:ncol(theta2)),
#   function(n){
#     topicNoModel <- RJSONIO::fromJSON(json)$topic.order[n]
#     topicData <- ddply(.data=theta2, .variables=.(year), .fun=function(tab){
#       distribution <- hist(tab[,topicNoModel], breaks=seq(0.0, 1.0, 0.1), plot=FALSE)$counts
#     })
#     rownames(topicData) <- topicData$year
#     topicData <- as.matrix(topicData[,2:ncol(topicData)])
#     topicData <- topicData / rowSums(topicData)
#     colnames(topicData) <- c(as.character(seq(0.1, 1.0, 0.1)))
#     # topicData[,1] <- apply(topicData, 1, function(x) 1 - sum(x[2:length(x)]) - min(topicData[,1]) )
#     # topicData[,1] <- apply(topicData, 1, function(x) 1 - sum(x[2:length(x)]) - 0.60 )
#     topicData <- t(apply(topicData, 1, function(x){
#       posBigger <- which(cumsum(x) >= (1-maxY))
#       x[1:(posBigger[1]-1)] <- 0
#       x[posBigger[1]] <- 1 - (1-maxY) - sum(x[(posBigger[1]+1):length(x)])
#       x
#     }))
#     # topicData[,1] <- 0
#     # topicData[,2] <- apply(topicData, 1, function(x) 1 - sum(x[3:length(x)]) - 0.50 )
#     topicDataLong <- melt(topicData)
#     colnames(topicDataLong) <- c("year", "Gruppierung", "value")
#     topicDataLong$Gruppierung <- factor(topicDataLong$Gruppierung)
#     topicDataLong$Gruppierung <- factor(topicDataLong$Gruppierung, levels=rev(levels(topicDataLong$Gruppierung)))
#     topicDataLong <- droplevels(topicDataLong)
#     d <- ggplot(
#       data=topicDataLong, aes(x=year, y=value, group=Gruppierung, colour=Gruppierung, fill=Gruppierung)
#       )
#     d <- d + geom_area(aes(x=year, y=value, order=Gruppierung), stat="identity")
#     d <- d + scale_fill_manual(values=c("#000000", rev(brewer.pal(9, "Blues"))))
#     d <- d + scale_color_manual(values=c("#000000", rev(brewer.pal(9, "Blues"))))
#     d <- d + scale_y_continuous(breaks=seq(0, 0.5, 0.1))
#     d <- d + scale_x_continuous(breaks=seq(1950, 2010, 10))
#     d <- d + ylab(NULL)
#     d <- d + xlab(NULL)
#     d <- d + theme(legend.position = "none") 
#     d <- d + labs(title=levels(yearTopicLong$topic)[n])
#     d
#   })
# names(plotList) <- levels(yearTopicLong$topic)
# if (query == "Vielfalt"){
#  plotsToRemove <- c("Entscheidungsfindung", "Gesellschaftskritik", "NA", "Tagesverlauf", "Geschlechterverhältnisse", "Historische Ereignisse", "Politische Debatten", "Fortschritt", "Historische Persönlichkeiten")
# for (toRemove in plotsToRemove) plotList[[toRemove]] <- NULL
# plotListFinal <- plotList
# } else if (query == "Integration"){
#   plotsToRemove <- c("Wirtschaft", "Auslandsberichterstattung", "Justiz", "Deutsche Regionen", "Nation und Identität", "Hochschule", "Kunst und Kultur", "Literatur", "Lokalpolitik", "Berliner Republik", "Mittel- und Osteuropa", "Großbritannien", "(ehem.) UdSSR", "Medizin", "Konflikt und Verteidigung", "Parteien", "Bundespolitik")
#   plotsToKeep <- c("EG", "Frankreich", "EU-Mitgliedstaaten", "Finanzen und Währung", "Sicherheitspolitik", "Ost-West-Konflikt", "Plurale Gesellschaften", "Arbeitsmarkt", "Schule", "Migration", "Religion")
#   plotListFinal <- list()
#   for (i in plotsToKeep) plotListFinal[[i]] <- plotList[[i]]
# }
# 
# do.call(grid.arrange, plotListFinal)
# ```