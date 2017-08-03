rm(list = ls())

library(igraph)
library(magrittr)
library(pheatmap)
library(ggplot2)




BaseDir <- "~/Google Drive/Manuscripts/Thrones"

setwd(BaseDir)

load("NetworksBySeason/Season1.Rdata")
load("NetworksBySeason/Season2.Rdata")
load("NetworksBySeason/Season3.Rdata")
load("NetworksBySeason/Season4.Rdata")
load("NetworksBySeason/Season5.Rdata")
load("NetworksBySeason/Season6.Rdata")



names(EpisodeCollection1) <- paste("S1E", 1:length(EpisodeCollection1), sep = '')
names(EpisodeCollection2) <- paste("S2E", 1:length(EpisodeCollection2), sep = '')
names(EpisodeCollection3) <- paste("S3E", 1:length(EpisodeCollection3), sep = '')
names(EpisodeCollection4) <- paste("S4E", 1:length(EpisodeCollection4), sep = '')
names(EpisodeCollection5) <- paste("S5E", 1:length(EpisodeCollection5), sep = '')
names(EpisodeCollection6) <- paste("S6E", 1:length(EpisodeCollection6), sep = '')




AllNet <- append(EpisodeCollection1, EpisodeCollection2) %>%
  append(., EpisodeCollection3) %>%
  append(., EpisodeCollection4) %>%
  append(., EpisodeCollection5) %>%
  append(., EpisodeCollection6)


AddMissingNodes <- function(x, AllNodes) {
  ToAdd <- setdiff(AllNodes, V(x)$name)
  if(length(ToAdd)>0){
    return(add.vertices(x, length(ToAdd), attr = list(name = ToAdd)))
  }
  x
}

AllNodes <- lapply(AllNet, V) %>%
  lapply(., names) %>%
  unlist(., use.names = FALSE) %>%
  unique(.)


AllNet.Plus <- lapply(AllNet, AddMissingNodes, AllNodes = AllNodes)


EPNameSeason <- strsplit(x = names(AllNet), split = "E") %>%
  lapply(., "[[", 1) %>%
  unlist %>%
  data.frame(Season = .)
rownames(EPNameSeason) <- names(AllNet)





AllDeg <- sapply(AllNet.Plus, function(x){
  degree(x)[order(names(degree(x)))]
})

png("LucaFig/DegreeByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllDeg, cluster_cols = FALSE, main = "Degree", annotation_col = EPNameSeason)
dev.off()

png("LucaFig/DegreeCorrelationByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllDeg), method = "spe"), main = "Degree correlation (Houses)")
dev.off()

png("LucaFig/DegreeCorrelationByEpisodes.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllDeg, method = "spe"), main = "Degree correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()




AllCent <- sapply(AllNet.Plus, function(x){
  eigen_centrality(x)$vector[order(names(eigen_centrality(x)$vector))]
  })


png("LucaFig/EgCeByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllCent, cluster_cols = FALSE, main = "Eigenvector Centrality", annotation_col = EPNameSeason)
dev.off()

png("LucaFig/EgCeCorrByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllCent), method = "spe"), main = "Eigenvector Centrality correlation (Houses)")
dev.off()

png("LucaFig/EgCeCorrByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllCent, method = "spe"), main = "Eigenvector Centrality correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()








AllClos <- sapply(AllNet.Plus, function(x){
  closeness(x)[order(names(closeness(x)))]
})


png("LucaFig/ClosByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllClos, cluster_cols = FALSE, main = "Closeness", annotation_col = EPNameSeason)
dev.off()

png("LucaFig/ClosCorrByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllClos), method = "spe"), main = "Closeness correlation (Houses)")
dev.off()

png("LucaFig/ClosCorrByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllClos, method = "spe"), main = "Closeness correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()








AllBetw <- sapply(AllNet.Plus, function(x){
    betweenness(x)[order(names(betweenness(x)))]
  })

png("LucaFig/BeCeByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllBetw, cluster_cols = FALSE, main = "Betweenness Centrality", annotation_col = EPNameSeason)
dev.off()

AllBetw.RF <- AllBetw[sapply(apply(AllBetw, 1, unique), length)>1, ]
AllBetw.CF <- AllBetw[, sapply(apply(AllBetw, 2, unique), length)>1]

png("LucaFig/BeCeCorByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllBetw.RF), method = "spe"), main = "Betweenness Centrality correlation (Houses)")
dev.off()

png("LucaFig/BeCeCorByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllBetw.CF, method = "spe"), main = "Betweenness Centrality correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()











AllTrans <- sapply(AllNet.Plus, function(x){
  transitivity(x, type = "local")[order(V(x)$name)]
})

AllTrans[is.na(AllTrans)] <- 0
rownames(AllTrans) <- V(AllNet.Plus[[1]])$name


png("LucaFig/TransByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllTrans, cluster_cols = FALSE, main = "Transitivity", annotation_col = EPNameSeason)
dev.off()

AllTrans.RF <- AllTrans[sapply(apply(AllTrans, 1, unique), length)>1, ]
AllTrans.CF <- AllTrans[, sapply(apply(AllTrans, 2, unique), length)>1]

png("LucaFig/TransCorByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllTrans.RF), method = "spe"), main = "Transitivity correlation (Houses)")
dev.off()

png("LucaFig/TransCorByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllTrans.CF, method = "spe"), main = "Transitivity correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()














AllPRC <- sapply(AllNet.Plus, function(x){
  page.rank(x)$vector[order(names(page.rank(x)$vector))]
})

png("LucaFig/PRCByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(AllPRC, cluster_cols = FALSE, main = "PageRank Centrality", annotation_col = EPNameSeason)
dev.off()

AllPRC.RF <- AllPRC[sapply(apply(AllPRC, 1, unique), length)>1, ]
AllPRC.CF <- AllPRC[, sapply(apply(AllPRC, 2, unique), length)>1]

png("LucaFig/PRCCorByHouse.png", width = 800, height = 800)
pheatmap::pheatmap(cor(t(AllPRC.RF), method = "spe"), main = "PageRank Centrality correlation (Houses)")
dev.off()

png("LucaFig/PRCCorByEpisode.png", width = 800, height = 800)
pheatmap::pheatmap(cor(AllPRC.CF, method = "spe"), main = "PageRank Centrality correlation (Episodes)",
                   annotation_row = EPNameSeason, cluster_cols = FALSE)
dev.off()















AllAss <- sapply(AllNet.Plus, function(x){
    assortativity_degree(x)
  })

OrderedEpisodes <- factor(names(AllAss), levels = names(AllAss))


p <- ggplot(data.frame(Episode = OrderedEpisodes, Assortativity = AllAss, Season = EPNameSeason$Season),
       mapping = aes(x = Episode, y = Assortativity, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Degree assortativity")



png("LucaFig/DegreeAssByEpisode.png", width = 800, height = 800)
print(p)
dev.off()



















GlobalCent <- sapply(AllNet.Plus, function(x){
  c(centr_degree(x)$centralization,
    centr_clo(x)$centralization,
    centr_eigen(x)$centralization)
})



p <- ggplot(data.frame(Episode = OrderedEpisodes, Centrality = GlobalCent[1,], Season = EPNameSeason$Season),
            mapping = aes(x = Episode, y = Centrality, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Centrality by degree")


png("LucaFig/CentralityDegreeByEpisode.png", width = 800, height = 800)
print(p)
dev.off()


p <- ggplot(data.frame(Episode = OrderedEpisodes, Centrality = GlobalCent[2,], Season = EPNameSeason$Season),
            mapping = aes(x = Episode, y = Centrality, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Closeness")


png("LucaFig/ClosenessByEpisode.png", width = 800, height = 800)
print(p)
dev.off()



p <- ggplot(data.frame(Episode = OrderedEpisodes, Centrality = GlobalCent[3,], Season = EPNameSeason$Season),
            mapping = aes(x = Episode, y = Centrality, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Eigenvector centrality")


png("LucaFig/EigenvectorCentralityByEpisode.png", width = 800, height = 800)
print(p)
dev.off()











Diameter <- sapply(AllNet.Plus, function(x){
  diameter(x)
})



p <- ggplot(data.frame(Episode = OrderedEpisodes, Centrality = Diameter, Season = EPNameSeason$Season),
            mapping = aes(x = Episode, y = Centrality, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Diameter")


png("LucaFig/DiuameterByEpisode.png", width = 800, height = 800)
print(p)
dev.off()








Transitivity <- sapply(AllNet.Plus, function(x){
  transitivity(x, type = "global")
})



p <- ggplot(data.frame(Episode = OrderedEpisodes, Centrality = Transitivity, Season = EPNameSeason$Season),
            mapping = aes(x = Episode, y = Centrality, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Transitivity")


png("LucaFig/TransitivityByEpisode.png", width = 800, height = 800)
print(p)
dev.off()







VerProp <- sapply(AllNet.Plus, function(x){
    table(factor(E(x)$color, levels = c("blue", "red")))/ecount(x)
  })



barplot(VerProp, col = c("blue", "red"), las =2)


EdgRem <- rep(0, length(AllNet.Plus)-1)
for(i in 2:length(AllNet.Plus)){
  EdgRem[i-1] <- ecount(difference(AllNet.Plus[[i-1]], AllNet.Plus[[i]]))
}


p <- ggplot(data.frame(Episode = OrderedEpisodes[-1], Removal = EdgRem, Season = EPNameSeason$Season[-1]),
            mapping = aes(x = Episode, y = Removal, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Dropped interactions", y = "Number of interactions")

png("LucaFig/EdgRemByEpisode.png", width = 800, height = 800)
print(p)
dev.off()




EdgAdd <- rep(0, length(AllNet.Plus)-1)
for(i in 2:length(AllNet.Plus)){
  EdgAdd[i-1] <- ecount(difference(AllNet.Plus[[i]], AllNet.Plus[[i-1]]))
}


p <- ggplot(data.frame(Episode = OrderedEpisodes[-1], Addition = EdgAdd, Season = EPNameSeason$Season[-1]),
            mapping = aes(x = Episode, y = Addition, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Introduced interactions", y = "Number of interactions")

png("LucaFig/EdgAddByEpisode.png", width = 800, height = 800)
print(p)
dev.off()





EdgColCha <- rep(0, length(AllNet.Plus)-1)
for(i in 2:length(AllNet.Plus)){
  IntGR <- intersection(AllNet.Plus[[i]], AllNet.Plus[[i-1]])
  EdgColCha[i-1] <- sum(E(IntGR)$color_1 != E(IntGR)$color_2)
}



p <- ggplot(data.frame(Episode = OrderedEpisodes[-1], Change = EdgColCha, Season = EPNameSeason$Season[-1]),
            mapping = aes(x = Episode, y = Change, color = Season)) + geom_point(size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Introduced changed", y = "Number of interactions")

png("LucaFig/EdgChangeByEpisode.png", width = 800, height = 800)
print(p)
dev.off()



PlotMat <- rbind(EdgRem, EdgAdd, EdgColCha)
colnames(PlotMat) <- names(AllNet.Plus)[-1]
rownames(PlotMat) <- c("Lost", "Gained", "Changed")





png("LucaFig/EdgVarByEpisodeNC.png", width = 800, height = 800)
pheatmap::pheatmap(PlotMat, cluster_cols = TRUE, annotation_col = EPNameSeason)
dev.off()

png("LucaFig/EdgVarByEpisodeC.png", width = 800, height = 800)
pheatmap::pheatmap(PlotMat, cluster_cols = FALSE, annotation_col = EPNameSeason)
dev.off()




barplot(rowSums(PlotMat))


barplot(colSums(PlotMat), las=2)

Type1 <- make_empty_graph(3, FALSE) %>%
  add_edges(c(1,2), color = "blue") %>%
  add_edges(c(2,3), color = "blue") %>%
  add_edges(c(3,1), color = "blue")

Type2 <- make_empty_graph(3, FALSE) %>%
  add_edges(c(1,2), color = "red") %>%
  add_edges(c(2,3), color = "blue") %>%
  add_edges(c(3,1), color = "blue")

Type3 <- make_empty_graph(3, FALSE) %>%
  add_edges(c(1,2), color = "red") %>%
  add_edges(c(2,3), color = "red") %>%
  add_edges(c(3,1), color = "blue")

Type4 <- make_empty_graph(3, FALSE) %>%
  add_edges(c(1,2), color = "red") %>%
  add_edges(c(2,3), color = "red") %>%
  add_edges(c(3,1), color = "red")


ColNumb <- c("red" = 1, "blue"=2)


TriadCt <- sapply(AllNet.Plus, function(x){

  T1C <- count_subgraph_isomorphisms(target = x, pattern = Type1,
                                     method = "vf2", edge.color1 = ColNumb[E(x)$color],
                                     edge.color2 = ColNumb[E(Type1)$color])

  T2C <- count_subgraph_isomorphisms(target = x, pattern = Type2,
                                     method = "vf2", edge.color1 = ColNumb[E(x)$color],
                                     edge.color2 = ColNumb[E(Type2)$color])

  T3C <- count_subgraph_isomorphisms(target = x, pattern = Type3,
                                     method = "vf2", edge.color1 = ColNumb[E(x)$color],
                                     edge.color2 = ColNumb[E(Type3)$color])

  T4C <- count_subgraph_isomorphisms(target = x, pattern = Type4,
                                     method = "vf2", edge.color1 = ColNumb[E(x)$color],
                                     edge.color2 = ColNumb[E(Type4)$color])

  return(c(T1C/3, T2C/2, T3C/2, T4C/3))

})



rownames(TriadCt) <- paste("Ty", 1:4, sep='')







png("LucaFig/BalByEpisodeNC.png", width = 800, height = 800)
pheatmap::pheatmap(TriadCt, cluster_cols = FALSE, annotation_col = EPNameSeason)
dev.off()

png("LucaFig/BalByEpisodeC.png", width = 800, height = 800)
pheatmap::pheatmap(TriadCt, cluster_cols = TRUE, annotation_col = EPNameSeason)
dev.off()


BalMat <- rbind(
  colSums(TriadCt[c(1,3),]),
  colSums(TriadCt[c(2,4),])
  )

rownames(BalMat) <- c("Balanced", "Umbalanced")







png("LucaFig/BalPercByEpisodeNC.png", width = 800, height = 800)
pheatmap::pheatmap(100*t(t(BalMat)/colSums(BalMat)), cluster_cols = FALSE, cluster_rows = FALSE, annotation_col = EPNameSeason)
dev.off()

png("LucaFig/BalPercByEpisodeC.png", width = 800, height = 800)
pheatmap::pheatmap(100*t(t(BalMat)/colSums(BalMat)), cluster_cols = TRUE, cluster_rows = FALSE, annotation_col = EPNameSeason)
dev.off()






p <- ggplot(data = data.frame(Balanced = BalMat[1,], UnBalanced = BalMat[2,], Season = EPNameSeason$Season),
       mapping = aes(y = Balanced, x = Season, fill = Season)) + geom_boxplot() + guides(fill="none") +
  labs(title = "Balanced triads", y = "Number of triads", x = "Season")

png("LucaFig/BalBySeason.png", width = 800, height = 800)
print(p)
dev.off()

p <- ggplot(data = data.frame(Balanced = 100*BalMat[1,]/colSums(BalMat), UnBalanced = 100*BalMat[2,]/colSums(BalMat), Season = EPNameSeason$Season),
            mapping = aes(y = Balanced, x = Season, fill = Season)) + geom_boxplot() + guides(fill="none") +
  labs(title = "Percentage of balanced triads", y = "Percentage of triads", x = "Season")

png("LucaFig/PercBalBySeason.png", width = 800, height = 800)
print(p)
dev.off()







library("rvest")
url <- "http://www.imdb.com/title/tt0944947/epdate"

EpisodesRating <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="tn15content"]/table[1]') %>%
  html_table(., dec="//")

EpisodesRating <- EpisodesRating[[1]]

SelData <- data.frame(Code = paste("S", gsub(pattern = ".", x = EpisodesRating$`#`,  replacement = "E", fixed = TRUE), sep = ''),
           Rating = as.numeric(EpisodesRating$UserRating),
           Votes = as.integer(gsub(pattern = ",", x = EpisodesRating$UserVotes,  replacement = "", fixed = TRUE))
)

rownames(SelData) <- as.character(SelData$Code)



SelData$Votes
SelData$Rating

for(i in 1:7){
  Idx <- grep(pattern = paste("S", i, sep = ''), SelData$Code, fixed = TRUE)
  #SelData$Votes[Idx] <- (SelData$Votes[Idx]-min(SelData$Votes[Idx]))/(max(SelData$Votes[Idx]) - min(SelData$Votes[Idx]))
  #SelData$Rating[Idx] <- (SelData$Rating[Idx]-min(SelData$Rating[Idx]))/(max(SelData$Rating[Idx]) - min(SelData$Rating[Idx]))
  SelData$Votes[Idx] <- (SelData$Votes[Idx])/mean(SelData$Votes[Idx])
  SelData$Rating[Idx] <- (SelData$Rating[Idx])/(mean(SelData$Rating[Idx]))
}

library(reshape)




DegMelt <- melt(AllDeg)
DegMelt <- cbind(DegMelt,
                 SelData[as.character(DegMelt$X2), "Votes"],
                 SelData[as.character(DegMelt$X2), "Rating"]
)

colnames(DegMelt) <- c("Node", "Ep", "Deg", "Votes", "Rating")

ggplot(DegMelt, mapping = aes(x = Deg, group = Deg, y = Votes)) + geom_boxplot() + facet_wrap( ~ Node) +
  labs(title = "Degree VS Votes", x = "Degree", y = "Number of votes")

ggplot(DegMelt, mapping = aes(x = Deg, group = Deg, y = Rating)) + geom_boxplot() + facet_wrap( ~ Node) +
  labs(title = "Degree VS Rating", x = "Degree", y = "Rating")



DegCorTest <- lapply(split(DegMelt[, c("Deg", "Votes")], DegMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(DegCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'
VecData=data.frame("Nodes" = names(DegCorTest), Cor = unlist(lapply(DegCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data =VecData ,
       mapping = aes(x=Nodes, y = Cor, fill=LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Degree and Votes", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorDegreeVSVotes.png", width = 800, height = 800)
print(p)
dev.off()

DegCorTest <- lapply(split(DegMelt[, c("Deg", "Rating")], DegMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(DegCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'
VecData=data.frame("Nodes" = names(DegCorTest), Cor = unlist(lapply(DegCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data =VecData ,
            mapping = aes(x=Nodes, y = Cor, fill=LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Degree and Rating", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorDegreeVSRating.png", width = 800, height = 800)
print(p)
dev.off()








CentMelt <- melt(AllCent)
CentMelt <- cbind(CentMelt,
                 SelData[as.character(CentMelt$X2), "Votes"],
                 SelData[as.character(CentMelt$X2), "Rating"]
)

colnames(CentMelt) <- c("Node", "Ep", "Cent", "Votes", "Rating")

ggplot(CentMelt, mapping = aes(x = Cent, y = Votes)) + geom_point() + facet_wrap( ~ Node, scales = "free_x") +
  labs(title = "Centrality VS Votes", x = "Centrality", y = "Number of votes")

ggplot(CentMelt, mapping = aes(x = Cent, y = Rating)) + geom_point() + facet_wrap( ~ Node, scales = "free_x") +
  labs(title = "Centrality VS Rating", x = "Centrality", y = "Rating")


CentCorTest <- lapply(split(CentMelt[, c("Cent", "Votes")], CentMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(CentCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'
VecData=data.frame("Nodes" = names(CentCorTest), Cor = unlist(lapply(CentCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data =VecData ,
            mapping = aes(x=Nodes, y = Cor, fill=LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Centrality and Votes", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorCentralityVSVotes.png", width = 800, height = 800)
print(p)
dev.off()


CentCorTest <- lapply(split(CentMelt[, c("Cent", "Rating")], CentMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(CentCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'
VecData=data.frame("Nodes" = names(CentCorTest), Cor = unlist(lapply(CentCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data =VecData ,
            mapping = aes(x=Nodes, y = Cor, fill=LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Centrality and Rating", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorCentralityVSRating.png", width = 800, height = 800)
print(p)
dev.off()













BetwMelt <- melt(AllBetw)
BetwMelt <- cbind(BetwMelt,
                  SelData[as.character(BetwMelt$X2), "Votes"],
                  SelData[as.character(BetwMelt$X2), "Rating"]
)

colnames(BetwMelt) <- c("Node", "Ep", "Betw", "Votes", "Rating")

ggplot(BetwMelt, mapping = aes(x = Betw, y = Votes)) + geom_point() + facet_wrap( ~ Node, scales = "free_x") +
  labs(title = "Betweenness VS Votes", x = "Betweenness", y = "Number of votes")

ggplot(BetwMelt, mapping = aes(x = Betw, y = Rating)) + geom_point() + facet_wrap( ~ Node, scales = "free_x") +
  labs(title = "Betweenness VS Votes", x = "Betweenness", y = "Number of votes")


BetwCorTest <- lapply(split(BetwMelt[, c("Betw", "Votes")], BetwMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(BetwCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'

VecData=data.frame("Nodes" = names(BetwCorTest), Cor = unlist(lapply(BetwCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data =VecData ,
            mapping = aes(x=Nodes, y = Cor, fill = LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Betweenness and Votes", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorBetweennessVSVotes.png", width = 800, height = 800)
print(p)
dev.off()


BetwCorTest <- lapply(split(BetwMelt[, c("Betw", "Rating")], CentMelt$Node), function(x){cor.test(x[,1], x[,2], method = "spe")})
pVal <-  unlist(lapply(BetwCorTest, "[[", "p.value"))
pVal[is.na(pVal)] <- 1
VecLab=rep('',length(pVal))
VecLab[pVal>0.01&pVal<=0.05]='*'
VecLab[pVal>0.001&pVal<=0.01]='**'
VecLab[pVal<=0.001]='***'

VecData=data.frame("Nodes" = names(BetwCorTest), Cor = unlist(lapply(BetwCorTest, "[[", "estimate")), LogP = log10(pVal))
p <- ggplot(data = VecData,
            mapping = aes(x=Nodes, y = Cor, fill = LogP)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Betweenness and Rating", y = "Spearman correlation", x = "Houses")
p <-p+ geom_text(data=VecData,aes(label = VecLab),size=20)
png("LucaFig/CorBetweennessVSRating.png", width = 800, height = 800)
print(p)
dev.off()





DF <- data.frame(EP = names(AllAss),
                 Votes = SelData[names(AllAss), "Votes"],
                 Rating = SelData[names(AllAss), "Rating"])

DF <- melt(DF)
DF <- data.frame(Assortativity = AllAss[DF$EP],
                 blue = VerProp[1,DF$EP],
                 red = VerProp[2,DF$EP],
                 Ty1 = TriadCt[1,DF$EP],
                 Ty2 = TriadCt[2,DF$EP],
                 Ty3 = TriadCt[3,DF$EP],
                 Ty4 = TriadCt[4,DF$EP],
                 Transitivity = Transitivity[DF$EP],
                 Diameter = Diameter[DF$EP],
                 CentDeg = GlobalCent[1,DF$EP],
                 Closeness = GlobalCent[2,DF$EP],
                 CentEigen = GlobalCent[3,DF$EP],
                 DF)

for(i in 1:12){
  paste("p <- ggplot(DF, aes(x =", colnames(DF)[i], ", y = value))") %>%
    parse(text = .) %>%
    eval(., envir = .GlobalEnv)

  p <- p + geom_point() +
    facet_wrap(~variable, scales = "free_y") +
    labs(y = '')

  print(p)

  print(colnames(DF)[i])
  split(DF[,c(i, 15)], DF$variable) %>%
    lapply(., function(x) {
      cor.test(x[,1], x[,2], method = "spe")
    }) %>%
    print()

}

