#this version make a heatmap of of degegree distribution and 
#this code looks at global network properties of the networks 
Season1=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season1.Rdata'))
Season2=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season2.Rdata'))
Season3=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season3.Rdata'))
Season4=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season4.Rdata'))
Season5=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season5.Rdata'))
Season6=get(load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season6.Rdata'))
AllSeasons=list(Season1,Season2,Season3,Season4,Season5,Season6)
length(AllSeasons)

Target=list(Season1[[1]],Season2[[1]],Season3[[1]],Season4[[1]],Season5[[1]],Season6[[1]])


pdf('LargeScaleProperties.pdf')
#degree distribution 
par(mfrow=c(1,1),mar=c(8,5,5,5))
for (i in 1:length(AllSeasons)){
Vecplot=degree(graph = Target[[i]],v = V(Target[[i]]))
barplot(sort(Vecplot,decreasing = T),las=2,ylab='Degree',main=paste('Season',i,'Episode1'),col='brown')
}


#centrality 

par(mfrow=c(1,1),mar=c(8,5,5,5))
for (i in 1:length(AllSeasons)){
        Vecplot=closeness(graph = Target[[i]],v = V(Target[[i]]))
        barplot(sort(Vecplot,decreasing = T),las=2,ylab='Centrality',main=paste('Season',i,'Episode1'),col='blue')
}


par(mfrow=c(1,1))
#assortativity coefficient 
VecAnalysis=list()
for (j in 1:length(AllSeasons)){
        
        for (m in 1:length(AllSeasons[[j]])){
                VecAnalysis[[length(VecAnalysis)+1]]= AllSeasons[[j]][[m]]  
        } 
}
length(VecAnalysis)

VecPlot1=NULL
VecPlot2=NULL
for (i in 1:length(VecAnalysis)){
        Target=VecAnalysis[[i]]
        VecPlot1=c(VecPlot1,assortativity(Target,types1 = degree(Target), directed = F) ) #assortativity coefficient by degree
        VecPlot2=c(VecPlot2,assortativity(Target,types1 = closeness(Target), directed = F) ) #assortativity coefficient by closeness
}
plot(VecPlot1,col='brown',type='p',pch=8,ylab ='Assortativity score by degree',main='Assortativity by degree',xlab='Episode number')
plot(VecPlot2,col='blue',type='p',pch=8,ylab ='Assortativity score by centrality',main='Assortativity by centrality',xlab='Episode number')



dev.off()
