#this is code to catch the change of edges 
GetTriads<-function(Target){
        library('igraph')
        Template<-graph.ring(n = 3,directed = F,circular = T)
        
        Target<-Target
        
        VeticesList<-graph.get.subisomorphisms.vf2(graph2 = Template,graph1 = Target)#here, we get 6 graphs from each trangle, let's get rid of 5
        
        
        ##this is the process to get rid of all the indentical traids
        
        N<-1
        repeat{
                N<-N+1
                if (N>length(VeticesList)){break}
                VecToRemove<-NULL
                for (i in (c(1:length(VeticesList))[-N])){
                        if(sum(names(VeticesList[[i]])%in%names(VeticesList[[N]]))==3){VecToRemove<-c(VecToRemove,i)}
                }
                VeticesList<-VeticesList[-VecToRemove]
        }
        
        
        
        TriadList<-lapply(X = VeticesList,FUN = function(x){induced.subgraph(graph = Target,vids = x)})#get all tge triads in the network
        return(TriadList)
}


#This is the function to calculate whether a triad is stable

BalanceCaculation<-function(Target.Triangle){
        for (i in 1:length(E(Target.Triangle))){ #introduce relationIndex, friendly is 1 and against each other is -1
                if(E(Target.Triangle)[i]$weight==1)  {E(Target.Triangle)[i]$RelationIndex<-1}
                if(E(Target.Triangle)[i]$weight==2)  {E(Target.Triangle)[i]$RelationIndex<--1}
        }
        
        if (prod(E(Target.Triangle)$RelationIndex)==1){Output<-0} else if (prod(E(Target.Triangle)$RelationIndex)==-1){Output<-1} 
        
        return(Output) #if the output=0 => balanced if output=1 =>imblanced
}


#function to calculate and summarise structure balance of the whole graph
CalculateTheGraphBalance<-function(TriadList){
        BalanceReport<-unlist(lapply(X =TriadList,FUN =BalanceCaculation  ))
        names(BalanceReport)<-unlist(lapply(X =TriadList,FUN =function(x){paste(V(x)$name,collapse = ' - ')}  ))
        BalanceRatio<-sum(BalanceReport)/length(BalanceReport)
        return(list(BalanceReport,BalanceRatio))
        
}




#Track each triads

##Function to asign a state to the triad , 1,2,3,4 

CalState<-function(TargetTriad){
        
        VecSum<-sum(E(TargetTriad)$weight==2)#this is weight not relation index
        StateOfTriad<-NULL
        if (VecSum==0){StateOfTriad<-1}
        if (VecSum==1){StateOfTriad<-2}
        if (VecSum==2){StateOfTriad<-3}
        if (VecSum==3){StateOfTriad<-4}
        names(StateOfTriad)<-paste(V(TargetTriad)$name,collapse = '-')
        return(StateOfTriad)}

#Get a matix of triads and episodes of a single season(1)



#Do the analysis to all seasons 

load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season1.Rdata')
load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season2.Rdata')
load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season3.Rdata')
load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season4.Rdata')
load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season5.Rdata')
load('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/NetworksBySeason/Season6.Rdata')
AllSeasons<-list(EpisodeCollection1,EpisodeCollection2,EpisodeCollection3,EpisodeCollection4,EpisodeCollection5,EpisodeCollection6)
#EpisodeCollection3
#EpisodeCollection5
#SHOW one of the episode, say 4.10



# xV<-c(0,0,-0.1,-0.03,0.1,0,-0.1,0.1,0,-0.08,0.1,0.15,0.1,0.15,-0.03)
# names(xV)<-names(V(House.Graph))
# yV<-c(1,0.8,0.6,0.4,0.8,0,-0.3,-0.4,-0.5,0,0.15,-0.15,0.4,0.4,-0.8)
# names(yV)<-names(V(House.Graph))
# LayoutMatrix<-cbind(xV,yV)
# 
# plot.igraph(AllSeasons[[4]][[10]],layout=LayoutMatrix)
# 
# E(AllSeasons[[4]][[10]])$color





#give a demostration of structure balance 

par(mfrow=c(2,2))

Triad<-graph.ring(n = 3,directed = F,circular = T)
E(Triad)$weight<-c(1,1,1)
E(Triad)$color<-rep('blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'red'

xV<-c(84,381,232)
yV<-c(0,1,261)
LayoutMatrix<-cbind(xV,yV)
plot.igraph(Triad,layout=LayoutMatrix)

E(Triad)$weight<-c(1,1,2)
E(Triad)$color<-rep('blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'red'
plot.igraph(Triad,layout=LayoutMatrix)


E(Triad)$weight<-c(1,2,2)
E(Triad)$color<-rep('blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'red'
plot.igraph(Triad,layout=LayoutMatrix)

E(Triad)$weight<-c(2,2,2)
E(Triad)$color<-rep('blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'red'
plot.igraph(Triad,layout=LayoutMatrix)

par(mfrow=c(1,1))






#Let me start with season 1 
AllSeasons[[1]]

##track all the changes 
TraidVetexNames<-lapply(AllSeasons,
                        FUN = function(Season){lapply(X=Season,FUN =function(x){lapply(X = GetTriads(x),FUN = function(TriVec){paste(names(V(TriVec)),collapse = '-')})} )})

length(unique(unlist(TraidVetexNames)))
VNameOfAllTriads<-unique(unlist(TraidVetexNames))


MatrixOfChange<-matrix(data = 0,nrow = 10*length(AllSeasons),ncol =length(VNameOfAllTriads) )
colnames(MatrixOfChange)<-VNameOfAllTriads
rownames(MatrixOfChange)<-unlist(lapply(AllSeasons,FUN = names))


for (Season in 1:length(AllSeasons)){
        for (N in 1:length(AllSeasons[[Season]])){
                TargetEpi<-AllSeasons[[Season]][[N]]
                
                VecState<-lapply(GetTriads(TargetEpi),FUN = CalState)
                
                
                for (i in 1:length(VecState)){
                        MatrixOfChange[(10*(Season-1)+N),names(VecState[[i]])]<-VecState[[i]]
                }
                print(paste('working on Season',Season, 'Episode',N))
        }
}

head(MatrixOfChange)
nrow(MatrixOfChange)



#this is to form a netowrk of triads 
sum(strsplit(VNameOfAllTriads[1:2],split = '-')[[1]] %in%strsplit(VNameOfAllTriads[1:2],split = '-')[[2]])


NetWorkMatrixTriad<-matrix(0,nrow = length(VNameOfAllTriads),ncol = length(VNameOfAllTriads))
colnames(NetWorkMatrixTriad)<-VNameOfAllTriads
rownames(NetWorkMatrixTriad)<-VNameOfAllTriads

TriadInteractionVec<-combn(x =VNameOfAllTriads,m = 2)
LinkVec<-combn(x =VNameOfAllTriads,m = 2,FUN = function(x){VecFun<-strsplit(x,split = '-')
sum(VecFun[[1]]%in%VecFun[[2]])})

TriadInteractionVec<-rbind(TriadInteractionVec,LinkVec)

for (i in 1:ncol(TriadInteractionVec)){
        if(TriadInteractionVec[3,i]==2){
                NetWorkMatrixTriad[TriadInteractionVec[1,i],TriadInteractionVec[2,i]]<-1
                NetWorkMatrixTriad[TriadInteractionVec[2,i],TriadInteractionVec[1,i]]<-1# update both halves of the mat.
        }
}

TriadNet<-graph.adjacency(adjmatrix =NetWorkMatrixTriad,mode = 'undirected',weighted = NULL )


#plot.igraph(TriadNet,layout=layout.sphere(TriadNet))

# Look at episode 1 and color the nodes 
TARGET<-TriadNet


#plot.igraph(TARGET,layout=layout_as_star(TARGET,center = V(TARGET)[4]))

LayoutVec<-layout_as_star(TARGET,center = V(TARGET)[4])

range(LayoutVec[,2])

par(mfrow=c(1,3))
for (j in 1:40){
        ColVec<-NULL
        Episode<-j
        for (i in names(V(TARGET))){
                COLLL<-NULL
                if (MatrixOfChange[Episode,i]==0){COLLL<-'grey'}
                if( MatrixOfChange[Episode,i]==1|MatrixOfChange[Episode,i]==3){COLLL<-'blue'}
                if( MatrixOfChange[Episode,i]==2|MatrixOfChange[Episode,i]==4){COLLL<-'red'}
                ColVec<-c(ColVec,COLLL)
                
        }}
length(ColVec)==length(V(TARGET))

V(TARGET)$color<-ColVec

VecPlot<-induced_subgraph(graph = TARGET,v = V(TARGET)[V(TARGET)$color!='grey'])
#plot.igraph(VecPlot,layout=LayoutVec[V(TARGET)$color!='grey',],ylim=c(-1,1),xlim=c(-1,1),main=j)}
par(mfrow=c(1,1))



#up to this point the first question is whether unstable triads easier to change (stay shorter) compared with stable ones 

tail(MatrixOfChange)

##look at all but the last state 


ALLStates<-NULL  # vector to store all the states of all the triads 
for (i in 1:ncol(MatrixOfChange)){
        VecInter<-rle(MatrixOfChange[,i])
        VecInter<-rbind(VecInter$lengths[-length(VecInter$lengths)],
                        VecInter$values[-length(VecInter$values)])
        ALLStates<-cbind(ALLStates,VecInter)
}

StableStates<-ALLStates[1,ALLStates[2,]==1|ALLStates[2,]==3]
UnstableStates<-ALLStates[1,ALLStates[2,]==2|ALLStates[2,]==4]

par(mfrow=c(1,1),mar=c(4,4,4,4))
boxplot(StableStates,UnstableStates)

median(StableStates)
median(UnstableStates)

wilcox.test(StableStates,UnstableStates)

S1<-ALLStates[1,ALLStates[2,]==1]
S2<-ALLStates[1,ALLStates[2,]==2]
S3<-ALLStates[1,ALLStates[2,]==3]
S4<-ALLStates[1,ALLStates[2,]==4]
boxplot(S1,S2,S3,S4,main='length of existence of each state')

##Make a better version of plot 


boxplot(S1,S2,S3,S4,log='')

wilcox.test(S2,S3)
ks.test(S1,S4)
wilcox.test(c(S1,S3),c(S4,S2),alternative = 'greater')

ks.test(c(S1,S3),c(S4,S2),alternative = 'greater')
boxplot(c(S1,S3),c(S4,S2))

Vec1=sample(c(S1,S3),10)
Vec2=sample(c(S4,S2),10)
wilcox.test(Vec1,Vec2,alternative = 'less')
wilcox.test(Vec1,Vec2,alternative = 'greater')
ks.test()
##next question is whether stable triads linked to unstable ones changes faster?
##Get all the stable traids , its length of existing and end point links 
ALLStateLinks<-NULL
for (i in 1:ncol(MatrixOfChange)){
        InterVec<-rle(MatrixOfChange[,i])
        InterVec$lengths<-InterVec$lengths[-length(InterVec$lengths)]#remove unfinihshed stories 
        InterVec$values<-InterVec$values[-length(InterVec$values)]
        
        if (any(InterVec$values==3|InterVec$values==1)){
                ValueVec<-InterVec$values[InterVec$values==3|InterVec$values==1]
                LengthVec<-InterVec$lengths[InterVec$values==3|InterVec$values==1]
                
                VecEnd<-which(InterVec$values==3|InterVec$values==1)
                EndPoints<-sapply(VecEnd,FUN = function(x){sum(InterVec$lengths[1:x])})
                StartPoints<-sapply(VecEnd,FUN = function(x){return(sum(InterVec$lengths[1:x])-InterVec$lengths[x]+1)})#starting point of each state
                
                LinkedTrids<-intersect(rownames(NetWorkMatrixTriad)[NetWorkMatrixTriad[colnames(MatrixOfChange)[i],]==1],
                                       colnames(MatrixOfChange)[MatrixOfChange[EndPoints,]!=0])
                UnstableStateLinked<-sum(MatrixOfChange[EndPoints,LinkedTrids]==2|MatrixOfChange[EndPoints,LinkedTrids]==4)
                
                LinkedTridsStart<-intersect(rownames(NetWorkMatrixTriad)[NetWorkMatrixTriad[colnames(MatrixOfChange)[i],]==1],
                                            colnames(MatrixOfChange)[MatrixOfChange[StartPoints,]!=0])
                
                UnstableStateLinkedStart<-sum(MatrixOfChange[StartPoints,LinkedTrids]==2|MatrixOfChange[StartPoints,LinkedTrids]==4)
                
                ALLStateLinks<-cbind(ALLStateLinks,rbind(ValueVec,LengthVec,UnstableStateLinked,UnstableStateLinkedStart))
        }}

#plot(ALLStateLinks[2,]~ALLStateLinks[3,])
#boxplot(ALLStateLinks[4,],ALLStateLinks[3,])
wilcox.test(ALLStateLinks[4,],ALLStateLinks[3,],alternative = 'less')

###! most stable triads end linked with unstable ones.and comparef with start of the state, they are associated with more unstable triads  


#Part 2 edges 

#Now get into the key questions: whether most of the changes from stable triads are caused by changes from linked unstable ones?

# in order to know whether most changes of edge are associated with unstable triads. I need to make a matrix of changes of edges 


## Get all the edges in the 40 episodes 
AllEdges<-NULL

for (i in 1:length(AllSeasons)){
        for (j in 1:length(AllSeasons[[i]])){
                Target<-AllSeasons[[i]][[j]]  
                EdgeEndVec<-ends(Target,E(Target),names=T)
                EdgeNamesVec<-paste(EdgeEndVec[,1],'--',EdgeEndVec[,2],sep='')
                AllEdges<-c(AllEdges,EdgeNamesVec)
        }
}

AllEdges<-unique(AllEdges) ##All the edges appeared                       



##Next get all the weight of the traids ,0: not exisit, 1 , friendly, 2 unfriendly
MatrixOfEdgeChanges<-matrix(data = 0,nrow = 10*length(AllSeasons),ncol = length(AllEdges))
dim(MatrixOfEdgeChanges)
colnames(MatrixOfEdgeChanges)<-AllEdges     






for (Season in 1:length(AllSeasons)){#assign status of each edge in each episode 
        for (Epi in 1:length(AllSeasons[[Season]])){
                Target<-AllSeasons[[Season]][[Epi]] 
                VecCheck<-ends(Target,E(Target),names=T)
                VecCheck<-cbind(VecCheck,Weight=E(Target)$weight)
                VecEnds<-strsplit(AllEdges,split = '--')
                for (m in 1:nrow(VecCheck)){                        
                        for (Edge in 1:length(AllEdges)){
                                if(VecCheck[m,1]%in%VecEnds[[Edge]]&VecCheck[m,2]%in%VecEnds[[Edge]]){
                                        MatrixOfEdgeChanges[10*(Season-1)+Epi,Edge]<-as.numeric(VecCheck[m,3])}
                        }
                }
        }
}


#next questions is to check how many changes were associate with each types of triads. ie link two matrix 

##ie identify the status of all triads the edge involved in before each change 

head(MatrixOfEdgeChanges)

RecordChanges<-NULL#matrix to record all changes and their associated triads 


for (j in 1:ncol(MatrixOfEdgeChanges)){
        Target<-MatrixOfEdgeChanges[,j]
        VecChanges<-rle(Target)
        if (length(VecChanges$lengths)>1){#some triads stay unchanged forever
                
                VecInter<-cumsum(VecChanges$lengths)
                
                EdgeNames<-colnames(MatrixOfEdgeChanges)[j]
                EdgeNames<-unlist(strsplit(EdgeNames,'--'))
                NamesTriad<-strsplit(colnames(MatrixOfChange),'-')
                VecWhich<-sapply(EdgeNames,function(x){lapply(NamesTriad,FUN = function(y){x%in%y})})
                VecWhich<-which(unlist(VecWhich[,1])&unlist(VecWhich[,2]))
                
                for (i in 1:(length(VecInter)-1)){
                        VecTriadStatus<-MatrixOfChange[VecInter[i],VecWhich]
                        
                        RecordVec<-matrix(data = 0,nrow = 1,ncol = 7)
                        colnames(RecordVec)<-c('ChangeFrom','ChangeTo','LengthBeforeChange',
                                               'State1','State2','State3','State4')
                        RecordVec[1,c(1,2)]<-VecChanges$values[c(i,i+1)]
                        RecordVec[1,3]<-VecChanges$lengths[i]
                        RecordVec[1,c(4:7)]<-sapply(1:4,function(x){sum(VecTriadStatus==x)})
                        
                        RecordChanges<-rbind(RecordChanges,RecordVec)#update record 
                }
                
                
        }
}

head(RecordChanges)


#How many percentage are related to unstable triads 

Target=as.data.frame(RecordChanges)
Target=Target[Target$ChangeFrom!=0,]
Vec1=Target$LengthBeforeChange[Target$State2+Target$State4>0]
Vec2=Target$LengthBeforeChange[Target$State2+Target$State4==0]
tail(Target)

boxplot(Vec1,Vec2,Target$LengthBeforeChange)
wilcox.test(Vec1,Vec2,alternative = 'less')
ks.test(Vec1,Vec2,alternative = 'less')
#Whether the length before change negatively correlate with number of unstable triad 

NoUnstable=Target$State2+Target$State4
plot(Target$LengthBeforeChange[NoUnstable!=4]~NoUnstable[NoUnstable!=4])
cor.test(Target$LengthBeforeChange[NoUnstable!=4],NoUnstable[NoUnstable!=4],method = 'spearman')




####Look at each type of changes 
VecCom<-t(combn(x =c(1,2,0),m = 2))
VecCom<-rbind(VecCom,VecCom[,c(2,1)])  
CountVec<-VecCom      


Target12<-RecordChanges[RecordChanges[,1]==1&RecordChanges[,2]==2,]
Target21<-RecordChanges[RecordChanges[,1]==2&RecordChanges[,2]==1,]



EdgeChanges<-NULL
for (i in 1:nrow(RecordChanges)){
        TypeVec<-paste0(RecordChanges[i,1],RecordChanges[i,2])
        EdgeChanges<-cbind(EdgeChanges,TypeVec)
}
EdgeChanges<-table(EdgeChanges)

#barplot(EdgeChanges,main='changes of edge type')

#How long each edge last 

Target1X<-RecordChanges[RecordChanges[,1]==1,]


Target2X<-RecordChanges[RecordChanges[,1]==2,]

#boxplot(Target1X[,3],Target2X[,3],main='stable vs. unstable')
wilcox.test(Target1X[,3],Target2X[,3])


# #whether triads each edge linked to affect its

RecordChanges<-data.frame(RecordChanges)


ChangeName<-sapply(1:nrow(RecordChanges),FUN = function(x){paste0(RecordChanges[x,1],RecordChanges[x,2])})
RecordChanges<-cbind(RecordChanges,ChangeName)  

library(lattice)
library(reshape2)
Target<-melt(RecordChanges[,-c(1,2,3)],id.vars = 'ChangeName')
#bwplot(value~ ChangeName|variable ,data = Target  )



###Calculate the probability edge in each direction 

MatrixOfEdgeChanges
ProChanges<-NULL

for (i in 1:length(unique(RecordChanges[,8]))){
        Target<-RecordChanges[RecordChanges[,8]==unique(RecordChanges[,8])[i],]
        ProVec<-nrow(Target)/sum(MatrixOfEdgeChanges==Target[1,1]) 
        ProChanges<-c(ProChanges,ProVec)
}
names(ProChanges)<-unique(RecordChanges[,8])
head(ProChanges)


#Look at effects of each endge sign changes , whether most changes of edge lead to a more balance network 


head(MatrixOfEdgeChanges)

RecordChanges2<-NULL#matrix to record all changes and their associated triads 



for (j in 1:ncol(MatrixOfEdgeChanges)){#count how many triads of each state before and after changes 
        Target<-MatrixOfEdgeChanges[,j]
        VecChanges<-rle(Target)
        if (length(VecChanges$lengths)>1){#some triads stay unchanged forever
                
                VecInter<-cumsum(VecChanges$lengths)
                
                EdgeNames<-colnames(MatrixOfEdgeChanges)[j]
                EdgeNames<-unlist(strsplit(EdgeNames,'--'))
                NamesTriad<-strsplit(colnames(MatrixOfChange),'-')
                VecWhich<-sapply(EdgeNames,function(x){lapply(NamesTriad,FUN = function(y){x%in%y})})
                VecWhich<-which(unlist(VecWhich[,1])&unlist(VecWhich[,2]))
                
                for (i in 1:(length(VecInter)-1)){
                        VecTriadStatus<-MatrixOfChange[VecInter[i],VecWhich]
                        
                        RecordVec<-matrix(data = 0,nrow = 1,ncol = 11)
                        colnames(RecordVec)<-c('ChangeFrom','ChangeTo','LengthBeforeChange',
                                               'Before_State1','Before_State2','Before_State3',
                                               'Before_State4','After_State1','After_State2','After_State3',
                                               'After_State4')
                        RecordVec[1,c(1,2)]<-VecChanges$values[c(i,i+1)]
                        RecordVec[1,3]<-VecChanges$lengths[i]
                        RecordVec[1,c(4:7)]<-sapply(1:4,function(x){sum(VecTriadStatus==x)})
                        
                        VecTriadStatusAfter<-MatrixOfChange[VecInter[i]+1,VecWhich]
                        RecordVec[1,c(8:11)]<-sapply(1:4,function(x){sum(VecTriadStatusAfter==x)})
                        
                        RecordChanges2<-rbind(RecordChanges2,RecordVec)#update record 
                }
                
                
        }
}

head(RecordChanges2)


#How each new edge occur by new node or linking existing nodes 


head(MatrixOfEdgeChanges)


#get all nodes 
ExistingNodeByTime=list()
for (i in 1:length(AllSeasons)){
        for (j in 1:length(AllSeasons[[i]])){
                Target= names(V(AllSeasons[[i]][[j]]))
                ExistingNodeByTime[[length(ExistingNodeByTime)+1]]=Target
        }
}
length(ExistingNodeByTime)

lapply(ExistingNodeByTime,length)


RecordChanges3<-NULL#matrix to record all changes and their associated triads 


for (j in 1:ncol(MatrixOfEdgeChanges)){#count how many triads of each state before and after changes 
        Target<-MatrixOfEdgeChanges[,j]
        VecChanges<-rle(Target)
        if (length(VecChanges$lengths)>1){#some triads stay unchanged forever
                
                VecInter<-cumsum(VecChanges$lengths)
                
                EdgeNames<-colnames(MatrixOfEdgeChanges)[j]
                EdgeNames<-unlist(strsplit(EdgeNames,'--'))
                NamesTriad<-strsplit(colnames(MatrixOfChange),'-')
                VecWhich<-sapply(EdgeNames,function(x){lapply(NamesTriad,FUN = function(y){x%in%y})})
                VecWhich<-which(unlist(VecWhich[,1])&unlist(VecWhich[,2]))
                
                for (i in 1:(length(VecInter)-1)){
                        VecTriadStatus<-MatrixOfChange[VecInter[i],VecWhich]
                        
                        RecordVec<-matrix(data = 0,nrow = 1,ncol = 13)
                        colnames(RecordVec)<-c('ChangeFrom','ChangeTo','LengthBeforeChange',
                                               'Before_State1','Before_State2','Before_State3',
                                               'Before_State4','After_State1','After_State2','After_State3',
                                               'After_State4','NoNodeBefore','NoNodeAfter')
                        RecordVec[1,c(1,2)]<-VecChanges$values[c(i,i+1)]
                        RecordVec[1,3]<-VecChanges$lengths[i]
                        RecordVec[1,c(4:7)]<-sapply(1:4,function(x){sum(VecTriadStatus==x)})
                        
                        VecTriadStatusAfter<-MatrixOfChange[VecInter[i]+1,VecWhich]
                        RecordVec[1,c(8:11)]<-sapply(1:4,function(x){sum(VecTriadStatusAfter==x)})
                        
                        
                        RecordVec[1,12]=sum(EdgeNames%in%ExistingNodeByTime[[VecInter[i]]])
                        RecordVec[1,13]=sum(EdgeNames%in%ExistingNodeByTime[[VecInter[i+1]]])                      
                        RecordChanges3<-rbind(RecordChanges3,RecordVec)#update record 
                }
                
                
        }
}

head(RecordChanges3)


Target=as.data.frame(RecordChanges3)
Target=Target[Target$ChangeFrom!=0&Target$ChangeTo!=0,]
table(Target$NoNodeAfter-Target$NoNodeBefore)




#track changes of EACH triad before and after edge change s 

RecordChanges4<-NULL#matrix to record all changes and their associated triads 


for (j in 1:ncol(MatrixOfEdgeChanges)){#count how many triads of each state before and after changes 
        Target<-MatrixOfEdgeChanges[,j]
        VecChanges<-rle(Target)
        if (length(VecChanges$lengths)>1){#some triads stay unchanged forever
                
                VecInter<-cumsum(VecChanges$lengths)
                
                EdgeNames<-colnames(MatrixOfEdgeChanges)[j]
                EdgeNames<-unlist(strsplit(EdgeNames,'--'))
                NamesTriad<-strsplit(colnames(MatrixOfChange),'-')
                VecWhich<-sapply(EdgeNames,function(x){lapply(NamesTriad,FUN = function(y){x%in%y})})
                VecWhich<-which(unlist(VecWhich[,1])&unlist(VecWhich[,2]))
                
                for (i in 1:(length(VecInter)-1)){
                        VecTriadStatus<-MatrixOfChange[VecInter[i],VecWhich]
                        
                        RecordVec<-matrix(data = 0,nrow = 1,ncol = 21)
                        colnames(RecordVec)<-c('ChangeFrom','ChangeTo','LengthBeforeChange',
                                               'Before_State1','Before_State2','Before_State3',
                                               'Before_State4','After_State1','After_State2','After_State3',
                                               'After_State4','NoNodeBefore','NoNodeAfter',
                                               "Triad_0_Stable","Triad_0_Unstable",'Triad_Stable_Unstable','Triad_Unstable_Stable',
                                               "Triad_Stable_0","Triad_Unstable_0",'Triad_Stable_Stable','Triad_Unstable_Unstable')
                        RecordVec[1,c(1,2)]<-VecChanges$values[c(i,i+1)]
                        RecordVec[1,3]<-VecChanges$lengths[i]
                        RecordVec[1,c(4:7)]<-sapply(1:4,function(x){sum(VecTriadStatus==x)})
                        
                        VecTriadStatusAfter<-MatrixOfChange[VecInter[i]+1,VecWhich]
                        RecordVec[1,c(8:11)]<-sapply(1:4,function(x){sum(VecTriadStatusAfter==x)})
                        
                        
                        RecordVec[1,12]=sum(EdgeNames%in%ExistingNodeByTime[[VecInter[i]]])
                        RecordVec[1,13]=sum(EdgeNames%in%ExistingNodeByTime[[VecInter[i+1]]])
                        
                        #record triad changes of each categories 
                        RecordVec[1,14]=sum(VecTriadStatus==0&(VecTriadStatusAfter==1|VecTriadStatusAfter==3))
                        RecordVec[1,15]=sum(VecTriadStatus==0&(VecTriadStatusAfter==2|VecTriadStatusAfter==4))
                        RecordVec[1,16]=sum((VecTriadStatus==1|VecTriadStatus==3)&(VecTriadStatusAfter==2|VecTriadStatusAfter==4))
                        RecordVec[1,17]=sum((VecTriadStatus==2|VecTriadStatus==4)&(VecTriadStatusAfter==1|VecTriadStatusAfter==3))
                        RecordVec[1,18]=sum((VecTriadStatus==1|VecTriadStatus==3)&VecTriadStatusAfter==0)
                        RecordVec[1,19]=sum((VecTriadStatus==2|VecTriadStatus==4)&VecTriadStatusAfter==0)
                        RecordVec[1,20]=sum((VecTriadStatus==1|VecTriadStatus==3)&(VecTriadStatusAfter==1|VecTriadStatusAfter==3))
                        RecordVec[1,21]=sum((VecTriadStatus==2|VecTriadStatus==4)&(VecTriadStatusAfter==2|VecTriadStatusAfter==4))
                        RecordChanges4<-rbind(RecordChanges4,RecordVec)#update record
                        
                }
                
                
        }
}



pdf('relationshipChanges.pdf')
#look at the edge dynamics 
Target=data.frame(RecordChanges4)
head(Target)

#Each types of changes 
Target$Types=paste0(Target$ChangeFrom,Target$ChangeTo)
VecPlot=table(Target$Types)
barplot(VecPlot,col='brown',ylab='Counts')


VecPlot2=c('Relationship formation'=sum(VecPlot[c('01','02')]),'Relationship Flipping'=sum(VecPlot[c('12','21')]),'Relationship Removal'=sum(VecPlot[c('10','20')]))
barplot(VecPlot2,col='brown',ylab='Counts')

Vec1=sum(Target$ChangeFrom==0&(Target$NoNodeAfter-Target$NoNodeBefore>0))
Vec2=sum(Target$ChangeFrom==0&(Target$NoNodeAfter-Target$NoNodeBefore==0))

Vec3=sum(Target$ChangeTo==0&(Target$NoNodeAfter-Target$NoNodeBefore<0))
Vec4=sum(Target$ChangeTo==0&(Target$NoNodeAfter-Target$NoNodeBefore==0))
VecPlot3=c('New entities'=Vec1,'Old entities'=Vec2,'Remove entities'=Vec3,'Keep entities'=Vec4)
barplot(VecPlot3,col=c('blue','blue','brown','brown'),ylab='Counts')
legend('topright',legend = c('Relationship formation','Relationship removal'),fill = c('blue','brown'),bty = 'n',inset = -0.2)
#Compare each types of changes and expected by random 
Target=MatrixOfEdgeChanges






#NUmber of changes vs. ratings and voter
Ratings<-read.csv('~/BoxSync/research/Third year 3/structural balance/game of thrones/Data/IMDratings/RatingsIMDB.csv',header = F)
names(Ratings)=c('Episdoe','Names','Rating','Votes')
head(Ratings)
nrow(Ratings)


Target=MatrixOfEdgeChanges
VecMinus=Target[-nrow(Target),]
VecMinus=rbind(rep(0,ncol(Target)),VecMinus)
head(VecMinus)
Target=Target-VecMinus
Target[Target!=0]=1
Target[1,]=0#there was no changes at the begining 
EdgeChangeOverTime=apply(Target,MARGIN = 1,sum)




VecViewers=Ratings$Votes
for( i in 1:(nrow(Ratings)/10)){
        Vec=VecViewers[(1+10*(i-1)):(10+10*(i-1))]
        #Vec=(Vec-min(Vec))/(max(Vec)-min(Vec))
        Vec=(Vec)/mean(Vec)
        #Vec=(Vec-mean(Vec))/sd(Vec)#Z score 
        VecViewers[(1+10*(i-1)):(10+10*(i-1))]=Vec
}


VecRatings=Ratings$Rating
for( i in 1:(nrow(Ratings)/10)){
        Vec=VecRatings[(1+10*(i-1)):(10+10*(i-1))]
        #Vec=(Vec-min(Vec))/(max(Vec)-min(Vec))
        Vec=(Vec)/mean(Vec)
        #Vec=(Vec-mean(Vec))/sd(Vec)#Z score 
        VecRatings[(1+10*(i-1)):(10+10*(i-1))]=Vec
}


par(mfrow=c(3,1))
plot(EdgeChangeOverTime,type='l',col='brown',lwd=2,ylab='No. of changes/Episode',xlab='Episode Number')
plot(VecViewers,type='l',col='brown',lwd=2,ylab='normalized number of votes by viewers',xlab='Episode Number')
plot(VecRatings,type='l',col='brown',lwd=2,ylab='normalized ratings by viewers',xlab='Episode Number')
par(mfrow=c(1,1))

plot(VecViewers~EdgeChangeOverTime,log='y',pch=8,col='brown',xlab='number Of changes/Episode',ylab='Normalized Number of votes by viewers' )
cor.test(VecViewers,EdgeChangeOverTime)
LM=lm(VecViewers~EdgeChangeOverTime)
abline(LM,lty=2,col='blue',untf=TRUE,lwd=2)
cor.test(VecViewers,EdgeChangeOverTime)

plot(VecRatings~EdgeChangeOverTime,log='y',pch=8,col='brown',xlab='number Of changes/Episode',ylab='Normalized ratings by viewers' )
cor.test(VecRatings,EdgeChangeOverTime)
LM=lm(VecRatings~EdgeChangeOverTime)
abline(LM,lty=2,col='blue',untf=TRUE,lwd=2)


plot(Ratings$Votes~EdgeChangeOverTime,log='y')
LM=lm(Ratings$Votes~EdgeChangeOverTime)
abline(LM,lty=2,col='blue',untf=TRUE)
cor.test(Ratings$Votes,EdgeChangeOverTime)

par(mfrow=c(2,3))
for (i in 1:(nrow(Ratings)/10)){
        VecVotes=Ratings$Votes[(1+10*(i-1)):(10+10*(i-1))]
        VecChanges=EdgeChangeOverTime[(1+10*(i-1)):(10+10*(i-1))]
        plot(VecVotes~VecChanges,log='y',main=paste('Season',i))
        LM=lm(VecVotes~VecChanges)
        abline(LM,lty=2,col='blue',untf=TRUE)
}
par(mfrow=c(1,1))




plot(Ratings$Rating~EdgeChangeOverTime,log='y')
LM=lm(Ratings$Rating~EdgeChangeOverTime)
abline(LM,lty=2,col='blue',untf=TRUE)
cor.test(Ratings$Rating,EdgeChangeOverTime)

par(mfrow=c(2,3))
for (i in 1:(nrow(Ratings)/10)){
        VecVotes=Ratings$Rating[(1+10*(i-1)):(10+10*(i-1))]
        VecChanges=EdgeChangeOverTime[(1+10*(i-1)):(10+10*(i-1))]
        plot(VecVotes~VecChanges,log='y',main=paste('Season',i))
        LM=lm(VecVotes~VecChanges)
        abline(LM,lty=2,col='blue',untf=TRUE)
}
par(mfrow=c(1,1))


##Ratings 



dev.off()
