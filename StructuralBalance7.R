#this is code for structureal balance 

#this version lloks at the updated networks 

#pdf(file = 'Figures.pdf')

#this is the function to get all in the triads as list
#install.packages('igraph')
library('igraph')
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
pdf('StructuralBalanceDemo.pdf')
par(mfrow=c(2,2))

Triad<-graph.ring(n = 3,directed = F,circular = T)
E(Triad)$weight<-c(1,1,1)
E(Triad)$color<-rep('sky blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'orangered2'

xV<-c(84,381,232)
yV<-c(0,1,261)
LayoutMatrix<-cbind(xV,yV)
vertex_size=30
plot.igraph(Triad,layout=LayoutMatrix,vertex.size=vertex_size,main='Type 1:Everyone is friend \n (balanced)')

E(Triad)$weight<-c(1,1,2)
E(Triad)$color<-rep('sky blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'orangered2'
plot.igraph(Triad,layout=LayoutMatrix,vertex.size=vertex_size,main='Type 2:An friend of my friend is my enemy \n (imbalanced)')


E(Triad)$weight<-c(1,2,2)
E(Triad)$color<-rep('sky blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'orangered2'
plot.igraph(Triad,layout=LayoutMatrix,vertex.size=vertex_size,main='Type 3:An enemy of my friend is my enemy \n (balanced)')

E(Triad)$weight<-c(2,2,2)
E(Triad)$color<-rep('sky blue',3)
E(Triad)$color[E(Triad)$weight==2]<-'orangered2'
plot.igraph(Triad,layout=LayoutMatrix,vertex.size=vertex_size,main='Type 4:Everyone is enemy \n (imbalanced)')

par(mfrow=c(1,1))
dev.off()





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
#ks.test()
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


##Whether edge change tries to eliminate imbalnaced triads 

head(RecordChanges4)
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom!=0&Target$ChangeTo!=0,]
nrow(Target)



sum(Target$Triad_Unstable_Stable!=0)
sum(Target$Triad_Unstable_Stable==0)

sum(Target$Triad_Unstable_Unstable!=0)
sum(Target$Triad_Stable_Stable!=0)

sum(Target$Triad_Unstable_Stable)
sum(Target$Triad_Stable_Unstable)

head(RecordChanges4)
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==1&Target$ChangeTo==2,]
nrow(Target)



sum(Target$Triad_Unstable_Stable!=0)
sum(Target$Triad_Unstable_Stable==0)
sum(Target$Triad_Unstable_Stable)
sum(Target$Triad_Stable_Unstable)



head(RecordChanges4)
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==2&Target$ChangeTo==1,]
nrow(Target)



sum(Target$Triad_Unstable_Stable!=0)
sum(Target$Triad_Unstable_Stable==0)
sum(Target$Triad_Unstable_Stable)
sum(Target$Triad_Stable_Unstable)




Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeTo==0,]
nrow(Target)
sum(Target$Triad_Unstable_0)
sum(Target$Triad_Stable_0)


Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==0,]
nrow(Target)
head(Target)
sum(Target$Triad_0_Stable)
sum(Target$Triad_0_Unstable)

sum(Target$Triad_0_Unstable!=0)
sum(Target$Triad_0_Stable!=0)


sum(Target$Triad_Unstable_Stable!=0)
sum(Target$Triad_Unstable_Stable==0)
sum(Target$Triad_Stable_Unstable!=0)

Target=as.data.frame(RecordChanges3)
Target=Target[Target$ChangeTo==0,]
Target$NoNodeAfter-Target$NoNodeBefore



##analyse the changes 

###Focus on those between triads insteaf creation/removal 
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom!=0&Target$ChangeTo!=0,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)




par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))



Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==1&Target$ChangeTo==2,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)




par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))




Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==2&Target$ChangeTo==1,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)




par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))


#deletion of edges 
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeTo==0,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)



par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))


Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeTo==0&Target$ChangeFrom==1,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)



par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))



Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeTo==0&Target$ChangeFrom==2,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)



par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))



###Formation of new edges 
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==0,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)

par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))

sum(ImbalancedAfter-ImbalancedBefore)
sum((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))



Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==0&Target$ChangeTo==1,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)

par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))



Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom==0&Target$ChangeTo==2,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)

par(mfrow=c(2,1),mar=c(2,2,2,2))
barplot(table(ImbalancedAfter-ImbalancedBefore),
        col='blue',xlab='changes in No. of imblanced triads',ylab='count of edge changes')

barplot(table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore)),
        col='blue',xlab='changes in No. of balanced triads',ylab='count of edge changes')
table((TotalAfter-ImbalancedAfter)-(TotalBefore-ImbalancedBefore))
par(mfrow=c(1,1))



###Edge changes reducing number of imblananced happend faster?
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom!=0,]
head(Target)

TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)

TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)

Vec2=Target$LengthBeforeChange

Target=Target[ImbalancedAfter-ImbalancedBefore<0,]
Vec1=Target$LengthBeforeChange
boxplot(Vec1,Vec2)
wilcox.test(Vec1,Vec2,alternative = 'less')

###Edge changes involving elimiating of unstable traids ihappend faster?
Target=as.data.frame(RecordChanges4)
Target=Target[Target$Triad_Unstable_Stable>0|Target$Triad_Unstable_0>0,]
head(Target)
Vec1=Target$LengthBeforeChange

Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom!=0,]
#Target=Target[Target$Triad_Unstable_Stable==0&Target$Triad_Unstable_0==0,]

Vec2=Target$LengthBeforeChange
boxplot(Vec1,Vec2)
wilcox.test(Vec1,Vec2,alternative = 'less')
ks.test(Vec1,Vec2,alternative = 'greater')
VecRecord=NULL
for (i in (1:100)){
Output=wilcox.test(sample(Vec1,1000,replace = T),sample(Vec2,1000,replace = T),alternative = 'less')
VecRecord<-c(VecRecord,Output$p.value)
}
sum(VecRecord<=0.05)


#Number of each type of changes 
Target=as.data.frame(RecordChanges4)
head(Target)
nrow(Target)

sum(Target$ChangeFrom==1&Target$ChangeTo==2)
sum(Target$ChangeFrom==2&Target$ChangeTo==1)
sum(Target$ChangeFrom==0&Target$ChangeTo==1)
sum(Target$ChangeFrom==0&Target$ChangeTo==2)
sum(Target$ChangeFrom==1&Target$ChangeTo==0)
sum(Target$ChangeFrom==2&Target$ChangeTo==0)


#Look at changes of Nonde Of Nodes 
Target=as.data.frame(RecordChanges4)
head(Target)
Target=Target[Target$ChangeFrom==0,]
Target$NoNodeAfter-Target$NoNodeBefore


Target=as.data.frame(RecordChanges4)
head(Target)
Target=Target[Target$ChangeTo==0,]
Target$NoNodeAfter-Target$NoNodeBefore


#Which type of Edge stay longer?
Target=as.data.frame(RecordChanges4)
Target=Target[Target$ChangeFrom!=0,]
Vec1=Target$LengthBeforeChange[Target$ChangeFrom==1]
Vec2=Target$LengthBeforeChange[Target$ChangeFrom==2]

boxplot(Vec1[-which.max(Vec1)],Vec2)
ks.test(Vec1[-which.max(Vec1)],Vec2,alternative = 'less')


#Look at changes at triangle level 

MatrixOfChange
par(mfrow=c(1,1),mar=c(5,5,5,5))
LengthNChanges<-apply(MatrixOfChange,MARGIN = 2,FUN = rle)#get length of each state and its length
Target<-lapply(X =LengthNChanges,FUN = function(x){paste(x$value,collapse = '')} )
AllChanges<-lapply(Target,FUN =function(x){if(nchar(x)>=2){
        sapply(X = 1:(nchar(x)-1),
               FUN =function(i){substr(x,start = i,stop = i+1)} )
}} )
AllChanges<-unlist(AllChanges)#this conludes all the changes in triads

#barplot(table(AllChanges))
names(table(AllChanges))

barplot(table(AllChanges)[grep(pattern = '^0',names(table(AllChanges)),value = T)],
        main='emerging of new triads')

TStoU<-sum(table(AllChanges)[grep(pattern = '^[1|3][2|4]',names(table(AllChanges)),value = T)])
TUtoS<-sum(table(AllChanges)[grep(pattern = '^[2|4][1|3]',names(table(AllChanges)),value = T)])
barplot(c(TStoU,TUtoS),names.arg = c('StableToUnstable','UnstableToStable'))


table(AllChanges)[c("01","02","03","04","10","20", "30" ,"40" ,"12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43" )]

barplot(table(AllChanges)[c("01","02","03","04","10","20", "30" ,"40" ,"12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43")],main='All triad changes')






#organise all the changes 

RecordChangesTriad<-NULL #matrix to record all changes of triads 

for (j in 1:ncol(MatrixOfChange)){
        Target<-MatrixOfChange[,j]
        VecChanges<-rle(Target)
        if (length(VecChanges$lengths)>1){#some triads stay unchanged forever
                
                Persistence<-cumsum(VecChanges$lengths)[1:length(VecChanges$lengths)-1]
                NameChanges<-sapply(1:(length(VecChanges$values)-1),function(x){paste0(VecChanges$values[x],VecChanges$values[x+1])})
                StartFrom<-VecChanges$values[1:(length(VecChanges$values)-1)]
                EndWith<-VecChanges$values[2:length(VecChanges$values)]
                RecordChangesTriad<-rbind(RecordChangesTriad,data.frame(cbind(StartFrom,EndWith,NameChanges,Persistence)))
        }
}


##look at the data

Target<-RecordChangesTriad[,c(3,4)]
library(lattice)
library(reshape2)

VecPlot<-lapply(split(Target,Target$NameChanges),FUN = function(x){as.numeric(as.character(x[,2]))})
#boxplot(VecPlot[c("12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43")])


    ##expected Time before changes at single change 

    p12<-(3*ProChanges['12']*((1-ProChanges['12'])^2))
    
    p23<-(2*ProChanges['12']*(1-ProChanges['21'])*(1-ProChanges['12']))
    p21<-(ProChanges['21']*((1-ProChanges['12'])^2))
    
    p32<-(2*(1-ProChanges['12'])*ProChanges['21']*(1-ProChanges['21']))
    p34<-(ProChanges['12']*((1-ProChanges['21'])^2))
    
    p43<-(3*ProChanges['21']*((1-ProChanges['21'])^2))
    
    p10<-(3*ProChanges['10']*((1-ProChanges['10'])^2))
    p20<-(ProChanges['20']*((1-ProChanges['10'])^2)+2*ProChanges['10']*((1-ProChanges['10'])*(1-ProChanges['20'])))
    p30<-(ProChanges['10']*((1-ProChanges['20'])^2)+2*ProChanges['20']*((1-ProChanges['10'])*(1-ProChanges['20'])))
    p40<-(3*ProChanges['20']*((1-ProChanges['20'])^2))
    ##the actually recorded value 
    #MatrixOfChange
    #RecordChangesTriad
    ActualChanges<-NULL
    
    for (i in 1:length(unique(RecordChangesTriad[,3]))){
            Target<-RecordChangesTriad[RecordChangesTriad[,3]==unique(RecordChangesTriad[,3])[i],]
            ProVec<-nrow(Target)/sum(MatrixOfChange==Target[1,1]) 
            ActualChanges<-c(ActualChanges,ProVec)
    }
    names(ActualChanges)<-unique(RecordChangesTriad[,3])
  
    B<-barplot(ActualChanges[c("12" , "21", "23","32", "34"  ,"43")],main='ceted vs. actual changes rate')
    
    ectVec<-c(p12,p21,p23,p32,p34,p43)
    points(cbind(B,ectVec))
    
    
    ##Improve figure for manuscript 
    
    B<-barplot(ActualChanges[c("12" , "21", "23","32", "34"  ,"43")],
                names.arg = c("1->2" , "2->1", "2->3","3->2", "3->4"  ,"4->3"), col=c('green','brown','brown','green','green','brown'))
    
    
    PrediectVec<-c(p12,p21,p23,p32,p34,p43)
    points(cbind(B,PrediectVec),pch=4,col='blue',cex=2)
    
    SToU<-sum(ActualChanges[c("12" ,"32", "34" )])
    PrediectVecSToU<-sum(p12,p32,p34)
    UToS<-sum(ActualChanges[c("21", "23","32","43")])
    PrediectVecUToS<-sum(p21,p23,p43)
    
    B<-barplot(c(SToU,UToS),col=c('green','brown'),names.arg = c('StableToUnstable','UnstableToStable'))
    points(cbind(B,c(PrediectVecSToU,PrediectVecUToS)),pch=4,col='blue',cex=2)
    
    nrow(RecordChangesTriad[RecordChangesTriad$StartFrom==0,])/nrow(RecordChangesTriad)
    
    
    #Organise all the changes 
    Target<-sapply(split(RecordChangesTriad,RecordChangesTriad$NameChanges),nrow)
    barplot(Target[c("01","02","03","04","10","20", "30" ,"40" ,"12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43")],
            names.arg =c("0->1","0->2","0->3","0->4","1->0","2->0", "3->0" ,"4->0" ,"1->2" ,"1->3" , "2->1", "2->3", "3->1" ,"3->2", "3->4"  ,"4->3"),las=2)
    
sum(Target[c('12','32','34')])
sum(Target[c('21','23','43')])
  
sum(Target[c('03')])/sum(Target[c('01','02','03','04')])







#unpreditrablyty 


#Percenage of unstable is correlated with length of exising ?

LengthExisting=sort(table(unlist(ExistingNodeByTime)))
barplot(LengthExisting,las=2)

plot(c(LengthExisting[names(VecPlot2)])~VecPlot2)
cor.test(c(LengthExisting[names(VecPlot2)]),VecPlot2)
#  plot(UnstaOfHouses['Stark',],type='l')
#  plot(UnstaOfHouses['Lanister',],type='l')
#  plot(UnstaOfHouses['Arryn',],type='l')

 
 plot(0, 0, xlim = c(0, 40), ylim = c(-0.1,1.1))
 Cols <- rainbow(n = nrow(UnstaOfHouses))
 for(i in 1:nrow(UnstaOfHouses)){
         points(UnstaOfHouses[i,], col=Cols[i], type='l')
 }
 
 abline(v=seq(from=0, to=40, by=10), lty=2)
# mapply(FUN = points, UnstaOfHouses, 2, points)
 


#barplot(sort(unlist(AppearByHouse)[c('baratheon','Tyrell','Baelish','bolton','Greyjoy','Stannis','Lanister' ,'Stark')],decreasing = T))



#     #Next I asked a question, whether initial triads , when ignore those introduced later, will to tend stableise themselves?
#     
#     Target<-MatrixOfChange
#     Target<-data.frame(Target[,Target[1,]!=0])
#     
#    
#     plot(Target[,1],type='l',ylim=c(0,5),col=1,lwd=2)
#     for (i in 2:ncol(Target)){
#             lines(Target[,i],col=i,lwd=2)
#     }   
    
 
 
 
#overall stability level 
    
    
    Target<-MatrixOfChange
    PercentageUnstable<-sapply(1:nrow(MatrixOfChange),FUN = function(x){sum(Target[x,]==2|Target[x,]==4)/sum(Target[x,]!=0)})
    NumberUnstable<-sapply(1:nrow(MatrixOfChange),FUN = function(x){sum(Target[x,]==2|Target[x,]==4)})
    par(mfrow=c(1,1))
    plot(PercentageUnstable,type='l',xlab='episode',ylab='%unbalanced triads')
    plot(NumberUnstable,type='l',xlab='episode',ylab='Number of unbalanced triads')
    
    
    #Normalized the Number of Unstable , by comapring with Mean number of unstable triads of that season
    NormaliseUnstable=NumberUnstable
    for( i in 1:(length(NormaliseUnstable)/10)){
            Vec=NormaliseUnstable[(1+10*(i-1)):(10+10*(i-1))]
            Vec=Vec/mean(Vec)
            NormaliseUnstable[(1+10*(i-1)):(10+10*(i-1))]=Vec
    }

    #NormaliseUnstable=NumberUnstable/mean(NumberUnstable)
    
    
    ##this is to use inverse of the median time of existence as unstability 
    Unstability<-1/sapply(list(S1,S2,S3,S4),mean)
    
    UnstabilityMatrix<-MatrixOfChange
    UnstabilityMatrix[UnstabilityMatrix==1]<-Unstability[1]
    UnstabilityMatrix[UnstabilityMatrix==2]<-Unstability[2]
    UnstabilityMatrix[UnstabilityMatrix==3]<-Unstability[3]
    UnstabilityMatrix[UnstabilityMatrix==4]<-Unstability[4]
    #VecPlot<-apply(UnstabilityMatrix,MARGIN = 1,FUN = function(x){sum(x)/sum(x!=0)})
   # plot(VecPlot,type='l',xlab='episode',ylab='Level of unstability')
    
   
    
 #Plot for structureal balance 
#pdf('StucturalBalanceAnalysis.pdf')
    #Sttrcutral balance over time 
    par(mfrow=c(2,1))
    plot(NumberUnstable,type='l',xlab='Episode Number',ylab='Number of imbalanced triads',col='brown',lwd=2)
    abline(h = mean(NumberUnstable),lty=2,lwd=2,col='blue')
    plot(PercentageUnstable,type='l',xlab='Episode Number',ylab='%imnbalanced triads',col='red',lwd=2)
    abline(h = mean(PercentageUnstable),lty=2,lwd=2,col='blue')
    par(mfrow=c(1,1))
 
 #Mechanisms that change structural balance 
    Target=as.data.frame(RecordChanges4)
    #Target=Target[Target$ChangeFrom!=0&Target$ChangeTo!=0,]
    head(Target)
    
    TotalBefore=apply(Target[,c(4,5,6,7)],MARGIN = 1,FUN = sum)
    ImbalancedBefore=apply(Target[,c(5,7)],MARGIN = 1,FUN = sum)
    
    TotalAfter=apply(Target[,c(8,9,10,11)],MARGIN = 1,FUN = sum)
    ImbalancedAfter=apply(Target[,c(9,11)],MARGIN = 1,FUN = sum)
    Target$ImbalanceChanges=ImbalancedAfter-ImbalancedBefore
    Target$type=paste0(Target$ChangeFrom,Target$ChangeTo)
    
    #Imbalance increases 
    Vec=Target[Target$ImbalanceChanges>0,]
    Vec=split(Vec,f = Vec$type)
    VecPlot=lapply(Vec,function(x){sum(x$ImbalanceChanges)})
    VecPlot=sort(unlist(VecPlot),decreasing = T)
    names(VecPlot)=c("+ to -",'0 to -',"0 to +",'- to +')
    par(mar=c(8.2,4,4,2))
    barplot(VecPlot,col='orange',ylab='Increase in No. of imbalanced triads',xlab='Type of relationship change')
    par(mar=c(5,5,5,5))
    par(mfrow=c(1,1))
   
    
    #Imbalance decrease 
    
    Vec=Target[Target$ImbalanceChanges<0,]
    Vec=split(Vec,f = Vec$type)
    VecPlot=lapply(Vec,function(x){sum(x$ImbalanceChanges)})
            VecPlot=sort(unlist(VecPlot),decreasing = F)
    names(VecPlot)=c("- to 0",'+ to 0',"+ to -",'- to +')
    par(mar=c(8.2,4,4,2))
    barplot(VecPlot,col='purple',ylab='Decrease in No. of imbalanced triads',xlab='Type of relationship change')
    par(mar=c(5,5,5,5))

    
    
    
    #distribution of unstability over houses
    NameOfHouses<-NULL
    for(i in 1:length(AllSeasons)){
            NameOfHouses<-c(NameOfHouses, unlist(sapply(AllSeasons[[i]],FUN = function(x){names(V(x))})))
            NameOfHouses<-unique(NameOfHouses)
    }
    
    
    
    
    
    #ranomizaing the sign for each edge for each episdoe see the level of imbalance 
    ##randomize the sign 
   
    TraidVetexNames<-lapply(AllSeasons,
                            FUN = function(Season){lapply(X=Season,FUN =function(x){lapply(X = GetTriads(x),FUN = function(TriVec){paste(names(V(TriVec)),collapse = '-')})} )})
    
    length(unique(unlist(TraidVetexNames)))
    VNameOfAllTriads<-unique(unlist(TraidVetexNames))
    
    
    AllRandomization=NULL
    for (Repeat in (1:10)){#10 rounds of randomization repears 
    
    RandomMatrixOfChange<-matrix(data = 0,nrow = 10*length(AllSeasons),ncol =length(VNameOfAllTriads) )
    colnames(RandomMatrixOfChange)<-VNameOfAllTriads
    rownames(RandomMatrixOfChange)<-unlist(lapply(AllSeasons,FUN = names))
    
    
    for (Season in 1:length(AllSeasons)){
            for (N in 1:length(AllSeasons[[Season]])){
                    TargetEpi<-AllSeasons[[Season]][[N]]
                    E(TargetEpi)$weight=sample(E(TargetEpi)$weight)
                    VecState<-lapply(GetTriads(TargetEpi),FUN = CalState)
                    
                    
                    for (i in 1:length(VecState)){
                            RandomMatrixOfChange[(10*(Season-1)+N),names(VecState[[i]])]<-VecState[[i]]
                    }
                    print(paste('working on Season',Season, 'Episode',N))
            }
    }
    
    head(RandomMatrixOfChange)
    nrow(RandomMatrixOfChange)
    
    
    
    
    RandomUnstaOfHouses<-matrix(data = NA,nrow = length(NameOfHouses),ncol = 10*length(AllSeasons))#matrix to store unstability if each node over ime
    rownames(UnstaOfHouses)<-NameOfHouses
    RandomstaOfHouses<-matrix(data = NA,nrow = length(NameOfHouses),ncol = 10*length(AllSeasons))#matrix to store unstability if each node over ime
    rownames(staOfHouses)<-NameOfHouses
    
    for(i in 1:nrow(RandomMatrixOfChange)){
            for (j in 1:length(NameOfHouses)){
                    VecStates<-RandomMatrixOfChange[i,which(grepl(NameOfHouses[j],colnames(RandomMatrixOfChange)))]#all the triads a node involved in the a episode 
                    VecStates<-VecStates[VecStates!=0]
                    if(length(VecStates)>0){VecStability<-sapply(VecStates,FUN = function(x){if(x==1|x==3){return('stable')}
                            if(x==2|x==4){return('unstable')} })
                    RandomUnstaOfHouses[j,i]<-sum(VecStability=='unstable')
                    RandomstaOfHouses[j,i]<-sum(VecStability=='stable')
                    }#update the matr with number unstabe       
            }        
            
    }
    rownames(RandomUnstaOfHouses)<-NameOfHouses
    VecEachRound=apply(RandomUnstaOfHouses,MARGIN = 1,FUN = function(x){mean(x[!is.na(x)])})
    names(VecEachRound)=NameOfHouses

    
    AllRandomization=rbind(AllRandomization,VecEachRound)
    }
    
    VecRandom=apply(AllRandomization,MARGIN = 2,FUN = function(x){mean(x,na.rm = T)})
    VecRandomSd=apply(AllRandomization,MARGIN = 2,FUN = function(x){sd(x,na.rm = T)})
    
    #real data 
    UnstaOfHouses<-matrix(data = NA,nrow = length(NameOfHouses),ncol = 10*length(AllSeasons))#matrix to store unstability if each node over ime
    rownames(UnstaOfHouses)<-NameOfHouses
    staOfHouses<-matrix(data = NA,nrow = length(NameOfHouses),ncol = 10*length(AllSeasons))#matrix to store unstability if each node over ime
    rownames(staOfHouses)<-NameOfHouses
    
    for(i in 1:nrow(MatrixOfChange)){
            for (j in 1:length(NameOfHouses)){
                    VecStates<-MatrixOfChange[i,which(grepl(NameOfHouses[j],colnames(MatrixOfChange)))]#all the triads a node involved in the a episode 
                    VecStates<-VecStates[VecStates!=0]
                    if(length(VecStates)>0){VecStability<-sapply(VecStates,FUN = function(x){if(x==1|x==3){return('stable')}
                            if(x==2|x==4){return('unstable')} })
                    UnstaOfHouses[j,i]<-sum(VecStability=='unstable')
                    staOfHouses[j,i]<-sum(VecStability=='stable')
                    }#update the matr with number unstabe       
            }        
            
    }
    
    
    
    
    
    
    
    #/length(VecStability)
    
    VecPlotUnstable<-sort(apply(UnstaOfHouses,MARGIN = 1,FUN = function(x){mean(x[!is.na(x)])}),decreasing = T)
    blue <- rgb(0, 0, 1, alpha=0.5)
    red <- rgb(1, 0, 0, alpha=0.5)
    par(mar=c(8.2,4,4,2))
    
    barplot(VecPlotUnstable,las=2,ylab='Average number of imbalanced Triads',col=red,ylim=c(0,5))#remove the iron bank since it is not in the Westero 
    B=barplot(VecRandom[names(VecPlotUnstable)],las=2,ylab='Average number of imbalanced Triads',col=blue,add=TRUE)
    arrows(B, VecRandom[names(VecPlotUnstable)], B, (VecRandom+VecRandomSd)[names(VecPlotUnstable)], length=0.05, angle=90, code=3)
    arrows(B, VecRandom[names(VecPlotUnstable)], B, (VecRandom-VecRandomSd)[names(VecPlotUnstable)], length=0.05, angle=90, code=3)
    legend(x=11,y=4,legend = c('real data','randomised networks'),fill = c(red,blue),bty = 'n')

    par(mar=c(5,5,5,5))
    
    dim(AllRandomization)
    Vec=apply(staOfHouses+UnstaOfHouses,MARGIN = 1,function(y){mean(y,na.rm = T)})
    length(Vec)
    AllRandomPer=apply(AllRandomization,MARGIN = 1,FUN = function(x){x/Vec})
    dim(AllRandomPer)
    VecRandomPer= apply(AllRandomPer,MARGIN = 1,FUN = function(x){mean(x,na.rm = T)})
    VecRandomPerSD=apply(AllRandomPer,MARGIN = 1,FUN = function(x){sd(x,na.rm = T)})
    
    
    PerUnstable=UnstaOfHouses/(staOfHouses+UnstaOfHouses)
    VecPlot2<-sort(apply(PerUnstable,MARGIN = 1,FUN = function(x){mean(x[!is.na(x)])}),decreasing = T)
    par(mar=c(8.2,4,4,2))
    
    barplot(VecPlot2,las=2,ylab='Average number of imbalanced Triads',col=red,ylim=c(0,1))#remove the iron bank since it is not in the Westero 
    B=barplot(VecRandomPer[names(VecPlot2)],las=2,ylab='Average number of imbalanced Triads',col=blue,add=TRUE)
    arrows(B, VecRandomPer[names(VecPlot2)], B, (VecRandomPer+VecRandomPerSD)[names(VecPlot2)], length=0.05, angle=90, code=3)
    arrows(B, VecRandomPer[names(VecPlot2)], B, (VecRandomPer-VecRandomPerSD)[names(VecPlot2)], length=0.05, angle=90, code=3)
    legend(x=11,y=4,legend = c('real data','randomised networks'),fill = c(red,blue),bty = 'n')
    par(mar=c(5,5,5,5))
    
    
    dim(UnstaOfHouses)
    dim(staOfHouses)
    PerUnstable=UnstaOfHouses/(staOfHouses+UnstaOfHouses)
    VecPlot2<-sort(apply(PerUnstable,MARGIN = 1,FUN = function(x){mean(x[!is.na(x)])}),decreasing = T)
    barplot(VecPlot2[names(VecPlot2)!='Iron Bank'],las=2,ylab='Average % of unstable Tiads',col='brown')#remove the iron bank since it is not in the Westero 
    
    names(UnstablByHouse)
    
   
    #Unpredictability
    Target<-sapply(split(RecordChangesTriad,RecordChangesTriad$NameChanges),nrow)
    barplot(Target[c("01","02","03","04","10","20", "30" ,"40" ,"12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43")],col=c(rep('brown',4),rep('blue',4),rep('purple',8)),
            names.arg =c("0->1","0->2","0->3","0->4","1->0","2->0", "3->0" ,"4->0" ,"1->2" ,"1->3" , "2->1", "2->3", "3->1" ,"3->2", "3->4"  ,"4->3"),las=2,ylab='Counts',
            xlab='Type of triad change')
    Target[c('03')]/sum(Target[c('03',"01",'02','04')])
    sum(Target[c('13',"23","31" ,"32", "34"  ,"43")])/sum(Target[c("12" ,"13" , "21", "23", "31" ,"32", "34"  ,"43")])
    sum(Target[c('30')])/sum(Target[c("10","20", "30" ,"40")])
    
    Target<-MatrixOfChange
    Target<-apply(Target,MARGIN = 1,FUN = function(x){sum(x==2|x==3)/(sum(x!=0))})
    plot(Target,type='l',col='brown',lwd=2,xlab='Episode Number',ylab='Level of unpredictability')
    abline(h=mean(Target),col='blue',lty=2,lwd=2)
    
dev.off()    
    
    