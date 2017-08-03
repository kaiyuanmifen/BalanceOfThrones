#this is to plot a graph for publication purpose
library('igraph')


#Episdoe 6.10
Name.Of.Houses<-c('The Nights Watch','Stark','Greyjoy','Tully','Arryn','Lannister','Tyrell',
                  'Martell','Targaryen','Baelish','Laddy Melisandre','Iron Bank',
                  "Yara and Theon")
##first building a relaiton matrix (not a adj. matrix yet),0, means no known relations,  1 stands for postive relation, -1 negative, 2 complicated relation
Matrix.Relation<-structure(matrix(data = 0,nrow=length(Name.Of.Houses),ncol=length(Name.Of.Houses)),dimnames=list(Name.Of.Houses,Name.Of.Houses))
##input the links according to real interaction
Matrix.Relation['Stark','Tully']<-0
Matrix.Relation['Stark','Arryn']<-1
Matrix.Relation['Stark','Greyjoy']<-2
Matrix.Relation['Stark','Baelish']<-1


Matrix.Relation['Lannister','Arryn']<-2
Matrix.Relation['Lannister','Stark']<-2
Matrix.Relation['Lannister','Baelish']<-2

Matrix.Relation['Lannister','Tully']<-1

Matrix.Relation['Lannister','Tyrell']<-2
Matrix.Relation['Lannister','Martell']<-2
Matrix.Relation['Lannister','Iron Bank']<-2

Matrix.Relation['Baelish','Lannister']<-1
Matrix.Relation['Baelish','Arryn']<-1
Matrix.Relation['Baelish','Tyrell']<-1




Matrix.Relation['The Nights Watch','Stark']<-1
Matrix.Relation['The Nights Watch','Laddy Melisandre']<-0


Matrix.Relation['Laddy Melisandre','Stark']<-2


Matrix.Relation["Yara and Theon",'Greyjoy']<-2
Matrix.Relation["Yara and Theon",'Targaryen']<-1
Matrix.Relation["Yara and Theon",'Lannister']<-2

Matrix.Relation['Targaryen','Lannister']<-1
Matrix.Relation['Targaryen','Tyrell']<-1

House.Graph<-graph.adjacency(adjmatrix =Matrix.Relation,mode='undirected',weighted =T)


E(House.Graph)[which(E(House.Graph)$weight==1)]$color<-rep('sky blue',length(E(House.Graph)[which(E(House.Graph)$weight==1)]))

E(House.Graph)[which(E(House.Graph)$weight==2)]$color<-rep('orangered2',length(E(House.Graph)[which(E(House.Graph)$weight==2)]))
E(House.Graph)
E(House.Graph)[which(E(House.Graph)$weight==3)]$color<-rep('green',length(E(House.Graph)[which(E(House.Graph)$weight==3)]))


xV<-c(0,0,-0.1,-0.03,0.1,0,-0.1,0,0.2,-0.08,0.2,0.15,-0.13)
names(xV)<-names(V(House.Graph))
yV<-c(1.5,1.2,0.6,0.4,0.8,0,-0.3,-0.4,-0.7,0,-0.15,-0.8,0.9)
names(yV)<-names(V(House.Graph))
LayoutMatrix<-cbind(xV,yV)



House.Graph$LayoutMatrix<-LayoutMatrix
E(House.Graph)$curved=seq(0.2, -0.2, length = ecount(Episode6.10))
Episode6.10<-House.Graph
par(mfrow=c(1,1))


pdf('DemonstrationOfNetworks.pdf')

plot.igraph(Episode6.10,edge.width=3,main='',layout=LayoutMatrix,vertex.size=15+degree(Episode6.10,v = V(Episode6.10))*2)
legend('topleft',legend = c('Friendly','Hostile'),lty = 1,col = c('sky blue','orangered2'),bty = 'n',lwd=3,inset = -0.1)
dev.off()
#tkplot(Episode6.10)
