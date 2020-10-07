getwd()
setwd("C:/Users/xiong/Desktop/UIUC 2018 spring/peer/peer2ndproject")
data1 <- read.csv("C:/Users/xiong/Desktop/UIUC 2018 spring/peer/woking data/peer2nd_project.csv")
dim(data)
View(data)
w1frimx <-read.csv('w1fri_matrix.csv',row.names = 1)
name = data1$ID
colnames(w1frimx)=name
w1frimx=as.matrix(w1frimx)
grp1 = w1frimx[1:88,1:88]
grp1=as.matrix(grp1)

g3 <- graph_from_adjacency_matrix(grp1, mode="directed")
V(g3)$degree=degree(g3)
set.seed(94)
plot(g3,
     vertex.size= V(g3)$degree*0.2,
     #vertex.label.dist=0.5,
     vertex.label.cex=0.5,
     edge.arrow.size=0.2,
     layout= layout.fruchterman.reingold)

install.packages('igraph')
library(igraph)
dataus=subset(data1,country==1)
y=data.frame(dataus$ID,dataus$Friends1W1)
net=graph.data.frame(y,directed = T)
V(net)
E(net)
V(net)$label =V(net)$name
V(net)$degree = degree(net)
set.seed(222)
plot(net,
     Vertex.color =rainbow(940),
     vertex.size= V(net)$degree*0.4,
     #vertex.label.dist=0.5,
     vertex.label.cex=0.5,
     edge.arrow.size=0.1,
     layout= layout.fruchterman.reingold)

hs<-hub_score(net)$vector
as<- authority.score(net)$vector

#net <- set.vertex.attribute(net, "color", value='grey')
par(mfrow=c(1,1))
set.seed(123)
plot(net,
     vertex.size= hs*10,
     main = 'hubs',
     vertex.label.dist=0.5,
     vertex.label.cex=0.5,
     edge.arrow.size=0.1,
     layout=layout.fruchterman.reingold)

#community detection
graph.data.frame(y,directed = F)
cnet<-cluster_edge_betweenness(net)
plot(cnet,net,vertex.size=4,vertex.label.cex=0.5)
