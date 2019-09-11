
library(igraph)

#Need export the Edgelist formatting file from Netlytic to create the network object
BestPicNet <- read.table("/Users/Luojiefan/Desktop/AOscars Datasets/net_Project-Best_Picture.csv",header=T,sep=",")

BestPicg <- graph.edgelist(as.matrix(BestPicNet[,c(2,3)]),directed=T)


#remove self-loops
BestPicg <- simplify(BestPicg, remove.multiple = FALSE, remove.loops = TRUE)
#layout <- layout.drl(BestPicg) 
layout <- layout.auto(BestPicg)
comm <- fastgreedy.community(as.undirected(BestPicg,,mode="collapse"))
comm <- edge.betweenness.community(as.undirected(BestPicg,mode="collapse"))
comm <- label.propagation.community(as.undirected(BestPicg ,mode="collapse"))
V(BestPicg)$color <- comm$membership+1

plot(BestPicg, layout=layout, vertex.label.color= "black",vertex.frame.color=V(BestPicg)$color, edge.arrow.size = 0.03,
     vertex.size = 2,vertex.label=ifelse(degree(BestPicg)>100,V(BestPicg)$name,NA),edge.width=1,vertex.label.font=0.001,vertex.label.cex=0.8)

