setwd('/home/mulctv/Rose_Classes/MathThesis/')
library(igraph)
dat<-read.csv('clean.csv',header=TRUE)
colnames(dat)<-NULL
el=as.matrix(dat)
el[,1]=as.character(el[,1])
el[,2]=as.character(el[,2])
g=graph.edgelist(el,directed=FALSE)
g = simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
plot(g)

#OR

g2=graph.data.frame(dat,directed=FALSE)
g2 = simplify(g2, remove.multiple = TRUE, remove.loops = FALSE)
plot(g2)
plot(cluster_walktrap(g2),g2, mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.4,vertex.size=7,margin=c(-.1,-.4,-.1,-.4))
