library(igraph)
setwd('/home/mulctv/Rose_Classes/MathThesis/')

#Gfootball
g<-read.graph('football.gml', format ="gml")
plot(g,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

#Walktrap
wc_walktrap<-cluster_walktrap(g)
plot(wc_walktrap,g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

#Fast Greedy
wc_fg<-cluster_fast_greedy(g)
plot(wc_fg,g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

#Spinglass
wc_sg<-cluster_spinglass(g)
plot(wc_sg,g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

#Leading Eigen
wc_le<-cluster_leading_eigen(g)
plot(wc_le,g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

#Edge Betweenness
wc_eb<-cluster_edge_betweenness(g)
plot(wc_eb,g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))
