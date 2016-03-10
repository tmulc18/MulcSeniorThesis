library(igraph)

#Generates a graph that is the union of length(n) random graphs, where each subgraph
# G_i=(V_i,E_i) with edge probabilities P_i; |V_i|=n_i
#
#n, vector of number of vertices in each class
#p, vector of edge probablities
generateIslands<-function(n,p,d=0){
  #check if divisions were given
  #if not, make each class have only 1 division
  if(d == 0){
    d = rep(1,length(n))
  }
  
  index=1
  for(i in 1:length(n)){
    if(d[i] == 1){
      g_i=erdos.renyi.game(n[i],p[i],loops=FALSE,directed=FALSE)
      g_i<-set.vertex.attribute(g_i, "name", value=index:(index+n[i]-1))
      if(i==1){
        g=g_i
      }else{
        g=graph.disjoint.union(g,g_i)
      }
      index=index+n[i]
    }
  }
  return(g)
}


#Sets the same probability between all clusters
#p is the probability of an edge between clusters i,j exists
GenSameProbBetweenClasses<-function(n,p){
  p_class <- vector()
  for(ij in 1:length(combn(length(n),2)[1,])){
    p_class[ij]<-p
  }
  return(p_class)
}

GenSameProbInClasses<-function(n,p){
  p_vect<-vector()
  for(i in 1:length(n)){
    p_vect[i]<-p
  }
  return(p_vect)
}

#returns a vector that contains the indices (also labels) of start nodes for each 
#class in the g
GetClassIndecies<-function(g,n){
  result<-vector()
  for(i in 1:length(n)){
    if(i == 1){
      result[i]=1
    }else{
      result[i]=result[i-1]+n[i-1]
    }
  }
  return(result)
}

#x: integer of Class x
#returns vectors of edges for the bipartite subgraph of Class x and Class y
ListEdgesInBiPartite<-function(g,n,x,y){
  edges<-vector()
  index=1
  class_indicies<-GetClassIndecies(g,n)
  x_start<-class_indicies[x]
  x_end<-x_start+n[x]-1
  y_start<-class_indicies[y]
  y_end<-y_start+n[y]-1
  
  for(i in x_start:x_end){
    for(j in y_start:y_end){
      edges[index]=i
      edges[index+1]=j
      index=index+2
    }
  }
  return(edges)
}

#Inserts edges in g for classes x and y at probability p
PlaceEdgesBetween<-function(g,n,x,y,p){
  biEdges<-ListEdgesInBiPartite(g,n,x,y)
  #Generate the number of edges using binomial
  numEdges<-rbinom(1,length(biEdges)/2,p)
  #Generate list of edges to be used
  indiciesOfEdges<-sample(x=seq(1,length(biEdges)/2),size=numEdges,replace=FALSE)
  for(k in 1:length(indiciesOfEdges)){
    ij<-indiciesOfEdges[k]
    g<-add_edges(g,c(biEdges[ij],biEdges[ij+1]))
  }
  
  
  #oldMethod
  #for(ij in seq(1,length(biEdges),2)){
   # randNum<-runif(1)
    #if(randNum<=p){
     # g<-add_edges(g,c(biEdges[ij],biEdges[ij+1]))
    #}
  #}
  return(g)
}


#p, vector of probabilities of edges between clusters
#e.g. Given length(n)=4, then p[1] = p_(1,2), p[2]=p_(1,3), .. p[6]=p_(3,4)
#
#length(p)=length(n)C2
PlaceEdgesBetweenClasses<-function(g,n,p){
  ClassCombinations<-combn(length(n),2)
  for(i in 1:length(p)){
    #class X
    x=ClassCombinations[1,i]
    #class Y
    y=ClassCombinations[2,i]
    if(p[i]!=0){
      g<-PlaceEdgesBetween(g,n,x,y,p[i])
    }
  }
  return(g)
}

#k: number of classes
#returns a vector of class sizes
GenGaussianClassSizes<-function(k=4,n=14,sd=1){
  return(ceiling(rnorm(k,n,sd)))
}

#k: number of classes
#n: size of the classes
#returns a vector of class sizes where each element is the same
GenSameClassSizes<-function(k=4,n=14){
  return(rep(n,k))
}

#k: number of classes
#n: number of divisions within a class
#returns a vector of divisions where each element is i the number of division in class i
GenSameClassSizes<-function(k=4,n=14){
  return(rep(n,k))
}

#returns a boolean vector of combinations of classes
GenBooleanClassCombn<-function(n,p=1){
  return(rbinom(length(combn(length(n),2)[1,]),1,p))
}

#-------------------------------------------------------------
# Setting up class sizes and edges within the class
#-------------------------------------------------------------
#Option 1: manually generate vectors
n=c(14,14,14,15)
p=c(.8,1,.9,.8)
g=generateIslands(n,p)

#Option 2: use random normal class sizes
n=GenGaussianClassSizes(7,14,1)
p=rep(.9,10) 
g=generateIslands(n,p)

#Option 3: use same class sizes and propbablilites 
n = GenSameClassSizes(k=18,n=10)
p = GenSameProbInClasses(n,.9)
g=generateIslands(n,p)


#-------------------------------------------------------------
#Setting up edges between the classes
#-------------------------------------------------------------
p_clust<-GenSameProbBetweenClasses(n,.05) #p_clust assigned  vector where each element is p
IncludePair<-GenBooleanClassCombn(n,.3) #Include Pair is a binary vector that decides if each pair of classes will have edges
p_clust<-p_clust*IncludePair #p_clust is now the final vector of probablilites 

g<-PlaceEdgesBetweenClasses(g,n,p_clust) #places the edges


#-------------------------------------------------------------
#Running clustering algorithms
#-------------------------------------------------------------
wc<-walktrap.community(g,steps=4)
eb <-cluster_edge_betweenness(g)


#-------------------------------------------------------------
#Plotting
#-------------------------------------------------------------
plot(g,layout=layout.kamada.kawai, vertex.size = 10)
plot(wc,g,layout=layout.kamada.kawai, vertex.size = 10)

wc$merges
dendPlot(eb,use.modularity=TRUE)

#rglplot(g)
#built in function
#g<-sample_islands(3,10,5/10,1)
#plot(g)