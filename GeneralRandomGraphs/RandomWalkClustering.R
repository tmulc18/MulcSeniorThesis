library(igraph)

#relabel C to only include membership 1 to k----------------------------------
#helper function for Algorithm 2
#mainly needed for coloring nodes
relabel<-function(C){
  C_temp = C
  q=c(1:length(unique(C)))
  for(i in q){
    min_C = min(C_temp)
    C[C==min_C]=i
    C_temp = setdiff(C_temp,min_C)
  }
  return(C)
}
#----------------------------------------------------------------------------------

#colors the nodes for plotting ---------------------------------------------------
color_clusters<-function(g){
  colors<-rainbow(length(unique(g$membership)))
  V(g)$color<-colors[g$membership]
  return(g)
}
#--------------------------------------------------------------------------------

#returns a graph with clustering via random walks the method described in the paper
random_walk_cluster_paper<-function(g,steps=3,num_C=0){
  steps = steps +1 #must add one to number of steps for rand_walk function
  
  #Algorithm 1-------------------------------------------------------------
  print(length(V(g)))
  S = matrix(ncol=length(V(g)),nrow=length(V(g)),0,0)  #initialize S to all zeros
  for(i in 1:length(V(g))){
    start = i
    C = random_walk(g,start,steps)
    for(j in C){
      for(k in C){
        if(j != k){
          S[j,k] = S[j,k]+1
        }
      }
    }
  }
  #---------------------------------------------------------------------------
  #Algorithm 2----------------------------------------------------------------
  C = c(1:length(V(g))) #C is the set of communities
  isDone = FALSE
  removed_rows = c()
  while(isDone == FALSE){
    maxIndicies = which(S == max(S),arr.ind = TRUE)
    max_i = maxIndicies[1,][1]
    max_j = maxIndicies[1,][2]
    max_value=max(S)
    #merged_row = apply(cbind(S[max_i,],rep(max_value,length(V(g)))),1,mean) #this is in the paper
    merged_row = apply(cbind(S[max_i,],S[max_j,]),1,mean)
    S[max_i,]= merged_row
    S[,max_j]=0
    C[max_j]=C[max_i]
    if( max(S) == 0 || length(unique(C)) <= num_C ){
      isDone = TRUE
      print(S)
    }
  }
  #-----------------------------------------------------------------------------
  C=relabel(C)
  g$membership=C
  g=color_clusters(g)
  return(g)
}


#returns a graph with clustering via random walks
random_walk_cluster<-function(g,steps=3,num_C=1){
  steps = steps +1 #must add one to number of steps for rand_walk function
  
  #Algorithm 1-------------------------------------------------------------
  S = matrix(ncol=length(V(g)),nrow=length(V(g)),0,0)  #initialize S to all zeros
  for(i in 1:length(V(g))){
    start = i
    C = random_walk(g,start,steps)
    for(j in C){
      for(k in C){
        if(j != k){
          S[j,k] = S[j,k]+1
        }
      }
    }
  }
  print(S)
  #---------------------------------------------------------------------------
  #Algorithm 2----------------------------------------------------------------
  C = c(1:length(V(g))) #C is the set of communities
  isDone = FALSE
  while(isDone == FALSE){
    maxIndicies = which(S == max(S),arr.ind = TRUE)
    max_i = as.integer(maxIndicies[1,][2])
    max_j = as.integer(maxIndicies[1,][1])
    print(c(max_i,max_j))
    #max_value=max(S)
    
    #merged_row = apply(cbind(S[max_i,],rep(max_value,length(V(g)))),1,mean) #this is in the paper
    ##merged_row = apply(cbind(S[max_i,],S[max_j,]),1,mean)
    ##S[max_i,]=merged_row
    ##S[,max_j]=t(t(merged_row))
    
    
    #max_i = ceiling(which.max(S)/length(V(g)))
    #max_j = which.max(S)-(max_i - 1)*length(V(g))
    #? merge commuinities i and j and replace with new community?
    #convention will be to take the smallest index and keep that as the community label
    
    #S[max_i,]=rowMeans(cbind(S[max_i,],S[max_j,]))
    S[max_i,max_j] = 0
    S[max_j,max_i] = 0
    merged_vec = apply(rbind(S[max_i,],S[max_j,]),2,mean)
    S[max_i,]=merged_vec
    S[,max_i]=t(t(merged_vec)) #newly added
    S[max_j,]=0
    S[,max_j]=0
    print(S)
    
    #labeling clusters
    C_gone=C[max_j]
    for(q in c(1:length(C))){
      if(C[q]==C_gone){
        C[q]=C[max_i]
      }
    }
    print(C)
    #C[max_j]=C[max_i]
    
    if( max(S) == 0 || length(unique(C)) <= num_C ){
      isDone = TRUE
      print(S)
      print(C)
    }
  }
  #-----------------------------------------------------------------------------
  C=relabel(C)
  g$membership=C
  g=color_clusters(g)
  return(g)
}

#Sample Graph Dumbell--------------------------------------------------
#n=6
n=10
p=.99
g=graph.disjoint.union(erdos.renyi.game(n,p,loops=FALSE,directed=FALSE),
                       erdos.renyi.game(n,p,loops=FALSE,directed=FALSE))
g<-add_edges(g,c(n+1,1))
#plot(g)

g=random_walk_cluster(g,steps=20)
modularity(g,g$membership)
plot(g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))
#-----------------------------------------------------------------------



#Testing many trials
#Steps = c(1:60)
#num_trial= 20
#steps_mod = c()

#for(s in Steps){
#  trial_mod = c()
#  for(t in c(1:num_trial)){
#    g=random_walk_cluster(g,steps=s)
#    trial_mod<-append(trial_mod,modularity(g,g$membership))
#  }
#  steps_mod<-append(steps_mod,mean(trial_mod))
#}
#jpeg(paste('Final_Steps',max(Steps),"Trials",num_trial,"_",format(Sys.time(), "%a %b %d %X %Y"),sep=""))
#plot(steps_mod)
#dev.off()

#Zachary test
#g = graph.famous("Zachary")
#g=random_walk_cluster(g,length(V(g)))
#modularity(g,g$membership)
#plot(g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))


#----------Example of Random Walk function---------
#g <- make_ring(10, directed = TRUE) %u%
#  make_star(11, center = 11) + edge(11, 1)
#ec <- eigen_centrality(g, directed = TRUE)$vector
#pg <- page_rank(g, damping = 0.999)$vector
#w <- random_walk(g, start = 1, steps = 10000)
## These are similar, but not exactly the same
#cor(table(w), ec)
## But these are (almost) the same
#cor(table(w), pg)
#---------------------------------------------------