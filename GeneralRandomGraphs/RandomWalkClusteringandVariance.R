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

shape_clusters<-function(g){
  V(g)$shape=shapes()[g$membership]
  return(g)
}

#--------------------------------------------------------------------------------

#returns a graph with clustering via random walks the method described in the paper SC
random_walk_cluster_paper<-function(g,steps=3,num_C=0){
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
    }
  }
  #-----------------------------------------------------------------------------
  C=relabel(C)
  g$membership=C
  g=color_clusters(g)
  g=shape_clusters(g)
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
  #---------------------------------------------------------------------------
  #Algorithm 2----------------------------------------------------------------
  C_max_mod = c(1:length(V(g))) #this is the place holder for the communities structure with the best modularity
  mod_max = 0
  C = c(1:length(V(g))) #C is the set of communities
  isDone = FALSE
  while(isDone == FALSE){
    maxIndicies = which(S == max(S),arr.ind = TRUE)
    max_i = as.integer(maxIndicies[1,][2])
    max_j = as.integer(maxIndicies[1,][1])
    #? merge commuinities i and j and replace with new community?
    #convention will be to take the smallest index and keep that as the community label
    
    S[max_i,max_j] = 0
    S[max_j,max_i] = 0
    merged_vec = apply(rbind(S[max_i,],S[max_j,]),2,mean)
    S[max_i,]=merged_vec
    S[,max_i]=t(t(merged_vec))
    S[max_j,]=0
    S[,max_j]=0
    
    #labeling clusters
    C_gone=C[max_j]
    for(q in c(1:length(C))){
      if(C[q]==C_gone){
        C[q]=C[max_i]
      }
    }
    
    #check if new maximum
    if(modularity(g,relabel(C))> mod_max){
      mod_max = modularity(g,relabel(C))
      C_max_mod = relabel(C)
    }
    
    #check if cluster number criteria is met
    if( max(S) == 0 || length(unique(C)) <= num_C ){
      isDone = TRUE
    }
  }
  #-----------------------------------------------------------------------------
  #max mod criteria
  #g$membership = C_max_mod
  #g=color_clusters(g)
  #g=shape_clusters(g)
  
  C=relabel(C)
  g$membership=C
  g=color_clusters(g)
  g=shape_clusters(g)
  return(g)
}


#give two clusterings, v1 and v2
#return: distances between two clusterings
distance<-function(v1,v2){
  dist = 0
  #all possible pairs
  for(i in c(1:length(v1))){
    for(j in c(1: i)){
      if(v1[i] == v1[j]){
        #if they were in the same cluster in 1
        if(v2[i] != v2[j]){
          dist = dist+1
        }
      }else{
        #if they were in different clusters in 1
        if(v2[i] == v2[j]){
          dist=dist+1
        }
      }
    }
  }
  return(dist/(length(v1)*(length(v1)-1)/2))
}

#give list of clusterings
#returns the variance from the list
variance<-function(clusterings){
  sums = 0
  max_distance = 0
  for(i in c(1:length(clusterings))){
    for(j in c(1:i)){
      sums = sums+distance(clusterings[[i]],clusterings[[j]])
      if(distance(clusterings[[i]],clusterings[[j]]) >= max_distance){
        max_distance = distance(clusterings[[i]],clusterings[[j]])
      }
    }
  }
  #print(max_distance)
  return(sums/(length(clusterings)^2))
}


#Sample Graph Dumbell--------------------------------------------------
#n=6
n=10
p=.99
g=graph.disjoint.union(erdos.renyi.game(n,p,loops=FALSE,directed=FALSE),
                       erdos.renyi.game(n,p,loops=FALSE,directed=FALSE))
g<-add_edges(g,c(n+1,1))
#plot(g)

g=random_walk_cluster(g,steps=100,num_C=2)
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

#Zachary test, |V(g)| = 34
g = graph.famous("Zachary")
#g=random_walk_cluster(g,length(V(g)))
g=random_walk_cluster(g,4)
modularity(g,g$membership)
plot(g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))

m = 100 #number of simulations
freebee_thresh = 0
clusterings=list()
i = 0
not_good = 0
while(i<m){
  g = graph.famous("Zachary")
  #n=10
  #p=.99
  #g=graph.disjoint.union(erdos.renyi.game(n,p,loops=FALSE,directed=FALSE),
  #                       erdos.renyi.game(n,p,loops=FALSE,directed=FALSE))
  #g<-add_edges(g,c(n+1,1))
  
  #g=random_walk_cluster(g,4,num_C=2) #SC
  
  g$membership=membership(cluster_label_prop(g)) #use with label prop
  #g$membership=membership(cluster_spinglass(g)) #use with spinglass
  #g$membership=membership(cluster_walktrap(g)) #use with walktrap
  if(length(which(g$membership == 1))<= freebee_thresh || length(which(g$membership == 1)) >= length(V(g))-freebee_thresh){
    not_good= not_good+1
  }else{
    clusterings[length(clusterings)+1] = list(g$membership)
    i=i+1
  }
}
#clusterings
variance(clusterings)
plot(g,mark.groups=NULL,layout=layout.kamada.kawai,vertex.label.cex=.8,vertex.size=15,margin=c(-.1,-.4,-.1,-.4))
#clusterings
