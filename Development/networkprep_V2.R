###################################################################
###Prepared by Chris Edge (cbedge@gmail.com or cedge@trca.on.ca)###
###Questions, bugs or comments should be directed to Chris      ###
###################################################################

#This is work in progress and does not do the calculations correctly yet.
#CHANGES
#Now a directed graph
#Must have upstream and downstream permeabilities in the Parch file
#Direction of graph changed from older versions, now in direction of river flow


###Prepares files to run DCI connectivity analyses###

#input is a file that must have the following fields
#Fnode = the upstream segment
#Tnode = the downstream segment
#BARRIER_CO = Indentifies the barrier between Fnode and Tnode, blank if no barrier
#Shape_Leng = The length of of Fnode
#PermUS = Permeability of barrier to move upstream from Tnode to Fnode.
#PermDS = permeability of barrier or junction to move downstream from Fnode to Tnode.

#These fields can be included but are not necessary
#Qual = The quality of habitat at Fnode


library(igraph)

calc.prep <- function(Parth, Quality = "Default"){
  Edge <- cbind(Parth$Fnode, Parth$Tnode, Parth$PermUS, Parth$PermDS) #The list of edges, change to now include US perm
  
  #Turning the list of edges into an adjacency matrix
  #For this to run correctly segments must be labelled 1, 2, 3... with no missing segments.
  Edge.adj <- matrix(0, nrow=length(Edge[,1]), ncol=length(Edge[,1]))
  for (i in 1:length(Edge[,1]))
  {
    Edge.adj[Edge[i,1], Edge[i,2]] <- Edge[i,3] #now a weighted matrix by US perm
    Edge.adj[Edge[i,2], Edge[i,1]] <- Edge[i,4] #weight by DS perm
  }
  
  #Now we create a directed graph of the river network from the adjacency matrix
  graph <- graph.adjacency(t(Edge.adj), mode="directed", weighted=TRUE) #Create a weighted graph
 
  #Now we create a Node file that contains all the attributes to run DCI
  Nodes <- data.frame(matrix(NA, nrow=length(Parth[,1]), ncol=6))
  colnames(Nodes) <- c("Junction", "ID", "Area", "Qual", "PermUS", "PermDS")
  
  Nodes$ID <- Parth$Fnode #upstream segment
  Nodes$Junction <- Parth$BARRIER_CO #barrier downstream of the segment, if present
  Nodes$Area <- Parth$Shape_Leng #length of segment
  
  if(Quality == "Default"){
    Nodes$Qual <- 1 #default value
  }else{
    Nodes$Qual <- Parth[,Quality]/100
  }
  Nodes$PermDS <- Parth$PermDS #permeability to move from focal segment to downstream segment
  Nodes$PermUS <- Parth$PermUS #assigning a permeability to move from segment downstream into the focal segment
  
  list("graph" = graph, "Nodes" = Nodes, "edgematrix" = Edge.adj)
}