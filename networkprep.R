##################################################################
###Prepared by Chris Edge (cbedge@gmail.com)                   ###
###Questions, bugs or comments should be directed to Chris     ###
##################################################################


###Prepares files to run DCI connectivity analyses###

#input is a file that must have the following fields
#Fnode = the upstream segment
#Tnode = the downstream segment
#BARRIER_CO = Indentifies the barrier between Fnode and Tnode, blank if no barrier
#Shape_Leng = The length of of Fnode
#PermPass = Permeability of barrier to move upstream from Tnode to Fnode. Leave blank if no barrier
#These fields can be included but are not necessary
#Qual = The quality of habitat at Fnode
#PermDS = permeability of barrier or junction to move downstrean from Fnode to Tnode. Leave blank if no barrier

library(igraph)

calc.prep <- function(Parth, Quality = "Default"){
  Edge <- cbind(Parth$Fnode, Parth$Tnode) #The list of edges
  
  #Turning the list of edges into an adjacency matrix
  #For this to run correctly segments must be labelled 1, 2, 3... with no missing segments.
  Edge.adj <- matrix(0, nrow=length(Edge[,1]), ncol=length(Edge[,1]))
  for (i in 1:length(Edge[,1]))
  {
    Edge.adj[Edge[i,1], Edge[i,2]] <- 1
  }
  
  #Now we create a directed graph of the river network from the adjacency matrix
  graph <- graph.adjacency(t(Edge.adj), mode="directed")
  
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
  #Nodes$Qual <- 100 #default value, can be changed
  Nodes$PermDS <- 1 #default value, completely permeable
  Nodes$PermUS <- Parth$PermPass #assigning a permeability to move from segment downstream into the focal segment
  Nodes$PermUS[is.na(Nodes$PermUS)] <- 1 #Replace NA's with 1 for completely permeable

  list("graph" = graph, "Nodes" = Nodes)
}