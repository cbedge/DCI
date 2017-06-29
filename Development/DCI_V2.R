###################################################################
###Prepared by Chris Edge (cbedge@gmail.com or cedge@trca.on.ca)###
###Questions, bugs or comments should be directed to Chris      ###
###################################################################

#This code calculates DCIp and DCId
#A large portion of the code is adapted from Greig Oldfords original work on FIPEX
#It has been adapted to run using R 3.X.X and igraph

####
#FUNCTION - DCI.calc: calculates DCIp and DCId
#REQUIRES a node file with the following colnames
#ID: ID of the segment and barrier
#Area: The length of the segment downstream of the barrier
#Qual: Quality of the segment, currently not used
#PermDS: downstream permeability of the barrier, this is usually 1
#PermUS: upstream permeability of the barrier, ranges between 0 (impermeable) and 1 (permeable)
#
#REQUIRES a graph object that can be any dendritic network describing the structure of the network
#mouth is the segment number that is the mouth of the river
####

library(igraph)

DCI.calc <- function(nodefile, graphfile, mouth, edgemat, calc.DCIs = 0){
  #Construct a table with the start and end of each path between all segments, length of that path,
  #the product of the permeabilites of barriers on that path, the length of the start segment, and the length of the last segment
  path.vals <- NULL
    for (k in 1:length(nodefile[,1])) {
    path <- all_simple_paths(graph = graphfile, from = k, to = V(graphfile), mode = "all")
    start.seg <- sapply(path, "[[", 1) #Identify the starting segment
    end.seg <- sapply(path, tail, 1) #Identify the final segment
    path.vals2 <- as.data.frame(cbind(start.seg, end.seg)) #combine start and end segments together
    
    for (j in 1:length(path)) {  
      cij <- 1 #Default value for cij
      for (i in 1:(length(path[[j]])-1)) {
        cij <- cij * (edgemat[path[[j]][i], path[[j]][i+1]] * edgemat[path[[j]][i+1], path[[j]][i]])
      }
      path.vals2$cij[j] <- cij
    }
    itself <- c(k,k,  1)
    path.vals <- rbind(path.vals, path.vals2, itself)
  }  
  path.vals$Start.Length <- (nodefile$Area[match(path.vals$start, nodefile$ID)]) * (nodefile$Qual[match(path.vals$start, nodefile$ID)])
  path.vals$End.Length <- (nodefile$Area[match(path.vals$end, nodefile$ID)]) * (nodefile$Qual[match(path.vals$end, nodefile$ID)])
  
  #DCIp Calculation
  tot.length <- sum(nodefile$Area)
  DCIp <- 0
  for (i in 1:nrow(path.vals)) {
    DCIp <- DCIp + (path.vals$cij[i] * (path.vals$Start.Length[i]/tot.length) * (path.vals$End.Length[i]/tot.length))
  }  
  
  #DCId Calculation
  DCId.data <- path.vals[path.vals$end.seg==mouth, ]
  DCId.data$val1 <- DCId.data$cij * (DCId.data$Start.Length/tot.length)
  DCId <- sum(DCId.data$val1)
  
  #Combine DCIp and DCId into output
  DCI.results <- data.frame(DCIp, DCId)
  colnames(DCI.results) <- c("DCIp", "DCId")
  
  #Calculating DCIs, recalculate DCId for each segement as the mouth of the network
  if(calc.DCIs == 1){
    DCIs.vals <- data.frame(ID=integer(length(nodefile$ID)), DCIs=double(length(nodefile$ID)) )
    j <- 1
    for (i in unique(nodefile$ID)){
      DCId.data2 <- path.vals[path.vals$end.seg==i, ]
      DCId.data2$val1 <- DCId.data2$cij * (DCId.data2$Start.Length/tot.length)
      DCIs <- sum(DCId.data2$val1)
      DCIs.vals$ID[j] <- i
      DCIs.vals$DCIs[j] <- DCIs
      j <- j+1
    }
  }else{
    DCIs.vals <- "DCIs was not calculated. To caluclate DCIs set calc.DCIs to 1"
  }
  
  list("Index" = DCI.results, "DCIs" = DCIs.vals)
}