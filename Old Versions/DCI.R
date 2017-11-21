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

DCI.calc <- function(nodefile, graphfile, mouth, calc.DCIs = 0){
  #Construct a table with the start and end of each path between all segments, length of that path,
  #the product of the permeabilites of barriers on that path, the length of the start segment, and the length of the last segment
  all.paths <- NULL
  for (b in 1:length(nodefile$ID)) {
    k <- unique(nodefile$ID)[b]
    paths <- all_simple_paths(graph=graphfile, from=k, mode = "all") #returns path from a segment to all other segements
    start <- sapply(paths, "[[", 1) #Identify the starting segment
    end <- sapply(paths, tail, 1) #Identify the final segment
    p.length <- NULL 
    pass <- NULL
    for (j in 1:(length(unique(nodefile$ID))-1)) {  
      p.length2 <- 0 #Stores the length of the path
      pass2 <- 1
        for (c in paths[[j]]) {
          i <- unique(nodefile$ID)[c]
          p.length2 <- p.length2 + (nodefile$Area[i] * nodefile$Qual[i]) #product of length of path and quality
          pass2 <- pass2 * (nodefile$PermUS[i] * nodefile$PermDS[i])
        }
      p.length <- c(p.length, p.length2)
      pass <- c(pass, pass2)
    }
    all.paths2 <- data.frame(start, end, p.length, pass)
    itself <- c(k, k, nodefile$Area[k], nodefile$PermUS[k] * nodefile$PermDS[k])
    all.paths <- rbind(all.paths, all.paths2, itself)
  }  
  all.paths$Start.Length <- (nodefile$Area[match(all.paths$start, nodefile$ID)]) * (nodefile$Qual[match(all.paths$start, nodefile$ID)])
  all.paths$End.Length <- (nodefile$Area[match(all.paths$end, nodefile$ID)]) * (nodefile$Qual[match(all.paths$end, nodefile$ID)])

  #DCIp Calculation
  tot.length <- sum(nodefile$Area)
  DCIp <- 0
  for (i in 1:nrow(all.paths)) {
    DCIp <- DCIp + (all.paths$pass[i] * (all.paths$Start.Length[i]/tot.length) * (all.paths$End.Length[i]/tot.length))
  }

  DCId.data <- all.paths[all.paths$end==mouth, ]
  DCId.data$val1 <- DCId.data$pass * (DCId.data$Start.Length/tot.length)
  DCId <- sum(DCId.data$val1)
  
  #Combine DCIp and DCId into output
  DCI.results <- data.frame(DCIp, DCId)
  colnames(DCI.results) <- c("DCIp", "DCId")
  
  #Calculating DCIs, recalculate DCId for each segement as the mouth of the network
  if(calc.DCIs == 1){
    DCIs.vals <- data.frame(ID=integer(length(nodefile$ID)), DCIs=double(length(nodefile$ID)) )
    j <- 1
    for (i in unique(nodefile$ID)){
      DCId.data2 <- all.paths[all.paths$end==i, ]
      DCId.data2$val1 <- DCId.data2$pass * (DCId.data2$Start.Length/tot.length)
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