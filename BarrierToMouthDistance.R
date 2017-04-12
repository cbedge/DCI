###Calculates the distance from each segement to the mouth of a river###

DistToMouth <- function(nodefile, graphfile, mouth){
  #Construct a table with the start and end of each path between all segments, length of that path,
  #the product of the permeabilites of barriers on that path, the length of the start segment, and the length of the last segment
  all.paths <- NULL
  for (b in 1:length(nodefile$ID)) {
    k <- unique(nodefile$ID)[b]
    paths <- all_simple_paths(graph=graphfile, from=k, mode = "all") #returns path from a segment to all other segements
    start <- sapply(paths, "[[", 1) #Identify the starting segment
    end <- sapply(paths, tail, 1) #Identify the final segment
    p.length <- NULL 
    for (j in 1:(length(unique(nodefile$ID))-1)) {  
      p.length2 <- 0 #Stores the length of the path
      for (c in paths[[j]]) {
        i <- unique(nodefile$ID)[c]
        p.length2 <- p.length2 + (nodefile$Area[i]) #product of length of path and quality
      }
      p.length <- c(p.length, p.length2)
    }
    all.paths2 <- data.frame(start, end, p.length)
    itself <- c(k, k, nodefile$Area[k])
    all.paths <- rbind(all.paths, all.paths2, itself)
  }
  
  all.paths$BarID <- nodefile$Junction[match(all.paths$start, nodefile$ID)]
  
  ToMouth <- all.paths[all.paths$end==mouth, ]
  ToMouth
}

  