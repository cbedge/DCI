library(igraph)

setwd("C:/Users/christopher.edge/Documents/GitHub/DCI/Data") #Update directory to local

WC <- read.csv(file = "testWC.csv", header=T) #Select file that describes network
names(WC)

WC.prep <- calc.prep(Parth = WC, Quality="Qual") #prepare the files for DCI calculation

plot(WC.prep$graph, layout=layout_with_fr, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5) #plot the graph

WCDCI <- DCI.calc(nodefile = WC.prep$Nodes, graphfile = WC.prep$graph, mouth=6, calc.DCIs = 1) #calculate DCI

WCDCI$Index #returns DCId and DCIp values

WCDCI$DCIs #returns DCIs values if calculated


ToMouth <- DistToMouth(nodefile = WC.prep$Nodes, graphfile = WC.prep$graph, mouth=6) #returns the distance from all segments to the mouth
ToMouth