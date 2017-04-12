library(igraph)

setwd("C:/Users/christopher.edge/Documents/GitHub/DCI/Data")

WC <- read.csv(file = "testWC.csv", header=T) #Select file that describes network

WC.prep <- calc.prep(Parth = WC, Quality="Qual") #prepare the files for DCI calculation

WCDCI <- DCI.calc(nodefile = WC.prep$Nodes, graphfile = WC.prep$graph, mouth=6, calc.DCIs = 1) #calculate DCI

WCDCI$Index #returns DCId and DCIp values

WCDCI$DCIs #returns DCIs values if calculated


ToMouth <- DistToMouth(nodefile = WC.prep$Nodes, graphfile = WC.prep$graph, mouth=6) #returns the distance from all segments to the mouth
