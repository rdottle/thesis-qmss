library(sp)
library(readr)
library(dplyr)
library(maptools)
library(rgdal)
library(leaflet)
library(GGally)
library(ggplot2)
library(rgeos)
library(geojsonio)
library(magrittr)
library(stringr)
library(shp2graph)
library(htmlwidgets)

library(RColorBrewer)


#source("src/R/utils.R")

chicagocrs = '+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' 

streets = readOGR("/Users/dottler/Dropbox/thesis/data/network-chicago/edges/", layer = "edges") 
proj4string(streets) = CRS("+init=epsg:4326")
streets %<>% spTransform(CRS("+init=epsg:4326"))
projection(streets)

street_graph_list = readshpnw(streets, ELComputed=TRUE, longlat=FALSE) 
street_graph = nel2igraph(street_graph_list[[2]],street_graph_list[[3]],
                          weight=street_graph_list[[4]])
# eadf = street_graph_list[[5]])

#' Remove self loops
street_graph %<>% simplify()

#Plot results
street_graph %>%
simplify() %>%
plot(vertex.label=NA, vertex.size=.1,vertex.size2=.1, edge.curved = FALSE)
 
#' Compute some centrality measures
eig = eigen_centrality(street_graph, weight=E(street_graph)$weight)$vector
deg = degree(street_graph)
bet = betweenness.estimate(street_graph, cutoff=20000, directed=FALSE)
close = closeness.estimate(street_graph, cutoff=20000)

V(street_graph)$degree = deg
V(street_graph)$closeness = close
V(street_graph)$betweenness = bet
V(street_graph)$eigen = eig


#' Create SpatialPoints from nodes
#' 
intersections_data_frame = get.data.frame(street_graph, what="vertices")

coordinates(intersections_data_frame)= ~ x +y
proj4string(intersections_data_frame ) = CRS("+init=epsg:4326")
intersections_data_frame %<>% spTransform(CRS("+init=epsg:4326"))

#' Save SpatialPointsDataFrame
writePointsShape(intersections_data_frame, "../Desktop/Git/thesis-qmss/data/chicago_nodes_proj.shp")

##' Convert back the graph object to a shapefile
#' Create data frame with the information for all edges
street_data_frame = as_data_frame(street_graph)
street_data_frame %<>% 
  mutate(
          closeness = (vertex_attr(street_graph, "closeness", to)+
          vertex_attr(street_graph, "closeness", from))/2,
         betweenness = (vertex_attr(street_graph, "betweenness", to)+
                          vertex_attr(street_graph, "betweenness", from))/2,
         degree = (vertex_attr(street_graph, "degree", to)+
                      vertex_attr(street_graph, "degree", from))/2,
         eigen = (vertex_attr(street_graph, "eigen", to)+
                     vertex_attr(street_graph, "eigen", from))/2,
         from.lon =vertex_attr(street_graph, "x", from),
         from.lat =vertex_attr(street_graph, "y", from),
         to.lon =vertex_attr(street_graph, "x", to),
         to.lat =vertex_attr(street_graph, "y", to)
  )%>% 
  tibble::rownames_to_column("id")

#' Create SpatialLinesDataFrame
lines_list = apply(street_data_frame, 1,function(row){
  Lines(
    Line(
      rbind(as.numeric(c(row["from.lon"], row["from.lat"])),
            as.numeric(c(row["to.lon"], row["to.lat"])))
    ),
    ID= row["id"])
})

spatial_lines = SpatialLines(lines_list)
lines_df = SpatialLinesDataFrame(SpatialLines(lines_list), data=as.data.frame(street_data_frame))
proj4string(lines_df) = CRS("+init=epsg:4326")

lines_df %<>% spTransform(CRS("+init=epsg:4326"))

writeOGR(lines_df, dsn="../Desktop/Git/thesis-qmss/data/" ,layer="chicago_lines_proj_2",driver="ESRI Shapefile")

palette <- rev(brewer.pal(10, "RdYlBu")) #Spectral #RdYlBu

roadPal = function(x) {colorQuantile(palette = palette, domain = x, n=10)}


bet_map = leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = .5, color=~roadPal(betweenness)(betweenness)) 
bet_map

close_map = leaflet(lines_df) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(weight = .5, color=~roadPal(closeness)(closeness)) 
close_map
saveWidget(bet_map, paste(getwd(),"/doc/plots/mex_street_betweeness.html", sep=""), selfcontained = TRUE)
# 
# 
# 
# set.seed(1234)
# x <- rnorm(n=1000, mean=500, sd=2)
# 
# 
# x   <- seq(5,15,length=1000)
# y   <- dnorm(x,mean=10, sd=3)
# plot(x,y, type="l", lwd=1)
# 
# 
# 
# x <- seq(0, 4, 0.1)
# 
# plot(x, dnorm(x, 2, 0.5), type = "l")