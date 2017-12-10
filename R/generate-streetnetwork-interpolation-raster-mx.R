library(maptools)
library(gdata)
library(classInt)
library(maps)
library(ggmap)
library(raster)  
library(rgdal)  
library(rgeos) 
library(spdep)
library(magrittr)
library(dplyr)
library(GISTools)
library(ggplot2)
library(gstat)
library(sp)
library(GGally)

install.packages("GISTools")

# source("src/R/utils.R")
#' Read in street intersection node data
streets = readOGR("../Desktop/Git/thesis-qmss/data/sacramento_nodes_2.shp")
# streets %>% (CRS("+init=epsg:4326"))
proj4string(streets) = CRS("+init=epsg:4326")
#' Plot kernel density function
#breach.dens = kde.points(ameni,lims=census)
#level.plot(pt)
streets@coords
plot(streets)

#' Generate grid to interpolate into from streets int
x.range <- as.numeric(range(streets@coords[,1]))
y.range <- as.numeric(range(streets@coords[,2]))
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=.01), y=seq(from=y.range[1], to=y.range[2], by=0.01))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
#project grid
proj4string(grd) <- CRS("+init=epsg:4326")
# grd = spTransform(grd, CRS(chicagocrs))
# x.seq = seq(from=x.range[1], to = x.range[2], by = 0)

#' Inverse distance weighting for interpolation w/ betweenness 
idw_close = idw(formula=streets$closeness ~ 1, locations=streets, newdata=grd)
names(idw_close)[1] = "closeness"
idw.output_close2 = as.data.frame(idw_close)
names(idw.output_close2)[1:3]<-c("long","lat","closeness")
glimpse(idw.output_close2)

idw_bet= idw(formula=streets$betweennes ~ 1, locations=streets, newdata=grd)
names(idw_bet)[1] = "betweenness"
idw.output_bet = as.data.frame(idw_bet)
names(idw.output_bet)[1:3]<-c("long","lat","betweenness")


idw_eig= idw(formula=streets$eigen ~ 1, locations=streets, newdata=grd)
names(idw_eig)[1] = "eigen"
idw.output_eig = as.data.frame(idw_eig)
names(idw.output_eig)[1:3]<-c("long","lat","eigen")


#' Plot raster of interpolated network betweenness
plot_close = ggplot(data=idw.output_close2,aes(x=long,y=lat)) +
  geom_tile(data=idw.output_close2,aes(fill=closeness)) +
  scale_fill_gradient(low="#FFFFCC", high="#000044")
plot_close

plot_bet = ggplot(data=idw.output_bet,aes(x=long,y=lat)) +
  geom_tile(data=idw.output_bet,aes(fill=betweenness)) +
  scale_fill_gradient(low="#FFFFCC", high="#000044") +
  coord_equal()
plot_bet

plot_eig = ggplot(data=idw.output_eig,aes(x=long,y=lat)) +
  geom_tile(data=idw.output_eig,aes(fill=eigen)) +
  scale_fill_gradient(low="#FFFFCC", high="#000044") +
  coord_equal()
plot_eig

# Read Census Data
census = readOGR("../Desktop/Git/thesis-qmss/data/sacramento_census.shp")

census@data
census@data %<>% dplyr::mutate(density=as.numeric(census@data$poverty)/raster::area(census))
proj4string(census) <- CRS("+init=epsg:4326")

projection(census)
# idw_raster = raster(idw)
# ext = extract(idw, census)
intersection_close = over(census, idw_close, fn = median)
intersection_bet = over(census, idw_bet, fn = median)
library(raster)
intersection_bet
census@data["closeness"] = intersection_close[,"closeness"]
census@data["betweenness"] = intersection_bet[,"betweenness"]

census@data %<>% mutate_at(vars(closeness:betweenness),
                           (funs(ifelse(is.na(.), mean(., na.rm=TRUE),.))))

writeOGR(census, dsn="../Desktop/Git/thesis-qmss/data/" ,layer="sacramento_interpolate_census",driver="ESRI Shapefile")



# leaflet_map = function(spatialDf, var, legend_title, palette = "YlGnBu",method = "quantile", n=5) {
#   require(rgdal)
#   require(leaflet)
#   
#   feature = unlist(spatialDf@data[var])
#   
#   spatialDf = spTransform(spatialDf, CRS("+init=epsg:4326"))
#   
#   leaflet(spatialDf) %>% addProviderTiles("CartoDB.Positron") %>% 
#     addPolygons(fillColor = pal(feature,palette,method,n)(feature), weight = .2, color="white",fillOpacity = 0.6) %>%
#     addLegend(pal = pal(feature, palette,method,n),
#               values = feature,
#               position = "bottomleft",title = legend_title
#     )
# }
# 
# 
# pal <- function(x, palette = "YlGnBu",method = "quantile", n =5) {
#   by = 1/n
#   if(method == "quantile"){
#     colorBin(palette, x, bins=quantile(x, probs = seq(0, 1, by), na.rm=TRUE))
#   }
#   else {
#     colorBin(palette, x, bins=5)
#   }
# }


# leaflet_map(census, "closeness", "closeness")
# leaflet_map(census, "betweenness", "betweenness")
# leaflet_map(census, "IMU", "deprivation", n=10)
# leaflet_map(census, "density", "density", n=10)

census@data$per_povert
cor(dplyr::select(census,closeness,betweenness) ,
    dplyr::select(census@data,poverty))

cor(as.numeric(census$closeness), as.numeric(census$density))
cor(as.numeric(census$betweenness), as.numeric(census$density))
census@data$per_pover2
census@data$per_pover2 = as.numeric(census@data$per_povert)
lm_closeness = summary(lm(per_pover2~closeness, census@data))
lm_betweenness = summary(lm(as.numeric(per_pover2)~betweenness, census@data))
lmdensclos = summary(lm(per_pover2~closeness+density, census@data))
lmdensbetween = summary(lm(as.numeric(poverty)~betweenness+density, census@data))
summary(lm(per_pover2~betweenness*closeness+density, census@data))

# 
# library(ggplot2)
ggpairs(dplyr::select(census@data, closeness, betweenness, density) %>% 
          mutate(log_betweenness = log(betweenness),
                 log_closeness = log(closeness),
                 inv_closeness = 1/closeness,
                 inv_betweenness = 1/betweenness))

qplot(x = density, y = betweenness, data = census@data)
qplot(x = density, y = closeness, data = census@data)

qplot(x = betweenness, y = closeness, data = census@data)
summary(census@data)
census@data %>% dplyr::select(GEOID, NAME, COUNTYFP, closeness, betweenness, density, per_pover2) %>% write_csv("../Desktop/Git/thesis-qmss/data/sacramento_networkfeatures.csv")
