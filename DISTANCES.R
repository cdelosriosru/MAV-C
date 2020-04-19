
MAV = "D:/MAV" #poner si esta en el hard drive

MAV_CPU = "C:/Users/camilodel/Desktop/CAMILO/MAV"
library(dplyr)
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(raster)
library(sf)
library(raster)
library(stars)
library(maps)
library(geosphere)
library(foreign)

setwd(MAV)

consejos_coca<-readOGR(dsn="unido", layer="consejos_coca")

setwd(MAV_CPU)
rios1<-readOGR(dsn="Rios_colombia1", layer="Rios_Nivel_2")
rios2<-readOGR(dsn="rios_colombia2", layer="Rios_Nivel_3")

rios<-raster::bind(rios1,rios2)
rios<-spTransform(rios, crs(consejos_coca)) 
writeOGR(rios, "." ,driver="ESRI Shapefile", layer="rios")

rios2 <- spTransform(rios, CRS("+init=epsg:4326"))
coca2 <- spTransform(consejos_coca, CRS("+init=epsg:4326"))

dist_cam<-dist2Line(coca2,rios2)


rios.sf<-st_as_sf(rios)
coca.sf<-st_as_sf(consejos_coca)
4326
dist <- geosphere::dist2Line(p = st_coordinates(coca.sf), line = st_coordinates(rios.sf))


distancia <- rgeos::gDistance(rios,consejos_coca, byid = TRUE)


