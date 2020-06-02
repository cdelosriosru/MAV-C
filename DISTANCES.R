
MAV = "D:/MAV" #poner si esta en el hard drive

MAV_CPU = "C:/Users/cdelo/Dropbox/MAV/"
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

consejos_coca<-readOGR(dsn="DATA/raw_coca", layer="consejos_coca")

#-----------------Rios-------------------------

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

dist <- geosphere::dist2Line(p = st_coordinates(coca.sf), line = st_coordinates(rios.sf))


distancia <- rgeos::gDistance(rios,consejos_coca, byid = TRUE)

#-----------------Parques-------------------------

MAV = "C:/Users/cdelo/Dropbox/MAV" # poner para usar Dopbox.
epsg.2062 <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs"
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
setwd(MAV)
parques<-readOGR(dsn="DATA/maps", layer="parques_litoral_singorgona")
parques <- spTransform(parques, wgs.84)
consejos_coca<-readOGR(dsn="DATA/raw_coca", layer="consejos_coca")
coca2 <- spTransform(consejos_coca, wgs.84)
coca2$ID_cam <- 1:nrow(coca2)
coc <- as.data.frame(coca2@data)
cents <- as.data.frame(coordinates(coca2))
cents$ID_cam <- coc$ID_cam
?readWKT
?rbind
coca2$ID_cam <- 1:nrow(coca2)
coc <- as.data.frame(coca2@data)
cents <- as.data.frame(coordinates(coca2))
cents$ID_cam <- coc$ID_cam
dis_parques <- NULL
View(cents)
for (i in 1:nrow(cents)){
  print(i)
  coord <- paste0("POINT(",cents[i,1]," ",cents[i,2],")")
  print(coord)
  MAD   <- readWKT(coord, p4s=CRS(wgs.84))
  parques.proj <- spTransform(parques,CRS(epsg.2062))
  MAD.proj   <- spTransform(MAD,CRS(epsg.2062))
  dis <- gDistance(MAD.proj,parques.proj)
  dis_parques <- rbind(dis_parques, dis)
}
cents$dis_parques <- dis_parques
cents2 = cents
write.dta(cents2, "DATA/distances/dist_parques.dta", convert.factors = "string")
write.dta(parques_inside, "DATA/distances/parques_inside.dta", convert.factors = "string")
write.dta(cents2, "DATA/distances/dist_parques.dta", convert.factors = "string")
parques_st<-st_as_sf(parques)
coca_st<-st_as_sf(coca2)
parques_inside_st<-st_intersection(parques_st,coca_st)
parques_inside <- as(parques_inside_st, "Spatial")
write.dta(parques_inside, "DATA/distances/parques_inside.dta", convert.factors = "string")
parques_inside_dta<- as.data.frame(parques_inside, xy=TRUE, na.rm=TRUE)
write.dta(parques_inside_dta, "DATA/distances/parques_inside.dta", convert.factors = "string")




#-------RESGUARDOS



MAV = "C:/Users/cdelo/Dropbox/MAV" # poner para usar Dopbox.
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
#--------------COORDINATES-------
epsg.2062 <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs"
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
consejos_coca<-readOGR(dsn="DATA/raw_coca", layer="consejos_coca")
dpto<-readOGR(dsn="DATA/PoliticalBoundaries/dpto", layer="dpto")
dpto <- spTransform(dpto, wgs.84)
resguardos<-readOGR(dsn="DATA/maps", layer="resguardos")
resguardos <- spTransform(resguardos, wgs.84)
View(dpto)
View(dpto)
View(dpto)
table(dpto$NOMBRE_DPT)
dpto <- dpto[grepl(c('VALLE DEL CAUCA|NARIÃ‘O|CHOCO|CAUCA'),dpto$NOMBRE_DPT),]
View(dpto)
table(dpto$NOMBRE_DPT)
writeOGR(dpto, "DATA/PoliticalBoundaries/dpto" ,driver="ESRI Shapefile", layer="dpto_p")
dpto<-readOGR(dsn="DATA/PoliticalBoundaries/dpto", layer="dpto_p")
resguardos_p<-raster::intersect(dpto,resguardos)
coca2 <- spTransform(consejos_coca, wgs.84)
coca2$ID_cam <- 1:nrow(coca2)
coc <- as.data.frame(coca2@data)
cents <- as.data.frame(coordinates(coca2))
cents$ID_cam <- coc$ID_cam
dis_resguardos <- NULL
dis_resguardos <- NULL
for (i in 1:nrow(cents)){
  print(i)
  coord <- paste0("POINT(",cents[i,1]," ",cents[i,2],")")
  print(coord)
  MAD   <- readWKT(coord, p4s=CRS(wgs.84))
  resguardos.proj <- spTransform(resguardos_p,CRS(epsg.2062))
  MAD.proj   <- spTransform(MAD,CRS(epsg.2062))
  dis <- gDistance(MAD.proj,resguardos.proj)
  dis_resguardos <- rbind(dis_resguardos, dis)
}
cents$dis_resguardos <- dis_resguardos
cents2 = cents
write.dta(cents2, "DATA/distances/dist_resguardos.dta", convert.factors = "string")



