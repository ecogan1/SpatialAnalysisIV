library(readxl)
library(maptools)
library(maps)
library(geoR)
library(raster)
library(rgdal)
library(readxl)


NIEFTY <- read_excel("Copy of NIFETY Data_remove2.xlsx")
View(NIEFTY)
baltcity <- readOGR("Baltimore_city_projected.shp")
plot(baltcity)
nif.geo <- as.geodata(NIEFTY, coords.col = 2:3, data.col = 1)
plot(nif.geo)
summary(nif.geo)
max.distance <- .2063272
max.d <- .2063272
incivility.vario <- variog(nif.geo, max.dist = max.d/2)
plot(incivility.vario)
incivil<-NIEFTY$Incivility
Long<-NIEFTY$Longitude
Lat<-NIEFTY$Latitude

plot(incivil,Lat,pch=16) 
plot(incivil,Long,pch=16)
modelLong<-lm(incivil~Long)
modelLong
modelLat<-lm(incivil~Lat)
modelLat




plot(Long,modelLong$residuals)
abline(h=0,lwd=2,col="blue")

plot(Lat,modelLat$residuals)
abline(h=0,lwd=2, col="purple")


residlong <-as.geodata(cbind(nif.geo$coords,modelLong$residuals))
names(residlong)
plot(residlong)

residlat <-as.geodata(cbind(nif.geo$coords,modelLat$residuals))
names(residlat)
plot(residlat)

residvariolong <- variog(residlong)
residvariolong$max.dist
residvariolong2<-variog(residlong,max.dist = .2063272/2)
plot(residvariolong2)

residvariolat <-variog(residlat)
residvariolat$max.dist
residvariolat2 <-variog(residlat,max.dist = .2063272/2)
plot(residvariolat2)

incivility.vario.wls <-variofit(incivility.vario, ini.cov.pars=c(30,.06),cov.model="spherical",
                          nugget=25,weights="cressie")

incivility.vario.wls
plot(incivility.vario.wls)

summary(nif.geo) - To determine grid max and min


grid<-expand.grid(Long=seq(-76.71,-76.53),
                  Lat=seq(39.22,39.28))
