setwd("C:/rod")
library(dismo)
library(maptools)
library(rgdal)#Sistema de coordenadas
library(raster)#Leitura de rasters
library(usdm)#An?lise de incerteza para MNE 
library(sdm)#MNE
library(sf)#Formato espacial simple feature
library(terra)
library(tmap)
################################################################################
#Pontos de ocorr?ncia 
C_coimbrai <- read.table('C_coimbrai.txt', header = T)
class(C_coimbrai)
C_coimbrai$species = 1 
head(C_coimbrai)
tail(C_coimbrai)
coordinates(C_coimbrai) = ~ longitude + latitude 
C_coimbrai
class(C_coimbrai)
plot(C_coimbrai)


###############################################################################

#Preditores ambienatais - BAIXAR ARQUIVOS (bio 30s) de https://www.worldclim.org/data/worldclim21.html
B1 <- raster("wc2.1_30s_bio_1.tif")
B1 = crop(B1, extent( -40.98333, -34.63333, -14.71667, -7.9)) 
plot(B1)
plot(C_coimbrai, add=T)

B2 <- raster("wc2.1_30s_bio_2.tif")
B2 = crop(B2, extent(B1))
plot(B2)

B3 <- raster("wc2.1_30s_bio_3.tif")
B3 = crop(B3, extent(B1))
plot(B3)

B4 <- raster("wc2.1_30s_bio_4.tif")
B4 = crop(B4, extent(B1))
plot(B4)

B5 <- raster("wc2.1_30s_bio_5.tif")
B5 = crop(B5, extent(B1))
plot(B5)

B6 <- raster("wc2.1_30s_bio_6.tif")
B6 = crop(B6, extent(B1))
plot(B6)

B7 <- raster("wc2.1_30s_bio_7.tif")
B7 = crop(B7, extent(B1))
plot(B7)

B8 <- raster("wc2.1_30s_bio_8.tif")
B8 = crop(B8, extent(B1))
plot(B8)

B9 <- raster("wc2.1_30s_bio_9.tif")
B9 = crop(B9, extent(B1))
plot(B9)

B10 <- raster("wc2.1_30s_bio_10.tif")
B10 = crop(B10, extent(B1))
plot(B10)

B11 <- raster("wc2.1_30s_bio_11.tif")
B11 = crop(B11, extent(B1))
plot(B11)

B12 <- raster("wc2.1_30s_bio_12.tif")
B12 = crop(B12, extent(B1))
plot(B12)

B13 <- raster("wc2.1_30s_bio_13.tif")
B13 = crop(B13, extent(B1))
plot(B13)

B14 <- raster("wc2.1_30s_bio_14.tif")
B14 = crop(B14, extent(B1))
plot(B14)

B15 <- raster("wc2.1_30s_bio_15.tif")
B15 = crop(B15, extent(B1))
plot(B15)

B16 <- raster("wc2.1_30s_bio_16.tif")
B16 = crop(B16, extent(B1))
plot(B16)

B17 <- raster("wc2.1_30s_bio_17.tif")
B17 = crop(B17, extent(B1))
plot(B17)

B18 <- raster("wc2.1_30s_bio_18.tif")
B18 = crop(B18, extent(B1))
plot(B18)

B19 <- raster("wc2.1_30s_bio_19.tif")
B19 = crop(B19, extent(B1)) 
plot(B19)

dados_clima <- stack(B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19)
plot(dados_clima)

#An?lise VIF para clima
vifstep(dados_clima) 
dados_clima2 <- stack(B3, B4, B8, B9, B13, B14, B15, B18)
vifstep(dados_clima2)

#Vari?veis de habitat - BAIXAR ARQUIVOS
evergreen<- raster ("consensus_full_class_2.tif")
evergreen<- crop(evergreen, extent(B1))

deciduous<- raster ("consensus_full_class_3.tif")
deciduous<- crop(deciduous, extent(B1))

#An?lise VIF para clima e habitat
predictors<- stack(B3, B4, B5, B8, B9, B15, B17, B19, evergreen, deciduous) #vifstep desses pede para tirar B7
vifstep(predictors)
predictors<- stack(B3, B4, B8, B9, B15, B17, B19, evergreen, deciduous) #vifstep desses pede para tirar B7

#MNE
memory.limit(size = 9000000)

D = sdmData(species~., C_coimbrai, predictors = predictors, bg = list(n = 100))
#bg ? o background, cria pseudo aus?ncias
D
getmethodNames()
m_C_coimbrai <- sdm(species~., D, methods = c('glm','gam','svm','brt','rf'), replication = 'sub', test.percent = 20, n = 5) #Caracteriza??o do modelo climatico
#sub ? um subsampling
?sdm
gui(m_C_coimbrai)
m_C_coimbrai

p_C_coimbrai = predict(m_C_coimbrai, predictors)
plot(p_C_coimbrai)
p_C_coimbrai
combinado_C_coimbrai<-calc(p_C_coimbrai, mean, 'C_coimbrai.tif', overwrite=TRUE) #mapa conjunto considerando os 40 modelos criados
plot(combinado_C_coimbrai)
combinado_C_coimbrai
#############################################################################
#Bin?rio
bin <- function(x) {
  ifelse(x <= 0.5, 0,
         ifelse(x > 0.5, 1, NA)) }
sp_bin <- calc(combinado_C_coimbrai, fun=bin) 
plot(sp_bin)
writeRaster(sp_bin,filename='binario_C_coimbrai.tif', overwrite=TRUE)

################################################################################
##Importando dados de rodovias 
rodovias<-read_sf("road_br_tod.shp")
st_layers("road_br_tod.shp")
View(rodovias)

# Sistema de proje??o no R
EPSG <- make_EPSG()
View(EPSG)

##Convers?o para coordenadas geogr?ficas datum WGS84
rodovias<-st_transform(rodovias, 4326)
st_crs(rodovias)
rodovias

### Crop
crop_rod = st_crop(rodovias, extent(-40.98333, -34.63333, -14.71667, -7.9))
plot(st_geometry(crop_rod))

################################################################################
##Plotagem dos maps
plot(combinado_C_coimbrai)
plot(st_geometry(crop_rod), col="red", add=T)

plot(combinado_C_coimbrai)
plot(C_coimbrai, add=T)
plot(st_geometry(crop_rod), col="red", add=T)
################################################################################
##Dist?ncia ptos de ocorr?ncia e rodovias 
C_coimbrai_sf<-st_as_sf(C_coimbrai)
st_crs(C_coimbrai_sf)<-4326
class(C_coimbrai_sf)

buffer<-st_buffer(C_coimbrai_sf, 500)
intersect<-crop_rod[buffer, op=st_intersects]
par(mfrow=c(1, 1))
plot(combinado_C_coimbrai)
plot(st_geometry(intersect), add=T)
View(intersect)
intersect
plot(st_geometry(intersect))


sf::st_write(intersect,"rodovias.shp") 

rod<-read_sf("rodovias.shp")
#################################################################################
#pontos e ocorr?ncia pr?ximos ?s rodovias
crop_rod<-st_transform(crop_rod, 4326)
intersect_pts<-buffer[crop_rod, op=st_intersects]
View(intersect_pts)
intersect_pts
intersect_pts_sf<-st_as_sf(intersect_pts)
intersect_sf<-st_as_sf(intersect)
sf::st_write(intersect_pts,"pontos_de_influencia.shp") 


par(mfrow=c(1, 1))
plot(st_geometry(intersect))
plot(st_geometry(intersect_pts), col="red", add=T)

plot(st_geometry(C_coimbrai_sf), pch=20, col="red", cex=1)
plot(st_geometry(intersect_pts_sf), pch=3, col="red", cex=100)
intersect_pts_sf

################################################################################
#Transformando vetor em raster

intersect_ras<-rasterize(intersect, combinado_C_coimbrai,  background = 0)#raster do modelo
values(intersect_ras)[values(intersect_ras)>0] = 1
intersect_ras
plot(intersect_ras)

#Mosaic/merge srtm 
mosaicr <- mosaic(combinado_C_coimbrai, 2*intersect_ras, fun="max")#soma de rasters
crs(mosaicr)<-"+proj=longlat +datum=WGS84 +no_defs"#precisa ser colocado ap?s a soma
soma<-mosaicr+combinado_C_coimbrai
unique(soma)
par(mfrow=c(1,1))
plot(soma)
plot(combinado_C_coimbrai)
################################################################################
#exportando mapa de mosaico
tm_shape(soma)+tm_raster(palette = "Greys")+
  tm_layout(legend.show=FALSE)+
  tmap_save(filename="final", dpi=300)

################################################################################
#exportando mapa de adequabilidade
tm_shape(combinado_C_coimbrai)+tm_raster(palette = "Greys")+
  tm_layout(legend.show=FALSE)+
  tmap_save(filename="adequabilidade9.tiff", dpi=300)

################################################################################
#exportando mapa binário
pb<-rasterToPolygons(sp_bin, dissolve=T)
plot(pb)
shapefile(pb, filename='pb.shp')

################################################################################
#exportando pontos de ocorrência próximos à rodovias
shapefile(C_coimbrai, filename="C. coimbrai pts.")
