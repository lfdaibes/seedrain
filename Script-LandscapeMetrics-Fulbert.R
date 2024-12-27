# Analises Mestrado # 
# Fulbertgnonlonfoun@gmail.com #
# Leec/unesp/RC
#2023_02_14
# 1 Landscapemetrics - -----------------------------------------------------
#Pacotes utilizados
pacotes <- c("landscapemetrics", "rgdal", "dplyr", "purrr", 
             "raster","psych", "vegan","tidyverse", "forcats", 
             "iNEXT","reshape2", "rgeos","wesanderson",
             "pak", "sf", "progress", "pbapply","readxl", "openxlsx" ,
             "reshape2","knitr", "remotes", "mgcv", "iNEXT", "terra",
             "tibble", "tmap", "RColorBrewer","sf")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# - -----------------------------------------------------------------------
#Apagar as listas
rm(list=ls())

#importar os dados
setwd("E:/04_rstudio/01_Luis/Dados_location")

# listar os arquivos
dir()# importar os dados

Data_luiz <-  read_excel("Pontos_Local_Luis_lat_long_filtrado_V2.xlsx")
data_luiz_filtrado <- distinct(Data_luiz)
Data_luiz

#
#################################################################################################
#Subindo pontos centrais de cada paisagem em formato shapefile
ptos_coleta_1 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_1") #subir o arquivo shape com pontos centrais dops buffers (UTM e metrico)
# tabela de atributos
sf :: st_drop_geometry( ptos_coleta_1)
ptos_coleta_1 <-  sf::st_as_sf(ptos_coleta_1, coords = c("x", "y"), crs = 31983, method = "ngb")
plot(ptos_coleta_3_3$geometry, col = "black", pch = 19, axes = TRUE, graticule = TRUE)


ptos_coleta_2 <-reaptos_coleta_2 <-reaptos_coleta_2 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_2") #subir o arquivo shape com pontos centrais dops buffers (UTM e metrico)
ptos_coleta_2 <-sf :: st_drop_geometry( ptos_coleta_2)
ptos_coleta_2 <- sf::st_as_sf(ptos_coleta_2, coords = c("x", "y"), crs = 31983, method = "ngb")


ptos_coleta_3_1 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_3_1")
ptos_coleta_3_1 <-sf :: st_drop_geometry( ptos_coleta_3_1)
ptos_coleta_3_1 <- sf::st_as_sf(ptos_coleta_3_1, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_3_2 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_3_2")
ptos_coleta_3_2 <-sf :: st_drop_geometry( ptos_coleta_3_2)
ptos_coleta_3_2 <- sf::st_as_sf(ptos_coleta_3_2, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_3_3 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_3_3")
ptos_coleta_3_3 <-sf :: st_drop_geometry( ptos_coleta_3_3)
ptos_coleta_3_3 <- sf::st_as_sf(ptos_coleta_3_3, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_4 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_4")
ptos_coleta_4 <-sf :: st_drop_geometry( ptos_coleta_4)
ptos_coleta_4 <- sf::st_as_sf(ptos_coleta_4, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_5 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_5")
ptos_coleta_5 <-sf :: st_drop_geometry( ptos_coleta_5)
ptos_coleta_5 <- sf::st_as_sf(ptos_coleta_5, coords = c("x", "y"), crs = 31983,method = "ngb")



ptos_coleta_6_1 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_6_1")
ptos_coleta_6_1 <-sf :: st_drop_geometry( ptos_coleta_6_1)
ptos_coleta_6_1 <- sf::st_as_sf(ptos_coleta_6_1, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_6_2 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_6_2")
ptos_coleta_6_2 <-sf :: st_drop_geometry( ptos_coleta_6_2)
ptos_coleta_6_2 <- sf::st_as_sf(ptos_coleta_6_2, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_6_3 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_6_3")
ptos_coleta_6_3 <-sf :: st_drop_geometry( ptos_coleta_6_3)
ptos_coleta_6_3 <- sf::st_as_sf(ptos_coleta_6_3, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_7 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_7")
ptos_coleta_7 <-sf :: st_drop_geometry( ptos_coleta_7)
ptos_coleta_7 <- sf::st_as_sf(ptos_coleta_7, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_8 <-readOGR(dsn = "E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz.rec_8")
ptos_coleta_8 <-sf :: st_drop_geometry( ptos_coleta_8)
ptos_coleta_8 <- sf::st_as_sf(ptos_coleta_8, coords = c("x", "y"), crs = 31983, method = "ngb")

ptos_coleta_3_1$geometry
ptos_coleta_3_1$id_num

plot(ptos_coleta_6_1$geometry, col = "black", pch = 19, axes = TRUE, graticule = TRUE)

#Para ver os pontos na Mapa
require(tmap)

tmap_mode("view")

tm_shape(ptos_coleta_6_3) + tm_sf(col="red", size=1)


#######################################################################################
#rateres
# landscape raster
mapbiomas_1 <- raster::raster("Raster_MA_rec_1.tif")
mapbiomas_2 <- raster::raster("Raster_MA_rec_2.tif")
mapbiomas_3_1 <- raster::raster("Raster_MA_rec_3_1.tif")
mapbiomas_3_2 <- raster::raster("Raster_MA_rec_3_2.tif")
mapbiomas_3_3 <- raster::raster("Raster_MA_rec_3_3.tif")
mapbiomas_4 <- raster::raster("Raster_MA_rec_4.tif")
mapbiomas_5 <- raster::raster("Raster_MA_rec_5.tif")
mapbiomas_6_1 <- raster::raster("Raster_MA_rec_6_1.tif")
mapbiomas_6_2 <- raster::raster("Raster_MA_rec_6_2.tif")
mapbiomas_6_3 <- raster::raster("Raster_MA_rec_6_3.tif")
mapbiomas_7 <- raster::raster("Raster_MA_rec_7.tif")
mapbiomas_8 <- raster::raster("Raster_MA_rec_8.tif")


#reprojetar raster
mapbiomas_utm_1 <- raster::projectRaster(mapbiomas_1, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_2 <- raster::projectRaster(mapbiomas_2, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_3_1 <- raster::projectRaster(mapbiomas_3_1, 
                                           crs = "EPSG:31983", 
                                           method = "ngb", 
                                           res = 10)
mapbiomas_utm_3_2 <- raster::projectRaster(mapbiomas_3_2, 
                                           crs = "EPSG:31983", 
                                           method = "ngb", 
                                           res = 10)
mapbiomas_utm_3_3 <- raster::projectRaster(mapbiomas_3_3, 
                                           crs = "EPSG:31983", 
                                           method = "ngb", 
                                           res = 10)
mapbiomas_utm_4 <- raster::projectRaster(mapbiomas_4, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_5 <- raster::projectRaster(mapbiomas_5, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_6_1 <- raster::projectRaster(mapbiomas_6_1, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_6_2 <- raster::projectRaster(mapbiomas_6_2, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_6_3 <- raster::projectRaster(mapbiomas_6_3, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_7 <- raster::projectRaster(mapbiomas_7, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)
mapbiomas_utm_8 <- raster::projectRaster(mapbiomas_8, 
                                         crs = "EPSG:31983", 
                                         method = "ngb", 
                                         res = 10)

 #  check_landscape() dar numero de class no raster 
check_landscape(mapbiomas_utm_8)


# Verificar o CRS atual dos pontos de coleta
st_crs(ptos_coleta_1)

# Verificar o CRS atual do raster
crs(mapbiomas_utm_1)

# Projetar o raster para o mesmo CRS dos pontos de coleta
 #mapbiomas_utm_3_1 <- projectRaster(mapbiomas_utm_3_1, crs = st_crs(ptos_coleta_3_1))
 #mapbiomas_utm_3_2 <- projectRaster(mapbiomas_utm_3_2, crs = st_crs(ptos_coleta_3_2))
 #mapbiomas_utm_3_3 <- projectRaster(mapbiomas_utm_3_3, crs = st_crs(ptos_coleta_3_3))
################################################################################################
# - -----------------------------------------------------------------------
#Calculo das metricas

#nomes das métricas
metrics.names<-lsm_abbreviations_names #Metricas paisagem
View(metrics.names)
landscapemetrics::list_lsm(level = c("class", "landscape"), type = "aggregation metric", 
                           simplify = TRUE)

view(landscapemetrics::list_lsm(level = "landscape"))

# patch metrics
patch_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "patch") %>% 
  dplyr::arrange(type)
patch_metrics

patch_metrics %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(n = n())

# class metrics
class_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "class") %>% 
  dplyr::arrange(type)
class_metrics

class_metrics_type <- class_metrics %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(n = n())
class_metrics_type


# landscape metrics
landscape_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "landscape") %>% 
  dplyr::arrange(type)
landscape_metrics
head(landscape_metrics)
names(landscape_metrics)



landscape_metrics_type <- landscape_metrics %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(n = n())
landscape_metrics_type

head(landscape_metrics_type)
names(landscape_metrics_type)

## c.lculate percentage of landscape of class
percentage_class <- lsm_c_pland(landscape = mapbiomas_utm_1 )

percentage_class


#landscapemetrics::check_landscape(mapbiomas_utm_1)


#par(mar=c(3,3,1,2))
#plot(mapbiomas_utm_1, "ffp", col=rev(rainbow(9)))
#points(ptos_coleta_1, pch=21, col="black", bg="white")

# ------------------------------------------------------ ------------------



#Calculando métricas
## c.lculate percentage of landscape of class
percentage_class <- lsm_c_pland(landscape = mapbiomas_utm_3)

percentage_class


#Calcule todas as métricas em nível de patch usando wrapper
#nlcd_patch <- landscapemetrics::calculate_lsm(landscape = mapbiomas_utm_1,level = "patch")
#nlcd_patch

#Mostrar abreviatura de todas as métricas calculadas:
#unique(nlcd_patch$metric)


# Calcule todas as métricas de agregação no nível do cenário:

nlcd_landscape_aggr <- landscapemetrics::calculate_lsm(landscape = mapbiomas_utm_1, 
                                                       level = "landscape", 
                                                       type = "aggregation metric")
nlcd_landscape_aggr



## c.nnected components labeling of landscape
cc_nlcd <- landscapemetrics::get_patches(mapbiomas_utm_1, directions = 8)

# summarize the SpatRaster for class 42: 
cc_nlcd$layer_1$class_3


#para plotar as floresta 

show_patches(landscape = mapbiomas_utm_1, class = c(3, 15), labels = FALSE)
show_cores(landscape = mapbiomas_utm_1, class = c(3), edge_depth = 776790 , labels = FALSE)
show_lsm(landscape = mapbiomas_utm_1, class = c(3), what = "lsm_p_area", labels = FALSE)

#$######################################################################################


# - Area and edge metric "lsm_p_area"  Patch em hectar (ha) 1ha = 10.000m2 ------------------------------
# extrair area Florestal do fragmento mais proximo do ponto de coleta.

path_area_ponto_1 <- extract_lsm(
  landscape = mapbiomas_utm_1,
  y = ptos_coleta_1,
  extract_id = ptos_coleta_1$id_num,
  what = "lsm_p_area",
  directions = 8
)


path_area_ponto_1 <-as.data.frame(subset(path_area_ponto_1, select=c(extract_id, level,metric, class, value)))
# Salvar os resultados em um arquivo Excel
write.xlsx(path_area_ponto_1,"E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_1.xlsx", sheetName = "path_area_ponto_1")


path_area_ponto_2 <- extract_lsm(mapbiomas_utm_2,
  y = ptos_coleta_2,
  extract_id = ptos_coleta_2$id_num,
  what = "lsm_p_area",
  directions = 8 
)
path_area_ponto_2 <-as.data.frame(subset(path_area_ponto_2, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_2.xlsx", sheetName = "path_area_ponto_2")




path_area_ponto_3_1 <- extract_lsm(mapbiomas_utm_3_1,
  y = ptos_coleta_3_1,
  extract_id = ptos_coleta_3_1$id_num,
  what = "lsm_p_area",
  directions = 8 
)
path_area_ponto_3_1 <-as.data.frame(subset(path_area_ponto_3_1, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_3_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_3_1.xlsx", sheetName = "path_area_ponto_3_1")


path_area_ponto_3_2 <- extract_lsm(mapbiomas_utm_3_2,
                                   y = ptos_coleta_3_2,
                                   extract_id = ptos_coleta_3_2$id_num,
                                   what = "lsm_p_area",
                                   directions = 8 
)
path_area_ponto_3_2 <-as.data.frame(subset(path_area_ponto_3_2, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_3_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_3_2.xlsx", sheetName = "path_area_ponto_3_2")


path_area_ponto_3_3 <- extract_lsm(mapbiomas_utm_3_3,
                                   y = ptos_coleta_3_3,
                                   extract_id = ptos_coleta_3_3$id_num,
                                   what = "lsm_p_area",
                                   directions = 8 
)
path_area_ponto_3_3 <-as.data.frame(subset(path_area_ponto_3_3, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_3_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_3_3.xlsx", sheetName = "path_area_ponto_3_3")




path_area_ponto_4 <- extract_lsm(mapbiomas_utm_4,
  y = ptos_coleta_4,
  extract_id = ptos_coleta_4$id_num,
  metric = "area",
  name = "patch area",
  type = "Area and edge metric",
  what = "lsm_p_area",
  directions = 8 
)
path_area_ponto_4 <-as.data.frame(subset(path_area_ponto_4, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_4, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_4.xlsx", sheetName = "path_area_ponto_4")



path_area_ponto_5 <- extract_lsm(mapbiomas_utm_5,
  y = ptos_coleta_5,
  extract_id = ptos_coleta_5$id_num,
  metric = "area",
  name = "patch area",
  type = "Area and edge metric",
  what = "lsm_p_area",
  directions = 8 
)
path_area_ponto_5 <-as.data.frame(subset(path_area_ponto_5, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_5, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_5.xlsx", sheetName = "path_area_ponto_5")



path_area_ponto_6_1 <- extract_lsm(mapbiomas_utm_6_1,
  y = ptos_coleta_6_1,
  extract_id = ptos_coleta_6_1$id_num,
  metric = "area",
  name = "patch area",
  type = "Area and edge metric",
  what = "lsm_p_area",
  directions = 8 # Specify the scale of the land cover map in meters
)
path_area_ponto_6_1 <-as.data.frame(subset(path_area_ponto_6_1, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_6_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_6_1.xlsx", sheetName = "path_area_ponto_6_1")




path_area_ponto_6_2 <- extract_lsm(mapbiomas_utm_6_2,
                                 y = ptos_coleta_6_2,
                                 extract_id = ptos_coleta_6_2$id_num,
                                 metric = "area",
                                 name = "patch area",
                                 type = "Area and edge metric",
                                 what = "lsm_p_area",
                                 directions = 8 # Specify the scale of the land cover map in meters
)
path_area_ponto_6_2 <-as.data.frame(subset(path_area_ponto_6_2, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_6_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_6_2.xlsx", sheetName = "path_area_ponto_6_2")



path_area_ponto_6_3 <- extract_lsm(mapbiomas_utm_6_3,
                                 y = ptos_coleta_6_3,
                                 extract_id = ptos_coleta_6_3$id_num,
                                 metric = "area",
                                 name = "patch area",
                                 type = "Area and edge metric",
                                 what = "lsm_p_area",
                                 directions = 8 # Specify the scale of the land cover map in meters
)
path_area_ponto_6_3 <-as.data.frame(subset(path_area_ponto_6_3, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_6_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_6_3.xlsx", sheetName = "path_area_ponto_6_3")



path_area_ponto_7 <- extract_lsm(mapbiomas_utm_7,
  y = ptos_coleta_7,
  extract_id = ptos_coleta_7$id_num,
  metric = "area",
  name = "patch area",
  type = "Area and edge metric",
  what = "lsm_p_area",
  directions = 8 # Specify the scale of the land cover map in meters
)
path_area_ponto_7 <-as.data.frame(subset(path_area_ponto_7, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_7, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_7.xlsx", sheetName = "path_area_ponto_7")



path_area_ponto_8 <- extract_lsm(mapbiomas_utm_8,
  y = ptos_coleta_8,
  extract_id = ptos_coleta_8$id_num,
  metric = "area",
  name = "patch area",
  type = "Area and edge metric",
  what = "lsm_p_area",
  directions = 8 # Specify the scale of the land cover map in meters
)
path_area_ponto_8 <-as.data.frame(subset(path_area_ponto_8, select=c(extract_id, level,metric, class, value)))
write.xlsx(path_area_ponto_8, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/path_area_ponto_8.xlsx", sheetName = "path_area_ponto_8")


#Exportar tabelas
# Combinação das tabelas
Table_patch_area  <- rbind(path_area_ponto_1,path_area_ponto_2, path_area_ponto_3_1, path_area_ponto_3_2, 
                           path_area_ponto_3_3, path_area_ponto_4,path_area_ponto_5, path_area_ponto_6_1, 
                           path_area_ponto_6_2, path_area_ponto_6_3, path_area_ponto_7, path_area_ponto_8)


library(openxlsx)#export table
#Exportar tabelas
write.xlsx(Table_patch_area, file= "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas")

#######################################################################################################################################


# - Percentage of landscape of class, "lsm_c_pland" Class em %--------------------------------------

# -buffers<-c(500, 1000,2000, 5000) -------------------------------------------------------------
#tamanho do buffer (raio)
buffers<- c(500, 1000,2000, 5000)
per_class_land_ponto_1<- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_1,
                      y=ptos_coleta_1,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_1$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_1 <-dcast(per_class_land_ponto_1 , plot_id ~ buffer + class)
per_class_land_ponto_1 [is.na(per_class_land_ponto_1 )] <- 0
per_class_land_ponto_1 <- per_class_land_ponto_1 %>% arrange(plot_id)


View(per_class_land_ponto_1 )
write.xlsx(per_class_land_ponto_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_1.xlsx", sheetName = "per_class_land_ponto_1")

per_class_land_ponto_2<- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_2,
                      y=ptos_coleta_2,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_2$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_2 <-dcast(per_class_land_ponto_2 , plot_id ~ buffer + class)
per_class_land_ponto_2 [is.na(per_class_land_ponto_2 )] <- 0
per_class_land_ponto_2 <- per_class_land_ponto_2 %>% arrange(plot_id)
View(per_class_land_ponto_2)
write.xlsx(per_class_land_ponto_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_2.xlsx", sheetName = "per_class_land_ponto_2")



# Verifique os CRS atuais
st_crs(ptos_coleta_3_1)
st_crs(mapbiomas_utm_3_1)

per_class_land_ponto_3_1<- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_3_1,
                      y=ptos_coleta_3_1,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_1$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_3_1 <-dcast(per_class_land_ponto_3_1 , plot_id ~ buffer + class)
per_class_land_ponto_3_1 [is.na(per_class_land_ponto_3_1 )] <- 0
per_class_land_ponto_3_1 <- per_class_land_ponto_3_1 %>% arrange(plot_id)
View(per_class_land_ponto_3_1)
write.xlsx(per_class_land_ponto_3_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_3_1.xlsx", sheetName = "per_class_land_ponto_3_1")


per_class_land_ponto_3_2<- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_3_2,
                      y=ptos_coleta_3_2,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_2$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_3_2 <-dcast(per_class_land_ponto_3_2 , plot_id ~ buffer + class)
per_class_land_ponto_3_2 [is.na(per_class_land_ponto_3_2 )] <- 0
per_class_land_ponto_3_2 <- per_class_land_ponto_3_2 %>% arrange(plot_id)
View(per_class_land_ponto_3_2)
write.xlsx(per_class_land_ponto_3_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_3_2.xlsx", sheetName = "per_class_land_ponto_3_2")


per_class_land_ponto_3_3<- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_3_3,
                      y=ptos_coleta_3_3,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_3$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_3_3 <-dcast(per_class_land_ponto_3_3 , plot_id ~ buffer + class)
per_class_land_ponto_3_3 [is.na(per_class_land_ponto_3_3 )] <- 0
per_class_land_ponto_3_3 <- per_class_land_ponto_3_3 %>% arrange(plot_id)
View(per_class_land_ponto_3_3)
write.xlsx(per_class_land_ponto_3_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_3_3.xlsx", sheetName = "per_class_land_ponto_3_3")


per_class_land_ponto_4 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_4,
                      y=ptos_coleta_4,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_4$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_4 <-dcast(per_class_land_ponto_4 , plot_id ~ buffer + class)
per_class_land_ponto_4 [is.na(per_class_land_ponto_4 )] <- 0
per_class_land_ponto_4 <- per_class_land_ponto_4 %>% arrange(plot_id)
View(per_class_land_ponto_4)
write.xlsx(per_class_land_ponto_4, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_4.xlsx", sheetName = "per_class_land_ponto_4")


per_class_land_ponto_5 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_5,
                      y=ptos_coleta_5,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_5$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_5 <-dcast(per_class_land_ponto_5 , plot_id ~ buffer + class)
per_class_land_ponto_5 [is.na(per_class_land_ponto_5 )] <- 0
per_class_land_ponto_5 <- per_class_land_ponto_5 %>% arrange(plot_id)
View(per_class_land_ponto_5)
write.xlsx(per_class_land_ponto_5, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_5.xlsx", sheetName = "per_class_land_ponto_5")

per_class_land_ponto_6_1 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_6_1,
                      y=ptos_coleta_6_1,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_1$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_6_1 <-dcast(per_class_land_ponto_6_1 , plot_id ~ buffer + class)
per_class_land_ponto_6_1 [is.na(per_class_land_ponto_6_1 )] <- 0
per_class_land_ponto_6_1 <- per_class_land_ponto_6_1 %>% arrange(plot_id)
View(per_class_land_ponto_6_1)
write.xlsx(per_class_land_ponto_6_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_6_1.xlsx", sheetName = "per_class_land_ponto_6_1")


per_class_land_ponto_6_2 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_6_2,
                      y=ptos_coleta_6_2,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_2$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_6_2 <-dcast(per_class_land_ponto_6_2 , plot_id ~ buffer + class)
per_class_land_ponto_6_2 [is.na(per_class_land_ponto_6_2 )] <- 0
per_class_land_ponto_6_2 <- per_class_land_ponto_6_2 %>% arrange(plot_id)
View(per_class_land_ponto_6_2)
write.xlsx(per_class_land_ponto_6_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_6_2.xlsx", sheetName = "per_class_land_ponto_6_2")


per_class_land_ponto_6_3 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_6_3,
                      y=ptos_coleta_6_3,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_3$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_6_3 <-dcast(per_class_land_ponto_6_3 , plot_id ~ buffer + class)
per_class_land_ponto_6_3 [is.na(per_class_land_ponto_6_3 )] <- 0
per_class_land_ponto_6_3 <- per_class_land_ponto_6_3 %>% arrange(plot_id)
View(per_class_land_ponto_6_3)
write.xlsx(per_class_land_ponto_6_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_6_3.xlsx", sheetName = "per_class_land_ponto_6_3")




per_class_land_ponto_7 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_7,
                      y=ptos_coleta_7,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_7$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_7 <-dcast(per_class_land_ponto_7 , plot_id ~ buffer + class)
per_class_land_ponto_7 [is.na(per_class_land_ponto_7 )] <- 0
per_class_land_ponto_7 <- per_class_land_ponto_7 %>% arrange(plot_id)
View(per_class_land_ponto_7)
write.xlsx(per_class_land_ponto_7, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_7.xlsx", sheetName = "per_class_land_ponto_7")


per_class_land_ponto_8 <- buffers%>%set_names()%>% 
  map_dfr(~sample_lsm(mapbiomas_utm_8,
                      y=ptos_coleta_8,
                      what = "lsm_c_pland",
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_8$id_num),
          .id = "buffer",  class = c(3)) 
# reshape
per_class_land_ponto_8 <-dcast(per_class_land_ponto_8 , plot_id ~ buffer + class)
per_class_land_ponto_8 [is.na(per_class_land_ponto_8 )] <- 0
per_class_land_ponto_8 <- per_class_land_ponto_8 %>% arrange(plot_id)
View(per_class_land_ponto_8)
write.xlsx(per_class_land_ponto_8, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/per_class_land_ponto_8.xlsx", sheetName = "per_class_land_ponto_8")


# Combinação das tabelas
Table_per_class_land <- rbind(per_class_land_ponto_1,per_class_land_ponto_2, per_class_land_ponto_3_1,
                          per_class_land_ponto_3_2 , per_class_land_ponto_3_3, per_class_land_ponto_4,
                          per_class_land_ponto_5, per_class_land_ponto_6_1, per_class_land_ponto_6_2,
                          per_class_land_ponto_6_3, per_class_land_ponto_7, per_class_land_ponto_8)





# Adicionar colunas ausentes com valor zero em cada data frame
per_class_land_ponto_1 <- per_class_land_ponto_1 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_2 <- per_class_land_ponto_2 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_3_1 <- per_class_land_ponto_3_1 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_3_2 <- per_class_land_ponto_3_2 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_3_3 <- per_class_land_ponto_3_3 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_4 <- per_class_land_ponto_4 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_5 <- per_class_land_ponto_5 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_6_1 <- per_class_land_ponto_6_1 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_6_2 <- per_class_land_ponto_6_2 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_6_3 <- per_class_land_ponto_6_3 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_7 <- per_class_land_ponto_7 %>%
  mutate(across(everything(), replace_na, replace = 0))

per_class_land_ponto_8 <- per_class_land_ponto_8 %>%
  mutate(across(everything(), replace_na, replace = 0))

# Combinar os data frames usando bind_rows()
Table_per_class_land <- bind_rows(
  per_class_land_ponto_1,
  per_class_land_ponto_2,
  per_class_land_ponto_3_1,
  per_class_land_ponto_3_2,
  per_class_land_ponto_3_3,
  per_class_land_ponto_4,
  per_class_land_ponto_5,
  per_class_land_ponto_6_1,
  per_class_land_ponto_6_2,
  per_class_land_ponto_6_3,
  per_class_land_ponto_7,
  per_class_land_ponto_8
)

# Substituir NaN por 0 em Table_per_class_land
Table_per_class_land[is.na(Table_per_class_land)] <- 0.000000

# Escrever o DataFrame modificado em um arquivo Excel
library(openxlsx)
write.xlsx(Table_per_class_land, file = "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/Table_per_class_land.xlsx", sheetName = "per_class_land_ponto")





# - area and edge metric, total edge, lsm_l_te, Units: Meters  ---------------------------------------------------------

# -buffers<-c(500, 1000,2000, 5000) -------------------------------------------------------------
#tamanho do buffer (raio) troca a cada escala
buffers<- c(500, 1000,2000, 5000)

# metricas multiplas escalas
total_edgea_ponto_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_1, 
                      y = ptos_coleta_1, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_1$id_num),
                      all_classes = TRUE,
                      return_raster = TRUE,
                      verbose = TRUE,
                      progress = TRUE, 
                      .id = "buffers")
# reshape
total_edgea_ponto_1<-dcast(total_edgea_ponto_1, plot_id ~ buffers )
total_edgea_ponto_1 [is.na(total_edgea_ponto_1 )] <- 0
total_edgea_ponto_1 <- total_edgea_ponto_1 %>% arrange(plot_id)
View(total_edgea_ponto_1)
write.xlsx(total_edgea_ponto_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_1.xlsx", sheetName = "total_edgea_ponto_1")





total_edgea_ponto_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_2, 
                      y = ptos_coleta_2, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_2<-dcast(total_edgea_ponto_2, plot_id ~ buffers )
total_edgea_ponto_2 [is.na(total_edgea_ponto_2 )] <- 0
total_edgea_ponto_2 <- total_edgea_ponto_2 %>% arrange(plot_id)
View(total_edgea_ponto_2)
write.xlsx(total_edgea_ponto_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_2.xlsx", sheetName = "total_edgea_ponto_2")


total_edgea_ponto_3_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_1, 
                      y = ptos_coleta_3_1, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_1$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_3_1<-dcast(total_edgea_ponto_3_1, plot_id ~ buffers )
total_edgea_ponto_3_1 [is.na(total_edgea_ponto_3_1 )] <- 0
total_edgea_ponto_3_1 <- total_edgea_ponto_3_1 %>% arrange(plot_id)
View(total_edgea_ponto_3_1)
write.xlsx(total_edgea_ponto_3_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_3_1.xlsx", sheetName = "total_edgea_ponto_3_1")

total_edgea_ponto_3_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_2, 
                      y = ptos_coleta_3_2, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_3_2<-dcast(total_edgea_ponto_3_2, plot_id ~ buffers )
total_edgea_ponto_3_2 [is.na(total_edgea_ponto_3_2 )] <- 0
total_edgea_ponto_3_2 <- total_edgea_ponto_3_2 %>% arrange(plot_id)
View(total_edgea_ponto_3_2)
write.xlsx(total_edgea_ponto_3_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_3_2.xlsx", sheetName = "total_edgea_ponto_3_2")


total_edgea_ponto_3_3 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_3, 
                      y = ptos_coleta_3_3, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_3$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_3_3<-dcast(total_edgea_ponto_3_3, plot_id ~ buffers )
total_edgea_ponto_3_3 [is.na(total_edgea_ponto_3_3 )] <- 0
total_edgea_ponto_3_3 <- total_edgea_ponto_3_3 %>% arrange(plot_id)
View(total_edgea_ponto_3_3)
write.xlsx(total_edgea_ponto_3_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_3_3.xlsx", sheetName = "total_edgea_ponto_3_3")



total_edgea_ponto_4 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_4, 
                      y = ptos_coleta_4, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_4$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_4<-dcast(total_edgea_ponto_4, plot_id ~ buffers )
total_edgea_ponto_4 [is.na(total_edgea_ponto_4)] <- 0
total_edgea_ponto_4 <- total_edgea_ponto_4 %>% arrange(plot_id)
View(total_edgea_ponto_4)
write.xlsx(total_edgea_ponto_4, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_4.xlsx", sheetName = "total_edgea_ponto_4")


total_edgea_ponto_5 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_5, 
                      y = ptos_coleta_5, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_5$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_5<-dcast(total_edgea_ponto_5, plot_id ~ buffers )
total_edgea_ponto_5 [is.na(total_edgea_ponto_5)] <- 0
total_edgea_ponto_5 <- total_edgea_ponto_5 %>% arrange(plot_id)
View(total_edgea_ponto_5)
write.xlsx(total_edgea_ponto_5, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_5.xlsx", sheetName = "total_edgea_ponto_5")



total_edgea_ponto_6_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_1, 
                      y = ptos_coleta_6_1, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_1$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_6_1 <-dcast(total_edgea_ponto_6_1, plot_id ~ buffers )
total_edgea_ponto_6_1 [is.na(total_edgea_ponto_6_1)] <- 0
total_edgea_ponto_6_1 <- total_edgea_ponto_6_1 %>% arrange(plot_id)
View(total_edgea_ponto_6_1)
write.xlsx(total_edgea_ponto_6_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_6_1.xlsx", sheetName = "total_edgea_ponto_6_1")


total_edgea_ponto_6_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_2, 
                      y = ptos_coleta_6_2, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_6_2 <-dcast(total_edgea_ponto_6_2, plot_id ~ buffers )
total_edgea_ponto_6_2 [is.na(total_edgea_ponto_6_2)] <- 0
total_edgea_ponto_6_2 <- total_edgea_ponto_6_2 %>% arrange(plot_id)
View(total_edgea_ponto_6_2)
write.xlsx(total_edgea_ponto_6_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_6_2.xlsx", sheetName = "total_edgea_ponto_6_2")


total_edgea_ponto_6_3 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_3, 
                      y = ptos_coleta_6_3, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_3$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_6_3 <-dcast(total_edgea_ponto_6_3, plot_id ~ buffers )
total_edgea_ponto_6_3 [is.na(total_edgea_ponto_6_3)] <- 0
total_edgea_ponto_6_3 <- total_edgea_ponto_6_3 %>% arrange(plot_id)
View(total_edgea_ponto_6_3)
write.xlsx(total_edgea_ponto_6_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_6_3.xlsx", sheetName = "total_edgea_ponto_6_3")


total_edgea_ponto_7 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_7, 
                      y = ptos_coleta_7, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_7$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_7 <-dcast(total_edgea_ponto_7, plot_id ~ buffers )
total_edgea_ponto_7 [is.na(total_edgea_ponto_7)] <- 0
total_edgea_ponto_7 <- total_edgea_ponto_7 %>% arrange(plot_id)
View(total_edgea_ponto_7)
write.xlsx(total_edgea_ponto_7, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_7.xlsx", sheetName = "total_edgea_ponto_7")


total_edgea_ponto_8 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_8, 
                      y = ptos_coleta_8, 
                      what = "lsm_l_te", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_8$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
total_edgea_ponto_8 <-dcast(total_edgea_ponto_8, plot_id ~ buffers )
total_edgea_ponto_8 [is.na(total_edgea_ponto_8)] <- 0
total_edgea_ponto_8 <- total_edgea_ponto_8 %>% arrange(plot_id)
View(total_edgea_ponto_8)
write.xlsx(total_edgea_ponto_8, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edgea_ponto_8.xlsx", sheetName = "total_edgea_ponto_8")

# Combinação das tabelas
Table_total_edge <- rbind(total_edgea_ponto_1, total_edgea_ponto_2, total_edgea_ponto_3_1,
                          total_edgea_ponto_3_2, total_edgea_ponto_3_3, total_edgea_ponto_4,
                          total_edgea_ponto_5, total_edgea_ponto_6_1, total_edgea_ponto_6_2,
                          total_edgea_ponto_6_3, total_edgea_ponto_7, total_edgea_ponto_8)
#Exportar tabelas
write.xlsx(Table_total_edge, file= "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/total_edge.xlsx", sheetName = "Table_total_edge")




# -patch cohesion index, aggregation metric, cohesion, lsm_c_cohesion , Units: Percent --------------------------------------------------------

# -buffers<-c(500, 1000,2000, 5000) -------------------------------------------------------------
#tamanho do buffer (raio)
buffers<-c(500, 1000,2000, 5000) 

lsm_c_cohesion_ponto_1 <- list(location = ptos_coleta_1$id_num)  # 
lsm_c_cohesion_ponto_1 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_1,
                       y = ptos_coleta_1,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_1$location),
          .id = "buffer",
          class = c(3))

# reshape
lsm_c_cohesion_ponto_1  <-dcast(lsm_c_cohesion_ponto_1  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_1 [is.na(lsm_c_cohesion_ponto_1 )] <- 0
lsm_c_cohesion_ponto_1 <- lsm_c_cohesion_ponto_1 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_1 )
write.xlsx(lsm_c_cohesion_ponto_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_1.xlsx", sheetName = "lsm_c_cohesion_ponto_1")


lsm_c_cohesion_ponto_2<- list(location = ptos_coleta_2$id_num)  # 
lsm_c_cohesion_ponto_2 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_2,
                       y = ptos_coleta_2,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_2$location),
          .id = "buffer",
          class = c(3))

# reshape
lsm_c_cohesion_ponto_2  <-dcast(lsm_c_cohesion_ponto_2  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_2 [is.na(lsm_c_cohesion_ponto_2 )] <- 0
lsm_c_cohesion_ponto_2 <- lsm_c_cohesion_ponto_2 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_2 )
write.xlsx(lsm_c_cohesion_ponto_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_2.xlsx", sheetName = "lsm_c_cohesion_ponto_2")


lsm_c_cohesion_ponto_3_1 <- list(location = ptos_coleta_3_1$id_num)  # 
lsm_c_cohesion_ponto_3_1 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_3_1,
                       y = ptos_coleta_3_1,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_3_1$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_3_1  <-dcast(lsm_c_cohesion_ponto_3_1  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_3_1 [is.na(lsm_c_cohesion_ponto_3_1 )] <- 0
lsm_c_cohesion_ponto_3_1 <- lsm_c_cohesion_ponto_3_1 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_3_1)
write.xlsx(lsm_c_cohesion_ponto_3_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_3_1.xlsx", sheetName = "lsm_c_cohesion_ponto_3_1")


lsm_c_cohesion_ponto_3_2 <- list(location = ptos_coleta_3_2$id_num)  # 
lsm_c_cohesion_ponto_3_2 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_3_2,
                       y = ptos_coleta_3_2,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_3_2$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_3_2  <-dcast(lsm_c_cohesion_ponto_3_2  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_3_2 [is.na(lsm_c_cohesion_ponto_3_2 )] <- 0
lsm_c_cohesion_ponto_3_2 <- lsm_c_cohesion_ponto_3_2 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_3_2)
write.xlsx(lsm_c_cohesion_ponto_3_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_3_2.xlsx", sheetName = "lsm_c_cohesion_ponto_3_2")

lsm_c_cohesion_ponto_3_3 <- list(location = ptos_coleta_3_3$id_num)  # 
lsm_c_cohesion_ponto_3_3 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_3_3,
                       y = ptos_coleta_3_3,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_3_3$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_3_3  <-dcast(lsm_c_cohesion_ponto_3_3  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_3_3 [is.na(lsm_c_cohesion_ponto_3_3 )] <- 0
lsm_c_cohesion_ponto_3_3 <- lsm_c_cohesion_ponto_3_3 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_3_3)
write.xlsx(lsm_c_cohesion_ponto_3_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_3_3.xlsx", sheetName = "lsm_c_cohesion_ponto_3_3")



lsm_c_cohesion_ponto_4 <- list(location = ptos_coleta_4$id_num)  # 
lsm_c_cohesion_ponto_4 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_4,
                       y = ptos_coleta_4,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_4$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_4  <-dcast(lsm_c_cohesion_ponto_4  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_4 [is.na(lsm_c_cohesion_ponto_4 )] <- 0
lsm_c_cohesion_ponto_4 <- lsm_c_cohesion_ponto_4 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_4)
write.xlsx(lsm_c_cohesion_ponto_4, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_4.xlsx", sheetName = "lsm_c_cohesion_ponto_4")



lsm_c_cohesion_ponto_5 <- list(location = ptos_coleta_5$id_num)  # 
lsm_c_cohesion_ponto_5 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_5,
                       y = ptos_coleta_5,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_5$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_5  <-dcast(lsm_c_cohesion_ponto_5  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_5 [is.na(lsm_c_cohesion_ponto_5 )] <- 0
lsm_c_cohesion_ponto_5 <- lsm_c_cohesion_ponto_5 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_5)
write.xlsx(lsm_c_cohesion_ponto_5, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_5.xlsx", sheetName = "lsm_c_cohesion_ponto_5")



lsm_c_cohesion_ponto_6_1 <- list(location = ptos_coleta_6_1$id_num)  # 
lsm_c_cohesion_ponto_6_1 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_6_1,
                       y = ptos_coleta_6_1,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_6_1$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_6_1  <-dcast(lsm_c_cohesion_ponto_6_1  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_6_1 [is.na(lsm_c_cohesion_ponto_6_1 )] <- 0
lsm_c_cohesion_ponto_6_1 <- lsm_c_cohesion_ponto_6_1 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_6_1)
write.xlsx(lsm_c_cohesion_ponto_6_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_6_1.xlsx", sheetName = "lsm_c_cohesion_ponto_6_1")


lsm_c_cohesion_ponto_6_2 <- list(location = ptos_coleta_6_2$id_num)  # 
lsm_c_cohesion_ponto_6_2 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_6_2,
                       y = ptos_coleta_6_2,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_6_2$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_6_2  <-dcast(lsm_c_cohesion_ponto_6_2  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_6_2 [is.na(lsm_c_cohesion_ponto_6_2 )] <- 0
lsm_c_cohesion_ponto_6_2 <- lsm_c_cohesion_ponto_6_2 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_6_2)
write.xlsx(lsm_c_cohesion_ponto_6_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_6_2.xlsx", sheetName = "lsm_c_cohesion_ponto_6_2")


lsm_c_cohesion_ponto_6_3 <- list(location = ptos_coleta_6_3$id_num)  # 
lsm_c_cohesion_ponto_6_3 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_6_3,
                       y = ptos_coleta_6_3,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_6_3$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_6_3  <-dcast(lsm_c_cohesion_ponto_6_3  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_6_3 [is.na(lsm_c_cohesion_ponto_6_3 )] <- 0
lsm_c_cohesion_ponto_6_3 <- lsm_c_cohesion_ponto_6_3 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_6_3)
write.xlsx(lsm_c_cohesion_ponto_6_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_6_3.xlsx", sheetName = "lsm_c_cohesion_ponto_6_3")



lsm_c_cohesion_ponto_7 <- list(location = ptos_coleta_7$id_num)  # 
lsm_c_cohesion_ponto_7 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_7,
                       y = ptos_coleta_7,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_7$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_7  <-dcast(lsm_c_cohesion_ponto_7  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_7 [is.na(lsm_c_cohesion_ponto_7 )] <- 0
lsm_c_cohesion_ponto_7 <- lsm_c_cohesion_ponto_7 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_7)
write.xlsx(lsm_c_cohesion_ponto_7, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_7.xlsx", sheetName = "lsm_c_cohesion_ponto_7")


lsm_c_cohesion_ponto_8 <- list(location = ptos_coleta_8$id_num)  # 
lsm_c_cohesion_ponto_8 <- buffers %>% set_names() %>% 
  map_dfr(~ sample_lsm(mapbiomas_utm_8,
                       y = ptos_coleta_8,
                       what = "lsm_c_cohesion",
                       size = .,
                       shape = "circle",
                       directions = 8,
                       lsm_c_cohesion_ponto_8$location),
          .id = "buffer",
          class = c(3))

lsm_c_cohesion_ponto_8  <-dcast(lsm_c_cohesion_ponto_8  , plot_id ~ buffer + class)
lsm_c_cohesion_ponto_8 [is.na(lsm_c_cohesion_ponto_8 )] <- 0
lsm_c_cohesion_ponto_8 <- lsm_c_cohesion_ponto_8 %>% arrange(plot_id)
View(lsm_c_cohesion_ponto_8)
write.xlsx(lsm_c_cohesion_ponto_8, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/lsm_c_cohesion_ponto_8.xlsx", sheetName = "lsm_c_cohesion_ponto_8")


# Adicionar colunas ausentes com valor zero em cada data frame
lsm_c_cohesion_ponto_1 <- lsm_c_cohesion_ponto_1 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_2 <- lsm_c_cohesion_ponto_2 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_3_1 <- lsm_c_cohesion_ponto_3_1%>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_3_2 <- lsm_c_cohesion_ponto_3_2%>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_3_3 <- lsm_c_cohesion_ponto_3_3%>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_4 <- lsm_c_cohesion_ponto_4%>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_5 <- lsm_c_cohesion_ponto_5 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_6_1 <- lsm_c_cohesion_ponto_6_1 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_6_2 <- lsm_c_cohesion_ponto_6_2 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_6_3 <- lsm_c_cohesion_ponto_6_3 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_7 <- lsm_c_cohesion_ponto_7 %>%
  mutate(across(everything(), replace_na, replace = 0))

lsm_c_cohesion_ponto_8 <- lsm_c_cohesion_ponto_8 %>%
  mutate(across(everything(), replace_na, replace = 0))

# Combinar os data frames usando bind_rows()
Table_patch_cohesion_index <- bind_rows(
  lsm_c_cohesion_ponto_1,
  lsm_c_cohesion_ponto_2,
  lsm_c_cohesion_ponto_3_1,
  lsm_c_cohesion_ponto_3_2,
  lsm_c_cohesion_ponto_3_3,
  lsm_c_cohesion_ponto_4,
  lsm_c_cohesion_ponto_5,
  lsm_c_cohesion_ponto_6_1,
  lsm_c_cohesion_ponto_6_2,
  lsm_c_cohesion_ponto_6_3,
  lsm_c_cohesion_ponto_7,
  lsm_c_cohesion_ponto_8
)

# Substituir NaN por 0 em Table_patch_cohesion_index
Table_patch_cohesion_index[is.na(Table_patch_cohesion_index)] <- 0.000000
#Exportar tabelas
write.xlsx(Table_patch_cohesion_index, file= "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/Table_patch_cohesion_index.xlsx", sheetName = "Table_patch_cohesion_index")




# -Integrity --------------------------------------------------------------
#library(raster)

po <- sf::st_read("E:/04_rstudio/01_Luis/Dados_location", layer= "Pontos_Luiz")

landscape <- raster("E:/04_rstudio/01_Luis/Dados_location/Raster_SouthAmerica_integrity_metrics.tif")

landscape[landscape == -9999] <- NA 

Integrity_metrics <-data.frame(raster::extract(landscape, po), extract_id=po$location)

Integritry_metrics[,1]<-Integritry_metrics[,1]/1000

colnames(Integritry_metrics)<-c("Integrity", "location")
Integritry_metrics <-as.data.frame(subset(flii_peldx, select=c(location, Integrity)))
write.xlsx(Integritry_metrics, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/Integrity.xlsx", sep="\t", dec=",")

#tamanho do buffer (raio)

#Integritry_metrics_500 <- data.frame(raster::extract(landscape, po, buffer = 500, fun = mean, na.rm = TRUE), extract_id=po$location)
#Integritry_metrics_500 [,1]<-Integritry_metrics_500 [,1]/1000
#colnames(Integritry_metrics_500)<-c("Integrity_500", "location")
#write.xlsx(Integritry_metrics, file= "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/Integritry_metrics.xlsx", sheetName = "Integritry_metrics")

# -shannon's diversity index, diversity metric,lsm_l_shdi, Heterogenidade, Units: None ----------------------------------------------

# -buffers<-c(500, 1000,2000, 5000) -------------------------------------------------------------
#tamanho do buffer (raio) troca a cada escala
buffers<- c(500, 1000,2000, 5000)

# metricas multiplas escalas
diversity_metric_ponto_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_1, 
                      y = ptos_coleta_1, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_1$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_1<-dcast(diversity_metric_ponto_1, plot_id ~ buffers )
diversity_metric_ponto_1 [is.na(diversity_metric_ponto_1)] <- 0
diversity_metric_ponto_1 <- diversity_metric_ponto_1 %>% arrange(plot_id)
View(diversity_metric_ponto_1)
write.xlsx(diversity_metric_ponto_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_1.xlsx", sheetName = "diversity_metric_ponto_1")



# metricas multiplas escalas
diversity_metric_ponto_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_2, 
                      y = ptos_coleta_2, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_2 <-dcast(diversity_metric_ponto_2, plot_id ~ buffers )
diversity_metric_ponto_2 [is.na(diversity_metric_ponto_2)] <- 0
diversity_metric_ponto_2 <- diversity_metric_ponto_2 %>% arrange(plot_id)
View(diversity_metric_ponto_2)
write.xlsx(diversity_metric_ponto_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_2.xlsx", sheetName = "diversity_metric_ponto_2")

diversity_metric_ponto_3_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_1, 
                      y = ptos_coleta_3_1, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_1$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_3_1 <-dcast(diversity_metric_ponto_3_1, plot_id ~ buffers )
diversity_metric_ponto_3_1 [is.na(diversity_metric_ponto_3_1)] <- 0
diversity_metric_ponto_3_1 <- diversity_metric_ponto_3_1 %>% arrange(plot_id)
View(diversity_metric_ponto_3_1)
write.xlsx(diversity_metric_ponto_3_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_3_1.xlsx", sheetName = "diversity_metric_ponto_3_1")


diversity_metric_ponto_3_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_2, 
                      y = ptos_coleta_3_2, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_3_2 <-dcast(diversity_metric_ponto_3_2, plot_id ~ buffers )
diversity_metric_ponto_3_2 [is.na(diversity_metric_ponto_3_2)] <- 0
diversity_metric_ponto_3_2 <- diversity_metric_ponto_3_2 %>% arrange(plot_id)
View(diversity_metric_ponto_3_2)
write.xlsx(diversity_metric_ponto_3_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_3_2.xlsx", sheetName = "diversity_metric_ponto_3_2")


diversity_metric_ponto_3_3 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_3_3, 
                      y = ptos_coleta_3_3, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_3_3$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_3_3 <-dcast(diversity_metric_ponto_3_3, plot_id ~ buffers )
diversity_metric_ponto_3_3 [is.na(diversity_metric_ponto_3_3)] <- 0
diversity_metric_ponto_3_3 <- diversity_metric_ponto_3_3 %>% arrange(plot_id)
View(diversity_metric_ponto_3_3)
write.xlsx(diversity_metric_ponto_3_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_3_3.xlsx", sheetName = "diversity_metric_ponto_3_3")



diversity_metric_ponto_4 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_4, 
                      y = ptos_coleta_4, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_4$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_4 <-dcast(diversity_metric_ponto_4, plot_id ~ buffers )
diversity_metric_ponto_4 [is.na(diversity_metric_ponto_4)] <- 0
diversity_metric_ponto_4 <- diversity_metric_ponto_4 %>% arrange(plot_id)
View(diversity_metric_ponto_4)
write.xlsx(diversity_metric_ponto_4, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_4.xlsx", sheetName = "diversity_metric_ponto_4")

diversity_metric_ponto_5 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_5, 
                      y = ptos_coleta_5, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_5$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_5 <-dcast(diversity_metric_ponto_5, plot_id ~ buffers )
diversity_metric_ponto_5 [is.na(diversity_metric_ponto_5)] <- 0
diversity_metric_ponto_5 <- diversity_metric_ponto_5 %>% arrange(plot_id)
View(diversity_metric_ponto_5)
write.xlsx(diversity_metric_ponto_5, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_5.xlsx", sheetName = "diversity_metric_ponto_5")

diversity_metric_ponto_6_1 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_1, 
                      y = ptos_coleta_6_1, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_1$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_6_1 <-dcast(diversity_metric_ponto_6_1, plot_id ~ buffers )
diversity_metric_ponto_6_1 [is.na(diversity_metric_ponto_6_1)] <- 0
diversity_metric_ponto_6_1 <- diversity_metric_ponto_6_1 %>% arrange(plot_id)
View(diversity_metric_ponto_6_1)
write.xlsx(diversity_metric_ponto_6_1, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_6_1.xlsx", sheetName = "diversity_metric_ponto_6_1")

diversity_metric_ponto_6_2 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_2, 
                      y = ptos_coleta_6_2, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_2$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_6_2 <-dcast(diversity_metric_ponto_6_2, plot_id ~ buffers )
diversity_metric_ponto_6_2 [is.na(diversity_metric_ponto_6_2)] <- 0
diversity_metric_ponto_6_2 <- diversity_metric_ponto_6_2 %>% arrange(plot_id)
View(diversity_metric_ponto_6_2)
write.xlsx(diversity_metric_ponto_6_2, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_6_2.xlsx", sheetName = "diversity_metric_ponto_6_2")



diversity_metric_ponto_6_3 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_6_3, 
                      y = ptos_coleta_6_3, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_6_3$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_6_3 <-dcast(diversity_metric_ponto_6_3, plot_id ~ buffers )
diversity_metric_ponto_6_3 [is.na(diversity_metric_ponto_6_3)] <- 0
diversity_metric_ponto_6_3 <- diversity_metric_ponto_6_3 %>% arrange(plot_id)
View(diversity_metric_ponto_6_3)
write.xlsx(diversity_metric_ponto_6_3, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_6_3.xlsx", sheetName = "diversity_metric_ponto_6_3")


diversity_metric_ponto_7 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_7, 
                      y = ptos_coleta_7, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_7$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_7 <-dcast(diversity_metric_ponto_7, plot_id ~ buffers )
diversity_metric_ponto_7 [is.na(diversity_metric_ponto_7)] <- 0
diversity_metric_ponto_7 <- diversity_metric_ponto_7 %>% arrange(plot_id)
View(diversity_metric_ponto_7)
write.xlsx(diversity_metric_ponto_7, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_7.xlsx", sheetName = "diversity_metric_ponto_7")

diversity_metric_ponto_8 <- buffers %>%  set_names() %>% 
  map_dfr(~sample_lsm(landscape = mapbiomas_utm_8, 
                      y = ptos_coleta_8, 
                      what = "lsm_l_shdi", #c("lsm_c_np", "lsm_c_area_mn", "lsm_c_pland"),
                      size=.,shape="circle", 
                      directions=8, 
                      plot_id=ptos_coleta_8$id_num),
          all_classes = TRUE,
          return_raster = TRUE,
          verbose = TRUE,
          progress = TRUE, 
          .id = "buffers")
# reshape
diversity_metric_ponto_8 <-dcast(diversity_metric_ponto_8, plot_id ~ buffers )
diversity_metric_ponto_8 [is.na(diversity_metric_ponto_8)] <- 0
diversity_metric_ponto_8 <- diversity_metric_ponto_8 %>% arrange(plot_id)
View(diversity_metric_ponto_8)
write.xlsx(diversity_metric_ponto_8, "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric_ponto_8.xlsx", sheetName = "diversity_metric_ponto_8")

# Combinação das tabelas
diversity_metric <- rbind(diversity_metric_ponto_1, diversity_metric_ponto_2, diversity_metric_ponto_3_1, 
                          diversity_metric_ponto_3_2, diversity_metric_ponto_3_3, diversity_metric_ponto_4,
                          diversity_metric_ponto_5, diversity_metric_ponto_6_1, diversity_metric_ponto_6_2,
                          diversity_metric_ponto_6_3,diversity_metric_ponto_7, diversity_metric_ponto_8)
#Exportar tabelas
write.xlsx(diversity_metric, file= "E:/04_rstudio/01_Luis/Dados_location/Tabelas_metricas/diversity_metric.xlsx", sheetName = "diversity_metric")

