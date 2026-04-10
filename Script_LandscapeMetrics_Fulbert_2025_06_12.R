# Analises landscapemetrics colaboração Luis# 
# Fulbertgnonlonfoun@gmail.com #
# Leec/unesp/RC #
#2025_06_d30
# 1 Landscapemetrics - -----------------------------------------------------
#Pacotes utilizados
pacotes <- c("landscapemetrics", "rgdal", "dplyr", "purrr", 
             "raster","psych", "vegan","tidyverse", "forcats", 
             "iNEXT","reshape2", "rgeos","wesanderson",
             "pak", "sf", "progress", "pbapply","readxl", "openxlsx" ,
             "reshape2","knitr", "remotes", "mgcv", "iNEXT", "terra",
             "tibble", "tmap", "RColorBrewer","sf","openxlsx")

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

#remotes::install_version("rgdal", version = "1.6-7", repos = "https://cloud.r-project.org")
#remotes::install_version("rgeos", version = "0.5-9", repos = "https://cloud.r-project.org")

# - -----------------------------------------------------------------------
#Apagar as listas
rm(list=ls())

# importar os dados
#setwd("C:/Fulbert_Dados/02_Mestrado/00_Projeto_Mestrado/10_coloboração/Luís_Felipes_Daibes_Banco_sementes/Dados_location")
setwd("D:/01_coloboração/Luís_Felipes_Daibes_Banco_sementes/Dados_location")

# listar os arquivos
dir()# importar os dados

Data_luiz <-  read_excel("Pontos_Local_Luis_lat_long_filtrado_V2.xlsx")
data_luiz_filtrado <- distinct(Data_luiz)
Data_luiz

#
#################################################################################################
#Subindo pontos centrais de cada paisagem em formato shapefile
# Caminho base para os shapefiles
caminho_base <- "F:/01_coloboração/Luís_Felipes_Daibes_Banco_sementes/Dados_location"

# Lista com os nomes dos shapefiles (sem extensão)
arquivos <- c("Pontos_Luiz.rec_1", "Pontos_Luiz.rec_2", "Pontos_Luiz.rec_3",
              "Pontos_Luiz.rec_4", "Pontos_Luiz.rec_5", "Pontos_Luiz.rec_6_1",
              "Pontos_Luiz.rec_6_2", "Pontos_Luiz.rec_6_3", "Pontos_Luiz.rec_7",
              "Pontos_Luiz.rec_8")

# Lê todos os arquivos shapefile como objetos sf
pontos_lista <- lapply(arquivos, function(nome) {
  st_read(dsn = caminho_base, layer = nome, quiet = TRUE)
})

# Nomeia os objetos de forma automática
names(pontos_lista) <- paste0("ptos_coleta_", c(1:5, "6_1", "6_2", "6_3", 7, 8))

# Atribui os objetos à memória global (opcional, mas útil se você quiser usar os nomes diretamente)
list2env(pontos_lista, envir = .GlobalEnv)

# Exemplo de visualização
plot(ptos_coleta_1$geometry, col = "black", pch = 19, axes = TRUE, graticule = TRUE)

# Exemplo para visualizar os atributos (sem geometria)
sf::st_drop_geometry(ptos_coleta_1)
#Para ver os pontos na Mapa
require(tmap)

tmap_mode("view")

tm_shape(ptos_coleta_2) + tm_sf(col="red", size=1)

#######################################################################################
#rateres
# landscape raster
mapbiomas_1 <- raster::raster("Raster_MA_rec_1.tif")
mapbiomas_2 <- raster::raster("Raster_MA_rec_2.tif")
mapbiomas_3 <- raster::raster("Raster_MA_rec_3.tif")
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
mapbiomas_utm_3 <- raster::projectRaster(mapbiomas_3, 
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


# Verificar o CRS atual do raster
crs(mapbiomas_utm_8)



#  check_landscape() dar numero de class no raster 
check_landscape(mapbiomas_utm_1)
check_landscape(mapbiomas_utm_2)
check_landscape(mapbiomas_utm_3)
check_landscape(mapbiomas_utm_4)
check_landscape(mapbiomas_utm_5)
check_landscape(mapbiomas_utm_6_1)
check_landscape(mapbiomas_utm_6_2)
check_landscape(mapbiomas_utm_6_3)
check_landscape(mapbiomas_utm_7)
check_landscape(mapbiomas_utm_8)

# - -----------------------------------------------------------------------
#nomes das métricas
metrics.names<-lsm_abbreviations_names #Metricas paisagem
View(metrics.names)
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



# ------------------------------------------------------ ------------------
#Calculando métricas
# Vetor com os buffers
buffers <- c(500, 1000,1500, 2000, 5000)

# Lista com os objetos raster e pontos de coleta
mapbiomas_list <- list(
  mapbiomas_utm_1, mapbiomas_utm_2, mapbiomas_utm_3, mapbiomas_utm_4,
  mapbiomas_utm_5, mapbiomas_utm_6_1, mapbiomas_utm_6_2, mapbiomas_utm_6_3,
  mapbiomas_utm_7, mapbiomas_utm_8)

pontos_list <- list(
  ptos_coleta_1, ptos_coleta_2, ptos_coleta_3, ptos_coleta_4,
  ptos_coleta_5, ptos_coleta_6_1, ptos_coleta_6_2, ptos_coleta_6_3,
  ptos_coleta_7, ptos_coleta_8)

# - -----------------------------------------------------------------------
# - Percentage of landscape of class, "lsm_c_pland" Class em %------------------

# -buffers<-c(500, 1000, 1500, 2000, 5000) -------------------------------------
#tamanho do buffer (raio)

# Nomes para os arquivos
nomes <- paste0("per_class_land_ponto_", c(1:5, "6_1", "6_2", "6_3", 7, 8))

# Caminho base para salvar os arquivos
path_base <- "F:/01_coloboração/Luís_Felipes_Daibes_Banco_sementes/Dados_location/Tabelas_metricas/"

# Função principal
calcular_metricas <- function(raster, pontos, nome) {
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_pland",
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer", class = c(3))
  
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, ".xlsx"), sheetName = nome)
  
  return(resultado)}

# Aplicar função para todas as combinações
resultados <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas)

# Combinar todos os dataframes
Table_per_class_land <- bind_rows(resultados)

# Exportar tabela final
write.xlsx(Table_per_class_land, file = paste0(path_base, "Table_per_class_land.xlsx"), sheetName = "per_class_land_ponto")

################################################################################
# - -----------------------------------------------------------------------
# - Number of patches (Aggregation metric), "lsm_c_np" where ni is the number of patches-------
#tamanho do buffer (raio)


calcular_metricas_np <- function(raster, pontos, nome) {
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_np",  # Alterado para "lsm_c_np"
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")
  
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)

  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_num_patches.xlsx"), sheetName = nome)
  
  return(resultado)
}  
  
# Aplicar função para todas as combinações
resultados_np <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_np)

# Combinar todos os resultados
Table_num_patches <- bind_rows(resultados_np)

# Exportar tabela final
write.xlsx(Table_num_patches, file = paste0(path_base, "Table_num_patches.xlsx"), sheetName = "num_patches")
############

# Filtrar os dados para remover 'class' igual a 0, e depois agrupar por 'plot_id' e 'class'
Table_num_patches_resumida_class <- Table_num_patches %>%
  filter(class != 0) %>%  # Remove as linhas onde 'class' é igual a 0
  group_by(plot_id, class) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),   # Soma dos valores por plot_id e class
    #media_value = mean(value, na.rm = TRUE),  # Média dos valores por plot_id e class
    .groups = 'drop'  # Para evitar manter os grupos após o summarise
  )

# Ver os resultados
View(Table_num_patches_resumida_class)

# Caso queira exportar os resultados
write.xlsx(Table_num_patches_resumida_class, file = paste0(path_base, "Table_num_patches_resumida_class_sem_0.xlsx"), sheetName = "resumo_num_patches_class")

###########

# Filtrar os dados para remover 'class' igual a 0, e depois agrupar por 'plot_id'
Table_num_patches_resumida <- Table_num_patches %>%
  filter(class != 0) %>%  # Remove as linhas onde 'class' é igual a 0
  group_by(plot_id) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),   # Soma dos valores por plot_id
    #media_value = mean(value, na.rm = TRUE),  # Média dos valores por plot_id
    .groups = 'drop'  # Para evitar manter os grupos após o summarise
  )

# Ver os resultados
View(Table_num_patches_resumida)

# Caso queira exportar os resultados
write.xlsx(Table_num_patches_resumida, file = paste0(path_base, "Table_num_patches_resumida_sem_class_0.xlsx"), sheetName = "resumo_num_patches")


################################################################################
# - -----------------------------------------------------------------------
# - area and edge metric, total edge, lsm_l_te, Units: Meters  
calcular_metricas_te <- function(raster, pontos, nome) {
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_l_te",  # Alterado para "lsm_l_te"
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")

  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_total_edge.xlsx"), sheetName = nome)
  
  return(resultado)}    
 
resultados_te <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_te)

# Combinar todos os resultados
Table_total_edge <- bind_rows(resultados_te)

# Exportar tabela final
write.xlsx(Table_total_edge, file = paste0(path_base, "Table_total_edge.xlsx"), sheetName = "total_edge")


################################################################################
# - -----------------------------------------------------------------------

# -patch cohesion index, aggregation metric, cohesion, lsm_c_cohesion , Units: Percent 

calcular_metricas_cohesion <- function(raster, pontos, nome) {
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_cohesion",  # Alterado para "lsm_c_cohesion"
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")
  
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_cohesion.xlsx"), sheetName = nome)
  
  return(resultado)}  
  
resultados_cohesion <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_cohesion)

# Combinar todos os resultados
Table_cohesion <- bind_rows(resultados_cohesion)

# Exportar tabela final
write.xlsx(Table_cohesion, file = paste0(path_base, "Table_cohesion.xlsx"), sheetName = "cohesion")


################################################################################
# - -----------------------------------------------------------------------
# -shannon's diversity index, diversity metric,lsm_l_shdi, Heterogenidade, Units: None 
calcular_metricas_shdi <- function(raster, pontos, nome) {
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_l_shdi",  # Alterado para "lsm_l_shdi"
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_shdi.xlsx"), sheetName = nome)
  
  return(resultado)}    
  
resultados_shdi <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_shdi)

# Combinar todos os resultados
Table_shdi <- bind_rows(resultados_shdi)

# Exportar tabela final
write.xlsx(Table_shdi, file = paste0(path_base, "Table_shdi.xlsx"), sheetName = "shdi")


################################################################################
# - -----------------------------------------------------------------------

#  - lsm_c_ai Aggregation index (Aggregation metric)
# -----------------------------------------
calcular_metricas_ai <- function(raster, pontos, nome) {
  
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_ai",           # <- Métrica desejada
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_aggregation_index.xlsx"), sheetName = nome)
  
  return(resultado)}  
  
# Aplicar a função para múltiplos rasters, pontos e nomes
resultados_ai <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_ai)

#  Combinar todos os resultados em uma única tabela

Table_aggregation_index <- bind_rows(resultados_ai)

# Exportar tabela consolidada
write.xlsx(Table_aggregation_index, file = paste0(path_base, "Table_aggregation_index.xlsx"), sheetName = "aggregation_index")

#  Resumo por plot_id e class (removendo classe 0)
# -----------------------------------------
Table_aggregation_index_resumida_class <- Table_aggregation_index %>%
  filter(class != 0) %>%  # Remove fundo/nodata
  group_by(plot_id, class) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    # media_value = mean(value, na.rm = TRUE),  # Opcional
    .groups = 'drop')

# Visualizar ou salvar a tabela resumida
View(Table_aggregation_index_resumida_class)

# Exportar resumo se quiser
write.xlsx(Table_aggregation_index_resumida_class, file = paste0(path_base, "Resumo_aggregation_index_por_classe.xlsx"), sheetName = "resumo_ai")


###############################################################################
# - -----------------------------------------------------------------------

# Função para calcular lsm_c_area_mn
# -----------------------------------------
calcular_metricas_area_mn <- function(raster, pontos, nome) {
  
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_area_mn",       # <- Métrica: área média
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")
  
  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_area_media_fragmentos.xlsx"), sheetName = nome)
  
  return(resultado)}  

#  Aplicar a função para listas
resultados_area_mn <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_area_mn)

# Combinar todos os resultados
Table_area_mn <- bind_rows(resultados_area_mn)

# Exportar tabela geral
write.xlsx(Table_area_mn, file = paste0(path_base, "Table_area_media_fragmentos.xlsx"), sheetName = "area_media")

#  Resumir por plot_id e class (remover classe 0)
# -----------------------------------------
Table_area_mn_resumida_class <- Table_area_mn %>%
  filter(class != 0) %>%
  group_by(plot_id, class) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    # media_value = mean(value, na.rm = TRUE),  # Se quiser média também
    .groups = 'drop'
  )

# Visualizar ou exportar resumo
View(Table_area_mn_resumida_class)

write.xlsx(Table_area_mn_resumida_class, file = paste0(path_base, "Resumo_area_media_por_classe.xlsx"), sheetName = "resumo_area_mn")

################################################################################
# - -----------------------------------------------------------------------
# lsm_c_ed The edge density 
# -----------------------------------------
calcular_metricas_ed <- function(raster, pontos, nome) {
  
  resultado <- buffers %>% 
    set_names() %>%
    map_dfr(~sample_lsm(raster,
                        y = pontos,
                        what = "lsm_c_ed",        # <- Métrica: Edge Density
                        size = .,
                        shape = "circle",
                        directions = 8,
                        plot_id = pontos$id_num),
            .id = "buffer")

  # Reshape
  resultado <- dcast(resultado, plot_id ~ buffer + class)
  resultado[is.na(resultado)] <- 0
  resultado <- resultado %>% arrange(plot_id)
  
  # Exportar Excel
  write.xlsx(resultado, file = paste0(path_base, nome, "_edge_density.xlsx"), sheetName = nome)
  
  return(resultado)}    

# Aplicar a função para cada conjunto de dados

resultados_ed <- pmap(list(mapbiomas_list, pontos_list, nomes), calcular_metricas_ed)


#  Combinar todos os resultados

Table_edge_density <- bind_rows(resultados_ed)

# Exportar tabela consolidada
write.xlsx(Table_edge_density, file = paste0(path_base, "Table_edge_density.xlsx"), sheetName = "edge_density")

# 4. Resumir por plot_id e class (excluindo classe 0)
Table_edge_density_resumida_class <- Table_edge_density %>%
  filter(class != 0) %>%
  group_by(plot_id, class) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    # media_value = mean(value, na.rm = TRUE),  # Opcional
    .groups = 'drop')

# Visualizar ou exportar resumo
View(Table_edge_density_resumida_class)

write.xlsx(Table_edge_density_resumida_class, file = paste0(path_base, "Resumo_edge_density_por_classe.xlsx"), sheetName = "resumo_ed")
