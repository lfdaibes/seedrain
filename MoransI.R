# Spatial autocorrelation 
library(openxlsx)
disp_table_read <- read.xlsx("DispTable.xlsx")
comm_table_read <- read.xlsx("LandscapeFinal-Nov6-2024-Fulbert.xlsx")

# Instalar e carregar pacotes necessários
#install.packages(c("spdep", "sf", "ggplot2", "patchwork", "sp"))

library(spdep)
library(sf)
library(ggplot2)
library(patchwork)
library(sp)

citation("spdep")
citation("sf")
citation("sp")

# Criar o objeto espacial usando as coordenadas de latitude e longitude
coords <- cbind(disp_table_read$location_long, disp_table_read$location_lat)
spatial_data <- SpatialPointsDataFrame(coords = coords, data = disp_table_read, 
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
spatial_data <- st_as_sf(spatial_data)  # Sistema de referência #datum=WGS84 #ou crs=31983
spatial_data <- st_transform(spatial_data, crs = 31983)

coords2 <- cbind(comm_table_read$location_long, comm_table_read$location_lat)
spatial_data2 <- SpatialPointsDataFrame(coords = coords2, data = comm_table_read, 
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
spatial_data2 <- st_as_sf(spatial_data2)  # Sistema de referência #datum=WGS84 #ou crs=31983
spatial_data2 <- st_transform(spatial_data2, crs = 31983)

# Definir vizinhos usando K vizinhos mais próximos (4 vizinhos como exemplo)
nb <- knearneigh(coords, k = 4)
listw <- nb2listw(knn2nb(nb))

nb2 <- knearneigh(coords2, k = 4)
listw2 <- nb2listw(knn2nb(nb2))

# Função para calcular o Moran's I e organizar o resultado
calc_moran <- function(var) {
   test <- moran.test(var, listw)
  return(data.frame(moran_I = test$estimate[1], p_value = test$p.value))}

calc_moran2 <- function(var) {
   test2 <- moran.test(var, listw2)
  return(data.frame(moran_I = test2$estimate[1], p_value = test2$p.value))}

# 1. Moran's I para AneAuto (prop_spp e seed_dens)
disp_aneauto <- spatial_data[spatial_data$disp_mode == "AneAuto", ]
coords_aneauto <- st_coordinates(disp_aneauto)  # Extrair coordenadas
nb_aneauto <- knearneigh(coords_aneauto, k = 4)
listw_aneauto <- nb2listw(knn2nb(nb_aneauto))
moran_prop_spp_AneAuto <- moran.test(disp_aneauto$prop_spp, listw_aneauto)
moran_seed_dens_AneAuto <- moran.test(disp_aneauto$seed_dens_log, listw_aneauto)

# 2. Moran's I para Zoo (prop_spp e seed_dens)
disp_zoo <- spatial_data[spatial_data$disp_mode == "Zoo", ]
coords_zoo <- st_coordinates(disp_zoo)  # Extrair coordenadas
nb_zoo <- knearneigh(coords_zoo, k = 4)
listw_zoo <- nb2listw(knn2nb(nb_zoo))
moran_prop_spp_Zoo <- moran.test(disp_zoo$prop_spp, listw_zoo)
moran_seed_dens_Zoo <- moran.test(disp_zoo$seed_dens_log, listw_zoo)

# 3. Moran's I para richness (comunidade toda)
disp_2 <- spatial_data2
coords_2 <- st_coordinates(disp_2)  # Extrair coordenadas
nb_2 <- knearneigh(coords_2, k = 4)
listw_2 <- nb2listw(knn2nb(nb_2))
moran_richness <- calc_moran2(comm_table_read$richness)

# 4. Moran's I para beta_SIM (comunidade toda)
moran_beta_SIM <- calc_moran2(comm_table_read$beta_SIM)


# Função para calcular o Moran's I para várias classes de distância
calc_moran_by_distance <- function(variable, coords, n_bins = 10) {
  # Definir matriz de distâncias #st_distance(spatial_data)/1000 
  dists <- as.matrix(dist(coords)) #st_distance(spatial_data) #as.matrix(dist(coords))
  
  # Definir classes de distância
  max_dist <- max(dists)
  bins <- seq(0, max_dist, length.out = n_bins + 1)
  
  # Armazenar resultados
  moran_values <- numeric(n_bins)
  p_values <- numeric(n_bins)
  
  for (i in 1:n_bins) {
    # Criar lista de vizinhança para cada bin de distância
    neighbors <- dnearneigh(coords, d1 = bins[i], d2 = bins[i + 1])
    
    # Criar lista de pesos
    listw <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    
    # Calcular Moran's I
    moran_test <- moran.test(variable, listw, zero.policy = TRUE)
    
    # Armazenar o valor de Moran's I e valor de p
    moran_values[i] <- moran_test$estimate[1]
    p_values[i] <- moran_test$p.value}
  
  # Retornar Moran's I, p-valor e as classes de distância
  data.frame(distance_class = bins[-1], moran_I = moran_values, p_value = p_values)}






# Função para calcular o Moran's I para várias classes de distância # Para a comunidade!!!
calc_moran_by_distance2 <- function(variable, coords2, n_bins = 10) {
  # Definir matriz de distâncias #st_distance(spatial_data)/1000 
  dists <- as.matrix(dist(coords2)) #st_distance(spatial_data) #as.matrix(dist(coords))
  
  # Definir classes de distância
  max_dist <- max(dists)
  bins <- seq(0, max_dist, length.out = n_bins + 1)
  
  # Armazenar resultados
  moran_values <- numeric(n_bins)
  p_values <- numeric(n_bins)
  
  for (i in 1:n_bins) {
    # Criar lista de vizinhança para cada bin de distância
    neighbors <- dnearneigh(coords2, d1 = bins[i], d2 = bins[i + 1])
    
    # Criar lista de pesos
    listw2 <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    
    # Calcular Moran's I
    moran_test2 <- moran.test(variable, listw2, zero.policy = TRUE)
    
    # Armazenar o valor de Moran's I e valor de p
    moran_values[i] <- moran_test2$estimate[1]
    p_values[i] <- moran_test2$p.value}
  
  # Retornar Moran's I, p-valor e as classes de distância
  data.frame(distance_class = bins[-1], moran_I = moran_values, p_value = p_values)}


# Aplicar a função para cada variável e modo de dispersão usando classes de distância automáticas
coords_aneauto <- st_coordinates(disp_aneauto) #st_as_sf(disp_aneauto) #st_coordinates(disp_aneauto)
coords_zoo <- st_coordinates(disp_zoo) #st_as_sf(disp_zoo) #st_coordinates(disp_zoo)
coords_2 <- st_coordinates(disp_2)

# prop_spp para AneAuto e Zoo
moran_prop_spp_AneAuto <- calc_moran_by_distance(disp_aneauto$prop_spp, coords_aneauto)
moran_prop_spp_Zoo <- calc_moran_by_distance(disp_zoo$prop_spp, coords_zoo)

# seed_dens_log para AneAuto e Zoo
moran_seed_dens_AneAuto <- calc_moran_by_distance(disp_aneauto$seed_dens_log, coords_aneauto)
moran_seed_dens_Zoo <- calc_moran_by_distance(disp_zoo$seed_dens_log, coords_zoo)
library(dplyr)
moran_seed_dens_combined <- bind_rows(AneAuto = moran_seed_dens_AneAuto, 
                                      Zoo = moran_seed_dens_Zoo, .id = "disp_mode")

# richness e beta_SIM para a comunidade toda
moran_richness <- calc_moran_by_distance2(spatial_data2$richness, coords_2)
moran_beta_SIM <- calc_moran_by_distance2(spatial_data2$beta_SIM, coords_2)

# Função para adicionar p-valor no gráfico
add_pvalue_text <- function(df, plot) {
  plot + 
    geom_text(aes(x = distance_class, y = moran_I, label = ifelse(p_value < 0.05, 
                                                                  paste0("p=", round(p_value, 3)), "")),
              vjust = -1, color = "red", size = 3)}

# Gráficos para prop_spp
format_k <- function(x) {ifelse(x >= 1000, paste0(x / 1000, "k"), x)} #showing 1000 meters as 1k

#p1 <- ggplot(moran_prop_spp_AneAuto, aes(x = distance_class, y = moran_I, group = 1)) +
 # geom_line() + geom_point() + ggtitle("Prop spp. (Abiotic disp)") +
  #xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  #theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  #scale_x_continuous(labels = format_k)
#p1 <- add_pvalue_text(moran_prop_spp_AneAuto, p1)

p2 <- ggplot(moran_prop_spp_Zoo, aes(x = distance_class, y = moran_I, group = 1)) +
  geom_line(color = "blue4") + geom_point(color = "blue4") + ggtitle("Prop. of Zoo spp.") +
  xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_x_continuous(labels = format_k)
#p2 <- add_pvalue_text(moran_prop_spp_Zoo, p2)

# Gráficos para seed_dens_log
#p3 <- ggplot(moran_seed_dens_AneAuto, aes(x = distance_class, y = moran_I, group = 1)) +
 # geom_line() + geom_point() + ggtitle("Seed density log (Abiotic disp)") +
  #xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  #theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  #scale_x_continuous(labels = format_k)
#p3 <- add_pvalue_text(moran_seed_dens_AneAuto, p3)

p4 <- ggplot(moran_seed_dens_combined, aes(x = distance_class, y = moran_I, col = disp_mode)) +
  geom_line() + geom_point() + ggtitle("Seed density log") +
  xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = c(0.8, 0.8))+
  scale_color_manual(name = "disp mode", # Título da legenda
    values = c("AneAuto" = "goldenrod3", "Zoo" = "purple4"), # Define as cores para cada categoria
    labels = c("AneAuto" = "Non-Zoo", "Zoo" = "Zoo"))+ # Renomeia os itens da legenda
    scale_x_continuous(labels = format_k)
#p4 <- add_pvalue_text(moran_seed_dens_Zoo, p4)

# Gráficos para richness e beta_SIM
p5 <- ggplot(moran_richness, aes(x = distance_class, y = moran_I, group = 1)) +
  geom_line(color = "blue4") + geom_point(color = "blue4") + ggtitle("Species richness") +
  xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_x_continuous(labels = format_k)
#p5 <- add_pvalue_text(moran_richness, p5)

p6 <- ggplot(moran_beta_SIM, aes(x = distance_class, y = moran_I, group = 1)) +
  geom_line(color = "blue4") + geom_point(color = "blue4") + ggtitle("Turnover") +
  xlab("distance class (m)") + ylab("Moran's I") + theme_minimal() + ylim(-1,1)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  scale_x_continuous(labels = format_k)
#p6 <- add_pvalue_text(moran_beta_SIM, p6)

library(gridExtra)
jpeg("MoranWithP-Distance-TryNEW-Dez2024.jpeg", width = 17, height = 16,
     units = "cm", pointsize = 9,
     bg = "white",  res = 400, restoreConsole = TRUE) # save in a file 
grid.arrange(p2, p4, p5, p6,
             nrow = 2, ncol = 2)
dev.off() 





