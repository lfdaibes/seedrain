# Analises colaboração Luis organização das Tabelas# 
# Fulbertgnonlonfoun@gmail.com #
# Leec/unesp/RC #
#2025_08_04
# 1 Organização- -----------------------------------------------------
#Pacotes utilizados
pacotes <- c("landscapemetrics", "rgdal", "dplyr", "purrr", 
             "raster","psych", "vegan","tidyverse", "forcats", 
             "iNEXT","reshape2", "rgeos","wesanderson", "readxl", 
             "pak", "sf", "progress", "pbapply","readxl", "readr", 
             "tmap", "ggplot2","remotes", "mgcv", "iNEXT", "terra", "sf")

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

# 1. Limpar ambiente
rm(list = ls())

# 3. Definir diretório onde estão seus arquivos Excel
setwd("C:/Fulbert_Dados/01_Doutorado/08_Colaborações/02_Luis/01_Luís_Felipes_Daibes_Banco_sementes/Tabelas_resultados_brutos")

# 1. Importar as tabelas
tabela_fragmentacao     <- read_excel("Tabela_Final_Metricas_Fragmentacao.xlsx")
tabela_matriz           <- read_excel("Tabela_Final_Metricas_Matriz.xlsx")
tabela_cohesion         <- read_excel("Table_cohesion.xlsx")
tabela_edge_density     <- read_excel("Table_edge_density.xlsx")
tabela_num_patches      <- read_excel("Table_num_patches.xlsx")
tabela_shdi             <- read_excel("Table_shdi.xlsx")




# Função para expandir uma tabela com repetição
expandir_com_reps <- function(caminho_arquivo, nome_metrica) {
  df <- read_excel(caminho_arquivo) %>%
    select(plot_id, buffer, value) %>%
    group_by(plot_id, buffer) %>%
    mutate(rep = row_number()) %>%
    ungroup() %>%
    mutate(coluna = paste0(nome_metrica, "_buffer_", buffer, "_rep", rep)) %>%
    select(plot_id, coluna, value) %>%
    pivot_wider(names_from = coluna, values_from = value)
  
  return(df)
}


# Definir os caminhos dos arquivos
caminhos <- list(
  cohesion = "Table_cohesion.xlsx",
  edge = "Table_edge_density.xlsx",
  shdi = "Table_shdi.xlsx",
  num_patches = "Table_num_patches.xlsx",
  matriz = "Tabela_Final_Metricas_Matriz.xlsx",
  fragmentacao ="Tabela_Final_Metricas_Fragmentacao.xlsx"
  
)

# Definir caminho onde salvar os arquivos exportados
diretorio_saida <- "C:/Fulbert_Dados/01_Doutorado/08_Colaborações/02_Luis/01_Luís_Felipes_Daibes_Banco_sementes/Tabelas_resultados_brutos" # <<< AJUSTE PARA SEU CASO

# Processar e salvar cada métrica
for (nome in names(caminhos)) {
  caminho_arquivo <- caminhos[[nome]]
  tabela_expandida <- expandir_com_class(caminho_arquivo, nome)
  
  # Nome do arquivo de saída
  nome_saida <- paste0("Tabela_", toupper(nome), "_Expandida_por_CLASS.xlsx")
  caminho_saida <- file.path(diretorio_saida, nome_saida)
  
  # Salvar como Excel
  write_xlsx(tabela_expandida, path = caminho_saida)
  
  cat("✅ Arquivo salvo:", caminho_saida, "\n")
}

