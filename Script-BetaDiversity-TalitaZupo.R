# Set working directory
setwd("C:/Users/luipe/OneDrive/Documentos/FAPESP_PD/Banco de Dados") 
getwd()
install.packages("betapart") # install required packages 

library(betapart)
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)


# Reading the Excel sheets as a list of data frames
df_list <- lapply(excel_sheets("ZFinal-Dez23-2024.xlsx"), function(x) 
  read_excel("ZFinal-Dez23-2024.xlsx", sheet = x))

# Function to transform values to 0-1 and selecting data
transform_to_0_1 <- function(df) {
  df <- df %>%
    mutate_if(is.numeric, ~as.numeric(. != 0)) %>% 
    select(-c(1, 2, 4)) %>% select(where(~ any(. != 0))) %>% 
    mutate_all(~ifelse(is.na(.), 0, .))
  
  return(df)
}

list_dataframes_transformed <- map(df_list, transform_to_0_1)

print(list_dataframes_transformed)


# Function to transform data frames to matrices
transform_to_matrix <- function(df) {
  mat <- as.matrix(df[, -1])  
  mat <- apply(mat, 2, as.integer)  
  unique_row_names <- make.unique(as.character(df[, 1]))
  unique_row_names <- unique_row_names[seq_len(nrow(mat))]
  rownames(mat) <- unique_row_names
  
  return(mat)
}

# Using 'map' function to transform all data frames at once (loop) 
list_matrices <- map(list_dataframes_transformed, transform_to_matrix)

# The input of betapart is also a presence-absence matrix: 0 or 1
# organizing the results:

# Initialize empty lists to store the results
beta_SIM <- list()
beta_SNE <- list()
beta_SOR <- list()
study_names <- c()

# Iterate over each matrix in the list
for (i in seq_along(list_matrices)) {
  # Calculate beta diversity components for the current matrix
  result <- beta.multi(list_matrices[[i]])
  
  # Store the result in the corresponding list
  beta_SIM[[i]] <- result$beta.SIM
  beta_SNE[[i]] <- result$beta.SNE
  beta_SOR[[i]] <- result$beta.SOR
  
  # Store the study name
  study_names[i] <- paste0("study", i)
}

# Combine the lists into a data frame
result_df <- data.frame(
  Study = study_names,
  beta_SIM = do.call(rbind, beta_SIM),
  beta_SNE = do.call(rbind, beta_SNE),
  beta_SOR = do.call(rbind, beta_SOR)
  )

# Print the resulting data frame
print(result_df)

write.table(result_df, "beta_diversityALL_final.csv", row.names=F, sep=",", dec=".")

# beta_SOR: total dissimilarity, beta_SIM: turnover, beta_SNE: nestedness.
# SOR = Sorensen!


