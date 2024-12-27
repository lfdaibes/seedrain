setwd("C:/Users/luipe/OneDrive/Documentos/FAPESP_PD/Banco de Dados") # set working directory
getwd()

install.packages("purrr") # install required packages

library(dplyr)
library(ggplot2)
library(iNEXT)
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

print(list_matrices)


# Apply as.incfreq to the list of matrices
Mx = lapply(list_matrices, as.incfreq)

tryx<-iNEXT(Mx, datatype="incidence_freq")

asyest<-tryx$AsyEst
str(asyest)
datainfo<-tryx$DataInfo

write.table(datainfo, "datainfo_final.csv", row.names=F, sep=",", dec=".")
write.table(asyest, "asyest_final.csv", row.names=F, sep=",", dec=".")



# Compare diversity with same coverage!
diversity<-estimateD(Mx, datatype="incidence_freq",
          base="coverage", level= 0.975)

###### outputs ####
####
study_column <- c()
for (i in 1:52) {
  # Repeat "study i" three times and append to the vector
  study_column <- c(study_column, paste("study", i, sep = " "))
  study_column <- c(study_column, paste("study", i, sep = " "))
  study_column <- c(study_column, paste("study", i, sep = " "))
}

print(study_column)

diversity$study <- study_column
#reorder columns
diversity <- diversity %>%
  select(study, everything())

write.table(diversity, "diversityALL_final.csv", row.names=F, sep=",", dec=".")

shannon<- ChaoShannon(Mx,datatype="incidence_freq")
richness<-ChaoRichness(Mx,datatype="incidence_freq")
