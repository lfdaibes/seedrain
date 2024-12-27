# Set working directory
setwd("C:/Users/luipe/OneDrive/Documentos/FAPESP_PD/Banco de Dados") 
getwd()
install.packages("geodata")
install.packages("terra")

library(geodata)
library(terra)
library(sp)
library(readxl)
library(openxlsx)
library(dplyr)

landscape <- read_excel("LandscapeFinal-Nov6-2024-Fulbert.xlsx")
class(landscape)

tavg <- worldclim_tile("tavg",
                       landscape[1,6],
                       landscape[1,5],
                       path = "WorldClimData")

tmin <- worldclim_tile("tmin",
                       landscape[1,6],
                       landscape[1,5],
                       path = "WorldClimData")

tmax <- worldclim_tile("tmax",
                       landscape[1,6],
                       landscape[1,5],
                       path = "WorldClimData")

prec <- worldclim_tile("prec",
                       landscape[1,6],
                       landscape[1,5],
                       path = "WorldClimData")

#bio <- worldclim_tile("bio",
 #                      landscape[1,4],
  #                     landscape[1,3],
   #                    path = "WorldClimData", version = "2.1")

# Convert data frame to vector object
points <- vect(landscape,
               geom=c("location_long", "location_lat"),
               crs = "EPSG:4326")

# Extract values
#values <- extract(bio,points)
values.avg <- extract(tavg,points)
values.avg$tavg.mean <- rowMeans(values.avg[,2:13])

values.min <- extract(tmin,points)
values.min$tmin.mean <- rowMeans(values.min[,2:13]) # for means across months
values.min$tmin <- apply(values.min[,2:13], 1, FUN = min) # for the min of coldest month
values.min %>% select(8,15)

values.max <- extract(tmax,points)
values.max$tmax.mean <- rowMeans(values.max[,2:13])
values.max$tmax <- apply(values.max[,2:13], 1, FUN = max)

values.prec <- extract(prec,points)
values.prec$prec.total <- rowSums(values.prec[,2:13])
values.prec$prec.mean <- rowMeans(values.prec[,2:13])

clim.new<-data.frame(landscape$data_owner,values.avg$tavg.mean,values.max$tmax,
                 values.min$tmin,values.prec$prec.total,values.prec$prec.mean) %>% 
            rename("data_owner"="landscape.data_owner","tavg"="values.avg.tavg.mean",
                   "tmax"="values.max.tmax","tmin"="values.min.tmin",
                   "prec.total"="values.prec.prec.total",
                   "prec.mean"="values.prec.prec.mean")
landscape.clim <- left_join(landscape,clim.new)
landscape.clim <- as.data.frame(landscape.clim)
class(landscape3)
write.xlsx(landscape.clim,"LandscapeWithClimateVar.xlsx",
           rowNames=T,quote=F)
write.xlsx(clim.new,"ClimateVariables.xlsx",
           rowNames=T,quote=F)

#biofinal <- values %>% select(2,6,7,13,14,15)
#biofinal[,2:3] <- biofinal[,2:3]*0.1

#biofinal$data_owner <- landscape$data_owner
#biofinal <- left_join(landscape,biofinal)
