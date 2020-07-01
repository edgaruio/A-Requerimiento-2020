rm(list = ls())
library(geosphere)

personas <- data.frame(longitude = c(80.15998, 72.89125, 77.65032, 77.60599, 
                                  72.88120, 76.65460, 72.88232, 77.49186, 
                                  72.82228, 72.88871, 72.82228, 72.88871,
                                  72.82228, 72.88871, 72.82228, 72.88871), 
                    latitude = c(12.90524, 19.08120, 12.97238, 12.90927, 
                                 19.08225, 12.81447, 19.08241, 13.00984,
                                 18.99347, 19.07990, 18.99347, 19.07990,
                                 18.99347, 19.07990, 18.99347, 19.07990))
puntos <- data.frame(longitude = c(72.89537, 77.65094, 73.95325, 72.96746, 
                                  77.65058, 77.66715, 77.64214, 77.58415,
                                  77.76180, 76.65460), 
                    latitude = c(19.07726, 13.03902, 18.50330, 19.16764, 
                                 12.90871, 13.01693, 13.00954, 12.92079,
                                 13.02212, 12.81447), 
                    locality = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"))

# create distance matrix
mat <- data.frame(round(distm(personas[,c('longitude','latitude')], puntos[,c('longitude','latitude')], 
                              fun=distVincentyEllipsoid)/1000,2))
dim(mat)
mat

# assign the name to the point in list1 based on shortest distance in the matrix
personas$dis <- apply(mat[1:dim(mat)[2]],1,min)
personas$point <- puntos$locality[max.col(-mat)]
dim(personas)
personas

# Con datos app
a <- Sys.time()
mat_v <- data.frame(distm(persona[,c('cx_persona','cy_persona')], geo_consulta[,c('CX','CY')], fun=distHaversine))
View(mat_v)
b <- Sys.time()
b-a

# Version 2
aux1 <- data.frame(matrix(1, nrow = dim(personas)[1], ncol = 1))
Dist_v <- c()
for (i in 1:dim(puntos)[1]) {
  Dist_v <- round(distHaversine(personas[,c('longitude','latitude')], puntos[i,c('longitude','latitude')])/1000,1)
  aux1 <- cbind(aux1,Dist_v)
  names(aux1)[i+1] <- paste("Punto",i,sep = "_")
}
View(aux1)

list1$Dist_V <- round(distHaversine(aux1[,c("cx_persona","cy_persona")], list2[,c("CX","CY")])/1000,1)

