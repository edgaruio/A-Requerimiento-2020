# Mapas estimador directo de Fay - Herriot 

rm(list = ls())
library(sae); library(survey); library(readxl);library(dplyr); library(maptools)
library(rgdal) #readOGR
library(RColorBrewer)
library(classInt)
library(spdep)
# Crear Matriz de fronteras

poligonos_mpios <- readOGR( "MGN2016_00_NACIONAL/MGN_ADM_MPIO_POLITICO.shp" , layer = "MGN_ADM_MPIO_POLITICO")

names(poligonos_mpios)

# Filtrar Cundinamarca
poligonos_mpios_boy = poligonos_mpios[poligonos_mpios$DPTO_CCDGO == "25", ]

# Visualizacion estimaciones

Resultados_ini <- readRDS("Resultados.rds") %>% 
  mutate(COD_DEP_Y_CIUD = substr(COD_DEP_Y_CIUD_Num, 3, 5)) 
str(Resultados_ini)

base_cundi <- data.frame(poligonos_mpios_boy@data$MPIO_CCDGO,poligonos_mpios_boy@data$MPIO_CNMBR) %>% 
  mutate(poligonos_mpios_boy.data.MPIO_CCDGO = as.character(poligonos_mpios_boy.data.MPIO_CCDGO))
str(base_cundi)

Resultados <- Resultados_ini %>% 
  left_join(base_cundi , by = c("COD_DEP_Y_CIUD"="poligonos_mpios_boy.data.MPIO_CCDGO"))
str(Resultados)

#### Mapa para Empresas ####

Resultados_emp <- abs(as.data.frame(Resultados[,c("Total_empresas")]))
names(Resultados_emp) <- "Total_empresas"
row.names(Resultados_emp) <- Resultados$COD_DEP_Y_CIUD
Resultados_emp$MPIO_CCDGO <- Resultados$COD_DEP_Y_CIUD
Resultados_emp <- arrange(Resultados_emp,  MPIO_CCDGO)
poligonos_mpios_boy <- poligonos_mpios_boy[order(poligonos_mpios_boy$MPIO_CCDGO),]
row.names(Resultados_emp) <- Resultados_emp$MPIO_CCDGO
Resultados_emp$MPIO_CCDGO <- NULL
row.names(Resultados_emp) <- row.names(poligonos_mpios_boy)
poligonos.data <- SpatialPolygonsDataFrame(poligonos_mpios_boy, Resultados_emp)
plotvar <- poligonos.data$Total_empresas
nclr <- 6 # Numero de colores
plotclr <- brewer.pal(nclr,"Blues")
class <- classIntervals(round(plotvar,1),nclr,style = "quantile", dataPrecision=0) # Aqui fijo el numero de decimales
colcode <- findColours(class,plotclr) # defino paleta de colores
centroides <- coordinates(poligonos_mpios_boy)
Nombres <- poligonos_mpios_boy$MPIO_CNMBR

plot(poligonos.data, col=colcode, border="grey", axes=T)
title(main = "Promedio Cantidad de Leche (FH)",cex=3)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=1.25)
pointLabel(centroides[,1],centroides[,2],Nombres, offset = 0, cex = .4)

