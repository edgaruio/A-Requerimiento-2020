#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Proyecto:     Análisis de Score Credict
# Descripcion:  Parte 2 - Análisis de correspondencia Múltiple
# Datos:        Cupo_com.rds & Consumo_com.rds
# Por:          Felipe Ruiz
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# rm(list = ls())

# Paquetes
require(FactoMineR)
require(ggplot2)

#### CUPO ####

# Leer datos
dd = readRDS("./Datos/Cupo_com.rds")

# select categorized continuous variables
#ddcat = subset(dd, select=c(ByM, DescBloqueo1, Sexo, estadocivil, DesProfesion,
#                            niveleducativo, tipovivienda, Desctipocontrato, 
#                            SegmentoPoblacion, Edad_R))
#str(ddcat)

ddcat = subset(dd, select=c(ByM, SegmentoPoblacion, Sexo, estadocivil, niveleducativo, tipovivienda))
str(ddcat)

# MCA
mca = MCA(ddcat, graph=FALSE)

# barplot de eigenvalues
eigs = mca$eig$eigenvalue
barplot(eigs, border=NA, names.arg=1:length(eigs), las=2, 
        cex.names=0.7, main="MCA eigenvalues", cex.main=0.9)

# Dimensiones significativas
nd = sum(eigs > 1/length(eigs))

# Gráfico de individuos
mca.ind = data.frame(ByM=dd$ByM, mca$ind$coord)

# Gráfico con 2 dimensiones
ggplot(data=mca.ind, aes(x=Dim.1, y=Dim.2, group=ByM)) + 
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(alpha=0.3, aes(colour=ByM)) +
  scale_x_continuous(limits = quantile(mca.ind$Dim.1, c(0.01,.99))) +
  scale_y_continuous(limits = quantile(mca.ind$Dim.2, c(0.01,.99)))

#### CONSUMO ####

rm(list = ls())

# Paquetes
require(FactoMineR)
require(ggplot2)

# Leer datos
dd = readRDS("./Datos/Consumo_com.rds")

# select categorized continuous variables
#ddcat = subset(dd, select=c(ByM, DescBloqueo1, Sexo, estadocivil, DesProfesion,
#                            niveleducativo, tipovivienda, Desctipocontrato, 
#                            SegmentoPoblacion, Edad_R))
#str(ddcat)

ddcat = subset(dd, select=c(ByM, SegmentoPoblacion, Edad_R))
str(ddcat)
sum(is.na(ddcat$Edad_R))

# MCA
mca = MCA(ddcat, graph=FALSE)

# barplot de eigenvalues
eigs = mca$eig$eigenvalue
barplot(eigs, border=NA, names.arg=1:length(eigs), las=2, 
        cex.names=0.7, main="MCA eigenvalues", cex.main=0.9)

# Dimensiones significativas
nd = sum(eigs > 1/length(eigs))

# Gráfico de individuos
mca.ind = data.frame(ByM=dd$ByM, mca$ind$coord)

# Gráfico con 2 dimensiones
ggplot(data=mca.ind, aes(x=Dim.1, y=Dim.2, group=ByM)) + 
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(alpha=0.3, aes(colour=ByM))
