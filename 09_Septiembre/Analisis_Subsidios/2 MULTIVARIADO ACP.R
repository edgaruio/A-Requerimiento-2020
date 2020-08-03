#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Proyecto:     Analisis de Subsidios
# Descripcion:  Parte 2 - ACP
# Datos:        bd_pca.rds
# Por:          Felipe Ruiz
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
library(readxl); library(ggplot2); library(car); library(dplyr); library(reshape2); library(caret)
library(faraway)

#### ACP ####

# Cargamos datos

bd_pca <- readRDS("Analisis_Subsidios/bd_pca.rds") %>% 
  mutate(Cupo_credito = ifelse(is.na(Cupo_credito), 0,
                               ifelse(Cupo_credito == "ACTIVO", 1, 0))) %>% 
  mutate(Genero = ifelse(F == 1, "F", "M"),
         Categoria = ifelse(A == 1, "A", "B"))
names(bd_pca)
str(bd_pca)
table(bd_pca$Cupo_credito)

bd_pca_con = subset(bd_pca, select=c(Salario,Edad,numero_beneficiario_cuota_monetaria:filial_proteccion,Bono_derecho:uso_mes,A:Medio)) %>% 
  na.omit()
str(bd_pca_con)
bd_pca_cat = subset(bd_pca, select=c(id_persona,Genero,Salario,Edad,numero_beneficiario_cuota_monetaria:filial_proteccion,Bono_derecho:uso_mes,A:Medio)) %>% 
  na.omit()

# SVariables con varianza cero
var_zero <- nearZeroVar(bd_pca_con)
bd_pca_con <- bd_pca_con[,-var_zero]

# PCA
pca = prcomp(bd_pca_con, scale=TRUE)

# screeplot
screeplot(pca, type="lines")

# Porcentaje de inercia
inertias = 100 * cumsum(pca$sdev^2) / ncol(bd_pca_con)
barplot(inertias, col="gray85", border=NA, names.arg=1:ncol(bd_pca_con),
        main="Percentage of inertia for each dimension")

# Numero de dimensiones
nd = 7

# eigenvalues, eigenvectors (loadings), and proyecciones (ie scores)
egis = pca$sdev[1:nd]^2
U = pca$rotation[,1:nd]
Psi = as.data.frame(pca$x[,1:nd])
Phi = as.data.frame(cor(bd_pca_con, Psi)) 

# Gráfico de variables
# Creamos un circulo circle
circle <- function(center=c(0,0), npoints=100)
{
  r = 1
  zz = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(zz)
  yy = center[1] + r * sin(zz)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0,0), npoints = 100)

# data frame with coordenas en filas
arrows = data.frame(
  x1=rep(0,ncol(bd_pca_con)), y1=rep(0,ncol(bd_pca_con)),
  x2=Phi$PC1, y2=Phi$PC2)

# Numero de dimensiones
Phi[,1:2]

# Estatus de crédito con PC1 y PC2
Psi$Genero = bd_pca_cat$Genero

# Centros de gravedad (cog)
stat1 = tapply(Psi[,1], bd_pca_cat$Genero, mean)
stat2 = tapply(Psi[,2], bd_pca_cat$Genero, mean) 
cog = data.frame(stat1, stat2, Genero=c("F","M"))

# Componente 1 vs 2
ggplot() +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  # geom_point(data=Psi, aes(x=PC1, y=PC2, colour=Genero), 
  #            alpha=0.1, size=0.5) +
  stat_density2d(data=Psi, aes(x=PC1, y=PC2, colour=Genero), alpha=0.4) + 
  geom_point(data=cog, aes(5*stat1, 5*stat2, colour=Genero), size=5) + 
  scale_color_brewer(palette="Dark2") +
  # geom_segment(data=arrows, aes(x=x1, y=y1, xend=4*x2, yend=4*y2), colour="gray") +
  geom_text(data=Phi, size = 3, colour = "darkgray", aes(x=4*PC1, y=4*PC2, label=rownames(Phi))) +
  scale_x_continuous(limits = quantile(Psi$PC1, c(0.01,.99))) +
  scale_y_continuous(limits = quantile(Psi$PC2, c(0.01,.99)))

# Componente 2 vs 3
ggplot() +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  # geom_point(data=Psi, aes(x=PC1, y=PC2, colour=Genero), 
  #            alpha=0.1, size=0.5) +
  stat_density2d(data=Psi, aes(x=PC2, y=PC3, colour=Genero), alpha=0.4) + 
  geom_point(data=cog, aes(5*stat1, 5*stat2, colour=Genero), size=5) + 
  scale_color_brewer(palette="Dark2") +
  # geom_segment(data=arrows, aes(x=x1, y=y1, xend=3*x2, yend=3*y2), colour="gray") +
  geom_text(data=Phi, size = 3, colour = "darkgray", aes(x=4*PC2, y=4*PC3, label=rownames(Phi))) +
  scale_x_continuous(limits = quantile(Psi$PC2, c(0.01,.99))) +
  scale_y_continuous(limits = quantile(Psi$PC3, c(0.01,.99)))

# Componente 2 vs 3
ggplot() +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  # geom_point(data=Psi, aes(x=PC1, y=PC2, colour=Genero), 
  #            alpha=0.1, size=0.5) +
  stat_density2d(data=Psi, aes(x=PC3, y=PC4, colour=Genero), alpha=0.4) + 
  geom_point(data=cog, aes(5*stat1, 5*stat2, colour=Genero), size=5) + 
  scale_color_brewer(palette="Dark2") +
  # geom_segment(data=arrows, aes(x=x1, y=y1, xend=3*x2, yend=3*y2), colour="gray") +
  geom_text(data=Phi, size = 3, colour = "darkgray", aes(x=4*PC3, y=4*PC4, label=rownames(Phi))) +
  scale_x_continuous(limits = quantile(Psi$PC3, c(0.01,.99))) +
  scale_y_continuous(limits = quantile(Psi$PC4, c(0.01,.99)))


#### ACM ####
library(FactoMineR)
bd_pca_cat2 = subset(bd_pca, select=c(Genero,piramide_1,piramide_2,estado_civil,nivel_academico,segmento_grupo_familiar,ACTIVIDAD,
                                      )) %>% 
  na.omit()
str(bd_pca_cat2)

bd_pca_cat2 <- bd_pca_cat2 %>% 
  mutate(Genero = as.factor(Genero),
         ACTIVIDAD = as.factor(ACTIVIDAD))
str(bd_pca_cat2)

# MCA
mca = MCA(bd_pca_cat2, graph=FALSE)

# barplot de eigenvalues
eigs = mca$eig[,1]
barplot(eigs, border=NA, names.arg=1:length(eigs), las=2, 
        cex.names=0.7, main="MCA eigenvalues", cex.main=0.9)

# Dimensiones significativas
nd = sum(eigs > 1/length(eigs))

# Gráfico de individuos
mca.ind = data.frame(Genero=bd_pca_cat2$Genero, mca$ind$coord)

# Gráfico con 2 dimensiones
ggplot(data=mca.ind, aes(x=Dim.2, y=Dim.3, group=Genero)) + 
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(alpha=0.3, aes(colour=Genero)) +
  scale_x_continuous(limits = quantile(mca.ind$Dim.2, c(0.01,.99))) +
  scale_y_continuous(limits = quantile(mca.ind$Dim.3, c(0.01,.99)))

#### AFM ####
library(FactoMineR); library(factoextra)
str(bd_pca)
nombres <- names(bd_pca)

bd_pca_m <- bd_pca %>% 
  select(id_persona,Genero,Categoria,estado_civil,nivel_academico,segmento_grupo_familiar,Salario,Edad,numero_beneficiario_cuota_monetaria:filial_proteccion,Subsidio_asignado,subsidio_entregado,
         Bono_derecho:Compra_vivienda,uso_mes,cabeza_hogar,trata_cronico,ACTIVIDAD,-pre_aprobado_cupo,-pre_aprobado_hipo) %>% 
  mutate(numero_hijos = ifelse(is.na(numero_hijos),0,numero_hijos),
         Genero = as.factor(Genero),
         Categoria = as.factor(Categoria),
         cabeza_hogar = as.factor(cabeza_hogar),
         ACTIVIDAD = as.factor(ACTIVIDAD)) 
str(bd_pca_m)


bd_pca_m <- as.data.frame(bd_pca_m)
rownames(bd_pca_m) <- bd_pca_m[,1]
bd_pca_m <- bd_pca_m[,-1]
str(bd_pca_m)
n <- round(0.10*nrow(bd_pca_m))
size1 <- 486142/nrow(bd_pca_m)
size2 <- 617799/nrow(bd_pca_m)
size3 <- 96636/nrow(bd_pca_m)
size4 <- 104409/nrow(bd_pca_m)

bd_pcaAF <- bd_pca_m %>% filter(Categoria == "A" & Genero == "F") %>% sample_n(size = round(size1*n,0)) #%>% na.omit()
bd_pcaAM <- bd_pca_m %>% filter(Categoria == "A" & Genero == "M") %>% sample_n(size = round(size2*n,0)) #%>% na.omit()
bd_pcaBF <- bd_pca_m %>% filter(Categoria == "B" & Genero == "F") %>% sample_n(size = round(size3*n,0)) #%>% na.omit()
bd_pcaBM <- bd_pca_m %>% filter(Categoria == "B" & Genero == "M") %>% sample_n(size = round(size4*n,0)) #%>% na.omit()
str(bd_pcaAF)
library(esquisse)
esquisser()

# Descriptivos por Grupo

bd_pcaBM2 <- bd_pcaBM %>%
 filter(!(nivel_academico %in% "SIN INFORMACION")) %>%
 filter(!is.na(cabeza_hogar)) %>%
 filter(!is.na(ACTIVIDAD))

library(ggplot2)

ggplot(bd_pcaBM2) +
 aes(x = cabeza_hogar, fill = nivel_academico) +
 geom_bar(position = "fill") +
 scale_fill_hue() +
 labs(x = "Cabeza de Hogar", y = "Porcentaje", title = "Distribución de Hombres Cat B", subtitle = "Cabeza de Hogar por Actividad y Nivel Académico", fill = "Nivel Académico") +
 coord_flip() +
 theme_minimal() +
 facet_wrap(vars(ACTIVIDAD))



# Para categoria A y Fememino
res_mfa_AF <- MFA(bd_pcaAF,
               group = c(5,2,7,9,8,12,2,1),
               type = c("n","s","s","s","s","s","n","n"),
               name.group = c("Identificacion","Salario_Edad","Nucleo_Familiar","Cobertura_Servcios","Cobertura_Basica",
                              "Consumos","Tipo_Mujer","Actividad"),
               num.group.sup = c(1,6,7),
               graph = F)
get_eigenvalue(res_mfa_AF)
fviz_screeplot(res_mfa_AF)
get_mfa_var(res_mfa_AF, "group")
fviz_mfa_var(res_mfa_AF, "group", title = "Plano Factorial - Proyección Grupos", xlab = "Dim 1 (43.5%)", ylab = "Dim 2 (31.6%)", 
             subtitle = "Categoría A y Genero Femenino")
fviz_mfa_var(res_mfa_AF, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             col.var.sup = "gray", repel = T, geom = c("point","text"))
fviz_mfa_ind(res_mfa_AF, addEllipses = TRUE, repel = TRUE)
fviz_contrib(res_mfa_AF, choice = "quanti.var", axes = 1, top = 20, pallete = "jco", title = "Contribución de Variables Cuantitativas a Dim 1", 
             subtitle = "Categoría A y Genero Femenino", legend.title = "Grupos")+
  labs(y = "Contribuciones (%)")
fviz_mfa_var(res_mfa_AF, "quanti.var", palette = "jco", col.var.sup = "gray", repel = T, geom = c("point","text"), legend = "bottom")


# Para categoria B y Fememino
res_mfa_BF <- MFA(bd_pcaBF,
                  group = c(5,2,7,9,8,12,2,1),
                  type = c("n","s","s","s","s","s","n","n"),
                  name.group = c("Identificacion","Salario_Edad","Nucleo_Familiar","Cobertura_Servicios","Cobertura_Basica","Consumos","Tipo_Mujer","Actividad"),
                  num.group.sup = c(1,6,7),
                  graph = F)
get_eigenvalue(res_mfa_BF)
fviz_screeplot(res_mfa_BF)
get_mfa_var(res_mfa_BF, "group")
fviz_mfa_var(res_mfa_BF, "group", title = "Plano Factorial - Proyección Grupos", xlab = "Dim 1 (40.5%)", ylab = "Dim 2 (31.1%)", 
             subtitle = "Categoría B y Genero Femenino")
fviz_mfa_var(res_mfa_BF, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             col.var.sup = "gray", repel = T, geom = c("point","text"))
fviz_contrib(res_mfa_BF, choice = "quanti.var", axes = 2, top = 20, pallete = "jco", title = "Contribución de Variables Cuantitativas a Dim 1", 
             subtitle = "Categoría B y Genero Femenino", legend.title = "Grupos")+
  labs(y = "Contribuciones (%)")
fviz_mfa_var(res_mfa_BF, "quanti.var", palette = "jco", col.var.sup = "gray", repel = T, geom = c("point","text"), legend = "bottom")

# Para categoria A y Masculino
res_mfa_AM <- MFA(bd_pcaAM,
                  group = c(5,2,7,9,8,12,2,1),
                  type = c("n","s","s","s","s","s","n","n"),
                  name.group = c("Identificacion","Salario_Edad","Nucleo_Familiar","Cobertura_Servicios","Cobertura_Basica","Consumos","Tipo_Mujer","Actividad"),
                  num.group.sup = c(1,6,7),
                  graph = F)
get_eigenvalue(res_mfa_AM)
fviz_screeplot(res_mfa_AM)
get_mfa_var(res_mfa_AM, "group")
fviz_mfa_var(res_mfa_AM, "group", title = "Plano Factorial - Proyección Grupos", xlab = "Dim 1 (40.5%)", ylab = "Dim 2 (29.9%)", 
             subtitle = "Categoría A y Genero Masculino")
fviz_mfa_var(res_mfa_AM, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             col.var.sup = "gray", repel = T, geom = c("point","text"))
fviz_contrib(res_mfa_AM, choice = "quanti.var", axes = 1, top = 20, pallete = "jco", title = "Contribución de Variables Cuantitativas a Dim 1", 
             subtitle = "Categoría A y Genero Masculino", legend.title = "Grupos")+
  labs(y = "Contribuciones (%)")
fviz_mfa_var(res_mfa_AM, "quanti.var", palette = "jco", col.var.sup = "gray", repel = T, geom = c("point","text"), legend = "bottom")

# Para categoria B y Masculino
res_mfa_BM <- MFA(bd_pcaBM,
                  group = c(5,2,7,9,8,12,2,1),
                  type = c("n","s","s","s","s","s","n","n"),
                  name.group = c("Identificacion","Salario_Edad","Nucleo_Familiar","Cobertura_Servicios","Cobertura_Basica","Consumos","Tipo_Mujer","Actividad"),
                  num.group.sup = c(1,6,7),
                  graph = F)
get_eigenvalue(res_mfa_BM)
fviz_screeplot(res_mfa_BM)
get_mfa_var(res_mfa_BM, "group")
fviz_mfa_var(res_mfa_BM, "group", title = "Plano Factorial - Proyección Grupos", xlab = "Dim 1 (41.1%)", ylab = "Dim 2 (31.2%)", 
             subtitle = "Categoría B y Genero Masculino")
fviz_mfa_var(res_mfa_BM, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             col.var.sup = "gray", repel = T, geom = c("point","text"))
fviz_contrib(res_mfa_BM, choice = "quanti.var", axes = 1, top = 20, pallete = "jco", title = "Contribución de Variables Cuantitativas a Dim 1", 
             subtitle = "Categoría B y Genero Masculino", legend.title = "Grupos")+
  labs(y = "Contribuciones (%)")
fviz_mfa_var(res_mfa_BM, "quanti.var", palette = "jco", col.var.sup = "gray", repel = T, geom = c("point","text"), legend = "bottom")


# Analisis cluster



)