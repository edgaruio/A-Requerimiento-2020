# Conectamos a access - Pronosticos para: 
# Cuota monetaria girada, kit escolar, Bono lonchera & otros subsidios
rm(list = ls())
options(scipen = 999)
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(forecast); library(ggplot2); library(readxl)

# Consolidada
consolidada <- readRDS("Analisis_Subsidios/ConsolidacionSEP2019.rds") %>% 
  select(id_persona,id_empresa,cx_persona,cy_persona,cx_empresa,cy_empresa) %>% 
  filter(!is.na(cx_persona)) %>% 
  distinct()
table(duplicated(consolidada$id_persona))
str(consolidada)

# Calculamos el supermercado mas cercano
# Matiz viven
library(geosphere)
bd_matriz_v <- data.frame(distm(df_bono_geo[,c('cx_persona','cy_persona')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v)

# Matriz trabajan
bd_matriz_t <- data.frame(distm(df_bono_geo[,c('cx_empresa','cy_empresa')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t)

# Calculo distancias
mat_v <- bd_matriz_v
df_bono_geo$dis_v <- round(apply(mat_v[1:dim(mat_v)[2]],1,min),2)
df_bono_geo$point_v <- geo_supermercados$NOMBRE[max.col(-mat_v)]

mat_t <- bd_matriz_t
df_bono_geo$dis_t <- round(apply(mat_t[1:dim(mat_t)[2]],1,min),2)
df_bono_geo$point_t <- geo_supermercados$NOMBRE[max.col(-mat_t)]

# Para kit escolar
bd_matriz_v2 <- data.frame(distm(df_kit_geo[,c('cx_persona','cy_persona')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v2)

# Matriz trabajan
bd_matriz_t2 <- data.frame(distm(df_kit_geo[,c('cx_empresa','cy_empresa')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t2)

# Calculo distancias
mat_v2 <- bd_matriz_v2
df_kit_geo$dis_v <- round(apply(mat_v2[1:dim(mat_v2)[2]],1,min),2)
df_kit_geo$point_v <- geo_supermercados$NOMBRE[max.col(-mat_v2)]

mat_t2 <- bd_matriz_t2
df_kit_geo$dis_t <- round(apply(mat_t2[1:dim(mat_t2)[2]],1,min),2)
df_kit_geo$point_t <- geo_supermercados$NOMBRE[max.col(-mat_t2)]

# Escribir base de datos
fwrite(df_bono_geo, "df_bono_geo.csv", row.names = F)
fwrite(df_kit_geo, "df_kit_geo.csv", row.names = F)

# Graficos
library(dplyr); library(data.table); library(esquisse); library(ggplot2)
df_bono_geo <- fread("df_bono_geo.csv") %>% 
  data.frame() %>% 
  na.omit() %>% 
  left_join(consolidada, by = "id_persona")
str(df_bono_geo)

df_kit_geo <- fread("df_kit_geo.csv") %>% 
  data.frame() %>% 
  na.omit() %>% 
  left_join(consolidada, by = "id_persona") %>% 
  left_join(df_kit_ini, by = "id_persona") %>% 
  mutate(estado_bono_escolar = ifelse(estado_bono_escolar == "ENTREGADO", "ENTREGADO", "DISPONIBLE"))
str(df_kit_geo)
# esquisse::esquisser()

df_bono_geo <- df_bono_geo %>%
 filter(dist_vt >= 0L & dist_vt <= 10L) %>%
 filter(!(Categoria %in% 
    "C"))

library(ggplot2)

ggplot(df_bono_geo) +
 aes(x = REDIMIO2, y = dist_vt, fill = REDIMIO2) +
 geom_boxplot() +
 scale_fill_brewer(palette = "Dark2") +
 labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Bono Lonchera por Actividad", fill = "Redime") +
 theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     

# # Ajustes distancias
# df_bono_geo <- fread("df_bono_geo.csv") %>% 
#   data.frame() %>% 
#   mutate(dist_vt = ifelse(dis_v < dis_t, dis_v, dis_t),
#          point_vt = ifelse(dis_v < dis_t, point_v, point_t))
# str(df_bono_geo)
# df_kit_geo <- fread("df_kit_geo.csv") %>% 
#   data.frame() %>% 
#   mutate(dist_vt = ifelse(dis_v < dis_t, dis_v, dis_t),
#          point_vt = ifelse(dis_v < dis_t, point_v, point_t))
# str(df_kit_geo)


df_kit_geo <- df_kit_geo %>%
  filter(dist_vt >= 0L & dist_vt <= 10L) %>%
  filter(!(Categoria %in% 
             "C")) %>% 
  na.omit()

library(ggplot2)

ggplot(df_kit_geo) +
  aes(x = estado_kit_escolar, y = dist_vt, fill = estado_kit_escolar) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Kit Escolar por Actividad", fill = "Redime") +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     

ggplot(df_kit_geo) +
  aes(x = estado_bono_escolar, y = dist_vt, fill = estado_bono_escolar) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Bono Kit Escolar por Actividad", fill = "Redime") +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     



# fwrite(df_bono_geo, "df_bono_geo2.csv", row.names = F)
# fwrite(df_kit_geo, "df_kit_geo2.csv", row.names = F)


