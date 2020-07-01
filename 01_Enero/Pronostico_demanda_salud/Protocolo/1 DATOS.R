# Exploracion de datos
rm(list = ls())
library(data.table); library(esquisse); library(dplyr); library(readxl); library(lubridate); library(tidyr)

### 2017 ====
lista2017 <- list.files(path = "PROYECTO/2017/", pattern = ".XLSX", recursive = T) %>% sort()
lista2017

ruta <- "PROYECTO/2017/"
aux_2017 <- read_excel(path = paste0(ruta,lista2017[1]))
for (i in 2:length(lista2017)) {
  actual <- read_excel(path = paste0(ruta,lista2017[i]))
  aux_2017 <- bind_rows(aux_2017,actual)
}
str(aux_2017)
rm(actual)

texto_material_2017 <- aux_2017 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

df_2017 <- aux_2017 %>% 
  select(material=Material,fecha=Fe.contabilización,cantidad=Cantidad) %>% 
  filter(!is.na(fecha)) %>% 
  mutate(anio_mes = factor(paste(year(fecha),month(fecha),sep = "_"), levels = c("2017_1","2017_2","2017_3","2017_4","2017_5","2017_6",
                                                                                 "2017_7","2017_8","2017_9","2017_10","2017_11","2017_12"))) %>% 
  group_by(material,anio_mes) %>% 
  summarise(cantidad = abs(sum(cantidad))) %>% 
  spread(anio_mes,cantidad,fill = 0) %>%
  data.frame()
str(df_2017)
names(df_2017)
rm(aux_2017)

### 2018 ====
lista2018 <- list.files(path = "PROYECTO/2018/", pattern = ".XLSX", recursive = T) %>% sort()
lista2018

ruta <- "PROYECTO/2018/"
aux_2018 <- read_excel(path = paste0(ruta,lista2018[1]))
for (i in 2:length(lista2018)) {
  actual <- read_excel(path = paste0(ruta,lista2018[i]))
  aux_2018 <- bind_rows(aux_2018,actual)
}
str(aux_2018)
rm(actual)

texto_material_2018 <- aux_2018 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

df_2018 <- aux_2018 %>% 
  select(material=Material,fecha=Fe.contabilización,cantidad=Cantidad) %>% 
  filter(!is.na(fecha)) %>%
  mutate(anio_mes = factor(paste(year(fecha),month(fecha),sep = "_"), levels = c("2018_1","2018_2","2018_3","2018_4","2018_5","2018_6",
                                                                                 "2018_7","2018_8","2018_9","2018_10","2018_11","2018_12"))) %>% 
  group_by(material,anio_mes) %>% 
  summarise(cantidad = abs(sum(cantidad))) %>% 
  spread(anio_mes,cantidad,fill = 0) %>%
  data.frame()
str(df_2018)
names(df_2018)
rm(aux_2018)

### 2019 ====
lista2019 <- list.files(path = "PROYECTO/2019/", pattern = ".XLSX", recursive = T) %>% sort()
lista2019

ruta <- "PROYECTO/2019/"
aux_2019 <- read_excel(path = paste0(ruta,lista2019[1]))
for (i in 2:length(lista2019)) {
  actual <- read_excel(path = paste0(ruta,lista2019[i]))
  aux_2019 <- bind_rows(aux_2019,actual)
}
str(aux_2019)
rm(actual)

texto_material_2019 <- aux_2019 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

df_2019 <- aux_2019 %>% 
  select(material=Material,fecha=Fe.contabilización,cantidad=Cantidad) %>% 
  filter(!is.na(fecha)) %>%
  mutate(anio_mes = factor(paste(year(fecha),month(fecha),sep = "_"), levels = c("2019_1","2019_2","2019_3","2019_4","2019_5","2019_6",
                                                                                 "2019_7","2019_8","2019_9","2019_10","2019_11","2019_12"))) %>% 
  group_by(material,anio_mes) %>% 
  summarise(cantidad = abs(sum(cantidad))) %>% 
  spread(anio_mes,cantidad,fill = 0) %>%
  data.frame()
str(df_2019)
names(df_2019)
rm(aux_2019)

### 2020 ====
lista2020 <- list.files(path = "PROYECTO/2020/", pattern = ".XLSX", recursive = T) %>% sort()
lista2020

ruta <- "PROYECTO/2020/"
aux_2020 <- read_excel(path = paste0(ruta,lista2020[1]))
for (i in 2:length(lista2020)) {
  actual <- read_excel(path = paste0(ruta,lista2020[i]))
  aux_2020 <- bind_rows(aux_2020,actual)
}
str(aux_2020)
rm(actual)

texto_material_2020 <- aux_2020 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

df_2020 <- aux_2020 %>% 
  select(material=Material,fecha=Fe.contabilización,cantidad=Cantidad) %>% 
  filter(!is.na(fecha)) %>%
  mutate(anio_mes = factor(paste(year(fecha),month(fecha),sep = "_"), levels = c("2020_1","2020_2","2020_3","2020_4","2020_5","2020_6",
                                                                                 "2020_7","2020_8","2020_9","2020_10","2020_11","2020_12"))) %>% 
  group_by(material,anio_mes) %>% 
  summarise(cantidad = abs(sum(cantidad))) %>% 
  spread(anio_mes,cantidad,fill = 0) %>%
  data.frame()
str(df_2020)
names(df_2020)
rm(aux_2020)


### Union ====

df_union <- df_2017 %>% 
  full_join(df_2018, by = "material") %>% 
  full_join(df_2019, by = "material") %>% 
  full_join(df_2020, by = "material") %>% 
  select(paste0("X",c("2017_1","2017_2","2017_3","2017_4","2017_5","2017_6",
         "2017_7","2017_8","2017_9","2017_10","2017_11","2017_12",
         "2018_1","2018_2","2018_3","2018_4","2018_5","2018_6",
         "2018_7","2018_8","2018_9","2018_10","2018_11","2018_12",
         "2019_1","2019_2","2019_3","2019_4","2019_5","2019_6",
         "2019_7","2019_8","2019_9","2019_10","2019_11","2019_12",
         "2020_1","2020_2","2020_3"
         # ,"2020_4","2020_5","2020_6",
         # "2020_7","2020_8","2020_9","2020_10","2020_11","2020_12"
         )))
str(df_union)
df_union[,c(2:ncol(df_union))][is.na(df_union[,c(2:ncol(df_union))])] <- 0
str(df_union)

saveRDS(df_union, "Resultados/df_union.rds")

df_texto <- bind_rows(texto_material_2017,texto_material_2018,texto_material_2019)
saveRDS(df_texto, "Resultados/df_material.rds")

# ### Analisis descriptivo ====
# rm(list = ls())
# df_union <- readRDS("Resultados/df_union.rds") %>% 
#   mutate(conteo_cero = rowSums(.[,c(2:36)]==0, na.rm = TRUE))
# str(df_union)
# 
# df_exploratorio <- df_union %>% 
#   mutate(promedio = round(rowMeans(.[,c(2:36)], na.rm = TRUE),1),
#          desv_est = round(apply(.[,c(2:36)], 1, sd, na.rm=TRUE),1),
#          cv = round(100*desv_est/promedio,1),
#          total = rowSums(.[,c(2:36)], na.rm = TRUE),
#          conteo_cero_2017 = rowSums(.[,c(2:13)]==0, na.rm = TRUE),
#          conteo_cero_2018 = rowSums(.[,c(14:25)]==0, na.rm = TRUE),
#          conteo_cero_2019 = rowSums(.[,c(26:36)]==0, na.rm = TRUE),
#          conteo_cero = rowSums(.[,c(2:36)]==0, na.rm = TRUE)) %>% 
#   mutate(estado = case_when(
#     conteo_cero_2017 < 5 & conteo_cero_2018 < 5 & conteo_cero_2019 < 5 ~ "Se mantiene",
#     promedio >= 0 & promedio <= 10 ~ "Baja rotacion",
#     promedio == 0 ~ "Descontinuado",
#     conteo_cero_2017 < 5 & conteo_cero_2018 > 5 & conteo_cero_2019 > 5 ~ "Descontinuado desde 2018",
#     conteo_cero_2017 < 5 & conteo_cero_2018 < 5 & conteo_cero_2019 > 5 ~ "Descontinuado desde 2019",
#     conteo_cero_2017 > 5 & conteo_cero_2018 < 5 & conteo_cero_2019 < 5 ~ "Nuevo desde 2018",
#     conteo_cero_2017 > 5 & conteo_cero_2018 >= 5 & conteo_cero_2019 < 5 ~ "Nuevo desde 2019"
#   )) %>% 
#   mutate(estado = ifelse(is.na(estado), "Otro", estado))
# str(df_exploratorio)
# sum(is.na(df_exploratorio$estado))
# library(esquisse)
# esquisser()
# 
# 
# library(ggplot2)
# ggplot(df_exploratorio) +
#  aes(x = estado, y = cv) +
#  geom_boxplot(fill = "#26828e") +
#  labs(x = "Tipo Estado", y = "CV en Porcentaje", title = "Distribución Coeficientes de Variación", subtitle = "Tipo Estado") +
#  theme_classic()
# 
# 
# # Clasiicacion en grupos mediante h_clust
# library(factoextra); library(FactoMineR)
# df_exploratorio_scale <- scale(df_exploratorio %>% select(38:43) %>% select(-cv)) %>% data.frame()
# 
# # Analisis de componentes principales
# acp_demanda <- PCA(df_exploratorio_scale, graph = FALSE)
# 
# # Cluster Jerarquico
# cluster <- HCPC(acp_demanda, nb.clust = 3, graph = FALSE)
# aa <- cluster$data.clust
# union <- cbind(df_exploratorio, aa[,6]) %>% 
#   rename("grupo"="aa[, 6]") %>% 
#   mutate(estado_grupo = paste(estado, grupo, sep = "_"))
# str(union)
# table(union$estado_grupo)
# 
# # Descriptivos
# with(union,table(estado,grupo))
# 
# # Kmeans
# fviz_nbclust(df_exploratorio_scale, kmeans,method = "gap_stat", k.max = 20)
# 
# library(factoextra) 
# # Se emplean los datos iris excluyendo la variable Species 
# km_clusters <- eclust(x = df_exploratorio_scale, FUNcluster = "kmeans", k = 10, 
#                       seed = 123, hc_metric = "euclidean", nstart = 50, graph = FALSE) 
# fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco", ggtheme = theme_classic())
# 
# 
# # Cluster test 2
# union$grupo2 <- km_clusters$silinfo$widths$cluster
# with(union,table(estado,grupo2))
# 
# ### Test Pronostico ====
# library(forecast)
# get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1)){
#   best.aic <- 1e8
#   n <- length(x.ts)
#   for (p in 0:maxord[1])
#     for (d in 0:maxord[2])
#       for (q in 0:maxord[3])
#         for (P in 0:maxord[4])
#           for (D in 0:maxord[5]) 
#             for (Q in 0:maxord[6]) 
#               {
#               fit <- arima(x.ts, order = c(p,d,q), seas = list(order = c(P,D,Q), frequency(x.ts)), method = "CSS")
#               fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
#               if (fit.aic < best.aic) 
#               {
#                 best.aic <- fit.aic
#                 best.fit <- fit
#                 best.model <- c(p,d,q,P,D,Q)
#               }
#             }
#   list(best.aic, best.fit, best.model)
# }
# 
# 
# media_movil <- function(ts_serie){
#   m = length(ts_serie)
#   n = 4 # N del promedio movil
#   estimacion <- c()
#   for (i in 5:m) {
#     estimacion <- mean(ts_serie[seq(i-n,i-1,by=1)])
#   }
# }
# 
# 
# funcion_pronostico3 <- function(data_input,inicio,final){
#   data_input <- data.frame(data_input)
#   n_material = nrow(data_input)
#   pronosticos <- c()
#   
#   for (i in 1:n_material) {
#     
#     if (as.numeric(data_input[i,"conteo_cero"]) >= 5) {
#       pronosticos[i] = rowMeans(data_input[i,c(30:36)], na.rm = T)
#     }
#     else {
#       vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
#       best.fit.elec <- auto.arima(vector_ts)
#       pronosticos[i] <- round(as.numeric(data.frame(forecast(best.fit.elec, 1))[1,1]))
#     }
#   }
#   data_output <- cbind(data_input,pronosticos)
# }
# 
# funcion_pronostico4 <- function(data_input,inicio,final){
#   n_material = nrow(data_input)
#   pronosticos <- c()
#   for (i in 1:n_material) {
#     
#     if (as.numeric(data_input[i,"conteo_cero"]) >= 3) {
#       pronosticos[i] = as.numeric(rowMeans(data_input[i,c(32:36)], na.rm = T))
#     } else {
#       vector_ts <- ts(as.numeric(data_input[i,c(2:36)]),start = inicio, end = final, frequency = 12)
#       best.fit.elec <- HoltWinters(vector_ts)
#       pronosticos[i] <- round(as.numeric(predict(best.fit.elec, 1)))
#     }
#   }
#   data_output <- cbind(data_input,pronosticos)
# }
# 
# 
# ### SALIDAS ====
# salida3 <- funcion_pronostico3(df_union,inicio = c(2017,1), final = c(2019,11))
# saveRDS(salida3, "Resultados/Salida_auto_arima.rds")
# # saveRDS(salida3_2, "Resultados/Salida_auto_arima2.rds")
# # salida4 <- funcion_pronostico4(df_union[1:2000,],inicio = c(2017,1), final = c(2019,11))
# 
# 
# # Prueba individual
# vector_ts <- ts(as.numeric(df_union[91,-1]),start = c(2017,1), end = c(2019,11), frequency = 12)
# best.fit.elec <- HoltWinters(vector_ts)
# pronosticos <- as.numeric(predict(best.fit.elec, 1))
# class(pronosticos)
# 
# 
# df_union[3,"conteo_cero"]
# as.numeric(df_union[1,c(2:36)])
# 
