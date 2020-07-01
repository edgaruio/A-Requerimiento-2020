# Analisis cluster
rm(list = ls())
library(dplyr); library(readxl); library(RODBC)

consolidada <- readRDS('//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionABR2020.rds') %>% 
  filter(marca_afiliado_unico) %>% 
  filter(DepartamentoPersona %in% c("DISTRITO CAPITAL")) %>% 
  select(id_persona,id_empresa,cx_persona,cy_persona,cx_empresa,cy_empresa) %>% 
  filter(!is.na(cx_persona | cx_empresa))
names(consolidada)
table(consolidada$DepartamentoPersona)

geo_afiliados <- consolidada %>% 
  select(cx_persona,cy_persona) %>% 
  na.omit()
str(geo_afiliados)

geo_afiliados_id <- consolidada %>% 
  select(id_persona,cx_persona,cy_persona) %>% 
  na.omit()
str(geo_afiliados_id)

# Infraestrutura
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Contacto/Infraestructura/Infraestructura.accdb"
)
sqlTables(channel)

geo_droguerias <- sqlQuery(
  channel ,
  paste ("select * from COLSUBISIDIO_INFRAESTRUCTURA_PROPIA"),
  as.is=T) %>%
  data.frame() %>%
  filter(DEPARTAMENTO == "DISTRITO CAPITAL") %>%
  filter(TIPO == "DROGUERIA") %>%
  select(COD,NOMBRE,CX,CY) %>%
  mutate(n_drogueria = as.character(1:144)) %>%
  data.frame()
str(geo_droguerias)
odbcCloseAll()

col_droguerias <- geo_droguerias %>% 
  select(CX,CY)

# Explorarion de cluster
distance<-matrix(-1, nrow = length(geo_afiliados$cx_persona), ncol= length(col_droguerias$CX))

library(geosphere)
dm<-apply(data.frame(1:length(col_droguerias$CX)), 1, function(x){ replace(distance[,x], 1:length(geo_afiliados$cx_persona), distGeo(col_droguerias[x,], geo_afiliados))})
dim(dm)

closestcenter<-apply(dm, 1, which.min)
summary(closestcenter)
length(closestcenter)

geo_cluster <- geo_afiliados_id %>% 
  mutate(drog_cercana = as.character(closestcenter)) %>% 
  left_join(geo_droguerias, by = c("drog_cercana"="n_drogueria")) %>% 
  mutate(drog_cercana = as.numeric(drog_cercana)) %>%
  data.frame()
str(geo_cluster)
saveRDS(geo_cluster, "Resultados/geo_cluster.rds")
rm(consolidada, distance, dm, geo_afiliados)

# Cargamos rds
rm(list = ls())
geo_cluster <- readRDS("Resultados/geo_cluster.rds")
barplot(table(geo_cluster$drog_cercana))
length(unique(geo_cluster$drog_cercana))

top10 <- geo_cluster %>% 
  group_by(NOMBRE) %>% 
  summarise(Cobertura = n()) %>% 
  arrange(desc(Cobertura)) %>% 
  top_n(10)

library(esquisse)
esquisser()

library(ggplot2); library(scales)
ggplot(top10) +
  aes(x = reorder(NOMBRE, Cobertura), weight = Cobertura) +
  geom_bar(fill = "#4292c6") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0, hjust = 1) + 
  labs(x = "Drogueria", y = "Afiliados", title = "Top 10 Droguerías", subtitle = "Cobertura de Afiliados") +
  coord_flip() +
  theme_light()

colors<-rainbow(140)
plot(geo_cluster[,c("cx_persona","cy_persona")] , col=colors[geo_cluster$drog_cercana], pch=19,  cex=0.01)


# Numero de cluster
library(factoextra); library(NbClust); library(dplyr)
length(names(table(geo_cluster$drog_cercana)))


# Agrupamiento con puntos fijos
library(cluster)
library(factoextra)
geo_cluster_scale <- geo_cluster %>% select(cx_persona,cy_persona)

# fviz_nbclust(geo_cluster_scale %>% sample_n(100000), clara, method = "silhouette")+
#   theme_classic()

clara_clusters <- clara(x = geo_cluster_scale, k = 150, metric = "manhattan", stand = FALSE,
                        samples = 50, pamLike = TRUE)
clara_clusters

# Visualizacion de Cluster
fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point", show.clust.cent = T,
             pointsize = 0.1) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

# Centroides
clara_clusters$clustering
centroides_clara <- data.frame(clara_clusters$medoids) %>%
  rename(cx_centroide=cx_persona,cy_centroide=cy_persona)
str(centroides_clara)

distance2<-matrix(-1, nrow = length(col_droguerias$CX), ncol= length(centroides_clara$cx_centroide))
dim(distance2)

library(geosphere)
dm2<-apply(data.frame(1:length(centroides_clara$cx_centroide)),
           1, 
           function(x){ replace(distance2[,x], 1:length(col_droguerias$CX), distGeo(centroides_clara[x,], col_droguerias))})
dim(dm2)

closestcenter2<-apply(dm2, 1, which.min)
length(closestcenter2)

str(geo_droguerias)
geo_droguerias_cluster <- geo_droguerias %>%
  mutate(cluster_drogueria = as.character(closestcenter2)) %>%
  select(-c(CX,CY))
str(geo_droguerias_cluster)

centroides_clara2 <- data.frame(clara_clusters$medoids) %>%
  mutate(id_centroide = as.character(1:150)) %>%
  rename(cx_centroide=cx_persona,cy_centroide=cy_persona)
str(centroides_clara2)

geo_cluster_clara <- geo_cluster %>%
  mutate(cluster = as.character(clara_clusters$clustering)) %>%
  left_join(centroides_clara2, by = c("cluster" = "id_centroide")) %>%
  left_join(geo_droguerias_cluster, by = c("NOMBRE" = "NOMBRE")) %>%
  data.frame()
str(geo_cluster_clara)

geo_cluster_clara %>% filter(cluster == cluster_drogueria) %>% dim()
sum(is.na(geo_cluster_clara$cluster_drogueria))

# Filtrado de poblacion con no optimización de drogrueria
geo_cluster_noopt <- geo_cluster_clara %>% filter(cluster != cluster_drogueria)
length(names(table(geo_cluster_noopt$cluster)))

colors <- rainbow(142)
plot(geo_cluster_noopt[,c("cx_persona","cy_persona")] , col=colors[geo_cluster_noopt$drog_cercana], pch=19,  cex=0.01)

library(leaflet)
pal0 <- colorFactor("Dark2", domain = geo_cluster$drog_cercana)
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 6, maxZoom = 20))
map %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE) %>% 
  addCircleMarkers(data = geo_cluster, lng = ~cx_persona, lat = ~cy_persona, fillOpacity = 0.3, 
                   color = ~pal0(geo_cluster$drog_cercana), stroke = FALSE, 
                   radius = 0.05)

pal <- colorFactor("Dark2", domain = geo_cluster_noopt$drog_cercana)
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 6, maxZoom = 20))
map %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE) %>% 
  addCircleMarkers(data = geo_cluster_noopt, lng = ~cx_persona, lat = ~cy_persona, fillOpacity = 0.3, 
                   color = ~pal(geo_cluster_noopt$drog_cercana), stroke = FALSE, 
                   radius = 0.05)


geo_cluster_opt <- geo_cluster_clara %>% filter(cluster == cluster_drogueria)
length(names(table(geo_cluster_opt$cluster)))

colors <- rainbow(142)
plot(geo_cluster_opt[,c("cx_persona","cy_persona")] , col=colors[geo_cluster_opt$drog_cercana], pch=19,  cex=0.01)

pal2 <- colorFactor("Dark2", domain = geo_cluster_opt$drog_cercana)
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 6, maxZoom = 20))
map %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE) %>% 
  addCircleMarkers(data = geo_cluster_opt, lng = ~cx_persona, lat = ~cy_persona, fillOpacity = 0.3, 
                   color = ~pal2(geo_cluster_opt$drog_cercana), stroke = FALSE, 
                   radius = 0.05)


geo_cluster_clara %>% 
  select(NOMBRE:cluster_drogueria) %>% 
  distinct() %>% 
  dim()

# save.image("D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/5 MAYO/6 NP_Droguerias/Resultados/Salida_20052020.RData")
load("Resultados/Salida_20052020.RData")

### Parte 3 Distancia de Centroides a droguerias ----

library(geosphere); library(dplyr)
distance3<-matrix(-1, nrow = length(centroides_clara$cx_centroide), ncol= length(col_droguerias$CX))
dim(distance3)

# dm3<-apply(data.frame(1:length(centroides_clara$cx_centroide)),
#            1, 
#            function(x){ replace(distance2[,x], 1:length, distGeo(centroides_clara[x,], col_droguerias))})
# dim(dm3)

dm3<-apply(data.frame(1:length(col_droguerias$CX)),
           1, 
           function(x){ replace(distance3[,x], 1:length(centroides_clara$cx_centroide), distGeo(col_droguerias[x,], centroides_clara))})
dim(dm3)

closestcenter3<-apply(dm3, 1, which.min)
length(closestcenter3)

tabla_centroides <- data.frame(medoides = clara_clusters$medoids) %>% 
  mutate(drogueria_cercana = as.character(closestcenter3)) %>% 
  left_join(geo_droguerias, by = c("drogueria_cercana"="n_drogueria"))
tabla_centroides$distancia = round(distHaversine(tabla_centroides[,c("medoides.cx_persona","medoides.cy_persona")], tabla_centroides[,c("CX","CY")])/1000,1)
str(tabla_centroides)
tabla_centroides_optimizados <- tabla_centroides %>% 
  arrange(desc(distancia)) %>% 
  top_n(10)
class(tabla_centroides)

library(writexl)
write_xlsx(tabla_centroides_optimizados, "Resultados/tabla_centroides_optimizados.xlsx")


# save.image("D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/5 MAYO/6 NP_Droguerias/Resultados/Salida_21052020.RData")
load("Resultados/Salida_21052020.RData")

### Competencia ----
library(data.table); library(dplyr); library(ggplot2); library(scales)
options(scipen = 999)

competencia <- fread("Datos/Competencia_Droguerias_1012018.csv") %>% 
  filter(DEPARTAMENTO == "DISTRITO CAPITAL") %>% 
  group_by(COMPETENCIA) %>% 
  summarise(Conteo = n()) %>% 
  arrange(desc(Conteo))
str(competencia)
table(competencia$CIUDAD)

str(geo_cluster)
pobla_drog <- geo_cluster %>% 
  group_by(NOMBRE) %>% 
  summarise(Cobertura = n()) %>% 
  ungroup() %>% 
  arrange(desc(Cobertura)) %>% 
  top_n(10)
str(pobla_drog)
mean(pobla_drog$Cobertura)

ggplot(pobla_drog) +
  aes(x = reorder(NOMBRE, Cobertura), weight = Cobertura) +
  geom_bar(fill = "#2171b5") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0, hjust= 1) +
  scale_y_continuous(labels = comma) +
  labs(x = "Droguería", y = "Cobertura", title = "Distribución Afiliados por Droguería", subtitle = "Cobertura") +
  coord_flip() +
  theme_classic()

# Ventas
ventas_drog <- fread("Datos/Ventas.Droguerias.csv", dec = ",") %>% 
  filter(Ciudad == "BOGOTA") %>% 
  mutate(Nombre = toupper(Nombre)) %>% 
  group_by(Centro,Nombre) %>% 
  summarise(Ventas = mean(Ventas)/1000000,
            No.Personas = mean(No.Personas),
            Transacciones = mean(Transacciones)) %>% 
  data.frame() %>% 
  arrange(desc(Ventas)) 
str(ventas_drog)
mean(ventas_drog$Transacciones)

library(ggplot2); library(scales)
ggplot(ventas_drog %>% 
         slice(1:10)) +
  aes(x = reorder(Nombre, Ventas), weight = Ventas) +
  geom_bar(fill = "#4292c6") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0.5, hjust = 1) + 
  labs(x = "Drogueria", y = "Ventas", title = "Top 10 Droguerías", subtitle = "Ventas Promedio Mensuales (Millones)") +
  coord_flip() +
  theme_classic()

ggplot(ventas_drog %>% 
         slice(1:10)) +
  aes(x = reorder(Nombre, Transacciones), weight = Transacciones) +
  geom_bar(fill = "#4292c6") +
  geom_text(stat='count', aes(label=comma(..count..)), vjust=0.5, hjust = 1) + 
  labs(x = "Drogueria", y = "Transacciones", title = "Top 10 Droguerías", subtitle = "Transacciones Promedio Mensuales") +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = comma)


# Correlacion vs Ventas
str(ventas_drog)
cor_ventas_pobla <- ventas_drog %>% 
  mutate(Nombre = gsub("DROG.","DROGUERIA",Nombre, fixed = T)) %>% 
  mutate(Nombre = chartr("ÁÉÍÓÚ","AEIOU",Nombre)) %>% 
  left_join(pobla_drog, by = c("Centro"="COD")) %>% 
  mutate(Cobertura = ifelse(is.na(Cobertura), 0, Cobertura))
str(cor_ventas_pobla)

table(duplicated(cor_ventas_pobla$Nombre))

library(esquisse); library(ggplot2); library(scales)
esquisser()

ggplot(cor_ventas_pobla) +
  aes(x = Cobertura, y = Ventas) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(x = "Cobertura (Personas)", y = "Ventas mensuales promedio", 
       title = "Diagrama de Dispersión", subtitle = "Ventas vs Cobertura") +
  theme_classic()+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

ggplot(cor_ventas_pobla) +
  aes(x = Cobertura, y = Transacciones) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(x = "Cobertura (Personas)", y = "Transacciones mensuales promedio", 
       title = "Diagrama de Dispersión", subtitle = "Transacciones vs Cobertura") +
  theme_classic()+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Consulta Habeas data

# autorizacion
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Autorizacion.accdb"
)
tb_autorizaciones <- sqlQuery( 
  channel , 
  paste ("select * from tb_autorizaciones")
) %>% 
  data.frame() %>% 
  mutate(
    id_persona = as.character(id_persona)
  ) %>% 
  mutate(
    Contacto_autorizacion = toupper(as.character(autorizacion)) 
  ) %>% 
  select(
    id_persona,Contacto_autorizacion
  )
odbcCloseAll()

# Celular
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Celular.accdb"
  # "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=d:/02 - Bases/0 - Bases Beimar/Contacto/Fuentes/Celular.accdb"
)
tb_celular <- sqlQuery( 
  channel , 
  paste ("select * from tb_celular")
) %>% 
  data.frame()  %>% 
  mutate(
    id_persona = as.character(id_persona),
    Contacto_Celular = (telefono_celular)
  ) %>% 
  select(
    id_persona,Contacto_Celular
  )
odbcCloseAll()

# Mail
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Contacto/Fuentes/Mail.accdb"
  # "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=d:/02 - Bases/0 - Bases Beimar/Contacto/Fuentes/Mail.accdb"
)
tb_mail <- sqlQuery( 
  channel , 
  paste ("select * from tb_mail")
) %>% 
  data.frame() %>% 
  mutate(
    id_persona = as.character(id_persona),
    Contacto_Mail = toupper(correo_electronico)
  ) %>% 
  select(
    id_persona,Contacto_Mail
  )
odbcCloseAll()

# tabla de contacto con HABEAS, celular y Mail
DATA_Habeas <- tb_autorizaciones %>% 
  left_join(tb_celular) %>% 
  left_join(tb_mail) %>% 
  filter(!duplicated(id_persona)) %>% 
  filter(Contacto_autorizacion == "SI")
str(DATA_Habeas)

poblacion_comunicacion <- geo_cluster %>% 
  filter(NOMBRE %in% pobla_drog$NOMBRE) %>% 
  left_join(DATA_Habeas, by = "id_persona") %>% 
  filter(Contacto_autorizacion == "SI") %>% 
  select(id_persona,NOMBRE,Contacto_autorizacion,Contacto_Celular,Contacto_Mail)
str(poblacion_comunicacion)
table(poblacion_comunicacion$NOMBRE)

library(writexl)
write_xlsx(poblacion_comunicacion, "Resultados/Poblacion_comunicacion.xlsx")
