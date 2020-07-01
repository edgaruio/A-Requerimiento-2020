# Conectamos a access
rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr)

tb_seg_poblacion <- read_excel("tb_segpob.xlsx")
str(tb_seg_poblacion)

conn_afil2017 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2017.accdb")
subset(sqlTables(conn_afil2017), tableType = "SYSTEM TABLE")
afil2017 <- sqlQuery(conn_afil2017, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2017)
odbcClose(conn_afil2017)

df_afil2017 <- afil2017 %>% 
  filter(id_empresa == "NIT8001972684") %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2017) %>% 
  data.frame()
str(df_afil2017)

conn_afil2018 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2018.accdb")
subset(sqlTables(conn_afil2018), tableType = "SYSTEM TABLE")
afil2018 <- sqlQuery(conn_afil2018, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2018)
odbcClose(conn_afil2018)

df_afil2018 <- afil2018 %>% 
  filter(id_empresa == "NIT8001972684") %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2018) %>% 
  data.frame()
str(df_afil2018)

conn_afil2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2019.accdb")
subset(sqlTables(conn_afil2019), tableType = "SYSTEM TABLE")
afil2019 <- sqlQuery(conn_afil2019, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2019)
odbcClose(conn_afil2019)

df_afil2019 <- afil2019 %>% 
  filter(id_empresa == "NIT8001972684") %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2019) %>% 
  data.frame()
str(df_afil2019)
# rm(afil2017,afil2018,afil2019)

df_union <- bind_rows(df_afil2017,df_afil2018,df_afil2019) 
str(df_union)
library(writexl)
writexl::write_xlsx(df_union, "df_union_anio_segmento.xlsx")


# trabajadores
conn_trabajadores <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Trabajador_del_pago_de_aporte.accdb")
subset(sqlTables(conn_trabajadores), tableType = "SYSTEM TABLE")
trabajadores <- sqlFetch(conn_trabajadores, "Trabajador_del_pago_de_aporte")
str(trabajadores)
odbcClose(conn_trabajadores)

df_trabajadores <- trabajadores %>% 
  filter(id_empresa == "NIT8001972684") %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  select(id_empresa,mes,conteo=trabajadores,anio=año) %>% 
  filter(anio %in% c(2014,2015,2016)) %>% 
  group_by(anio) %>% 
  summarise(conteo = mean(conteo)) %>% 
  ungroup() %>% 
  mutate(id_empresa = "NIT8001972684") %>% 
  select(3,1,2)
str(df_trabajadores)
# Consolidado

df_union <- bind_rows(df_trabajadores,df_afil2017,df_afil2018,df_afil2019) 
str(df_union)
library(writexl)
writexl::write_xlsx(df_union, "df_union_anio.xlsx")

# Consolidada
consolidada <- readRDS("ConsolidacionDIC2019.rds") %>% 
  filter(id_empresa == "NIT8001972684")
str(consolidada)

table(consolidada$MunicipioPersona) %>% data.frame() %>% arrange(desc(Freq))


library(ggplot2)
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
data <- data %>% data.frame()

# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()

data_head <- consolidada %>% 
  filter(numero_hijos>=1) %>% 
  mutate(SMMLV = round(Salario/877803,2),
         SMMLV_RANGO=case_when(SMMLV < 2 ~ "Menos de 2 SMMLV",
                               SMMLV >=2 & SMMLV < 4 ~ "Entre 2 y 4 [2:4)",
                               SMMLV >=4 & SMMLV < 6 ~ "Entre 4 y 6 [4:6)",
                               SMMLV >=6 & SMMLV < 8 ~ "Entre 6 y 8 [6:8)",
                               SMMLV >=8 ~ "Mayor a 8 SMMLV")) %>% 
  mutate(SMMLV_RANGO = factor(SMMLV_RANGO, levels = c("Menos de 2 SMMLV", "Entre 2 y 4 [2:4)", "Entre 4 y 6 [4:6)", "Entre 6 y 8 [6:8)", "Mayor a 8 SMMLV"))) %>% 
  select(SMMLV_RANGO,Categoria,numero_hijos) %>% 
  group_by(SMMLV_RANGO,Categoria) %>% 
  summarise(hijos = mean(numero_hijos, na.rm = T)) %>% 
  data.frame() %>% 
  arrange(desc(hijos))

str(data_head)
ggplot(data_head, aes(Categoria, SMMLV_RANGO, fill= hijos)) + 
  geom_tile()

library(esquisse)
esquisse::esquisser()

library(ggplot2)

ggplot(data_head) +
 aes(x = Categoria, y = SMMLV_RANGO, fill = hijos) +
 geom_tile(size = 1L) +
 scale_fill_viridis_c(option = "viridis") +
 labs(title = "HeadMap Número de Hijos", subtitle = "SMMLV vs.Categoria (Personas con hijos)", fill = "N.Hijos", 
      y = "Rango SMMLV") +
 theme_minimal()

# Cruce
str(consolidada)
test <- consolidada %>% 
  select(Categoria,Segmento_poblacional,id_persona) %>% 
  group_by(Segmento_poblacional,Categoria) %>% 
  summarise(conteo = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(conteo),
         porc = 100*round(conteo/total,4))
View(test)
write_xlsx(test, "test.xlsx")

# Consumo indivivual
consumo_ind <- readRDS("consumo_individual_Noviembre.rds")
str(consumo_ind)
sort(names(table(consumo_ind$ues)))

test_consumo <- consumo_ind %>% 
  filter(anno == 2019 & servicio == "Credito convenios" & id_empresa == "NIT8001972684") %>% 
  select(id_persona, categoria, segmento_poblacional) %>% 
  group_by(categoria, segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  data.frame()
str(test_consumo)
write_xlsx(test_consumo, "test_consumo.xlsxo.xlsx")


# COnsulta aporte y recaudo
library(RODBC)
conn_aporte <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/ENERO/Dian_16012020/Aporte.accdb")
subset(sqlTables(conn_aporte), tableType = "SYSTEM TABLE")
consulta_aporte <- sqlFetch(conn_aporte, "aporte")
str(consulta_aporte)
odbcClose(conn_aporte)

conn_recaudo <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/ENERO/Dian_16012020/Recaudo.accdb")
subset(sqlTables(conn_recaudo), tableType = "SYSTEM TABLE")
consulta_recaudo <- sqlFetch(conn_recaudo, "Recaudo")
str(consulta_recaudo)
odbcClose(conn_recaudo)


df_aporte <- consulta_aporte %>% 
  filter(año == 2018 & mes == 11)
sum(df_aporte$aporte_4)

df_recaudo <- consulta_recaudo %>% 
  filter(año == 2018 & mes == 11)
sum(df_recaudo$valor_planilla)

# TORTAS
# Consolidada
library(plotly)
consolidada <- readRDS("ConsolidacionDIC2019.rds") %>% 
  filter(id_empresa == "NIT8001972684")
str(consolidada)

# Global
data_plot <- consolidada %>%
  filter(numero_hijos >= 1) %>% 
  dplyr::select(id_persona,Categoria) %>%
  group_by(Categoria) %>%
  summarise(Conteo = n_distinct(id_persona))

m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
f1 <- list(family = "Arial, sans-serif",size = 18,color = "black")
f2 <- list(family = "Old Standard TT, serif",size = 14,color = "black")

p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
              textinfo = 'text+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste('Empleados:', Conteo),
              showlegend = T,
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 2))) %>%
  layout(margin = m,
         title = 'Participación por Categoria \n (Afiliados con Hijos)',
         font = list(color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
         legend = list(x = 0.8, y = 0.5),
         paper_bgcolor='transparent',
         plot_bgcolor='transparent') %>%
  config(displayModeBar = F)
p1


# Global
data_plot <- consolidada %>%
  filter(numero_hijos == 0) %>% 
  dplyr::select(id_persona,Categoria) %>%
  group_by(Categoria) %>%
  summarise(Conteo = n_distinct(id_persona))

m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
f1 <- list(family = "Arial, sans-serif",size = 18,color = "black")
f2 <- list(family = "Old Standard TT, serif",size = 14,color = "black")

p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
              textinfo = 'text+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste('Empleados:', Conteo),
              showlegend = T,
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 2))) %>%
  layout(margin = m,
         title = 'Participación por Categoria \n (Afiliados sin Hijos)',
         font = list(color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
         legend = list(x = 0.8, y = 0.5),
         paper_bgcolor='transparent',
         plot_bgcolor='transparent') %>%
  config(displayModeBar = F)
p1


####  Mapa====
# # Capas
library(rgdal); library(raster); library(readxl); library(leaflet); library(sf)
cundi <- readRDS("poligonos-localidades/Cundinamarca.rds")
localidad <- readOGR("poligonos-localidades/poligonos-localidades.shp")
departamento <- readOGR("datos/DEPARTAMENTOS_WGS84.shp")
st_crs(localidad)
st_crs(departamento)

# # Transformar 
# sf.departamento <- st_set_crs(departamento, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
# s.sf.departamento <-  st_transform(departamento, "+proj=longlat +datum=WGS84")
# st_crs(s.sf.gcs)

# Infraestructura Colsubsidio
infra <- read_excel("INFRAESTRUCTURA_PROPIA_COLS.xlsx")
AGENCIA  <- infra %>% filter(UES == "AGENCIA DE EMPLEO")
CSERVICIOS <- infra %>% filter(UES=="CENTROS DE SERVICIO")
EDUCACION <- infra %>% filter(UES=="EDUCACION")
MERCADEO_SOCIAL <- infra %>% filter(UES=="MERCADEO SOCIAL")
SUPERMERCADOS <- infra %>% filter(TIPO=="SUPERMERCADOS")
MEDICAMENTOS <- infra %>% filter(TIPO=="DROGUERIA")
RYT <- infra %>% filter(UES=="RECREACION Y TURISMO")
SALUD <- infra %>% filter(UES=="SALUD")
VIVIENDA <- infra %>% filter(UES=="VIVIENDA")


# Infraestrutura LogColsubsidio

leafIconsSP <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "EDUCACION",
                   "icons/ICONOS_ACT/Supermercados.png","icons/ICONOS_ACT/Supermercados.png"),
  iconWidth = 15, iconHeight = 20,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsDR <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "EDUCACION",
                   "icons/ICONOS_ACT/Farmacias.png","icons/ICONOS_ACT/Farmacias.png"),
  iconWidth = 15, iconHeight = 20,
  iconAnchorX = 16, iconAnchorY = 40)

# Consulta mapa
paleta <- colorFactor(topo.colors(5), departamento$NOMBREDEPT)

map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 4, maxZoom = 20))

map2 %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  # addCircleMarkers(data=aux_info, lng =~cx_persona, lat =~cy_persona,fillOpacity = 0.2,radius = 2, stroke=FALSE) %>%
  addPolygons(data=departamento, fill = F, stroke = T, color = "navy", weight = 1, smoothFactor = 0.5, opacity = 1, fillColor = ~paleta(NOMBREDEPT)) %>%
  # addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
  addLayersControl(
    # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
    overlayGroups =  c("Supermercados","Medicamentos"),
    options = layersControlOptions(collapsed = TRUE), position = "bottomleft") %>%
  # addMarkers(data=AGENCIA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsAG, group = "Agencia de Empleo") %>%
  # addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsCS, group = "Centros de Servicio") %>%
  # addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsED, group = "Educacion") %>%
  addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSP, group = "Supermercados") %>%
  addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsDR, group = "Medicamentos") %>%
  # addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsRYT, group = "Recreacion y Turismo") %>%
  # addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsSL, group = "Salud") %>%
  # addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsVV, group = "Vivienda") %>%
  hideGroup(c("Supermercados")) %>% 
  setView(-74.078773, 4.64144452, zoom = 5) %>% 
  addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)

# Con mapview
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")


some.eu.countries <- c("Colombia")
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long)+4, lat = mean(lat)+4)

departamento_df_gru <- departamento_df %>% 
  mutate(NOMBREDEPT = iconv(NOMBREDEPT, from = "UTF-8", to = "ASCII//TRANSLIT")) %>% 
  group_by(NOMBREDEPT) %>% 
  summarise(long = mean(long), lat = mean(lat))
  

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  # geom_polygon(aes(departamento@polygons))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d(begin = 0.5)+
  theme_void()+
  theme(legend.position = "none")

MEDICAMENTOS_IMG <- MEDICAMENTOS %>% 
  mutate(image = c("icons/ICONOS_ACT/Farmacias.png"))

class(departamento)
spdf_to_df <- function(spdf) {
  df <- suppressMessages(sp::merge(broom::tidy(spdf), as.data.frame(spdf),
                                   by.x = "id", by.y = 0))
  if ("MTRS" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTRS = as.character(MTRS))
  }
  if ("MTR" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTR = as.character(MTR))
  }
  return(df)
}

departamento_df <- spdf_to_df(departamento)
str(departamento_df)
# Version 2
library(ggspatial); library(ggimage)
ggplot(data = departamento, aes(x = long, y = lat)) +
  # geom_sf(fill= "antiquewhite") +
  geom_polygon(aes( group = group)) +
  geom_polygon(data = departamento_df, aes( group = group, col="lightgrey"), fill = "lightgrey") +
  geom_text(aes(label=region), data= region.lab.data, color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  geom_text(aes(label=NOMBREDEPT), data= departamento_df_gru, color = "darkgray", fontface = "italic", check_overlap = FALSE) +
  annotate(geom = "text", x = -60, y = 10, label = "Droguerias", fontface = "italic", color = "grey22", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
  geom_path(aes(group=group), size=1) + 
  geom_point(data = MEDICAMENTOS, aes(x = CX, y = CY), col="yellow", size=2, shape=16) +
  # geom_image(aes(image=image), size=.05) + 
  coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Droguerias Colsubsidio") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


ggplot(data = departamento, aes(x = long, y = lat)) +
  # geom_sf(fill= "antiquewhite") +
  geom_polygon(aes( group = group)) +
  geom_polygon(data = departamento_df, aes( group = group, col="lightgrey"), fill = "lightgrey") +
  geom_text(aes(label=region), data= region.lab.data, color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  geom_text(aes(label=NOMBREDEPT), data= departamento_df_gru, color = "darkgray", fontface = "italic", check_overlap = FALSE) +
  annotate(geom = "text", x = -60, y = 10, label = "Droguerias", fontface = "italic", color = "grey22", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", 
                         # which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "tr") +
  geom_path(aes(group=group), size=1) + 
  geom_point(data = SUPERMERCADOS, aes(x = CX, y = CY), col="yellow", size=2, shape=16) +
  # geom_image(aes(image=image), size=.05) + 
  coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Supermercados Colsubsidio") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


