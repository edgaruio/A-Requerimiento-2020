# Cargamos librerias
library(leaflet); library(dplyr); library(writexl); library(data.table)

puntos <- data.frame(
  Direccion = c("Carrera 10 # 16-82", "Carrera 13 # 77-90", "Calle 42 Sur # 23B-45", "Suba Calle 145 No. 103b - 69",
             "Calle 17 # 68B-48"),
  cx = c(-74.074988, -74.056799, -74.123816, -74.095935, -74.118348),
  cy = c(4.604784, 4.663713, 4.579725, 4.746059, 4.641923)
)

puntos

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 4, maxZoom = 20))
map %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addMarkers(data=puntos, lng =~cx, lat =~cy) %>%
  setView(-73.918635, 4.937807, zoom = 10)

write_xlsx(puntos, "Resultados/Puntos.xlsx")
fwrite(puntos %>% 
         rename(NOMBRE = Direccion, CX = cx, CY = cy), 
       "Resultados/Puntos.csv", sep = ",", dec = ".", row.names = F)


# Descriptivo afiliados ====
afil_obj <- fread("Resultados/Afiliados_Obj_2020-06-30.csv", dec = ",", encoding = "UTF-8") %>% 
  select(id_persona,Dis_v,NOMBRE) %>% 
  rename(Nombre_punto=NOMBRE) %>% 
  filter(Dis_v <= 1) %>% 
  data.frame()
str(afil_obj)
table(duplicated(afil_obj$id_persona))

consolidada <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionMAY2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona,Genero,Edad,Salario,Categoria,Segmento_poblacional,total_numero_grupo_familiar,segmento_grupo_familiar,
         id_empresa,RazonSocial,Piramide1,Piramide2,promedio_aportes,promedio_remaneto,SectorCIIU,ActividadCIIU,NumEmpleados,
         Seg_Alto:Seg_Medio) %>% 
  mutate(nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU, SectorCIIU),
         nuevo_ciiu = ifelse(is.na(nuevo_ciiu), "Sin información", nuevo_ciiu))
names(consolidada)
table(consolidada$SectorCIIU)

df_afiliados <- afil_obj %>% 
  left_join(consolidada, by = "id_persona")
str(df_afiliados)

library(esquisse)
esquisser()

df_afiliados <- df_afiliados %>%
 filter(!(Piramide1 %in% "5 Micro")) %>%
 filter(!is.na(SectorCIIU))

library(ggplot2)

ggplot(df_afiliados) +
 aes(x = Piramide1, fill = SectorCIIU) +
 geom_bar(position = "fill") +
 scale_fill_hue() +
 labs(x = "Pirámide", y = "Participación", title = "Distribución de Empresas", fill = "Sector") +
 coord_flip() +
 theme_ft_rc()

saveRDS(df_afiliados, "Resultados/df_afil_obj.rds")
