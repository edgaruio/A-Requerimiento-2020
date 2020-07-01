# Librerias

library(readxl); library(dplyr); library(geosphere); library(tidyr); library(readxl)

### Datos ====
infraestructura <- read_excel("Datos/INFRAESTRUCTURA_PROPIA_COLSUBSIDIO.xlsx") %>% 
  filter(TIPO == "SUPERMERCADO") %>% 
  select(COD, NOMBRE, CX, CY) %>% 
  # mutate(punto = paste0("S", seq(1:84))) %>% 
  data.frame()
str(infraestructura)
table(infraestructura$TIPO)


red_empresas <- read_excel("Datos/Red_empresas.xlsx") %>% 
  mutate_all(as.character) %>% 
  data.frame()
str(red_empresas)


consolidada <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionMAY2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  mutate(cx_persona = ifelse(cx_persona == 0, NA, cx_persona),
         cy_persona = ifelse(cy_persona == 0, NA, cy_persona)) %>%
  # filter(!is.na(cx_persona) | !is.na(cx_empresa)) %>% 
  data.frame()
str(consolidada)


afil_obj <- consolidada %>% 
  select(id_persona, id_empresa, RazonSocial, NumIdEmpresa, cx_persona, cy_persona, cx_empresa, cy_empresa) %>% 
  mutate(NumIdEmpresa = as.character(NumIdEmpresa)) %>%
  filter(NumIdEmpresa %in% red_empresas$NIT) %>%
  data.frame()
str(afil_obj)
sort(unique(afil_obj$RazonSocial))


# afiliados_v <- afil_obj %>% 
#   select(id_persona,cx_persona,cy_persona) %>% 
#   filter(!is.na(cx_persona) | !is.na(cy_persona))
# str(afiliados_v)
# 
# 
# afiliados_t <- afil_obj %>% 
#   select(id_persona,cx_empresa,cy_empresa) %>% 
#   filter(!is.na(cx_empresa) | !is.na(cy_empresa))
# str(afiliados_t)



### Cercanía ====

n_puntos <- nrow(infraestructura)
puntos <- infraestructura %>% 
  mutate(puntos = as.character(1:n_puntos))
cedulas <- data.frame(id_persona = c(),
                      Dis_v = c(),
                      Dis_t = c())

for (i in 1:n_puntos){
  consulta <- afil_obj %>%
    mutate(Dis_v = distHaversine(cbind(cx_persona,cy_persona), puntos[i,c('CX','CY')])/1000,
           Dis_t = distHaversine(cbind(cx_empresa,cy_empresa), puntos[i,c('CX','CY')])/1000) %>%
    # filter(Dis_v <= 50 | Dis_t <= 50) %>%
    select(id_persona, Dis_v, Dis_t) %>%
    mutate(Punto = as.character(i)) %>% 
    left_join(puntos %>% dplyr::select(COD, NOMBRE, puntos), by = c("Punto"="puntos"))
  
  print(paste("Punto", i, "de", n_puntos))
  
  cedulas <- bind_rows(cedulas,consulta)
}
# cedulas <- cedulas %>% distinct()
table(duplicated(cedulas$id_persona))

### Resultados ====

# CC1000215766
cedulas_v <- cedulas %>% 
  select(id_persona, Dis_v, NOMBRE) %>% 
  arrange(id_persona, Dis_v) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  data.frame() %>% 
  rename(Super_cercano_viven = NOMBRE)

cedulas_t <- cedulas %>% 
  select(id_persona, Dis_t, NOMBRE) %>% 
  arrange(id_persona, Dis_t) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  data.frame() %>% 
  rename(Super_cercano_trabajo = NOMBRE)

union_cc <- cedulas_v %>% 
  full_join(cedulas_t, by = "id_persona") %>% 
  mutate(Super_cercano_viven = ifelse(is.na(Dis_v), NA, Super_cercano_viven),
         Super_cercano_trabajo = ifelse(is.na(Dis_t), NA, Super_cercano_trabajo)) %>% 
  left_join(afil_obj %>% select(id_persona, id_empresa, RazonSocial, cx_persona, cy_persona,
                                cx_empresa, cy_empresa), by = "id_persona")
str(union_cc)
table(duplicated(union_cc$id_persona))
write_xlsx(union_cc, "Resultados/Afiliados_empresas.xlsx")


### Mapa ===
library(writexl); library(leaflet); library(leaflet.extras)


leafIconsSP <- icons(
  iconUrl = "Datos/Supermercados.png",
  iconWidth = 10, iconHeight = 14,
  iconAnchorX = 16, iconAnchorY = 40)

map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 4, maxZoom = 20))
map2 %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers(data=union_cc, lng =~cx_persona, lat =~cy_persona, fillOpacity = 0.5,radius = 0.5, stroke=FALSE,
                   color = "darkgreen") %>%
  addMarkers(data=infraestructura, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSP, group = "Supermercados") %>%
  setView(-73.918635, 4.937807, zoom = 10)

union_emp <- union_cc %>% 
  filter(!is.na(cx_empresa)) %>% 
  select(RazonSocial, cx_empresa, cy_empresa) %>% 
  distinct()
map3 <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 4, maxZoom = 20))
map3 %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addMarkers(data=union_emp, lng =~cx_empresa, lat =~cy_empresa) %>%
  addMarkers(data=infraestructura, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSP, group = "Supermercados") %>%
  setView(-73.918635, 4.937807, zoom = 10)

resumen <- union_cc %>% 
  mutate(con_geo_v = ifelse(!is.na(Dis_v), 1, 0),
         con_geo_t = ifelse(!is.na(Dis_t), 1, 0)) %>% 
  group_by(RazonSocial) %>% 
  summarise(Empleados = n(),
            Tienen_geo_v = sum(con_geo_v),
            Tienen_geo_t = sum(con_geo_t))
str(resumen)
write_xlsx(resumen, "Resultados/Resumen.xlsx")

