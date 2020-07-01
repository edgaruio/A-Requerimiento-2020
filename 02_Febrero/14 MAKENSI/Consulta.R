library(RODBC); library(dplyr); library(tidyr); library(geosphere); library(Hmisc); library(scales); library(stringr)

plaza_americas <- data.frame(CX = -74.135424, CY = 4.618715)
str(plaza_americas)

Beneficiarios_nucleo <- readRDS("consulta_sub24102019.rds") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  group_by(id_persona) %>% 
  summarise(beneficiarios = n_distinct(id_persona_familiar) + 1)
str(Beneficiarios_nucleo)

consolidada <- readRDS("//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionNOV2019.rds") %>% 
  mutate_at(.vars = c("id_persona","NumIdPersona"), .funs = as.character) %>% 
  filter(marca_afiliado_unico) %>% 
  left_join(Beneficiarios_nucleo, by = "id_persona") %>% 
  mutate(beneficiarios = ifelse(is.na(beneficiarios), 1, beneficiarios)) %>% 
  mutate(dv = round(distHaversine(.[,c("cx_persona","cy_persona")], plaza_americas[,c("CX","CY")])/1000,1),
         dt = round(distHaversine(.[,c("cx_empresa","cy_empresa")], plaza_americas[,c("CX","CY")])/1000,1)) %>% 
  mutate(
    dvt = case_when(
      dv <= 2 | dt <= 2 ~ "Menor a 2 km",
      dv > 2 & dv <= 4 | dt > 2 & dt <= 4 ~ "Entre 2 y 4 km",
      dv > 6 | dt > 6 ~ "Más de 6 km"),
    dvt = ifelse(is.na(dvt), "Sin información", dvt),
    Edad_i = cut2(Edad, g = 5),
    Edad_i = ifelse(is.na(as.character(Edad_i)),"Sin información",as.character(Edad_i)),
    salario_percapita = Salario/beneficiarios,
    SMMLV_percapita = salario_percapita/878000,
    SMMLV_percapita_i = cut2(SMMLV_percapita, g = 5),
    SMMLV_percapita_i = ifelse(is.na(as.character(SMMLV_percapita_i)),"Sin información", as.character(SMMLV_percapita_i))
  )
names(consolidada)

afiliados_cercania <- consolidada %>% 
  group_by(Edad_i,dvt) %>% 
  summarise(Afiliados = n_distinct(id_persona))
str(afiliados_cercania)

options(scipen = 999)
ggplot(afiliados_cercania) +
  aes(x = Edad_i, y = dvt, fill = Afiliados) +
  geom_text(aes(label = Afiliados)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = comma(round(Afiliados, 1))), size = 4.8) +
  labs(x = "Edad", y = "Distancia", title = "Distribución Afiliados")

### Beneficiaros percapita ====

Beneficiarios_percapita <- readRDS("consulta_sub24102019.rds") %>% 
  filter(marca_afiliado_unico == "X" & parentesco == "HIJO" & Persona_1_Edad <= 29) %>% 
  left_join(consolidada %>% select(id_persona,Edad,Salario,cx_persona,cy_persona,cx_empresa,cy_empresa), by = c("id_persona"="id_persona")) %>% 
  left_join(Beneficiarios_nucleo, by = "id_persona") %>% 
  mutate(beneficiarios = ifelse(is.na(beneficiarios), 1, beneficiarios)) %>% 
  mutate(dv = round(distHaversine(.[,c("cx_persona","cy_persona")], plaza_americas[,c("CX","CY")])/1000,1),
         dt = round(distHaversine(.[,c("cx_empresa","cy_empresa")], plaza_americas[,c("CX","CY")])/1000,1)) %>% 
  mutate(
    dvt = case_when(
      dv <= 2 | dt <= 2 ~ "Menor a 2 km",
      dv > 2 & dv <= 4 | dt > 2 & dt <= 4 ~ "Entre 2 y 4 km",
      dv > 6 | dt > 6 ~ "Más de 6 km"),
    dvt = ifelse(is.na(dvt), "Sin información", dvt),
    Persona_1_Edad_i = case_when(
      Persona_1_Edad < 18 ~ "Menor a 18",
      Persona_1_Edad >= 18 & Persona_1_Edad < 22 ~ "Entre 18 y 21",
      Persona_1_Edad >= 22 & Persona_1_Edad < 26 ~ "Entre 22 y 25",
      Persona_1_Edad >= 26 & Persona_1_Edad <= 29 ~ "Entre 26 y 29",),
    salario_percapita = Salario/beneficiarios,
    SMMLV_percapita = salario_percapita/878000,
    SMMLV_percapita_i = cut2(SMMLV_percapita, g = 5),
    SMMLV_percapita_i = ifelse(is.na(as.character(SMMLV_percapita_i)),"Sin información", as.character(SMMLV_percapita_i))
  )
str(Beneficiarios_percapita)

educacion_hijos <- Beneficiarios_percapita %>% 
  group_by(SMMLV_percapita_i,dvt) %>% 
  summarise(Beneficiarios = n_distinct(id_persona_familiar)) 
# %>% 
#   dplyr::rename("Edad hijo" = "Persona_1_Edad_i")
str(educacion_hijos)

options(scipen = 999)
ggplot(educacion_hijos) +
  aes(x = SMMLV_percapita_i, y = dvt, fill = Beneficiarios) +
  geom_text(aes(label = Beneficiarios)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = comma(round(Beneficiarios, 1))), size = 4.8) +
  labs(x = "SMMLV Percapita", y = "Distancia", title = "Distribución de hijos beneficiarios (Menores a 29 años)")


# con_persona <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/PERSONA.accdb")
# sqlTables(con_persona)
# tb_persona <- sqlQuery(con_persona, paste ("select Id_persona, Edad from Persona"), as.is=T) %>% 
#   data.frame()
# odbcClose(con_persona) 
# str(tb_persona)

conn_educacion <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/FEBRERO/14 MAKENSI/Consumo_Educacion.accdb")
subset(sqlTables(conn_educacion), tableType = "SYSTEM TABLE")
consumo_educacion <- sqlFetch(conn_educacion, "Consulta_Makenci")
str(consumo_educacion)
odbcClose(conn_educacion)

df_consumo_educacion <- consumo_educacion %>% 
  filter(año == 2019 & Año_tb_Alumnos == 2019) %>% 
  group_by(noDocumentoRes,noDocumentoAlu) %>% 
  arrange(desc(mes)) %>% 
  filter(row_number()==1) %>% 
  data.frame() %>% 
  mutate_at(.vars = c("noDocumentoRes","noDocumentoAlu"), .funs = as.character) %>% 
  left_join(consolidada %>% select(id_persona,NumIdPersona,Edad,Salario,cx_persona,cy_persona,cx_empresa,cy_empresa), by = c("noDocumentoRes"="NumIdPersona")) %>% 
  left_join(Beneficiarios_nucleo, by = "id_persona") %>% 
  mutate(beneficiarios = ifelse(is.na(beneficiarios), 1, beneficiarios)) %>% 
  mutate(dv = round(distHaversine(.[,c("cx_persona","cy_persona")], plaza_americas[,c("CX","CY")])/1000,1),
         dt = round(distHaversine(.[,c("cx_empresa","cy_empresa")], plaza_americas[,c("CX","CY")])/1000,1)) %>% 
  mutate(
    dvt = case_when(
      dv <= 2 | dt <= 2 ~ "Menor a 2 km",
      dv > 2 & dv <= 4 | dt > 2 & dt <= 4 ~ "Entre 2 y 4 km",
      dv > 6 | dt > 6 ~ "Más de 6 km"),
    dvt = ifelse(is.na(dvt), "Sin información", dvt),
    Edad_i = case_when(
      Edad < 29 ~ "Menor a 29",
      Edad >= 29 ~ "Más de 29"),
    salario_percapita = Salario/beneficiarios,
    SMMLV_percapita = salario_percapita/878000,
    SMMLV_percapita_i = cut2(SMMLV_percapita, g = 5),
    SMMLV_percapita_i = ifelse(is.na(as.character(SMMLV_percapita_i)),"Sin información", as.character(SMMLV_percapita_i))
  )
str(df_consumo_educacion)
sum(is.na(df_consumo_educacion$salario_percapita))
table(df_consumo_educacion$SMMLV_percapita_i)

### Adultos ====
educacion_adultos <- df_consumo_educacion %>% 
  filter(Infraestructura == "Bac") %>% 
  group_by(SMMLV_percapita_i,dvt) %>% 
  summarise(Estudiantes = n_distinct(noDocumentoAlu)) %>% 
  data.frame()
str(educacion_adultos)

ggplot(educacion_adultos) +
  aes(x = SMMLV_percapita_i, y = dvt, fill = Estudiantes) +
  geom_text(aes(label = Estudiantes)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = comma(round(Estudiantes, 1))), size =5) +
  labs(x = "SMMLV Percapita", y = "Distancia", title = "Distribución de Estudiantes por Ciclos")

### Educación formal ====
educacion_formal <- df_consumo_educacion %>% 
  filter(Infraestructura != "Bac") %>% 
  filter(Grado %in% c("Décimo","Undécimo")) %>% 
  group_by(SMMLV_percapita_i,dvt) %>% 
  summarise(Estudiantes = n_distinct(noDocumentoAlu)) %>% 
  # mutate(Grado = factor(Grado, levels = c("PREJARDÍN","Jardín","Transición",
  #                                         "Primero","Segundo","Tercero","Cuarto","Quinto",
  #                                         "Sexto","Séptimo","Octavo","Noveno","Décimo","Undécimo"),
  #                       labels = c("PREJARDÍN","Jardín","Transición",
  #                                  "Primero","Segundo","Tercero","Cuarto","Quinto",
  #                                  "Sexto","Séptimo","Octavo","Noveno","Décimo","Undécimo"))) %>%
  data.frame()
str(educacion_formal)

ggplot(educacion_formal) +
  aes(x = SMMLV_percapita_i, y = dvt, fill = Estudiantes) +
  geom_text(aes(label = Estudiantes)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = comma(round(Estudiantes, 1))), size = 4.8) +
  labs(x = "SMMLV Percapita", y = "Distancia", title = "Distribución de Estudiantes Décimo - Once")


library(ggplot2)
ggplot(educacion_adultos) +
 aes(x = Grado, weight = n_estudiantes) +
 geom_bar(fill = "#377eb8") +
  geom_text(stat='count', aes(label=..count..), vjust=0.5, hjust=-0.5, size = 8) +
 labs(x = "Grado", y = "Conteo", title = "Distribución Estudiantes ", subtitle = "Adulto Mayor") +
 coord_flip() +
 theme_classic() +
  theme(text = element_text(size=20))
  
ggplot(educacion_formal) +
 aes(x = Grado, weight = n_estudiantes) +
 geom_bar(fill = "#377eb8") +
  geom_text(stat='count', aes(label=..count..), vjust=0.5, hjust=-0.5, size = 8) +
 labs(x = "Grado", y = "Conteo", title = "Distribución de Estudiantes por Grado", subtitle = "Educacación Formal") +
 coord_flip() +
 theme_classic() +
  theme(text = element_text(size=20))


# Ciuu educacion
library(readxl)

# ciuu_educacion <- read_excel("CIUU_Educacion.xlsx")
# str(ciuu_educacion)

ciuu_educacion <- empresas %>% filter(grepl("educac", DescripcionCIIU, ignore.case = T)) %>% select(CIIU, DescripcionCIIU) %>% distinct() %>% 
  mutate(CIIU = as.character(CIIU)) %>% 
  filter(CIIU %in% c("8530","8521"))
str(ciuu_educacion)

empresas_ciiu <- empresas %>% 
  mutate(CIIU = as.character(CIIU)) %>% 
  filter(CIIU %in% ciuu_educacion$CIIU) %>% 
  filter(!is.na(cx_empresa))
str(empresas_ciiu)
sum(is.na(empresas_ciiu$cx_empresa))
table(empresas_ciiu$DescripcionCIIU)

tb_colegios_ciic <- empresas_ciiu %>% 
  group_by(DescripcionCIIU) %>% 
  summarise(conteo = n_distinct(id_empresa)) %>% 
  arrange(desc(conteo))

ggplot(tb_colegios_ciic) +
  aes(x = DescripcionCIIU, weight = conteo) +
  geom_bar(fill = "#377eb8") +
  geom_text(stat='count', aes(label=..count..), vjust=0.5, hjust=-0.5, size = 15) +
  labs(x = "CIIU", y = "Conteo", title = "Distribución de Colegios por Grupo CIIU") +
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  ylim(0,260)

tb_empresas_ciiu <- empresas_ciiu %>% 
  group_by(Piramide1) %>% 
  summarise(conteo = n_distinct(id_empresa))
str(tb_empresas_ciiu)
library(writexl)
write_xlsx(tb_empresas_ciiu, "Salidas/tb_empresas_ciiu.xlsx")

### Mapa Empresas afiliadas con ciuu educacion
library(leaflet); library(leaflet.extras)

pal <- colorFactor(palette = 'Dark2',domain = empresas_ciiu$Piramide1)

map <- leaflet(data = empresas_ciiu, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addMarkers(lng =~ -74.135424, lat =~ 4.618715) %>%
  # addCircles(data = aux_punto, lng = ~CX, lat = ~CY, color = "steelblue", radius = input$Distancia*1000) %>%
  addCircleMarkers(data = empresas_ciiu, lng = ~cx_empresa, lat = ~cy_empresa, fillOpacity = 0.6, color = ~pal(empresas_ciiu$Piramide1), stroke = FALSE, 
                   radius = ~ifelse(empresas_ciiu$Piramide1 == "1 Emp Grandes", 15,
                                    ifelse(empresas_ciiu$Piramide1 == "2 Emp Medio", 10,
                                           ifelse(empresas_ciiu$Piramide1 == "3 Empresas Pymes", 6, 
                                                  ifelse(empresas_ciiu$Piramide1 == "4 Micro", 2 , 10))))) %>%
  addLegend(pal=pal, values= empresas_ciiu$Piramide1, opacity=0.7, title = "Piramide 1", position = "bottomright") 
# %>%
#   addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "study area") %>%
#   addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
#   addLayersControl(
#     # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
#     overlayGroups =  c("Agencia de Empleo","Centros de Servicio","Educación","Supermercados","Medicamentos","Recreación y Turismo","Salud","Vivienda"),
#     options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
#   addMarkers(data=AGENCIA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsAG, group = "Agencia de Empleo") %>%
#   addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsCS, group = "Centros de Servicio") %>%
#   addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsED, group = "Educación") %>%
#   addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsSP, group = "Supermercados") %>%
#   addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsDR, group = "Medicamentos") %>%
#   addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsRYT, group = "Recreación y Turismo") %>%
#   addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsSL, group = "Salud") %>%
#   addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
#              icon = leafIconsVV, group = "Vivienda") %>%
#   hideGroup(c("Centros de Servicio","Educación","Supermercados","Medicamentos","Recreación y Turismo","Salud","Vivienda")) %>%
#   # addLegend(pal=mypalette, values=~aportes_pro, opacity=0.9, title = "Aportes Promedio", position = "bottomright") %>%
#   setView(input$CX, input$CY, zoom = 15) %>% 
#   addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)

# COnsulta genero

consolidada <- readRDS("ConsolidacionENE2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona,DepartamentoPersona,Genero) %>% 
  na.omit() %>% 
  group_by(DepartamentoPersona,Genero) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  data.frame() %>% 
  filter(DepartamentoPersona == "CUNDINAMARCA")
str(consolidada)

empresas <- readRDS("ConsolidacionENE2020.rds") %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct()
str(empresas)

empresas_x_departamento <- empresas %>% 
  group_by(MunicipioEmpresa) %>% 
  summarise(conteo = n_distinct(id_empresa))

### Mapa Colegios en Alianza

channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Contacto/Infraestructura/Infraestructura.accdb"
)
sqlTables(channel)

Tb_alianzas<- sqlQuery( 
  channel , 
  paste ("select * from Alianzas_Educativas"),
  as.is=T
) %>% 
  data.frame()
odbcCloseAll() 

str(Tb_alianzas)
map <- leaflet(data = Tb_alianzas, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addMarkers(lng =~ -74.135424, lat =~ 4.618715) %>%
  # addCircles(data = aux_punto, lng = ~CX, lat = ~CY, color = "steelblue", radius = input$Distancia*1000) %>%
  addCircleMarkers(data = Tb_alianzas, lng = ~CX, lat = ~CY, fillOpacity = 0.6, stroke = FALSE, radius = 2, color = "darkgreen") 
# %>%
#   addPolygons(data=localidad, fill = F, stroke = T, color = "navy") %>%
#   addPolygons(data=cundi, fill = F, stroke = T, color = "red")


### Beneficios ====
library(dplyr)
consolidada <- readRDS("ConsolidacionENE2020.rds") %>% 
  filter(marca_afiliado_unico)

Beneficiarios <- readRDS("consulta_sub24102019.rds") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  group_by(piramide_2) %>% 
  summarise(Beneficiarios = n_distinct(id_persona_familiar))
str(Beneficiarios)

library(data.table)
afil_objetivo <- fread("Afiliados_2020-03-05 10_33_50_.csv") %>% 
  left_join(consolidada, by = "id_persona") %>% 
  group_by(Piramide2) %>% 
  summarise(conteo = n_distinct(id_persona))
str(afil_objetivo)

library(data.table)
afil_objetivo <- fread("Afiliados_2020-03-05 10_33_50_.csv")

Beneficiarios_obj <- readRDS("consulta_sub24102019.rds") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  filter(id_persona %in% afil_objetivo$id_persona) %>% 
  group_by(piramide_2) %>% 
  summarise(Beneficiarios = n_distinct(id_persona_familiar))
str(Beneficiarios_obj)

library(dplyr)
Beneficiarios_por_edad <- readRDS("consulta_sub24102019.rds") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  filter(id_persona %in% afil_objetivo$id_persona) %>% 
  filter(piramide_2 %in% c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
                           "4.1 Estándar","4.5 Transaccional")) %>% 
  mutate(rango_edad_beneficiario = cut(Edad_Beneficiario, breaks = seq(0,120,5),include.lowest = T)) %>% 
  group_by(piramide_2,rango_edad_beneficiario) %>% 
  summarise(Conteo = n_distinct(id_persona_familiar)) %>% 
  data.frame() 
# %>% 
#   mutate(piramide_2 = factor(piramide_2, 
#                              labels = c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
#                                                     "4.1 Estándar","4.5 Transaccional"),
#                              ordered = c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
#                                          "4.1 Estándar","4.5 Transaccional")))
# %>% 
#   mutate(piramide_2 = factor(piramide_2, 
#                              levels = c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
#                                         "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
#                                         "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional","4.6 Transaccional - Facultativo",
#                                         "4.7 Transaccional - Independiente","4.8 Transaccional - Pensionado","5.1 Colsubsidio"), 
#                              labels = c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
#                                         "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
#                                         "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional","4.6 Transaccional - Facultativo",
#                                         "4.7 Transaccional - Independiente","4.8 Transaccional - Pensionado","5.1 Colsubsidio")))
str(Beneficiarios_por_edad)
unique(Beneficiarios_por_edad$piramide_2)

total <- Beneficiarios_por_edad %>% 
  group_by(rango_edad_beneficiario) %>% 
  summarise(conteo = sum(Conteo))
library(writexl)
writexl::write_xlsx(total, "Salidas/Total.xlsx")
sum(total$conteo)

library(ggplot2); library(scales); library(stringr)
ggplot(Beneficiarios_por_edad) +
  aes(x = rango_edad_beneficiario, y = piramide_2, fill = Conteo) +
  geom_text(aes(label = Conteo)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(size=15)) +
  geom_text(aes(label = comma(round(Conteo, 1))), size = 4.8) +
  labs(x = "Rango Edad Beneficiarios", y = "Piramide 2", title = "Distribución de Beneficiarios a 2 km")
