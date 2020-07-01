# Consulta información
rm(list = ls())

# librerias
library(tidyr)
library(dplyr)
library(RODBC)
library(leaflet)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data_consolidada ----
consolidada <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionMAR2020.rds') %>% 
  filter(marca_afiliado_unico,
         !is.na(cx_persona)) %>% 
  select(id_persona,id_empresa,Edad,Segmento_poblacional,Genero,Categoria,Salario,
         cx_persona,cy_persona,cx_empresa,cy_empresa,
         MunicipioPersona,MunicipioEmpresa,DepartamentoPersona,DepartamentoEmpresa) %>% 
  filter(cx_persona > 0 | cy_persona > 0)
names(consolidada)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Habeas y contacto ----

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

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Contruccion mapa ----

recta1 <- data.frame(p1 = c(4.721139, -74.026373),
                     p2 = c(4.731663, -74.067779)) %>% 
  t() %>% 
  data.frame() %>% 
  dplyr::rename(CY = X1, CX = X2)
recta1
recta2 <- data.frame(p1 = c(4.731663, -74.067779),
                     p2 = c(4.760440, -74.066047))%>% 
  t() %>% 
  data.frame() %>% 
  dplyr::rename(CY = X1, CX = X2)
recta2

recta3 <- data.frame(p1 = c(4.760440, -74.066047),
                     p2 = c(4.875529, -73.988220))%>% 
  t() %>% 
  data.frame() %>% 
  dplyr::rename(CY = X1, CX = X2)
recta3

recta4 <- data.frame(p1 = c(4.875529, -73.988220),
                     p2 = c(4.721139, -74.026373))%>% 
  t() %>% 
  data.frame() %>% 
  dplyr::rename(CY = X1, CX = X2)
recta4


modelo1 <- as.vector(coef(lm(CY ~ CX, data = recta1)))
modelo2 <- as.vector(coef(lm(CY ~ CX, data = recta2)))
modelo3 <- as.vector(coef(lm(CY ~ CX, data = recta3)))
modelo4 <- as.vector(coef(lm(CY ~ CX, data = recta4)))


# Filtrado base
unique(consolidada$MunicipioPersona)
consolidada1 <- consolidada %>% 
  filter(MunicipioPersona %in% c("CAJICA","CHIA","ZIPAQUIRA","TOCANCIPA","COTA","SOPO") | MunicipioEmpresa %in% c("CAJICA","CHIA","ZIPAQUIRA","TOCANCIPA","COTA","SOPO")) %>% 
  filter(!is.na(cx_persona))

consolidada2 <- consolidada %>% 
  filter(cy_persona >= modelo1[1] + modelo1[2]*cx_persona & 
           cy_persona <= modelo2[1] + modelo2[2]*cx_persona & 
           cy_persona <= modelo3[1] + modelo3[2]*cx_persona & 
           cy_persona >= modelo4[1] + modelo4[2]*cx_persona) 

consolidada3 <- consolidada %>% 
  filter(cy_empresa >= modelo1[1] + modelo1[2]*cx_empresa & 
           cy_empresa <= modelo2[1] + modelo2[2]*cx_empresa & 
           cy_empresa <= modelo3[1] + modelo3[2]*cx_empresa & 
           cy_empresa >= modelo4[1] + modelo4[2]*cx_empresa) 
str(consolidada)

union_consolidada <- bind_rows(consolidada1,consolidada2,consolidada3) %>% 
  distinct() %>% 
  left_join(tb_autorizaciones, by = "id_persona")
str(union_consolidada)
table(duplicated(union_consolidada$id_persona))

union_consolidada_mapa <-union_consolidada %>% 
  # filter(DepartamentoPersona,) %>% 
  sample_n(5000)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 2)) %>%
  addCircleMarkers(data = union_consolidada_mapa, lng =~ cx_persona, lat =~ cy_persona, radius = 0.01, opacity = 0.09)

library(Hmisc)
bd_entrega <- union_consolidada %>% 
  mutate(GrupoEtario = case_when(Edad <= 5 ~ "0 a 5 años",
                                 Edad >= 6 & Edad <= 11 ~ "6 a 11 años",
                                 Edad >= 12 & Edad <= 18 ~ "12 a 18 años",
                                 Edad >= 19 & Edad <= 26 ~ "19 a 26 años",
                                 Edad >= 27 & Edad <= 59 ~ "27 a 59 años",
                                 Edad >= 60 ~ "Mas de 60 años"),
         RangoSMMLV= cut2(x = round(Salario/877802,1), g = 10),
         RangoSMMLV = case_when(RangoSMMLV == "[0.1,   1.1)" ~ "Entre 0 y 1",
                                RangoSMMLV == "[1.1,   1.4)" ~ "Entre 1.1 y 1.3",
                                RangoSMMLV == "[1.4,   1.8)" ~ "Entre 1.4 y 1.7",
                                RangoSMMLV == "[1.8,   2.4)" ~ "Entre 1.8 y 2.3",
                                RangoSMMLV == "[2.4,   3.5)" ~ "Entre 2.4 y 3.4",
                                RangoSMMLV == "[3.5,   5.8)" ~ "Entre 3.5 y 5.7",
                                RangoSMMLV == "[5.8,1912.0]" ~ "Mas de 5.8")) %>% 
  select(c(MunicipioPersona,MunicipioEmpresa,
           SegmentoPoblacional=Segmento_poblacional,Genero,Categoria,GrupoEtario,RangoSMMLV,Autorizacion=Contacto_autorizacion)) %>% 
  data.frame()
str(bd_entrega)
table(bd_entrega$RangoSMMLV)

library(writexl)
write_xlsx(bd_entrega, "Poblacion_Bellavista.xlsx")
