# Librerias
library(dplyr); library(readxl); library(geosphere); library(tidyr)

consolidada <- readRDS("//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionFEB2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona,cx_persona,cy_persona,cx_empresa,cy_empresa) %>% 
  filter(!is.na(cx_persona) | !is.na(cy_persona) | !is.na(cx_empresa) | !is.na(cy_empresa))
str(consolidada)

cedulas <- readRDS("Cedulas.rds") %>% 
  select(id_persona) %>% 
  distinct() %>% 
  left_join(consolidada, by = "id_persona")
str(cedulas)
table(duplicated(cedulas$id_persona))

infraestructura <- read_excel("INFRAESTRUCTURA_PROPIA_COLS.xlsx") %>% 
  filter(UES == "CENTROS DE SERVICIO") %>% 
  select(NOMBRE,CX,CY) %>% 
  mutate(punto = c(paste0("d",c(1:15))))
str(infraestructura)
table(infraestructura$UES)

dist_cedulas <- cedulas %>% 
  mutate(
    d1 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[1,c("CX","CY")])/1000,1),
    d2 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[2,c("CX","CY")])/1000,1),
    d3 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[3,c("CX","CY")])/1000,1),
    d4 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[4,c("CX","CY")])/1000,1),
    d5 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[5,c("CX","CY")])/1000,1),
    d6 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[6,c("CX","CY")])/1000,1),
    d7 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[7,c("CX","CY")])/1000,1),
    d8 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[8,c("CX","CY")])/1000,1),
    d9 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[9,c("CX","CY")])/1000,1),
    d10 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[10,c("CX","CY")])/1000,1),
    d11 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[11,c("CX","CY")])/1000,1),
    d12 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[12,c("CX","CY")])/1000,1),
    d13 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[13,c("CX","CY")])/1000,1),
    d14 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[14,c("CX","CY")])/1000,1),
    d15 = round(distHaversine(.[,c("cx_persona","cy_persona")], infraestructura[15,c("CX","CY")])/1000,1),
    d1.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[1,c("CX","CY")])/1000,1),
    d2.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[2,c("CX","CY")])/1000,1),
    d3.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[3,c("CX","CY")])/1000,1),
    d4.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[4,c("CX","CY")])/1000,1),
    d5.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[5,c("CX","CY")])/1000,1),
    d6.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[6,c("CX","CY")])/1000,1),
    d7.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[7,c("CX","CY")])/1000,1),
    d8.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[8,c("CX","CY")])/1000,1),
    d9.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[9,c("CX","CY")])/1000,1),
    d10.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[10,c("CX","CY")])/1000,1),
    d1.t1 = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[11,c("CX","CY")])/1000,1),
    d12.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[12,c("CX","CY")])/1000,1),
    d13.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[13,c("CX","CY")])/1000,1),
    d14.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[14,c("CX","CY")])/1000,1),
    d15.t = round(distHaversine(.[,c("cx_empresa","cy_empresa")], infraestructura[15,c("CX","CY")])/1000,1)
    ) %>% 
  select(id_persona,d1:d15.t) %>% 
  gather("punto","distancia",2:31) %>% 
  data.frame()
str(dist_cedulas)

punto_cercano <- dist_cedulas %>% 
  arrange(id_persona) %>% 
  group_by(id_persona) %>% 
  arrange(distancia) %>% 
  filter(row_number()<=3) %>%
  ungroup() %>% 
  data.frame() %>% 
  mutate(punto2 = gsub(".t","",x = punto, fixed = T)) %>% 
  left_join(infraestructura %>% select(NOMBRE,punto), by = c("punto2"="punto"))
str(punto_cercano)

p1 <- punto_cercano %>% 
  arrange(id_persona,distancia) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  data.frame()
str(p1)

p2 <- punto_cercano %>% 
  arrange(id_persona,distancia) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==2) %>% 
  data.frame()

p3 <- punto_cercano %>% 
  arrange(id_persona,distancia) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==3) %>% 
  data.frame()

bd_entrega <- cedulas %>% 
  select(id_persona) %>% 
  left_join(p1 %>% select(id_persona,Punto1=NOMBRE), by = "id_persona") %>% 
  left_join(p2 %>% select(id_persona,Punto2=NOMBRE), by = "id_persona") %>% 
  left_join(p3 %>% select(id_persona,Punto3=NOMBRE), by = "id_persona")
str(bd_entrega)
saveRDS(bd_entrega, "cs_cercano.rds")
