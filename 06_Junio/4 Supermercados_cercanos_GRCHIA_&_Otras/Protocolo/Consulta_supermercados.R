# Librerias
library(readxl); library(dplyr); library(geosphere)
infraestructura <- read_excel("INFRAESTRUCTURA_PROPIA_COLSUBSIDIO.xlsx") %>% 
  filter(TIPO == "SUPERMERCADO") %>% 
  mutate(punto = paste0("S", seq(1:84)))
str(infraestructura)
table(infraestructura$TIPO)

choconta <- data.frame(CX = -73.685051, CY = 5.145504)
macheta <- data.frame(CX = -73.607714, CY = 5.080759)

infraestructura$dist_choconta <- round(distHaversine(infraestructura[,c("CX","CY")], choconta[,c("CX","CY")])/1000,1)
infraestructura$dist_macheta <- round(distHaversine(infraestructura[,c("CX","CY")], macheta[,c("CX","CY")])/1000,1)

library(writexl)
write_xlsx(infraestructura, "infraestructura.xlsx")

# Librerias
library(dplyr); library(readxl); library(geosphere); library(tidyr)

consolidada <- readRDS("//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionABR2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  filter(id_empresa == "NIT8001192975") %>% 
  mutate(cx_persona = ifelse(cx_persona == 0, NA, cx_persona),
         cy_persona = ifelse(cy_persona == 0, NA, cy_persona))
str(consolidada)

afiliados_v <- consolidada %>% 
  select(id_persona,cx_persona,cy_persona,cx_empresa,cy_empresa) %>% 
  filter(!is.na(cx_persona) | !is.na(cy_persona))
str(afiliados_v)

afiliados_t <- consolidada %>% 
  select(id_persona,cx_persona,cy_persona,cx_empresa,cy_empresa) %>% 
  filter(!is.na(cx_empresa) | !is.na(cy_empresa))
str(afiliados_t)

mat_v <- data.frame(round(distm(afiliados_v[,c('cx_persona','cy_persona')], infraestructura[,c('CX','CY')],
                                fun=distVincentyEllipsoid)/1000,2))
dim(mat_v)

mat_t <- data.frame(round(distm(afiliados_t[,c('cx_empresa','cy_empresa')], infraestructura[,c('CX','CY')],
                                fun=distVincentyEllipsoid)/1000,2))
dim(mat_t)

df_mat_v <- bind_cols(afiliados_v %>% select(id_persona), mat_v)
names(df_mat_v) <- c("id_persona" ,paste0("S", seq(1:84)))
str(df_mat_v)

df_mat_t <- bind_cols(afiliados_t %>% select(id_persona), mat_t)
names(df_mat_t) <- c("id_persona" ,paste0("S", seq(1:84)))
str(df_mat_t)


afiliados_v_super <- df_mat_v %>% 
  gather("Supermercado_v","distancia_v",2:85) %>% 
  data.frame() %>% 
  arrange(id_persona) %>% 
  group_by(id_persona) %>% 
  arrange(distancia_v) %>% 
  filter(row_number()<=2) %>%
  ungroup() %>% 
  data.frame() %>% 
  left_join(infraestructura %>% select(NOMBRE,punto), by = c("Supermercado_v"="punto"))
str(afiliados_v_super)

afiliados_t_super <- df_mat_t %>% 
  gather("Supermercado_t","distancia_t",2:85) %>% 
  data.frame() %>% 
  arrange(id_persona) %>% 
  group_by(id_persona) %>% 
  arrange(distancia_t) %>% 
  filter(row_number()<=2) %>%
  ungroup() %>% 
  data.frame() %>% 
  left_join(infraestructura %>% select(NOMBRE,punto), by = c("Supermercado_t"="punto"))
str(afiliados_t_super)


p1_v <- afiliados_v_super %>% 
  arrange(id_persona,distancia_v) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  data.frame()
str(p1_v)

p2_v <- afiliados_v_super %>% 
  arrange(id_persona,distancia_v) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==2) %>% 
  data.frame()
str(p2_v)


p1_t <- afiliados_t_super %>% 
  arrange(id_persona,distancia_t) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  data.frame()
str(p1_t)

p2_t <- afiliados_t_super %>% 
  arrange(id_persona,distancia_t) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==2) %>% 
  data.frame()
str(p2_t)

names(consolidada)
bd_entrega <- consolidada %>% 
  select(id_persona, Edad, Genero, MunicipioPersona) %>% 
  left_join(p1_v %>% select(id_persona,Supermercado1_vive=NOMBRE), by = "id_persona") %>% 
  left_join(p2_v %>% select(id_persona,Supermercado2_vive=NOMBRE), by = "id_persona") %>% 
  left_join(p1_t %>% select(id_persona,Supermercado1_trabajo=NOMBRE), by = "id_persona") %>% 
  left_join(p2_t %>% select(id_persona,Supermercado2_trabajo=NOMBRE), by = "id_persona")
str(bd_entrega)
write_xlsx(bd_entrega, "supermerdado_cercano.xlsx")
sum(!is.na(bd_entrega$MunicipioPersona))
