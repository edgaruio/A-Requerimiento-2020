#### Carga e instalacion de paquetes ====

# library(tidyverse)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(leaflet)
library(plotly) 
library(DT) 
library(data.table)
library(shinythemes)
library(shinydashboardPlus)
library(pipeR)
library(scales)
library(shiny)
library(Hmisc)
library(shinycustomloader)
library(readxl)
library(lubridate)
library(ggalt)
library(stringr)
# library(bit64)

### Carga de datos ----
# saveRDS(base_contratos, file = "Mis v?deos/base_contratos_edit.rds")

# Tabla tipo contrato
tb_tipo_contrato <- read_excel("Data/tb_tipo_contrato.xlsx") %>% 
  mutate(Llave = as.character(Llave))
str(tb_tipo_contrato)

base_contratos <- fread("Data/Contratos_Diarios_2020_Nuevo.csv", na.strings = c("", "NA", "#/NA", "na")) %>% 
  data.frame() 
names(base_contratos) <- tolower(gsub(".","_",names(base_contratos),fixed = T))
names(base_contratos) <- chartr("áéíóú","aeiou",names(base_contratos))
base_contratos <- base_contratos %>%
  mutate(valor_inicial_del_contrato_o_convenio = as.numeric(valor_inicial_del_contrato_o_convenio),
         valor_de_la_adicion__si_las_hay_ = as.numeric(valor_de_la_adicion__si_las_hay_)) %>% 
  mutate(tipo_de_contrato_o_convenio = as.character(tipo_de_contrato_o_convenio)) %>%
  left_join(tb_tipo_contrato, by = c("tipo_de_contrato_o_convenio"="Llave")) %>% 
  mutate(tipo_contrato = ifelse(is.na(tipo_contrato),"Sin informacion",tipo_contrato),
         valor_inicial_del_contrato_o_convenio_m = as.numeric(valor_inicial_del_contrato_o_convenio)/1000000) %>%
  data.frame()
str(base_contratos)

name_contrato_convenio <- toupper(c("TODAS",unique(base_contratos$tipo_contrato)))
name_depedencia <- toupper(c("TODAS",unique(base_contratos$dependencia)))
name_tipo_reque <- toupper(c("TODAS",unique(base_contratos$tipo_requerimiento)))

### Pruebas ====

# aux <- base_contratos %>%
#   select(dependencia,fecha_solicitud,fecha_final_respuesta) %>%
#   dplyr::filter(!is.na(fecha_final_respuesta)) %>%
#   mutate(inicio_anio = "01/01/2020") %>%
#   mutate(inicio_anio = as.Date.character(inicio_anio, format = "%d/%m/%Y"),
#          fecha_final_respuesta = as.Date.character(fecha_final_respuesta, format = "%d/%m/%Y"),
#          fecha_solicitud = as.Date.character(fecha_solicitud, format = "%d/%m/%Y"),
#          dif_dias = difftime(fecha_final_respuesta,fecha_solicitud,units = "days"),
#          dia_min = difftime(fecha_solicitud,inicio_anio,units = "days") + 1) %>%
#   dplyr::filter(!is.na(dif_dias)) %>%
#   group_by(dependencia) %>%
#   summarise(pro_finalizacion = round(mean(dif_dias),1)) %>%
#   ungroup() %>%
#   mutate(dia_min = 0) %>%
#   arrange(desc(pro_finalizacion))

# library(ggalt)
# ggplot(aux, aes(x=dia_min, xend=pro_finalizacion, y=dependencia)) + 
#   #create a thick line between x and xend instead of using defaut 
#   #provided by geom_dubbell
#   geom_segment(aes(x=dia_min, 
#                    xend=pro_finalizacion, 
#                    y=dependencia, 
#                    yend=dependencia), 
#                color="#b2b2b2", size=1.5)+
#   geom_dumbbell(color="light blue", 
#                 size_x=3.5, 
#                 size_xend = 3.5,
#                 #Note: there is no US:'color' for UK:'colour' 
#                 # in geom_dumbbel unlike standard geoms in ggplot()
#                 colour_x="#edae52", 
#                 colour_xend = "#9fb059")+
#   labs(x=NULL, y=NULL, 
#        title="Contratos Finalizados 2020", 
#        subtitle=" Promedio atención (Días)")+
#   geom_text(color="black", size=2, hjust=-0.5,
#             aes(x=dia_min, label=dia_min))+
#   geom_text(aes(x=pro_finalizacion, label=pro_finalizacion), 
#             color="black", size=2, hjust=1.5)
# str(base_contratos)

# aux <- base_contratos %>%
#   filter_all(any_vars(str_detect(.,pattern = "EN TRAMITE UES"))) %>%
#   mutate(fecha_solicitud = as.Date.character(fecha_solicitud, format = "%d/%m/%Y"),
#          max_fecha = apply(.[,c('fecha_solicitud_1','fecha_solicitud_2','fecha_solicitud_3','fecha_solicitud_4',
#                                 'fecha_solicitud_5','fecha_solicitud_6','fecha_solicitud_7')], 1, max, na.rm=TRUE)) %>% 
#   select(dependencia,fecha_solicitud,max_fecha) %>% 
#   mutate(max_fecha = as.Date.character(max_fecha, tryFormats = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d"))) %>% 
#   mutate(dif_dias = difftime(max_fecha,fecha_solicitud,units = "days")) %>%
#   dplyr::filter(!is.na(dif_dias)) %>%
#   group_by(dependencia) %>%
#   summarise(pro_finalizacion = round(mean(dif_dias),1)) %>%
#   ungroup() %>%
#   mutate(dia_min = 0) %>%
#   arrange(desc(pro_finalizacion))
