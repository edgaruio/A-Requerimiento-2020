# Cargar datos y librerias

library(shiny); library(ggplot2)
library(dplyr); library(plotly)
library(shinydashboard);library(DT)
library(shinyjs); library(reshape)
library(data.table); library(tidyr)
library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes)
library(dashboardthemes)
library(scales); library(shinycustomloader)
library(readxl); library(writexl)
library(shinyWidgets); library(lubridate)
library(janitor)

# Cargamos datos
poblacion_potencial <- readRDS("Data/df_poblacion_historico_28052020.rds") %>% 
  mutate(nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU,SectorCIIU),
         Categoria = as.character(Categoria)) %>% 
  data.frame()

poblacion_subsidio <- readRDS("Data/poblacion_subsidio_info.rds") %>% 
  data.frame()

tb_mes <- read_excel("Data/tb_mes.xlsx") %>% 
  mutate(mes = as.character(mes))
str(tb_mes)

poblacion_cajas <- read_excel("Data/Poblacion_Cajas.xlsx", sheet = 2) %>% 
  data.frame() %>% 
  mutate(FECHA = as.Date.character(paste(FECHA, "01", sep = "-"), format = "%Y-%m-%d"),
         CAFAM = ifelse(is.na(CAFAM), 0, CAFAM),
         COLSUBSIDIO = ifelse(is.na(COLSUBSIDIO), 0, COLSUBSIDIO),
         COMPENSAR = ifelse(is.na(COMPENSAR), 0, COMPENSAR))
str(poblacion_cajas)

# Entradas botones
name_actividad <- sort(unique(poblacion_potencial$nuevo_ciiu))
name_piramide <- sort(unique(poblacion_subsidio$piramide_2))
name_categoria <- unique(poblacion_potencial$Categoria)
fecha_min = min(poblacion_cajas$FECHA, na.rm = T)
fecha_max = max(poblacion_cajas$FECHA, na.rm = T)

# Pruebas

# test <- poblacion_cajas %>%
#   mutate(llave = paste(TIPO, FECHA, sep = "_")) %>%
#   select(-c(FECHA,TIPO)) %>%
#   gather("CCF","AFILIADOS",CAFAM:COMPENSAR) %>%
#   data.frame() %>%
#   mutate(FECHA = substr(llave, start = nchar(llave)-9, stop = nchar(llave)),
#          TIPO = substr(llave, start = 1, stop = nchar(llave)-11)) %>%
#   select(-llave)
# str(test)
# 
# test1 <- test %>%
#   filter(TIPO != "Total") %>%
#   select(FECHA, TIPO, CCF, AFILIADOS) %>%
#   group_by(FECHA,TIPO) %>%
#   mutate(PROP = AFILIADOS/sum(AFILIADOS)) %>%
#   ungroup()
# str(test1)
# 
# test2 <- test %>%
#   filter(TIPO == "Total") %>%
#   select(FECHA, TIPO, CCF, AFILIADOS) %>%
#   group_by(FECHA) %>%
#   mutate(PROP = AFILIADOS/sum(AFILIADOS)) %>%
#   ungroup()
# str(test2)
# 
# union_test <- bind_rows(test1,test2) %>%
#   data.frame() %>%
#   filter(TIPO == "Total") %>%
#   select(FECHA, CCF, PROP) %>%
#   plot_ly(x = ~FECHA, y = ~PROP, color = ~CCF) %>%
#   add_bars() %>%
#   layout(barmode = "stack")
# union_test

# %>% 
#   ggplot(.) +
#   aes(x = FECHA, fill = CCF, weight = AFILIADOS) +
#   geom_bar(position = "fill") +
#   scale_fill_hue() +
#   labs(x = "Fecha", y = "Participacion", title = "Distribucion", subtitle = "Participacion", fill = "CCF") +
#   theme_minimal()
# test
