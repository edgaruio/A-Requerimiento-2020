# Cargamos librerias
# options(scipen = 999)
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader)


bd_empresas <- readRDS("Data/info_actualizada12_23062020.rds") %>% 
  data.frame() %>% 
  filter(id_empresa == "NIT8600073861") %>%
  mutate(afiliados = numempleados,
         tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundaria"))%>% 
  data.frame()
names(bd_empresas)  

# Ajuste piramide_poblacional
consulta_piramide <- readRDS("Data/consulta_piramide_plot.rds") %>% 
  data.frame() %>% 
  filter(id_empresa == "NIT8600073861")
str(consulta_piramide)

# Ajuste Consumo empresarial
consumo_emp <- readRDS("Data/consumo_empresarial_Mayo2020.rds") %>% 
  mutate(anio_mes = paste(anno,mes,sep = "_")) %>% 
  filter(anio_mes %in% c("2019_6","2019_7","2019_8","2019_9","2019_10","2019_11","2019_12",
                         "2020_1","2020_2","2020_3","2020_4","2020_5")) %>%
  inner_join(bd_empresas %>% select(id_empresa,piramide1,piramide2,actividadciiu),by = "id_empresa") %>%
  filter(id_empresa == "NIT8600073861") %>%
  data.frame()
str(consumo_emp)
table(consumo_emp$servicio)
