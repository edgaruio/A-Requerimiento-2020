# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader)

nit <- "NIT8999990429" 

bd_afiliados <- readRDS("Data/bd_afiliados_28012020.rds") %>% 
  filter(id_empresa == nit) %>% 
  mutate(Genero = as.factor(Genero)) %>% 
  mutate(CuadranteViv2 = as.character(ifelse(is.na(CuadranteViv), NA, as.character(CuadranteViv)))) %>% 
  data.frame()
str(bd_afiliados)
sapply(bd_afiliados, function(x) sum(is.na(x)))

name_segmento <- c("Total","Alto","Joven","Medio","Básico")
name_categoria <- c("Total","A","B","C")

### Consultas ====

# numero de hijos
tb_hijos <- bd_afiliados %>%
  group_by(numero_hijos) %>%
  summarise(conteo = n())
tb_hijos
# library(esquisse)
# esquisser()

library(ggplot2)
ggplot(tb_hijos) +
 aes(x = numero_hijos, weight = conteo) +
 geom_bar(fill = "#0c4c8a") +
 labs(x = "Número de Hijos", y = "Conteo", title = "Distribución Número de Hijos") +
 theme_minimal()


# Nivel académico
table(bd_afiliados$nivel_academico)
