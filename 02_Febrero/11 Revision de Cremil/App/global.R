# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader); library(readxl)

cremil <- readRDS("Data/cremil.rds") %>% 
  data.frame()

bd_afiliados <- readRDS("Data/bd_afiliados_28012020.rds") %>% 
  mutate(identificacion = gsub("\\D","",id_persona)) %>% 
  mutate(Genero = as.factor(Genero)) %>% 
  mutate(CuadranteViv2 = as.character(ifelse(is.na(CuadranteViv), NA, as.character(CuadranteViv)))) %>% 
  data.frame() %>% 
  filter(identificacion %in% cremil$id_persona) %>% 
  mutate(Piscilago = ifelse(Piscilago >= 1, 1, 0),
         Club = ifelse(Club >= 1, 1, 0),
         Hotel = ifelse(Hotel >= 1, 1, 0),
         RyT = ifelse(RyT >= 1, 1, 0)) %>% 
  data.frame()
str(bd_afiliados)
sapply(bd_afiliados, function(x) sum(is.na(x)))

name_segmento <- c("Total","Alto","Joven","Medio","Básico")
name_categoria <- c("Total","A","B","C")

