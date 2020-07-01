# Cargar datos y librerias

rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader); library(writexl)

df_material <- readRDS("Data/df_material.rds") %>% 
  distinct() %>% 
  data.frame() %>% 
  dplyr::rename(texto=Texto.breve.de.material)
str(df_material)

pronostico_grupo <- readRDS("Data/salida_24032020.rds") %>% 
  left_join(df_material, by = c("material"="Material")) %>% 
  mutate(pronostico = round(pronostico))
str(pronostico_grupo)
# pronostico_grupo %>% filter(duplicated(material))

# fit_arima <- readRDS(here("models", 'arima.rds'))
# fit_BC <- readRDS(here("models", 'box_cox.rds'))
# fit_net <- readRDS(here("models", 'neural_net.rds'))
# fit_meanf <- readRDS(here("models", 'meanf.rds'))
# fit_naive <- readRDS(here("models", 'naive.rds'))
# fit_snaive <- readRDS(here("models", 'snaive.rds'))
# fit_ets <- readRDS(here("models", 'ets.rds'))



