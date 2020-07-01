# Librerias
options(scipen = 999)
rm(list = ls())
library(tidyr)
library(dplyr)
library(RODBC)
library(readxl)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data_consolidada ----
Data_consolidada <- readRDS('//BOG08HERNYATT/Analitica/BaseConsolidada/ConsolidacionFEB2020.rds')
names(Data_consolidada)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Empresas CIIU ----
empresas_ciiu <- read_excel("Fuentes/EmpresasCIIUCondicion V3.xlsx", sheet = "EmpresasCIIUCondicion") %>% 
  select(NumIdEmpresa,CIIU) %>% 
  data.frame() %>% 
  mutate(NumIdEmpresa = as.character(NumIdEmpresa))
str(empresas_ciiu)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Subsidio Mentario por Empresa ----

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Proteccion_Social/Subsidio_monetario_por_empresa.accdb")
sqlTables(channel)

tb_subsidio_emp <- sqlQuery(channel, paste ("select * from Subsidio_por_empresa"), as.is=T) %>% 
  data.frame()
str(tb_subsidio_emp)
odbcCloseAll() 

df_subsidio_emp <- tb_subsidio_emp  %>% 
  mutate(num_id_empresa = gsub("\\D","",id_empresa)) %>% 
  filter(num_id_empresa %in% empresas_ciiu$NumIdEmpresa) %>% 
  filter(!is.na(numero_de_cuotas_pagadas) & año >= 2018) %>% 
  mutate(anio_mes = factor(paste(año, mes, 01, sep = "/"))) %>% 
  mutate(Fecha = as.Date.character(anio_mes, format = "%Y/%m/%d")) %>% 
  select(id_empresa,Fecha,numero_de_cuotas_pagadas) %>% 
  group_by(Fecha) %>% 
  summarise(numero_de_cuotas_pagadas = sum(numero_de_cuotas_pagadas, na.rm = T)) %>% 
  ungroup() %>% 
  # spread(anio_mes, numero_de_cuotas_pagadas, fill = 0) %>% 
  data.frame() %>% 
  mutate(Fecha = as.character(Fecha))
str(df_subsidio_emp)


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Base Indicadores ----

indicadores <- read_excel("Fuentes/Consolidado_tasas.xlsx") %>%
  mutate(anio = substr(Fecha, 1, 4),
         mes = substr(Fecha, 5, 6)) %>% 
  filter(anio >= 2018) %>% 
  mutate(Fecha = as.Date.character(paste(anio, mes, 01, sep = "/"), format = "%Y/%m/%d")) %>%
  mutate(Fecha = as.character(Fecha)) %>% 
  data.frame()
str(indicadores)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Union bases ----

df_union <- df_subsidio_emp %>% 
  left_join(indicadores, by = "Fecha") %>% 
  select(-c(anio,mes))
names(df_union) <- chartr("áéíóú","aeiou",tolower(names(df_union)))
str(df_union)


saveRDS(df_union, "df_union.rds")
