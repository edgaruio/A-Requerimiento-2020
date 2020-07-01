# Consulta 0

# Requerimiento_2020_01_17_Key_z7zim1tB <- read_excel("Requerimiento_2020-01-17_Key_z7zim1tB.xlsx", sheet = "Data_Dane")
# View(Requerimiento_2020_01_17_Key_z7zim1tB)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Consideraciones iniciales ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# reboot
rm(list = ls())

# notacion cientifica
options(scipen = 999)

# librerias
library(tidyr)
library(dplyr)
library(RODBC)
library(readxl)
library(data.table)
library(writexl)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data_consolidada ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Data_consolidada <- readRDS('ConsolidacionDIC2019.rds')
sort(names(Data_consolidada))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data tablas Conversion ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# channel <- odbcDriverConnect(
#   "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/SEPTIEMBRE/Analisis_Subsidios/Consulta_sub.accdb"
# )
# sqlTables(channel)
# 
# Tb_localidad<- sqlQuery( 
#   channel , 
#   paste ("select * from consulta_subsidio"),
#   as.is=T) %>% 
#   data.frame()
# 
# odbcCloseAll() 

Tb_localidad <- readRDS("consulta_poblacion.rds") %>% 
  mutate(Edad = as.numeric(Edad),
         Edad.1 = as.numeric(Edad.1)) %>% 
  filter(marca_afiliado_unico == "X") %>% 
  select(-Edad)
str(Tb_localidad)

# Cargamos pob obje
pob_obj <- fread("Afiliados2020-01-31 10_36_17.csv") %>% 
  select(id_persona,Segmento_poblacional,Categoria,Edad,Genero) %>%
  data.frame()
str(pob_obj)

pon_obj_fil <- pob_obj %>% 
  left_join(Tb_localidad, by = "id_persona") %>% 
  mutate(
    rangoedad = case_when(
      Edad < 7 ~ "Menos de 6 años",
      Edad > 6 & Edad < 9 ~ "Entre 7 y 8",
      Edad > 8 & Edad < 11 ~ "Entre 9 y 10",
      Edad > 10 & Edad < 13 ~ "Entre 11 y 12",
      Edad > 12 & Edad < 15 ~ "Entre 13 y 14",
      Edad > 14 & Edad < 18 ~ "Entre 15 y 17",
      Edad > 17 & Edad < 50 ~ "Entre 18 y 49",
      Edad > 49 ~ "Mas de 50"),
    rangoedad_ben = case_when(
      Edad.1 < 7 ~ "Menos de 6 años",
      Edad.1 > 6 & Edad.1 < 9 ~ "Entre 7 y 8",
      Edad.1 > 8 & Edad.1 < 11 ~ "Entre 9 y 10",
      Edad.1 > 10 & Edad.1 < 13 ~ "Entre 11 y 12",
      Edad.1 > 12 & Edad.1 < 15 ~ "Entre 13 y 14",
      Edad.1 > 14 & Edad.1 < 18 ~ "Entre 15 y 17",
      Edad.1 > 17 & Edad.1 < 50 ~ "Entre 18 y 49",
      Edad.1 > 49 ~ "Mas de 50")
    )

write_xlsx(pon_obj_fil, "Poblacion_filtrada.xlsx")
# saveRDS(Tb_localidad,"consulta_poblacion.rds")
