rm(list = ls())
library(readxl); library(dplyr); library(tidyr)

# Extraer tabla
# library(tabulizer)
# 
# ruta <- "midiendo_el_crimen_1.pdf"
# 
# tabla <- extract_tables(ruta,output = "data.frame",pages = 66, encoding = "UTF-8", method = "lattice")
# tabla[[1]]

base_delitos_n <- read_excel("Datos_interes.xlsx", range = "A1:AI3") %>% 
  gather("departamento","pro_riesgo", 2:35)

base_delitos_p <- read_excel("Datos_interes.xlsx", range = "A6:AH8") %>% 
  gather("departamento","pro_riesgo", 2:34) 
str(base_delitos_p)

lavado <- base_delitos_p %>% 
  filter(`Bien Jurídico / Conducta Punible`  == "Lavado de Activos") %>% 
  select(-`Bien Jurídico / Conducta Punible`) %>% 
  mutate(departamento = chartr("ÁÉÍÓÚ","AEIOU",departamento),
         pro_riesgo = round(100*pro_riesgo,2))
str(lavado)

terrorismo <- base_delitos_p %>% 
  filter(`Bien Jurídico / Conducta Punible` == "Terrorismo") %>% 
  select(-`Bien Jurídico / Conducta Punible`) %>% 
  mutate(departamento = chartr("ÁÉÍÓÚ","AEIOU",departamento),
         pro_riesgo = round(100*pro_riesgo,2))
str(terrorismo)

# Droguerias
library(plotly); library(writexl)
droguerias <- read_excel("DIRECTORIO GENERAL MEDICAMENTOS ACTUALIZADO MAYO_2019_Serlaf.xls", sheet = "Hoja2") %>% 
  mutate(DEPARTAME = toupper(DEPARTAME)) %>% 
  mutate(DEPARTAME = ifelse(CIUDAD == "BOGOTA", CIUDAD, DEPARTAME)) %>% 
  mutate(DEPARTAME = ifelse(DEPARTAME == "BOGOTA", "BOGOTA D.C.", DEPARTAME)) %>% 
  group_by(DEPARTAME) %>% 
  summarise(conteo_drog = n_distinct(Cod))
str(droguerias)

riesgo_dro_terrorismo <- droguerias %>% 
  full_join(terrorismo, by = c("DEPARTAME"="departamento")) %>% 
  arrange(DEPARTAME) %>% 
  mutate(pro_droguerias = 100*round(conteo_drog/sum(conteo_drog, na.rm = T), 4))
str(riesgo_dro_terrorismo)
riesgo_dro_terrorismo[,c(2:4)][is.na(riesgo_dro_terrorismo[,c(2:4)])] <- 0

riesgo_dro_lavado <- droguerias %>% 
  full_join(lavado, by = c("DEPARTAME"="departamento")) %>% 
  arrange(DEPARTAME) %>% 
  mutate(pro_droguerias = 100*round(conteo_drog/sum(conteo_drog, na.rm = T), 4))
str(riesgo_dro_lavado)
riesgo_dro_lavado[,c(2:4)][is.na(riesgo_dro_lavado[,c(2:4)])] <- 0

# Salidas
write_xlsx(droguerias, "Presentacion/droguerias.xlsx")
write_xlsx(riesgo_dro_lavado, "Presentacion/riesgo_droguerias_lavado.xlsx")
write_xlsx(riesgo_dro_terrorismo, "Presentacion/riesgo_droguerias_terrorismo.xlsx")

