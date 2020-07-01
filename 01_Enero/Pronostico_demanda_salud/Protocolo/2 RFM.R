### LIBRERIAS ====
rm(list = ls())
library(data.table); library(esquisse); library(dplyr); library(readxl); library(lubridate); library(tidyr); library(Hmisc)

### PREPROCESADO RFM ====
### 2017
lista2017 <- list.files(path = "PROYECTO/2017/", pattern = ".XLSX", recursive = T) %>% sort()
lista2017

ruta <- "PROYECTO/2017/"
aux_2017 <- read_excel(path = paste0(ruta,lista2017[1]))
for (i in 2:length(lista2017)) {
  actual <- read_excel(path = paste0(ruta,lista2017[i]))
  aux_2017 <- rbind(aux_2017,actual)
}
str(aux_2017)
rm(actual)

texto_material_2017 <- aux_2017 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

lista2018 <- list.files(path = "PROYECTO/2018/", pattern = ".XLSX", recursive = T) %>% sort()
lista2018

### 2018
lista2018 <- list.files(path = "PROYECTO/2018/", pattern = ".XLSX", recursive = T) %>% sort()
lista2018

ruta <- "PROYECTO/2018/"
aux_2018 <- read_excel(path = paste0(ruta,lista2018[1]))
for (i in 2:length(lista2018)) {
  actual <- read_excel(path = paste0(ruta,lista2018[i]))
  aux_2018 <- rbind(aux_2018,actual)
}
str(aux_2018)
rm(actual)

texto_material_2018 <- aux_2018 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()

### 2019
lista2019 <- list.files(path = "PROYECTO/2019/", pattern = ".XLSX", recursive = T) %>% sort()
lista2019

ruta <- "PROYECTO/2019/"
aux_2019 <- read_excel(path = paste0(ruta,lista2019[1]))
for (i in 2:length(lista2019)) {
  actual <- read_excel(path = paste0(ruta,lista2019[i]))
  aux_2019 <- rbind(aux_2019,actual)
}
str(aux_2019)
rm(actual)

texto_material_2019 <- aux_2019 %>% 
  select(Material,`Texto breve de material`) %>% 
  distinct()


### Union para RFM
library(writexl)
df_rfm <- bind_rows(aux_2017 %>% select(Material, Fe.contabilización, Cantidad, `Importe ML`),
                    aux_2018 %>% select(Material, Fe.contabilización, Cantidad, `Importe ML`),
                    aux_2019 %>% select(Material, Fe.contabilización, Cantidad, `Importe ML`)) %>% 
  dplyr::rename(fecha=Fe.contabilización, 
                valor = `Importe ML`) %>% 
  group_by(Material,fecha) %>% 
  summarise(cantidad = sum(abs(Cantidad), na.rm = T),
            valor = sum(abs(valor), na.rm = T)) %>% 
  data.frame()
str(df_rfm)
saveRDS(df_rfm, "Protocolo/datos_rfm.rds")

df_rfm_2017 <- aux_2017 %>% 
  select(Material, Fe.contabilización, Cantidad) %>% 
  group_by(Material,Fe.contabilización) %>% 
  summarise(Cantidad = sum(abs(Cantidad), na.rm = T)) %>% 
  data.frame()
str(df_rfm_2017)
fwrite(df_rfm_2017[1:1000,], file = "Protocolo/datos_rfm_2017.csv", row.names = F)
write_xlsx(df_rfm_2017[1:1000,], "Protocolo/datos_rfm_2017.xlsx")
saveRDS(df_rfm_2017, "Protocolo/datos_rfm_2017.rds")

df_rfm_2018 <- aux_2018 %>% 
  select(Material, Fe.contabilización, Cantidad) %>% 
  group_by(Material,Fe.contabilización) %>% 
  summarise(Cantidad = sum(abs(Cantidad), na.rm = T)) %>% 
  data.frame()
str(df_rfm_2018)
fwrite(df_rfm_2018, file = "Protocolo/datos_rfm_2018.csv", row.names = F)

df_rfm_2019 <- aux_2019 %>% 
  select(Material, Fe.contabilización, Cantidad) %>% 
  group_by(Material,Fe.contabilización) %>% 
  summarise(Cantidad = sum(abs(Cantidad), na.rm = T)) %>% 
  data.frame()
str(df_rfm_2019)
fwrite(df_rfm_2019, file = "Protocolo/datos_rfm_2019.csv", row.names = F)



### ANALISIS RFM ====
options(scipen = 999)
rm(list = ls())
df_rfm <- readRDS("Protocolo/datos_rfm.rds") %>% 
  mutate(Recencia = as.numeric(difftime(Sys.Date(),fecha,units = "days"))) %>% 
  group_by(Material) %>% 
  summarise(Recencia = min(Recencia),
            Frecuencia = sum(cantidad),
            Monto = sum(valor)
            ) %>% 
  ungroup() %>% 
  mutate(R_Score = cut2(Recencia, g = 5),
         F_Score = cut2(Frecuencia, g = 5),
         M_Score = cut2(Monto, g = 5)
         )
levels(df_rfm$R_Score) <- seq(5,1, by = -1)
levels(df_rfm$F_Score) <- seq(1,5)
levels(df_rfm$M_Score) <- seq(1,5)

df_rfm <- df_rfm %>%
  mutate(Puntaje = as.numeric(as.character(R_Score))*as.numeric(as.character(F_Score))*as.numeric(as.character(M_Score))) %>% 
  mutate(Calificacion = case_when(Puntaje >= 80 ~ "1. Alto",
                                  Puntaje < 80 & Puntaje >= 40 ~ "2. Medio",
                                  TRUE ~ "3. Bajo")) 
str(df_rfm)
table(df_rfm$Calificacion)

library(esquisse)
esquisser()

saveRDS(df_rfm, "Protocolo/bd_rfm_Score.rds")



