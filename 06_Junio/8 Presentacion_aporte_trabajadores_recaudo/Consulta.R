rm(list = ls())
library(RODBC); library(dplyr); library(tidyr); library(readxl)

fechas <- c("Marzo_2019","Abril_2019","Mayo","Junio_2019","Julio_2019","Agosto_2019","Septiembre_2019",
            "Octubre_2019","Noviembre_2019","Diciembre_2019",
            "Enero_2020","Febrero_2020")
fechas[12]

tb_mes <- read_excel("tb_mes.xlsx") %>% 
  mutate_all(as.character)
str(tb_mes)

# Aportes ====
conn_aportes <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Aporte.accdb")
subset(sqlTables(conn_aportes), tableType = "SYSTEM TABLE")
aporte <- sqlFetch(conn_aportes, "aporte")
str(aporte)
odbcClose(conn_aportes)

aporte_marzo <- aporte %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Marzo_2020") %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,aporte_4) %>% 
  rename(aporte_marzo = aporte_4)
str(aporte_marzo)

aporte_abril <- aporte %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Abril_2020") %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,aporte_4) %>% 
  rename(aporte_abril = aporte_4)

aporte_mayo <- aporte %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Mayo_2020") %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,aporte_4) %>% 
  rename(aporte_mayo = aporte_4)

str(aporte)
df_aporte <- aporte %>% 
  mutate(mes = as.character(mes)) %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes %in% fechas) %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,aporte_4,anio_mes) %>% 
  group_by(id_empresa) %>% 
  summarise(pro_aporte_4 = mean(aporte_4, na.rm = T),
            sd_aporte = sd(aporte_4, na.rm = T)) %>% 
  data.frame() %>% 
  left_join(aporte_marzo, by = "id_empresa") %>% 
  left_join(aporte_abril, by = "id_empresa") %>% 
  left_join(aporte_mayo, by = "id_empresa") %>% 
  mutate(pro_aporte_4 = ifelse(is.na(pro_aporte_4), 0, pro_aporte_4),
         sd_aporte = ifelse(is.na(sd_aporte), 0, sd_aporte),
         aporte_marzo = ifelse(is.na(aporte_marzo), 0, aporte_marzo),
         aporte_abril = ifelse(is.na(aporte_abril), 0, aporte_abril),
         aporte_mayo = ifelse(is.na(aporte_mayo), 0, aporte_mayo)) %>% 
  mutate(com_marzo = case_when(
    aporte_marzo >= (pro_aporte_4 + sd_aporte) ~ "Aumenta",
    (aporte_marzo >= (pro_aporte_4 - sd_aporte) & aporte_marzo <= (pro_aporte_4 + sd_aporte)) | sd_aporte == 0 ~ "Se mantiene" ,
    aporte_marzo <= (pro_aporte_4 - sd_aporte) ~ "Disminuye")) %>%
  mutate(com_abril = case_when(
    aporte_abril >= (pro_aporte_4 + sd_aporte) ~ "Aumenta",
    (aporte_abril >= (pro_aporte_4 - sd_aporte) & aporte_abril <= (pro_aporte_4 + sd_aporte)) | sd_aporte == 0 ~ "Se mantiene",
    aporte_abril <= (pro_aporte_4 - sd_aporte) ~ "Disminuye")) %>%
  mutate(com_mayo = case_when(
    aporte_mayo >= (pro_aporte_4 + sd_aporte) ~ "Aumenta",
    (aporte_mayo >= (pro_aporte_4 - sd_aporte) & aporte_mayo <= (pro_aporte_4 + sd_aporte)) | sd_aporte == 0 ~ "Se mantiene",
    aporte_mayo <= (pro_aporte_4 - sd_aporte) ~ "Disminuye")) %>% 
  mutate(nid_empresa = gsub("\\D","",id_empresa)) %>% 
  data.frame()
str(df_aporte)

# Recaudo ====
channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Proteccion_Social/Recaudo.accdb"
)
sqlTables(channel)

recaudo <- sqlQuery( 
  channel , 
  paste ("select * from Recaudo"),
  as.is=T
) %>% 
  data.frame()  

odbcCloseAll()

str(recaudo)
table(recaudo$fuente)

recaudo_marzo <- recaudo %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Marzo_2020") %>% 
  mutate(fecha_periodo_pago = funcion_fecha(periodo_pago),
         fecha_transaccion = as.Date.character(paste(año, mes, "01", sep = "-"), formar = "%Y-%m-%d") - 15) %>% 
  mutate(fecha_periodo_pago = paste0(year(fecha_periodo_pago), month(fecha_periodo_pago)),
         fecha_transaccion = paste0(year(fecha_transaccion), toupper(month(fecha_transaccion)))) %>% 
  mutate(TipoPago=case_when(fecha_periodo_pago < fecha_transaccion ~ "Pago anterior",
                            fecha_periodo_pago == fecha_transaccion ~ "Aporte",
                            fecha_periodo_pago > fecha_transaccion ~ "Anticipo")) %>%
  select(tx_nit_empresa,valor_planilla) %>% 
  rename(recaudo_marzo = valor_planilla) %>% 
  group_by(tx_nit_empresa) %>% 
  summarise(recaudo_marzo = sum(recaudo_marzo, na.rm = T)) %>% 
  data.frame()
str(recaudo_marzo)
table(duplicated(recaudo_marzo$tx_nit_empresa))

recaudo_abril <- recaudo %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Abril_2020") %>% 
  mutate(fecha_periodo_pago = funcion_fecha(periodo_pago),
         fecha_transaccion = as.Date.character(paste(año, mes, "01", sep = "-"), formar = "%Y-%m-%d") - 15) %>% 
  mutate(fecha_periodo_pago = paste0(year(fecha_periodo_pago), month(fecha_periodo_pago)),
         fecha_transaccion = paste0(year(fecha_transaccion), toupper(month(fecha_transaccion)))) %>% 
  mutate(TipoPago=case_when(fecha_periodo_pago < fecha_transaccion ~ "Pago anterior",
                            fecha_periodo_pago == fecha_transaccion ~ "Aporte",
                            fecha_periodo_pago > fecha_transaccion ~ "Anticipo")) %>%
  select(tx_nit_empresa,valor_planilla) %>% 
  rename(recaudo_abril = valor_planilla) %>% 
  group_by(tx_nit_empresa) %>% 
  summarise(recaudo_abril = sum(recaudo_abril, na.rm = T)) %>% 
  data.frame()
str(recaudo_abril)
table(duplicated(recaudo_abril$tx_nit_empresa))

recaudo_mayo <- recaudo %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes == "Abril_2020") %>% 
  mutate(fecha_periodo_pago = funcion_fecha(periodo_pago),
         fecha_transaccion = as.Date.character(paste(año, mes, "01", sep = "-"), formar = "%Y-%m-%d") - 15) %>% 
  mutate(fecha_periodo_pago = paste0(year(fecha_periodo_pago), month(fecha_periodo_pago)),
         fecha_transaccion = paste0(year(fecha_transaccion), toupper(month(fecha_transaccion)))) %>% 
  mutate(TipoPago=case_when(fecha_periodo_pago < fecha_transaccion ~ "Pago anterior",
                            fecha_periodo_pago == fecha_transaccion ~ "Aporte",
                            fecha_periodo_pago > fecha_transaccion ~ "Anticipo")) %>%
  select(tx_nit_empresa,valor_planilla) %>% 
  rename(recaudo_mayo = valor_planilla) %>% 
  group_by(tx_nit_empresa) %>% 
  summarise(recaudo_mayo = sum(recaudo_mayo, na.rm = T)) %>% 
  data.frame()
str(recaudo_mayo)
table(duplicated(recaudo_mayo$tx_nit_empresa))

funcion_fecha <- function(fecha){
  fecha <- as.character(fecha)
  fecha <- as.Date.character(paste(substr(fecha,1,4), substr(fecha, 5, nchar(fecha)), "01", sep = "-"), format = "%Y-%m-%d")
  return(fecha)
}

table(duplicated(df_recaudo$tx_nit_empresa))
library(lubridate)
df_recaudo <- recaudo %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(anio_mes = paste(mes2,año,sep="_")) %>% 
  filter(anio_mes %in% fechas) %>% 
  mutate(fecha_periodo_pago = funcion_fecha(periodo_pago),
         fecha_transaccion = as.Date.character(paste(año, mes, "01", sep = "-"), formar = "%Y-%m-%d") - 15) %>% 
  mutate(fecha_periodo_pago = paste0(year(fecha_periodo_pago), month(fecha_periodo_pago)),
         fecha_transaccion = paste0(year(fecha_transaccion), toupper(month(fecha_transaccion)))) %>% 
  mutate(TipoPago=case_when(fecha_periodo_pago < fecha_transaccion ~ "Pago anterior",
                            fecha_periodo_pago == fecha_transaccion ~ "Aporte",
                            fecha_periodo_pago > fecha_transaccion ~ "Anticipo")) %>%
  select(tx_nit_empresa,fecha_transaccion, fecha_periodo_pago, TipoPago, valor_planilla) %>%
  group_by(tx_nit_empresa) %>% 
  summarise(pro_recaudo = mean(valor_planilla, na.rm = T),
         sd_recaudo = sd(valor_planilla, na.rm = T)) %>% 
  data.frame() %>% 
  left_join(recaudo_marzo, by = "tx_nit_empresa") %>% 
  left_join(recaudo_abril, by = "tx_nit_empresa") %>%
  left_join(recaudo_mayo, by = "tx_nit_empresa") %>% 
  select(tx_nit_empresa,pro_recaudo,sd_recaudo,recaudo_marzo,recaudo_abril,recaudo_mayo) %>% 
  mutate(pro_recaudo = ifelse(is.na(pro_recaudo), 0, pro_recaudo),
         sd_recaudo = ifelse(is.na(sd_recaudo), 0, sd_recaudo),
         recaudo_marzo = ifelse(is.na(recaudo_marzo), 0, recaudo_marzo),
         recaudo_abril = ifelse(is.na(recaudo_abril), 0, recaudo_abril),
         recaudo_mayo = ifelse(is.na(recaudo_mayo), 0, recaudo_mayo)) %>% 
  mutate(com_marzo_recaudo = case_when(
    recaudo_marzo >= (pro_recaudo + sd_recaudo) ~ "Aumenta",
    (recaudo_marzo >= (pro_recaudo - sd_recaudo) & recaudo_marzo <= (pro_recaudo + sd_recaudo)) | sd_recaudo == 0 ~ "Se mantiene",
    recaudo_marzo <= (pro_recaudo - sd_recaudo) ~ "Disminuye")) %>%
  mutate(com_abril_recaudo = case_when(
    recaudo_abril >= (pro_recaudo + sd_recaudo) ~ "Aumenta",
    (recaudo_abril >= (pro_recaudo - sd_recaudo) & recaudo_abril <= (pro_recaudo + sd_recaudo)) | sd_recaudo == 0 ~ "Se mantiene",
    recaudo_abril <= (pro_recaudo - sd_recaudo) ~ "Disminuye")) %>%
  mutate(com_mayo_recaudo = case_when(
    recaudo_mayo >= (pro_recaudo + sd_recaudo) ~ "Aumenta",
    (recaudo_mayo >= (pro_recaudo - sd_recaudo) & recaudo_mayo <= (pro_recaudo + sd_recaudo)) | sd_recaudo == 0 ~ "Se mantiene",
    recaudo_mayo <= (pro_recaudo - sd_recaudo) ~ "Disminuye"))
table(duplicated(df_recaudo$tx_nit_empresa))
str(df_recaudo)
str(df_aporte)
sum(is.na(df_recaudo$com_mayo_recaudo))

consolidada <- readRDS("//Bogak08beimrodc/bi/Base_Mes/ConsolidadosMensuales/ConsolidacionMAY2020.rds") %>% 
  select(id_empresa,RazonSocial,Piramide1,Piramide2,SectorCIIU,ActividadCIIU) %>% 
  distinct() %>% 
  mutate(nuevo_ciiu = ifelse(SectorCIIU == "Prestación de Servicios (Educación, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                             ActividadCIIU, SectorCIIU),
         nuevo_ciiu = ifelse(is.na(nuevo_ciiu), "Sin información", nuevo_ciiu))
str(consolidada)
unique(consolidada$nuevo_ciiu)

# Uniones ====
df_trabajadores <- readRDS("base_consol_trabajadores.rds") %>% 
  filter(id_empresa != "#VALUE!") %>% 
  rename(com_marzo_t=com_marzo,
         com_abril_t=com_abril,
         com_mayo_t=com_mayo)
str(df_trabajadores)
table(duplicated(df_trabajadores$id_empresa))

str(df_recaudo)
df_union <- consolidada %>% 
  left_join(df_aporte, by = "id_empresa") %>% 
  left_join(df_trabajadores, by = "id_empresa") %>% 
  left_join(df_recaudo, by = c("nid_empresa"="tx_nit_empresa")) %>% 
  filter(!(Piramide2) %in% c("4.6 Transaccional - Facultativo ","4.7 Transaccional - Independiente","4.8 Transaccional - Pensionado")) %>% 
  mutate(com_mayo = ifelse(is.na(com_mayo), "Sin información", com_mayo),
         com_mayo_t = ifelse(is.na(com_mayo_t), "Sin información", com_mayo_t),
         com_mayo_recaudo = ifelse(is.na(com_mayo_recaudo), "Sin información", com_mayo_recaudo))
str(df_union)
table(df_union$Piramide2)
sum(is.na(df_union$com_mayo))

library(writexl)
write_xlsx(df_union, "Salida.xlsx")
sum(is.na(df_recaudo$com_mayo_recaudo))

salida2 <- df_union %>% 
  select(id_empresa,RazonSocial,Piramide1,Piramide2,com_mayo_aporte=com_mayo,com_mayo_tra=com_mayo_t,com_mayo_recaudo)
str(salida2)
write_xlsx(salida2, "salida2.xlsx")
