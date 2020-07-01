# Consulta_infografia

# Conectamos a access
rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(readxl)

#Tb mes
tb_mes <- read_excel("tb_mes.xlsx") %>% 
  mutate(mes = as.character(mes))
str(tb_mes)

# Base consolidada
personas <- readRDS("ConsolidacionABR2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona,id_empresa)
str(personas)
empresas <- readRDS("ConsolidacionABR2020.rds") %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  select(id_empresa,NumEmpleados,Gen_F,Gen_M)
str(empresas)

# Distribucion hijos
distribucion_hijos <- readRDS("ConsolidacionABR2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_empresa,id_persona,numero_hijos) %>% 
  group_by(id_empresa,numero_hijos) %>% 
  summarise(conteo = n()) %>% 
  spread(numero_hijos,conteo, fill = 0) %>% 
  data.frame()
names(distribucion_hijos) <- gsub("X","Afiliados_con_hijos",names(distribucion_hijos), fixed = T)
str(distribucion_hijos)

# Poblacion beneficiaria
poblacion_bene <- fread("consulta_subsidio.txt") %>% 
  filter(marca_afiliado_unico == "X",
         parentesco == "HIJO") %>% 
  mutate(Edad_hijos = cut(Persona_1_Edad, breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120), include.lowest = TRUE,
                          labels = c("Conteo_hijos_0_10","Conteo_hijos_10_20","Conteo_hijos_20_30","Conteo_hijos_30_40","Conteo_hijos_40_50","Conteo_hijos_50_60",
                                     "Conteo_hijos_60_70","Conteo_hijos_70_80","Conteo_hijos_80_90","Conteo_hijos_90_100","Conteo_hijos_100_110","Conteo_hijos_110_120"))) %>% 
  group_by(id_empresa,Edad_hijos) %>% 
  summarise(conteo = n_distinct(id_persona_familiar)) %>% 
  spread(Edad_hijos, conteo, fill = 0) %>% 
  data.frame() %>% 
  mutate(Num_hijos = apply(.[,c(2:12)], 1, sum))
str(poblacion_bene)

# Aportes
conn_aportes <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO 2020/3 MARZO/7 Consulta_Infografías/Aporte.accdb")
subset(sqlTables(conn_aportes), tableType = "SYSTEM TABLE")
aporte <- sqlFetch(conn_aportes, "aporte")
str(aporte)
odbcClose(conn_aportes)

df_aporte <- aporte %>% 
  mutate(mes = as.character(mes)) %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste(mes_2,año,sep="_")) %>% 
  filter(anio_mes %in% c("Mayo_2019","Junio_2019",
                         "Julio_2019","Agosto_2019","Septiembre_2019","Octubre_2019","Noviembre_2019","Diciembre_2019",
                         "Enero_2020","Febrero_2020","Marzo_2020","Abril_2020")) %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,aporte_4,anio_mes) %>% 
  group_by(id_empresa,anio_mes) %>% 
  summarise(aporte_4 = sum(aporte_4, na.rm = T)) %>% 
  spread(key = anio_mes, value = aporte_4, fill = 0) %>% 
  data.frame() %>% 
  mutate(aporte12 = rowSums(.[,c("Mayo_2019","Junio_2019",
                                 "Julio_2019","Agosto_2019","Septiembre_2019","Octubre_2019","Noviembre_2019","Diciembre_2019",
                                 "Enero_2020","Febrero_2020","Marzo_2020", "Abril_2020")])) %>% 
  select(id_empresa,Abril_2020,aporte12) %>% 
  dplyr::rename(Aporte_Abril_2020=Abril_2020)
str(df_aporte)
table(duplicated(df_aporte$id_empresa))
table(df_aporte$año)
rm(aporte)

# Incluimos remanentes
conn_remanente <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Remanente_bruto_y_neto_mes_a_mes.accdb")
subset(sqlTables(conn_remanente), tableType = "SYSTEM TABLE")
remanente_bruto <- sqlFetch(conn_remanente, "remanente_bruto")
remanente_neto <- sqlFetch(conn_remanente, "remanente_neto")
str(remanente_bruto)
str(remanente_neto)
odbcClose(conn_remanente)

df_remanente_bruto <- remanente_bruto %>% 
  mutate(mes = as.character(mes)) %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste("Rbruto",mes_2,año,sep="_")) %>% 
  filter(anio_mes %in% c("Rbruto_Mayo_2019","Rbruto_Junio_2019","Rbruto_Julio_2019","Rbruto_Agosto_2019",
                         "Rbruto_Septiembre_2019","Rbruto_Octubre_2019","Rbruto_Noviembre_2019",
                         "Rbruto_Diciembre_2019","Rbruto_Enero_2020","Rbruto_Febrero_2020",
                         "Rbruto_Marzo_2020","Rbruto_Abril_2020")) %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,remanente_bruto,anio_mes) %>% 
  group_by(id_empresa,anio_mes) %>% 
  summarise(remanente_bruto = sum(remanente_bruto,na.rm = T)) %>% 
  spread(key = anio_mes, value = remanente_bruto, fill = 0) %>% 
  data.frame() %>% 
  mutate(remanente_bruto12 = rowSums(.[,c("Rbruto_Mayo_2019","Rbruto_Junio_2019","Rbruto_Julio_2019","Rbruto_Agosto_2019",
                                          "Rbruto_Septiembre_2019","Rbruto_Octubre_2019","Rbruto_Noviembre_2019",
                                          "Rbruto_Diciembre_2019","Rbruto_Enero_2020","Rbruto_Febrero_2020",
                                          "Rbruto_Marzo_2020","Rbruto_Abril_2020")])) %>% 
  select(id_empresa,Rbruto_Abril_2020,remanente_bruto12)
str(df_remanente_bruto)

df_remanente_neto <- remanente_neto %>% 
  mutate(mes = as.character(mes)) %>% 
  left_join(tb_mes, by = "mes") %>% 
  mutate(id_empresa = as.character(id_empresa),
         anio_mes = paste("Rneto",mes_2,año,sep="_")) %>% 
  filter(anio_mes %in% c("Rneto_Mayo_2019","Rneto_Junio_2019","Rneto_Julio_2019",
                         "Rneto_Agosto_2019","Rneto_Septiembre_2019","Rneto_Octubre_2019","Rneto_Noviembre_2019",
                         "Rneto_Diciembre_2019","Rneto_Enero_2020","Rneto_Febrero_2020","Rneto_Marzo_2020",
                         "Rneto_Abril_2020")) %>% 
  filter(id_empresa != "#VALUE!") %>% 
  select(id_empresa,remanente_neto,anio_mes) %>% 
  group_by(id_empresa,anio_mes) %>% 
  summarise(remanente_neto = sum(remanente_neto,na.rm = T)) %>% 
  spread(key = anio_mes, value = remanente_neto, fill = 0) %>% 
  data.frame() %>% 
  mutate(remanente_neto12 = rowSums(.[,c("Rneto_Mayo_2019","Rneto_Junio_2019","Rneto_Julio_2019",
                                         "Rneto_Agosto_2019","Rneto_Septiembre_2019","Rneto_Octubre_2019",
                                         "Rneto_Noviembre_2019","Rneto_Diciembre_2019","Rneto_Enero_2020",
                                         "Rneto_Febrero_2020","Rneto_Marzo_2020","Rneto_Abril_2020")])) %>% 
  select(id_empresa,Rneto_Abril_2020,remanente_neto12)
str(df_remanente_neto)

# Cuota monetaria
conn_cm <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/GiroCuotaMonetaria2018.accdb")
subset(sqlTables(conn_cm), tableType = "SYSTEM TABLE")
cm <- sqlFetch(conn_cm, "giro_cuotamonetaria")
str(cm)
odbcClose(conn_cm)

df_cm <- cm %>% 
  mutate(mes = as.character(mes)) %>% 
  mutate(id_persona = as.character(id_persona)) %>% 
  left_join(tb_mes, by = "mes") %>%
  mutate(anio_mes = paste("Cm",mes_2,año,sep = "_")) %>%
  filter(anio_mes %in% c("Cm_Mayo_2019","Cm_Junio_2019","Cm_Julio_2019",
                         "Cm_Agosto_2019","Cm_Septiembre_2019","Cm_Octubre_2019","Cm_Noviembre_2019",
                         "Cm_Diciembre_2019","Cm_Enero_2020","Cm_Febrero_2020","Cm_Marzo_2020","Cm_Abril_2020")) %>% 
  left_join(personas, by = "id_persona") %>% 
  group_by(id_empresa,anio_mes) %>% 
  summarise(cm_giro = sum(valor, na.rm = T)) %>% 
  spread(key = anio_mes, value = cm_giro, fill = 0) %>% 
  data.frame() %>% 
  mutate(Cm12 = rowSums(.[,c("Cm_Mayo_2019","Cm_Junio_2019","Cm_Julio_2019",
                             "Cm_Agosto_2019","Cm_Septiembre_2019","Cm_Octubre_2019","Cm_Noviembre_2019",
                             "Cm_Diciembre_2019","Cm_Enero_2020","Cm_Febrero_2020","Cm_Marzo_2020","Cm_Abril_2020")])) %>% 
  select(id_empresa,Cm_Abril_2020,Cm12)
str(df_cm)

# Bono lonchera
conn_bono <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/BonoLonchera.accdb")
subset(sqlTables(conn_bono), tableType = "SYSTEM TABLE")
bono <- sqlFetch(conn_bono, "BonoLochera")
str(bono)
odbcClose(conn_bono)

df_bono <- bono %>% 
  mutate(MES = as.character(MES)) %>% 
  mutate(id_persona = as.character(id_persona)) %>% 
  left_join(tb_mes, by = c("MES"="mes")) %>%
  mutate(anio_mes = paste("Bono",mes_2,AÑO,sep = "_")) %>%
  filter(anio_mes %in% c("Bono_Mayo_2019","Bono_Junio_2019","Bono_Julio_2019",
                         "Bono_Agosto_2019","Bono_Septiembre_2019","Bono_Octubre_2019","Bono_Noviembre_2019","Bono_Diciembre_2019",
                         "Bono_Enero_2020","Bono_Febrero_2020","Bono_Marzo_2020","Bono_Abril_2020")) %>% 
  left_join(personas, by = "id_persona") %>% 
  group_by(id_empresa,anio_mes) %>% 
  summarise(bono_girado = n(),
            valor_bono = 15000*bono_girado) %>% 
  select(-bono_girado) %>% 
  spread(key = anio_mes, value = valor_bono, fill = 0) %>% 
  data.frame() %>% 
  mutate(Bono12 = rowSums(.[,c("Bono_Mayo_2019","Bono_Junio_2019","Bono_Julio_2019",
                               "Bono_Agosto_2019","Bono_Septiembre_2019","Bono_Octubre_2019","Bono_Noviembre_2019","Bono_Diciembre_2019",
                               "Bono_Enero_2020","Bono_Febrero_2020","Bono_Marzo_2020","Bono_Abril_2020")])) %>% 
  select(id_empresa,Bono_Abril_2020,Bono12)
str(df_bono)

# kit escolar
conn_kit2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Kit_Escolar_2020.accdb")
subset(sqlTables(conn_kit2019), tableType = "SYSTEM TABLE")
kit2019 <- sqlFetch(conn_kit2019, "base_kit_escolar_2020")
str(kit2019)
odbcClose(conn_kit2019)

df_kit2019 <- kit2019 %>% 
  select(id_persona,id_empresa,id_persona_grupofamiliar) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  group_by(id_empresa) %>% 
  summarise(kit_girado = n_distinct(id_persona_grupofamiliar),
            valor_kit = 75464*kit_girado) %>% 
  select(id_empresa,valor_kit)
str(df_kit2019)

# df_escolaridad
df_escolaridad <- readRDS("ConsolidacionABR2020.rds") %>% 
  filter(marca_afiliado_unico) %>% 
  select(id_persona,id_empresa,nivel_academico) %>% 
  group_by(id_empresa,nivel_academico) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  ungroup() %>% 
  arrange(id_empresa, desc(conteo)) %>% 
  group_by(id_empresa) %>% 
  filter(row_number()==1) %>% 
  data.frame() %>% 
  select(-conteo)
str(df_escolaridad)
table(duplicated(df_escolaridad$id_empresa))

# Consolidada por empresas rentabilidad
str(df_kit2019)
consolidada_rentabilidad <- empresas %>% 
  left_join(distribucion_hijos, by = "id_empresa") %>% 
  left_join(poblacion_bene, by = "id_empresa") %>% 
  left_join(df_aporte, by = "id_empresa") %>% 
  left_join(df_remanente_bruto, by = "id_empresa") %>% 
  left_join(df_remanente_neto, by = "id_empresa") %>% 
  left_join(df_cm, by = "id_empresa") %>% 
  left_join(df_bono, by = "id_empresa") %>% 
  left_join(df_kit2019, by = "id_empresa") %>% 
  left_join(df_escolaridad, by = "id_empresa")
str(consolidada_rentabilidad)

library(writexl)
write_xlsx(consolidada_rentabilidad, "consulta_empresas_30052020.xlsx")


