# Consulta_infografia

# Conectamos a access
rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(readxl)

### HITSS COLOMBIA SAS
nit_consulta <- "NIT9004208145"

tb_seg_poblacion <- read_excel("tb_segpob.xlsx")
str(tb_seg_poblacion)

conn_afil2015 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2015.accdb")
subset(sqlTables(conn_afil2015), tableType = "SYSTEM TABLE")
afil2015 <- sqlQuery(conn_afil2015, paste ("SELECT Afiliados.id_Empresa, Afiliados.id_Persona, Afiliados.Categoria, Afiliados.MES, Afiliados.Cod_SegmentoPoblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2015)
odbcClose(conn_afil2015)

df_afil2015 <- afil2015 %>% 
  dplyr::rename(id_empresa = id_Empresa, id_persona = id_Persona) %>% 
  filter(id_empresa == nit_consulta) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = c("Cod_SegmentoPoblacional"="codigo_segmento_poblacional")) %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2015) %>% 
  data.frame()
str(df_afil2015)


conn_afil2016 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2016.accdb")
subset(sqlTables(conn_afil2016), tableType = "SYSTEM TABLE")
afil2016 <- sqlQuery(conn_afil2016, paste ("SELECT Afiliados.id_Empresa, Afiliados.id_Persona, Afiliados.Categoria, Afiliados.MES, Afiliados.Cod_SegmentoPoblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2016)
odbcClose(conn_afil2016)

df_afil2016 <- afil2016 %>% 
  dplyr::rename(id_empresa = id_Empresa, id_persona = id_Persona) %>% 
  filter(id_empresa == nit_consulta) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = c("Cod_SegmentoPoblacional"="codigo_segmento_poblacional")) %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2016) %>% 
  data.frame()
str(df_afil2016)


conn_afil2017 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2017.accdb")
subset(sqlTables(conn_afil2017), tableType = "SYSTEM TABLE")
afil2017 <- sqlQuery(conn_afil2017, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2017)
odbcClose(conn_afil2017)

df_afil2017 <- afil2017 %>% 
  filter(id_empresa == nit_consulta) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2017) %>% 
  data.frame()
str(df_afil2017)


conn_afil2018 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados2018.accdb")
subset(sqlTables(conn_afil2018), tableType = "SYSTEM TABLE")
afil2018 <- sqlQuery(conn_afil2018, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2018)
odbcClose(conn_afil2018)

df_afil2018 <- afil2018 %>% 
  filter(id_empresa == nit_consulta) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2018) %>% 
  data.frame()
str(df_afil2018)


conn_afil2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Persona/Afiliados 2019.accdb")
subset(sqlTables(conn_afil2019), tableType = "SYSTEM TABLE")
afil2019 <- sqlQuery(conn_afil2019, paste ("SELECT Afiliados.id_empresa, Afiliados.id_persona, Afiliados.categoria, Afiliados.mes, Afiliados.codigo_segmento_poblacional FROM Afiliados"),stringsAsFactors = FALSE)
str(afil2019)
odbcClose(conn_afil2019)

df_afil2019 <- afil2019 %>% 
  filter(id_empresa == nit_consulta) %>% 
  mutate(id_empresa = as.character(id_empresa)) %>% 
  left_join(tb_seg_poblacion, by = "codigo_segmento_poblacional") %>% 
  group_by(id_empresa,segmento_poblacional) %>% 
  summarise(conteo = n_distinct(id_persona)) %>% 
  mutate(anio = 2019) %>% 
  data.frame()
str(df_afil2019)
sum(df_afil2019$conteo)
# rm(afil2017,afil2018,afil2019)


# Consolidado

df_union <- bind_rows(df_afil2018,df_afil2019) 
str(df_union)
library(writexl)
writexl::write_xlsx(df_union, "df_union_anio.xlsx")


# Crecimiento
crecimiento <- df_union %>% 
  group_by(anio) %>% 
  summarise(conteo = n_distint(id_persona))

# Consolidada
consolidada <- readRDS("ConsolidacionDIC2019.rds") %>% 
  filter(id_empresa == "nit_consulta")
str(consolidada)

table(consolidada$MunicipioPersona) %>% data.frame() %>% arrange(desc(Freq))


#### Usa de algun servicio
bd_afiliados <- readRDS("bd_afiliados_28012020.rds") %>% 
mutate(Genero = as.factor(Genero)) %>% 
  mutate(CuadranteViv2 = as.character(ifelse(is.na(CuadranteViv), NA, as.character(CuadranteViv)))) %>% 
  # left_join(consulta_piramide %>% select(id_persona,ACTIVIDAD), by = "id_persona") %>% 
  data.frame()
str(bd_afiliados)
sapply(bd_afiliados, function(x) sum(is.na(x)))

# Consulta 
nits <- c("NIT9004208145")
# NIT8600052167
test <- bd_afiliados %>% 
  dplyr::filter(id_empresa %in% nits) %>% 
  group_by(razon_social,id_empresa,numero_hijos) %>% 
  summarise(conteo = n())

test2 <- bd_afiliados %>% 
  dplyr::filter(id_empresa %in% nits)
table(test2$nivel_academico)

usa_servicios <- bd_afiliados %>% 
  dplyr::filter(id_empresa %in% nits) %>% 
  mutate(usa_servicios = RyT + Educacion + Supermercados + Medicamentos)
sum(usa_servicios$usa_servicios >= 1)/nrow(usa_servicios)

