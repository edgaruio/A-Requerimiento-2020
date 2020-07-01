# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Cargamos librerias----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
library(RODBC); library(dplyr); library(tidyr)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Afiliados 2015----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

con2015 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/Afiliados 2015.accdb")
sqlTables(con2015)
# CC1000000189
tb_2015 <- sqlQuery(con2015, paste ("select Id_Empresa, Id_Persona, MES, estado from Afiliados"), as.is=T) %>% 
  data.frame()
odbcCloseAll() 
str(tb_2015)
table(tb_2015$estado)

afil_2015 <- tb_2015 %>% 
  mutate(anio_mes = as.factor(paste("2015", MES, sep = "_")),
         anio_mes = ordered(anio_mes, 
                            levels = c("2015_1","2015_2","2015_3","2015_4","2015_5","2015_6",
                                                 "2015_7","2015_8","2015_9","2015_10","2015_11","2015_12"),
                            labels = c("2015_1","2015_2","2015_3","2015_4","2015_5","2015_6",
                                       "2015_7","2015_8","2015_9","2015_10","2015_11","2015_12"))) %>%
  select(Id_Persona, anio_mes, estado_afil) %>%
  na.omit() %>% 
  group_by(Id_Persona,anio_mes) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  spread(key = anio_mes, value = estado)
str(afil_2015)
# rm(tb_2015)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Afiliados 2016----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

con2016 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/Afiliados 2016.accdb")
sqlTables(con2016)
# CC1000000189
tb_2016 <- sqlQuery(con2016, paste ("select Id_Empresa, Id_Persona, MES, estado from Afiliados"), as.is=T) %>% 
  data.frame()
odbcCloseAll() 
str(tb_2016)
table(tb_2016$estado)

afil_2016 <- tb_2016 %>% 
  mutate(anio_mes = as.factor(paste("2016", MES, sep = "_")),
         anio_mes = ordered(anio_mes, 
                            levels = c("2016_1","2016_2","2016_3","2016_4","2016_5","2016_6",
                                       "2016_7","2016_8","2016_9","2016_10","2016_11","2016_12"),
                            labels = c("2016_1","2016_2","2016_3","2016_4","2016_5","2016_6",
                                       "2016_7","2016_8","2016_9","2016_10","2016_11","2016_12"))) %>%
  select(Id_Persona, anio_mes, estado) %>%
  na.omit() %>% 
  group_by(Id_Persona,anio_mes) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  spread(key = anio_mes, value = estado, fill = NA)
str(afil_2016)
