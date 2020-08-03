# Librerias

library(dplyr); library(RODBC); library(lubridate)

# Kit Escolar 2019
con4.3 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2019.accdb")
kit2019 <- sqlFetch(con4.3, "base_kit_escolar_2019")
str(kit2019)
odbcCloseAll()

df_kit2019 <- kit2019 %>% 
  select(id_persona, estado_kit_escolar, estado_bono_escolar, fecha_redencion) %>% 
  mutate(anio_mes = paste(year(fecha_redencion),month(fecha_redencion), sep = "_")) %>% 
  group_by(anio_mes,estado_kit_escolar) %>% 
  count() %>% 
  data.frame()
str(df_kit2019)

# Kit Escolar 2018
con4 <- odbcDriverConnect( "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/BI/Proteccion_Social/Kit_Escolar_2018.accdb")
kit2018 <- sqlFetch(con4, "base_kit_escolar_2019")
str(kit2019)
odbcCloseAll()

df_kit2019 <- kit2019 %>% 
  select(id_persona, estado_kit_escolar, estado_bono_escolar)
str(df_kit2019)