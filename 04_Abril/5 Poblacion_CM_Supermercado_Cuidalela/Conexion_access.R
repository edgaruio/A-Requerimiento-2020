# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Data tablas Conversion ----
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

channel <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Tabla_conversion/Tabla_conversion.accdb"
)
sqlTables(channel)

Tb_localidad<- sqlQuery( 
  channel , 
  paste ("select * from Tb_localidad"),
  as.is=T
) %>% 
  data.frame()

Tb_Division_Politica_Dane<- sqlQuery( 
  channel , 
  paste ("select * from Tb_Division_Politica_Dane"),
  as.is=T
) %>% 
  data.frame()

odbcCloseAll() 
